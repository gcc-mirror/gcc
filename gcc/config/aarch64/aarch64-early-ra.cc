// Early register allocation pass.
// Copyright (C) 2023-2024 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.
//
// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// This pass implements a simple form of early register allocation.
// It is restricted to FP/SIMD registers, and it only allocates
// a region of FP/SIMD usage if it can do so without any spilling.
// It punts on anything too complicated, leaving it to the real
// register allocator.
//
// There are two main purposes:
//
// (1) The pass runs before scheduling.  It therefore has a chance to
//     bag a spill-free allocation, if there is one, before scheduling
//     moves things around.
//
// (2) The pass can make use of strided register operations, such as the
//     strided forms of LD1 and ST1 in SME2.
//
// The allocator works at the level of individual FPRs, rather than whole
// pseudo registers.  It is mostly intended to help optimize ACLE code.
//
// The pass is very simplistic.  There are many things that could be improved.
#define IN_TARGET_CODE 1

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_ARRAY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "rtl-ssa.h"
#include "tree-pass.h"
#include "target.h"
#include "expr.h"
#include "cfgrtl.h"
#include "print-rtl.h"
#include "insn-attr.h"
#include "insn-opinit.h"
#include "reload.h"

template<typename T>
class simple_iterator : public wrapper_iterator<T>
{
public:
  using wrapper_iterator<T>::wrapper_iterator;

  simple_iterator &operator-- () { --this->m_contents; return *this; }
  simple_iterator operator-- (int) { return this->m_contents--; }
  simple_iterator &operator++ () { ++this->m_contents; return *this; }
  simple_iterator operator++ (int) { return this->m_contents++; }
};

using namespace rtl_ssa;

namespace {
const pass_data pass_data_early_ra =
{
  RTL_PASS, // type
  "early_ra", // name
  OPTGROUP_NONE, // optinfo_flags
  TV_NONE, // tv_id
  0, // properties_required
  0, // properties_provided
  0, // properties_destroyed
  0, // todo_flags_start
  TODO_df_finish, // todo_flags_finish
};

using allocno_iterator = simple_iterator<unsigned int>;

// Class that represents one run of the pass.
class early_ra
{
public:
  early_ra (function *fn);
  ~early_ra ();
  void execute ();

private:
  // Whether to test only things that are required for correctness,
  // or whether to take optimization heuristics into account as well.
  enum test_strictness { CORRECTNESS_ONLY, ALL_REASONS };

  static_assert (MAX_RECOG_OPERANDS <= 32, "Operand mask is 32 bits");
  using operand_mask = uint32_t;

  // Points in the function are represented using "program points".
  // The program points are allocated in reverse order, with smaller
  // numbers indicating later points.  These special values indicate
  // the start and end of a region.
  static constexpr unsigned int START_OF_REGION = ~0U;
  static constexpr unsigned int END_OF_REGION = 0U;

  // An invalid allocno index, used to represent no allocno.
  static constexpr unsigned int INVALID_ALLOCNO = ~0U;

  // Enumerates the single FPR sizes that matter for register allocation.
  // Anything smaller than 64 bits is treated as FPR_D.
  enum fpr_size_info
  {
    FPR_D,
    FPR_Q,
    FPR_Z
  };

  // A live range for an FPR, containing program points [START_POINT,
  // END_POINT].  If ALLOCNO is not INVALID_ALLOCNO, the FPR is known
  // to be equal to ALLOCNO for the duration of the live range.
  struct fpr_range_info
  {
    unsigned int start_point;
    unsigned int end_point;
    unsigned int allocno;
  };

  // Flags used in pseudo_reg_info.
  //
  // Whether the pseudo register occurs in one instruction alternative that
  // matches (respectively) V0-V7, V0-V15, V0-V31 or a non-FP register.
  static constexpr unsigned int ALLOWS_FPR8 = 1U << 0;
  static constexpr unsigned int ALLOWS_FPR16 = 1U << 1;
  static constexpr unsigned int ALLOWS_FPR32 = 1U << 2;
  static constexpr unsigned int ALLOWS_NONFPR = 1U << 3;
  //
  // Likewise whether the register occurs in an instruction that requires
  // the associated register type.
  static constexpr unsigned int NEEDS_FPR8 = 1U << 4;
  static constexpr unsigned int NEEDS_FPR16 = 1U << 5;
  static constexpr unsigned int NEEDS_FPR32 = 1U << 6;
  static constexpr unsigned int NEEDS_NONFPR = 1U << 7;
  //
  // Whether the pseudo register is copied to or from a hard FP register.
  static constexpr unsigned int HAS_FPR_COPY = 1U << 8;
  //
  // Whether the pseudo register is copied to or from a hard non-FP register.
  static constexpr unsigned int HAS_NONFPR_COPY = 1U << 9;
  //
  // Whether the pseudo register is used as a multi-register vector operand
  // to an instruction that supports strided accesses, and whether it is used
  // as a multi-register vector operand in some other non-move instruction.
  static constexpr unsigned int HAS_FLEXIBLE_STRIDE = 1U << 10;
  static constexpr unsigned int HAS_FIXED_STRIDE = 1U << 11;
  //
  // Whether we have decided not to allocate the pseudo register.
  static constexpr unsigned int IGNORE_REG = 1U << 12;

  // Flags that should be propagated across moves between pseudo registers.
  static constexpr unsigned int PSEUDO_COPY_FLAGS = ~(HAS_FLEXIBLE_STRIDE
						      | HAS_FIXED_STRIDE);

  // Information about a copy between two registers.
  struct reg_copy_info
  {
    // The two registers, in order.
    unsigned int regnos[2];

    // Index I gives the index of the next reg_copy_info involving REGNOS[I],
    // or 0 if none.
    unsigned int next_copies[2];
  };

  // Information about a pseudo register.
  struct pseudo_reg_info
  {
    // Flags describing how the register is used, defined above.
    unsigned int flags : 16;

    // The mode of the pseudo register, cached for convenience.
    machine_mode mode : 16;

    // The index of the first copy, or 0 if none.
    unsigned int first_copy;
  };

  // Information about a group of allocnos that have a fixed offset
  // relative to each other.  The allocnos in each group must be allocated
  // together.
  //
  // Allocnos that can share the same hard register are eventually
  // chained together.  These chains represent edges on a graph of
  // allocnos, such that two allocnos joined by an edge use the same FPR.
  // These chains are formed between individual allocnos rather than
  // whole groups, although the system is required to be self-consistent.
  // Each clique in the graph has at least one "full-width" allocno group
  // that has one allocno for every FPR that needs to be allocated to
  // the clique.
  //
  // One group of allocnos is chosen as the "color representative" of
  // each clique in the graph.  This group will be a full-width group.
  struct allocno_info;
  struct allocno_group_info
  {
    array_slice<unsigned int> chain_heads ();
    array_slice<allocno_info> allocnos ();
    allocno_group_info *color_rep ();
    allocno_info *allocno (unsigned int);

    // The color representative of the containing clique.
    allocno_group_info *m_color_rep;

    // The pseudo register associated with this allocno, or INVALID_REGNUM
    // if none.
    unsigned int regno;

    // The offset of the first allocno (and thus this group) from the start
    // of color_rep.
    unsigned int color_rep_offset : 8;

    // The number of allocnos in the group, and thus the number of FPRs
    // that need to be allocated.
    unsigned int size : 8;

    // The gap between FPRs in the group.  This is normally 1, but can be
    // higher if we've decided to use strided multi-register accesses.
    unsigned int stride : 4;

    // Used temporarily while deciding which allocnos should have non-unit
    // strides; see find_strided_accesses for details.
    int consecutive_pref : 4;
    int strided_polarity : 2;

    // The largest size of FPR needed by references to the allocno group.
    fpr_size_info fpr_size : 2;

    // True if all non-move accesses can be converted to strided form.
    unsigned int has_flexible_stride : 1;

    // True if we've assigned a color index to this group.
    unsigned int has_color : 1;

    // The mask of FPRs that would make valid choices for the first allocno,
    // taking the requirements of all the allocnos in the group into account.
    unsigned int fpr_candidates;

    // The index of the color that has been assigned to the containing clique.
    unsigned int color;
  };

  // Represents a single FPR-sized quantity that needs to be allocated.
  // Each allocno is identified by index (for compactness).
  //
  // Quantities that span multiple FPRs are assigned groups of consecutive
  // allocnos.  Quantities that occupy a single FPR are assigned their own
  // group.
  struct allocno_info
  {
    allocno_group_info *group ();
    bool is_shared ();
    bool is_equiv_to (unsigned int);

    // The allocno's unique identifier.
    unsigned int id;

    // The offset of this allocno into the containing group.
    unsigned int offset : 8;

    // The number of allocnos in the containing group.
    unsigned int group_size : 8;

    // If the allocno has an affinity with at least one hard register
    // (so that choosing that hard register would avoid a copy), this is
    // the number of one such hard register, otherwise it is
    // FIRST_PSEUDO_REGISTER.
    unsigned int hard_regno : 8;

    // Set to 1 if the allocno has a single definition or 2 if it has more.
    unsigned int num_defs : 2;

    // True if, at START_POINT, another allocno is copied to this one.
    // See callers of record_copy for what counts as a copy.
    unsigned int is_copy_dest : 1;

    // True if, at START_POINT, another allocno is copied to this one,
    // and if the allocnos at both ends of the copy chain have an affinity
    // with the same hard register.
    unsigned int is_strong_copy_dest : 1;

    // True if, at END_POINT, this allocno is copied to another one,
    // and both allocnos have an affinity with the same hard register.
    unsigned int is_strong_copy_src : 1;

    // True if the allocno is subject to an earlyclobber at END_POINT,
    // so that it cannot be tied to the destination of the instruction.
    unsigned int is_earlyclobbered : 1;

    // True if this allocno is known to be equivalent to related_allocno
    // for the whole of this allocno's lifetime.
    unsigned int is_equiv : 1;

    // The inclusive range of program points spanned by the allocno.
    // START_POINT >= END_POINT.
    unsigned int start_point;
    unsigned int end_point;

    // If, at END_POINT, this allocno is copied to another allocno, this
    // is the index of that allocno, otherwise it is INVALID_ALLOCNO.
    // See callers of record_copy for what counts as a copy.
    unsigned int copy_dest;

    // If this field is not INVALID_ALLOCNO, it indicates one of two things:
    //
    // - if is_equiv, this allocno is equivalent to related_allocno for
    //   the whole of this allocno's lifetime.
    //
    // - if !is_equiv, this allocno's live range is a subrange of
    //   related_allocno's and we have committed to making this allocno
    //   share whatever register related_allocno uses.
    unsigned int related_allocno;

    union
    {
      // The program point at which the allocno was last defined,
      // or START_OF_REGION if none.  This is only used temporarily
      // while recording allocnos; after that, chain_next below is
      // used instead.
      unsigned int last_def_point;

      // The next chained allocno in program order (i.e. at lower program
      // points), or INVALID_ALLOCNO if none.
      unsigned int chain_next;
    };

    union
    {
      // The program point before start_point at which the allocno was
      // last used, or END_OF_REGION if none.  This is only used temporarily
      // while recording allocnos; after that, chain_prev below is used
      // instead.
      unsigned int last_use_point;

      // The previous chained allocno in program order (i.e. at higher
      // program points), or INVALID_ALLOCNO if none.
      unsigned int chain_prev;
    };
  };

  // Information about a full allocno group or a subgroup of it.
  // The subgroup can be empty to indicate "none".
  struct allocno_subgroup
  {
    array_slice<allocno_info> allocnos ();
    allocno_info *allocno (unsigned int);

    // True if a subgroup is present.
    operator bool () const { return count; }

    // The containing group.
    allocno_group_info *group;

    // The offset of the subgroup from the start of GROUP.
    unsigned int start;

    // The number of allocnos in the subgroup.
    unsigned int count;
  };

  // Represents information about a copy between an allocno and an FPR.
  // This establishes an affinity between the allocno and the FPR.
  struct allocno_copy_info
  {
    // The allocno involved in the copy.
    unsigned int allocno;

    // The FPR involved in the copy, relative to V0_REGNUM.
    unsigned int fpr : 16;

    // A measure of how strong the affinity between the allocno and FPR is.
    unsigned int weight : 16;
  };

  // Information about a possible allocno chain.
  struct chain_candidate_info
  {
    // The candidate target allocno.
    allocno_info *allocno;

    // A rating of the candidate (higher is better).
    int score;
  };

  // Information about an allocno color.
  struct color_info
  {
    // The color's unique identifier.
    int id;

    // The allocated hard register, when known.
    unsigned int hard_regno;

    // The clique's representative group.
    allocno_group_info *group;

    // The number of FPR preferences recorded in fpr_preferences.
    unsigned int num_fpr_preferences;

    // Weights in favor of choosing each FPR as the first register for GROUP.
    int8_t fpr_preferences[32];
  };

  template<typename T, typename... Ts>
  T *region_allocate (Ts...);

  allocno_info *chain_prev (allocno_info *);
  allocno_info *chain_next (allocno_info *);

  void dump_pseudo_regs ();
  void dump_fpr_ranges ();
  void dump_copies ();
  void dump_allocnos ();
  void dump_colors ();

  iterator_range<allocno_iterator> get_group_allocnos (unsigned int);

  void preprocess_move (rtx, rtx);
  void process_pseudo_reg_constraints (rtx_insn *);
  void preprocess_insns ();

  int fpr_preference (unsigned int);
  void propagate_pseudo_reg_info ();

  void choose_fpr_pseudos ();

  void start_new_region ();

  template<typename T>
  void record_live_range_failure (T);
  template<typename T>
  void record_allocation_failure (T);

  allocno_group_info *create_allocno_group (unsigned int, unsigned int);
  allocno_subgroup get_allocno_subgroup (rtx);
  void record_fpr_use (unsigned int);
  void record_fpr_def (unsigned int);
  void record_allocno_use (allocno_info *);
  void record_allocno_def (allocno_info *);
  allocno_info *find_related_start (allocno_info *, allocno_info *, bool);
  void accumulate_defs (allocno_info *, allocno_info *);
  void record_copy (rtx, rtx, bool = false);
  void record_constraints (rtx_insn *);
  void record_artificial_refs (unsigned int);
  void record_insn_defs (rtx_insn *);
  void record_insn_call (rtx_call_insn *);
  void record_insn_uses (rtx_insn *);

  bool consider_strong_copy_src_chain (allocno_info *);
  int strided_polarity_pref (allocno_info *, allocno_info *);
  void find_strided_accesses ();

  template<unsigned int allocno_info::*field>
  static int cmp_increasing (const void *, const void *);
  bool is_chain_candidate (allocno_info *, allocno_info *, test_strictness);
  int rate_chain (allocno_info *, allocno_info *);
  static int cmp_chain_candidates (const void *, const void *);
  void chain_allocnos (unsigned int &, unsigned int &);
  void merge_fpr_info (allocno_group_info *, allocno_group_info *,
		       unsigned int);
  void set_single_color_rep (allocno_info *, allocno_group_info *,
			     unsigned int);
  void set_color_rep (allocno_group_info *, allocno_group_info *,
		      unsigned int);
  bool try_to_chain_allocnos (allocno_info *, allocno_info *);
  void create_color (allocno_group_info *);
  void form_chains ();

  bool fpr_conflicts_with_allocno_p (unsigned int, allocno_info *);
  bool call_in_range_p (unsigned int, unsigned int, unsigned int);
  unsigned int partial_fpr_clobbers (unsigned int, fpr_size_info);

  void process_copies ();

  static int cmp_allocation_order (const void *, const void *);
  void allocate_colors ();
  allocno_info *find_independent_subchain (allocno_info *);
  color_info *find_oldest_color (unsigned int, unsigned int);
  void broaden_colors ();
  void finalize_allocation ();

  bool replace_regs (rtx_insn *, df_ref);
  int try_enforce_constraints (rtx_insn *, vec<std::pair<int, int>> &);
  void enforce_constraints (rtx_insn *);
  bool maybe_convert_to_strided_access (rtx_insn *);
  void apply_allocation ();

  void process_region ();
  bool is_dead_insn (rtx_insn *);
  bool could_split_region_here ();
  void process_block (basic_block, bool);
  void process_blocks ();

  // ----------------------------------------------------------------------

  // The function we're operating on.
  function *m_fn;

  // Information about each pseudo register, indexed by REGNO.
  auto_vec<pseudo_reg_info> m_pseudo_regs;

  // All recorded register copies.
  auto_vec<reg_copy_info> m_pseudo_reg_copies;

  // The set of pseudos that we've decided to allocate an FPR to.
  auto_bitmap m_fpr_pseudos;

  // ----------------------------------------------------------------------

  // An obstack for allocating information that is referenced by the member
  // variables below.
  obstack m_region_obstack;
  void *m_region_alloc_start;

  // ----------------------------------------------------------------------

  // The basic block that we're currently processing.
  basic_block m_current_bb;

  // The lowest-numbered program point in the current basic block.
  unsigned int m_current_bb_point;

  // The program point that we're currently processing (described above).
  unsigned int m_current_point;

  // The set of allocnos that are currently live.
  auto_bitmap m_live_allocnos;

  // The set of FPRs that are currently live.
  unsigned int m_live_fprs;

  // A unique one-based identifier for the current region.
  unsigned int m_current_region;

  // The region in which each FPR was last used, or 0 if none.
  unsigned int m_fpr_recency[32];

  // ----------------------------------------------------------------------

  // A mask of the FPRs that have already been allocated.
  unsigned int m_allocated_fprs;

  // A mask of the FPRs that must be at least partially preserved by the
  // current function.
  unsigned int m_call_preserved_fprs;

  // True if we have so far been able to track the live ranges of individual
  // allocnos.
  bool m_accurate_live_ranges;

  // True if we haven't yet failed to allocate the current region.
  bool m_allocation_successful;

  // A map from pseudo registers to the first allocno in their associated
  // allocno groups.
  hash_map<int_hash<unsigned int, INVALID_REGNUM>,
	   allocno_group_info *> m_regno_to_group;

  // All recorded copies between allocnos and FPRs.
  auto_vec<allocno_copy_info> m_allocno_copies;

  // All allocnos, by index.
  auto_vec<allocno_info *> m_allocnos;

  // All allocnos, by increasing START_POINT.
  auto_vec<allocno_info *> m_sorted_allocnos;

  // Allocnos for which is_shared is true.
  auto_vec<allocno_info *> m_shared_allocnos;

  // All colors, by index.
  auto_vec<color_info *> m_colors;

  // The instruction ranges that make up the current region,
  // as half-open ranges [LAST, FIRST).
  auto_vec<std::pair<rtx_insn *, rtx_insn *>> m_insn_ranges;

  // The live ranges of each FPR, in order of increasing program point.
  auto_vec<fpr_range_info> m_fpr_ranges[32];

  // For each function call id, a list of program points at which a call
  // to such a function is made.  Each list is in order of increasing
  // program point.
  auto_vec<unsigned int> m_call_points[NUM_ABI_IDS];

  // A list of instructions that can be removed if allocation succeeds.
  auto_vec<rtx_insn *> m_dead_insns;
};

// True if PAT is something that would typically be treated as a move.
static inline bool
is_move_set (rtx pat)
{
  if (GET_CODE (pat) != SET)
    return false;

  rtx dest = SET_DEST (pat);
  if (SUBREG_P (dest))
    dest = SUBREG_REG (dest);
  if (!OBJECT_P (dest))
    return false;

  rtx src = SET_SRC (pat);
  if (SUBREG_P (src))
    src = SUBREG_REG (src);
  if (!OBJECT_P (src) && !CONSTANT_P (src))
    return false;

  return true;
}

// Return true if operand OP is likely to match OP_ALT after register
// allocation.
static bool
likely_operand_match_p (const operand_alternative &op_alt, rtx op)
{
  // Empty constraints match everything.
  const char *constraint = op_alt.constraint;
  if (constraint[0] == 0 || constraint[0] == ',')
    return true;

  for (;;)
    {
      char c = *constraint;
      int len = CONSTRAINT_LEN (c, constraint);
      if (c == 0 || c == ',')
	break;

      if (c == 'X')
	return true;

      auto cn = lookup_constraint (constraint);
      switch (get_constraint_type (cn))
	{
	case CT_REGISTER:
	  if (REG_P (op) || SUBREG_P (op))
	    return true;
	  break;

	case CT_MEMORY:
	case CT_SPECIAL_MEMORY:
	case CT_RELAXED_MEMORY:
	  if (MEM_P (op))
	    return true;
	  break;

	case CT_CONST_INT:
	case CT_ADDRESS:
	case CT_FIXED_FORM:
	  if (constraint_satisfied_p (op, cn))
	    return true;
	  break;
	}

      constraint += len;
    }

  if (op_alt.matches >= 0)
    {
      rtx other = recog_data.operand[op_alt.matches];
      if ((REG_P (other) || SUBREG_P (other))
	  && (REG_P (op) || SUBREG_P (op)))
	return true;
    }
  return false;
}

// Return true if the operands of the current instruction are likely to
// match OP_ALT.
static bool
likely_alternative_match_p (const operand_alternative *op_alt)
{
  for (int i = 0; i < recog_data.n_operands; ++i)
    if (!likely_operand_match_p (op_alt[i], recog_data.operand[i]))
      return false;
  return true;
}

// Return the sum of how disparaged OP_ALT is.
static int
count_rejects (const operand_alternative *op_alt)
{
  int reject = 0;
  for (int opno = 0; opno < recog_data.n_operands; ++opno)
    reject += op_alt[opno].reject;
  return reject;
}

// Allocate a T from the region obstack.
template<typename T, typename... Ts>
inline T *
early_ra::region_allocate (Ts... args)
{
  static_assert (std::is_trivially_destructible<T>::value,
		 "destructor won't be called");
  void *addr = obstack_alloc (&m_region_obstack, sizeof (T));
  return new (addr) T (std::forward<Ts> (args)...);
}

early_ra::early_ra (function *fn) : m_fn (fn), m_live_fprs (0)
{
  gcc_obstack_init (&m_region_obstack);
  m_region_alloc_start = obstack_alloc (&m_region_obstack, 0);
  bitmap_tree_view (m_live_allocnos);
}

early_ra::~early_ra ()
{
  obstack_free (&m_region_obstack, nullptr);
}

// Return an array that, for each allocno A in the group, contains the index
// of the allocno at the head of A's chain (that is, the one with the highest
// START_POINT).  The index is INVALID_ALLOCNO if the chain is empty.
inline array_slice<unsigned int>
early_ra::allocno_group_info::chain_heads ()
{
  auto *start = reinterpret_cast<unsigned int *> (this + 1);
  return { start, size };
}

// Return the array of allocnos in the group.
inline array_slice<early_ra::allocno_info>
early_ra::allocno_group_info::allocnos ()
{
  gcc_checking_assert (regno != INVALID_REGNUM);
  auto *chain_end = reinterpret_cast<unsigned int *> (this + 1) + size;
  auto *allocno_start = reinterpret_cast<allocno_info *> (chain_end);
  return { allocno_start, size };
}

// Return the group's color representative.
inline early_ra::allocno_group_info *
early_ra::allocno_group_info::color_rep ()
{
  gcc_checking_assert (m_color_rep->m_color_rep == m_color_rep);
  return m_color_rep;
}

// Return the group that contains the allocno.
inline early_ra::allocno_group_info *
early_ra::allocno_info::group ()
{
  auto *chain_end = reinterpret_cast<unsigned int *> (this - offset);
  return reinterpret_cast<allocno_group_info *> (chain_end - group_size) - 1;
}

// Return true if this allocno's live range is a subrange of related_allocno's
// and if we have committed to making this allocno share whatever register
// related_allocno uses.
inline bool
early_ra::allocno_info::is_shared ()
{
  return related_allocno != INVALID_ALLOCNO && !is_equiv;
}

// Return true if this allocno is known to be equivalent to ALLOCNO.
inline bool
early_ra::allocno_info::is_equiv_to (unsigned int allocno)
{
  return is_equiv && related_allocno == allocno;
}

// Return the allocnos in the subgroup.
inline array_slice<early_ra::allocno_info>
early_ra::allocno_subgroup::allocnos ()
{
  if (!count)
    return {};
  return { &group->allocnos ()[start], count };
}

// Return allocno I in the subgroup, with 0 being the first.
inline early_ra::allocno_info *
early_ra::allocno_subgroup::allocno (unsigned int i)
{
  return &group->allocnos ()[start + i];
}

// Return the previous (earlier) allocno in ALLOCNO's chain, or null if none.
inline early_ra::allocno_info *
early_ra::chain_prev (allocno_info *allocno)
{
  if (allocno->chain_prev != INVALID_ALLOCNO)
    return m_allocnos[allocno->chain_prev];
  return nullptr;
}

// Return the next (later) allocno in ALLOCNO's chain, or null if none.
inline early_ra::allocno_info *
early_ra::chain_next (allocno_info *allocno)
{
  if (allocno->chain_next != INVALID_ALLOCNO)
    return m_allocnos[allocno->chain_next];
  return nullptr;
}

// Dump the information in m_pseudo_regs.
void
early_ra::dump_pseudo_regs ()
{
  fprintf (dump_file, "\nPseudos:\n");
  fprintf (dump_file, "  %6s %6s %6s %6s %6s %6s %8s %s\n",
	   "Id", "FPR8", "FPR16", "FPR32", "NONFPR", "Stride",
	   "FPRness", "Copies");
  pseudo_reg_info unused_reg = {};
  for (unsigned int regno = FIRST_PSEUDO_REGISTER;
       regno < m_pseudo_regs.length (); ++regno)
    {
      const auto &reg = m_pseudo_regs[regno];
      if (memcmp (&reg, &unused_reg, sizeof (reg)) == 0)
	continue;

      fprintf (dump_file, "  %6d %6s %6s %6s %6s %6s %8d", regno,
	       reg.flags & NEEDS_FPR8 ? "Req"
	       : reg.flags & ALLOWS_FPR8 ? "OK" : "-",
	       reg.flags & NEEDS_FPR16 ? "Req"
	       : reg.flags & ALLOWS_FPR16 ? "OK" : "-",
	       reg.flags & NEEDS_FPR32 ? "Req"
	       : reg.flags & ALLOWS_FPR32 ? "OK" : "-",
	       reg.flags & NEEDS_NONFPR ? "Req"
	       : reg.flags & ALLOWS_NONFPR ? "OK" : "-",
	       ~reg.flags & HAS_FLEXIBLE_STRIDE ? "-"
	       : reg.flags & HAS_FIXED_STRIDE ? "Some" : "All",
	       fpr_preference (regno));
      if (reg.flags & HAS_FPR_COPY)
	fprintf (dump_file, " FPR");
      if (reg.flags & HAS_NONFPR_COPY)
	fprintf (dump_file, " Non-FPR");
      unsigned int copyi = reg.first_copy;
      while (copyi)
	{
	  const auto &copy = m_pseudo_reg_copies[copyi];
	  if (copy.regnos[0] == regno)
	    {
	      fprintf (dump_file, " r%d", copy.regnos[1]);
	      copyi = copy.next_copies[0];
	    }
	  else
	    {
	      fprintf (dump_file, " r%d", copy.regnos[0]);
	      copyi = copy.next_copies[1];
	    }
	}
      fprintf (dump_file, "\n");
    }
}

// Dump the information in m_fpr_ranges.
void
early_ra::dump_fpr_ranges ()
{
  fprintf (dump_file, "\nFPR live ranges:\n");
  for (unsigned int fpr = 0; fpr < 32; ++fpr)
    {
      auto &intervals = m_fpr_ranges[fpr];
      if (intervals.is_empty ())
	continue;

      fprintf (dump_file, "  %2d", fpr);
      for (unsigned int i = 0; i < intervals.length (); ++i)
	{
	  auto &interval = intervals[i];
	  if (i && (i % 4) == 0)
	    fprintf (dump_file, "\n    ");
	  fprintf (dump_file, " [ %6d %6d ]", interval.start_point,
		   interval.end_point);
	}
      fprintf (dump_file, "\n");
    }
}

// Dump the information in m_allocno_copies.
void
early_ra::dump_copies ()
{
  fprintf (dump_file, "\nCopies:\n");
  fprintf (dump_file, "  %8s %3s %6s\n",
	   "Allocno", "FPR", "Weight");
  for (const auto &copy : m_allocno_copies)
    fprintf (dump_file, "  %8d %3d %6d\n", copy.allocno,
	     copy.fpr, copy.weight);
}

// Dump the information in m_allocnos.
void
early_ra::dump_allocnos ()
{
  char buffer[sizeof ("r[:]") + 3 * 3 * sizeof (int) + 1];
  fprintf (dump_file, "\nAllocno groups:\n");
  fprintf (dump_file,
	   "  %12s %12s %4s %6s %8s %s\n",
	   "Ids", "Regno", "Size", "Stride", "Cands", "Heads");
  for (unsigned int ai = 0; ai < m_allocnos.length (); ++ai)
    {
      auto *allocno = m_allocnos[ai];
      if (allocno->offset != 0)
	continue;
      auto *group = allocno->group ();
      snprintf (buffer, sizeof (buffer), "[%d:%d]", allocno->id,
		allocno->id + group->size - 1);
      fprintf (dump_file, "  %12s", buffer);
      snprintf (buffer, sizeof (buffer), "r%d[0:%d]", group->regno,
		group->size - 1);
      fprintf (dump_file, " %12s %4s %6d %08x", buffer,
	       group->fpr_size == FPR_D ? "D"
	       : group->fpr_size == FPR_Q ? "Q" : "Z",
	       group->stride,
	       group->fpr_candidates);
      for (auto head : group->chain_heads ())
	if (head == INVALID_ALLOCNO)
	  fprintf (dump_file, " -");
	else
	  fprintf (dump_file, " %d", head);
      fprintf (dump_file, "\n");
    }

  fprintf (dump_file, "\nAllocno chains:\n");
  fprintf (dump_file, "      %5s %12s %12s %6s %5s %5s %6s %5s\n",
	   "Id", "Regno", "Range ", "Src", "Dest", "Equiv", "Shared", "FPR");
  for (unsigned int ai = 0; ai < m_allocnos.length (); ++ai)
    {
      auto *allocno = m_allocnos[ai];
      if (allocno->chain_prev != INVALID_ALLOCNO)
	continue;
      const char *prefix = "=>";
      for (;;)
	{
	  auto *group = allocno->group ();
	  fprintf (dump_file, "  %2s", prefix);
	  fprintf (dump_file, "  %5d", allocno->id);
	  snprintf (buffer, sizeof (buffer), "r%d[%d]", group->regno,
		    allocno->offset);
	  fprintf (dump_file, " %12s", buffer);
	  snprintf (buffer, sizeof (buffer), "[%d,%d]",
		    allocno->start_point, allocno->end_point);
	  fprintf (dump_file, " %11s%s %6s", buffer,
		   allocno->is_earlyclobbered ? "*" : " ",
		   allocno->is_strong_copy_dest ? "Strong"
		   : allocno->is_copy_dest ? "Yes" : "-");
	  if (allocno->copy_dest == INVALID_ALLOCNO)
	    fprintf (dump_file, " %5s", "-");
	  else
	    fprintf (dump_file, " %5d", allocno->copy_dest);
	  if (allocno->is_equiv)
	    fprintf (dump_file, " %5d", allocno->related_allocno);
	  else
	    fprintf (dump_file, " %5s", "-");
	  if (allocno->is_shared ())
	    fprintf (dump_file, " %6d", allocno->related_allocno);
	  else
	    fprintf (dump_file, " %6s", "-");
	  if (allocno->hard_regno == FIRST_PSEUDO_REGISTER)
	    fprintf (dump_file, " %5s", "-");
	  else
	    fprintf (dump_file, " %5s", reg_names[allocno->hard_regno]);
	  fprintf (dump_file, "\n");
	  if (allocno->chain_next == INVALID_ALLOCNO)
	    break;
	  allocno = m_allocnos[allocno->chain_next];
	  prefix = "";
	}
    }
}

// Dump the information in m_colors.
void
early_ra::dump_colors ()
{
  fprintf (dump_file, "\nColors:\n");
  for (unsigned int i = 0; i < m_colors.length (); ++i)
    {
      auto *color = m_colors[i];
      if (!color->group)
	continue;

      fprintf (dump_file, "  color %d:\n", i);
      fprintf (dump_file, "    chains:\n");
      auto heads = color->group->chain_heads ();
      for (unsigned int i = 0; i < color->group->size; ++i)
	{
	  fprintf (dump_file, "      %2d:", i);
	  auto ai = heads[i];
	  while (ai != INVALID_ALLOCNO)
	    {
	      auto *allocno = m_allocnos[ai];
	      fprintf (dump_file, " r%d[%d]", allocno->group ()->regno,
		       allocno->offset);
	      ai = allocno->chain_next;
	    }
	  fprintf (dump_file, "\n");
	}
      fprintf (dump_file, "    FPR candidates:");
      for (unsigned int fpr = 0; fpr < 32; ++fpr)
	fprintf (dump_file, "%s%c", fpr % 8 ? "" : " ",
		 color->group->fpr_candidates & (1U << fpr) ? 'Y' : '-');
      fprintf (dump_file, "\n");
      fprintf (dump_file, "    FPR preferences:");
      for (unsigned int fpr = 0; fpr < 32; ++fpr)
	if (color->fpr_preferences[fpr])
	  fprintf (dump_file, " %d(%d)", fpr, color->fpr_preferences[fpr]);
      fprintf (dump_file, "\n");
    }
}

// Record any necessary information about a move from SRC to DEST.
void
early_ra::preprocess_move (rtx dest, rtx src)
{
  if (SUBREG_P (dest))
    dest = SUBREG_REG (dest);
  if (!REG_P (dest))
    return;

  if (SUBREG_P (src))
    src = SUBREG_REG (src);
  if (!REG_P (src))
    return;

  // Sort the registers by increasing REGNO.
  rtx regs[] = { dest, src };
  if (REGNO (dest) > REGNO (src))
    std::swap (regs[0], regs[1]);
  unsigned int regno0 = REGNO (regs[0]);
  unsigned int regno1 = REGNO (regs[1]);

  // Ignore moves between hard registers.
  if (HARD_REGISTER_NUM_P (regno1))
    return;

  // For moves between hard registers and pseudos, just record the type
  // of hard register involved.
  auto &reg1 = m_pseudo_regs[regno1];
  reg1.mode = GET_MODE (regs[1]);
  if (HARD_REGISTER_NUM_P (regno0))
    {
      reg1.flags |= (FP_REGNUM_P (regno0) ? HAS_FPR_COPY : HAS_NONFPR_COPY);
      return;
    }

  // Record a move between two pseudo registers.
  auto &reg0 = m_pseudo_regs[regno0];
  reg0.mode = GET_MODE (regs[0]);

  reg_copy_info copy;
  copy.regnos[0] = regno0;
  copy.regnos[1] = regno1;
  copy.next_copies[0] = reg0.first_copy;
  copy.next_copies[1] = reg1.first_copy;

  reg0.first_copy = reg1.first_copy = m_pseudo_reg_copies.length ();
  m_pseudo_reg_copies.safe_push (copy);
}

// Return true if INSN has a multi-vector operand and if that operand
// could be converted to strided form.
static bool
is_stride_candidate (rtx_insn *insn)
{
  if (recog_memoized (insn) < 0)
    return false;

  auto stride_type = get_attr_stride_type (insn);
  return (TARGET_STREAMING_SME2
	  && (stride_type == STRIDE_TYPE_LD1_CONSECUTIVE
	      || stride_type == STRIDE_TYPE_ST1_CONSECUTIVE));
}

// Go through the constraints of INSN, which has already been extracted,
// and record any relevant information about pseudo registers.
void
early_ra::process_pseudo_reg_constraints (rtx_insn *insn)
{
  extract_insn (insn);
  preprocess_constraints (insn);

  // Flags that describe any multi-register vector operands.
  unsigned int insn_flags = (is_stride_candidate (insn)
			     ? HAS_FLEXIBLE_STRIDE
			     : HAS_FIXED_STRIDE);

  auto alts = get_preferred_alternatives (insn);

  int operand_matches[MAX_RECOG_OPERANDS];
  unsigned int operand_flags[MAX_RECOG_OPERANDS];
  for (int i = 0; i < recog_data.n_operands; ++i)
    {
      operand_matches[i] = -1;
      operand_flags[i] = 0;
    }

  // Extract information from the constraints, considering all plausible
  // alternatives.
  for (int altno = 0; altno < recog_data.n_alternatives; ++altno)
    {
      if (!(alts & ALTERNATIVE_BIT (altno)))
	continue;

      auto *op_alt = &recog_op_alt[altno * recog_data.n_operands];
      if (!likely_alternative_match_p (op_alt))
	continue;

      // Use SRC_OPNO's constraints to derive information about DEST_OPNO.
      auto record_operand = [&](int src_opno, int dest_opno)
	{
	  int matches = op_alt[src_opno].matches;
	  if (matches >= 0)
	    operand_matches[dest_opno] = matches;

	  auto cl = alternative_class (op_alt, src_opno);
	  if (cl != NO_REGS)
	    {
	      if (reg_class_subset_p (cl, FP_REGS))
		operand_flags[dest_opno] |= ALLOWS_FPR32;
	      if (reg_class_subset_p (cl, FP_LO_REGS))
		operand_flags[dest_opno] |= ALLOWS_FPR16;
	      if (reg_class_subset_p (cl, FP_LO8_REGS))
		operand_flags[dest_opno] |= ALLOWS_FPR8;
	      if (!reg_classes_intersect_p (cl, FP_REGS))
		operand_flags[dest_opno] |= ALLOWS_NONFPR;
	    }
	};

      for (int i = 0; i < recog_data.n_operands; ++i)
	{
	  record_operand (i, i);
	  if (recog_data.constraints[i][0] == '%')
	    {
	      record_operand (i, i + 1);
	      record_operand (i + 1, i);
	    }
	}
    }

  // Process the information we collected above.
  for (int i = 0; i < recog_data.n_operands; ++i)
    {
      rtx op = recog_data.operand[i];
      machine_mode orig_mode = GET_MODE (op);
      if (SUBREG_P (op))
	op = SUBREG_REG (op);

      // Record the accumulated information in m_pseudo_regs.
      if (REG_P (op) && !HARD_REGISTER_P (op))
	{
	  // The flags so far just describe what at least one alternative
	  // would accept.  Calculate the associated NEEDS_* information.
	  auto flags = operand_flags[i];
	  if (!(flags & ALLOWS_FPR32) && (flags & ALLOWS_NONFPR))
	    flags |= NEEDS_NONFPR;
	  else if ((flags & ALLOWS_FPR32) && !(flags & ALLOWS_NONFPR))
	    {
	      if (flags & ALLOWS_FPR8)
		flags |= NEEDS_FPR8;
	      if (flags & ALLOWS_FPR16)
		flags |= NEEDS_FPR16;
	      flags |= NEEDS_FPR32;
	    }

	  // Look for multi-register vector operands.
	  if (VECTOR_MODE_P (orig_mode)
	      && targetm.hard_regno_mode_ok (V0_REGNUM, orig_mode)
	      && hard_regno_nregs (V0_REGNUM, orig_mode) > 1)
	    flags |= insn_flags;

	  m_pseudo_regs[REGNO (op)].flags |= flags;
	  m_pseudo_regs[REGNO (op)].mode = GET_MODE (op);
	}

      // Treat matching constraints as equivalent to moves.
      if (operand_matches[i] >= 0)
	preprocess_move (recog_data.operand[operand_matches[i]], op);
    }
}

// Make one pass through the instructions, collecting information that
// will be needed later.
void
early_ra::preprocess_insns ()
{
  m_pseudo_regs.safe_grow_cleared (max_reg_num ());
  m_pseudo_reg_copies.safe_push (reg_copy_info ());
  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      // Mark all registers that occur in addresses as needing a GPR.
      vec_rtx_properties properties;
      properties.add_insn (insn, true);
      for (rtx_obj_reference ref : properties.refs ())
	if (ref.is_reg ()
	    && ref.in_address ()
	    && !HARD_REGISTER_NUM_P (ref.regno))
	  m_pseudo_regs[ref.regno].flags |= ALLOWS_NONFPR | NEEDS_NONFPR;

      if (GET_CODE (PATTERN (insn)) == USE
	  || GET_CODE (PATTERN (insn)) == CLOBBER)
	continue;

      rtx set = single_set (insn);
      if (set && is_move_set (set))
	preprocess_move (SET_DEST (set), SET_SRC (set));
      else
	process_pseudo_reg_constraints (insn);
    }
}

// Return a signed integer that says (roughly) how strong an affinity
// pseudo register REGNO has with FPRs.  A positive value indicates
// that we should try to allocate an FPR, a negative value indicates
// that we shouldn't, and 0 indicates neutrality.
int
early_ra::fpr_preference (unsigned int regno)
{
  auto mode = m_pseudo_regs[regno].mode;
  auto flags = m_pseudo_regs[regno].flags;
  if (flags & IGNORE_REG)
    return -4;
  else if (mode == VOIDmode || !targetm.hard_regno_mode_ok (V0_REGNUM, mode))
    return -3;
  else if (flags & HAS_FLEXIBLE_STRIDE)
    return 3;
  else if (flags & NEEDS_FPR32)
    return 2;
  else if (!(flags & ALLOWS_FPR32) && (flags & ALLOWS_NONFPR))
    return -2;
  else if ((flags & HAS_FPR_COPY) && !(flags & HAS_NONFPR_COPY))
    return 1;
  else if ((flags & HAS_NONFPR_COPY) && !(flags & HAS_FPR_COPY))
    return -1;
  else
    return 0;
}

// Propagate information about pseudo-registers along copy edges,
// while doing so doesn't create conflicting FPR preferences.
void
early_ra::propagate_pseudo_reg_info ()
{
  struct stack_entry { unsigned int regno, copyi; };

  auto_vec<stack_entry, 32> stack;
  for (unsigned int i = FIRST_PSEUDO_REGISTER;
       i < m_pseudo_regs.length (); ++i)
    {
      auto start = m_pseudo_regs[i].first_copy;
      if (!start)
	continue;

      stack.quick_push ({ i, start });
      while (!stack.is_empty ())
	{
	  auto entry = stack.pop ();
	  auto &copy = m_pseudo_reg_copies[entry.copyi];
	  auto src_regno = entry.regno;
	  auto dest_regno = (src_regno == copy.regnos[1]
			     ? copy.regnos[0]
			     : copy.regnos[1]);
	  auto next_copyi = (src_regno == copy.regnos[1]
			     ? copy.next_copies[1]
			     : copy.next_copies[0]);
	  if (next_copyi)
	    stack.safe_push ({ src_regno, next_copyi });

	  auto &src_reg = m_pseudo_regs[src_regno];
	  auto &dest_reg = m_pseudo_regs[dest_regno];

	  if (src_reg.flags & ~dest_reg.flags & PSEUDO_COPY_FLAGS)
	    {
	      auto src_preference = fpr_preference (src_regno);
	      auto dest_preference = fpr_preference (dest_regno);
	      if ((src_preference >= 0 && dest_preference >= 0)
		  || (src_preference <= 0 && dest_preference <= 0))
		{
		  dest_reg.flags |= (src_reg.flags & PSEUDO_COPY_FLAGS);
		  stack.safe_push ({ dest_regno, dest_reg.first_copy });
		}
	    }
	}
    }
}

// Decide which pseudos should be allocated an FPR, setting m_fpr_pseudos
// accordingly.
void
early_ra::choose_fpr_pseudos ()
{
  for (unsigned int i = FIRST_PSEUDO_REGISTER;
       i < m_pseudo_regs.length (); ++i)
    if (fpr_preference (i) > 0)
      bitmap_set_bit (m_fpr_pseudos, i);
}

// Clear out information about the previous CFG region (if any)
// and set up the data for a new region.
void
early_ra::start_new_region ()
{
  obstack_free (&m_region_obstack, m_region_alloc_start);
  m_regno_to_group.empty ();
  m_allocno_copies.truncate (0);
  m_allocnos.truncate (0);
  m_sorted_allocnos.truncate (0);
  m_shared_allocnos.truncate (0);
  m_colors.truncate (0);
  m_insn_ranges.truncate (0);
  for (auto &fpr_ranges : m_fpr_ranges)
    fpr_ranges.truncate (0);
  for (auto &call_points : m_call_points)
    call_points.truncate (0);
  gcc_assert (bitmap_empty_p (m_live_allocnos) && m_live_fprs == 0);
  m_dead_insns.truncate (0);
  m_allocated_fprs = 0;
  m_call_preserved_fprs = 0;
  m_accurate_live_ranges = true;
  m_allocation_successful = true;
  m_current_region += 1;
}

// Record that we can no longer track the liveness of individual allocnos.
// Call DUMP to dump the reason to a dump file.
template<typename T>
void
early_ra::record_live_range_failure (T dump)
{
  if (!m_accurate_live_ranges)
    return;

  m_accurate_live_ranges = false;
  m_allocation_successful = false;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Unable to track live ranges further: ");
      dump ();
      fprintf (dump_file, "\n");
    }
}

// Record that the allocation of the current region has filed.  Call DUMP to
// dump the reason to a dump file.
template<typename T>
void
early_ra::record_allocation_failure (T dump)
{
  if (!m_allocation_successful)
    return;

  m_allocation_successful = false;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Not allocating region: ");
      dump ();
      fprintf (dump_file, "\n");
    }
}

// Create and return an allocno group of size SIZE for register REGNO.
// REGNO can be INVALID_REGNUM if the group just exists to allow
// other groups to be chained together, and does not have any new
// allocnos of its own.
early_ra::allocno_group_info *
early_ra::create_allocno_group (unsigned int regno, unsigned int size)
{
  static_assert (alignof (unsigned int) == alignof (allocno_info),
		 "allocno_info alignment");
  unsigned int num_allocnos = (regno != INVALID_REGNUM ? size : 0);

  // Allocate an allocno_group_info, followed by an array of chain heads,
  // followed by the allocnos themselves.
  size_t alloc_size = (sizeof (allocno_group_info)
		       + size * sizeof (unsigned int)
		       + num_allocnos * sizeof (allocno_info));
  void *data = obstack_alloc (&m_region_obstack, alloc_size);

  // Initialize the group.
  auto *group = reinterpret_cast<allocno_group_info *> (data);
  memset (group, 0, sizeof (*group));
  group->m_color_rep = group;
  group->regno = regno;
  group->size = size;
  group->stride = 1;
  group->fpr_size = FPR_D;
  group->fpr_candidates = ~0U;

  // Initialize the chain heads.
  auto heads = group->chain_heads ();
  for (unsigned int i = 0; i < heads.size (); ++i)
    heads[i] = (i < num_allocnos ? m_allocnos.length () + i : INVALID_ALLOCNO);

  // Initialize the allocnos.
  if (num_allocnos > 0)
    {
      auto allocnos = group->allocnos ();
      memset (allocnos.begin (), 0, num_allocnos * sizeof (allocno_info));
      for (unsigned int i = 0; i < num_allocnos; ++i)
	{
	  auto *allocno = &allocnos[i];
	  allocno->id = m_allocnos.length ();
	  allocno->offset = i;
	  allocno->group_size = size;
	  allocno->hard_regno = FIRST_PSEUDO_REGISTER;
	  allocno->start_point = END_OF_REGION;
	  allocno->end_point = START_OF_REGION;
	  allocno->copy_dest = INVALID_ALLOCNO;
	  allocno->related_allocno = INVALID_ALLOCNO;
	  allocno->chain_next = INVALID_ALLOCNO;
	  allocno->chain_prev = INVALID_ALLOCNO;
	  m_allocnos.safe_push (allocno);
	}
    }
  return group;
}

// If REG refers to a pseudo register that might be allocated to FPRs,
// return the associated range of allocnos, creating new ones if necessary.
// Return an empty range otherwise.
early_ra::allocno_subgroup
early_ra::get_allocno_subgroup (rtx reg)
{
  if (GET_CODE (reg) == SUBREG)
    {
      allocno_subgroup inner = get_allocno_subgroup (SUBREG_REG (reg));
      if (!inner)
	return {};

      if (!targetm.can_change_mode_class (GET_MODE (SUBREG_REG (reg)),
					  GET_MODE (reg), FP_REGS))
	{
	  record_live_range_failure ([&](){
	    fprintf (dump_file, "cannot refer to r%d:%s in mode %s",
		     REGNO (SUBREG_REG (reg)),
		     GET_MODE_NAME (GET_MODE (SUBREG_REG (reg))),
		     GET_MODE_NAME (GET_MODE (reg)));
	  });
	  return {};
	}

      if (!targetm.modes_tieable_p (GET_MODE (SUBREG_REG (reg)),
				    GET_MODE (reg)))
	record_allocation_failure ([&](){
	    fprintf (dump_file, "r%d's mode %s is not tieable to mode %s",
		     REGNO (SUBREG_REG (reg)),
		     GET_MODE_NAME (GET_MODE (SUBREG_REG (reg))),
		     GET_MODE_NAME (GET_MODE (reg)));
	});

      subreg_info info;
      subreg_get_info (V0_REGNUM, GET_MODE (SUBREG_REG (reg)),
		       SUBREG_BYTE (reg), GET_MODE (reg), &info);
      if (!info.representable_p)
	{
	  record_live_range_failure ([&](){
	    fprintf (dump_file, "subreg of r%d is invalid for V0",
		     REGNO (SUBREG_REG (reg)));
	  });
	  return {};
	}

      inner.start += info.offset;
      inner.count = info.nregs;
      return inner;
    }

  if (!REG_P (reg) || HARD_REGISTER_P (reg))
    return {};

  unsigned int regno = REGNO (reg);
  if (fpr_preference (regno) <= 0)
    return {};

  unsigned int count = hard_regno_nregs (V0_REGNUM, GET_MODE (reg));
  bool existed;
  auto &entry = m_regno_to_group.get_or_insert (regno, &existed);
  if (!existed)
    {
      auto *group = create_allocno_group (regno, count);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  auto allocnos = group->allocnos ();
	  fprintf (dump_file, "Creating allocnos [%d:%d] for r%d\n",
		   allocnos.front ().id, allocnos.back ().id, regno);
	}

      auto reg_bits = GET_MODE_BITSIZE (GET_MODE (reg));
      auto fpr_bits = exact_div (reg_bits, count);
      auto flags = m_pseudo_regs[regno].flags;

      // Punt for now if there is a choice to be made between using an
      // FPR and a non-FPR.
      if ((flags & NEEDS_NONFPR)
	  || ((flags & ALLOWS_NONFPR)
	      && !FLOAT_MODE_P (GET_MODE (reg))
	      && !VECTOR_MODE_P (GET_MODE (reg))))
	record_allocation_failure ([&](){
	  fprintf (dump_file, "r%d has FPR and non-FPR references", regno);
	});

      if (flags & ALLOWS_FPR8)
	group->fpr_candidates &= 0xff;
      else if (flags & ALLOWS_FPR16)
	group->fpr_candidates &= 0xffff;
      group->fpr_candidates &= ~0U >> (count - 1);

      group->has_flexible_stride = ((flags & HAS_FLEXIBLE_STRIDE) != 0
				    && (flags & HAS_FIXED_STRIDE) == 0);

      group->fpr_size = (maybe_gt (fpr_bits, 128) ? FPR_Z
			 : maybe_gt (fpr_bits, 64) ? FPR_Q : FPR_D);

      entry = group;
    }
  return { entry, 0, count };
}

// Record a use of FPR REGNO at the current program point, as part of
// a backwards walk over a block.
void
early_ra::record_fpr_use (unsigned int regno)
{
  gcc_assert (IN_RANGE (regno, V0_REGNUM, V31_REGNUM));
  unsigned int offset = regno - V0_REGNUM;
  if (!(m_live_fprs & (1U << offset)))
    {
      m_fpr_ranges[offset].safe_push ({ START_OF_REGION, m_current_point,
					INVALID_ALLOCNO });
      m_live_fprs |= 1U << offset;
    }
}

// Record a definition of FPR REGNO at the current program point, as part of
// a backwards walk over a block.
void
early_ra::record_fpr_def (unsigned int regno)
{
  gcc_assert (IN_RANGE (regno, V0_REGNUM, V31_REGNUM));
  unsigned int offset = regno - V0_REGNUM;

  // The definition completes the current live range.  If the result
  // of the definition is used, the live range extends to the last use.
  // Otherwise the live range is just a momentary blip at the current point.
  auto &ranges = m_fpr_ranges[offset];
  if (m_live_fprs & (1U << offset))
    {
      ranges.last ().start_point = m_current_point;
      m_live_fprs &= ~(1U << offset);
    }
  else
    ranges.safe_push ({ m_current_point, m_current_point, INVALID_ALLOCNO });
}

// Record a use of allocno ALLOCNO at the current program point, as part
// of a backwards walk over a block.
void
early_ra::record_allocno_use (allocno_info *allocno)
{
  if (allocno->start_point == m_current_point)
    return;

  gcc_checking_assert (!allocno->is_shared ());
  bitmap_set_bit (m_live_allocnos, allocno->id);
  if (allocno->end_point > m_current_point)
    {
      allocno->end_point = m_current_point;
      allocno->last_def_point = START_OF_REGION;
      allocno->last_use_point = END_OF_REGION;
    }
  else
    allocno->last_use_point = allocno->start_point;
  allocno->start_point = m_current_point;
  allocno->is_copy_dest = false;
  allocno->is_strong_copy_src = false;
  allocno->related_allocno = INVALID_ALLOCNO;
  allocno->is_equiv = false;
}

// Record a definition of the allocno with index AI at the current program
// point, as part of a backwards walk over a block.  The allocno is known
// to be live.
void
early_ra::record_allocno_def (allocno_info *allocno)
{
  gcc_checking_assert (!allocno->is_shared ());
  allocno->last_use_point = allocno->start_point;
  allocno->last_def_point = m_current_point;
  allocno->start_point = m_current_point;
  allocno->num_defs = MIN (allocno->num_defs + 1, 2);
  gcc_checking_assert (!allocno->is_copy_dest
		       && !allocno->is_strong_copy_src);
  if (!bitmap_clear_bit (m_live_allocnos, allocno->id))
    gcc_unreachable ();
}

// SRC_ALLOCNO is copied or tied to DEST_ALLOCNO; IS_EQUIV is true if the
// two allocnos are known to be equal.  See whether we can mark a chain of
// allocnos ending at DEST_ALLOCNO as related to SRC_ALLOCNO.   Return the
// start of the chain if so, otherwise return null.
//
// If IS_EQUIV, a chain that contains just DEST_ALLOCNO should be treated
// as an equivalence.  Otherwise the chain should be shared with SRC_ALLOCNO.
//
// Sharing chains are a rather hacky workaround for the fact that we
// don't collect segmented live ranges, and that in the end we want to do
// simple interval graph coloring.
early_ra::allocno_info *
early_ra::find_related_start (allocno_info *dest_allocno,
			      allocno_info *src_allocno, bool is_equiv)
{
  allocno_info *res = nullptr;
  for (;;)
    {
      if (src_allocno->end_point > dest_allocno->end_point)
	// The src allocno dies first.
	return res;

      if (src_allocno->num_defs != 0)
	{
	  if (dest_allocno->end_point < m_current_bb_point)
	    // We don't currently track enough information to handle multiple
	    // definitions across basic block boundaries.
	    return res;

	  if (src_allocno->last_def_point >= dest_allocno->end_point)
	    // There is another definition during the destination's live range.
	    return res;
	}
      if (is_equiv)
	{
	  if (dest_allocno->num_defs == 1)
	    // dest_allocno is equivalent to src_allocno for dest_allocno's
	    // entire live range.  Fall back to that if we can't establish
	    // a sharing chain.
	    res = dest_allocno;
	}
      else
	{
	  if (src_allocno->last_use_point >= dest_allocno->end_point)
	    // src_allocno is live during dest_allocno's live range,
	    // and the two allocnos do not necessarily have the same value.
	    return res;
	}

      if (dest_allocno->group_size != 1
	  // Account for definitions by shared registers.
	  || dest_allocno->num_defs > 1
	  || DF_REG_DEF_COUNT (dest_allocno->group ()->regno) != 1)
	// Currently only single allocnos that are defined once can
	// share registers with non-equivalent allocnos.  This could be
	// relaxed, but at the time of writing, aggregates are not valid
	// SSA names and so generally only use a single pseudo throughout
	// their lifetime.
	return res;

      if (dest_allocno->copy_dest == src_allocno->id)
	// We've found a complete and valid sharing chain.
	return dest_allocno;

      if (dest_allocno->copy_dest == INVALID_ALLOCNO)
	return res;

      auto *next_allocno = m_allocnos[dest_allocno->copy_dest];
      if (!is_chain_candidate (dest_allocno, next_allocno, ALL_REASONS))
	return res;

      dest_allocno = next_allocno;
      is_equiv = false;
    }
}

// Add FROM_ALLOCNO's definition information to TO_ALLOCNO's.
void
early_ra::accumulate_defs (allocno_info *to_allocno,
			   allocno_info *from_allocno)
{
  if (from_allocno->num_defs > 0)
    {
      to_allocno->num_defs = MIN (from_allocno->num_defs
				  + to_allocno->num_defs, 2);
      to_allocno->last_def_point = MAX (to_allocno->last_def_point,
					from_allocno->last_def_point);
    }
}

// Record any relevant allocno-related information for an actual or imagined
// copy from SRC to DEST.  FROM_MOVE_P is true if the copy was an explicit
// move instruction, false if it represents one way of satisfying the previous
// instruction's constraints.
void
early_ra::record_copy (rtx dest, rtx src, bool from_move_p)
{
  auto dest_range = get_allocno_subgroup (dest);
  auto src_range = get_allocno_subgroup (src);
  if (from_move_p
      && dest_range
      && REG_P (src)
      && FP_REGNUM_P (REGNO (src)))
    {
      // A copy from an FPR to an allocno group.
      unsigned int fpr = REGNO (src) - V0_REGNUM;
      m_allocno_copies.safe_push ({ dest_range.allocno (0)->id, fpr,
				    dest_range.count });

      // If the allocno at the other end of the chain of copies from DEST
      // has a copy to the same FPR, record that all intervening copy chains
      // could become "strong" ones.  This indicates that picking the FPR
      // avoids a copy at both ends.
      unsigned int hard_regno = REGNO (src);
      for (auto &dest_allocno : dest_range.allocnos ())
	if (dest_allocno.hard_regno == hard_regno++)
	  dest_allocno.is_strong_copy_src = true;
    }
  else if (from_move_p
	   && src_range
	   && REG_P (dest)
	   && FP_REGNUM_P (REGNO (dest)))
    {
      // A copy from an allocno group to an FPR.
      unsigned int fpr = REGNO (dest) - V0_REGNUM;
      m_allocno_copies.safe_push ({ src_range.allocno (0)->id, fpr,
				    src_range.count });
      for (auto &src_allocno : src_range.allocnos ())
	{
	  // If the copy comes from a move, see whether the destination
	  // FPR is known to be equal to the source allocno for the FPR's
	  // last live range.
	  if (from_move_p && src_allocno.num_defs == 0)
	    {
	      auto &last_range = m_fpr_ranges[fpr].last ();
	      if (last_range.end_point >= src_allocno.end_point)
		last_range.allocno = src_allocno.id;
	    }
	  src_allocno.hard_regno = V0_REGNUM + fpr;
	  fpr += 1;
	}
    }
  else if (src_range && dest_range)
    {
      // A copy between two allocno groups.  We can only have a mismatched
      // number of FPRs for imaginary, non-move copies.  In that case
      // the matching happens on the common lowparts.
      gcc_assert (!from_move_p || src_range.count == dest_range.count);
      unsigned int count = std::min (src_range.count, dest_range.count);
      if (WORDS_BIG_ENDIAN)
	{
	  src_range.start += src_range.count - count;
	  dest_range.start += dest_range.count - count;
	}
      src_range.count = count;
      dest_range.count = count;

      // Ignore (imaginary non-move) copies if the destination is still live.
      for (auto &dest_allocno : dest_range.allocnos ())
	if (bitmap_bit_p (m_live_allocnos, dest_allocno.id))
	  return;

      for (unsigned int i = 0; i < src_range.count; ++i)
	{
	  auto *dest_allocno = dest_range.allocno (i);
	  auto *src_allocno = src_range.allocno (i);
	  if (src_allocno->end_point > dest_allocno->start_point)
	    {
	      gcc_assert (src_allocno->copy_dest == INVALID_ALLOCNO
			  || src_allocno->copy_dest == dest_allocno->id);
	      src_allocno->copy_dest = dest_allocno->id;
	      src_allocno->hard_regno = dest_allocno->hard_regno;
	      dest_allocno->is_copy_dest = 1;
	    }
	  else if (auto *start_allocno = find_related_start (dest_allocno,
							     src_allocno,
							     from_move_p))
	    {
	      auto *next_allocno = dest_allocno;
	      for (;;)
		{
		  next_allocno->related_allocno = src_allocno->id;
		  next_allocno->is_equiv = (start_allocno == dest_allocno
					    && from_move_p);
		  // If we're sharing two allocnos that are not equivalent,
		  // carry across the definition information.  This is needed
		  // to prevent multiple incompatible attempts to share with
		  // the same register.
		  if (next_allocno->is_shared ())
		    accumulate_defs (src_allocno, next_allocno);
		  src_allocno->last_use_point
		    = MAX (src_allocno->last_use_point,
			   next_allocno->last_use_point);

		  if (next_allocno == start_allocno)
		    break;
		  next_allocno = m_allocnos[next_allocno->copy_dest];
		}
	    }
	}
    }
}

// Record any relevant allocno-related information about the constraints
// on INSN, which has already been extracted.
void
early_ra::record_constraints (rtx_insn *insn)
{
  preprocess_constraints (insn);

  int operand_matches[MAX_RECOG_OPERANDS];
  for (int i = 0; i < recog_data.n_operands; ++i)
    operand_matches[i] = -1;

  auto alts = get_preferred_alternatives (insn);
  bool any_ok = recog_data.n_alternatives == 0;

  // The set of output operands that are earlyclobber in at least one
  // alternative.
  operand_mask earlyclobber_operands = 0;

  // The set of output operands that are matched to inputs in at least
  // one alternative.
  operand_mask matched_operands = 0;

  // The set of output operands that are not matched to inputs in at least
  // one alternative.
  operand_mask unmatched_operands = 0;

  // The set of input operands that are matched to outputs in at least one
  // alternative, or that overlap with such an input if the output is not
  // earlyclobber.  The latter part of the condition copes with things
  // like y = x * x, where the first x is tied to the destination, and where
  // y is not earlyclobber.
  operand_mask matches_operands = 0;

  for (int altno = 0; altno < recog_data.n_alternatives; ++altno)
    {
      if (!(alts & ALTERNATIVE_BIT (altno)))
	continue;

      auto *op_alt = &recog_op_alt[altno * recog_data.n_operands];
      if (!likely_alternative_match_p (op_alt))
	continue;

      any_ok = true;

      // Update the information for operand DEST_OPNO based on the constraint
      // information for operand SRC_OPNO.  The numbers can be different for
      // swapped commutative operands.
      auto record_operand = [&](int src_opno, int dest_opno)
	{
	  int matches = op_alt[src_opno].matches;
	  // A matched earlyclobber cannot be used if the same operand value
	  // occurs in an unmatched operand.  E.g. for y = x * x, a matched
	  // earlyclobber on the first input does not cover the second input.
	  if (matches >= 0)
	    {
	      rtx op = recog_data.operand[dest_opno];
	      operand_mask overlaps = 0;
	      for (int i = 0; i < recog_data.n_operands; ++i)
		if (i != dest_opno
		    && !recog_data.is_operator[i]
		    && recog_data.operand_type[i] != OP_OUT
		    && reg_overlap_mentioned_p (op, recog_data.operand[i]))
		  overlaps |= 1U << i;
	      if (!op_alt[matches].earlyclobber || overlaps == 0)
		{
		  operand_matches[dest_opno] = matches;
		  matches_operands |= (1U << dest_opno) | overlaps;
		}
	    }
	};

      auto reject = count_rejects (op_alt);
      for (int opno = 0; opno < recog_data.n_operands; ++opno)
	{
	  operand_mask op_mask = operand_mask (1) << opno;

	  if (recog_data.operand_type[opno] != OP_IN)
	    {
	      if (reject == 0 && op_alt[opno].matched >= 0)
		matched_operands |= op_mask;
	      else
		unmatched_operands |= op_mask;
	    }

	  if (op_alt[opno].earlyclobber)
	    earlyclobber_operands |= op_mask;

	  // Punt for now on scratches.  If we wanted to handle them,
	  // we'd need to create allocnos for them, like IRA does.
	  rtx op = recog_data.operand[opno];
	  if (GET_CODE (op) == SCRATCH
	      && reg_classes_intersect_p (op_alt[opno].cl, FP_REGS))
	    record_allocation_failure ([&](){
	      fprintf (dump_file, "insn %d has FPR match_scratch",
		       INSN_UID (insn));
	    });

	  // Record filter information, which applies to the first register
	  // in the operand.
	  if (auto filters = alternative_register_filters (op_alt, opno))
	    if (auto range = get_allocno_subgroup (recog_data.operand[opno]))
	      for (unsigned int fpr = range.start; fpr < 32; ++fpr)
		if (!test_register_filters (filters, fpr))
		  range.group->fpr_candidates &= ~(1U << (fpr - range.start));

	  if (reject == 0)
	    {
	      // Record possible matched operands.
	      record_operand (opno, opno);
	      if (recog_data.constraints[opno][0] == '%')
		{
		  record_operand (opno, opno + 1);
		  record_operand (opno + 1, opno);
		}
	    }
	}
    }

  if (!any_ok)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "       -- no match\n");
      record_allocation_failure ([&](){
	fprintf (dump_file, "no matching constraints for insn %d",
		 INSN_UID (insn));
      });
    }

  rtx dest_op = NULL_RTX;
  for (int opno = 0; opno < recog_data.n_operands; ++opno)
    {
      rtx op = recog_data.operand[opno];
      auto op_mask = operand_mask (1) << opno;
      // Record if there is an output operand that is "independent" of the
      // inputs, in the sense that it is never earlyclobber and is never
      // matched to an input.  See the comment below for how this is used.
      if (recog_data.operand_type[opno] == OP_OUT
	  && (earlyclobber_operands & op_mask) == 0
	  && (matched_operands & op_mask) == 0)
	{
	  dest_op = op;
	  break;
	}
      // We sometimes decide not to allocate pseudos even if they meet
      // the normal FPR preference criteria; see the setters of IGNORE_REG
      // for details.  However, the premise is that we should only ignore
      // definitions of such pseudos if they are independent of the other
      // operands in the instruction.
      else if (recog_data.operand_type[opno] != OP_IN
	       && REG_P (op)
	       && !HARD_REGISTER_P (op)
	       && (m_pseudo_regs[REGNO (op)].flags & IGNORE_REG) != 0)
	record_allocation_failure ([&](){
	  fprintf (dump_file, "ignored register r%d depends on input operands",
		   REGNO (op));
	});
    }

  for (int opno = 0; opno < recog_data.n_operands; ++opno)
    {
      auto op_mask = operand_mask (1) << opno;
      rtx op = recog_data.operand[opno];
      int matches = operand_matches[opno];

      // Punt for now on operands that already have a fixed choice of
      // register, since we don't have IRA's ability to find an alternative.
      // It's better if earlier passes don't create this kind of situation.
      if (REG_P (op) && FP_REGNUM_P (REGNO (op)))
	record_allocation_failure ([&](){
	  fprintf (dump_file, "operand %d of insn %d refers directly to %s",
		   opno, INSN_UID (insn), reg_names[REGNO (op)]);
	});

      // Treat input operands as being earlyclobbered if an output is
      // sometimes earlyclobber and if the input never matches an output.
      // Do the same if there is an output that is always matched to an
      // input, and if this operand doesn't match that input.  In both
      // cases, tying the input and the output would lead to an impossible
      // combination (or at least one that is difficult to reload).
      if (recog_data.operand_type[opno] != OP_OUT
	  && ((earlyclobber_operands && matches < 0)
	      || ((matched_operands & ~unmatched_operands)
		  && !(matches_operands & op_mask))))
	for (auto &allocno : get_allocno_subgroup (op).allocnos ())
	  if (allocno.end_point + 1 == m_current_point)
	    allocno.is_earlyclobbered = true;

      // Create copies between operands that can be tied.  This (deliberately)
      // might add several copies to the same destination register; later code
      // can then choose between them based on other criteria.
      //
      // If there is an output operand that is never matched or earlyclobber,
      // and an input operand that never matches an output operand, create
      // a tentative copy between them.  This allows hard register preferences
      // to be transmitted along the copy chains.
      if (matches >= 0)
	record_copy (recog_data.operand[matches], op);
      else if (dest_op && recog_data.operand_type[opno] == OP_IN)
	record_copy (dest_op, op);
    }
}

// If FLAGS is DF_REF_AT_TOP, model the artificial uses and defs at the
// start of the current basic block, otherwise model the artificial uses
// and defs at the end of the basic block.  This is done as part of a
// backwards walk, so defs should be processed before uses.
void
early_ra::record_artificial_refs (unsigned int flags)
{
  df_ref ref;

  FOR_EACH_ARTIFICIAL_DEF (ref, m_current_bb->index)
    if ((DF_REF_FLAGS (ref) & DF_REF_AT_TOP) == flags
	&& IN_RANGE (DF_REF_REGNO (ref), V0_REGNUM, V31_REGNUM))
      record_fpr_def (DF_REF_REGNO (ref));
  m_current_point += 1;

  FOR_EACH_ARTIFICIAL_USE (ref, m_current_bb->index)
    if ((DF_REF_FLAGS (ref) & DF_REF_AT_TOP) == flags
	&& IN_RANGE (DF_REF_REGNO (ref), V0_REGNUM, V31_REGNUM))
      record_fpr_use (DF_REF_REGNO (ref));
  m_current_point += 1;
}

// Called as part of a backwards walk over a block.  Model the definitions
// in INSN, excluding partial call clobbers.
void
early_ra::record_insn_defs (rtx_insn *insn)
{
  df_ref ref;

  FOR_EACH_INSN_DEF (ref, insn)
    if (IN_RANGE (DF_REF_REGNO (ref), V0_REGNUM, V31_REGNUM))
      record_fpr_def (DF_REF_REGNO (ref));
    else
      {
	auto range = get_allocno_subgroup (DF_REF_REG (ref));
	for (auto &allocno : range.allocnos ())
	  {
	    // If the destination is unused, record a momentary blip
	    // in its live range.
	    if (!bitmap_bit_p (m_live_allocnos, allocno.id))
	      record_allocno_use (&allocno);
	    record_allocno_def (&allocno);
	  }
      }
  m_current_point += 1;
}

// Called as part of a backwards walk over a block.  Model the call made
// by INSN as a separate phase in the evaluation of the insn.  Any partial
// call clobbers happen at that point, rather than in the definition or use
// phase of the insn.
void
early_ra::record_insn_call (rtx_call_insn *insn)
{
  function_abi abi = insn_callee_abi (insn);
  m_call_points[abi.id ()].safe_push (m_current_point);
  m_current_point += 1;
}

// Called as part of a backwards walk over a block.  Model the uses in INSN.
// We can ignore READ_MODIFY_WRITE uses of plain subregs, since we track the
// FPR-sized parts of them individually.
void
early_ra::record_insn_uses (rtx_insn *insn)
{
  df_ref ref;

  FOR_EACH_INSN_USE (ref, insn)
    if (IN_RANGE (DF_REF_REGNO (ref), V0_REGNUM, V31_REGNUM))
      record_fpr_use (DF_REF_REGNO (ref));
    else if (!DF_REF_FLAGS_IS_SET (ref, DF_REF_READ_WRITE)
	     || DF_REF_FLAGS_IS_SET (ref, DF_REF_STRICT_LOW_PART)
	     || DF_REF_FLAGS_IS_SET (ref, DF_REF_ZERO_EXTRACT))
      {
	auto range = get_allocno_subgroup (DF_REF_REG (ref));
	for (auto &allocno : range.allocnos ())
	  record_allocno_use (&allocno);
      }
  m_current_point += 1;
}

// ALLOCNO->is_strong_copy_src is true.  See whether ALLOCNO heads a
// natural chain that has an affinity with the same hard register at
// both ends.
bool
early_ra::consider_strong_copy_src_chain (allocno_info *allocno)
{
  auto *src_allocno = allocno;
  while (src_allocno->copy_dest != INVALID_ALLOCNO)
    {
      auto *dest_allocno = m_allocnos[src_allocno->copy_dest];
      if (dest_allocno->start_point > src_allocno->end_point
	  || dest_allocno->hard_regno != src_allocno->hard_regno)
	return false;
      gcc_checking_assert (dest_allocno->is_copy_dest);
      src_allocno = dest_allocno;
    }

  while (allocno->copy_dest != INVALID_ALLOCNO)
    {
      allocno->is_strong_copy_src = 1;
      allocno = m_allocnos[allocno->copy_dest];
      allocno->is_strong_copy_dest = 1;
    }
  return true;
}

// ALLOCNO1 and ALLOCNO2 are linked in some way, and might end up being
// chained together.  See whether chaining them requires the containing
// groups to have the same stride, or whether it requires them to have
// different strides.  Return 1 if they should have the same stride,
// -1 if they should have different strides, or 0 if it doesn't matter.
int
early_ra::strided_polarity_pref (allocno_info *allocno1,
				 allocno_info *allocno2)
{
  if (allocno1->offset + 1 < allocno1->group_size
      && allocno2->offset + 1 < allocno2->group_size)
    {
      if (is_chain_candidate (allocno1 + 1, allocno2 + 1, ALL_REASONS))
	return 1;
      else
	return -1;
    }

  if (allocno1->offset > 0 && allocno2->offset > 0)
    {
      if (is_chain_candidate (allocno1 - 1, allocno2 - 1, ALL_REASONS))
	return 1;
      else
	return -1;
    }

  return 0;
}

// Decide which groups should be strided.  Also complete "strong" copy chains.
void
early_ra::find_strided_accesses ()
{
  // This function forms a graph of allocnos, linked by equivalences and
  // natural copy chains.  It temporarily uses chain_next to record the
  // reverse of equivalence edges (related_allocno) and chain_prev to record
  // the reverse of copy edges (copy_dest).
  unsigned int allocno_info::*links[] = {
    &allocno_info::chain_next,
    &allocno_info::chain_prev,
    &allocno_info::copy_dest,
    &allocno_info::related_allocno
  };

  // Set up the temporary reverse edges.  Check for strong copy chains.
  for (unsigned int i = m_allocnos.length (); i-- > 0; )
    {
      auto *allocno1 = m_allocnos[i];
      if (allocno1->copy_dest != INVALID_ALLOCNO)
	m_allocnos[allocno1->copy_dest]->chain_prev = allocno1->id;
      if (allocno1->related_allocno != INVALID_ALLOCNO)
	m_allocnos[allocno1->related_allocno]->chain_next = allocno1->id;

      if (allocno1->is_strong_copy_src
	  && !allocno1->is_copy_dest
	  && !consider_strong_copy_src_chain (allocno1))
	allocno1->is_strong_copy_src = false;
    }

  // Partition the graph into cliques based on edges that have the following
  // properties:
  //
  // - the edge joins two allocnos whose groups have a free choice between
  //   consecutive and strided allocations.
  //
  // - the two groups have a relative strided polarity preference (that is
  //   they should make the same choice between consecutive and strided,
  //   or they should make opposite choices).
  //
  // Assign relative polarities to each group connected in this way.
  //
  // The aim is to discover natural move-free striding choices, which will
  // often exist in carefully written ACLE code.
  unsigned int num_edges = m_allocnos.length () * ARRAY_SIZE (links);
  auto_sbitmap visited_edges (num_edges);
  bitmap_clear (visited_edges);

  auto_vec<unsigned int, 32> worklist;
  for (unsigned int i = 0; i < num_edges; ++i)
    {
      if (!bitmap_set_bit (visited_edges, i))
	continue;
      worklist.quick_push (i);
      while (!worklist.is_empty ())
	{
	  auto ei = worklist.pop ();
	  auto *allocno1 = m_allocnos[ei / ARRAY_SIZE (links)];
	  auto ai2 = allocno1->*links[ei % ARRAY_SIZE (links)];
	  if (ai2 == INVALID_ALLOCNO)
	    continue;

	  auto *allocno2 = m_allocnos[ai2];
	  auto *group1 = allocno1->group ();
	  auto *group2 = allocno2->group ();
	  if (!group1->has_flexible_stride || !group2->has_flexible_stride)
	    continue;

	  int pref = strided_polarity_pref (allocno1, allocno2);
	  if (pref == 0)
	    continue;

	  for (auto *group : { group1, group2 })
	    for (auto &allocno : group->allocnos ())
	      for (unsigned int j = 0; j < ARRAY_SIZE (links); ++j)
		if (bitmap_set_bit (visited_edges, allocno.id * 4 + j))
		  worklist.safe_push (allocno.id * 4 + j);

	  if (group1->strided_polarity)
	    group2->strided_polarity = group1->strided_polarity * pref;
	  else if (group2->strided_polarity)
	    group1->strided_polarity = group2->strided_polarity * pref;
	  else
	    {
	      group1->strided_polarity = 1;
	      group2->strided_polarity = pref;
	    }
	}
    }

  // Now look for edges between allocnos in multi-register groups where:
  //
  // - the two groups have a relative strided polarity preference (as above).
  //
  // - one group (G1) has a free choice between consecutive and strided
  //   allocations.
  //
  // - the other group (G2) must use consecutive allocations.
  //
  // Update G1's individual preference for strided or consecutive allocations
  // based on G2.  If the previous loop chose a polarity for G1, work out
  // whether it is better for polarity 1 or -1 to correspond to consecutive
  // allocation.
  int consecutive_pref = 0;
  for (unsigned int i = m_allocnos.length (); i-- > 0; )
    {
      auto *allocno1 = m_allocnos[i];
      for (auto link : links)
	{
	  auto ai2 = allocno1->*link;
	  if (ai2 == INVALID_ALLOCNO)
	    continue;

	  auto *allocno2 = m_allocnos[ai2];
	  auto *group1 = allocno1->group ();
	  auto *group2 = allocno2->group ();
	  if (group1->has_flexible_stride == group2->has_flexible_stride)
	    continue;

	  int pref = strided_polarity_pref (allocno1, allocno2);
	  if (pref == 0)
	    continue;

	  auto *group = (group1->has_flexible_stride ? group1 : group2);
	  consecutive_pref += group->strided_polarity * pref;
	  group->consecutive_pref += pref;
	}
    }

  // If it doesn't matter whether polarity 1 or -1 corresponds to consecutive
  // allocation, arbitrarily pick 1.
  if (consecutive_pref == 0)
    consecutive_pref = 1;

  // Record which multi-register groups should use strided allocations.
  // Clear out the temporary edges.
  for (unsigned int ai = 0; ai < m_allocnos.length (); ++ai)
    {
      auto *allocno = m_allocnos[ai];
      allocno->chain_prev = INVALID_ALLOCNO;
      allocno->chain_next = INVALID_ALLOCNO;

      if (allocno->offset != 0)
	continue;

      auto *group = allocno->group ();
      if (!group->has_flexible_stride)
	continue;

      bool make_strided = (group->strided_polarity
			   ? (consecutive_pref * group->strided_polarity) < 0
			   : group->consecutive_pref < 0);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Allocno [%d:%d]: strided polarity %d,"
		 " consecutive pref %d, %s\n",
		 allocno->id, allocno->id + group->size - 1,
		 group->strided_polarity, group->consecutive_pref,
		 make_strided ? "making strided" : "keeping consecutive");
      if (!make_strided)
	continue;

      // 2-register groups have a stride of 8 FPRs and must start in
      // registers matching the mask 0x17.  4-register groups have a stride
      // of 4 FPRs and must start in registers matching the mask 0x13.
      group->stride = group->size == 2 ? 8 : 4;
      gcc_checking_assert (group->fpr_candidates
			   == (group->size == 2 ? 0x55555555 : 0x11111111));
      group->fpr_candidates = (group->size == 2 ? 0xff00ff : 0xf000f);
    }
}

// Compare the allocnos at *ALLOCNO1_PTR and *ALLOCNO2_PTR and return a <=>
// result that puts allocnos in order of increasing FIELD.
template<unsigned int early_ra::allocno_info::*field>
int
early_ra::cmp_increasing (const void *allocno1_ptr, const void *allocno2_ptr)
{
  auto *allocno1 = *(allocno_info *const *) allocno1_ptr;
  auto *allocno2 = *(allocno_info *const *) allocno2_ptr;

  if (allocno1->*field != allocno2->*field)
    return allocno1->*field < allocno2->*field ? -1 : 1;
  return (allocno1->id < allocno2->id ? -1
	  : allocno1->id == allocno2->id ? 0 : 1);
}

// Return true if we should consider chaining ALLOCNO1 onto the head
// of ALLOCNO2.  STRICTNESS says whether we should take copy-elision
// heuristics into account, or whether we should just consider things
// that matter for correctness.
//
// This is just a local test of the two allocnos; it doesn't guarantee
// that chaining them would give a self-consistent system.
bool
early_ra::is_chain_candidate (allocno_info *allocno1, allocno_info *allocno2,
			      test_strictness strictness)
{
  if (allocno2->is_shared ())
    return false;

  while (allocno1->is_equiv)
    allocno1 = m_allocnos[allocno1->related_allocno];

  if (allocno2->start_point >= allocno1->end_point
      && !allocno2->is_equiv_to (allocno1->id))
    return false;

  if (allocno1->is_earlyclobbered
      && allocno1->end_point == allocno2->start_point + 1)
    return false;

  if (strictness == ALL_REASONS && allocno2->is_copy_dest)
    {
      if (allocno1->copy_dest != allocno2->id)
	return false;
      if (allocno2->is_strong_copy_dest && !allocno1->is_strong_copy_src)
	return false;
    }
  return true;
}

// We're trying to chain allocno ALLOCNO1 to a later allocno.
// Rate how good a choice ALLOCNO2 would be, with higher being better.
int
early_ra::rate_chain (allocno_info *allocno1, allocno_info *allocno2)
{
  int score = 0;
  if (allocno2->is_strong_copy_dest)
    score += 256;
  else if (allocno2->is_copy_dest)
    score += 128;

  // Prefer well-aligned matches.
  auto *group1 = allocno1->group ();
  auto *group2 = allocno2->group ();
  if (group1->stride == 1 && group2->stride == 1)
    {
      unsigned int min_size = std::min (group1->color_rep ()->size,
					group2->color_rep ()->size);
      if ((group1->color_rep_offset + allocno1->offset) % min_size
	  == (group2->color_rep_offset + allocno2->offset) % min_size)
	score += min_size;
      else
	score -= min_size;
    }
  return score;
}

// Sort the chain_candidate_infos at ARG1 and ARG2 in order of decreasing
// score.
int
early_ra::cmp_chain_candidates (const void *arg1, const void *arg2)
{
  auto &candidate1 = *(const chain_candidate_info *) arg1;
  auto &candidate2 = *(const chain_candidate_info *) arg2;
  if (candidate1.score != candidate2.score)
    return candidate1.score > candidate2.score ? -1 : 1;

  // Prefer to increase the gap between uses of the allocated register,
  // to give the scheduler more freedom.
  auto *allocno1 = candidate1.allocno;
  auto *allocno2 = candidate2.allocno;
  if (allocno1->start_point != allocno2->start_point)
    return allocno1->start_point < allocno2->start_point ? -1 : 1;

  if (allocno1 != allocno2)
    return allocno1->id < allocno2->id ? -1 : 1;

  return 0;
}

// Join the chains of allocnos that start at HEADI1 and HEADI2.
// HEADI1 is either empty or a single allocno.
void
early_ra::chain_allocnos (unsigned int &headi1, unsigned int &headi2)
{
  if (headi1 == INVALID_ALLOCNO)
    headi1 = headi2;
  else if (headi2 == INVALID_ALLOCNO)
    headi2 = headi1;
  else
    {
      auto *head1 = m_allocnos[headi1];
      auto *head2 = m_allocnos[headi2];
      gcc_checking_assert (head1->chain_next == INVALID_ALLOCNO
			   && head1->chain_prev == INVALID_ALLOCNO
			   && head2->chain_prev == INVALID_ALLOCNO);

      if (head1->is_equiv
	  && m_allocnos[head1->related_allocno]->copy_dest == headi2)
	{
	  head1->is_copy_dest = head2->is_copy_dest;
	  head1->is_strong_copy_dest = head2->is_strong_copy_dest;
	  m_allocnos[head1->related_allocno]->copy_dest = headi1;
	}
      head1->chain_next = headi2;
      head2->chain_prev = headi1;

      headi2 = headi1;
    }
}

// Add GROUP2's FPR information to GROUP1's, given that GROUP2 starts
// OFFSET allocnos into GROUP2.
void
early_ra::merge_fpr_info (allocno_group_info *group1,
			  allocno_group_info *group2,
			  unsigned int offset)
{
  group1->fpr_size = std::max (group1->fpr_size, group2->fpr_size);
  group1->fpr_candidates &= (group2->fpr_candidates
			     >> (offset * group1->stride));
}

// Set the color representative of ALLOCNO's group to REP, such that ALLOCNO
// ends being at allocno offset REP_OFFSET from the start of REP.
void
early_ra::set_single_color_rep (allocno_info *allocno, allocno_group_info *rep,
				unsigned int rep_offset)
{
  auto *group = allocno->group ();
  if (group->m_color_rep == rep)
    return;

  group->m_color_rep = rep;
  gcc_checking_assert (multiple_p (group->stride, rep->stride));
  unsigned int factor = group->stride / rep->stride;
  gcc_checking_assert (rep_offset >= allocno->offset * factor);
  group->color_rep_offset = rep_offset - allocno->offset * factor;
  merge_fpr_info (rep, group, group->color_rep_offset);
}

// REP1 and REP2 are color representatives.  Change REP1's color representative
// to REP2, with REP1 starting at allocno offset REP2_OFFSET into REP2.
void
early_ra::set_color_rep (allocno_group_info *rep1, allocno_group_info *rep2,
			 unsigned int rep2_offset)
{
  gcc_checking_assert (rep1 != rep2
		       && rep2->m_color_rep == rep2
		       && multiple_p (rep1->stride, rep2->stride));

  auto heads1 = rep1->chain_heads ();
  auto heads2 = rep2->chain_heads ();
  for (unsigned int i1 = 0; i1 < heads1.size (); ++i1)
    if (heads1[i1] != INVALID_ALLOCNO)
      {
	unsigned int i2 = rep2_offset + i1 * rep1->stride / rep2->stride;
	if (heads2[i2] == INVALID_ALLOCNO)
	  heads2[i2] = heads1[i1];
	else
	  gcc_checking_assert (heads2[i2] == heads1[i1]);
	set_single_color_rep (m_allocnos[heads1[i1]], rep2, i2);
      }
}

// Try to chain ALLOCNO1 to the head of the chain starting at ALLOCNO2.
// Return true on success.
bool
early_ra::try_to_chain_allocnos (allocno_info *allocno1,
				 allocno_info *allocno2)
{
  auto *group1 = allocno1->group ()->color_rep ();
  auto *group2 = allocno2->group ()->color_rep ();

  // Avoid trying to tie different subgroups of the same group.  This can
  // happen if the parts of a register are defined and used piecemeal.
  if (group1 == group2)
    return false;

  // The stride (in FPRs) between allocnos of each color representative.
  auto fpr_stride1 = group1->stride;
  auto fpr_stride2 = group2->stride;

  // The offset (in FPRs) of each allocno group from its color representative.
  auto fpr_offset1 = allocno1->group ()->color_rep_offset * fpr_stride1;
  auto fpr_offset2 = allocno2->group ()->color_rep_offset * fpr_stride2;

  // The offset (in FPRs) of each allocno from its color representative.
  fpr_offset1 += allocno1->offset * allocno1->group ()->stride;
  fpr_offset2 += allocno2->offset * allocno2->group ()->stride;

  // The FPR overlap is in multiples of the larger stride.
  auto max_fpr_stride = std::max (fpr_stride1, fpr_stride2);
  auto min_fpr_offset = std::min (fpr_offset1, fpr_offset2);
  auto fpr_overlap_offset = ROUND_DOWN (min_fpr_offset, max_fpr_stride);

  // The offset (in FPRs) of the start of the overlapping region from
  // each color representative.
  fpr_offset1 -= fpr_overlap_offset;
  fpr_offset2 -= fpr_overlap_offset;

  // The number of FPRs in each color representative after the start
  // of the overlapping region.
  auto fpr_after1 = (group1->size - 1) * fpr_stride1 - fpr_offset1;
  auto fpr_after2 = (group2->size - 1) * fpr_stride2 - fpr_offset2;

  auto min_fpr_after = std::min (fpr_after1, fpr_after2);

  // The number of overlapping allocnos.
  auto allocno_overlap_size = min_fpr_after / max_fpr_stride + 1;

  // The offset (in allocnos) of the overlapping region from the start
  // of each color representative.
  auto allocno_offset1 = fpr_offset1 / fpr_stride1;
  auto allocno_offset2 = fpr_offset2 / fpr_stride2;

  // The stride (in allocnos) between overlapping allocnos.
  auto allocno_stride1 = max_fpr_stride / fpr_stride1;
  auto allocno_stride2 = max_fpr_stride / fpr_stride2;

  // Reject combinations that are impossible to allocate.
  auto fprs1 = group1->fpr_candidates;
  auto fprs2 = group2->fpr_candidates;
  if (fpr_offset1 > fpr_offset2)
    fprs2 >>= (fpr_offset1 - fpr_offset2);
  else
    fprs1 >>= (fpr_offset2 - fpr_offset1);
  if ((fprs1 & fprs2) == 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "    - cannot chain %d->%d, no FPRs in common"
		 " (%08x@%d and %08x@%d)\n", allocno1->id, allocno2->id,
		 group1->fpr_candidates, fpr_offset1,
		 group2->fpr_candidates, fpr_offset2);
      return false;
    }

  // Check whether the chain can be formed.
  auto heads1 = group1->chain_heads ();
  auto heads2 = group2->chain_heads ();
  for (unsigned int i = 0; i < allocno_overlap_size; ++i)
    {
      auto headi1 = heads1[allocno_offset1 + i * allocno_stride1];
      auto headi2 = heads2[allocno_offset2 + i * allocno_stride2];
      if (headi1 != INVALID_ALLOCNO && headi2 != INVALID_ALLOCNO)
	{
	  auto *head1 = m_allocnos[headi1];
	  auto *head2 = m_allocnos[headi2];
	  if (head1->chain_next != INVALID_ALLOCNO)
	    return false;
	  if (!is_chain_candidate (head1, head2, CORRECTNESS_ONLY))
	    return false;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "    - chaining allocnos [");
      for (unsigned int i = 0; i < allocno_overlap_size; ++i)
	fprintf (dump_file, "%s%d", i ? "," : "",
		 heads1[allocno_offset1 + i * allocno_stride1]);
      fprintf (dump_file, "] and [");
      for (unsigned int i = 0; i < allocno_overlap_size; ++i)
	fprintf (dump_file, "%s%d", i ? "," : "",
		 heads2[allocno_offset2 + i * allocno_stride2]);
      fprintf (dump_file, "]\n");
    }

  // Chain the allocnos, updating the chain heads.
  for (unsigned int i = 0; i < allocno_overlap_size; ++i)
    chain_allocnos (heads1[allocno_offset1 + i * allocno_stride1],
		    heads2[allocno_offset2 + i * allocno_stride2]);

  // Pick a color representative for the merged groups.
  allocno_group_info *new_rep;
  if (allocno_offset1 == 0
      && group1->size == allocno_overlap_size * allocno_stride1
      && multiple_p (fpr_stride1, fpr_stride2))
    {
      // The first group fits within the second.
      set_color_rep (group1, group2, allocno_offset2);
      new_rep = group2;
    }
  else if (allocno_offset2 == 0
	   && group2->size == allocno_overlap_size * allocno_stride2
	   && multiple_p (fpr_stride2, fpr_stride1))
    {
      // The second group fits within the first.
      set_color_rep (group2, group1, allocno_offset1);
      new_rep = group1;
    }
  else
    {
      // We need a new group that is big enough to span both groups.
      // The new group always has an FPR stride of 1.
      auto max_fpr_offset = std::max (fpr_offset1, fpr_offset2);
      auto max_fpr_after = std::max (fpr_after1, fpr_after2);
      auto new_size = max_fpr_offset + max_fpr_after + 1;
      new_rep = create_allocno_group (INVALID_REGNUM, new_size);

      set_color_rep (group1, new_rep, max_fpr_offset - fpr_offset1);
      set_color_rep (group2, new_rep, max_fpr_offset - fpr_offset2);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "    - new frontier [");
      auto new_heads = new_rep->chain_heads ();
      for (unsigned int i = 0; i < new_heads.size (); ++i)
	{
	  if (i)
	    fprintf (dump_file, ",");
	  if (new_heads[i] == INVALID_ALLOCNO)
	    fprintf (dump_file, "-");
	  else
	    fprintf (dump_file, "%d", new_heads[i]);
	}
      fprintf (dump_file, "]\n");
    }

  return true;
}

// Create a color_info for color representative GROUP.
void
early_ra::create_color (allocno_group_info *group)
{
  auto *color = region_allocate<color_info> ();
  color->id = m_colors.length ();
  color->hard_regno = FIRST_PSEUDO_REGISTER;
  color->group = group;

  gcc_checking_assert (group->m_color_rep == group);
  group->has_color = true;
  group->color = m_colors.length ();

  m_colors.safe_push (color);
}

// Form allocnos into chains.  Create colors for each resulting clique.
void
early_ra::form_chains ()
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nChaining allocnos:\n");

  // Perform (modified) interval graph coloring.  First sort by
  // increasing start point.
  m_sorted_allocnos.reserve (m_allocnos.length ());
  m_sorted_allocnos.splice (m_allocnos);
  m_sorted_allocnos.qsort (cmp_increasing<&allocno_info::start_point>);

  // During this phase, color representatives are only correct for
  // unprocessed allocno groups (where the color representative is
  // the group itself) and for groups that contain a current chain head.
  unsigned int ti = 0;
  auto_vec<chain_candidate_info> candidates;
  for (unsigned int hi = 0; hi < m_sorted_allocnos.length (); ++hi)
    {
      auto *allocno1 = m_sorted_allocnos[hi];
      if (allocno1->chain_next != INVALID_ALLOCNO)
	continue;

      // Record conflicts with direct uses for FPR hard registers.
      auto *group1 = allocno1->group ();
      for (unsigned int fpr = allocno1->offset; fpr < 32; ++fpr)
	if (fpr_conflicts_with_allocno_p (fpr, allocno1))
	  group1->fpr_candidates &= ~(1U << (fpr - allocno1->offset));

      // Record conflicts due to partially call-clobbered registers.
      // (Full clobbers are handled by the previous loop.)
      for (unsigned int abi_id = 0; abi_id < NUM_ABI_IDS; ++abi_id)
	if (call_in_range_p (abi_id, allocno1->start_point,
			     allocno1->end_point))
	  {
	    auto fprs = partial_fpr_clobbers (abi_id, group1->fpr_size);
	    group1->fpr_candidates &= ~fprs >> allocno1->offset;
	  }

      if (allocno1->is_shared ())
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Allocno %d shares the same hard register"
		     " as allocno %d\n", allocno1->id,
		     allocno1->related_allocno);
	  auto *allocno2 = m_allocnos[allocno1->related_allocno];
	  merge_fpr_info (allocno2->group (), group1, allocno2->offset);
	  m_shared_allocnos.safe_push (allocno1);
	  continue;
	}

      // Find earlier allocnos (in processing order) that could be chained
      // to this one.
      candidates.truncate (0);
      for (unsigned int sci = ti; sci < hi; ++sci)
	{
	  auto *allocno2 = m_sorted_allocnos[sci];
	  if (allocno2->chain_prev == INVALID_ALLOCNO)
	    {
	      if (!is_chain_candidate (allocno1, allocno2, ALL_REASONS))
		continue;
	      chain_candidate_info candidate;
	      candidate.allocno = allocno2;
	      candidate.score = rate_chain (allocno1, allocno2);
	      candidates.safe_push (candidate);
	    }
	  else if (sci == ti)
	    ++ti;
	}

      // Sort the candidates by decreasing score.
      candidates.qsort (cmp_chain_candidates);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Chain candidates for %d:", allocno1->id);
	  for (auto &candidate : candidates)
	    fprintf (dump_file, " %d(%d)", candidate.allocno->id,
		     candidate.score);
	  fprintf (dump_file, "\n");
	}

      // Pick the first candidate that works.
      for (auto &candidate : candidates)
	if (try_to_chain_allocnos (allocno1, candidate.allocno))
	  break;
    }

  // Create color_infos for each group.  Make sure that each group's
  // color representative is up to date.
  for (unsigned int hi = m_sorted_allocnos.length (); hi-- > 0; )
    {
      auto *allocno = m_sorted_allocnos[hi];
      if (allocno->is_shared ())
	continue;

      auto *rep = allocno->group ()->color_rep ();
      if (rep->has_color)
	continue;

      create_color (rep);
      auto heads = rep->chain_heads ();
      for (unsigned int i = 0; i < heads.size (); ++i)
	{
	  unsigned int ai = heads[i];
	  while (ai != INVALID_ALLOCNO)
	    {
	      allocno = m_allocnos[ai];
	      set_single_color_rep (allocno, rep, i * rep->stride);
	      ai = allocno->chain_next;
	    }
	}
    }
}

// Return true if the given FPR (starting at 0) conflicts with allocno
// ALLOCNO.
bool
early_ra::fpr_conflicts_with_allocno_p (unsigned int fpr,
					allocno_info *allocno)
{
  auto &ranges = m_fpr_ranges[fpr];
  unsigned int start_i = 0;
  unsigned int end_i = ranges.length ();
  while (start_i < end_i)
    {
      unsigned int mid_i = (start_i + end_i) / 2;
      auto &range = ranges[mid_i];
      if (allocno->end_point > range.start_point)
	start_i = mid_i + 1;
      else if (allocno->start_point < range.end_point)
	end_i = mid_i;
      else
	{
	  if (range.allocno != allocno->id)
	    return true;
	  // The FPR is equivalent to ALLOCNO for this particular range.
	  // See whether ALLOCNO conflicts with a neighboring range.
	  if (mid_i > 0
	      && ranges[mid_i - 1].start_point >= allocno->end_point)
	    return true;
	  if (mid_i + 1 < ranges.length ()
	      && ranges[mid_i + 1].end_point <= allocno->start_point)
	    return true;
	  return false;
	}
    }
  return false;
}

// Return true if there is a call with ABI identifier ABI_ID in the inclusive
// program point range [START_POINT, END_POINT].
bool
early_ra::call_in_range_p (unsigned int abi_id, unsigned int start_point,
			   unsigned int end_point)
{
  auto &points = m_call_points[abi_id];
  unsigned int start_i = 0;
  unsigned int end_i = points.length ();
  while (start_i < end_i)
    {
      unsigned int mid_i = (start_i + end_i) / 2;
      auto point = points[mid_i];
      if (end_point > point)
	start_i = mid_i + 1;
      else if (start_point < point)
	end_i = mid_i;
      else
	return true;
    }
  return false;
}

// Return the set of FPRs for which a value of size SIZE will be clobbered
// by a call to a function with ABI identifier ABI_ID, but would not be
// for some smaller size.  The set therefore excludes FPRs that are
// fully-clobbered, like V0 in the base ABI.
unsigned int
early_ra::partial_fpr_clobbers (unsigned int abi_id, fpr_size_info size)
{
  auto &abi = function_abis[abi_id];
  unsigned int clobbers = 0;
  machine_mode mode = (size == FPR_D ? V8QImode
		       : size == FPR_Q ? V16QImode : VNx16QImode);
  for (unsigned int regno = V0_REGNUM; regno <= V31_REGNUM; ++regno)
    if (!abi.clobbers_full_reg_p (regno)
	&& abi.clobbers_reg_p (mode, regno))
      clobbers |= 1U << (regno - V0_REGNUM);
  return clobbers;
}

// Process copies between pseudo registers and hard registers and update
// the FPR preferences for the associated colors.
void
early_ra::process_copies ()
{
  for (auto &copy : m_allocno_copies)
    {
      auto *allocno = m_allocnos[copy.allocno];
      auto *group = allocno->group ();
      auto offset = group->color_rep_offset + allocno->offset;
      if (offset > copy.fpr)
	continue;

      unsigned int fpr = copy.fpr - offset;
      auto *color = m_colors[group->color_rep ()->color];
      color->fpr_preferences[fpr] = MIN (color->fpr_preferences[fpr]
					 + copy.weight, 127);
      color->num_fpr_preferences += copy.weight;
    }
}

// Compare the colors at *COLOR1_PTR and *COLOR2_PTR and return a <=>
// result that puts colors in allocation order.
int
early_ra::cmp_allocation_order (const void *color1_ptr, const void *color2_ptr)
{
  auto *color1 = *(color_info *const *) color1_ptr;
  auto *color2 = *(color_info *const *) color2_ptr;

  // Allocate bigger groups before smaller groups.
  if (color1->group->size != color2->group->size)
    return color1->group->size > color2->group->size ? -1 : 1;

  // Allocate groups with stronger FPR preferences before groups with weaker
  // FPR preferences.
  if (color1->num_fpr_preferences != color2->num_fpr_preferences)
    return color1->num_fpr_preferences > color2->num_fpr_preferences ? -1 : 1;

  return (color1->id < color2->id ? -1
	  : color1->id == color2->id ? 0 : 1);
}

// Allocate a register to each color.  If we run out of registers,
// give up on doing a full allocation of the FPR-based pseudos in the
// region.
void
early_ra::allocate_colors ()
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nAllocating registers:\n");

  auto_vec<color_info *> sorted_colors;
  sorted_colors.safe_splice (m_colors);
  sorted_colors.qsort (cmp_allocation_order);

  for (unsigned int i = 0; i < 32; ++i)
    if (!crtl->abi->clobbers_full_reg_p (V0_REGNUM + i))
      m_call_preserved_fprs |= 1U << i;

  for (auto *color : sorted_colors)
    {
      unsigned int candidates = color->group->fpr_candidates;
      for (unsigned int i = 0; i < color->group->size; ++i)
	candidates &= ~(m_allocated_fprs >> i);
      unsigned int best = INVALID_REGNUM;
      int best_weight = 0;
      unsigned int best_recency = 0;
      for (unsigned int fpr = 0; fpr <= 32U - color->group->size; ++fpr)
	{
	  if ((candidates & (1U << fpr)) == 0)
	    continue;
	  int weight = color->fpr_preferences[fpr];
	  unsigned int recency = 0;
	  // Account for registers that the current function must preserve.
	  for (unsigned int i = 0; i < color->group->size; ++i)
	    {
	      if (m_call_preserved_fprs & (1U << (fpr + i)))
		weight -= 1;
	      recency = MAX (recency, m_fpr_recency[fpr + i]);
	    }
	  // Prefer higher-numbered registers in the event of a tie.
	  // This should tend to keep lower-numbered registers free
	  // for allocnos that require V0-V7 or V0-V15.
	  if (best == INVALID_REGNUM
	      || best_weight < weight
	      || (best_weight == weight && recency <= best_recency))
	    {
	      best = fpr;
	      best_weight = weight;
	      best_recency = recency;
	    }
	}

      if (best == INVALID_REGNUM)
	{
	  record_allocation_failure ([&](){
	    fprintf (dump_file, "no free register for color %d", color->id);
	  });
	  return;
	}

      color->hard_regno = best + V0_REGNUM;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  Allocating [v%d:v%d] to color %d\n",
		 best, best + color->group->size - 1, color->id);
      m_allocated_fprs |= ((1U << color->group->size) - 1) << best;
    }
}

// See if ALLOCNO ends a subchain of single registers that can be split
// off without affecting the rest of the chain, and without introducing
// any moves.  Return the start of the chain if so (which might be ALLOCNO
// itself), otherwise return null.
early_ra::allocno_info *
early_ra::find_independent_subchain (allocno_info *allocno)
{
  // Make sure ALLOCNO ends a natural subchain.
  if (auto *next_allocno = chain_next (allocno))
    if (next_allocno->start_point + 1 >= allocno->end_point)
      return nullptr;

  // Check the allocnos in the purported subchain and find the other end.
  for (;;)
    {
      auto *group = allocno->group ();
      if (group->m_color_rep == group)
	return nullptr;
      if (group->size != 1)
	return nullptr;

      auto *prev_allocno = chain_prev (allocno);
      if (!prev_allocno || allocno->start_point + 1 < prev_allocno->end_point)
	return allocno;

      allocno = prev_allocno;
    }
}

// Search the colors starting at index FIRST_COLOR whose FPRs do not belong
// to FPR_CONFLICTS.  Return the first such color that has no group.  If all
// such colors have groups, instead return the color with the latest
// (smallest) start point.
early_ra::color_info *
early_ra::find_oldest_color (unsigned int first_color,
			     unsigned int fpr_conflicts)
{
  color_info *best = nullptr;
  unsigned int best_start_point = ~0U;
  unsigned int best_recency = 0;
  for (unsigned int ci = first_color; ci < m_colors.length (); ++ci)
    {
      auto *color = m_colors[ci];
      unsigned int fpr = color->hard_regno - V0_REGNUM;
      if (fpr_conflicts & (1U << fpr))
	continue;
      unsigned int start_point = 0;
      if (color->group)
	{
	  auto chain_head = color->group->chain_heads ()[0];
	  start_point = m_allocnos[chain_head]->start_point;
	}
      unsigned int recency = m_fpr_recency[fpr];
      if (!best
	  || best_start_point > start_point
	  || (best_start_point == start_point && recency < best_recency))
	{
	  best = color;
	  best_start_point = start_point;
	  best_recency = recency;
	}
    }
  return best;
}

// If there are some spare FPRs that can be reused without introducing saves,
// restores, or moves, use them to "broaden" the allocation, in order to give
// the scheduler more freedom.  This is particularly useful for forming LDPs
// and STPs.
void
early_ra::broaden_colors ()
{
  // Create dummy colors for every leftover FPR that can be used cheaply.
  unsigned int first_color = m_colors.length ();
  for (unsigned int fpr = 0; fpr < 32; ++fpr)
    if (((m_allocated_fprs | m_call_preserved_fprs) & (1U << fpr)) == 0)
      {
	auto *color = region_allocate<color_info> ();
	color->id = m_colors.length ();
	color->hard_regno = V0_REGNUM + fpr;
	color->group = nullptr;
	m_colors.safe_push (color);
      }

  // Exit early if there are no spare FPRs.
  if (first_color == m_colors.length ())
    return;

  // Go through the allocnos in order, seeing if there is a subchain of
  // single-FPR allocnos that can be split off from the containingg clique.
  // Allocate such subchains to the new colors on an oldest-first basis.
  for (auto *allocno : m_sorted_allocnos)
    if (auto *start_allocno = find_independent_subchain (allocno))
      {
	unsigned int fpr_conflicts = 0;
	auto *member = allocno;
	for (;;)
	  {
	    fpr_conflicts |= ~member->group ()->fpr_candidates;
	    if (member == start_allocno)
	      break;
	    member = m_allocnos[member->chain_prev];
	  }

	auto *color = find_oldest_color (first_color, fpr_conflicts);
	if (!color)
	  continue;

	if (!color->group)
	  {
	    auto *group = allocno->group ();
	    color->group = group;
	    group->color = color->id;
	    group->chain_heads ()[0] = INVALID_ALLOCNO;
	  }
	else
	  {
	    auto chain_head = color->group->chain_heads ()[0];
	    auto start_point = m_allocnos[chain_head]->start_point;
	    if (start_point >= allocno->end_point)
	      // Allocating to COLOR isn't viable, and it was the best
	      // option available.
	      continue;

	    auto *next_allocno = chain_next (allocno);
	    if (!next_allocno || next_allocno->start_point <= start_point)
	      // The current allocation gives at least as much scheduling
	      // freedom as COLOR would.
	      continue;
	  }

	// Unlink the chain.
	if (auto *next_allocno = chain_next (allocno))
	  next_allocno->chain_prev = start_allocno->chain_prev;
	if (auto *prev_allocno = chain_prev (start_allocno))
	  prev_allocno->chain_next = allocno->chain_next;

	// Make the subchain use COLOR.
	allocno->chain_next = color->group->chain_heads ()[0];
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "Moving to optional color %d (register %s):",
		   color->id, reg_names[color->hard_regno]);
	for (;;)
	  {
	    auto *group = allocno->group ();
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, " r%d", group->regno);
	    group->m_color_rep = color->group;
	    group->color_rep_offset = 0;
	    if (allocno == start_allocno)
	      break;
	    allocno = m_allocnos[allocno->chain_prev];
	  }
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "\n");
	color->group->chain_heads ()[0] = start_allocno->id;
      }
}

// Record the final choice of hard register for each allocno.
void
early_ra::finalize_allocation ()
{
  for (auto *color : m_colors)
    if (color->group)
      {
	unsigned int fpr = color->hard_regno - V0_REGNUM;
	for (unsigned int i = 0; i < color->group->size; ++i)
	  m_fpr_recency[fpr + i] = m_current_region;
      }
  for (auto *allocno : m_allocnos)
    {
      if (allocno->is_shared ())
	continue;
      auto *group = allocno->group ();
      auto *rep = group->color_rep ();
      auto rep_regno = m_colors[rep->color]->hard_regno;
      auto group_regno = rep_regno + group->color_rep_offset;
      allocno->hard_regno = group_regno + allocno->offset * group->stride;
    }
  for (auto *allocno : m_shared_allocnos)
    allocno->hard_regno = m_allocnos[allocno->related_allocno]->hard_regno;
}

// Replace any allocno references in REFS with the allocated register.
// INSN is the instruction that contains REFS.
bool
early_ra::replace_regs (rtx_insn *insn, df_ref refs)
{
  bool changed = false;
  for (df_ref ref = refs; ref; ref = DF_REF_NEXT_LOC (ref))
    {
      auto range = get_allocno_subgroup (DF_REF_REG (ref));
      if (!range)
	continue;

      auto new_regno = range.allocno (0)->hard_regno;
      if (new_regno == FIRST_PSEUDO_REGISTER)
	{
	  // Reset a debug instruction if, after DCE, the only remaining
	  // references to a register are in such instructions.
	  gcc_assert (DEBUG_INSN_P (insn));
	  INSN_VAR_LOCATION_LOC (insn) = gen_rtx_UNKNOWN_VAR_LOC ();
	  return true;
	}
      *DF_REF_LOC (ref) = gen_rtx_REG (GET_MODE (DF_REF_REG (ref)), new_regno);
      changed = true;
    }
  return changed;
}

// Try to make INSN match its FPR-related constraints.  If this needs
// a source operand (SRC) to be copied to a destination operand (DEST)
// before INSN, add the associated (DEST, SRC) pairs to MOVES.
//
// Return -1 on failure, otherwise return a ?/!-style reject count.
// The reject count doesn't model the moves, just the internal alternative
// preferences.
int
early_ra::try_enforce_constraints (rtx_insn *insn,
				   vec<std::pair<int, int>> &moves)
{
  if (!constrain_operands (0, get_preferred_alternatives (insn)))
    return -1;

  // Pick the alternative with the lowest cost.
  int best = -1;
  auto alts = get_preferred_alternatives (insn);
  for (int altno = 0; altno < recog_data.n_alternatives; ++altno)
    {
      if (!(alts & ALTERNATIVE_BIT (altno)))
	continue;

      auto *op_alt = &recog_op_alt[altno * recog_data.n_operands];
      if (!likely_alternative_match_p (op_alt))
	continue;

      auto_vec<std::pair<int, int>, 4> new_moves;
      for (int opno = 0; opno < recog_data.n_operands; ++opno)
	{
	  rtx op = recog_data.operand[opno];
	  if (REG_P (op)
	      && FP_REGNUM_P (REGNO (op))
	      && op_alt[opno].matched >= 0)
	    {
	      rtx old_src = recog_data.operand[op_alt[opno].matched];
	      if (!operands_match_p (op, old_src))
		{
		  for (int i = 0; i < recog_data.n_operands; ++i)
		    if (i != opno)
		      {
			rtx other = recog_data.operand[i];
			if (reg_overlap_mentioned_p (op, other))
			  {
			    old_src = NULL_RTX;
			    break;
			  }
		      }
		  if (!old_src)
		    continue;
		  new_moves.safe_push ({ opno, op_alt[opno].matched });
		}
	    }
	}
      int cost = count_rejects (op_alt) + new_moves.length () * 7;
      if (best < 0 || cost < best)
	{
	  best = cost;
	  moves.truncate (0);
	  moves.safe_splice (new_moves);
	}
    }
  return best;
}

// Make INSN matches its FPR-related constraints.
void
early_ra::enforce_constraints (rtx_insn *insn)
{
  extract_insn (insn);
  preprocess_constraints (insn);

  // First try with the operands they are.
  auto_vec<std::pair<int, int>, 4> moves;
  int cost = try_enforce_constraints (insn, moves);

  // Next try taking advantage of commutativity.
  for (int opno = 0; opno < recog_data.n_operands - 1; ++opno)
    if (recog_data.constraints[opno][0] == '%')
      {
	std::swap (*recog_data.operand_loc[opno],
		   *recog_data.operand_loc[opno + 1]);
	std::swap (recog_data.operand[opno],
		   recog_data.operand[opno + 1]);
	auto_vec<std::pair<int, int>, 4> swapped_moves;
	int swapped_cost = try_enforce_constraints (insn, swapped_moves);
	if (swapped_cost >= 0 && (cost < 0 || swapped_cost < cost))
	  {
	    cost = swapped_cost;
	    moves.truncate (0);
	    moves.safe_splice (swapped_moves);
	  }
	else
	  {
	    std::swap (*recog_data.operand_loc[opno],
		       *recog_data.operand_loc[opno + 1]);
	    std::swap (recog_data.operand[opno],
		       recog_data.operand[opno + 1]);
	  }
      }

  // The allocation should ensure that there is at least one valid combination.
  // It's too late to back out now if not.
  gcc_assert (cost >= 0);
  for (int i = 0; i < recog_data.n_dups; ++i)
    {
      int dup_of = recog_data.dup_num[i];
      rtx new_op = *recog_data.operand_loc[dup_of];
      if (new_op != recog_data.operand[dup_of])
	*recog_data.dup_loc[i] = copy_rtx (new_op);
    }
  for (auto move : moves)
    {
      int dest_opno = move.first;
      int src_opno = move.second;
      rtx dest = recog_data.operand[dest_opno];
      rtx old_src = recog_data.operand[src_opno];
      rtx new_src = lowpart_subreg (GET_MODE (old_src), dest, GET_MODE (dest));
      emit_insn_before (gen_move_insn (new_src, old_src), insn);
      *recog_data.operand_loc[src_opno] = new_src;
    }
}

// See whether INSN is an instruction that operates on multi-register vectors,
// and if we have decided to make it use strided rather than consecutive
// accesses.  Update the pattern and return true if so.
bool
early_ra::maybe_convert_to_strided_access (rtx_insn *insn)
{
  if (!NONJUMP_INSN_P (insn) || recog_memoized (insn) < 0)
    return false;

  auto stride_type = get_attr_stride_type (insn);
  rtx pat = PATTERN (insn);
  rtx op;
  if (TARGET_STREAMING_SME2 && stride_type == STRIDE_TYPE_LD1_CONSECUTIVE)
    op = SET_DEST (pat);
  else if (TARGET_STREAMING_SME2 && stride_type == STRIDE_TYPE_ST1_CONSECUTIVE)
    op = XVECEXP (SET_SRC (pat), 0, 1);
  else
    return false;

  auto range = get_allocno_subgroup (op);
  if (!range || range.group->stride == 1)
    return false;

  gcc_assert (range.start == 0 && range.count == range.group->size);
  auto elt_mode = GET_MODE_INNER (GET_MODE (op));
  auto single_mode = aarch64_full_sve_mode (elt_mode).require ();
  auto_vec<rtx, 4> regs;
  for (unsigned int i = 0; i < range.count; ++i)
    regs.quick_push (gen_rtx_REG (single_mode, range.allocno (i)->hard_regno));

  extract_insn (insn);
  if (stride_type == STRIDE_TYPE_LD1_CONSECUTIVE)
    {
      auto unspec = XINT (SET_SRC (pat), 1);
      if (range.count == 2)
	pat = gen_aarch64_strided2 (unspec, GET_MODE (op), regs[0], regs[1],
				    recog_data.operand[1],
				    recog_data.operand[2]);
      else
	pat = gen_aarch64_strided4 (unspec, GET_MODE (op),
				    regs[0], regs[1], regs[2], regs[3],
				    recog_data.operand[1],
				    recog_data.operand[2]);
    }
  else if (stride_type == STRIDE_TYPE_ST1_CONSECUTIVE)
    {
      auto unspec = XINT (SET_SRC (pat), 1);
      if (range.count == 2)
	pat = gen_aarch64_strided2 (unspec, GET_MODE (op),
				    recog_data.operand[0],
				    recog_data.operand[2], regs[0], regs[1]);
      else
	pat = gen_aarch64_strided4 (unspec, GET_MODE (op),
				    recog_data.operand[0],
				    recog_data.operand[2],
				    regs[0], regs[1], regs[2], regs[3]);
      // Ensure correct sharing for the source memory.
      //
      // ??? Why doesn't the generator get this right?
      XVECEXP (SET_SRC (pat), 0, XVECLEN (SET_SRC (pat), 0) - 1)
	= *recog_data.dup_loc[0];
    }
  else
    gcc_unreachable ();
  PATTERN (insn) = pat;
  INSN_CODE (insn) = -1;
  df_insn_rescan (insn);
  return true;
}

// We've successfully allocated the current region.  Apply the allocation
// to the instructions.
void
early_ra::apply_allocation ()
{
  for (auto *insn : m_dead_insns)
    set_insn_deleted (insn);

  rtx_insn *prev;
  for (auto insn_range : m_insn_ranges)
    for (rtx_insn *insn = insn_range.first;
	 insn != insn_range.second;
	 insn = prev)
      {
	prev = PREV_INSN (insn);
	if (!INSN_P (insn))
	  continue;

	bool changed = maybe_convert_to_strided_access (insn);
	changed |= replace_regs (insn, DF_INSN_DEFS (insn));
	changed |= replace_regs (insn, DF_INSN_USES (insn));
	if (changed && NONDEBUG_INSN_P (insn))
	  {
	    if (GET_CODE (PATTERN (insn)) != USE
		&& GET_CODE (PATTERN (insn)) != CLOBBER
		&& !is_move_set (PATTERN (insn)))
	      enforce_constraints (insn);

	    // A REG_EQUIV note establishes an equivalence throughout
	    // the function, but here we're reusing hard registers for
	    // multiple pseudo registers.  We also no longer need REG_EQUIV
	    // notes that record potential spill locations, since we've
	    // allocated the pseudo register without spilling.
	    rtx *ptr = &REG_NOTES (insn);
	    while (*ptr)
	      if (REG_NOTE_KIND (*ptr) == REG_EQUIV)
		*ptr = XEXP (*ptr, 1);
	      else
		ptr = &XEXP (*ptr, 1);
	  }
	changed |= replace_regs (insn, DF_INSN_EQ_USES (insn));
	if (changed)
	  df_insn_rescan (insn);
      }

  for (auto *insn : m_dead_insns)
    delete_insn (insn);
}

// Try to allocate the current region.  Update the instructions if successful.
void
early_ra::process_region ()
{
  for (auto *allocno : m_allocnos)
    {
      allocno->chain_next = INVALID_ALLOCNO;
      allocno->chain_prev = INVALID_ALLOCNO;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_fpr_ranges ();
      dump_copies ();
      dump_allocnos ();
    }

  find_strided_accesses ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_allocnos ();

  form_chains ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_allocnos ();

  process_copies ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_colors ();

  allocate_colors ();
  if (!m_allocation_successful)
    return;

  broaden_colors ();
  finalize_allocation ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nAllocation successful\nFinal allocation:\n");
      dump_allocnos ();
      dump_colors ();
    }

  apply_allocation ();
}

// Return true if INSN would become dead if we successfully allocate the
// current region.
bool
early_ra::is_dead_insn (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return false;

  rtx dest = SET_DEST (set);
  auto dest_range = get_allocno_subgroup (dest);
  if (!dest_range)
    return false;

  for (auto &allocno : dest_range.allocnos ())
    if (bitmap_bit_p (m_live_allocnos, allocno.id))
      return false;

  if (side_effects_p (set))
    return false;

  /* If we can't delete dead exceptions and the insn throws,
     then the instruction is not dead.  */
  if (!cfun->can_delete_dead_exceptions
      && !insn_nothrow_p (insn))
    return false;

  return true;
}

// Return true if we could split a single-block region into two regions
// at the current program point.  This is true if the block up to this
// point is worth treating as an independent region and if no registers
// that would affect the allocation are currently live.
inline bool
early_ra::could_split_region_here ()
{
  return (m_accurate_live_ranges
	  && bitmap_empty_p (m_live_allocnos)
	  && m_live_fprs == 0
	  && !m_allocnos.is_empty ());
}

// Return true if any of the pseudos defined by INSN are also defined
// elsewhere.
static bool
defines_multi_def_pseudo (rtx_insn *insn)
{
  df_ref ref;

  FOR_EACH_INSN_DEF (ref, insn)
    {
      unsigned int regno = DF_REF_REGNO (ref);
      if (!HARD_REGISTER_NUM_P (regno) && DF_REG_DEF_COUNT (regno) > 1)
	return true;
    }
  return false;
}

// Build up information about block BB.  IS_ISOLATED is true if the
// block is not part of a larger region.
void
early_ra::process_block (basic_block bb, bool is_isolated)
{
  m_current_bb = bb;
  m_current_point += 1;
  m_current_bb_point = m_current_point;

  // Process live-out FPRs.
  bitmap live_out = df_get_live_out (bb);
  for (unsigned int regno = V0_REGNUM; regno <= V31_REGNUM; ++regno)
    if (bitmap_bit_p (live_out, regno))
      record_fpr_use (regno);

  // Process live-out allocnos.  We don't track individual FPR liveness
  // across block boundaries, so we have to assume that the whole pseudo
  // register is live.
  bitmap_iterator bi;
  unsigned int regno;
  EXECUTE_IF_AND_IN_BITMAP (df_get_live_out (bb), m_fpr_pseudos,
			    FIRST_PSEUDO_REGISTER, regno, bi)
    {
      auto range = get_allocno_subgroup (regno_reg_rtx[regno]);
      for (auto &allocno : range.allocnos ())
	record_allocno_use (&allocno);
    }

  m_current_point += 1;

  record_artificial_refs (0);

  bool is_first = true;
  rtx_insn *start_insn = BB_END (bb);
  rtx_insn *insn;
  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      // CLOBBERs are used to prevent pseudos from being upwards exposed.
      // We can ignore them if allocation is successful.
      if (GET_CODE (PATTERN (insn)) == CLOBBER)
	{
	  if (get_allocno_subgroup (XEXP (PATTERN (insn), 0)))
	    m_dead_insns.safe_push (insn);
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (is_first)
	    fprintf (dump_file, "\nBlock %d:\n", bb->index);
	  fprintf (dump_file, "%6d:", m_current_point);
	  pretty_printer rtl_slim_pp;
	  rtl_slim_pp.set_output_stream (dump_file);
	  print_insn (&rtl_slim_pp, insn, 1);
	  pp_flush (&rtl_slim_pp);
	  fprintf (dump_file, "\n");
	}
      is_first = false;

      if (is_dead_insn (insn))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "%14s -- dead\n", "");
	  m_dead_insns.safe_push (insn);
	}
      else
	{
	  record_insn_defs (insn);

	  // If we've decided not to allocate the current region, and if
	  // all relevant registers are now dead, consider splitting the
	  // block into two regions at this point.
	  //
	  // This is useful when a sequence of vector code ends in a
	  // vector-to-scalar-integer operation.  We wouldn't normally
	  // want to allocate the scalar integer, since IRA is much better
	  // at handling cross-file moves.  But if nothing of relevance is
	  // live between the death of the final vector and the birth of the
	  // scalar integer, we can try to split the region at that point.
	  // We'd then allocate the vector input and leave the real RA
	  // to handle the scalar output.
	  if (is_isolated
	      && !m_allocation_successful
	      && could_split_region_here ()
	      && !defines_multi_def_pseudo (insn))
	    {
	      // Mark that we've deliberately chosen to leave the output
	      // pseudos to the real RA.
	      df_ref ref;
	      FOR_EACH_INSN_DEF (ref, insn)
		{
		  unsigned int regno = DF_REF_REGNO (ref);
		  if (!HARD_REGISTER_NUM_P (regno))
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "Ignoring register r%d\n", regno);
		      m_pseudo_regs[regno].flags |= IGNORE_REG;
		      bitmap_clear_bit (m_fpr_pseudos, regno);
		    }
		}

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "\nStarting new region at insn %d\n",
			 INSN_UID (insn));

	      // Start a new region and replay the definitions.  The replay
	      // is needed if the instruction sets a hard FP register.
	      start_new_region ();
	      record_insn_defs (insn);
	      start_insn = insn;
	    }

	  if (auto *call_insn = dyn_cast<rtx_call_insn *> (insn))
	    record_insn_call (call_insn);
	  record_insn_uses (insn);
	  rtx pat = PATTERN (insn);
	  if (is_move_set (pat))
	    record_copy (SET_DEST (pat), SET_SRC (pat), true);
	  else
	    {
	      extract_insn (insn);
	      record_constraints (insn);
	    }
	}

      // See whether we have a complete region, with no remaining live
      // allocnos.
      if (is_isolated && could_split_region_here ())
	{
	  rtx_insn *prev_insn = PREV_INSN (insn);
	  if (m_allocation_successful)
	    {
	      m_insn_ranges.safe_push ({ start_insn, prev_insn });
	      process_region ();
	    }
	  start_new_region ();
	  is_first = true;
	  start_insn = prev_insn;
	}
    }
  m_insn_ranges.safe_push ({ start_insn, BB_HEAD (bb) });

  record_artificial_refs (DF_REF_AT_TOP);

  // Process live-in FPRs.
  bitmap live_in = df_get_live_in (bb);
  for (unsigned int regno = V0_REGNUM; regno <= V31_REGNUM; ++regno)
    if (bitmap_bit_p (live_in, regno)
	&& (m_live_fprs & (1U << (regno - V0_REGNUM))))
      record_fpr_def (regno);

  // Process live-in allocnos.
  EXECUTE_IF_AND_IN_BITMAP (live_in, m_fpr_pseudos,
			    FIRST_PSEUDO_REGISTER, regno, bi)
    {
      auto range = get_allocno_subgroup (regno_reg_rtx[regno]);
      for (auto &allocno : range.allocnos ())
	if (bitmap_bit_p (m_live_allocnos, allocno.id))
	  record_allocno_def (&allocno);
    }

  m_current_point += 1;

  bitmap_clear (m_live_allocnos);
  m_live_fprs = 0;
}

// Divide the function into regions, such that there no edges into or out
// of the region have live "FPR pseudos".
void
early_ra::process_blocks ()
{
  auto_sbitmap visited (last_basic_block_for_fn (m_fn));
  auto_sbitmap fpr_pseudos_live_out (last_basic_block_for_fn (m_fn));
  auto_sbitmap fpr_pseudos_live_in (last_basic_block_for_fn (m_fn));

  bitmap_clear (visited);
  bitmap_clear (fpr_pseudos_live_out);
  bitmap_clear (fpr_pseudos_live_in);

  // Record which blocks have live FPR pseudos on entry and exit.
  basic_block bb;
  FOR_EACH_BB_FN (bb, m_fn)
    {
      if (bitmap_intersect_p (df_get_live_out (bb), m_fpr_pseudos))
	bitmap_set_bit (fpr_pseudos_live_out, bb->index);
      if (bitmap_intersect_p (df_get_live_in (bb), m_fpr_pseudos))
	bitmap_set_bit (fpr_pseudos_live_in, bb->index);
    }

  // This is incremented by 1 at the start of each region.
  m_current_region = 0;
  memset (m_fpr_recency, 0, sizeof (m_fpr_recency));

  struct stack_node { edge_iterator ei; basic_block bb; };

  auto_vec<stack_node, 32> stack;
  auto_vec<basic_block, 32> region;

  // Go through the function in reverse postorder and process the region
  // containing each block.
  unsigned int n_blocks = df_get_n_blocks (DF_FORWARD);
  int *order = df_get_postorder (DF_FORWARD);
  for (unsigned int bbi = 0; bbi < n_blocks; ++bbi)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (m_fn, order[bbi]);
      if (bb->index < NUM_FIXED_BLOCKS)
	continue;

      if (!bitmap_set_bit (visited, bb->index))
	continue;

      // Process forward edges before backward edges (so push backward
      // edges first).  Build the region in an approximation of reverse
      // program order.
      if (bitmap_bit_p (fpr_pseudos_live_in, bb->index))
	stack.quick_push ({ ei_start (bb->preds), nullptr });
      if (bitmap_bit_p (fpr_pseudos_live_out, bb->index))
	stack.quick_push ({ ei_start (bb->succs), bb });
      else
	region.safe_push (bb);
      while (!stack.is_empty ())
	{
	  auto &node = stack.last ();
	  if (ei_end_p (node.ei))
	    {
	      if (node.bb)
		region.safe_push (node.bb);
	      stack.pop ();
	      continue;
	    }
	  edge e = ei_edge (node.ei);
	  if (node.bb)
	    {
	      // A forward edge from a node that has not yet been added
	      // to region.
	      if (bitmap_bit_p (fpr_pseudos_live_in, e->dest->index)
		  && bitmap_set_bit (visited, e->dest->index))
		{
		  stack.safe_push ({ ei_start (e->dest->preds), nullptr });
		  if (bitmap_bit_p (fpr_pseudos_live_out, e->dest->index))
		    stack.safe_push ({ ei_start (e->dest->succs), e->dest });
		  else
		    region.safe_push (e->dest);
		}
	      else
		ei_next (&node.ei);
	    }
	  else
	    {
	      // A backward edge from a node that has already been added
	      // to the region.
	      if (bitmap_bit_p (fpr_pseudos_live_out, e->src->index)
		  && bitmap_set_bit (visited, e->src->index))
		{
		  if (bitmap_bit_p (fpr_pseudos_live_in, e->src->index))
		    stack.safe_push ({ ei_start (e->src->preds), nullptr });
		  stack.safe_push ({ ei_start (e->src->succs), e->src });
		}
	      else
		ei_next (&node.ei);
	    }
	}

      m_current_point = 2;
      start_new_region ();

      if (region.is_empty ())
	process_block (bb, true);
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\nRegion (from %d):", bb->index);
	      for (unsigned int j = 0; j < region.length (); ++j)
		fprintf (dump_file, " %d", region[j]->index);
	      fprintf (dump_file, "\n");
	    }
	  for (unsigned int j = 0; j < region.length (); ++j)
	    {
	      basic_block bb = region[j];
	      bool is_isolated
		= ((j == 0 && !bitmap_bit_p (fpr_pseudos_live_out, bb->index))
		   || (j == region.length () - 1
		       && !bitmap_bit_p (fpr_pseudos_live_in, bb->index)));
	      process_block (bb, is_isolated);
	    }
	}
      region.truncate (0);

      if (!m_allocnos.is_empty () && m_allocation_successful)
	process_region ();
    }
}

// Run the pass on the current function.
void
early_ra::execute ()
{
  df_analyze ();

  preprocess_insns ();
  propagate_pseudo_reg_info ();
  choose_fpr_pseudos ();
  if (bitmap_empty_p (m_fpr_pseudos))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_pseudo_regs ();

  process_blocks ();
  df_verify ();
}

class pass_early_ra : public rtl_opt_pass
{
public:
  pass_early_ra (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_early_ra, ctxt)
  {}

  // opt_pass methods:
  virtual bool gate (function *);
  virtual unsigned int execute (function *);
};

bool
pass_early_ra::gate (function *)
{
  // Require a vector ISA to be enabled.
  if (!TARGET_SIMD && !TARGET_SVE)
    return false;

  if (aarch64_early_ra == AARCH64_EARLY_RA_NONE)
    return false;

  if (aarch64_early_ra == AARCH64_EARLY_RA_STRIDED
      && !TARGET_STREAMING_SME2)
    return false;

  return true;
}

unsigned int
pass_early_ra::execute (function *fn)
{
  early_ra (fn).execute ();
  return 0;
}

} // end namespace

// Create a new instance of the pass.
rtl_opt_pass *
make_pass_aarch64_early_ra (gcc::context *ctxt)
{
  return new pass_early_ra (ctxt);
}
