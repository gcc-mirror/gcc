// LoadPair fusion optimization pass for AArch64.
// Copyright (C) 2023-2024 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_LIST
#define INCLUDE_TYPE_TRAITS
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "rtl-iter.h"
#include "rtl-ssa.h"
#include "cfgcleanup.h"
#include "tree-pass.h"
#include "ordered-hash-map.h"
#include "tree-dfa.h"
#include "fold-const.h"
#include "tree-hash-traits.h"
#include "print-tree.h"
#include "insn-attr.h"

using namespace rtl_ssa;

static constexpr HOST_WIDE_INT LDP_IMM_BITS = 7;
static constexpr HOST_WIDE_INT LDP_IMM_SIGN_BIT = (1 << (LDP_IMM_BITS - 1));
static constexpr HOST_WIDE_INT LDP_MAX_IMM = LDP_IMM_SIGN_BIT - 1;
static constexpr HOST_WIDE_INT LDP_MIN_IMM = -LDP_MAX_IMM - 1;

// We pack these fields (load_p, fpsimd_p, and size) into an integer
// (LFS) which we use as part of the key into the main hash tables.
//
// The idea is that we group candidates together only if they agree on
// the fields below.  Candidates that disagree on any of these
// properties shouldn't be merged together.
struct lfs_fields
{
  bool load_p;
  bool fpsimd_p;
  unsigned size;
};

using insn_list_t = std::list<insn_info *>;
using insn_iter_t = insn_list_t::iterator;

// Information about the accesses at a given offset from a particular
// base.  Stored in an access_group, see below.
struct access_record
{
  poly_int64 offset;
  std::list<insn_info *> cand_insns;
  std::list<access_record>::iterator place;

  access_record (poly_int64 off) : offset (off) {}
};

// A group of accesses where adjacent accesses could be ldp/stp
// candidates.  The splay tree supports efficient insertion,
// while the list supports efficient iteration.
struct access_group
{
  splay_tree<access_record *> tree;
  std::list<access_record> list;

  template<typename Alloc>
  inline void track (Alloc node_alloc, poly_int64 offset, insn_info *insn);
};

// Information about a potential base candidate, used in try_fuse_pair.
// There may be zero, one, or two viable RTL bases for a given pair.
struct base_cand
{
  // DEF is the def of the base register to be used by the pair.
  def_info *def;

  // FROM_INSN is -1 if the base candidate is already shared by both
  // candidate insns.  Otherwise it holds the index of the insn from
  // which the base originated.
  //
  // In the case that the base is shared, either DEF is already used
  // by both candidate accesses, or both accesses see different versions
  // of the same regno, in which case DEF is the def consumed by the
  // first candidate access.
  int from_insn;

  // To form a pair, we do so by moving the first access down and the second
  // access up.  To determine where to form the pair, and whether or not
  // it is safe to form the pair, we track instructions which cannot be
  // re-ordered past due to either dataflow or alias hazards.
  //
  // Since we allow changing the base used by an access, the choice of
  // base can change which instructions act as re-ordering hazards for
  // this pair (due to different dataflow).  We store the initial
  // dataflow hazards for this choice of base candidate in HAZARDS.
  //
  // These hazards act as re-ordering barriers to each candidate insn
  // respectively, in program order.
  //
  // Later on, when we take alias analysis into account, we narrow
  // HAZARDS accordingly.
  insn_info *hazards[2];

  base_cand (def_info *def, int insn)
    : def (def), from_insn (insn), hazards {nullptr, nullptr} {}

  base_cand (def_info *def) : base_cand (def, -1) {}

  // Test if this base candidate is viable according to HAZARDS.
  bool viable () const
  {
    return !hazards[0] || !hazards[1] || (*hazards[0] > *hazards[1]);
  }
};

// Information about an alternate base.  For a def_info D, it may
// instead be expressed as D = BASE + OFFSET.
struct alt_base
{
  def_info *base;
  poly_int64 offset;
};

// State used by the pass for a given basic block.
struct ldp_bb_info
{
  using def_hash = nofree_ptr_hash<def_info>;
  using expr_key_t = pair_hash<tree_operand_hash, int_hash<int, -1, -2>>;
  using def_key_t = pair_hash<def_hash, int_hash<int, -1, -2>>;

  // Map of <tree base, LFS> -> access_group.
  ordered_hash_map<expr_key_t, access_group> expr_map;

  // Map of <RTL-SSA def_info *, LFS> -> access_group.
  ordered_hash_map<def_key_t, access_group> def_map;

  // Given the def_info for an RTL base register, express it as an offset from
  // some canonical base instead.
  //
  // Canonicalizing bases in this way allows us to identify adjacent accesses
  // even if they see different base register defs.
  hash_map<def_hash, alt_base> canon_base_map;

  static const size_t obstack_alignment = sizeof (void *);
  bb_info *m_bb;

  ldp_bb_info (bb_info *bb) : m_bb (bb), m_emitted_tombstone (false)
  {
    obstack_specify_allocation (&m_obstack, OBSTACK_CHUNK_SIZE,
				obstack_alignment, obstack_chunk_alloc,
				obstack_chunk_free);
  }
  ~ldp_bb_info ()
  {
    obstack_free (&m_obstack, nullptr);

    if (m_emitted_tombstone)
      {
	bitmap_release (&m_tombstone_bitmap);
	bitmap_obstack_release (&m_bitmap_obstack);
      }
  }

  inline void track_access (insn_info *, bool load, rtx mem);
  inline void transform ();
  inline void cleanup_tombstones ();

private:
  obstack m_obstack;

  // State for keeping track of tombstone insns emitted for this BB.
  bitmap_obstack m_bitmap_obstack;
  bitmap_head m_tombstone_bitmap;
  bool m_emitted_tombstone;

  inline splay_tree_node<access_record *> *node_alloc (access_record *);

  template<typename Map>
  inline void traverse_base_map (Map &map);
  inline void transform_for_base (int load_size, access_group &group);

  inline void merge_pairs (insn_list_t &, insn_list_t &,
			   bool load_p, unsigned access_size);

  inline bool try_fuse_pair (bool load_p, unsigned access_size,
			     insn_info *i1, insn_info *i2);

  inline bool fuse_pair (bool load_p, unsigned access_size,
			 int writeback,
			 insn_info *i1, insn_info *i2,
			 base_cand &base,
			 const insn_range_info &move_range);

  inline void track_tombstone (int uid);

  inline bool track_via_mem_expr (insn_info *, rtx mem, lfs_fields lfs);
};

splay_tree_node<access_record *> *
ldp_bb_info::node_alloc (access_record *access)
{
  using T = splay_tree_node<access_record *>;
  void *addr = obstack_alloc (&m_obstack, sizeof (T));
  return new (addr) T (access);
}

// Given a mem MEM, if the address has side effects, return a MEM that accesses
// the same address but without the side effects.  Otherwise, return
// MEM unchanged.
static rtx
drop_writeback (rtx mem)
{
  rtx addr = XEXP (mem, 0);

  if (!side_effects_p (addr))
    return mem;

  switch (GET_CODE (addr))
    {
    case PRE_MODIFY:
      addr = XEXP (addr, 1);
      break;
    case POST_MODIFY:
    case POST_INC:
    case POST_DEC:
      addr = XEXP (addr, 0);
      break;
    case PRE_INC:
    case PRE_DEC:
    {
      poly_int64 adjustment = GET_MODE_SIZE (GET_MODE (mem));
      if (GET_CODE (addr) == PRE_DEC)
	adjustment *= -1;
      addr = plus_constant (GET_MODE (addr), XEXP (addr, 0), adjustment);
      break;
    }
    default:
      gcc_unreachable ();
    }

  return change_address (mem, GET_MODE (mem), addr);
}

// Convenience wrapper around strip_offset that can also look through
// RTX_AUTOINC addresses.  The interface is like strip_offset except we take a
// MEM so that we know the mode of the access.
static rtx
ldp_strip_offset (rtx mem, poly_int64 *offset)
{
  rtx addr = XEXP (mem, 0);

  switch (GET_CODE (addr))
    {
    case PRE_MODIFY:
    case POST_MODIFY:
      addr = strip_offset (XEXP (addr, 1), offset);
      gcc_checking_assert (REG_P (addr));
      gcc_checking_assert (rtx_equal_p (XEXP (XEXP (mem, 0), 0), addr));
      break;
    case PRE_INC:
    case POST_INC:
      addr = XEXP (addr, 0);
      *offset = GET_MODE_SIZE (GET_MODE (mem));
      gcc_checking_assert (REG_P (addr));
      break;
    case PRE_DEC:
    case POST_DEC:
      addr = XEXP (addr, 0);
      *offset = -GET_MODE_SIZE (GET_MODE (mem));
      gcc_checking_assert (REG_P (addr));
      break;

    default:
      addr = strip_offset (addr, offset);
    }

  return addr;
}

// Return true if X is a PRE_{INC,DEC,MODIFY} rtx.
static bool
any_pre_modify_p (rtx x)
{
  const auto code = GET_CODE (x);
  return code == PRE_INC || code == PRE_DEC || code == PRE_MODIFY;
}

// Return true if X is a POST_{INC,DEC,MODIFY} rtx.
static bool
any_post_modify_p (rtx x)
{
  const auto code = GET_CODE (x);
  return code == POST_INC || code == POST_DEC || code == POST_MODIFY;
}

// Return true if we should consider forming ldp/stp insns from memory
// accesses with operand mode MODE at this stage in compilation.
static bool
ldp_operand_mode_ok_p (machine_mode mode)
{
  const bool allow_qregs
    = !(aarch64_tune_params.extra_tuning_flags
	& AARCH64_EXTRA_TUNE_NO_LDP_STP_QREGS);

  if (!aarch64_ldpstp_operand_mode_p (mode))
    return false;

  const auto size = GET_MODE_SIZE (mode).to_constant ();
  if (size == 16 && !allow_qregs)
    return false;

  // We don't pair up TImode accesses before RA because TImode is
  // special in that it can be allocated to a pair of GPRs or a single
  // FPR, and the RA is best placed to make that decision.
  return reload_completed || mode != TImode;
}

// Given LFS (load_p, fpsimd_p, size) fields in FIELDS, encode these
// into an integer for use as a hash table key.
static int
encode_lfs (lfs_fields fields)
{
  int size_log2 = exact_log2 (fields.size);
  gcc_checking_assert (size_log2 >= 2 && size_log2 <= 4);
  return ((int)fields.load_p << 3)
    | ((int)fields.fpsimd_p << 2)
    | (size_log2 - 2);
}

// Inverse of encode_lfs.
static lfs_fields
decode_lfs (int lfs)
{
  bool load_p = (lfs & (1 << 3));
  bool fpsimd_p = (lfs & (1 << 2));
  unsigned size = 1U << ((lfs & 3) + 2);
  return { load_p, fpsimd_p, size };
}

// Track the access INSN at offset OFFSET in this access group.
// ALLOC_NODE is used to allocate splay tree nodes.
template<typename Alloc>
void
access_group::track (Alloc alloc_node, poly_int64 offset, insn_info *insn)
{
  auto insert_before = [&](std::list<access_record>::iterator after)
    {
      auto it = list.emplace (after, offset);
      it->cand_insns.push_back (insn);
      it->place = it;
      return &*it;
    };

  if (!list.size ())
    {
      auto access = insert_before (list.end ());
      tree.insert_max_node (alloc_node (access));
      return;
    }

  auto compare = [&](splay_tree_node<access_record *> *node)
    {
      return compare_sizes_for_sort (offset, node->value ()->offset);
    };
  auto result = tree.lookup (compare);
  splay_tree_node<access_record *> *node = tree.root ();
  if (result == 0)
    node->value ()->cand_insns.push_back (insn);
  else
    {
      auto it = node->value ()->place;
      auto after = (result > 0) ? std::next (it) : it;
      auto access = insert_before (after);
      tree.insert_child (node, result > 0, alloc_node (access));
    }
}

// Given a candidate access INSN (with mem MEM), see if it has a suitable
// MEM_EXPR base (i.e. a tree decl) relative to which we can track the access.
// LFS is used as part of the key to the hash table, see track_access.
bool
ldp_bb_info::track_via_mem_expr (insn_info *insn, rtx mem, lfs_fields lfs)
{
  if (!MEM_EXPR (mem) || !MEM_OFFSET_KNOWN_P (mem))
    return false;

  poly_int64 offset;
  tree base_expr = get_addr_base_and_unit_offset (MEM_EXPR (mem),
						  &offset);
  if (!base_expr || !DECL_P (base_expr))
    return false;

  offset += MEM_OFFSET (mem);

  const machine_mode mem_mode = GET_MODE (mem);
  const HOST_WIDE_INT mem_size = GET_MODE_SIZE (mem_mode).to_constant ();

  // Punt on misaligned offsets.  LDP/STP instructions require offsets to be a
  // multiple of the access size, and we believe that misaligned offsets on
  // MEM_EXPR bases are likely to lead to misaligned offsets w.r.t. RTL bases.
  if (!multiple_p (offset, mem_size))
    return false;

  const auto key = std::make_pair (base_expr, encode_lfs (lfs));
  access_group &group = expr_map.get_or_insert (key, NULL);
  auto alloc = [&](access_record *access) { return node_alloc (access); };
  group.track (alloc, offset, insn);

  if (dump_file)
    {
      fprintf (dump_file, "[bb %u] tracking insn %d via ",
	       m_bb->index (), insn->uid ());
      print_node_brief (dump_file, "mem expr", base_expr, 0);
      fprintf (dump_file, " [L=%d FP=%d, %smode, off=",
	       lfs.load_p, lfs.fpsimd_p, mode_name[mem_mode]);
      print_dec (offset, dump_file);
      fprintf (dump_file, "]\n");
    }

  return true;
}

// Main function to begin pair discovery.  Given a memory access INSN,
// determine whether it could be a candidate for fusing into an ldp/stp,
// and if so, track it in the appropriate data structure for this basic
// block.  LOAD_P is true if the access is a load, and MEM is the mem
// rtx that occurs in INSN.
void
ldp_bb_info::track_access (insn_info *insn, bool load_p, rtx mem)
{
  // We can't combine volatile MEMs, so punt on these.
  if (MEM_VOLATILE_P (mem))
    return;

  // Ignore writeback accesses if the param says to do so.
  if (!aarch64_ldp_writeback
      && GET_RTX_CLASS (GET_CODE (XEXP (mem, 0))) == RTX_AUTOINC)
    return;

  const machine_mode mem_mode = GET_MODE (mem);
  if (!ldp_operand_mode_ok_p (mem_mode))
    return;

  rtx reg_op = XEXP (PATTERN (insn->rtl ()), !load_p);

  // Ignore the access if the register operand isn't suitable for ldp/stp.
  if (load_p
      ? !aarch64_ldp_reg_operand (reg_op, mem_mode)
      : !aarch64_stp_reg_operand (reg_op, mem_mode))
    return;

  // We want to segregate FP/SIMD accesses from GPR accesses.
  //
  // Before RA, we use the modes, noting that stores of constant zero
  // operands use GPRs (even in non-integer modes).  After RA, we use
  // the hard register numbers.
  const bool fpsimd_op_p
    = reload_completed
    ? (REG_P (reg_op) && FP_REGNUM_P (REGNO (reg_op)))
    : (GET_MODE_CLASS (mem_mode) != MODE_INT
       && (load_p || !aarch64_const_zero_rtx_p (reg_op)));

  // Note ldp_operand_mode_ok_p already rejected VL modes.
  const HOST_WIDE_INT mem_size = GET_MODE_SIZE (mem_mode).to_constant ();
  const lfs_fields lfs = { load_p, fpsimd_op_p, mem_size };

  if (track_via_mem_expr (insn, mem, lfs))
    return;

  poly_int64 mem_off;
  rtx addr = XEXP (mem, 0);
  const bool autoinc_p = GET_RTX_CLASS (GET_CODE (addr)) == RTX_AUTOINC;
  rtx base = ldp_strip_offset (mem, &mem_off);
  if (!REG_P (base))
    return;

  // Need to calculate two (possibly different) offsets:
  //  - Offset at which the access occurs.
  //  - Offset of the new base def.
  poly_int64 access_off;
  if (autoinc_p && any_post_modify_p (addr))
    access_off = 0;
  else
    access_off = mem_off;

  poly_int64 new_def_off = mem_off;

  // Punt on accesses relative to eliminable regs.  Since we don't know the
  // elimination offset pre-RA, we should postpone forming pairs on such
  // accesses until after RA.
  //
  // As it stands, addresses with offsets in range for LDR but not
  // in range for LDP/STP are currently reloaded inefficiently,
  // ending up with a separate base register for each pair.
  //
  // In theory LRA should make use of
  // targetm.legitimize_address_displacement to promote sharing of
  // bases among multiple (nearby) address reloads, but the current
  // LRA code returns early from process_address_1 for operands that
  // satisfy "m", even if they don't satisfy the real (relaxed) address
  // constraint; this early return means we never get to the code
  // that calls targetm.legitimize_address_displacement.
  //
  // So for now, it's better to punt when we can't be sure that the
  // offset is in range for LDP/STP.  Out-of-range cases can then be
  // handled after RA by the out-of-range LDP/STP peepholes.  Eventually, it
  // would be nice to handle known out-of-range opportunities in the
  // pass itself (for stack accesses, this would be in the post-RA pass).
  if (!reload_completed
      && (REGNO (base) == FRAME_POINTER_REGNUM
	  || REGNO (base) == ARG_POINTER_REGNUM))
    return;

  // Now need to find def of base register.
  use_info *base_use = find_access (insn->uses (), REGNO (base));
  gcc_assert (base_use);
  def_info *base_def = base_use->def ();
  if (!base_def)
    {
      if (dump_file)
	fprintf (dump_file,
		 "base register (regno %d) of insn %d is undefined",
		 REGNO (base), insn->uid ());
      return;
    }

  alt_base *canon_base = canon_base_map.get (base_def);
  if (canon_base)
    {
      // Express this as the combined offset from the canonical base.
      base_def = canon_base->base;
      new_def_off += canon_base->offset;
      access_off += canon_base->offset;
    }

  if (autoinc_p)
    {
      auto def = find_access (insn->defs (), REGNO (base));
      gcc_assert (def);

      // Record that DEF = BASE_DEF + MEM_OFF.
      if (dump_file)
	{
	  pretty_printer pp;
	  pp_access (&pp, def, 0);
	  pp_string (&pp, " = ");
	  pp_access (&pp, base_def, 0);
	  fprintf (dump_file, "[bb %u] recording %s + ",
		   m_bb->index (), pp_formatted_text (&pp));
	  print_dec (new_def_off, dump_file);
	  fprintf (dump_file, "\n");
	}

      alt_base base_rec { base_def, new_def_off };
      if (canon_base_map.put (def, base_rec))
	gcc_unreachable (); // Base defs should be unique.
    }

  // Punt on misaligned offsets.  LDP/STP require offsets to be a multiple of
  // the access size.
  if (!multiple_p (mem_off, mem_size))
    return;

  const auto key = std::make_pair (base_def, encode_lfs (lfs));
  access_group &group = def_map.get_or_insert (key, NULL);
  auto alloc = [&](access_record *access) { return node_alloc (access); };
  group.track (alloc, access_off, insn);

  if (dump_file)
    {
      pretty_printer pp;
      pp_access (&pp, base_def, 0);

      fprintf (dump_file, "[bb %u] tracking insn %d via %s",
	       m_bb->index (), insn->uid (), pp_formatted_text (&pp));
      fprintf (dump_file,
	       " [L=%d, WB=%d, FP=%d, %smode, off=",
	       lfs.load_p, autoinc_p, lfs.fpsimd_p, mode_name[mem_mode]);
      print_dec (access_off, dump_file);
      fprintf (dump_file, "]\n");
    }
}

// Dummy predicate that never ignores any insns.
static bool no_ignore (insn_info *) { return false; }

// Return the latest dataflow hazard before INSN.
//
// If IGNORE is non-NULL, this points to a sub-rtx which we should ignore for
// dataflow purposes.  This is needed when considering changing the RTL base of
// an access discovered through a MEM_EXPR base.
//
// If IGNORE_INSN is non-NULL, we should further ignore any hazards arising
// from that insn.
//
// N.B. we ignore any defs/uses of memory here as we deal with that separately,
// making use of alias disambiguation.
static insn_info *
latest_hazard_before (insn_info *insn, rtx *ignore,
		      insn_info *ignore_insn = nullptr)
{
  insn_info *result = nullptr;

  // If the insn can throw then it is at the end of a BB and we can't
  // move it, model this by recording a hazard in the previous insn
  // which will prevent moving the insn up.
  if (cfun->can_throw_non_call_exceptions
      && find_reg_note (insn->rtl (), REG_EH_REGION, NULL_RTX))
    return insn->prev_nondebug_insn ();

  // Return true if we registered the hazard.
  auto hazard = [&](insn_info *h) -> bool
    {
      gcc_checking_assert (*h < *insn);
      if (h == ignore_insn)
	return false;

      if (!result || *h > *result)
	result = h;

      return true;
    };

  rtx pat = PATTERN (insn->rtl ());
  auto ignore_use = [&](use_info *u)
    {
      if (u->is_mem ())
	return true;

      return !refers_to_regno_p (u->regno (), u->regno () + 1, pat, ignore);
    };

  // Find defs of uses in INSN (RaW).
  for (auto use : insn->uses ())
    if (!ignore_use (use) && use->def ())
      hazard (use->def ()->insn ());

  // Find previous defs (WaW) or previous uses (WaR) of defs in INSN.
  for (auto def : insn->defs ())
    {
      if (def->is_mem ())
	continue;

      if (def->prev_def ())
	{
	  hazard (def->prev_def ()->insn ()); // WaW

	  auto set = dyn_cast<set_info *> (def->prev_def ());
	  if (set && set->has_nondebug_insn_uses ())
	    for (auto use : set->reverse_nondebug_insn_uses ())
	      if (use->insn () != insn && hazard (use->insn ())) // WaR
		break;
	}

      if (!HARD_REGISTER_NUM_P (def->regno ()))
	continue;

      // Also need to check backwards for call clobbers (WaW).
      for (auto call_group : def->ebb ()->call_clobbers ())
	{
	  if (!call_group->clobbers (def->resource ()))
	    continue;

	  auto clobber_insn = prev_call_clobbers_ignoring (*call_group,
							   def->insn (),
							   no_ignore);
	  if (clobber_insn)
	    hazard (clobber_insn);
	}

    }

  return result;
}

// Return the first dataflow hazard after INSN.
//
// If IGNORE is non-NULL, this points to a sub-rtx which we should ignore for
// dataflow purposes.  This is needed when considering changing the RTL base of
// an access discovered through a MEM_EXPR base.
//
// N.B. we ignore any defs/uses of memory here as we deal with that separately,
// making use of alias disambiguation.
static insn_info *
first_hazard_after (insn_info *insn, rtx *ignore)
{
  insn_info *result = nullptr;
  auto hazard = [insn, &result](insn_info *h)
    {
      gcc_checking_assert (*h > *insn);
      if (!result || *h < *result)
	result = h;
    };

  rtx pat = PATTERN (insn->rtl ());
  auto ignore_use = [&](use_info *u)
    {
      if (u->is_mem ())
	return true;

      return !refers_to_regno_p (u->regno (), u->regno () + 1, pat, ignore);
    };

  for (auto def : insn->defs ())
    {
      if (def->is_mem ())
	continue;

      if (def->next_def ())
	hazard (def->next_def ()->insn ()); // WaW

      auto set = dyn_cast<set_info *> (def);
      if (set && set->has_nondebug_insn_uses ())
	hazard (set->first_nondebug_insn_use ()->insn ()); // RaW

      if (!HARD_REGISTER_NUM_P (def->regno ()))
	continue;

      // Also check for call clobbers of this def (WaW).
      for (auto call_group : def->ebb ()->call_clobbers ())
	{
	  if (!call_group->clobbers (def->resource ()))
	    continue;

	  auto clobber_insn = next_call_clobbers_ignoring (*call_group,
							   def->insn (),
							   no_ignore);
	  if (clobber_insn)
	    hazard (clobber_insn);
	}
    }

  // Find any subsequent defs of uses in INSN (WaR).
  for (auto use : insn->uses ())
    {
      if (ignore_use (use))
	continue;

      if (use->def ())
	{
	  auto def = use->def ()->next_def ();
	  if (def && def->insn () == insn)
	    def = def->next_def ();

	  if (def)
	    hazard (def->insn ());
	}

      if (!HARD_REGISTER_NUM_P (use->regno ()))
	continue;

      // Also need to handle call clobbers of our uses (again WaR).
      //
      // See restrict_movement_for_uses_ignoring for why we don't
      // need to check backwards for call clobbers.
      for (auto call_group : use->ebb ()->call_clobbers ())
	{
	  if (!call_group->clobbers (use->resource ()))
	    continue;

	  auto clobber_insn = next_call_clobbers_ignoring (*call_group,
							   use->insn (),
							   no_ignore);
	  if (clobber_insn)
	    hazard (clobber_insn);
	}
    }

  return result;
}

// Return true iff R1 and R2 overlap.
static bool
ranges_overlap_p (const insn_range_info &r1, const insn_range_info &r2)
{
  // If either range is empty, then their intersection is empty.
  if (!r1 || !r2)
    return false;

  // When do they not overlap? When one range finishes before the other
  // starts, i.e. (*r1.last < *r2.first || *r2.last < *r1.first).
  // Inverting this, we get the below.
  return *r1.last >= *r2.first && *r2.last >= *r1.first;
}

// Get the range of insns that def feeds.
static insn_range_info get_def_range (def_info *def)
{
  insn_info *last = def->next_def ()->insn ()->prev_nondebug_insn ();
  return { def->insn (), last };
}

// Given a def (of memory), return the downwards range within which we
// can safely move this def.
static insn_range_info
def_downwards_move_range (def_info *def)
{
  auto range = get_def_range (def);

  auto set = dyn_cast<set_info *> (def);
  if (!set || !set->has_any_uses ())
    return range;

  auto use = set->first_nondebug_insn_use ();
  if (use)
    range = move_earlier_than (range, use->insn ());

  return range;
}

// Given a def (of memory), return the upwards range within which we can
// safely move this def.
static insn_range_info
def_upwards_move_range (def_info *def)
{
  def_info *prev = def->prev_def ();
  insn_range_info range { prev->insn (), def->insn () };

  auto set = dyn_cast<set_info *> (prev);
  if (!set || !set->has_any_uses ())
    return range;

  auto use = set->last_nondebug_insn_use ();
  if (use)
    range = move_later_than (range, use->insn ());

  return range;
}

// Class that implements a state machine for building the changes needed to form
// a store pair instruction.  This allows us to easily build the changes in
// program order, as required by rtl-ssa.
struct stp_change_builder
{
  enum class state
  {
    FIRST,
    INSERT,
    FIXUP_USE,
    LAST,
    DONE
  };

  enum class action
  {
    TOMBSTONE,
    CHANGE,
    INSERT,
    FIXUP_USE
  };

  struct change
  {
    action type;
    insn_info *insn;
  };

  bool done () const { return m_state == state::DONE; }

  stp_change_builder (insn_info *insns[2],
		      insn_info *repurpose,
		      insn_info *dest)
    : m_state (state::FIRST), m_insns { insns[0], insns[1] },
      m_repurpose (repurpose), m_dest (dest), m_use (nullptr) {}

  change get_change () const
  {
    switch (m_state)
      {
      case state::FIRST:
	return {
	  m_insns[0] == m_repurpose ? action::CHANGE : action::TOMBSTONE,
	  m_insns[0]
	};
      case state::LAST:
	return {
	  m_insns[1] == m_repurpose ? action::CHANGE : action::TOMBSTONE,
	  m_insns[1]
	};
      case state::INSERT:
	return { action::INSERT, m_dest };
      case state::FIXUP_USE:
	return { action::FIXUP_USE, m_use->insn () };
      case state::DONE:
	break;
      }

    gcc_unreachable ();
  }

  // Transition to the next state.
  void advance ()
  {
    switch (m_state)
      {
      case state::FIRST:
	if (m_repurpose)
	  m_state = state::LAST;
	else
	  m_state = state::INSERT;
	break;
      case state::INSERT:
      {
	def_info *def = memory_access (m_insns[0]->defs ());
	while (*def->next_def ()->insn () <= *m_dest)
	  def = def->next_def ();

	// Now we know DEF feeds the insertion point for the new stp.
	// Look for any uses of DEF that will consume the new stp.
	gcc_assert (*def->insn () <= *m_dest
		    && *def->next_def ()->insn () > *m_dest);

	auto set = as_a<set_info *> (def);
	for (auto use : set->nondebug_insn_uses ())
	  if (*use->insn () > *m_dest)
	    {
	      m_use = use;
	      break;
	    }

	if (m_use)
	  m_state = state::FIXUP_USE;
	else
	  m_state = state::LAST;
	break;
      }
      case state::FIXUP_USE:
	m_use = m_use->next_nondebug_insn_use ();
	if (!m_use)
	  m_state = state::LAST;
	break;
      case state::LAST:
	m_state = state::DONE;
	break;
      case state::DONE:
	gcc_unreachable ();
      }
  }

private:
  state m_state;

  // Original candidate stores.
  insn_info *m_insns[2];

  // If non-null, this is a candidate insn to change into an stp.  Otherwise we
  // are deleting both original insns and inserting a new insn for the stp.
  insn_info *m_repurpose;

  // Destionation of the stp, it will be placed immediately after m_dest.
  insn_info *m_dest;

  // Current nondebug use that needs updating due to stp insertion.
  use_info *m_use;
};

// Given candidate store insns FIRST and SECOND, see if we can re-purpose one
// of them (together with its def of memory) for the stp insn.  If so, return
// that insn.  Otherwise, return null.
static insn_info *
try_repurpose_store (insn_info *first,
		     insn_info *second,
		     const insn_range_info &move_range)
{
  def_info * const defs[2] = {
    memory_access (first->defs ()),
    memory_access (second->defs ())
  };

  if (move_range.includes (first)
      || ranges_overlap_p (move_range, def_downwards_move_range (defs[0])))
    return first;

  if (move_range.includes (second)
      || ranges_overlap_p (move_range, def_upwards_move_range (defs[1])))
    return second;

  return nullptr;
}

// Generate the RTL pattern for a "tombstone"; used temporarily during this pass
// to replace stores that are marked for deletion where we can't immediately
// delete the store (since there are uses of mem hanging off the store).
//
// These are deleted at the end of the pass and uses re-parented appropriately
// at this point.
static rtx
gen_tombstone (void)
{
  return gen_rtx_CLOBBER (VOIDmode,
			  gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode)));
}

// Given a pair mode MODE, return a canonical mode to be used for a single
// operand of such a pair.  Currently we only use this when promoting a
// non-writeback pair into a writeback pair, as it isn't otherwise clear
// which mode to use when storing a modeless CONST_INT.
static machine_mode
aarch64_operand_mode_for_pair_mode (machine_mode mode)
{
  switch (mode)
    {
    case E_V2x4QImode:
      return SImode;
    case E_V2x8QImode:
      return DImode;
    case E_V2x16QImode:
      return V16QImode;
    default:
      gcc_unreachable ();
    }
}

// Go through the reg notes rooted at NOTE, dropping those that we should drop,
// and preserving those that we want to keep by prepending them to (and
// returning) RESULT.  EH_REGION is used to make sure we have at most one
// REG_EH_REGION note in the resulting list.  FR_EXPR is used to return any
// REG_FRAME_RELATED_EXPR note we find, as these can need special handling in
// combine_reg_notes.
static rtx
filter_notes (rtx note, rtx result, bool *eh_region, rtx *fr_expr)
{
  for (; note; note = XEXP (note, 1))
    {
      switch (REG_NOTE_KIND (note))
	{
	case REG_DEAD:
	  // REG_DEAD notes aren't required to be maintained.
	case REG_EQUAL:
	case REG_EQUIV:
	case REG_UNUSED:
	case REG_NOALIAS:
	  // These can all be dropped.  For REG_EQU{AL,IV} they cannot apply to
	  // non-single_set insns, and REG_UNUSED is re-computed by RTl-SSA, see
	  // rtl-ssa/changes.cc:update_notes.
	  //
	  // Similarly, REG_NOALIAS cannot apply to a parallel.
	case REG_INC:
	  // When we form the pair insn, the reg update is implemented
	  // as just another SET in the parallel, so isn't really an
	  // auto-increment in the RTL sense, hence we drop the note.
	  break;
	case REG_EH_REGION:
	  gcc_assert (!*eh_region);
	  *eh_region = true;
	  result = alloc_reg_note (REG_EH_REGION, XEXP (note, 0), result);
	  break;
	case REG_CFA_DEF_CFA:
	case REG_CFA_OFFSET:
	case REG_CFA_RESTORE:
	  result = alloc_reg_note (REG_NOTE_KIND (note),
				   copy_rtx (XEXP (note, 0)),
				   result);
	  break;
	case REG_FRAME_RELATED_EXPR:
	  gcc_assert (!*fr_expr);
	  *fr_expr = copy_rtx (XEXP (note, 0));
	  break;
	default:
	  // Unexpected REG_NOTE kind.
	  gcc_unreachable ();
	}
    }

  return result;
}

// Return the notes that should be attached to a combination of I1 and I2, where
// *I1 < *I2.  LOAD_P is true for loads.
static rtx
combine_reg_notes (insn_info *i1, insn_info *i2, bool load_p)
{
  // Temporary storage for REG_FRAME_RELATED_EXPR notes.
  rtx fr_expr[2] = {};

  bool found_eh_region = false;
  rtx result = NULL_RTX;
  result = filter_notes (REG_NOTES (i2->rtl ()), result,
			 &found_eh_region, fr_expr + 1);
  result = filter_notes (REG_NOTES (i1->rtl ()), result,
			 &found_eh_region, fr_expr);

  if (!load_p)
    {
      // Simple frame-related sp-relative saves don't need CFI notes, but when
      // we combine them into an stp we will need a CFI note as dwarf2cfi can't
      // interpret the unspec pair representation directly.
      if (RTX_FRAME_RELATED_P (i1->rtl ()) && !fr_expr[0])
	fr_expr[0] = copy_rtx (PATTERN (i1->rtl ()));
      if (RTX_FRAME_RELATED_P (i2->rtl ()) && !fr_expr[1])
	fr_expr[1] = copy_rtx (PATTERN (i2->rtl ()));
    }

  rtx fr_pat = NULL_RTX;
  if (fr_expr[0] && fr_expr[1])
    {
      // Combining two frame-related insns, need to construct
      // a REG_FRAME_RELATED_EXPR note which represents the combined
      // operation.
      RTX_FRAME_RELATED_P (fr_expr[1]) = 1;
      fr_pat = gen_rtx_PARALLEL (VOIDmode,
				 gen_rtvec (2, fr_expr[0], fr_expr[1]));
    }
  else
    fr_pat = fr_expr[0] ? fr_expr[0] : fr_expr[1];

  if (fr_pat)
    result = alloc_reg_note (REG_FRAME_RELATED_EXPR,
			     fr_pat, result);

  return result;
}

// Given two memory accesses in PATS, at least one of which is of a
// writeback form, extract two non-writeback memory accesses addressed
// relative to the initial value of the base register, and output these
// in PATS.  Return an rtx that represents the overall change to the
// base register.
static rtx
extract_writebacks (bool load_p, rtx pats[2], int changed)
{
  rtx base_reg = NULL_RTX;
  poly_int64 current_offset = 0;

  poly_int64 offsets[2];

  for (int i = 0; i < 2; i++)
    {
      rtx mem = XEXP (pats[i], load_p);
      rtx reg = XEXP (pats[i], !load_p);

      rtx addr = XEXP (mem, 0);
      const bool autoinc_p = GET_RTX_CLASS (GET_CODE (addr)) == RTX_AUTOINC;

      poly_int64 offset;
      rtx this_base = ldp_strip_offset (mem, &offset);
      gcc_assert (REG_P (this_base));
      if (base_reg)
	gcc_assert (rtx_equal_p (base_reg, this_base));
      else
	base_reg = this_base;

      // If we changed base for the current insn, then we already
      // derived the correct mem for this insn from the effective
      // address of the other access.
      if (i == changed)
	{
	  gcc_checking_assert (!autoinc_p);
	  offsets[i] = offset;
	  continue;
	}

      if (autoinc_p && any_pre_modify_p (addr))
	current_offset += offset;

      poly_int64 this_off = current_offset;
      if (!autoinc_p)
	this_off += offset;

      offsets[i] = this_off;
      rtx new_mem = change_address (mem, GET_MODE (mem),
				    plus_constant (GET_MODE (base_reg),
						   base_reg, this_off));
      pats[i] = load_p
	? gen_rtx_SET (reg, new_mem)
	: gen_rtx_SET (new_mem, reg);

      if (autoinc_p && any_post_modify_p (addr))
	current_offset += offset;
    }

  if (known_eq (current_offset, 0))
    return NULL_RTX;

  return gen_rtx_SET (base_reg, plus_constant (GET_MODE (base_reg),
					       base_reg, current_offset));
}

// INSNS contains either {nullptr, pair insn} (when promoting an existing
// non-writeback pair) or contains the candidate insns used to form the pair
// (when fusing a new pair).
//
// PAIR_RANGE specifies where we want to form the final pair.
// INITIAL_OFFSET gives the current base offset for the pair.
// Bit I of INITIAL_WRITEBACK is set if INSNS[I] initially had writeback.
// ACCESS_SIZE gives the access size for a single arm of the pair.
// BASE_DEF gives the initial def of the base register consumed by the pair.
//
// Given the above, this function looks for a trailing destructive update of the
// base register.  If there is one, we choose the first such update after
// PAIR_DST that is still in the same BB as our pair.  We return the new def in
// *ADD_DEF and the resulting writeback effect in *WRITEBACK_EFFECT.
static insn_info *
find_trailing_add (insn_info *insns[2],
		   const insn_range_info &pair_range,
		   int initial_writeback,
		   rtx *writeback_effect,
		   def_info **add_def,
		   def_info *base_def,
		   poly_int64 initial_offset,
		   unsigned access_size)
{
  // Punt on frame-related insns, it is better to be conservative and
  // not try to form writeback pairs here, and means we don't have to
  // worry about the writeback case in forming REG_FRAME_RELATED_EXPR
  // notes (see combine_reg_notes).
  if ((insns[0] && RTX_FRAME_RELATED_P (insns[0]->rtl ()))
      || RTX_FRAME_RELATED_P (insns[1]->rtl ()))
    return nullptr;

  insn_info *pair_dst = pair_range.singleton ();
  gcc_assert (pair_dst);

  def_info *def = base_def->next_def ();

  // In the case that either of the initial pair insns had writeback,
  // then there will be intervening defs of the base register.
  // Skip over these.
  for (int i = 0; i < 2; i++)
    if (initial_writeback & (1 << i))
      {
	gcc_assert (def->insn () == insns[i]);
	def = def->next_def ();
      }

  if (!def || def->bb () != pair_dst->bb ())
    return nullptr;

  // DEF should now be the first def of the base register after PAIR_DST.
  insn_info *cand = def->insn ();
  gcc_assert (*cand > *pair_dst);

  const auto base_regno = base_def->regno ();

  // If CAND doesn't also use our base register,
  // it can't destructively update it.
  if (!find_access (cand->uses (), base_regno))
    return nullptr;

  auto rti = cand->rtl ();

  if (!INSN_P (rti))
    return nullptr;

  auto pat = PATTERN (rti);
  if (GET_CODE (pat) != SET)
    return nullptr;

  auto dest = XEXP (pat, 0);
  if (!REG_P (dest) || REGNO (dest) != base_regno)
    return nullptr;

  poly_int64 offset;
  rtx rhs_base = strip_offset (XEXP (pat, 1), &offset);
  if (!REG_P (rhs_base)
      || REGNO (rhs_base) != base_regno
      || !offset.is_constant ())
    return nullptr;

  // If the initial base offset is zero, we can handle any add offset
  // (post-inc).  Otherwise, we require the offsets to match (pre-inc).
  if (!known_eq (initial_offset, 0) && !known_eq (offset, initial_offset))
    return nullptr;

  auto off_hwi = offset.to_constant ();

  if (off_hwi % access_size != 0)
    return nullptr;

  off_hwi /= access_size;

  if (off_hwi < LDP_MIN_IMM || off_hwi > LDP_MAX_IMM)
    return nullptr;

  auto dump_prefix = [&]()
    {
      if (!insns[0])
	fprintf (dump_file, "existing pair i%d: ", insns[1]->uid ());
      else
	fprintf (dump_file, "  (%d,%d)",
		 insns[0]->uid (), insns[1]->uid ());
    };

  insn_info *hazard = latest_hazard_before (cand, nullptr, insns[1]);
  if (!hazard || *hazard <= *pair_dst)
    {
      if (dump_file)
	{
	  dump_prefix ();
	  fprintf (dump_file,
		   "folding in trailing add (%d) to use writeback form\n",
		   cand->uid ());
	}

      *add_def = def;
      *writeback_effect = copy_rtx (pat);
      return cand;
    }

  if (dump_file)
    {
      dump_prefix ();
      fprintf (dump_file,
	       "can't fold in trailing add (%d), hazard = %d\n",
	       cand->uid (), hazard->uid ());
    }

  return nullptr;
}

// We just emitted a tombstone with uid UID, track it in a bitmap for
// this BB so we can easily identify it later when cleaning up tombstones.
void
ldp_bb_info::track_tombstone (int uid)
{
  if (!m_emitted_tombstone)
    {
      // Lazily initialize the bitmap for tracking tombstone insns.
      bitmap_obstack_initialize (&m_bitmap_obstack);
      bitmap_initialize (&m_tombstone_bitmap, &m_bitmap_obstack);
      m_emitted_tombstone = true;
    }

  if (!bitmap_set_bit (&m_tombstone_bitmap, uid))
    gcc_unreachable (); // Bit should have changed.
}

// Reset the debug insn containing USE (the debug insn has been
// optimized away).
static void
reset_debug_use (use_info *use)
{
  auto use_insn = use->insn ();
  auto use_rtl = use_insn->rtl ();
  insn_change change (use_insn);
  change.new_uses = {};
  INSN_VAR_LOCATION_LOC (use_rtl) = gen_rtx_UNKNOWN_VAR_LOC ();
  crtl->ssa->change_insn (change);
}

// USE is a debug use that needs updating because DEF (a def of the same
// register) is being re-ordered over it.  If BASE is non-null, then DEF
// is an update of the register BASE by a constant, given by WB_OFFSET,
// and we can preserve debug info by accounting for the change in side
// effects.
static void
fixup_debug_use (obstack_watermark &attempt,
		 use_info *use,
		 def_info *def,
		 rtx base,
		 poly_int64 wb_offset)
{
  auto use_insn = use->insn ();
  if (base)
    {
      auto use_rtl = use_insn->rtl ();
      insn_change change (use_insn);

      gcc_checking_assert (REG_P (base) && use->regno () == REGNO (base));
      change.new_uses = check_remove_regno_access (attempt,
						   change.new_uses,
						   use->regno ());

      // The effect of the writeback is to add WB_OFFSET to BASE.  If
      // we're re-ordering DEF below USE, then we update USE by adding
      // WB_OFFSET to it.  Otherwise, if we're re-ordering DEF above
      // USE, we update USE by undoing the effect of the writeback
      // (subtracting WB_OFFSET).
      use_info *new_use;
      if (*def->insn () > *use_insn)
	{
	  // We now need USE_INSN to consume DEF.  Create a new use of DEF.
	  //
	  // N.B. this means until we call change_insns for the main change
	  // group we will temporarily have a debug use consuming a def that
	  // comes after it, but RTL-SSA doesn't currently support updating
	  // debug insns as part of the main change group (together with
	  // nondebug changes), so we will have to live with this update
	  // leaving the IR being temporarily inconsistent.  It seems to
	  // work out OK once the main change group is applied.
	  wb_offset *= -1;
	  new_use = crtl->ssa->create_use (attempt,
					   use_insn,
					   as_a<set_info *> (def));
	}
      else
	new_use = find_access (def->insn ()->uses (), use->regno ());

      change.new_uses = insert_access (attempt, new_use, change.new_uses);

      if (dump_file)
	{
	  const char *dir = (*def->insn () < *use_insn) ? "down" : "up";
	  pretty_printer pp;
	  pp_string (&pp, "[");
	  pp_access (&pp, use, 0);
	  pp_string (&pp, "]");
	  pp_string (&pp, " due to wb def ");
	  pp_string (&pp, "[");
	  pp_access (&pp, def, 0);
	  pp_string (&pp, "]");
	  fprintf (dump_file,
		   "  i%d: fix up debug use %s re-ordered %s, "
		   "sub r%u -> r%u + ",
		   use_insn->uid (), pp_formatted_text (&pp),
		   dir, REGNO (base), REGNO (base));
	  print_dec (wb_offset, dump_file);
	  fprintf (dump_file, "\n");
	}

      insn_propagation prop (use_rtl, base,
			     plus_constant (GET_MODE (base), base, wb_offset));
      if (prop.apply_to_pattern (&INSN_VAR_LOCATION_LOC (use_rtl)))
	crtl->ssa->change_insn (change);
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "  i%d: RTL substitution failed (%s)"
		     ", resetting debug insn", use_insn->uid (),
		     prop.failure_reason);
	  reset_debug_use (use);
	}
    }
  else
    {
      if (dump_file)
	{
	  pretty_printer pp;
	  pp_string (&pp, "[");
	  pp_access (&pp, use, 0);
	  pp_string (&pp, "] due to re-ordered load def [");
	  pp_access (&pp, def, 0);
	  pp_string (&pp, "]");
	  fprintf (dump_file, "  i%d: resetting debug use %s\n",
		   use_insn->uid (), pp_formatted_text (&pp));
	}
      reset_debug_use (use);
    }
}

// Update debug uses when folding in a trailing add insn to form a
// writeback pair.
//
// ATTEMPT is used to allocate RTL-SSA temporaries for the changes,
// the final pair is placed immediately after PAIR_DST, TRAILING_ADD
// is a trailing add insn which is being folded into the pair to make it
// use writeback addressing, and WRITEBACK_EFFECT is the pattern for
// TRAILING_ADD.
static void
fixup_debug_uses_trailing_add (obstack_watermark &attempt,
			       insn_info *pair_dst,
			       insn_info *trailing_add,
			       rtx writeback_effect)
{
  rtx base = SET_DEST (writeback_effect);

  poly_int64 wb_offset;
  rtx base2 = strip_offset (SET_SRC (writeback_effect), &wb_offset);
  gcc_checking_assert (rtx_equal_p (base, base2));

  auto defs = trailing_add->defs ();
  gcc_checking_assert (defs.size () == 1);
  def_info *def = defs[0];

  if (auto set = safe_dyn_cast<set_info *> (def->prev_def ()))
    for (auto use : iterate_safely (set->debug_insn_uses ()))
      if (*use->insn () > *pair_dst)
	// DEF is getting re-ordered above USE, fix up USE accordingly.
	fixup_debug_use (attempt, use, def, base, wb_offset);
}

// Called from fuse_pair, fixes up any debug uses that will be affected
// by the changes.
//
// ATTEMPT is the obstack watermark used to allocate RTL-SSA temporaries for
// the changes, INSNS gives the candidate insns: at this point the use/def
// information should still be as on entry to fuse_pair, but the patterns may
// have changed, hence we pass ORIG_RTL which contains the original patterns
// for the candidate insns.
//
// The final pair will be placed immediately after PAIR_DST, LOAD_P is true if
// it is a load pair, bit I of WRITEBACK is set if INSNS[I] originally had
// writeback, and WRITEBACK_EFFECT is an rtx describing the overall update to
// the base register in the final pair (if any).  BASE_REGNO gives the register
// number of the base register used in the final pair.
static void
fixup_debug_uses (obstack_watermark &attempt,
		  insn_info *insns[2],
		  rtx orig_rtl[2],
		  insn_info *pair_dst,
		  insn_info *trailing_add,
		  bool load_p,
		  int writeback,
		  rtx writeback_effect,
		  unsigned base_regno)
{
  // USE is a debug use that needs updating because DEF (a def of the
  // resource) is being re-ordered over it.  If WRITEBACK_PAT is non-NULL,
  // then it gives the original RTL pattern for DEF's insn, and DEF is a
  // writeback update of the base register.
  //
  // This simply unpacks WRITEBACK_PAT if needed and calls fixup_debug_use.
  auto update_debug_use = [&](use_info *use, def_info *def,
			      rtx writeback_pat)
    {
      poly_int64 offset = 0;
      rtx base = NULL_RTX;
      if (writeback_pat)
	{
	  rtx mem = XEXP (writeback_pat, load_p);
	  gcc_checking_assert (GET_RTX_CLASS (GET_CODE (XEXP (mem, 0)))
			       == RTX_AUTOINC);

	  base = ldp_strip_offset (mem, &offset);
	  gcc_checking_assert (REG_P (base) && REGNO (base) == base_regno);
	}
      fixup_debug_use (attempt, use, def, base, offset);
    };

  // Reset any debug uses of mem over which we re-ordered a store.
  //
  // It would be nice to try and preserve debug info here, but it seems that
  // would require doing alias analysis to see if the store aliases with the
  // debug use, which seems a little extravagant just to preserve debug info.
  if (!load_p)
    {
      auto def = memory_access (insns[0]->defs ());
      auto last_def = memory_access (insns[1]->defs ());
      for (; def != last_def; def = def->next_def ())
	{
	  auto set = as_a<set_info *> (def);
	  for (auto use : iterate_safely (set->debug_insn_uses ()))
	    {
	      if (dump_file)
		fprintf (dump_file, "  i%d: resetting debug use of mem\n",
			 use->insn ()->uid ());
	      reset_debug_use (use);
	    }
	}
    }

  // Now let's take care of register uses, starting with debug uses
  // attached to defs from our first insn.
  for (auto def : insns[0]->defs ())
    {
      auto set = dyn_cast<set_info *> (def);
      if (!set || set->is_mem () || !set->first_debug_insn_use ())
	continue;

      def_info *defs[2] = {
	def,
	find_access (insns[1]->defs (), def->regno ())
      };

      rtx writeback_pats[2] = {};
      if (def->regno () == base_regno)
	for (int i = 0; i < 2; i++)
	  if (writeback & (1 << i))
	    {
	      gcc_checking_assert (defs[i]);
	      writeback_pats[i] = orig_rtl[i];
	    }

      // Now that we've characterized the defs involved, go through the
      // debug uses and determine how to update them (if needed).
      for (auto use : iterate_safely (set->debug_insn_uses ()))
	{
	  if (*pair_dst < *use->insn () && defs[1])
	    // We're re-ordering defs[1] above a previous use of the
	    // same resource.
	    update_debug_use (use, defs[1], writeback_pats[1]);
	  else if (*pair_dst >= *use->insn ())
	    // We're re-ordering defs[0] below its use.
	    update_debug_use (use, defs[0], writeback_pats[0]);
	}
    }

  // Now let's look at registers which are def'd by the second insn
  // but not by the first insn, there may still be debug uses of a
  // previous def which can be affected by moving the second insn up.
  for (auto def : insns[1]->defs ())
    {
      // This should be M log N where N is the number of defs in
      // insns[0] and M is the number of defs in insns[1].
      if (def->is_mem () || find_access (insns[0]->defs (), def->regno ()))
	  continue;

      auto prev_set = safe_dyn_cast<set_info *> (def->prev_def ());
      if (!prev_set)
	continue;

      rtx writeback_pat = NULL_RTX;
      if (def->regno () == base_regno && (writeback & 2))
	writeback_pat = orig_rtl[1];

      // We have a def in insns[1] which isn't def'd by the first insn.
      // Look to the previous def and see if it has any debug uses.
      for (auto use : iterate_safely (prev_set->debug_insn_uses ()))
	if (*pair_dst < *use->insn ())
	  // We're ordering DEF above a previous use of the same register.
	  update_debug_use (use, def, writeback_pat);
    }

  if ((writeback & 2) && !writeback_effect)
    {
      // If the second insn initially had writeback but the final
      // pair does not, then there may be trailing debug uses of the
      // second writeback def which need re-parenting: do that.
      auto def = find_access (insns[1]->defs (), base_regno);
      gcc_assert (def);
      auto set = as_a<set_info *> (def);
      for (auto use : iterate_safely (set->debug_insn_uses ()))
	{
	  insn_change change (use->insn ());
	  change.new_uses = check_remove_regno_access (attempt,
						       change.new_uses,
						       base_regno);
	  auto new_use = find_access (insns[0]->uses (), base_regno);

	  // N.B. insns must have already shared a common base due to writeback.
	  gcc_assert (new_use);

	  if (dump_file)
	    fprintf (dump_file,
		     "  i%d: cancelling wb, re-parenting trailing debug use\n",
		     use->insn ()->uid ());

	  change.new_uses = insert_access (attempt, new_use, change.new_uses);
	  crtl->ssa->change_insn (change);
	}
    }
  else if (trailing_add)
    fixup_debug_uses_trailing_add (attempt, pair_dst, trailing_add,
				   writeback_effect);
}

// Try and actually fuse the pair given by insns I1 and I2.
//
// Here we've done enough analysis to know this is safe, we only
// reject the pair at this stage if either the tuning policy says to,
// or recog fails on the final pair insn.
//
// LOAD_P is true for loads, ACCESS_SIZE gives the access size of each
// candidate insn.  Bit i of WRITEBACK is set if the ith insn (in program
// order) uses writeback.
//
// BASE gives the chosen base candidate for the pair and MOVE_RANGE is
// a singleton range which says where to place the pair.
bool
ldp_bb_info::fuse_pair (bool load_p,
			unsigned access_size,
			int writeback,
			insn_info *i1, insn_info *i2,
			base_cand &base,
			const insn_range_info &move_range)
{
  auto attempt = crtl->ssa->new_change_attempt ();

  auto make_change = [&attempt](insn_info *insn)
    {
      return crtl->ssa->change_alloc<insn_change> (attempt, insn);
    };
  auto make_delete = [&attempt](insn_info *insn)
    {
      return crtl->ssa->change_alloc<insn_change> (attempt,
						   insn,
						   insn_change::DELETE);
    };

  insn_info *first = (*i1 < *i2) ? i1 : i2;
  insn_info *second = (first == i1) ? i2 : i1;

  insn_info *pair_dst = move_range.singleton ();
  gcc_assert (pair_dst);

  insn_info *insns[2] = { first, second };

  auto_vec<insn_change *> changes;
  auto_vec<int, 2> tombstone_uids (2);

  rtx pats[2] = {
    PATTERN (first->rtl ()),
    PATTERN (second->rtl ())
  };

  // Make copies of the patterns as we might need to refer to the original RTL
  // later, for example when updating debug uses (which is after we've updated
  // one or both of the patterns in the candidate insns).
  rtx orig_rtl[2];
  for (int i = 0; i < 2; i++)
    orig_rtl[i] = copy_rtx (pats[i]);

  use_array input_uses[2] = { first->uses (), second->uses () };
  def_array input_defs[2] = { first->defs (), second->defs () };

  int changed_insn = -1;
  if (base.from_insn != -1)
    {
      // If we're not already using a shared base, we need
      // to re-write one of the accesses to use the base from
      // the other insn.
      gcc_checking_assert (base.from_insn == 0 || base.from_insn == 1);
      changed_insn = !base.from_insn;

      rtx base_pat = pats[base.from_insn];
      rtx change_pat = pats[changed_insn];
      rtx base_mem = XEXP (base_pat, load_p);
      rtx change_mem = XEXP (change_pat, load_p);

      const bool lower_base_p = (insns[base.from_insn] == i1);
      HOST_WIDE_INT adjust_amt = access_size;
      if (!lower_base_p)
	adjust_amt *= -1;

      rtx change_reg = XEXP (change_pat, !load_p);
      machine_mode mode_for_mem = GET_MODE (change_mem);
      rtx effective_base = drop_writeback (base_mem);
      rtx new_mem = adjust_address_nv (effective_base,
				       mode_for_mem,
				       adjust_amt);
      rtx new_set = load_p
	? gen_rtx_SET (change_reg, new_mem)
	: gen_rtx_SET (new_mem, change_reg);

      pats[changed_insn] = new_set;

      auto keep_use = [&](use_info *u)
	{
	  return refers_to_regno_p (u->regno (), u->regno () + 1,
				    change_pat, &XEXP (change_pat, load_p));
	};

      // Drop any uses that only occur in the old address.
      input_uses[changed_insn] = filter_accesses (attempt,
						  input_uses[changed_insn],
						  keep_use);
    }

  rtx writeback_effect = NULL_RTX;
  if (writeback)
    writeback_effect = extract_writebacks (load_p, pats, changed_insn);

  const auto base_regno = base.def->regno ();

  if (base.from_insn == -1 && (writeback & 1))
    {
      // If the first of the candidate insns had a writeback form, we'll need to
      // drop the use of the updated base register from the second insn's uses.
      //
      // N.B. we needn't worry about the base register occurring as a store
      // operand, as we checked that there was no non-address true dependence
      // between the insns in try_fuse_pair.
      gcc_checking_assert (find_access (input_uses[1], base_regno));
      input_uses[1] = check_remove_regno_access (attempt,
						 input_uses[1],
						 base_regno);
    }

  // Go through and drop uses that only occur in register notes,
  // as we won't be preserving those.
  for (int i = 0; i < 2; i++)
    {
      auto rti = insns[i]->rtl ();
      if (!REG_NOTES (rti))
	continue;

      input_uses[i] = remove_note_accesses (attempt, input_uses[i]);
    }

  // Edge case: if the first insn is a writeback load and the
  // second insn is a non-writeback load which transfers into the base
  // register, then we should drop the writeback altogether as the
  // update of the base register from the second load should prevail.
  //
  // For example:
  //   ldr x2, [x1], #8
  //   ldr x1, [x1]
  //   -->
  //   ldp x2, x1, [x1]
  if (writeback == 1
      && load_p
      && find_access (input_defs[1], base_regno))
    {
      if (dump_file)
	fprintf (dump_file,
		 "  ldp: i%d has wb but subsequent i%d has non-wb "
		 "update of base (r%d), dropping wb\n",
		 insns[0]->uid (), insns[1]->uid (), base_regno);
      gcc_assert (writeback_effect);
      writeback_effect = NULL_RTX;
    }

  // So far the patterns have been in instruction order,
  // now we want them in offset order.
  if (i1 != first)
    std::swap (pats[0], pats[1]);

  poly_int64 offsets[2];
  for (int i = 0; i < 2; i++)
    {
      rtx mem = XEXP (pats[i], load_p);
      gcc_checking_assert (MEM_P (mem));
      rtx base = strip_offset (XEXP (mem, 0), offsets + i);
      gcc_checking_assert (REG_P (base));
      gcc_checking_assert (base_regno == REGNO (base));
    }

  // If either of the original insns had writeback, but the resulting pair insn
  // does not (can happen e.g. in the ldp edge case above, or if the writeback
  // effects cancel out), then drop the def(s) of the base register as
  // appropriate.
  //
  // Also drop the first def in the case that both of the original insns had
  // writeback.  The second def could well have uses, but the first def should
  // only be used by the second insn (and we dropped that use above).
  for (int i = 0; i < 2; i++)
    if ((!writeback_effect && (writeback & (1 << i)))
	|| (i == 0 && writeback == 3))
      input_defs[i] = check_remove_regno_access (attempt,
						 input_defs[i],
						 base_regno);

  // If we don't currently have a writeback pair, and we don't have
  // a load that clobbers the base register, look for a trailing destructive
  // update of the base register and try and fold it in to make this into a
  // writeback pair.
  insn_info *trailing_add = nullptr;
  if (aarch64_ldp_writeback > 1
      && !writeback_effect
      && (!load_p || (!refers_to_regno_p (base_regno, base_regno + 1,
					 XEXP (pats[0], 0), nullptr)
		      && !refers_to_regno_p (base_regno, base_regno + 1,
					     XEXP (pats[1], 0), nullptr))))
    {
      def_info *add_def;
      trailing_add = find_trailing_add (insns, move_range, writeback,
					&writeback_effect,
					&add_def, base.def, offsets[0],
					access_size);
      if (trailing_add)
	{
	  // The def of the base register from the trailing add should prevail.
	  input_defs[0] = insert_access (attempt, add_def, input_defs[0]);
	  gcc_assert (input_defs[0].is_valid ());
	}
    }

  // Now that we know what base mem we're going to use, check if it's OK
  // with the ldp/stp policy.
  rtx first_mem = XEXP (pats[0], load_p);
  if (!aarch64_mem_ok_with_ldpstp_policy_model (first_mem,
						load_p,
						GET_MODE (first_mem)))
    {
      if (dump_file)
	fprintf (dump_file, "punting on pair (%d,%d), ldp/stp policy says no\n",
		 i1->uid (), i2->uid ());
      return false;
    }

  rtx reg_notes = combine_reg_notes (first, second, load_p);

  rtx pair_pat;
  if (writeback_effect)
    {
      auto patvec = gen_rtvec (3, writeback_effect, pats[0], pats[1]);
      pair_pat = gen_rtx_PARALLEL (VOIDmode, patvec);
    }
  else if (load_p)
    pair_pat = aarch64_gen_load_pair (XEXP (pats[0], 0),
				      XEXP (pats[1], 0),
				      XEXP (pats[0], 1));
  else
    pair_pat = aarch64_gen_store_pair (XEXP (pats[0], 0),
				       XEXP (pats[0], 1),
				       XEXP (pats[1], 1));

  insn_change *pair_change = nullptr;
  auto set_pair_pat = [pair_pat,reg_notes](insn_change *change) {
    rtx_insn *rti = change->insn ()->rtl ();
    validate_unshare_change (rti, &PATTERN (rti), pair_pat, true);
    validate_change (rti, &REG_NOTES (rti), reg_notes, true);
  };

  if (load_p)
    {
      changes.safe_push (make_delete (first));
      pair_change = make_change (second);
      changes.safe_push (pair_change);

      pair_change->move_range = move_range;
      pair_change->new_defs = merge_access_arrays (attempt,
						   input_defs[0],
						   input_defs[1]);
      gcc_assert (pair_change->new_defs.is_valid ());

      pair_change->new_uses
	= merge_access_arrays (attempt,
			       drop_memory_access (input_uses[0]),
			       drop_memory_access (input_uses[1]));
      gcc_assert (pair_change->new_uses.is_valid ());
      set_pair_pat (pair_change);
    }
  else
    {
      using Action = stp_change_builder::action;
      insn_info *store_to_change = try_repurpose_store (first, second,
							move_range);
      stp_change_builder builder (insns, store_to_change, pair_dst);
      insn_change *change;
      set_info *new_set = nullptr;
      for (; !builder.done (); builder.advance ())
	{
	  auto action = builder.get_change ();
	  change = (action.type == Action::INSERT)
	    ? nullptr : make_change (action.insn);
	  switch (action.type)
	    {
	    case Action::CHANGE:
	    {
	      set_pair_pat (change);
	      change->new_uses = merge_access_arrays (attempt,
						      input_uses[0],
						      input_uses[1]);
	      auto d1 = drop_memory_access (input_defs[0]);
	      auto d2 = drop_memory_access (input_defs[1]);
	      change->new_defs = merge_access_arrays (attempt, d1, d2);
	      gcc_assert (change->new_defs.is_valid ());
	      def_info *stp_def = memory_access (change->insn ()->defs ());
	      change->new_defs = insert_access (attempt,
						stp_def,
						change->new_defs);
	      gcc_assert (change->new_defs.is_valid ());
	      change->move_range = move_range;
	      pair_change = change;
	      break;
	    }
	    case Action::TOMBSTONE:
	    {
	      tombstone_uids.quick_push (change->insn ()->uid ());
	      rtx_insn *rti = change->insn ()->rtl ();
	      validate_change (rti, &PATTERN (rti), gen_tombstone (), true);
	      validate_change (rti, &REG_NOTES (rti), NULL_RTX, true);
	      change->new_uses = use_array (nullptr, 0);
	      break;
	    }
	    case Action::INSERT:
	    {
	      if (dump_file)
		fprintf (dump_file,
			 "  stp: cannot re-purpose candidate stores\n");

	      auto new_insn = crtl->ssa->create_insn (attempt, INSN, pair_pat);
	      change = make_change (new_insn);
	      change->move_range = move_range;
	      change->new_uses = merge_access_arrays (attempt,
						      input_uses[0],
						      input_uses[1]);
	      gcc_assert (change->new_uses.is_valid ());

	      auto d1 = drop_memory_access (input_defs[0]);
	      auto d2 = drop_memory_access (input_defs[1]);
	      change->new_defs = merge_access_arrays (attempt, d1, d2);
	      gcc_assert (change->new_defs.is_valid ());

	      new_set = crtl->ssa->create_set (attempt, new_insn, memory);
	      change->new_defs = insert_access (attempt, new_set,
						change->new_defs);
	      gcc_assert (change->new_defs.is_valid ());
	      pair_change = change;
	      break;
	    }
	    case Action::FIXUP_USE:
	    {
	      // This use now needs to consume memory from our stp.
	      if (dump_file)
		fprintf (dump_file,
			 "  stp: changing i%d to use mem from new stp "
			 "(after i%d)\n",
			 action.insn->uid (), pair_dst->uid ());
	      change->new_uses = drop_memory_access (change->new_uses);
	      gcc_assert (new_set);
	      auto new_use = crtl->ssa->create_use (attempt, action.insn,
						    new_set);
	      change->new_uses = insert_access (attempt, new_use,
						change->new_uses);
	      break;
	    }
	    }
	  changes.safe_push (change);
	}
    }

  if (trailing_add)
    changes.safe_push (make_delete (trailing_add));
  else if ((writeback & 2) && !writeback_effect)
    {
      // The second insn initially had writeback but now the pair does not,
      // need to update any nondebug uses of the base register def in the
      // second insn.  We'll take care of debug uses later.
      auto def = find_access (insns[1]->defs (), base_regno);
      gcc_assert (def);
      auto set = dyn_cast<set_info *> (def);
      if (set && set->has_nondebug_uses ())
	{
	  auto orig_use = find_access (insns[0]->uses (), base_regno);
	  for (auto use : set->nondebug_insn_uses ())
	    {
	      auto change = make_change (use->insn ());
	      change->new_uses = check_remove_regno_access (attempt,
							    change->new_uses,
							    base_regno);
	      change->new_uses = insert_access (attempt,
						orig_use,
						change->new_uses);
	      changes.safe_push (change);
	    }
	}
    }

  auto is_changing = insn_is_changing (changes);
  for (unsigned i = 0; i < changes.length (); i++)
    gcc_assert (rtl_ssa::restrict_movement_ignoring (*changes[i], is_changing));

  // Check the pair pattern is recog'd.
  if (!rtl_ssa::recog_ignoring (attempt, *pair_change, is_changing))
    {
      if (dump_file)
	fprintf (dump_file, "  failed to form pair, recog failed\n");

      // Free any reg notes we allocated.
      while (reg_notes)
	{
	  rtx next = XEXP (reg_notes, 1);
	  free_EXPR_LIST_node (reg_notes);
	  reg_notes = next;
	}
      cancel_changes (0);
      return false;
    }

  gcc_assert (crtl->ssa->verify_insn_changes (changes));

  // Fix up any debug uses that will be affected by the changes.
  if (MAY_HAVE_DEBUG_INSNS)
    fixup_debug_uses (attempt, insns, orig_rtl, pair_dst, trailing_add,
		      load_p, writeback, writeback_effect, base_regno);

  confirm_change_group ();
  crtl->ssa->change_insns (changes);

  gcc_checking_assert (tombstone_uids.length () <= 2);
  for (auto uid : tombstone_uids)
    track_tombstone (uid);

  return true;
}

// Return true if STORE_INSN may modify mem rtx MEM.  Make sure we keep
// within our BUDGET for alias analysis.
static bool
store_modifies_mem_p (rtx mem, insn_info *store_insn, int &budget)
{
  if (!budget)
    {
      if (dump_file)
	{
	  fprintf (dump_file,
		   "exceeded budget, assuming store %d aliases with mem ",
		   store_insn->uid ());
	  print_simple_rtl (dump_file, mem);
	  fprintf (dump_file, "\n");
	}

      return true;
    }

  budget--;
  return memory_modified_in_insn_p (mem, store_insn->rtl ());
}

// Return true if LOAD may be modified by STORE.  Make sure we keep
// within our BUDGET for alias analysis.
static bool
load_modified_by_store_p (insn_info *load,
			  insn_info *store,
			  int &budget)
{
  gcc_checking_assert (budget >= 0);

  if (!budget)
    {
      if (dump_file)
	{
	  fprintf (dump_file,
		   "exceeded budget, assuming load %d aliases with store %d\n",
		   load->uid (), store->uid ());
	}
      return true;
    }

  // It isn't safe to re-order stores over calls.
  if (CALL_P (load->rtl ()))
    return true;

  budget--;

  // Iterate over all MEMs in the load, seeing if any alias with
  // our store.
  subrtx_var_iterator::array_type array;
  rtx pat = PATTERN (load->rtl ());
  FOR_EACH_SUBRTX_VAR (iter, array, pat, NONCONST)
    if (MEM_P (*iter) && memory_modified_in_insn_p (*iter, store->rtl ()))
      return true;

  return false;
}

// Virtual base class for load/store walkers used in alias analysis.
struct alias_walker
{
  virtual bool conflict_p (int &budget) const = 0;
  virtual insn_info *insn () const = 0;
  virtual bool valid () const = 0;
  virtual void advance () = 0;
};

// Implement some common functionality used by both store_walker
// and load_walker.
template<bool reverse>
class def_walker : public alias_walker
{
protected:
  using def_iter_t = typename std::conditional<reverse,
	reverse_def_iterator, def_iterator>::type;

  static use_info *start_use_chain (def_iter_t &def_iter)
  {
    set_info *set = nullptr;
    for (; *def_iter; def_iter++)
      {
	set = dyn_cast<set_info *> (*def_iter);
	if (!set)
	  continue;

	use_info *use = reverse
	  ? set->last_nondebug_insn_use ()
	  : set->first_nondebug_insn_use ();

	if (use)
	  return use;
      }

    return nullptr;
  }

  def_iter_t def_iter;
  insn_info *limit;
  def_walker (def_info *def, insn_info *limit) :
    def_iter (def), limit (limit) {}

  virtual bool iter_valid () const { return *def_iter; }

public:
  insn_info *insn () const override { return (*def_iter)->insn (); }
  void advance () override { def_iter++; }
  bool valid () const override final
  {
    if (!iter_valid ())
      return false;

    if (reverse)
      return *(insn ()) > *limit;
    else
      return *(insn ()) < *limit;
  }
};

// alias_walker that iterates over stores.
template<bool reverse, typename InsnPredicate>
class store_walker : public def_walker<reverse>
{
  rtx cand_mem;
  InsnPredicate tombstone_p;

public:
  store_walker (def_info *mem_def, rtx mem, insn_info *limit_insn,
		InsnPredicate tombstone_fn) :
    def_walker<reverse> (mem_def, limit_insn),
    cand_mem (mem), tombstone_p (tombstone_fn) {}

  bool conflict_p (int &budget) const override final
  {
    if (tombstone_p (this->insn ()))
      return false;

    return store_modifies_mem_p (cand_mem, this->insn (), budget);
  }
};

// alias_walker that iterates over loads.
template<bool reverse>
class load_walker : public def_walker<reverse>
{
  using Base = def_walker<reverse>;
  using use_iter_t = typename std::conditional<reverse,
	reverse_use_iterator, nondebug_insn_use_iterator>::type;

  use_iter_t use_iter;
  insn_info *cand_store;

  bool iter_valid () const override final { return *use_iter; }

public:
  void advance () override final
  {
    use_iter++;
    if (*use_iter)
      return;
    this->def_iter++;
    use_iter = Base::start_use_chain (this->def_iter);
  }

  insn_info *insn () const override final
  {
    return (*use_iter)->insn ();
  }

  bool conflict_p (int &budget) const override final
  {
    return load_modified_by_store_p (insn (), cand_store, budget);
  }

  load_walker (def_info *def, insn_info *store, insn_info *limit_insn)
    : Base (def, limit_insn),
      use_iter (Base::start_use_chain (this->def_iter)),
      cand_store (store) {}
};

// Process our alias_walkers in a round-robin fashion, proceeding until
// nothing more can be learned from alias analysis.
//
// We try to maintain the invariant that if a walker becomes invalid, we
// set its pointer to null.
static void
do_alias_analysis (insn_info *alias_hazards[4],
		   alias_walker *walkers[4],
		   bool load_p)
{
  const int n_walkers = 2 + (2 * !load_p);
  int budget = aarch64_ldp_alias_check_limit;

  auto next_walker = [walkers,n_walkers](int current) -> int {
    for (int j = 1; j <= n_walkers; j++)
      {
	int idx = (current + j) % n_walkers;
	if (walkers[idx])
	  return idx;
      }
    return -1;
  };

  int i = -1;
  for (int j = 0; j < n_walkers; j++)
    {
      alias_hazards[j] = nullptr;
      if (!walkers[j])
	continue;

      if (!walkers[j]->valid ())
	walkers[j] = nullptr;
      else if (i == -1)
	i = j;
    }

  while (i >= 0)
    {
      int insn_i = i % 2;
      int paired_i = (i & 2) + !insn_i;
      int pair_fst = (i & 2);
      int pair_snd = (i & 2) + 1;

      if (walkers[i]->conflict_p (budget))
	{
	  alias_hazards[i] = walkers[i]->insn ();

	  // We got an aliasing conflict for this {load,store} walker,
	  // so we don't need to walk any further.
	  walkers[i] = nullptr;

	  // If we have a pair of alias conflicts that prevent
	  // forming the pair, stop.  There's no need to do further
	  // analysis.
	  if (alias_hazards[paired_i]
	      && (*alias_hazards[pair_fst] <= *alias_hazards[pair_snd]))
	    return;

	  if (!load_p)
	    {
	      int other_pair_fst = (pair_fst ? 0 : 2);
	      int other_paired_i = other_pair_fst + !insn_i;

	      int x_pair_fst = (i == pair_fst) ? i : other_paired_i;
	      int x_pair_snd = (i == pair_fst) ? other_paired_i : i;

	      // Similarly, handle the case where we have a {load,store}
	      // or {store,load} alias hazard pair that prevents forming
	      // the pair.
	      if (alias_hazards[other_paired_i]
		  && *alias_hazards[x_pair_fst] <= *alias_hazards[x_pair_snd])
		return;
	    }
	}

      if (walkers[i])
	{
	  walkers[i]->advance ();

	  if (!walkers[i]->valid ())
	    walkers[i] = nullptr;
	}

      i = next_walker (i);
    }
}

// Given INSNS (in program order) which are known to be adjacent, look
// to see if either insn has a suitable RTL (register) base that we can
// use to form a pair.  Push these to BASE_CANDS if we find any.  CAND_MEMs
// gives the relevant mems from the candidate insns, ACCESS_SIZE gives the
// size of a single candidate access, and REVERSED says whether the accesses
// are inverted in offset order.
//
// Returns an integer where bit (1 << i) is set if INSNS[i] uses writeback
// addressing.
static int
get_viable_bases (insn_info *insns[2],
		  vec<base_cand> &base_cands,
		  rtx cand_mems[2],
		  unsigned access_size,
		  bool reversed)
{
  // We discovered this pair through a common base.  Need to ensure that
  // we have a common base register that is live at both locations.
  def_info *base_defs[2] = {};
  int writeback = 0;
  for (int i = 0; i < 2; i++)
    {
      const bool is_lower = (i == reversed);
      poly_int64 poly_off;
      rtx base = ldp_strip_offset (cand_mems[i], &poly_off);
      if (GET_RTX_CLASS (GET_CODE (XEXP (cand_mems[i], 0))) == RTX_AUTOINC)
	writeback |= (1 << i);

      if (!REG_P (base) || !poly_off.is_constant ())
	continue;

      // Punt on accesses relative to eliminable regs.  See the comment in
      // ldp_bb_info::track_access for a detailed explanation of this.
      if (!reload_completed
	  && (REGNO (base) == FRAME_POINTER_REGNUM
	      || REGNO (base) == ARG_POINTER_REGNUM))
	continue;

      HOST_WIDE_INT base_off = poly_off.to_constant ();

      // It should be unlikely that we ever punt here, since MEM_EXPR offset
      // alignment should be a good proxy for register offset alignment.
      if (base_off % access_size != 0)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "base not viable, offset misaligned (insn %d)\n",
		     insns[i]->uid ());
	  continue;
	}

      base_off /= access_size;

      if (!is_lower)
	base_off--;

      if (base_off < LDP_MIN_IMM || base_off > LDP_MAX_IMM)
	continue;

      use_info *use = find_access (insns[i]->uses (), REGNO (base));
      gcc_assert (use);
      base_defs[i] = use->def ();
    }

  if (!base_defs[0] && !base_defs[1])
    {
      if (dump_file)
	fprintf (dump_file, "no viable base register for pair (%d,%d)\n",
		 insns[0]->uid (), insns[1]->uid ());
      return writeback;
    }

  for (int i = 0; i < 2; i++)
    if ((writeback & (1 << i)) && !base_defs[i])
      {
	if (dump_file)
	  fprintf (dump_file, "insn %d has writeback but base isn't viable\n",
		   insns[i]->uid ());
	return writeback;
      }

  if (writeback == 3
      && base_defs[0]->regno () != base_defs[1]->regno ())
    {
      if (dump_file)
	fprintf (dump_file,
		 "pair (%d,%d): double writeback with distinct regs (%d,%d): "
		 "punting\n",
		 insns[0]->uid (), insns[1]->uid (),
		 base_defs[0]->regno (), base_defs[1]->regno ());
      return writeback;
    }

  if (base_defs[0] && base_defs[1]
      && base_defs[0]->regno () == base_defs[1]->regno ())
    {
      // Easy case: insns already share the same base reg.
      base_cands.quick_push (base_defs[0]);
      return writeback;
    }

  // Otherwise, we know that one of the bases must change.
  //
  // Note that if there is writeback we must use the writeback base
  // (we know now there is exactly one).
  for (int i = 0; i < 2; i++)
    if (base_defs[i] && (!writeback || (writeback & (1 << i))))
      base_cands.quick_push (base_cand { base_defs[i], i });

  return writeback;
}

// Given two adjacent memory accesses of the same size, I1 and I2, try
// and see if we can merge them into a ldp or stp.
//
// ACCESS_SIZE gives the (common) size of a single access, LOAD_P is true
// if the accesses are both loads, otherwise they are both stores.
bool
ldp_bb_info::try_fuse_pair (bool load_p, unsigned access_size,
			    insn_info *i1, insn_info *i2)
{
  if (dump_file)
    fprintf (dump_file, "analyzing pair (load=%d): (%d,%d)\n",
	     load_p, i1->uid (), i2->uid ());

  insn_info *insns[2];
  bool reversed = false;
  if (*i1 < *i2)
    {
      insns[0] = i1;
      insns[1] = i2;
    }
  else
    {
      insns[0] = i2;
      insns[1] = i1;
      reversed = true;
    }

  rtx cand_mems[2];
  rtx reg_ops[2];
  rtx pats[2];
  for (int i = 0; i < 2; i++)
    {
      pats[i] = PATTERN (insns[i]->rtl ());
      cand_mems[i] = XEXP (pats[i], load_p);
      reg_ops[i] = XEXP (pats[i], !load_p);
    }

  if (load_p && reg_overlap_mentioned_p (reg_ops[0], reg_ops[1]))
    {
      if (dump_file)
	fprintf (dump_file,
		 "punting on ldp due to reg conflcits (%d,%d)\n",
		 insns[0]->uid (), insns[1]->uid ());
      return false;
    }

  if (cfun->can_throw_non_call_exceptions
      && find_reg_note (insns[0]->rtl (), REG_EH_REGION, NULL_RTX)
      && find_reg_note (insns[1]->rtl (), REG_EH_REGION, NULL_RTX))
    {
      if (dump_file)
	fprintf (dump_file,
		 "can't combine insns with EH side effects (%d,%d)\n",
		 insns[0]->uid (), insns[1]->uid ());
      return false;
    }

  auto_vec<base_cand, 2> base_cands (2);

  int writeback = get_viable_bases (insns, base_cands, cand_mems,
				    access_size, reversed);
  if (base_cands.is_empty ())
    {
      if (dump_file)
	fprintf (dump_file, "no viable base for pair (%d,%d)\n",
		 insns[0]->uid (), insns[1]->uid ());
      return false;
    }

  // Punt on frame-related insns with writeback.  We probably won't see
  // these in practice, but this is conservative and ensures we don't
  // have to worry about these later on.
  if (writeback && (RTX_FRAME_RELATED_P (i1->rtl ())
		    || RTX_FRAME_RELATED_P (i2->rtl ())))
    {
      if (dump_file)
	fprintf (dump_file,
		 "rejecting pair (%d,%d): frame-related insn with writeback\n",
		 i1->uid (), i2->uid ());
      return false;
    }

  rtx *ignore = &XEXP (pats[1], load_p);
  for (auto use : insns[1]->uses ())
    if (!use->is_mem ()
	&& refers_to_regno_p (use->regno (), use->regno () + 1, pats[1], ignore)
	&& use->def () && use->def ()->insn () == insns[0])
      {
	// N.B. we allow a true dependence on the base address, as this
	// happens in the case of auto-inc accesses.  Consider a post-increment
	// load followed by a regular indexed load, for example.
	if (dump_file)
	  fprintf (dump_file,
		   "%d has non-address true dependence on %d, rejecting pair\n",
		   insns[1]->uid (), insns[0]->uid ());
	return false;
      }

  unsigned i = 0;
  while (i < base_cands.length ())
    {
      base_cand &cand = base_cands[i];

      rtx *ignore[2] = {};
      for (int j = 0; j < 2; j++)
	if (cand.from_insn == !j)
	  ignore[j] = &XEXP (cand_mems[j], 0);

      insn_info *h = first_hazard_after (insns[0], ignore[0]);
      if (h && *h < *insns[1])
	cand.hazards[0] = h;

      h = latest_hazard_before (insns[1], ignore[1]);
      if (h && *h > *insns[0])
	cand.hazards[1] = h;

      if (!cand.viable ())
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "pair (%d,%d): rejecting base %d due to dataflow "
		     "hazards (%d,%d)\n",
		     insns[0]->uid (),
		     insns[1]->uid (),
		     cand.def->regno (),
		     cand.hazards[0]->uid (),
		     cand.hazards[1]->uid ());

	  base_cands.ordered_remove (i);
	}
      else
	i++;
    }

  if (base_cands.is_empty ())
    {
      if (dump_file)
	fprintf (dump_file,
		 "can't form pair (%d,%d) due to dataflow hazards\n",
		 insns[0]->uid (), insns[1]->uid ());
      return false;
    }

  insn_info *alias_hazards[4] = {};

  // First def of memory after the first insn, and last def of memory
  // before the second insn, respectively.
  def_info *mem_defs[2] = {};
  if (load_p)
    {
      if (!MEM_READONLY_P (cand_mems[0]))
	{
	  mem_defs[0] = memory_access (insns[0]->uses ())->def ();
	  gcc_checking_assert (mem_defs[0]);
	  mem_defs[0] = mem_defs[0]->next_def ();
	}
      if (!MEM_READONLY_P (cand_mems[1]))
	{
	  mem_defs[1] = memory_access (insns[1]->uses ())->def ();
	  gcc_checking_assert (mem_defs[1]);
	}
    }
  else
    {
      mem_defs[0] = memory_access (insns[0]->defs ())->next_def ();
      mem_defs[1] = memory_access (insns[1]->defs ())->prev_def ();
      gcc_checking_assert (mem_defs[0]);
      gcc_checking_assert (mem_defs[1]);
    }

  auto tombstone_p = [&](insn_info *insn) -> bool {
    return m_emitted_tombstone
	   && bitmap_bit_p (&m_tombstone_bitmap, insn->uid ());
  };

  store_walker<false, decltype(tombstone_p)>
    forward_store_walker (mem_defs[0], cand_mems[0], insns[1], tombstone_p);

  store_walker<true, decltype(tombstone_p)>
    backward_store_walker (mem_defs[1], cand_mems[1], insns[0], tombstone_p);

  alias_walker *walkers[4] = {};
  if (mem_defs[0])
    walkers[0] = &forward_store_walker;
  if (mem_defs[1])
    walkers[1] = &backward_store_walker;

  if (load_p && (mem_defs[0] || mem_defs[1]))
    do_alias_analysis (alias_hazards, walkers, load_p);
  else
    {
      // We want to find any loads hanging off the first store.
      mem_defs[0] = memory_access (insns[0]->defs ());
      load_walker<false> forward_load_walker (mem_defs[0], insns[0], insns[1]);
      load_walker<true> backward_load_walker (mem_defs[1], insns[1], insns[0]);
      walkers[2] = &forward_load_walker;
      walkers[3] = &backward_load_walker;
      do_alias_analysis (alias_hazards, walkers, load_p);
      // Now consolidate hazards back down.
      if (alias_hazards[2]
	  && (!alias_hazards[0] || (*alias_hazards[2] < *alias_hazards[0])))
	alias_hazards[0] = alias_hazards[2];

      if (alias_hazards[3]
	  && (!alias_hazards[1] || (*alias_hazards[3] > *alias_hazards[1])))
	alias_hazards[1] = alias_hazards[3];
    }

  if (alias_hazards[0] && alias_hazards[1]
      && *alias_hazards[0] <= *alias_hazards[1])
    {
      if (dump_file)
	fprintf (dump_file,
		 "cannot form pair (%d,%d) due to alias conflicts (%d,%d)\n",
		 i1->uid (), i2->uid (),
		 alias_hazards[0]->uid (), alias_hazards[1]->uid ());
      return false;
    }

  // Now narrow the hazards on each base candidate using
  // the alias hazards.
  i = 0;
  while (i < base_cands.length ())
    {
      base_cand &cand = base_cands[i];
      if (alias_hazards[0] && (!cand.hazards[0]
			       || *alias_hazards[0] < *cand.hazards[0]))
	cand.hazards[0] = alias_hazards[0];
      if (alias_hazards[1] && (!cand.hazards[1]
			       || *alias_hazards[1] > *cand.hazards[1]))
	cand.hazards[1] = alias_hazards[1];

      if (cand.viable ())
	i++;
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "pair (%d,%d): rejecting base %d due to "
				"alias/dataflow hazards (%d,%d)",
				insns[0]->uid (), insns[1]->uid (),
				cand.def->regno (),
				cand.hazards[0]->uid (),
				cand.hazards[1]->uid ());

	  base_cands.ordered_remove (i);
	}
    }

  if (base_cands.is_empty ())
    {
      if (dump_file)
	fprintf (dump_file,
		 "cannot form pair (%d,%d) due to alias/dataflow hazards",
		 insns[0]->uid (), insns[1]->uid ());

      return false;
    }

  base_cand *base = &base_cands[0];
  if (base_cands.length () > 1)
    {
      // If there are still multiple viable bases, it makes sense
      // to choose one that allows us to reduce register pressure,
      // for loads this means moving further down, for stores this
      // means moving further up.
      gcc_checking_assert (base_cands.length () == 2);
      const int hazard_i = !load_p;
      if (base->hazards[hazard_i])
	{
	  if (!base_cands[1].hazards[hazard_i])
	    base = &base_cands[1];
	  else if (load_p
		   && *base_cands[1].hazards[hazard_i]
		      > *(base->hazards[hazard_i]))
	    base = &base_cands[1];
	  else if (!load_p
		   && *base_cands[1].hazards[hazard_i]
		      < *(base->hazards[hazard_i]))
	    base = &base_cands[1];
	}
    }

  // Otherwise, hazards[0] > hazards[1].
  // Pair can be formed anywhere in (hazards[1], hazards[0]).
  insn_range_info range (insns[0], insns[1]);
  if (base->hazards[1])
    range.first = base->hazards[1];
  if (base->hazards[0])
    range.last = base->hazards[0]->prev_nondebug_insn ();

  // If the second insn can throw, narrow the move range to exactly that insn.
  // This prevents us trying to move the second insn from the end of the BB.
  if (cfun->can_throw_non_call_exceptions
      && find_reg_note (insns[1]->rtl (), REG_EH_REGION, NULL_RTX))
    {
      gcc_assert (range.includes (insns[1]));
      range = insn_range_info (insns[1]);
    }

  // Placement strategy: push loads down and pull stores up, this should
  // help register pressure by reducing live ranges.
  if (load_p)
    range.first = range.last;
  else
    range.last = range.first;

  if (dump_file)
    {
      auto print_hazard = [](insn_info *i)
	{
	  if (i)
	    fprintf (dump_file, "%d", i->uid ());
	  else
	    fprintf (dump_file, "-");
	};
      auto print_pair = [print_hazard](insn_info **i)
	{
	  print_hazard (i[0]);
	  fprintf (dump_file, ",");
	  print_hazard (i[1]);
	};

      fprintf (dump_file, "fusing pair [L=%d] (%d,%d), base=%d, hazards: (",
	      load_p, insns[0]->uid (), insns[1]->uid (),
	      base->def->regno ());
      print_pair (base->hazards);
      fprintf (dump_file, "), move_range: (%d,%d)\n",
	       range.first->uid (), range.last->uid ());
    }

  return fuse_pair (load_p, access_size, writeback,
		    i1, i2, *base, range);
}

static void
dump_insn_list (FILE *f, const insn_list_t &l)
{
  fprintf (f, "(");

  auto i = l.begin ();
  auto end = l.end ();

  if (i != end)
    fprintf (f, "%d", (*i)->uid ());
  i++;

  for (; i != end; i++)
    fprintf (f, ", %d", (*i)->uid ());

  fprintf (f, ")");
}

DEBUG_FUNCTION void
debug (const insn_list_t &l)
{
  dump_insn_list (stderr, l);
  fprintf (stderr, "\n");
}

// LEFT_LIST and RIGHT_LIST are lists of candidate instructions where all insns
// in LEFT_LIST are known to be adjacent to those in RIGHT_LIST.
//
// This function traverses the resulting 2D matrix of possible pair candidates
// and attempts to merge them into pairs.
//
// The algorithm is straightforward: if we consider a combined list of
// candidates X obtained by merging LEFT_LIST and RIGHT_LIST in program order,
// then we advance through X until we reach a crossing point (where X[i] and
// X[i+1] come from different source lists).
//
// At this point we know X[i] and X[i+1] are adjacent accesses, and we try to
// fuse them into a pair.  If this succeeds, we remove X[i] and X[i+1] from
// their original lists and continue as above.
//
// In the failure case, we advance through the source list containing X[i] and
// continue as above (proceeding to the next crossing point).
//
// The rationale for skipping over groups of consecutive candidates from the
// same source list is as follows:
//
// In the store case, the insns in the group can't be re-ordered over each
// other as they are guaranteed to store to the same location, so we're
// guaranteed not to lose opportunities by doing this.
//
// In the load case, subsequent loads from the same location are either
// redundant (in which case they should have been cleaned up by an earlier
// optimization pass) or there is an intervening aliasing hazard, in which case
// we can't re-order them anyway, so provided earlier passes have cleaned up
// redundant loads, we shouldn't miss opportunities by doing this.
void
ldp_bb_info::merge_pairs (insn_list_t &left_list,
			  insn_list_t &right_list,
			  bool load_p,
			  unsigned access_size)
{
  if (dump_file)
    {
      fprintf (dump_file, "merge_pairs [L=%d], cand vecs ", load_p);
      dump_insn_list (dump_file, left_list);
      fprintf (dump_file, " x ");
      dump_insn_list (dump_file, right_list);
      fprintf (dump_file, "\n");
    }

  auto iter_l = left_list.begin ();
  auto iter_r = right_list.begin ();

  while (iter_l != left_list.end () && iter_r != right_list.end ())
    {
      auto next_l = std::next (iter_l);
      auto next_r = std::next (iter_r);
      if (**iter_l < **iter_r
	  && next_l != left_list.end ()
	  && **next_l < **iter_r)
	iter_l = next_l;
      else if (**iter_r < **iter_l
	       && next_r != right_list.end ()
	       && **next_r < **iter_l)
	iter_r = next_r;
      else if (try_fuse_pair (load_p, access_size, *iter_l, *iter_r))
	{
	  left_list.erase (iter_l);
	  iter_l = next_l;
	  right_list.erase (iter_r);
	  iter_r = next_r;
	}
      else if (**iter_l < **iter_r)
	iter_l = next_l;
      else
	iter_r = next_r;
    }
}

// Iterate over the accesses in GROUP, looking for adjacent sets
// of accesses.  If we find two sets of adjacent accesses, call
// merge_pairs.
void
ldp_bb_info::transform_for_base (int encoded_lfs,
				 access_group &group)
{
  const auto lfs = decode_lfs (encoded_lfs);
  const unsigned access_size = lfs.size;

  bool skip_next = true;
  access_record *prev_access = nullptr;

  for (auto &access : group.list)
    {
      if (skip_next)
	skip_next = false;
      else if (known_eq (access.offset, prev_access->offset + access_size))
	{
	  merge_pairs (prev_access->cand_insns,
		       access.cand_insns,
		       lfs.load_p,
		       access_size);
	  skip_next = access.cand_insns.empty ();
	}
      prev_access = &access;
    }
}

// If we emitted tombstone insns for this BB, iterate through the BB
// and remove all the tombstone insns, being sure to reparent any uses
// of mem to previous defs when we do this.
void
ldp_bb_info::cleanup_tombstones ()
{
  // No need to do anything if we didn't emit a tombstone insn for this BB.
  if (!m_emitted_tombstone)
    return;

  for (auto insn : iterate_safely (m_bb->nondebug_insns ()))
    {
      if (!insn->is_real ()
	  || !bitmap_bit_p (&m_tombstone_bitmap, insn->uid ()))
	continue;

      auto set = as_a<set_info *> (memory_access (insn->defs ()));
      if (set->has_any_uses ())
	{
	  auto prev_set = as_a<set_info *> (set->prev_def ());
	  while (set->first_use ())
	    crtl->ssa->reparent_use (set->first_use (), prev_set);
	}

      // Now set has no uses, we can delete it.
      insn_change change (insn, insn_change::DELETE);
      crtl->ssa->change_insn (change);
    }
}

template<typename Map>
void
ldp_bb_info::traverse_base_map (Map &map)
{
  for (auto kv : map)
    {
      const auto &key = kv.first;
      auto &value = kv.second;
      transform_for_base (key.second, value);
    }
}

void
ldp_bb_info::transform ()
{
  traverse_base_map (expr_map);
  traverse_base_map (def_map);
}

static void
ldp_fusion_init ()
{
  calculate_dominance_info (CDI_DOMINATORS);
  df_analyze ();
  crtl->ssa = new rtl_ssa::function_info (cfun);
}

static void
ldp_fusion_destroy ()
{
  if (crtl->ssa->perform_pending_updates ())
    cleanup_cfg (0);

  free_dominance_info (CDI_DOMINATORS);

  delete crtl->ssa;
  crtl->ssa = nullptr;
}

// Given a load pair insn in PATTERN, unpack the insn, storing
// the registers in REGS and returning the mem.
static rtx
aarch64_destructure_load_pair (rtx regs[2], rtx pattern)
{
  rtx mem = NULL_RTX;

  for (int i = 0; i < 2; i++)
    {
      rtx pat = XVECEXP (pattern, 0, i);
      regs[i] = XEXP (pat, 0);
      rtx unspec = XEXP (pat, 1);
      gcc_checking_assert (GET_CODE (unspec) == UNSPEC);
      rtx this_mem = XVECEXP (unspec, 0, 0);
      if (mem)
	gcc_checking_assert (rtx_equal_p (mem, this_mem));
      else
	{
	  gcc_checking_assert (MEM_P (this_mem));
	  mem = this_mem;
	}
    }

  return mem;
}

// Given a store pair insn in PATTERN, unpack the insn, storing
// the register operands in REGS, and returning the mem.
static rtx
aarch64_destructure_store_pair (rtx regs[2], rtx pattern)
{
  rtx mem = XEXP (pattern, 0);
  rtx unspec = XEXP (pattern, 1);
  gcc_checking_assert (GET_CODE (unspec) == UNSPEC);
  for (int i = 0; i < 2; i++)
    regs[i] = XVECEXP (unspec, 0, i);
  return mem;
}

// Given a pair mem in PAIR_MEM, register operands in REGS, and an rtx
// representing the effect of writeback on the base register in WB_EFFECT,
// return an insn representing a writeback variant of this pair.
// LOAD_P is true iff the pair is a load.
//
// This is used when promoting existing non-writeback pairs to writeback
// variants.
static rtx
aarch64_gen_writeback_pair (rtx wb_effect, rtx pair_mem, rtx regs[2],
			    bool load_p)
{
  auto op_mode = aarch64_operand_mode_for_pair_mode (GET_MODE (pair_mem));

  machine_mode modes[2];
  for (int i = 0; i < 2; i++)
    {
      machine_mode mode = GET_MODE (regs[i]);
      if (load_p)
	gcc_checking_assert (mode != VOIDmode);
      else if (mode == VOIDmode)
	mode = op_mode;

      modes[i] = mode;
    }

  const auto op_size = GET_MODE_SIZE (modes[0]);
  gcc_checking_assert (known_eq (op_size, GET_MODE_SIZE (modes[1])));

  rtx pats[2];
  for (int i = 0; i < 2; i++)
    {
      rtx mem = adjust_address_nv (pair_mem, modes[i], op_size * i);
      pats[i] = load_p
	? gen_rtx_SET (regs[i], mem)
	: gen_rtx_SET (mem, regs[i]);
    }

  return gen_rtx_PARALLEL (VOIDmode,
			   gen_rtvec (3, wb_effect, pats[0], pats[1]));
}

// Given an existing pair insn INSN, look for a trailing update of
// the base register which we can fold in to make this pair use
// a writeback addressing mode.
static void
try_promote_writeback (insn_info *insn)
{
  auto rti = insn->rtl ();
  const auto attr = get_attr_ldpstp (rti);
  if (attr == LDPSTP_NONE)
    return;

  bool load_p = (attr == LDPSTP_LDP);
  gcc_checking_assert (load_p || attr == LDPSTP_STP);

  rtx regs[2];
  rtx mem = NULL_RTX;
  if (load_p)
    mem = aarch64_destructure_load_pair (regs, PATTERN (rti));
  else
    mem = aarch64_destructure_store_pair (regs, PATTERN (rti));
  gcc_checking_assert (MEM_P (mem));

  poly_int64 offset;
  rtx base = strip_offset (XEXP (mem, 0), &offset);
  gcc_assert (REG_P (base));

  const auto access_size = GET_MODE_SIZE (GET_MODE (mem)).to_constant () / 2;

  if (find_access (insn->defs (), REGNO (base)))
    {
      gcc_assert (load_p);
      if (dump_file)
	fprintf (dump_file,
		 "ldp %d clobbers base r%d, can't promote to writeback\n",
		 insn->uid (), REGNO (base));
      return;
    }

  auto base_use = find_access (insn->uses (), REGNO (base));
  gcc_assert (base_use);

  if (!base_use->def ())
    {
      if (dump_file)
	fprintf (dump_file,
		 "found pair (i%d, L=%d): but base r%d is upwards exposed\n",
		 insn->uid (), load_p, REGNO (base));
      return;
    }

  auto base_def = base_use->def ();

  rtx wb_effect = NULL_RTX;
  def_info *add_def;
  const insn_range_info pair_range (insn);
  insn_info *insns[2] = { nullptr, insn };
  insn_info *trailing_add = find_trailing_add (insns, pair_range, 0, &wb_effect,
					       &add_def, base_def, offset,
					       access_size);
  if (!trailing_add)
    return;

  auto attempt = crtl->ssa->new_change_attempt ();

  insn_change pair_change (insn);
  insn_change del_change (trailing_add, insn_change::DELETE);
  insn_change *changes[] = { &pair_change, &del_change };

  rtx pair_pat = aarch64_gen_writeback_pair (wb_effect, mem, regs, load_p);
  validate_unshare_change (rti, &PATTERN (rti), pair_pat, true);

  // The pair must gain the def of the base register from the add.
  pair_change.new_defs = insert_access (attempt,
					add_def,
					pair_change.new_defs);
  gcc_assert (pair_change.new_defs.is_valid ());

  auto is_changing = insn_is_changing (changes);
  for (unsigned i = 0; i < ARRAY_SIZE (changes); i++)
    gcc_assert (rtl_ssa::restrict_movement_ignoring (*changes[i], is_changing));

  if (!rtl_ssa::recog_ignoring (attempt, pair_change, is_changing))
    {
      if (dump_file)
	fprintf (dump_file, "i%d: recog failed on wb pair, bailing out\n",
		 insn->uid ());
      cancel_changes (0);
      return;
    }

  gcc_assert (crtl->ssa->verify_insn_changes (changes));

  if (MAY_HAVE_DEBUG_INSNS)
    fixup_debug_uses_trailing_add (attempt, insn, trailing_add, wb_effect);

  confirm_change_group ();
  crtl->ssa->change_insns (changes);
}

// Main function for the pass.  Iterate over the insns in BB looking
// for load/store candidates.  If running after RA, also try and promote
// non-writeback pairs to use writeback addressing.  Then try to fuse
// candidates into pairs.
void ldp_fusion_bb (bb_info *bb)
{
  const bool track_loads
    = aarch64_tune_params.ldp_policy_model != AARCH64_LDP_STP_POLICY_NEVER;
  const bool track_stores
    = aarch64_tune_params.stp_policy_model != AARCH64_LDP_STP_POLICY_NEVER;

  ldp_bb_info bb_state (bb);

  for (auto insn : bb->nondebug_insns ())
    {
      rtx_insn *rti = insn->rtl ();

      if (!rti || !INSN_P (rti))
	continue;

      rtx pat = PATTERN (rti);
      if (reload_completed
	  && aarch64_ldp_writeback > 1
	  && GET_CODE (pat) == PARALLEL
	  && XVECLEN (pat, 0) == 2)
	try_promote_writeback (insn);

      if (GET_CODE (pat) != SET)
	continue;

      if (track_stores && MEM_P (XEXP (pat, 0)))
	bb_state.track_access (insn, false, XEXP (pat, 0));
      else if (track_loads && MEM_P (XEXP (pat, 1)))
	bb_state.track_access (insn, true, XEXP (pat, 1));
    }

  bb_state.transform ();
  bb_state.cleanup_tombstones ();
}

void ldp_fusion ()
{
  ldp_fusion_init ();

  for (auto bb : crtl->ssa->bbs ())
    ldp_fusion_bb (bb);

  ldp_fusion_destroy ();
}

namespace {

const pass_data pass_data_ldp_fusion =
{
  RTL_PASS, /* type */
  "ldp_fusion", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_ldp_fusion : public rtl_opt_pass
{
public:
  pass_ldp_fusion (gcc::context *ctx)
    : rtl_opt_pass (pass_data_ldp_fusion, ctx)
    {}

  opt_pass *clone () override { return new pass_ldp_fusion (m_ctxt); }

  bool gate (function *) final override
    {
      if (!optimize || optimize_debug)
	return false;

      // If the tuning policy says never to form ldps or stps, don't run
      // the pass.
      if ((aarch64_tune_params.ldp_policy_model
	   == AARCH64_LDP_STP_POLICY_NEVER)
	  && (aarch64_tune_params.stp_policy_model
	      == AARCH64_LDP_STP_POLICY_NEVER))
	return false;

      if (reload_completed)
	return flag_aarch64_late_ldp_fusion;
      else
	return flag_aarch64_early_ldp_fusion;
    }

  unsigned execute (function *) final override
    {
      ldp_fusion ();
      return 0;
    }
};

} // anon namespace

rtl_opt_pass *
make_pass_ldp_fusion (gcc::context *ctx)
{
  return new pass_ldp_fusion (ctx);
}
