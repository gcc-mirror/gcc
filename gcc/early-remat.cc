/* Early (pre-RA) rematerialization
   Copyright (C) 2017-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "tree-pass.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "insn-config.h"
#include "recog.h"
/* FIXME: The next two are only needed for gen_move_insn.  */
#include "tree.h"
#include "expr.h"
#include "target.h"
#include "inchash.h"
#include "rtlhash.h"
#include "print-rtl.h"
#include "rtl-iter.h"
#include "regs.h"
#include "function-abi.h"

/* This pass runs before register allocation and implements an aggressive
   form of rematerialization.  It looks for pseudo registers R of mode M
   for which:

     (a) there are no call-preserved registers of mode M; and
     (b) spilling R to the stack is expensive.

   The assumption is that it's better to recompute R after each call instead
   of spilling it, even if this extends the live ranges of other registers.

   The motivating example for which these conditions hold are AArch64 SVE
   vectors and predicates.  Spilling them to the stack makes the frame
   variable-sized, which we'd like to avoid if possible.  It's also very
   rare for SVE values to be "naturally" live across a call: usually this
   happens as a result of CSE or other code motion.

   The pass is split into the following phases:

   Collection phase
   ================

   First we go through all pseudo registers looking for any that meet
   the conditions above.  For each such register R, we go through each
   instruction that defines R to see whether any of them are suitable
   rematerialization candidates.  If at least one is, we treat all the
   instructions that define R as candidates, but record which ones are
   not in fact suitable.  These unsuitable candidates exist only for the
   sake of calculating reaching definitions (see below).

   A "candidate" is a single instruction that we want to rematerialize
   and a "candidate register" is a register that is set by at least one
   candidate.

   Candidate sorting
   =================

   Next we sort the candidates based on the cfg postorder, so that if
   candidate C1 uses candidate C2, C1 has a lower index than C2.
   This is useful when iterating through candidate bitmaps.

   Reaching definition calculation
   ===============================

   We then compute standard reaching-definition sets for each candidate.
   Each set specifies which candidates might provide the current definition
   of a live candidate register.

   From here on, a candidate C is "live" at a point P if the candidate
   register defined by C is live at P and if C's definition reaches P.
   An instruction I "uses" a candidate C if I takes the register defined by
   C as input and if C is one of the reaching definitions of that register.

   Candidate validation and value numbering
   ========================================

   Next we simultaneously decide which candidates are valid and look
   for candidates that are equivalent to each other, assigning numbers
   to each unique candidate value.  A candidate C is invalid if:

     (a) C uses an invalid candidate;

     (b) there is a cycle of candidate uses involving C; or

     (c) C takes a candidate register R as input and the reaching
         definitions of R do not have the same value number.

   We assign a "representative" candidate C to each value number and from
   here on replace references to other candidates with that value number
   with references to C.  It is then only possible to rematerialize a
   register R at point P if (after this replacement) there is a single
   reaching definition of R at P.

   Local phase
   ===========

   During this phase we go through each block and look for cases in which:

     (a) an instruction I comes between two call instructions CI1 and CI2;

     (b) I uses a candidate register R;

     (c) a candidate C provides the only reaching definition of R; and

     (d) C does not come between CI1 and I.

   We then emit a copy of C after CI1, as well as the transitive closure
   TC of the candidates used by C.  The copies of TC might use the original
   candidate registers or new temporary registers, depending on circumstances.

   For example, if elsewhere we have:

       C3: R3 <- f3 (...)
	   ...
       C2: R2 <- f2 (...)
	   ...
       C1: R1 <- f1 (R2, R3, ...)  // uses C2 and C3

   then for a block containing:

      CI1: call
	   ...
	I: use R1  // uses C1
	   ...
      CI2: call

   we would emit:

      CI1: call
      C3': R3' <- f3 (...)
      C2': R2' <- f2 (...)
      C1': R1 <- f1 (R2', R3', ...)
	   ...
	I: use R1
	   ...
      CI2: call

   where R2' and R3' might be fresh registers.  If instead we had:

      CI1: call
	   ...
       I1: use R1  // uses C1
	   ...
       I2: use R3  // uses C3
	   ...
      CI2: call

   we would keep the original R3:

      CI1: call
      C3': R3 <- f3 (...)
      C2': R2' <- f2 (...)
      C1': R1 <- f1 (R2', R3, ...)
	   ...
       I1: use R1  // uses C1
	   ...
       I2: use R3  // uses C3
	   ...
      CI2: call

   We also record the last call in each block (if any) and compute:

     rd_after_call:
       The set of candidates that either (a) are defined outside the block
       and are live after the last call or (b) are defined within the block
       and reach the end of the last call.  (We don't track whether the
       latter values are live or not.)

     required_after_call:
       The set of candidates that need to be rematerialized after the
       last call in order to satisfy uses in the block itself.

     required_in:
       The set of candidates that are live on entry to the block and are
       used without an intervening call.

   In addition, we compute the initial values of the sets required by
   the global phase below.

   Global phase
   ============

   We next compute a maximal solution to the following availability
   problem:

     available_in:
       The set of candidates that are live on entry to a block and can
       be used at that point without rematerialization.

     available_out:
       The set of candidates that are live on exit from a block and can
       be used at that point without rematerialization.

     available_locally:
       The subset of available_out that is due to code in the block itself.
       It contains candidates that are defined or used in the block and
       not invalidated by a later call.

   We then go through each block B and look for an appropriate place
   to insert copies of required_in - available_in.  Conceptually we
   start by placing the copies at the head of B, but then move the
   copy of a candidate C to predecessors if:

     (a) that seems cheaper;

     (b) there is more than one reaching definition of C's register at
	 the head of B; or

     (c) copying C would clobber a hard register that is live on entry to B.

   Moving a copy of C to a predecessor block PB involves:

     (1) adding C to PB's required_after_call, if PB contains a call; or

     (2) adding C PB's required_in otherwise.

   C is then available on output from each PB and on input to B.

   Once all this is done, we emit instructions for the final required_in
   and required_after_call sets.  */

namespace {

/* An invalid candidate index, used to indicate that there is more than
   one reaching definition.  */
const unsigned int MULTIPLE_CANDIDATES = -1U;

/* Pass-specific information about one basic block.  */
struct remat_block_info {
  /* The last call instruction in the block.  */
  rtx_insn *last_call;

  /* The set of candidates that are live on entry to the block.  NULL is
     equivalent to an empty set.  */
  bitmap rd_in;

  /* The set of candidates that are live on exit from the block.  This might
     reuse rd_in.  NULL is equivalent to an empty set.  */
  bitmap rd_out;

  /* The subset of RD_OUT that comes from local definitions.  NULL is
     equivalent to an empty set.  */
  bitmap rd_gen;

  /* The set of candidates that the block invalidates (because it defines
     the register to something else, or because the register's value is
     no longer important).  NULL is equivalent to an empty set.  */
  bitmap rd_kill;

  /* The set of candidates that either (a) are defined outside the block
     and are live after LAST_CALL or (b) are defined within the block
     and reach the instruction after LAST_CALL.  (We don't track whether
     the latter values are live or not.)

     Only used if LAST_CALL is nonnull.  NULL is equivalent to an
     empty set.  */
  bitmap rd_after_call;

  /* Candidates that are live and available without rematerialization
     on entry to the block.  NULL is equivalent to an empty set.  */
  bitmap available_in;

  /* Candidates that become available without rematerialization within the
     block, and remain so on exit.  NULL is equivalent to an empty set.  */
  bitmap available_locally;

  /* Candidates that are available without rematerialization on exit from
     the block.  This might reuse available_in or available_locally.  */
  bitmap available_out;

  /* Candidates that need to be rematerialized either at the start of the
     block or before entering the block.  */
  bitmap required_in;

  /* Candidates that need to be rematerialized after LAST_CALL.
     Only used if LAST_CALL is nonnull.  */
  bitmap required_after_call;

  /* The number of candidates in the block.  */
  unsigned int num_candidates;

  /* The earliest candidate in the block (i.e. the one with the
     highest index).  Only valid if NUM_CANDIDATES is nonzero.  */
  unsigned int first_candidate;

  /* The best (lowest) execution frequency for rematerializing REQUIRED_IN.
     This is the execution frequency of the block if LOCAL_REMAT_CHEAPER_P,
     otherwise it is the sum of the execution frequencies of whichever
     predecessor blocks would do the rematerialization.  */
  int remat_frequency;

  /* True if the block ends with an abnormal call.  */
  unsigned int abnormal_call_p : 1;

  /* Used to record whether a graph traversal has visited this block.  */
  unsigned int visited_p : 1;

  /* True if we have calculated REMAT_FREQUENCY.  */
  unsigned int remat_frequency_valid_p : 1;

  /* True if it is cheaper to rematerialize candidates at the start of
     the block, rather than moving them to predecessor blocks.  */
  unsigned int local_remat_cheaper_p : 1;
};

/* Information about a group of candidates with the same value number.  */
struct remat_equiv_class {
  /* The candidates that have the same value number.  */
  bitmap members;

  /* The candidate that was first added to MEMBERS.  */
  unsigned int earliest;

  /* The candidate that represents the others.  This is always the one
     with the highest index.  */
  unsigned int representative;
};

/* Information about an instruction that we might want to rematerialize.  */
struct remat_candidate {
  /* The pseudo register that the instruction sets.  */
  unsigned int regno;

  /* A temporary register used when rematerializing uses of this candidate,
     if REGNO doesn't have the right value or isn't worth using.  */
  unsigned int copy_regno;

  /* True if we intend to rematerialize this instruction by emitting
     a move of a constant into REGNO, false if we intend to emit a
     copy of the original instruction.  */
  unsigned int constant_p : 1;

  /* True if we still think it's possible to rematerialize INSN.  */
  unsigned int can_copy_p : 1;

  /* Used to record whether a graph traversal has visited this candidate.  */
  unsigned int visited_p : 1;

  /* True if we have verified that it's possible to rematerialize INSN.
     Once this is true, both it and CAN_COPY_P remain true.  */
  unsigned int validated_p : 1;

  /* True if we have "stabilized" INSN, i.e. ensured that all non-candidate
     registers read by INSN will have the same value when rematerializing INSN.
     Only ever true if CAN_COPY_P.  */
  unsigned int stabilized_p : 1;

  /* Hash value used for value numbering.  */
  hashval_t hash;

  /* The instruction that sets REGNO.  */
  rtx_insn *insn;

  /* If CONSTANT_P, the value that should be moved into REGNO when
     rematerializing, otherwise the pattern of the instruction that
     should be used.  */
  rtx remat_rtx;

  /* The set of candidates that INSN takes as input.  NULL is equivalent
     to the empty set.  All candidates in this set have a higher index
     than the current candidate.  */
  bitmap uses;

  /* The set of hard registers that would be clobbered by rematerializing
     the candidate, including (transitively) all those that would be
     clobbered by rematerializing USES.  */
  bitmap clobbers;

  /* The equivalence class to which the candidate belongs, or null if none.  */
  remat_equiv_class *equiv_class;
};

/* Hash functions used for value numbering.  */
struct remat_candidate_hasher : nofree_ptr_hash <remat_candidate>
{
  typedef value_type compare_type;
  static hashval_t hash (const remat_candidate *);
  static bool equal (const remat_candidate *, const remat_candidate *);
};

/* Main class for this pass.  */
class early_remat {
public:
  early_remat (function *, sbitmap);
  ~early_remat ();

  void run (void);

private:
  bitmap alloc_bitmap (void);
  bitmap get_bitmap (bitmap *);
  void init_temp_bitmap (bitmap *);
  void copy_temp_bitmap (bitmap *, bitmap *);

  void dump_insn_id (rtx_insn *);
  void dump_candidate_bitmap (bitmap);
  void dump_all_candidates (void);
  void dump_edge_list (basic_block, bool);
  void dump_block_info (basic_block);
  void dump_all_blocks (void);

  bool interesting_regno_p (unsigned int);
  remat_candidate *add_candidate (rtx_insn *, unsigned int, bool);
  bool maybe_add_candidate (rtx_insn *, unsigned int);
  bool collect_candidates (void);
  void init_block_info (void);
  void sort_candidates (void);
  void finalize_candidate_indices (void);
  void record_equiv_candidates (unsigned int, unsigned int);
  static bool rd_confluence_n (edge);
  static bool rd_transfer (int);
  void compute_rd (void);
  unsigned int canon_candidate (unsigned int);
  void canon_bitmap (bitmap *);
  unsigned int resolve_reaching_def (bitmap);
  bool check_candidate_uses (unsigned int);
  void compute_clobbers (unsigned int);
  void assign_value_number (unsigned int);
  void decide_candidate_validity (void);
  void restrict_remat_for_unavail_regs (bitmap, const_bitmap);
  void restrict_remat_for_call (bitmap, rtx_insn *);
  bool stable_use_p (unsigned int);
  void emit_copy_before (unsigned int, rtx, rtx);
  void stabilize_pattern (unsigned int);
  void replace_dest_with_copy (unsigned int);
  void stabilize_candidate_uses (unsigned int, bitmap, bitmap, bitmap,
				 bitmap);
  void emit_remat_insns (bitmap, bitmap, bitmap, rtx_insn *);
  bool set_available_out (remat_block_info *);
  void process_block (basic_block);
  void local_phase (void);
  static bool avail_confluence_n (edge);
  static bool avail_transfer (int);
  void compute_availability (void);
  void unshare_available_sets (remat_block_info *);
  bool can_move_across_edge_p (edge);
  bool local_remat_cheaper_p (unsigned int);
  bool need_to_move_candidate_p (unsigned int, unsigned int);
  void compute_minimum_move_set (unsigned int, bitmap);
  void move_to_predecessors (unsigned int, bitmap, bitmap);
  void choose_rematerialization_points (void);
  void emit_remat_insns_for_block (basic_block);
  void global_phase (void);

  /* The function that we're optimizing.  */
  function *m_fn;

  /* The modes that we want to rematerialize.  */
  sbitmap m_selected_modes;

  /* All rematerialization candidates, identified by their index into the
     vector.  */
  auto_vec<remat_candidate> m_candidates;

  /* The set of candidate registers.  */
  bitmap_head m_candidate_regnos;

  /* Temporary sets.  */
  bitmap_head m_tmp_bitmap;
  bitmap m_available;
  bitmap m_required;

  /* Information about each basic block.  */
  auto_vec<remat_block_info> m_block_info;

  /* A mapping from register numbers to the set of associated candidates.
     Only valid for registers in M_CANDIDATE_REGNOS.  */
  auto_vec<bitmap> m_regno_to_candidates;

  /* An obstack used for allocating bitmaps, so that we can free them all
     in one go.  */
  bitmap_obstack m_obstack;

  /* A hash table of candidates used for value numbering.  If a candidate
     in the table is in an equivalence class, the candidate is marked as
     the earliest member of the class.  */
  hash_table<remat_candidate_hasher> m_value_table;

  /* Used temporarily by callback functions.  */
  static early_remat *er;
};

}

early_remat *early_remat::er;

/* rtx_equal_p callback that treats any two SCRATCHes as equal.
   This allows us to compare two copies of a pattern, even though their
   SCRATCHes are always distinct.  */

static bool
scratch_equal (const_rtx *x, const_rtx *y, rtx *nx, rtx *ny)
{
  if (GET_CODE (*x) == SCRATCH && GET_CODE (*y) == SCRATCH)
    {
      *nx = const0_rtx;
      *ny = const0_rtx;
      return true;
    }
  return false;
}

/* Hash callback functions for remat_candidate.  */

hashval_t
remat_candidate_hasher::hash (const remat_candidate *cand)
{
  return cand->hash;
}

bool
remat_candidate_hasher::equal (const remat_candidate *cand1,
			       const remat_candidate *cand2)
{
  return (cand1->regno == cand2->regno
	  && cand1->constant_p == cand2->constant_p
	  && rtx_equal_p (cand1->remat_rtx, cand2->remat_rtx,
			  cand1->constant_p ? NULL : scratch_equal)
	  && (!cand1->uses || bitmap_equal_p (cand1->uses, cand2->uses)));
}

/* Return true if B is null or empty.  */

inline bool
empty_p (bitmap b)
{
  return !b || bitmap_empty_p (b);
}

/* Allocate a new bitmap.  It will be automatically freed at the end of
   the pass.  */

inline bitmap
early_remat::alloc_bitmap (void)
{
  return bitmap_alloc (&m_obstack);
}

/* Initialize *PTR to an empty bitmap if it is currently null.  */

inline bitmap
early_remat::get_bitmap (bitmap *ptr)
{
  if (!*ptr)
    *ptr = alloc_bitmap ();
  return *ptr;
}

/* *PTR is either null or empty.  If it is null, initialize it to an
   empty bitmap.  */

inline void
early_remat::init_temp_bitmap (bitmap *ptr)
{
  if (!*ptr)
    *ptr = alloc_bitmap ();
  else
    gcc_checking_assert (bitmap_empty_p (*ptr));
}

/* Move *SRC to *DEST and leave *SRC empty.  */

inline void
early_remat::copy_temp_bitmap (bitmap *dest, bitmap *src)
{
  if (!empty_p (*src))
    {
      *dest = *src;
      *src = NULL;
    }
  else
    *dest = NULL;
}

/* Print INSN's identifier to the dump file.  */

void
early_remat::dump_insn_id (rtx_insn *insn)
{
  fprintf (dump_file, "%d[bb:%d]", INSN_UID (insn),
	   BLOCK_FOR_INSN (insn)->index);
}

/* Print candidate set CANDIDATES to the dump file, with a leading space.  */

void
early_remat::dump_candidate_bitmap (bitmap candidates)
{
  if (empty_p (candidates))
    {
      fprintf (dump_file, " none");
      return;
    }

  unsigned int cand_index;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (candidates, 0, cand_index, bi)
    fprintf (dump_file, " %d", cand_index);
}

/* Print information about all candidates to the dump file.  */

void
early_remat::dump_all_candidates (void)
{
  fprintf (dump_file, "\n;; Candidates:\n;;\n");
  fprintf (dump_file, ";; %5s %5s %8s %s\n", "#", "reg", "mode", "insn");
  fprintf (dump_file, ";; %5s %5s %8s %s\n", "=", "===", "====", "====");
  unsigned int cand_index;
  remat_candidate *cand;
  FOR_EACH_VEC_ELT (m_candidates, cand_index, cand)
    {
      fprintf (dump_file, ";; %5d %5d %8s ", cand_index, cand->regno,
	       GET_MODE_NAME (GET_MODE (regno_reg_rtx[cand->regno])));
      dump_insn_id (cand->insn);
      if (!cand->can_copy_p)
	fprintf (dump_file, "   -- can't copy");
      fprintf (dump_file, "\n");
    }

  fprintf (dump_file, "\n;; Register-to-candidate mapping:\n;;\n");
  unsigned int regno;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (&m_candidate_regnos, 0, regno, bi)
    {
      fprintf (dump_file, ";; %5d:", regno);
      dump_candidate_bitmap (m_regno_to_candidates[regno]);
      fprintf (dump_file, "\n");
    }
}

/* Print the predecessors or successors of BB to the dump file, with a
   leading space.  DO_SUCC is true to print successors and false to print
   predecessors.  */

void
early_remat::dump_edge_list (basic_block bb, bool do_succ)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, do_succ ? bb->succs : bb->preds)
    dump_edge_info (dump_file, e, TDF_NONE, do_succ);
}

/* Print information about basic block BB to the dump file.  */

void
early_remat::dump_block_info (basic_block bb)
{
  remat_block_info *info = &m_block_info[bb->index];
  fprintf (dump_file, ";;\n;; Block %d:", bb->index);
  int width = 25;

  fprintf (dump_file, "\n;;%*s:", width, "predecessors");
  dump_edge_list (bb, false);

  fprintf (dump_file, "\n;;%*s:", width, "successors");
  dump_edge_list (bb, true);

  fprintf (dump_file, "\n;;%*s: %d", width, "frequency",
	   bb->count.to_frequency (m_fn));

  if (info->last_call)
    fprintf (dump_file, "\n;;%*s: %d", width, "last call",
	     INSN_UID (info->last_call));

  if (!empty_p (info->rd_in))
    {
      fprintf (dump_file, "\n;;%*s:", width, "RD in");
      dump_candidate_bitmap (info->rd_in);
    }
  if (!empty_p (info->rd_kill))
    {
      fprintf (dump_file, "\n;;%*s:", width, "RD kill");
      dump_candidate_bitmap (info->rd_kill);
    }
  if (!empty_p (info->rd_gen))
    {
      fprintf (dump_file, "\n;;%*s:", width, "RD gen");
      dump_candidate_bitmap (info->rd_gen);
    }
  if (!empty_p (info->rd_after_call))
    {
      fprintf (dump_file, "\n;;%*s:", width, "RD after call");
      dump_candidate_bitmap (info->rd_after_call);
    }
  if (!empty_p (info->rd_out))
    {
      fprintf (dump_file, "\n;;%*s:", width, "RD out");
      if (info->rd_in == info->rd_out)
	fprintf (dump_file, " RD in");
      else
	dump_candidate_bitmap (info->rd_out);
    }
  if (!empty_p (info->available_in))
    {
      fprintf (dump_file, "\n;;%*s:", width, "available in");
      dump_candidate_bitmap (info->available_in);
    }
  if (!empty_p (info->available_locally))
    {
      fprintf (dump_file, "\n;;%*s:", width, "available locally");
      dump_candidate_bitmap (info->available_locally);
    }
  if (!empty_p (info->available_out))
    {
      fprintf (dump_file, "\n;;%*s:", width, "available out");
      if (info->available_in == info->available_out)
	fprintf (dump_file, " available in");
      else if (info->available_locally == info->available_out)
	fprintf (dump_file, " available locally");
      else
	dump_candidate_bitmap (info->available_out);
    }
  if (!empty_p (info->required_in))
    {
      fprintf (dump_file, "\n;;%*s:", width, "required in");
      dump_candidate_bitmap (info->required_in);
    }
  if (!empty_p (info->required_after_call))
    {
      fprintf (dump_file, "\n;;%*s:", width, "required after call");
      dump_candidate_bitmap (info->required_after_call);
    }
  fprintf (dump_file, "\n");
}

/* Print information about all basic blocks to the dump file.  */

void
early_remat::dump_all_blocks (void)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, m_fn)
    dump_block_info (bb);
}

/* Return true if REGNO is worth rematerializing.  */

bool
early_remat::interesting_regno_p (unsigned int regno)
{
  /* Ignore unused registers.  */
  rtx reg = regno_reg_rtx[regno];
  if (!reg || DF_REG_DEF_COUNT (regno) == 0)
    return false;

  /* Make sure the register has a mode that we want to rematerialize.  */
  if (!bitmap_bit_p (m_selected_modes, GET_MODE (reg)))
    return false;

  /* Ignore values that might sometimes be used uninitialized.  We could
     instead add dummy candidates for the entry block definition, and so
     handle uses that are definitely not uninitialized, but the combination
     of the two should be rare in practice.  */
  if (bitmap_bit_p (DF_LR_OUT (ENTRY_BLOCK_PTR_FOR_FN (m_fn)), regno))
    return false;

  return true;
}

/* Record the set of register REGNO in instruction INSN as a
   rematerialization candidate.  CAN_COPY_P is true unless we already
   know that rematerialization is impossible (in which case the candidate
   only exists for the reaching definition calculation).

   The candidate's index is not fixed at this stage.  */

remat_candidate *
early_remat::add_candidate (rtx_insn *insn, unsigned int regno,
			    bool can_copy_p)
{
  remat_candidate cand;
  memset (&cand, 0, sizeof (cand));
  cand.regno = regno;
  cand.insn = insn;
  cand.remat_rtx = PATTERN (insn);
  cand.can_copy_p = can_copy_p;
  m_candidates.safe_push (cand);

  bitmap_set_bit (&m_candidate_regnos, regno);

  return &m_candidates.last ();
}

/* Return true if we can rematerialize the set of register REGNO in
   instruction INSN, and add it as a candidate if so.  When returning
   false, print the reason to the dump file.  */

bool
early_remat::maybe_add_candidate (rtx_insn *insn, unsigned int regno)
{
#define FAILURE_FORMAT ";; Can't rematerialize set of reg %d in %d[bb:%d]: "
#define FAILURE_ARGS regno, INSN_UID (insn), BLOCK_FOR_INSN (insn)->index

  /* The definition must come from an ordinary instruction.  */
  basic_block bb = BLOCK_FOR_INSN (insn);
  if (!NONJUMP_INSN_P (insn)
      || (insn == BB_END (bb)
	  && has_abnormal_or_eh_outgoing_edge_p (bb)))
    {
      if (dump_file)
	fprintf (dump_file, FAILURE_FORMAT "insn alters control flow\n",
		 FAILURE_ARGS);
      return false;
    }

  /* Prefer to rematerialize constants directly -- it's much easier.  */
  machine_mode mode = GET_MODE (regno_reg_rtx[regno]);
  if (rtx note = find_reg_equal_equiv_note (insn))
    {
      rtx val = XEXP (note, 0);
      if (CONSTANT_P (val)
	  && targetm.legitimate_constant_p (mode, val))
	{
	  remat_candidate *cand = add_candidate (insn, regno, true);
	  cand->constant_p = true;
	  cand->remat_rtx = val;
	  return true;
	}
    }

  /* See whether the target has reasons to prevent a copy.  */
  if (targetm.cannot_copy_insn_p && targetm.cannot_copy_insn_p (insn))
    {
      if (dump_file)
	fprintf (dump_file, FAILURE_FORMAT "target forbids copying\n",
		 FAILURE_ARGS);
      return false;
    }

  /* We can't copy trapping instructions.  */
  rtx pat = PATTERN (insn);
  if (may_trap_p (pat))
    {
      if (dump_file)
	fprintf (dump_file, FAILURE_FORMAT "insn might trap\n", FAILURE_ARGS);
      return false;
    }

  /* We can't copy instructions that read memory, unless we know that
     the contents never change.  */
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, pat, ALL)
    if (MEM_P (*iter) && !MEM_READONLY_P (*iter))
      {
	if (dump_file)
	  fprintf (dump_file, FAILURE_FORMAT "insn references non-constant"
		   " memory\n", FAILURE_ARGS);
	return false;
      }

  /* Check each defined register.  */
  df_ref ref;
  FOR_EACH_INSN_DEF (ref, insn)
    {
      unsigned int def_regno = DF_REF_REGNO (ref);
      if (def_regno == regno)
	{
	  /* Make sure the definition is write-only.  (Partial definitions,
	     such as setting the low part and clobbering the high part,
	     are otherwise OK.)  */
	  if (DF_REF_FLAGS_IS_SET (ref, DF_REF_READ_WRITE))
	    {
	      if (dump_file)
		fprintf (dump_file, FAILURE_FORMAT "destination is"
			 " read-modify-write\n", FAILURE_ARGS);
	      return false;
	    }
	}
      else
	{
	  /* The instruction can set additional registers, provided that
	     they're hard registers.  This is useful for instructions
	     that alter the condition codes.  */
	  if (!HARD_REGISTER_NUM_P (def_regno))
	    {
	      if (dump_file)
		fprintf (dump_file, FAILURE_FORMAT "insn also sets"
			 " pseudo reg %d\n", FAILURE_ARGS, def_regno);
	      return false;
	    }
	}
    }

  /* If the instruction uses fixed hard registers, check that those
     registers have the same value throughout the function.  If the
     instruction uses non-fixed hard registers, check that we can
     replace them with pseudos.  */
  FOR_EACH_INSN_USE (ref, insn)
    {
      unsigned int use_regno = DF_REF_REGNO (ref);
      if (HARD_REGISTER_NUM_P (use_regno) && fixed_regs[use_regno])
	{
	  if (rtx_unstable_p (DF_REF_REAL_REG (ref)))
	    {
	      if (dump_file)
		fprintf (dump_file, FAILURE_FORMAT "insn uses fixed hard reg"
			 " %d\n", FAILURE_ARGS, use_regno);
	      return false;
	    }
	}
      else if (HARD_REGISTER_NUM_P (use_regno))
	{
	  /* Allocate a dummy pseudo register and temporarily install it.
	     Make the register number depend on the mode, which should
	     provide enough sharing for match_dup while also weeding
	     out cases in which operands with different modes are
	     explicitly tied.  */
	  rtx *loc = DF_REF_REAL_LOC (ref);
	  unsigned int size = RTX_CODE_SIZE (REG);
	  rtx new_reg = (rtx) alloca (size);
	  memset (new_reg, 0, size);
	  PUT_CODE (new_reg, REG);
	  set_mode_and_regno (new_reg, GET_MODE (*loc),
			      LAST_VIRTUAL_REGISTER + 1 + GET_MODE (*loc));
	  validate_change (insn, loc, new_reg, 1);
	}
    }
  bool ok_p = verify_changes (0);
  cancel_changes (0);
  if (!ok_p)
    {
      if (dump_file)
	fprintf (dump_file, FAILURE_FORMAT "insn does not allow hard"
		 " register inputs to be replaced\n", FAILURE_ARGS);
      return false;
    }

#undef FAILURE_ARGS
#undef FAILURE_FORMAT

  add_candidate (insn, regno, true);
  return true;
}

/* Calculate the set of rematerialization candidates.  Return true if
   we find at least one.  */

bool
early_remat::collect_candidates (void)
{
  unsigned int nregs = DF_REG_SIZE (df);
  for (unsigned int regno = FIRST_PSEUDO_REGISTER; regno < nregs; ++regno)
    if (interesting_regno_p (regno))
      {
	/* Create candidates for all suitable definitions.  */
	bitmap_clear (&m_tmp_bitmap);
	unsigned int bad = 0;
	unsigned int id = 0;
	for (df_ref ref = DF_REG_DEF_CHAIN (regno); ref;
	     ref = DF_REF_NEXT_REG (ref))
	  {
	    rtx_insn *insn = DF_REF_INSN (ref);
	    if (maybe_add_candidate (insn, regno))
	      bitmap_set_bit (&m_tmp_bitmap, id);
	    else
	      bad += 1;
	    id += 1;
	  }

	/* If we found at least one suitable definition, add dummy
	   candidates for the rest, so that we can see which definitions
	   are live where.  */
	if (!bitmap_empty_p (&m_tmp_bitmap) && bad)
	  {
	    id = 0;
	    for (df_ref ref = DF_REG_DEF_CHAIN (regno); ref;
		 ref = DF_REF_NEXT_REG (ref))
	      {
		if (!bitmap_bit_p (&m_tmp_bitmap, id))
		  add_candidate (DF_REF_INSN (ref), regno, false);
		id += 1;
	      }
	  }
      }


  return !m_candidates.is_empty ();
}

/* Initialize the m_block_info array.  */

void
early_remat::init_block_info (void)
{
  unsigned int n_blocks = last_basic_block_for_fn (m_fn);
  m_block_info.safe_grow_cleared (n_blocks, true);
}

/* Maps basic block indices to their position in the forward RPO.  */
static unsigned int *rpo_index;

/* Order remat_candidates X_IN and Y_IN according to the cfg postorder.  */

static int
compare_candidates (const void *x_in, const void *y_in)
{
  const remat_candidate *x = (const remat_candidate *) x_in;
  const remat_candidate *y = (const remat_candidate *) y_in;
  basic_block x_bb = BLOCK_FOR_INSN (x->insn);
  basic_block y_bb = BLOCK_FOR_INSN (y->insn);
  if (x_bb != y_bb)
    /* Make X and Y follow block postorder.  */
    return rpo_index[y_bb->index] - rpo_index[x_bb->index];

  /* Make X and Y follow a backward traversal of the containing block.  */
  return DF_INSN_LUID (y->insn) - DF_INSN_LUID (x->insn);
}

/* Sort the collected rematerialization candidates so that they follow
   cfg postorder.  */

void
early_remat::sort_candidates (void)
{
  /* Make sure the DF LUIDs are up-to-date for all the blocks we
     care about.  */
  bitmap_clear (&m_tmp_bitmap);
  unsigned int cand_index;
  remat_candidate *cand;
  FOR_EACH_VEC_ELT (m_candidates, cand_index, cand)
    {
      basic_block bb = BLOCK_FOR_INSN (cand->insn);
      if (bitmap_set_bit (&m_tmp_bitmap, bb->index))
	df_recompute_luids (bb);
    }

  /* Create a mapping from block numbers to their position in the
     postorder.  */
  unsigned int n_blocks = last_basic_block_for_fn (m_fn);
  int *rpo = df_get_postorder (DF_FORWARD);
  unsigned int rpo_len = df_get_n_blocks (DF_FORWARD);
  rpo_index = new unsigned int[n_blocks];
  for (unsigned int i = 0; i < rpo_len; ++i)
    rpo_index[rpo[i]] = i;

  m_candidates.qsort (compare_candidates);

  delete[] rpo_index;
}

/* Commit to the current candidate indices and initialize cross-references.  */

void
early_remat::finalize_candidate_indices (void)
{
  /* Create a bitmap for each candidate register.  */
  m_regno_to_candidates.safe_grow (max_reg_num (), true);
  unsigned int regno;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (&m_candidate_regnos, 0, regno, bi)
    m_regno_to_candidates[regno] = alloc_bitmap ();

  /* Go through each candidate and record its index.  */
  unsigned int cand_index;
  remat_candidate *cand;
  FOR_EACH_VEC_ELT (m_candidates, cand_index, cand)
    {
      basic_block bb = BLOCK_FOR_INSN (cand->insn);
      remat_block_info *info = &m_block_info[bb->index];
      info->num_candidates += 1;
      info->first_candidate = cand_index;
      bitmap_set_bit (m_regno_to_candidates[cand->regno], cand_index);
    }
}

/* Record that candidates CAND1_INDEX and CAND2_INDEX are equivalent.
   CAND1_INDEX might already have an equivalence class, but CAND2_INDEX
   doesn't.  */

void
early_remat::record_equiv_candidates (unsigned int cand1_index,
				      unsigned int cand2_index)
{
  if (dump_file)
    fprintf (dump_file, ";; Candidate %d is equivalent to candidate %d\n",
	     cand2_index, cand1_index);

  remat_candidate *cand1 = &m_candidates[cand1_index];
  remat_candidate *cand2 = &m_candidates[cand2_index];
  gcc_checking_assert (!cand2->equiv_class);

  remat_equiv_class *ec = cand1->equiv_class;
  if (!ec)
    {
      ec = XOBNEW (&m_obstack.obstack, remat_equiv_class);
      ec->members = alloc_bitmap ();
      bitmap_set_bit (ec->members, cand1_index);
      ec->earliest = cand1_index;
      ec->representative = cand1_index;
      cand1->equiv_class = ec;
    }
  cand2->equiv_class = ec;
  bitmap_set_bit (ec->members, cand2_index);
  if (cand2_index > ec->representative)
    ec->representative = cand2_index;
}

/* Propagate information from the rd_out set of E->src to the rd_in set
   of E->dest, when computing global reaching definitions.  Return true
   if something changed.  */

bool
early_remat::rd_confluence_n (edge e)
{
  remat_block_info *src = &er->m_block_info[e->src->index];
  remat_block_info *dest = &er->m_block_info[e->dest->index];

  /* available_in temporarily contains the set of candidates whose
     registers are live on entry.  */
  if (empty_p (src->rd_out) || empty_p (dest->available_in))
    return false;

  return bitmap_ior_and_into (er->get_bitmap (&dest->rd_in),
			      src->rd_out, dest->available_in);
}

/* Propagate information from the rd_in set of block BB_INDEX to rd_out.
   Return true if something changed.  */

bool
early_remat::rd_transfer (int bb_index)
{
  remat_block_info *info = &er->m_block_info[bb_index];

  if (empty_p (info->rd_in))
    return false;

  if (empty_p (info->rd_kill))
    {
      gcc_checking_assert (empty_p (info->rd_gen));
      if (!info->rd_out)
	info->rd_out = info->rd_in;
      else
	gcc_checking_assert (info->rd_out == info->rd_in);
      /* Assume that we only get called if something changed.  */
      return true;
    }

  if (empty_p (info->rd_gen))
    return bitmap_and_compl (er->get_bitmap (&info->rd_out),
			     info->rd_in, info->rd_kill);

  return bitmap_ior_and_compl (er->get_bitmap (&info->rd_out), info->rd_gen,
			       info->rd_in, info->rd_kill);
}

/* Calculate the rd_* sets for each block.  */

void
early_remat::compute_rd (void)
{
  /* First calculate the rd_kill and rd_gen sets, using the fact
     that m_candidates is sorted in order of decreasing LUID.  */
  unsigned int cand_index;
  remat_candidate *cand;
  FOR_EACH_VEC_ELT_REVERSE (m_candidates, cand_index, cand)
    {
      rtx_insn *insn = cand->insn;
      basic_block bb = BLOCK_FOR_INSN (insn);
      remat_block_info *info = &m_block_info[bb->index];
      bitmap kill = m_regno_to_candidates[cand->regno];
      bitmap_ior_into (get_bitmap (&info->rd_kill), kill);
      if (bitmap_bit_p (DF_LR_OUT (bb), cand->regno))
	{
	  bitmap_and_compl_into (get_bitmap (&info->rd_gen), kill);
	  bitmap_set_bit (info->rd_gen, cand_index);
	}
    }

  /* Set up the initial values of the other sets.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, m_fn)
    {
      remat_block_info *info = &m_block_info[bb->index];
      unsigned int regno;
      bitmap_iterator bi;
      EXECUTE_IF_AND_IN_BITMAP (DF_LR_IN (bb), &m_candidate_regnos,
				0, regno, bi)
	{
	  /* Use available_in to record the set of candidates whose
	     registers are live on entry (i.e. a maximum bound on rd_in).  */
	  bitmap_ior_into (get_bitmap (&info->available_in),
			   m_regno_to_candidates[regno]);

	  /* Add registers that die in a block to the block's kill set,
	     so that we don't needlessly propagate them through the rest
	     of the function.  */
	  if (!bitmap_bit_p (DF_LR_OUT (bb), regno))
	    bitmap_ior_into (get_bitmap (&info->rd_kill),
			     m_regno_to_candidates[regno]);
	}

      /* Initialize each block's rd_out to the minimal set (the set of
	 local definitions).  */
      if (!empty_p (info->rd_gen))
	bitmap_copy (get_bitmap (&info->rd_out), info->rd_gen);
    }

  /* Iterate until we reach a fixed point.  */
  er = this;
  bitmap_clear (&m_tmp_bitmap);
  bitmap_set_range (&m_tmp_bitmap, 0, last_basic_block_for_fn (m_fn));
  df_simple_dataflow (DF_FORWARD, NULL, NULL, rd_confluence_n, rd_transfer,
		      &m_tmp_bitmap, df_get_postorder (DF_FORWARD),
		      df_get_n_blocks (DF_FORWARD));
  er = 0;

  /* Work out which definitions reach which candidates, again taking
     advantage of the candidate order.  */
  bitmap_head reaching;
  bitmap_initialize (&reaching, &m_obstack);
  basic_block old_bb = NULL;
  FOR_EACH_VEC_ELT_REVERSE (m_candidates, cand_index, cand)
    {
      bb = BLOCK_FOR_INSN (cand->insn);
      if (bb != old_bb)
	{
	  /* Get the definitions that reach the start of the new block.  */
	  remat_block_info *info = &m_block_info[bb->index];
	  if (info->rd_in)
	    bitmap_copy (&reaching, info->rd_in);
	  else
	    bitmap_clear (&reaching);
	  old_bb = bb;
	}
      else
	{
	  /* Process the definitions of the previous instruction.  */
	  bitmap kill = m_regno_to_candidates[cand[1].regno];
	  bitmap_and_compl_into (&reaching, kill);
	  bitmap_set_bit (&reaching, cand_index + 1);
	}

      if (cand->can_copy_p && !cand->constant_p)
	{
	  df_ref ref;
	  FOR_EACH_INSN_USE (ref, cand->insn)
	    {
	      unsigned int regno = DF_REF_REGNO (ref);
	      if (bitmap_bit_p (&m_candidate_regnos, regno))
		{
		  bitmap defs = m_regno_to_candidates[regno];
		  bitmap_and (&m_tmp_bitmap, defs, &reaching);
		  bitmap_ior_into (get_bitmap (&cand->uses), &m_tmp_bitmap);
		}
	    }
	}
    }
  bitmap_clear (&reaching);
}

/* If CAND_INDEX is in an equivalence class, return the representative
   of the class, otherwise return CAND_INDEX.  */

inline unsigned int
early_remat::canon_candidate (unsigned int cand_index)
{
  if (remat_equiv_class *ec = m_candidates[cand_index].equiv_class)
    return ec->representative;
  return cand_index;
}

/* Make candidate set *PTR refer to candidates using the representative
   of each equivalence class.  */

void
early_remat::canon_bitmap (bitmap *ptr)
{
  bitmap old_set = *ptr;
  if (empty_p (old_set))
    return;

  bitmap new_set = NULL;
  unsigned int old_index;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (old_set, 0, old_index, bi)
    {
      unsigned int new_index = canon_candidate (old_index);
      if (old_index != new_index)
	{
	  if (!new_set)
	    {
	      new_set = alloc_bitmap ();
	      bitmap_copy (new_set, old_set);
	    }
	  bitmap_clear_bit (new_set, old_index);
	  bitmap_set_bit (new_set, new_index);
	}
    }
  if (new_set)
    {
      BITMAP_FREE (*ptr);
      *ptr = new_set;
    }
}

/* If the candidates in REACHING all have the same value, return the
   earliest instance of that value (i.e. the first one to be added
   to m_value_table), otherwise return MULTIPLE_CANDIDATES.  */

unsigned int
early_remat::resolve_reaching_def (bitmap reaching)
{
  unsigned int cand_index = bitmap_first_set_bit (reaching);
  if (remat_equiv_class *ec = m_candidates[cand_index].equiv_class)
    {
      if (!bitmap_intersect_compl_p (reaching, ec->members))
	return ec->earliest;
    }
  else if (bitmap_single_bit_set_p (reaching))
    return cand_index;

  return MULTIPLE_CANDIDATES;
}

/* Check whether all candidate registers used by candidate CAND_INDEX have
   unique definitions.  Return true if so, replacing the candidate's uses
   set with the appropriate form for value numbering.  */

bool
early_remat::check_candidate_uses (unsigned int cand_index)
{
  remat_candidate *cand = &m_candidates[cand_index];

  /* Process the uses for each register in turn.  */
  bitmap_head uses;
  bitmap_initialize (&uses, &m_obstack);
  bitmap_copy (&uses, cand->uses);
  bitmap uses_ec = alloc_bitmap ();
  while (!bitmap_empty_p (&uses))
    {
      /* Get the register for the lowest-indexed candidate remaining,
	 and the reaching definitions of that register.  */
      unsigned int first = bitmap_first_set_bit (&uses);
      unsigned int regno = m_candidates[first].regno;
      bitmap_and (&m_tmp_bitmap, &uses, m_regno_to_candidates[regno]);

      /* See whether all reaching definitions have the same value and if
	 so get the index of the first candidate we saw with that value.  */
      unsigned int def = resolve_reaching_def (&m_tmp_bitmap);
      if (def == MULTIPLE_CANDIDATES)
	{
	  if (dump_file)
	    fprintf (dump_file, ";; Removing candidate %d because there is"
		     " more than one reaching definition of reg %d\n",
		     cand_index, regno);
	  cand->can_copy_p = false;
	  break;
	}
      bitmap_set_bit (uses_ec, def);
      bitmap_and_compl_into (&uses, &m_tmp_bitmap);
    }
  BITMAP_FREE (cand->uses);
  cand->uses = uses_ec;
  return cand->can_copy_p;
}

/* Calculate the set of hard registers that would be clobbered by
   rematerializing candidate CAND_INDEX.  At this point the candidate's
   set of uses is final.  */

void
early_remat::compute_clobbers (unsigned int cand_index)
{
  remat_candidate *cand = &m_candidates[cand_index];
  if (cand->uses)
    {
      unsigned int use_index;
      bitmap_iterator bi;
      EXECUTE_IF_SET_IN_BITMAP (cand->uses, 0, use_index, bi)
	if (bitmap clobbers = m_candidates[use_index].clobbers)
	  bitmap_ior_into (get_bitmap (&cand->clobbers), clobbers);
    }

  df_ref ref;
  FOR_EACH_INSN_DEF (ref, cand->insn)
    {
      unsigned int def_regno = DF_REF_REGNO (ref);
      if (def_regno != cand->regno)
	bitmap_set_bit (get_bitmap (&cand->clobbers), def_regno);
    }
}

/* Mark candidate CAND_INDEX as validated and add it to the value table.  */

void
early_remat::assign_value_number (unsigned int cand_index)
{
  remat_candidate *cand = &m_candidates[cand_index];
  gcc_checking_assert (cand->can_copy_p && !cand->validated_p);

  compute_clobbers (cand_index);
  cand->validated_p = true;

  inchash::hash h;
  h.add_int (cand->regno);
  inchash::add_rtx (cand->remat_rtx, h);
  cand->hash = h.end ();

  remat_candidate **slot
    = m_value_table.find_slot_with_hash (cand, cand->hash, INSERT);
  if (!*slot)
    {
      *slot = cand;
      if (dump_file)
	fprintf (dump_file, ";; Candidate %d is not equivalent to"
		 " others seen so far\n", cand_index);
    }
  else
    record_equiv_candidates (*slot - m_candidates.address (), cand_index);
}

/* Make a final decision about which candidates are valid and assign
   value numbers to those that are.  */

void
early_remat::decide_candidate_validity (void)
{
  auto_vec<unsigned int, 16> stack;
  unsigned int cand1_index;
  remat_candidate *cand1;
  FOR_EACH_VEC_ELT_REVERSE (m_candidates, cand1_index, cand1)
    {
      if (!cand1->can_copy_p || cand1->validated_p)
	continue;

      if (empty_p (cand1->uses))
	{
	  assign_value_number (cand1_index);
	  continue;
	}

      stack.safe_push (cand1_index);
      while (!stack.is_empty ())
	{
	  unsigned int cand2_index = stack.last ();
	  unsigned int watermark = stack.length ();
	  remat_candidate *cand2 = &m_candidates[cand2_index];
	  if (!cand2->can_copy_p || cand2->validated_p)
	    {
	      stack.pop ();
	      continue;
	    }
	  cand2->visited_p = true;
	  unsigned int cand3_index;
	  bitmap_iterator bi;
	  EXECUTE_IF_SET_IN_BITMAP (cand2->uses, 0, cand3_index, bi)
	    {
	      remat_candidate *cand3 = &m_candidates[cand3_index];
	      if (!cand3->can_copy_p)
		{
		  if (dump_file)
		    fprintf (dump_file, ";; Removing candidate %d because"
			     " it uses removed candidate %d\n", cand2_index,
			     cand3_index);
		  cand2->can_copy_p = false;
		  break;
		}
	      if (!cand3->validated_p)
		{
		  if (empty_p (cand3->uses))
		    assign_value_number (cand3_index);
		  else if (cand3->visited_p)
		    {
		      if (dump_file)
			fprintf (dump_file, ";; Removing candidate %d"
				 " because its definition is cyclic\n",
				 cand2_index);
		      cand2->can_copy_p = false;
		      break;
		    }
		  else
		    stack.safe_push (cand3_index);
		}
	    }
	  if (!cand2->can_copy_p)
	    {
	      cand2->visited_p = false;
	      stack.truncate (watermark - 1);
	    }
	  else if (watermark == stack.length ())
	    {
	      cand2->visited_p = false;
	      if (check_candidate_uses (cand2_index))
		assign_value_number (cand2_index);
	      stack.pop ();
	    }
	}
    }

  /* Ensure that the candidates always use the same candidate index
     to refer to an equivalence class.  */
  FOR_EACH_VEC_ELT_REVERSE (m_candidates, cand1_index, cand1)
    if (cand1->can_copy_p && !empty_p (cand1->uses))
      {
	canon_bitmap (&cand1->uses);
	gcc_checking_assert (bitmap_first_set_bit (cand1->uses) > cand1_index);
      }
}

/* Remove any candidates in CANDIDATES that would clobber a register in
   UNAVAIL_REGS.  */

void
early_remat::restrict_remat_for_unavail_regs (bitmap candidates,
					      const_bitmap unavail_regs)
{
  bitmap_clear (&m_tmp_bitmap);
  unsigned int cand_index;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (candidates, 0, cand_index, bi)
    {
      remat_candidate *cand = &m_candidates[cand_index];
      if (cand->clobbers
	  && bitmap_intersect_p (cand->clobbers, unavail_regs))
	bitmap_set_bit (&m_tmp_bitmap, cand_index);
    }
  bitmap_and_compl_into (candidates, &m_tmp_bitmap);
}

/* Remove any candidates in CANDIDATES that would clobber a register
   that is potentially live across CALL.  */

void
early_remat::restrict_remat_for_call (bitmap candidates, rtx_insn *call)
{
  /* We don't know whether partially-clobbered registers are live
     across the call or not, so assume that they are.  */
  bitmap_view<HARD_REG_SET> call_preserved_regs
    (~insn_callee_abi (call).full_reg_clobbers ());
  restrict_remat_for_unavail_regs (candidates, call_preserved_regs);
}

/* Assuming that every path reaching a point P contains a copy of a
   use U of REGNO, return true if another copy of U at P would have
   access to the same value of REGNO.  */

bool
early_remat::stable_use_p (unsigned int regno)
{
  /* Conservatively assume not for hard registers.  */
  if (HARD_REGISTER_NUM_P (regno))
    return false;

  /* See if REGNO has a single definition and is never used uninitialized.
     In this case the definition of REGNO dominates the common dominator
     of the uses U, which in turn dominates P.  */
  if (DF_REG_DEF_COUNT (regno) == 1
      && !bitmap_bit_p (DF_LR_OUT (ENTRY_BLOCK_PTR_FOR_FN (m_fn)), regno))
    return true;

  return false;
}

/* Emit a copy from register DEST to register SRC before candidate
   CAND_INDEX's instruction.  */

void
early_remat::emit_copy_before (unsigned int cand_index, rtx dest, rtx src)
{
  remat_candidate *cand = &m_candidates[cand_index];
  if (dump_file)
    {
      fprintf (dump_file, ";; Stabilizing insn ");
      dump_insn_id (cand->insn);
      fprintf (dump_file, " by copying source reg %d:%s to temporary reg %d\n",
	       REGNO (src), GET_MODE_NAME (GET_MODE (src)), REGNO (dest));
    }
  emit_insn_before (gen_move_insn (dest, src), cand->insn);
}

/* Check whether any inputs to candidate CAND_INDEX's instruction could
   change at rematerialization points and replace them with new pseudo
   registers if so.  */

void
early_remat::stabilize_pattern (unsigned int cand_index)
{
  remat_candidate *cand = &m_candidates[cand_index];
  if (cand->stabilized_p)
    return;

  remat_equiv_class *ec = cand->equiv_class;
  gcc_checking_assert (!ec || cand_index == ec->representative);

  /* Record the replacements we've made so far, so that we don't
     create two separate registers for match_dups.  Lookup is O(n),
     but the n is very small.  */
  typedef std::pair<rtx, rtx> reg_pair;
  auto_vec<reg_pair, 16> reg_map;

  rtx_insn *insn = cand->insn;
  df_ref ref;
  FOR_EACH_INSN_USE (ref, insn)
    {
      unsigned int old_regno = DF_REF_REGNO (ref);
      rtx *loc = DF_REF_REAL_LOC (ref);

      if (HARD_REGISTER_NUM_P (old_regno) && fixed_regs[old_regno])
	{
	  /* We checked when adding the candidate that the value is stable.  */
	  gcc_checking_assert (!rtx_unstable_p (*loc));
	  continue;
	}

      if (bitmap_bit_p (&m_candidate_regnos, old_regno))
	/* We already know which candidate provides the definition
	   and will handle it during copying.  */
	continue;

      if (stable_use_p (old_regno))
	/* We can continue to use the existing register.  */
	continue;

      /* We need to replace the register.  See whether we've already
	 created a suitable copy.  */
      rtx old_reg = *loc;
      rtx new_reg = NULL_RTX;
      machine_mode mode = GET_MODE (old_reg);
      reg_pair *p;
      unsigned int pi;
      FOR_EACH_VEC_ELT (reg_map, pi, p)
	if (REGNO (p->first) == old_regno
	    && GET_MODE (p->first) == mode)
	  {
	    new_reg = p->second;
	    break;
	  }

      if (!new_reg)
	{
	  /* Create a new register and initialize it just before
	     the instruction.  */
	  new_reg = gen_reg_rtx (mode);
	  reg_map.safe_push (reg_pair (old_reg, new_reg));
	  if (ec)
	    {
	      unsigned int member_index;
	      bitmap_iterator bi;
	      EXECUTE_IF_SET_IN_BITMAP (ec->members, 0, member_index, bi)
		emit_copy_before (member_index, new_reg, old_reg);
	    }
	  else
	    emit_copy_before (cand_index, new_reg, old_reg);
	}
      validate_change (insn, loc, new_reg, true);
    }
  if (num_changes_pending ())
    {
      if (!apply_change_group ())
	/* We checked when adding the candidates that the pattern allows
	   hard registers to be replaced.  Nothing else should make the
	   changes invalid.  */
	gcc_unreachable ();

      if (ec)
	{
	  /* Copy the new pattern to other members of the equivalence
	     class.  */
	  unsigned int member_index;
	  bitmap_iterator bi;
	  EXECUTE_IF_SET_IN_BITMAP (ec->members, 0, member_index, bi)
	    if (cand_index != member_index)
	      {
		rtx_insn *other_insn = m_candidates[member_index].insn;
		if (!validate_change (other_insn, &PATTERN (other_insn),
				      copy_insn (PATTERN (insn)), 0))
		  /* If the original instruction was valid then the copy
		     should be too.  */
		  gcc_unreachable ();
	      }
	}
    }

  cand->stabilized_p = true;
}

/* Change CAND's instruction so that it sets CAND->copy_regno instead
   of CAND->regno.  */

void
early_remat::replace_dest_with_copy (unsigned int cand_index)
{
  remat_candidate *cand = &m_candidates[cand_index];
  df_ref def;
  FOR_EACH_INSN_DEF (def, cand->insn)
    if (DF_REF_REGNO (def) == cand->regno)
      validate_change (cand->insn, DF_REF_REAL_LOC (def),
		       regno_reg_rtx[cand->copy_regno], 1);
}

/* Make sure that the candidates used by candidate CAND_INDEX are available.
   There are two ways of doing this for an input candidate I:

   (1) Using the existing register number and ensuring that I is available.

   (2) Using a new register number (recorded in copy_regno) and adding I
       to VIA_COPY.  This guarantees that making I available does not
       conflict with other uses of the original register.

   REQUIRED is the set of candidates that are required but not available
   before the copy of CAND_INDEX.  AVAILABLE is the set of candidates
   that are already available before the copy of CAND_INDEX.  REACHING
   is the set of candidates that reach the copy of CAND_INDEX.  VIA_COPY
   is the set of candidates that will use new register numbers recorded
   in copy_regno instead of the original ones.  */

void
early_remat::stabilize_candidate_uses (unsigned int cand_index,
				       bitmap required, bitmap available,
				       bitmap reaching, bitmap via_copy)
{
  remat_candidate *cand = &m_candidates[cand_index];
  df_ref use;
  FOR_EACH_INSN_USE (use, cand->insn)
    {
      unsigned int regno = DF_REF_REGNO (use);
      if (!bitmap_bit_p (&m_candidate_regnos, regno))
	continue;

      /* Work out which candidate provides the definition.  */
      bitmap defs = m_regno_to_candidates[regno];
      bitmap_and (&m_tmp_bitmap, cand->uses, defs);
      gcc_checking_assert (bitmap_single_bit_set_p (&m_tmp_bitmap));
      unsigned int def_index = bitmap_first_set_bit (&m_tmp_bitmap);

      /* First see if DEF_INDEX is the only reaching definition of REGNO
	 at this point too and if it is or will become available.  We can
	 continue to use REGNO if so.  */
      bitmap_and (&m_tmp_bitmap, reaching, defs);
      if (bitmap_single_bit_set_p (&m_tmp_bitmap)
	  && bitmap_first_set_bit (&m_tmp_bitmap) == def_index
	  && ((available && bitmap_bit_p (available, def_index))
	      || bitmap_bit_p (required, def_index)))
	{
	  if (dump_file)
	    fprintf (dump_file, ";; Keeping reg %d for use of candidate %d"
		     " in candidate %d\n", regno, def_index, cand_index);
	  continue;
	}

      /* Otherwise fall back to using a copy.  There are other cases
	 in which we *could* continue to use REGNO, but there's not
	 really much point.  Using a separate register ought to make
	 things easier for the register allocator.  */
      remat_candidate *def_cand = &m_candidates[def_index];
      rtx *loc = DF_REF_REAL_LOC (use);
      rtx new_reg;
      if (bitmap_set_bit (via_copy, def_index))
	{
	  new_reg = gen_reg_rtx (GET_MODE (*loc));
	  def_cand->copy_regno = REGNO (new_reg);
	  if (dump_file)
	    fprintf (dump_file, ";; Creating reg %d for use of candidate %d"
		     " in candidate %d\n", REGNO (new_reg), def_index,
		     cand_index);
	}
      else
	new_reg = regno_reg_rtx[def_cand->copy_regno];
      validate_change (cand->insn, loc, new_reg, 1);
    }
}

/* Rematerialize the candidates in REQUIRED after instruction INSN,
   given that the candidates in AVAILABLE are already available
   and that REACHING is the set of candidates live after INSN.
   REQUIRED and AVAILABLE are disjoint on entry.

   Clear REQUIRED on exit.  */

void
early_remat::emit_remat_insns (bitmap required, bitmap available,
			       bitmap reaching, rtx_insn *insn)
{
  /* Quick exit if there's nothing to do.  */
  if (empty_p (required))
    return;

  /* Only reaching definitions should be available or required.  */
  gcc_checking_assert (!bitmap_intersect_compl_p (required, reaching));
  if (available)
    gcc_checking_assert (!bitmap_intersect_compl_p (available, reaching));

  bitmap_head via_copy;
  bitmap_initialize (&via_copy, &m_obstack);
  while (!bitmap_empty_p (required) || !bitmap_empty_p (&via_copy))
    {
      /* Pick the lowest-indexed candidate left.  */
      unsigned int required_index = (bitmap_empty_p (required)
				     ? ~0U : bitmap_first_set_bit (required));
      unsigned int via_copy_index = (bitmap_empty_p (&via_copy)
				     ? ~0U : bitmap_first_set_bit (&via_copy));
      unsigned int cand_index = MIN (required_index, via_copy_index);
      remat_candidate *cand = &m_candidates[cand_index];

      bool via_copy_p = (cand_index == via_copy_index);
      if (via_copy_p)
	bitmap_clear_bit (&via_copy, cand_index);
      else
	{
	  /* Remove all candidates for the same register from REQUIRED.  */
	  bitmap_and (&m_tmp_bitmap, reaching,
		      m_regno_to_candidates[cand->regno]);
	  bitmap_and_compl_into (required, &m_tmp_bitmap);
	  gcc_checking_assert (!bitmap_bit_p (required, cand_index));

	  /* Only rematerialize if we have a single reaching definition
	     of the register.  */
	  if (!bitmap_single_bit_set_p (&m_tmp_bitmap))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, ";; Can't rematerialize reg %d after ",
			   cand->regno);
		  dump_insn_id (insn);
		  fprintf (dump_file, ": more than one reaching definition\n");
		}
	      continue;
	    }

	  /* Skip candidates that can't be rematerialized.  */
	  if (!cand->can_copy_p)
	    continue;

	  /* Check the function precondition.  */
	  gcc_checking_assert (!available
			       || !bitmap_bit_p (available, cand_index));
	}

      /* Invalid candidates should have been weeded out by now.  */
      gcc_assert (cand->can_copy_p);

      rtx new_pattern;
      if (cand->constant_p)
	{
	  /* Emit a simple move.  */
	  unsigned int regno = via_copy_p ? cand->copy_regno : cand->regno;
	  new_pattern = gen_move_insn (regno_reg_rtx[regno], cand->remat_rtx);
	}
      else
	{
	  /* If this is the first time we've copied the instruction, make
	     sure that any inputs will have the same value after INSN.  */
	  stabilize_pattern (cand_index);

	  /* Temporarily adjust the original instruction so that it has
	     the right form for the copy.  */
	  if (via_copy_p)
	    replace_dest_with_copy (cand_index);
	  if (cand->uses)
	    stabilize_candidate_uses (cand_index, required, available,
				      reaching, &via_copy);

	  /* Get the new instruction pattern.  */
	  new_pattern = copy_insn (cand->remat_rtx);

	  /* Undo the temporary changes.  */
	  cancel_changes (0);
	}

      /* Emit the new instruction.  */
      rtx_insn *new_insn = emit_insn_after (new_pattern, insn);

      if (dump_file)
	{
	  fprintf (dump_file, ";; Rematerializing candidate %d after ",
		   cand_index);
	  dump_insn_id (insn);
	  if (via_copy_p)
	    fprintf (dump_file, " with new destination reg %d",
		     cand->copy_regno);
	  fprintf (dump_file, ":\n\n");
	  print_rtl_single (dump_file, new_insn);
	  fprintf (dump_file, "\n");
	}
    }
}

/* Recompute INFO's available_out set, given that it's distinct from
   available_in and available_locally.  */

bool
early_remat::set_available_out (remat_block_info *info)
{
  if (empty_p (info->available_locally))
    return bitmap_and_compl (get_bitmap (&info->available_out),
			     info->available_in, info->rd_kill);

  if (empty_p (info->rd_kill))
    return bitmap_ior (get_bitmap (&info->available_out),
		       info->available_locally, info->available_in);

  return bitmap_ior_and_compl (get_bitmap (&info->available_out),
			       info->available_locally, info->available_in,
			       info->rd_kill);
}

/* If BB has more than one call, decide which candidates should be
   rematerialized after the non-final calls and emit the associated
   instructions.  Record other information about the block in preparation
   for the global phase.  */

void
early_remat::process_block (basic_block bb)
{
  remat_block_info *info = &m_block_info[bb->index];
  rtx_insn *last_call = NULL;
  rtx_insn *insn;

  /* Ensure that we always use the same candidate index to refer to an
     equivalence class.  */
  if (info->rd_out == info->rd_in)
    {
      canon_bitmap (&info->rd_in);
      info->rd_out = info->rd_in;
    }
  else
    {
      canon_bitmap (&info->rd_in);
      canon_bitmap (&info->rd_out);
    }
  canon_bitmap (&info->rd_kill);
  canon_bitmap (&info->rd_gen);

  /* The set of candidates that should be rematerialized on entry to the
     block or after the previous call (whichever is more recent).  */
  init_temp_bitmap (&m_required);

  /* The set of candidates that reach the current instruction (i.e. are
     live just before the instruction).  */
  bitmap_head reaching;
  bitmap_initialize (&reaching, &m_obstack);
  if (info->rd_in)
    bitmap_copy (&reaching, info->rd_in);

  /* The set of candidates that are live and available without
     rematerialization just before the current instruction.  This only
     accounts for earlier candidates in the block, or those that become
     available by being added to M_REQUIRED.  */
  init_temp_bitmap (&m_available);

  /* Get the range of candidates in the block.  */
  unsigned int next_candidate = info->first_candidate;
  unsigned int num_candidates = info->num_candidates;
  remat_candidate *next_def = (num_candidates > 0
			       ? &m_candidates[next_candidate]
			       : NULL);

  FOR_BB_INSNS (bb, insn)
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      /* First process uses, since this is a forward walk.  */
      df_ref ref;
      FOR_EACH_INSN_USE (ref, insn)
	{
	  unsigned int regno = DF_REF_REGNO (ref);
	  if (bitmap_bit_p (&m_candidate_regnos, regno))
	    {
	      bitmap defs = m_regno_to_candidates[regno];
	      bitmap_and (&m_tmp_bitmap, defs, &reaching);
	      gcc_checking_assert (!bitmap_empty_p (&m_tmp_bitmap));
	      if (!bitmap_intersect_p (defs, m_available))
		{
		  /* There has been no definition of the register since
		     the last call or the start of the block (whichever
		     is most recent).  Mark the reaching definitions
		     as required at that point and thus available here.  */
		  bitmap_ior_into (m_required, &m_tmp_bitmap);
		  bitmap_ior_into (m_available, &m_tmp_bitmap);
		}
	    }
	}

      if (CALL_P (insn))
	{
	  if (!last_call)
	    {
	      /* The first call in the block.  Record which candidates are
		 required at the start of the block.  */
	      copy_temp_bitmap (&info->required_in, &m_required);
	      init_temp_bitmap (&m_required);
	    }
	  else
	    {
	      /* The fully-local case: candidates that need to be
		 rematerialized after a previous call in the block.  */
	      restrict_remat_for_call (m_required, last_call);
	      emit_remat_insns (m_required, NULL, info->rd_after_call,
				last_call);
	    }
	  last_call = insn;
	  bitmap_clear (m_available);
	  gcc_checking_assert (empty_p (m_required));
	}

      /* Now process definitions.  */
      while (next_def && insn == next_def->insn)
	{
	  unsigned int gen = canon_candidate (next_candidate);

	  /* Other candidates with the same regno are not available
	     any more.  */
	  bitmap kill = m_regno_to_candidates[next_def->regno];
	  bitmap_and_compl_into (m_available, kill);
	  bitmap_and_compl_into (&reaching, kill);

	  /* Record that this candidate is available without
	     rematerialization.  */
	  bitmap_set_bit (m_available, gen);
	  bitmap_set_bit (&reaching, gen);

	  /* Find the next candidate in the block.  */
	  num_candidates -= 1;
	  next_candidate -= 1;
	  if (num_candidates > 0)
	    next_def -= 1;
	  else
	    next_def = NULL;
	}

      if (insn == last_call)
	bitmap_copy (get_bitmap (&info->rd_after_call), &reaching);
    }
  bitmap_clear (&reaching);
  gcc_checking_assert (num_candidates == 0);

  /* Remove values from the available set if they aren't live (and so
     aren't interesting to successor blocks).  */
  if (info->rd_out)
    bitmap_and_into (m_available, info->rd_out);

  /* Record the accumulated information.  */
  info->last_call = last_call;
  info->abnormal_call_p = (last_call
			   && last_call == BB_END (bb)
			   && has_abnormal_or_eh_outgoing_edge_p (bb));
  copy_temp_bitmap (&info->available_locally, &m_available);
  if (last_call)
    copy_temp_bitmap (&info->required_after_call, &m_required);
  else
    copy_temp_bitmap (&info->required_in, &m_required);

  /* Assume at first that all live-in values are available without
     rematerialization (i.e. start with the most optimistic assumption).  */
  if (info->available_in)
    {
      if (info->rd_in)
	bitmap_copy (info->available_in, info->rd_in);
      else
	BITMAP_FREE (info->available_in);
    }

  if (last_call || empty_p (info->available_in))
    /* The values available on exit from the block are exactly those that
       are available locally.  This set doesn't change.  */
    info->available_out = info->available_locally;
  else if (empty_p (info->available_locally) && empty_p (info->rd_kill))
    /* The values available on exit are the same as those available on entry.
       Updating one updates the other.  */
    info->available_out = info->available_in;
  else
    set_available_out (info);
}

/* Process each block as for process_block, visiting dominators before
   the blocks they dominate.  */

void
early_remat::local_phase (void)
{
  if (dump_file)
    fprintf (dump_file, "\n;; Local phase:\n");

  int *rpo = df_get_postorder (DF_FORWARD);
  unsigned int rpo_len = df_get_n_blocks (DF_FORWARD);
  for (unsigned int i = 0; i < rpo_len; ++i)
    if (rpo[i] >= NUM_FIXED_BLOCKS)
      process_block (BASIC_BLOCK_FOR_FN (m_fn, rpo[i]));
}

/* Return true if available values survive across edge E.  */

static inline bool
available_across_edge_p (edge e)
{
  return (e->flags & EDGE_EH) == 0;
}

/* Propagate information from the available_out set of E->src to the
   available_in set of E->dest, when computing global availability.
   Return true if something changed.  */

bool
early_remat::avail_confluence_n (edge e)
{
  remat_block_info *src = &er->m_block_info[e->src->index];
  remat_block_info *dest = &er->m_block_info[e->dest->index];

  if (!available_across_edge_p (e))
    return false;

  if (empty_p (dest->available_in))
    return false;

  if (!src->available_out)
    {
      bitmap_clear (dest->available_in);
      return true;
    }

  return bitmap_and_into (dest->available_in, src->available_out);
}

/* Propagate information from the available_in set of block BB_INDEX
   to available_out.  Return true if something changed.  */

bool
early_remat::avail_transfer (int bb_index)
{
  remat_block_info *info = &er->m_block_info[bb_index];

  if (info->available_out == info->available_locally)
    return false;

  if (info->available_out == info->available_in)
    /* Assume that we are only called if the input changed.  */
    return true;

  return er->set_available_out (info);
}

/* Compute global availability for the function, starting with the local
   information computed by local_phase.  */

void
early_remat::compute_availability (void)
{
  /* We use df_simple_dataflow instead of the lcm routines for three reasons:

     (1) it avoids recomputing the traversal order;
     (2) many of the sets are likely to be sparse, so we don't necessarily
	 want to use sbitmaps; and
     (3) it means we can avoid creating an explicit kill set for the call.  */
  er = this;
  bitmap_clear (&m_tmp_bitmap);
  bitmap_set_range (&m_tmp_bitmap, 0, last_basic_block_for_fn (m_fn));
  df_simple_dataflow (DF_FORWARD, NULL, NULL,
		      avail_confluence_n, avail_transfer,
		      &m_tmp_bitmap, df_get_postorder (DF_FORWARD),
		      df_get_n_blocks (DF_FORWARD));
  er = 0;

  /* Restrict the required_in sets to values that aren't available.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, m_fn)
    {
      remat_block_info *info = &m_block_info[bb->index];
      if (info->required_in && info->available_in)
	bitmap_and_compl_into (info->required_in, info->available_in);
    }
}

/* Make sure that INFO's available_out and available_in sets are unique.  */

inline void
early_remat::unshare_available_sets (remat_block_info *info)
{
  if (info->available_in && info->available_in == info->available_out)
    {
      info->available_in = alloc_bitmap ();
      bitmap_copy (info->available_in, info->available_out);
    }
}

/* Return true if it is possible to move rematerializations from the
   destination of E to the source of E.  */

inline bool
early_remat::can_move_across_edge_p (edge e)
{
  return (available_across_edge_p (e)
	  && !m_block_info[e->src->index].abnormal_call_p);
}

/* Return true if it is cheaper to rematerialize values at the head of
   block QUERY_BB_INDEX instead of rematerializing in its predecessors.  */

bool
early_remat::local_remat_cheaper_p (unsigned int query_bb_index)
{
  if (m_block_info[query_bb_index].remat_frequency_valid_p)
    return m_block_info[query_bb_index].local_remat_cheaper_p;

  /* Iteratively compute the cost of rematerializing values in the
     predecessor blocks, then compare that with the cost of
     rematerializing at the head of the block.

     A cycle indicates that there is no call on that execution path,
     so it isn't necessary to rematerialize on that path.  */
  auto_vec<basic_block, 16> stack;
  stack.quick_push (BASIC_BLOCK_FOR_FN (m_fn, query_bb_index));
  while (!stack.is_empty ())
    {
      basic_block bb = stack.last ();
      remat_block_info *info = &m_block_info[bb->index];
      if (info->remat_frequency_valid_p)
	{
	  stack.pop ();
	  continue;
	}

      info->visited_p = true;
      int frequency = 0;
      bool can_move_p = true;
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (!can_move_across_edge_p (e))
	  {
	    can_move_p = false;
	    break;
	  }
	else if (m_block_info[e->src->index].last_call)
	  /* We'll rematerialize after the call.  */
	  frequency += e->src->count.to_frequency (m_fn);
	else if (m_block_info[e->src->index].remat_frequency_valid_p)
	  /* Add the cost of rematerializing at the head of E->src
	     or in its predecessors (whichever is cheaper).  */
	  frequency += m_block_info[e->src->index].remat_frequency;
	else if (!m_block_info[e->src->index].visited_p)
	  /* Queue E->src and then revisit this block again.  */
	  stack.safe_push (e->src);

      /* Come back to this block later if we need to process some of
	 its predecessors.  */
      if (stack.last () != bb)
	continue;

      /* If rematerializing in and before the block have equal cost, prefer
	 rematerializing in the block.  This should shorten the live range.  */
      int bb_frequency = bb->count.to_frequency (m_fn);
      if (!can_move_p || frequency >= bb_frequency)
	{
	  info->local_remat_cheaper_p = true;
	  info->remat_frequency = bb_frequency;
	}
      else
	info->remat_frequency = frequency;
      info->remat_frequency_valid_p = true;
      info->visited_p = false;
      if (dump_file)
	{
	  if (!can_move_p)
	    fprintf (dump_file, ";; Need to rematerialize at the head of"
		     " block %d; cannot move to predecessors.\n", bb->index);
	  else
	    {
	      fprintf (dump_file, ";; Block %d has frequency %d,"
		       " rematerializing in predecessors has frequency %d",
		       bb->index, bb_frequency, frequency);
	      if (info->local_remat_cheaper_p)
		fprintf (dump_file, "; prefer to rematerialize"
			 " in the block\n");
	      else
		fprintf (dump_file, "; prefer to rematerialize"
			 " in predecessors\n");
	    }
	}
      stack.pop ();
    }
  return m_block_info[query_bb_index].local_remat_cheaper_p;
}

/* Return true if we cannot rematerialize candidate CAND_INDEX at the head of
   block BB_INDEX.  */

bool
early_remat::need_to_move_candidate_p (unsigned int bb_index,
				       unsigned int cand_index)
{
  remat_block_info *info = &m_block_info[bb_index];
  remat_candidate *cand = &m_candidates[cand_index];
  basic_block bb = BASIC_BLOCK_FOR_FN (m_fn, bb_index);

  /* If there is more than one reaching definition of REGNO,
     we'll need to rematerialize in predecessors instead.  */
  bitmap_and (&m_tmp_bitmap, info->rd_in, m_regno_to_candidates[cand->regno]);
  if (!bitmap_single_bit_set_p (&m_tmp_bitmap))
    {
      if (dump_file)
	fprintf (dump_file, ";; Cannot rematerialize %d at the"
		 " head of block %d because there is more than one"
		 " reaching definition of reg %d\n", cand_index,
		 bb_index, cand->regno);
      return true;
    }

  /* Likewise if rematerializing CAND here would clobber a live register.  */
  if (cand->clobbers
      && bitmap_intersect_p (cand->clobbers, DF_LR_IN (bb)))
    {
      if (dump_file)
	fprintf (dump_file, ";; Cannot rematerialize %d at the"
		 " head of block %d because it would clobber live"
		 " registers\n", cand_index, bb_index);
      return true;
    }

  return false;
}

/* Set REQUIRED to the minimum set of candidates that must be rematerialized
   in predecessors of block BB_INDEX instead of at the start of the block.  */

void
early_remat::compute_minimum_move_set (unsigned int bb_index,
				       bitmap required)
{
  remat_block_info *info = &m_block_info[bb_index];
  bitmap_head remaining;

  bitmap_clear (required);
  bitmap_initialize (&remaining, &m_obstack);
  bitmap_copy (&remaining, info->required_in);
  while (!bitmap_empty_p (&remaining))
    {
      unsigned int cand_index = bitmap_first_set_bit (&remaining);
      remat_candidate *cand = &m_candidates[cand_index];
      bitmap_clear_bit (&remaining, cand_index);

      /* Leave invalid candidates where they are.  */
      if (!cand->can_copy_p)
	continue;

      /* Decide whether to move this candidate.  */
      if (!bitmap_bit_p (required, cand_index))
	{
	  if (!need_to_move_candidate_p (bb_index, cand_index))
	    continue;
	  bitmap_set_bit (required, cand_index);
	}

      /* Also move values used by the candidate, so that we don't
	 rematerialize them twice.  */
      if (cand->uses)
	{
	  bitmap_ior_and_into (required, cand->uses, info->required_in);
	  bitmap_ior_and_into (&remaining, cand->uses, info->required_in);
	}
    }
}

/* Make the predecessors of BB_INDEX rematerialize the candidates in
   REQUIRED.  Add any blocks whose required_in set changes to
   PENDING_BLOCKS.  */

void
early_remat::move_to_predecessors (unsigned int bb_index, bitmap required,
				   bitmap pending_blocks)
{
  if (empty_p (required))
    return;
  remat_block_info *dest_info = &m_block_info[bb_index];
  basic_block bb = BASIC_BLOCK_FOR_FN (m_fn, bb_index);
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      remat_block_info *src_info = &m_block_info[e->src->index];

      /* Restrict the set we add to the reaching definitions.  */
      bitmap_and (&m_tmp_bitmap, required, src_info->rd_out);
      if (bitmap_empty_p (&m_tmp_bitmap))
	continue;

      if (!can_move_across_edge_p (e))
	{
	  /* We can't move the rematerialization and we can't do it at
	     the start of the block either.  In this case we just give up
	     and rely on spilling to make the values available across E.  */
	  if (dump_file)
	    {
	      fprintf (dump_file, ";; Cannot rematerialize the following"
		       " candidates in block %d:", e->src->index);
	      dump_candidate_bitmap (required);
	      fprintf (dump_file, "\n");
	    }
	  continue;
	}

      /* Remove candidates that are already available.  */
      if (src_info->available_out)
	{
	  bitmap_and_compl_into (&m_tmp_bitmap, src_info->available_out);
	  if (bitmap_empty_p (&m_tmp_bitmap))
	    continue;
	}

      /* Add the remaining candidates to the appropriate required set.  */
      if (dump_file)
	{
	  fprintf (dump_file, ";; Moving this set from block %d"
		   " to block %d:", bb_index, e->src->index);
	  dump_candidate_bitmap (&m_tmp_bitmap);
	  fprintf (dump_file, "\n");
	}
      /* If the source block contains a call, we want to rematerialize
	 after the call, otherwise we want to rematerialize at the start
	 of the block.  */
      bitmap src_required = get_bitmap (src_info->last_call
					? &src_info->required_after_call
					: &src_info->required_in);
      if (bitmap_ior_into (src_required, &m_tmp_bitmap))
	{
	  if (!src_info->last_call)
	    bitmap_set_bit (pending_blocks, e->src->index);
	  unshare_available_sets (src_info);
	  bitmap_ior_into (get_bitmap (&src_info->available_out),
			   &m_tmp_bitmap);
	}
    }

  /* The candidates are now available on entry to the block.  */
  bitmap_and_compl_into (dest_info->required_in, required);
  unshare_available_sets (dest_info);
  bitmap_ior_into (get_bitmap (&dest_info->available_in), required);
}

/* Go through the candidates that are currently marked as being
   rematerialized at the beginning of a block.  Decide in each case
   whether that's valid and profitable; if it isn't, move the
   rematerialization to predecessor blocks instead.  */

void
early_remat::choose_rematerialization_points (void)
{
  bitmap_head required;
  bitmap_head pending_blocks;

  int *postorder = df_get_postorder (DF_BACKWARD);
  unsigned int postorder_len = df_get_n_blocks (DF_BACKWARD);
  bitmap_initialize (&required, &m_obstack);
  bitmap_initialize (&pending_blocks, &m_obstack);
  do
    /* Process the blocks in postorder, to reduce the number of iterations
       of the outer loop.  */
    for (unsigned int i = 0; i < postorder_len; ++i)
      {
	unsigned int bb_index = postorder[i];
	remat_block_info *info = &m_block_info[bb_index];
	bitmap_clear_bit (&pending_blocks, bb_index);

	if (empty_p (info->required_in))
	  continue;

	if (info->available_in)
	  gcc_checking_assert (!bitmap_intersect_p (info->required_in,
						    info->available_in));

	if (local_remat_cheaper_p (bb_index))
	  {
	    /* We'd prefer to rematerialize at the head of the block.
	       Only move candidates if we need to.  */
	    compute_minimum_move_set (bb_index, &required);
	    move_to_predecessors (bb_index, &required, &pending_blocks);
	  }
	else
	  move_to_predecessors (bb_index, info->required_in,
				&pending_blocks);
      }
  while (!bitmap_empty_p (&pending_blocks));
  bitmap_clear (&required);
}

/* Emit all rematerialization instructions queued for BB.  */

void
early_remat::emit_remat_insns_for_block (basic_block bb)
{
  remat_block_info *info = &m_block_info[bb->index];

  if (info->last_call && !empty_p (info->required_after_call))
    {
      restrict_remat_for_call (info->required_after_call, info->last_call);
      emit_remat_insns (info->required_after_call, NULL,
			info->rd_after_call, info->last_call);
    }

  if (!empty_p (info->required_in))
    {
      rtx_insn *insn = BB_HEAD (bb);
      while (insn != BB_END (bb)
	     && !INSN_P (NEXT_INSN (insn)))
	insn = NEXT_INSN (insn);
      restrict_remat_for_unavail_regs (info->required_in, DF_LR_IN (bb));
      emit_remat_insns (info->required_in, info->available_in,
			info->rd_in, insn);
    }
}

/* Decide which candidates in each block's REQUIRED_IN set need to be
   rematerialized and decide where the rematerialization instructions
   should go.  Emit queued rematerialization instructions at the start
   of blocks and after the last calls in blocks.  */

void
early_remat::global_phase (void)
{
  compute_availability ();
  if (dump_file)
    {
      fprintf (dump_file, "\n;; Blocks after computing global"
	       " availability:\n");
      dump_all_blocks ();
    }

  choose_rematerialization_points ();
  if (dump_file)
    {
      fprintf (dump_file, "\n;; Blocks after choosing rematerialization"
	       " points:\n");
      dump_all_blocks ();
    }

  basic_block bb;
  FOR_EACH_BB_FN (bb, m_fn)
    emit_remat_insns_for_block (bb);
}

/* Main function for the pass.  */

void
early_remat::run (void)
{
  df_analyze ();

  if (!collect_candidates ())
    return;

  init_block_info ();
  sort_candidates ();
  finalize_candidate_indices ();
  if (dump_file)
    dump_all_candidates ();

  compute_rd ();
  decide_candidate_validity ();
  local_phase ();
  global_phase ();
}

early_remat::early_remat (function *fn, sbitmap selected_modes)
  : m_fn (fn),
    m_selected_modes (selected_modes),
    m_available (0),
    m_required (0),
    m_value_table (63)
{
  bitmap_obstack_initialize (&m_obstack);
  bitmap_initialize (&m_candidate_regnos, &m_obstack);
  bitmap_initialize (&m_tmp_bitmap, &m_obstack);
}

early_remat::~early_remat ()
{
  bitmap_obstack_release (&m_obstack);
}

namespace {

const pass_data pass_data_early_remat =
{
  RTL_PASS, /* type */
  "early_remat", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_EARLY_REMAT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_early_remat : public rtl_opt_pass
{
public:
  pass_early_remat (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_early_remat, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
  {
    return optimize > 1 && NUM_POLY_INT_COEFFS > 1;
  }

  unsigned int execute (function *f) final override
  {
    auto_sbitmap selected_modes (NUM_MACHINE_MODES);
    bitmap_clear (selected_modes);
    targetm.select_early_remat_modes (selected_modes);
    if (!bitmap_empty_p (selected_modes))
      early_remat (f, selected_modes).run ();
    return 0;
  }
}; // class pass_early_remat

} // anon namespace

rtl_opt_pass *
make_pass_early_remat (gcc::context *ctxt)
{
  return new pass_early_remat (ctxt);
}
