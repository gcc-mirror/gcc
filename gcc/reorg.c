/* Perform instruction reorganizations for delay slot filling.
   Copyright (C) 1992-2018 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu).
   Hacked by Michael Tiemann (tiemann@cygnus.com).

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

/* Instruction reorganization pass.

   This pass runs after register allocation and final jump
   optimization.  It should be the last pass to run before peephole.
   It serves primarily to fill delay slots of insns, typically branch
   and call insns.  Other insns typically involve more complicated
   interactions of data dependencies and resource constraints, and
   are better handled by scheduling before register allocation (by the
   function `schedule_insns').

   The Branch Penalty is the number of extra cycles that are needed to
   execute a branch insn.  On an ideal machine, branches take a single
   cycle, and the Branch Penalty is 0.  Several RISC machines approach
   branch delays differently:

   The MIPS has a single branch delay slot.  Most insns
   (except other branches) can be used to fill this slot.  When the
   slot is filled, two insns execute in two cycles, reducing the
   branch penalty to zero.

   The SPARC always has a branch delay slot, but its effects can be
   annulled when the branch is not taken.  This means that failing to
   find other sources of insns, we can hoist an insn from the branch
   target that would only be safe to execute knowing that the branch
   is taken.

   The HP-PA always has a branch delay slot.  For unconditional branches
   its effects can be annulled when the branch is taken.  The effects
   of the delay slot in a conditional branch can be nullified for forward
   taken branches, or for untaken backward branches.  This means
   we can hoist insns from the fall-through path for forward branches or
   steal insns from the target of backward branches.

   The TMS320C3x and C4x have three branch delay slots.  When the three
   slots are filled, the branch penalty is zero.  Most insns can fill the
   delay slots except jump insns.

   Three techniques for filling delay slots have been implemented so far:

   (1) `fill_simple_delay_slots' is the simplest, most efficient way
   to fill delay slots.  This pass first looks for insns which come
   from before the branch and which are safe to execute after the
   branch.  Then it searches after the insn requiring delay slots or,
   in the case of a branch, for insns that are after the point at
   which the branch merges into the fallthrough code, if such a point
   exists.  When such insns are found, the branch penalty decreases
   and no code expansion takes place.

   (2) `fill_eager_delay_slots' is more complicated: it is used for
   scheduling conditional jumps, or for scheduling jumps which cannot
   be filled using (1).  A machine need not have annulled jumps to use
   this strategy, but it helps (by keeping more options open).
   `fill_eager_delay_slots' tries to guess the direction the branch
   will go; if it guesses right 100% of the time, it can reduce the
   branch penalty as much as `fill_simple_delay_slots' does.  If it
   guesses wrong 100% of the time, it might as well schedule nops.  When
   `fill_eager_delay_slots' takes insns from the fall-through path of
   the jump, usually there is no code expansion; when it takes insns
   from the branch target, there is code expansion if it is not the
   only way to reach that target.

   (3) `relax_delay_slots' uses a set of rules to simplify code that
   has been reorganized by (1) and (2).  It finds cases where
   conditional test can be eliminated, jumps can be threaded, extra
   insns can be eliminated, etc.  It is the job of (1) and (2) to do a
   good job of scheduling locally; `relax_delay_slots' takes care of
   making the various individual schedules work well together.  It is
   especially tuned to handle the control flow interactions of branch
   insns.  It does nothing for insns with delay slots that do not
   branch.

   On machines that use CC0, we are very conservative.  We will not make
   a copy of an insn involving CC0 since we want to maintain a 1-1
   correspondence between the insn that sets and uses CC0.  The insns are
   allowed to be separated by placing an insn that sets CC0 (but not an insn
   that uses CC0; we could do this, but it doesn't seem worthwhile) in a
   delay slot.  In that case, we point each insn at the other with REG_CC_USER
   and REG_CC_SETTER notes.  Note that these restrictions affect very few
   machines because most RISC machines with delay slots will not use CC0
   (the RT is the only known exception at this point).  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "memmodel.h"
#include "tm_p.h"
#include "expmed.h"
#include "insn-config.h"
#include "emit-rtl.h"
#include "recog.h"
#include "insn-attr.h"
#include "resource.h"
#include "params.h"
#include "tree-pass.h"


/* First, some functions that were used before GCC got a control flow graph.
   These functions are now only used here in reorg.c, and have therefore
   been moved here to avoid inadvertent misuse elsewhere in the compiler.  */

/* Return the last label to mark the same position as LABEL.  Return LABEL
   itself if it is null or any return rtx.  */

static rtx
skip_consecutive_labels (rtx label_or_return)
{
  rtx_insn *insn;

  if (label_or_return && ANY_RETURN_P (label_or_return))
    return label_or_return;

  rtx_insn *label = as_a <rtx_insn *> (label_or_return);

  for (insn = label; insn != 0 && !INSN_P (insn); insn = NEXT_INSN (insn))
    if (LABEL_P (insn))
      label = insn;

  return label;
}

/* INSN uses CC0 and is being moved into a delay slot.  Set up REG_CC_SETTER
   and REG_CC_USER notes so we can find it.  */

static void
link_cc0_insns (rtx_insn *insn)
{
  rtx user = next_nonnote_insn (insn);

  if (NONJUMP_INSN_P (user) && GET_CODE (PATTERN (user)) == SEQUENCE)
    user = XVECEXP (PATTERN (user), 0, 0);

  add_reg_note (user, REG_CC_SETTER, insn);
  add_reg_note (insn, REG_CC_USER, user);
}

/* Insns which have delay slots that have not yet been filled.  */

static struct obstack unfilled_slots_obstack;
static rtx *unfilled_firstobj;

/* Define macros to refer to the first and last slot containing unfilled
   insns.  These are used because the list may move and its address
   should be recomputed at each use.  */

#define unfilled_slots_base	\
  ((rtx_insn **) obstack_base (&unfilled_slots_obstack))

#define unfilled_slots_next	\
  ((rtx_insn **) obstack_next_free (&unfilled_slots_obstack))

/* Points to the label before the end of the function, or before a
   return insn.  */
static rtx_code_label *function_return_label;
/* Likewise for a simple_return.  */
static rtx_code_label *function_simple_return_label;

/* Mapping between INSN_UID's and position in the code since INSN_UID's do
   not always monotonically increase.  */
static int *uid_to_ruid;

/* Highest valid index in `uid_to_ruid'.  */
static int max_uid;

static int stop_search_p (rtx_insn *, int);
static int resource_conflicts_p (struct resources *, struct resources *);
static int insn_references_resource_p (rtx, struct resources *, bool);
static int insn_sets_resource_p (rtx, struct resources *, bool);
static rtx_code_label *find_end_label (rtx);
static rtx_insn *emit_delay_sequence (rtx_insn *, const vec<rtx_insn *> &,
				      int);
static void add_to_delay_list (rtx_insn *, vec<rtx_insn *> *);
static rtx_insn *delete_from_delay_slot (rtx_insn *);
static void delete_scheduled_jump (rtx_insn *);
static void note_delay_statistics (int, int);
static int get_jump_flags (const rtx_insn *, rtx);
static int mostly_true_jump (rtx);
static rtx get_branch_condition (const rtx_insn *, rtx);
static int condition_dominates_p (rtx, const rtx_insn *);
static int redirect_with_delay_slots_safe_p (rtx_insn *, rtx, rtx);
static int redirect_with_delay_list_safe_p (rtx_insn *, rtx,
					    const vec<rtx_insn *> &);
static int check_annul_list_true_false (int, const vec<rtx_insn *> &);
static void steal_delay_list_from_target (rtx_insn *, rtx, rtx_sequence *,
					  vec<rtx_insn *> *,
					  struct resources *,
					  struct resources *,
					  struct resources *,
					  int, int *, int *,
					  rtx *);
static void steal_delay_list_from_fallthrough (rtx_insn *, rtx, rtx_sequence *,
					       vec<rtx_insn *> *,
					       struct resources *,
					       struct resources *,
					       struct resources *,
					       int, int *, int *);
static void try_merge_delay_insns (rtx_insn *, rtx_insn *);
static rtx_insn *redundant_insn (rtx, rtx_insn *, const vec<rtx_insn *> &);
static int own_thread_p (rtx, rtx, int);
static void update_block (rtx_insn *, rtx_insn *);
static int reorg_redirect_jump (rtx_jump_insn *, rtx);
static void update_reg_dead_notes (rtx_insn *, rtx_insn *);
static void fix_reg_dead_note (rtx_insn *, rtx);
static void update_reg_unused_notes (rtx_insn *, rtx);
static void fill_simple_delay_slots (int);
static void fill_slots_from_thread (rtx_jump_insn *, rtx, rtx, rtx,
				    int, int, int, int,
				    int *, vec<rtx_insn *> *);
static void fill_eager_delay_slots (void);
static void relax_delay_slots (rtx_insn *);
static void make_return_insns (rtx_insn *);

/* A wrapper around next_active_insn which takes care to return ret_rtx
   unchanged.  */

static rtx
first_active_target_insn (rtx insn)
{
  if (ANY_RETURN_P (insn))
    return insn;
  return next_active_insn (as_a <rtx_insn *> (insn));
}

/* Return true iff INSN is a simplejump, or any kind of return insn.  */

static bool
simplejump_or_return_p (rtx insn)
{
  return (JUMP_P (insn)
	  && (simplejump_p (as_a <rtx_insn *> (insn))
	      || ANY_RETURN_P (PATTERN (insn))));
}

/* Return TRUE if this insn should stop the search for insn to fill delay
   slots.  LABELS_P indicates that labels should terminate the search.
   In all cases, jumps terminate the search.  */

static int
stop_search_p (rtx_insn *insn, int labels_p)
{
  if (insn == 0)
    return 1;

  /* If the insn can throw an exception that is caught within the function,
     it may effectively perform a jump from the viewpoint of the function.
     Therefore act like for a jump.  */
  if (can_throw_internal (insn))
    return 1;

  switch (GET_CODE (insn))
    {
    case NOTE:
    case CALL_INSN:
    case DEBUG_INSN:
      return 0;

    case CODE_LABEL:
      return labels_p;

    case JUMP_INSN:
    case BARRIER:
      return 1;

    case INSN:
      /* OK unless it contains a delay slot or is an `asm' insn of some type.
	 We don't know anything about these.  */
      return (GET_CODE (PATTERN (insn)) == SEQUENCE
	      || GET_CODE (PATTERN (insn)) == ASM_INPUT
	      || asm_noperands (PATTERN (insn)) >= 0);

    default:
      gcc_unreachable ();
    }
}

/* Return TRUE if any resources are marked in both RES1 and RES2 or if either
   resource set contains a volatile memory reference.  Otherwise, return FALSE.  */

static int
resource_conflicts_p (struct resources *res1, struct resources *res2)
{
  if ((res1->cc && res2->cc) || (res1->memory && res2->memory)
      || res1->volatil || res2->volatil)
    return 1;

  return hard_reg_set_intersect_p (res1->regs, res2->regs);
}

/* Return TRUE if any resource marked in RES, a `struct resources', is
   referenced by INSN.  If INCLUDE_DELAYED_EFFECTS is set, return if the called
   routine is using those resources.

   We compute this by computing all the resources referenced by INSN and
   seeing if this conflicts with RES.  It might be faster to directly check
   ourselves, and this is the way it used to work, but it means duplicating
   a large block of complex code.  */

static int
insn_references_resource_p (rtx insn, struct resources *res,
			    bool include_delayed_effects)
{
  struct resources insn_res;

  CLEAR_RESOURCE (&insn_res);
  mark_referenced_resources (insn, &insn_res, include_delayed_effects);
  return resource_conflicts_p (&insn_res, res);
}

/* Return TRUE if INSN modifies resources that are marked in RES.
   INCLUDE_DELAYED_EFFECTS is set if the actions of that routine should be
   included.   CC0 is only modified if it is explicitly set; see comments
   in front of mark_set_resources for details.  */

static int
insn_sets_resource_p (rtx insn, struct resources *res,
		      bool include_delayed_effects)
{
  struct resources insn_sets;

  CLEAR_RESOURCE (&insn_sets);
  mark_set_resources (insn, &insn_sets, 0,
		      (include_delayed_effects
		       ? MARK_SRC_DEST_CALL
		       : MARK_SRC_DEST));
  return resource_conflicts_p (&insn_sets, res);
}

/* Find a label at the end of the function or before a RETURN.  If there
   is none, try to make one.  If that fails, returns 0.

   The property of such a label is that it is placed just before the
   epilogue or a bare RETURN insn, so that another bare RETURN can be
   turned into a jump to the label unconditionally.  In particular, the
   label cannot be placed before a RETURN insn with a filled delay slot.

   ??? There may be a problem with the current implementation.  Suppose
   we start with a bare RETURN insn and call find_end_label.  It may set
   function_return_label just before the RETURN.  Suppose the machinery
   is able to fill the delay slot of the RETURN insn afterwards.  Then
   function_return_label is no longer valid according to the property
   described above and find_end_label will still return it unmodified.
   Note that this is probably mitigated by the following observation:
   once function_return_label is made, it is very likely the target of
   a jump, so filling the delay slot of the RETURN will be much more
   difficult.
   KIND is either simple_return_rtx or ret_rtx, indicating which type of
   return we're looking for.  */

static rtx_code_label *
find_end_label (rtx kind)
{
  rtx_insn *insn;
  rtx_code_label **plabel;

  if (kind == ret_rtx)
    plabel = &function_return_label;
  else
    {
      gcc_assert (kind == simple_return_rtx);
      plabel = &function_simple_return_label;
    }

  /* If we found one previously, return it.  */
  if (*plabel)
    return *plabel;

  /* Otherwise, see if there is a label at the end of the function.  If there
     is, it must be that RETURN insns aren't needed, so that is our return
     label and we don't have to do anything else.  */

  insn = get_last_insn ();
  while (NOTE_P (insn)
	 || (NONJUMP_INSN_P (insn)
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER
		 || GET_CODE (PATTERN (insn)) == CLOBBER_HIGH)))
    insn = PREV_INSN (insn);

  /* When a target threads its epilogue we might already have a
     suitable return insn.  If so put a label before it for the
     function_return_label.  */
  if (BARRIER_P (insn)
      && JUMP_P (PREV_INSN (insn))
      && PATTERN (PREV_INSN (insn)) == kind)
    {
      rtx_insn *temp = PREV_INSN (PREV_INSN (insn));
      rtx_code_label *label = gen_label_rtx ();
      LABEL_NUSES (label) = 0;

      /* Put the label before any USE insns that may precede the RETURN
	 insn.  */
      while (GET_CODE (temp) == USE)
	temp = PREV_INSN (temp);

      emit_label_after (label, temp);
      *plabel = label;
    }

  else if (LABEL_P (insn))
    *plabel = as_a <rtx_code_label *> (insn);
  else
    {
      rtx_code_label *label = gen_label_rtx ();
      LABEL_NUSES (label) = 0;
      /* If the basic block reorder pass moves the return insn to
	 some other place try to locate it again and put our
	 function_return_label there.  */
      while (insn && ! (JUMP_P (insn) && (PATTERN (insn) == kind)))
	insn = PREV_INSN (insn);
      if (insn)
	{
	  insn = PREV_INSN (insn);

	  /* Put the label before any USE insns that may precede the
	     RETURN insn.  */
	  while (GET_CODE (insn) == USE)
	    insn = PREV_INSN (insn);

	  emit_label_after (label, insn);
	}
      else
	{
	  if (targetm.have_epilogue () && ! targetm.have_return ())
	    /* The RETURN insn has its delay slot filled so we cannot
	       emit the label just before it.  Since we already have
	       an epilogue and cannot emit a new RETURN, we cannot
	       emit the label at all.  */
	    return NULL;

	  /* Otherwise, make a new label and emit a RETURN and BARRIER,
	     if needed.  */
	  emit_label (label);
	  if (targetm.have_return ())
	    {
	      /* The return we make may have delay slots too.  */
	      rtx_insn *pat = targetm.gen_return ();
	      rtx_insn *insn = emit_jump_insn (pat);
	      set_return_jump_label (insn);
	      emit_barrier ();
	      if (num_delay_slots (insn) > 0)
		obstack_ptr_grow (&unfilled_slots_obstack, insn);
	    }
	}
      *plabel = label;
    }

  /* Show one additional use for this label so it won't go away until
     we are done.  */
  ++LABEL_NUSES (*plabel);

  return *plabel;
}

/* Put INSN and LIST together in a SEQUENCE rtx of LENGTH, and replace
   the pattern of INSN with the SEQUENCE.

   Returns the insn containing the SEQUENCE that replaces INSN.  */

static rtx_insn *
emit_delay_sequence (rtx_insn *insn, const vec<rtx_insn *> &list, int length)
{
  /* Allocate the rtvec to hold the insns and the SEQUENCE.  */
  rtvec seqv = rtvec_alloc (length + 1);
  rtx seq = gen_rtx_SEQUENCE (VOIDmode, seqv);
  rtx_insn *seq_insn = make_insn_raw (seq);

  /* If DELAY_INSN has a location, use it for SEQ_INSN.  If DELAY_INSN does
     not have a location, but one of the delayed insns does, we pick up a
     location from there later.  */
  INSN_LOCATION (seq_insn) = INSN_LOCATION (insn);

  /* Unlink INSN from the insn chain, so that we can put it into
     the SEQUENCE.   Remember where we want to emit SEQUENCE in AFTER.  */
  rtx_insn *after = PREV_INSN (insn);
  remove_insn (insn);
  SET_NEXT_INSN (insn) = SET_PREV_INSN (insn) = NULL;

  /* Build our SEQUENCE and rebuild the insn chain.  */
  start_sequence ();
  XVECEXP (seq, 0, 0) = emit_insn (insn);

  unsigned int delay_insns = list.length ();
  gcc_assert (delay_insns == (unsigned int) length);
  for (unsigned int i = 0; i < delay_insns; i++)
    {
      rtx_insn *tem = list[i];
      rtx note, next;

      /* Show that this copy of the insn isn't deleted.  */
      tem->set_undeleted ();

      /* Unlink insn from its original place, and re-emit it into
	 the sequence.  */
      SET_NEXT_INSN (tem) = SET_PREV_INSN (tem) = NULL;
      XVECEXP (seq, 0, i + 1) = emit_insn (tem);

      /* SPARC assembler, for instance, emit warning when debug info is output
         into the delay slot.  */
      if (INSN_LOCATION (tem) && !INSN_LOCATION (seq_insn))
	INSN_LOCATION (seq_insn) = INSN_LOCATION (tem);
      INSN_LOCATION (tem) = 0;

      for (note = REG_NOTES (tem); note; note = next)
	{
	  next = XEXP (note, 1);
	  switch (REG_NOTE_KIND (note))
	    {
	    case REG_DEAD:
	      /* Remove any REG_DEAD notes because we can't rely on them now
		 that the insn has been moved.  */
	      remove_note (tem, note);
	      break;

	    case REG_LABEL_OPERAND:
	    case REG_LABEL_TARGET:
	      /* Keep the label reference count up to date.  */
	      if (LABEL_P (XEXP (note, 0)))
		LABEL_NUSES (XEXP (note, 0)) ++;
	      break;

	    default:
	      break;
	    }
	}
    }
  end_sequence ();

  /* Splice our SEQUENCE into the insn stream where INSN used to be.  */
  add_insn_after (seq_insn, after, NULL);

  return seq_insn;
}

/* Add INSN to DELAY_LIST and return the head of the new list.  The list must
   be in the order in which the insns are to be executed.  */

static void
add_to_delay_list (rtx_insn *insn, vec<rtx_insn *> *delay_list)
{
  /* If INSN has its block number recorded, clear it since we may
     be moving the insn to a new block.  */
      clear_hashed_info_for_insn (insn);
      delay_list->safe_push (insn);
}

/* Delete INSN from the delay slot of the insn that it is in, which may
   produce an insn with no delay slots.  Return the new insn.  */

static rtx_insn *
delete_from_delay_slot (rtx_insn *insn)
{
  rtx_insn *trial, *seq_insn, *prev;
  rtx_sequence *seq;
  int i;
  int had_barrier = 0;

  /* We first must find the insn containing the SEQUENCE with INSN in its
     delay slot.  Do this by finding an insn, TRIAL, where
     PREV_INSN (NEXT_INSN (TRIAL)) != TRIAL.  */

  for (trial = insn;
       PREV_INSN (NEXT_INSN (trial)) == trial;
       trial = NEXT_INSN (trial))
    ;

  seq_insn = PREV_INSN (NEXT_INSN (trial));
  seq = as_a <rtx_sequence *> (PATTERN (seq_insn));

  if (NEXT_INSN (seq_insn) && BARRIER_P (NEXT_INSN (seq_insn)))
    had_barrier = 1;

  /* Create a delay list consisting of all the insns other than the one
     we are deleting (unless we were the only one).  */
  auto_vec<rtx_insn *, 5> delay_list;
  if (seq->len () > 2)
    for (i = 1; i < seq->len (); i++)
      if (seq->insn (i) != insn)
	add_to_delay_list (seq->insn (i), &delay_list);

  /* Delete the old SEQUENCE, re-emit the insn that used to have the delay
     list, and rebuild the delay list if non-empty.  */
  prev = PREV_INSN (seq_insn);
  trial = seq->insn (0);
  delete_related_insns (seq_insn);
  add_insn_after (trial, prev, NULL);

  /* If there was a barrier after the old SEQUENCE, remit it.  */
  if (had_barrier)
    emit_barrier_after (trial);

  /* If there are any delay insns, remit them.  Otherwise clear the
     annul flag.  */
  if (!delay_list.is_empty ())
    trial = emit_delay_sequence (trial, delay_list, XVECLEN (seq, 0) - 2);
  else if (JUMP_P (trial))
    INSN_ANNULLED_BRANCH_P (trial) = 0;

  INSN_FROM_TARGET_P (insn) = 0;

  /* Show we need to fill this insn again.  */
  obstack_ptr_grow (&unfilled_slots_obstack, trial);

  return trial;
}

/* Delete INSN, a JUMP_INSN.  If it is a conditional jump, we must track down
   the insn that sets CC0 for it and delete it too.  */

static void
delete_scheduled_jump (rtx_insn *insn)
{
  /* Delete the insn that sets cc0 for us.  On machines without cc0, we could
     delete the insn that sets the condition code, but it is hard to find it.
     Since this case is rare anyway, don't bother trying; there would likely
     be other insns that became dead anyway, which we wouldn't know to
     delete.  */

  if (HAVE_cc0 && reg_mentioned_p (cc0_rtx, insn))
    {
      rtx note = find_reg_note (insn, REG_CC_SETTER, NULL_RTX);

      /* If a reg-note was found, it points to an insn to set CC0.  This
	 insn is in the delay list of some other insn.  So delete it from
	 the delay list it was in.  */
      if (note)
	{
	  if (! FIND_REG_INC_NOTE (XEXP (note, 0), NULL_RTX)
	      && sets_cc0_p (PATTERN (XEXP (note, 0))) == 1)
	    delete_from_delay_slot (as_a <rtx_insn *> (XEXP (note, 0)));
	}
      else
	{
	  /* The insn setting CC0 is our previous insn, but it may be in
	     a delay slot.  It will be the last insn in the delay slot, if
	     it is.  */
	  rtx_insn *trial = previous_insn (insn);
	  if (NOTE_P (trial))
	    trial = prev_nonnote_insn (trial);
	  if (sets_cc0_p (PATTERN (trial)) != 1
	      || FIND_REG_INC_NOTE (trial, NULL_RTX))
	    return;
	  if (PREV_INSN (NEXT_INSN (trial)) == trial)
	    delete_related_insns (trial);
	  else
	    delete_from_delay_slot (trial);
	}
    }

  delete_related_insns (insn);
}

/* Counters for delay-slot filling.  */

#define NUM_REORG_FUNCTIONS 2
#define MAX_DELAY_HISTOGRAM 3
#define MAX_REORG_PASSES 2

static int num_insns_needing_delays[NUM_REORG_FUNCTIONS][MAX_REORG_PASSES];

static int num_filled_delays[NUM_REORG_FUNCTIONS][MAX_DELAY_HISTOGRAM+1][MAX_REORG_PASSES];

static int reorg_pass_number;

static void
note_delay_statistics (int slots_filled, int index)
{
  num_insns_needing_delays[index][reorg_pass_number]++;
  if (slots_filled > MAX_DELAY_HISTOGRAM)
    slots_filled = MAX_DELAY_HISTOGRAM;
  num_filled_delays[index][slots_filled][reorg_pass_number]++;
}

/* Optimize the following cases:

   1.  When a conditional branch skips over only one instruction,
       use an annulling branch and put that insn in the delay slot.
       Use either a branch that annuls when the condition if true or
       invert the test with a branch that annuls when the condition is
       false.  This saves insns, since otherwise we must copy an insn
       from the L1 target.

        (orig)		 (skip)		(otherwise)
	Bcc.n L1	Bcc',a L1	Bcc,a L1'
	insn		insn		insn2
      L1:	      L1:	      L1:
	insn2		insn2		insn2
	insn3		insn3	      L1':
					insn3

   2.  When a conditional branch skips over only one instruction,
       and after that, it unconditionally branches somewhere else,
       perform the similar optimization. This saves executing the
       second branch in the case where the inverted condition is true.

	Bcc.n L1	Bcc',a L2
	insn		insn
      L1:	      L1:
	Bra L2		Bra L2

   INSN is a JUMP_INSN.

   This should be expanded to skip over N insns, where N is the number
   of delay slots required.  */

static void
optimize_skip (rtx_jump_insn *insn, vec<rtx_insn *> *delay_list)
{
  rtx_insn *trial = next_nonnote_insn (insn);
  rtx_insn *next_trial = next_active_insn (trial);
  int flags;

  flags = get_jump_flags (insn, JUMP_LABEL (insn));

  if (trial == 0
      || !NONJUMP_INSN_P (trial)
      || GET_CODE (PATTERN (trial)) == SEQUENCE
      || recog_memoized (trial) < 0
      || (! eligible_for_annul_false (insn, 0, trial, flags)
	  && ! eligible_for_annul_true (insn, 0, trial, flags))
      || RTX_FRAME_RELATED_P (trial)
      || can_throw_internal (trial))
    return;

  /* There are two cases where we are just executing one insn (we assume
     here that a branch requires only one insn; this should be generalized
     at some point):  Where the branch goes around a single insn or where
     we have one insn followed by a branch to the same label we branch to.
     In both of these cases, inverting the jump and annulling the delay
     slot give the same effect in fewer insns.  */
  if (next_trial == next_active_insn (JUMP_LABEL_AS_INSN (insn))
      || (next_trial != 0
	  && simplejump_or_return_p (next_trial)
	  && JUMP_LABEL (insn) == JUMP_LABEL (next_trial)))
    {
      if (eligible_for_annul_false (insn, 0, trial, flags))
	{
	  if (invert_jump (insn, JUMP_LABEL (insn), 1))
	    INSN_FROM_TARGET_P (trial) = 1;
	  else if (! eligible_for_annul_true (insn, 0, trial, flags))
	    return;
	}

      add_to_delay_list (trial, delay_list);
      next_trial = next_active_insn (trial);
      update_block (trial, trial);
      delete_related_insns (trial);

      /* Also, if we are targeting an unconditional
	 branch, thread our jump to the target of that branch.  Don't
	 change this into a RETURN here, because it may not accept what
	 we have in the delay slot.  We'll fix this up later.  */
      if (next_trial && simplejump_or_return_p (next_trial))
	{
	  rtx target_label = JUMP_LABEL (next_trial);
	  if (ANY_RETURN_P (target_label))
	    target_label = find_end_label (target_label);

	  if (target_label)
	    {
	      /* Recompute the flags based on TARGET_LABEL since threading
		 the jump to TARGET_LABEL may change the direction of the
		 jump (which may change the circumstances in which the
		 delay slot is nullified).  */
	      flags = get_jump_flags (insn, target_label);
	      if (eligible_for_annul_true (insn, 0, trial, flags))
		reorg_redirect_jump (insn, target_label);
	    }
	}

      INSN_ANNULLED_BRANCH_P (insn) = 1;
    }
}

/*  Encode and return branch direction and prediction information for
    INSN assuming it will jump to LABEL.

    Non conditional branches return no direction information and
    are predicted as very likely taken.  */

static int
get_jump_flags (const rtx_insn *insn, rtx label)
{
  int flags;

  /* get_jump_flags can be passed any insn with delay slots, these may
     be INSNs, CALL_INSNs, or JUMP_INSNs.  Only JUMP_INSNs have branch
     direction information, and only if they are conditional jumps.

     If LABEL is a return, then there is no way to determine the branch
     direction.  */
  if (JUMP_P (insn)
      && (condjump_p (insn) || condjump_in_parallel_p (insn))
      && !ANY_RETURN_P (label)
      && INSN_UID (insn) <= max_uid
      && INSN_UID (label) <= max_uid)
    flags
      = (uid_to_ruid[INSN_UID (label)] > uid_to_ruid[INSN_UID (insn)])
	 ? ATTR_FLAG_forward : ATTR_FLAG_backward;
  /* No valid direction information.  */
  else
    flags = 0;

  return flags;
}

/* Return truth value of the statement that this branch
   is mostly taken.  If we think that the branch is extremely likely
   to be taken, we return 2.  If the branch is slightly more likely to be
   taken, return 1.  If the branch is slightly less likely to be taken,
   return 0 and if the branch is highly unlikely to be taken, return -1.  */

static int
mostly_true_jump (rtx jump_insn)
{
  /* If branch probabilities are available, then use that number since it
     always gives a correct answer.  */
  rtx note = find_reg_note (jump_insn, REG_BR_PROB, 0);
  if (note)
    {
      int prob = profile_probability::from_reg_br_prob_note (XINT (note, 0))
			.to_reg_br_prob_base ();

      if (prob >= REG_BR_PROB_BASE * 9 / 10)
	return 2;
      else if (prob >= REG_BR_PROB_BASE / 2)
	return 1;
      else if (prob >= REG_BR_PROB_BASE / 10)
	return 0;
      else
	return -1;
    }

  /* If there is no note, assume branches are not taken.
     This should be rare.  */
    return 0;
}

/* Return the condition under which INSN will branch to TARGET.  If TARGET
   is zero, return the condition under which INSN will return.  If INSN is
   an unconditional branch, return const_true_rtx.  If INSN isn't a simple
   type of jump, or it doesn't go to TARGET, return 0.  */

static rtx
get_branch_condition (const rtx_insn *insn, rtx target)
{
  rtx pat = PATTERN (insn);
  rtx src;

  if (condjump_in_parallel_p (insn))
    pat = XVECEXP (pat, 0, 0);

  if (ANY_RETURN_P (pat) && pat == target)
    return const_true_rtx;

  if (GET_CODE (pat) != SET || SET_DEST (pat) != pc_rtx)
    return 0;

  src = SET_SRC (pat);
  if (GET_CODE (src) == LABEL_REF && label_ref_label (src) == target)
    return const_true_rtx;

  else if (GET_CODE (src) == IF_THEN_ELSE
	   && XEXP (src, 2) == pc_rtx
	   && ((GET_CODE (XEXP (src, 1)) == LABEL_REF
		&& label_ref_label (XEXP (src, 1)) == target)
	       || (ANY_RETURN_P (XEXP (src, 1)) && XEXP (src, 1) == target)))
    return XEXP (src, 0);

  else if (GET_CODE (src) == IF_THEN_ELSE
	   && XEXP (src, 1) == pc_rtx
	   && ((GET_CODE (XEXP (src, 2)) == LABEL_REF
		&& label_ref_label (XEXP (src, 2)) == target)
	       || (ANY_RETURN_P (XEXP (src, 2)) && XEXP (src, 2) == target)))
    {
      enum rtx_code rev;
      rev = reversed_comparison_code (XEXP (src, 0), insn);
      if (rev != UNKNOWN)
	return gen_rtx_fmt_ee (rev, GET_MODE (XEXP (src, 0)),
			       XEXP (XEXP (src, 0), 0),
			       XEXP (XEXP (src, 0), 1));
    }

  return 0;
}

/* Return nonzero if CONDITION is more strict than the condition of
   INSN, i.e., if INSN will always branch if CONDITION is true.  */

static int
condition_dominates_p (rtx condition, const rtx_insn *insn)
{
  rtx other_condition = get_branch_condition (insn, JUMP_LABEL (insn));
  enum rtx_code code = GET_CODE (condition);
  enum rtx_code other_code;

  if (rtx_equal_p (condition, other_condition)
      || other_condition == const_true_rtx)
    return 1;

  else if (condition == const_true_rtx || other_condition == 0)
    return 0;

  other_code = GET_CODE (other_condition);
  if (GET_RTX_LENGTH (code) != 2 || GET_RTX_LENGTH (other_code) != 2
      || ! rtx_equal_p (XEXP (condition, 0), XEXP (other_condition, 0))
      || ! rtx_equal_p (XEXP (condition, 1), XEXP (other_condition, 1)))
    return 0;

  return comparison_dominates_p (code, other_code);
}

/* Return nonzero if redirecting JUMP to NEWLABEL does not invalidate
   any insns already in the delay slot of JUMP.  */

static int
redirect_with_delay_slots_safe_p (rtx_insn *jump, rtx newlabel, rtx seq)
{
  int flags, i;
  rtx_sequence *pat = as_a <rtx_sequence *> (PATTERN (seq));

  /* Make sure all the delay slots of this jump would still
     be valid after threading the jump.  If they are still
     valid, then return nonzero.  */

  flags = get_jump_flags (jump, newlabel);
  for (i = 1; i < pat->len (); i++)
    if (! (
#if ANNUL_IFFALSE_SLOTS
	   (INSN_ANNULLED_BRANCH_P (jump)
	    && INSN_FROM_TARGET_P (pat->insn (i)))
	   ? eligible_for_annul_false (jump, i - 1, pat->insn (i), flags) :
#endif
#if ANNUL_IFTRUE_SLOTS
	   (INSN_ANNULLED_BRANCH_P (jump)
	    && ! INSN_FROM_TARGET_P (XVECEXP (pat, 0, i)))
	   ? eligible_for_annul_true (jump, i - 1, pat->insn (i), flags) :
#endif
	   eligible_for_delay (jump, i - 1, pat->insn (i), flags)))
      break;

  return (i == pat->len ());
}

/* Return nonzero if redirecting JUMP to NEWLABEL does not invalidate
   any insns we wish to place in the delay slot of JUMP.  */

static int
redirect_with_delay_list_safe_p (rtx_insn *jump, rtx newlabel,
				 const vec<rtx_insn *> &delay_list)
{
  /* Make sure all the insns in DELAY_LIST would still be
     valid after threading the jump.  If they are still
     valid, then return nonzero.  */

  int flags = get_jump_flags (jump, newlabel);
  unsigned int delay_insns = delay_list.length ();
  unsigned int i = 0;
  for (; i < delay_insns; i++)
    if (! (
#if ANNUL_IFFALSE_SLOTS
	   (INSN_ANNULLED_BRANCH_P (jump)
	    && INSN_FROM_TARGET_P (delay_list[i]))
	   ? eligible_for_annul_false (jump, i, delay_list[i], flags) :
#endif
#if ANNUL_IFTRUE_SLOTS
	   (INSN_ANNULLED_BRANCH_P (jump)
	    && ! INSN_FROM_TARGET_P (delay_list[i]))
	   ? eligible_for_annul_true (jump, i, delay_list[i], flags) :
#endif
	   eligible_for_delay (jump, i, delay_list[i], flags)))
      break;

  return i == delay_insns;
}

/* DELAY_LIST is a list of insns that have already been placed into delay
   slots.  See if all of them have the same annulling status as ANNUL_TRUE_P.
   If not, return 0; otherwise return 1.  */

static int
check_annul_list_true_false (int annul_true_p,
			     const vec<rtx_insn *> &delay_list)
{
  rtx_insn *trial;
  unsigned int i;
  FOR_EACH_VEC_ELT (delay_list, i, trial)
    if ((annul_true_p && INSN_FROM_TARGET_P (trial))
	|| (!annul_true_p && !INSN_FROM_TARGET_P (trial)))
      return 0;

  return 1;
}

/* INSN branches to an insn whose pattern SEQ is a SEQUENCE.  Given that
   the condition tested by INSN is CONDITION and the resources shown in
   OTHER_NEEDED are needed after INSN, see whether INSN can take all the insns
   from SEQ's delay list, in addition to whatever insns it may execute
   (in DELAY_LIST).   SETS and NEEDED are denote resources already set and
   needed while searching for delay slot insns.  Return the concatenated
   delay list if possible, otherwise, return 0.

   SLOTS_TO_FILL is the total number of slots required by INSN, and
   PSLOTS_FILLED points to the number filled so far (also the number of
   insns in DELAY_LIST).  It is updated with the number that have been
   filled from the SEQUENCE, if any.

   PANNUL_P points to a nonzero value if we already know that we need
   to annul INSN.  If this routine determines that annulling is needed,
   it may set that value nonzero.

   PNEW_THREAD points to a location that is to receive the place at which
   execution should continue.  */

static void
steal_delay_list_from_target (rtx_insn *insn, rtx condition, rtx_sequence *seq,
			      vec<rtx_insn *> *delay_list,
			      struct resources *sets,
			      struct resources *needed,
			      struct resources *other_needed,
			      int slots_to_fill, int *pslots_filled,
			      int *pannul_p, rtx *pnew_thread)
{
  int slots_remaining = slots_to_fill - *pslots_filled;
  int total_slots_filled = *pslots_filled;
  auto_vec<rtx_insn *, 5> new_delay_list;
  int must_annul = *pannul_p;
  int used_annul = 0;
  int i;
  struct resources cc_set;
  rtx_insn **redundant;

  /* We can't do anything if there are more delay slots in SEQ than we
     can handle, or if we don't know that it will be a taken branch.
     We know that it will be a taken branch if it is either an unconditional
     branch or a conditional branch with a stricter branch condition.

     Also, exit if the branch has more than one set, since then it is computing
     other results that can't be ignored, e.g. the HPPA mov&branch instruction.
     ??? It may be possible to move other sets into INSN in addition to
     moving the instructions in the delay slots.

     We can not steal the delay list if one of the instructions in the
     current delay_list modifies the condition codes and the jump in the
     sequence is a conditional jump. We can not do this because we can
     not change the direction of the jump because the condition codes
     will effect the direction of the jump in the sequence.  */

  CLEAR_RESOURCE (&cc_set);

  rtx_insn *trial;
  FOR_EACH_VEC_ELT (*delay_list, i, trial)
    {
      mark_set_resources (trial, &cc_set, 0, MARK_SRC_DEST_CALL);
      if (insn_references_resource_p (seq->insn (0), &cc_set, false))
	return;
    }

  if (XVECLEN (seq, 0) - 1 > slots_remaining
      || ! condition_dominates_p (condition, seq->insn (0))
      || ! single_set (seq->insn (0)))
    return;

  /* On some targets, branches with delay slots can have a limited
     displacement.  Give the back end a chance to tell us we can't do
     this.  */
  if (! targetm.can_follow_jump (insn, seq->insn (0)))
    return;

  redundant = XALLOCAVEC (rtx_insn *, XVECLEN (seq, 0));
  for (i = 1; i < seq->len (); i++)
    {
      rtx_insn *trial = seq->insn (i);
      int flags;

      if (insn_references_resource_p (trial, sets, false)
	  || insn_sets_resource_p (trial, needed, false)
	  || insn_sets_resource_p (trial, sets, false)
	  /* If TRIAL sets CC0, we can't copy it, so we can't steal this
	     delay list.  */
	  || (HAVE_cc0 && find_reg_note (trial, REG_CC_USER, NULL_RTX))
	  /* If TRIAL is from the fallthrough code of an annulled branch insn
	     in SEQ, we cannot use it.  */
	  || (INSN_ANNULLED_BRANCH_P (seq->insn (0))
	      && ! INSN_FROM_TARGET_P (trial)))
	return;

      /* If this insn was already done (usually in a previous delay slot),
	 pretend we put it in our delay slot.  */
      redundant[i] = redundant_insn (trial, insn, new_delay_list);
      if (redundant[i])
	continue;

      /* We will end up re-vectoring this branch, so compute flags
	 based on jumping to the new label.  */
      flags = get_jump_flags (insn, JUMP_LABEL (seq->insn (0)));

      if (! must_annul
	  && ((condition == const_true_rtx
	       || (! insn_sets_resource_p (trial, other_needed, false)
		   && ! may_trap_or_fault_p (PATTERN (trial)))))
	  ? eligible_for_delay (insn, total_slots_filled, trial, flags)
	  : (must_annul || (delay_list->is_empty () && new_delay_list.is_empty ()))
	     && (must_annul = 1,
		 check_annul_list_true_false (0, *delay_list)
	         && check_annul_list_true_false (0, new_delay_list)
	         && eligible_for_annul_false (insn, total_slots_filled,
					      trial, flags)))
	{
	  if (must_annul)
	    {
	      /* Frame related instructions cannot go into annulled delay
		 slots, it messes up the dwarf info.  */
	      if (RTX_FRAME_RELATED_P (trial))
		return;
	      used_annul = 1;
	    }
	  rtx_insn *temp = copy_delay_slot_insn (trial);
	  INSN_FROM_TARGET_P (temp) = 1;
	  add_to_delay_list (temp, &new_delay_list);
	  total_slots_filled++;

	  if (--slots_remaining == 0)
	    break;
	}
      else
	return;
    }

  /* Record the effect of the instructions that were redundant and which
     we therefore decided not to copy.  */
  for (i = 1; i < seq->len (); i++)
    if (redundant[i])
      {
	fix_reg_dead_note (redundant[i], insn);
	update_block (seq->insn (i), insn);
      }

  /* Show the place to which we will be branching.  */
  *pnew_thread = first_active_target_insn (JUMP_LABEL (seq->insn (0)));

  /* Add any new insns to the delay list and update the count of the
     number of slots filled.  */
  *pslots_filled = total_slots_filled;
  if (used_annul)
    *pannul_p = 1;

  rtx_insn *temp;
  FOR_EACH_VEC_ELT (new_delay_list, i, temp)
    add_to_delay_list (temp, delay_list);
}

/* Similar to steal_delay_list_from_target except that SEQ is on the
   fallthrough path of INSN.  Here we only do something if the delay insn
   of SEQ is an unconditional branch.  In that case we steal its delay slot
   for INSN since unconditional branches are much easier to fill.  */

static void
steal_delay_list_from_fallthrough (rtx_insn *insn, rtx condition,
				   rtx_sequence *seq,
				   vec<rtx_insn *> *delay_list,
				   struct resources *sets,
				   struct resources *needed,
				   struct resources *other_needed,
				   int slots_to_fill, int *pslots_filled,
				   int *pannul_p)
{
  int i;
  int flags;
  int must_annul = *pannul_p;
  int used_annul = 0;

  flags = get_jump_flags (insn, JUMP_LABEL (insn));

  /* We can't do anything if SEQ's delay insn isn't an
     unconditional branch.  */

  if (! simplejump_or_return_p (seq->insn (0)))
    return;

  for (i = 1; i < seq->len (); i++)
    {
      rtx_insn *trial = seq->insn (i);
      rtx_insn *prior_insn;

      /* If TRIAL sets CC0, stealing it will move it too far from the use
	 of CC0.  */
      if (insn_references_resource_p (trial, sets, false)
	  || insn_sets_resource_p (trial, needed, false)
	  || insn_sets_resource_p (trial, sets, false)
	  || (HAVE_cc0 && sets_cc0_p (PATTERN (trial))))

	break;

      /* If this insn was already done, we don't need it.  */
      if ((prior_insn = redundant_insn (trial, insn, *delay_list)))
	{
	  fix_reg_dead_note (prior_insn, insn);
	  update_block (trial, insn);
	  delete_from_delay_slot (trial);
	  continue;
	}

      if (! must_annul
	  && ((condition == const_true_rtx
	       || (! insn_sets_resource_p (trial, other_needed, false)
		   && ! may_trap_or_fault_p (PATTERN (trial)))))
	  ? eligible_for_delay (insn, *pslots_filled, trial, flags)
	  : (must_annul || delay_list->is_empty ()) && (must_annul = 1,
	     check_annul_list_true_false (1, *delay_list)
	     && eligible_for_annul_true (insn, *pslots_filled, trial, flags)))
	{
	  if (must_annul)
	    used_annul = 1;
	  delete_from_delay_slot (trial);
	  add_to_delay_list (trial, delay_list);

	  if (++(*pslots_filled) == slots_to_fill)
	    break;
	}
      else
	break;
    }

  if (used_annul)
    *pannul_p = 1;
}

/* Try merging insns starting at THREAD which match exactly the insns in
   INSN's delay list.

   If all insns were matched and the insn was previously annulling, the
   annul bit will be cleared.

   For each insn that is merged, if the branch is or will be non-annulling,
   we delete the merged insn.  */

static void
try_merge_delay_insns (rtx_insn *insn, rtx_insn *thread)
{
  rtx_insn *trial, *next_trial;
  rtx_insn *delay_insn = as_a <rtx_insn *> (XVECEXP (PATTERN (insn), 0, 0));
  int annul_p = JUMP_P (delay_insn) && INSN_ANNULLED_BRANCH_P (delay_insn);
  int slot_number = 1;
  int num_slots = XVECLEN (PATTERN (insn), 0);
  rtx next_to_match = XVECEXP (PATTERN (insn), 0, slot_number);
  struct resources set, needed, modified;
  auto_vec<std::pair<rtx_insn *, bool>, 10> merged_insns;
  int flags;

  flags = get_jump_flags (delay_insn, JUMP_LABEL (delay_insn));

  CLEAR_RESOURCE (&needed);
  CLEAR_RESOURCE (&set);

  /* If this is not an annulling branch, take into account anything needed in
     INSN's delay slot.  This prevents two increments from being incorrectly
     folded into one.  If we are annulling, this would be the correct
     thing to do.  (The alternative, looking at things set in NEXT_TO_MATCH
     will essentially disable this optimization.  This method is somewhat of
     a kludge, but I don't see a better way.)  */
  if (! annul_p)
    for (int i = 1; i < num_slots; i++)
      if (XVECEXP (PATTERN (insn), 0, i))
	mark_referenced_resources (XVECEXP (PATTERN (insn), 0, i), &needed,
				   true);

  for (trial = thread; !stop_search_p (trial, 1); trial = next_trial)
    {
      rtx pat = PATTERN (trial);
      rtx oldtrial = trial;

      next_trial = next_nonnote_insn (trial);

      /* TRIAL must be a CALL_INSN or INSN.  Skip USE and CLOBBER.  */
      if (NONJUMP_INSN_P (trial)
	  && (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER
	      || GET_CODE (pat) == CLOBBER_HIGH))
	continue;

      if (GET_CODE (next_to_match) == GET_CODE (trial)
	  /* We can't share an insn that sets cc0.  */
	  && (!HAVE_cc0 || ! sets_cc0_p (pat))
	  && ! insn_references_resource_p (trial, &set, true)
	  && ! insn_sets_resource_p (trial, &set, true)
	  && ! insn_sets_resource_p (trial, &needed, true)
	  && (trial = try_split (pat, trial, 0)) != 0
	  /* Update next_trial, in case try_split succeeded.  */
	  && (next_trial = next_nonnote_insn (trial))
	  /* Likewise THREAD.  */
	  && (thread = oldtrial == thread ? trial : thread)
	  && rtx_equal_p (PATTERN (next_to_match), PATTERN (trial))
	  /* Have to test this condition if annul condition is different
	     from (and less restrictive than) non-annulling one.  */
	  && eligible_for_delay (delay_insn, slot_number - 1, trial, flags))
	{

	  if (! annul_p)
	    {
	      update_block (trial, thread);
	      if (trial == thread)
		thread = next_active_insn (thread);

	      delete_related_insns (trial);
	      INSN_FROM_TARGET_P (next_to_match) = 0;
	    }
	  else
	    merged_insns.safe_push (std::pair<rtx_insn *, bool> (trial, false));

	  if (++slot_number == num_slots)
	    break;

	  next_to_match = XVECEXP (PATTERN (insn), 0, slot_number);
	}

      mark_set_resources (trial, &set, 0, MARK_SRC_DEST_CALL);
      mark_referenced_resources (trial, &needed, true);
    }

  /* See if we stopped on a filled insn.  If we did, try to see if its
     delay slots match.  */
  if (slot_number != num_slots
      && trial && NONJUMP_INSN_P (trial)
      && GET_CODE (PATTERN (trial)) == SEQUENCE
      && !(JUMP_P (XVECEXP (PATTERN (trial), 0, 0))
           && INSN_ANNULLED_BRANCH_P (XVECEXP (PATTERN (trial), 0, 0))))
    {
      rtx_sequence *pat = as_a <rtx_sequence *> (PATTERN (trial));
      rtx filled_insn = XVECEXP (pat, 0, 0);

      /* Account for resources set/needed by the filled insn.  */
      mark_set_resources (filled_insn, &set, 0, MARK_SRC_DEST_CALL);
      mark_referenced_resources (filled_insn, &needed, true);

      for (int i = 1; i < pat->len (); i++)
	{
	  rtx_insn *dtrial = pat->insn (i);

	  CLEAR_RESOURCE (&modified);
	  /* Account for resources set by the insn following NEXT_TO_MATCH
	     inside INSN's delay list. */
	  for (int j = 1; slot_number + j < num_slots; j++)
	    mark_set_resources (XVECEXP (PATTERN (insn), 0, slot_number + j),
				&modified, 0, MARK_SRC_DEST_CALL);
	  /* Account for resources set by the insn before DTRIAL and inside
	     TRIAL's delay list. */
	  for (int j = 1; j < i; j++)
	    mark_set_resources (XVECEXP (pat, 0, j),
				&modified, 0, MARK_SRC_DEST_CALL); 
	  if (! insn_references_resource_p (dtrial, &set, true)
	      && ! insn_sets_resource_p (dtrial, &set, true)
	      && ! insn_sets_resource_p (dtrial, &needed, true)
	      && (!HAVE_cc0 || ! sets_cc0_p (PATTERN (dtrial)))
	      && rtx_equal_p (PATTERN (next_to_match), PATTERN (dtrial))
	      /* Check that DTRIAL and NEXT_TO_MATCH does not reference a 
	         resource modified between them (only dtrial is checked because
	         next_to_match and dtrial shall to be equal in order to hit
	         this line) */
	      && ! insn_references_resource_p (dtrial, &modified, true)
	      && eligible_for_delay (delay_insn, slot_number - 1, dtrial, flags))
	    {
	      if (! annul_p)
		{
		  rtx_insn *new_rtx;

		  update_block (dtrial, thread);
		  new_rtx = delete_from_delay_slot (dtrial);
	          if (thread->deleted ())
		    thread = new_rtx;
		  INSN_FROM_TARGET_P (next_to_match) = 0;
		}
	      else
		merged_insns.safe_push (std::pair<rtx_insn *, bool> (dtrial,
								     true));

	      if (++slot_number == num_slots)
		break;

	      next_to_match = XVECEXP (PATTERN (insn), 0, slot_number);
	    }
	  else
	    {
	      /* Keep track of the set/referenced resources for the delay
		 slots of any trial insns we encounter.  */
	      mark_set_resources (dtrial, &set, 0, MARK_SRC_DEST_CALL);
	      mark_referenced_resources (dtrial, &needed, true);
	    }
	}
    }

  /* If all insns in the delay slot have been matched and we were previously
     annulling the branch, we need not any more.  In that case delete all the
     merged insns.  Also clear the INSN_FROM_TARGET_P bit of each insn in
     the delay list so that we know that it isn't only being used at the
     target.  */
  if (slot_number == num_slots && annul_p)
    {
      unsigned int len = merged_insns.length ();
      for (unsigned int i = len - 1; i < len; i--)
	if (merged_insns[i].second)
	  {
	    update_block (merged_insns[i].first, thread);
	    rtx_insn *new_rtx = delete_from_delay_slot (merged_insns[i].first);
	    if (thread->deleted ())
	      thread = new_rtx;
	  }
	else
	  {
	    update_block (merged_insns[i].first, thread);
	    delete_related_insns (merged_insns[i].first);
	  }

      INSN_ANNULLED_BRANCH_P (delay_insn) = 0;

      for (int i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	INSN_FROM_TARGET_P (XVECEXP (PATTERN (insn), 0, i)) = 0;
    }
}

/* See if INSN is redundant with an insn in front of TARGET.  Often this
   is called when INSN is a candidate for a delay slot of TARGET.
   DELAY_LIST are insns that will be placed in delay slots of TARGET in front
   of INSN.  Often INSN will be redundant with an insn in a delay slot of
   some previous insn.  This happens when we have a series of branches to the
   same label; in that case the first insn at the target might want to go
   into each of the delay slots.

   If we are not careful, this routine can take up a significant fraction
   of the total compilation time (4%), but only wins rarely.  Hence we
   speed this routine up by making two passes.  The first pass goes back
   until it hits a label and sees if it finds an insn with an identical
   pattern.  Only in this (relatively rare) event does it check for
   data conflicts.

   We do not split insns we encounter.  This could cause us not to find a
   redundant insn, but the cost of splitting seems greater than the possible
   gain in rare cases.  */

static rtx_insn *
redundant_insn (rtx insn, rtx_insn *target, const vec<rtx_insn *> &delay_list)
{
  rtx target_main = target;
  rtx ipat = PATTERN (insn);
  rtx_insn *trial;
  rtx pat;
  struct resources needed, set;
  int i;
  unsigned insns_to_search;

  /* If INSN has any REG_UNUSED notes, it can't match anything since we
     are allowed to not actually assign to such a register.  */
  if (find_reg_note (insn, REG_UNUSED, NULL_RTX) != 0)
    return 0;

  /* Scan backwards looking for a match.  */
  for (trial = PREV_INSN (target),
	 insns_to_search = MAX_DELAY_SLOT_INSN_SEARCH;
       trial && insns_to_search > 0;
       trial = PREV_INSN (trial))
    {
      /* (use (insn))s can come immediately after a barrier if the
	 label that used to precede them has been deleted as dead.
	 See delete_related_insns.  */
      if (LABEL_P (trial) || BARRIER_P (trial))
	return 0;

      if (!INSN_P (trial))
	continue;
      --insns_to_search;

      pat = PATTERN (trial);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER
	  || GET_CODE (pat) == CLOBBER_HIGH)
	continue;

      if (GET_CODE (trial) == DEBUG_INSN)
	continue;

      if (rtx_sequence *seq = dyn_cast <rtx_sequence *> (pat))
	{
	  /* Stop for a CALL and its delay slots because it is difficult to
	     track its resource needs correctly.  */
	  if (CALL_P (seq->element (0)))
	    return 0;

	  /* Stop for an INSN or JUMP_INSN with delayed effects and its delay
	     slots because it is difficult to track its resource needs
	     correctly.  */

	  if (INSN_SETS_ARE_DELAYED (seq->insn (0)))
	    return 0;

	  if (INSN_REFERENCES_ARE_DELAYED (seq->insn (0)))
	    return 0;

	  /* See if any of the insns in the delay slot match, updating
	     resource requirements as we go.  */
	  for (i = seq->len () - 1; i > 0; i--)
	    if (GET_CODE (seq->element (i)) == GET_CODE (insn)
		&& rtx_equal_p (PATTERN (seq->element (i)), ipat)
		&& ! find_reg_note (seq->element (i), REG_UNUSED, NULL_RTX))
	      break;

	  /* If found a match, exit this loop early.  */
	  if (i > 0)
	    break;
	}

      else if (GET_CODE (trial) == GET_CODE (insn) && rtx_equal_p (pat, ipat)
	       && ! find_reg_note (trial, REG_UNUSED, NULL_RTX))
	break;
    }

  /* If we didn't find an insn that matches, return 0.  */
  if (trial == 0)
    return 0;

  /* See what resources this insn sets and needs.  If they overlap, or
     if this insn references CC0, it can't be redundant.  */

  CLEAR_RESOURCE (&needed);
  CLEAR_RESOURCE (&set);
  mark_set_resources (insn, &set, 0, MARK_SRC_DEST_CALL);
  mark_referenced_resources (insn, &needed, true);

  /* If TARGET is a SEQUENCE, get the main insn.  */
  if (NONJUMP_INSN_P (target) && GET_CODE (PATTERN (target)) == SEQUENCE)
    target_main = XVECEXP (PATTERN (target), 0, 0);

  if (resource_conflicts_p (&needed, &set)
      || (HAVE_cc0 && reg_mentioned_p (cc0_rtx, ipat))
      /* The insn requiring the delay may not set anything needed or set by
	 INSN.  */
      || insn_sets_resource_p (target_main, &needed, true)
      || insn_sets_resource_p (target_main, &set, true))
    return 0;

  /* Insns we pass may not set either NEEDED or SET, so merge them for
     simpler tests.  */
  needed.memory |= set.memory;
  IOR_HARD_REG_SET (needed.regs, set.regs);

  /* This insn isn't redundant if it conflicts with an insn that either is
     or will be in a delay slot of TARGET.  */

  unsigned int j;
  rtx_insn *temp;
  FOR_EACH_VEC_ELT (delay_list, j, temp)
    if (insn_sets_resource_p (temp, &needed, true))
      return 0;

  if (NONJUMP_INSN_P (target) && GET_CODE (PATTERN (target)) == SEQUENCE)
    for (i = 1; i < XVECLEN (PATTERN (target), 0); i++)
      if (insn_sets_resource_p (XVECEXP (PATTERN (target), 0, i), &needed,
				true))
	return 0;

  /* Scan backwards until we reach a label or an insn that uses something
     INSN sets or sets something insn uses or sets.  */

  for (trial = PREV_INSN (target),
	 insns_to_search = MAX_DELAY_SLOT_INSN_SEARCH;
       trial && !LABEL_P (trial) && insns_to_search > 0;
       trial = PREV_INSN (trial))
    {
      if (!INSN_P (trial))
	continue;
      --insns_to_search;

      pat = PATTERN (trial);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER
	  || GET_CODE (pat) == CLOBBER_HIGH)
	continue;

      if (GET_CODE (trial) == DEBUG_INSN)
	continue;

      if (rtx_sequence *seq = dyn_cast <rtx_sequence *> (pat))
	{
	  bool annul_p = false;
          rtx_insn *control = seq->insn (0);

	  /* If this is a CALL_INSN and its delay slots, it is hard to track
	     the resource needs properly, so give up.  */
	  if (CALL_P (control))
	    return 0;

	  /* If this is an INSN or JUMP_INSN with delayed effects, it
	     is hard to track the resource needs properly, so give up.  */

	  if (INSN_SETS_ARE_DELAYED (control))
	    return 0;

	  if (INSN_REFERENCES_ARE_DELAYED (control))
	    return 0;

	  if (JUMP_P (control))
	    annul_p = INSN_ANNULLED_BRANCH_P (control);

	  /* See if any of the insns in the delay slot match, updating
	     resource requirements as we go.  */
	  for (i = seq->len () - 1; i > 0; i--)
	    {
	      rtx_insn *candidate = seq->insn (i);

	      /* If an insn will be annulled if the branch is false, it isn't
		 considered as a possible duplicate insn.  */
	      if (rtx_equal_p (PATTERN (candidate), ipat)
		  && ! (annul_p && INSN_FROM_TARGET_P (candidate)))
		{
		  /* Show that this insn will be used in the sequel.  */
		  INSN_FROM_TARGET_P (candidate) = 0;
		  return candidate;
		}

	      /* Unless this is an annulled insn from the target of a branch,
		 we must stop if it sets anything needed or set by INSN.  */
	      if ((!annul_p || !INSN_FROM_TARGET_P (candidate))
		  && insn_sets_resource_p (candidate, &needed, true))
		return 0;
	    }

	  /* If the insn requiring the delay slot conflicts with INSN, we
	     must stop.  */
	  if (insn_sets_resource_p (control, &needed, true))
	    return 0;
	}
      else
	{
	  /* See if TRIAL is the same as INSN.  */
	  pat = PATTERN (trial);
	  if (rtx_equal_p (pat, ipat))
	    return trial;

	  /* Can't go any further if TRIAL conflicts with INSN.  */
	  if (insn_sets_resource_p (trial, &needed, true))
	    return 0;
	}
    }

  return 0;
}

/* Return 1 if THREAD can only be executed in one way.  If LABEL is nonzero,
   it is the target of the branch insn being scanned.  If ALLOW_FALLTHROUGH
   is nonzero, we are allowed to fall into this thread; otherwise, we are
   not.

   If LABEL is used more than one or we pass a label other than LABEL before
   finding an active insn, we do not own this thread.  */

static int
own_thread_p (rtx thread, rtx label, int allow_fallthrough)
{
  rtx_insn *active_insn;
  rtx_insn *insn;

  /* We don't own the function end.  */
  if (thread == 0 || ANY_RETURN_P (thread))
    return 0;

  /* We have a non-NULL insn.  */
  rtx_insn *thread_insn = as_a <rtx_insn *> (thread);

  /* Get the first active insn, or THREAD_INSN, if it is an active insn.  */
  active_insn = next_active_insn (PREV_INSN (thread_insn));

  for (insn = thread_insn; insn != active_insn; insn = NEXT_INSN (insn))
    if (LABEL_P (insn)
	&& (insn != label || LABEL_NUSES (insn) != 1))
      return 0;

  if (allow_fallthrough)
    return 1;

  /* Ensure that we reach a BARRIER before any insn or label.  */
  for (insn = prev_nonnote_insn (thread_insn);
       insn == 0 || !BARRIER_P (insn);
       insn = prev_nonnote_insn (insn))
    if (insn == 0
	|| LABEL_P (insn)
	|| (NONJUMP_INSN_P (insn)
	    && GET_CODE (PATTERN (insn)) != USE
	    && GET_CODE (PATTERN (insn)) != CLOBBER
	    && GET_CODE (PATTERN (insn)) != CLOBBER_HIGH))
      return 0;

  return 1;
}

/* Called when INSN is being moved from a location near the target of a jump.
   We leave a marker of the form (use (INSN)) immediately in front of WHERE
   for mark_target_live_regs.  These markers will be deleted at the end.

   We used to try to update the live status of registers if WHERE is at
   the start of a basic block, but that can't work since we may remove a
   BARRIER in relax_delay_slots.  */

static void
update_block (rtx_insn *insn, rtx_insn *where)
{
  emit_insn_before (gen_rtx_USE (VOIDmode, insn), where);

  /* INSN might be making a value live in a block where it didn't use to
     be.  So recompute liveness information for this block.  */
  incr_ticks_for_insn (insn);
}

/* Similar to REDIRECT_JUMP except that we update the BB_TICKS entry for
   the basic block containing the jump.  */

static int
reorg_redirect_jump (rtx_jump_insn *jump, rtx nlabel)
{
  incr_ticks_for_insn (jump);
  return redirect_jump (jump, nlabel, 1);
}

/* Called when INSN is being moved forward into a delay slot of DELAYED_INSN.
   We check every instruction between INSN and DELAYED_INSN for REG_DEAD notes
   that reference values used in INSN.  If we find one, then we move the
   REG_DEAD note to INSN.

   This is needed to handle the case where a later insn (after INSN) has a
   REG_DEAD note for a register used by INSN, and this later insn subsequently
   gets moved before a CODE_LABEL because it is a redundant insn.  In this
   case, mark_target_live_regs may be confused into thinking the register
   is dead because it sees a REG_DEAD note immediately before a CODE_LABEL.  */

static void
update_reg_dead_notes (rtx_insn *insn, rtx_insn *delayed_insn)
{
  rtx link, next;
  rtx_insn *p;

  for (p = next_nonnote_insn (insn); p != delayed_insn;
       p = next_nonnote_insn (p))
    for (link = REG_NOTES (p); link; link = next)
      {
	next = XEXP (link, 1);

	if (REG_NOTE_KIND (link) != REG_DEAD
	    || !REG_P (XEXP (link, 0)))
	  continue;

	if (reg_referenced_p (XEXP (link, 0), PATTERN (insn)))
	  {
	    /* Move the REG_DEAD note from P to INSN.  */
	    remove_note (p, link);
	    XEXP (link, 1) = REG_NOTES (insn);
	    REG_NOTES (insn) = link;
	  }
      }
}

/* Called when an insn redundant with start_insn is deleted.  If there
   is a REG_DEAD note for the target of start_insn between start_insn
   and stop_insn, then the REG_DEAD note needs to be deleted since the
   value no longer dies there.

   If the REG_DEAD note isn't deleted, then mark_target_live_regs may be
   confused into thinking the register is dead.  */

static void
fix_reg_dead_note (rtx_insn *start_insn, rtx stop_insn)
{
  rtx link, next;
  rtx_insn *p;

  for (p = next_nonnote_insn (start_insn); p != stop_insn;
       p = next_nonnote_insn (p))
    for (link = REG_NOTES (p); link; link = next)
      {
	next = XEXP (link, 1);

	if (REG_NOTE_KIND (link) != REG_DEAD
	    || !REG_P (XEXP (link, 0)))
	  continue;

	if (reg_set_p (XEXP (link, 0), PATTERN (start_insn)))
	  {
	    remove_note (p, link);
	    return;
	  }
      }
}

/* Delete any REG_UNUSED notes that exist on INSN but not on OTHER_INSN.

   This handles the case of udivmodXi4 instructions which optimize their
   output depending on whether any REG_UNUSED notes are present.  We must
   make sure that INSN calculates as many results as OTHER_INSN does.  */

static void
update_reg_unused_notes (rtx_insn *insn, rtx other_insn)
{
  rtx link, next;

  for (link = REG_NOTES (insn); link; link = next)
    {
      next = XEXP (link, 1);

      if (REG_NOTE_KIND (link) != REG_UNUSED
	  || !REG_P (XEXP (link, 0)))
	continue;

      if (!find_regno_note (other_insn, REG_UNUSED, REGNO (XEXP (link, 0))))
	remove_note (insn, link);
    }
}

static vec <rtx> sibling_labels;

/* Return the label before INSN, or put a new label there.  If SIBLING is
   non-zero, it is another label associated with the new label (if any),
   typically the former target of the jump that will be redirected to
   the new label.  */

static rtx_insn *
get_label_before (rtx_insn *insn, rtx sibling)
{
  rtx_insn *label;

  /* Find an existing label at this point
     or make a new one if there is none.  */
  label = prev_nonnote_insn (insn);

  if (label == 0 || !LABEL_P (label))
    {
      rtx_insn *prev = PREV_INSN (insn);

      label = gen_label_rtx ();
      emit_label_after (label, prev);
      LABEL_NUSES (label) = 0;
      if (sibling)
	{
	  sibling_labels.safe_push (label);
	  sibling_labels.safe_push (sibling);
	}
    }
  return label;
}

/* Scan a function looking for insns that need a delay slot and find insns to
   put into the delay slot.

   NON_JUMPS_P is nonzero if we are to only try to fill non-jump insns (such
   as calls).  We do these first since we don't want jump insns (that are
   easier to fill) to get the only insns that could be used for non-jump insns.
   When it is zero, only try to fill JUMP_INSNs.

   When slots are filled in this manner, the insns (including the
   delay_insn) are put together in a SEQUENCE rtx.  In this fashion,
   it is possible to tell whether a delay slot has really been filled
   or not.  `final' knows how to deal with this, by communicating
   through FINAL_SEQUENCE.  */

static void
fill_simple_delay_slots (int non_jumps_p)
{
  rtx_insn *insn, *trial, *next_trial;
  rtx pat;
  int i;
  int num_unfilled_slots = unfilled_slots_next - unfilled_slots_base;
  struct resources needed, set;
  int slots_to_fill, slots_filled;
  auto_vec<rtx_insn *, 5> delay_list;

  for (i = 0; i < num_unfilled_slots; i++)
    {
      int flags;
      /* Get the next insn to fill.  If it has already had any slots assigned,
	 we can't do anything with it.  Maybe we'll improve this later.  */

      insn = unfilled_slots_base[i];
      if (insn == 0
	  || insn->deleted ()
	  || (NONJUMP_INSN_P (insn)
	      && GET_CODE (PATTERN (insn)) == SEQUENCE)
	  || (JUMP_P (insn) && non_jumps_p)
	  || (!JUMP_P (insn) && ! non_jumps_p))
	continue;

      /* It may have been that this insn used to need delay slots, but
	 now doesn't; ignore in that case.  This can happen, for example,
	 on the HP PA RISC, where the number of delay slots depends on
	 what insns are nearby.  */
      slots_to_fill = num_delay_slots (insn);

      /* Some machine description have defined instructions to have
	 delay slots only in certain circumstances which may depend on
	 nearby insns (which change due to reorg's actions).

	 For example, the PA port normally has delay slots for unconditional
	 jumps.

	 However, the PA port claims such jumps do not have a delay slot
	 if they are immediate successors of certain CALL_INSNs.  This
	 allows the port to favor filling the delay slot of the call with
	 the unconditional jump.  */
      if (slots_to_fill == 0)
	continue;

      /* This insn needs, or can use, some delay slots.  SLOTS_TO_FILL
	 says how many.  After initialization, first try optimizing

	 call _foo		call _foo
	 nop			add %o7,.-L1,%o7
	 b,a L1
	 nop

	 If this case applies, the delay slot of the call is filled with
	 the unconditional jump.  This is done first to avoid having the
	 delay slot of the call filled in the backward scan.  Also, since
	 the unconditional jump is likely to also have a delay slot, that
	 insn must exist when it is subsequently scanned.

	 This is tried on each insn with delay slots as some machines
	 have insns which perform calls, but are not represented as
	 CALL_INSNs.  */

      slots_filled = 0;
      delay_list.truncate (0);

      if (JUMP_P (insn))
	flags = get_jump_flags (insn, JUMP_LABEL (insn));
      else
	flags = get_jump_flags (insn, NULL_RTX);

      if ((trial = next_active_insn (insn))
	  && JUMP_P (trial)
	  && simplejump_p (trial)
	  && eligible_for_delay (insn, slots_filled, trial, flags)
	  && no_labels_between_p (insn, trial)
	  && ! can_throw_internal (trial))
	{
	  rtx_insn **tmp;
	  slots_filled++;
	  add_to_delay_list (trial, &delay_list);

	  /* TRIAL may have had its delay slot filled, then unfilled.  When
	     the delay slot is unfilled, TRIAL is placed back on the unfilled
	     slots obstack.  Unfortunately, it is placed on the end of the
	     obstack, not in its original location.  Therefore, we must search
	     from entry i + 1 to the end of the unfilled slots obstack to
	     try and find TRIAL.  */
	  tmp = &unfilled_slots_base[i + 1];
	  while (*tmp != trial && tmp != unfilled_slots_next)
	    tmp++;

	  /* Remove the unconditional jump from consideration for delay slot
	     filling and unthread it.  */
	  if (*tmp == trial)
	    *tmp = 0;
	  {
	    rtx_insn *next = NEXT_INSN (trial);
	    rtx_insn *prev = PREV_INSN (trial);
	    if (prev)
	      SET_NEXT_INSN (prev) = next;
	    if (next)
	      SET_PREV_INSN (next) = prev;
	  }
	}

      /* Now, scan backwards from the insn to search for a potential
	 delay-slot candidate.  Stop searching when a label or jump is hit.

	 For each candidate, if it is to go into the delay slot (moved
	 forward in execution sequence), it must not need or set any resources
	 that were set by later insns and must not set any resources that
	 are needed for those insns.

	 The delay slot insn itself sets resources unless it is a call
	 (in which case the called routine, not the insn itself, is doing
	 the setting).  */

      if (slots_filled < slots_to_fill)
	{
	  /* If the flags register is dead after the insn, then we want to be
	     able to accept a candidate that clobbers it.  For this purpose,
	     we need to filter the flags register during life analysis, so
	     that it doesn't create RAW and WAW dependencies, while still
	     creating the necessary WAR dependencies.  */
	  bool filter_flags
	    = (slots_to_fill == 1
	       && targetm.flags_regnum != INVALID_REGNUM
	       && find_regno_note (insn, REG_DEAD, targetm.flags_regnum));
	  struct resources fset;
	  CLEAR_RESOURCE (&needed);
	  CLEAR_RESOURCE (&set);
	  mark_set_resources (insn, &set, 0, MARK_SRC_DEST);
	  if (filter_flags)
	    {
	      CLEAR_RESOURCE (&fset);
	      mark_set_resources (insn, &fset, 0, MARK_SRC_DEST);
	    }
	  mark_referenced_resources (insn, &needed, false);

	  for (trial = prev_nonnote_insn (insn); ! stop_search_p (trial, 1);
	       trial = next_trial)
	    {
	      next_trial = prev_nonnote_insn (trial);

	      /* This must be an INSN or CALL_INSN.  */
	      pat = PATTERN (trial);

	      /* Stand-alone USE and CLOBBER are just for flow.  */
	      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER
		  || GET_CODE (pat) == CLOBBER_HIGH)
		continue;

	      /* And DEBUG_INSNs never go into delay slots.  */
	      if (GET_CODE (trial) == DEBUG_INSN)
		continue;

	      /* Check for resource conflict first, to avoid unnecessary
		 splitting.  */
	      if (! insn_references_resource_p (trial, &set, true)
		  && ! insn_sets_resource_p (trial,
					     filter_flags ? &fset : &set,
					     true)
		  && ! insn_sets_resource_p (trial, &needed, true)
		  /* Can't separate set of cc0 from its use.  */
		  && (!HAVE_cc0 || ! (reg_mentioned_p (cc0_rtx, pat) && ! sets_cc0_p (pat)))
		  && ! can_throw_internal (trial))
		{
		  trial = try_split (pat, trial, 1);
		  next_trial = prev_nonnote_insn (trial);
		  if (eligible_for_delay (insn, slots_filled, trial, flags))
		    {
		      /* In this case, we are searching backward, so if we
			 find insns to put on the delay list, we want
			 to put them at the head, rather than the
			 tail, of the list.  */

		      update_reg_dead_notes (trial, insn);
		      delay_list.safe_insert (0, trial);
		      update_block (trial, trial);
		      delete_related_insns (trial);
		      if (slots_to_fill == ++slots_filled)
			break;
		      continue;
		    }
		}

	      mark_set_resources (trial, &set, 0, MARK_SRC_DEST_CALL);
	      if (filter_flags)
		{
		  mark_set_resources (trial, &fset, 0, MARK_SRC_DEST_CALL);
		  /* If the flags register is set, then it doesn't create RAW
		     dependencies any longer and it also doesn't create WAW
		     dependencies since it's dead after the original insn.  */
		  if (TEST_HARD_REG_BIT (fset.regs, targetm.flags_regnum))
		    {
		      CLEAR_HARD_REG_BIT (needed.regs, targetm.flags_regnum);
		      CLEAR_HARD_REG_BIT (fset.regs, targetm.flags_regnum);
		    }
		}
	      mark_referenced_resources (trial, &needed, true);
	    }
	}

      /* If all needed slots haven't been filled, we come here.  */

      /* Try to optimize case of jumping around a single insn.  */
      if ((ANNUL_IFTRUE_SLOTS || ANNUL_IFFALSE_SLOTS)
	&& slots_filled != slots_to_fill
	  && delay_list.is_empty ()
	  && JUMP_P (insn)
	  && (condjump_p (insn) || condjump_in_parallel_p (insn))
	  && !ANY_RETURN_P (JUMP_LABEL (insn)))
	{
	  optimize_skip (as_a <rtx_jump_insn *> (insn), &delay_list);
	  if (!delay_list.is_empty ())
	    slots_filled += 1;
	}

      /* Try to get insns from beyond the insn needing the delay slot.
	 These insns can neither set or reference resources set in insns being
	 skipped, cannot set resources in the insn being skipped, and, if this
	 is a CALL_INSN (or a CALL_INSN is passed), cannot trap (because the
	 call might not return).

	 There used to be code which continued past the target label if
	 we saw all uses of the target label.  This code did not work,
	 because it failed to account for some instructions which were
	 both annulled and marked as from the target.  This can happen as a
	 result of optimize_skip.  Since this code was redundant with
	 fill_eager_delay_slots anyways, it was just deleted.  */

      if (slots_filled != slots_to_fill
	  /* If this instruction could throw an exception which is
	     caught in the same function, then it's not safe to fill
	     the delay slot with an instruction from beyond this
	     point.  For example, consider:

               int i = 2;

	       try {
                 f();
	         i = 3;
               } catch (...) {}

               return i;

	     Even though `i' is a local variable, we must be sure not
	     to put `i = 3' in the delay slot if `f' might throw an
	     exception.

	     Presumably, we should also check to see if we could get
	     back to this function via `setjmp'.  */
	  && ! can_throw_internal (insn)
	  && !JUMP_P (insn))
	{
	  int maybe_never = 0;
	  rtx pat, trial_delay;

	  CLEAR_RESOURCE (&needed);
	  CLEAR_RESOURCE (&set);
	  mark_set_resources (insn, &set, 0, MARK_SRC_DEST_CALL);
	  mark_referenced_resources (insn, &needed, true);

	  if (CALL_P (insn))
	    maybe_never = 1;

	  for (trial = next_nonnote_insn (insn); !stop_search_p (trial, 1);
	       trial = next_trial)
	    {
	      next_trial = next_nonnote_insn (trial);

	      /* This must be an INSN or CALL_INSN.  */
	      pat = PATTERN (trial);

	      /* Stand-alone USE and CLOBBER are just for flow.  */
	      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER
		  || GET_CODE (pat) == CLOBBER_HIGH)
		continue;

	      /* And DEBUG_INSNs do not go in delay slots.  */
	      if (GET_CODE (trial) == DEBUG_INSN)
		continue;

	      /* If this already has filled delay slots, get the insn needing
		 the delay slots.  */
	      if (GET_CODE (pat) == SEQUENCE)
		trial_delay = XVECEXP (pat, 0, 0);
	      else
		trial_delay = trial;

	      /* Stop our search when seeing a jump.  */
	      if (JUMP_P (trial_delay))
		break;

	      /* See if we have a resource problem before we try to split.  */
	      if (GET_CODE (pat) != SEQUENCE
		  && ! insn_references_resource_p (trial, &set, true)
		  && ! insn_sets_resource_p (trial, &set, true)
		  && ! insn_sets_resource_p (trial, &needed, true)
		  && (!HAVE_cc0 && ! (reg_mentioned_p (cc0_rtx, pat) && ! sets_cc0_p (pat)))
		  && ! (maybe_never && may_trap_or_fault_p (pat))
		  && (trial = try_split (pat, trial, 0))
		  && eligible_for_delay (insn, slots_filled, trial, flags)
		  && ! can_throw_internal (trial))
		{
		  next_trial = next_nonnote_insn (trial);
		  add_to_delay_list (trial, &delay_list);
		  if (HAVE_cc0 && reg_mentioned_p (cc0_rtx, pat))
		    link_cc0_insns (trial);

		  delete_related_insns (trial);
		  if (slots_to_fill == ++slots_filled)
		    break;
		  continue;
		}

	      mark_set_resources (trial, &set, 0, MARK_SRC_DEST_CALL);
	      mark_referenced_resources (trial, &needed, true);

	      /* Ensure we don't put insns between the setting of cc and the
		 comparison by moving a setting of cc into an earlier delay
		 slot since these insns could clobber the condition code.  */
	      set.cc = 1;

	      /* If this is a call, we might not get here.  */
	      if (CALL_P (trial_delay))
		maybe_never = 1;
	    }

	  /* If there are slots left to fill and our search was stopped by an
	     unconditional branch, try the insn at the branch target.  We can
	     redirect the branch if it works.

	     Don't do this if the insn at the branch target is a branch.  */
	  if (slots_to_fill != slots_filled
	      && trial
	      && jump_to_label_p (trial)
	      && simplejump_p (trial)
	      && (next_trial = next_active_insn (JUMP_LABEL_AS_INSN (trial))) != 0
	      && ! (NONJUMP_INSN_P (next_trial)
		    && GET_CODE (PATTERN (next_trial)) == SEQUENCE)
	      && !JUMP_P (next_trial)
	      && ! insn_references_resource_p (next_trial, &set, true)
	      && ! insn_sets_resource_p (next_trial, &set, true)
	      && ! insn_sets_resource_p (next_trial, &needed, true)
	      && (!HAVE_cc0 || ! reg_mentioned_p (cc0_rtx, PATTERN (next_trial)))
	      && ! (maybe_never && may_trap_or_fault_p (PATTERN (next_trial)))
	      && (next_trial = try_split (PATTERN (next_trial), next_trial, 0))
	      && eligible_for_delay (insn, slots_filled, next_trial, flags)
	      && ! can_throw_internal (trial))
	    {
	      /* See comment in relax_delay_slots about necessity of using
		 next_real_nondebug_insn here.  */
	      rtx_insn *new_label = next_real_nondebug_insn (next_trial);

	      if (new_label != 0)
		new_label = get_label_before (new_label, JUMP_LABEL (trial));
	      else
		new_label = find_end_label (simple_return_rtx);

	      if (new_label)
	        {
		  add_to_delay_list (copy_delay_slot_insn (next_trial),
				     &delay_list);
		  slots_filled++;
		  reorg_redirect_jump (as_a <rtx_jump_insn *> (trial),
				       new_label);
		}
	    }
	}

      /* If this is an unconditional jump, then try to get insns from the
	 target of the jump.  */
      rtx_jump_insn *jump_insn;
      if ((jump_insn = dyn_cast <rtx_jump_insn *> (insn))
	  && simplejump_p (jump_insn)
	  && slots_filled != slots_to_fill)
	fill_slots_from_thread (jump_insn, const_true_rtx,
				next_active_insn (JUMP_LABEL_AS_INSN (insn)),
				NULL, 1, 1, own_thread_p (JUMP_LABEL (insn),
						 JUMP_LABEL (insn), 0),
				slots_to_fill, &slots_filled, &delay_list);

      if (!delay_list.is_empty ())
	unfilled_slots_base[i]
	  = emit_delay_sequence (insn, delay_list, slots_filled);

      if (slots_to_fill == slots_filled)
	unfilled_slots_base[i] = 0;

      note_delay_statistics (slots_filled, 0);
    }
}

/* Follow any unconditional jump at LABEL, for the purpose of redirecting JUMP;
   return the ultimate label reached by any such chain of jumps.
   Return a suitable return rtx if the chain ultimately leads to a
   return instruction.
   If LABEL is not followed by a jump, return LABEL.
   If the chain loops or we can't find end, return LABEL,
   since that tells caller to avoid changing the insn.
   If the returned label is obtained by following a crossing jump,
   set *CROSSING to true, otherwise set it to false.  */

static rtx
follow_jumps (rtx label, rtx_insn *jump, bool *crossing)
{
  rtx_insn *insn;
  rtx_insn *next;
  int depth;

  *crossing = false;
  if (ANY_RETURN_P (label))
    return label;

  rtx_insn *value = as_a <rtx_insn *> (label);

  for (depth = 0;
       (depth < 10
	&& (insn = next_active_insn (value)) != 0
	&& JUMP_P (insn)
	&& JUMP_LABEL (insn) != NULL_RTX
	&& ((any_uncondjump_p (insn) && onlyjump_p (insn))
	    || ANY_RETURN_P (PATTERN (insn)))
	&& (next = NEXT_INSN (insn))
	&& BARRIER_P (next));
       depth++)
    {
      rtx this_label_or_return = JUMP_LABEL (insn);

      /* If we have found a cycle, make the insn jump to itself.  */
      if (this_label_or_return == label)
	return label;

      /* Cannot follow returns and cannot look through tablejumps.  */
      if (ANY_RETURN_P (this_label_or_return))
	return this_label_or_return;

      rtx_insn *this_label = as_a <rtx_insn *> (this_label_or_return);
      if (NEXT_INSN (this_label)
	  && JUMP_TABLE_DATA_P (NEXT_INSN (this_label)))
	break;

      if (!targetm.can_follow_jump (jump, insn))
	break;
      if (!*crossing)
	*crossing = CROSSING_JUMP_P (jump);
      value = this_label;
    }
  if (depth == 10)
    return label;
  return value;
}

/* Try to find insns to place in delay slots.

   INSN is the jump needing SLOTS_TO_FILL delay slots.  It tests CONDITION
   or is an unconditional branch if CONDITION is const_true_rtx.
   *PSLOTS_FILLED is updated with the number of slots that we have filled.

   THREAD is a flow-of-control, either the insns to be executed if the
   branch is true or if the branch is false, THREAD_IF_TRUE says which.

   OPPOSITE_THREAD is the thread in the opposite direction.  It is used
   to see if any potential delay slot insns set things needed there.

   LIKELY is nonzero if it is extremely likely that the branch will be
   taken and THREAD_IF_TRUE is set.  This is used for the branch at the
   end of a loop back up to the top.

   OWN_THREAD is true if we are the only user of the thread, i.e. it is
   the target of the jump when we are the only jump going there.

   If OWN_THREAD is false, it must be the "true" thread of a jump.  In that
   case, we can only take insns from the head of the thread for our delay
   slot.  We then adjust the jump to point after the insns we have taken.  */

static void
fill_slots_from_thread (rtx_jump_insn *insn, rtx condition,
			rtx thread_or_return, rtx opposite_thread, int likely,
			int thread_if_true, int own_thread, int slots_to_fill,
			int *pslots_filled, vec<rtx_insn *> *delay_list)
{
  rtx new_thread;
  struct resources opposite_needed, set, needed;
  rtx_insn *trial;
  int lose = 0;
  int must_annul = 0;
  int flags;

  /* Validate our arguments.  */
  gcc_assert (condition != const_true_rtx || thread_if_true);
  gcc_assert (own_thread || thread_if_true);

  flags = get_jump_flags (insn, JUMP_LABEL (insn));

  /* If our thread is the end of subroutine, we can't get any delay
     insns from that.  */
  if (thread_or_return == NULL_RTX || ANY_RETURN_P (thread_or_return))
    return;

  rtx_insn *thread = as_a <rtx_insn *> (thread_or_return);

  /* If this is an unconditional branch, nothing is needed at the
     opposite thread.  Otherwise, compute what is needed there.  */
  if (condition == const_true_rtx)
    CLEAR_RESOURCE (&opposite_needed);
  else
    mark_target_live_regs (get_insns (), opposite_thread, &opposite_needed);

  /* If the insn at THREAD can be split, do it here to avoid having to
     update THREAD and NEW_THREAD if it is done in the loop below.  Also
     initialize NEW_THREAD.  */

  new_thread = thread = try_split (PATTERN (thread), thread, 0);

  /* Scan insns at THREAD.  We are looking for an insn that can be removed
     from THREAD (it neither sets nor references resources that were set
     ahead of it and it doesn't set anything needs by the insns ahead of
     it) and that either can be placed in an annulling insn or aren't
     needed at OPPOSITE_THREAD.  */

  CLEAR_RESOURCE (&needed);
  CLEAR_RESOURCE (&set);

  /* If we do not own this thread, we must stop as soon as we find
     something that we can't put in a delay slot, since all we can do
     is branch into THREAD at a later point.  Therefore, labels stop
     the search if this is not the `true' thread.  */

  for (trial = thread;
       ! stop_search_p (trial, ! thread_if_true) && (! lose || own_thread);
       trial = next_nonnote_insn (trial))
    {
      rtx pat, old_trial;

      /* If we have passed a label, we no longer own this thread.  */
      if (LABEL_P (trial))
	{
	  own_thread = 0;
	  continue;
	}

      pat = PATTERN (trial);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER
	  || GET_CODE (pat) == CLOBBER_HIGH)
	continue;

      if (GET_CODE (trial) == DEBUG_INSN)
	continue;

      /* If TRIAL conflicts with the insns ahead of it, we lose.  Also,
	 don't separate or copy insns that set and use CC0.  */
      if (! insn_references_resource_p (trial, &set, true)
	  && ! insn_sets_resource_p (trial, &set, true)
	  && ! insn_sets_resource_p (trial, &needed, true)
	  && (!HAVE_cc0 || (! (reg_mentioned_p (cc0_rtx, pat)
			      && (! own_thread || ! sets_cc0_p (pat)))))
	  && ! can_throw_internal (trial))
	{
	  rtx_insn *prior_insn;

	  /* If TRIAL is redundant with some insn before INSN, we don't
	     actually need to add it to the delay list; we can merely pretend
	     we did.  */
	  if ((prior_insn = redundant_insn (trial, insn, *delay_list)))
	    {
	      fix_reg_dead_note (prior_insn, insn);
	      if (own_thread)
		{
		  update_block (trial, thread);
		  if (trial == thread)
		    {
		      thread = next_active_insn (thread);
		      if (new_thread == trial)
			new_thread = thread;
		    }

		  delete_related_insns (trial);
		}
	      else
		{
		  update_reg_unused_notes (prior_insn, trial);
		  new_thread = next_active_insn (trial);
		}

	      continue;
	    }

	  /* There are two ways we can win:  If TRIAL doesn't set anything
	     needed at the opposite thread and can't trap, or if it can
	     go into an annulled delay slot.  But we want neither to copy
	     nor to speculate frame-related insns.  */
	  if (!must_annul
	      && ((condition == const_true_rtx
		   && (own_thread || !RTX_FRAME_RELATED_P (trial)))
	          || (! insn_sets_resource_p (trial, &opposite_needed, true)
		      && ! may_trap_or_fault_p (pat)
		      && ! RTX_FRAME_RELATED_P (trial))))
	    {
	      old_trial = trial;
	      trial = try_split (pat, trial, 0);
	      if (new_thread == old_trial)
		new_thread = trial;
	      if (thread == old_trial)
		thread = trial;
	      pat = PATTERN (trial);
	      if (eligible_for_delay (insn, *pslots_filled, trial, flags))
		goto winner;
	    }
	  else if (!RTX_FRAME_RELATED_P (trial)
		   && ((ANNUL_IFTRUE_SLOTS && ! thread_if_true)
		        || (ANNUL_IFFALSE_SLOTS && thread_if_true)))
	    {
	      old_trial = trial;
	      trial = try_split (pat, trial, 0);
	      if (new_thread == old_trial)
		new_thread = trial;
	      if (thread == old_trial)
		thread = trial;
	      pat = PATTERN (trial);
	      if ((must_annul || delay_list->is_empty ()) && (thread_if_true
		   ? check_annul_list_true_false (0, *delay_list)
		     && eligible_for_annul_false (insn, *pslots_filled, trial, flags)
		   : check_annul_list_true_false (1, *delay_list)
		     && eligible_for_annul_true (insn, *pslots_filled, trial, flags)))
		{
		  rtx_insn *temp;

		  must_annul = 1;
		winner:

		  if (HAVE_cc0 && reg_mentioned_p (cc0_rtx, pat))
		    link_cc0_insns (trial);

		  /* If we own this thread, delete the insn.  If this is the
		     destination of a branch, show that a basic block status
		     may have been updated.  In any case, mark the new
		     starting point of this thread.  */
		  if (own_thread)
		    {
		      rtx note;

		      update_block (trial, thread);
		      if (trial == thread)
			{
			  thread = next_active_insn (thread);
			  if (new_thread == trial)
			    new_thread = thread;
			}

		      /* We are moving this insn, not deleting it.  We must
			 temporarily increment the use count on any referenced
			 label lest it be deleted by delete_related_insns.  */
		      for (note = REG_NOTES (trial);
			   note != NULL_RTX;
			   note = XEXP (note, 1))
			if (REG_NOTE_KIND (note) == REG_LABEL_OPERAND
			    || REG_NOTE_KIND (note) == REG_LABEL_TARGET)
			  {
			    /* REG_LABEL_OPERAND could be
			       NOTE_INSN_DELETED_LABEL too.  */
			    if (LABEL_P (XEXP (note, 0)))
			      LABEL_NUSES (XEXP (note, 0))++;
			    else
			      gcc_assert (REG_NOTE_KIND (note)
					  == REG_LABEL_OPERAND);
			  }
		      if (jump_to_label_p (trial))
			LABEL_NUSES (JUMP_LABEL (trial))++;

		      delete_related_insns (trial);

		      for (note = REG_NOTES (trial);
			   note != NULL_RTX;
			   note = XEXP (note, 1))
			if (REG_NOTE_KIND (note) == REG_LABEL_OPERAND
			    || REG_NOTE_KIND (note) == REG_LABEL_TARGET)
			  {
			    /* REG_LABEL_OPERAND could be
			       NOTE_INSN_DELETED_LABEL too.  */
			    if (LABEL_P (XEXP (note, 0)))
			      LABEL_NUSES (XEXP (note, 0))--;
			    else
			      gcc_assert (REG_NOTE_KIND (note)
					  == REG_LABEL_OPERAND);
			  }
		      if (jump_to_label_p (trial))
			LABEL_NUSES (JUMP_LABEL (trial))--;
		    }
		  else
		    new_thread = next_active_insn (trial);

		  temp = own_thread ? trial : copy_delay_slot_insn (trial);
		  if (thread_if_true)
		    INSN_FROM_TARGET_P (temp) = 1;

		  add_to_delay_list (temp, delay_list);

		  if (slots_to_fill == ++(*pslots_filled))
		    {
		      /* Even though we have filled all the slots, we
			 may be branching to a location that has a
			 redundant insn.  Skip any if so.  */
		      while (new_thread && ! own_thread
			     && ! insn_sets_resource_p (new_thread, &set, true)
			     && ! insn_sets_resource_p (new_thread, &needed,
							true)
			     && ! insn_references_resource_p (new_thread,
							      &set, true)
			     && (prior_insn
				 = redundant_insn (new_thread, insn,
						   *delay_list)))
			{
			  /* We know we do not own the thread, so no need
			     to call update_block and delete_insn.  */
			  fix_reg_dead_note (prior_insn, insn);
			  update_reg_unused_notes (prior_insn, new_thread);
			  new_thread
			    = next_active_insn (as_a<rtx_insn *> (new_thread));
			}
		      break;
		    }

		  continue;
		}
	    }
	}

      /* This insn can't go into a delay slot.  */
      lose = 1;
      mark_set_resources (trial, &set, 0, MARK_SRC_DEST_CALL);
      mark_referenced_resources (trial, &needed, true);

      /* Ensure we don't put insns between the setting of cc and the comparison
	 by moving a setting of cc into an earlier delay slot since these insns
	 could clobber the condition code.  */
      set.cc = 1;

      /* If this insn is a register-register copy and the next insn has
	 a use of our destination, change it to use our source.  That way,
	 it will become a candidate for our delay slot the next time
	 through this loop.  This case occurs commonly in loops that
	 scan a list.

	 We could check for more complex cases than those tested below,
	 but it doesn't seem worth it.  It might also be a good idea to try
	 to swap the two insns.  That might do better.

	 We can't do this if the next insn modifies our destination, because
	 that would make the replacement into the insn invalid.  We also can't
	 do this if it modifies our source, because it might be an earlyclobber
	 operand.  This latter test also prevents updating the contents of
	 a PRE_INC.  We also can't do this if there's overlap of source and
	 destination.  Overlap may happen for larger-than-register-size modes.  */

      if (NONJUMP_INSN_P (trial) && GET_CODE (pat) == SET
	  && REG_P (SET_SRC (pat))
	  && REG_P (SET_DEST (pat))
	  && !reg_overlap_mentioned_p (SET_DEST (pat), SET_SRC (pat)))
	{
	  rtx_insn *next = next_nonnote_insn (trial);

	  if (next && NONJUMP_INSN_P (next)
	      && GET_CODE (PATTERN (next)) != USE
	      && ! reg_set_p (SET_DEST (pat), next)
	      && ! reg_set_p (SET_SRC (pat), next)
	      && reg_referenced_p (SET_DEST (pat), PATTERN (next))
	      && ! modified_in_p (SET_DEST (pat), next))
	    validate_replace_rtx (SET_DEST (pat), SET_SRC (pat), next);
	}
    }

  /* If we stopped on a branch insn that has delay slots, see if we can
     steal some of the insns in those slots.  */
  if (trial && NONJUMP_INSN_P (trial)
      && GET_CODE (PATTERN (trial)) == SEQUENCE
      && JUMP_P (XVECEXP (PATTERN (trial), 0, 0)))
    {
      rtx_sequence *sequence = as_a <rtx_sequence *> (PATTERN (trial));
      /* If this is the `true' thread, we will want to follow the jump,
	 so we can only do this if we have taken everything up to here.  */
      if (thread_if_true && trial == new_thread)
	{
	  steal_delay_list_from_target (insn, condition, sequence,
					delay_list, &set, &needed,
					&opposite_needed, slots_to_fill,
					pslots_filled, &must_annul,
					&new_thread);
	  /* If we owned the thread and are told that it branched
	     elsewhere, make sure we own the thread at the new location.  */
	  if (own_thread && trial != new_thread)
	    own_thread = own_thread_p (new_thread, new_thread, 0);
	}
      else if (! thread_if_true)
	steal_delay_list_from_fallthrough (insn, condition, sequence,
					   delay_list, &set, &needed,
					   &opposite_needed, slots_to_fill,
					   pslots_filled, &must_annul);
    }

  /* If we haven't found anything for this delay slot and it is very
     likely that the branch will be taken, see if the insn at our target
     increments or decrements a register with an increment that does not
     depend on the destination register.  If so, try to place the opposite
     arithmetic insn after the jump insn and put the arithmetic insn in the
     delay slot.  If we can't do this, return.  */
  if (delay_list->is_empty () && likely
      && new_thread && !ANY_RETURN_P (new_thread)
      && NONJUMP_INSN_P (new_thread)
      && !RTX_FRAME_RELATED_P (new_thread)
      && GET_CODE (PATTERN (new_thread)) != ASM_INPUT
      && asm_noperands (PATTERN (new_thread)) < 0)
    {
      rtx pat = PATTERN (new_thread);
      rtx dest;
      rtx src;

      /* We know "new_thread" is an insn due to NONJUMP_INSN_P (new_thread)
	 above.  */
      trial = as_a <rtx_insn *> (new_thread);
      pat = PATTERN (trial);

      if (!NONJUMP_INSN_P (trial)
	  || GET_CODE (pat) != SET
	  || ! eligible_for_delay (insn, 0, trial, flags)
	  || can_throw_internal (trial))
	return;

      dest = SET_DEST (pat), src = SET_SRC (pat);
      if ((GET_CODE (src) == PLUS || GET_CODE (src) == MINUS)
	  && rtx_equal_p (XEXP (src, 0), dest)
	  && (!FLOAT_MODE_P (GET_MODE (src))
	      || flag_unsafe_math_optimizations)
	  && ! reg_overlap_mentioned_p (dest, XEXP (src, 1))
	  && ! side_effects_p (pat))
	{
	  rtx other = XEXP (src, 1);
	  rtx new_arith;
	  rtx_insn *ninsn;

	  /* If this is a constant adjustment, use the same code with
	     the negated constant.  Otherwise, reverse the sense of the
	     arithmetic.  */
	  if (CONST_INT_P (other))
	    new_arith = gen_rtx_fmt_ee (GET_CODE (src), GET_MODE (src), dest,
					negate_rtx (GET_MODE (src), other));
	  else
	    new_arith = gen_rtx_fmt_ee (GET_CODE (src) == PLUS ? MINUS : PLUS,
					GET_MODE (src), dest, other);

	  ninsn = emit_insn_after (gen_rtx_SET (dest, new_arith), insn);

	  if (recog_memoized (ninsn) < 0
	      || (extract_insn (ninsn),
		  !constrain_operands (1, get_preferred_alternatives (ninsn))))
	    {
	      delete_related_insns (ninsn);
	      return;
	    }

	  if (own_thread)
	    {
	      update_block (trial, thread);
	      if (trial == thread)
		{
		  thread = next_active_insn (thread);
		  if (new_thread == trial)
		    new_thread = thread;
		}
	      delete_related_insns (trial);
	    }
	  else
	    new_thread = next_active_insn (trial);

	  ninsn = own_thread ? trial : copy_delay_slot_insn (trial);
	  if (thread_if_true)
	    INSN_FROM_TARGET_P (ninsn) = 1;

	  add_to_delay_list (ninsn, delay_list);
	  (*pslots_filled)++;
	}
    }

  if (!delay_list->is_empty () && must_annul)
    INSN_ANNULLED_BRANCH_P (insn) = 1;

  /* If we are to branch into the middle of this thread, find an appropriate
     label or make a new one if none, and redirect INSN to it.  If we hit the
     end of the function, use the end-of-function label.  */
  if (new_thread != thread)
    {
      rtx label;
      bool crossing = false;

      gcc_assert (thread_if_true);

      if (new_thread && simplejump_or_return_p (new_thread)
	  && redirect_with_delay_list_safe_p (insn,
					      JUMP_LABEL (new_thread),
					      *delay_list))
	new_thread = follow_jumps (JUMP_LABEL (new_thread), insn,
				   &crossing);

      if (ANY_RETURN_P (new_thread))
	label = find_end_label (new_thread);
      else if (LABEL_P (new_thread))
	label = new_thread;
      else
	label = get_label_before (as_a <rtx_insn *> (new_thread),
				  JUMP_LABEL (insn));

      if (label)
	{
	  reorg_redirect_jump (insn, label);
	  if (crossing)
	    CROSSING_JUMP_P (insn) = 1;
	}
    }
}

/* Make another attempt to find insns to place in delay slots.

   We previously looked for insns located in front of the delay insn
   and, for non-jump delay insns, located behind the delay insn.

   Here only try to schedule jump insns and try to move insns from either
   the target or the following insns into the delay slot.  If annulling is
   supported, we will be likely to do this.  Otherwise, we can do this only
   if safe.  */

static void
fill_eager_delay_slots (void)
{
  rtx_insn *insn;
  int i;
  int num_unfilled_slots = unfilled_slots_next - unfilled_slots_base;

  for (i = 0; i < num_unfilled_slots; i++)
    {
      rtx condition;
      rtx target_label, insn_at_target;
      rtx_insn *fallthrough_insn;
      auto_vec<rtx_insn *, 5> delay_list;
      rtx_jump_insn *jump_insn;
      int own_target;
      int own_fallthrough;
      int prediction, slots_to_fill, slots_filled;

      insn = unfilled_slots_base[i];
      if (insn == 0
	  || insn->deleted ()
	  || ! (jump_insn = dyn_cast <rtx_jump_insn *> (insn))
	  || ! (condjump_p (jump_insn) || condjump_in_parallel_p (jump_insn)))
	continue;

      slots_to_fill = num_delay_slots (jump_insn);
      /* Some machine description have defined instructions to have
	 delay slots only in certain circumstances which may depend on
	 nearby insns (which change due to reorg's actions).

	 For example, the PA port normally has delay slots for unconditional
	 jumps.

	 However, the PA port claims such jumps do not have a delay slot
	 if they are immediate successors of certain CALL_INSNs.  This
	 allows the port to favor filling the delay slot of the call with
	 the unconditional jump.  */
      if (slots_to_fill == 0)
	continue;

      slots_filled = 0;
      target_label = JUMP_LABEL (jump_insn);
      condition = get_branch_condition (jump_insn, target_label);

      if (condition == 0)
	continue;

      /* Get the next active fallthrough and target insns and see if we own
	 them.  Then see whether the branch is likely true.  We don't need
	 to do a lot of this for unconditional branches.  */

      insn_at_target = first_active_target_insn (target_label);
      own_target = own_thread_p (target_label, target_label, 0);

      if (condition == const_true_rtx)
	{
	  own_fallthrough = 0;
	  fallthrough_insn = 0;
	  prediction = 2;
	}
      else
	{
	  fallthrough_insn = next_active_insn (jump_insn);
	  own_fallthrough = own_thread_p (NEXT_INSN (jump_insn), NULL_RTX, 1);
	  prediction = mostly_true_jump (jump_insn);
	}

      /* If this insn is expected to branch, first try to get insns from our
	 target, then our fallthrough insns.  If it is not expected to branch,
	 try the other order.  */

      if (prediction > 0)
	{
	  fill_slots_from_thread (jump_insn, condition, insn_at_target,
				  fallthrough_insn, prediction == 2, 1,
				  own_target,
				  slots_to_fill, &slots_filled, &delay_list);

	  if (delay_list.is_empty () && own_fallthrough)
	    {
	      /* Even though we didn't find anything for delay slots,
		 we might have found a redundant insn which we deleted
		 from the thread that was filled.  So we have to recompute
		 the next insn at the target.  */
	      target_label = JUMP_LABEL (jump_insn);
	      insn_at_target = first_active_target_insn (target_label);

	      fill_slots_from_thread (jump_insn, condition, fallthrough_insn,
				      insn_at_target, 0, 0, own_fallthrough,
				      slots_to_fill, &slots_filled,
				      &delay_list);
	    }
	}
      else
	{
	  if (own_fallthrough)
	    fill_slots_from_thread (jump_insn, condition, fallthrough_insn,
				    insn_at_target, 0, 0, own_fallthrough,
				    slots_to_fill, &slots_filled, &delay_list);

	  if (delay_list.is_empty ())
	    fill_slots_from_thread (jump_insn, condition, insn_at_target,
				    next_active_insn (insn), 0, 1, own_target,
				    slots_to_fill, &slots_filled, &delay_list);
	}

      if (!delay_list.is_empty ())
	unfilled_slots_base[i]
	  = emit_delay_sequence (jump_insn, delay_list, slots_filled);

      if (slots_to_fill == slots_filled)
	unfilled_slots_base[i] = 0;

      note_delay_statistics (slots_filled, 1);
    }
}

static void delete_computation (rtx_insn *insn);

/* Recursively delete prior insns that compute the value (used only by INSN
   which the caller is deleting) stored in the register mentioned by NOTE
   which is a REG_DEAD note associated with INSN.  */

static void
delete_prior_computation (rtx note, rtx_insn *insn)
{
  rtx_insn *our_prev;
  rtx reg = XEXP (note, 0);

  for (our_prev = prev_nonnote_insn (insn);
       our_prev && (NONJUMP_INSN_P (our_prev)
		    || CALL_P (our_prev));
       our_prev = prev_nonnote_insn (our_prev))
    {
      rtx pat = PATTERN (our_prev);

      /* If we reach a CALL which is not calling a const function
	 or the callee pops the arguments, then give up.  */
      if (CALL_P (our_prev)
	  && (! RTL_CONST_CALL_P (our_prev)
	      || GET_CODE (pat) != SET || GET_CODE (SET_SRC (pat)) != CALL))
	break;

      /* If we reach a SEQUENCE, it is too complex to try to
	 do anything with it, so give up.  We can be run during
	 and after reorg, so SEQUENCE rtl can legitimately show
	 up here.  */
      if (GET_CODE (pat) == SEQUENCE)
	break;

      if (GET_CODE (pat) == USE
	  && NONJUMP_INSN_P (XEXP (pat, 0)))
	/* reorg creates USEs that look like this.  We leave them
	   alone because reorg needs them for its own purposes.  */
	break;

      if (reg_set_p (reg, pat))
	{
	  if (side_effects_p (pat) && !CALL_P (our_prev))
	    break;

	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* If we find a SET of something else, we can't
		 delete the insn.  */

	      int i;

	      for (i = 0; i < XVECLEN (pat, 0); i++)
		{
		  rtx part = XVECEXP (pat, 0, i);

		  if (GET_CODE (part) == SET
		      && SET_DEST (part) != reg)
		    break;
		}

	      if (i == XVECLEN (pat, 0))
		delete_computation (our_prev);
	    }
	  else if (GET_CODE (pat) == SET
		   && REG_P (SET_DEST (pat)))
	    {
	      int dest_regno = REGNO (SET_DEST (pat));
	      int dest_endregno = END_REGNO (SET_DEST (pat));
	      int regno = REGNO (reg);
	      int endregno = END_REGNO (reg);

	      if (dest_regno >= regno
		  && dest_endregno <= endregno)
		delete_computation (our_prev);

	      /* We may have a multi-word hard register and some, but not
		 all, of the words of the register are needed in subsequent
		 insns.  Write REG_UNUSED notes for those parts that were not
		 needed.  */
	      else if (dest_regno <= regno
		       && dest_endregno >= endregno)
		{
		  int i;

		  add_reg_note (our_prev, REG_UNUSED, reg);

		  for (i = dest_regno; i < dest_endregno; i++)
		    if (! find_regno_note (our_prev, REG_UNUSED, i))
		      break;

		  if (i == dest_endregno)
		    delete_computation (our_prev);
		}
	    }

	  break;
	}

      /* If PAT references the register that dies here, it is an
	 additional use.  Hence any prior SET isn't dead.  However, this
	 insn becomes the new place for the REG_DEAD note.  */
      if (reg_overlap_mentioned_p (reg, pat))
	{
	  XEXP (note, 1) = REG_NOTES (our_prev);
	  REG_NOTES (our_prev) = note;
	  break;
	}
    }
}

/* Delete INSN and recursively delete insns that compute values used only
   by INSN.  This uses the REG_DEAD notes computed during flow analysis.

   Look at all our REG_DEAD notes.  If a previous insn does nothing other
   than set a register that dies in this insn, we can delete that insn
   as well.

   On machines with CC0, if CC0 is used in this insn, we may be able to
   delete the insn that set it.  */

static void
delete_computation (rtx_insn *insn)
{
  rtx note, next;

  if (HAVE_cc0 && reg_referenced_p (cc0_rtx, PATTERN (insn)))
    {
      rtx_insn *prev = prev_nonnote_insn (insn);
      /* We assume that at this stage
	 CC's are always set explicitly
	 and always immediately before the jump that
	 will use them.  So if the previous insn
	 exists to set the CC's, delete it
	 (unless it performs auto-increments, etc.).  */
      if (prev && NONJUMP_INSN_P (prev)
	  && sets_cc0_p (PATTERN (prev)))
	{
	  if (sets_cc0_p (PATTERN (prev)) > 0
	      && ! side_effects_p (PATTERN (prev)))
	    delete_computation (prev);
	  else
	    /* Otherwise, show that cc0 won't be used.  */
	    add_reg_note (prev, REG_UNUSED, cc0_rtx);
	}
    }

  for (note = REG_NOTES (insn); note; note = next)
    {
      next = XEXP (note, 1);

      if (REG_NOTE_KIND (note) != REG_DEAD
	  /* Verify that the REG_NOTE is legitimate.  */
	  || !REG_P (XEXP (note, 0)))
	continue;

      delete_prior_computation (note, insn);
    }

  delete_related_insns (insn);
}

/* If all INSN does is set the pc, delete it,
   and delete the insn that set the condition codes for it
   if that's what the previous thing was.  */

static void
delete_jump (rtx_insn *insn)
{
  rtx set = single_set (insn);

  if (set && GET_CODE (SET_DEST (set)) == PC)
    delete_computation (insn);
}

static rtx_insn *
label_before_next_insn (rtx_insn *x, rtx scan_limit)
{
  rtx_insn *insn = next_active_insn (x);
  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == scan_limit || insn == NULL_RTX)
	return NULL;
      if (LABEL_P (insn))
	break;
    }
  return insn;
}

/* Return TRUE if there is a NOTE_INSN_SWITCH_TEXT_SECTIONS note in between
   BEG and END.  */

static bool
switch_text_sections_between_p (const rtx_insn *beg, const rtx_insn *end)
{
  const rtx_insn *p;
  for (p = beg; p != end; p = NEXT_INSN (p))
    if (NOTE_P (p) && NOTE_KIND (p) == NOTE_INSN_SWITCH_TEXT_SECTIONS)
      return true;
  return false;
}


/* Once we have tried two ways to fill a delay slot, make a pass over the
   code to try to improve the results and to do such things as more jump
   threading.  */

static void
relax_delay_slots (rtx_insn *first)
{
  rtx_insn *insn, *next;
  rtx_sequence *pat;
  rtx_insn *delay_insn;
  rtx target_label;

  /* Look at every JUMP_INSN and see if we can improve it.  */
  for (insn = first; insn; insn = next)
    {
      rtx_insn *other, *prior_insn;
      bool crossing;

      next = next_active_insn (insn);

      /* If this is a jump insn, see if it now jumps to a jump, jumps to
	 the next insn, or jumps to a label that is not the last of a
	 group of consecutive labels.  */
      if (is_a <rtx_jump_insn *> (insn)
	  && (condjump_p (insn) || condjump_in_parallel_p (insn))
	  && !ANY_RETURN_P (target_label = JUMP_LABEL (insn)))
	{
	  rtx_jump_insn *jump_insn = as_a <rtx_jump_insn *> (insn);
	  target_label
	    = skip_consecutive_labels (follow_jumps (target_label, jump_insn,
						     &crossing));
	  if (ANY_RETURN_P (target_label))
	    target_label = find_end_label (target_label);

	  if (target_label
	      && next_active_insn (as_a<rtx_insn *> (target_label)) == next
	      && ! condjump_in_parallel_p (jump_insn)
	      && ! (next && switch_text_sections_between_p (jump_insn, next)))
	    {
	      delete_jump (jump_insn);
	      continue;
	    }

	  if (target_label && target_label != JUMP_LABEL (jump_insn))
	    {
	      reorg_redirect_jump (jump_insn, target_label);
	      if (crossing)
		CROSSING_JUMP_P (jump_insn) = 1;
	    }

	  /* See if this jump conditionally branches around an unconditional
	     jump.  If so, invert this jump and point it to the target of the
	     second jump.  Check if it's possible on the target.  */
	  if (next && simplejump_or_return_p (next)
	      && any_condjump_p (jump_insn)
	      && target_label
	      && (next_active_insn (as_a<rtx_insn *> (target_label))
		  == next_active_insn (next))
	      && no_labels_between_p (jump_insn, next)
	      && targetm.can_follow_jump (jump_insn, next))
	    {
	      rtx label = JUMP_LABEL (next);

	      /* Be careful how we do this to avoid deleting code or
		 labels that are momentarily dead.  See similar optimization
		 in jump.c.

		 We also need to ensure we properly handle the case when
		 invert_jump fails.  */

	      ++LABEL_NUSES (target_label);
	      if (!ANY_RETURN_P (label))
		++LABEL_NUSES (label);

	      if (invert_jump (jump_insn, label, 1))
		{
		  delete_related_insns (next);
		  next = jump_insn;
		}

	      if (!ANY_RETURN_P (label))
		--LABEL_NUSES (label);

	      if (--LABEL_NUSES (target_label) == 0)
		delete_related_insns (target_label);

	      continue;
	    }
	}

      /* If this is an unconditional jump and the previous insn is a
	 conditional jump, try reversing the condition of the previous
	 insn and swapping our targets.  The next pass might be able to
	 fill the slots.

	 Don't do this if we expect the conditional branch to be true, because
	 we would then be making the more common case longer.  */

      if (simplejump_or_return_p (insn)
	  && (other = prev_active_insn (insn)) != 0
	  && any_condjump_p (other)
	  && no_labels_between_p (other, insn)
	  && mostly_true_jump (other) < 0)
	{
	  rtx other_target = JUMP_LABEL (other);
	  target_label = JUMP_LABEL (insn);

	  if (invert_jump (as_a <rtx_jump_insn *> (other), target_label, 0))
	    reorg_redirect_jump (as_a <rtx_jump_insn *> (insn), other_target);
	}

      /* Now look only at cases where we have a filled delay slot.  */
      if (!NONJUMP_INSN_P (insn) || GET_CODE (PATTERN (insn)) != SEQUENCE)
	continue;

      pat = as_a <rtx_sequence *> (PATTERN (insn));
      delay_insn = pat->insn (0);

      /* See if the first insn in the delay slot is redundant with some
	 previous insn.  Remove it from the delay slot if so; then set up
	 to reprocess this insn.  */
      if ((prior_insn = redundant_insn (pat->insn (1), delay_insn, vNULL)))
	{
	  fix_reg_dead_note (prior_insn, insn);
	  update_block (pat->insn (1), insn);
	  delete_from_delay_slot (pat->insn (1));
	  next = prev_active_insn (next);
	  continue;
	}

      /* See if we have a RETURN insn with a filled delay slot followed
	 by a RETURN insn with an unfilled a delay slot.  If so, we can delete
	 the first RETURN (but not its delay insn).  This gives the same
	 effect in fewer instructions.

	 Only do so if optimizing for size since this results in slower, but
	 smaller code.  */
      if (optimize_function_for_size_p (cfun)
	  && ANY_RETURN_P (PATTERN (delay_insn))
	  && next
	  && JUMP_P (next)
	  && PATTERN (next) == PATTERN (delay_insn))
	{
	  rtx_insn *after;
	  int i;

	  /* Delete the RETURN and just execute the delay list insns.

	     We do this by deleting the INSN containing the SEQUENCE, then
	     re-emitting the insns separately, and then deleting the RETURN.
	     This allows the count of the jump target to be properly
	     decremented.

	     Note that we need to change the INSN_UID of the re-emitted insns
	     since it is used to hash the insns for mark_target_live_regs and
	     the re-emitted insns will no longer be wrapped up in a SEQUENCE.

	     Clear the from target bit, since these insns are no longer
	     in delay slots.  */
	  for (i = 0; i < XVECLEN (pat, 0); i++)
	    INSN_FROM_TARGET_P (XVECEXP (pat, 0, i)) = 0;

	  rtx_insn *prev = PREV_INSN (insn);
	  delete_related_insns (insn);
	  gcc_assert (GET_CODE (pat) == SEQUENCE);
	  add_insn_after (delay_insn, prev, NULL);
	  after = delay_insn;
	  for (i = 1; i < pat->len (); i++)
	    after = emit_copy_of_insn_after (pat->insn (i), after);
	  delete_scheduled_jump (delay_insn);
	  continue;
	}

      /* Now look only at the cases where we have a filled JUMP_INSN.  */
      rtx_jump_insn *delay_jump_insn =
		dyn_cast <rtx_jump_insn *> (delay_insn);
      if (! delay_jump_insn || !(condjump_p (delay_jump_insn)
	  || condjump_in_parallel_p (delay_jump_insn)))
	continue;

      target_label = JUMP_LABEL (delay_jump_insn);
      if (target_label && ANY_RETURN_P (target_label))
	continue;

      /* If this jump goes to another unconditional jump, thread it, but
	 don't convert a jump into a RETURN here.  */
      rtx trial = skip_consecutive_labels (follow_jumps (target_label,
							 delay_jump_insn,
							 &crossing));
      if (ANY_RETURN_P (trial))
	trial = find_end_label (trial);

      if (trial && trial != target_label
	  && redirect_with_delay_slots_safe_p (delay_jump_insn, trial, insn))
	{
	  reorg_redirect_jump (delay_jump_insn, trial);
	  target_label = trial;
	  if (crossing)
	    CROSSING_JUMP_P (delay_jump_insn) = 1;
	}

      /* If the first insn at TARGET_LABEL is redundant with a previous
	 insn, redirect the jump to the following insn and process again.
	 We use next_real_nondebug_insn instead of next_active_insn so we
	 don't skip USE-markers, or we'll end up with incorrect
	 liveness info.  */
      trial = next_real_nondebug_insn (target_label);
      if (trial && GET_CODE (PATTERN (trial)) != SEQUENCE
	  && redundant_insn (trial, insn, vNULL)
	  && ! can_throw_internal (trial))
	{
	  /* Figure out where to emit the special USE insn so we don't
	     later incorrectly compute register live/death info.  */
	  rtx_insn *tmp = next_active_insn (as_a<rtx_insn *> (trial));
	  if (tmp == 0)
	    tmp = find_end_label (simple_return_rtx);

	  if (tmp)
	    {
	      /* Insert the special USE insn and update dataflow info.
		 We know "trial" is an insn here as it is the output of
		 next_real_nondebug_insn () above.  */
	      update_block (as_a <rtx_insn *> (trial), tmp);
	      
	      /* Now emit a label before the special USE insn, and
		 redirect our jump to the new label.  */
	      target_label = get_label_before (PREV_INSN (tmp), target_label);
	      reorg_redirect_jump (delay_jump_insn, target_label);
	      next = insn;
	      continue;
	    }
	}

      /* Similarly, if it is an unconditional jump with one insn in its
	 delay list and that insn is redundant, thread the jump.  */
      rtx_sequence *trial_seq =
	trial ? dyn_cast <rtx_sequence *> (PATTERN (trial)) : NULL;
      if (trial_seq
	  && trial_seq->len () == 2
	  && JUMP_P (trial_seq->insn (0))
	  && simplejump_or_return_p (trial_seq->insn (0))
	  && redundant_insn (trial_seq->insn (1), insn, vNULL))
	{
	  rtx temp_label = JUMP_LABEL (trial_seq->insn (0));
	  if (ANY_RETURN_P (temp_label))
	    temp_label = find_end_label (temp_label);
	  
	  if (temp_label
	      && redirect_with_delay_slots_safe_p (delay_jump_insn,
						   temp_label, insn))
	    {
	      update_block (trial_seq->insn (1), insn);
	      reorg_redirect_jump (delay_jump_insn, temp_label);
	      next = insn;
	      continue;
	    }
	}

      /* See if we have a simple (conditional) jump that is useless.  */
      if (!CROSSING_JUMP_P (delay_jump_insn)
	  && !INSN_ANNULLED_BRANCH_P (delay_jump_insn)
	  && !condjump_in_parallel_p (delay_jump_insn)
	  && prev_active_insn (as_a<rtx_insn *> (target_label)) == insn
	  && !BARRIER_P (prev_nonnote_insn (as_a<rtx_insn *> (target_label)))
	  /* If the last insn in the delay slot sets CC0 for some insn,
	     various code assumes that it is in a delay slot.  We could
	     put it back where it belonged and delete the register notes,
	     but it doesn't seem worthwhile in this uncommon case.  */
	  && (!HAVE_cc0
	      || ! find_reg_note (XVECEXP (pat, 0, XVECLEN (pat, 0) - 1),
				  REG_CC_USER, NULL_RTX)))
	{
	  rtx_insn *after;
	  int i;

	  /* All this insn does is execute its delay list and jump to the
	     following insn.  So delete the jump and just execute the delay
	     list insns.

	     We do this by deleting the INSN containing the SEQUENCE, then
	     re-emitting the insns separately, and then deleting the jump.
	     This allows the count of the jump target to be properly
	     decremented.

	     Note that we need to change the INSN_UID of the re-emitted insns
	     since it is used to hash the insns for mark_target_live_regs and
	     the re-emitted insns will no longer be wrapped up in a SEQUENCE.

	     Clear the from target bit, since these insns are no longer
	     in delay slots.  */
	  for (i = 0; i < XVECLEN (pat, 0); i++)
	    INSN_FROM_TARGET_P (XVECEXP (pat, 0, i)) = 0;

	  rtx_insn *prev = PREV_INSN (insn);
	  delete_related_insns (insn);
	  gcc_assert (GET_CODE (pat) == SEQUENCE);
	  add_insn_after (delay_jump_insn, prev, NULL);
	  after = delay_jump_insn;
	  for (i = 1; i < pat->len (); i++)
	    after = emit_copy_of_insn_after (pat->insn (i), after);
	  delete_scheduled_jump (delay_jump_insn);
	  continue;
	}

      /* See if this is an unconditional jump around a single insn which is
	 identical to the one in its delay slot.  In this case, we can just
	 delete the branch and the insn in its delay slot.  */
      if (next && NONJUMP_INSN_P (next)
	  && label_before_next_insn (next, insn) == target_label
	  && simplejump_p (insn)
	  && XVECLEN (pat, 0) == 2
	  && rtx_equal_p (PATTERN (next), PATTERN (pat->insn (1))))
	{
	  delete_related_insns (insn);
	  continue;
	}

      /* See if this jump (with its delay slots) conditionally branches
	 around an unconditional jump (without delay slots).  If so, invert
	 this jump and point it to the target of the second jump.  We cannot
	 do this for annulled jumps, though.  Again, don't convert a jump to
	 a RETURN here.  */
      if (! INSN_ANNULLED_BRANCH_P (delay_jump_insn)
	  && any_condjump_p (delay_jump_insn)
	  && next && simplejump_or_return_p (next)
	  && (next_active_insn (as_a<rtx_insn *> (target_label))
	      == next_active_insn (next))
	  && no_labels_between_p (insn, next))
	{
	  rtx label = JUMP_LABEL (next);
	  rtx old_label = JUMP_LABEL (delay_jump_insn);

	  if (ANY_RETURN_P (label))
	    label = find_end_label (label);

	  /* find_end_label can generate a new label. Check this first.  */
	  if (label
	      && no_labels_between_p (insn, next)
	      && redirect_with_delay_slots_safe_p (delay_jump_insn,
						   label, insn))
	    {
	      /* Be careful how we do this to avoid deleting code or labels
		 that are momentarily dead.  See similar optimization in
		 jump.c  */
	      if (old_label)
		++LABEL_NUSES (old_label);

	      if (invert_jump (delay_jump_insn, label, 1))
		{
		  int i;

		  /* Must update the INSN_FROM_TARGET_P bits now that
		     the branch is reversed, so that mark_target_live_regs
		     will handle the delay slot insn correctly.  */
		  for (i = 1; i < XVECLEN (PATTERN (insn), 0); i++)
		    {
		      rtx slot = XVECEXP (PATTERN (insn), 0, i);
		      INSN_FROM_TARGET_P (slot) = ! INSN_FROM_TARGET_P (slot);
		    }

		  delete_related_insns (next);
		  next = insn;
		}

	      if (old_label && --LABEL_NUSES (old_label) == 0)
		delete_related_insns (old_label);
	      continue;
	    }
	}

      /* If we own the thread opposite the way this insn branches, see if we
	 can merge its delay slots with following insns.  */
      if (INSN_FROM_TARGET_P (pat->insn (1))
	  && own_thread_p (NEXT_INSN (insn), 0, 1))
	try_merge_delay_insns (insn, next);
      else if (! INSN_FROM_TARGET_P (pat->insn (1))
	       && own_thread_p (target_label, target_label, 0))
	try_merge_delay_insns (insn,
			       next_active_insn (as_a<rtx_insn *> (target_label)));

      /* If we get here, we haven't deleted INSN.  But we may have deleted
	 NEXT, so recompute it.  */
      next = next_active_insn (insn);
    }
}


/* Look for filled jumps to the end of function label.  We can try to convert
   them into RETURN insns if the insns in the delay slot are valid for the
   RETURN as well.  */

static void
make_return_insns (rtx_insn *first)
{
  rtx_insn *insn;
  rtx_jump_insn *jump_insn;
  rtx real_return_label = function_return_label;
  rtx real_simple_return_label = function_simple_return_label;
  int slots, i;

  /* See if there is a RETURN insn in the function other than the one we
     made for END_OF_FUNCTION_LABEL.  If so, set up anything we can't change
     into a RETURN to jump to it.  */
  for (insn = first; insn; insn = NEXT_INSN (insn))
    if (JUMP_P (insn) && ANY_RETURN_P (PATTERN (insn)))
      {
	rtx t = get_label_before (insn, NULL_RTX);
	if (PATTERN (insn) == ret_rtx)
	  real_return_label = t;
	else
	  real_simple_return_label = t;
	break;
      }

  /* Show an extra usage of REAL_RETURN_LABEL so it won't go away if it
     was equal to END_OF_FUNCTION_LABEL.  */
  if (real_return_label)
    LABEL_NUSES (real_return_label)++;
  if (real_simple_return_label)
    LABEL_NUSES (real_simple_return_label)++;

  /* Clear the list of insns to fill so we can use it.  */
  obstack_free (&unfilled_slots_obstack, unfilled_firstobj);

  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      int flags;
      rtx kind, real_label;

      /* Only look at filled JUMP_INSNs that go to the end of function
	 label.  */
      if (!NONJUMP_INSN_P (insn))
	continue;

      if (GET_CODE (PATTERN (insn)) != SEQUENCE)
	continue;

      rtx_sequence *pat = as_a <rtx_sequence *> (PATTERN (insn));

      if (!jump_to_label_p (pat->insn (0)))
	continue;

      if (JUMP_LABEL (pat->insn (0)) == function_return_label)
	{
	  kind = ret_rtx;
	  real_label = real_return_label;
	}
      else if (JUMP_LABEL (pat->insn (0)) == function_simple_return_label)
	{
	  kind = simple_return_rtx;
	  real_label = real_simple_return_label;
	}
      else
	continue;

      jump_insn = as_a <rtx_jump_insn *> (pat->insn (0));

      /* If we can't make the jump into a RETURN, try to redirect it to the best
	 RETURN and go on to the next insn.  */
      if (!reorg_redirect_jump (jump_insn, kind))
	{
	  /* Make sure redirecting the jump will not invalidate the delay
	     slot insns.  */
	  if (redirect_with_delay_slots_safe_p (jump_insn, real_label, insn))
	    reorg_redirect_jump (jump_insn, real_label);
	  continue;
	}

      /* See if this RETURN can accept the insns current in its delay slot.
	 It can if it has more or an equal number of slots and the contents
	 of each is valid.  */

      flags = get_jump_flags (jump_insn, JUMP_LABEL (jump_insn));
      slots = num_delay_slots (jump_insn);
      if (slots >= XVECLEN (pat, 0) - 1)
	{
	  for (i = 1; i < XVECLEN (pat, 0); i++)
	    if (! (
#if ANNUL_IFFALSE_SLOTS
		   (INSN_ANNULLED_BRANCH_P (jump_insn)
		    && INSN_FROM_TARGET_P (pat->insn (i)))
		   ? eligible_for_annul_false (jump_insn, i - 1,
					       pat->insn (i), flags) :
#endif
#if ANNUL_IFTRUE_SLOTS
		   (INSN_ANNULLED_BRANCH_P (jump_insn)
		    && ! INSN_FROM_TARGET_P (pat->insn (i)))
		   ? eligible_for_annul_true (jump_insn, i - 1,
					      pat->insn (i), flags) :
#endif
		   eligible_for_delay (jump_insn, i - 1,
				       pat->insn (i), flags)))
	      break;
	}
      else
	i = 0;

      if (i == XVECLEN (pat, 0))
	continue;

      /* We have to do something with this insn.  If it is an unconditional
	 RETURN, delete the SEQUENCE and output the individual insns,
	 followed by the RETURN.  Then set things up so we try to find
	 insns for its delay slots, if it needs some.  */
      if (ANY_RETURN_P (PATTERN (jump_insn)))
	{
	  rtx_insn *prev = PREV_INSN (insn);

	  delete_related_insns (insn);
	  for (i = 1; i < XVECLEN (pat, 0); i++)
	    {
	      rtx_insn *in_seq_insn = as_a<rtx_insn *> (XVECEXP (pat, 0, i));
	      prev = emit_insn_after_setloc (PATTERN (in_seq_insn), prev,
					     INSN_LOCATION (in_seq_insn));
	    }

	  insn = emit_jump_insn_after_setloc (PATTERN (jump_insn), prev,
					      INSN_LOCATION (jump_insn));
	  emit_barrier_after (insn);

	  if (slots)
	    obstack_ptr_grow (&unfilled_slots_obstack, insn);
	}
      else
	/* It is probably more efficient to keep this with its current
	   delay slot as a branch to a RETURN.  */
	reorg_redirect_jump (jump_insn, real_label);
    }

  /* Now delete REAL_RETURN_LABEL if we never used it.  Then try to fill any
     new delay slots we have created.  */
  if (real_return_label != NULL_RTX && --LABEL_NUSES (real_return_label) == 0)
    delete_related_insns (real_return_label);
  if (real_simple_return_label != NULL_RTX
      && --LABEL_NUSES (real_simple_return_label) == 0)
    delete_related_insns (real_simple_return_label);

  fill_simple_delay_slots (1);
  fill_simple_delay_slots (0);
}

/* Try to find insns to place in delay slots.  */

static void
dbr_schedule (rtx_insn *first)
{
  rtx_insn *insn, *next, *epilogue_insn = 0;
  int i;
  bool need_return_insns;

  /* If the current function has no insns other than the prologue and
     epilogue, then do not try to fill any delay slots.  */
  if (n_basic_blocks_for_fn (cfun) == NUM_FIXED_BLOCKS)
    return;

  /* Find the highest INSN_UID and allocate and initialize our map from
     INSN_UID's to position in code.  */
  for (max_uid = 0, insn = first; insn; insn = NEXT_INSN (insn))
    {
      if (INSN_UID (insn) > max_uid)
	max_uid = INSN_UID (insn);
      if (NOTE_P (insn)
	  && NOTE_KIND (insn) == NOTE_INSN_EPILOGUE_BEG)
	epilogue_insn = insn;
    }

  uid_to_ruid = XNEWVEC (int, max_uid + 1);
  for (i = 0, insn = first; insn; i++, insn = NEXT_INSN (insn))
    uid_to_ruid[INSN_UID (insn)] = i;

  /* Initialize the list of insns that need filling.  */
  if (unfilled_firstobj == 0)
    {
      gcc_obstack_init (&unfilled_slots_obstack);
      unfilled_firstobj = XOBNEWVAR (&unfilled_slots_obstack, rtx, 0);
    }

  for (insn = next_active_insn (first); insn; insn = next_active_insn (insn))
    {
      rtx target;

      /* Skip vector tables.  We can't get attributes for them.  */
      if (JUMP_TABLE_DATA_P (insn))
	continue;

      if (JUMP_P (insn))
        INSN_ANNULLED_BRANCH_P (insn) = 0;
      INSN_FROM_TARGET_P (insn) = 0;

      if (num_delay_slots (insn) > 0)
	obstack_ptr_grow (&unfilled_slots_obstack, insn);

      /* Ensure all jumps go to the last of a set of consecutive labels.  */
      if (JUMP_P (insn)
	  && (condjump_p (insn) || condjump_in_parallel_p (insn))
	  && !ANY_RETURN_P (JUMP_LABEL (insn))
	  && ((target = skip_consecutive_labels (JUMP_LABEL (insn)))
	      != JUMP_LABEL (insn)))
	redirect_jump (as_a <rtx_jump_insn *> (insn), target, 1);
    }

  init_resource_info (epilogue_insn);

  /* Show we haven't computed an end-of-function label yet.  */
  function_return_label = function_simple_return_label = NULL;

  /* Initialize the statistics for this function.  */
  memset (num_insns_needing_delays, 0, sizeof num_insns_needing_delays);
  memset (num_filled_delays, 0, sizeof num_filled_delays);

  /* Now do the delay slot filling.  Try everything twice in case earlier
     changes make more slots fillable.  */

  for (reorg_pass_number = 0;
       reorg_pass_number < MAX_REORG_PASSES;
       reorg_pass_number++)
    {
      fill_simple_delay_slots (1);
      fill_simple_delay_slots (0);
      if (!targetm.no_speculation_in_delay_slots_p ())
	fill_eager_delay_slots ();
      relax_delay_slots (first);
    }

  /* If we made an end of function label, indicate that it is now
     safe to delete it by undoing our prior adjustment to LABEL_NUSES.
     If it is now unused, delete it.  */
  if (function_return_label && --LABEL_NUSES (function_return_label) == 0)
    delete_related_insns (function_return_label);
  if (function_simple_return_label
      && --LABEL_NUSES (function_simple_return_label) == 0)
    delete_related_insns (function_simple_return_label);

  need_return_insns = false;
  need_return_insns |= targetm.have_return () && function_return_label != 0;
  need_return_insns |= (targetm.have_simple_return ()
			&& function_simple_return_label != 0);
  if (need_return_insns)
    make_return_insns (first);

  /* Delete any USE insns made by update_block; subsequent passes don't need
     them or know how to deal with them.  */
  for (insn = first; insn; insn = next)
    {
      next = NEXT_INSN (insn);

      if (NONJUMP_INSN_P (insn) && GET_CODE (PATTERN (insn)) == USE
	  && INSN_P (XEXP (PATTERN (insn), 0)))
	next = delete_related_insns (insn);
    }

  obstack_free (&unfilled_slots_obstack, unfilled_firstobj);

  /* It is not clear why the line below is needed, but it does seem to be.  */
  unfilled_firstobj = XOBNEWVAR (&unfilled_slots_obstack, rtx, 0);

  if (dump_file)
    {
      int i, j, need_comma;
      int total_delay_slots[MAX_DELAY_HISTOGRAM + 1];
      int total_annul_slots[MAX_DELAY_HISTOGRAM + 1];

      for (reorg_pass_number = 0;
	   reorg_pass_number < MAX_REORG_PASSES;
	   reorg_pass_number++)
	{
	  fprintf (dump_file, ";; Reorg pass #%d:\n", reorg_pass_number + 1);
	  for (i = 0; i < NUM_REORG_FUNCTIONS; i++)
	    {
	      need_comma = 0;
	      fprintf (dump_file, ";; Reorg function #%d\n", i);

	      fprintf (dump_file, ";; %d insns needing delay slots\n;; ",
		       num_insns_needing_delays[i][reorg_pass_number]);

	      for (j = 0; j < MAX_DELAY_HISTOGRAM + 1; j++)
		if (num_filled_delays[i][j][reorg_pass_number])
		  {
		    if (need_comma)
		      fprintf (dump_file, ", ");
		    need_comma = 1;
		    fprintf (dump_file, "%d got %d delays",
			     num_filled_delays[i][j][reorg_pass_number], j);
		  }
	      fprintf (dump_file, "\n");
	    }
	}
      memset (total_delay_slots, 0, sizeof total_delay_slots);
      memset (total_annul_slots, 0, sizeof total_annul_slots);
      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  if (! insn->deleted ()
	      && NONJUMP_INSN_P (insn)
	      && GET_CODE (PATTERN (insn)) != USE
	      && GET_CODE (PATTERN (insn)) != CLOBBER
	      && GET_CODE (PATTERN (insn)) != CLOBBER_HIGH)
	    {
	      if (GET_CODE (PATTERN (insn)) == SEQUENCE)
		{
                  rtx control;
		  j = XVECLEN (PATTERN (insn), 0) - 1;
		  if (j > MAX_DELAY_HISTOGRAM)
		    j = MAX_DELAY_HISTOGRAM;
                  control = XVECEXP (PATTERN (insn), 0, 0);
		  if (JUMP_P (control) && INSN_ANNULLED_BRANCH_P (control))
		    total_annul_slots[j]++;
		  else
		    total_delay_slots[j]++;
		}
	      else if (num_delay_slots (insn) > 0)
		total_delay_slots[0]++;
	    }
	}
      fprintf (dump_file, ";; Reorg totals: ");
      need_comma = 0;
      for (j = 0; j < MAX_DELAY_HISTOGRAM + 1; j++)
	{
	  if (total_delay_slots[j])
	    {
	      if (need_comma)
		fprintf (dump_file, ", ");
	      need_comma = 1;
	      fprintf (dump_file, "%d got %d delays", total_delay_slots[j], j);
	    }
	}
      fprintf (dump_file, "\n");

      if (ANNUL_IFTRUE_SLOTS || ANNUL_IFFALSE_SLOTS)
	{
	  fprintf (dump_file, ";; Reorg annuls: ");
	  need_comma = 0;
	  for (j = 0; j < MAX_DELAY_HISTOGRAM + 1; j++)
	    {
	      if (total_annul_slots[j])
		{
		  if (need_comma)
		    fprintf (dump_file, ", ");
		  need_comma = 1;
		  fprintf (dump_file, "%d got %d delays", total_annul_slots[j], j);
		}
	    }
	  fprintf (dump_file, "\n");
	}

      fprintf (dump_file, "\n");
    }

  if (!sibling_labels.is_empty ())
    {
      update_alignments (sibling_labels);
      sibling_labels.release ();
    }

  free_resource_info ();
  free (uid_to_ruid);
  crtl->dbr_scheduled_p = true;
}

/* Run delay slot optimization.  */
static unsigned int
rest_of_handle_delay_slots (void)
{
  if (DELAY_SLOTS)
    dbr_schedule (get_insns ());

  return 0;
}

namespace {

const pass_data pass_data_delay_slots =
{
  RTL_PASS, /* type */
  "dbr", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_DBR_SCHED, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_delay_slots : public rtl_opt_pass
{
public:
  pass_delay_slots (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_delay_slots, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *)
    {
      return rest_of_handle_delay_slots ();
    }

}; // class pass_delay_slots

bool
pass_delay_slots::gate (function *)
{
  /* At -O0 dataflow info isn't updated after RA.  */
  if (DELAY_SLOTS)
    return optimize > 0 && flag_delayed_branch && !crtl->dbr_scheduled_p;

  return false;
}

} // anon namespace

rtl_opt_pass *
make_pass_delay_slots (gcc::context *ctxt)
{
  return new pass_delay_slots (ctxt);
}

/* Machine dependent reorg pass.  */

namespace {

const pass_data pass_data_machine_reorg =
{
  RTL_PASS, /* type */
  "mach", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MACH_DEP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_machine_reorg : public rtl_opt_pass
{
public:
  pass_machine_reorg (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_machine_reorg, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return targetm.machine_dependent_reorg != 0;
    }

  virtual unsigned int execute (function *)
    {
      targetm.machine_dependent_reorg ();
      return 0;
    }

}; // class pass_machine_reorg

} // anon namespace

rtl_opt_pass *
make_pass_machine_reorg (gcc::context *ctxt)
{
  return new pass_machine_reorg (ctxt);
}
