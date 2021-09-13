/* Speculation tracking and mitigation (e.g. CVE 2017-5753) for AArch64.
   Copyright (C) 2018-2021 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "rtl.h"
#include "tree-pass.h"
#include "profile-count.h"
#include "backend.h"
#include "cfgbuild.h"
#include "print-rtl.h"
#include "cfgrtl.h"
#include "function.h"
#include "basic-block.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "insn-attr.h"
#include "df.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"

/* This pass scans the RTL just before the final branch
   re-organisation pass.  The aim is to identify all places where
   there is conditional control flow and to insert code that tracks
   any speculative execution of a conditional branch.

   To do this we reserve a call-clobbered register (so that it can be
   initialized very early in the function prologue) that can then be
   updated each time there is a conditional branch.  At each such
   branch we then generate a code sequence that uses conditional
   select operations that are not subject to speculation themselves
   (we ignore for the moment situations where that might not always be
   strictly true).  For example, a branch sequence such as:

	B.EQ	<dst>
	...
   <dst>:

   is transformed to:

	B.EQ	<dst>
	CSEL	tracker, tracker, XZr, ne
	...
   <dst>:
	CSEL	tracker, tracker, XZr, eq

   Since we start with the tracker initialized to all bits one, if at any
   time the predicted control flow diverges from the architectural program
   behavior, then the tracker will become zero (but not otherwise).

   The tracker value can be used at any time at which a value needs
   guarding against incorrect speculation.  This can be done in
   several ways, but they all amount to the same thing.  For an
   untrusted address, or an untrusted offset to a trusted address, we
   can simply mask the address with the tracker with the untrusted
   value.  If the CPU is not speculating, or speculating correctly,
   then the value will remain unchanged, otherwise it will be clamped
   to zero.  For more complex scenarios we can compare the tracker
   against zero and use the flags to form a new selection with an
   alternate safe value.

   On implementations where the data processing instructions may
   themselves produce speculative values, the architecture requires
   that a CSDB instruction will resolve such data speculation, so each
   time we use the tracker for protecting a vulnerable value we also
   emit a CSDB: we do not need to do that each time the tracker itself
   is updated.

   At function boundaries, we need to communicate the speculation
   tracking state with the caller or the callee.  This is tricky
   because there is no register available for such a purpose without
   creating a new ABI.  We deal with this by relying on the principle
   that in all real programs the stack pointer, SP will never be NULL
   at a function boundary; we can thus encode the speculation state in
   SP by clearing SP if the speculation tracker itself is NULL.  After
   the call we recover the tracking state back from SP into the
   tracker register.  The results is that a function call sequence is
   transformed to

	MOV	tmp, SP
	AND	tmp, tmp, tracker
	MOV	SP, tmp
	BL	<callee>
	CMP	SP, #0
	CSETM	tracker, ne

   The additional MOV instructions in the pre-call sequence are needed
   because SP cannot be used directly with the AND instruction.

   The code inside a function body uses the post-call sequence in the
   prologue to establish the tracker and the pre-call sequence in the
   epilogue to re-encode the state for the return.

   The code sequences have the nice property that if called from, or
   calling a function that does not track speculation then the stack pointer
   will always be non-NULL and hence the tracker will be initialized to all
   bits one as we need: we lose the ability to fully track speculation in that
   case, but we are still architecturally safe.

   Tracking speculation in this way is quite expensive, both in code
   size and execution time.  We employ a number of tricks to try to
   limit this:

   1) Simple leaf functions with no conditional branches (or use of
   the tracker) do not need to establish a new tracker: they simply
   carry the tracking state through SP for the duration of the call.
   The same is also true for leaf functions that end in a tail-call.

   2) Back-to-back function calls in a single basic block also do not
   need to re-establish the tracker between the calls.  Again, we can
   carry the tracking state in SP for this period of time unless the
   tracker value is needed at that point in time.

   We run the pass just before the final branch reorganization pass so
   that we can handle most of the conditional branch cases using the
   standard edge insertion code.  The reorg pass will hopefully clean
   things up for afterwards so that the results aren't too
   horrible.  */

/* Generate a code sequence to clobber SP if speculating incorreclty.  */
static rtx_insn *
aarch64_speculation_clobber_sp ()
{
  rtx sp = gen_rtx_REG (DImode, SP_REGNUM);
  rtx tracker = gen_rtx_REG (DImode, SPECULATION_TRACKER_REGNUM);
  rtx scratch = gen_rtx_REG (DImode, SPECULATION_SCRATCH_REGNUM);

  start_sequence ();
  emit_insn (gen_rtx_SET (scratch, sp));
  emit_insn (gen_anddi3 (scratch, scratch, tracker));
  emit_insn (gen_rtx_SET (sp, scratch));
  rtx_insn *seq = get_insns ();
  end_sequence ();
  return seq;
}

/* Generate a code sequence to establish the tracker variable from the
   contents of SP.  */
static rtx_insn *
aarch64_speculation_establish_tracker ()
{
  rtx sp = gen_rtx_REG (DImode, SP_REGNUM);
  rtx tracker = gen_rtx_REG (DImode, SPECULATION_TRACKER_REGNUM);
  start_sequence ();
  rtx cc = aarch64_gen_compare_reg (EQ, sp, const0_rtx);
  emit_insn (gen_cstoredi_neg (tracker,
			       gen_rtx_NE (CCmode, cc, const0_rtx), cc));
  rtx_insn *seq = get_insns ();
  end_sequence ();
  return seq;
}

/* Main speculation tracking pass.  */
unsigned int
aarch64_do_track_speculation ()
{
  basic_block bb;
  bool needs_tracking = false;
  bool need_second_pass = false;
  rtx_insn *insn;
  int fixups_pending = 0;

  FOR_EACH_BB_FN (bb, cfun)
    {
      insn = BB_END (bb);

      if (dump_file)
	fprintf (dump_file, "Basic block %d:\n", bb->index);

      while (insn != BB_HEAD (bb)
	     && NOTE_P (insn))
	insn = PREV_INSN (insn);

      if (control_flow_insn_p (insn))
	{
	  if (any_condjump_p (insn))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "  condjump\n");
		  dump_insn_slim (dump_file, insn);
		}

	      rtx src = SET_SRC (pc_set (insn));

	      /* Check for an inverted jump, where the fall-through edge
		 appears first.  */
	      bool inverted = GET_CODE (XEXP (src, 2)) != PC;
	      /* The other edge must be the PC (we assume that we don't
		 have conditional return instructions).  */
	      gcc_assert (GET_CODE (XEXP (src, 1 + !inverted)) == PC);

	      rtx cond = copy_rtx (XEXP (src, 0));
	      gcc_assert (COMPARISON_P (cond)
			  && REG_P (XEXP (cond, 0))
			  && REGNO (XEXP (cond, 0)) == CC_REGNUM
			  && XEXP (cond, 1) == const0_rtx);
	      rtx branch_tracker = gen_speculation_tracker (copy_rtx (cond));
	      rtx fallthru_tracker = gen_speculation_tracker_rev (cond);
	      if (inverted)
		std::swap (branch_tracker, fallthru_tracker);

	      insert_insn_on_edge (branch_tracker, BRANCH_EDGE (bb));
	      insert_insn_on_edge (fallthru_tracker, FALLTHRU_EDGE (bb));
	      needs_tracking = true;
	    }
	  else if (GET_CODE (PATTERN (insn)) == RETURN)
	    {
	      /* If we already know we'll need a second pass, don't put
		 out the return sequence now, or we might end up with
		 two copies.  Instead, we'll do all return statements
		 during the second pass.  However, if this is the
		 first return insn we've found and we already
		 know that we'll need to emit the code, we can save a
		 second pass by emitting the code now.  */
	      if (needs_tracking && ! need_second_pass)
		{
		  rtx_insn *seq = aarch64_speculation_clobber_sp ();
		  emit_insn_before (seq, insn);
		}
	      else
		{
		  fixups_pending++;
		  need_second_pass = true;
		}
	    }
	  else if (find_reg_note (insn, REG_NON_LOCAL_GOTO, NULL_RTX))
	    {
	      rtx_insn *seq = aarch64_speculation_clobber_sp ();
	      emit_insn_before (seq, insn);
	      needs_tracking = true;
	    }
	}
      else
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "  other\n");
	      dump_insn_slim (dump_file, insn);
	    }
	}
    }

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *end = BB_END (bb);
      rtx_insn *call_insn = NULL;

      if (bb->flags & BB_NON_LOCAL_GOTO_TARGET)
	{
	  rtx_insn *label = NULL;
	  /* For non-local goto targets we have to recover the
	     speculation state from SP.  Find the last code label at
	     the head of the block and place the fixup sequence after
	     that.  */
	  for (insn = BB_HEAD (bb); insn != end; insn = NEXT_INSN (insn))
	    {
	      if (LABEL_P (insn))
		label = insn;
	      /* Never put anything before the basic block note.  */
	      if (NOTE_INSN_BASIC_BLOCK_P (insn))
		label = insn;
	      if (INSN_P (insn))
		break;
	    }

	  gcc_assert (label);
	  emit_insn_after (aarch64_speculation_establish_tracker (), label);
	}

      /* Scan the insns looking for calls.  We need to pass the
	 speculation tracking state encoded in to SP.  After a call we
	 restore the speculation tracking into the tracker register.
	 To avoid unnecessary transfers we look for two or more calls
	 within a single basic block and eliminate, where possible,
	 any redundant operations.  */
      for (insn = BB_HEAD (bb); ; insn = NEXT_INSN (insn))
	{
	  if (NONDEBUG_INSN_P (insn)
	      && recog_memoized (insn) >= 0
	      && (get_attr_speculation_barrier (insn)
		  == SPECULATION_BARRIER_TRUE))
	    {
	      if (call_insn)
		{
		  /* This instruction requires the speculation
		     tracking to be in the tracker register.  If there
		     was an earlier call in this block, we need to
		     copy the speculation tracking back there.  */
		  emit_insn_after (aarch64_speculation_establish_tracker (),
				   call_insn);
		  call_insn = NULL;
		}

	      needs_tracking = true;
	    }

	  if (CALL_P (insn))
	    {
	      bool tailcall
		= (SIBLING_CALL_P (insn)
		   || find_reg_note (insn, REG_NORETURN, NULL_RTX));

	      /* Tailcalls are like returns, we can eliminate the
		 transfer between the tracker register and SP if we
		 know that this function does not itself need
		 tracking.  */
	      if (tailcall && (need_second_pass || !needs_tracking))
		{
		  /* Don't clear call_insn if it is set - needs_tracking
		     will be true in that case and so we will end
		     up putting out mitigation sequences.  */
		  fixups_pending++;
		  need_second_pass = true;
		  break;
		}

	      needs_tracking = true;

	      /* We always need a transfer before the first call in a BB.  */
	      if (!call_insn)
		emit_insn_before (aarch64_speculation_clobber_sp (), insn);

	      /* Tail-calls and no-return calls don't need any post-call
		 reestablishment of the tracker.  */
	      if (! tailcall)
		call_insn = insn;
	      else
		call_insn = NULL;
	    }

	  if (insn == end)
	    break;
	}

      if (call_insn)
	{
	  rtx_insn *seq = aarch64_speculation_establish_tracker ();

	  /* Handle debug insns at the end of the BB.  Put the extra
	     insns after them.  This ensures that we have consistent
	     behaviour for the placement of the extra insns between
	     debug and non-debug builds.  */
	  for (insn = call_insn;
	       insn != end && DEBUG_INSN_P (NEXT_INSN (insn));
	       insn = NEXT_INSN (insn))
	    ;

	  if (insn == end)
	    {
	      edge e = find_fallthru_edge (bb->succs);
	      /* We need to be very careful about some calls that
		 appear at the end of a basic block.  If the call
		 involves exceptions, then the compiler may depend on
		 this being the last instruction in the block.  The
		 easiest way to handle this is to commit the new
		 instructions on the fall-through edge and to let
		 commit_edge_insertions clean things up for us.

		 Sometimes, eg with OMP, there may not even be an
		 outgoing edge after the call.  In that case, there's
		 not much we can do, presumably the compiler has
		 decided that the call can never return in this
		 context.  */
	      if (e)
		{
		  /* We need to set the location lists explicitly in
		     this case.  */
		  if (! INSN_P (seq))
		    {
		      start_sequence ();
		      emit_insn (seq);
		      seq = get_insns ();
		      end_sequence ();
		    }

		  for (rtx_insn *list = seq; list; list = NEXT_INSN (list))
		    INSN_LOCATION (list) = INSN_LOCATION (call_insn);

		  insert_insn_on_edge (seq, e);
		}
	    }
	  else
	    emit_insn_after (seq, call_insn);
	}
    }

  if (needs_tracking)
    {
      if (need_second_pass)
	{
	  /* We found a return instruction before we found out whether
	     or not we need to emit the tracking code, but we now
	     know we do.  Run quickly over the basic blocks and
	     fix up the return insns.  */
	  FOR_EACH_BB_FN (bb, cfun)
	    {
	      insn = BB_END (bb);

	      while (insn != BB_HEAD (bb)
		     && NOTE_P (insn))
		insn = PREV_INSN (insn);

	      if ((control_flow_insn_p (insn)
		   && GET_CODE (PATTERN (insn)) == RETURN)
		  || (CALL_P (insn)
		      && (SIBLING_CALL_P (insn)
			  || find_reg_note (insn, REG_NORETURN, NULL_RTX))))
		{
		  rtx_insn *seq = aarch64_speculation_clobber_sp ();
		  emit_insn_before (seq, insn);
		  fixups_pending--;
		}
	    }
	  gcc_assert (fixups_pending == 0);
	}

      /* Set up the initial value of the tracker, using the incoming SP.  */
      insert_insn_on_edge (aarch64_speculation_establish_tracker (),
			   single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun)));
      commit_edge_insertions ();
    }

  return 0;
}

namespace {

const pass_data pass_data_aarch64_track_speculation =
{
  RTL_PASS,		/* type.  */
  "speculation",	/* name.  */
  OPTGROUP_NONE,	/* optinfo_flags.  */
  TV_MACH_DEP,		/* tv_id.  */
  0,			/* properties_required.  */
  0,			/* properties_provided.  */
  0,			/* properties_destroyed.  */
  0,			/* todo_flags_start.  */
  0			/* todo_flags_finish.  */
};

class pass_track_speculation : public rtl_opt_pass
{
 public:
  pass_track_speculation(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_aarch64_track_speculation, ctxt)
    {}

  /* opt_pass methods:  */
  virtual bool gate (function *)
    {
      return aarch64_track_speculation;
    }

  virtual unsigned int execute (function *)
    {
      return aarch64_do_track_speculation ();
    }
}; // class pass_track_speculation.
} // anon namespace.

/* Create a new pass instance.  */
rtl_opt_pass *
make_pass_track_speculation (gcc::context *ctxt)
{
  return new pass_track_speculation (ctxt);
}
