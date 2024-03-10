/* This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file contains code aimed at optimizing function generated with the
   use of '-msave-restore.  The goal is to identify cases where the call
   out to the save/restore routines are sub-optimal, and remove the calls
   in this case.

   As GCC currently makes the choice between using or not using
   save/restore early on (during the gimple expand pass) once we have
   selected to use save/restore we are stuck with it.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "function.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "target.h"
#include "basic-block.h"
#include "bitmap.h"
#include "df.h"
#include "tree.h"
#include "expr.h"
#include "cfg.h"

/* This file should be included last.  */
#include "hard-reg-set.h"

/* Look in the function prologue for a call to the save stub.  Ensure that
   the instruction is as we expect (see detail below) and if the
   instruction matches return a pointer to it.  Otherwise, return NULL.

   We expect the function prologue to look like this:

   (note NOTE_INSN_BASIC_BLOCK)
   (insn (parallel [
	          (unspec_volatile [
	                  (const_int 2 [0x2])
	              ] UNSPECV_GPR_SAVE)
	          (clobber (reg:SI 5 t0))
	          (clobber (reg:SI 6 t1))])
   (note NOTE_INSN_PROLOGUE_END)

   Between the NOTE_INSN_BASIC_BLOCK and the GPR_SAVE insn we might find
   other notes of type NOTE_INSN_DELETED and/or NOTE_INSN_FUNCTION_BEG.

   The parameter BODY is updated to point to the first instruction after
   the NOTE_INSN_PROLOGUE_END or will be updated to NULL if the prologue
   end note was not found.  */

static rtx_insn *
riscv_sr_match_prologue (rtx_insn **body)
{
  rtx_insn *insn, *bb_note;
  *body = NULL;

  /* Find the prologue end note.  */
  for (insn = get_insns (); insn != NULL; insn = NEXT_INSN (insn))
    if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_PROLOGUE_END)
      {
	*body = NEXT_INSN (insn);
	break;
      }

  /* If we don't have the prologue end note and at least one instruction
     before it, then this function doesn't have the structure we expect.  */
  if (insn == NULL
      || PREV_INSN (insn) == NULL)
    return NULL;

  /* The INSN is the end of prologue note, before this we expect to find
     one real instruction which makes the prologue, and before that we
     expect to find some number of notes for deleted instructions, the
     beginning of the function, and finally a basicblock beginning.  The
     following loop checks that this assumption is true.  */
  for (bb_note = PREV_INSN (PREV_INSN (insn));
       bb_note != NULL;
       bb_note = PREV_INSN (bb_note))
    {
      if (!NOTE_P (bb_note))
	return NULL;
      if (NOTE_KIND (bb_note) == NOTE_INSN_BASIC_BLOCK)
	break;
      if (NOTE_KIND (bb_note) != NOTE_INSN_DELETED
	  && NOTE_KIND (bb_note) != NOTE_INSN_FUNCTION_BEG)
	return NULL;
    }
  if (bb_note == NULL)
    return NULL;

  /* Set INSN to point to the actual interesting prologue instruction.  */
  insn = PREV_INSN (insn);
  if (INSN_P (insn)
      && INSN_CODE (insn) == CODE_FOR_gpr_save
      /* Check this is a call to _riscv_save_0.  */
      && GET_CODE (PATTERN (insn)) == PARALLEL
      && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == UNSPEC_VOLATILE
      && (GET_CODE (XVECEXP (XVECEXP (PATTERN (insn), 0, 0), 0, 0))
	  == CONST_INT)
      && INTVAL (XVECEXP (XVECEXP (PATTERN (insn), 0, 0), 0, 0)) == 0)
    return insn;

  return NULL;
}

/* Find the first instruction in the epilogue of the current function, and
   return a pointer to that instruction if, and only if, the epilogue has
   the correct structure that would allow us to optimize out the call to
   _riscv_restore_0.  */

static rtx_insn *
riscv_sr_match_epilogue (void)
{
  /* Find the first instruction in the epilogue.  */
  rtx_insn *insn, *start;
  for (insn = get_insns (); insn != NULL; insn = NEXT_INSN (insn))
    if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_EPILOGUE_BEG)
      {
	insn = NEXT_INSN (insn);
	break;
      }
  if (insn == NULL)
    return NULL;

  /* At this point INSN is the first instruction in the epilogue.  A
     standard epilogue (of the form we expect to handle) consists of the
     following instructions:

     1. A stack_tiesi or stack_tiedi (for RV32 and RV64 respectively),

     2. An optional use instruction for the register holding the return
        value.  This will be missing in functions with no return value,

     3. A gpr_restore instruction, and

     4. A jump instruction of type gpr_restore_return.  */
  start = insn;
  if (INSN_CODE (insn) != CODE_FOR_stack_tiesi
      && INSN_CODE (insn) != CODE_FOR_stack_tiedi)
    return NULL;

  insn = NEXT_INSN (insn);
  if (INSN_P (insn) && GET_CODE (PATTERN (insn)) == USE)
    insn = NEXT_INSN (insn);

  if (!INSN_P (insn) || INSN_CODE (insn) != CODE_FOR_gpr_restore)
    return NULL;

  insn = NEXT_INSN (insn);
  if (!INSN_P (insn) || INSN_CODE (insn) != CODE_FOR_gpr_restore_return)
    return NULL;

  return start;
}

/* Helper for riscv_remove_unneeded_save_restore_calls.  If we match the
   prologue instructions but not the epilogue then we might have the case
   where the epilogue has been optimized out due to a call to a no-return
   function.  In this case we might be able to remove the prologue too -
   that's what this function does.  PROLOGUE is the matched prolgoue
   instruction, by the time this function returns the progloue instruction
   may have been removed.  */

static void
check_for_no_return_call (rtx_insn *prologue)
{
  /* Check to see if we have the following pattern:

     PROLOGUE instruction
     NOTE_INSN_PROLOGUE_END
     A no-return call instruction

     If we do, then we can remove the prologue instruction safely. Remember
     that we've already confirmed by this point that the prologue is a call
     to riscv_save_0.  */

  if (dump_file)
    fprintf (dump_file,
	     "Prologue matched, checking for no-return epilogue.\n");

  rtx_insn *tmp = NEXT_INSN (prologue);
  if (!NOTE_P (tmp) || NOTE_KIND (tmp) != NOTE_INSN_PROLOGUE_END)
    return;

  /* Skip any extra notes in here, they're most likely just debug.  */
  do
    {
      tmp = NEXT_INSN (tmp);
    }
  while (tmp != NULL && NOTE_P (tmp));

  if (tmp == NULL || !INSN_P (tmp))
    return;

  bool noreturn_p = find_reg_note (tmp, REG_NORETURN, NULL_RTX) != NULL_RTX;
  if (!CALL_P (tmp) || !noreturn_p)
    return;

  if (dump_file)
    fprintf (dump_file,
	     "Prologue call to riscv_save_0 followed by noreturn call, "
	     "removing prologue.\n");
  remove_insn (prologue);
}

/* Entry point called from riscv_reorg to remove some unneeded calls to
   the save and restore stubs.  This should only be called when
   -msave-restore is in use.

   We identify some simple cases where the function looks like this:

   call t0,__riscv_save_0
   <other-code>
   call foo
   tail __riscv_restore_0

   And transform it into something like this:

   <other-code>
   tail foo

   In the above examples, what can appear in <other-code> is pretty
   restricted; only caller saved registers can be touched, this prevents
   any additional calls (as they would write to 'ra').  */

void
riscv_remove_unneeded_save_restore_calls (void)
{
  /* We'll adjust stack size after this optimization, that require update every
     sp use site, which could be unsafe, so we decide to turn off this
     optimization if there are any arguments put on stack.  */
  if (known_ne (crtl->args.size, 0))
    return;

  /* Will point to the first instruction of the function body, after the
     prologue end note.  */
  rtx_insn *body = NULL;

  /* Should only be called with -msave-restore is in use.  */
  gcc_assert (TARGET_SAVE_RESTORE);

  /* Match the expected prologue and epilogue patterns.  If either of these
     fail to match then we abandon our attempt to optimize this function.  */
  rtx_insn *prologue_matched = riscv_sr_match_prologue (&body);
  if (prologue_matched == NULL || body == NULL)
    return;

  rtx_insn *epilogue_matched = riscv_sr_match_epilogue ();
  if (epilogue_matched == NULL)
    {
      check_for_no_return_call (prologue_matched);
      return;
    }

  if (dump_file)
    fprintf (dump_file,
	     "Could be a candidate for save/restore removal\n");

  /* We want to check which registers this function uses.  */
  df_analyze ();

  int call_count = 0;
  bool good_use = true;
  int epilogue_count = 0;

  /* Now examine all of the instructions that make up this function, we're
     looking for call instructions and also double checking register usage
     while we're at it (see comments below).  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;

      FOR_BB_INSNS (bb, insn)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Block %d, Insn %d\n", bb->index, INSN_UID (insn));

	  /* If we scan the epilogue we will fall foul of our register
	     usage check below (due to it's use of the return address), so
	     once we spot we're at the epilogue, just skip the rest of this
	     block.  Scanning the prologue instructions again (if they
	     match the expected pattern) is harmless.  */
	  if (NOTE_P (insn)
	      && NOTE_KIND (insn) == NOTE_INSN_EPILOGUE_BEG)
	    {
	      ++epilogue_count;
	      break;
	    }

	  if (!INSN_P (insn))
	    continue;

	  if (CALL_P (insn))
	    ++call_count;
	  /* Ignore any USEs in the gpr_save pattern.  They don't prevent us
	     from optimizing away the save call.  */
	  else if (insn == prologue_matched)
	    ;
	  else
	    {
	      df_ref use;

	      FOR_EACH_INSN_USE (use, insn)
		{
		  /* If the function makes use of any registers that are
		     callee saved then we should be saving them in this
		     function, which would suggest that a call to the save
		     and restore functions is required.  This would seem to
		     indicate that something has gone wrong above, as we
		     should only get here if we are saving zero registers.

		     The one exception to this rule is the return address
		     register used within a call instruction.  We can
		     optimize a single call within a function (by making it
		     a tail call), so we skip call instructions here.  */
		  if (!call_used_regs[DF_REF_REGNO (use)])
		    {
		      if (dump_file)
			fprintf (dump_file,
				 "Found unsupported use of callee saved "
				 "register in instruction %d\n",
				 INSN_UID (insn));
		      good_use = false;
		      break;
		    }
		}
	      if (!good_use)
		break;
	    }
	}
    }

  /* If we used any registers that would indicate a need for a call to a
     save/restore stub then don't optimize.  */
  if (!good_use)
    return;

  /* If this function has multiple epilogues, then for now we don't try to
     optimize it.  */
  if (epilogue_count != 1)
    return;

  /* We can only optimize functions containing a single call, any more
     would require us to add instructions to store the return address on
     the stack (and restore it before we return).  We could do this in the
     future, but for now we don't.  A single call can be transformed into
     a tail call reasonably easily.  */
  if (call_count > 1)
    {
      if (dump_file)
	fprintf (dump_file,
		 "Found too many call instructions\n");
      return;
    }

  rtx_insn *epilogue_begin_note = PREV_INSN (epilogue_matched);
  gcc_assert (NOTE_P (epilogue_begin_note)
	      && NOTE_KIND (epilogue_begin_note) == NOTE_INSN_EPILOGUE_BEG);

  df_finish_pass (false);

  /* Find the first instruction before the function epilogue.  */
  rtx_insn *insn_before_epilogue;
  for (insn_before_epilogue = PREV_INSN (epilogue_begin_note);
       NOTE_P (insn_before_epilogue);
       insn_before_epilogue = PREV_INSN (insn_before_epilogue))
    ;

  /* Leaf functions will not generate calls to the save/restore stubs, so
     there's no need for this optimization there.  We know this function
     has no more than 1 call (checked above).  To convert this single call
     into a tail call we rely on the call being the last thing before the
     epilogue.  */
  if (GET_CODE (insn_before_epilogue) != CALL_INSN)
    return;

  /* The last instruction in this block, just before the epilogue is a
     call.  We can potentially change this call into a tail call.  */
  rtx_insn *call = insn_before_epilogue;

  /* Transform call in insn to a sibcall, this will only be done if the
     last thing in the function is a call.  */
  rtx callpat = PATTERN (call);
  gcc_assert (GET_CODE (callpat) == PARALLEL);

  /* Extract from CALLPAT the information we need to build the sibcall.  */
  rtx target_call = NULL;
  rtx tmp_rtx = XVECEXP (callpat, 0, 0);
  rtx set_target = NULL;
  switch (GET_CODE (tmp_rtx))
    {
    case CALL:
      target_call = tmp_rtx;
      break;

    case SET:
      {
	set_target = XEXP (tmp_rtx, 0);
	tmp_rtx = XEXP (tmp_rtx, 1);
	if (GET_CODE (tmp_rtx) != CALL)
	  return;
	target_call = tmp_rtx;
	break;
      }

    default:
      return;
    }

  rtx target_mem = XEXP (target_call, 0);
  if (GET_CODE (target_mem) != MEM)
    return;

  rtx target = XEXP (target_mem, 0);
  if (GET_CODE (target) != SYMBOL_REF && GET_CODE (target) != REG)
    return;

  /* The sibcall instructions can only use a specific subset of
     registers, we're about to (possibly) move a call through a
     register from the function body and make it a sibcall.  If we're
     not using an appropriate register then we can't make this change.

     Maybe in some future iteration we could actually scan the
     function, find a suitable sibcall register, and switch over the
     registers.  But we don't do that yet.  */
  if (GET_CODE (target) == REG
      && !SIBCALL_REG_P (REGNO (target)))
    return;

  riscv_cc cc = get_riscv_cc (XVECEXP (callpat, 0, 1));
  rtx sibcall = NULL;
  if (set_target != NULL)
    sibcall = gen_sibcall_value_internal (set_target, target, const0_rtx,
					  gen_int_mode (cc, SImode));
  else
    sibcall
      = gen_sibcall_internal (target, const0_rtx, gen_int_mode (cc, SImode));

  rtx_insn *before_call = PREV_INSN (call);
  remove_insn (call);
  rtx_insn *insn = emit_call_insn_after_setloc (sibcall, before_call,
						INSN_LOCATION (call));
  REG_NOTES (insn) = REG_NOTES (call);
  SIBLING_CALL_P (insn) = 1;

  /* Now update the prologue and epilogue to take account of the
     changes within the function body.  */
  remove_insn (prologue_matched);
  remove_insn (NEXT_INSN (NEXT_INSN (NEXT_INSN (epilogue_matched))));
  remove_insn (NEXT_INSN (NEXT_INSN (epilogue_matched)));
  remove_insn (NEXT_INSN (epilogue_matched));
  remove_insn (epilogue_matched);

  if (dump_file)
    fprintf (dump_file,
	     "Save/restore successfully removed\n");
}
