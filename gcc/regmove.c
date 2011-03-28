/* Move registers around to reduce number of move instructions needed.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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


/* This module makes some simple RTL code transformations which
   improve the subsequent register allocation.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "target.h"
#include "output.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "basic-block.h"
#include "except.h"
#include "diagnostic-core.h"
#include "reload.h"
#include "timevar.h"
#include "tree-pass.h"
#include "df.h"
#include "ira.h"

static int optimize_reg_copy_1 (rtx, rtx, rtx);
static void optimize_reg_copy_2 (rtx, rtx, rtx);
static void optimize_reg_copy_3 (rtx, rtx, rtx);
static void copy_src_to_dest (rtx, rtx, rtx);

enum match_use
{
  READ,
  WRITE,
  READWRITE
};

struct match {
  int with[MAX_RECOG_OPERANDS];
  enum match_use use[MAX_RECOG_OPERANDS];
  int commutative[MAX_RECOG_OPERANDS];
  int early_clobber[MAX_RECOG_OPERANDS];
};

static int find_matches (rtx, struct match *);
static int fixup_match_2 (rtx, rtx, rtx, rtx);

/* Return nonzero if registers with CLASS1 and CLASS2 can be merged without
   causing too much register allocation problems.  */
static int
regclass_compatible_p (reg_class_t class0, reg_class_t class1)
{
  return (class0 == class1
	  || (reg_class_subset_p (class0, class1)
	      && ! targetm.class_likely_spilled_p (class0))
	  || (reg_class_subset_p (class1, class0)
	      && ! targetm.class_likely_spilled_p (class1)));
}


#ifdef AUTO_INC_DEC

/* Find the place in the rtx X where REG is used as a memory address.
   Return the MEM rtx that so uses it.
   If PLUSCONST is nonzero, search instead for a memory address equivalent to
   (plus REG (const_int PLUSCONST)).

   If such an address does not appear, return 0.
   If REG appears more than once, or is used other than in such an address,
   return (rtx) 1.  */

static rtx
find_use_as_address (rtx x, rtx reg, HOST_WIDE_INT plusconst)
{
  enum rtx_code code = GET_CODE (x);
  const char * const fmt = GET_RTX_FORMAT (code);
  int i;
  rtx value = 0;
  rtx tem;

  if (code == MEM && XEXP (x, 0) == reg && plusconst == 0)
    return x;

  if (code == MEM && GET_CODE (XEXP (x, 0)) == PLUS
      && XEXP (XEXP (x, 0), 0) == reg
      && CONST_INT_P (XEXP (XEXP (x, 0), 1))
      && INTVAL (XEXP (XEXP (x, 0), 1)) == plusconst)
    return x;

  if (code == SIGN_EXTRACT || code == ZERO_EXTRACT)
    {
      /* If REG occurs inside a MEM used in a bit-field reference,
	 that is unacceptable.  */
      if (find_use_as_address (XEXP (x, 0), reg, 0) != 0)
	return (rtx) (size_t) 1;
    }

  if (x == reg)
    return (rtx) (size_t) 1;

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  tem = find_use_as_address (XEXP (x, i), reg, plusconst);
	  if (value == 0)
	    value = tem;
	  else if (tem != 0)
	    return (rtx) (size_t) 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      tem = find_use_as_address (XVECEXP (x, i, j), reg, plusconst);
	      if (value == 0)
		value = tem;
	      else if (tem != 0)
		return (rtx) (size_t) 1;
	    }
	}
    }

  return value;
}


/* INC_INSN is an instruction that adds INCREMENT to REG.
   Try to fold INC_INSN as a post/pre in/decrement into INSN.
   Iff INC_INSN_SET is nonzero, inc_insn has a destination different from src.
   Return nonzero for success.  */
static int
try_auto_increment (rtx insn, rtx inc_insn, rtx inc_insn_set, rtx reg,
		    HOST_WIDE_INT increment, int pre)
{
  enum rtx_code inc_code;

  rtx pset = single_set (insn);
  if (pset)
    {
      /* Can't use the size of SET_SRC, we might have something like
	 (sign_extend:SI (mem:QI ...  */
      rtx use = find_use_as_address (pset, reg, 0);
      if (use != 0 && use != (rtx) (size_t) 1)
	{
	  int size = GET_MODE_SIZE (GET_MODE (use));
	  if (0
	      || (HAVE_POST_INCREMENT
		  && pre == 0 && (inc_code = POST_INC, increment == size))
	      || (HAVE_PRE_INCREMENT
		  && pre == 1 && (inc_code = PRE_INC, increment == size))
	      || (HAVE_POST_DECREMENT
		  && pre == 0 && (inc_code = POST_DEC, increment == -size))
	      || (HAVE_PRE_DECREMENT
		  && pre == 1 && (inc_code = PRE_DEC, increment == -size))
	  )
	    {
	      if (inc_insn_set)
		validate_change
		  (inc_insn,
		   &SET_SRC (inc_insn_set),
		   XEXP (SET_SRC (inc_insn_set), 0), 1);
	      validate_change (insn, &XEXP (use, 0),
			       gen_rtx_fmt_e (inc_code,
					      GET_MODE (XEXP (use, 0)), reg),
			       1);
	      if (apply_change_group ())
		{
		  /* If there is a REG_DEAD note on this insn, we must
		     change this not to REG_UNUSED meaning that the register
		     is set, but the value is dead.  Failure to do so will
		     result in sched1 dying -- when it recomputes lifetime
		     information, the number of REG_DEAD notes will have
		     changed.  */
		  rtx note = find_reg_note (insn, REG_DEAD, reg);
		  if (note)
		    PUT_REG_NOTE_KIND (note, REG_UNUSED);

		  add_reg_note (insn, REG_INC, reg);

		  if (! inc_insn_set)
		    delete_insn (inc_insn);
		  return 1;
		}
	    }
	}
    }
  return 0;
}
#endif


static int *regno_src_regno;

/* INSN is a copy from SRC to DEST, both registers, and SRC does not die
   in INSN.

   Search forward to see if SRC dies before either it or DEST is modified,
   but don't scan past the end of a basic block.  If so, we can replace SRC
   with DEST and let SRC die in INSN.

   This will reduce the number of registers live in that range and may enable
   DEST to be tied to SRC, thus often saving one register in addition to a
   register-register copy.  */

static int
optimize_reg_copy_1 (rtx insn, rtx dest, rtx src)
{
  rtx p, q;
  rtx note;
  rtx dest_death = 0;
  int sregno = REGNO (src);
  int dregno = REGNO (dest);
  basic_block bb = BLOCK_FOR_INSN (insn);

  /* We don't want to mess with hard regs if register classes are small.  */
  if (sregno == dregno
      || (targetm.small_register_classes_for_mode_p (GET_MODE (src))
	  && (sregno < FIRST_PSEUDO_REGISTER
	      || dregno < FIRST_PSEUDO_REGISTER))
      /* We don't see all updates to SP if they are in an auto-inc memory
	 reference, so we must disallow this optimization on them.  */
      || sregno == STACK_POINTER_REGNUM || dregno == STACK_POINTER_REGNUM)
    return 0;

  for (p = NEXT_INSN (insn); p; p = NEXT_INSN (p))
    {
      if (! INSN_P (p))
	continue;
      if (BLOCK_FOR_INSN (p) != bb)
	break;

      if (reg_set_p (src, p) || reg_set_p (dest, p)
	  /* If SRC is an asm-declared register, it must not be replaced
	     in any asm.  Unfortunately, the REG_EXPR tree for the asm
	     variable may be absent in the SRC rtx, so we can't check the
	     actual register declaration easily (the asm operand will have
	     it, though).  To avoid complicating the test for a rare case,
	     we just don't perform register replacement for a hard reg
	     mentioned in an asm.  */
	  || (sregno < FIRST_PSEUDO_REGISTER
	      && asm_noperands (PATTERN (p)) >= 0
	      && reg_overlap_mentioned_p (src, PATTERN (p)))
	  /* Don't change hard registers used by a call.  */
	  || (CALL_P (p) && sregno < FIRST_PSEUDO_REGISTER
	      && find_reg_fusage (p, USE, src))
	  /* Don't change a USE of a register.  */
	  || (GET_CODE (PATTERN (p)) == USE
	      && reg_overlap_mentioned_p (src, XEXP (PATTERN (p), 0))))
	break;

      /* See if all of SRC dies in P.  This test is slightly more
	 conservative than it needs to be.  */
      if ((note = find_regno_note (p, REG_DEAD, sregno)) != 0
	  && GET_MODE (XEXP (note, 0)) == GET_MODE (src))
	{
	  int failed = 0;
	  int d_length = 0;
	  int s_length = 0;
	  int d_n_calls = 0;
	  int s_n_calls = 0;
	  int s_freq_calls = 0;
	  int d_freq_calls = 0;

	  /* We can do the optimization.  Scan forward from INSN again,
	     replacing regs as we go.  Set FAILED if a replacement can't
	     be done.  In that case, we can't move the death note for SRC.
	     This should be rare.  */

	  /* Set to stop at next insn.  */
	  for (q = next_real_insn (insn);
	       q != next_real_insn (p);
	       q = next_real_insn (q))
	    {
	      if (reg_overlap_mentioned_p (src, PATTERN (q)))
		{
		  /* If SRC is a hard register, we might miss some
		     overlapping registers with validate_replace_rtx,
		     so we would have to undo it.  We can't if DEST is
		     present in the insn, so fail in that combination
		     of cases.  */
		  if (sregno < FIRST_PSEUDO_REGISTER
		      && reg_mentioned_p (dest, PATTERN (q)))
		    failed = 1;

		  /* Attempt to replace all uses.  */
		  else if (!validate_replace_rtx (src, dest, q))
		    failed = 1;

		  /* If this succeeded, but some part of the register
		     is still present, undo the replacement.  */
		  else if (sregno < FIRST_PSEUDO_REGISTER
			   && reg_overlap_mentioned_p (src, PATTERN (q)))
		    {
		      validate_replace_rtx (dest, src, q);
		      failed = 1;
		    }
		}

	      /* For SREGNO, count the total number of insns scanned.
		 For DREGNO, count the total number of insns scanned after
		 passing the death note for DREGNO.  */
	      if (!DEBUG_INSN_P (p))
		{
		  s_length++;
		  if (dest_death)
		    d_length++;
		}

	      /* If the insn in which SRC dies is a CALL_INSN, don't count it
		 as a call that has been crossed.  Otherwise, count it.  */
	      if (q != p && CALL_P (q))
		{
		  /* Similarly, total calls for SREGNO, total calls beyond
		     the death note for DREGNO.  */
		  s_n_calls++;
		  s_freq_calls += REG_FREQ_FROM_BB  (BLOCK_FOR_INSN (q));
		  if (dest_death)
		    {
		      d_n_calls++;
		      d_freq_calls += REG_FREQ_FROM_BB  (BLOCK_FOR_INSN (q));
		    }
		}

	      /* If DEST dies here, remove the death note and save it for
		 later.  Make sure ALL of DEST dies here; again, this is
		 overly conservative.  */
	      if (dest_death == 0
		  && (dest_death = find_regno_note (q, REG_DEAD, dregno)) != 0)
		{
		  if (GET_MODE (XEXP (dest_death, 0)) != GET_MODE (dest))
		    failed = 1, dest_death = 0;
		  else
		    remove_note (q, dest_death);
		}
	    }

	  if (! failed)
	    {
	      /* These counters need to be updated if and only if we are
		 going to move the REG_DEAD note.  */
	      if (sregno >= FIRST_PSEUDO_REGISTER)
		{
		  if (REG_LIVE_LENGTH (sregno) >= 0)
		    {
		      REG_LIVE_LENGTH (sregno) -= s_length;
		      /* REG_LIVE_LENGTH is only an approximation after
			 combine if sched is not run, so make sure that we
			 still have a reasonable value.  */
		      if (REG_LIVE_LENGTH (sregno) < 2)
			REG_LIVE_LENGTH (sregno) = 2;
		    }

		  REG_N_CALLS_CROSSED (sregno) -= s_n_calls;
		  REG_FREQ_CALLS_CROSSED (sregno) -= s_freq_calls;
		}

	      /* Move death note of SRC from P to INSN.  */
	      remove_note (p, note);
	      XEXP (note, 1) = REG_NOTES (insn);
	      REG_NOTES (insn) = note;
	    }

	  /* DEST is also dead if INSN has a REG_UNUSED note for DEST.  */
	  if (! dest_death
	      && (dest_death = find_regno_note (insn, REG_UNUSED, dregno)))
	    {
	      PUT_REG_NOTE_KIND (dest_death, REG_DEAD);
	      remove_note (insn, dest_death);
	    }

	  /* Put death note of DEST on P if we saw it die.  */
	  if (dest_death)
	    {
	      XEXP (dest_death, 1) = REG_NOTES (p);
	      REG_NOTES (p) = dest_death;

	      if (dregno >= FIRST_PSEUDO_REGISTER)
		{
		  /* If and only if we are moving the death note for DREGNO,
		     then we need to update its counters.  */
		  if (REG_LIVE_LENGTH (dregno) >= 0)
		    REG_LIVE_LENGTH (dregno) += d_length;
		  REG_N_CALLS_CROSSED (dregno) += d_n_calls;
		  REG_FREQ_CALLS_CROSSED (dregno) += d_freq_calls;
		}
	    }

	  return ! failed;
	}

      /* If SRC is a hard register which is set or killed in some other
	 way, we can't do this optimization.  */
      else if (sregno < FIRST_PSEUDO_REGISTER
	       && dead_or_set_p (p, src))
	break;
    }
  return 0;
}

/* INSN is a copy of SRC to DEST, in which SRC dies.  See if we now have
   a sequence of insns that modify DEST followed by an insn that sets
   SRC to DEST in which DEST dies, with no prior modification of DEST.
   (There is no need to check if the insns in between actually modify
   DEST.  We should not have cases where DEST is not modified, but
   the optimization is safe if no such modification is detected.)
   In that case, we can replace all uses of DEST, starting with INSN and
   ending with the set of SRC to DEST, with SRC.  We do not do this
   optimization if a CALL_INSN is crossed unless SRC already crosses a
   call or if DEST dies before the copy back to SRC.

   It is assumed that DEST and SRC are pseudos; it is too complicated to do
   this for hard registers since the substitutions we may make might fail.  */

static void
optimize_reg_copy_2 (rtx insn, rtx dest, rtx src)
{
  rtx p, q;
  rtx set;
  int sregno = REGNO (src);
  int dregno = REGNO (dest);
  basic_block bb = BLOCK_FOR_INSN (insn);

  for (p = NEXT_INSN (insn); p; p = NEXT_INSN (p))
    {
      if (! INSN_P (p))
	continue;
      if (BLOCK_FOR_INSN (p) != bb)
	break;

      set = single_set (p);
      if (set && SET_SRC (set) == dest && SET_DEST (set) == src
	  && find_reg_note (p, REG_DEAD, dest))
	{
	  /* We can do the optimization.  Scan forward from INSN again,
	     replacing regs as we go.  */

	  /* Set to stop at next insn.  */
	  for (q = insn; q != NEXT_INSN (p); q = NEXT_INSN (q))
	    if (INSN_P (q))
	      {
		if (reg_mentioned_p (dest, PATTERN (q)))
		  {
		    rtx note;

		    PATTERN (q) = replace_rtx (PATTERN (q), dest, src);
		    note = FIND_REG_INC_NOTE (q, dest);
		    if (note)
		      {
			remove_note (q, note);
			add_reg_note (q, REG_INC, src);
		      }
		    df_insn_rescan (q);
		  }

		if (CALL_P (q))
		  {
		    int freq = REG_FREQ_FROM_BB  (BLOCK_FOR_INSN (q));
		    REG_N_CALLS_CROSSED (dregno)--;
		    REG_N_CALLS_CROSSED (sregno)++;
		    REG_FREQ_CALLS_CROSSED (dregno) -= freq;
		    REG_FREQ_CALLS_CROSSED (sregno) += freq;
		  }
	      }

	  remove_note (p, find_reg_note (p, REG_DEAD, dest));
	  REG_N_DEATHS (dregno)--;
	  remove_note (insn, find_reg_note (insn, REG_DEAD, src));
	  REG_N_DEATHS (sregno)--;
	  return;
	}

      if (reg_set_p (src, p)
	  || find_reg_note (p, REG_DEAD, dest)
	  || (CALL_P (p) && REG_N_CALLS_CROSSED (sregno) == 0))
	break;
    }
}

/* INSN is a ZERO_EXTEND or SIGN_EXTEND of SRC to DEST.
   Look if SRC dies there, and if it is only set once, by loading
   it from memory.  If so, try to incorporate the zero/sign extension
   into the memory read, change SRC to the mode of DEST, and alter
   the remaining accesses to use the appropriate SUBREG.  This allows
   SRC and DEST to be tied later.  */
static void
optimize_reg_copy_3 (rtx insn, rtx dest, rtx src)
{
  rtx src_reg = XEXP (src, 0);
  int src_no = REGNO (src_reg);
  int dst_no = REGNO (dest);
  rtx p, set, set_insn;
  enum machine_mode old_mode;
  basic_block bb = BLOCK_FOR_INSN (insn);

  if (src_no < FIRST_PSEUDO_REGISTER
      || dst_no < FIRST_PSEUDO_REGISTER
      || ! find_reg_note (insn, REG_DEAD, src_reg)
      || REG_N_DEATHS (src_no) != 1
      || REG_N_SETS (src_no) != 1)
    return;

  for (p = PREV_INSN (insn); p && ! reg_set_p (src_reg, p); p = PREV_INSN (p))
    if (INSN_P (p) && BLOCK_FOR_INSN (p) != bb)
      break;

  if (! p || BLOCK_FOR_INSN (p) != bb)
    return;

  if (! (set = single_set (p))
      || !MEM_P (SET_SRC (set))
      /* If there's a REG_EQUIV note, this must be an insn that loads an
	 argument.  Prefer keeping the note over doing this optimization.  */
      || find_reg_note (p, REG_EQUIV, NULL_RTX)
      || SET_DEST (set) != src_reg)
    return;

  /* Be conservative: although this optimization is also valid for
     volatile memory references, that could cause trouble in later passes.  */
  if (MEM_VOLATILE_P (SET_SRC (set)))
    return;

  /* Do not use a SUBREG to truncate from one mode to another if truncation
     is not a nop.  */
  if (GET_MODE_BITSIZE (GET_MODE (src_reg)) <= GET_MODE_BITSIZE (GET_MODE (src))
      && !TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (GET_MODE (src)),
				 GET_MODE_BITSIZE (GET_MODE (src_reg))))
    return;

  set_insn = p;
  old_mode = GET_MODE (src_reg);
  PUT_MODE (src_reg, GET_MODE (src));
  XEXP (src, 0) = SET_SRC (set);

  /* Include this change in the group so that it's easily undone if
     one of the changes in the group is invalid.  */
  validate_change (p, &SET_SRC (set), src, 1);

  /* Now walk forward making additional replacements.  We want to be able
     to undo all the changes if a later substitution fails.  */
  while (p = NEXT_INSN (p), p != insn)
    {
      if (! INSN_P (p))
	continue;

      /* Make a tentative change.  */
      validate_replace_rtx_group (src_reg,
				  gen_lowpart_SUBREG (old_mode, src_reg),
				  p);
    }

  validate_replace_rtx_group (src, src_reg, insn);

  /* Now see if all the changes are valid.  */
  if (! apply_change_group ())
    {
      /* One or more changes were no good.  Back out everything.  */
      PUT_MODE (src_reg, old_mode);
      XEXP (src, 0) = src_reg;
    }
  else
    {
      rtx note = find_reg_note (set_insn, REG_EQUAL, NULL_RTX);
      if (note)
	{
	  if (rtx_equal_p (XEXP (note, 0), XEXP (src, 0)))
	    {
	      XEXP (note, 0)
		= gen_rtx_fmt_e (GET_CODE (src), GET_MODE (src),
				 XEXP (note, 0));
	      df_notes_rescan (set_insn);
	    }
	  else
	    remove_note (set_insn, note);
	}
    }
}


/* If we were not able to update the users of src to use dest directly, try
   instead moving the value to dest directly before the operation.  */

static void
copy_src_to_dest (rtx insn, rtx src, rtx dest)
{
  rtx seq;
  rtx link;
  rtx next;
  rtx set;
  rtx move_insn;
  rtx *p_insn_notes;
  rtx *p_move_notes;
  int src_regno;
  int dest_regno;

  /* A REG_LIVE_LENGTH of -1 indicates the register is equivalent to a constant
     or memory location and is used infrequently; a REG_LIVE_LENGTH of -2 is
     parameter when there is no frame pointer that is not allocated a register.
     For now, we just reject them, rather than incrementing the live length.  */

  if (REG_P (src)
      && REG_LIVE_LENGTH (REGNO (src)) > 0
      && REG_P (dest)
      && REG_LIVE_LENGTH (REGNO (dest)) > 0
      && (set = single_set (insn)) != NULL_RTX
      && !reg_mentioned_p (dest, SET_SRC (set))
      && GET_MODE (src) == GET_MODE (dest))
    {
      int old_num_regs = reg_rtx_no;

      /* Generate the src->dest move.  */
      start_sequence ();
      emit_move_insn (dest, src);
      seq = get_insns ();
      end_sequence ();
      /* If this sequence uses new registers, we may not use it.  */
      if (old_num_regs != reg_rtx_no
	  || ! validate_replace_rtx (src, dest, insn))
	{
	  /* We have to restore reg_rtx_no to its old value, lest
	     recompute_reg_usage will try to compute the usage of the
	     new regs, yet reg_n_info is not valid for them.  */
	  reg_rtx_no = old_num_regs;
	  return;
	}
      emit_insn_before (seq, insn);
      move_insn = PREV_INSN (insn);
      p_move_notes = &REG_NOTES (move_insn);
      p_insn_notes = &REG_NOTES (insn);

      /* Move any notes mentioning src to the move instruction.  */
      for (link = REG_NOTES (insn); link != NULL_RTX; link = next)
	{
	  next = XEXP (link, 1);
	  if (XEXP (link, 0) == src)
	    {
	      *p_move_notes = link;
	      p_move_notes = &XEXP (link, 1);
	    }
	  else
	    {
	      *p_insn_notes = link;
	      p_insn_notes = &XEXP (link, 1);
	    }
	}

      *p_move_notes = NULL_RTX;
      *p_insn_notes = NULL_RTX;

      /* Update the various register tables.  */
      dest_regno = REGNO (dest);
      INC_REG_N_SETS (dest_regno, 1);
      REG_LIVE_LENGTH (dest_regno)++;
      src_regno = REGNO (src);
      if (! find_reg_note (move_insn, REG_DEAD, src))
	REG_LIVE_LENGTH (src_regno)++;
    }
}

/* reg_set_in_bb[REGNO] points to basic block iff the register is set
   only once in the given block and has REG_EQUAL note.  */

static basic_block *reg_set_in_bb;

/* Size of reg_set_in_bb array.  */
static unsigned int max_reg_computed;


/* Return whether REG is set in only one location, and is set to a
   constant, but is set in a different basic block from INSN (an
   instructions which uses REG).  In this case REG is equivalent to a
   constant, and we don't want to break that equivalence, because that
   may increase register pressure and make reload harder.  If REG is
   set in the same basic block as INSN, we don't worry about it,
   because we'll probably need a register anyhow (??? but what if REG
   is used in a different basic block as well as this one?).  */

static bool
reg_is_remote_constant_p (rtx reg, rtx insn)
{
  basic_block bb;
  rtx p;
  int max;

  if (!reg_set_in_bb)
    {
      max_reg_computed = max = max_reg_num ();
      reg_set_in_bb = XCNEWVEC (basic_block, max);

      FOR_EACH_BB (bb)
	FOR_BB_INSNS (bb, p)
	  {
	    rtx s;

	    if (!INSN_P (p))
	      continue;
	    s = single_set (p);
	    /* This is the instruction which sets REG.  If there is a
	       REG_EQUAL note, then REG is equivalent to a constant.  */
	    if (s != 0
	        && REG_P (SET_DEST (s))
	        && REG_N_SETS (REGNO (SET_DEST (s))) == 1
	        && find_reg_note (p, REG_EQUAL, NULL_RTX))
	      reg_set_in_bb[REGNO (SET_DEST (s))] = bb;
	  }
    }

  gcc_assert (REGNO (reg) < max_reg_computed);
  if (reg_set_in_bb[REGNO (reg)] == NULL)
    return false;
  return (reg_set_in_bb[REGNO (reg)] != BLOCK_FOR_INSN (insn));
}

/* INSN is adding a CONST_INT to a REG.  We search backwards looking for
   another add immediate instruction with the same source and dest registers,
   and if we find one, we change INSN to an increment, and return 1.  If
   no changes are made, we return 0.

   This changes
     (set (reg100) (plus reg1 offset1))
     ...
     (set (reg100) (plus reg1 offset2))
   to
     (set (reg100) (plus reg1 offset1))
     ...
     (set (reg100) (plus reg100 offset2-offset1))  */

/* ??? What does this comment mean?  */
/* cse disrupts preincrement / postdecrement sequences when it finds a
   hard register as ultimate source, like the frame pointer.  */

static int
fixup_match_2 (rtx insn, rtx dst, rtx src, rtx offset)
{
  rtx p, dst_death = 0;
  int length, num_calls = 0, freq_calls = 0;
  basic_block bb = BLOCK_FOR_INSN (insn);

  /* If SRC dies in INSN, we'd have to move the death note.  This is
     considered to be very unlikely, so we just skip the optimization
     in this case.  */
  if (find_regno_note (insn, REG_DEAD, REGNO (src)))
    return 0;

  /* Scan backward to find the first instruction that sets DST.  */

  for (length = 0, p = PREV_INSN (insn); p; p = PREV_INSN (p))
    {
      rtx pset;

      if (! INSN_P (p))
	continue;
      if (BLOCK_FOR_INSN (p) != bb)
	break;

      if (find_regno_note (p, REG_DEAD, REGNO (dst)))
	dst_death = p;
      if (! dst_death && !DEBUG_INSN_P (p))
	length++;

      pset = single_set (p);
      if (pset && SET_DEST (pset) == dst
	  && GET_CODE (SET_SRC (pset)) == PLUS
	  && XEXP (SET_SRC (pset), 0) == src
	  && CONST_INT_P (XEXP (SET_SRC (pset), 1)))
	{
	  HOST_WIDE_INT newconst
	    = INTVAL (offset) - INTVAL (XEXP (SET_SRC (pset), 1));
	  rtx add = gen_add3_insn (dst, dst, GEN_INT (newconst));

	  if (add && validate_change (insn, &PATTERN (insn), add, 0))
	    {
	      /* Remove the death note for DST from DST_DEATH.  */
	      if (dst_death)
		{
		  remove_death (REGNO (dst), dst_death);
		  REG_LIVE_LENGTH (REGNO (dst)) += length;
		  REG_N_CALLS_CROSSED (REGNO (dst)) += num_calls;
		  REG_FREQ_CALLS_CROSSED (REGNO (dst)) += freq_calls;
		}

	      if (dump_file)
		fprintf (dump_file,
			 "Fixed operand of insn %d.\n",
			  INSN_UID (insn));

#ifdef AUTO_INC_DEC
	      for (p = PREV_INSN (insn); p; p = PREV_INSN (p))
		{
		  if (! INSN_P (p))
		    continue;
		  if (BLOCK_FOR_INSN (p) != bb)
		    break;
		  if (reg_overlap_mentioned_p (dst, PATTERN (p)))
		    {
		      if (try_auto_increment (p, insn, 0, dst, newconst, 0))
			return 1;
		      break;
		    }
		}
	      for (p = NEXT_INSN (insn); p; p = NEXT_INSN (p))
		{
		  if (! INSN_P (p))
		    continue;
		  if (BLOCK_FOR_INSN (p) != bb)
		    break;
		  if (reg_overlap_mentioned_p (dst, PATTERN (p)))
		    {
		      try_auto_increment (p, insn, 0, dst, newconst, 1);
		      break;
		    }
		}
#endif
	      return 1;
	    }
	}

      if (reg_set_p (dst, PATTERN (p)))
	break;

      /* If we have passed a call instruction, and the
         pseudo-reg SRC is not already live across a call,
         then don't perform the optimization.  */
      /* reg_set_p is overly conservative for CALL_INSNS, thinks that all
	 hard regs are clobbered.  Thus, we only use it for src for
	 non-call insns.  */
      if (CALL_P (p))
	{
	  if (! dst_death)
	    {
	      num_calls++;
	      freq_calls += REG_FREQ_FROM_BB  (BLOCK_FOR_INSN (p));
	    }

	  if (REG_N_CALLS_CROSSED (REGNO (src)) == 0)
	    break;

	  if (call_used_regs [REGNO (dst)]
	      || find_reg_fusage (p, CLOBBER, dst))
	    break;
	}
      else if (reg_set_p (src, PATTERN (p)))
	break;
    }

  return 0;
}

/* A forward pass.  Replace output operands with input operands.  */

static void
regmove_forward_pass (void)
{
  basic_block bb;
  rtx insn;

  if (! flag_expensive_optimizations)
    return;

  if (dump_file)
    fprintf (dump_file, "Starting forward pass...\n");

  FOR_EACH_BB (bb)
    {
      FOR_BB_INSNS (bb, insn)
	{
	  rtx set = single_set (insn);
	  if (! set)
	    continue;

	  if ((GET_CODE (SET_SRC (set)) == SIGN_EXTEND
	       || GET_CODE (SET_SRC (set)) == ZERO_EXTEND)
	      && REG_P (XEXP (SET_SRC (set), 0))
	      && REG_P (SET_DEST (set)))
	    optimize_reg_copy_3 (insn, SET_DEST (set), SET_SRC (set));

	  if (REG_P (SET_SRC (set))
	      && REG_P (SET_DEST (set)))
	    {
	      /* If this is a register-register copy where SRC is not dead,
		 see if we can optimize it.  If this optimization succeeds,
		 it will become a copy where SRC is dead.  */
	      if ((find_reg_note (insn, REG_DEAD, SET_SRC (set))
		   || optimize_reg_copy_1 (insn, SET_DEST (set), SET_SRC (set)))
		  && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER)
		{
		  /* Similarly for a pseudo-pseudo copy when SRC is dead.  */
		  if (REGNO (SET_SRC (set)) >= FIRST_PSEUDO_REGISTER)
		    optimize_reg_copy_2 (insn, SET_DEST (set), SET_SRC (set));
		  if (regno_src_regno[REGNO (SET_DEST (set))] < 0
		      && SET_SRC (set) != SET_DEST (set))
		    {
		      int srcregno = REGNO (SET_SRC (set));
		      if (regno_src_regno[srcregno] >= 0)
			srcregno = regno_src_regno[srcregno];
		      regno_src_regno[REGNO (SET_DEST (set))] = srcregno;
		    }
		}
	    }
	}
    }
}

/* A backward pass.  Replace input operands with output operands.  */

static void
regmove_backward_pass (void)
{
  basic_block bb;
  rtx insn, prev;

  if (dump_file)
    fprintf (dump_file, "Starting backward pass...\n");

  FOR_EACH_BB_REVERSE (bb)
    {
      /* ??? Use the safe iterator because fixup_match_2 can remove
	     insns via try_auto_increment.  */
      FOR_BB_INSNS_REVERSE_SAFE (bb, insn, prev)
	{
	  struct match match;
	  rtx copy_src, copy_dst;
	  int op_no, match_no;
	  int success = 0;

	  if (! INSN_P (insn))
	    continue;

	  if (! find_matches (insn, &match))
	    continue;

	  /* Now scan through the operands looking for a destination operand
	     which is supposed to match a source operand.
	     Then scan backward for an instruction which sets the source
	     operand.  If safe, then replace the source operand with the
	     dest operand in both instructions.  */

	  copy_src = NULL_RTX;
	  copy_dst = NULL_RTX;
	  for (op_no = 0; op_no < recog_data.n_operands; op_no++)
	    {
	      rtx set, p, src, dst;
	      rtx src_note, dst_note;
	      int num_calls = 0, freq_calls = 0;
	      enum reg_class src_class, dst_class;
	      int length;

	      match_no = match.with[op_no];

	      /* Nothing to do if the two operands aren't supposed to match.  */
	      if (match_no < 0)
		continue;

	      dst = recog_data.operand[match_no];
	      src = recog_data.operand[op_no];

	      if (!REG_P (src))
		continue;

	      if (!REG_P (dst)
		  || REGNO (dst) < FIRST_PSEUDO_REGISTER
		  || REG_LIVE_LENGTH (REGNO (dst)) < 0
		  || GET_MODE (src) != GET_MODE (dst))
		continue;

	      /* If the operands already match, then there is nothing to do.  */
	      if (operands_match_p (src, dst))
		continue;

	      if (match.commutative[op_no] >= 0)
		{
		  rtx comm = recog_data.operand[match.commutative[op_no]];
		  if (operands_match_p (comm, dst))
		    continue;
		}

	      set = single_set (insn);
	      if (! set)
		continue;

	      /* Note that single_set ignores parts of a parallel set for
		 which one of the destinations is REG_UNUSED.  We can't
		 handle that here, since we can wind up rewriting things
		 such that a single register is set twice within a single
		 parallel.  */
	      if (reg_set_p (src, insn))
		continue;

	      /* match_no/dst must be a write-only operand, and
		 operand_operand/src must be a read-only operand.  */
	      if (match.use[op_no] != READ
		  || match.use[match_no] != WRITE)
		continue;

	      if (match.early_clobber[match_no]
		  && count_occurrences (PATTERN (insn), src, 0) > 1)
		continue;

	      /* Make sure match_no is the destination.  */
	      if (recog_data.operand[match_no] != SET_DEST (set))
		continue;

	      if (REGNO (src) < FIRST_PSEUDO_REGISTER)
		{
		  if (GET_CODE (SET_SRC (set)) == PLUS
		      && CONST_INT_P (XEXP (SET_SRC (set), 1))
		      && XEXP (SET_SRC (set), 0) == src
		      && fixup_match_2 (insn, dst, src,
					XEXP (SET_SRC (set), 1)))
		    break;
		  continue;
		}
	      src_class = reg_preferred_class (REGNO (src));
	      dst_class = reg_preferred_class (REGNO (dst));

	      if (! (src_note = find_reg_note (insn, REG_DEAD, src)))
		{
		  /* We used to force the copy here like in other cases, but
		     it produces worse code, as it eliminates no copy
		     instructions and the copy emitted will be produced by
		     reload anyway.  On patterns with multiple alternatives,
		     there may be better solution available.

		     In particular this change produced slower code for numeric
		     i387 programs.  */

		  continue;
		}

	      if (! regclass_compatible_p (src_class, dst_class))
		{
		  if (!copy_src)
		    {
		      copy_src = src;
		      copy_dst = dst;
		    }
		  continue;
		}

	      /* Can not modify an earlier insn to set dst if this insn
		 uses an old value in the source.  */
	      if (reg_overlap_mentioned_p (dst, SET_SRC (set)))
		{
		  if (!copy_src)
		    {
		      copy_src = src;
		      copy_dst = dst;
		    }
		  continue;
		}

	      /* If src is set once in a different basic block,
		 and is set equal to a constant, then do not use
		 it for this optimization, as this would make it
		 no longer equivalent to a constant.  */

	      if (reg_is_remote_constant_p (src, insn))
		{
		  if (!copy_src)
		    {
		      copy_src = src;
		      copy_dst = dst;
		    }
		  continue;
		}


	      if (dump_file)
		fprintf (dump_file,
			 "Could fix operand %d of insn %d matching operand %d.\n",
			 op_no, INSN_UID (insn), match_no);

	      /* Scan backward to find the first instruction that uses
		 the input operand.  If the operand is set here, then
		 replace it in both instructions with match_no.  */

	      for (length = 0, p = PREV_INSN (insn); p; p = PREV_INSN (p))
		{
		  rtx pset;

		  if (! INSN_P (p))
		    continue;
		  if (BLOCK_FOR_INSN (p) != bb)
		    break;

		  if (!DEBUG_INSN_P (p))
		    length++;

		  /* ??? See if all of SRC is set in P.  This test is much
		     more conservative than it needs to be.  */
		  pset = single_set (p);
		  if (pset && SET_DEST (pset) == src)
		    {
		      /* We use validate_replace_rtx, in case there
			 are multiple identical source operands.  All
			 of them have to be changed at the same time:
			 when validate_replace_rtx() calls
			 apply_change_group().  */
		      validate_change (p, &SET_DEST (pset), dst, 1);
		      if (validate_replace_rtx (src, dst, insn))
			success = 1;
		      break;
		    }

		  /* We can't make this change if DST is mentioned at
		     all in P, since we are going to change its value.
		     We can't make this change if SRC is read or
		     partially written in P, since we are going to
		     eliminate SRC.  However, if it's a debug insn, we
		     can't refrain from making the change, for this
		     would cause codegen differences, so instead we
		     invalidate debug expressions that reference DST,
		     and adjust references to SRC in them so that they
		     become references to DST.  */
		  if (reg_mentioned_p (dst, PATTERN (p)))
		    {
		      if (DEBUG_INSN_P (p))
			validate_change (p, &INSN_VAR_LOCATION_LOC (p),
					 gen_rtx_UNKNOWN_VAR_LOC (), 1);
		      else
			break;
		    }
		  if (reg_overlap_mentioned_p (src, PATTERN (p)))
		    {
		      if (DEBUG_INSN_P (p))
			validate_replace_rtx_group (src, dst, p);
		      else
			break;
		    }

		  /* If we have passed a call instruction, and the
		     pseudo-reg DST is not already live across a call,
		     then don't perform the optimization.  */
		  if (CALL_P (p))
		    {
		      num_calls++;
		      freq_calls += REG_FREQ_FROM_BB  (BLOCK_FOR_INSN (p));

		      if (REG_N_CALLS_CROSSED (REGNO (dst)) == 0)
			break;
		    }
		}

	      if (success)
		{
		  int dstno, srcno;

		  /* Remove the death note for SRC from INSN.  */
		  remove_note (insn, src_note);
		  /* Move the death note for SRC to P if it is used
		     there.  */
		  if (reg_overlap_mentioned_p (src, PATTERN (p)))
		    {
		      XEXP (src_note, 1) = REG_NOTES (p);
		      REG_NOTES (p) = src_note;
		    }
		  /* If there is a REG_DEAD note for DST on P, then remove
		     it, because DST is now set there.  */
		  if ((dst_note = find_reg_note (p, REG_DEAD, dst)))
		    remove_note (p, dst_note);

		  dstno = REGNO (dst);
		  srcno = REGNO (src);

		  INC_REG_N_SETS (dstno, 1);
		  INC_REG_N_SETS (srcno, -1);

		  REG_N_CALLS_CROSSED (dstno) += num_calls;
		  REG_N_CALLS_CROSSED (srcno) -= num_calls;
		  REG_FREQ_CALLS_CROSSED (dstno) += freq_calls;
		  REG_FREQ_CALLS_CROSSED (srcno) -= freq_calls;

		  REG_LIVE_LENGTH (dstno) += length;
		  if (REG_LIVE_LENGTH (srcno) >= 0)
		    {
		      REG_LIVE_LENGTH (srcno) -= length;
		      /* REG_LIVE_LENGTH is only an approximation after
			 combine if sched is not run, so make sure that we
			 still have a reasonable value.  */
		      if (REG_LIVE_LENGTH (srcno) < 2)
			REG_LIVE_LENGTH (srcno) = 2;
		    }

		  if (dump_file)
		    fprintf (dump_file,
			     "Fixed operand %d of insn %d matching operand %d.\n",
			     op_no, INSN_UID (insn), match_no);

		  break;
		}
	      else if (num_changes_pending () > 0)
		cancel_changes (0);
	    }

	  /* If we weren't able to replace any of the alternatives, try an
	     alternative approach of copying the source to the destination.  */
	  if (!success && copy_src != NULL_RTX)
	    copy_src_to_dest (insn, copy_src, copy_dst);
	}
    }
}

/* Main entry for the register move optimization.  */

static unsigned int
regmove_optimize (void)
{
  int i;
  int nregs = max_reg_num ();

  df_note_add_problem ();
  df_analyze ();

  if (flag_ira_loop_pressure)
    ira_set_pseudo_classes (dump_file);

  regstat_init_n_sets_and_refs ();
  regstat_compute_ri ();

  regno_src_regno = XNEWVEC (int, nregs);
  for (i = nregs; --i >= 0; )
    regno_src_regno[i] = -1;

  /* A forward pass.  Replace output operands with input operands.  */
  regmove_forward_pass ();

  /* A backward pass.  Replace input operands with output operands.  */
  regmove_backward_pass ();

  /* Clean up.  */
  free (regno_src_regno);
  if (reg_set_in_bb)
    {
      free (reg_set_in_bb);
      reg_set_in_bb = NULL;
    }
  regstat_free_n_sets_and_refs ();
  regstat_free_ri ();
  if (flag_ira_loop_pressure)
    free_reg_info ();
  return 0;
}

/* Returns nonzero if INSN's pattern has matching constraints for any operand.
   Returns 0 if INSN can't be recognized, or if the alternative can't be
   determined.

   Initialize the info in MATCHP based on the constraints.  */

static int
find_matches (rtx insn, struct match *matchp)
{
  int likely_spilled[MAX_RECOG_OPERANDS];
  int op_no;
  int any_matches = 0;

  extract_insn (insn);
  if (! constrain_operands (0))
    return 0;

  /* Must initialize this before main loop, because the code for
     the commutative case may set matches for operands other than
     the current one.  */
  for (op_no = recog_data.n_operands; --op_no >= 0; )
    matchp->with[op_no] = matchp->commutative[op_no] = -1;

  for (op_no = 0; op_no < recog_data.n_operands; op_no++)
    {
      const char *p;
      char c;
      int i = 0;

      p = recog_data.constraints[op_no];

      likely_spilled[op_no] = 0;
      matchp->use[op_no] = READ;
      matchp->early_clobber[op_no] = 0;
      if (*p == '=')
	matchp->use[op_no] = WRITE;
      else if (*p == '+')
	matchp->use[op_no] = READWRITE;

      for (;*p && i < which_alternative; p++)
	if (*p == ',')
	  i++;

      while ((c = *p) != '\0' && c != ',')
	{
	  switch (c)
	    {
	    case '=':
	      break;
	    case '+':
	      break;
	    case '&':
	      matchp->early_clobber[op_no] = 1;
	      break;
	    case '%':
	      matchp->commutative[op_no] = op_no + 1;
	      matchp->commutative[op_no + 1] = op_no;
	      break;

	    case '0': case '1': case '2': case '3': case '4':
	    case '5': case '6': case '7': case '8': case '9':
	      {
		char *end;
		unsigned long match_ul = strtoul (p, &end, 10);
		int match = match_ul;

		p = end;

		if (match < op_no && likely_spilled[match])
		  continue;
		matchp->with[op_no] = match;
		any_matches = 1;
		if (matchp->commutative[op_no] >= 0)
		  matchp->with[matchp->commutative[op_no]] = match;
	      }
	    continue;

	  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'h':
	  case 'j': case 'k': case 'l': case 'p': case 'q': case 't': case 'u':
	  case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
	  case 'C': case 'D': case 'W': case 'Y': case 'Z':
	    if (targetm.class_likely_spilled_p (REG_CLASS_FROM_CONSTRAINT ((unsigned char) c, p)))
	      likely_spilled[op_no] = 1;
	    break;
	  }
	  p += CONSTRAINT_LEN (c, p);
	}
    }
  return any_matches;
}



static bool
gate_handle_regmove (void)
{
  return (optimize > 0 && flag_regmove);
}


struct rtl_opt_pass pass_regmove =
{
 {
  RTL_PASS,
  "regmove",                            /* name */
  gate_handle_regmove,                  /* gate */
  regmove_optimize,			/* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_REGMOVE,                           /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_dump_func |
  TODO_ggc_collect                      /* todo_flags_finish */
 }
};
