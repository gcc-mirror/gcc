/* Move registers around to reduce number of move instructions needed.
   Copyright (C) 1987, 88, 89, 92-5, 1996, 1997 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This module looks for cases where matching constraints would force
   an instruction to need a reload, and this reload would be a register
   to register move.  It then attempts to change the registers used by the
   instruction to avoid the move instruction.  */

#include "config.h"
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/* Must precede rtl.h for FFS.  */
#include <stdio.h>

#include "rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "reload.h"
#include "regs.h"

static int stable_but_for_p PROTO((rtx, rtx, rtx));

#if defined (HAVE_POST_INCREMENT) || defined (HAVE_POST_DECREMENT) \
    || defined (HAVE_PRE_INCREMENT) || defined (HAVE_PRE_DECREMENT)

/* INC_INSN is an instruction that adds INCREMENT to REG.
   Try to fold INC_INSN as a post/pre in/decrement into INSN.
   Iff INC_INSN_SET is nonzero, inc_insn has a destination different from src.
   Return nonzero for success.  */
static int
try_auto_increment (insn, inc_insn, inc_insn_set, reg, increment, pre)
     rtx reg, insn, inc_insn ,inc_insn_set;
     HOST_WIDE_INT increment;
     int pre;
{
  enum rtx_code inc_code;

  rtx pset = single_set (insn);
  if (pset)
    {
      /* Can't use the size of SET_SRC, we might have something like
	 (sign_extend:SI (mem:QI ...  */
      rtx use = find_use_as_address (pset, reg, 0);
      if (use != 0 && use != (rtx) 1)
	{
	  int size = GET_MODE_SIZE (GET_MODE (use));
	  if (0
#ifdef HAVE_POST_INCREMENT
	      || (pre == 0 && (inc_code = POST_INC, increment == size))
#endif
#ifdef HAVE_PRE_INCREMENT
	      || (pre == 1 && (inc_code = PRE_INC, increment == size))
#endif
#ifdef HAVE_POST_DECREMENT
	      || (pre == 0 && (inc_code = POST_DEC, increment == -size))
#endif
#ifdef HAVE_PRE_DECREMENT
	      || (pre == 1 && (inc_code = PRE_DEC, increment == -size))
#endif
          )
            {
              if (inc_insn_set)
                validate_change
                  (inc_insn, 
                   &SET_SRC (inc_insn_set),
		   XEXP (SET_SRC (inc_insn_set), 0), 1);
              validate_change (insn, &XEXP (use, 0),
                               gen_rtx (inc_code,
                                        Pmode,
                                        reg), 1);
              if (apply_change_group ())
                {
                  REG_NOTES (insn)
                    = gen_rtx (EXPR_LIST, REG_INC,
                               reg, REG_NOTES (insn));
                  if (! inc_insn_set)
                    {
                      PUT_CODE (inc_insn, NOTE);
                      NOTE_LINE_NUMBER (inc_insn) = NOTE_INSN_DELETED;
                      NOTE_SOURCE_FILE (inc_insn) = 0;
                    }
		  return 1;
                }
            }
        }
    }
  return 0;
}
#endif  /* defined (HAVE_POST_INCREMENT) || defined (HAVE_POST_DECREMENT) */

void
regmove_optimize (f, nregs, regmove_dump_file)
     rtx f;
     int nregs;
     FILE *regmove_dump_file;
{
#ifdef REGISTER_CONSTRAINTS
  rtx insn;
  int matches[MAX_RECOG_OPERANDS][MAX_RECOG_OPERANDS];
  int modified[MAX_RECOG_OPERANDS];
  int early_clobber[MAX_RECOG_OPERANDS];
  int commutative;
  int pass;

  /* A forward/backward pass.  Replace output operands with input operands.  */

  for (pass = 0; pass < 2; pass++)
    {
      if (regmove_dump_file)
	fprintf (regmove_dump_file, "Starting %s pass...\n",
		 pass ? "backward" : "forward");

      for (insn = pass ? get_last_insn () : f; insn;
	   insn = pass ? PREV_INSN (insn) : NEXT_INSN (insn))
	{
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    {
	      int insn_code_number = recog_memoized (insn);
	      int operand_number, match_number;
	      
	      if (insn_code_number < 0)
		continue;
    
	      insn_extract (insn);
	      if (! constrain_operands (insn_code_number, 0))
		continue;
	      
	      commutative = -1;
    
	      /* Must initialize this before the loop, because the code for
		 the commutative case may set matches for operands other than
		 the current one.  */
	      bzero ((char *)matches, sizeof (matches));
    
	      for (operand_number = 0;
		   operand_number < insn_n_operands[insn_code_number];
		   operand_number++)
		{
		  int output_operand = 0;
		  int matching_operand = operand_number;
		  char *p, c;
		  int i = 0;
    
		  modified[operand_number] = 0;
		  early_clobber[operand_number] = 0;
    
		  p = insn_operand_constraint[insn_code_number][operand_number];

		  if (*p == '=')
		    modified[operand_number] = 2;
		  else if (*p == '+')
		    modified[operand_number] = 1;

		  for (;*p && i < which_alternative; p++)
		    if (*p == ',')
		      i++;
    
		  while ((c = *p++) != '\0' && c != ',')
		    switch (c)
		      {
		      case '=':
			break;
		      case '+':
			break;
		      case '&':
			early_clobber[operand_number] = 1;
			break;
		      case '%':
			commutative = operand_number;
			break;
		      case '0': case '1': case '2': case '3': case '4':
		      case '5': case '6': case '7': case '8': case '9':
			c -= '0';
			matches[operand_number][c] = 1;
			if (commutative >= 0)
			  {
			    if (c == commutative || c == commutative + 1)
			      {
				int other = c + (c == commutative ? 1 : -1);
				matches[operand_number][other] = 1;
			      }
			    if (operand_number == commutative
				|| operand_number == commutative + 1)
			      {
				int other = (operand_number
					     + (operand_number == commutative
						? 1 : -1));
				matches[other][c] = 1;
			      }
			  }
			break;
		      }
		}
    
	      /* Now scan through the operands looking for a source operand
		 which is supposed to match the destination operand.
		 Then scan forward for an instruction which uses the dest
		 operand.
		 If it dies there, then replace the dest in both operands with
		 the source operand.  */
    
	      for (operand_number = 0;
		   operand_number < insn_n_operands[insn_code_number];
		   operand_number++)
		{
		  for (match_number = 0;
		       match_number < insn_n_operands[insn_code_number];
		       match_number++)
		    {
		      rtx set, p, src, dst, src_subreg;
		      rtx post_inc = 0, post_inc_set = 0, search_end = 0;
		      rtx src_note, dst_note;
		      int success = 0;
		      int num_calls = 0;
		      enum rtx_code code = NOTE;
		      HOST_WIDE_INT insn_const, newconst;
		      rtx overlap = 0; /* need to move insn ? */
    
		      /* Nothing to do if the two operands aren't supposed to
			 match.  */
		      if (matches[operand_number][match_number] == 0)
			continue;
    
		      src = recog_operand[operand_number];
		      dst = recog_operand[match_number];
    
		      if (GET_CODE (src) != REG
			  || REGNO (src) < FIRST_PSEUDO_REGISTER)
			continue;
    
		      src_subreg = src;
		      if (GET_CODE (dst) == SUBREG
			  && GET_MODE_SIZE (GET_MODE (dst))
			     >= GET_MODE_SIZE (GET_MODE (SUBREG_REG (dst))))
			{
			  src_subreg
			    = gen_rtx(SUBREG,  GET_MODE (SUBREG_REG (dst)),
				      src, SUBREG_WORD (dst));
			  dst = SUBREG_REG (dst);
			}
		      if (GET_CODE (dst) != REG
			  || REGNO (dst) < FIRST_PSEUDO_REGISTER)
			continue;
    
		      /* If the operands already match, then there is nothing
			 to do.  */
		      if (operands_match_p (src, dst))
			continue;
    
		      set = single_set (insn);
		      if (! set)
			continue;
    
		      /* operand_number/src must be a read-only operand, and
			 match_operand/dst must be a write-only operand.  */
		      if (modified[match_number] != 2)
			continue;
    
		      if (early_clobber[match_number] == 1)
			continue;
    
		      if (modified[operand_number] != 0)
			continue;
    
		      /* Make sure match_operand is the destination.  */
		      if (recog_operand[match_number] != SET_DEST (set))
			continue;
		  
		      src_note = find_reg_note (insn, REG_DEAD, src);
    
		      if (! src_note)
			{
			  /* Look for (set (regX) (op regA constX))
				      (set (regY) (op regA constY))
			     and change that to
				      (set (regA) (op regA constX)).
				      (set (regY) (op regA constY-constX)).
			     This works for add and shift operations, if
			     regA is dead after or set by the second insn.  */

			  code = GET_CODE (SET_SRC (set));
			  if ((code == PLUS || code == LSHIFTRT
			       || code == ASHIFT || code == ASHIFTRT)
			      && XEXP (SET_SRC (set), 0) == src
			      && (GET_CODE (XEXP (SET_SRC (set), 1))
				  == CONST_INT))
			    insn_const = INTVAL (XEXP (SET_SRC (set), 1));
			  else if (! stable_but_for_p (SET_SRC (set), src, dst))
			    continue;
			  else
			    /* We might find a src_note while scanning.  */
			    code = NOTE;
			}

		      if (regmove_dump_file)
			fprintf (regmove_dump_file,
				 "Could fix operand %d of insn %d matching operand %d.\n",
				 operand_number, INSN_UID (insn), match_number);
    
		      /* ??? If src is set once, and is set equal to a
			 constant, then do not use it for this optimization,
			 as this would make it no longer equivalent to a
			 constant?  */
    
		      /* Scan forward to find the next instruction that
			 uses the output operand.  If the operand dies here,
			 then replace it in both instructions with
			 operand_number.  */
    
		      for (p = NEXT_INSN (insn); p; p = NEXT_INSN (p))
			{
			  if (GET_CODE (p) == CODE_LABEL
			      || GET_CODE (p) == JUMP_INSN
			      || (GET_CODE (p) == NOTE
				  && ((NOTE_LINE_NUMBER (p)
				       == NOTE_INSN_LOOP_BEG)
				      || (NOTE_LINE_NUMBER (p)
					  == NOTE_INSN_LOOP_END))))
			    break;
    
			  if (GET_RTX_CLASS (GET_CODE (p)) != 'i')
			    continue;
    
			  if (reg_set_p (src, p) || reg_set_p (dst, p)
			      || (GET_CODE (PATTERN (p)) == USE
				  && reg_overlap_mentioned_p (src,
							      XEXP (PATTERN (p),
							      0))))
			    break;
    
			  /* See if all of DST dies in P.  This test is
			     slightly more conservative than it needs to be.  */
			  if ((dst_note
				= find_regno_note (p, REG_DEAD, REGNO (dst)))
			      && (GET_MODE (XEXP (dst_note, 0))
				  == GET_MODE (dst)))
			    {
			      if (! src_note)
				{
				  rtx q;
				  rtx set2;
    
				  /* If an optimization is done, the value
				     of SRC while P is executed will be
				     changed.  Check that this is OK.  */
				  if (reg_overlap_mentioned_p (src,
							       PATTERN (p)))
				    break;
				  for (q = p; q; q = NEXT_INSN (q))
				    {
				      if (GET_CODE (q) == CODE_LABEL
					  || GET_CODE (q) == JUMP_INSN
					  || (GET_CODE (q) == NOTE
					      && ((NOTE_LINE_NUMBER (q)
						   == NOTE_INSN_LOOP_BEG)
						  || (NOTE_LINE_NUMBER (q)
						      == NOTE_INSN_LOOP_END))))
					{
					  q = 0;
					  break;
					}
				      if (GET_RTX_CLASS (GET_CODE (q)) != 'i')
					continue;
				      if (reg_overlap_mentioned_p (src,
								   PATTERN (q))
					  || reg_set_p (src, q))
					break;
				    }
				  if (q)
				    set2 = single_set (q);
				  if (! q || ! set2
				      || GET_CODE (SET_SRC (set2)) != code
				      || XEXP (SET_SRC (set2), 0) != src
				      || (GET_CODE (XEXP (SET_SRC (set2), 1))
					  != CONST_INT)
				      || (SET_DEST (set2) != src
					  && !find_reg_note (q, REG_DEAD, src)))
				    {
				      /* If this is a PLUS, we can still save
					 a register by doing
					 src += insn_const;
					 P;
					 src -= insn_const; .
					 This also gives opportunities for
					 subsequent optimizations in the
					 backward pass, so do it there.  */
				      if (code == PLUS && pass == 1
#ifdef HAVE_cc0
					  /* We man not emit an insn directly
					     after P if the latter sets CC0.  */
					  && ! sets_cc0_p (PATTERN (p))
#endif
					  )

					{
					  search_end = q;
					  q = insn;
					  set2 = set;
					  newconst = -insn_const;
					  code = MINUS;
					}
				      else
					break;
				    }
				  else
				    {
				      newconst
					= (INTVAL (XEXP (SET_SRC (set2), 1))
					   - insn_const);
				      /* Reject out of range shifts.  */
				      if (code != PLUS
					  && (newconst < 0
					      || (newconst
						  >= GET_MODE_BITSIZE (GET_MODE (SET_SRC (set2))))))
					break;
				      if (code == PLUS)
					{
					  post_inc = q;
					  if (SET_DEST (set2) != src)
					    post_inc_set = set2;
					}
				    }
				  /* We use 1 as last argument to
				     validate_change so that all changes
				     are accepted or rejected together by
				     apply_change_group when it is called
				     by validate_replace_rtx .  */
				  validate_change (q, &XEXP (SET_SRC (set2), 1),
						   GEN_INT (newconst), 1);
				}
			      validate_change (insn,
					       recog_operand_loc[match_number],
					       src, 1);
			      if (validate_replace_rtx (dst, src_subreg, p))
				success = 1;
			      break;
			    }
    
			  if (reg_overlap_mentioned_p (dst, PATTERN (p)))
			    break;
			  if (! src_note
			      && reg_overlap_mentioned_p (src, PATTERN (p)))
			    {
			      /* INSN was already checked to be movable when
				 we found no REG_DEAD note for src on it.  */
			      overlap = p;
			      src_note = find_reg_note (p, REG_DEAD, src);
			    }
    
			  /* If we have passed a call instruction, and the
			     pseudo-reg SRC is not already live across a call,
			     then don't perform the optimization.  */
			  if (GET_CODE (p) == CALL_INSN)
			    {
			      num_calls++;
    
			      if (REG_N_CALLS_CROSSED (REGNO (src)) == 0)
				break;
			    }
			}
    
		      if (success)
			{
			  /* Remove the death note for DST from P.  */
			  remove_note (p, dst_note);
			  if (code == MINUS)
			    {
			      post_inc
				= emit_insn_after (copy_rtx (PATTERN (insn)),
						   p);
#if defined (HAVE_PRE_INCREMENT) || defined (HAVE_PRE_DECREMENT)
			      if (search_end
				  && try_auto_increment (search_end, post_inc,
							 0, src, newconst, 1))
				post_inc = 0;
#endif
			      validate_change (insn, &XEXP (SET_SRC (set), 1),
					       GEN_INT (insn_const), 0);
			      REG_N_SETS (REGNO (src))++;
			    }
			  if (overlap)
			    {
			      /* The lifetime of src and dest overlap,
				 but we can change this by moving insn.  */
			      rtx pat = PATTERN (insn);
			      if (src_note)
				remove_note (overlap, src_note);
#if defined (HAVE_POST_INCREMENT) || defined (HAVE_POST_DECREMENT)
			      if (code == PLUS
				  && try_auto_increment (overlap, insn, 0,
							 src, insn_const, 0))
				insn = overlap;
			      else
#endif
				{
				  rtx notes = REG_NOTES (insn);

				  emit_insn_after_with_line_notes
				    (pat, PREV_INSN (p), insn);
				  PUT_CODE (insn, NOTE);
				  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
				  NOTE_SOURCE_FILE (insn) = 0;
				  /* emit_insn_after_with_line_notes
				     has no return value, so search
				     for the new insn.  */
				  for (insn = p; PATTERN (insn) != pat; )
				    insn = PREV_INSN (insn);

				  REG_NOTES (insn) = notes;
				}
			    }
			  /* Sometimes we'd generate src = const; src += n;
			     if so, replace the instruction that set src
			     in the first place.  */
			
			  if (! overlap && (code == PLUS || code == MINUS))
			    {
			      rtx note
				= find_reg_note (insn, REG_EQUAL, NULL_RTX);
			      rtx q, set2;
			      int num_calls2 = 0;

			      if (note && CONSTANT_P (XEXP (note, 0)))
				{
				  for (q = PREV_INSN (insn); q;
				       q = PREV_INSN(q))
				    {
				      if (GET_CODE (q) == JUMP_INSN)
					{
					  q = 0;
					  break;
					}
				      if (GET_RTX_CLASS (GET_CODE (q)) != 'i')
					continue;
				      if (reg_set_p (src, q))
					{
					  set2 = single_set (q);
					  break;
					}
				      if (reg_overlap_mentioned_p (src,
					  PATTERN (q)))
					{
					  q = 0;
					  break;
					}
				      if (GET_CODE (p) == CALL_INSN)
					num_calls2++;
				    }
				  if (q && set2 && SET_DEST (set2) == src
				      && CONSTANT_P (SET_SRC (set2))
				      && validate_change (insn, &SET_SRC (set),
							  XEXP (note, 0), 0))
				    {
				      PUT_CODE (q, NOTE);
				      NOTE_LINE_NUMBER (q) = NOTE_INSN_DELETED;
				      NOTE_SOURCE_FILE (q) = 0;
				      REG_N_SETS (REGNO (src))--;
				      REG_N_CALLS_CROSSED (REGNO (src))
					-= num_calls2;
				      insn_const = 0;
				    }
				}
			    }
			  if (0) ;
#if defined (HAVE_PRE_INCREMENT) || defined (HAVE_PRE_DECREMENT)
			  else if ((code == PLUS || code == MINUS)
				   && insn_const
				   && try_auto_increment (p, insn, 0,
							  src, insn_const, 1))
			    insn = p;
#endif
#if defined (HAVE_POST_INCREMENT) || defined (HAVE_POST_DECREMENT)
			  else if (post_inc
				   && try_auto_increment (p, post_inc,
							  post_inc_set, src,
							  newconst, 0))
			    post_inc = 0;
#endif
#if defined (HAVE_PRE_INCREMENT) || defined (HAVE_PRE_DECREMENT)
			  /* If post_inc still prevails, try to find an
			     insn where it can be used as a pre-in/decrement.
			     If code is MINUS, this was already tried.  */
			  if (post_inc && code == PLUS
			  /* Check that newconst is likely to be usable
			     in a pre-in/decrement before starting the
			     search.  */
			      && (0
#if defined (HAVE_PRE_INCREMENT)
				  || (newconst > 0 && newconst <= MOVE_MAX)
#endif
#if defined (HAVE_PRE_DECREMENT)
				  || (newconst < 0 && newconst >= -MOVE_MAX)
#endif
				 ) && exact_log2 (newconst))
			    {
			      rtx q, inc_dest;

			      inc_dest
				= post_inc_set ? SET_DEST (post_inc_set) : src;
			      for (q = post_inc; q = NEXT_INSN (q); )
				{
				  if (GET_CODE (q) == CODE_LABEL
				      || GET_CODE (q) == JUMP_INSN
				      || (GET_CODE (q) == NOTE
					  && ((NOTE_LINE_NUMBER (q)
					       == NOTE_INSN_LOOP_BEG)
					      || (NOTE_LINE_NUMBER (q)
						  == NOTE_INSN_LOOP_END))))
				    break;
				  if (GET_RTX_CLASS (GET_CODE (q)) != 'i')
				    continue;
				  if (src != inc_dest
				      && (reg_overlap_mentioned_p (src,
								   PATTERN (q))
					  || reg_set_p (src, q)))
				    break;
				  if (reg_set_p (inc_dest, q))
				    break;
				  if (reg_overlap_mentioned_p (inc_dest,
							       PATTERN (q)))
				    {
				      try_auto_increment (q, post_inc,
							  post_inc_set,
							  inc_dest,
							  newconst, 1);
				      break;
				    }
				}
			    }
#endif /* defined (HAVE_PRE_INCREMENT) || defined (HAVE_PRE_DECREMENT) */
			  /* Move the death note for DST to INSN if it is used
			     there.  */
			  if (reg_overlap_mentioned_p (dst, PATTERN (insn)))
			    {
			      XEXP (dst_note, 1) = REG_NOTES (insn);
			      REG_NOTES (insn) = dst_note;
			    }
    
			  if (src_note)
			    {
			      /* Move the death note for SRC from INSN to P.  */
			      if (! overlap)
				remove_note (insn, src_note);
			      XEXP (src_note, 1) = REG_NOTES (p);
			      REG_NOTES (p) = src_note;
    
			      REG_N_CALLS_CROSSED (REGNO (src)) += num_calls;
			    }
    
			  REG_N_SETS (REGNO (src))++;
			  REG_N_SETS (REGNO (dst))--;
    
			  REG_N_CALLS_CROSSED (REGNO (dst)) -= num_calls;
    
			  /* ??? Must adjust reg_live_length, and reg_n_refs for
			     both registers.  Must keep track of loop_depth in
			     order to get reg_n_refs adjustment correct.  */
    
			  if (regmove_dump_file)
			    fprintf (regmove_dump_file,
				     "Fixed operand %d of insn %d matching operand %d.\n",
				     operand_number, INSN_UID (insn),
				     match_number);
    
			  goto done_forwards;
			}
		    }
		}
	    done_forwards:
	      ;
	    }
	}
    }

  /* A backward pass.  Replace input operands with output operands.  */

  if (regmove_dump_file)
    fprintf (regmove_dump_file, "Starting backward pass...\n");

  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  int insn_code_number = recog_memoized (insn);
	  int operand_number, match_number;
	  
	  if (insn_code_number < 0)
	    continue;

	  insn_extract (insn);
	  if (! constrain_operands (insn_code_number, 0))
	    continue;
	  
	  commutative = -1;

	  /* Must initialize this before the loop, because the code for
	     the commutative case may set matches for operands other than
	     the current one.  */
	  bzero ((char *) matches, sizeof (matches));

	  for (operand_number = 0;
	       operand_number < insn_n_operands[insn_code_number];
	       operand_number++)
	    {
	      int output_operand = 0;
	      int matching_operand = operand_number;
	      char *p, c;
	      int i = 0;

	      modified[operand_number] = 0;
	      early_clobber[operand_number] = 0;

	      p = insn_operand_constraint[insn_code_number][operand_number];

	      if (*p == '=')
		modified[operand_number] = 2;
	      else if (*p == '+')
		modified[operand_number] = 1;

	      for (; *p && i < which_alternative; p++)
		if (*p == ',')
		  i++;

	      while ((c = *p++) != '\0' && c != ',')
		switch (c)
		  {
		  case '=':
		    break;
		  case '+':
		    break;
		  case '&':
		    early_clobber[operand_number] = 1;
		    break;
		  case '%':
		    commutative = operand_number;
		    break;
		  case '0': case '1': case '2': case '3': case '4':
		  case '5': case '6': case '7': case '8': case '9':
		    c -= '0';
		    matches[c][operand_number] = 1;
		    if (commutative >= 0)
		      {
			if (c == commutative || c == commutative + 1)
			  {
			    int other = c + (c == commutative ? 1 : -1);
			    matches[other][operand_number] = 1;
			  }
			if (operand_number == commutative
			    || operand_number == commutative + 1)
			  {
			    int other = (operand_number
					 + (operand_number == commutative
					    ? 1 : -1));
			    matches[c][other] = 1;
			  }
		      }
		    break;
		  }
	    }

	  /* Now scan through the operands looking for a destination operand
	     which is supposed to match a source operand.
	     Then scan backward for an instruction which sets the source
	     operand.  If safe, then replace the source operand with the
	     dest operand in both instructions.  */

	  for (operand_number = 0;
	       operand_number < insn_n_operands[insn_code_number];
	       operand_number++)
	    {
	      for (match_number = 0;
		   match_number < insn_n_operands[insn_code_number];
		   match_number++)
		{
		  rtx set, p, src, dst;
		  rtx src_note, dst_note;
		  int success = 0;
		  int num_calls = 0;

		  /* Nothing to do if the two operands aren't supposed to
		     match.  */
		  if (matches[operand_number][match_number] == 0)
		    continue;

		  dst = recog_operand[operand_number];
		  src = recog_operand[match_number];

		  if (GET_CODE (src) != REG
		      || REGNO (src) < FIRST_PSEUDO_REGISTER)
		    continue;

		  if (GET_CODE (dst) != REG
		      || REGNO (dst) < FIRST_PSEUDO_REGISTER)
		    continue;

		  /* If the operands already match, then there is nothing
		     to do.  */
		  if (operands_match_p (src, dst))
		    continue;

		  set = single_set (insn);
		  if (! set)
		    continue;

		  /* operand_number/dst must be a write-only operand, and
		     match_operand/src must be a read-only operand.  */
		  if (modified[match_number] != 0)
		    continue;

		  if (early_clobber[operand_number] == 1)
		    continue;

		  if (modified[operand_number] != 2)
		    continue;

		  /* Make sure operand_number is the destination.  */
		  if (recog_operand[operand_number] != SET_DEST (set))
		    continue;
	      
		  if (! (src_note = find_reg_note (insn, REG_DEAD, src)))
		    continue;

		  /* Can not modify an earlier insn to set dst if this insn
		     uses an old value in the source.  */
		  if (reg_overlap_mentioned_p (dst, SET_SRC (set)))
		    continue;

		  if (regmove_dump_file)
		    fprintf (regmove_dump_file,
			     "Could fix operand %d of insn %d matching operand %d.\n",
			     operand_number, INSN_UID (insn), match_number);

		  /* ??? If src is set once, and is set equal to a constant,
		     then do not use it for this optimization, as this would
		     make it no longer equivalent to a constant?  */

		  /* Scan backward to find the first instruction that uses
		     the input operand.  If the operand is set here, then
		     replace it in both instructions with operand_number.  */

		  for (p = PREV_INSN (insn); p; p = PREV_INSN (p))
		    {
		      rtx pset;

		      if (GET_CODE (p) == CODE_LABEL
			  || GET_CODE (p) == JUMP_INSN
			  || (GET_CODE (p) == NOTE
			      && (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_BEG
				  || NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_END)))
			break;

		      if (GET_RTX_CLASS (GET_CODE (p)) != 'i')
			continue;

		      /* ??? See if all of SRC is set in P.  This test is much
			 more conservative than it needs to be.  */
		      pset = single_set (p);
		      if (pset && SET_DEST (pset) == src)
			{
			  /* We use validate_replace_rtx, in case there
			     are multiple identical source operands.  All of
			     them have to be changed at the same time.  */
			  if (validate_replace_rtx (src, dst, insn))
			    {
			      if (validate_change (p, &SET_DEST (pset),
						   dst, 0))
				success = 1;
			      else
				{
				  /* Change all source operands back.
				     This modifies the dst as a side-effect.  */
				  validate_replace_rtx (dst, src, insn);
				  /* Now make sure the dst is right.  */
				  validate_change (insn,
						   recog_operand_loc[operand_number],
						   dst, 0);
				}
			    }
			  break;
			}

		      if (reg_overlap_mentioned_p (src, PATTERN (p))
			  || reg_overlap_mentioned_p (dst, PATTERN (p)))
			break;

		      /* If we have passed a call instruction, and the
			 pseudo-reg DST is not already live across a call,
			 then don't perform the optimization.  */
		      if (GET_CODE (p) == CALL_INSN)
			{
			  num_calls++;

			  if (REG_N_CALLS_CROSSED (REGNO (dst)) == 0)
			    break;
			}
		    }

		  if (success)
		    {
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
		      if (dst_note = find_reg_note (p, REG_DEAD, dst))
			remove_note (p, dst_note);

		      REG_N_SETS (REGNO (dst))++;
		      REG_N_SETS (REGNO (src))--;

		      REG_N_CALLS_CROSSED (REGNO (dst)) += num_calls;
		      REG_N_CALLS_CROSSED (REGNO (src)) -= num_calls;

		      /* ??? Must adjust reg_live_length, and reg_n_refs for
			 both registers.  Must keep track of loop_depth in
			 order to get reg_n_refs adjustment correct.  */

		      if (regmove_dump_file)
			fprintf (regmove_dump_file,
				 "Fixed operand %d of insn %d matching operand %d.\n",
				 operand_number, INSN_UID (insn), match_number);

		      goto done_backwards;
		    }
		}
	    }
	done_backwards:
	  ;
	}
    }
#endif /* REGISTER_CONSTRAINTS */
}

/* return nonzero if X is stable but for mentioning SRC or mentioning /
   changing DST .  If in doubt, presume it is unstable.  */
static int
stable_but_for_p (x, src, dst)
     rtx x, src, dst;
{
  RTX_CODE code = GET_CODE (x);
  switch (GET_RTX_CLASS (code))
    {
    case '<': case '1': case 'c': case '2': case 'b': case '3':
      {
	int i;
	char *fmt = GET_RTX_FORMAT (code);
	for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	  if (fmt[i] == 'e' && ! stable_but_for_p (XEXP (x, i), src, dst))
	      return 0;
	return 1;
      }
    case 'o':
      if (x == src || x == dst)
	return 1;
      /* fall through */
    default:
      return ! rtx_unstable_p (x);
    }
}
