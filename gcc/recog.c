/* Subroutines used by or related to instruction recognition.
   Copyright (C) 1987, 1988, 91-99, 2000 Free Software Foundation, Inc.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "recog.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "function.h"
#include "flags.h"
#include "real.h"
#include "toplev.h"
#include "basic-block.h"
#include "output.h"
#include "resource.h"

#ifndef STACK_PUSH_CODE
#ifdef STACK_GROWS_DOWNWARD
#define STACK_PUSH_CODE PRE_DEC
#else
#define STACK_PUSH_CODE PRE_INC
#endif
#endif

#ifndef STACK_POP_CODE
#ifdef STACK_GROWS_DOWNWARD
#define STACK_POP_CODE POST_INC
#else
#define STACK_POP_CODE POST_DEC
#endif
#endif

static void validate_replace_rtx_1	PARAMS ((rtx *, rtx, rtx, rtx));
static rtx *find_single_use_1		PARAMS ((rtx, rtx *));
static rtx *find_constant_term_loc	PARAMS ((rtx *));
static int insn_invalid_p		PARAMS ((rtx));

/* Nonzero means allow operands to be volatile.
   This should be 0 if you are generating rtl, such as if you are calling
   the functions in optabs.c and expmed.c (most of the time).
   This should be 1 if all valid insns need to be recognized,
   such as in regclass.c and final.c and reload.c.

   init_recog and init_recog_no_volatile are responsible for setting this.  */

int volatile_ok;

struct recog_data recog_data;

/* Contains a vector of operand_alternative structures for every operand.
   Set up by preprocess_constraints.  */
struct operand_alternative recog_op_alt[MAX_RECOG_OPERANDS][MAX_RECOG_ALTERNATIVES];

/* On return from `constrain_operands', indicate which alternative
   was satisfied.  */

int which_alternative;

/* Nonzero after end of reload pass.
   Set to 1 or 0 by toplev.c.
   Controls the significance of (SUBREG (MEM)).  */

int reload_completed;

/* Initialize data used by the function `recog'.
   This must be called once in the compilation of a function
   before any insn recognition may be done in the function.  */

void
init_recog_no_volatile ()
{
  volatile_ok = 0;
}

void
init_recog ()
{
  volatile_ok = 1;
}

/* Try recognizing the instruction INSN,
   and return the code number that results.
   Remember the code so that repeated calls do not
   need to spend the time for actual rerecognition.

   This function is the normal interface to instruction recognition.
   The automatically-generated function `recog' is normally called
   through this one.  (The only exception is in combine.c.)  */

int
recog_memoized (insn)
     rtx insn;
{
  if (INSN_CODE (insn) < 0)
    INSN_CODE (insn) = recog (PATTERN (insn), insn, NULL_PTR);
  return INSN_CODE (insn);
}

/* Check that X is an insn-body for an `asm' with operands
   and that the operands mentioned in it are legitimate.  */

int
check_asm_operands (x)
     rtx x;
{
  int noperands;
  rtx *operands;
  const char **constraints;
  int i;

  /* Post-reload, be more strict with things.  */
  if (reload_completed)
    {
      /* ??? Doh!  We've not got the wrapping insn.  Cook one up.  */
      extract_insn (make_insn_raw (x));
      constrain_operands (1);
      return which_alternative >= 0;
    }

  noperands = asm_noperands (x);
  if (noperands < 0)
    return 0;
  if (noperands == 0)
    return 1;

  operands = (rtx *) alloca (noperands * sizeof (rtx));
  constraints = (const char **) alloca (noperands * sizeof (char *));

  decode_asm_operands (x, operands, NULL_PTR, constraints, NULL_PTR);

  for (i = 0; i < noperands; i++)
    {
      const char *c = constraints[i];
      if (c[0] == '%')
	c++;
      if (ISDIGIT ((unsigned char)c[0]) && c[1] == '\0')
	c = constraints[c[0] - '0'];

      if (! asm_operand_ok (operands[i], c))
        return 0;
    }

  return 1;
}

/* Static data for the next two routines.  */

typedef struct change_t
{
  rtx object;
  int old_code;
  rtx *loc;
  rtx old;
} change_t;

static change_t *changes;
static int changes_allocated;

static int num_changes = 0;

/* Validate a proposed change to OBJECT.  LOC is the location in the rtl for
   at which NEW will be placed.  If OBJECT is zero, no validation is done,
   the change is simply made.

   Two types of objects are supported:  If OBJECT is a MEM, memory_address_p
   will be called with the address and mode as parameters.  If OBJECT is
   an INSN, CALL_INSN, or JUMP_INSN, the insn will be re-recognized with
   the change in place.

   IN_GROUP is non-zero if this is part of a group of changes that must be
   performed as a group.  In that case, the changes will be stored.  The
   function `apply_change_group' will validate and apply the changes.

   If IN_GROUP is zero, this is a single change.  Try to recognize the insn
   or validate the memory reference with the change applied.  If the result
   is not valid for the machine, suppress the change and return zero.
   Otherwise, perform the change and return 1.  */

int
validate_change (object, loc, new, in_group)
    rtx object;
    rtx *loc;
    rtx new;
    int in_group;
{
  rtx old = *loc;

  if (old == new || rtx_equal_p (old, new))
    return 1;

  if (in_group == 0 && num_changes != 0)
    abort ();

  *loc = new;

  /* Save the information describing this change.  */
  if (num_changes >= changes_allocated)
    {
      if (changes_allocated == 0)
	/* This value allows for repeated substitutions inside complex
	   indexed addresses, or changes in up to 5 insns.  */
	changes_allocated = MAX_RECOG_OPERANDS * 5;
      else
	changes_allocated *= 2;

      changes = 
	(change_t*) xrealloc (changes, 
			      sizeof (change_t) * changes_allocated); 
    }
  
  changes[num_changes].object = object;
  changes[num_changes].loc = loc;
  changes[num_changes].old = old;

  if (object && GET_CODE (object) != MEM)
    {
      /* Set INSN_CODE to force rerecognition of insn.  Save old code in
	 case invalid.  */
      changes[num_changes].old_code = INSN_CODE (object);
      INSN_CODE (object) = -1;
    }

  num_changes++;

  /* If we are making a group of changes, return 1.  Otherwise, validate the
     change group we made.  */

  if (in_group)
    return 1;
  else
    return apply_change_group ();
}

/* This subroutine of apply_change_group verifies whether the changes to INSN
   were valid; i.e. whether INSN can still be recognized.  */

static int
insn_invalid_p (insn)
     rtx insn;
{
  int icode = recog_memoized (insn);
  int is_asm = icode < 0 && asm_noperands (PATTERN (insn)) >= 0;

  if (is_asm && ! check_asm_operands (PATTERN (insn)))
    return 1;
  if (! is_asm && icode < 0)
    return 1;

  /* After reload, verify that all constraints are satisfied.  */
  if (reload_completed)
    {
      extract_insn (insn);

      if (! constrain_operands (1))
	return 1;
    }

  return 0;
}

/* Apply a group of changes previously issued with `validate_change'.
   Return 1 if all changes are valid, zero otherwise.  */

int
apply_change_group ()
{
  int i;

  /* The changes have been applied and all INSN_CODEs have been reset to force
     rerecognition.

     The changes are valid if we aren't given an object, or if we are
     given a MEM and it still is a valid address, or if this is in insn
     and it is recognized.  In the latter case, if reload has completed,
     we also require that the operands meet the constraints for
     the insn.  */

  for (i = 0; i < num_changes; i++)
    {
      rtx object = changes[i].object;

      if (object == 0)
	continue;

      if (GET_CODE (object) == MEM)
	{
	  if (! memory_address_p (GET_MODE (object), XEXP (object, 0)))
	    break;
	}
      else if (insn_invalid_p (object))
	{
	  rtx pat = PATTERN (object);

	  /* Perhaps we couldn't recognize the insn because there were
	     extra CLOBBERs at the end.  If so, try to re-recognize
	     without the last CLOBBER (later iterations will cause each of
	     them to be eliminated, in turn).  But don't do this if we
	     have an ASM_OPERAND.  */
	  if (GET_CODE (pat) == PARALLEL
	      && GET_CODE (XVECEXP (pat, 0, XVECLEN (pat, 0) - 1)) == CLOBBER
	      && asm_noperands (PATTERN (object)) < 0)
	    {
	       rtx newpat;

	       if (XVECLEN (pat, 0) == 2)
		 newpat = XVECEXP (pat, 0, 0);
	       else
		 {
		   int j;

		   newpat
		     = gen_rtx_PARALLEL (VOIDmode, 
					 gen_rtvec (XVECLEN (pat, 0) - 1));
		   for (j = 0; j < XVECLEN (newpat, 0); j++)
		     XVECEXP (newpat, 0, j) = XVECEXP (pat, 0, j);
		 }

	       /* Add a new change to this group to replace the pattern
		  with this new pattern.  Then consider this change
		  as having succeeded.  The change we added will
		  cause the entire call to fail if things remain invalid.

		  Note that this can lose if a later change than the one
		  we are processing specified &XVECEXP (PATTERN (object), 0, X)
		  but this shouldn't occur.  */

	       validate_change (object, &PATTERN (object), newpat, 1);
	     }
	  else if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER)
	    /* If this insn is a CLOBBER or USE, it is always valid, but is
	       never recognized.  */
	    continue;
	  else
	    break;
	}
    }

  if (i == num_changes)
    {
      num_changes = 0;
      return 1;
    }
  else
    {
      cancel_changes (0);
      return 0;
    }
}

/* Return the number of changes so far in the current group.   */

int
num_validated_changes ()
{
  return num_changes;
}

/* Retract the changes numbered NUM and up.  */

void
cancel_changes (num)
     int num;
{
  int i;

  /* Back out all the changes.  Do this in the opposite order in which
     they were made.  */
  for (i = num_changes - 1; i >= num; i--)
    {
      *changes[i].loc = changes[i].old;
      if (changes[i].object && GET_CODE (changes[i].object) != MEM)
	INSN_CODE (changes[i].object) = changes[i].old_code;
    }
  num_changes = num;
}

/* Replace every occurrence of FROM in X with TO.  Mark each change with
   validate_change passing OBJECT.  */

static void
validate_replace_rtx_1 (loc, from, to, object)
     rtx *loc;
     rtx from, to, object;
{
  register int i, j;
  register const char *fmt;
  register rtx x = *loc;
  enum rtx_code code = GET_CODE (x);

  /* X matches FROM if it is the same rtx or they are both referring to the
     same register in the same mode.  Avoid calling rtx_equal_p unless the
     operands look similar.  */

  if (x == from
      || (GET_CODE (x) == REG && GET_CODE (from) == REG
	  && GET_MODE (x) == GET_MODE (from)
	  && REGNO (x) == REGNO (from))
      || (GET_CODE (x) == GET_CODE (from) && GET_MODE (x) == GET_MODE (from)
	  && rtx_equal_p (x, from)))
    {
      validate_change (object, loc, to, 1);
      return;
    }

  /* For commutative or comparison operations, try replacing each argument
     separately and seeing if we made any changes.  If so, put a constant
     argument last.*/
  if (GET_RTX_CLASS (code) == '<' || GET_RTX_CLASS (code) == 'c')
    {
      int prev_changes = num_changes;

      validate_replace_rtx_1 (&XEXP (x, 0), from, to, object);
      validate_replace_rtx_1 (&XEXP (x, 1), from, to, object);
      if (prev_changes != num_changes && CONSTANT_P (XEXP (x, 0)))
	{
	  validate_change (object, loc,
			   gen_rtx_fmt_ee (GET_RTX_CLASS (code) == 'c' ? code
					   : swap_condition (code),
					   GET_MODE (x), XEXP (x, 1),
					   XEXP (x, 0)),
			   1);
	  x = *loc;
	  code = GET_CODE (x);
	}
    }

  /* Note that if CODE's RTX_CLASS is "c" or "<" we will have already
     done the substitution, otherwise we won't.  */

  switch (code)
    {
    case PLUS:
      /* If we have a PLUS whose second operand is now a CONST_INT, use
	 plus_constant to try to simplify it.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT && XEXP (x, 1) == to)
	validate_change (object, loc, plus_constant (XEXP (x, 0), INTVAL (to)),
			 1);
      return;

    case MINUS:
      if (GET_CODE (to) == CONST_INT && XEXP (x, 1) == from)
	{
	  validate_change (object, loc,
			   plus_constant (XEXP (x, 0), - INTVAL (to)),
			   1);
	  return;
	}
      break;
      
    case ZERO_EXTEND:
    case SIGN_EXTEND:
      /* In these cases, the operation to be performed depends on the mode
	 of the operand.  If we are replacing the operand with a VOIDmode
	 constant, we lose the information.  So try to simplify the operation
	 in that case.  If it fails, substitute in something that we know
	 won't be recognized.  */
      if (GET_MODE (to) == VOIDmode
	  && (XEXP (x, 0) == from
	      || (GET_CODE (XEXP (x, 0)) == REG && GET_CODE (from) == REG
		  && GET_MODE (XEXP (x, 0)) == GET_MODE (from)
		  && REGNO (XEXP (x, 0)) == REGNO (from))))
	{
	  rtx new = simplify_unary_operation (code, GET_MODE (x), to,
					      GET_MODE (from));
	  if (new == 0)
	    new = gen_rtx_CLOBBER (GET_MODE (x), const0_rtx);

	  validate_change (object, loc, new, 1);
	  return;
	}
      break;
	
    case SUBREG:
      /* If we have a SUBREG of a register that we are replacing and we are
	 replacing it with a MEM, make a new MEM and try replacing the
	 SUBREG with it.  Don't do this if the MEM has a mode-dependent address
	 or if we would be widening it.  */

      if (SUBREG_REG (x) == from
	  && GET_CODE (from) == REG
	  && GET_CODE (to) == MEM
	  && ! mode_dependent_address_p (XEXP (to, 0))
	  && ! MEM_VOLATILE_P (to)
	  && GET_MODE_SIZE (GET_MODE (x)) <= GET_MODE_SIZE (GET_MODE (to)))
	{
	  int offset = SUBREG_WORD (x) * UNITS_PER_WORD;
	  enum machine_mode mode = GET_MODE (x);
	  rtx new;

	  if (BYTES_BIG_ENDIAN)
	    offset += (MIN (UNITS_PER_WORD,
			    GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
		       - MIN (UNITS_PER_WORD, GET_MODE_SIZE (mode)));

	  new = gen_rtx_MEM (mode, plus_constant (XEXP (to, 0), offset));
	  RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (to);
	  MEM_COPY_ATTRIBUTES (new, to);
	  validate_change (object, loc, new, 1);
	  return;
	}
      break;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      /* If we are replacing a register with memory, try to change the memory
	 to be the mode required for memory in extract operations (this isn't
	 likely to be an insertion operation; if it was, nothing bad will
	 happen, we might just fail in some cases).  */

      if (XEXP (x, 0) == from && GET_CODE (from) == REG && GET_CODE (to) == MEM
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && GET_CODE (XEXP (x, 2)) == CONST_INT
	  && ! mode_dependent_address_p (XEXP (to, 0))
	  && ! MEM_VOLATILE_P (to))
	{
	  enum machine_mode wanted_mode = VOIDmode;
	  enum machine_mode is_mode = GET_MODE (to);
	  int pos = INTVAL (XEXP (x, 2));

#ifdef HAVE_extzv
	  if (code == ZERO_EXTRACT)
	    {
	      wanted_mode = insn_data[(int) CODE_FOR_extzv].operand[1].mode;
	      if (wanted_mode == VOIDmode)
		wanted_mode = word_mode;
	    }
#endif
#ifdef HAVE_extv
	  if (code == SIGN_EXTRACT)
	    {
	      wanted_mode = insn_data[(int) CODE_FOR_extv].operand[1].mode;
	      if (wanted_mode == VOIDmode)
		wanted_mode = word_mode;
	    }
#endif

	  /* If we have a narrower mode, we can do something.  */
	  if (wanted_mode != VOIDmode
	      && GET_MODE_SIZE (wanted_mode) < GET_MODE_SIZE (is_mode))
	    {
	      int offset = pos / BITS_PER_UNIT;
	      rtx newmem;

		  /* If the bytes and bits are counted differently, we
		     must adjust the offset.  */
	      if (BYTES_BIG_ENDIAN != BITS_BIG_ENDIAN)
		offset = (GET_MODE_SIZE (is_mode) - GET_MODE_SIZE (wanted_mode)
			  - offset);

	      pos %= GET_MODE_BITSIZE (wanted_mode);

	      newmem = gen_rtx_MEM (wanted_mode,
				    plus_constant (XEXP (to, 0), offset));
	      RTX_UNCHANGING_P (newmem) = RTX_UNCHANGING_P (to);
	      MEM_COPY_ATTRIBUTES (newmem, to);

	      validate_change (object, &XEXP (x, 2), GEN_INT (pos), 1);
	      validate_change (object, &XEXP (x, 0), newmem, 1);
	    }
	}

      break;
      
    default:
      break;
    }
      
  /* For commutative or comparison operations we've already performed
     replacements.  Don't try to perform them again.  */
  if (GET_RTX_CLASS (code) != '<' && GET_RTX_CLASS (code) != 'c')
    {
      fmt = GET_RTX_FORMAT (code);
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
        {
          if (fmt[i] == 'e')
            validate_replace_rtx_1 (&XEXP (x, i), from, to, object);
          else if (fmt[i] == 'E')
            for (j = XVECLEN (x, i) - 1; j >= 0; j--)
              validate_replace_rtx_1 (&XVECEXP (x, i, j), from, to, object);
        }
    }
}

/* Try replacing every occurrence of FROM in INSN with TO.  After all
   changes have been made, validate by seeing if INSN is still valid.  */

int
validate_replace_rtx (from, to, insn)
     rtx from, to, insn;
{
  validate_replace_rtx_1 (&PATTERN (insn), from, to, insn);
  return apply_change_group ();
}

/* Try replacing every occurrence of FROM in INSN with TO.  After all
   changes have been made, validate by seeing if INSN is still valid.  */

void
validate_replace_rtx_group (from, to, insn)
     rtx from, to, insn;
{
  validate_replace_rtx_1 (&PATTERN (insn), from, to, insn);
}

/* Try replacing every occurrence of FROM in INSN with TO, avoiding
   SET_DESTs.  After all changes have been made, validate by seeing if
   INSN is still valid.  */

int
validate_replace_src (from, to, insn)
     rtx from, to, insn;
{
  if ((GET_CODE (insn) != INSN && GET_CODE (insn) != JUMP_INSN)
      || GET_CODE (PATTERN (insn)) != SET)
    abort ();

  validate_replace_rtx_1 (&SET_SRC (PATTERN (insn)), from, to, insn);
  if (GET_CODE (SET_DEST (PATTERN (insn))) == MEM)
    validate_replace_rtx_1 (&XEXP (SET_DEST (PATTERN (insn)), 0),
			    from, to, insn);
  return apply_change_group ();
}

#ifdef HAVE_cc0
/* Return 1 if the insn using CC0 set by INSN does not contain
   any ordered tests applied to the condition codes.
   EQ and NE tests do not count.  */

int
next_insn_tests_no_inequality (insn)
     rtx insn;
{
  register rtx next = next_cc0_user (insn);

  /* If there is no next insn, we have to take the conservative choice.  */
  if (next == 0)
    return 0;

  return ((GET_CODE (next) == JUMP_INSN
	   || GET_CODE (next) == INSN
	   || GET_CODE (next) == CALL_INSN)
	  && ! inequality_comparisons_p (PATTERN (next)));
}

#if 0  /* This is useless since the insn that sets the cc's
	  must be followed immediately by the use of them.  */
/* Return 1 if the CC value set up by INSN is not used.  */

int
next_insns_test_no_inequality (insn)
     rtx insn;
{
  register rtx next = NEXT_INSN (insn);

  for (; next != 0; next = NEXT_INSN (next))
    {
      if (GET_CODE (next) == CODE_LABEL
	  || GET_CODE (next) == BARRIER)
	return 1;
      if (GET_CODE (next) == NOTE)
	continue;
      if (inequality_comparisons_p (PATTERN (next)))
	return 0;
      if (sets_cc0_p (PATTERN (next)) == 1)
	return 1;
      if (! reg_mentioned_p (cc0_rtx, PATTERN (next)))
	return 1;
    }
  return 1;
}
#endif
#endif

/* This is used by find_single_use to locate an rtx that contains exactly one
   use of DEST, which is typically either a REG or CC0.  It returns a
   pointer to the innermost rtx expression containing DEST.  Appearances of
   DEST that are being used to totally replace it are not counted.  */

static rtx *
find_single_use_1 (dest, loc)
     rtx dest;
     rtx *loc;
{
  rtx x = *loc;
  enum rtx_code code = GET_CODE (x);
  rtx *result = 0;
  rtx *this_result;
  int i;
  const char *fmt;

  switch (code)
    {
    case CONST_INT:
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_DOUBLE:
    case CLOBBER:
      return 0;

    case SET:
      /* If the destination is anything other than CC0, PC, a REG or a SUBREG
	 of a REG that occupies all of the REG, the insn uses DEST if
	 it is mentioned in the destination or the source.  Otherwise, we
	 need just check the source.  */
      if (GET_CODE (SET_DEST (x)) != CC0
	  && GET_CODE (SET_DEST (x)) != PC
	  && GET_CODE (SET_DEST (x)) != REG
	  && ! (GET_CODE (SET_DEST (x)) == SUBREG
		&& GET_CODE (SUBREG_REG (SET_DEST (x))) == REG
		&& (((GET_MODE_SIZE (GET_MODE (SUBREG_REG (SET_DEST (x))))
		      + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)
		    == ((GET_MODE_SIZE (GET_MODE (SET_DEST (x)))
			 + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD))))
	break;

      return find_single_use_1 (dest, &SET_SRC (x));

    case MEM:
    case SUBREG:
      return find_single_use_1 (dest, &XEXP (x, 0));
      
    default:
      break;
    }

  /* If it wasn't one of the common cases above, check each expression and
     vector of this code.  Look for a unique usage of DEST.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (dest == XEXP (x, i)
	      || (GET_CODE (dest) == REG && GET_CODE (XEXP (x, i)) == REG
		  && REGNO (dest) == REGNO (XEXP (x, i))))
	    this_result = loc;
	  else
	    this_result = find_single_use_1 (dest, &XEXP (x, i));

	  if (result == 0)
	    result = this_result;
	  else if (this_result)
	    /* Duplicate usage.  */
	    return 0;
	}
      else if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      if (XVECEXP (x, i, j) == dest
		  || (GET_CODE (dest) == REG
		      && GET_CODE (XVECEXP (x, i, j)) == REG
		      && REGNO (XVECEXP (x, i, j)) == REGNO (dest)))
		this_result = loc;
	      else
		this_result = find_single_use_1 (dest, &XVECEXP (x, i, j));

	      if (result == 0)
		result = this_result;
	      else if (this_result)
		return 0;
	    }
	}
    }

  return result;
}

/* See if DEST, produced in INSN, is used only a single time in the
   sequel.  If so, return a pointer to the innermost rtx expression in which
   it is used.

   If PLOC is non-zero, *PLOC is set to the insn containing the single use.

   This routine will return usually zero either before flow is called (because
   there will be no LOG_LINKS notes) or after reload (because the REG_DEAD
   note can't be trusted).

   If DEST is cc0_rtx, we look only at the next insn.  In that case, we don't
   care about REG_DEAD notes or LOG_LINKS.

   Otherwise, we find the single use by finding an insn that has a
   LOG_LINKS pointing at INSN and has a REG_DEAD note for DEST.  If DEST is
   only referenced once in that insn, we know that it must be the first
   and last insn referencing DEST.  */

rtx *
find_single_use (dest, insn, ploc)
     rtx dest;
     rtx insn;
     rtx *ploc;
{
  rtx next;
  rtx *result;
  rtx link;

#ifdef HAVE_cc0
  if (dest == cc0_rtx)
    {
      next = NEXT_INSN (insn);
      if (next == 0
	  || (GET_CODE (next) != INSN && GET_CODE (next) != JUMP_INSN))
	return 0;

      result = find_single_use_1 (dest, &PATTERN (next));
      if (result && ploc)
	*ploc = next;
      return result;
    }
#endif

  if (reload_completed || reload_in_progress || GET_CODE (dest) != REG)
    return 0;

  for (next = next_nonnote_insn (insn);
       next != 0 && GET_CODE (next) != CODE_LABEL;
       next = next_nonnote_insn (next))
    if (GET_RTX_CLASS (GET_CODE (next)) == 'i' && dead_or_set_p (next, dest))
      {
	for (link = LOG_LINKS (next); link; link = XEXP (link, 1))
	  if (XEXP (link, 0) == insn)
	    break;

	if (link)
	  {
	    result = find_single_use_1 (dest, &PATTERN (next));
	    if (ploc)
	      *ploc = next;
	    return result;
	  }
      }

  return 0;
}

/* Return 1 if OP is a valid general operand for machine mode MODE.
   This is either a register reference, a memory reference,
   or a constant.  In the case of a memory reference, the address
   is checked for general validity for the target machine.

   Register and memory references must have mode MODE in order to be valid,
   but some constants have no machine mode and are valid for any mode.

   If MODE is VOIDmode, OP is checked for validity for whatever mode
   it has.

   The main use of this function is as a predicate in match_operand
   expressions in the machine description.

   For an explanation of this function's behavior for registers of
   class NO_REGS, see the comment for `register_operand'.  */

int
general_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  register enum rtx_code code = GET_CODE (op);
  int mode_altering_drug = 0;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  /* Don't accept CONST_INT or anything similar
     if the caller wants something floating.  */
  if (GET_MODE (op) == VOIDmode && mode != VOIDmode
      && GET_MODE_CLASS (mode) != MODE_INT
      && GET_MODE_CLASS (mode) != MODE_PARTIAL_INT)
    return 0;

  if (CONSTANT_P (op))
    return ((GET_MODE (op) == VOIDmode || GET_MODE (op) == mode)
#ifdef LEGITIMATE_PIC_OPERAND_P
	    && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (op))
#endif
	    && LEGITIMATE_CONSTANT_P (op));

  /* Except for certain constants with VOIDmode, already checked for,
     OP's mode must match MODE if MODE specifies a mode.  */

  if (GET_MODE (op) != mode)
    return 0;

  if (code == SUBREG)
    {
#ifdef INSN_SCHEDULING
      /* On machines that have insn scheduling, we want all memory
	 reference to be explicit, so outlaw paradoxical SUBREGs.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM
	  && GET_MODE_SIZE (mode) > GET_MODE_SIZE (GET_MODE (SUBREG_REG (op))))
	return 0;
#endif

      op = SUBREG_REG (op);
      code = GET_CODE (op);
#if 0
      /* No longer needed, since (SUBREG (MEM...))
	 will load the MEM into a reload reg in the MEM's own mode.  */
      mode_altering_drug = 1;
#endif
    }

  if (code == REG)
    /* A register whose class is NO_REGS is not a general operand.  */
    return (REGNO (op) >= FIRST_PSEUDO_REGISTER
	    || REGNO_REG_CLASS (REGNO (op)) != NO_REGS);

  if (code == MEM)
    {
      register rtx y = XEXP (op, 0);
      if (! volatile_ok && MEM_VOLATILE_P (op))
	return 0;
      if (GET_CODE (y) == ADDRESSOF)
	return 1;
      /* Use the mem's mode, since it will be reloaded thus.  */
      mode = GET_MODE (op);
      GO_IF_LEGITIMATE_ADDRESS (mode, y, win);
    }

  /* Pretend this is an operand for now; we'll run force_operand
     on its replacement in fixup_var_refs_1.  */
  if (code == ADDRESSOF)
    return 1;

  return 0;

 win:
  if (mode_altering_drug)
    return ! mode_dependent_address_p (XEXP (op, 0));
  return 1;
}

/* Return 1 if OP is a valid memory address for a memory reference
   of mode MODE.

   The main use of this function is as a predicate in match_operand
   expressions in the machine description.  */

int
address_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return memory_address_p (mode, op);
}

/* Return 1 if OP is a register reference of mode MODE.
   If MODE is VOIDmode, accept a register in any mode.

   The main use of this function is as a predicate in match_operand
   expressions in the machine description.

   As a special exception, registers whose class is NO_REGS are
   not accepted by `register_operand'.  The reason for this change
   is to allow the representation of special architecture artifacts
   (such as a condition code register) without extending the rtl
   definitions.  Since registers of class NO_REGS cannot be used
   as registers in any case where register classes are examined,
   it is most consistent to keep this function from accepting them.  */

int
register_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    {
      /* Before reload, we can allow (SUBREG (MEM...)) as a register operand
	 because it is guaranteed to be reloaded into one.
	 Just make sure the MEM is valid in itself.
	 (Ideally, (SUBREG (MEM)...) should not exist after reload,
	 but currently it does result from (SUBREG (REG)...) where the
	 reg went on the stack.)  */
      if (! reload_completed && GET_CODE (SUBREG_REG (op)) == MEM)
	return general_operand (op, mode);

#ifdef CLASS_CANNOT_CHANGE_SIZE
      if (GET_CODE (SUBREG_REG (op)) == REG
	  && REGNO (SUBREG_REG (op)) < FIRST_PSEUDO_REGISTER
	  && TEST_HARD_REG_BIT (reg_class_contents[(int) CLASS_CANNOT_CHANGE_SIZE],
				REGNO (SUBREG_REG (op)))
	  && (GET_MODE_SIZE (mode)
	      != GET_MODE_SIZE (GET_MODE (SUBREG_REG (op))))
	  && GET_MODE_CLASS (GET_MODE (SUBREG_REG (op))) != MODE_COMPLEX_INT
	  && GET_MODE_CLASS (GET_MODE (SUBREG_REG (op))) != MODE_COMPLEX_FLOAT)
	return 0;
#endif

      op = SUBREG_REG (op);
    }

  /* If we have an ADDRESSOF, consider it valid since it will be
     converted into something that will not be a MEM. */
  if (GET_CODE (op) == ADDRESSOF)
    return 1;

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
}

/* Return 1 for a register in Pmode; ignore the tested mode.  */

int
pmode_register_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return register_operand (op, Pmode);
}

/* Return 1 if OP should match a MATCH_SCRATCH, i.e., if it is a SCRATCH
   or a hard register.  */

int
scratch_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  return (GET_CODE (op) == SCRATCH
	  || (GET_CODE (op) == REG
	      && REGNO (op) < FIRST_PSEUDO_REGISTER));
}

/* Return 1 if OP is a valid immediate operand for mode MODE.

   The main use of this function is as a predicate in match_operand
   expressions in the machine description.  */

int
immediate_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  /* Don't accept CONST_INT or anything similar
     if the caller wants something floating.  */
  if (GET_MODE (op) == VOIDmode && mode != VOIDmode
      && GET_MODE_CLASS (mode) != MODE_INT
      && GET_MODE_CLASS (mode) != MODE_PARTIAL_INT)
    return 0;

  /* Accept CONSTANT_P_RTX, since it will be gone by CSE1 and
     result in 0/1.  It seems a safe assumption that this is
     in range for everyone.  */
  if (GET_CODE (op) == CONSTANT_P_RTX)
    return 1;

  return (CONSTANT_P (op)
	  && (GET_MODE (op) == mode || mode == VOIDmode
	      || GET_MODE (op) == VOIDmode)
#ifdef LEGITIMATE_PIC_OPERAND_P
	  && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (op))
#endif
	  && LEGITIMATE_CONSTANT_P (op));
}

/* Returns 1 if OP is an operand that is a CONST_INT.  */

int
const_int_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == CONST_INT;
}

/* Returns 1 if OP is an operand that is a constant integer or constant
   floating-point number.  */

int
const_double_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  /* Don't accept CONST_INT or anything similar
     if the caller wants something floating.  */
  if (GET_MODE (op) == VOIDmode && mode != VOIDmode
      && GET_MODE_CLASS (mode) != MODE_INT
      && GET_MODE_CLASS (mode) != MODE_PARTIAL_INT)
    return 0;

  return ((GET_CODE (op) == CONST_DOUBLE || GET_CODE (op) == CONST_INT)
	  && (mode == VOIDmode || GET_MODE (op) == mode
	      || GET_MODE (op) == VOIDmode));
}

/* Return 1 if OP is a general operand that is not an immediate operand.  */

int
nonimmediate_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (general_operand (op, mode) && ! CONSTANT_P (op));
}

/* Return 1 if OP is a register reference or immediate value of mode MODE.  */

int
nonmemory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (CONSTANT_P (op))
    {
      /* Don't accept CONST_INT or anything similar
	 if the caller wants something floating.  */
      if (GET_MODE (op) == VOIDmode && mode != VOIDmode
	  && GET_MODE_CLASS (mode) != MODE_INT
	  && GET_MODE_CLASS (mode) != MODE_PARTIAL_INT)
	return 0;

      return ((GET_MODE (op) == VOIDmode || GET_MODE (op) == mode)
#ifdef LEGITIMATE_PIC_OPERAND_P
	      && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (op))
#endif
	      && LEGITIMATE_CONSTANT_P (op));
    }

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    {
      /* Before reload, we can allow (SUBREG (MEM...)) as a register operand
	 because it is guaranteed to be reloaded into one.
	 Just make sure the MEM is valid in itself.
	 (Ideally, (SUBREG (MEM)...) should not exist after reload,
	 but currently it does result from (SUBREG (REG)...) where the
	 reg went on the stack.)  */
      if (! reload_completed && GET_CODE (SUBREG_REG (op)) == MEM)
	return general_operand (op, mode);
      op = SUBREG_REG (op);
    }

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
}

/* Return 1 if OP is a valid operand that stands for pushing a
   value of mode MODE onto the stack.

   The main use of this function is as a predicate in match_operand
   expressions in the machine description.  */

int
push_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;

  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  op = XEXP (op, 0);

  if (GET_CODE (op) != STACK_PUSH_CODE)
    return 0;

  return XEXP (op, 0) == stack_pointer_rtx;
}

/* Return 1 if OP is a valid operand that stands for popping a
   value of mode MODE off the stack.

   The main use of this function is as a predicate in match_operand
   expressions in the machine description.  */

int
pop_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;

  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  op = XEXP (op, 0);

  if (GET_CODE (op) != STACK_POP_CODE)
    return 0;

  return XEXP (op, 0) == stack_pointer_rtx;
}

/* Return 1 if ADDR is a valid memory address for mode MODE.  */

int
memory_address_p (mode, addr)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     register rtx addr;
{
  if (GET_CODE (addr) == ADDRESSOF)
    return 1;
  
  GO_IF_LEGITIMATE_ADDRESS (mode, addr, win);
  return 0;

 win:
  return 1;
}

/* Return 1 if OP is a valid memory reference with mode MODE,
   including a valid address.

   The main use of this function is as a predicate in match_operand
   expressions in the machine description.  */

int
memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx inner;

  if (! reload_completed)
    /* Note that no SUBREG is a memory operand before end of reload pass,
       because (SUBREG (MEM...)) forces reloading into a register.  */
    return GET_CODE (op) == MEM && general_operand (op, mode);

  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  inner = op;
  if (GET_CODE (inner) == SUBREG)
    inner = SUBREG_REG (inner);

  return (GET_CODE (inner) == MEM && general_operand (op, mode));
}

/* Return 1 if OP is a valid indirect memory reference with mode MODE;
   that is, a memory reference whose address is a general_operand.  */

int
indirect_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  /* Before reload, a SUBREG isn't in memory (see memory_operand, above).  */
  if (! reload_completed
      && GET_CODE (op) == SUBREG && GET_CODE (SUBREG_REG (op)) == MEM)
    {
      register int offset = SUBREG_WORD (op) * UNITS_PER_WORD;
      rtx inner = SUBREG_REG (op);

      if (BYTES_BIG_ENDIAN)
	offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (op)))
		   - MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (inner))));

      if (mode != VOIDmode && GET_MODE (op) != mode)
	return 0;

      /* The only way that we can have a general_operand as the resulting
	 address is if OFFSET is zero and the address already is an operand
	 or if the address is (plus Y (const_int -OFFSET)) and Y is an
	 operand.  */

      return ((offset == 0 && general_operand (XEXP (inner, 0), Pmode))
	      || (GET_CODE (XEXP (inner, 0)) == PLUS
		  && GET_CODE (XEXP (XEXP (inner, 0), 1)) == CONST_INT
		  && INTVAL (XEXP (XEXP (inner, 0), 1)) == -offset
		  && general_operand (XEXP (XEXP (inner, 0), 0), Pmode)));
    }

  return (GET_CODE (op) == MEM
	  && memory_operand (op, mode)
	  && general_operand (XEXP (op, 0), Pmode));
}

/* Return 1 if this is a comparison operator.  This allows the use of
   MATCH_OPERATOR to recognize all the branch insns.  */

int
comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && GET_RTX_CLASS (GET_CODE (op)) == '<');
}

/* If BODY is an insn body that uses ASM_OPERANDS,
   return the number of operands (both input and output) in the insn.
   Otherwise return -1.  */

int
asm_noperands (body)
     rtx body;
{
  if (GET_CODE (body) == ASM_OPERANDS)
    /* No output operands: return number of input operands.  */
    return ASM_OPERANDS_INPUT_LENGTH (body);
  if (GET_CODE (body) == SET && GET_CODE (SET_SRC (body)) == ASM_OPERANDS)
    /* Single output operand: BODY is (set OUTPUT (asm_operands ...)).  */
    return ASM_OPERANDS_INPUT_LENGTH (SET_SRC (body)) + 1;
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == SET
	   && GET_CODE (SET_SRC (XVECEXP (body, 0, 0))) == ASM_OPERANDS)
    {
      /* Multiple output operands, or 1 output plus some clobbers:
	 body is [(set OUTPUT (asm_operands ...))... (clobber (reg ...))...].  */
      int i;
      int n_sets;

      /* Count backwards through CLOBBERs to determine number of SETs.  */
      for (i = XVECLEN (body, 0); i > 0; i--)
	{
	  if (GET_CODE (XVECEXP (body, 0, i - 1)) == SET)
	    break;
	  if (GET_CODE (XVECEXP (body, 0, i - 1)) != CLOBBER)
	    return -1;
	}

      /* N_SETS is now number of output operands.  */
      n_sets = i;

      /* Verify that all the SETs we have
	 came from a single original asm_operands insn
	 (so that invalid combinations are blocked).  */
      for (i = 0; i < n_sets; i++)
	{
	  rtx elt = XVECEXP (body, 0, i);
	  if (GET_CODE (elt) != SET)
	    return -1;
	  if (GET_CODE (SET_SRC (elt)) != ASM_OPERANDS)
	    return -1;
	  /* If these ASM_OPERANDS rtx's came from different original insns
	     then they aren't allowed together.  */
	  if (ASM_OPERANDS_INPUT_VEC (SET_SRC (elt))
	      != ASM_OPERANDS_INPUT_VEC (SET_SRC (XVECEXP (body, 0, 0))))
	    return -1;
	}
      return (ASM_OPERANDS_INPUT_LENGTH (SET_SRC (XVECEXP (body, 0, 0)))
	      + n_sets);
    }
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == ASM_OPERANDS)
    {
      /* 0 outputs, but some clobbers:
	 body is [(asm_operands ...) (clobber (reg ...))...].  */
      int i;

      /* Make sure all the other parallel things really are clobbers.  */
      for (i = XVECLEN (body, 0) - 1; i > 0; i--)
	if (GET_CODE (XVECEXP (body, 0, i)) != CLOBBER)
	  return -1;

      return ASM_OPERANDS_INPUT_LENGTH (XVECEXP (body, 0, 0));
    }
  else
    return -1;
}

/* Assuming BODY is an insn body that uses ASM_OPERANDS,
   copy its operands (both input and output) into the vector OPERANDS,
   the locations of the operands within the insn into the vector OPERAND_LOCS,
   and the constraints for the operands into CONSTRAINTS.
   Write the modes of the operands into MODES.
   Return the assembler-template.

   If MODES, OPERAND_LOCS, CONSTRAINTS or OPERANDS is 0,
   we don't store that info.  */

char *
decode_asm_operands (body, operands, operand_locs, constraints, modes)
     rtx body;
     rtx *operands;
     rtx **operand_locs;
     const char **constraints;
     enum machine_mode *modes;
{
  register int i;
  int noperands;
  char *template = 0;

  if (GET_CODE (body) == SET && GET_CODE (SET_SRC (body)) == ASM_OPERANDS)
    {
      rtx asmop = SET_SRC (body);
      /* Single output operand: BODY is (set OUTPUT (asm_operands ....)).  */

      noperands = ASM_OPERANDS_INPUT_LENGTH (asmop) + 1;

      for (i = 1; i < noperands; i++)
	{
	  if (operand_locs)
	    operand_locs[i] = &ASM_OPERANDS_INPUT (asmop, i - 1);
	  if (operands)
	    operands[i] = ASM_OPERANDS_INPUT (asmop, i - 1);
	  if (constraints)
	    constraints[i] = ASM_OPERANDS_INPUT_CONSTRAINT (asmop, i - 1);
	  if (modes)
	    modes[i] = ASM_OPERANDS_INPUT_MODE (asmop, i - 1);
	}

      /* The output is in the SET.
	 Its constraint is in the ASM_OPERANDS itself.  */
      if (operands)
	operands[0] = SET_DEST (body);
      if (operand_locs)
	operand_locs[0] = &SET_DEST (body);
      if (constraints)
	constraints[0] = ASM_OPERANDS_OUTPUT_CONSTRAINT (asmop);
      if (modes)
	modes[0] = GET_MODE (SET_DEST (body));
      template = ASM_OPERANDS_TEMPLATE (asmop);
    }
  else if (GET_CODE (body) == ASM_OPERANDS)
    {
      rtx asmop = body;
      /* No output operands: BODY is (asm_operands ....).  */

      noperands = ASM_OPERANDS_INPUT_LENGTH (asmop);

      /* The input operands are found in the 1st element vector.  */
      /* Constraints for inputs are in the 2nd element vector.  */
      for (i = 0; i < noperands; i++)
	{
	  if (operand_locs)
	    operand_locs[i] = &ASM_OPERANDS_INPUT (asmop, i);
	  if (operands)
	    operands[i] = ASM_OPERANDS_INPUT (asmop, i);
	  if (constraints)
	    constraints[i] = ASM_OPERANDS_INPUT_CONSTRAINT (asmop, i);
	  if (modes)
	    modes[i] = ASM_OPERANDS_INPUT_MODE (asmop, i);
	}
      template = ASM_OPERANDS_TEMPLATE (asmop);
    }
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == SET)
    {
      rtx asmop = SET_SRC (XVECEXP (body, 0, 0));
      int nparallel = XVECLEN (body, 0); /* Includes CLOBBERs.  */
      int nin = ASM_OPERANDS_INPUT_LENGTH (asmop);
      int nout = 0;		/* Does not include CLOBBERs.  */

      /* At least one output, plus some CLOBBERs.  */

      /* The outputs are in the SETs.
	 Their constraints are in the ASM_OPERANDS itself.  */
      for (i = 0; i < nparallel; i++)
	{
	  if (GET_CODE (XVECEXP (body, 0, i)) == CLOBBER)
	    break;		/* Past last SET */
	  
	  if (operands)
	    operands[i] = SET_DEST (XVECEXP (body, 0, i));
	  if (operand_locs)
	    operand_locs[i] = &SET_DEST (XVECEXP (body, 0, i));
	  if (constraints)
	    constraints[i] = XSTR (SET_SRC (XVECEXP (body, 0, i)), 1);
	  if (modes)
	    modes[i] = GET_MODE (SET_DEST (XVECEXP (body, 0, i)));
	  nout++;
	}

      for (i = 0; i < nin; i++)
	{
	  if (operand_locs)
	    operand_locs[i + nout] = &ASM_OPERANDS_INPUT (asmop, i);
	  if (operands)
	    operands[i + nout] = ASM_OPERANDS_INPUT (asmop, i);
	  if (constraints)
	    constraints[i + nout] = ASM_OPERANDS_INPUT_CONSTRAINT (asmop, i);
	  if (modes)
	    modes[i + nout] = ASM_OPERANDS_INPUT_MODE (asmop, i);
	}

      template = ASM_OPERANDS_TEMPLATE (asmop);
    }
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == ASM_OPERANDS)
    {
      /* No outputs, but some CLOBBERs.  */

      rtx asmop = XVECEXP (body, 0, 0);
      int nin = ASM_OPERANDS_INPUT_LENGTH (asmop);

      for (i = 0; i < nin; i++)
	{
	  if (operand_locs)
	    operand_locs[i] = &ASM_OPERANDS_INPUT (asmop, i);
	  if (operands)
	    operands[i] = ASM_OPERANDS_INPUT (asmop, i);
	  if (constraints)
	    constraints[i] = ASM_OPERANDS_INPUT_CONSTRAINT (asmop, i);
	  if (modes)
	    modes[i] = ASM_OPERANDS_INPUT_MODE (asmop, i);
	}

      template = ASM_OPERANDS_TEMPLATE (asmop);
    }

  return template;
}

/* Check if an asm_operand matches it's constraints. 
   Return > 0 if ok, = 0 if bad, < 0 if inconclusive.  */

int
asm_operand_ok (op, constraint)
     rtx op;
     const char *constraint;
{
  int result = 0;

  /* Use constrain_operands after reload.  */
  if (reload_completed)
    abort ();

  while (*constraint)
    {
      switch (*constraint++)
	{
	case '=':
	case '+':
	case '*':
	case '%':
	case '?':
	case '!':
	case '#':
	case '&':
	case ',':
	  break;

	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  /* For best results, our caller should have given us the
	     proper matching constraint, but we can't actually fail
	     the check if they didn't.  Indicate that results are
	     inconclusive.  */
	  result = -1;
	  break;

	case 'p':
	  if (address_operand (op, VOIDmode))
	    return 1;
	  break;

	case 'm':
	case 'V': /* non-offsettable */
	  if (memory_operand (op, VOIDmode))
	    return 1;
	  break;

	case 'o': /* offsettable */
	  if (offsettable_nonstrict_memref_p (op))
	    return 1;
	  break;

	case '<':
	  /* ??? Before flow, auto inc/dec insns are not supposed to exist,
	     excepting those that expand_call created.  Further, on some
	     machines which do not have generalized auto inc/dec, an inc/dec
	     is not a memory_operand.

	     Match any memory and hope things are resolved after reload.  */

	  if (GET_CODE (op) == MEM
	      && (1
		  || GET_CODE (XEXP (op, 0)) == PRE_DEC
                  || GET_CODE (XEXP (op, 0)) == POST_DEC))
	    return 1;
	  break;

	case '>':
	  if (GET_CODE (op) == MEM
	      && (1
		  || GET_CODE (XEXP (op, 0)) == PRE_INC
                  || GET_CODE (XEXP (op, 0)) == POST_INC))
	    return 1;
	  break;

	case 'E':
#ifndef REAL_ARITHMETIC
	  /* Match any floating double constant, but only if
	     we can examine the bits of it reliably.  */
	  if ((HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
	       || HOST_BITS_PER_WIDE_INT != BITS_PER_WORD)
	      && GET_MODE (op) != VOIDmode && ! flag_pretend_float)
	    break;
#endif
	  /* FALLTHRU */

	case 'F':
	  if (GET_CODE (op) == CONST_DOUBLE)
	    return 1;
	  break;

	case 'G':
	  if (GET_CODE (op) == CONST_DOUBLE
	      && CONST_DOUBLE_OK_FOR_LETTER_P (op, 'G'))
	    return 1;
	  break;
	case 'H':
	  if (GET_CODE (op) == CONST_DOUBLE
	      && CONST_DOUBLE_OK_FOR_LETTER_P (op, 'H'))
	    return 1;
	  break;

	case 's':
	  if (GET_CODE (op) == CONST_INT
	      || (GET_CODE (op) == CONST_DOUBLE
		  && GET_MODE (op) == VOIDmode))
	    break;
	  /* FALLTHRU */

	case 'i':
	  if (CONSTANT_P (op)
#ifdef LEGITIMATE_PIC_OPERAND_P
	      && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (op))
#endif
	      )
	    return 1;
	  break;

	case 'n':
	  if (GET_CODE (op) == CONST_INT
	      || (GET_CODE (op) == CONST_DOUBLE
		  && GET_MODE (op) == VOIDmode))
	    return 1;
	  break;

	case 'I':
	  if (GET_CODE (op) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (op), 'I'))
	    return 1;
	  break;
	case 'J':
	  if (GET_CODE (op) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (op), 'J'))
	    return 1;
	  break;
	case 'K':
	  if (GET_CODE (op) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (op), 'K'))
	    return 1;
	  break;
	case 'L':
	  if (GET_CODE (op) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (op), 'L'))
	    return 1;
	  break;
	case 'M':
	  if (GET_CODE (op) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (op), 'M'))
	    return 1;
	  break;
	case 'N':
	  if (GET_CODE (op) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (op), 'N'))
	    return 1;
	  break;
	case 'O':
	  if (GET_CODE (op) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (op), 'O'))
	    return 1;
	  break;
	case 'P':
	  if (GET_CODE (op) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (op), 'P'))
	    return 1;
	  break;

	case 'X':
	  return 1;

	case 'g':
	  if (general_operand (op, VOIDmode))
	    return 1;
	  break;

#ifdef EXTRA_CONSTRAINT
	case 'Q':
	  if (EXTRA_CONSTRAINT (op, 'Q'))
	    return 1;
	  break;
	case 'R':
	  if (EXTRA_CONSTRAINT (op, 'R'))
	    return 1;
	  break;
	case 'S':
	  if (EXTRA_CONSTRAINT (op, 'S'))
	    return 1;
	  break;
	case 'T':
	  if (EXTRA_CONSTRAINT (op, 'T'))
	    return 1;
	  break;
	case 'U':
	  if (EXTRA_CONSTRAINT (op, 'U'))
	    return 1;
	  break;
#endif

	case 'r':
	default:
	  if (GET_MODE (op) == BLKmode)
	    break;
	  if (register_operand (op, VOIDmode))
	    return 1;
	  break;
	}
    }

  return result;
}

/* Given an rtx *P, if it is a sum containing an integer constant term,
   return the location (type rtx *) of the pointer to that constant term.
   Otherwise, return a null pointer.  */

static rtx *
find_constant_term_loc (p)
     rtx *p;
{
  register rtx *tem;
  register enum rtx_code code = GET_CODE (*p);

  /* If *P IS such a constant term, P is its location.  */

  if (code == CONST_INT || code == SYMBOL_REF || code == LABEL_REF
      || code == CONST)
    return p;

  /* Otherwise, if not a sum, it has no constant term.  */

  if (GET_CODE (*p) != PLUS)
    return 0;

  /* If one of the summands is constant, return its location.  */

  if (XEXP (*p, 0) && CONSTANT_P (XEXP (*p, 0))
      && XEXP (*p, 1) && CONSTANT_P (XEXP (*p, 1)))
    return p;

  /* Otherwise, check each summand for containing a constant term.  */

  if (XEXP (*p, 0) != 0)
    {
      tem = find_constant_term_loc (&XEXP (*p, 0));
      if (tem != 0)
	return tem;
    }

  if (XEXP (*p, 1) != 0)
    {
      tem = find_constant_term_loc (&XEXP (*p, 1));
      if (tem != 0)
	return tem;
    }

  return 0;
}

/* Return 1 if OP is a memory reference
   whose address contains no side effects
   and remains valid after the addition
   of a positive integer less than the
   size of the object being referenced.

   We assume that the original address is valid and do not check it.

   This uses strict_memory_address_p as a subroutine, so
   don't use it before reload.  */

int
offsettable_memref_p (op)
     rtx op;
{
  return ((GET_CODE (op) == MEM)
	  && offsettable_address_p (1, GET_MODE (op), XEXP (op, 0)));
}

/* Similar, but don't require a strictly valid mem ref:
   consider pseudo-regs valid as index or base regs.  */

int
offsettable_nonstrict_memref_p (op)
     rtx op;
{
  return ((GET_CODE (op) == MEM)
	  && offsettable_address_p (0, GET_MODE (op), XEXP (op, 0)));
}

/* Return 1 if Y is a memory address which contains no side effects
   and would remain valid after the addition of a positive integer
   less than the size of that mode.

   We assume that the original address is valid and do not check it.
   We do check that it is valid for narrower modes.

   If STRICTP is nonzero, we require a strictly valid address,
   for the sake of use in reload.c.  */

int
offsettable_address_p (strictp, mode, y)
     int strictp;
     enum machine_mode mode;
     register rtx y;
{
  register enum rtx_code ycode = GET_CODE (y);
  register rtx z;
  rtx y1 = y;
  rtx *y2;
  int (*addressp) PARAMS ((enum machine_mode, rtx)) =
    (strictp ? strict_memory_address_p : memory_address_p);

  if (CONSTANT_ADDRESS_P (y))
    return 1;

  /* Adjusting an offsettable address involves changing to a narrower mode.
     Make sure that's OK.  */

  if (mode_dependent_address_p (y))
    return 0;

  /* If the expression contains a constant term,
     see if it remains valid when max possible offset is added.  */

  if ((ycode == PLUS) && (y2 = find_constant_term_loc (&y1)))
    {
      int good;

      y1 = *y2;
      *y2 = plus_constant (*y2, GET_MODE_SIZE (mode) - 1);
      /* Use QImode because an odd displacement may be automatically invalid
	 for any wider mode.  But it should be valid for a single byte.  */
      good = (*addressp) (QImode, y);

      /* In any case, restore old contents of memory.  */
      *y2 = y1;
      return good;
    }

  if (ycode == PRE_DEC || ycode == PRE_INC
      || ycode == POST_DEC || ycode == POST_INC)
    return 0;

  /* The offset added here is chosen as the maximum offset that
     any instruction could need to add when operating on something
     of the specified mode.  We assume that if Y and Y+c are
     valid addresses then so is Y+d for all 0<d<c.  */

  z = plus_constant_for_output (y, GET_MODE_SIZE (mode) - 1);

  /* Use QImode because an odd displacement may be automatically invalid
     for any wider mode.  But it should be valid for a single byte.  */
  return (*addressp) (QImode, z);
}

/* Return 1 if ADDR is an address-expression whose effect depends
   on the mode of the memory reference it is used in.

   Autoincrement addressing is a typical example of mode-dependence
   because the amount of the increment depends on the mode.  */

int
mode_dependent_address_p (addr)
  rtx addr ATTRIBUTE_UNUSED; /* Maybe used in GO_IF_MODE_DEPENDENT_ADDRESS. */
{
  GO_IF_MODE_DEPENDENT_ADDRESS (addr, win);
  return 0;
  /* Label `win' might (not) be used via GO_IF_MODE_DEPENDENT_ADDRESS. */
 win: ATTRIBUTE_UNUSED_LABEL
  return 1;
}

/* Return 1 if OP is a general operand
   other than a memory ref with a mode dependent address.  */

int
mode_independent_operand (op, mode)
     enum machine_mode mode;
     rtx op;
{
  rtx addr;

  if (! general_operand (op, mode))
    return 0;

  if (GET_CODE (op) != MEM)
    return 1;

  addr = XEXP (op, 0);
  GO_IF_MODE_DEPENDENT_ADDRESS (addr, lose);
  return 1;
  /* Label `lose' might (not) be used via GO_IF_MODE_DEPENDENT_ADDRESS. */
 lose: ATTRIBUTE_UNUSED_LABEL
  return 0;
}

/* Given an operand OP that is a valid memory reference
   which satisfies offsettable_memref_p,
   return a new memory reference whose address has been adjusted by OFFSET.
   OFFSET should be positive and less than the size of the object referenced.
*/

rtx
adj_offsettable_operand (op, offset)
     rtx op;
     int offset;
{
  register enum rtx_code code = GET_CODE (op);

  if (code == MEM) 
    {
      register rtx y = XEXP (op, 0);
      register rtx new;

      if (CONSTANT_ADDRESS_P (y))
	{
	  new = gen_rtx_MEM (GET_MODE (op),
			     plus_constant_for_output (y, offset));
	  RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (op);
	  return new;
	}

      if (GET_CODE (y) == PLUS)
	{
	  rtx z = y;
	  register rtx *const_loc;

	  op = copy_rtx (op);
	  z = XEXP (op, 0);
	  const_loc = find_constant_term_loc (&z);
	  if (const_loc)
	    {
	      *const_loc = plus_constant_for_output (*const_loc, offset);
	      return op;
	    }
	}

      new = gen_rtx_MEM (GET_MODE (op), plus_constant_for_output (y, offset));
      RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (op);
      return new;
    }
  abort ();
}

/* Analyze INSN and fill in recog_data.  */

void
extract_insn (insn)
     rtx insn;
{
  int i;
  int icode;
  int noperands;
  rtx body = PATTERN (insn);

  recog_data.n_operands = 0;
  recog_data.n_alternatives = 0;
  recog_data.n_dups = 0;

  switch (GET_CODE (body))
    {
    case USE:
    case CLOBBER:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

    case SET:
    case PARALLEL:
    case ASM_OPERANDS:
      recog_data.n_operands = noperands = asm_noperands (body);
      if (noperands >= 0)
	{
	  /* This insn is an `asm' with operands.  */

	  /* expand_asm_operands makes sure there aren't too many operands.  */
	  if (noperands > MAX_RECOG_OPERANDS)
	    abort ();

	  /* Now get the operand values and constraints out of the insn.  */
	  decode_asm_operands (body, recog_data.operand,
			       recog_data.operand_loc,
			       recog_data.constraints,
			       recog_data.operand_mode);
	  if (noperands > 0)
	    {
	      const char *p =  recog_data.constraints[0];
	      recog_data.n_alternatives = 1;
	      while (*p)
		recog_data.n_alternatives += (*p++ == ',');
	    }
	  break;
	}

      /* FALLTHROUGH */

    default:
      /* Ordinary insn: recognize it, get the operands via insn_extract
	 and get the constraints.  */

      icode = recog_memoized (insn);
      if (icode < 0)
	fatal_insn_not_found (insn);

      recog_data.n_operands = noperands = insn_data[icode].n_operands;
      recog_data.n_alternatives = insn_data[icode].n_alternatives;
      recog_data.n_dups = insn_data[icode].n_dups;

      insn_extract (insn);

      for (i = 0; i < noperands; i++)
	{
	  recog_data.constraints[i] = insn_data[icode].operand[i].constraint;
	  recog_data.operand_mode[i] = insn_data[icode].operand[i].mode;
	}
    }
  for (i = 0; i < noperands; i++)
    recog_data.operand_type[i]
      = (recog_data.constraints[i][0] == '=' ? OP_OUT
	 : recog_data.constraints[i][0] == '+' ? OP_INOUT
	 : OP_IN);

  if (recog_data.n_alternatives > MAX_RECOG_ALTERNATIVES)
    abort ();
}

/* After calling extract_insn, you can use this function to extract some
   information from the constraint strings into a more usable form.
   The collected data is stored in recog_op_alt.  */
void
preprocess_constraints ()
{
  int i;

  memset (recog_op_alt, 0, sizeof recog_op_alt);
  for (i = 0; i < recog_data.n_operands; i++)
    {
      int j;
      struct operand_alternative *op_alt;
      const char *p = recog_data.constraints[i];

      op_alt = recog_op_alt[i];

      for (j = 0; j < recog_data.n_alternatives; j++)
	{
	  op_alt[j].class = NO_REGS;
	  op_alt[j].constraint = p;
	  op_alt[j].matches = -1;
	  op_alt[j].matched = -1;

	  if (*p == '\0' || *p == ',')
	    {
	      op_alt[j].anything_ok = 1;
	      continue;
	    }

	  for (;;)
	    {
	      char c = *p++;
	      if (c == '#')
		do
		  c = *p++;
		while (c != ',' && c != '\0');
	      if (c == ',' || c == '\0')
		break;

	      switch (c)
		{
		case '=': case '+': case '*': case '%':
		case 'E': case 'F': case 'G': case 'H':
		case 's': case 'i': case 'n':
		case 'I': case 'J': case 'K': case 'L':
		case 'M': case 'N': case 'O': case 'P':
#ifdef EXTRA_CONSTRAINT
		case 'Q': case 'R': case 'S': case 'T': case 'U':
#endif
		  /* These don't say anything we care about.  */
		  break;

		case '?':
		  op_alt[j].reject += 6;
		  break;
		case '!':
		  op_alt[j].reject += 600;
		  break;
		case '&':
		  op_alt[j].earlyclobber = 1;
		  break;		  

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		  op_alt[j].matches = c - '0';
		  op_alt[op_alt[j].matches].matched = i;
		  break;

		case 'm':
		  op_alt[j].memory_ok = 1;
		  break;
		case '<':
		  op_alt[j].decmem_ok = 1;
		  break;
		case '>':
		  op_alt[j].incmem_ok = 1;
		  break;
		case 'V':
		  op_alt[j].nonoffmem_ok = 1;
		  break;
		case 'o':
		  op_alt[j].offmem_ok = 1;
		  break;
		case 'X':
		  op_alt[j].anything_ok = 1;
		  break;

		case 'p':
		  op_alt[j].class = reg_class_subunion[(int) op_alt[j].class][(int) BASE_REG_CLASS];
		  break;

		case 'g': case 'r':
		  op_alt[j].class = reg_class_subunion[(int) op_alt[j].class][(int) GENERAL_REGS];
		  break;

		default:
		  op_alt[j].class = reg_class_subunion[(int) op_alt[j].class][(int) REG_CLASS_FROM_LETTER ((unsigned char)c)];
		  break;
		}
	    }
	}
    }
}
 
/* Check the operands of an insn against the insn's operand constraints
   and return 1 if they are valid.
   The information about the insn's operands, constraints, operand modes
   etc. is obtained from the global variables set up by extract_insn.

   WHICH_ALTERNATIVE is set to a number which indicates which
   alternative of constraints was matched: 0 for the first alternative,
   1 for the next, etc.

   In addition, when two operands are match
   and it happens that the output operand is (reg) while the
   input operand is --(reg) or ++(reg) (a pre-inc or pre-dec),
   make the output operand look like the input.
   This is because the output operand is the one the template will print.

   This is used in final, just before printing the assembler code and by
   the routines that determine an insn's attribute.

   If STRICT is a positive non-zero value, it means that we have been
   called after reload has been completed.  In that case, we must
   do all checks strictly.  If it is zero, it means that we have been called
   before reload has completed.  In that case, we first try to see if we can
   find an alternative that matches strictly.  If not, we try again, this
   time assuming that reload will fix up the insn.  This provides a "best
   guess" for the alternative and is used to compute attributes of insns prior
   to reload.  A negative value of STRICT is used for this internal call.  */

struct funny_match
{
  int this, other;
};

int
constrain_operands (strict)
     int strict;
{
  const char *constraints[MAX_RECOG_OPERANDS];
  int matching_operands[MAX_RECOG_OPERANDS];
  int earlyclobber[MAX_RECOG_OPERANDS];
  register int c;

  struct funny_match funny_match[MAX_RECOG_OPERANDS];
  int funny_match_index;

  if (recog_data.n_operands == 0 || recog_data.n_alternatives == 0)
    return 1;

  for (c = 0; c < recog_data.n_operands; c++)
    {
      constraints[c] = recog_data.constraints[c];
      matching_operands[c] = -1;
    }

  which_alternative = 0;

  while (which_alternative < recog_data.n_alternatives)
    {
      register int opno;
      int lose = 0;
      funny_match_index = 0;

      for (opno = 0; opno < recog_data.n_operands; opno++)
	{
	  register rtx op = recog_data.operand[opno];
	  enum machine_mode mode = GET_MODE (op);
	  register const char *p = constraints[opno];
	  int offset = 0;
	  int win = 0;
	  int val;

	  earlyclobber[opno] = 0;

	  /* A unary operator may be accepted by the predicate, but it
	     is irrelevant for matching constraints.  */
	  if (GET_RTX_CLASS (GET_CODE (op)) == '1')
	    op = XEXP (op, 0);

	  if (GET_CODE (op) == SUBREG)
	    {
	      if (GET_CODE (SUBREG_REG (op)) == REG
		  && REGNO (SUBREG_REG (op)) < FIRST_PSEUDO_REGISTER)
		offset = SUBREG_WORD (op);
	      op = SUBREG_REG (op);
	    }

	  /* An empty constraint or empty alternative
	     allows anything which matched the pattern.  */
	  if (*p == 0 || *p == ',')
	    win = 1;

	  while (*p && (c = *p++) != ',')
	    switch (c)
	      {
	      case '?':  case '!': case '*':  case '%':
	      case '=':  case '+':
		break;

	      case '#':
		/* Ignore rest of this alternative as far as
		   constraint checking is concerned.  */
		while (*p && *p != ',')
		  p++;
		break;

	      case '&':
		earlyclobber[opno] = 1;
		break;

	      case '0':  case '1':  case '2':  case '3':  case '4':
	      case '5':  case '6':  case '7':  case '8':  case '9':

		/* This operand must be the same as a previous one.
		   This kind of constraint is used for instructions such
		   as add when they take only two operands.

		   Note that the lower-numbered operand is passed first.

		   If we are not testing strictly, assume that this constraint
		   will be satisfied.  */
		if (strict < 0)
		  val = 1;
		else
		  {
		    rtx op1 = recog_data.operand[c - '0'];
		    rtx op2 = recog_data.operand[opno];

	            /* A unary operator may be accepted by the predicate,
		       but it is irrelevant for matching constraints.  */
	            if (GET_RTX_CLASS (GET_CODE (op1)) == '1')
	              op1 = XEXP (op1, 0);
	            if (GET_RTX_CLASS (GET_CODE (op2)) == '1')
	              op2 = XEXP (op2, 0);

		    val = operands_match_p (op1, op2);
		  }

		matching_operands[opno] = c - '0';
		matching_operands[c - '0'] = opno;

		if (val != 0)
		  win = 1;
		/* If output is *x and input is *--x,
		   arrange later to change the output to *--x as well,
		   since the output op is the one that will be printed.  */
		if (val == 2 && strict > 0)
		  {
		    funny_match[funny_match_index].this = opno;
		    funny_match[funny_match_index++].other = c - '0';
		  }
		break;

	      case 'p':
		/* p is used for address_operands.  When we are called by
		   gen_reload, no one will have checked that the address is
		   strictly valid, i.e., that all pseudos requiring hard regs
		   have gotten them.  */
		if (strict <= 0
		    || (strict_memory_address_p (recog_data.operand_mode[opno],
						 op)))
		  win = 1;
		break;

		/* No need to check general_operand again;
		   it was done in insn-recog.c.  */
	      case 'g':
		/* Anything goes unless it is a REG and really has a hard reg
		   but the hard reg is not in the class GENERAL_REGS.  */
		if (strict < 0
		    || GENERAL_REGS == ALL_REGS
		    || GET_CODE (op) != REG
		    || (reload_in_progress
			&& REGNO (op) >= FIRST_PSEUDO_REGISTER)
		    || reg_fits_class_p (op, GENERAL_REGS, offset, mode))
		  win = 1;
		break;

	      case 'r':
		if (strict < 0
		    || (strict == 0
			&& GET_CODE (op) == REG
			&& REGNO (op) >= FIRST_PSEUDO_REGISTER)
		    || (strict == 0 && GET_CODE (op) == SCRATCH)
		    || (GET_CODE (op) == REG
			&& ((GENERAL_REGS == ALL_REGS
			     && REGNO (op) < FIRST_PSEUDO_REGISTER)
			    || reg_fits_class_p (op, GENERAL_REGS,
						 offset, mode))))
		  win = 1;
		break;

	      case 'X':
		/* This is used for a MATCH_SCRATCH in the cases when
		   we don't actually need anything.  So anything goes
		   any time.  */
		win = 1;
		break;

	      case 'm':
		if (GET_CODE (op) == MEM
		    /* Before reload, accept what reload can turn into mem.  */
		    || (strict < 0 && CONSTANT_P (op))
		    /* During reload, accept a pseudo  */
		    || (reload_in_progress && GET_CODE (op) == REG
			&& REGNO (op) >= FIRST_PSEUDO_REGISTER))
		  win = 1;
		break;

	      case '<':
		if (GET_CODE (op) == MEM
		    && (GET_CODE (XEXP (op, 0)) == PRE_DEC
			|| GET_CODE (XEXP (op, 0)) == POST_DEC))
		  win = 1;
		break;

	      case '>':
		if (GET_CODE (op) == MEM
		    && (GET_CODE (XEXP (op, 0)) == PRE_INC
			|| GET_CODE (XEXP (op, 0)) == POST_INC))
		  win = 1;
		break;

	      case 'E':
#ifndef REAL_ARITHMETIC
		/* Match any CONST_DOUBLE, but only if
		   we can examine the bits of it reliably.  */
		if ((HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
		     || HOST_BITS_PER_WIDE_INT != BITS_PER_WORD)
		    && GET_MODE (op) != VOIDmode && ! flag_pretend_float)
		  break;
#endif
		if (GET_CODE (op) == CONST_DOUBLE)
		  win = 1;
		break;

	      case 'F':
		if (GET_CODE (op) == CONST_DOUBLE)
		  win = 1;
		break;

	      case 'G':
	      case 'H':
		if (GET_CODE (op) == CONST_DOUBLE
		    && CONST_DOUBLE_OK_FOR_LETTER_P (op, c))
		  win = 1;
		break;

	      case 's':
		if (GET_CODE (op) == CONST_INT
		    || (GET_CODE (op) == CONST_DOUBLE
			&& GET_MODE (op) == VOIDmode))
		  break;
	      case 'i':
		if (CONSTANT_P (op))
		  win = 1;
		break;

	      case 'n':
		if (GET_CODE (op) == CONST_INT
		    || (GET_CODE (op) == CONST_DOUBLE
			&& GET_MODE (op) == VOIDmode))
		  win = 1;
		break;

	      case 'I':
	      case 'J':
	      case 'K':
	      case 'L':
	      case 'M':
	      case 'N':
	      case 'O':
	      case 'P':
		if (GET_CODE (op) == CONST_INT
		    && CONST_OK_FOR_LETTER_P (INTVAL (op), c))
		  win = 1;
		break;

#ifdef EXTRA_CONSTRAINT
              case 'Q':
              case 'R':
              case 'S':
              case 'T':
              case 'U':
		if (EXTRA_CONSTRAINT (op, c))
		  win = 1;
		break;
#endif

	      case 'V':
		if (GET_CODE (op) == MEM
		    && ((strict > 0 && ! offsettable_memref_p (op))
			|| (strict < 0
			    && !(CONSTANT_P (op) || GET_CODE (op) == MEM))
			|| (reload_in_progress
			    && !(GET_CODE (op) == REG
				 && REGNO (op) >= FIRST_PSEUDO_REGISTER))))
		  win = 1;
		break;

	      case 'o':
		if ((strict > 0 && offsettable_memref_p (op))
		    || (strict == 0 && offsettable_nonstrict_memref_p (op))
		    /* Before reload, accept what reload can handle.  */
		    || (strict < 0
			&& (CONSTANT_P (op) || GET_CODE (op) == MEM))
		    /* During reload, accept a pseudo  */
		    || (reload_in_progress && GET_CODE (op) == REG
			&& REGNO (op) >= FIRST_PSEUDO_REGISTER))
		  win = 1;
		break;

	      default:
		if (strict < 0
		    || (strict == 0
			&& GET_CODE (op) == REG
			&& REGNO (op) >= FIRST_PSEUDO_REGISTER)
		    || (strict == 0 && GET_CODE (op) == SCRATCH)
		    || (GET_CODE (op) == REG
			&& reg_fits_class_p (op, REG_CLASS_FROM_LETTER (c),
					     offset, mode)))
		  win = 1;
	      }

	  constraints[opno] = p;
	  /* If this operand did not win somehow,
	     this alternative loses.  */
	  if (! win)
	    lose = 1;
	}
      /* This alternative won; the operands are ok.
	 Change whichever operands this alternative says to change.  */
      if (! lose)
	{
	  int opno, eopno;

	  /* See if any earlyclobber operand conflicts with some other
	     operand.  */

	  if (strict > 0)
	    for (eopno = 0; eopno < recog_data.n_operands; eopno++)
	      /* Ignore earlyclobber operands now in memory,
		 because we would often report failure when we have
		 two memory operands, one of which was formerly a REG.  */
	      if (earlyclobber[eopno]
		  && GET_CODE (recog_data.operand[eopno]) == REG)
		for (opno = 0; opno < recog_data.n_operands; opno++)
		  if ((GET_CODE (recog_data.operand[opno]) == MEM
		       || recog_data.operand_type[opno] != OP_OUT)
		      && opno != eopno
		      /* Ignore things like match_operator operands.  */
		      && *recog_data.constraints[opno] != 0
		      && ! (matching_operands[opno] == eopno
			    && operands_match_p (recog_data.operand[opno],
						 recog_data.operand[eopno]))
		      && ! safe_from_earlyclobber (recog_data.operand[opno],
						   recog_data.operand[eopno]))
		    lose = 1;

	  if (! lose)
	    {
	      while (--funny_match_index >= 0)
		{
		  recog_data.operand[funny_match[funny_match_index].other]
		    = recog_data.operand[funny_match[funny_match_index].this];
		}

	      return 1;
	    }
	}

      which_alternative++;
    }

  /* If we are about to reject this, but we are not to test strictly,
     try a very loose test.  Only return failure if it fails also.  */
  if (strict == 0)
    return constrain_operands (-1);
  else
    return 0;
}

/* Return 1 iff OPERAND (assumed to be a REG rtx)
   is a hard reg in class CLASS when its regno is offset by OFFSET
   and changed to mode MODE.
   If REG occupies multiple hard regs, all of them must be in CLASS.  */

int
reg_fits_class_p (operand, class, offset, mode)
     rtx operand;
     register enum reg_class class;
     int offset;
     enum machine_mode mode;
{
  register int regno = REGNO (operand);
  if (regno < FIRST_PSEUDO_REGISTER
      && TEST_HARD_REG_BIT (reg_class_contents[(int) class],
			    regno + offset))
    {
      register int sr;
      regno += offset;
      for (sr = HARD_REGNO_NREGS (regno, mode) - 1;
	   sr > 0; sr--)
	if (! TEST_HARD_REG_BIT (reg_class_contents[(int) class],
				 regno + sr))
	  break;
      return sr == 0;
    }

  return 0;
}

/* Split all insns in the function.  If UPD_LIFE, update life info after.  */

void
split_all_insns (upd_life)
     int upd_life;
{
  sbitmap blocks;
  int changed;
  int i;

  blocks = sbitmap_alloc (n_basic_blocks);
  sbitmap_zero (blocks);
  changed = 0;

  for (i = n_basic_blocks - 1; i >= 0; --i)
    {
      basic_block bb = BASIC_BLOCK (i);
      rtx insn, next;

      for (insn = bb->head; insn ; insn = next)
	{
	  rtx set;

	  /* Can't use `next_real_insn' because that might go across
	     CODE_LABELS and short-out basic blocks.  */
	  next = NEXT_INSN (insn);
	  if (GET_CODE (insn) != INSN)
	    ;

	  /* Don't split no-op move insns.  These should silently
	     disappear later in final.  Splitting such insns would
	     break the code that handles REG_NO_CONFLICT blocks.  */

	  else if ((set = single_set (insn)) != NULL
		   && rtx_equal_p (SET_SRC (set), SET_DEST (set)))
	    {
	      /* Nops get in the way while scheduling, so delete them
		 now if register allocation has already been done.  It
		 is too risky to try to do this before register
		 allocation, and there are unlikely to be very many
		 nops then anyways.  */
	      if (reload_completed)
		{
		  PUT_CODE (insn, NOTE);
		  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
		  NOTE_SOURCE_FILE (insn) = 0;
		}
	    }
	  else
	    {
	      /* Split insns here to get max fine-grain parallelism.  */
	      rtx first = PREV_INSN (insn);
	      rtx last = try_split (PATTERN (insn), insn, 1);

	      if (last != insn)
		{
		  SET_BIT (blocks, i);
		  changed = 1;

		  /* try_split returns the NOTE that INSN became.  */
		  first = NEXT_INSN (first);
		  PUT_CODE (insn, NOTE);
		  NOTE_SOURCE_FILE (insn) = 0;
		  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;

		  if (insn == bb->end)
		    {
		      bb->end = last;
		      break;
		    }
		}
	    }

	  if (insn == bb->end)
	    break;
	}

      /* ??? When we're called from just after reload, the CFG is in bad
	 shape, and we may have fallen off the end.  This could be fixed
	 by having reload not try to delete unreachable code.  Otherwise
	 assert we found the end insn.  */
      if (insn == NULL && upd_life)
	abort ();
    }

  if (changed && upd_life)
    {
      compute_bb_for_insn (get_max_uid ());
      count_or_remove_death_notes (blocks, 1);
      update_life_info (blocks, UPDATE_LIFE_LOCAL, PROP_DEATH_NOTES);
    }

  sbitmap_free (blocks);
}

#ifdef HAVE_peephole2
/* This is the last insn we'll allow recog_next_insn to consider.  */
static rtx recog_last_allowed_insn;

/* Return the Nth non-note insn after INSN, or return NULL_RTX if it does
   not exist.  Used by the recognizer to find the next insn to match in a
   multi-insn pattern.  */
rtx
recog_next_insn (insn, n)
     rtx insn;
     int n;
{
  if (insn != NULL_RTX)
    {
      while (n > 0)
	{
	  if (insn == recog_last_allowed_insn)
	    return NULL_RTX;

	  insn = NEXT_INSN (insn);
	  if (insn == NULL_RTX)
	    break;

	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    n -= 1;
	}
    }

  return insn;
}

/* Perform the peephole2 optimization pass. */
void
peephole2_optimize (dump_file)
     FILE *dump_file ATTRIBUTE_UNUSED;
{
  rtx insn, prev;
  int i, changed;
  sbitmap blocks;

  /* ??? TODO: Arrange with resource.c to start at bb->global_live_at_end
     and backtrack insn by insn as we proceed through the block.  In this
     way we'll not need to keep searching forward from the beginning of 
     basic blocks to find register life info.  */

  init_resource_info (NULL);

  blocks = sbitmap_alloc (n_basic_blocks);
  sbitmap_zero (blocks);
  changed = 0;

  for (i = n_basic_blocks - 1; i >= 0; --i)
    {
      basic_block bb = BASIC_BLOCK (i);

      /* Since we don't update life info until the very end, we can't
	 allow matching instructions that we've replaced before.  Walk
	 backward through the basic block so that we don't have to 
	 care about subsequent life info; recog_last_allowed_insn to
	 restrict how far forward we will allow the match to proceed.  */

      recog_last_allowed_insn = NEXT_INSN (bb->end);
      for (insn = bb->end; ; insn = prev)
	{
	  prev = PREV_INSN (insn);
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    {
	      rtx try, last_insn;

	      try = peephole2_insns (PATTERN (insn), insn, &last_insn);
	      if (try != NULL)
		{
		  flow_delete_insn_chain (insn, last_insn);
		  try = emit_insn_after (try, prev);

		  if (last_insn == bb->end)
		    bb->end = try;
		  if (insn == bb->head)
		    bb->head = NEXT_INSN (prev);

		  recog_last_allowed_insn = NEXT_INSN (prev);
		  SET_BIT (blocks, i);
		  changed = 1;
		}
	    }

	  if (insn == bb->head)
	    break;
	}
    }

  free_resource_info ();

  compute_bb_for_insn (get_max_uid ());
  count_or_remove_death_notes (blocks, 1);
  update_life_info (blocks, UPDATE_LIFE_LOCAL, PROP_DEATH_NOTES);
}
#endif
