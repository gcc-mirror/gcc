/* Expand the basic unary and binary arithmetic operations, for GNU compiler.
   Copyright (C) 1987, 1988, 1992 Free Software Foundation, Inc.

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


#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "expr.h"
#include "insn-config.h"
#include "recog.h"
#include <ctype.h>

/* Each optab contains info on how this target machine
   can perform a particular operation
   for all sizes and kinds of operands.

   The operation to be performed is often specified
   by passing one of these optabs as an argument.

   See expr.h for documentation of these optabs.  */

optab add_optab;
optab sub_optab;
optab smul_optab;
optab smul_widen_optab;
optab umul_widen_optab;
optab sdiv_optab;
optab sdivmod_optab;
optab udiv_optab;
optab udivmod_optab;
optab smod_optab;
optab umod_optab;
optab flodiv_optab;
optab ftrunc_optab;
optab and_optab;
optab ior_optab;
optab xor_optab;
optab ashl_optab;
optab lshr_optab;
optab lshl_optab;
optab ashr_optab;
optab rotl_optab;
optab rotr_optab;
optab smin_optab;
optab smax_optab;
optab umin_optab;
optab umax_optab;

optab mov_optab;
optab movstrict_optab;

optab neg_optab;
optab abs_optab;
optab one_cmpl_optab;
optab ffs_optab;
optab sqrt_optab;
optab sin_optab;
optab cos_optab;

optab cmp_optab;
optab ucmp_optab;  /* Used only for libcalls for unsigned comparisons.  */
optab tst_optab;

optab strlen_optab;

/* SYMBOL_REF rtx's for the library functions that are called
   implicitly and not via optabs.  */

rtx extendsfdf2_libfunc;
rtx extendsfxf2_libfunc;
rtx extendsftf2_libfunc;
rtx extenddfxf2_libfunc;
rtx extenddftf2_libfunc;

rtx truncdfsf2_libfunc;
rtx truncxfsf2_libfunc;
rtx trunctfsf2_libfunc;
rtx truncxfdf2_libfunc;
rtx trunctfdf2_libfunc;

rtx memcpy_libfunc;
rtx bcopy_libfunc;
rtx memcmp_libfunc;
rtx bcmp_libfunc;
rtx memset_libfunc;
rtx bzero_libfunc;

rtx eqsf2_libfunc;
rtx nesf2_libfunc;
rtx gtsf2_libfunc;
rtx gesf2_libfunc;
rtx ltsf2_libfunc;
rtx lesf2_libfunc;

rtx eqdf2_libfunc;
rtx nedf2_libfunc;
rtx gtdf2_libfunc;
rtx gedf2_libfunc;
rtx ltdf2_libfunc;
rtx ledf2_libfunc;

rtx eqxf2_libfunc;
rtx nexf2_libfunc;
rtx gtxf2_libfunc;
rtx gexf2_libfunc;
rtx ltxf2_libfunc;
rtx lexf2_libfunc;

rtx eqtf2_libfunc;
rtx netf2_libfunc;
rtx gttf2_libfunc;
rtx getf2_libfunc;
rtx lttf2_libfunc;
rtx letf2_libfunc;

rtx floatsisf_libfunc;
rtx floatdisf_libfunc;
rtx floattisf_libfunc;

rtx floatsidf_libfunc;
rtx floatdidf_libfunc;
rtx floattidf_libfunc;

rtx floatsixf_libfunc;
rtx floatdixf_libfunc;
rtx floattixf_libfunc;

rtx floatsitf_libfunc;
rtx floatditf_libfunc;
rtx floattitf_libfunc;

rtx fixsfsi_libfunc;
rtx fixsfdi_libfunc;
rtx fixsfti_libfunc;

rtx fixdfsi_libfunc;
rtx fixdfdi_libfunc;
rtx fixdfti_libfunc;

rtx fixxfsi_libfunc;
rtx fixxfdi_libfunc;
rtx fixxfti_libfunc;

rtx fixtfsi_libfunc;
rtx fixtfdi_libfunc;
rtx fixtfti_libfunc;

rtx fixunssfsi_libfunc;
rtx fixunssfdi_libfunc;
rtx fixunssfti_libfunc;

rtx fixunsdfsi_libfunc;
rtx fixunsdfdi_libfunc;
rtx fixunsdfti_libfunc;

rtx fixunsxfsi_libfunc;
rtx fixunsxfdi_libfunc;
rtx fixunsxfti_libfunc;

rtx fixunstfsi_libfunc;
rtx fixunstfdi_libfunc;
rtx fixunstfti_libfunc;

/* from emit-rtl.c */
extern rtx gen_highpart ();

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the gen_function to make a branch to test that condition.  */

rtxfun bcc_gen_fctn[NUM_RTX_CODE];

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the insn code to make a store-condition insn
   to test that condition.  */

enum insn_code setcc_gen_code[NUM_RTX_CODE];

static void emit_float_lib_cmp ();

/* Add a REG_EQUAL note to the last insn in SEQ.  TARGET is being set to
   the result of operation CODE applied to OP0 (and OP1 if it is a binary
   operation).

   If the last insn does not set TARGET, don't do anything, but return 1.

   If a previous insn sets TARGET and TARGET is one of OP0 or OP1,
   don't add the REG_EQUAL note but return 0.  Our caller can then try
   again, ensuring that TARGET is not one of the operands.  */

static int
add_equal_note (seq, target, code, op0, op1)
     rtx seq;
     rtx target;
     enum rtx_code code;
     rtx op0, op1;
{
  rtx set;
  int i;
  rtx note;

  if ((GET_RTX_CLASS (code) != '1' && GET_RTX_CLASS (code) != '2'
       && GET_RTX_CLASS (code) != 'c' && GET_RTX_CLASS (code) != '<')
      || GET_CODE (seq) != SEQUENCE
      || (set = single_set (XVECEXP (seq, 0, XVECLEN (seq, 0) - 1))) == 0
      || GET_CODE (target) == ZERO_EXTRACT
      || (! rtx_equal_p (SET_DEST (set), target)
	  /* For a STRICT_LOW_PART, the REG_NOTE applies to what is inside the
	     SUBREG.  */
	  && (GET_CODE (SET_DEST (set)) != STRICT_LOW_PART
	      || ! rtx_equal_p (SUBREG_REG (XEXP (SET_DEST (set), 0)),
				target))))
    return 1;

  /* If TARGET is in OP0 or OP1, check if anything in SEQ sets TARGET
     besides the last insn.  */
  if (reg_overlap_mentioned_p (target, op0)
      || (op1 && reg_overlap_mentioned_p (target, op1)))
    for (i = XVECLEN (seq, 0) - 2; i >= 0; i--)
      if (reg_set_p (target, XVECEXP (seq, 0, i)))
	return 0;

  if (GET_RTX_CLASS (code) == '1')
    note = gen_rtx (code, GET_MODE (target), op0);
  else
    note = gen_rtx (code, GET_MODE (target), op0, op1);

  REG_NOTES (XVECEXP (seq, 0, XVECLEN (seq, 0) - 1))
    = gen_rtx (EXPR_LIST, REG_EQUAL, note,
	       REG_NOTES (XVECEXP (seq, 0, XVECLEN (seq, 0) - 1)));

  return 1;
}

/* Generate code to perform an operation specified by BINOPTAB
   on operands OP0 and OP1, with result having machine-mode MODE.

   UNSIGNEDP is for the case where we have to widen the operands
   to perform the operation.  It says to use zero-extension.

   If TARGET is nonzero, the value
   is generated there, if it is convenient to do so.
   In all cases an rtx is returned for the locus of the value;
   this may or may not be TARGET.  */

rtx
expand_binop (mode, binoptab, op0, op1, target, unsignedp, methods)
     enum machine_mode mode;
     optab binoptab;
     rtx op0, op1;
     rtx target;
     int unsignedp;
     enum optab_methods methods;
{
  enum mode_class class;
  enum machine_mode wider_mode;
  register rtx temp;
  int commutative_op = 0;
  int shift_op = (binoptab->code ==  ASHIFT
		  || binoptab->code == ASHIFTRT
		  || binoptab->code == LSHIFT
		  || binoptab->code == LSHIFTRT
		  || binoptab->code == ROTATE
		  || binoptab->code == ROTATERT);
  rtx entry_last = get_last_insn ();
  rtx last;

  class = GET_MODE_CLASS (mode);

  op0 = protect_from_queue (op0, 0);
  op1 = protect_from_queue (op1, 0);
  if (target)
    target = protect_from_queue (target, 1);

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  /* If subtracting an integer constant, convert this into an addition of
     the negated constant.  */

  if (binoptab == sub_optab && GET_CODE (op1) == CONST_INT)
    {
      op1 = negate_rtx (mode, op1);
      binoptab = add_optab;
    }

  /* If we are inside an appropriately-short loop and one operand is an
     expensive constant, force it into a register.  */
  if (CONSTANT_P (op0) && preserve_subexpressions_p ()
      && rtx_cost (op0, binoptab->code) > 2)
    op0 = force_reg (mode, op0);

  if (CONSTANT_P (op1) && preserve_subexpressions_p ()
      && rtx_cost (op1, binoptab->code) > 2)
    op1 = force_reg (shift_op ? word_mode : mode, op1);

  /* Record where to delete back to if we backtrack.  */
  last = get_last_insn ();

  /* If operation is commutative,
     try to make the first operand a register.
     Even better, try to make it the same as the target.
     Also try to make the last operand a constant.  */
  if (GET_RTX_CLASS (binoptab->code) == 'c'
      || binoptab == smul_widen_optab
      || binoptab == umul_widen_optab)
    {
      commutative_op = 1;

      if (((target == 0 || GET_CODE (target) == REG)
	   ? ((GET_CODE (op1) == REG
	       && GET_CODE (op0) != REG)
	      || target == op1)
	   : rtx_equal_p (op1, target))
	  || GET_CODE (op0) == CONST_INT)
	{
	  temp = op1;
	  op1 = op0;
	  op0 = temp;
	}
    }

  /* If we can do it with a three-operand insn, do so.  */

  if (methods != OPTAB_MUST_WIDEN
      && binoptab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) binoptab->handlers[(int) mode].insn_code;
      enum machine_mode mode0 = insn_operand_mode[icode][1];
      enum machine_mode mode1 = insn_operand_mode[icode][2];
      rtx pat;
      rtx xop0 = op0, xop1 = op1;

      if (target)
	temp = target;
      else
	temp = gen_reg_rtx (mode);

      /* If it is a commutative operator and the modes would match
	 if we would swap the operands, we can save the conversions. */
      if (commutative_op)
	{
	  if (GET_MODE (op0) != mode0 && GET_MODE (op1) != mode1
	      && GET_MODE (op0) == mode1 && GET_MODE (op1) == mode0)
	    {
	      register rtx tmp;

	      tmp = op0; op0 = op1; op1 = tmp;
	      tmp = xop0; xop0 = xop1; xop1 = tmp;
	    }
	}

      /* In case the insn wants input operands in modes different from
	 the result, convert the operands.  */

      if (GET_MODE (op0) != VOIDmode
	  && GET_MODE (op0) != mode0)
	xop0 = convert_to_mode (mode0, xop0, unsignedp);

      if (GET_MODE (xop1) != VOIDmode
	  && GET_MODE (xop1) != mode1)
	xop1 = convert_to_mode (mode1, xop1, unsignedp);

      /* Now, if insn's predicates don't allow our operands, put them into
	 pseudo regs.  */

      if (! (*insn_operand_predicate[icode][1]) (xop0, mode0))
	xop0 = copy_to_mode_reg (mode0, xop0);

      if (! (*insn_operand_predicate[icode][2]) (xop1, mode1))
	xop1 = copy_to_mode_reg (mode1, xop1);

      if (! (*insn_operand_predicate[icode][0]) (temp, mode))
	temp = gen_reg_rtx (mode);

      pat = GEN_FCN (icode) (temp, xop0, xop1);
      if (pat)
	{
	  /* If PAT is a multi-insn sequence, try to add an appropriate
	     REG_EQUAL note to it.  If we can't because TEMP conflicts with an
	     operand, call ourselves again, this time without a target.  */
	  if (GET_CODE (pat) == SEQUENCE
	      && ! add_equal_note (pat, temp, binoptab->code, xop0, xop1))
	    {
	      delete_insns_since (last);
	      return expand_binop (mode, binoptab, op0, op1, NULL_RTX,
				   unsignedp, methods);
	    }

	  emit_insn (pat);
	  return temp;
	}
      else
	delete_insns_since (last);
    }

  /* If this is a multiply, see if we can do a widening operation that
     takes operands of this mode and makes a wider mode.  */

  if (binoptab == smul_optab && GET_MODE_WIDER_MODE (mode) != VOIDmode
      && (((unsignedp ? umul_widen_optab : smul_widen_optab)
	   ->handlers[(int) GET_MODE_WIDER_MODE (mode)].insn_code)
	  != CODE_FOR_nothing))
    {
      temp = expand_binop (GET_MODE_WIDER_MODE (mode),
			   unsignedp ? umul_widen_optab : smul_widen_optab,
			   op0, op1, 0, unsignedp, OPTAB_DIRECT);

      if (GET_MODE_CLASS (mode) == MODE_INT)
	return gen_lowpart (mode, temp);
      else
	return convert_to_mode (mode, temp, unsignedp);
    }

  /* Look for a wider mode of the same class for which we think we
     can open-code the operation.  Check for a widening multiply at the
     wider mode as well.  */

  if ((class == MODE_INT || class == MODE_FLOAT || class == MODE_COMPLEX_FLOAT)
      && methods != OPTAB_DIRECT && methods != OPTAB_LIB)
    for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
	 wider_mode = GET_MODE_WIDER_MODE (wider_mode))
      {
	if (binoptab->handlers[(int) wider_mode].insn_code != CODE_FOR_nothing
	    || (binoptab == smul_optab
		&& GET_MODE_WIDER_MODE (wider_mode) != VOIDmode
		&& (((unsignedp ? umul_widen_optab : smul_widen_optab)
		     ->handlers[(int) GET_MODE_WIDER_MODE (wider_mode)].insn_code)
		    != CODE_FOR_nothing)))
	  {
	    rtx xop0 = op0, xop1 = op1;
	    int no_extend = 0;

	    /* For certain integer operations, we need not actually extend
	       the narrow operands, as long as we will truncate
	       the results to the same narrowness.  Don't do this when
	       WIDER_MODE is wider than a word since a paradoxical SUBREG
	       isn't valid for such modes.  */

	    if ((binoptab == ior_optab || binoptab == and_optab
		 || binoptab == xor_optab
		 || binoptab == add_optab || binoptab == sub_optab
		 || binoptab == smul_optab
		 || binoptab == ashl_optab || binoptab == lshl_optab)
		&& class == MODE_INT
		&& GET_MODE_SIZE (wider_mode) <= UNITS_PER_WORD)
	      no_extend = 1;

	    /* If an operand is a constant integer, we might as well
	       convert it since that is more efficient than using a SUBREG,
	       unlike the case for other operands.  Similarly for
	       SUBREGs that were made due to promoted objects.  */

	    if (no_extend && GET_MODE (xop0) != VOIDmode
		&& ! (GET_CODE (xop0) == SUBREG
		      && SUBREG_PROMOTED_VAR_P (xop0)))
	      xop0 = gen_rtx (SUBREG, wider_mode,
			      force_reg (GET_MODE (xop0), xop0), 0);
	    else
	      xop0 = convert_to_mode (wider_mode, xop0, unsignedp);

	    if (no_extend && GET_MODE (xop1) != VOIDmode
		&& ! (GET_CODE (xop1) == SUBREG
		      && SUBREG_PROMOTED_VAR_P (xop1)))
	      xop1 = gen_rtx (SUBREG, wider_mode,
				force_reg (GET_MODE (xop1), xop1), 0);
	    else
	      xop1 = convert_to_mode (wider_mode, xop1, unsignedp);

	    temp = expand_binop (wider_mode, binoptab, xop0, xop1, NULL_RTX,
				 unsignedp, OPTAB_DIRECT);
	    if (temp)
	      {
		if (class != MODE_INT)
		  {
		    if (target == 0)
		      target = gen_reg_rtx (mode);
		    convert_move (target, temp, 0);
		    return target;
		  }
		else
		  return gen_lowpart (mode, temp);
	      }
	    else
	      delete_insns_since (last);
	  }
      }

  /* These can be done a word at a time.  */
  if ((binoptab == and_optab || binoptab == ior_optab || binoptab == xor_optab)
      && class == MODE_INT
      && GET_MODE_SIZE (mode) > UNITS_PER_WORD
      && binoptab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing)
    {
      int i;
      rtx insns;
      rtx equiv_value;

      /* If TARGET is the same as one of the operands, the REG_EQUAL note
	 won't be accurate, so use a new target.  */
      if (target == 0 || target == op0 || target == op1)
	target = gen_reg_rtx (mode);

      start_sequence ();

      /* Do the actual arithmetic.  */
      for (i = 0; i < GET_MODE_BITSIZE (mode) / BITS_PER_WORD; i++)
	{
	  rtx target_piece = operand_subword (target, i, 1, mode);
	  rtx x = expand_binop (word_mode, binoptab,
				operand_subword_force (op0, i, mode),
				operand_subword_force (op1, i, mode),
				target_piece, unsignedp, methods);
	  if (target_piece != x)
	    emit_move_insn (target_piece, x);
	}

      insns = get_insns ();
      end_sequence ();

      if (binoptab->code != UNKNOWN)
	equiv_value = gen_rtx (binoptab->code, mode, op0, op1);
      else
	equiv_value = 0;

      emit_no_conflict_block (insns, target, op0, op1, equiv_value);
      return target;
    }

  /* These can be done a word at a time by propagating carries.  */
  if ((binoptab == add_optab || binoptab == sub_optab)
      && class == MODE_INT
      && GET_MODE_SIZE (mode) >= 2 * UNITS_PER_WORD
      && binoptab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing)
    {
      int i;
      rtx carry_tmp = gen_reg_rtx (word_mode);
      optab otheroptab = binoptab == add_optab ? sub_optab : add_optab;
      int nwords = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;
      rtx carry_in, carry_out;
      rtx xop0, xop1;

      /* We can handle either a 1 or -1 value for the carry.  If STORE_FLAG
	 value is one of those, use it.  Otherwise, use 1 since it is the
	 one easiest to get.  */
#if STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1
      int normalizep = STORE_FLAG_VALUE;
#else
      int normalizep = 1;
#endif

      /* Prepare the operands.  */
      xop0 = force_reg (mode, op0);
      xop1 = force_reg (mode, op1);

      if (target == 0 || GET_CODE (target) != REG
	  || target == xop0 || target == xop1)
	target = gen_reg_rtx (mode);

      /* Do the actual arithmetic.  */
      for (i = 0; i < nwords; i++)
	{
	  int index = (WORDS_BIG_ENDIAN ? nwords - i - 1 : i);
	  rtx target_piece = operand_subword (target, index, 1, mode);
	  rtx op0_piece = operand_subword_force (xop0, index, mode);
	  rtx op1_piece = operand_subword_force (xop1, index, mode);
	  rtx x;

	  /* Main add/subtract of the input operands.  */
	  x = expand_binop (word_mode, binoptab,
			    op0_piece, op1_piece,
			    target_piece, unsignedp, methods);
	  if (x == 0)
	    break;

	  if (i + 1 < nwords)
	    {
	      /* Store carry from main add/subtract.  */
	      carry_out = gen_reg_rtx (word_mode);
	      carry_out = emit_store_flag (carry_out,
					   binoptab == add_optab ? LTU : GTU,
					   x, op0_piece,
					   word_mode, 1, normalizep);
	      if (!carry_out)
		break;
	    }

	  if (i > 0)
	    {
	      /* Add/subtract previous carry to main result.  */
	      x = expand_binop (word_mode,
				normalizep == 1 ? binoptab : otheroptab,
				x, carry_in,
				target_piece, 1, methods);
	      if (target_piece != x)
		emit_move_insn (target_piece, x);

	      if (i + 1 < nwords)
		{
		  /* THIS CODE HAS NOT BEEN TESTED.  */
		  /* Get out carry from adding/subtracting carry in.  */
		  carry_tmp = emit_store_flag (carry_tmp,
					       binoptab == add_optab
					         ? LTU : GTU,
					       x, carry_in,
					       word_mode, 1, normalizep);
		  /* Logical-ior the two poss. carry together.  */
		  carry_out = expand_binop (word_mode, ior_optab,
					    carry_out, carry_tmp,
					    carry_out, 0, methods);
		  if (!carry_out)
		    break;
		}
	    }

	  carry_in = carry_out;
	}	

      if (i == GET_MODE_BITSIZE (mode) / BITS_PER_WORD)
	{
	  rtx temp;
	  
	  temp = emit_move_insn (target, target);
	  REG_NOTES (temp) = gen_rtx (EXPR_LIST, REG_EQUAL,
				      gen_rtx (binoptab->code, mode, xop0, xop1),
				      REG_NOTES (temp));
	  return target;
	}
      else
	delete_insns_since (last);
    }

  /* If we want to multiply two two-word values and have normal and widening
     multiplies of single-word values, we can do this with three smaller
     multiplications.  Note that we do not make a REG_NO_CONFLICT block here
     because we are not operating on one word at a time. 

     The multiplication proceeds as follows:
			         _______________________
			        [__op0_high_|__op0_low__]
			         _______________________
        *			    [__op1_high_|__op1_low__]
        _______________________________________________
			         _______________________
    (1)			    [__op0_low__*__op1_low__]
		     _______________________
    (2a)		[__op0_low__*__op1_high_]
		     _______________________
    (2b)		[__op0_high_*__op1_low__]
         _______________________
    (3) [__op0_high_*__op1_high_]


    This gives a 4-word result.  Since we are only interested in the
    lower 2 words, partial result (3) and the upper words of (2a) and
    (2b) don't need to be calculated.  Hence (2a) and (2b) can be
    calculated using non-widening multiplication.

    (1), however, needs to be calculated with an unsigned widening
    multiplication.  If this operation is not directly supported we
    try using a signed widening multiplication and adjust the result.
    This adjustment works as follows:

      If both operands are positive then no adjustment is needed.

      If the operands have different signs, for example op0_low < 0 and
      op1_low >= 0, the instruction treats the most significant bit of
      op0_low as a sign bit instead of a bit with significance
      2**(BITS_PER_WORD-1), i.e. the instruction multiplies op1_low
      with 2**BITS_PER_WORD - op0_low, and two's complements the
      result.  Conclusion: We need to add op1_low * 2**BITS_PER_WORD to
      the result.

      Similarly, if both operands are negative, we need to add
      (op0_low + op1_low) * 2**BITS_PER_WORD.

      We use a trick to adjust quickly.  We logically shift op0_low right
      (op1_low) BITS_PER_WORD-1 steps to get 0 or 1, and add this to
      op0_high (op1_high) before it is used to calculate 2b (2a).  If no
      logical shift exists, we do an arithmetic right shift and subtract
      the 0 or -1.  */

  if (binoptab == smul_optab
      && class == MODE_INT
      && GET_MODE_SIZE (mode) == 2 * UNITS_PER_WORD
      && smul_optab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing
      && add_optab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing
      && ((umul_widen_optab->handlers[(int) mode].insn_code
	   != CODE_FOR_nothing)
	  || (smul_widen_optab->handlers[(int) mode].insn_code
	      != CODE_FOR_nothing)))
    {
      int low = (WORDS_BIG_ENDIAN ? 1 : 0);
      int high = (WORDS_BIG_ENDIAN ? 0 : 1);
      rtx op0_high = operand_subword_force (op0, high, mode);
      rtx op0_low = operand_subword_force (op0, low, mode);
      rtx op1_high = operand_subword_force (op1, high, mode);
      rtx op1_low = operand_subword_force (op1, low, mode);
      rtx product = 0;
      rtx op0_xhigh;
      rtx op1_xhigh;

      /* If the target is the same as one of the inputs, don't use it.  This
	 prevents problems with the REG_EQUAL note.  */
      if (target == op0 || target == op1)
	target = 0;

      /* Multiply the two lower words to get a double-word product.
	 If unsigned widening multiplication is available, use that;
	 otherwise use the signed form and compensate.  */

      if (umul_widen_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
	{
	  product = expand_binop (mode, umul_widen_optab, op0_low, op1_low,
				  target, 1, OPTAB_DIRECT);

	  /* If we didn't succeed, delete everything we did so far.  */
	  if (product == 0)
	    delete_insns_since (last);
	  else
	    op0_xhigh = op0_high, op1_xhigh = op1_high;
	}

      if (product == 0
	  && smul_widen_optab->handlers[(int) mode].insn_code
	       != CODE_FOR_nothing)
	{
	  rtx wordm1 = GEN_INT (BITS_PER_WORD - 1);
	  product = expand_binop (mode, smul_widen_optab, op0_low, op1_low,
				  target, 1, OPTAB_DIRECT);
	  op0_xhigh = expand_binop (word_mode, lshr_optab, op0_low, wordm1,
				    NULL_RTX, 1, OPTAB_DIRECT);
	  if (op0_xhigh)
	    op0_xhigh = expand_binop (word_mode, add_optab, op0_high,
				      op0_xhigh, op0_xhigh, 0, OPTAB_DIRECT);
	  else
	    {
	      op0_xhigh = expand_binop (word_mode, ashr_optab, op0_low, wordm1,
					NULL_RTX, 0, OPTAB_DIRECT);
	      if (op0_xhigh)
		op0_xhigh = expand_binop (word_mode, sub_optab, op0_high,
					  op0_xhigh, op0_xhigh, 0,
					  OPTAB_DIRECT);
	    }

	  op1_xhigh = expand_binop (word_mode, lshr_optab, op1_low, wordm1,
				    NULL_RTX, 1, OPTAB_DIRECT);
	  if (op1_xhigh)
	    op1_xhigh = expand_binop (word_mode, add_optab, op1_high,
				      op1_xhigh, op1_xhigh, 0, OPTAB_DIRECT);
	  else
	    {
	      op1_xhigh = expand_binop (word_mode, ashr_optab, op1_low, wordm1,
					NULL_RTX, 0, OPTAB_DIRECT);
	      if (op1_xhigh)
		op1_xhigh = expand_binop (word_mode, sub_optab, op1_high,
					  op1_xhigh, op1_xhigh, 0,
					  OPTAB_DIRECT);
	    }
	}

      /* If we have been able to directly compute the product of the
	 low-order words of the operands and perform any required adjustments
	 of the operands, we proceed by trying two more multiplications
	 and then computing the appropriate sum.

	 We have checked above that the required addition is provided.
	 Full-word addition will normally always succeed, especially if
	 it is provided at all, so we don't worry about its failure.  The
	 multiplication may well fail, however, so we do handle that.  */

      if (product && op0_xhigh && op1_xhigh)
	{
	  rtx product_piece;
	  rtx product_high = operand_subword (product, high, 1, mode);
	  rtx temp = expand_binop (word_mode, binoptab, op0_low, op1_xhigh,
				   NULL_RTX, 0, OPTAB_DIRECT);

	  if (temp)
	    {
	      product_piece = expand_binop (word_mode, add_optab, temp,
					    product_high, product_high,
					    0, OPTAB_LIB_WIDEN);
	      if (product_piece != product_high)
		emit_move_insn (product_high, product_piece);

	      temp = expand_binop (word_mode, binoptab, op1_low, op0_xhigh, 
				   NULL_RTX, 0, OPTAB_DIRECT);

	      product_piece = expand_binop (word_mode, add_optab, temp,
					    product_high, product_high,
					    0, OPTAB_LIB_WIDEN);
	      if (product_piece != product_high)
		emit_move_insn (product_high, product_piece);

	      temp = emit_move_insn (product, product);
	      REG_NOTES (temp) = gen_rtx (EXPR_LIST, REG_EQUAL,
					  gen_rtx (MULT, mode, op0, op1),
					  REG_NOTES (temp));

	      return product;
	    }
	}

      /* If we get here, we couldn't do it for some reason even though we
	 originally thought we could.  Delete anything we've emitted in
	 trying to do it.  */

      delete_insns_since (last);
    }

  /* We need to open-code the complex type operations: '+, -, * and /' */

  /* At this point we allow operations between two similar complex
     numbers, and also if one of the operands is not a complex number
     but rather of MODE_FLOAT or MODE_INT. However, the caller
     must make sure that the MODE of the non-complex operand matches
     the SUBMODE of the complex operand.  */

  if (class == MODE_COMPLEX_FLOAT || class == MODE_COMPLEX_INT)
    {
      rtx real0 = (rtx) 0;
      rtx imag0 = (rtx) 0;
      rtx real1 = (rtx) 0;
      rtx imag1 = (rtx) 0;
      rtx realr;
      rtx imagr;
      rtx res;
      rtx seq;
      rtx equiv_value;

      /* Find the correct mode for the real and imaginary parts */
      enum machine_mode submode
	= mode_for_size (GET_MODE_UNIT_SIZE (mode) * BITS_PER_UNIT,
			 class == MODE_COMPLEX_INT ? MODE_INT : MODE_FLOAT,
			 0);

      if (submode == BLKmode)
	abort ();

      if (! target)
	target = gen_reg_rtx (mode);

      start_sequence ();

      realr = gen_realpart  (submode, target);
      imagr = gen_imagpart (submode, target);

      if (GET_MODE (op0) == mode)
	{
	  real0 = gen_realpart  (submode, op0);
	  imag0 = gen_imagpart (submode, op0);
	}
      else
	real0 = op0;

      if (GET_MODE (op1) == mode)
	{
	  real1 = gen_realpart  (submode, op1);
	  imag1 = gen_imagpart (submode, op1);
	}
      else
	real1 = op1;

      if (! real0 || ! real1 || ! (imag0 || imag1))
	abort ();

      switch (binoptab->code)
	{
	case PLUS:
	case MINUS:
	  res = expand_binop (submode, binoptab, real0, real1,
			      realr, unsignedp, methods);
	  if (res != realr)
	    emit_move_insn (realr, res);

	  if (imag0 && imag1)
	    res = expand_binop (submode, binoptab, imag0, imag1,
				imagr, unsignedp, methods);
	  else if (imag0)
	    res = imag0;
	  else if (binoptab->code == MINUS)
	    res = expand_unop (submode, neg_optab, imag1, imagr, unsignedp);
	  else
	    res = imag1;

	  if (res != imagr)
	    emit_move_insn (imagr, res);
	  break;

	case MULT:
	  /* (a+ib) * (c+id) = (ac-bd) + i(ad+cb) */

	  res = expand_binop (submode, binoptab, real0, real1,
			      realr, unsignedp, methods);

	  if (imag0 && imag1)
	    {
	      rtx temp =
		expand_binop (submode, sub_optab, res,
			      expand_binop (submode, binoptab, imag0, imag1,
					    0, unsignedp, methods),
			      realr, unsignedp, methods);

	      if (temp != realr)
		emit_move_insn (realr, temp);

	      res = expand_binop (submode, add_optab,
				  expand_binop (submode, binoptab,
						real0, imag1,
						0, unsignedp, methods),
				  expand_binop (submode, binoptab,
						real1, imag0,
						0, unsignedp, methods),
				  imagr, unsignedp, methods);
	      if (res != imagr)
		emit_move_insn (imagr, res);
	    }
	  else
	    {
	      if (res != realr)
		emit_move_insn (realr, res);

	      if (imag0)
		res = expand_binop (submode, binoptab,
				    real1, imag0, imagr, unsignedp, methods);
	      else
		res = expand_binop (submode, binoptab,
				    real0, imag1, imagr, unsignedp, methods);
	      if (res != imagr)
		emit_move_insn (imagr, res);
	    }
	  break;

	case DIV:
	  /* (c+id)/(a+ib) == ((c+id)*(a-ib))/(a*a+b*b) */
	  
	  if (! imag1)
	    {
	      /* Simply divide the real and imaginary parts by `a' */
	      res = expand_binop (submode, binoptab, real0, real1,
				  realr, unsignedp, methods);
	      if (res != realr)
		emit_move_insn (realr, res);

	      res = expand_binop (submode, binoptab, imag0, real1,
				  imagr, unsignedp, methods);
	      if (res != imagr)
		emit_move_insn (imagr, res);
	    }
	  else			/* Divisor is of complex type */
	    {			/* X/(a+ib) */

	      rtx divisor;
	      rtx real_t;
	      rtx imag_t;
	      
	      optab mulopt = unsignedp ? umul_widen_optab : smul_optab;

	      /* Divisor: c*c + d*d */
	      divisor = expand_binop (submode, add_optab,
				      expand_binop (submode, mulopt,
						    real1, real1,
						    0, unsignedp, methods),
				      expand_binop (submode, mulopt,
						    imag1, imag1,
						    0, unsignedp, methods),
				      0, unsignedp, methods);

	      if (! imag0)	/* ((a)(c-id))/divisor */
		{	/* (a+i0) / (c+id) = (ac/(cc+dd)) + i(-ad/(cc+dd)) */
		  /* Calculate the dividend */
		  real_t = expand_binop (submode, mulopt, real0, real1,
					 0, unsignedp, methods);
		  
		  imag_t
		    = expand_unop (submode, neg_optab,
				   expand_binop (submode, mulopt, real0, imag1,
						 0, unsignedp, methods),
				   0, unsignedp);
		}
	      else		/* ((a+ib)(c-id))/divider */
		{
		  /* Calculate the dividend */
		  real_t = expand_binop (submode, add_optab,
					 expand_binop (submode, mulopt,
						       real0, real1,
						       0, unsignedp, methods),
					 expand_binop (submode, mulopt,
						       imag0, imag1,
						       0, unsignedp, methods),
					 0, unsignedp, methods);
		  
		  imag_t = expand_binop (submode, sub_optab,
					 expand_binop (submode, mulopt,
						       imag0, real1,
						       0, unsignedp, methods),
					 expand_binop (submode, mulopt,
						       real0, imag1,
						       0, unsignedp, methods),
					 0, unsignedp, methods);

		}

	      res = expand_binop (submode, binoptab, real_t, divisor,
				  realr, unsignedp, methods);
	      if (res != realr)
		emit_move_insn (realr, res);

	      res = expand_binop (submode, binoptab, imag_t, divisor,
				  imagr, unsignedp, methods);
	      if (res != imagr)
		emit_move_insn (imagr, res);
	    }
	  break;
	  
	default:
	  abort ();
	}

      seq = get_insns ();
      end_sequence ();

      if (binoptab->code != UNKNOWN)
	equiv_value = gen_rtx (binoptab->code, mode, op0, op1);
      else
	equiv_value = 0;
	  
      emit_no_conflict_block (seq, target, op0, op1, equiv_value);
      
      return target;
    }

  /* It can't be open-coded in this mode.
     Use a library call if one is available and caller says that's ok.  */

  if (binoptab->handlers[(int) mode].libfunc
      && (methods == OPTAB_LIB || methods == OPTAB_LIB_WIDEN))
    {
      rtx insns;
      rtx funexp = binoptab->handlers[(int) mode].libfunc;
      rtx op1x = op1;
      enum machine_mode op1_mode = mode;

      start_sequence ();

      if (shift_op)
	{
	  op1_mode = word_mode;
	  /* Specify unsigned here,
	     since negative shift counts are meaningless.  */
	  op1x = convert_to_mode (word_mode, op1, 1);
	}

      /* Pass 1 for NO_QUEUE so we don't lose any increments
	 if the libcall is cse'd or moved.  */
      emit_library_call (binoptab->handlers[(int) mode].libfunc,
			 1, mode, 2, op0, mode, op1x, op1_mode);

      insns = get_insns ();
      end_sequence ();

      target = gen_reg_rtx (mode);
      emit_libcall_block (insns, target, hard_libcall_value (mode),
			  gen_rtx (binoptab->code, mode, op0, op1));

      return target;
    }

  delete_insns_since (last);

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if (! (methods == OPTAB_WIDEN || methods == OPTAB_LIB_WIDEN
	 || methods == OPTAB_MUST_WIDEN))
    {
      /* Caller says, don't even try.  */
      delete_insns_since (entry_last);
      return 0;
    }

  /* Compute the value of METHODS to pass to recursive calls.
     Don't allow widening to be tried recursively.  */

  methods = (methods == OPTAB_LIB_WIDEN ? OPTAB_LIB : OPTAB_DIRECT);

  /* Look for a wider mode of the same class for which it appears we can do
     the operation.  */

  if (class == MODE_INT || class == MODE_FLOAT || class == MODE_COMPLEX_FLOAT)
    {
      for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
	   wider_mode = GET_MODE_WIDER_MODE (wider_mode))
	{
	  if ((binoptab->handlers[(int) wider_mode].insn_code
	       != CODE_FOR_nothing)
	      || (methods == OPTAB_LIB
		  && binoptab->handlers[(int) wider_mode].libfunc))
	    {
	      rtx xop0 = op0, xop1 = op1;
	      int no_extend = 0;

	      /* For certain integer operations, we need not actually extend
		 the narrow operands, as long as we will truncate
		 the results to the same narrowness.  Don't do this when
		 WIDER_MODE is wider than a word since a paradoxical SUBREG
		 isn't valid for such modes.  */

	      if ((binoptab == ior_optab || binoptab == and_optab
		   || binoptab == xor_optab
		   || binoptab == add_optab || binoptab == sub_optab
		   || binoptab == smul_optab
		   || binoptab == ashl_optab || binoptab == lshl_optab)
		  && class == MODE_INT
		  && GET_MODE_SIZE (wider_mode) <= UNITS_PER_WORD)
		no_extend = 1;

	      /* If an operand is a constant integer, we might as well
		 convert it since that is more efficient than using a SUBREG,
		 unlike the case for other operands.  Similarly for
		 SUBREGs that were made due to promoted objects.*/

	      if (no_extend && GET_MODE (xop0) != VOIDmode
		&& ! (GET_CODE (xop0) == SUBREG
		      && SUBREG_PROMOTED_VAR_P (xop0)))
		xop0 = gen_rtx (SUBREG, wider_mode,
				force_reg (GET_MODE (xop0), xop0), 0);
	      else
		xop0 = convert_to_mode (wider_mode, xop0, unsignedp);

	      if (no_extend && GET_MODE (xop1) != VOIDmode
		&& ! (GET_CODE (xop1) == SUBREG
		      && SUBREG_PROMOTED_VAR_P (xop1)))
		xop1 = gen_rtx (SUBREG, wider_mode,
				force_reg (GET_MODE (xop1), xop1), 0);
	      else
		xop1 = convert_to_mode (wider_mode, xop1, unsignedp);

	      temp = expand_binop (wider_mode, binoptab, xop0, xop1, NULL_RTX,
				   unsignedp, methods);
	      if (temp)
		{
		  if (class != MODE_INT)
		    {
		      if (target == 0)
			target = gen_reg_rtx (mode);
		      convert_move (target, temp, 0);
		      return target;
		    }
		  else
		    return gen_lowpart (mode, temp);
		}
	      else
		delete_insns_since (last);
	    }
	}
    }

  delete_insns_since (entry_last);
  return 0;
}

/* Expand a binary operator which has both signed and unsigned forms.
   UOPTAB is the optab for unsigned operations, and SOPTAB is for
   signed operations.

   If we widen unsigned operands, we may use a signed wider operation instead
   of an unsigned wider operation, since the result would be the same.  */

rtx
sign_expand_binop (mode, uoptab, soptab, op0, op1, target, unsignedp, methods)
    enum machine_mode mode;
    optab uoptab, soptab;
    rtx op0, op1, target;
    int unsignedp;
    enum optab_methods methods;
{
  register rtx temp;
  optab direct_optab = unsignedp ? uoptab : soptab;
  struct optab wide_soptab;

  /* Do it without widening, if possible.  */
  temp = expand_binop (mode, direct_optab, op0, op1, target,
		       unsignedp, OPTAB_DIRECT);
  if (temp || methods == OPTAB_DIRECT)
    return temp;

  /* Try widening to a signed int.  Make a fake signed optab that
     hides any signed insn for direct use.  */
  wide_soptab = *soptab;
  wide_soptab.handlers[(int) mode].insn_code = CODE_FOR_nothing;
  wide_soptab.handlers[(int) mode].libfunc = 0;

  temp = expand_binop (mode, &wide_soptab, op0, op1, target,
		       unsignedp, OPTAB_WIDEN);

  /* For unsigned operands, try widening to an unsigned int.  */
  if (temp == 0 && unsignedp)
    temp = expand_binop (mode, uoptab, op0, op1, target,
			 unsignedp, OPTAB_WIDEN);
  if (temp || methods == OPTAB_WIDEN)
    return temp;

  /* Use the right width lib call if that exists.  */
  temp = expand_binop (mode, direct_optab, op0, op1, target, unsignedp, OPTAB_LIB);
  if (temp || methods == OPTAB_LIB)
    return temp;

  /* Must widen and use a lib call, use either signed or unsigned.  */
  temp = expand_binop (mode, &wide_soptab, op0, op1, target,
		       unsignedp, methods);
  if (temp != 0)
    return temp;
  if (unsignedp)
    return expand_binop (mode, uoptab, op0, op1, target,
			 unsignedp, methods);
  return 0;
}

/* Generate code to perform an operation specified by BINOPTAB
   on operands OP0 and OP1, with two results to TARG1 and TARG2.
   We assume that the order of the operands for the instruction
   is TARG0, OP0, OP1, TARG1, which would fit a pattern like
   [(set TARG0 (operate OP0 OP1)) (set TARG1 (operate ...))].

   Either TARG0 or TARG1 may be zero, but what that means is that
   that result is not actually wanted.  We will generate it into
   a dummy pseudo-reg and discard it.  They may not both be zero.

   Returns 1 if this operation can be performed; 0 if not.  */

int
expand_twoval_binop (binoptab, op0, op1, targ0, targ1, unsignedp)
     optab binoptab;
     rtx op0, op1;
     rtx targ0, targ1;
     int unsignedp;
{
  enum machine_mode mode = GET_MODE (targ0 ? targ0 : targ1);
  enum mode_class class;
  enum machine_mode wider_mode;
  rtx entry_last = get_last_insn ();
  rtx last;

  class = GET_MODE_CLASS (mode);

  op0 = protect_from_queue (op0, 0);
  op1 = protect_from_queue (op1, 0);

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  /* If we are inside an appropriately-short loop and one operand is an
     expensive constant, force it into a register.  */
  if (CONSTANT_P (op0) && preserve_subexpressions_p ()
      && rtx_cost (op0, binoptab->code) > 2)
    op0 = force_reg (mode, op0);

  if (CONSTANT_P (op1) && preserve_subexpressions_p ()
      && rtx_cost (op1, binoptab->code) > 2)
    op1 = force_reg (mode, op1);

  if (targ0)
    targ0 = protect_from_queue (targ0, 1);
  else
    targ0 = gen_reg_rtx (mode);
  if (targ1)
    targ1 = protect_from_queue (targ1, 1);
  else
    targ1 = gen_reg_rtx (mode);

  /* Record where to go back to if we fail.  */
  last = get_last_insn ();

  if (binoptab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) binoptab->handlers[(int) mode].insn_code;
      enum machine_mode mode0 = insn_operand_mode[icode][1];
      enum machine_mode mode1 = insn_operand_mode[icode][2];
      rtx pat;
      rtx xop0 = op0, xop1 = op1;

      /* In case this insn wants input operands in modes different from the
	 result, convert the operands.  */
      if (GET_MODE (op0) != VOIDmode && GET_MODE (op0) != mode0)
	xop0 = convert_to_mode (mode0, xop0, unsignedp);

      if (GET_MODE (op1) != VOIDmode && GET_MODE (op1) != mode1)
	xop1 = convert_to_mode (mode1, xop1, unsignedp);

      /* Now, if insn doesn't accept these operands, put them into pseudos.  */
      if (! (*insn_operand_predicate[icode][1]) (xop0, mode0))
	xop0 = copy_to_mode_reg (mode0, xop0);

      if (! (*insn_operand_predicate[icode][2]) (xop1, mode1))
	xop1 = copy_to_mode_reg (mode1, xop1);

      /* We could handle this, but we should always be called with a pseudo
	 for our targets and all insns should take them as outputs.  */
      if (! (*insn_operand_predicate[icode][0]) (targ0, mode)
	  || ! (*insn_operand_predicate[icode][3]) (targ1, mode))
	abort ();
	
      pat = GEN_FCN (icode) (targ0, xop0, xop1, targ1);
      if (pat)
	{
	  emit_insn (pat);
	  return 1;
	}
      else
	delete_insns_since (last);
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if (class == MODE_INT || class == MODE_FLOAT || class == MODE_COMPLEX_FLOAT)
    {
      for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
	   wider_mode = GET_MODE_WIDER_MODE (wider_mode))
	{
	  if (binoptab->handlers[(int) wider_mode].insn_code
	      != CODE_FOR_nothing)
	    {
	      register rtx t0 = gen_reg_rtx (wider_mode);
	      register rtx t1 = gen_reg_rtx (wider_mode);

	      if (expand_twoval_binop (binoptab,
				       convert_to_mode (wider_mode, op0,
							unsignedp),
				       convert_to_mode (wider_mode, op1,
							unsignedp),
				       t0, t1, unsignedp))
		{
		  convert_move (targ0, t0, unsignedp);
		  convert_move (targ1, t1, unsignedp);
		  return 1;
		}
	      else
		delete_insns_since (last);
	    }
	}
    }

  delete_insns_since (entry_last);
  return 0;
}

/* Generate code to perform an operation specified by UNOPTAB
   on operand OP0, with result having machine-mode MODE.

   UNSIGNEDP is for the case where we have to widen the operands
   to perform the operation.  It says to use zero-extension.

   If TARGET is nonzero, the value
   is generated there, if it is convenient to do so.
   In all cases an rtx is returned for the locus of the value;
   this may or may not be TARGET.  */

rtx
expand_unop (mode, unoptab, op0, target, unsignedp)
     enum machine_mode mode;
     optab unoptab;
     rtx op0;
     rtx target;
     int unsignedp;
{
  enum mode_class class;
  enum machine_mode wider_mode;
  register rtx temp;
  rtx last = get_last_insn ();
  rtx pat;

  class = GET_MODE_CLASS (mode);

  op0 = protect_from_queue (op0, 0);

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
    }

  if (target)
    target = protect_from_queue (target, 1);

  if (unoptab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) unoptab->handlers[(int) mode].insn_code;
      enum machine_mode mode0 = insn_operand_mode[icode][1];
      rtx xop0 = op0;

      if (target)
	temp = target;
      else
	temp = gen_reg_rtx (mode);

      if (GET_MODE (xop0) != VOIDmode
	  && GET_MODE (xop0) != mode0)
	xop0 = convert_to_mode (mode0, xop0, unsignedp);

      /* Now, if insn doesn't accept our operand, put it into a pseudo.  */

      if (! (*insn_operand_predicate[icode][1]) (xop0, mode0))
	xop0 = copy_to_mode_reg (mode0, xop0);

      if (! (*insn_operand_predicate[icode][0]) (temp, mode))
	temp = gen_reg_rtx (mode);

      pat = GEN_FCN (icode) (temp, xop0);
      if (pat)
	{
	  if (GET_CODE (pat) == SEQUENCE
	      && ! add_equal_note (pat, temp, unoptab->code, xop0, NULL_RTX))
	    {
	      delete_insns_since (last);
	      return expand_unop (mode, unoptab, op0, NULL_RTX, unsignedp);
	    }

	  emit_insn (pat);
	  
	  return temp;
	}
      else
	delete_insns_since (last);
    }

  /* It can't be done in this mode.  Can we open-code it in a wider mode?  */

  if (class == MODE_INT || class == MODE_FLOAT || class == MODE_COMPLEX_FLOAT)
    for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
	 wider_mode = GET_MODE_WIDER_MODE (wider_mode))
      {
	if (unoptab->handlers[(int) wider_mode].insn_code != CODE_FOR_nothing)
	  {
	    rtx xop0 = op0;

	    /* For certain operations, we need not actually extend
	       the narrow operand, as long as we will truncate the
	       results to the same narrowness.  But it is faster to
	       convert a SUBREG due to mode promotion.  */

	    if ((unoptab == neg_optab || unoptab == one_cmpl_optab)
		&& GET_MODE_SIZE (wider_mode) <= UNITS_PER_WORD
		&& class == MODE_INT
		&& ! (GET_CODE (xop0) == SUBREG
		      && SUBREG_PROMOTED_VAR_P (xop0)))
	      xop0 = gen_rtx (SUBREG, wider_mode, force_reg (mode, xop0), 0);
	    else
	      xop0 = convert_to_mode (wider_mode, xop0, unsignedp);
	      
	    temp = expand_unop (wider_mode, unoptab, xop0, NULL_RTX,
				unsignedp);

	    if (temp)
	      {
		if (class != MODE_INT)
		  {
		    if (target == 0)
		      target = gen_reg_rtx (mode);
		    convert_move (target, temp, 0);
		    return target;
		  }
		else
		  return gen_lowpart (mode, temp);
	      }
	    else
	      delete_insns_since (last);
	  }
      }

  /* These can be done a word at a time.  */
  if (unoptab == one_cmpl_optab
      && class == MODE_INT
      && GET_MODE_SIZE (mode) > UNITS_PER_WORD
      && unoptab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing)
    {
      int i;
      rtx insns;

      if (target == 0 || target == op0)
	target = gen_reg_rtx (mode);

      start_sequence ();

      /* Do the actual arithmetic.  */
      for (i = 0; i < GET_MODE_BITSIZE (mode) / BITS_PER_WORD; i++)
	{
	  rtx target_piece = operand_subword (target, i, 1, mode);
	  rtx x = expand_unop (word_mode, unoptab,
			       operand_subword_force (op0, i, mode),
			       target_piece, unsignedp);
	  if (target_piece != x)
	    emit_move_insn (target_piece, x);
	}

      insns = get_insns ();
      end_sequence ();

      emit_no_conflict_block (insns, target, op0, NULL_RTX,
			      gen_rtx (unoptab->code, mode, op0));
      return target;
    }

  /* Open-code the complex negation operation.  */
  else if (unoptab == neg_optab
	   && (class == MODE_COMPLEX_FLOAT || class == MODE_COMPLEX_INT))
    {
      rtx target_piece;
      rtx x;
      rtx seq;

      /* Find the correct mode for the real and imaginary parts */
      enum machine_mode submode
	= mode_for_size (GET_MODE_UNIT_SIZE (mode) * BITS_PER_UNIT,
			 class == MODE_COMPLEX_INT ? MODE_INT : MODE_FLOAT,
			 0);

      if (submode == BLKmode)
	abort ();

      if (target == 0)
	target = gen_reg_rtx (mode);
      
      start_sequence ();

      target_piece = gen_imagpart (submode, target);
      x = expand_unop (submode, unoptab,
		       gen_imagpart (submode, op0),
		       target_piece, unsignedp);
      if (target_piece != x)
	emit_move_insn (target_piece, x);

      target_piece = gen_realpart (submode, target);
      x = expand_unop (submode, unoptab,
		       gen_realpart (submode, op0),
		       target_piece, unsignedp);
      if (target_piece != x)
	emit_move_insn (target_piece, x);

      seq = get_insns ();
      end_sequence ();

      emit_no_conflict_block (seq, target, op0, 0,
			      gen_rtx (unoptab->code, mode, op0));
      return target;
    }

  /* Now try a library call in this mode.  */
  if (unoptab->handlers[(int) mode].libfunc)
    {
      rtx insns;
      rtx funexp = unoptab->handlers[(int) mode].libfunc;

      start_sequence ();

      /* Pass 1 for NO_QUEUE so we don't lose any increments
	 if the libcall is cse'd or moved.  */
      emit_library_call (unoptab->handlers[(int) mode].libfunc,
			 1, mode, 1, op0, mode);
      insns = get_insns ();
      end_sequence ();

      target = gen_reg_rtx (mode);
      emit_libcall_block (insns, target, hard_libcall_value (mode),
			  gen_rtx (unoptab->code, mode, op0));

      return target;
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if (class == MODE_INT || class == MODE_FLOAT || class == MODE_COMPLEX_FLOAT)
    {
      for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
	   wider_mode = GET_MODE_WIDER_MODE (wider_mode))
	{
	  if ((unoptab->handlers[(int) wider_mode].insn_code
	       != CODE_FOR_nothing)
	      || unoptab->handlers[(int) wider_mode].libfunc)
	    {
	      rtx xop0 = op0;

	      /* For certain operations, we need not actually extend
		 the narrow operand, as long as we will truncate the
		 results to the same narrowness.  */

	      if ((unoptab == neg_optab || unoptab == one_cmpl_optab)
		  && GET_MODE_SIZE (wider_mode) <= UNITS_PER_WORD
		  && class == MODE_INT
		  && ! (GET_CODE (xop0) == SUBREG
			&& SUBREG_PROMOTED_VAR_P (xop0)))
		xop0 = gen_rtx (SUBREG, wider_mode, force_reg (mode, xop0), 0);
	      else
		xop0 = convert_to_mode (wider_mode, xop0, unsignedp);
	      
	      temp = expand_unop (wider_mode, unoptab, xop0, NULL_RTX,
				  unsignedp);

	      if (temp)
		{
		  if (class != MODE_INT)
		    {
		      if (target == 0)
			target = gen_reg_rtx (mode);
		      convert_move (target, temp, 0);
		      return target;
		    }
		  else
		    return gen_lowpart (mode, temp);
		}
	      else
		delete_insns_since (last);
	    }
	}
    }

  return 0;
}

/* Emit code to compute the absolute value of OP0, with result to
   TARGET if convenient.  (TARGET may be 0.)  The return value says
   where the result actually is to be found.

   MODE is the mode of the operand; the mode of the result is
   different but can be deduced from MODE.

   UNSIGNEDP is relevant for complex integer modes.  */

rtx
expand_complex_abs (mode, op0, target, unsignedp)
     enum machine_mode mode;
     rtx op0;
     rtx target;
     int unsignedp;
{
  enum mode_class class = GET_MODE_CLASS (mode);
  enum machine_mode wider_mode;
  register rtx temp;
  rtx entry_last = get_last_insn ();
  rtx last;
  rtx pat;

  /* Find the correct mode for the real and imaginary parts.  */
  enum machine_mode submode
    = mode_for_size (GET_MODE_UNIT_SIZE (mode) * BITS_PER_UNIT,
		     class == MODE_COMPLEX_INT ? MODE_INT : MODE_FLOAT,
		     0);

  if (submode == BLKmode)
    abort ();

  op0 = protect_from_queue (op0, 0);

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
    }

  last = get_last_insn ();

  if (target)
    target = protect_from_queue (target, 1);

  if (abs_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) abs_optab->handlers[(int) mode].insn_code;
      enum machine_mode mode0 = insn_operand_mode[icode][1];
      rtx xop0 = op0;

      if (target)
	temp = target;
      else
	temp = gen_reg_rtx (submode);

      if (GET_MODE (xop0) != VOIDmode
	  && GET_MODE (xop0) != mode0)
	xop0 = convert_to_mode (mode0, xop0, unsignedp);

      /* Now, if insn doesn't accept our operand, put it into a pseudo.  */

      if (! (*insn_operand_predicate[icode][1]) (xop0, mode0))
	xop0 = copy_to_mode_reg (mode0, xop0);

      if (! (*insn_operand_predicate[icode][0]) (temp, submode))
	temp = gen_reg_rtx (submode);

      pat = GEN_FCN (icode) (temp, xop0);
      if (pat)
	{
	  if (GET_CODE (pat) == SEQUENCE
	      && ! add_equal_note (pat, temp, abs_optab->code, xop0, NULL_RTX))
	    {
	      delete_insns_since (last);
	      return expand_unop (mode, abs_optab, op0, NULL_RTX, unsignedp);
	    }

	  emit_insn (pat);
	  
	  return temp;
	}
      else
	delete_insns_since (last);
    }

  /* It can't be done in this mode.  Can we open-code it in a wider mode?  */

  for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
       wider_mode = GET_MODE_WIDER_MODE (wider_mode))
    {
      if (abs_optab->handlers[(int) wider_mode].insn_code != CODE_FOR_nothing)
	{
	  rtx xop0 = op0;

	  xop0 = convert_to_mode (wider_mode, xop0, unsignedp);
	  temp = expand_complex_abs (wider_mode, xop0, NULL_RTX, unsignedp);

	  if (temp)
	    {
	      if (class != MODE_COMPLEX_INT)
		{
		  if (target == 0)
		    target = gen_reg_rtx (submode);
		  convert_move (target, temp, 0);
		  return target;
		}
	      else
		return gen_lowpart (submode, temp);
	    }
	  else
	    delete_insns_since (last);
	}
    }

  /* Open-code the complex absolute-value operation
     if we can open-code sqrt.  Otherwise it's not worth while.  */
  if (sqrt_optab->handlers[(int) submode].insn_code != CODE_FOR_nothing)
    {
      rtx real, imag, total;

      real = gen_realpart (submode, op0);
      imag = gen_imagpart (submode, op0);
      /* Square both parts.  */
      real = expand_mult (mode, real, real, NULL_RTX, 0);
      imag = expand_mult (mode, imag, imag, NULL_RTX, 0);
      /* Sum the parts.  */
      total = expand_binop (submode, add_optab, real, imag, 0,
			    0, OPTAB_LIB_WIDEN);
      /* Get sqrt in TARGET.  Set TARGET to where the result is.  */
      target = expand_unop (submode, sqrt_optab, total, target, 0);
      if (target == 0)
	delete_insns_since (last);
      else
	return target;
    }

  /* Now try a library call in this mode.  */
  if (abs_optab->handlers[(int) mode].libfunc)
    {
      rtx insns;
      rtx funexp = abs_optab->handlers[(int) mode].libfunc;

      start_sequence ();

      /* Pass 1 for NO_QUEUE so we don't lose any increments
	 if the libcall is cse'd or moved.  */
      emit_library_call (abs_optab->handlers[(int) mode].libfunc,
			 1, mode, 1, op0, mode);
      insns = get_insns ();
      end_sequence ();

      target = gen_reg_rtx (submode);
      emit_libcall_block (insns, target, hard_libcall_value (submode),
			  gen_rtx (abs_optab->code, mode, op0));

      return target;
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
       wider_mode = GET_MODE_WIDER_MODE (wider_mode))
    {
      if ((abs_optab->handlers[(int) wider_mode].insn_code
	   != CODE_FOR_nothing)
	  || abs_optab->handlers[(int) wider_mode].libfunc)
	{
	  rtx xop0 = op0;

	  xop0 = convert_to_mode (wider_mode, xop0, unsignedp);

	  temp = expand_complex_abs (wider_mode, xop0, NULL_RTX, unsignedp);

	  if (temp)
	    {
	      if (class != MODE_COMPLEX_INT)
		{
		  if (target == 0)
		    target = gen_reg_rtx (submode);
		  convert_move (target, temp, 0);
		  return target;
		}
	      else
		return gen_lowpart (submode, temp);
	    }
	  else
	    delete_insns_since (last);
	}
    }

  delete_insns_since (entry_last);
  return 0;
}

/* Generate an instruction whose insn-code is INSN_CODE,
   with two operands: an output TARGET and an input OP0.
   TARGET *must* be nonzero, and the output is always stored there.
   CODE is an rtx code such that (CODE OP0) is an rtx that describes
   the value that is stored into TARGET.  */

void
emit_unop_insn (icode, target, op0, code)
     int icode;
     rtx target;
     rtx op0;
     enum rtx_code code;
{
  register rtx temp;
  enum machine_mode mode0 = insn_operand_mode[icode][1];
  rtx pat;

  temp = target = protect_from_queue (target, 1);

  op0 = protect_from_queue (op0, 0);

  if (flag_force_mem)
    op0 = force_not_mem (op0);

  /* Now, if insn does not accept our operands, put them into pseudos.  */

  if (! (*insn_operand_predicate[icode][1]) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  if (! (*insn_operand_predicate[icode][0]) (temp, GET_MODE (temp))
      || (flag_force_mem && GET_CODE (temp) == MEM))
    temp = gen_reg_rtx (GET_MODE (temp));

  pat = GEN_FCN (icode) (temp, op0);

  if (GET_CODE (pat) == SEQUENCE && code != UNKNOWN)
    add_equal_note (pat, temp, code, op0, NULL_RTX);
  
  emit_insn (pat);

  if (temp != target)
    emit_move_insn (target, temp);
}

/* Emit code to perform a series of operations on a multi-word quantity, one
   word at a time.

   Such a block is preceded by a CLOBBER of the output, consists of multiple
   insns, each setting one word of the output, and followed by a SET copying
   the output to itself.

   Each of the insns setting words of the output receives a REG_NO_CONFLICT
   note indicating that it doesn't conflict with the (also multi-word)
   inputs.  The entire block is surrounded by REG_LIBCALL and REG_RETVAL
   notes.

   INSNS is a block of code generated to perform the operation, not including
   the CLOBBER and final copy.  All insns that compute intermediate values
   are first emitted, followed by the block as described above.  Only
   INSNs are allowed in the block; no library calls or jumps may be
   present.

   TARGET, OP0, and OP1 are the output and inputs of the operations,
   respectively.  OP1 may be zero for a unary operation.

   EQUIV, if non-zero, is an expression to be placed into a REG_EQUAL note
   on the last insn.

   If TARGET is not a register, INSNS is simply emitted with no special
   processing.

   The final insn emitted is returned.  */

rtx
emit_no_conflict_block (insns, target, op0, op1, equiv)
     rtx insns;
     rtx target;
     rtx op0, op1;
     rtx equiv;
{
  rtx prev, next, first, last, insn;

  if (GET_CODE (target) != REG || reload_in_progress)
    return emit_insns (insns);

  /* First emit all insns that do not store into words of the output and remove
     these from the list.  */
  for (insn = insns; insn; insn = next)
    {
      rtx set = 0;
      int i;

      next = NEXT_INSN (insn);

      if (GET_CODE (insn) != INSN)
	abort ();

      if (GET_CODE (PATTERN (insn)) == SET)
	set = PATTERN (insn);
      else if (GET_CODE (PATTERN (insn)) == PARALLEL)
	{
	  for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	    if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) == SET)
	      {
		set = XVECEXP (PATTERN (insn), 0, i);
		break;
	      }
	}

      if (set == 0)
	abort ();

      if (! reg_overlap_mentioned_p (target, SET_DEST (set)))
	{
	  if (PREV_INSN (insn))
	    NEXT_INSN (PREV_INSN (insn)) = next;
	  else
	    insns = next;

	  if (next)
	    PREV_INSN (next) = PREV_INSN (insn);

	  add_insn (insn);
	}
    }

  prev = get_last_insn ();

  /* Now write the CLOBBER of the output, followed by the setting of each
     of the words, followed by the final copy.  */
  if (target != op0 && target != op1)
    emit_insn (gen_rtx (CLOBBER, VOIDmode, target));

  for (insn = insns; insn; insn = next)
    {
      next = NEXT_INSN (insn);
      add_insn (insn);

      if (op1 && GET_CODE (op1) == REG)
	REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_NO_CONFLICT, op1,
				    REG_NOTES (insn));

      if (op0 && GET_CODE (op0) == REG)
	REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_NO_CONFLICT, op0,
				    REG_NOTES (insn));
    }

  last = emit_move_insn (target, target);
  if (equiv)
    REG_NOTES (last) = gen_rtx (EXPR_LIST, REG_EQUAL, equiv, REG_NOTES (last));

  if (prev == 0)
    first = get_insns ();
  else
    first = NEXT_INSN (prev);

  /* Encapsulate the block so it gets manipulated as a unit.  */
  REG_NOTES (first) = gen_rtx (INSN_LIST, REG_LIBCALL, last,
			       REG_NOTES (first));
  REG_NOTES (last) = gen_rtx (INSN_LIST, REG_RETVAL, first, REG_NOTES (last));

  return last;
}

/* Emit code to make a call to a constant function or a library call.

   INSNS is a list containing all insns emitted in the call.
   These insns leave the result in RESULT.  Our block is to copy RESULT
   to TARGET, which is logically equivalent to EQUIV.

   We first emit any insns that set a pseudo on the assumption that these are
   loading constants into registers; doing so allows them to be safely cse'ed
   between blocks.  Then we emit all the other insns in the block, followed by
   an insn to move RESULT to TARGET.  This last insn will have a REQ_EQUAL
   note with an operand of EQUIV.

   Moving assignments to pseudos outside of the block is done to improve
   the generated code, but is not required to generate correct code,
   hence being unable to move an assignment is not grounds for not making
   a libcall block.  There are two reasons why it is safe to leave these
   insns inside the block: First, we know that these pseudos cannot be
   used in generated RTL outside the block since they are created for
   temporary purposes within the block.  Second, CSE will not record the
   values of anything set inside a libcall block, so we know they must
   be dead at the end of the block.

   Except for the first group of insns (the ones setting pseudos), the
   block is delimited by REG_RETVAL and REG_LIBCALL notes.  */

void
emit_libcall_block (insns, target, result, equiv)
     rtx insns;
     rtx target;
     rtx result;
     rtx equiv;
{
  rtx prev, next, first, last, insn;

  /* First emit all insns that set pseudos.  Remove them from the list as
     we go.  Avoid insns that set pseudo which were referenced in previous
     insns.  These can be generated by move_by_pieces, for example,
     to update an address.  */

  for (insn = insns; insn; insn = next)
    {
      rtx set = single_set (insn);

      next = NEXT_INSN (insn);

      if (set != 0 && GET_CODE (SET_DEST (set)) == REG
	  && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER
	  && (insn == insns
	      || (! reg_mentioned_p (SET_DEST (set), PATTERN (insns))
		  && ! reg_used_between_p (SET_DEST (set), insns, insn))))
	{
	  if (PREV_INSN (insn))
	    NEXT_INSN (PREV_INSN (insn)) = next;
	  else
	    insns = next;

	  if (next)
	    PREV_INSN (next) = PREV_INSN (insn);

	  add_insn (insn);
	}
    }

  prev = get_last_insn ();

  /* Write the remaining insns followed by the final copy.  */

  for (insn = insns; insn; insn = next)
    {
      next = NEXT_INSN (insn);

      add_insn (insn);
    }

  last = emit_move_insn (target, result);
  REG_NOTES (last) = gen_rtx (EXPR_LIST, REG_EQUAL, equiv, REG_NOTES (last));

  if (prev == 0)
    first = get_insns ();
  else
    first = NEXT_INSN (prev);

  /* Encapsulate the block so it gets manipulated as a unit.  */
  REG_NOTES (first) = gen_rtx (INSN_LIST, REG_LIBCALL, last,
			       REG_NOTES (first));
  REG_NOTES (last) = gen_rtx (INSN_LIST, REG_RETVAL, first, REG_NOTES (last));
}

/* Generate code to store zero in X.  */

void
emit_clr_insn (x)
     rtx x;
{
  emit_move_insn (x, const0_rtx);
}

/* Generate code to store 1 in X
   assuming it contains zero beforehand.  */

void
emit_0_to_1_insn (x)
     rtx x;
{
  emit_move_insn (x, const1_rtx);
}

/* Generate code to compare X with Y
   so that the condition codes are set.

   MODE is the mode of the inputs (in case they are const_int).
   UNSIGNEDP nonzero says that X and Y are unsigned;
   this matters if they need to be widened.

   If they have mode BLKmode, then SIZE specifies the size of both X and Y,
   and ALIGN specifies the known shared alignment of X and Y.

   COMPARISON is the rtl operator to compare with (EQ, NE, GT, etc.).
   It is ignored for fixed-point and block comparisons;
   it is used only for floating-point comparisons.  */

void
emit_cmp_insn (x, y, comparison, size, mode, unsignedp, align)
     rtx x, y;
     enum rtx_code comparison;
     rtx size;
     enum machine_mode mode;
     int unsignedp;
     int align;
{
  enum mode_class class;
  enum machine_mode wider_mode;

  class = GET_MODE_CLASS (mode);

  /* They could both be VOIDmode if both args are immediate constants,
     but we should fold that at an earlier stage.
     With no special code here, this will call abort,
     reminding the programmer to implement such folding.  */

  if (mode != BLKmode && flag_force_mem)
    {
      x = force_not_mem (x);
      y = force_not_mem (y);
    }

  /* If we are inside an appropriately-short loop and one operand is an
     expensive constant, force it into a register.  */
  if (CONSTANT_P (x) && preserve_subexpressions_p () && rtx_cost (x, COMPARE) > 2)
    x = force_reg (mode, x);

  if (CONSTANT_P (y) && preserve_subexpressions_p () && rtx_cost (y, COMPARE) > 2)
    y = force_reg (mode, y);

  /* Don't let both operands fail to indicate the mode.  */
  if (GET_MODE (x) == VOIDmode && GET_MODE (y) == VOIDmode)
    x = force_reg (mode, x);

  /* Handle all BLKmode compares.  */

  if (mode == BLKmode)
    {
      emit_queue ();
      x = protect_from_queue (x, 0);
      y = protect_from_queue (y, 0);

      if (size == 0)
	abort ();
#ifdef HAVE_cmpstrqi
      if (HAVE_cmpstrqi
	  && GET_CODE (size) == CONST_INT
	  && INTVAL (size) < (1 << GET_MODE_BITSIZE (QImode)))
	{
	  enum machine_mode result_mode
	    = insn_operand_mode[(int) CODE_FOR_cmpstrqi][0];
	  rtx result = gen_reg_rtx (result_mode);
	  emit_insn (gen_cmpstrqi (result, x, y, size, GEN_INT (align)));
	  emit_cmp_insn (result, const0_rtx, comparison, NULL_RTX,
			 result_mode, 0, 0);
	}
      else
#endif
#ifdef HAVE_cmpstrhi
      if (HAVE_cmpstrhi
	  && GET_CODE (size) == CONST_INT
	  && INTVAL (size) < (1 << GET_MODE_BITSIZE (HImode)))
	{
	  enum machine_mode result_mode
	    = insn_operand_mode[(int) CODE_FOR_cmpstrhi][0];
	  rtx result = gen_reg_rtx (result_mode);
	  emit_insn (gen_cmpstrhi (result, x, y, size, GEN_INT (align)));
	  emit_cmp_insn (result, const0_rtx, comparison, NULL_RTX,
			 result_mode, 0, 0);
	}
      else
#endif
#ifdef HAVE_cmpstrsi
      if (HAVE_cmpstrsi)
	{
	  enum machine_mode result_mode
	    = insn_operand_mode[(int) CODE_FOR_cmpstrsi][0];
	  rtx result = gen_reg_rtx (result_mode);
	  size = protect_from_queue (size, 0);
	  emit_insn (gen_cmpstrsi (result, x, y,
				   convert_to_mode (SImode, size, 1),
				   GEN_INT (align)));
	  emit_cmp_insn (result, const0_rtx, comparison, NULL_RTX,
			 result_mode, 0, 0);
	}
      else
#endif
	{
#ifdef TARGET_MEM_FUNCTIONS
	  emit_library_call (memcmp_libfunc, 0,
			     TYPE_MODE (integer_type_node), 3,
			     XEXP (x, 0), Pmode, XEXP (y, 0), Pmode,
			     size, Pmode);
#else
	  emit_library_call (bcmp_libfunc, 0,
			     TYPE_MODE (integer_type_node), 3,
			     XEXP (x, 0), Pmode, XEXP (y, 0), Pmode,
			     size, Pmode);
#endif
	  emit_cmp_insn (hard_libcall_value (TYPE_MODE (integer_type_node)),
			 const0_rtx, comparison, NULL_RTX,
			 TYPE_MODE (integer_type_node), 0, 0);
	}
      return;
    }

  /* Handle some compares against zero.  */

  if (y == CONST0_RTX (mode)
      && tst_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) tst_optab->handlers[(int) mode].insn_code;

      emit_queue ();
      x = protect_from_queue (x, 0);
      y = protect_from_queue (y, 0);

      /* Now, if insn does accept these operands, put them into pseudos.  */
      if (! (*insn_operand_predicate[icode][0])
	  (x, insn_operand_mode[icode][0]))
	x = copy_to_mode_reg (insn_operand_mode[icode][0], x);

      emit_insn (GEN_FCN (icode) (x));
      return;
    }

  /* Handle compares for which there is a directly suitable insn.  */

  if (cmp_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) cmp_optab->handlers[(int) mode].insn_code;

      emit_queue ();
      x = protect_from_queue (x, 0);
      y = protect_from_queue (y, 0);

      /* Now, if insn doesn't accept these operands, put them into pseudos.  */
      if (! (*insn_operand_predicate[icode][0])
	  (x, insn_operand_mode[icode][0]))
	x = copy_to_mode_reg (insn_operand_mode[icode][0], x);

      if (! (*insn_operand_predicate[icode][1])
	  (y, insn_operand_mode[icode][1]))
	y = copy_to_mode_reg (insn_operand_mode[icode][1], y);

      emit_insn (GEN_FCN (icode) (x, y));
      return;
    }

  /* Try widening if we can find a direct insn that way.  */

  if (class == MODE_INT || class == MODE_FLOAT || class == MODE_COMPLEX_FLOAT)
    {
      for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
	   wider_mode = GET_MODE_WIDER_MODE (wider_mode))
	{
	  if (cmp_optab->handlers[(int) wider_mode].insn_code
	      != CODE_FOR_nothing)
	    {
	      x = protect_from_queue (x, 0);
	      y = protect_from_queue (y, 0);
	      x = convert_to_mode (wider_mode, x, unsignedp);
	      y = convert_to_mode (wider_mode, y, unsignedp);
	      emit_cmp_insn (x, y, comparison, NULL_RTX,
			     wider_mode, unsignedp, align);
	      return;
	    }
	}
    }

  /* Handle a lib call just for the mode we are using.  */

  if (cmp_optab->handlers[(int) mode].libfunc
      && class != MODE_FLOAT)
    {
      rtx libfunc = cmp_optab->handlers[(int) mode].libfunc;
      /* If we want unsigned, and this mode has a distinct unsigned
	 comparison routine, use that.  */
      if (unsignedp && ucmp_optab->handlers[(int) mode].libfunc)
	libfunc = ucmp_optab->handlers[(int) mode].libfunc;

      emit_library_call (libfunc, 1,
			 SImode, 2, x, mode, y, mode);

      /* Integer comparison returns a result that must be compared against 1,
	 so that even if we do an unsigned compare afterward,
	 there is still a value that can represent the result "less than".  */

      emit_cmp_insn (hard_libcall_value (SImode), const1_rtx,
		     comparison, NULL_RTX, SImode, unsignedp, 0);
      return;
    }

  if (class == MODE_FLOAT)
    emit_float_lib_cmp (x, y, comparison);

  else
    abort ();
}

/* Nonzero if a compare of mode MODE can be done straightforwardly
   (without splitting it into pieces).  */

int
can_compare_p (mode)
     enum machine_mode mode;
{
  do
    {
      if (cmp_optab->handlers[(int)mode].insn_code != CODE_FOR_nothing)
	return 1;
      mode = GET_MODE_WIDER_MODE (mode);
    } while (mode != VOIDmode);

  return 0;
}

/* Emit a library call comparison between floating point X and Y.
   COMPARISON is the rtl operator to compare with (EQ, NE, GT, etc.).  */

static void
emit_float_lib_cmp (x, y, comparison)
     rtx x, y;
     enum rtx_code comparison;
{
  enum machine_mode mode = GET_MODE (x);
  rtx libfunc;

  if (mode == SFmode)
    switch (comparison)
      {
      case EQ:
	libfunc = eqsf2_libfunc;
	break;

      case NE:
	libfunc = nesf2_libfunc;
	break;

      case GT:
	libfunc = gtsf2_libfunc;
	break;

      case GE:
	libfunc = gesf2_libfunc;
	break;

      case LT:
	libfunc = ltsf2_libfunc;
	break;

      case LE:
	libfunc = lesf2_libfunc;
	break;
      }
  else if (mode == DFmode)
    switch (comparison)
      {
      case EQ:
	libfunc = eqdf2_libfunc;
	break;

      case NE:
	libfunc = nedf2_libfunc;
	break;

      case GT:
	libfunc = gtdf2_libfunc;
	break;

      case GE:
	libfunc = gedf2_libfunc;
	break;

      case LT:
	libfunc = ltdf2_libfunc;
	break;

      case LE:
	libfunc = ledf2_libfunc;
	break;
      }
  else if (mode == XFmode)
    switch (comparison)
      {
      case EQ:
	libfunc = eqxf2_libfunc;
	break;

      case NE:
	libfunc = nexf2_libfunc;
	break;

      case GT:
	libfunc = gtxf2_libfunc;
	break;

      case GE:
	libfunc = gexf2_libfunc;
	break;

      case LT:
	libfunc = ltxf2_libfunc;
	break;

      case LE:
	libfunc = lexf2_libfunc;
	break;
      }
  else if (mode == TFmode)
    switch (comparison)
      {
      case EQ:
	libfunc = eqtf2_libfunc;
	break;

      case NE:
	libfunc = netf2_libfunc;
	break;

      case GT:
	libfunc = gttf2_libfunc;
	break;

      case GE:
	libfunc = getf2_libfunc;
	break;

      case LT:
	libfunc = lttf2_libfunc;
	break;

      case LE:
	libfunc = letf2_libfunc;
	break;
      }
  else
    {
      enum machine_mode wider_mode;

      for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
	   wider_mode = GET_MODE_WIDER_MODE (wider_mode))
	{
	  if ((cmp_optab->handlers[(int) wider_mode].insn_code
	       != CODE_FOR_nothing)
	      || (cmp_optab->handlers[(int) wider_mode].libfunc != 0))
	    {
	      x = protect_from_queue (x, 0);
	      y = protect_from_queue (y, 0);
	      x = convert_to_mode (wider_mode, x, 0);
	      y = convert_to_mode (wider_mode, y, 0);
	      emit_float_lib_cmp (x, y, comparison);
	      return;
	    }
	}
      abort ();
    }

  emit_library_call (libfunc, 1,
		     SImode, 2, x, mode, y, mode);

  emit_cmp_insn (hard_libcall_value (SImode), const0_rtx, comparison,
		 NULL_RTX, SImode, 0, 0);
}

/* Generate code to indirectly jump to a location given in the rtx LOC.  */

void
emit_indirect_jump (loc)
     rtx loc;
{
  if (! ((*insn_operand_predicate[(int)CODE_FOR_indirect_jump][0])
	 (loc, VOIDmode)))
    loc = copy_to_mode_reg (insn_operand_mode[(int)CODE_FOR_indirect_jump][0],
			    loc);

  emit_jump_insn (gen_indirect_jump (loc));
  emit_barrier ();
}

/* These three functions generate an insn body and return it
   rather than emitting the insn.

   They do not protect from queued increments,
   because they may be used 1) in protect_from_queue itself
   and 2) in other passes where there is no queue.  */

/* Generate and return an insn body to add Y to X.  */

rtx
gen_add2_insn (x, y)
     rtx x, y;
{
  int icode = (int) add_optab->handlers[(int) GET_MODE (x)].insn_code; 

  if (! (*insn_operand_predicate[icode][0]) (x, insn_operand_mode[icode][0])
      || ! (*insn_operand_predicate[icode][1]) (x, insn_operand_mode[icode][1])
      || ! (*insn_operand_predicate[icode][2]) (y, insn_operand_mode[icode][2]))
    abort ();

  return (GEN_FCN (icode) (x, x, y));
}

int
have_add2_insn (mode)
     enum machine_mode mode;
{
  return add_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing;
}

/* Generate and return an insn body to subtract Y from X.  */

rtx
gen_sub2_insn (x, y)
     rtx x, y;
{
  int icode = (int) sub_optab->handlers[(int) GET_MODE (x)].insn_code; 

  if (! (*insn_operand_predicate[icode][0]) (x, insn_operand_mode[icode][0])
      || ! (*insn_operand_predicate[icode][1]) (x, insn_operand_mode[icode][1])
      || ! (*insn_operand_predicate[icode][2]) (y, insn_operand_mode[icode][2]))
    abort ();

  return (GEN_FCN (icode) (x, x, y));
}

int
have_sub2_insn (mode)
     enum machine_mode mode;
{
  return sub_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing;
}

/* Generate the body of an instruction to copy Y into X.  */

rtx
gen_move_insn (x, y)
     rtx x, y;
{
  register enum machine_mode mode = GET_MODE (x);
  enum insn_code insn_code;

  if (mode == VOIDmode)
    mode = GET_MODE (y); 

  insn_code = mov_optab->handlers[(int) mode].insn_code;

  /* Handle MODE_CC modes:  If we don't have a special move insn for this mode,
     find a mode to do it in.  If we have a movcc, use it.  Otherwise,
     find the MODE_INT mode of the same width.  */

  if (insn_code == CODE_FOR_nothing)
    {
      enum machine_mode tmode = VOIDmode;
      rtx x1 = x, y1 = y;

      if (GET_MODE_CLASS (mode) == MODE_CC && mode != CCmode
	  && mov_optab->handlers[(int) CCmode].insn_code != CODE_FOR_nothing)
	tmode = CCmode;
      else if (GET_MODE_CLASS (mode) == MODE_CC)
	for (tmode = QImode; tmode != VOIDmode;
	     tmode = GET_MODE_WIDER_MODE (tmode))
	  if (GET_MODE_SIZE (tmode) == GET_MODE_SIZE (mode))
	    break;

      if (tmode == VOIDmode)
	abort ();

      /* Get X and Y in TMODE.  We can't use gen_lowpart here because it
	 may call change_address which is not appropriate if we were
	 called when a reload was in progress.  We don't have to worry
	 about changing the address since the size in bytes is supposed to
	 be the same.  Copy the MEM to change the mode and move any
	 substitutions from the old MEM to the new one.  */

      if (reload_in_progress)
	{
	  x = gen_lowpart_common (tmode, x1);
	  if (x == 0 && GET_CODE (x1) == MEM)
	    {
	      x = gen_rtx (MEM, tmode, XEXP (x1, 0));
	      RTX_UNCHANGING_P (x) = RTX_UNCHANGING_P (x1);
	      MEM_IN_STRUCT_P (x) = MEM_IN_STRUCT_P (x1);
	      MEM_VOLATILE_P (x) = MEM_VOLATILE_P (x1);
	      copy_replacements (x1, x);
	    }

	  y = gen_lowpart_common (tmode, y1);
	  if (y == 0 && GET_CODE (y1) == MEM)
	    {
	      y = gen_rtx (MEM, tmode, XEXP (y1, 0));
	      RTX_UNCHANGING_P (y) = RTX_UNCHANGING_P (y1);
	      MEM_IN_STRUCT_P (y) = MEM_IN_STRUCT_P (y1);
	      MEM_VOLATILE_P (y) = MEM_VOLATILE_P (y1);
	      copy_replacements (y1, y);
	    }
	}
      else
	{
	  x = gen_lowpart (tmode, x);
	  y = gen_lowpart (tmode, y);
	}
	  
      insn_code = mov_optab->handlers[(int) tmode].insn_code;
    }

  return (GEN_FCN (insn_code) (x, y));
}

/* Tables of patterns for extending one integer mode to another.  */
static enum insn_code extendtab[MAX_MACHINE_MODE][MAX_MACHINE_MODE][2];

/* Return the insn code used to extend FROM_MODE to TO_MODE.
   UNSIGNEDP specifies zero-extension instead of sign-extension.  If
   no such operation exists, CODE_FOR_nothing will be returned.  */

enum insn_code
can_extend_p (to_mode, from_mode, unsignedp)
     enum machine_mode to_mode, from_mode;
     int unsignedp;
{
  return extendtab[(int) to_mode][(int) from_mode][unsignedp];
}

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */

rtx
gen_extend_insn (x, y, mto, mfrom, unsignedp)
     rtx x, y;
     enum machine_mode mto, mfrom;
     int unsignedp;
{
  return (GEN_FCN (extendtab[(int) mto][(int) mfrom][unsignedp]) (x, y));
}

static void
init_extends ()
{
  enum insn_code *p;

  for (p = extendtab[0][0];
       p < extendtab[0][0] + sizeof extendtab / sizeof extendtab[0][0][0];
       p++)
    *p = CODE_FOR_nothing;

#ifdef HAVE_extendditi2
  if (HAVE_extendditi2)
    extendtab[(int) TImode][(int) DImode][0] = CODE_FOR_extendditi2;
#endif
#ifdef HAVE_extendsiti2
  if (HAVE_extendsiti2)
    extendtab[(int) TImode][(int) SImode][0] = CODE_FOR_extendsiti2;
#endif
#ifdef HAVE_extendhiti2
  if (HAVE_extendhiti2)
    extendtab[(int) TImode][(int) HImode][0] = CODE_FOR_extendhiti2;
#endif
#ifdef HAVE_extendqiti2
  if (HAVE_extendqiti2)
    extendtab[(int) TImode][(int) QImode][0] = CODE_FOR_extendqiti2;
#endif
#ifdef HAVE_extendsidi2
  if (HAVE_extendsidi2)
    extendtab[(int) DImode][(int) SImode][0] = CODE_FOR_extendsidi2;
#endif
#ifdef HAVE_extendhidi2
  if (HAVE_extendhidi2)
    extendtab[(int) DImode][(int) HImode][0] = CODE_FOR_extendhidi2;
#endif
#ifdef HAVE_extendqidi2
  if (HAVE_extendqidi2)
    extendtab[(int) DImode][(int) QImode][0] = CODE_FOR_extendqidi2;
#endif
#ifdef HAVE_extendhisi2
  if (HAVE_extendhisi2)
    extendtab[(int) SImode][(int) HImode][0] = CODE_FOR_extendhisi2;
#endif
#ifdef HAVE_extendqisi2
  if (HAVE_extendqisi2)
    extendtab[(int) SImode][(int) QImode][0] = CODE_FOR_extendqisi2;
#endif
#ifdef HAVE_extendqihi2
  if (HAVE_extendqihi2)
    extendtab[(int) HImode][(int) QImode][0] = CODE_FOR_extendqihi2;
#endif

#ifdef HAVE_zero_extendditi2
  if (HAVE_zero_extendsiti2)
    extendtab[(int) TImode][(int) DImode][1] = CODE_FOR_zero_extendditi2;
#endif
#ifdef HAVE_zero_extendsiti2
  if (HAVE_zero_extendsiti2)
    extendtab[(int) TImode][(int) SImode][1] = CODE_FOR_zero_extendsiti2;
#endif
#ifdef HAVE_zero_extendhiti2
  if (HAVE_zero_extendhiti2)
    extendtab[(int) TImode][(int) HImode][1] = CODE_FOR_zero_extendhiti2;
#endif
#ifdef HAVE_zero_extendqiti2
  if (HAVE_zero_extendqiti2)
    extendtab[(int) TImode][(int) QImode][1] = CODE_FOR_zero_extendqiti2;
#endif
#ifdef HAVE_zero_extendsidi2
  if (HAVE_zero_extendsidi2)
    extendtab[(int) DImode][(int) SImode][1] = CODE_FOR_zero_extendsidi2;
#endif
#ifdef HAVE_zero_extendhidi2
  if (HAVE_zero_extendhidi2)
    extendtab[(int) DImode][(int) HImode][1] = CODE_FOR_zero_extendhidi2;
#endif
#ifdef HAVE_zero_extendqidi2
  if (HAVE_zero_extendqidi2)
    extendtab[(int) DImode][(int) QImode][1] = CODE_FOR_zero_extendqidi2;
#endif
#ifdef HAVE_zero_extendhisi2
  if (HAVE_zero_extendhisi2)
    extendtab[(int) SImode][(int) HImode][1] = CODE_FOR_zero_extendhisi2;
#endif
#ifdef HAVE_zero_extendqisi2
  if (HAVE_zero_extendqisi2)
    extendtab[(int) SImode][(int) QImode][1] = CODE_FOR_zero_extendqisi2;
#endif
#ifdef HAVE_zero_extendqihi2
  if (HAVE_zero_extendqihi2)
    extendtab[(int) HImode][(int) QImode][1] = CODE_FOR_zero_extendqihi2;
#endif
}

/* can_fix_p and can_float_p say whether the target machine
   can directly convert a given fixed point type to
   a given floating point type, or vice versa.
   The returned value is the CODE_FOR_... value to use,
   or CODE_FOR_nothing if these modes cannot be directly converted.  */

static enum insn_code fixtab[NUM_MACHINE_MODES][NUM_MACHINE_MODES][2];
static enum insn_code fixtrunctab[NUM_MACHINE_MODES][NUM_MACHINE_MODES][2];
static enum insn_code floattab[NUM_MACHINE_MODES][NUM_MACHINE_MODES][2];

/* *TRUNCP_PTR is set to 1 if it is necessary to output
   an explicit FTRUNC insn before the fix insn; otherwise 0.  */

static enum insn_code
can_fix_p (fixmode, fltmode, unsignedp, truncp_ptr)
     enum machine_mode fltmode, fixmode;
     int unsignedp;
     int *truncp_ptr;
{
  *truncp_ptr = 0;
  if (fixtrunctab[(int) fltmode][(int) fixmode][unsignedp] != CODE_FOR_nothing)
    return fixtrunctab[(int) fltmode][(int) fixmode][unsignedp];

  if (ftrunc_optab->handlers[(int) fltmode].insn_code != CODE_FOR_nothing)
    {
      *truncp_ptr = 1;
      return fixtab[(int) fltmode][(int) fixmode][unsignedp];
    }
  return CODE_FOR_nothing;
}

static enum insn_code
can_float_p (fltmode, fixmode, unsignedp)
     enum machine_mode fixmode, fltmode;
     int unsignedp;
{
  return floattab[(int) fltmode][(int) fixmode][unsignedp];
}

void
init_fixtab ()
{
  enum insn_code *p;
  for (p = fixtab[0][0];
       p < fixtab[0][0] + sizeof fixtab / sizeof (fixtab[0][0][0]); 
       p++)
    *p = CODE_FOR_nothing;
  for (p = fixtrunctab[0][0];
       p < fixtrunctab[0][0] + sizeof fixtrunctab / sizeof (fixtrunctab[0][0][0]); 
       p++)
    *p = CODE_FOR_nothing;

#ifdef HAVE_fixsfqi2
  if (HAVE_fixsfqi2)
    fixtab[(int) SFmode][(int) QImode][0] = CODE_FOR_fixsfqi2;
#endif
#ifdef HAVE_fixsfhi2
  if (HAVE_fixsfhi2)
    fixtab[(int) SFmode][(int) HImode][0] = CODE_FOR_fixsfhi2;
#endif
#ifdef HAVE_fixsfsi2
  if (HAVE_fixsfsi2)
    fixtab[(int) SFmode][(int) SImode][0] = CODE_FOR_fixsfsi2;
#endif
#ifdef HAVE_fixsfdi2
  if (HAVE_fixsfdi2)
    fixtab[(int) SFmode][(int) DImode][0] = CODE_FOR_fixsfdi2;
#endif

#ifdef HAVE_fixdfqi2
  if (HAVE_fixdfqi2)
    fixtab[(int) DFmode][(int) QImode][0] = CODE_FOR_fixdfqi2;
#endif
#ifdef HAVE_fixdfhi2
  if (HAVE_fixdfhi2)
    fixtab[(int) DFmode][(int) HImode][0] = CODE_FOR_fixdfhi2;
#endif
#ifdef HAVE_fixdfsi2
  if (HAVE_fixdfsi2)
    fixtab[(int) DFmode][(int) SImode][0] = CODE_FOR_fixdfsi2;
#endif
#ifdef HAVE_fixdfdi2
  if (HAVE_fixdfdi2)
    fixtab[(int) DFmode][(int) DImode][0] = CODE_FOR_fixdfdi2;
#endif
#ifdef HAVE_fixdfti2
  if (HAVE_fixdfti2)
    fixtab[(int) DFmode][(int) TImode][0] = CODE_FOR_fixdfti2;
#endif

#ifdef HAVE_fixxfqi2
  if (HAVE_fixxfqi2)
    fixtab[(int) XFmode][(int) QImode][0] = CODE_FOR_fixxfqi2;
#endif
#ifdef HAVE_fixxfhi2
  if (HAVE_fixxfhi2)
    fixtab[(int) XFmode][(int) HImode][0] = CODE_FOR_fixxfhi2;
#endif
#ifdef HAVE_fixxfsi2
  if (HAVE_fixxfsi2)
    fixtab[(int) XFmode][(int) SImode][0] = CODE_FOR_fixxfsi2;
#endif
#ifdef HAVE_fixxfdi2
  if (HAVE_fixxfdi2)
    fixtab[(int) XFmode][(int) DImode][0] = CODE_FOR_fixxfdi2;
#endif
#ifdef HAVE_fixxfti2
  if (HAVE_fixxfti2)
    fixtab[(int) XFmode][(int) TImode][0] = CODE_FOR_fixxfti2;
#endif

#ifdef HAVE_fixtfqi2
  if (HAVE_fixtfqi2)
    fixtab[(int) TFmode][(int) QImode][0] = CODE_FOR_fixtfqi2;
#endif
#ifdef HAVE_fixtfhi2
  if (HAVE_fixtfhi2)
    fixtab[(int) TFmode][(int) HImode][0] = CODE_FOR_fixtfhi2;
#endif
#ifdef HAVE_fixtfsi2
  if (HAVE_fixtfsi2)
    fixtab[(int) TFmode][(int) SImode][0] = CODE_FOR_fixtfsi2;
#endif
#ifdef HAVE_fixtfdi2
  if (HAVE_fixtfdi2)
    fixtab[(int) TFmode][(int) DImode][0] = CODE_FOR_fixtfdi2;
#endif
#ifdef HAVE_fixtfti2
  if (HAVE_fixtfti2)
    fixtab[(int) TFmode][(int) TImode][0] = CODE_FOR_fixtfti2;
#endif

#ifdef HAVE_fixunssfqi2
  if (HAVE_fixunssfqi2)
    fixtab[(int) SFmode][(int) QImode][1] = CODE_FOR_fixunssfqi2;
#endif
#ifdef HAVE_fixunssfhi2
  if (HAVE_fixunssfhi2)
    fixtab[(int) SFmode][(int) HImode][1] = CODE_FOR_fixunssfhi2;
#endif
#ifdef HAVE_fixunssfsi2
  if (HAVE_fixunssfsi2)
    fixtab[(int) SFmode][(int) SImode][1] = CODE_FOR_fixunssfsi2;
#endif
#ifdef HAVE_fixunssfdi2
  if (HAVE_fixunssfdi2)
    fixtab[(int) SFmode][(int) DImode][1] = CODE_FOR_fixunssfdi2;
#endif

#ifdef HAVE_fixunsdfqi2
  if (HAVE_fixunsdfqi2)
    fixtab[(int) DFmode][(int) QImode][1] = CODE_FOR_fixunsdfqi2;
#endif
#ifdef HAVE_fixunsdfhi2
  if (HAVE_fixunsdfhi2)
    fixtab[(int) DFmode][(int) HImode][1] = CODE_FOR_fixunsdfhi2;
#endif
#ifdef HAVE_fixunsdfsi2
  if (HAVE_fixunsdfsi2)
    fixtab[(int) DFmode][(int) SImode][1] = CODE_FOR_fixunsdfsi2;
#endif
#ifdef HAVE_fixunsdfdi2
  if (HAVE_fixunsdfdi2)
    fixtab[(int) DFmode][(int) DImode][1] = CODE_FOR_fixunsdfdi2;
#endif
#ifdef HAVE_fixunsdfti2
  if (HAVE_fixunsdfti2)
    fixtab[(int) DFmode][(int) TImode][1] = CODE_FOR_fixunsdfti2;
#endif

#ifdef HAVE_fixunsxfqi2
  if (HAVE_fixunsxfqi2)
    fixtab[(int) XFmode][(int) QImode][1] = CODE_FOR_fixunsxfqi2;
#endif
#ifdef HAVE_fixunsxfhi2
  if (HAVE_fixunsxfhi2)
    fixtab[(int) XFmode][(int) HImode][1] = CODE_FOR_fixunsxfhi2;
#endif
#ifdef HAVE_fixunsxfsi2
  if (HAVE_fixunsxfsi2)
    fixtab[(int) XFmode][(int) SImode][1] = CODE_FOR_fixunsxfsi2;
#endif
#ifdef HAVE_fixunsxfdi2
  if (HAVE_fixunsxfdi2)
    fixtab[(int) XFmode][(int) DImode][1] = CODE_FOR_fixunsxfdi2;
#endif
#ifdef HAVE_fixunsxfti2
  if (HAVE_fixunsxfti2)
    fixtab[(int) XFmode][(int) TImode][1] = CODE_FOR_fixunsxfti2;
#endif

#ifdef HAVE_fixunstfqi2
  if (HAVE_fixunstfqi2)
    fixtab[(int) TFmode][(int) QImode][1] = CODE_FOR_fixunstfqi2;
#endif
#ifdef HAVE_fixunstfhi2
  if (HAVE_fixunstfhi2)
    fixtab[(int) TFmode][(int) HImode][1] = CODE_FOR_fixunstfhi2;
#endif
#ifdef HAVE_fixunstfsi2
  if (HAVE_fixunstfsi2)
    fixtab[(int) TFmode][(int) SImode][1] = CODE_FOR_fixunstfsi2;
#endif
#ifdef HAVE_fixunstfdi2
  if (HAVE_fixunstfdi2)
    fixtab[(int) TFmode][(int) DImode][1] = CODE_FOR_fixunstfdi2;
#endif
#ifdef HAVE_fixunstfti2
  if (HAVE_fixunstfti2)
    fixtab[(int) TFmode][(int) TImode][1] = CODE_FOR_fixunstfti2;
#endif

#ifdef HAVE_fix_truncsfqi2
  if (HAVE_fix_truncsfqi2)
    fixtrunctab[(int) SFmode][(int) QImode][0] = CODE_FOR_fix_truncsfqi2;
#endif
#ifdef HAVE_fix_truncsfhi2
  if (HAVE_fix_truncsfhi2)
    fixtrunctab[(int) SFmode][(int) HImode][0] = CODE_FOR_fix_truncsfhi2;
#endif
#ifdef HAVE_fix_truncsfsi2
  if (HAVE_fix_truncsfsi2)
    fixtrunctab[(int) SFmode][(int) SImode][0] = CODE_FOR_fix_truncsfsi2;
#endif
#ifdef HAVE_fix_truncsfdi2
  if (HAVE_fix_truncsfdi2)
    fixtrunctab[(int) SFmode][(int) DImode][0] = CODE_FOR_fix_truncsfdi2;
#endif

#ifdef HAVE_fix_truncdfqi2
  if (HAVE_fix_truncdfqi2)
    fixtrunctab[(int) DFmode][(int) QImode][0] = CODE_FOR_fix_truncdfqi2;
#endif
#ifdef HAVE_fix_truncdfhi2
  if (HAVE_fix_truncdfhi2)
    fixtrunctab[(int) DFmode][(int) HImode][0] = CODE_FOR_fix_truncdfhi2;
#endif
#ifdef HAVE_fix_truncdfsi2
  if (HAVE_fix_truncdfsi2)
    fixtrunctab[(int) DFmode][(int) SImode][0] = CODE_FOR_fix_truncdfsi2;
#endif
#ifdef HAVE_fix_truncdfdi2
  if (HAVE_fix_truncdfdi2)
    fixtrunctab[(int) DFmode][(int) DImode][0] = CODE_FOR_fix_truncdfdi2;
#endif
#ifdef HAVE_fix_truncdfti2
  if (HAVE_fix_truncdfti2)
    fixtrunctab[(int) DFmode][(int) TImode][0] = CODE_FOR_fix_truncdfti2;
#endif

#ifdef HAVE_fix_truncxfqi2
  if (HAVE_fix_truncxfqi2)
    fixtrunctab[(int) XFmode][(int) QImode][0] = CODE_FOR_fix_truncxfqi2;
#endif
#ifdef HAVE_fix_truncxfhi2
  if (HAVE_fix_truncxfhi2)
    fixtrunctab[(int) XFmode][(int) HImode][0] = CODE_FOR_fix_truncxfhi2;
#endif
#ifdef HAVE_fix_truncxfsi2
  if (HAVE_fix_truncxfsi2)
    fixtrunctab[(int) XFmode][(int) SImode][0] = CODE_FOR_fix_truncxfsi2;
#endif
#ifdef HAVE_fix_truncxfdi2
  if (HAVE_fix_truncxfdi2)
    fixtrunctab[(int) XFmode][(int) DImode][0] = CODE_FOR_fix_truncxfdi2;
#endif
#ifdef HAVE_fix_truncxfti2
  if (HAVE_fix_truncxfti2)
    fixtrunctab[(int) XFmode][(int) TImode][0] = CODE_FOR_fix_truncxfti2;
#endif

#ifdef HAVE_fix_trunctfqi2
  if (HAVE_fix_trunctfqi2)
    fixtrunctab[(int) TFmode][(int) QImode][0] = CODE_FOR_fix_trunctfqi2;
#endif
#ifdef HAVE_fix_trunctfhi2
  if (HAVE_fix_trunctfhi2)
    fixtrunctab[(int) TFmode][(int) HImode][0] = CODE_FOR_fix_trunctfhi2;
#endif
#ifdef HAVE_fix_trunctfsi2
  if (HAVE_fix_trunctfsi2)
    fixtrunctab[(int) TFmode][(int) SImode][0] = CODE_FOR_fix_trunctfsi2;
#endif
#ifdef HAVE_fix_trunctfdi2
  if (HAVE_fix_trunctfdi2)
    fixtrunctab[(int) TFmode][(int) DImode][0] = CODE_FOR_fix_trunctfdi2;
#endif
#ifdef HAVE_fix_trunctfti2
  if (HAVE_fix_trunctfti2)
    fixtrunctab[(int) TFmode][(int) TImode][0] = CODE_FOR_fix_trunctfti2;
#endif

#ifdef HAVE_fixuns_truncsfqi2
  if (HAVE_fixuns_truncsfqi2)
    fixtrunctab[(int) SFmode][(int) QImode][1] = CODE_FOR_fixuns_truncsfqi2;
#endif
#ifdef HAVE_fixuns_truncsfhi2
  if (HAVE_fixuns_truncsfhi2)
    fixtrunctab[(int) SFmode][(int) HImode][1] = CODE_FOR_fixuns_truncsfhi2;
#endif
#ifdef HAVE_fixuns_truncsfsi2
  if (HAVE_fixuns_truncsfsi2)
    fixtrunctab[(int) SFmode][(int) SImode][1] = CODE_FOR_fixuns_truncsfsi2;
#endif
#ifdef HAVE_fixuns_truncsfdi2
  if (HAVE_fixuns_truncsfdi2)
    fixtrunctab[(int) SFmode][(int) DImode][1] = CODE_FOR_fixuns_truncsfdi2;
#endif

#ifdef HAVE_fixuns_truncdfqi2
  if (HAVE_fixuns_truncdfqi2)
    fixtrunctab[(int) DFmode][(int) QImode][1] = CODE_FOR_fixuns_truncdfqi2;
#endif
#ifdef HAVE_fixuns_truncdfhi2
  if (HAVE_fixuns_truncdfhi2)
    fixtrunctab[(int) DFmode][(int) HImode][1] = CODE_FOR_fixuns_truncdfhi2;
#endif
#ifdef HAVE_fixuns_truncdfsi2
  if (HAVE_fixuns_truncdfsi2)
    fixtrunctab[(int) DFmode][(int) SImode][1] = CODE_FOR_fixuns_truncdfsi2;
#endif
#ifdef HAVE_fixuns_truncdfdi2
  if (HAVE_fixuns_truncdfdi2)
    fixtrunctab[(int) DFmode][(int) DImode][1] = CODE_FOR_fixuns_truncdfdi2;
#endif
#ifdef HAVE_fixuns_truncdfti2
  if (HAVE_fixuns_truncdfti2)
    fixtrunctab[(int) DFmode][(int) TImode][1] = CODE_FOR_fixuns_truncdfti2;
#endif

#ifdef HAVE_fixuns_truncxfqi2
  if (HAVE_fixuns_truncxfqi2)
    fixtrunctab[(int) XFmode][(int) QImode][1] = CODE_FOR_fixuns_truncxfqi2;
#endif
#ifdef HAVE_fixuns_truncxfhi2
  if (HAVE_fixuns_truncxfhi2)
    fixtrunctab[(int) XFmode][(int) HImode][1] = CODE_FOR_fixuns_truncxfhi2;
#endif
#ifdef HAVE_fixuns_truncxfsi2
  if (HAVE_fixuns_truncxfsi2)
    fixtrunctab[(int) XFmode][(int) SImode][1] = CODE_FOR_fixuns_truncxfsi2;
#endif
#ifdef HAVE_fixuns_truncxfdi2
  if (HAVE_fixuns_truncxfdi2)
    fixtrunctab[(int) XFmode][(int) DImode][1] = CODE_FOR_fixuns_truncxfdi2;
#endif
#ifdef HAVE_fixuns_truncxfti2
  if (HAVE_fixuns_truncxfti2)
    fixtrunctab[(int) XFmode][(int) TImode][1] = CODE_FOR_fixuns_truncxfti2;
#endif

#ifdef HAVE_fixuns_trunctfqi2
  if (HAVE_fixuns_trunctfqi2)
    fixtrunctab[(int) TFmode][(int) QImode][1] = CODE_FOR_fixuns_trunctfqi2;
#endif
#ifdef HAVE_fixuns_trunctfhi2
  if (HAVE_fixuns_trunctfhi2)
    fixtrunctab[(int) TFmode][(int) HImode][1] = CODE_FOR_fixuns_trunctfhi2;
#endif
#ifdef HAVE_fixuns_trunctfsi2
  if (HAVE_fixuns_trunctfsi2)
    fixtrunctab[(int) TFmode][(int) SImode][1] = CODE_FOR_fixuns_trunctfsi2;
#endif
#ifdef HAVE_fixuns_trunctfdi2
  if (HAVE_fixuns_trunctfdi2)
    fixtrunctab[(int) TFmode][(int) DImode][1] = CODE_FOR_fixuns_trunctfdi2;
#endif
#ifdef HAVE_fixuns_trunctfti2
  if (HAVE_fixuns_trunctfti2)
    fixtrunctab[(int) TFmode][(int) TImode][1] = CODE_FOR_fixuns_trunctfti2;
#endif

#ifdef FIXUNS_TRUNC_LIKE_FIX_TRUNC
  /* This flag says the same insns that convert to a signed fixnum
     also convert validly to an unsigned one.  */
  {
    int i;
    int j;
    for (i = 0; i < NUM_MACHINE_MODES; i++)
      for (j = 0; j < NUM_MACHINE_MODES; j++)
	fixtrunctab[i][j][1] = fixtrunctab[i][j][0];
  }
#endif
}

void
init_floattab ()
{
  enum insn_code *p;
  for (p = floattab[0][0];
       p < floattab[0][0] + sizeof floattab / sizeof (floattab[0][0][0]); 
       p++)
    *p = CODE_FOR_nothing;

#ifdef HAVE_floatqisf2
  if (HAVE_floatqisf2)
    floattab[(int) SFmode][(int) QImode][0] = CODE_FOR_floatqisf2;
#endif
#ifdef HAVE_floathisf2
  if (HAVE_floathisf2)
    floattab[(int) SFmode][(int) HImode][0] = CODE_FOR_floathisf2;
#endif
#ifdef HAVE_floatsisf2
  if (HAVE_floatsisf2)
    floattab[(int) SFmode][(int) SImode][0] = CODE_FOR_floatsisf2;
#endif
#ifdef HAVE_floatdisf2
  if (HAVE_floatdisf2)
    floattab[(int) SFmode][(int) DImode][0] = CODE_FOR_floatdisf2;
#endif
#ifdef HAVE_floattisf2
  if (HAVE_floattisf2)
    floattab[(int) SFmode][(int) TImode][0] = CODE_FOR_floattisf2;
#endif

#ifdef HAVE_floatqidf2
  if (HAVE_floatqidf2)
    floattab[(int) DFmode][(int) QImode][0] = CODE_FOR_floatqidf2;
#endif
#ifdef HAVE_floathidf2
  if (HAVE_floathidf2)
    floattab[(int) DFmode][(int) HImode][0] = CODE_FOR_floathidf2;
#endif
#ifdef HAVE_floatsidf2
  if (HAVE_floatsidf2)
    floattab[(int) DFmode][(int) SImode][0] = CODE_FOR_floatsidf2;
#endif
#ifdef HAVE_floatdidf2
  if (HAVE_floatdidf2)
    floattab[(int) DFmode][(int) DImode][0] = CODE_FOR_floatdidf2;
#endif
#ifdef HAVE_floattidf2
  if (HAVE_floattidf2)
    floattab[(int) DFmode][(int) TImode][0] = CODE_FOR_floattidf2;
#endif

#ifdef HAVE_floatqixf2
  if (HAVE_floatqixf2)
    floattab[(int) XFmode][(int) QImode][0] = CODE_FOR_floatqixf2;
#endif
#ifdef HAVE_floathixf2
  if (HAVE_floathixf2)
    floattab[(int) XFmode][(int) HImode][0] = CODE_FOR_floathixf2;
#endif
#ifdef HAVE_floatsixf2
  if (HAVE_floatsixf2)
    floattab[(int) XFmode][(int) SImode][0] = CODE_FOR_floatsixf2;
#endif
#ifdef HAVE_floatdixf2
  if (HAVE_floatdixf2)
    floattab[(int) XFmode][(int) DImode][0] = CODE_FOR_floatdixf2;
#endif
#ifdef HAVE_floattixf2
  if (HAVE_floattixf2)
    floattab[(int) XFmode][(int) TImode][0] = CODE_FOR_floattixf2;
#endif

#ifdef HAVE_floatqitf2
  if (HAVE_floatqitf2)
    floattab[(int) TFmode][(int) QImode][0] = CODE_FOR_floatqitf2;
#endif
#ifdef HAVE_floathitf2
  if (HAVE_floathitf2)
    floattab[(int) TFmode][(int) HImode][0] = CODE_FOR_floathitf2;
#endif
#ifdef HAVE_floatsitf2
  if (HAVE_floatsitf2)
    floattab[(int) TFmode][(int) SImode][0] = CODE_FOR_floatsitf2;
#endif
#ifdef HAVE_floatditf2
  if (HAVE_floatditf2)
    floattab[(int) TFmode][(int) DImode][0] = CODE_FOR_floatditf2;
#endif
#ifdef HAVE_floattitf2
  if (HAVE_floattitf2)
    floattab[(int) TFmode][(int) TImode][0] = CODE_FOR_floattitf2;
#endif

#ifdef HAVE_floatunsqisf2
  if (HAVE_floatunsqisf2)
    floattab[(int) SFmode][(int) QImode][1] = CODE_FOR_floatunsqisf2;
#endif
#ifdef HAVE_floatunshisf2
  if (HAVE_floatunshisf2)
    floattab[(int) SFmode][(int) HImode][1] = CODE_FOR_floatunshisf2;
#endif
#ifdef HAVE_floatunssisf2
  if (HAVE_floatunssisf2)
    floattab[(int) SFmode][(int) SImode][1] = CODE_FOR_floatunssisf2;
#endif
#ifdef HAVE_floatunsdisf2
  if (HAVE_floatunsdisf2)
    floattab[(int) SFmode][(int) DImode][1] = CODE_FOR_floatunsdisf2;
#endif
#ifdef HAVE_floatunstisf2
  if (HAVE_floatunstisf2)
    floattab[(int) SFmode][(int) TImode][1] = CODE_FOR_floatunstisf2;
#endif

#ifdef HAVE_floatunsqidf2
  if (HAVE_floatunsqidf2)
    floattab[(int) DFmode][(int) QImode][1] = CODE_FOR_floatunsqidf2;
#endif
#ifdef HAVE_floatunshidf2
  if (HAVE_floatunshidf2)
    floattab[(int) DFmode][(int) HImode][1] = CODE_FOR_floatunshidf2;
#endif
#ifdef HAVE_floatunssidf2
  if (HAVE_floatunssidf2)
    floattab[(int) DFmode][(int) SImode][1] = CODE_FOR_floatunssidf2;
#endif
#ifdef HAVE_floatunsdidf2
  if (HAVE_floatunsdidf2)
    floattab[(int) DFmode][(int) DImode][1] = CODE_FOR_floatunsdidf2;
#endif
#ifdef HAVE_floatunstidf2
  if (HAVE_floatunstidf2)
    floattab[(int) DFmode][(int) TImode][1] = CODE_FOR_floatunstidf2;
#endif

#ifdef HAVE_floatunsqixf2
  if (HAVE_floatunsqixf2)
    floattab[(int) XFmode][(int) QImode][1] = CODE_FOR_floatunsqixf2;
#endif
#ifdef HAVE_floatunshixf2
  if (HAVE_floatunshixf2)
    floattab[(int) XFmode][(int) HImode][1] = CODE_FOR_floatunshixf2;
#endif
#ifdef HAVE_floatunssixf2
  if (HAVE_floatunssixf2)
    floattab[(int) XFmode][(int) SImode][1] = CODE_FOR_floatunssixf2;
#endif
#ifdef HAVE_floatunsdixf2
  if (HAVE_floatunsdixf2)
    floattab[(int) XFmode][(int) DImode][1] = CODE_FOR_floatunsdixf2;
#endif
#ifdef HAVE_floatunstixf2
  if (HAVE_floatunstixf2)
    floattab[(int) XFmode][(int) TImode][1] = CODE_FOR_floatunstixf2;
#endif

#ifdef HAVE_floatunsqitf2
  if (HAVE_floatunsqitf2)
    floattab[(int) TFmode][(int) QImode][1] = CODE_FOR_floatunsqitf2;
#endif
#ifdef HAVE_floatunshitf2
  if (HAVE_floatunshitf2)
    floattab[(int) TFmode][(int) HImode][1] = CODE_FOR_floatunshitf2;
#endif
#ifdef HAVE_floatunssitf2
  if (HAVE_floatunssitf2)
    floattab[(int) TFmode][(int) SImode][1] = CODE_FOR_floatunssitf2;
#endif
#ifdef HAVE_floatunsditf2
  if (HAVE_floatunsditf2)
    floattab[(int) TFmode][(int) DImode][1] = CODE_FOR_floatunsditf2;
#endif
#ifdef HAVE_floatunstitf2
  if (HAVE_floatunstitf2)
    floattab[(int) TFmode][(int) TImode][1] = CODE_FOR_floatunstitf2;
#endif
}

/* Generate code to convert FROM to floating point
   and store in TO.  FROM must be fixed point and not VOIDmode.
   UNSIGNEDP nonzero means regard FROM as unsigned.
   Normally this is done by correcting the final value
   if it is negative.  */

void
expand_float (to, from, unsignedp)
     rtx to, from;
     int unsignedp;
{
  enum insn_code icode;
  register rtx target = to;
  enum machine_mode fmode, imode;

  /* Crash now, because we won't be able to decide which mode to use.  */
  if (GET_MODE (from) == VOIDmode)
    abort ();

  /* Look for an insn to do the conversion.  Do it in the specified
     modes if possible; otherwise convert either input, output or both to
     wider mode.  If the integer mode is wider than the mode of FROM,
     we can do the conversion signed even if the input is unsigned.  */

  for (imode = GET_MODE (from); imode != VOIDmode;
       imode = GET_MODE_WIDER_MODE (imode))
    for (fmode = GET_MODE (to); fmode != VOIDmode;
	 fmode = GET_MODE_WIDER_MODE (fmode))
      {
	int doing_unsigned = unsignedp;

	icode = can_float_p (fmode, imode, unsignedp);
	if (icode == CODE_FOR_nothing && imode != GET_MODE (from) && unsignedp)
	  icode = can_float_p (fmode, imode, 0), doing_unsigned = 0;

	if (icode != CODE_FOR_nothing)
	  {
	    to = protect_from_queue (to, 1);
	    from = protect_from_queue (from, 0);

	    if (imode != GET_MODE (from))
	      from = convert_to_mode (imode, from, unsignedp);

	    if (fmode != GET_MODE (to))
	      target = gen_reg_rtx (fmode);

	    emit_unop_insn (icode, target, from,
			    doing_unsigned ? UNSIGNED_FLOAT : FLOAT);

	    if (target != to)
	      convert_move (to, target, 0);
	    return;
	  }
    }

#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)

  /* Unsigned integer, and no way to convert directly.
     Convert as signed, then conditionally adjust the result.  */
  if (unsignedp)
    {
      rtx label = gen_label_rtx ();
      rtx temp;
      REAL_VALUE_TYPE offset;

      emit_queue ();

      to = protect_from_queue (to, 1);
      from = protect_from_queue (from, 0);

      if (flag_force_mem)
	from = force_not_mem (from);

      /* If we are about to do some arithmetic to correct for an
	 unsigned operand, do it in a pseudo-register.  */

      if (GET_CODE (to) != REG || REGNO (to) <= LAST_VIRTUAL_REGISTER)
	target = gen_reg_rtx (GET_MODE (to));

      /* Convert as signed integer to floating.  */
      expand_float (target, from, 0);

      /* If FROM is negative (and therefore TO is negative),
	 correct its value by 2**bitwidth.  */

      do_pending_stack_adjust ();
      emit_cmp_insn (from, const0_rtx, GE, NULL_RTX, GET_MODE (from), 0, 0);
      emit_jump_insn (gen_bge (label));
      /* On SCO 3.2.1, ldexp rejects values outside [0.5, 1).
	 Rather than setting up a dconst_dot_5, let's hope SCO
	 fixes the bug.  */
      offset = REAL_VALUE_LDEXP (dconst1, GET_MODE_BITSIZE (GET_MODE (from)));
      temp = expand_binop (GET_MODE (to), add_optab, target,
			   immed_real_const_1 (offset, GET_MODE (to)),
			   target, 0, OPTAB_LIB_WIDEN);
      if (temp != target)
	emit_move_insn (target, temp);
      do_pending_stack_adjust ();
      emit_label (label);
    }
  else
#endif

  /* No hardware instruction available; call a library rotine to convert from
     SImode, DImode, or TImode into SFmode, DFmode, XFmode, or TFmode.  */
    {
      rtx libfcn;
      rtx insns;

      to = protect_from_queue (to, 1);
      from = protect_from_queue (from, 0);

      if (GET_MODE_SIZE (GET_MODE (from)) < GET_MODE_SIZE (SImode))
	from = convert_to_mode (SImode, from, unsignedp);

      if (flag_force_mem)
	from = force_not_mem (from);

      if (GET_MODE (to) == SFmode)
	{
	  if (GET_MODE (from) == SImode)
	    libfcn = floatsisf_libfunc;
	  else if (GET_MODE (from) == DImode)
	    libfcn = floatdisf_libfunc;
	  else if (GET_MODE (from) == TImode)
	    libfcn = floattisf_libfunc;
	  else
	    abort ();
	}
      else if (GET_MODE (to) == DFmode)
	{
	  if (GET_MODE (from) == SImode)
	    libfcn = floatsidf_libfunc;
	  else if (GET_MODE (from) == DImode)
	    libfcn = floatdidf_libfunc;
	  else if (GET_MODE (from) == TImode)
	    libfcn = floattidf_libfunc;
	  else
	    abort ();
	}
      else if (GET_MODE (to) == XFmode)
	{
	  if (GET_MODE (from) == SImode)
	    libfcn = floatsixf_libfunc;
	  else if (GET_MODE (from) == DImode)
	    libfcn = floatdixf_libfunc;
	  else if (GET_MODE (from) == TImode)
	    libfcn = floattixf_libfunc;
	  else
	    abort ();
	}
      else if (GET_MODE (to) == TFmode)
	{
	  if (GET_MODE (from) == SImode)
	    libfcn = floatsitf_libfunc;
	  else if (GET_MODE (from) == DImode)
	    libfcn = floatditf_libfunc;
	  else if (GET_MODE (from) == TImode)
	    libfcn = floattitf_libfunc;
	  else
	    abort ();
	}
      else
	abort ();

      start_sequence ();

      emit_library_call (libfcn, 1, GET_MODE (to), 1, from, GET_MODE (from));
      insns = get_insns ();
      end_sequence ();

      emit_libcall_block (insns, target, hard_libcall_value (GET_MODE (to)),
			  gen_rtx (FLOAT, GET_MODE (to), from));
    }

  /* Copy result to requested destination
     if we have been computing in a temp location.  */

  if (target != to)
    {
      if (GET_MODE (target) == GET_MODE (to))
	emit_move_insn (to, target);
      else
	convert_move (to, target, 0);
    }
}

/* expand_fix: generate code to convert FROM to fixed point
   and store in TO.  FROM must be floating point.  */

static rtx
ftruncify (x)
     rtx x;
{
  rtx temp = gen_reg_rtx (GET_MODE (x));
  return expand_unop (GET_MODE (x), ftrunc_optab, x, temp, 0);
}

void
expand_fix (to, from, unsignedp)
     register rtx to, from;
     int unsignedp;
{
  enum insn_code icode;
  register rtx target = to;
  enum machine_mode fmode, imode;
  int must_trunc = 0;
  rtx libfcn = 0;

  /* We first try to find a pair of modes, one real and one integer, at
     least as wide as FROM and TO, respectively, in which we can open-code
     this conversion.  If the integer mode is wider than the mode of TO,
     we can do the conversion either signed or unsigned.  */

  for (imode = GET_MODE (to); imode != VOIDmode;
       imode = GET_MODE_WIDER_MODE (imode))
    for (fmode = GET_MODE (from); fmode != VOIDmode;
	 fmode = GET_MODE_WIDER_MODE (fmode))
      {
	int doing_unsigned = unsignedp;

	icode = can_fix_p (imode, fmode, unsignedp, &must_trunc);
	if (icode == CODE_FOR_nothing && imode != GET_MODE (to) && unsignedp)
	  icode = can_fix_p (imode, fmode, 0, &must_trunc), doing_unsigned = 0;

	if (icode != CODE_FOR_nothing)
	  {
	    to = protect_from_queue (to, 1);
	    from = protect_from_queue (from, 0);

	    if (fmode != GET_MODE (from))
	      from = convert_to_mode (fmode, from, 0);

	    if (must_trunc)
	      from = ftruncify (from);

	    if (imode != GET_MODE (to))
	      target = gen_reg_rtx (imode);

	    emit_unop_insn (icode, target, from,
			    doing_unsigned ? UNSIGNED_FIX : FIX);
	    if (target != to)
	      convert_move (to, target, unsignedp);
	    return;
	  }
      }

#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
  /* For an unsigned conversion, there is one more way to do it.
     If we have a signed conversion, we generate code that compares
     the real value to the largest representable positive number.  If if
     is smaller, the conversion is done normally.  Otherwise, subtract
     one plus the highest signed number, convert, and add it back.

     We only need to check all real modes, since we know we didn't find
     anything with a wider integer mode.  */

  if (unsignedp && GET_MODE_BITSIZE (GET_MODE (to)) <= HOST_BITS_PER_WIDE_INT)
    for (fmode = GET_MODE (from); fmode != VOIDmode;
	 fmode = GET_MODE_WIDER_MODE (fmode))
      /* Make sure we won't lose significant bits doing this.  */
      if (GET_MODE_BITSIZE (fmode) > GET_MODE_BITSIZE (GET_MODE (to))
	  && CODE_FOR_nothing != can_fix_p (GET_MODE (to), fmode, 0,
					    &must_trunc))
	{
	  int bitsize = GET_MODE_BITSIZE (GET_MODE (to));
	  REAL_VALUE_TYPE offset = REAL_VALUE_LDEXP (dconst1, bitsize - 1);
	  rtx limit = immed_real_const_1 (offset, fmode);
	  rtx lab1 = gen_label_rtx ();
	  rtx lab2 = gen_label_rtx ();
	  rtx insn;

	  emit_queue ();
	  to = protect_from_queue (to, 1);
	  from = protect_from_queue (from, 0);

	  if (flag_force_mem)
	    from = force_not_mem (from);

	  if (fmode != GET_MODE (from))
	    from = convert_to_mode (fmode, from, 0);

	  /* See if we need to do the subtraction.  */
	  do_pending_stack_adjust ();
	  emit_cmp_insn (from, limit, GE, NULL_RTX, GET_MODE (from), 0, 0);
	  emit_jump_insn (gen_bge (lab1));

	  /* If not, do the signed "fix" and branch around fixup code.  */
	  expand_fix (to, from, 0);
	  emit_jump_insn (gen_jump (lab2));
	  emit_barrier ();

	  /* Otherwise, subtract 2**(N-1), convert to signed number,
	     then add 2**(N-1).  Do the addition using XOR since this
	     will often generate better code.  */
	  emit_label (lab1);
	  target = expand_binop (GET_MODE (from), sub_optab, from, limit,
				 NULL_RTX, 0, OPTAB_LIB_WIDEN);
	  expand_fix (to, target, 0);
	  target = expand_binop (GET_MODE (to), xor_optab, to,
				 GEN_INT ((HOST_WIDE_INT) 1 << (bitsize - 1)),
				 to, 1, OPTAB_LIB_WIDEN);

	  if (target != to)
	    emit_move_insn (to, target);

	  emit_label (lab2);

	  /* Make a place for a REG_NOTE and add it.  */
	  insn = emit_move_insn (to, to);
	  REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_EQUAL,
				      gen_rtx (UNSIGNED_FIX, GET_MODE (to),
					       from), REG_NOTES (insn));

	  return;
	}
#endif

  /* We can't do it with an insn, so use a library call.  But first ensure
     that the mode of TO is at least as wide as SImode, since those are the
     only library calls we know about.  */

  if (GET_MODE_SIZE (GET_MODE (to)) < GET_MODE_SIZE (SImode))
    {
      target = gen_reg_rtx (SImode);

      expand_fix (target, from, unsignedp);
    }
  else if (GET_MODE (from) == SFmode)
    {
      if (GET_MODE (to) == SImode)
	libfcn = unsignedp ? fixunssfsi_libfunc : fixsfsi_libfunc;
      else if (GET_MODE (to) == DImode)
	libfcn = unsignedp ? fixunssfdi_libfunc : fixsfdi_libfunc;
      else if (GET_MODE (to) == TImode)
	libfcn = unsignedp ? fixunssfti_libfunc : fixsfti_libfunc;
      else
	abort ();
    }
  else if (GET_MODE (from) == DFmode)
    {
      if (GET_MODE (to) == SImode)
	libfcn = unsignedp ? fixunsdfsi_libfunc : fixdfsi_libfunc;
      else if (GET_MODE (to) == DImode)
	libfcn = unsignedp ? fixunsdfdi_libfunc : fixdfdi_libfunc;
      else if (GET_MODE (to) == TImode)
	libfcn = unsignedp ? fixunsdfti_libfunc : fixdfti_libfunc;
      else
	abort ();
    }
  else if (GET_MODE (from) == XFmode)
    {
      if (GET_MODE (to) == SImode)
	libfcn = unsignedp ? fixunsxfsi_libfunc : fixxfsi_libfunc;
      else if (GET_MODE (to) == DImode)
	libfcn = unsignedp ? fixunsxfdi_libfunc : fixxfdi_libfunc;
      else if (GET_MODE (to) == TImode)
	libfcn = unsignedp ? fixunsxfti_libfunc : fixxfti_libfunc;
      else
	abort ();
    }
  else if (GET_MODE (from) == TFmode)
    {
      if (GET_MODE (to) == SImode)
	libfcn = unsignedp ? fixunstfsi_libfunc : fixtfsi_libfunc;
      else if (GET_MODE (to) == DImode)
	libfcn = unsignedp ? fixunstfdi_libfunc : fixtfdi_libfunc;
      else if (GET_MODE (to) == TImode)
	libfcn = unsignedp ? fixunstfti_libfunc : fixtfti_libfunc;
      else
	abort ();
    }
  else
    abort ();

  if (libfcn)
    {
      rtx insns;

      to = protect_from_queue (to, 1);
      from = protect_from_queue (from, 0);

      if (flag_force_mem)
	from = force_not_mem (from);

      start_sequence ();

      emit_library_call (libfcn, 1, GET_MODE (to), 1, from, GET_MODE (from));
      insns = get_insns ();
      end_sequence ();

      emit_libcall_block (insns, target, hard_libcall_value (GET_MODE (to)),
			  gen_rtx (unsignedp ? FIX : UNSIGNED_FIX,
				   GET_MODE (to), from));
    }
      
  if (GET_MODE (to) == GET_MODE (target))
    emit_move_insn (to, target);
  else
    convert_move (to, target, 0);
}

static optab
init_optab (code)
     enum rtx_code code;
{
  int i;
  optab op = (optab) xmalloc (sizeof (struct optab));
  op->code = code;
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      op->handlers[i].insn_code = CODE_FOR_nothing;
      op->handlers[i].libfunc = 0;
    }
  return op;
}

/* Initialize the libfunc fields of an entire group of entries in some
   optab.  Each entry is set equal to a string consisting of a leading
   pair of underscores followed by a generic operation name followed by
   a mode name (downshifted to lower case) followed by a single character
   representing the number of operands for the given operation (which is
   usually one of the characters '2', '3', or '4').

   OPTABLE is the table in which libfunc fields are to be initialized.
   FIRST_MODE is the first machine mode index in the given optab to
     initialize.
   LAST_MODE is the last machine mode index in the given optab to
     initialize.
   OPNAME is the generic (string) name of the operation.
   SUFFIX is the character which specifies the number of operands for
     the given generic operation.
*/

static void
init_libfuncs (optable, first_mode, last_mode, opname, suffix)
    register optab optable;
    register char *opname;
    register enum machine_mode first_mode;
    register enum machine_mode last_mode;
    register char suffix;
{
  register enum machine_mode mode;
  register unsigned opname_len = strlen (opname);

  for (mode = first_mode; (int) mode <= (int) last_mode;
       mode = (enum machine_mode) ((int) mode + 1))
    {
      register char *mname = mode_name[(int) mode];
      register unsigned mname_len = strlen (mname);
      register char *libfunc_name
	= (char *) xmalloc (2 + opname_len + mname_len + 1 + 1);
      register char *p;
      register char *q;

      p = libfunc_name;
      *p++ = '_';
      *p++ = '_';
      for (q = opname; *q; )
	*p++ = *q++;
      for (q = mname; *q; q++)
	*p++ = tolower (*q);
      *p++ = suffix;
      *p++ = '\0';
      optable->handlers[(int) mode].libfunc
	= gen_rtx (SYMBOL_REF, Pmode, libfunc_name);
    }
}

/* Initialize the libfunc fields of an entire group of entries in some
   optab which correspond to all integer mode operations.  The parameters
   have the same meaning as similarly named ones for the `init_libfuncs'
   routine.  (See above).  */

static void
init_integral_libfuncs (optable, opname, suffix)
    register optab optable;
    register char *opname;
    register char suffix;
{
  init_libfuncs (optable, SImode, TImode, opname, suffix);
}

/* Initialize the libfunc fields of an entire group of entries in some
   optab which correspond to all real mode operations.  The parameters
   have the same meaning as similarly named ones for the `init_libfuncs'
   routine.  (See above).  */

static void
init_floating_libfuncs (optable, opname, suffix)
    register optab optable;
    register char *opname;
    register char suffix;
{
  init_libfuncs (optable, SFmode, TFmode, opname, suffix);
}

/* Initialize the libfunc fields of an entire group of entries in some
   optab which correspond to all complex floating modes.  The parameters
   have the same meaning as similarly named ones for the `init_libfuncs'
   routine.  (See above).  */

static void
init_complex_libfuncs (optable, opname, suffix)
    register optab optable;
    register char *opname;
    register char suffix;
{
  init_libfuncs (optable, SCmode, TCmode, opname, suffix);
}

/* Call this once to initialize the contents of the optabs
   appropriately for the current target machine.  */

void
init_optabs ()
{
  int i;

  init_fixtab ();
  init_floattab ();
  init_extends ();

  add_optab = init_optab (PLUS);
  sub_optab = init_optab (MINUS);
  smul_optab = init_optab (MULT);
  smul_widen_optab = init_optab (UNKNOWN);
  umul_widen_optab = init_optab (UNKNOWN);
  sdiv_optab = init_optab (DIV);
  sdivmod_optab = init_optab (UNKNOWN);
  udiv_optab = init_optab (UDIV);
  udivmod_optab = init_optab (UNKNOWN);
  smod_optab = init_optab (MOD);
  umod_optab = init_optab (UMOD);
  flodiv_optab = init_optab (DIV);
  ftrunc_optab = init_optab (UNKNOWN);
  and_optab = init_optab (AND);
  ior_optab = init_optab (IOR);
  xor_optab = init_optab (XOR);
  ashl_optab = init_optab (ASHIFT);
  ashr_optab = init_optab (ASHIFTRT);
  lshl_optab = init_optab (LSHIFT);
  lshr_optab = init_optab (LSHIFTRT);
  rotl_optab = init_optab (ROTATE);
  rotr_optab = init_optab (ROTATERT);
  smin_optab = init_optab (SMIN);
  smax_optab = init_optab (SMAX);
  umin_optab = init_optab (UMIN);
  umax_optab = init_optab (UMAX);
  mov_optab = init_optab (UNKNOWN);
  movstrict_optab = init_optab (UNKNOWN);
  cmp_optab = init_optab (UNKNOWN);
  ucmp_optab = init_optab (UNKNOWN);
  tst_optab = init_optab (UNKNOWN);
  neg_optab = init_optab (NEG);
  abs_optab = init_optab (ABS);
  one_cmpl_optab = init_optab (NOT);
  ffs_optab = init_optab (FFS);
  sqrt_optab = init_optab (SQRT);
  sin_optab = init_optab (UNKNOWN);
  cos_optab = init_optab (UNKNOWN);
  strlen_optab = init_optab (UNKNOWN);

#ifdef HAVE_addqi3
  if (HAVE_addqi3)
    add_optab->handlers[(int) QImode].insn_code = CODE_FOR_addqi3;
#endif
#ifdef HAVE_addhi3
  if (HAVE_addhi3)
    add_optab->handlers[(int) HImode].insn_code = CODE_FOR_addhi3;
#endif
#ifdef HAVE_addpsi3
  if (HAVE_addpsi3)
    add_optab->handlers[(int) PSImode].insn_code = CODE_FOR_addpsi3;
#endif
#ifdef HAVE_addsi3
  if (HAVE_addsi3)
    add_optab->handlers[(int) SImode].insn_code = CODE_FOR_addsi3;
#endif
#ifdef HAVE_adddi3
  if (HAVE_adddi3)
    add_optab->handlers[(int) DImode].insn_code = CODE_FOR_adddi3;
#endif
#ifdef HAVE_addti3
  if (HAVE_addti3)
    add_optab->handlers[(int) TImode].insn_code = CODE_FOR_addti3;
#endif
#ifdef HAVE_addsf3
  if (HAVE_addsf3)
    add_optab->handlers[(int) SFmode].insn_code = CODE_FOR_addsf3;
#endif
#ifdef HAVE_adddf3
  if (HAVE_adddf3)
    add_optab->handlers[(int) DFmode].insn_code = CODE_FOR_adddf3;
#endif
#ifdef HAVE_addxf3
  if (HAVE_addxf3)
    add_optab->handlers[(int) XFmode].insn_code = CODE_FOR_addxf3;
#endif
#ifdef HAVE_addtf3
  if (HAVE_addtf3)
    add_optab->handlers[(int) TFmode].insn_code = CODE_FOR_addtf3;
#endif
  init_integral_libfuncs (add_optab, "add", '3');
  init_floating_libfuncs (add_optab, "add", '3');

#ifdef HAVE_subqi3
  if (HAVE_subqi3)
    sub_optab->handlers[(int) QImode].insn_code = CODE_FOR_subqi3;
#endif
#ifdef HAVE_subhi3
  if (HAVE_subhi3)
    sub_optab->handlers[(int) HImode].insn_code = CODE_FOR_subhi3;
#endif
#ifdef HAVE_subpsi3
  if (HAVE_subpsi3)
    sub_optab->handlers[(int) PSImode].insn_code = CODE_FOR_subpsi3;
#endif
#ifdef HAVE_subsi3
  if (HAVE_subsi3)
    sub_optab->handlers[(int) SImode].insn_code = CODE_FOR_subsi3;
#endif
#ifdef HAVE_subdi3
  if (HAVE_subdi3)
    sub_optab->handlers[(int) DImode].insn_code = CODE_FOR_subdi3;
#endif
#ifdef HAVE_subti3
  if (HAVE_subti3)
    sub_optab->handlers[(int) TImode].insn_code = CODE_FOR_subti3;
#endif
#ifdef HAVE_subsf3
  if (HAVE_subsf3)
    sub_optab->handlers[(int) SFmode].insn_code = CODE_FOR_subsf3;
#endif
#ifdef HAVE_subdf3
  if (HAVE_subdf3)
    sub_optab->handlers[(int) DFmode].insn_code = CODE_FOR_subdf3;
#endif
#ifdef HAVE_subxf3
  if (HAVE_subxf3)
    sub_optab->handlers[(int) XFmode].insn_code = CODE_FOR_subxf3;
#endif
#ifdef HAVE_subtf3
  if (HAVE_subtf3)
    sub_optab->handlers[(int) TFmode].insn_code = CODE_FOR_subtf3;
#endif
  init_integral_libfuncs (sub_optab, "sub", '3');
  init_floating_libfuncs (sub_optab, "sub", '3');

#ifdef HAVE_mulqi3
  if (HAVE_mulqi3)
    smul_optab->handlers[(int) QImode].insn_code = CODE_FOR_mulqi3;
#endif
#ifdef HAVE_mulhi3
  if (HAVE_mulhi3)
    smul_optab->handlers[(int) HImode].insn_code = CODE_FOR_mulhi3;
#endif
#ifdef HAVE_mulpsi3
  if (HAVE_mulpsi3)
    smul_optab->handlers[(int) PSImode].insn_code = CODE_FOR_mulpsi3;
#endif
#ifdef HAVE_mulsi3
  if (HAVE_mulsi3)
    smul_optab->handlers[(int) SImode].insn_code = CODE_FOR_mulsi3;
#endif
#ifdef HAVE_muldi3
  if (HAVE_muldi3)
    smul_optab->handlers[(int) DImode].insn_code = CODE_FOR_muldi3;
#endif
#ifdef HAVE_multi3
  if (HAVE_multi3)
    smul_optab->handlers[(int) TImode].insn_code = CODE_FOR_multi3;
#endif
#ifdef HAVE_mulsf3
  if (HAVE_mulsf3)
    smul_optab->handlers[(int) SFmode].insn_code = CODE_FOR_mulsf3;
#endif
#ifdef HAVE_muldf3
  if (HAVE_muldf3)
    smul_optab->handlers[(int) DFmode].insn_code = CODE_FOR_muldf3;
#endif
#ifdef HAVE_mulxf3
  if (HAVE_mulxf3)
    smul_optab->handlers[(int) XFmode].insn_code = CODE_FOR_mulxf3;
#endif
#ifdef HAVE_multf3
  if (HAVE_multf3)
    smul_optab->handlers[(int) TFmode].insn_code = CODE_FOR_multf3;
#endif
  init_integral_libfuncs (smul_optab, "mul", '3');
  init_floating_libfuncs (smul_optab, "mul", '3');

#ifdef MULSI3_LIBCALL
  smul_optab->handlers[(int) SImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, MULSI3_LIBCALL);
#endif
#ifdef MULDI3_LIBCALL
  smul_optab->handlers[(int) DImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, MULDI3_LIBCALL);
#endif
#ifdef MULTI3_LIBCALL
  smul_optab->handlers[(int) TImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, MULTI3_LIBCALL);
#endif

#ifdef HAVE_mulqihi3
  if (HAVE_mulqihi3)
    smul_widen_optab->handlers[(int) HImode].insn_code = CODE_FOR_mulqihi3;
#endif
#ifdef HAVE_mulhisi3
  if (HAVE_mulhisi3)
    smul_widen_optab->handlers[(int) SImode].insn_code = CODE_FOR_mulhisi3;
#endif
#ifdef HAVE_mulsidi3
  if (HAVE_mulsidi3)
    smul_widen_optab->handlers[(int) DImode].insn_code = CODE_FOR_mulsidi3;
#endif
#ifdef HAVE_mulditi3
  if (HAVE_mulditi3)
    smul_widen_optab->handlers[(int) TImode].insn_code = CODE_FOR_mulditi3;
#endif

#ifdef HAVE_umulqihi3
  if (HAVE_umulqihi3)
    umul_widen_optab->handlers[(int) HImode].insn_code = CODE_FOR_umulqihi3;
#endif
#ifdef HAVE_umulhisi3
  if (HAVE_umulhisi3)
    umul_widen_optab->handlers[(int) SImode].insn_code = CODE_FOR_umulhisi3;
#endif
#ifdef HAVE_umulsidi3
  if (HAVE_umulsidi3)
    umul_widen_optab->handlers[(int) DImode].insn_code = CODE_FOR_umulsidi3;
#endif
#ifdef HAVE_umulditi3
  if (HAVE_umulditi3)
    umul_widen_optab->handlers[(int) TImode].insn_code = CODE_FOR_umulditi3;
#endif

#ifdef HAVE_divqi3
  if (HAVE_divqi3)
    sdiv_optab->handlers[(int) QImode].insn_code = CODE_FOR_divqi3;
#endif
#ifdef HAVE_divhi3
  if (HAVE_divhi3)
    sdiv_optab->handlers[(int) HImode].insn_code = CODE_FOR_divhi3;
#endif
#ifdef HAVE_divpsi3
  if (HAVE_divpsi3)
    sdiv_optab->handlers[(int) PSImode].insn_code = CODE_FOR_divpsi3;
#endif
#ifdef HAVE_divsi3
  if (HAVE_divsi3)
    sdiv_optab->handlers[(int) SImode].insn_code = CODE_FOR_divsi3;
#endif
#ifdef HAVE_divdi3
  if (HAVE_divdi3)
    sdiv_optab->handlers[(int) DImode].insn_code = CODE_FOR_divdi3;
#endif
#ifdef HAVE_divti3
  if (HAVE_divti3)
    sdiv_optab->handlers[(int) TImode].insn_code = CODE_FOR_divti3;
#endif
  init_integral_libfuncs (sdiv_optab, "div", '3');

#ifdef DIVSI3_LIBCALL
  sdiv_optab->handlers[(int) SImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, DIVSI3_LIBCALL);
#endif
#ifdef DIVDI3_LIBCALL
  sdiv_optab->handlers[(int) DImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, DIVDI3_LIBCALL);
#endif
#ifdef DIVTI3_LIBCALL
  sdiv_optab->handlers[(int) TImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, DIVTI3_LIBCALL);
#endif

#ifdef HAVE_udivqi3
  if (HAVE_udivqi3)
    udiv_optab->handlers[(int) QImode].insn_code = CODE_FOR_udivqi3;
#endif
#ifdef HAVE_udivhi3
  if (HAVE_udivhi3)
    udiv_optab->handlers[(int) HImode].insn_code = CODE_FOR_udivhi3;
#endif
#ifdef HAVE_udivpsi3
  if (HAVE_udivpsi3)
    udiv_optab->handlers[(int) PSImode].insn_code = CODE_FOR_udivpsi3;
#endif
#ifdef HAVE_udivsi3
  if (HAVE_udivsi3)
    udiv_optab->handlers[(int) SImode].insn_code = CODE_FOR_udivsi3;
#endif
#ifdef HAVE_udivdi3
  if (HAVE_udivdi3)
    udiv_optab->handlers[(int) DImode].insn_code = CODE_FOR_udivdi3;
#endif
#ifdef HAVE_udivti3
  if (HAVE_udivti3)
    udiv_optab->handlers[(int) TImode].insn_code = CODE_FOR_udivti3;
#endif
  init_integral_libfuncs (udiv_optab, "udiv", '3');

#ifdef UDIVSI3_LIBCALL
  udiv_optab->handlers[(int) SImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, UDIVSI3_LIBCALL);
#endif
#ifdef UDIVDI3_LIBCALL
  udiv_optab->handlers[(int) DImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, UDIVDI3_LIBCALL);
#endif
#ifdef UDIVTI3_LIBCALL
  udiv_optab->handlers[(int) TImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, UDIVTI3_LIBCALL);
#endif

#ifdef HAVE_divmodqi4
  if (HAVE_divmodqi4)
    sdivmod_optab->handlers[(int) QImode].insn_code = CODE_FOR_divmodqi4;
#endif
#ifdef HAVE_divmodhi4
  if (HAVE_divmodhi4)
    sdivmod_optab->handlers[(int) HImode].insn_code = CODE_FOR_divmodhi4;
#endif
#ifdef HAVE_divmodsi4
  if (HAVE_divmodsi4)
    sdivmod_optab->handlers[(int) SImode].insn_code = CODE_FOR_divmodsi4;
#endif
#ifdef HAVE_divmoddi4
  if (HAVE_divmoddi4)
    sdivmod_optab->handlers[(int) DImode].insn_code = CODE_FOR_divmoddi4;
#endif
#ifdef HAVE_divmodti4
  if (HAVE_divmodti4)
    sdivmod_optab->handlers[(int) TImode].insn_code = CODE_FOR_divmodti4;
#endif
  init_integral_libfuncs (sdivmod_optab, "divmod", '4');

#ifdef HAVE_udivmodqi4
  if (HAVE_udivmodqi4)
    udivmod_optab->handlers[(int) QImode].insn_code = CODE_FOR_udivmodqi4;
#endif
#ifdef HAVE_udivmodhi4
  if (HAVE_udivmodhi4)
    udivmod_optab->handlers[(int) HImode].insn_code = CODE_FOR_udivmodhi4;
#endif
#ifdef HAVE_udivmodsi4
  if (HAVE_udivmodsi4)
    udivmod_optab->handlers[(int) SImode].insn_code = CODE_FOR_udivmodsi4;
#endif
#ifdef HAVE_udivmoddi4
  if (HAVE_udivmoddi4)
    udivmod_optab->handlers[(int) DImode].insn_code = CODE_FOR_udivmoddi4;
#endif
#ifdef HAVE_udivmodti4
  if (HAVE_udivmodti4)
    udivmod_optab->handlers[(int) TImode].insn_code = CODE_FOR_udivmodti4;
#endif
  init_integral_libfuncs (udivmod_optab, "udivmod", '4');

#ifdef HAVE_modqi3
  if (HAVE_modqi3)
    smod_optab->handlers[(int) QImode].insn_code = CODE_FOR_modqi3;
#endif
#ifdef HAVE_modhi3
  if (HAVE_modhi3)
    smod_optab->handlers[(int) HImode].insn_code = CODE_FOR_modhi3;
#endif
#ifdef HAVE_modpsi3
  if (HAVE_modpsi3)
    smod_optab->handlers[(int) PSImode].insn_code = CODE_FOR_modpsi3;
#endif
#ifdef HAVE_modsi3
  if (HAVE_modsi3)
    smod_optab->handlers[(int) SImode].insn_code = CODE_FOR_modsi3;
#endif
#ifdef HAVE_moddi3
  if (HAVE_moddi3)
    smod_optab->handlers[(int) DImode].insn_code = CODE_FOR_moddi3;
#endif
#ifdef HAVE_modti3
  if (HAVE_modti3)
    smod_optab->handlers[(int) TImode].insn_code = CODE_FOR_modti3;
#endif
  init_integral_libfuncs (smod_optab, "mod", '3');

#ifdef MODSI3_LIBCALL
  smod_optab->handlers[(int) SImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, MODSI3_LIBCALL);
#endif
#ifdef MODDI3_LIBCALL
  smod_optab->handlers[(int) DImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, MODDI3_LIBCALL);
#endif
#ifdef MODTI3_LIBCALL
  smod_optab->handlers[(int) TImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, MODTI3_LIBCALL);
#endif

#ifdef HAVE_umodqi3
  if (HAVE_umodqi3)
    umod_optab->handlers[(int) QImode].insn_code = CODE_FOR_umodqi3;
#endif
#ifdef HAVE_umodhi3
  if (HAVE_umodhi3)
    umod_optab->handlers[(int) HImode].insn_code = CODE_FOR_umodhi3;
#endif
#ifdef HAVE_umodpsi3
  if (HAVE_umodpsi3)
    umod_optab->handlers[(int) PSImode].insn_code = CODE_FOR_umodpsi3;
#endif
#ifdef HAVE_umodsi3
  if (HAVE_umodsi3)
    umod_optab->handlers[(int) SImode].insn_code = CODE_FOR_umodsi3;
#endif
#ifdef HAVE_umoddi3
  if (HAVE_umoddi3)
    umod_optab->handlers[(int) DImode].insn_code = CODE_FOR_umoddi3;
#endif
#ifdef HAVE_umodti3
  if (HAVE_umodti3)
    umod_optab->handlers[(int) TImode].insn_code = CODE_FOR_umodti3;
#endif
  init_integral_libfuncs (umod_optab, "umod", '3');

#ifdef UMODSI3_LIBCALL
  umod_optab->handlers[(int) SImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, UMODSI3_LIBCALL);
#endif
#ifdef UMODDI3_LIBCALL
  umod_optab->handlers[(int) DImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, UMODDI3_LIBCALL);
#endif
#ifdef UMODTI3_LIBCALL
  umod_optab->handlers[(int) TImode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, UMODTI3_LIBCALL);
#endif

#ifdef HAVE_divsf3
  if (HAVE_divsf3)
    flodiv_optab->handlers[(int) SFmode].insn_code = CODE_FOR_divsf3;
#endif
#ifdef HAVE_divdf3
  if (HAVE_divdf3)
    flodiv_optab->handlers[(int) DFmode].insn_code = CODE_FOR_divdf3;
#endif
#ifdef HAVE_divxf3
  if (HAVE_divxf3)
    flodiv_optab->handlers[(int) XFmode].insn_code = CODE_FOR_divxf3;
#endif
#ifdef HAVE_divtf3
  if (HAVE_divtf3)
    flodiv_optab->handlers[(int) TFmode].insn_code = CODE_FOR_divtf3;
#endif
  init_floating_libfuncs (flodiv_optab, "div", '3');

#ifdef HAVE_ftruncsf2
  if (HAVE_ftruncsf2)
    ftrunc_optab->handlers[(int) SFmode].insn_code = CODE_FOR_ftruncsf2;
#endif
#ifdef HAVE_ftruncdf2
  if (HAVE_ftruncdf2)
    ftrunc_optab->handlers[(int) DFmode].insn_code = CODE_FOR_ftruncdf2;
#endif
#ifdef HAVE_ftruncxf2
  if (HAVE_ftruncxf2)
    ftrunc_optab->handlers[(int) XFmode].insn_code = CODE_FOR_ftruncxf2;
#endif
#ifdef HAVE_ftrunctf2
  if (HAVE_ftrunctf2)
    ftrunc_optab->handlers[(int) TFmode].insn_code = CODE_FOR_ftrunctf2;
#endif
  init_floating_libfuncs (ftrunc_optab, "ftrunc", '2');

#ifdef HAVE_andqi3
  if (HAVE_andqi3)
    and_optab->handlers[(int) QImode].insn_code = CODE_FOR_andqi3;
#endif
#ifdef HAVE_andhi3
  if (HAVE_andhi3)
    and_optab->handlers[(int) HImode].insn_code = CODE_FOR_andhi3;
#endif
#ifdef HAVE_andpsi3
  if (HAVE_andpsi3)
    and_optab->handlers[(int) PSImode].insn_code = CODE_FOR_andpsi3;
#endif
#ifdef HAVE_andsi3
  if (HAVE_andsi3)
    and_optab->handlers[(int) SImode].insn_code = CODE_FOR_andsi3;
#endif
#ifdef HAVE_anddi3
  if (HAVE_anddi3)
    and_optab->handlers[(int) DImode].insn_code = CODE_FOR_anddi3;
#endif
#ifdef HAVE_andti3
  if (HAVE_andti3)
    and_optab->handlers[(int) TImode].insn_code = CODE_FOR_andti3;
#endif
  init_integral_libfuncs (and_optab, "and", '3');

#ifdef HAVE_iorqi3
  if (HAVE_iorqi3)
    ior_optab->handlers[(int) QImode].insn_code = CODE_FOR_iorqi3;
#endif
#ifdef HAVE_iorhi3
  if (HAVE_iorhi3)
    ior_optab->handlers[(int) HImode].insn_code = CODE_FOR_iorhi3;
#endif
#ifdef HAVE_iorpsi3
  if (HAVE_iorpsi3)
    ior_optab->handlers[(int) PSImode].insn_code = CODE_FOR_iorpsi3;
#endif
#ifdef HAVE_iorsi3
  if (HAVE_iorsi3)
    ior_optab->handlers[(int) SImode].insn_code = CODE_FOR_iorsi3;
#endif
#ifdef HAVE_iordi3
  if (HAVE_iordi3)
    ior_optab->handlers[(int) DImode].insn_code = CODE_FOR_iordi3;
#endif
#ifdef HAVE_iorti3
  if (HAVE_iorti3)
    ior_optab->handlers[(int) TImode].insn_code = CODE_FOR_iorti3;
#endif
  init_integral_libfuncs (ior_optab, "ior", '3');

#ifdef HAVE_xorqi3
  if (HAVE_xorqi3)
    xor_optab->handlers[(int) QImode].insn_code = CODE_FOR_xorqi3;
#endif
#ifdef HAVE_xorhi3
  if (HAVE_xorhi3)
    xor_optab->handlers[(int) HImode].insn_code = CODE_FOR_xorhi3;
#endif
#ifdef HAVE_xorpsi3
  if (HAVE_xorpsi3)
    xor_optab->handlers[(int) PSImode].insn_code = CODE_FOR_xorpsi3;
#endif
#ifdef HAVE_xorsi3
  if (HAVE_xorsi3)
    xor_optab->handlers[(int) SImode].insn_code = CODE_FOR_xorsi3;
#endif
#ifdef HAVE_xordi3
  if (HAVE_xordi3)
    xor_optab->handlers[(int) DImode].insn_code = CODE_FOR_xordi3;
#endif
#ifdef HAVE_xorti3
  if (HAVE_xorti3)
    xor_optab->handlers[(int) TImode].insn_code = CODE_FOR_xorti3;
#endif
  init_integral_libfuncs (xor_optab, "xor", '3');

#ifdef HAVE_ashlqi3
  if (HAVE_ashlqi3)
    ashl_optab->handlers[(int) QImode].insn_code = CODE_FOR_ashlqi3;
#endif
#ifdef HAVE_ashlhi3
  if (HAVE_ashlhi3)
    ashl_optab->handlers[(int) HImode].insn_code = CODE_FOR_ashlhi3;
#endif
#ifdef HAVE_ashlpsi3
  if (HAVE_ashlpsi3)
    ashl_optab->handlers[(int) PSImode].insn_code = CODE_FOR_ashlpsi3;
#endif
#ifdef HAVE_ashlsi3
  if (HAVE_ashlsi3)
    ashl_optab->handlers[(int) SImode].insn_code = CODE_FOR_ashlsi3;
#endif
#ifdef HAVE_ashldi3
  if (HAVE_ashldi3)
    ashl_optab->handlers[(int) DImode].insn_code = CODE_FOR_ashldi3;
#endif
#ifdef HAVE_ashlti3
  if (HAVE_ashlti3)
    ashl_optab->handlers[(int) TImode].insn_code = CODE_FOR_ashlti3;
#endif
  init_integral_libfuncs (ashl_optab, "ashl", '3');

#ifdef HAVE_ashrqi3
  if (HAVE_ashrqi3)
    ashr_optab->handlers[(int) QImode].insn_code = CODE_FOR_ashrqi3;
#endif
#ifdef HAVE_ashrhi3
  if (HAVE_ashrhi3)
    ashr_optab->handlers[(int) HImode].insn_code = CODE_FOR_ashrhi3;
#endif
#ifdef HAVE_ashrpsi3
  if (HAVE_ashrpsi3)
    ashr_optab->handlers[(int) PSImode].insn_code = CODE_FOR_ashrpsi3;
#endif
#ifdef HAVE_ashrsi3
  if (HAVE_ashrsi3)
    ashr_optab->handlers[(int) SImode].insn_code = CODE_FOR_ashrsi3;
#endif
#ifdef HAVE_ashrdi3
  if (HAVE_ashrdi3)
    ashr_optab->handlers[(int) DImode].insn_code = CODE_FOR_ashrdi3;
#endif
#ifdef HAVE_ashrti3
  if (HAVE_ashrti3)
    ashr_optab->handlers[(int) TImode].insn_code = CODE_FOR_ashrti3;
#endif
  init_integral_libfuncs (ashr_optab, "ashr", '3');

#ifdef HAVE_lshlqi3
  if (HAVE_lshlqi3)
    lshl_optab->handlers[(int) QImode].insn_code = CODE_FOR_lshlqi3;
#endif
#ifdef HAVE_lshlhi3
  if (HAVE_lshlhi3)
    lshl_optab->handlers[(int) HImode].insn_code = CODE_FOR_lshlhi3;
#endif
#ifdef HAVE_lshlpsi3
  if (HAVE_lshlpsi3)
    lshl_optab->handlers[(int) PSImode].insn_code = CODE_FOR_lshlpsi3;
#endif
#ifdef HAVE_lshlsi3
  if (HAVE_lshlsi3)
    lshl_optab->handlers[(int) SImode].insn_code = CODE_FOR_lshlsi3;
#endif
#ifdef HAVE_lshldi3
  if (HAVE_lshldi3)
    lshl_optab->handlers[(int) DImode].insn_code = CODE_FOR_lshldi3;
#endif
#ifdef HAVE_lshlti3
  if (HAVE_lshlti3)
    lshl_optab->handlers[(int) TImode].insn_code = CODE_FOR_lshlti3;
#endif
  init_integral_libfuncs (lshl_optab, "lshl", '3');

#ifdef HAVE_lshrqi3
  if (HAVE_lshrqi3)
    lshr_optab->handlers[(int) QImode].insn_code = CODE_FOR_lshrqi3;
#endif
#ifdef HAVE_lshrhi3
  if (HAVE_lshrhi3)
    lshr_optab->handlers[(int) HImode].insn_code = CODE_FOR_lshrhi3;
#endif
#ifdef HAVE_lshrpsi3
  if (HAVE_lshrpsi3)
    lshr_optab->handlers[(int) PSImode].insn_code = CODE_FOR_lshrpsi3;
#endif
#ifdef HAVE_lshrsi3
  if (HAVE_lshrsi3)
    lshr_optab->handlers[(int) SImode].insn_code = CODE_FOR_lshrsi3;
#endif
#ifdef HAVE_lshrdi3
  if (HAVE_lshrdi3)
    lshr_optab->handlers[(int) DImode].insn_code = CODE_FOR_lshrdi3;
#endif
#ifdef HAVE_lshrti3
  if (HAVE_lshrti3)
    lshr_optab->handlers[(int) TImode].insn_code = CODE_FOR_lshrti3;
#endif
  init_integral_libfuncs (lshr_optab, "lshr", '3');

#ifdef HAVE_rotlqi3
  if (HAVE_rotlqi3)
    rotl_optab->handlers[(int) QImode].insn_code = CODE_FOR_rotlqi3;
#endif
#ifdef HAVE_rotlhi3
  if (HAVE_rotlhi3)
    rotl_optab->handlers[(int) HImode].insn_code = CODE_FOR_rotlhi3;
#endif
#ifdef HAVE_rotlpsi3
  if (HAVE_rotlpsi3)
    rotl_optab->handlers[(int) PSImode].insn_code = CODE_FOR_rotlpsi3;
#endif
#ifdef HAVE_rotlsi3
  if (HAVE_rotlsi3)
    rotl_optab->handlers[(int) SImode].insn_code = CODE_FOR_rotlsi3;
#endif
#ifdef HAVE_rotldi3
  if (HAVE_rotldi3)
    rotl_optab->handlers[(int) DImode].insn_code = CODE_FOR_rotldi3;
#endif
#ifdef HAVE_rotlti3
  if (HAVE_rotlti3)
    rotl_optab->handlers[(int) TImode].insn_code = CODE_FOR_rotlti3;
#endif
  init_integral_libfuncs (rotl_optab, "rotl", '3');

#ifdef HAVE_rotrqi3
  if (HAVE_rotrqi3)
    rotr_optab->handlers[(int) QImode].insn_code = CODE_FOR_rotrqi3;
#endif
#ifdef HAVE_rotrhi3
  if (HAVE_rotrhi3)
    rotr_optab->handlers[(int) HImode].insn_code = CODE_FOR_rotrhi3;
#endif
#ifdef HAVE_rotrpsi3
  if (HAVE_rotrpsi3)
    rotr_optab->handlers[(int) PSImode].insn_code = CODE_FOR_rotrpsi3;
#endif
#ifdef HAVE_rotrsi3
  if (HAVE_rotrsi3)
    rotr_optab->handlers[(int) SImode].insn_code = CODE_FOR_rotrsi3;
#endif
#ifdef HAVE_rotrdi3
  if (HAVE_rotrdi3)
    rotr_optab->handlers[(int) DImode].insn_code = CODE_FOR_rotrdi3;
#endif
#ifdef HAVE_rotrti3
  if (HAVE_rotrti3)
    rotr_optab->handlers[(int) TImode].insn_code = CODE_FOR_rotrti3;
#endif
  init_integral_libfuncs (rotr_optab, "rotr", '3');

#ifdef HAVE_sminqi3
  if (HAVE_sminqi3)
    smin_optab->handlers[(int) QImode].insn_code = CODE_FOR_sminqi3;
#endif
#ifdef HAVE_sminhi3
  if (HAVE_sminhi3)
    smin_optab->handlers[(int) HImode].insn_code = CODE_FOR_sminhi3;
#endif
#ifdef HAVE_sminsi3
  if (HAVE_sminsi3)
    smin_optab->handlers[(int) SImode].insn_code = CODE_FOR_sminsi3;
#endif
#ifdef HAVE_smindi3
  if (HAVE_smindi3)
    smin_optab->handlers[(int) DImode].insn_code = CODE_FOR_smindi3;
#endif
#ifdef HAVE_sminti3
  if (HAVE_sminti3)
    smin_optab->handlers[(int) TImode].insn_code = CODE_FOR_sminti3;
#endif
#ifdef HAVE_minsf3
  if (HAVE_minsf3)
    smin_optab->handlers[(int) SFmode].insn_code = CODE_FOR_minsf3;
#endif
#ifdef HAVE_mindf3
  if (HAVE_mindf3)
    smin_optab->handlers[(int) DFmode].insn_code = CODE_FOR_mindf3;
#endif
#ifdef HAVE_minxf3
  if (HAVE_minxf3)
    smin_optab->handlers[(int) XFmode].insn_code = CODE_FOR_minxf3;
#endif
#ifdef HAVE_mintf3
  if (HAVE_mintf3)
    smin_optab->handlers[(int) TFmode].insn_code = CODE_FOR_mintf3;
#endif
  init_integral_libfuncs (smin_optab, "min", '3');
  init_floating_libfuncs (smin_optab, "min", '3');

#ifdef HAVE_smaxqi3
  if (HAVE_smaxqi3)
    smax_optab->handlers[(int) QImode].insn_code = CODE_FOR_smaxqi3;
#endif
#ifdef HAVE_smaxhi3
  if (HAVE_smaxhi3)
    smax_optab->handlers[(int) HImode].insn_code = CODE_FOR_smaxhi3;
#endif
#ifdef HAVE_smaxsi3
  if (HAVE_smaxsi3)
    smax_optab->handlers[(int) SImode].insn_code = CODE_FOR_smaxsi3;
#endif
#ifdef HAVE_smaxdi3
  if (HAVE_smaxdi3)
    smax_optab->handlers[(int) DImode].insn_code = CODE_FOR_smaxdi3;
#endif
#ifdef HAVE_smaxti3
  if (HAVE_smaxti3)
    smax_optab->handlers[(int) TImode].insn_code = CODE_FOR_smaxti3;
#endif
#ifdef HAVE_maxsf3
  if (HAVE_maxsf3)
    smax_optab->handlers[(int) SFmode].insn_code = CODE_FOR_maxsf3;
#endif
#ifdef HAVE_maxdf3
  if (HAVE_maxdf3)
    smax_optab->handlers[(int) DFmode].insn_code = CODE_FOR_maxdf3;
#endif
#ifdef HAVE_maxxf3
  if (HAVE_maxxf3)
    smax_optab->handlers[(int) XFmode].insn_code = CODE_FOR_maxxf3;
#endif
#ifdef HAVE_maxtf3
  if (HAVE_maxtf3)
    smax_optab->handlers[(int) TFmode].insn_code = CODE_FOR_maxtf3;
#endif
  init_integral_libfuncs (smax_optab, "max", '3');
  init_floating_libfuncs (smax_optab, "max", '3');

#ifdef HAVE_uminqi3
  if (HAVE_uminqi3)
    umin_optab->handlers[(int) QImode].insn_code = CODE_FOR_uminqi3;
#endif
#ifdef HAVE_uminhi3
  if (HAVE_uminhi3)
    umin_optab->handlers[(int) HImode].insn_code = CODE_FOR_uminhi3;
#endif
#ifdef HAVE_uminsi3
  if (HAVE_uminsi3)
    umin_optab->handlers[(int) SImode].insn_code = CODE_FOR_uminsi3;
#endif
#ifdef HAVE_umindi3
  if (HAVE_umindi3)
    umin_optab->handlers[(int) DImode].insn_code = CODE_FOR_umindi3;
#endif
#ifdef HAVE_uminti3
  if (HAVE_uminti3)
    umin_optab->handlers[(int) TImode].insn_code = CODE_FOR_uminti3;
#endif
  init_integral_libfuncs (umin_optab, "umin", '3');

#ifdef HAVE_umaxqi3
  if (HAVE_umaxqi3)
    umax_optab->handlers[(int) QImode].insn_code = CODE_FOR_umaxqi3;
#endif
#ifdef HAVE_umaxhi3
  if (HAVE_umaxhi3)
    umax_optab->handlers[(int) HImode].insn_code = CODE_FOR_umaxhi3;
#endif
#ifdef HAVE_umaxsi3
  if (HAVE_umaxsi3)
    umax_optab->handlers[(int) SImode].insn_code = CODE_FOR_umaxsi3;
#endif
#ifdef HAVE_umaxdi3
  if (HAVE_umaxdi3)
    umax_optab->handlers[(int) DImode].insn_code = CODE_FOR_umaxdi3;
#endif
#ifdef HAVE_umaxti3
  if (HAVE_umaxti3)
    umax_optab->handlers[(int) TImode].insn_code = CODE_FOR_umaxti3;
#endif
  init_integral_libfuncs (umax_optab, "umax", '3');

#ifdef HAVE_negqi2
  if (HAVE_negqi2)
    neg_optab->handlers[(int) QImode].insn_code = CODE_FOR_negqi2;
#endif
#ifdef HAVE_neghi2
  if (HAVE_neghi2)
    neg_optab->handlers[(int) HImode].insn_code = CODE_FOR_neghi2;
#endif
#ifdef HAVE_negpsi2
  if (HAVE_negpsi2)
    neg_optab->handlers[(int) PSImode].insn_code = CODE_FOR_negpsi2;
#endif
#ifdef HAVE_negsi2
  if (HAVE_negsi2)
    neg_optab->handlers[(int) SImode].insn_code = CODE_FOR_negsi2;
#endif
#ifdef HAVE_negdi2
  if (HAVE_negdi2)
    neg_optab->handlers[(int) DImode].insn_code = CODE_FOR_negdi2;
#endif
#ifdef HAVE_negti2
  if (HAVE_negti2)
    neg_optab->handlers[(int) TImode].insn_code = CODE_FOR_negti2;
#endif
#ifdef HAVE_negsf2
  if (HAVE_negsf2)
    neg_optab->handlers[(int) SFmode].insn_code = CODE_FOR_negsf2;
#endif
#ifdef HAVE_negdf2
  if (HAVE_negdf2)
    neg_optab->handlers[(int) DFmode].insn_code = CODE_FOR_negdf2;
#endif
#ifdef HAVE_negxf2
  if (HAVE_negxf2)
    neg_optab->handlers[(int) XFmode].insn_code = CODE_FOR_negxf2;
#endif
#ifdef HAVE_negtf2
  if (HAVE_negtf2)
    neg_optab->handlers[(int) TFmode].insn_code = CODE_FOR_negtf2;
#endif
  init_integral_libfuncs (neg_optab, "neg", '2');
  init_floating_libfuncs (neg_optab, "neg", '2');

#ifdef HAVE_absqi2
  if (HAVE_absqi2)
    abs_optab->handlers[(int) QImode].insn_code = CODE_FOR_absqi2;
#endif
#ifdef HAVE_abshi2
  if (HAVE_abshi2)
    abs_optab->handlers[(int) HImode].insn_code = CODE_FOR_abshi2;
#endif
#ifdef HAVE_abspsi2
  if (HAVE_abspsi2)
    abs_optab->handlers[(int) PSImode].insn_code = CODE_FOR_abspsi2;
#endif
#ifdef HAVE_abssi2
  if (HAVE_abssi2)
    abs_optab->handlers[(int) SImode].insn_code = CODE_FOR_abssi2;
#endif
#ifdef HAVE_absdi2
  if (HAVE_absdi2)
    abs_optab->handlers[(int) DImode].insn_code = CODE_FOR_absdi2;
#endif
#ifdef HAVE_absti2
  if (HAVE_absti2)
    abs_optab->handlers[(int) TImode].insn_code = CODE_FOR_absti2;
#endif
#ifdef HAVE_abssf2
  if (HAVE_abssf2)
    abs_optab->handlers[(int) SFmode].insn_code = CODE_FOR_abssf2;
#endif
#ifdef HAVE_absdf2
  if (HAVE_absdf2)
    abs_optab->handlers[(int) DFmode].insn_code = CODE_FOR_absdf2;
#endif
#ifdef HAVE_absxf2
  if (HAVE_absxf2)
    abs_optab->handlers[(int) XFmode].insn_code = CODE_FOR_absxf2;
#endif
#ifdef HAVE_abstf2
  if (HAVE_abstf2)
    abs_optab->handlers[(int) TFmode].insn_code = CODE_FOR_abstf2;
#endif

  /* Use cabs for DC complex abs, since systems generally have cabs.
     Don't define any libcall for SCmode, so that cabs will be used.  */
  abs_optab->handlers[(int) DCmode].libfunc
    = gen_rtx (SYMBOL_REF, Pmode, "cabs");

#ifdef HAVE_sqrtqi2
  if (HAVE_sqrtqi2)
    sqrt_optab->handlers[(int) QImode].insn_code = CODE_FOR_sqrtqi2;
#endif
#ifdef HAVE_sqrthi2
  if (HAVE_sqrthi2)
    sqrt_optab->handlers[(int) HImode].insn_code = CODE_FOR_sqrthi2;
#endif
#ifdef HAVE_sqrtpsi2
  if (HAVE_sqrtpsi2)
    sqrt_optab->handlers[(int) PSImode].insn_code = CODE_FOR_sqrtpsi2;
#endif
#ifdef HAVE_sqrtsi2
  if (HAVE_sqrtsi2)
    sqrt_optab->handlers[(int) SImode].insn_code = CODE_FOR_sqrtsi2;
#endif
#ifdef HAVE_sqrtdi2
  if (HAVE_sqrtdi2)
    sqrt_optab->handlers[(int) DImode].insn_code = CODE_FOR_sqrtdi2;
#endif
#ifdef HAVE_sqrtti2
  if (HAVE_sqrtti2)
    sqrt_optab->handlers[(int) TImode].insn_code = CODE_FOR_sqrtti2;
#endif
#ifdef HAVE_sqrtsf2
  if (HAVE_sqrtsf2)
    sqrt_optab->handlers[(int) SFmode].insn_code = CODE_FOR_sqrtsf2;
#endif
#ifdef HAVE_sqrtdf2
  if (HAVE_sqrtdf2)
    sqrt_optab->handlers[(int) DFmode].insn_code = CODE_FOR_sqrtdf2;
#endif
#ifdef HAVE_sqrttf2
  if (HAVE_sqrttf2)
    sqrt_optab->handlers[(int) TFmode].insn_code = CODE_FOR_sqrttf2;
#endif
  /* No library calls here!  If there is no sqrt instruction expand_builtin
     should force the library call.  */

#ifdef HAVE_sinsf2
  if (HAVE_sinsf2)
    sin_optab->handlers[(int) SFmode].insn_code = CODE_FOR_sinsf2;
#endif
#ifdef HAVE_sindf2
  if (HAVE_sindf2)
    sin_optab->handlers[(int) DFmode].insn_code = CODE_FOR_sindf2;
#endif
#ifdef HAVE_sintf2
  if (HAVE_sintf2)
    sin_optab->handlers[(int) TFmode].insn_code = CODE_FOR_sintf2;
#endif
  /* No library calls here!  If there is no sin instruction expand_builtin
     should force the library call.  */

#ifdef HAVE_cossf2
  if (HAVE_cossf2)
    cos_optab->handlers[(int) SFmode].insn_code = CODE_FOR_cossf2;
#endif
#ifdef HAVE_cosdf2
  if (HAVE_cosdf2)
    cos_optab->handlers[(int) DFmode].insn_code = CODE_FOR_cosdf2;
#endif
#ifdef HAVE_costf2
  if (HAVE_costf2)
    cos_optab->handlers[(int) TFmode].insn_code = CODE_FOR_costf2;
#endif
  /* No library calls here!  If there is no cos instruction expand_builtin
     should force the library call.  */

#ifdef HAVE_strlenqi
  if (HAVE_strlenqi)
    strlen_optab->handlers[(int) QImode].insn_code = CODE_FOR_strlenqi;
#endif
#ifdef HAVE_strlenhi
  if (HAVE_strlenhi)
    strlen_optab->handlers[(int) HImode].insn_code = CODE_FOR_strlenhi;
#endif
#ifdef HAVE_strlenpsi
  if (HAVE_strlenpsi)
    strlen_optab->handlers[(int) PSImode].insn_code = CODE_FOR_strlenpsi;
#endif
#ifdef HAVE_strlensi
  if (HAVE_strlensi)
    strlen_optab->handlers[(int) SImode].insn_code = CODE_FOR_strlensi;
#endif
#ifdef HAVE_strlendi
  if (HAVE_strlendi)
    strlen_optab->handlers[(int) DImode].insn_code = CODE_FOR_strlendi;
#endif
#ifdef HAVE_strlenti
  if (HAVE_strlenti)
    strlen_optab->handlers[(int) TImode].insn_code = CODE_FOR_strlenti;
#endif
  /* No library calls here!  If there is no strlen instruction expand_builtin
     should force the library call.  */

#ifdef HAVE_one_cmplqi2
  if (HAVE_one_cmplqi2)
    one_cmpl_optab->handlers[(int) QImode].insn_code = CODE_FOR_one_cmplqi2;
#endif
#ifdef HAVE_one_cmplhi2
  if (HAVE_one_cmplhi2)
    one_cmpl_optab->handlers[(int) HImode].insn_code = CODE_FOR_one_cmplhi2;
#endif
#ifdef HAVE_one_cmplpsi2
  if (HAVE_one_cmplpsi2)
    one_cmpl_optab->handlers[(int) PSImode].insn_code = CODE_FOR_one_cmplpsi2;
#endif
#ifdef HAVE_one_cmplsi2
  if (HAVE_one_cmplsi2)
    one_cmpl_optab->handlers[(int) SImode].insn_code = CODE_FOR_one_cmplsi2;
#endif
#ifdef HAVE_one_cmpldi2
  if (HAVE_one_cmpldi2)
    one_cmpl_optab->handlers[(int) DImode].insn_code = CODE_FOR_one_cmpldi2;
#endif
#ifdef HAVE_one_cmplti2
  if (HAVE_one_cmplti2)
    one_cmpl_optab->handlers[(int) TImode].insn_code = CODE_FOR_one_cmplti2;
#endif
  init_integral_libfuncs (one_cmpl_optab, "one_cmpl", '2');

#ifdef HAVE_ffsqi2
  if (HAVE_ffsqi2)
    ffs_optab->handlers[(int) QImode].insn_code = CODE_FOR_ffsqi2;
#endif
#ifdef HAVE_ffshi2
  if (HAVE_ffshi2)
    ffs_optab->handlers[(int) HImode].insn_code = CODE_FOR_ffshi2;
#endif
#ifdef HAVE_ffspsi2
  if (HAVE_ffspsi2)
    ffs_optab->handlers[(int) PSImode].insn_code = CODE_FOR_ffspsi2;
#endif
#ifdef HAVE_ffssi2
  if (HAVE_ffssi2)
    ffs_optab->handlers[(int) SImode].insn_code = CODE_FOR_ffssi2;
#endif
#ifdef HAVE_ffsdi2
  if (HAVE_ffsdi2)
    ffs_optab->handlers[(int) DImode].insn_code = CODE_FOR_ffsdi2;
#endif
#ifdef HAVE_ffsti2
  if (HAVE_ffsti2)
    ffs_optab->handlers[(int) TImode].insn_code = CODE_FOR_ffsti2;
#endif
  init_integral_libfuncs (ffs_optab, "ffs", '2');

#ifdef HAVE_movqi
  if (HAVE_movqi)
    mov_optab->handlers[(int) QImode].insn_code = CODE_FOR_movqi;
#endif
#ifdef HAVE_movhi
  if (HAVE_movhi)
    mov_optab->handlers[(int) HImode].insn_code = CODE_FOR_movhi;
#endif
#ifdef HAVE_movpsi
  if (HAVE_movpsi)
    mov_optab->handlers[(int) PSImode].insn_code = CODE_FOR_movpsi;
#endif
#ifdef HAVE_movsi
  if (HAVE_movsi)
    mov_optab->handlers[(int) SImode].insn_code = CODE_FOR_movsi;
#endif
#ifdef HAVE_movdi
  if (HAVE_movdi)
    mov_optab->handlers[(int) DImode].insn_code = CODE_FOR_movdi;
#endif
#ifdef HAVE_movti
  if (HAVE_movti)
    mov_optab->handlers[(int) TImode].insn_code = CODE_FOR_movti;
#endif
#ifdef HAVE_movsf
  if (HAVE_movsf)
    mov_optab->handlers[(int) SFmode].insn_code = CODE_FOR_movsf;
#endif
#ifdef HAVE_movdf
  if (HAVE_movdf)
    mov_optab->handlers[(int) DFmode].insn_code = CODE_FOR_movdf;
#endif
#ifdef HAVE_movxf
  if (HAVE_movxf)
    mov_optab->handlers[(int) XFmode].insn_code = CODE_FOR_movxf;
#endif
#ifdef HAVE_movtf
  if (HAVE_movtf)
    mov_optab->handlers[(int) TFmode].insn_code = CODE_FOR_movtf;
#endif
#ifdef HAVE_movcc
  if (HAVE_movcc)
    mov_optab->handlers[(int) CCmode].insn_code = CODE_FOR_movcc;
#endif

#ifdef EXTRA_CC_MODES
  init_mov_optab ();
#endif

#ifdef HAVE_movstrictqi
  if (HAVE_movstrictqi)
    movstrict_optab->handlers[(int) QImode].insn_code = CODE_FOR_movstrictqi;
#endif
#ifdef HAVE_movstricthi
  if (HAVE_movstricthi)
    movstrict_optab->handlers[(int) HImode].insn_code = CODE_FOR_movstricthi;
#endif
#ifdef HAVE_movstrictpsi
  if (HAVE_movstrictpsi)
    movstrict_optab->handlers[(int) PSImode].insn_code = CODE_FOR_movstrictpsi;
#endif
#ifdef HAVE_movstrictsi
  if (HAVE_movstrictsi)
    movstrict_optab->handlers[(int) SImode].insn_code = CODE_FOR_movstrictsi;
#endif
#ifdef HAVE_movstrictdi
  if (HAVE_movstrictdi)
    movstrict_optab->handlers[(int) DImode].insn_code = CODE_FOR_movstrictdi;
#endif
#ifdef HAVE_movstrictti
  if (HAVE_movstrictti)
    movstrict_optab->handlers[(int) TImode].insn_code = CODE_FOR_movstrictti;
#endif

#ifdef HAVE_cmpqi
  if (HAVE_cmpqi)
    cmp_optab->handlers[(int) QImode].insn_code = CODE_FOR_cmpqi;
#endif
#ifdef HAVE_cmphi
  if (HAVE_cmphi)
    cmp_optab->handlers[(int) HImode].insn_code = CODE_FOR_cmphi;
#endif
#ifdef HAVE_cmppsi
  if (HAVE_cmppsi)
    cmp_optab->handlers[(int) PSImode].insn_code = CODE_FOR_cmppsi;
#endif
#ifdef HAVE_cmpsi
  if (HAVE_cmpsi)
    cmp_optab->handlers[(int) SImode].insn_code = CODE_FOR_cmpsi;
#endif
#ifdef HAVE_cmpdi
  if (HAVE_cmpdi)
    cmp_optab->handlers[(int) DImode].insn_code = CODE_FOR_cmpdi;
#endif
#ifdef HAVE_cmpti
  if (HAVE_cmpti)
    cmp_optab->handlers[(int) TImode].insn_code = CODE_FOR_cmpti;
#endif
#ifdef HAVE_cmpsf
  if (HAVE_cmpsf)
    cmp_optab->handlers[(int) SFmode].insn_code = CODE_FOR_cmpsf;
#endif
#ifdef HAVE_cmpdf
  if (HAVE_cmpdf)
    cmp_optab->handlers[(int) DFmode].insn_code = CODE_FOR_cmpdf;
#endif
#ifdef HAVE_cmpxf
  if (HAVE_cmpxf)
    cmp_optab->handlers[(int) XFmode].insn_code = CODE_FOR_cmpxf;
#endif
#ifdef HAVE_cmptf
  if (HAVE_cmptf)
    cmp_optab->handlers[(int) TFmode].insn_code = CODE_FOR_cmptf;
#endif
  /* Comparison libcalls for integers MUST come in pairs, signed/unsigned.  */
  init_integral_libfuncs (cmp_optab, "cmp", '2');
  init_integral_libfuncs (ucmp_optab, "ucmp", '2');
  init_floating_libfuncs (cmp_optab, "cmp", '2');

#ifdef HAVE_tstqi
  if (HAVE_tstqi)
    tst_optab->handlers[(int) QImode].insn_code = CODE_FOR_tstqi;
#endif
#ifdef HAVE_tsthi
  if (HAVE_tsthi)
    tst_optab->handlers[(int) HImode].insn_code = CODE_FOR_tsthi;
#endif
#ifdef HAVE_tstpsi
  if (HAVE_tstpsi)
    tst_optab->handlers[(int) PSImode].insn_code = CODE_FOR_tstpsi;
#endif
#ifdef HAVE_tstsi
  if (HAVE_tstsi)
    tst_optab->handlers[(int) SImode].insn_code = CODE_FOR_tstsi;
#endif
#ifdef HAVE_tstdi
  if (HAVE_tstdi)
    tst_optab->handlers[(int) DImode].insn_code = CODE_FOR_tstdi;
#endif
#ifdef HAVE_tstti
  if (HAVE_tstti)
    tst_optab->handlers[(int) TImode].insn_code = CODE_FOR_tstti;
#endif
#ifdef HAVE_tstsf
  if (HAVE_tstsf)
    tst_optab->handlers[(int) SFmode].insn_code = CODE_FOR_tstsf;
#endif
#ifdef HAVE_tstdf
  if (HAVE_tstdf)
    tst_optab->handlers[(int) DFmode].insn_code = CODE_FOR_tstdf;
#endif
#ifdef HAVE_tstxf
  if (HAVE_tstxf)
    tst_optab->handlers[(int) XFmode].insn_code = CODE_FOR_tstxf;
#endif
#ifdef HAVE_tsttf
  if (HAVE_tsttf)
    tst_optab->handlers[(int) TFmode].insn_code = CODE_FOR_tsttf;
#endif

#ifdef HAVE_beq
  if (HAVE_beq)
    bcc_gen_fctn[(int) EQ] = gen_beq;
#endif
#ifdef HAVE_bne
  if (HAVE_bne)
    bcc_gen_fctn[(int) NE] = gen_bne;
#endif
#ifdef HAVE_bgt
  if (HAVE_bgt)
    bcc_gen_fctn[(int) GT] = gen_bgt;
#endif
#ifdef HAVE_bge
  if (HAVE_bge)
    bcc_gen_fctn[(int) GE] = gen_bge;
#endif
#ifdef HAVE_bgtu
  if (HAVE_bgtu)
    bcc_gen_fctn[(int) GTU] = gen_bgtu;
#endif
#ifdef HAVE_bgeu
  if (HAVE_bgeu)
    bcc_gen_fctn[(int) GEU] = gen_bgeu;
#endif
#ifdef HAVE_blt
  if (HAVE_blt)
    bcc_gen_fctn[(int) LT] = gen_blt;
#endif
#ifdef HAVE_ble
  if (HAVE_ble)
    bcc_gen_fctn[(int) LE] = gen_ble;
#endif
#ifdef HAVE_bltu
  if (HAVE_bltu)
    bcc_gen_fctn[(int) LTU] = gen_bltu;
#endif
#ifdef HAVE_bleu
  if (HAVE_bleu)
    bcc_gen_fctn[(int) LEU] = gen_bleu;
#endif

  for (i = 0; i < NUM_RTX_CODE; i++)
    setcc_gen_code[i] = CODE_FOR_nothing;

#ifdef HAVE_seq
  if (HAVE_seq)
    setcc_gen_code[(int) EQ] = CODE_FOR_seq;
#endif
#ifdef HAVE_sne
  if (HAVE_sne)
    setcc_gen_code[(int) NE] = CODE_FOR_sne;
#endif
#ifdef HAVE_sgt
  if (HAVE_sgt)
    setcc_gen_code[(int) GT] = CODE_FOR_sgt;
#endif
#ifdef HAVE_sge
  if (HAVE_sge)
    setcc_gen_code[(int) GE] = CODE_FOR_sge;
#endif
#ifdef HAVE_sgtu
  if (HAVE_sgtu)
    setcc_gen_code[(int) GTU] = CODE_FOR_sgtu;
#endif
#ifdef HAVE_sgeu
  if (HAVE_sgeu)
    setcc_gen_code[(int) GEU] = CODE_FOR_sgeu;
#endif
#ifdef HAVE_slt
  if (HAVE_slt)
    setcc_gen_code[(int) LT] = CODE_FOR_slt;
#endif
#ifdef HAVE_sle
  if (HAVE_sle)
    setcc_gen_code[(int) LE] = CODE_FOR_sle;
#endif
#ifdef HAVE_sltu
  if (HAVE_sltu)
    setcc_gen_code[(int) LTU] = CODE_FOR_sltu;
#endif
#ifdef HAVE_sleu
  if (HAVE_sleu)
    setcc_gen_code[(int) LEU] = CODE_FOR_sleu;
#endif

  extendsfdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__extendsfdf2");
  extendsfxf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__extendsfxf2");
  extendsftf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__extendsftf2");
  extenddfxf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__extenddfxf2");
  extenddftf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__extenddftf2");

  truncdfsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__truncdfsf2");
  truncxfsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__truncxfsf2");
  trunctfsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__trunctfsf2");
  truncxfdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__truncxfdf2");
  trunctfdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__trunctfdf2");

  memcpy_libfunc = gen_rtx (SYMBOL_REF, Pmode, "memcpy");
  bcopy_libfunc = gen_rtx (SYMBOL_REF, Pmode, "bcopy");
  memcmp_libfunc = gen_rtx (SYMBOL_REF, Pmode, "memcmp");
  bcmp_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__gcc_bcmp");
  memset_libfunc = gen_rtx (SYMBOL_REF, Pmode, "memset");
  bzero_libfunc = gen_rtx (SYMBOL_REF, Pmode, "bzero");

  eqsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__eqsf2");
  nesf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__nesf2");
  gtsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__gtsf2");
  gesf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__gesf2");
  ltsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__ltsf2");
  lesf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__lesf2");

  eqdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__eqdf2");
  nedf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__nedf2");
  gtdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__gtdf2");
  gedf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__gedf2");
  ltdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__ltdf2");
  ledf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__ledf2");

  eqxf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__eqxf2");
  nexf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__nexf2");
  gtxf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__gtxf2");
  gexf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__gexf2");
  ltxf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__ltxf2");
  lexf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__lexf2");

  eqtf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__eqtf2");
  netf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__netf2");
  gttf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__gttf2");
  getf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__getf2");
  lttf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__lttf2");
  letf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__letf2");

  floatsisf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floatsisf");
  floatdisf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floatdisf");
  floattisf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floattisf");

  floatsidf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floatsidf");
  floatdidf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floatdidf");
  floattidf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floattidf");

  floatsixf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floatsixf");
  floatdixf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floatdixf");
  floattixf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floattixf");

  floatsitf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floatsitf");
  floatditf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floatditf");
  floattitf_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__floattitf");

  fixsfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixsfsi");
  fixsfdi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixsfdi");
  fixsfti_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixsfti");

  fixdfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixdfsi");
  fixdfdi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixdfdi");
  fixdfti_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixdfti");

  fixxfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixxfsi");
  fixxfdi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixxfdi");
  fixxfti_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixxfti");

  fixtfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixtfsi");
  fixtfdi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixtfdi");
  fixtfti_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixtfti");

  fixunssfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunssfsi");
  fixunssfdi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunssfdi");
  fixunssfti_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunssfti");

  fixunsdfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunsdfsi");
  fixunsdfdi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunsdfdi");
  fixunsdfti_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunsdfti");

  fixunsxfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunsxfsi");
  fixunsxfdi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunsxfdi");
  fixunsxfti_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunsxfti");

  fixunstfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunstfsi");
  fixunstfdi_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunstfdi");
  fixunstfti_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__fixunstfti");
}

#ifdef BROKEN_LDEXP

/* SCO 3.2 apparently has a broken ldexp. */

double
ldexp(x,n)
     double x;
     int n;
{
  if (n > 0)
    while (n--)
      x *= 2;

  return x;
}
#endif /* BROKEN_LDEXP */
