/* Expand the basic unary and binary arithmetic operations, for GNU compiler.
   Copyright (C) 1987-2017 Free Software Foundation, Inc.

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
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "predict.h"
#include "tm_p.h"
#include "expmed.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"

/* Include insn-config.h before expr.h so that HAVE_conditional_move
   is properly defined.  */
#include "stor-layout.h"
#include "except.h"
#include "dojump.h"
#include "explow.h"
#include "expr.h"
#include "optabs-tree.h"
#include "libfuncs.h"

static void prepare_float_lib_cmp (rtx, rtx, enum rtx_code, rtx *,
				   machine_mode *);
static rtx expand_unop_direct (machine_mode, optab, rtx, rtx, int);
static void emit_libcall_block_1 (rtx_insn *, rtx, rtx, rtx, bool);

/* Debug facility for use in GDB.  */
void debug_optab_libfuncs (void);

/* Add a REG_EQUAL note to the last insn in INSNS.  TARGET is being set to
   the result of operation CODE applied to OP0 (and OP1 if it is a binary
   operation).

   If the last insn does not set TARGET, don't do anything, but return 1.

   If the last insn or a previous insn sets TARGET and TARGET is one of OP0
   or OP1, don't add the REG_EQUAL note but return 0.  Our caller can then
   try again, ensuring that TARGET is not one of the operands.  */

static int
add_equal_note (rtx_insn *insns, rtx target, enum rtx_code code, rtx op0, rtx op1)
{
  rtx_insn *last_insn;
  rtx set;
  rtx note;

  gcc_assert (insns && INSN_P (insns) && NEXT_INSN (insns));

  if (GET_RTX_CLASS (code) != RTX_COMM_ARITH
      && GET_RTX_CLASS (code) != RTX_BIN_ARITH
      && GET_RTX_CLASS (code) != RTX_COMM_COMPARE
      && GET_RTX_CLASS (code) != RTX_COMPARE
      && GET_RTX_CLASS (code) != RTX_UNARY)
    return 1;

  if (GET_CODE (target) == ZERO_EXTRACT)
    return 1;

  for (last_insn = insns;
       NEXT_INSN (last_insn) != NULL_RTX;
       last_insn = NEXT_INSN (last_insn))
    ;

  /* If TARGET is in OP0 or OP1, punt.  We'd end up with a note referencing
     a value changing in the insn, so the note would be invalid for CSE.  */
  if (reg_overlap_mentioned_p (target, op0)
      || (op1 && reg_overlap_mentioned_p (target, op1)))
    {
      if (MEM_P (target)
	  && (rtx_equal_p (target, op0)
	      || (op1 && rtx_equal_p (target, op1))))
	{
	  /* For MEM target, with MEM = MEM op X, prefer no REG_EQUAL note
	     over expanding it as temp = MEM op X, MEM = temp.  If the target
	     supports MEM = MEM op X instructions, it is sometimes too hard
	     to reconstruct that form later, especially if X is also a memory,
	     and due to multiple occurrences of addresses the address might
	     be forced into register unnecessarily.
	     Note that not emitting the REG_EQUIV note might inhibit
	     CSE in some cases.  */
	  set = single_set (last_insn);
	  if (set
	      && GET_CODE (SET_SRC (set)) == code
	      && MEM_P (SET_DEST (set))
	      && (rtx_equal_p (SET_DEST (set), XEXP (SET_SRC (set), 0))
		  || (op1 && rtx_equal_p (SET_DEST (set),
					  XEXP (SET_SRC (set), 1)))))
	    return 1;
	}
      return 0;
    }

  set = set_for_reg_notes (last_insn);
  if (set == NULL_RTX)
    return 1;

  if (! rtx_equal_p (SET_DEST (set), target)
      /* For a STRICT_LOW_PART, the REG_NOTE applies to what is inside it.  */
      && (GET_CODE (SET_DEST (set)) != STRICT_LOW_PART
	  || ! rtx_equal_p (XEXP (SET_DEST (set), 0), target)))
    return 1;

  if (GET_RTX_CLASS (code) == RTX_UNARY)
    switch (code)
      {
      case FFS:
      case CLZ:
      case CTZ:
      case CLRSB:
      case POPCOUNT:
      case PARITY:
      case BSWAP:
	if (GET_MODE (op0) != VOIDmode && GET_MODE (target) != GET_MODE (op0))
	  {
	    note = gen_rtx_fmt_e (code, GET_MODE (op0), copy_rtx (op0));
	    if (GET_MODE_SIZE (GET_MODE (op0))
		> GET_MODE_SIZE (GET_MODE (target)))
	      note = simplify_gen_unary (TRUNCATE, GET_MODE (target),
					 note, GET_MODE (op0));
	    else
	      note = simplify_gen_unary (ZERO_EXTEND, GET_MODE (target),
					 note, GET_MODE (op0));
	    break;
	  }
	/* FALLTHRU */
      default:
	note = gen_rtx_fmt_e (code, GET_MODE (target), copy_rtx (op0));
	break;
      }
  else
    note = gen_rtx_fmt_ee (code, GET_MODE (target), copy_rtx (op0), copy_rtx (op1));

  set_unique_reg_note (last_insn, REG_EQUAL, note);

  return 1;
}

/* Given two input operands, OP0 and OP1, determine what the correct from_mode
   for a widening operation would be.  In most cases this would be OP0, but if
   that's a constant it'll be VOIDmode, which isn't useful.  */

static machine_mode
widened_mode (machine_mode to_mode, rtx op0, rtx op1)
{
  machine_mode m0 = GET_MODE (op0);
  machine_mode m1 = GET_MODE (op1);
  machine_mode result;

  if (m0 == VOIDmode && m1 == VOIDmode)
    return to_mode;
  else if (m0 == VOIDmode || GET_MODE_SIZE (m0) < GET_MODE_SIZE (m1))
    result = m1;
  else
    result = m0;

  if (GET_MODE_SIZE (result) > GET_MODE_SIZE (to_mode))
    return to_mode;

  return result;
}

/* Widen OP to MODE and return the rtx for the widened operand.  UNSIGNEDP
   says whether OP is signed or unsigned.  NO_EXTEND is nonzero if we need
   not actually do a sign-extend or zero-extend, but can leave the
   higher-order bits of the result rtx undefined, for example, in the case
   of logical operations, but not right shifts.  */

static rtx
widen_operand (rtx op, machine_mode mode, machine_mode oldmode,
	       int unsignedp, int no_extend)
{
  rtx result;
  scalar_int_mode int_mode;

  /* If we don't have to extend and this is a constant, return it.  */
  if (no_extend && GET_MODE (op) == VOIDmode)
    return op;

  /* If we must extend do so.  If OP is a SUBREG for a promoted object, also
     extend since it will be more efficient to do so unless the signedness of
     a promoted object differs from our extension.  */
  if (! no_extend
      || !is_a <scalar_int_mode> (mode, &int_mode)
      || (GET_CODE (op) == SUBREG && SUBREG_PROMOTED_VAR_P (op)
	  && SUBREG_CHECK_PROMOTED_SIGN (op, unsignedp)))
    return convert_modes (mode, oldmode, op, unsignedp);

  /* If MODE is no wider than a single word, we return a lowpart or paradoxical
     SUBREG.  */
  if (GET_MODE_SIZE (int_mode) <= UNITS_PER_WORD)
    return gen_lowpart (int_mode, force_reg (GET_MODE (op), op));

  /* Otherwise, get an object of MODE, clobber it, and set the low-order
     part to OP.  */

  result = gen_reg_rtx (int_mode);
  emit_clobber (result);
  emit_move_insn (gen_lowpart (GET_MODE (op), result), op);
  return result;
}

/* Expand vector widening operations.

   There are two different classes of operations handled here:
   1) Operations whose result is wider than all the arguments to the operation.
      Examples: VEC_UNPACK_HI/LO_EXPR, VEC_WIDEN_MULT_HI/LO_EXPR
      In this case OP0 and optionally OP1 would be initialized,
      but WIDE_OP wouldn't (not relevant for this case).
   2) Operations whose result is of the same size as the last argument to the
      operation, but wider than all the other arguments to the operation.
      Examples: WIDEN_SUM_EXPR, VEC_DOT_PROD_EXPR.
      In the case WIDE_OP, OP0 and optionally OP1 would be initialized.

   E.g, when called to expand the following operations, this is how
   the arguments will be initialized:
                                nops    OP0     OP1     WIDE_OP
   widening-sum                 2       oprnd0  -       oprnd1
   widening-dot-product         3       oprnd0  oprnd1  oprnd2
   widening-mult                2       oprnd0  oprnd1  -
   type-promotion (vec-unpack)  1       oprnd0  -       -  */

rtx
expand_widen_pattern_expr (sepops ops, rtx op0, rtx op1, rtx wide_op,
			   rtx target, int unsignedp)
{
  struct expand_operand eops[4];
  tree oprnd0, oprnd1, oprnd2;
  machine_mode wmode = VOIDmode, tmode0, tmode1 = VOIDmode;
  optab widen_pattern_optab;
  enum insn_code icode;
  int nops = TREE_CODE_LENGTH (ops->code);
  int op;

  oprnd0 = ops->op0;
  tmode0 = TYPE_MODE (TREE_TYPE (oprnd0));
  widen_pattern_optab =
    optab_for_tree_code (ops->code, TREE_TYPE (oprnd0), optab_default);
  if (ops->code == WIDEN_MULT_PLUS_EXPR
      || ops->code == WIDEN_MULT_MINUS_EXPR)
    icode = find_widening_optab_handler (widen_pattern_optab,
					 TYPE_MODE (TREE_TYPE (ops->op2)),
					 tmode0, 0);
  else
    icode = optab_handler (widen_pattern_optab, tmode0);
  gcc_assert (icode != CODE_FOR_nothing);

  if (nops >= 2)
    {
      oprnd1 = ops->op1;
      tmode1 = TYPE_MODE (TREE_TYPE (oprnd1));
    }

  /* The last operand is of a wider mode than the rest of the operands.  */
  if (nops == 2)
    wmode = tmode1;
  else if (nops == 3)
    {
      gcc_assert (tmode1 == tmode0);
      gcc_assert (op1);
      oprnd2 = ops->op2;
      wmode = TYPE_MODE (TREE_TYPE (oprnd2));
    }

  op = 0;
  create_output_operand (&eops[op++], target, TYPE_MODE (ops->type));
  create_convert_operand_from (&eops[op++], op0, tmode0, unsignedp);
  if (op1)
    create_convert_operand_from (&eops[op++], op1, tmode1, unsignedp);
  if (wide_op)
    create_convert_operand_from (&eops[op++], wide_op, wmode, unsignedp);
  expand_insn (icode, op, eops);
  return eops[0].value;
}

/* Generate code to perform an operation specified by TERNARY_OPTAB
   on operands OP0, OP1 and OP2, with result having machine-mode MODE.

   UNSIGNEDP is for the case where we have to widen the operands
   to perform the operation.  It says to use zero-extension.

   If TARGET is nonzero, the value
   is generated there, if it is convenient to do so.
   In all cases an rtx is returned for the locus of the value;
   this may or may not be TARGET.  */

rtx
expand_ternary_op (machine_mode mode, optab ternary_optab, rtx op0,
		   rtx op1, rtx op2, rtx target, int unsignedp)
{
  struct expand_operand ops[4];
  enum insn_code icode = optab_handler (ternary_optab, mode);

  gcc_assert (optab_handler (ternary_optab, mode) != CODE_FOR_nothing);

  create_output_operand (&ops[0], target, mode);
  create_convert_operand_from (&ops[1], op0, mode, unsignedp);
  create_convert_operand_from (&ops[2], op1, mode, unsignedp);
  create_convert_operand_from (&ops[3], op2, mode, unsignedp);
  expand_insn (icode, 4, ops);
  return ops[0].value;
}


/* Like expand_binop, but return a constant rtx if the result can be
   calculated at compile time.  The arguments and return value are
   otherwise the same as for expand_binop.  */

rtx
simplify_expand_binop (machine_mode mode, optab binoptab,
		       rtx op0, rtx op1, rtx target, int unsignedp,
		       enum optab_methods methods)
{
  if (CONSTANT_P (op0) && CONSTANT_P (op1))
    {
      rtx x = simplify_binary_operation (optab_to_code (binoptab),
					 mode, op0, op1);
      if (x)
	return x;
    }

  return expand_binop (mode, binoptab, op0, op1, target, unsignedp, methods);
}

/* Like simplify_expand_binop, but always put the result in TARGET.
   Return true if the expansion succeeded.  */

bool
force_expand_binop (machine_mode mode, optab binoptab,
		    rtx op0, rtx op1, rtx target, int unsignedp,
		    enum optab_methods methods)
{
  rtx x = simplify_expand_binop (mode, binoptab, op0, op1,
				 target, unsignedp, methods);
  if (x == 0)
    return false;
  if (x != target)
    emit_move_insn (target, x);
  return true;
}

/* Create a new vector value in VMODE with all elements set to OP.  The
   mode of OP must be the element mode of VMODE.  If OP is a constant,
   then the return value will be a constant.  */

static rtx
expand_vector_broadcast (machine_mode vmode, rtx op)
{
  enum insn_code icode;
  rtvec vec;
  rtx ret;
  int i, n;

  gcc_checking_assert (VECTOR_MODE_P (vmode));

  n = GET_MODE_NUNITS (vmode);
  vec = rtvec_alloc (n);
  for (i = 0; i < n; ++i)
    RTVEC_ELT (vec, i) = op;

  if (CONSTANT_P (op))
    return gen_rtx_CONST_VECTOR (vmode, vec);

  /* ??? If the target doesn't have a vec_init, then we have no easy way
     of performing this operation.  Most of this sort of generic support
     is hidden away in the vector lowering support in gimple.  */
  icode = convert_optab_handler (vec_init_optab, vmode,
				 GET_MODE_INNER (vmode));
  if (icode == CODE_FOR_nothing)
    return NULL;

  ret = gen_reg_rtx (vmode);
  emit_insn (GEN_FCN (icode) (ret, gen_rtx_PARALLEL (vmode, vec)));

  return ret;
}

/* This subroutine of expand_doubleword_shift handles the cases in which
   the effective shift value is >= BITS_PER_WORD.  The arguments and return
   value are the same as for the parent routine, except that SUPERWORD_OP1
   is the shift count to use when shifting OUTOF_INPUT into INTO_TARGET.
   INTO_TARGET may be null if the caller has decided to calculate it.  */

static bool
expand_superword_shift (optab binoptab, rtx outof_input, rtx superword_op1,
			rtx outof_target, rtx into_target,
			int unsignedp, enum optab_methods methods)
{
  if (into_target != 0)
    if (!force_expand_binop (word_mode, binoptab, outof_input, superword_op1,
			     into_target, unsignedp, methods))
      return false;

  if (outof_target != 0)
    {
      /* For a signed right shift, we must fill OUTOF_TARGET with copies
	 of the sign bit, otherwise we must fill it with zeros.  */
      if (binoptab != ashr_optab)
	emit_move_insn (outof_target, CONST0_RTX (word_mode));
      else
	if (!force_expand_binop (word_mode, binoptab,
				 outof_input, GEN_INT (BITS_PER_WORD - 1),
				 outof_target, unsignedp, methods))
	  return false;
    }
  return true;
}

/* This subroutine of expand_doubleword_shift handles the cases in which
   the effective shift value is < BITS_PER_WORD.  The arguments and return
   value are the same as for the parent routine.  */

static bool
expand_subword_shift (scalar_int_mode op1_mode, optab binoptab,
		      rtx outof_input, rtx into_input, rtx op1,
		      rtx outof_target, rtx into_target,
		      int unsignedp, enum optab_methods methods,
		      unsigned HOST_WIDE_INT shift_mask)
{
  optab reverse_unsigned_shift, unsigned_shift;
  rtx tmp, carries;

  reverse_unsigned_shift = (binoptab == ashl_optab ? lshr_optab : ashl_optab);
  unsigned_shift = (binoptab == ashl_optab ? ashl_optab : lshr_optab);

  /* The low OP1 bits of INTO_TARGET come from the high bits of OUTOF_INPUT.
     We therefore need to shift OUTOF_INPUT by (BITS_PER_WORD - OP1) bits in
     the opposite direction to BINOPTAB.  */
  if (CONSTANT_P (op1) || shift_mask >= BITS_PER_WORD)
    {
      carries = outof_input;
      tmp = immed_wide_int_const (wi::shwi (BITS_PER_WORD,
					    op1_mode), op1_mode);
      tmp = simplify_expand_binop (op1_mode, sub_optab, tmp, op1,
				   0, true, methods);
    }
  else
    {
      /* We must avoid shifting by BITS_PER_WORD bits since that is either
	 the same as a zero shift (if shift_mask == BITS_PER_WORD - 1) or
	 has unknown behavior.  Do a single shift first, then shift by the
	 remainder.  It's OK to use ~OP1 as the remainder if shift counts
	 are truncated to the mode size.  */
      carries = expand_binop (word_mode, reverse_unsigned_shift,
			      outof_input, const1_rtx, 0, unsignedp, methods);
      if (shift_mask == BITS_PER_WORD - 1)
	{
	  tmp = immed_wide_int_const
	    (wi::minus_one (GET_MODE_PRECISION (op1_mode)), op1_mode);
	  tmp = simplify_expand_binop (op1_mode, xor_optab, op1, tmp,
				       0, true, methods);
	}
      else
	{
	  tmp = immed_wide_int_const (wi::shwi (BITS_PER_WORD - 1,
						op1_mode), op1_mode);
	  tmp = simplify_expand_binop (op1_mode, sub_optab, tmp, op1,
				       0, true, methods);
	}
    }
  if (tmp == 0 || carries == 0)
    return false;
  carries = expand_binop (word_mode, reverse_unsigned_shift,
			  carries, tmp, 0, unsignedp, methods);
  if (carries == 0)
    return false;

  /* Shift INTO_INPUT logically by OP1.  This is the last use of INTO_INPUT
     so the result can go directly into INTO_TARGET if convenient.  */
  tmp = expand_binop (word_mode, unsigned_shift, into_input, op1,
		      into_target, unsignedp, methods);
  if (tmp == 0)
    return false;

  /* Now OR in the bits carried over from OUTOF_INPUT.  */
  if (!force_expand_binop (word_mode, ior_optab, tmp, carries,
			   into_target, unsignedp, methods))
    return false;

  /* Use a standard word_mode shift for the out-of half.  */
  if (outof_target != 0)
    if (!force_expand_binop (word_mode, binoptab, outof_input, op1,
			     outof_target, unsignedp, methods))
      return false;

  return true;
}


/* Try implementing expand_doubleword_shift using conditional moves.
   The shift is by < BITS_PER_WORD if (CMP_CODE CMP1 CMP2) is true,
   otherwise it is by >= BITS_PER_WORD.  SUBWORD_OP1 and SUPERWORD_OP1
   are the shift counts to use in the former and latter case.  All other
   arguments are the same as the parent routine.  */

static bool
expand_doubleword_shift_condmove (scalar_int_mode op1_mode, optab binoptab,
				  enum rtx_code cmp_code, rtx cmp1, rtx cmp2,
				  rtx outof_input, rtx into_input,
				  rtx subword_op1, rtx superword_op1,
				  rtx outof_target, rtx into_target,
				  int unsignedp, enum optab_methods methods,
				  unsigned HOST_WIDE_INT shift_mask)
{
  rtx outof_superword, into_superword;

  /* Put the superword version of the output into OUTOF_SUPERWORD and
     INTO_SUPERWORD.  */
  outof_superword = outof_target != 0 ? gen_reg_rtx (word_mode) : 0;
  if (outof_target != 0 && subword_op1 == superword_op1)
    {
      /* The value INTO_TARGET >> SUBWORD_OP1, which we later store in
	 OUTOF_TARGET, is the same as the value of INTO_SUPERWORD.  */
      into_superword = outof_target;
      if (!expand_superword_shift (binoptab, outof_input, superword_op1,
				   outof_superword, 0, unsignedp, methods))
	return false;
    }
  else
    {
      into_superword = gen_reg_rtx (word_mode);
      if (!expand_superword_shift (binoptab, outof_input, superword_op1,
				   outof_superword, into_superword,
				   unsignedp, methods))
	return false;
    }

  /* Put the subword version directly in OUTOF_TARGET and INTO_TARGET.  */
  if (!expand_subword_shift (op1_mode, binoptab,
			     outof_input, into_input, subword_op1,
			     outof_target, into_target,
			     unsignedp, methods, shift_mask))
    return false;

  /* Select between them.  Do the INTO half first because INTO_SUPERWORD
     might be the current value of OUTOF_TARGET.  */
  if (!emit_conditional_move (into_target, cmp_code, cmp1, cmp2, op1_mode,
			      into_target, into_superword, word_mode, false))
    return false;

  if (outof_target != 0)
    if (!emit_conditional_move (outof_target, cmp_code, cmp1, cmp2, op1_mode,
				outof_target, outof_superword,
				word_mode, false))
      return false;

  return true;
}

/* Expand a doubleword shift (ashl, ashr or lshr) using word-mode shifts.
   OUTOF_INPUT and INTO_INPUT are the two word-sized halves of the first
   input operand; the shift moves bits in the direction OUTOF_INPUT->
   INTO_TARGET.  OUTOF_TARGET and INTO_TARGET are the equivalent words
   of the target.  OP1 is the shift count and OP1_MODE is its mode.
   If OP1 is constant, it will have been truncated as appropriate
   and is known to be nonzero.

   If SHIFT_MASK is zero, the result of word shifts is undefined when the
   shift count is outside the range [0, BITS_PER_WORD).  This routine must
   avoid generating such shifts for OP1s in the range [0, BITS_PER_WORD * 2).

   If SHIFT_MASK is nonzero, all word-mode shift counts are effectively
   masked by it and shifts in the range [BITS_PER_WORD, SHIFT_MASK) will
   fill with zeros or sign bits as appropriate.

   If SHIFT_MASK is BITS_PER_WORD - 1, this routine will synthesize
   a doubleword shift whose equivalent mask is BITS_PER_WORD * 2 - 1.
   Doing this preserves semantics required by SHIFT_COUNT_TRUNCATED.
   In all other cases, shifts by values outside [0, BITS_PER_UNIT * 2)
   are undefined.

   BINOPTAB, UNSIGNEDP and METHODS are as for expand_binop.  This function
   may not use INTO_INPUT after modifying INTO_TARGET, and similarly for
   OUTOF_INPUT and OUTOF_TARGET.  OUTOF_TARGET can be null if the parent
   function wants to calculate it itself.

   Return true if the shift could be successfully synthesized.  */

static bool
expand_doubleword_shift (scalar_int_mode op1_mode, optab binoptab,
			 rtx outof_input, rtx into_input, rtx op1,
			 rtx outof_target, rtx into_target,
			 int unsignedp, enum optab_methods methods,
			 unsigned HOST_WIDE_INT shift_mask)
{
  rtx superword_op1, tmp, cmp1, cmp2;
  enum rtx_code cmp_code;

  /* See if word-mode shifts by BITS_PER_WORD...BITS_PER_WORD * 2 - 1 will
     fill the result with sign or zero bits as appropriate.  If so, the value
     of OUTOF_TARGET will always be (SHIFT OUTOF_INPUT OP1).   Recursively call
     this routine to calculate INTO_TARGET (which depends on both OUTOF_INPUT
     and INTO_INPUT), then emit code to set up OUTOF_TARGET.

     This isn't worthwhile for constant shifts since the optimizers will
     cope better with in-range shift counts.  */
  if (shift_mask >= BITS_PER_WORD
      && outof_target != 0
      && !CONSTANT_P (op1))
    {
      if (!expand_doubleword_shift (op1_mode, binoptab,
				    outof_input, into_input, op1,
				    0, into_target,
				    unsignedp, methods, shift_mask))
	return false;
      if (!force_expand_binop (word_mode, binoptab, outof_input, op1,
			       outof_target, unsignedp, methods))
	return false;
      return true;
    }

  /* Set CMP_CODE, CMP1 and CMP2 so that the rtx (CMP_CODE CMP1 CMP2)
     is true when the effective shift value is less than BITS_PER_WORD.
     Set SUPERWORD_OP1 to the shift count that should be used to shift
     OUTOF_INPUT into INTO_TARGET when the condition is false.  */
  tmp = immed_wide_int_const (wi::shwi (BITS_PER_WORD, op1_mode), op1_mode);
  if (!CONSTANT_P (op1) && shift_mask == BITS_PER_WORD - 1)
    {
      /* Set CMP1 to OP1 & BITS_PER_WORD.  The result is zero iff OP1
	 is a subword shift count.  */
      cmp1 = simplify_expand_binop (op1_mode, and_optab, op1, tmp,
				    0, true, methods);
      cmp2 = CONST0_RTX (op1_mode);
      cmp_code = EQ;
      superword_op1 = op1;
    }
  else
    {
      /* Set CMP1 to OP1 - BITS_PER_WORD.  */
      cmp1 = simplify_expand_binop (op1_mode, sub_optab, op1, tmp,
				    0, true, methods);
      cmp2 = CONST0_RTX (op1_mode);
      cmp_code = LT;
      superword_op1 = cmp1;
    }
  if (cmp1 == 0)
    return false;

  /* If we can compute the condition at compile time, pick the
     appropriate subroutine.  */
  tmp = simplify_relational_operation (cmp_code, SImode, op1_mode, cmp1, cmp2);
  if (tmp != 0 && CONST_INT_P (tmp))
    {
      if (tmp == const0_rtx)
	return expand_superword_shift (binoptab, outof_input, superword_op1,
				       outof_target, into_target,
				       unsignedp, methods);
      else
	return expand_subword_shift (op1_mode, binoptab,
				     outof_input, into_input, op1,
				     outof_target, into_target,
				     unsignedp, methods, shift_mask);
    }

  /* Try using conditional moves to generate straight-line code.  */
  if (HAVE_conditional_move)
    {
      rtx_insn *start = get_last_insn ();
      if (expand_doubleword_shift_condmove (op1_mode, binoptab,
					    cmp_code, cmp1, cmp2,
					    outof_input, into_input,
					    op1, superword_op1,
					    outof_target, into_target,
					    unsignedp, methods, shift_mask))
	return true;
      delete_insns_since (start);
    }

  /* As a last resort, use branches to select the correct alternative.  */
  rtx_code_label *subword_label = gen_label_rtx ();
  rtx_code_label *done_label = gen_label_rtx ();

  NO_DEFER_POP;
  do_compare_rtx_and_jump (cmp1, cmp2, cmp_code, false, op1_mode,
			   0, 0, subword_label,
			   profile_probability::uninitialized ());
  OK_DEFER_POP;

  if (!expand_superword_shift (binoptab, outof_input, superword_op1,
			       outof_target, into_target,
			       unsignedp, methods))
    return false;

  emit_jump_insn (targetm.gen_jump (done_label));
  emit_barrier ();
  emit_label (subword_label);

  if (!expand_subword_shift (op1_mode, binoptab,
			     outof_input, into_input, op1,
			     outof_target, into_target,
			     unsignedp, methods, shift_mask))
    return false;

  emit_label (done_label);
  return true;
}

/* Subroutine of expand_binop.  Perform a double word multiplication of
   operands OP0 and OP1 both of mode MODE, which is exactly twice as wide
   as the target's word_mode.  This function return NULL_RTX if anything
   goes wrong, in which case it may have already emitted instructions
   which need to be deleted.

   If we want to multiply two two-word values and have normal and widening
   multiplies of single-word values, we can do this with three smaller
   multiplications.

   The multiplication proceeds as follows:
			         _______________________
			        [__op0_high_|__op0_low__]
			         _______________________
        *			[__op1_high_|__op1_low__]
        _______________________________________________
			         _______________________
    (1)				[__op0_low__*__op1_low__]
		     _______________________
    (2a)	    [__op0_low__*__op1_high_]
		     _______________________
    (2b)	    [__op0_high_*__op1_low__]
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

static rtx
expand_doubleword_mult (machine_mode mode, rtx op0, rtx op1, rtx target,
		       bool umulp, enum optab_methods methods)
{
  int low = (WORDS_BIG_ENDIAN ? 1 : 0);
  int high = (WORDS_BIG_ENDIAN ? 0 : 1);
  rtx wordm1 = umulp ? NULL_RTX : GEN_INT (BITS_PER_WORD - 1);
  rtx product, adjust, product_high, temp;

  rtx op0_high = operand_subword_force (op0, high, mode);
  rtx op0_low = operand_subword_force (op0, low, mode);
  rtx op1_high = operand_subword_force (op1, high, mode);
  rtx op1_low = operand_subword_force (op1, low, mode);

  /* If we're using an unsigned multiply to directly compute the product
     of the low-order words of the operands and perform any required
     adjustments of the operands, we begin by trying two more multiplications
     and then computing the appropriate sum.

     We have checked above that the required addition is provided.
     Full-word addition will normally always succeed, especially if
     it is provided at all, so we don't worry about its failure.  The
     multiplication may well fail, however, so we do handle that.  */

  if (!umulp)
    {
      /* ??? This could be done with emit_store_flag where available.  */
      temp = expand_binop (word_mode, lshr_optab, op0_low, wordm1,
			   NULL_RTX, 1, methods);
      if (temp)
	op0_high = expand_binop (word_mode, add_optab, op0_high, temp,
				 NULL_RTX, 0, OPTAB_DIRECT);
      else
	{
	  temp = expand_binop (word_mode, ashr_optab, op0_low, wordm1,
			       NULL_RTX, 0, methods);
	  if (!temp)
	    return NULL_RTX;
	  op0_high = expand_binop (word_mode, sub_optab, op0_high, temp,
				   NULL_RTX, 0, OPTAB_DIRECT);
	}

      if (!op0_high)
	return NULL_RTX;
    }

  adjust = expand_binop (word_mode, smul_optab, op0_high, op1_low,
			 NULL_RTX, 0, OPTAB_DIRECT);
  if (!adjust)
    return NULL_RTX;

  /* OP0_HIGH should now be dead.  */

  if (!umulp)
    {
      /* ??? This could be done with emit_store_flag where available.  */
      temp = expand_binop (word_mode, lshr_optab, op1_low, wordm1,
			   NULL_RTX, 1, methods);
      if (temp)
	op1_high = expand_binop (word_mode, add_optab, op1_high, temp,
				 NULL_RTX, 0, OPTAB_DIRECT);
      else
	{
	  temp = expand_binop (word_mode, ashr_optab, op1_low, wordm1,
			       NULL_RTX, 0, methods);
	  if (!temp)
	    return NULL_RTX;
	  op1_high = expand_binop (word_mode, sub_optab, op1_high, temp,
				   NULL_RTX, 0, OPTAB_DIRECT);
	}

      if (!op1_high)
	return NULL_RTX;
    }

  temp = expand_binop (word_mode, smul_optab, op1_high, op0_low,
		       NULL_RTX, 0, OPTAB_DIRECT);
  if (!temp)
    return NULL_RTX;

  /* OP1_HIGH should now be dead.  */

  adjust = expand_binop (word_mode, add_optab, adjust, temp,
			 NULL_RTX, 0, OPTAB_DIRECT);

  if (target && !REG_P (target))
    target = NULL_RTX;

  if (umulp)
    product = expand_binop (mode, umul_widen_optab, op0_low, op1_low,
			    target, 1, OPTAB_DIRECT);
  else
    product = expand_binop (mode, smul_widen_optab, op0_low, op1_low,
			    target, 1, OPTAB_DIRECT);

  if (!product)
    return NULL_RTX;

  product_high = operand_subword (product, high, 1, mode);
  adjust = expand_binop (word_mode, add_optab, product_high, adjust,
			 NULL_RTX, 0, OPTAB_DIRECT);
  emit_move_insn (product_high, adjust);
  return product;
}

/* Wrapper around expand_binop which takes an rtx code to specify
   the operation to perform, not an optab pointer.  All other
   arguments are the same.  */
rtx
expand_simple_binop (machine_mode mode, enum rtx_code code, rtx op0,
		     rtx op1, rtx target, int unsignedp,
		     enum optab_methods methods)
{
  optab binop = code_to_optab (code);
  gcc_assert (binop);

  return expand_binop (mode, binop, op0, op1, target, unsignedp, methods);
}

/* Return whether OP0 and OP1 should be swapped when expanding a commutative
   binop.  Order them according to commutative_operand_precedence and, if
   possible, try to put TARGET or a pseudo first.  */
static bool
swap_commutative_operands_with_target (rtx target, rtx op0, rtx op1)
{
  int op0_prec = commutative_operand_precedence (op0);
  int op1_prec = commutative_operand_precedence (op1);

  if (op0_prec < op1_prec)
    return true;

  if (op0_prec > op1_prec)
    return false;

  /* With equal precedence, both orders are ok, but it is better if the
     first operand is TARGET, or if both TARGET and OP0 are pseudos.  */
  if (target == 0 || REG_P (target))
    return (REG_P (op1) && !REG_P (op0)) || target == op1;
  else
    return rtx_equal_p (op1, target);
}

/* Return true if BINOPTAB implements a shift operation.  */

static bool
shift_optab_p (optab binoptab)
{
  switch (optab_to_code (binoptab))
    {
    case ASHIFT:
    case SS_ASHIFT:
    case US_ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATE:
    case ROTATERT:
      return true;

    default:
      return false;
    }
}

/* Return true if BINOPTAB implements a commutative binary operation.  */

static bool
commutative_optab_p (optab binoptab)
{
  return (GET_RTX_CLASS (optab_to_code (binoptab)) == RTX_COMM_ARITH
	  || binoptab == smul_widen_optab
	  || binoptab == umul_widen_optab
	  || binoptab == smul_highpart_optab
	  || binoptab == umul_highpart_optab);
}

/* X is to be used in mode MODE as operand OPN to BINOPTAB.  If we're
   optimizing, and if the operand is a constant that costs more than
   1 instruction, force the constant into a register and return that
   register.  Return X otherwise.  UNSIGNEDP says whether X is unsigned.  */

static rtx
avoid_expensive_constant (machine_mode mode, optab binoptab,
			  int opn, rtx x, bool unsignedp)
{
  bool speed = optimize_insn_for_speed_p ();

  if (mode != VOIDmode
      && optimize
      && CONSTANT_P (x)
      && (rtx_cost (x, mode, optab_to_code (binoptab), opn, speed)
	  > set_src_cost (x, mode, speed)))
    {
      if (CONST_INT_P (x))
	{
	  HOST_WIDE_INT intval = trunc_int_for_mode (INTVAL (x), mode);
	  if (intval != INTVAL (x))
	    x = GEN_INT (intval);
	}
      else
	x = convert_modes (mode, VOIDmode, x, unsignedp);
      x = force_reg (mode, x);
    }
  return x;
}

/* Helper function for expand_binop: handle the case where there
   is an insn that directly implements the indicated operation.
   Returns null if this is not possible.  */
static rtx
expand_binop_directly (machine_mode mode, optab binoptab,
		       rtx op0, rtx op1,
		       rtx target, int unsignedp, enum optab_methods methods,
		       rtx_insn *last)
{
  machine_mode from_mode = widened_mode (mode, op0, op1);
  enum insn_code icode = find_widening_optab_handler (binoptab, mode,
						      from_mode, 1);
  machine_mode xmode0 = insn_data[(int) icode].operand[1].mode;
  machine_mode xmode1 = insn_data[(int) icode].operand[2].mode;
  machine_mode mode0, mode1, tmp_mode;
  struct expand_operand ops[3];
  bool commutative_p;
  rtx_insn *pat;
  rtx xop0 = op0, xop1 = op1;
  bool canonicalize_op1 = false;

  /* If it is a commutative operator and the modes would match
     if we would swap the operands, we can save the conversions.  */
  commutative_p = commutative_optab_p (binoptab);
  if (commutative_p
      && GET_MODE (xop0) != xmode0 && GET_MODE (xop1) != xmode1
      && GET_MODE (xop0) == xmode1 && GET_MODE (xop1) == xmode1)
    std::swap (xop0, xop1);

  /* If we are optimizing, force expensive constants into a register.  */
  xop0 = avoid_expensive_constant (xmode0, binoptab, 0, xop0, unsignedp);
  if (!shift_optab_p (binoptab))
    xop1 = avoid_expensive_constant (xmode1, binoptab, 1, xop1, unsignedp);
  else
    /* Shifts and rotates often use a different mode for op1 from op0;
       for VOIDmode constants we don't know the mode, so force it
       to be canonicalized using convert_modes.  */
    canonicalize_op1 = true;

  /* In case the insn wants input operands in modes different from
     those of the actual operands, convert the operands.  It would
     seem that we don't need to convert CONST_INTs, but we do, so
     that they're properly zero-extended, sign-extended or truncated
     for their mode.  */

  mode0 = GET_MODE (xop0) != VOIDmode ? GET_MODE (xop0) : mode;
  if (xmode0 != VOIDmode && xmode0 != mode0)
    {
      xop0 = convert_modes (xmode0, mode0, xop0, unsignedp);
      mode0 = xmode0;
    }

  mode1 = ((GET_MODE (xop1) != VOIDmode || canonicalize_op1)
	   ? GET_MODE (xop1) : mode);
  if (xmode1 != VOIDmode && xmode1 != mode1)
    {
      xop1 = convert_modes (xmode1, mode1, xop1, unsignedp);
      mode1 = xmode1;
    }

  /* If operation is commutative,
     try to make the first operand a register.
     Even better, try to make it the same as the target.
     Also try to make the last operand a constant.  */
  if (commutative_p
      && swap_commutative_operands_with_target (target, xop0, xop1))
    std::swap (xop0, xop1);

  /* Now, if insn's predicates don't allow our operands, put them into
     pseudo regs.  */

  if (binoptab == vec_pack_trunc_optab
      || binoptab == vec_pack_usat_optab
      || binoptab == vec_pack_ssat_optab
      || binoptab == vec_pack_ufix_trunc_optab
      || binoptab == vec_pack_sfix_trunc_optab)
    {
      /* The mode of the result is different then the mode of the
	 arguments.  */
      tmp_mode = insn_data[(int) icode].operand[0].mode;
      if (VECTOR_MODE_P (mode)
	  && GET_MODE_NUNITS (tmp_mode) != 2 * GET_MODE_NUNITS (mode))
	{
	  delete_insns_since (last);
	  return NULL_RTX;
	}
    }
  else
    tmp_mode = mode;

  create_output_operand (&ops[0], target, tmp_mode);
  create_input_operand (&ops[1], xop0, mode0);
  create_input_operand (&ops[2], xop1, mode1);
  pat = maybe_gen_insn (icode, 3, ops);
  if (pat)
    {
      /* If PAT is composed of more than one insn, try to add an appropriate
	 REG_EQUAL note to it.  If we can't because TEMP conflicts with an
	 operand, call expand_binop again, this time without a target.  */
      if (INSN_P (pat) && NEXT_INSN (pat) != NULL_RTX
	  && ! add_equal_note (pat, ops[0].value,
			       optab_to_code (binoptab),
			       ops[1].value, ops[2].value))
	{
	  delete_insns_since (last);
	  return expand_binop (mode, binoptab, op0, op1, NULL_RTX,
			       unsignedp, methods);
	}

      emit_insn (pat);
      return ops[0].value;
    }
  delete_insns_since (last);
  return NULL_RTX;
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
expand_binop (machine_mode mode, optab binoptab, rtx op0, rtx op1,
	      rtx target, int unsignedp, enum optab_methods methods)
{
  enum optab_methods next_methods
    = (methods == OPTAB_LIB || methods == OPTAB_LIB_WIDEN
       ? OPTAB_WIDEN : methods);
  enum mode_class mclass;
  machine_mode wider_mode;
  scalar_int_mode int_mode;
  rtx libfunc;
  rtx temp;
  rtx_insn *entry_last = get_last_insn ();
  rtx_insn *last;

  mclass = GET_MODE_CLASS (mode);

  /* If subtracting an integer constant, convert this into an addition of
     the negated constant.  */

  if (binoptab == sub_optab && CONST_INT_P (op1))
    {
      op1 = negate_rtx (mode, op1);
      binoptab = add_optab;
    }
  /* For shifts, constant invalid op1 might be expanded from different
     mode than MODE.  As those are invalid, force them to a register
     to avoid further problems during expansion.  */
  else if (CONST_INT_P (op1)
	   && shift_optab_p (binoptab)
	   && UINTVAL (op1) >= GET_MODE_BITSIZE (GET_MODE_INNER (mode)))
    {
      op1 = gen_int_mode (INTVAL (op1), GET_MODE_INNER (mode));
      op1 = force_reg (GET_MODE_INNER (mode), op1);
    }

  /* Record where to delete back to if we backtrack.  */
  last = get_last_insn ();

  /* If we can do it with a three-operand insn, do so.  */

  if (methods != OPTAB_MUST_WIDEN
      && find_widening_optab_handler (binoptab, mode,
				      widened_mode (mode, op0, op1), 1)
	    != CODE_FOR_nothing)
    {
      temp = expand_binop_directly (mode, binoptab, op0, op1, target,
				    unsignedp, methods, last);
      if (temp)
	return temp;
    }

  /* If we were trying to rotate, and that didn't work, try rotating
     the other direction before falling back to shifts and bitwise-or.  */
  if (((binoptab == rotl_optab
	&& optab_handler (rotr_optab, mode) != CODE_FOR_nothing)
       || (binoptab == rotr_optab
	   && optab_handler (rotl_optab, mode) != CODE_FOR_nothing))
      && is_int_mode (mode, &int_mode))
    {
      optab otheroptab = (binoptab == rotl_optab ? rotr_optab : rotl_optab);
      rtx newop1;
      unsigned int bits = GET_MODE_PRECISION (int_mode);

      if (CONST_INT_P (op1))
        newop1 = GEN_INT (bits - INTVAL (op1));
      else if (targetm.shift_truncation_mask (int_mode) == bits - 1)
        newop1 = negate_rtx (GET_MODE (op1), op1);
      else
        newop1 = expand_binop (GET_MODE (op1), sub_optab,
			       gen_int_mode (bits, GET_MODE (op1)), op1,
			       NULL_RTX, unsignedp, OPTAB_DIRECT);

      temp = expand_binop_directly (int_mode, otheroptab, op0, newop1,
				    target, unsignedp, methods, last);
      if (temp)
	return temp;
    }

  /* If this is a multiply, see if we can do a widening operation that
     takes operands of this mode and makes a wider mode.  */

  if (binoptab == smul_optab
      && GET_MODE_2XWIDER_MODE (mode).exists (&wider_mode)
      && (convert_optab_handler ((unsignedp
				  ? umul_widen_optab
				  : smul_widen_optab),
				 wider_mode, mode) != CODE_FOR_nothing))
    {
      temp = expand_binop (wider_mode,
			   unsignedp ? umul_widen_optab : smul_widen_optab,
			   op0, op1, NULL_RTX, unsignedp, OPTAB_DIRECT);

      if (temp != 0)
	{
	  if (GET_MODE_CLASS (mode) == MODE_INT
	      && TRULY_NOOP_TRUNCATION_MODES_P (mode, GET_MODE (temp)))
	    return gen_lowpart (mode, temp);
	  else
	    return convert_to_mode (mode, temp, unsignedp);
	}
    }

  /* If this is a vector shift by a scalar, see if we can do a vector
     shift by a vector.  If so, broadcast the scalar into a vector.  */
  if (mclass == MODE_VECTOR_INT)
    {
      optab otheroptab = unknown_optab;

      if (binoptab == ashl_optab)
	otheroptab = vashl_optab;
      else if (binoptab == ashr_optab)
	otheroptab = vashr_optab;
      else if (binoptab == lshr_optab)
	otheroptab = vlshr_optab;
      else if (binoptab == rotl_optab)
	otheroptab = vrotl_optab;
      else if (binoptab == rotr_optab)
	otheroptab = vrotr_optab;

      if (otheroptab && optab_handler (otheroptab, mode) != CODE_FOR_nothing)
	{
	  /* The scalar may have been extended to be too wide.  Truncate
	     it back to the proper size to fit in the broadcast vector.  */
	  scalar_mode inner_mode = GET_MODE_INNER (mode);
	  if (!CONST_INT_P (op1)
	      && (GET_MODE_BITSIZE (as_a <scalar_int_mode> (GET_MODE (op1)))
		  > GET_MODE_BITSIZE (inner_mode)))
	    op1 = force_reg (inner_mode,
			     simplify_gen_unary (TRUNCATE, inner_mode, op1,
						 GET_MODE (op1)));
	  rtx vop1 = expand_vector_broadcast (mode, op1);
	  if (vop1)
	    {
	      temp = expand_binop_directly (mode, otheroptab, op0, vop1,
					    target, unsignedp, methods, last);
	      if (temp)
		return temp;
	    }
	}
    }

  /* Look for a wider mode of the same class for which we think we
     can open-code the operation.  Check for a widening multiply at the
     wider mode as well.  */

  if (CLASS_HAS_WIDER_MODES_P (mclass)
      && methods != OPTAB_DIRECT && methods != OPTAB_LIB)
    FOR_EACH_WIDER_MODE (wider_mode, mode)
      {
	machine_mode next_mode;
	if (optab_handler (binoptab, wider_mode) != CODE_FOR_nothing
	    || (binoptab == smul_optab
		&& GET_MODE_WIDER_MODE (wider_mode).exists (&next_mode)
		&& (find_widening_optab_handler ((unsignedp
						  ? umul_widen_optab
						  : smul_widen_optab),
						 next_mode, mode, 0)
		    != CODE_FOR_nothing)))
	  {
	    rtx xop0 = op0, xop1 = op1;
	    int no_extend = 0;

	    /* For certain integer operations, we need not actually extend
	       the narrow operands, as long as we will truncate
	       the results to the same narrowness.  */

	    if ((binoptab == ior_optab || binoptab == and_optab
		 || binoptab == xor_optab
		 || binoptab == add_optab || binoptab == sub_optab
		 || binoptab == smul_optab || binoptab == ashl_optab)
		&& mclass == MODE_INT)
	      {
		no_extend = 1;
		xop0 = avoid_expensive_constant (mode, binoptab, 0,
						 xop0, unsignedp);
		if (binoptab != ashl_optab)
		  xop1 = avoid_expensive_constant (mode, binoptab, 1,
						   xop1, unsignedp);
	      }

	    xop0 = widen_operand (xop0, wider_mode, mode, unsignedp, no_extend);

	    /* The second operand of a shift must always be extended.  */
	    xop1 = widen_operand (xop1, wider_mode, mode, unsignedp,
				  no_extend && binoptab != ashl_optab);

	    temp = expand_binop (wider_mode, binoptab, xop0, xop1, NULL_RTX,
				 unsignedp, OPTAB_DIRECT);
	    if (temp)
	      {
		if (mclass != MODE_INT
                    || !TRULY_NOOP_TRUNCATION_MODES_P (mode, wider_mode))
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

  /* If operation is commutative,
     try to make the first operand a register.
     Even better, try to make it the same as the target.
     Also try to make the last operand a constant.  */
  if (commutative_optab_p (binoptab)
      && swap_commutative_operands_with_target (target, op0, op1))
    std::swap (op0, op1);

  /* These can be done a word at a time.  */
  if ((binoptab == and_optab || binoptab == ior_optab || binoptab == xor_optab)
      && is_int_mode (mode, &int_mode)
      && GET_MODE_SIZE (int_mode) > UNITS_PER_WORD
      && optab_handler (binoptab, word_mode) != CODE_FOR_nothing)
    {
      int i;
      rtx_insn *insns;

      /* If TARGET is the same as one of the operands, the REG_EQUAL note
	 won't be accurate, so use a new target.  */
      if (target == 0
	  || target == op0
	  || target == op1
	  || !valid_multiword_target_p (target))
	target = gen_reg_rtx (int_mode);

      start_sequence ();

      /* Do the actual arithmetic.  */
      for (i = 0; i < GET_MODE_BITSIZE (int_mode) / BITS_PER_WORD; i++)
	{
	  rtx target_piece = operand_subword (target, i, 1, int_mode);
	  rtx x = expand_binop (word_mode, binoptab,
				operand_subword_force (op0, i, int_mode),
				operand_subword_force (op1, i, int_mode),
				target_piece, unsignedp, next_methods);

	  if (x == 0)
	    break;

	  if (target_piece != x)
	    emit_move_insn (target_piece, x);
	}

      insns = get_insns ();
      end_sequence ();

      if (i == GET_MODE_BITSIZE (int_mode) / BITS_PER_WORD)
	{
	  emit_insn (insns);
	  return target;
	}
    }

  /* Synthesize double word shifts from single word shifts.  */
  if ((binoptab == lshr_optab || binoptab == ashl_optab
       || binoptab == ashr_optab)
      && is_int_mode (mode, &int_mode)
      && (CONST_INT_P (op1) || optimize_insn_for_speed_p ())
      && GET_MODE_SIZE (int_mode) == 2 * UNITS_PER_WORD
      && GET_MODE_PRECISION (int_mode) == GET_MODE_BITSIZE (int_mode)
      && optab_handler (binoptab, word_mode) != CODE_FOR_nothing
      && optab_handler (ashl_optab, word_mode) != CODE_FOR_nothing
      && optab_handler (lshr_optab, word_mode) != CODE_FOR_nothing)
    {
      unsigned HOST_WIDE_INT shift_mask, double_shift_mask;
      scalar_int_mode op1_mode;

      double_shift_mask = targetm.shift_truncation_mask (int_mode);
      shift_mask = targetm.shift_truncation_mask (word_mode);
      op1_mode = (GET_MODE (op1) != VOIDmode
		  ? as_a <scalar_int_mode> (GET_MODE (op1))
		  : word_mode);

      /* Apply the truncation to constant shifts.  */
      if (double_shift_mask > 0 && CONST_INT_P (op1))
	op1 = GEN_INT (INTVAL (op1) & double_shift_mask);

      if (op1 == CONST0_RTX (op1_mode))
	return op0;

      /* Make sure that this is a combination that expand_doubleword_shift
	 can handle.  See the comments there for details.  */
      if (double_shift_mask == 0
	  || (shift_mask == BITS_PER_WORD - 1
	      && double_shift_mask == BITS_PER_WORD * 2 - 1))
	{
	  rtx_insn *insns;
	  rtx into_target, outof_target;
	  rtx into_input, outof_input;
	  int left_shift, outof_word;

	  /* If TARGET is the same as one of the operands, the REG_EQUAL note
	     won't be accurate, so use a new target.  */
	  if (target == 0
	      || target == op0
	      || target == op1
	      || !valid_multiword_target_p (target))
	    target = gen_reg_rtx (int_mode);

	  start_sequence ();

	  /* OUTOF_* is the word we are shifting bits away from, and
	     INTO_* is the word that we are shifting bits towards, thus
	     they differ depending on the direction of the shift and
	     WORDS_BIG_ENDIAN.  */

	  left_shift = binoptab == ashl_optab;
	  outof_word = left_shift ^ ! WORDS_BIG_ENDIAN;

	  outof_target = operand_subword (target, outof_word, 1, int_mode);
	  into_target = operand_subword (target, 1 - outof_word, 1, int_mode);

	  outof_input = operand_subword_force (op0, outof_word, int_mode);
	  into_input = operand_subword_force (op0, 1 - outof_word, int_mode);

	  if (expand_doubleword_shift (op1_mode, binoptab,
				       outof_input, into_input, op1,
				       outof_target, into_target,
				       unsignedp, next_methods, shift_mask))
	    {
	      insns = get_insns ();
	      end_sequence ();

	      emit_insn (insns);
	      return target;
	    }
	  end_sequence ();
	}
    }

  /* Synthesize double word rotates from single word shifts.  */
  if ((binoptab == rotl_optab || binoptab == rotr_optab)
      && is_int_mode (mode, &int_mode)
      && CONST_INT_P (op1)
      && GET_MODE_PRECISION (int_mode) == 2 * BITS_PER_WORD
      && optab_handler (ashl_optab, word_mode) != CODE_FOR_nothing
      && optab_handler (lshr_optab, word_mode) != CODE_FOR_nothing)
    {
      rtx_insn *insns;
      rtx into_target, outof_target;
      rtx into_input, outof_input;
      rtx inter;
      int shift_count, left_shift, outof_word;

      /* If TARGET is the same as one of the operands, the REG_EQUAL note
	 won't be accurate, so use a new target. Do this also if target is not
	 a REG, first because having a register instead may open optimization
	 opportunities, and second because if target and op0 happen to be MEMs
	 designating the same location, we would risk clobbering it too early
	 in the code sequence we generate below.  */
      if (target == 0
	  || target == op0
	  || target == op1
	  || !REG_P (target)
	  || !valid_multiword_target_p (target))
	target = gen_reg_rtx (int_mode);

      start_sequence ();

      shift_count = INTVAL (op1);

      /* OUTOF_* is the word we are shifting bits away from, and
	 INTO_* is the word that we are shifting bits towards, thus
	 they differ depending on the direction of the shift and
	 WORDS_BIG_ENDIAN.  */

      left_shift = (binoptab == rotl_optab);
      outof_word = left_shift ^ ! WORDS_BIG_ENDIAN;

      outof_target = operand_subword (target, outof_word, 1, int_mode);
      into_target = operand_subword (target, 1 - outof_word, 1, int_mode);

      outof_input = operand_subword_force (op0, outof_word, int_mode);
      into_input = operand_subword_force (op0, 1 - outof_word, int_mode);

      if (shift_count == BITS_PER_WORD)
	{
	  /* This is just a word swap.  */
	  emit_move_insn (outof_target, into_input);
	  emit_move_insn (into_target, outof_input);
	  inter = const0_rtx;
	}
      else
	{
	  rtx into_temp1, into_temp2, outof_temp1, outof_temp2;
	  rtx first_shift_count, second_shift_count;
	  optab reverse_unsigned_shift, unsigned_shift;

	  reverse_unsigned_shift = (left_shift ^ (shift_count < BITS_PER_WORD)
				    ? lshr_optab : ashl_optab);

	  unsigned_shift = (left_shift ^ (shift_count < BITS_PER_WORD)
			    ? ashl_optab : lshr_optab);

	  if (shift_count > BITS_PER_WORD)
	    {
	      first_shift_count = GEN_INT (shift_count - BITS_PER_WORD);
	      second_shift_count = GEN_INT (2 * BITS_PER_WORD - shift_count);
	    }
	  else
	    {
	      first_shift_count = GEN_INT (BITS_PER_WORD - shift_count);
	      second_shift_count = GEN_INT (shift_count);
	    }

	  into_temp1 = expand_binop (word_mode, unsigned_shift,
				     outof_input, first_shift_count,
				     NULL_RTX, unsignedp, next_methods);
	  into_temp2 = expand_binop (word_mode, reverse_unsigned_shift,
				     into_input, second_shift_count,
				     NULL_RTX, unsignedp, next_methods);

	  if (into_temp1 != 0 && into_temp2 != 0)
	    inter = expand_binop (word_mode, ior_optab, into_temp1, into_temp2,
				  into_target, unsignedp, next_methods);
	  else
	    inter = 0;

	  if (inter != 0 && inter != into_target)
	    emit_move_insn (into_target, inter);

	  outof_temp1 = expand_binop (word_mode, unsigned_shift,
				      into_input, first_shift_count,
				      NULL_RTX, unsignedp, next_methods);
	  outof_temp2 = expand_binop (word_mode, reverse_unsigned_shift,
				      outof_input, second_shift_count,
				      NULL_RTX, unsignedp, next_methods);

	  if (inter != 0 && outof_temp1 != 0 && outof_temp2 != 0)
	    inter = expand_binop (word_mode, ior_optab,
				  outof_temp1, outof_temp2,
				  outof_target, unsignedp, next_methods);

	  if (inter != 0 && inter != outof_target)
	    emit_move_insn (outof_target, inter);
	}

      insns = get_insns ();
      end_sequence ();

      if (inter != 0)
	{
	  emit_insn (insns);
	  return target;
	}
    }

  /* These can be done a word at a time by propagating carries.  */
  if ((binoptab == add_optab || binoptab == sub_optab)
      && is_int_mode (mode, &int_mode)
      && GET_MODE_SIZE (int_mode) >= 2 * UNITS_PER_WORD
      && optab_handler (binoptab, word_mode) != CODE_FOR_nothing)
    {
      unsigned int i;
      optab otheroptab = binoptab == add_optab ? sub_optab : add_optab;
      const unsigned int nwords = GET_MODE_BITSIZE (int_mode) / BITS_PER_WORD;
      rtx carry_in = NULL_RTX, carry_out = NULL_RTX;
      rtx xop0, xop1, xtarget;

      /* We can handle either a 1 or -1 value for the carry.  If STORE_FLAG
	 value is one of those, use it.  Otherwise, use 1 since it is the
	 one easiest to get.  */
#if STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1
      int normalizep = STORE_FLAG_VALUE;
#else
      int normalizep = 1;
#endif

      /* Prepare the operands.  */
      xop0 = force_reg (int_mode, op0);
      xop1 = force_reg (int_mode, op1);

      xtarget = gen_reg_rtx (int_mode);

      if (target == 0 || !REG_P (target) || !valid_multiword_target_p (target))
	target = xtarget;

      /* Indicate for flow that the entire target reg is being set.  */
      if (REG_P (target))
	emit_clobber (xtarget);

      /* Do the actual arithmetic.  */
      for (i = 0; i < nwords; i++)
	{
	  int index = (WORDS_BIG_ENDIAN ? nwords - i - 1 : i);
	  rtx target_piece = operand_subword (xtarget, index, 1, int_mode);
	  rtx op0_piece = operand_subword_force (xop0, index, int_mode);
	  rtx op1_piece = operand_subword_force (xop1, index, int_mode);
	  rtx x;

	  /* Main add/subtract of the input operands.  */
	  x = expand_binop (word_mode, binoptab,
			    op0_piece, op1_piece,
			    target_piece, unsignedp, next_methods);
	  if (x == 0)
	    break;

	  if (i + 1 < nwords)
	    {
	      /* Store carry from main add/subtract.  */
	      carry_out = gen_reg_rtx (word_mode);
	      carry_out = emit_store_flag_force (carry_out,
						 (binoptab == add_optab
						  ? LT : GT),
						 x, op0_piece,
						 word_mode, 1, normalizep);
	    }

	  if (i > 0)
	    {
	      rtx newx;

	      /* Add/subtract previous carry to main result.  */
	      newx = expand_binop (word_mode,
				   normalizep == 1 ? binoptab : otheroptab,
				   x, carry_in,
				   NULL_RTX, 1, next_methods);

	      if (i + 1 < nwords)
		{
		  /* Get out carry from adding/subtracting carry in.  */
		  rtx carry_tmp = gen_reg_rtx (word_mode);
		  carry_tmp = emit_store_flag_force (carry_tmp,
						     (binoptab == add_optab
						      ? LT : GT),
						     newx, x,
						     word_mode, 1, normalizep);

		  /* Logical-ior the two poss. carry together.  */
		  carry_out = expand_binop (word_mode, ior_optab,
					    carry_out, carry_tmp,
					    carry_out, 0, next_methods);
		  if (carry_out == 0)
		    break;
		}
	      emit_move_insn (target_piece, newx);
	    }
	  else
	    {
	      if (x != target_piece)
		emit_move_insn (target_piece, x);
	    }

	  carry_in = carry_out;
	}

      if (i == GET_MODE_BITSIZE (int_mode) / (unsigned) BITS_PER_WORD)
	{
	  if (optab_handler (mov_optab, int_mode) != CODE_FOR_nothing
	      || ! rtx_equal_p (target, xtarget))
	    {
	      rtx_insn *temp = emit_move_insn (target, xtarget);

	      set_dst_reg_note (temp, REG_EQUAL,
				gen_rtx_fmt_ee (optab_to_code (binoptab),
						int_mode, copy_rtx (xop0),
						copy_rtx (xop1)),
				target);
	    }
	  else
	    target = xtarget;

	  return target;
	}

      else
	delete_insns_since (last);
    }

  /* Attempt to synthesize double word multiplies using a sequence of word
     mode multiplications.  We first attempt to generate a sequence using a
     more efficient unsigned widening multiply, and if that fails we then
     try using a signed widening multiply.  */

  if (binoptab == smul_optab
      && is_int_mode (mode, &int_mode)
      && GET_MODE_SIZE (int_mode) == 2 * UNITS_PER_WORD
      && optab_handler (smul_optab, word_mode) != CODE_FOR_nothing
      && optab_handler (add_optab, word_mode) != CODE_FOR_nothing)
    {
      rtx product = NULL_RTX;
      if (widening_optab_handler (umul_widen_optab, int_mode, word_mode)
	  != CODE_FOR_nothing)
	{
	  product = expand_doubleword_mult (int_mode, op0, op1, target,
					    true, methods);
	  if (!product)
	    delete_insns_since (last);
	}

      if (product == NULL_RTX
	  && (widening_optab_handler (smul_widen_optab, int_mode, word_mode)
	      != CODE_FOR_nothing))
	{
	  product = expand_doubleword_mult (int_mode, op0, op1, target,
					    false, methods);
	  if (!product)
	    delete_insns_since (last);
	}

      if (product != NULL_RTX)
	{
	  if (optab_handler (mov_optab, int_mode) != CODE_FOR_nothing)
	    {
	      rtx_insn *move = emit_move_insn (target ? target : product,
					       product);
	      set_dst_reg_note (move,
				REG_EQUAL,
				gen_rtx_fmt_ee (MULT, int_mode,
						copy_rtx (op0),
						copy_rtx (op1)),
				target ? target : product);
	    }
	  return product;
	}
    }

  /* It can't be open-coded in this mode.
     Use a library call if one is available and caller says that's ok.  */

  libfunc = optab_libfunc (binoptab, mode);
  if (libfunc
      && (methods == OPTAB_LIB || methods == OPTAB_LIB_WIDEN))
    {
      rtx_insn *insns;
      rtx op1x = op1;
      machine_mode op1_mode = mode;
      rtx value;

      start_sequence ();

      if (shift_optab_p (binoptab))
	{
	  op1_mode = targetm.libgcc_shift_count_mode ();
	  /* Specify unsigned here,
	     since negative shift counts are meaningless.  */
	  op1x = convert_to_mode (op1_mode, op1, 1);
	}

      if (GET_MODE (op0) != VOIDmode
	  && GET_MODE (op0) != mode)
	op0 = convert_to_mode (mode, op0, unsignedp);

      /* Pass 1 for NO_QUEUE so we don't lose any increments
	 if the libcall is cse'd or moved.  */
      value = emit_library_call_value (libfunc,
				       NULL_RTX, LCT_CONST, mode, 2,
				       op0, mode, op1x, op1_mode);

      insns = get_insns ();
      end_sequence ();

      bool trapv = trapv_binoptab_p (binoptab);
      target = gen_reg_rtx (mode);
      emit_libcall_block_1 (insns, target, value,
			    trapv ? NULL_RTX
			    : gen_rtx_fmt_ee (optab_to_code (binoptab),
					      mode, op0, op1), trapv);

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

  if (CLASS_HAS_WIDER_MODES_P (mclass))
    {
      FOR_EACH_WIDER_MODE (wider_mode, mode)
	{
	  if (find_widening_optab_handler (binoptab, wider_mode, mode, 1)
		  != CODE_FOR_nothing
	      || (methods == OPTAB_LIB
		  && optab_libfunc (binoptab, wider_mode)))
	    {
	      rtx xop0 = op0, xop1 = op1;
	      int no_extend = 0;

	      /* For certain integer operations, we need not actually extend
		 the narrow operands, as long as we will truncate
		 the results to the same narrowness.  */

	      if ((binoptab == ior_optab || binoptab == and_optab
		   || binoptab == xor_optab
		   || binoptab == add_optab || binoptab == sub_optab
		   || binoptab == smul_optab || binoptab == ashl_optab)
		  && mclass == MODE_INT)
		no_extend = 1;

	      xop0 = widen_operand (xop0, wider_mode, mode,
				    unsignedp, no_extend);

	      /* The second operand of a shift must always be extended.  */
	      xop1 = widen_operand (xop1, wider_mode, mode, unsignedp,
				    no_extend && binoptab != ashl_optab);

	      temp = expand_binop (wider_mode, binoptab, xop0, xop1, NULL_RTX,
				   unsignedp, methods);
	      if (temp)
		{
		  if (mclass != MODE_INT
		      || !TRULY_NOOP_TRUNCATION_MODES_P (mode, wider_mode))
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
sign_expand_binop (machine_mode mode, optab uoptab, optab soptab,
		   rtx op0, rtx op1, rtx target, int unsignedp,
		   enum optab_methods methods)
{
  rtx temp;
  optab direct_optab = unsignedp ? uoptab : soptab;
  bool save_enable;

  /* Do it without widening, if possible.  */
  temp = expand_binop (mode, direct_optab, op0, op1, target,
		       unsignedp, OPTAB_DIRECT);
  if (temp || methods == OPTAB_DIRECT)
    return temp;

  /* Try widening to a signed int.  Disable any direct use of any
     signed insn in the current mode.  */
  save_enable = swap_optab_enable (soptab, mode, false);

  temp = expand_binop (mode, soptab, op0, op1, target,
		       unsignedp, OPTAB_WIDEN);

  /* For unsigned operands, try widening to an unsigned int.  */
  if (!temp && unsignedp)
    temp = expand_binop (mode, uoptab, op0, op1, target,
			 unsignedp, OPTAB_WIDEN);
  if (temp || methods == OPTAB_WIDEN)
    goto egress;

  /* Use the right width libcall if that exists.  */
  temp = expand_binop (mode, direct_optab, op0, op1, target,
		       unsignedp, OPTAB_LIB);
  if (temp || methods == OPTAB_LIB)
    goto egress;

  /* Must widen and use a libcall, use either signed or unsigned.  */
  temp = expand_binop (mode, soptab, op0, op1, target,
		       unsignedp, methods);
  if (!temp && unsignedp)
    temp = expand_binop (mode, uoptab, op0, op1, target,
			 unsignedp, methods);

 egress:
  /* Undo the fiddling above.  */
  if (save_enable)
    swap_optab_enable (soptab, mode, true);
  return temp;
}

/* Generate code to perform an operation specified by UNOPPTAB
   on operand OP0, with two results to TARG0 and TARG1.
   We assume that the order of the operands for the instruction
   is TARG0, TARG1, OP0.

   Either TARG0 or TARG1 may be zero, but what that means is that
   the result is not actually wanted.  We will generate it into
   a dummy pseudo-reg and discard it.  They may not both be zero.

   Returns 1 if this operation can be performed; 0 if not.  */

int
expand_twoval_unop (optab unoptab, rtx op0, rtx targ0, rtx targ1,
		    int unsignedp)
{
  machine_mode mode = GET_MODE (targ0 ? targ0 : targ1);
  enum mode_class mclass;
  machine_mode wider_mode;
  rtx_insn *entry_last = get_last_insn ();
  rtx_insn *last;

  mclass = GET_MODE_CLASS (mode);

  if (!targ0)
    targ0 = gen_reg_rtx (mode);
  if (!targ1)
    targ1 = gen_reg_rtx (mode);

  /* Record where to go back to if we fail.  */
  last = get_last_insn ();

  if (optab_handler (unoptab, mode) != CODE_FOR_nothing)
    {
      struct expand_operand ops[3];
      enum insn_code icode = optab_handler (unoptab, mode);

      create_fixed_operand (&ops[0], targ0);
      create_fixed_operand (&ops[1], targ1);
      create_convert_operand_from (&ops[2], op0, mode, unsignedp);
      if (maybe_expand_insn (icode, 3, ops))
	return 1;
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if (CLASS_HAS_WIDER_MODES_P (mclass))
    {
      FOR_EACH_WIDER_MODE (wider_mode, mode)
	{
	  if (optab_handler (unoptab, wider_mode) != CODE_FOR_nothing)
	    {
	      rtx t0 = gen_reg_rtx (wider_mode);
	      rtx t1 = gen_reg_rtx (wider_mode);
	      rtx cop0 = convert_modes (wider_mode, mode, op0, unsignedp);

	      if (expand_twoval_unop (unoptab, cop0, t0, t1, unsignedp))
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

/* Generate code to perform an operation specified by BINOPTAB
   on operands OP0 and OP1, with two results to TARG1 and TARG2.
   We assume that the order of the operands for the instruction
   is TARG0, OP0, OP1, TARG1, which would fit a pattern like
   [(set TARG0 (operate OP0 OP1)) (set TARG1 (operate ...))].

   Either TARG0 or TARG1 may be zero, but what that means is that
   the result is not actually wanted.  We will generate it into
   a dummy pseudo-reg and discard it.  They may not both be zero.

   Returns 1 if this operation can be performed; 0 if not.  */

int
expand_twoval_binop (optab binoptab, rtx op0, rtx op1, rtx targ0, rtx targ1,
		     int unsignedp)
{
  machine_mode mode = GET_MODE (targ0 ? targ0 : targ1);
  enum mode_class mclass;
  machine_mode wider_mode;
  rtx_insn *entry_last = get_last_insn ();
  rtx_insn *last;

  mclass = GET_MODE_CLASS (mode);

  if (!targ0)
    targ0 = gen_reg_rtx (mode);
  if (!targ1)
    targ1 = gen_reg_rtx (mode);

  /* Record where to go back to if we fail.  */
  last = get_last_insn ();

  if (optab_handler (binoptab, mode) != CODE_FOR_nothing)
    {
      struct expand_operand ops[4];
      enum insn_code icode = optab_handler (binoptab, mode);
      machine_mode mode0 = insn_data[icode].operand[1].mode;
      machine_mode mode1 = insn_data[icode].operand[2].mode;
      rtx xop0 = op0, xop1 = op1;

      /* If we are optimizing, force expensive constants into a register.  */
      xop0 = avoid_expensive_constant (mode0, binoptab, 0, xop0, unsignedp);
      xop1 = avoid_expensive_constant (mode1, binoptab, 1, xop1, unsignedp);

      create_fixed_operand (&ops[0], targ0);
      create_convert_operand_from (&ops[1], op0, mode, unsignedp);
      create_convert_operand_from (&ops[2], op1, mode, unsignedp);
      create_fixed_operand (&ops[3], targ1);
      if (maybe_expand_insn (icode, 4, ops))
	return 1;
      delete_insns_since (last);
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if (CLASS_HAS_WIDER_MODES_P (mclass))
    {
      FOR_EACH_WIDER_MODE (wider_mode, mode)
	{
	  if (optab_handler (binoptab, wider_mode) != CODE_FOR_nothing)
	    {
	      rtx t0 = gen_reg_rtx (wider_mode);
	      rtx t1 = gen_reg_rtx (wider_mode);
	      rtx cop0 = convert_modes (wider_mode, mode, op0, unsignedp);
	      rtx cop1 = convert_modes (wider_mode, mode, op1, unsignedp);

	      if (expand_twoval_binop (binoptab, cop0, cop1,
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

/* Expand the two-valued library call indicated by BINOPTAB, but
   preserve only one of the values.  If TARG0 is non-NULL, the first
   value is placed into TARG0; otherwise the second value is placed
   into TARG1.  Exactly one of TARG0 and TARG1 must be non-NULL.  The
   value stored into TARG0 or TARG1 is equivalent to (CODE OP0 OP1).
   This routine assumes that the value returned by the library call is
   as if the return value was of an integral mode twice as wide as the
   mode of OP0.  Returns 1 if the call was successful.  */

bool
expand_twoval_binop_libfunc (optab binoptab, rtx op0, rtx op1,
			     rtx targ0, rtx targ1, enum rtx_code code)
{
  machine_mode mode;
  machine_mode libval_mode;
  rtx libval;
  rtx_insn *insns;
  rtx libfunc;

  /* Exactly one of TARG0 or TARG1 should be non-NULL.  */
  gcc_assert (!targ0 != !targ1);

  mode = GET_MODE (op0);
  libfunc = optab_libfunc (binoptab, mode);
  if (!libfunc)
    return false;

  /* The value returned by the library function will have twice as
     many bits as the nominal MODE.  */
  libval_mode = smallest_int_mode_for_size (2 * GET_MODE_BITSIZE (mode));
  start_sequence ();
  libval = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST,
				    libval_mode, 2,
				    op0, mode,
				    op1, mode);
  /* Get the part of VAL containing the value that we want.  */
  libval = simplify_gen_subreg (mode, libval, libval_mode,
				targ0 ? 0 : GET_MODE_SIZE (mode));
  insns = get_insns ();
  end_sequence ();
  /* Move the into the desired location.  */
  emit_libcall_block (insns, targ0 ? targ0 : targ1, libval,
		      gen_rtx_fmt_ee (code, mode, op0, op1));

  return true;
}


/* Wrapper around expand_unop which takes an rtx code to specify
   the operation to perform, not an optab pointer.  All other
   arguments are the same.  */
rtx
expand_simple_unop (machine_mode mode, enum rtx_code code, rtx op0,
		    rtx target, int unsignedp)
{
  optab unop = code_to_optab (code);
  gcc_assert (unop);

  return expand_unop (mode, unop, op0, target, unsignedp);
}

/* Try calculating
	(clz:narrow x)
   as
	(clz:wide (zero_extend:wide x)) - ((width wide) - (width narrow)).

   A similar operation can be used for clrsb.  UNOPTAB says which operation
   we are trying to expand.  */
static rtx
widen_leading (scalar_int_mode mode, rtx op0, rtx target, optab unoptab)
{
  opt_scalar_int_mode wider_mode_iter;
  FOR_EACH_WIDER_MODE (wider_mode_iter, mode)
    {
      scalar_int_mode wider_mode = wider_mode_iter.require ();
      if (optab_handler (unoptab, wider_mode) != CODE_FOR_nothing)
	{
	  rtx xop0, temp;
	  rtx_insn *last;

	  last = get_last_insn ();

	  if (target == 0)
	    target = gen_reg_rtx (mode);
	  xop0 = widen_operand (op0, wider_mode, mode,
				unoptab != clrsb_optab, false);
	  temp = expand_unop (wider_mode, unoptab, xop0, NULL_RTX,
			      unoptab != clrsb_optab);
	  if (temp != 0)
	    temp = expand_binop
	      (wider_mode, sub_optab, temp,
	       gen_int_mode (GET_MODE_PRECISION (wider_mode)
			     - GET_MODE_PRECISION (mode),
			     wider_mode),
	       target, true, OPTAB_DIRECT);
	  if (temp == 0)
	    delete_insns_since (last);

	  return temp;
	}
    }
  return 0;
}

/* Try calculating clz of a double-word quantity as two clz's of word-sized
   quantities, choosing which based on whether the high word is nonzero.  */
static rtx
expand_doubleword_clz (scalar_int_mode mode, rtx op0, rtx target)
{
  rtx xop0 = force_reg (mode, op0);
  rtx subhi = gen_highpart (word_mode, xop0);
  rtx sublo = gen_lowpart (word_mode, xop0);
  rtx_code_label *hi0_label = gen_label_rtx ();
  rtx_code_label *after_label = gen_label_rtx ();
  rtx_insn *seq;
  rtx temp, result;

  /* If we were not given a target, use a word_mode register, not a
     'mode' register.  The result will fit, and nobody is expecting
     anything bigger (the return type of __builtin_clz* is int).  */
  if (!target)
    target = gen_reg_rtx (word_mode);

  /* In any case, write to a word_mode scratch in both branches of the
     conditional, so we can ensure there is a single move insn setting
     'target' to tag a REG_EQUAL note on.  */
  result = gen_reg_rtx (word_mode);

  start_sequence ();

  /* If the high word is not equal to zero,
     then clz of the full value is clz of the high word.  */
  emit_cmp_and_jump_insns (subhi, CONST0_RTX (word_mode), EQ, 0,
			   word_mode, true, hi0_label);

  temp = expand_unop_direct (word_mode, clz_optab, subhi, result, true);
  if (!temp)
    goto fail;

  if (temp != result)
    convert_move (result, temp, true);

  emit_jump_insn (targetm.gen_jump (after_label));
  emit_barrier ();

  /* Else clz of the full value is clz of the low word plus the number
     of bits in the high word.  */
  emit_label (hi0_label);

  temp = expand_unop_direct (word_mode, clz_optab, sublo, 0, true);
  if (!temp)
    goto fail;
  temp = expand_binop (word_mode, add_optab, temp,
		       gen_int_mode (GET_MODE_BITSIZE (word_mode), word_mode),
		       result, true, OPTAB_DIRECT);
  if (!temp)
    goto fail;
  if (temp != result)
    convert_move (result, temp, true);

  emit_label (after_label);
  convert_move (target, result, true);

  seq = get_insns ();
  end_sequence ();

  add_equal_note (seq, target, CLZ, xop0, 0);
  emit_insn (seq);
  return target;

 fail:
  end_sequence ();
  return 0;
}

/* Try calculating popcount of a double-word quantity as two popcount's of
   word-sized quantities and summing up the results.  */
static rtx
expand_doubleword_popcount (scalar_int_mode mode, rtx op0, rtx target)
{
  rtx t0, t1, t;
  rtx_insn *seq;

  start_sequence ();

  t0 = expand_unop_direct (word_mode, popcount_optab,
			   operand_subword_force (op0, 0, mode), NULL_RTX,
			   true);
  t1 = expand_unop_direct (word_mode, popcount_optab,
			   operand_subword_force (op0, 1, mode), NULL_RTX,
			   true);
  if (!t0 || !t1)
    {
      end_sequence ();
      return NULL_RTX;
    }

  /* If we were not given a target, use a word_mode register, not a
     'mode' register.  The result will fit, and nobody is expecting
     anything bigger (the return type of __builtin_popcount* is int).  */
  if (!target)
    target = gen_reg_rtx (word_mode);

  t = expand_binop (word_mode, add_optab, t0, t1, target, 0, OPTAB_DIRECT);

  seq = get_insns ();
  end_sequence ();

  add_equal_note (seq, t, POPCOUNT, op0, 0);
  emit_insn (seq);
  return t;
}

/* Try calculating
	(parity:wide x)
   as
	(parity:narrow (low (x) ^ high (x))) */
static rtx
expand_doubleword_parity (scalar_int_mode mode, rtx op0, rtx target)
{
  rtx t = expand_binop (word_mode, xor_optab,
			operand_subword_force (op0, 0, mode),
			operand_subword_force (op0, 1, mode),
			NULL_RTX, 0, OPTAB_DIRECT);
  return expand_unop (word_mode, parity_optab, t, target, true);
}

/* Try calculating
	(bswap:narrow x)
   as
	(lshiftrt:wide (bswap:wide x) ((width wide) - (width narrow))).  */
static rtx
widen_bswap (scalar_int_mode mode, rtx op0, rtx target)
{
  rtx x;
  rtx_insn *last;
  opt_scalar_int_mode wider_mode_iter;

  FOR_EACH_WIDER_MODE (wider_mode_iter, mode)
    if (optab_handler (bswap_optab, wider_mode_iter.require ())
	!= CODE_FOR_nothing)
      break;

  if (!wider_mode_iter.exists ())
    return NULL_RTX;

  scalar_int_mode wider_mode = wider_mode_iter.require ();
  last = get_last_insn ();

  x = widen_operand (op0, wider_mode, mode, true, true);
  x = expand_unop (wider_mode, bswap_optab, x, NULL_RTX, true);

  gcc_assert (GET_MODE_PRECISION (wider_mode) == GET_MODE_BITSIZE (wider_mode)
	      && GET_MODE_PRECISION (mode) == GET_MODE_BITSIZE (mode));
  if (x != 0)
    x = expand_shift (RSHIFT_EXPR, wider_mode, x,
		      GET_MODE_BITSIZE (wider_mode)
		      - GET_MODE_BITSIZE (mode),
		      NULL_RTX, true);

  if (x != 0)
    {
      if (target == 0)
	target = gen_reg_rtx (mode);
      emit_move_insn (target, gen_lowpart (mode, x));
    }
  else
    delete_insns_since (last);

  return target;
}

/* Try calculating bswap as two bswaps of two word-sized operands.  */

static rtx
expand_doubleword_bswap (machine_mode mode, rtx op, rtx target)
{
  rtx t0, t1;

  t1 = expand_unop (word_mode, bswap_optab,
		    operand_subword_force (op, 0, mode), NULL_RTX, true);
  t0 = expand_unop (word_mode, bswap_optab,
		    operand_subword_force (op, 1, mode), NULL_RTX, true);

  if (target == 0 || !valid_multiword_target_p (target))
    target = gen_reg_rtx (mode);
  if (REG_P (target))
    emit_clobber (target);
  emit_move_insn (operand_subword (target, 0, 1, mode), t0);
  emit_move_insn (operand_subword (target, 1, 1, mode), t1);

  return target;
}

/* Try calculating (parity x) as (and (popcount x) 1), where
   popcount can also be done in a wider mode.  */
static rtx
expand_parity (scalar_int_mode mode, rtx op0, rtx target)
{
  enum mode_class mclass = GET_MODE_CLASS (mode);
  opt_scalar_int_mode wider_mode_iter;
  FOR_EACH_MODE_FROM (wider_mode_iter, mode)
    {
      scalar_int_mode wider_mode = wider_mode_iter.require ();
      if (optab_handler (popcount_optab, wider_mode) != CODE_FOR_nothing)
	{
	  rtx xop0, temp;
	  rtx_insn *last;

	  last = get_last_insn ();

	  if (target == 0 || GET_MODE (target) != wider_mode)
	    target = gen_reg_rtx (wider_mode);

	  xop0 = widen_operand (op0, wider_mode, mode, true, false);
	  temp = expand_unop (wider_mode, popcount_optab, xop0, NULL_RTX,
			      true);
	  if (temp != 0)
	    temp = expand_binop (wider_mode, and_optab, temp, const1_rtx,
				 target, true, OPTAB_DIRECT);

	  if (temp)
	    {
	      if (mclass != MODE_INT
		  || !TRULY_NOOP_TRUNCATION_MODES_P (mode, wider_mode))
		return convert_to_mode (mode, temp, 0);
	      else
		return gen_lowpart (mode, temp);
	    }
	  else
	    delete_insns_since (last);
	}
    }
  return 0;
}

/* Try calculating ctz(x) as K - clz(x & -x) ,
   where K is GET_MODE_PRECISION(mode) - 1.

   Both __builtin_ctz and __builtin_clz are undefined at zero, so we
   don't have to worry about what the hardware does in that case.  (If
   the clz instruction produces the usual value at 0, which is K, the
   result of this code sequence will be -1; expand_ffs, below, relies
   on this.  It might be nice to have it be K instead, for consistency
   with the (very few) processors that provide a ctz with a defined
   value, but that would take one more instruction, and it would be
   less convenient for expand_ffs anyway.  */

static rtx
expand_ctz (scalar_int_mode mode, rtx op0, rtx target)
{
  rtx_insn *seq;
  rtx temp;

  if (optab_handler (clz_optab, mode) == CODE_FOR_nothing)
    return 0;

  start_sequence ();

  temp = expand_unop_direct (mode, neg_optab, op0, NULL_RTX, true);
  if (temp)
    temp = expand_binop (mode, and_optab, op0, temp, NULL_RTX,
			 true, OPTAB_DIRECT);
  if (temp)
    temp = expand_unop_direct (mode, clz_optab, temp, NULL_RTX, true);
  if (temp)
    temp = expand_binop (mode, sub_optab,
			 gen_int_mode (GET_MODE_PRECISION (mode) - 1, mode),
			 temp, target,
			 true, OPTAB_DIRECT);
  if (temp == 0)
    {
      end_sequence ();
      return 0;
    }

  seq = get_insns ();
  end_sequence ();

  add_equal_note (seq, temp, CTZ, op0, 0);
  emit_insn (seq);
  return temp;
}


/* Try calculating ffs(x) using ctz(x) if we have that instruction, or
   else with the sequence used by expand_clz.

   The ffs builtin promises to return zero for a zero value and ctz/clz
   may have an undefined value in that case.  If they do not give us a
   convenient value, we have to generate a test and branch.  */
static rtx
expand_ffs (scalar_int_mode mode, rtx op0, rtx target)
{
  HOST_WIDE_INT val = 0;
  bool defined_at_zero = false;
  rtx temp;
  rtx_insn *seq;

  if (optab_handler (ctz_optab, mode) != CODE_FOR_nothing)
    {
      start_sequence ();

      temp = expand_unop_direct (mode, ctz_optab, op0, 0, true);
      if (!temp)
	goto fail;

      defined_at_zero = (CTZ_DEFINED_VALUE_AT_ZERO (mode, val) == 2);
    }
  else if (optab_handler (clz_optab, mode) != CODE_FOR_nothing)
    {
      start_sequence ();
      temp = expand_ctz (mode, op0, 0);
      if (!temp)
	goto fail;

      if (CLZ_DEFINED_VALUE_AT_ZERO (mode, val) == 2)
	{
	  defined_at_zero = true;
	  val = (GET_MODE_PRECISION (mode) - 1) - val;
	}
    }
  else
    return 0;

  if (defined_at_zero && val == -1)
    /* No correction needed at zero.  */;
  else
    {
      /* We don't try to do anything clever with the situation found
	 on some processors (eg Alpha) where ctz(0:mode) ==
	 bitsize(mode).  If someone can think of a way to send N to -1
	 and leave alone all values in the range 0..N-1 (where N is a
	 power of two), cheaper than this test-and-branch, please add it.

	 The test-and-branch is done after the operation itself, in case
	 the operation sets condition codes that can be recycled for this.
	 (This is true on i386, for instance.)  */

      rtx_code_label *nonzero_label = gen_label_rtx ();
      emit_cmp_and_jump_insns (op0, CONST0_RTX (mode), NE, 0,
			       mode, true, nonzero_label);

      convert_move (temp, GEN_INT (-1), false);
      emit_label (nonzero_label);
    }

  /* temp now has a value in the range -1..bitsize-1.  ffs is supposed
     to produce a value in the range 0..bitsize.  */
  temp = expand_binop (mode, add_optab, temp, gen_int_mode (1, mode),
		       target, false, OPTAB_DIRECT);
  if (!temp)
    goto fail;

  seq = get_insns ();
  end_sequence ();

  add_equal_note (seq, temp, FFS, op0, 0);
  emit_insn (seq);
  return temp;

 fail:
  end_sequence ();
  return 0;
}

/* Extract the OMODE lowpart from VAL, which has IMODE.  Under certain
   conditions, VAL may already be a SUBREG against which we cannot generate
   a further SUBREG.  In this case, we expect forcing the value into a
   register will work around the situation.  */

static rtx
lowpart_subreg_maybe_copy (machine_mode omode, rtx val,
			   machine_mode imode)
{
  rtx ret;
  ret = lowpart_subreg (omode, val, imode);
  if (ret == NULL)
    {
      val = force_reg (imode, val);
      ret = lowpart_subreg (omode, val, imode);
      gcc_assert (ret != NULL);
    }
  return ret;
}

/* Expand a floating point absolute value or negation operation via a
   logical operation on the sign bit.  */

static rtx
expand_absneg_bit (enum rtx_code code, scalar_float_mode mode,
		   rtx op0, rtx target)
{
  const struct real_format *fmt;
  int bitpos, word, nwords, i;
  scalar_int_mode imode;
  rtx temp;
  rtx_insn *insns;

  /* The format has to have a simple sign bit.  */
  fmt = REAL_MODE_FORMAT (mode);
  if (fmt == NULL)
    return NULL_RTX;

  bitpos = fmt->signbit_rw;
  if (bitpos < 0)
    return NULL_RTX;

  /* Don't create negative zeros if the format doesn't support them.  */
  if (code == NEG && !fmt->has_signed_zero)
    return NULL_RTX;

  if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
    {
      if (!int_mode_for_mode (mode).exists (&imode))
	return NULL_RTX;
      word = 0;
      nwords = 1;
    }
  else
    {
      imode = word_mode;

      if (FLOAT_WORDS_BIG_ENDIAN)
	word = (GET_MODE_BITSIZE (mode) - bitpos) / BITS_PER_WORD;
      else
	word = bitpos / BITS_PER_WORD;
      bitpos = bitpos % BITS_PER_WORD;
      nwords = (GET_MODE_BITSIZE (mode) + BITS_PER_WORD - 1) / BITS_PER_WORD;
    }

  wide_int mask = wi::set_bit_in_zero (bitpos, GET_MODE_PRECISION (imode));
  if (code == ABS)
    mask = ~mask;

  if (target == 0
      || target == op0
      || (nwords > 1 && !valid_multiword_target_p (target)))
    target = gen_reg_rtx (mode);

  if (nwords > 1)
    {
      start_sequence ();

      for (i = 0; i < nwords; ++i)
	{
	  rtx targ_piece = operand_subword (target, i, 1, mode);
	  rtx op0_piece = operand_subword_force (op0, i, mode);

	  if (i == word)
	    {
	      temp = expand_binop (imode, code == ABS ? and_optab : xor_optab,
				   op0_piece,
				   immed_wide_int_const (mask, imode),
				   targ_piece, 1, OPTAB_LIB_WIDEN);
	      if (temp != targ_piece)
		emit_move_insn (targ_piece, temp);
	    }
	  else
	    emit_move_insn (targ_piece, op0_piece);
	}

      insns = get_insns ();
      end_sequence ();

      emit_insn (insns);
    }
  else
    {
      temp = expand_binop (imode, code == ABS ? and_optab : xor_optab,
			   gen_lowpart (imode, op0),
			   immed_wide_int_const (mask, imode),
		           gen_lowpart (imode, target), 1, OPTAB_LIB_WIDEN);
      target = lowpart_subreg_maybe_copy (mode, temp, imode);

      set_dst_reg_note (get_last_insn (), REG_EQUAL,
			gen_rtx_fmt_e (code, mode, copy_rtx (op0)),
			target);
    }

  return target;
}

/* As expand_unop, but will fail rather than attempt the operation in a
   different mode or with a libcall.  */
static rtx
expand_unop_direct (machine_mode mode, optab unoptab, rtx op0, rtx target,
		    int unsignedp)
{
  if (optab_handler (unoptab, mode) != CODE_FOR_nothing)
    {
      struct expand_operand ops[2];
      enum insn_code icode = optab_handler (unoptab, mode);
      rtx_insn *last = get_last_insn ();
      rtx_insn *pat;

      create_output_operand (&ops[0], target, mode);
      create_convert_operand_from (&ops[1], op0, mode, unsignedp);
      pat = maybe_gen_insn (icode, 2, ops);
      if (pat)
	{
	  if (INSN_P (pat) && NEXT_INSN (pat) != NULL_RTX
	      && ! add_equal_note (pat, ops[0].value,
				   optab_to_code (unoptab),
				   ops[1].value, NULL_RTX))
	    {
	      delete_insns_since (last);
	      return expand_unop (mode, unoptab, op0, NULL_RTX, unsignedp);
	    }

	  emit_insn (pat);

	  return ops[0].value;
	}
    }
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
expand_unop (machine_mode mode, optab unoptab, rtx op0, rtx target,
	     int unsignedp)
{
  enum mode_class mclass = GET_MODE_CLASS (mode);
  machine_mode wider_mode;
  scalar_int_mode int_mode;
  scalar_float_mode float_mode;
  rtx temp;
  rtx libfunc;

  temp = expand_unop_direct (mode, unoptab, op0, target, unsignedp);
  if (temp)
    return temp;

  /* It can't be done in this mode.  Can we open-code it in a wider mode?  */

  /* Widening (or narrowing) clz needs special treatment.  */
  if (unoptab == clz_optab)
    {
      if (is_a <scalar_int_mode> (mode, &int_mode))
	{
	  temp = widen_leading (int_mode, op0, target, unoptab);
	  if (temp)
	    return temp;

	  if (GET_MODE_SIZE (int_mode) == 2 * UNITS_PER_WORD
	      && optab_handler (unoptab, word_mode) != CODE_FOR_nothing)
	    {
	      temp = expand_doubleword_clz (int_mode, op0, target);
	      if (temp)
		return temp;
	    }
	}

      goto try_libcall;
    }

  if (unoptab == clrsb_optab)
    {
      if (is_a <scalar_int_mode> (mode, &int_mode))
	{
	  temp = widen_leading (int_mode, op0, target, unoptab);
	  if (temp)
	    return temp;
	}
      goto try_libcall;
    }

  if (unoptab == popcount_optab
      && is_a <scalar_int_mode> (mode, &int_mode)
      && GET_MODE_SIZE (int_mode) == 2 * UNITS_PER_WORD
      && optab_handler (unoptab, word_mode) != CODE_FOR_nothing
      && optimize_insn_for_speed_p ())
    {
      temp = expand_doubleword_popcount (int_mode, op0, target);
      if (temp)
	return temp;
    }

  if (unoptab == parity_optab
      && is_a <scalar_int_mode> (mode, &int_mode)
      && GET_MODE_SIZE (int_mode) == 2 * UNITS_PER_WORD
      && (optab_handler (unoptab, word_mode) != CODE_FOR_nothing
	  || optab_handler (popcount_optab, word_mode) != CODE_FOR_nothing)
      && optimize_insn_for_speed_p ())
    {
      temp = expand_doubleword_parity (int_mode, op0, target);
      if (temp)
	return temp;
    }

  /* Widening (or narrowing) bswap needs special treatment.  */
  if (unoptab == bswap_optab)
    {
      /* HImode is special because in this mode BSWAP is equivalent to ROTATE
	 or ROTATERT.  First try these directly; if this fails, then try the
	 obvious pair of shifts with allowed widening, as this will probably
	 be always more efficient than the other fallback methods.  */
      if (mode == HImode)
	{
	  rtx_insn *last;
	  rtx temp1, temp2;

	  if (optab_handler (rotl_optab, mode) != CODE_FOR_nothing)
	    {
	      temp = expand_binop (mode, rotl_optab, op0, GEN_INT (8), target,
				   unsignedp, OPTAB_DIRECT);
	      if (temp)
		return temp;
	     }

	  if (optab_handler (rotr_optab, mode) != CODE_FOR_nothing)
	    {
	      temp = expand_binop (mode, rotr_optab, op0, GEN_INT (8), target,
				   unsignedp, OPTAB_DIRECT);
	      if (temp)
		return temp;
	    }

	  last = get_last_insn ();

	  temp1 = expand_binop (mode, ashl_optab, op0, GEN_INT (8), NULL_RTX,
			        unsignedp, OPTAB_WIDEN);
	  temp2 = expand_binop (mode, lshr_optab, op0, GEN_INT (8), NULL_RTX,
			        unsignedp, OPTAB_WIDEN);
	  if (temp1 && temp2)
	    {
	      temp = expand_binop (mode, ior_optab, temp1, temp2, target,
				   unsignedp, OPTAB_WIDEN);
	      if (temp)
		return temp;
	    }

	  delete_insns_since (last);
	}

      if (is_a <scalar_int_mode> (mode, &int_mode))
	{
	  temp = widen_bswap (int_mode, op0, target);
	  if (temp)
	    return temp;

	  if (GET_MODE_SIZE (int_mode) == 2 * UNITS_PER_WORD
	      && optab_handler (unoptab, word_mode) != CODE_FOR_nothing)
	    {
	      temp = expand_doubleword_bswap (mode, op0, target);
	      if (temp)
		return temp;
	    }
	}

      goto try_libcall;
    }

  if (CLASS_HAS_WIDER_MODES_P (mclass))
    FOR_EACH_WIDER_MODE (wider_mode, mode)
      {
	if (optab_handler (unoptab, wider_mode) != CODE_FOR_nothing)
	  {
	    rtx xop0 = op0;
	    rtx_insn *last = get_last_insn ();

	    /* For certain operations, we need not actually extend
	       the narrow operand, as long as we will truncate the
	       results to the same narrowness.  */

	    xop0 = widen_operand (xop0, wider_mode, mode, unsignedp,
				  (unoptab == neg_optab
				   || unoptab == one_cmpl_optab)
				  && mclass == MODE_INT);

	    temp = expand_unop (wider_mode, unoptab, xop0, NULL_RTX,
				unsignedp);

	    if (temp)
	      {
		if (mclass != MODE_INT
		    || !TRULY_NOOP_TRUNCATION_MODES_P (mode, wider_mode))
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
      && is_int_mode (mode, &int_mode)
      && GET_MODE_SIZE (int_mode) > UNITS_PER_WORD
      && optab_handler (unoptab, word_mode) != CODE_FOR_nothing)
    {
      int i;
      rtx_insn *insns;

      if (target == 0 || target == op0 || !valid_multiword_target_p (target))
	target = gen_reg_rtx (int_mode);

      start_sequence ();

      /* Do the actual arithmetic.  */
      for (i = 0; i < GET_MODE_BITSIZE (int_mode) / BITS_PER_WORD; i++)
	{
	  rtx target_piece = operand_subword (target, i, 1, int_mode);
	  rtx x = expand_unop (word_mode, unoptab,
			       operand_subword_force (op0, i, int_mode),
			       target_piece, unsignedp);

	  if (target_piece != x)
	    emit_move_insn (target_piece, x);
	}

      insns = get_insns ();
      end_sequence ();

      emit_insn (insns);
      return target;
    }

  if (optab_to_code (unoptab) == NEG)
    {
      /* Try negating floating point values by flipping the sign bit.  */
      if (is_a <scalar_float_mode> (mode, &float_mode))
	{
	  temp = expand_absneg_bit (NEG, float_mode, op0, target);
	  if (temp)
	    return temp;
	}

      /* If there is no negation pattern, and we have no negative zero,
	 try subtracting from zero.  */
      if (!HONOR_SIGNED_ZEROS (mode))
	{
	  temp = expand_binop (mode, (unoptab == negv_optab
				      ? subv_optab : sub_optab),
			       CONST0_RTX (mode), op0, target,
			       unsignedp, OPTAB_DIRECT);
	  if (temp)
	    return temp;
	}
    }

  /* Try calculating parity (x) as popcount (x) % 2.  */
  if (unoptab == parity_optab && is_a <scalar_int_mode> (mode, &int_mode))
    {
      temp = expand_parity (int_mode, op0, target);
      if (temp)
	return temp;
    }

  /* Try implementing ffs (x) in terms of clz (x).  */
  if (unoptab == ffs_optab && is_a <scalar_int_mode> (mode, &int_mode))
    {
      temp = expand_ffs (int_mode, op0, target);
      if (temp)
	return temp;
    }

  /* Try implementing ctz (x) in terms of clz (x).  */
  if (unoptab == ctz_optab && is_a <scalar_int_mode> (mode, &int_mode))
    {
      temp = expand_ctz (int_mode, op0, target);
      if (temp)
	return temp;
    }

 try_libcall:
  /* Now try a library call in this mode.  */
  libfunc = optab_libfunc (unoptab, mode);
  if (libfunc)
    {
      rtx_insn *insns;
      rtx value;
      rtx eq_value;
      machine_mode outmode = mode;

      /* All of these functions return small values.  Thus we choose to
	 have them return something that isn't a double-word.  */
      if (unoptab == ffs_optab || unoptab == clz_optab || unoptab == ctz_optab
	  || unoptab == clrsb_optab || unoptab == popcount_optab
	  || unoptab == parity_optab)
	outmode
	  = GET_MODE (hard_libcall_value (TYPE_MODE (integer_type_node),
					  optab_libfunc (unoptab, mode)));

      start_sequence ();

      /* Pass 1 for NO_QUEUE so we don't lose any increments
	 if the libcall is cse'd or moved.  */
      value = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST, outmode,
				       1, op0, mode);
      insns = get_insns ();
      end_sequence ();

      target = gen_reg_rtx (outmode);
      bool trapv = trapv_unoptab_p (unoptab);
      if (trapv)
	eq_value = NULL_RTX;
      else
	{
	  eq_value = gen_rtx_fmt_e (optab_to_code (unoptab), mode, op0);
	  if (GET_MODE_SIZE (outmode) < GET_MODE_SIZE (mode))
	    eq_value = simplify_gen_unary (TRUNCATE, outmode, eq_value, mode);
	  else if (GET_MODE_SIZE (outmode) > GET_MODE_SIZE (mode))
	    eq_value = simplify_gen_unary (ZERO_EXTEND,
					   outmode, eq_value, mode);
	}
      emit_libcall_block_1 (insns, target, value, eq_value, trapv);

      return target;
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if (CLASS_HAS_WIDER_MODES_P (mclass))
    {
      FOR_EACH_WIDER_MODE (wider_mode, mode)
	{
	  if (optab_handler (unoptab, wider_mode) != CODE_FOR_nothing
	      || optab_libfunc (unoptab, wider_mode))
	    {
	      rtx xop0 = op0;
	      rtx_insn *last = get_last_insn ();

	      /* For certain operations, we need not actually extend
		 the narrow operand, as long as we will truncate the
		 results to the same narrowness.  */
	      xop0 = widen_operand (xop0, wider_mode, mode, unsignedp,
				    (unoptab == neg_optab
				     || unoptab == one_cmpl_optab
				     || unoptab == bswap_optab)
				    && mclass == MODE_INT);

	      temp = expand_unop (wider_mode, unoptab, xop0, NULL_RTX,
				  unsignedp);

	      /* If we are generating clz using wider mode, adjust the
		 result.  Similarly for clrsb.  */
	      if ((unoptab == clz_optab || unoptab == clrsb_optab)
		  && temp != 0)
		{
		  scalar_int_mode wider_int_mode
		    = as_a <scalar_int_mode> (wider_mode);
		  int_mode = as_a <scalar_int_mode> (mode);
		  temp = expand_binop
		    (wider_mode, sub_optab, temp,
		     gen_int_mode (GET_MODE_PRECISION (wider_int_mode)
				   - GET_MODE_PRECISION (int_mode),
				   wider_int_mode),
		     target, true, OPTAB_DIRECT);
		}

	      /* Likewise for bswap.  */
	      if (unoptab == bswap_optab && temp != 0)
		{
		  scalar_int_mode wider_int_mode
		    = as_a <scalar_int_mode> (wider_mode);
		  int_mode = as_a <scalar_int_mode> (mode);
		  gcc_assert (GET_MODE_PRECISION (wider_int_mode)
			      == GET_MODE_BITSIZE (wider_int_mode)
			      && GET_MODE_PRECISION (int_mode)
				 == GET_MODE_BITSIZE (int_mode));

		  temp = expand_shift (RSHIFT_EXPR, wider_int_mode, temp,
				       GET_MODE_BITSIZE (wider_int_mode)
				       - GET_MODE_BITSIZE (int_mode),
				       NULL_RTX, true);
		}

	      if (temp)
		{
		  if (mclass != MODE_INT)
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

  /* One final attempt at implementing negation via subtraction,
     this time allowing widening of the operand.  */
  if (optab_to_code (unoptab) == NEG && !HONOR_SIGNED_ZEROS (mode))
    {
      rtx temp;
      temp = expand_binop (mode,
                           unoptab == negv_optab ? subv_optab : sub_optab,
                           CONST0_RTX (mode), op0,
                           target, unsignedp, OPTAB_LIB_WIDEN);
      if (temp)
        return temp;
    }

  return 0;
}

/* Emit code to compute the absolute value of OP0, with result to
   TARGET if convenient.  (TARGET may be 0.)  The return value says
   where the result actually is to be found.

   MODE is the mode of the operand; the mode of the result is
   different but can be deduced from MODE.

 */

rtx
expand_abs_nojump (machine_mode mode, rtx op0, rtx target,
		   int result_unsignedp)
{
  rtx temp;

  if (GET_MODE_CLASS (mode) != MODE_INT
      || ! flag_trapv)
    result_unsignedp = 1;

  /* First try to do it with a special abs instruction.  */
  temp = expand_unop (mode, result_unsignedp ? abs_optab : absv_optab,
                      op0, target, 0);
  if (temp != 0)
    return temp;

  /* For floating point modes, try clearing the sign bit.  */
  scalar_float_mode float_mode;
  if (is_a <scalar_float_mode> (mode, &float_mode))
    {
      temp = expand_absneg_bit (ABS, float_mode, op0, target);
      if (temp)
	return temp;
    }

  /* If we have a MAX insn, we can do this as MAX (x, -x).  */
  if (optab_handler (smax_optab, mode) != CODE_FOR_nothing
      && !HONOR_SIGNED_ZEROS (mode))
    {
      rtx_insn *last = get_last_insn ();

      temp = expand_unop (mode, result_unsignedp ? neg_optab : negv_optab,
			  op0, NULL_RTX, 0);
      if (temp != 0)
	temp = expand_binop (mode, smax_optab, op0, temp, target, 0,
			     OPTAB_WIDEN);

      if (temp != 0)
	return temp;

      delete_insns_since (last);
    }

  /* If this machine has expensive jumps, we can do integer absolute
     value of X as (((signed) x >> (W-1)) ^ x) - ((signed) x >> (W-1)),
     where W is the width of MODE.  */

  scalar_int_mode int_mode;
  if (is_int_mode (mode, &int_mode)
      && BRANCH_COST (optimize_insn_for_speed_p (),
	      	      false) >= 2)
    {
      rtx extended = expand_shift (RSHIFT_EXPR, int_mode, op0,
				   GET_MODE_PRECISION (int_mode) - 1,
				   NULL_RTX, 0);

      temp = expand_binop (int_mode, xor_optab, extended, op0, target, 0,
			   OPTAB_LIB_WIDEN);
      if (temp != 0)
	temp = expand_binop (int_mode,
			     result_unsignedp ? sub_optab : subv_optab,
                             temp, extended, target, 0, OPTAB_LIB_WIDEN);

      if (temp != 0)
	return temp;
    }

  return NULL_RTX;
}

rtx
expand_abs (machine_mode mode, rtx op0, rtx target,
	    int result_unsignedp, int safe)
{
  rtx temp;
  rtx_code_label *op1;

  if (GET_MODE_CLASS (mode) != MODE_INT
      || ! flag_trapv)
    result_unsignedp = 1;

  temp = expand_abs_nojump (mode, op0, target, result_unsignedp);
  if (temp != 0)
    return temp;

  /* If that does not win, use conditional jump and negate.  */

  /* It is safe to use the target if it is the same
     as the source if this is also a pseudo register */
  if (op0 == target && REG_P (op0)
      && REGNO (op0) >= FIRST_PSEUDO_REGISTER)
    safe = 1;

  op1 = gen_label_rtx ();
  if (target == 0 || ! safe
      || GET_MODE (target) != mode
      || (MEM_P (target) && MEM_VOLATILE_P (target))
      || (REG_P (target)
	  && REGNO (target) < FIRST_PSEUDO_REGISTER))
    target = gen_reg_rtx (mode);

  emit_move_insn (target, op0);
  NO_DEFER_POP;

  do_compare_rtx_and_jump (target, CONST0_RTX (mode), GE, 0, mode,
			   NULL_RTX, NULL, op1,
			   profile_probability::uninitialized ());

  op0 = expand_unop (mode, result_unsignedp ? neg_optab : negv_optab,
                     target, target, 0);
  if (op0 != target)
    emit_move_insn (target, op0);
  emit_label (op1);
  OK_DEFER_POP;
  return target;
}

/* Emit code to compute the one's complement absolute value of OP0
   (if (OP0 < 0) OP0 = ~OP0), with result to TARGET if convenient.
   (TARGET may be NULL_RTX.)  The return value says where the result
   actually is to be found.

   MODE is the mode of the operand; the mode of the result is
   different but can be deduced from MODE.  */

rtx
expand_one_cmpl_abs_nojump (machine_mode mode, rtx op0, rtx target)
{
  rtx temp;

  /* Not applicable for floating point modes.  */
  if (FLOAT_MODE_P (mode))
    return NULL_RTX;

  /* If we have a MAX insn, we can do this as MAX (x, ~x).  */
  if (optab_handler (smax_optab, mode) != CODE_FOR_nothing)
    {
      rtx_insn *last = get_last_insn ();

      temp = expand_unop (mode, one_cmpl_optab, op0, NULL_RTX, 0);
      if (temp != 0)
	temp = expand_binop (mode, smax_optab, op0, temp, target, 0,
			     OPTAB_WIDEN);

      if (temp != 0)
	return temp;

      delete_insns_since (last);
    }

  /* If this machine has expensive jumps, we can do one's complement
     absolute value of X as (((signed) x >> (W-1)) ^ x).  */

  scalar_int_mode int_mode;
  if (is_int_mode (mode, &int_mode)
      && BRANCH_COST (optimize_insn_for_speed_p (),
	             false) >= 2)
    {
      rtx extended = expand_shift (RSHIFT_EXPR, int_mode, op0,
				   GET_MODE_PRECISION (int_mode) - 1,
				   NULL_RTX, 0);

      temp = expand_binop (int_mode, xor_optab, extended, op0, target, 0,
			   OPTAB_LIB_WIDEN);

      if (temp != 0)
	return temp;
    }

  return NULL_RTX;
}

/* A subroutine of expand_copysign, perform the copysign operation using the
   abs and neg primitives advertised to exist on the target.  The assumption
   is that we have a split register file, and leaving op0 in fp registers,
   and not playing with subregs so much, will help the register allocator.  */

static rtx
expand_copysign_absneg (scalar_float_mode mode, rtx op0, rtx op1, rtx target,
		        int bitpos, bool op0_is_abs)
{
  scalar_int_mode imode;
  enum insn_code icode;
  rtx sign;
  rtx_code_label *label;

  if (target == op1)
    target = NULL_RTX;

  /* Check if the back end provides an insn that handles signbit for the
     argument's mode. */
  icode = optab_handler (signbit_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      imode = as_a <scalar_int_mode> (insn_data[(int) icode].operand[0].mode);
      sign = gen_reg_rtx (imode);
      emit_unop_insn (icode, sign, op1, UNKNOWN);
    }
  else
    {
      if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
	{
	  if (!int_mode_for_mode (mode).exists (&imode))
	    return NULL_RTX;
	  op1 = gen_lowpart (imode, op1);
	}
      else
	{
	  int word;

	  imode = word_mode;
	  if (FLOAT_WORDS_BIG_ENDIAN)
	    word = (GET_MODE_BITSIZE (mode) - bitpos) / BITS_PER_WORD;
	  else
	    word = bitpos / BITS_PER_WORD;
	  bitpos = bitpos % BITS_PER_WORD;
	  op1 = operand_subword_force (op1, word, mode);
	}

      wide_int mask = wi::set_bit_in_zero (bitpos, GET_MODE_PRECISION (imode));
      sign = expand_binop (imode, and_optab, op1,
			   immed_wide_int_const (mask, imode),
			   NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }

  if (!op0_is_abs)
    {
      op0 = expand_unop (mode, abs_optab, op0, target, 0);
      if (op0 == NULL)
	return NULL_RTX;
      target = op0;
    }
  else
    {
      if (target == NULL_RTX)
        target = copy_to_reg (op0);
      else
	emit_move_insn (target, op0);
    }

  label = gen_label_rtx ();
  emit_cmp_and_jump_insns (sign, const0_rtx, EQ, NULL_RTX, imode, 1, label);

  if (CONST_DOUBLE_AS_FLOAT_P (op0))
    op0 = simplify_unary_operation (NEG, mode, op0, mode);
  else
    op0 = expand_unop (mode, neg_optab, op0, target, 0);
  if (op0 != target)
    emit_move_insn (target, op0);

  emit_label (label);

  return target;
}


/* A subroutine of expand_copysign, perform the entire copysign operation
   with integer bitmasks.  BITPOS is the position of the sign bit; OP0_IS_ABS
   is true if op0 is known to have its sign bit clear.  */

static rtx
expand_copysign_bit (scalar_float_mode mode, rtx op0, rtx op1, rtx target,
		     int bitpos, bool op0_is_abs)
{
  scalar_int_mode imode;
  int word, nwords, i;
  rtx temp;
  rtx_insn *insns;

  if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
    {
      if (!int_mode_for_mode (mode).exists (&imode))
	return NULL_RTX;
      word = 0;
      nwords = 1;
    }
  else
    {
      imode = word_mode;

      if (FLOAT_WORDS_BIG_ENDIAN)
	word = (GET_MODE_BITSIZE (mode) - bitpos) / BITS_PER_WORD;
      else
	word = bitpos / BITS_PER_WORD;
      bitpos = bitpos % BITS_PER_WORD;
      nwords = (GET_MODE_BITSIZE (mode) + BITS_PER_WORD - 1) / BITS_PER_WORD;
    }

  wide_int mask = wi::set_bit_in_zero (bitpos, GET_MODE_PRECISION (imode));

  if (target == 0
      || target == op0
      || target == op1
      || (nwords > 1 && !valid_multiword_target_p (target)))
    target = gen_reg_rtx (mode);

  if (nwords > 1)
    {
      start_sequence ();

      for (i = 0; i < nwords; ++i)
	{
	  rtx targ_piece = operand_subword (target, i, 1, mode);
	  rtx op0_piece = operand_subword_force (op0, i, mode);

	  if (i == word)
	    {
	      if (!op0_is_abs)
		op0_piece
		  = expand_binop (imode, and_optab, op0_piece,
				  immed_wide_int_const (~mask, imode),
				  NULL_RTX, 1, OPTAB_LIB_WIDEN);
	      op1 = expand_binop (imode, and_optab,
				  operand_subword_force (op1, i, mode),
				  immed_wide_int_const (mask, imode),
				  NULL_RTX, 1, OPTAB_LIB_WIDEN);

	      temp = expand_binop (imode, ior_optab, op0_piece, op1,
				   targ_piece, 1, OPTAB_LIB_WIDEN);
	      if (temp != targ_piece)
		emit_move_insn (targ_piece, temp);
	    }
	  else
	    emit_move_insn (targ_piece, op0_piece);
	}

      insns = get_insns ();
      end_sequence ();

      emit_insn (insns);
    }
  else
    {
      op1 = expand_binop (imode, and_optab, gen_lowpart (imode, op1),
		          immed_wide_int_const (mask, imode),
		          NULL_RTX, 1, OPTAB_LIB_WIDEN);

      op0 = gen_lowpart (imode, op0);
      if (!op0_is_abs)
	op0 = expand_binop (imode, and_optab, op0,
			    immed_wide_int_const (~mask, imode),
			    NULL_RTX, 1, OPTAB_LIB_WIDEN);

      temp = expand_binop (imode, ior_optab, op0, op1,
			   gen_lowpart (imode, target), 1, OPTAB_LIB_WIDEN);
      target = lowpart_subreg_maybe_copy (mode, temp, imode);
    }

  return target;
}

/* Expand the C99 copysign operation.  OP0 and OP1 must be the same
   scalar floating point mode.  Return NULL if we do not know how to
   expand the operation inline.  */

rtx
expand_copysign (rtx op0, rtx op1, rtx target)
{
  scalar_float_mode mode;
  const struct real_format *fmt;
  bool op0_is_abs;
  rtx temp;

  mode = as_a <scalar_float_mode> (GET_MODE (op0));
  gcc_assert (GET_MODE (op1) == mode);

  /* First try to do it with a special instruction.  */
  temp = expand_binop (mode, copysign_optab, op0, op1,
		       target, 0, OPTAB_DIRECT);
  if (temp)
    return temp;

  fmt = REAL_MODE_FORMAT (mode);
  if (fmt == NULL || !fmt->has_signed_zero)
    return NULL_RTX;

  op0_is_abs = false;
  if (CONST_DOUBLE_AS_FLOAT_P (op0))
    {
      if (real_isneg (CONST_DOUBLE_REAL_VALUE (op0)))
	op0 = simplify_unary_operation (ABS, mode, op0, mode);
      op0_is_abs = true;
    }

  if (fmt->signbit_ro >= 0
      && (CONST_DOUBLE_AS_FLOAT_P (op0) 
	  || (optab_handler (neg_optab, mode) != CODE_FOR_nothing
	      && optab_handler (abs_optab, mode) != CODE_FOR_nothing)))
    {
      temp = expand_copysign_absneg (mode, op0, op1, target,
				     fmt->signbit_ro, op0_is_abs);
      if (temp)
	return temp;
    }

  if (fmt->signbit_rw < 0)
    return NULL_RTX;
  return expand_copysign_bit (mode, op0, op1, target,
			      fmt->signbit_rw, op0_is_abs);
}

/* Generate an instruction whose insn-code is INSN_CODE,
   with two operands: an output TARGET and an input OP0.
   TARGET *must* be nonzero, and the output is always stored there.
   CODE is an rtx code such that (CODE OP0) is an rtx that describes
   the value that is stored into TARGET.

   Return false if expansion failed.  */

bool
maybe_emit_unop_insn (enum insn_code icode, rtx target, rtx op0,
		      enum rtx_code code)
{
  struct expand_operand ops[2];
  rtx_insn *pat;

  create_output_operand (&ops[0], target, GET_MODE (target));
  create_input_operand (&ops[1], op0, GET_MODE (op0));
  pat = maybe_gen_insn (icode, 2, ops);
  if (!pat)
    return false;

  if (INSN_P (pat) && NEXT_INSN (pat) != NULL_RTX
      && code != UNKNOWN)
    add_equal_note (pat, ops[0].value, code, ops[1].value, NULL_RTX);

  emit_insn (pat);

  if (ops[0].value != target)
    emit_move_insn (target, ops[0].value);
  return true;
}
/* Generate an instruction whose insn-code is INSN_CODE,
   with two operands: an output TARGET and an input OP0.
   TARGET *must* be nonzero, and the output is always stored there.
   CODE is an rtx code such that (CODE OP0) is an rtx that describes
   the value that is stored into TARGET.  */

void
emit_unop_insn (enum insn_code icode, rtx target, rtx op0, enum rtx_code code)
{
  bool ok = maybe_emit_unop_insn (icode, target, op0, code);
  gcc_assert (ok);
}

struct no_conflict_data
{
  rtx target;
  rtx_insn *first, *insn;
  bool must_stay;
};

/* Called via note_stores by emit_libcall_block.  Set P->must_stay if
   the currently examined clobber / store has to stay in the list of
   insns that constitute the actual libcall block.  */
static void
no_conflict_move_test (rtx dest, const_rtx set, void *p0)
{
  struct no_conflict_data *p= (struct no_conflict_data *) p0;

  /* If this inns directly contributes to setting the target, it must stay.  */
  if (reg_overlap_mentioned_p (p->target, dest))
    p->must_stay = true;
  /* If we haven't committed to keeping any other insns in the list yet,
     there is nothing more to check.  */
  else if (p->insn == p->first)
    return;
  /* If this insn sets / clobbers a register that feeds one of the insns
     already in the list, this insn has to stay too.  */
  else if (reg_overlap_mentioned_p (dest, PATTERN (p->first))
	   || (CALL_P (p->first) && (find_reg_fusage (p->first, USE, dest)))
	   || reg_used_between_p (dest, p->first, p->insn)
	   /* Likewise if this insn depends on a register set by a previous
	      insn in the list, or if it sets a result (presumably a hard
	      register) that is set or clobbered by a previous insn.
	      N.B. the modified_*_p (SET_DEST...) tests applied to a MEM
	      SET_DEST perform the former check on the address, and the latter
	      check on the MEM.  */
	   || (GET_CODE (set) == SET
	       && (modified_in_p (SET_SRC (set), p->first)
		   || modified_in_p (SET_DEST (set), p->first)
		   || modified_between_p (SET_SRC (set), p->first, p->insn)
		   || modified_between_p (SET_DEST (set), p->first, p->insn))))
    p->must_stay = true;
}


/* Emit code to make a call to a constant function or a library call.

   INSNS is a list containing all insns emitted in the call.
   These insns leave the result in RESULT.  Our block is to copy RESULT
   to TARGET, which is logically equivalent to EQUIV.

   We first emit any insns that set a pseudo on the assumption that these are
   loading constants into registers; doing so allows them to be safely cse'ed
   between blocks.  Then we emit all the other insns in the block, followed by
   an insn to move RESULT to TARGET.  This last insn will have a REQ_EQUAL
   note with an operand of EQUIV.  */

static void
emit_libcall_block_1 (rtx_insn *insns, rtx target, rtx result, rtx equiv,
		      bool equiv_may_trap)
{
  rtx final_dest = target;
  rtx_insn *next, *last, *insn;

  /* If this is a reg with REG_USERVAR_P set, then it could possibly turn
     into a MEM later.  Protect the libcall block from this change.  */
  if (! REG_P (target) || REG_USERVAR_P (target))
    target = gen_reg_rtx (GET_MODE (target));

  /* If we're using non-call exceptions, a libcall corresponding to an
     operation that may trap may also trap.  */
  /* ??? See the comment in front of make_reg_eh_region_note.  */
  if (cfun->can_throw_non_call_exceptions
      && (equiv_may_trap || may_trap_p (equiv)))
    {
      for (insn = insns; insn; insn = NEXT_INSN (insn))
	if (CALL_P (insn))
	  {
	    rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
	    if (note)
	      {
		int lp_nr = INTVAL (XEXP (note, 0));
		if (lp_nr == 0 || lp_nr == INT_MIN)
		  remove_note (insn, note);
	      }
	  }
    }
  else
    {
      /* Look for any CALL_INSNs in this sequence, and attach a REG_EH_REGION
	 reg note to indicate that this call cannot throw or execute a nonlocal
	 goto (unless there is already a REG_EH_REGION note, in which case
	 we update it).  */
      for (insn = insns; insn; insn = NEXT_INSN (insn))
	if (CALL_P (insn))
	  make_reg_eh_region_note_nothrow_nononlocal (insn);
    }

  /* First emit all insns that set pseudos.  Remove them from the list as
     we go.  Avoid insns that set pseudos which were referenced in previous
     insns.  These can be generated by move_by_pieces, for example,
     to update an address.  Similarly, avoid insns that reference things
     set in previous insns.  */

  for (insn = insns; insn; insn = next)
    {
      rtx set = single_set (insn);

      next = NEXT_INSN (insn);

      if (set != 0 && REG_P (SET_DEST (set))
	  && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER)
	{
	  struct no_conflict_data data;

	  data.target = const0_rtx;
	  data.first = insns;
	  data.insn = insn;
	  data.must_stay = 0;
	  note_stores (PATTERN (insn), no_conflict_move_test, &data);
	  if (! data.must_stay)
	    {
	      if (PREV_INSN (insn))
		SET_NEXT_INSN (PREV_INSN (insn)) = next;
	      else
		insns = next;

	      if (next)
		SET_PREV_INSN (next) = PREV_INSN (insn);

	      add_insn (insn);
	    }
	}

      /* Some ports use a loop to copy large arguments onto the stack.
	 Don't move anything outside such a loop.  */
      if (LABEL_P (insn))
	break;
    }

  /* Write the remaining insns followed by the final copy.  */
  for (insn = insns; insn; insn = next)
    {
      next = NEXT_INSN (insn);

      add_insn (insn);
    }

  last = emit_move_insn (target, result);
  if (equiv)
    set_dst_reg_note (last, REG_EQUAL, copy_rtx (equiv), target);

  if (final_dest != target)
    emit_move_insn (final_dest, target);
}

void
emit_libcall_block (rtx_insn *insns, rtx target, rtx result, rtx equiv)
{
  emit_libcall_block_1 (insns, target, result, equiv, false);
}

/* Nonzero if we can perform a comparison of mode MODE straightforwardly.
   PURPOSE describes how this comparison will be used.  CODE is the rtx
   comparison code we will be using.

   ??? Actually, CODE is slightly weaker than that.  A target is still
   required to implement all of the normal bcc operations, but not
   required to implement all (or any) of the unordered bcc operations.  */

int
can_compare_p (enum rtx_code code, machine_mode mode,
	       enum can_compare_purpose purpose)
{
  rtx test;
  test = gen_rtx_fmt_ee (code, mode, const0_rtx, const0_rtx);
  do
    {
      enum insn_code icode;

      if (purpose == ccp_jump
          && (icode = optab_handler (cbranch_optab, mode)) != CODE_FOR_nothing
          && insn_operand_matches (icode, 0, test))
        return 1;
      if (purpose == ccp_store_flag
          && (icode = optab_handler (cstore_optab, mode)) != CODE_FOR_nothing
          && insn_operand_matches (icode, 1, test))
        return 1;
      if (purpose == ccp_cmov
	  && optab_handler (cmov_optab, mode) != CODE_FOR_nothing)
	return 1;

      mode = GET_MODE_WIDER_MODE (mode).else_void ();
      PUT_MODE (test, mode);
    }
  while (mode != VOIDmode);

  return 0;
}

/* This function is called when we are going to emit a compare instruction that
   compares the values found in X and Y, using the rtl operator COMPARISON.

   If they have mode BLKmode, then SIZE specifies the size of both operands.

   UNSIGNEDP nonzero says that the operands are unsigned;
   this matters if they need to be widened (as given by METHODS).

   *PTEST is where the resulting comparison RTX is returned or NULL_RTX
   if we failed to produce one.

   *PMODE is the mode of the inputs (in case they are const_int).

   This function performs all the setup necessary so that the caller only has
   to emit a single comparison insn.  This setup can involve doing a BLKmode
   comparison or emitting a library call to perform the comparison if no insn
   is available to handle it.
   The values which are passed in through pointers can be modified; the caller
   should perform the comparison on the modified values.  Constant
   comparisons must have already been folded.  */

static void
prepare_cmp_insn (rtx x, rtx y, enum rtx_code comparison, rtx size,
		  int unsignedp, enum optab_methods methods,
		  rtx *ptest, machine_mode *pmode)
{
  machine_mode mode = *pmode;
  rtx libfunc, test;
  machine_mode cmp_mode;
  enum mode_class mclass;

  /* The other methods are not needed.  */
  gcc_assert (methods == OPTAB_DIRECT || methods == OPTAB_WIDEN
	      || methods == OPTAB_LIB_WIDEN);

  /* If we are optimizing, force expensive constants into a register.  */
  if (CONSTANT_P (x) && optimize
      && (rtx_cost (x, mode, COMPARE, 0, optimize_insn_for_speed_p ())
          > COSTS_N_INSNS (1)))
    x = force_reg (mode, x);

  if (CONSTANT_P (y) && optimize
      && (rtx_cost (y, mode, COMPARE, 1, optimize_insn_for_speed_p ())
          > COSTS_N_INSNS (1)))
    y = force_reg (mode, y);

#if HAVE_cc0
  /* Make sure if we have a canonical comparison.  The RTL
     documentation states that canonical comparisons are required only
     for targets which have cc0.  */
  gcc_assert (!CONSTANT_P (x) || CONSTANT_P (y));
#endif

  /* Don't let both operands fail to indicate the mode.  */
  if (GET_MODE (x) == VOIDmode && GET_MODE (y) == VOIDmode)
    x = force_reg (mode, x);
  if (mode == VOIDmode)
    mode = GET_MODE (x) != VOIDmode ? GET_MODE (x) : GET_MODE (y);

  /* Handle all BLKmode compares.  */

  if (mode == BLKmode)
    {
      machine_mode result_mode;
      enum insn_code cmp_code;
      rtx result;
      rtx opalign
	= GEN_INT (MIN (MEM_ALIGN (x), MEM_ALIGN (y)) / BITS_PER_UNIT);

      gcc_assert (size);

      /* Try to use a memory block compare insn - either cmpstr
	 or cmpmem will do.  */
      opt_scalar_int_mode cmp_mode_iter;
      FOR_EACH_MODE_IN_CLASS (cmp_mode_iter, MODE_INT)
	{
	  scalar_int_mode cmp_mode = cmp_mode_iter.require ();
	  cmp_code = direct_optab_handler (cmpmem_optab, cmp_mode);
	  if (cmp_code == CODE_FOR_nothing)
	    cmp_code = direct_optab_handler (cmpstr_optab, cmp_mode);
	  if (cmp_code == CODE_FOR_nothing)
	    cmp_code = direct_optab_handler (cmpstrn_optab, cmp_mode);
	  if (cmp_code == CODE_FOR_nothing)
	    continue;

	  /* Must make sure the size fits the insn's mode.  */
	  if (CONST_INT_P (size)
	      ? INTVAL (size) >= (1 << GET_MODE_BITSIZE (cmp_mode))
	      : (GET_MODE_BITSIZE (as_a <scalar_int_mode> (GET_MODE (size)))
		 > GET_MODE_BITSIZE (cmp_mode)))
	    continue;

	  result_mode = insn_data[cmp_code].operand[0].mode;
	  result = gen_reg_rtx (result_mode);
	  size = convert_to_mode (cmp_mode, size, 1);
	  emit_insn (GEN_FCN (cmp_code) (result, x, y, size, opalign));

          *ptest = gen_rtx_fmt_ee (comparison, VOIDmode, result, const0_rtx);
          *pmode = result_mode;
	  return;
	}

      if (methods != OPTAB_LIB && methods != OPTAB_LIB_WIDEN)
	goto fail;

      /* Otherwise call a library function.  */
      result = emit_block_comp_via_libcall (XEXP (x, 0), XEXP (y, 0), size);

      x = result;
      y = const0_rtx;
      mode = TYPE_MODE (integer_type_node);
      methods = OPTAB_LIB_WIDEN;
      unsignedp = false;
    }

  /* Don't allow operands to the compare to trap, as that can put the
     compare and branch in different basic blocks.  */
  if (cfun->can_throw_non_call_exceptions)
    {
      if (may_trap_p (x))
	x = copy_to_reg (x);
      if (may_trap_p (y))
	y = copy_to_reg (y);
    }

  if (GET_MODE_CLASS (mode) == MODE_CC)
    {
      enum insn_code icode = optab_handler (cbranch_optab, CCmode);
      test = gen_rtx_fmt_ee (comparison, VOIDmode, x, y);
      gcc_assert (icode != CODE_FOR_nothing
                  && insn_operand_matches (icode, 0, test));
      *ptest = test;
      return;
    }

  mclass = GET_MODE_CLASS (mode);
  test = gen_rtx_fmt_ee (comparison, VOIDmode, x, y);
  FOR_EACH_MODE_FROM (cmp_mode, mode)
    {
      enum insn_code icode;
      icode = optab_handler (cbranch_optab, cmp_mode);
      if (icode != CODE_FOR_nothing
	  && insn_operand_matches (icode, 0, test))
	{
	  rtx_insn *last = get_last_insn ();
	  rtx op0 = prepare_operand (icode, x, 1, mode, cmp_mode, unsignedp);
	  rtx op1 = prepare_operand (icode, y, 2, mode, cmp_mode, unsignedp);
	  if (op0 && op1
	      && insn_operand_matches (icode, 1, op0)
	      && insn_operand_matches (icode, 2, op1))
	    {
	      XEXP (test, 0) = op0;
	      XEXP (test, 1) = op1;
	      *ptest = test;
	      *pmode = cmp_mode;
	      return;
	    }
	  delete_insns_since (last);
	}

      if (methods == OPTAB_DIRECT || !CLASS_HAS_WIDER_MODES_P (mclass))
	break;
    }

  if (methods != OPTAB_LIB_WIDEN)
    goto fail;

  if (!SCALAR_FLOAT_MODE_P (mode))
    {
      rtx result;
      machine_mode ret_mode;

      /* Handle a libcall just for the mode we are using.  */
      libfunc = optab_libfunc (cmp_optab, mode);
      gcc_assert (libfunc);

      /* If we want unsigned, and this mode has a distinct unsigned
	 comparison routine, use that.  */
      if (unsignedp)
	{
	  rtx ulibfunc = optab_libfunc (ucmp_optab, mode);
	  if (ulibfunc)
	    libfunc = ulibfunc;
	}

      ret_mode = targetm.libgcc_cmp_return_mode ();
      result = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST,
					ret_mode, 2, x, mode, y, mode);

      /* There are two kinds of comparison routines. Biased routines
	 return 0/1/2, and unbiased routines return -1/0/1. Other parts
	 of gcc expect that the comparison operation is equivalent
	 to the modified comparison. For signed comparisons compare the
	 result against 1 in the biased case, and zero in the unbiased
	 case. For unsigned comparisons always compare against 1 after
	 biasing the unbiased result by adding 1. This gives us a way to
	 represent LTU.
	 The comparisons in the fixed-point helper library are always
	 biased.  */
      x = result;
      y = const1_rtx;

      if (!TARGET_LIB_INT_CMP_BIASED && !ALL_FIXED_POINT_MODE_P (mode))
	{
	  if (unsignedp)
	    x = plus_constant (ret_mode, result, 1);
	  else
	    y = const0_rtx;
	}

      *pmode = ret_mode;
      prepare_cmp_insn (x, y, comparison, NULL_RTX, unsignedp, methods,
			ptest, pmode);
    }
  else
    prepare_float_lib_cmp (x, y, comparison, ptest, pmode);

  return;

 fail:
  *ptest = NULL_RTX;
}

/* Before emitting an insn with code ICODE, make sure that X, which is going
   to be used for operand OPNUM of the insn, is converted from mode MODE to
   WIDER_MODE (UNSIGNEDP determines whether it is an unsigned conversion), and
   that it is accepted by the operand predicate.  Return the new value.  */

rtx
prepare_operand (enum insn_code icode, rtx x, int opnum, machine_mode mode,
		 machine_mode wider_mode, int unsignedp)
{
  if (mode != wider_mode)
    x = convert_modes (wider_mode, mode, x, unsignedp);

  if (!insn_operand_matches (icode, opnum, x))
    {
      machine_mode op_mode = insn_data[(int) icode].operand[opnum].mode;
      if (reload_completed)
	return NULL_RTX;
      if (GET_MODE (x) != op_mode && GET_MODE (x) != VOIDmode)
	return NULL_RTX;
      x = copy_to_mode_reg (op_mode, x);
    }

  return x;
}

/* Subroutine of emit_cmp_and_jump_insns; this function is called when we know
   we can do the branch.  */

static void
emit_cmp_and_jump_insn_1 (rtx test, machine_mode mode, rtx label,
			  profile_probability prob)
{
  machine_mode optab_mode;
  enum mode_class mclass;
  enum insn_code icode;
  rtx_insn *insn;

  mclass = GET_MODE_CLASS (mode);
  optab_mode = (mclass == MODE_CC) ? CCmode : mode;
  icode = optab_handler (cbranch_optab, optab_mode);

  gcc_assert (icode != CODE_FOR_nothing);
  gcc_assert (insn_operand_matches (icode, 0, test));
  insn = emit_jump_insn (GEN_FCN (icode) (test, XEXP (test, 0),
                                          XEXP (test, 1), label));
  if (prob.initialized_p ()
      && profile_status_for_fn (cfun) != PROFILE_ABSENT
      && insn
      && JUMP_P (insn)
      && any_condjump_p (insn)
      && !find_reg_note (insn, REG_BR_PROB, 0))
    add_reg_br_prob_note (insn, prob);
}

/* Generate code to compare X with Y so that the condition codes are
   set and to jump to LABEL if the condition is true.  If X is a
   constant and Y is not a constant, then the comparison is swapped to
   ensure that the comparison RTL has the canonical form.

   UNSIGNEDP nonzero says that X and Y are unsigned; this matters if they
   need to be widened.  UNSIGNEDP is also used to select the proper
   branch condition code.

   If X and Y have mode BLKmode, then SIZE specifies the size of both X and Y.

   MODE is the mode of the inputs (in case they are const_int).

   COMPARISON is the rtl operator to compare with (EQ, NE, GT, etc.).
   It will be potentially converted into an unsigned variant based on
   UNSIGNEDP to select a proper jump instruction.
   
   PROB is the probability of jumping to LABEL.  */

void
emit_cmp_and_jump_insns (rtx x, rtx y, enum rtx_code comparison, rtx size,
			 machine_mode mode, int unsignedp, rtx label,
                         profile_probability prob)
{
  rtx op0 = x, op1 = y;
  rtx test;

  /* Swap operands and condition to ensure canonical RTL.  */
  if (swap_commutative_operands_p (x, y)
      && can_compare_p (swap_condition (comparison), mode, ccp_jump))
    {
      op0 = y, op1 = x;
      comparison = swap_condition (comparison);
    }

  /* If OP0 is still a constant, then both X and Y must be constants
     or the opposite comparison is not supported.  Force X into a register
     to create canonical RTL.  */
  if (CONSTANT_P (op0))
    op0 = force_reg (mode, op0);

  if (unsignedp)
    comparison = unsigned_condition (comparison);

  prepare_cmp_insn (op0, op1, comparison, size, unsignedp, OPTAB_LIB_WIDEN,
		    &test, &mode);
  emit_cmp_and_jump_insn_1 (test, mode, label, prob);
}


/* Emit a library call comparison between floating point X and Y.
   COMPARISON is the rtl operator to compare with (EQ, NE, GT, etc.).  */

static void
prepare_float_lib_cmp (rtx x, rtx y, enum rtx_code comparison,
		       rtx *ptest, machine_mode *pmode)
{
  enum rtx_code swapped = swap_condition (comparison);
  enum rtx_code reversed = reverse_condition_maybe_unordered (comparison);
  machine_mode orig_mode = GET_MODE (x);
  machine_mode mode;
  rtx true_rtx, false_rtx;
  rtx value, target, equiv;
  rtx_insn *insns;
  rtx libfunc = 0;
  bool reversed_p = false;
  scalar_int_mode cmp_mode = targetm.libgcc_cmp_return_mode ();

  FOR_EACH_MODE_FROM (mode, orig_mode)
    {
      if (code_to_optab (comparison)
	  && (libfunc = optab_libfunc (code_to_optab (comparison), mode)))
	break;

      if (code_to_optab (swapped)
	  && (libfunc = optab_libfunc (code_to_optab (swapped), mode)))
	{
	  std::swap (x, y);
	  comparison = swapped;
	  break;
	}

      if (code_to_optab (reversed)
	  && (libfunc = optab_libfunc (code_to_optab (reversed), mode)))
	{
	  comparison = reversed;
	  reversed_p = true;
	  break;
	}
    }

  gcc_assert (mode != VOIDmode);

  if (mode != orig_mode)
    {
      x = convert_to_mode (mode, x, 0);
      y = convert_to_mode (mode, y, 0);
    }

  /* Attach a REG_EQUAL note describing the semantics of the libcall to
     the RTL.  The allows the RTL optimizers to delete the libcall if the
     condition can be determined at compile-time.  */
  if (comparison == UNORDERED
      || FLOAT_LIB_COMPARE_RETURNS_BOOL (mode, comparison))
    {
      true_rtx = const_true_rtx;
      false_rtx = const0_rtx;
    }
  else
    {
      switch (comparison)
        {
        case EQ:
          true_rtx = const0_rtx;
          false_rtx = const_true_rtx;
          break;

        case NE:
          true_rtx = const_true_rtx;
          false_rtx = const0_rtx;
          break;

        case GT:
          true_rtx = const1_rtx;
          false_rtx = const0_rtx;
          break;

        case GE:
          true_rtx = const0_rtx;
          false_rtx = constm1_rtx;
          break;

        case LT:
          true_rtx = constm1_rtx;
          false_rtx = const0_rtx;
          break;

        case LE:
          true_rtx = const0_rtx;
          false_rtx = const1_rtx;
          break;

        default:
          gcc_unreachable ();
        }
    }

  if (comparison == UNORDERED)
    {
      rtx temp = simplify_gen_relational (NE, cmp_mode, mode, x, x);
      equiv = simplify_gen_relational (NE, cmp_mode, mode, y, y);
      equiv = simplify_gen_ternary (IF_THEN_ELSE, cmp_mode, cmp_mode,
				    temp, const_true_rtx, equiv);
    }
  else
    {
      equiv = simplify_gen_relational (comparison, cmp_mode, mode, x, y);
      if (! FLOAT_LIB_COMPARE_RETURNS_BOOL (mode, comparison))
        equiv = simplify_gen_ternary (IF_THEN_ELSE, cmp_mode, cmp_mode,
                                      equiv, true_rtx, false_rtx);
    }

  start_sequence ();
  value = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST,
				   cmp_mode, 2, x, mode, y, mode);
  insns = get_insns ();
  end_sequence ();

  target = gen_reg_rtx (cmp_mode);
  emit_libcall_block (insns, target, value, equiv);

  if (comparison == UNORDERED
      || FLOAT_LIB_COMPARE_RETURNS_BOOL (mode, comparison)
      || reversed_p)
    *ptest = gen_rtx_fmt_ee (reversed_p ? EQ : NE, VOIDmode, target, false_rtx);
  else
    *ptest = gen_rtx_fmt_ee (comparison, VOIDmode, target, const0_rtx);

  *pmode = cmp_mode;
}

/* Generate code to indirectly jump to a location given in the rtx LOC.  */

void
emit_indirect_jump (rtx loc)
{
  if (!targetm.have_indirect_jump ())
    sorry ("indirect jumps are not available on this target");
  else
    {
      struct expand_operand ops[1];
      create_address_operand (&ops[0], loc);
      expand_jump_insn (targetm.code_for_indirect_jump, 1, ops);
      emit_barrier ();
    }
}


/* Emit a conditional move instruction if the machine supports one for that
   condition and machine mode.

   OP0 and OP1 are the operands that should be compared using CODE.  CMODE is
   the mode to use should they be constants.  If it is VOIDmode, they cannot
   both be constants.

   OP2 should be stored in TARGET if the comparison is true, otherwise OP3
   should be stored there.  MODE is the mode to use should they be constants.
   If it is VOIDmode, they cannot both be constants.

   The result is either TARGET (perhaps modified) or NULL_RTX if the operation
   is not supported.  */

rtx
emit_conditional_move (rtx target, enum rtx_code code, rtx op0, rtx op1,
		       machine_mode cmode, rtx op2, rtx op3,
		       machine_mode mode, int unsignedp)
{
  rtx comparison;
  rtx_insn *last;
  enum insn_code icode;
  enum rtx_code reversed;

  /* If the two source operands are identical, that's just a move.  */

  if (rtx_equal_p (op2, op3))
    {
      if (!target)
	target = gen_reg_rtx (mode);

      emit_move_insn (target, op3);
      return target;
    }

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
    {
      std::swap (op0, op1);
      code = swap_condition (code);
    }

  /* get_condition will prefer to generate LT and GT even if the old
     comparison was against zero, so undo that canonicalization here since
     comparisons against zero are cheaper.  */
  if (code == LT && op1 == const1_rtx)
    code = LE, op1 = const0_rtx;
  else if (code == GT && op1 == constm1_rtx)
    code = GE, op1 = const0_rtx;

  if (cmode == VOIDmode)
    cmode = GET_MODE (op0);

  enum rtx_code orig_code = code;
  bool swapped = false;
  if (swap_commutative_operands_p (op2, op3)
      && ((reversed = reversed_comparison_code_parts (code, op0, op1, NULL))
          != UNKNOWN))
    {
      std::swap (op2, op3);
      code = reversed;
      swapped = true;
    }

  if (mode == VOIDmode)
    mode = GET_MODE (op2);

  icode = direct_optab_handler (movcc_optab, mode);

  if (icode == CODE_FOR_nothing)
    return NULL_RTX;

  if (!target)
    target = gen_reg_rtx (mode);

  for (int pass = 0; ; pass++)
    {
      code = unsignedp ? unsigned_condition (code) : code;
      comparison = simplify_gen_relational (code, VOIDmode, cmode, op0, op1);

      /* We can get const0_rtx or const_true_rtx in some circumstances.  Just
	 punt and let the caller figure out how best to deal with this
	 situation.  */
      if (COMPARISON_P (comparison))
	{
	  saved_pending_stack_adjust save;
	  save_pending_stack_adjust (&save);
	  last = get_last_insn ();
	  do_pending_stack_adjust ();
	  prepare_cmp_insn (XEXP (comparison, 0), XEXP (comparison, 1),
			    GET_CODE (comparison), NULL_RTX, unsignedp,
			    OPTAB_WIDEN, &comparison, &cmode);
	  if (comparison)
	    {
	      struct expand_operand ops[4];

	      create_output_operand (&ops[0], target, mode);
	      create_fixed_operand (&ops[1], comparison);
	      create_input_operand (&ops[2], op2, mode);
	      create_input_operand (&ops[3], op3, mode);
	      if (maybe_expand_insn (icode, 4, ops))
		{
		  if (ops[0].value != target)
		    convert_move (target, ops[0].value, false);
		  return target;
		}
	    }
	  delete_insns_since (last);
	  restore_pending_stack_adjust (&save);
	}

      if (pass == 1)
	return NULL_RTX;

      /* If the preferred op2/op3 order is not usable, retry with other
	 operand order, perhaps it will expand successfully.  */
      if (swapped)
	code = orig_code;
      else if ((reversed = reversed_comparison_code_parts (orig_code, op0, op1,
							   NULL))
	       != UNKNOWN)
	code = reversed;
      else
	return NULL_RTX;
      std::swap (op2, op3);
    }
}


/* Emit a conditional negate or bitwise complement using the
   negcc or notcc optabs if available.  Return NULL_RTX if such operations
   are not available.  Otherwise return the RTX holding the result.
   TARGET is the desired destination of the result.  COMP is the comparison
   on which to negate.  If COND is true move into TARGET the negation
   or bitwise complement of OP1.  Otherwise move OP2 into TARGET.
   CODE is either NEG or NOT.  MODE is the machine mode in which the
   operation is performed.  */

rtx
emit_conditional_neg_or_complement (rtx target, rtx_code code,
				     machine_mode mode, rtx cond, rtx op1,
				     rtx op2)
{
  optab op = unknown_optab;
  if (code == NEG)
    op = negcc_optab;
  else if (code == NOT)
    op = notcc_optab;
  else
    gcc_unreachable ();

  insn_code icode = direct_optab_handler (op, mode);

  if (icode == CODE_FOR_nothing)
    return NULL_RTX;

  if (!target)
    target = gen_reg_rtx (mode);

  rtx_insn *last = get_last_insn ();
  struct expand_operand ops[4];

  create_output_operand (&ops[0], target, mode);
  create_fixed_operand (&ops[1], cond);
  create_input_operand (&ops[2], op1, mode);
  create_input_operand (&ops[3], op2, mode);

  if (maybe_expand_insn (icode, 4, ops))
    {
      if (ops[0].value != target)
	convert_move (target, ops[0].value, false);

      return target;
    }
  delete_insns_since (last);
  return NULL_RTX;
}

/* Emit a conditional addition instruction if the machine supports one for that
   condition and machine mode.

   OP0 and OP1 are the operands that should be compared using CODE.  CMODE is
   the mode to use should they be constants.  If it is VOIDmode, they cannot
   both be constants.

   OP2 should be stored in TARGET if the comparison is false, otherwise OP2+OP3
   should be stored there.  MODE is the mode to use should they be constants.
   If it is VOIDmode, they cannot both be constants.

   The result is either TARGET (perhaps modified) or NULL_RTX if the operation
   is not supported.  */

rtx
emit_conditional_add (rtx target, enum rtx_code code, rtx op0, rtx op1,
		      machine_mode cmode, rtx op2, rtx op3,
		      machine_mode mode, int unsignedp)
{
  rtx comparison;
  rtx_insn *last;
  enum insn_code icode;

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
    {
      std::swap (op0, op1);
      code = swap_condition (code);
    }

  /* get_condition will prefer to generate LT and GT even if the old
     comparison was against zero, so undo that canonicalization here since
     comparisons against zero are cheaper.  */
  if (code == LT && op1 == const1_rtx)
    code = LE, op1 = const0_rtx;
  else if (code == GT && op1 == constm1_rtx)
    code = GE, op1 = const0_rtx;

  if (cmode == VOIDmode)
    cmode = GET_MODE (op0);

  if (mode == VOIDmode)
    mode = GET_MODE (op2);

  icode = optab_handler (addcc_optab, mode);

  if (icode == CODE_FOR_nothing)
    return 0;

  if (!target)
    target = gen_reg_rtx (mode);

  code = unsignedp ? unsigned_condition (code) : code;
  comparison = simplify_gen_relational (code, VOIDmode, cmode, op0, op1);

  /* We can get const0_rtx or const_true_rtx in some circumstances.  Just
     return NULL and let the caller figure out how best to deal with this
     situation.  */
  if (!COMPARISON_P (comparison))
    return NULL_RTX;

  do_pending_stack_adjust ();
  last = get_last_insn ();
  prepare_cmp_insn (XEXP (comparison, 0), XEXP (comparison, 1),
                    GET_CODE (comparison), NULL_RTX, unsignedp, OPTAB_WIDEN,
                    &comparison, &cmode);
  if (comparison)
    {
      struct expand_operand ops[4];

      create_output_operand (&ops[0], target, mode);
      create_fixed_operand (&ops[1], comparison);
      create_input_operand (&ops[2], op2, mode);
      create_input_operand (&ops[3], op3, mode);
      if (maybe_expand_insn (icode, 4, ops))
	{
	  if (ops[0].value != target)
	    convert_move (target, ops[0].value, false);
	  return target;
	}
    }
  delete_insns_since (last);
  return NULL_RTX;
}

/* These functions attempt to generate an insn body, rather than
   emitting the insn, but if the gen function already emits them, we
   make no attempt to turn them back into naked patterns.  */

/* Generate and return an insn body to add Y to X.  */

rtx_insn *
gen_add2_insn (rtx x, rtx y)
{
  enum insn_code icode = optab_handler (add_optab, GET_MODE (x));

  gcc_assert (insn_operand_matches (icode, 0, x));
  gcc_assert (insn_operand_matches (icode, 1, x));
  gcc_assert (insn_operand_matches (icode, 2, y));

  return GEN_FCN (icode) (x, x, y);
}

/* Generate and return an insn body to add r1 and c,
   storing the result in r0.  */

rtx_insn *
gen_add3_insn (rtx r0, rtx r1, rtx c)
{
  enum insn_code icode = optab_handler (add_optab, GET_MODE (r0));

  if (icode == CODE_FOR_nothing
      || !insn_operand_matches (icode, 0, r0)
      || !insn_operand_matches (icode, 1, r1)
      || !insn_operand_matches (icode, 2, c))
    return NULL;

  return GEN_FCN (icode) (r0, r1, c);
}

int
have_add2_insn (rtx x, rtx y)
{
  enum insn_code icode;

  gcc_assert (GET_MODE (x) != VOIDmode);

  icode = optab_handler (add_optab, GET_MODE (x));

  if (icode == CODE_FOR_nothing)
    return 0;

  if (!insn_operand_matches (icode, 0, x)
      || !insn_operand_matches (icode, 1, x)
      || !insn_operand_matches (icode, 2, y))
    return 0;

  return 1;
}

/* Generate and return an insn body to add Y to X.  */

rtx_insn *
gen_addptr3_insn (rtx x, rtx y, rtx z)
{
  enum insn_code icode = optab_handler (addptr3_optab, GET_MODE (x));

  gcc_assert (insn_operand_matches (icode, 0, x));
  gcc_assert (insn_operand_matches (icode, 1, y));
  gcc_assert (insn_operand_matches (icode, 2, z));

  return GEN_FCN (icode) (x, y, z);
}

/* Return true if the target implements an addptr pattern and X, Y,
   and Z are valid for the pattern predicates.  */

int
have_addptr3_insn (rtx x, rtx y, rtx z)
{
  enum insn_code icode;

  gcc_assert (GET_MODE (x) != VOIDmode);

  icode = optab_handler (addptr3_optab, GET_MODE (x));

  if (icode == CODE_FOR_nothing)
    return 0;

  if (!insn_operand_matches (icode, 0, x)
      || !insn_operand_matches (icode, 1, y)
      || !insn_operand_matches (icode, 2, z))
    return 0;

  return 1;
}

/* Generate and return an insn body to subtract Y from X.  */

rtx_insn *
gen_sub2_insn (rtx x, rtx y)
{
  enum insn_code icode = optab_handler (sub_optab, GET_MODE (x));

  gcc_assert (insn_operand_matches (icode, 0, x));
  gcc_assert (insn_operand_matches (icode, 1, x));
  gcc_assert (insn_operand_matches (icode, 2, y));

  return GEN_FCN (icode) (x, x, y);
}

/* Generate and return an insn body to subtract r1 and c,
   storing the result in r0.  */

rtx_insn *
gen_sub3_insn (rtx r0, rtx r1, rtx c)
{
  enum insn_code icode = optab_handler (sub_optab, GET_MODE (r0));

  if (icode == CODE_FOR_nothing
      || !insn_operand_matches (icode, 0, r0)
      || !insn_operand_matches (icode, 1, r1)
      || !insn_operand_matches (icode, 2, c))
    return NULL;

  return GEN_FCN (icode) (r0, r1, c);
}

int
have_sub2_insn (rtx x, rtx y)
{
  enum insn_code icode;

  gcc_assert (GET_MODE (x) != VOIDmode);

  icode = optab_handler (sub_optab, GET_MODE (x));

  if (icode == CODE_FOR_nothing)
    return 0;

  if (!insn_operand_matches (icode, 0, x)
      || !insn_operand_matches (icode, 1, x)
      || !insn_operand_matches (icode, 2, y))
    return 0;

  return 1;
}

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */

rtx_insn *
gen_extend_insn (rtx x, rtx y, machine_mode mto,
		 machine_mode mfrom, int unsignedp)
{
  enum insn_code icode = can_extend_p (mto, mfrom, unsignedp);
  return GEN_FCN (icode) (x, y);
}

/* Generate code to convert FROM to floating point
   and store in TO.  FROM must be fixed point and not VOIDmode.
   UNSIGNEDP nonzero means regard FROM as unsigned.
   Normally this is done by correcting the final value
   if it is negative.  */

void
expand_float (rtx to, rtx from, int unsignedp)
{
  enum insn_code icode;
  rtx target = to;
  scalar_mode from_mode, to_mode;
  machine_mode fmode, imode;
  bool can_do_signed = false;

  /* Crash now, because we won't be able to decide which mode to use.  */
  gcc_assert (GET_MODE (from) != VOIDmode);

  /* Look for an insn to do the conversion.  Do it in the specified
     modes if possible; otherwise convert either input, output or both to
     wider mode.  If the integer mode is wider than the mode of FROM,
     we can do the conversion signed even if the input is unsigned.  */

  FOR_EACH_MODE_FROM (fmode, GET_MODE (to))
    FOR_EACH_MODE_FROM (imode, GET_MODE (from))
      {
	int doing_unsigned = unsignedp;

	if (fmode != GET_MODE (to)
	    && significand_size (fmode) < GET_MODE_PRECISION (GET_MODE (from)))
	  continue;

	icode = can_float_p (fmode, imode, unsignedp);
	if (icode == CODE_FOR_nothing && unsignedp)
	  {
	    enum insn_code scode = can_float_p (fmode, imode, 0);
	    if (scode != CODE_FOR_nothing)
	      can_do_signed = true;
	    if (imode != GET_MODE (from))
	      icode = scode, doing_unsigned = 0;
	  }

	if (icode != CODE_FOR_nothing)
	  {
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

  /* Unsigned integer, and no way to convert directly.  Convert as signed,
     then unconditionally adjust the result.  */
  if (unsignedp
      && can_do_signed
      && is_a <scalar_mode> (GET_MODE (to), &to_mode)
      && is_a <scalar_mode> (GET_MODE (from), &from_mode))
    {
      rtx_code_label *label = gen_label_rtx ();
      rtx temp;
      REAL_VALUE_TYPE offset;

      /* Look for a usable floating mode FMODE wider than the source and at
	 least as wide as the target.  Using FMODE will avoid rounding woes
	 with unsigned values greater than the signed maximum value.  */

      FOR_EACH_MODE_FROM (fmode, to_mode)
	if (GET_MODE_PRECISION (from_mode) < GET_MODE_BITSIZE (fmode)
	    && can_float_p (fmode, from_mode, 0) != CODE_FOR_nothing)
	  break;

      if (fmode == VOIDmode)
	{
	  /* There is no such mode.  Pretend the target is wide enough.  */
	  fmode = to_mode;

	  /* Avoid double-rounding when TO is narrower than FROM.  */
	  if ((significand_size (fmode) + 1)
	      < GET_MODE_PRECISION (from_mode))
	    {
	      rtx temp1;
	      rtx_code_label *neglabel = gen_label_rtx ();

	      /* Don't use TARGET if it isn't a register, is a hard register,
		 or is the wrong mode.  */
	      if (!REG_P (target)
		  || REGNO (target) < FIRST_PSEUDO_REGISTER
		  || GET_MODE (target) != fmode)
		target = gen_reg_rtx (fmode);

	      imode = from_mode;
	      do_pending_stack_adjust ();

	      /* Test whether the sign bit is set.  */
	      emit_cmp_and_jump_insns (from, const0_rtx, LT, NULL_RTX, imode,
				       0, neglabel);

	      /* The sign bit is not set.  Convert as signed.  */
	      expand_float (target, from, 0);
	      emit_jump_insn (targetm.gen_jump (label));
	      emit_barrier ();

	      /* The sign bit is set.
		 Convert to a usable (positive signed) value by shifting right
		 one bit, while remembering if a nonzero bit was shifted
		 out; i.e., compute  (from & 1) | (from >> 1).  */

	      emit_label (neglabel);
	      temp = expand_binop (imode, and_optab, from, const1_rtx,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
	      temp1 = expand_shift (RSHIFT_EXPR, imode, from, 1, NULL_RTX, 1);
	      temp = expand_binop (imode, ior_optab, temp, temp1, temp, 1,
				   OPTAB_LIB_WIDEN);
	      expand_float (target, temp, 0);

	      /* Multiply by 2 to undo the shift above.  */
	      temp = expand_binop (fmode, add_optab, target, target,
				   target, 0, OPTAB_LIB_WIDEN);
	      if (temp != target)
		emit_move_insn (target, temp);

	      do_pending_stack_adjust ();
	      emit_label (label);
	      goto done;
	    }
	}

      /* If we are about to do some arithmetic to correct for an
	 unsigned operand, do it in a pseudo-register.  */

      if (to_mode != fmode
	  || !REG_P (to) || REGNO (to) < FIRST_PSEUDO_REGISTER)
	target = gen_reg_rtx (fmode);

      /* Convert as signed integer to floating.  */
      expand_float (target, from, 0);

      /* If FROM is negative (and therefore TO is negative),
	 correct its value by 2**bitwidth.  */

      do_pending_stack_adjust ();
      emit_cmp_and_jump_insns (from, const0_rtx, GE, NULL_RTX, from_mode,
			       0, label);


      real_2expN (&offset, GET_MODE_PRECISION (from_mode), fmode);
      temp = expand_binop (fmode, add_optab, target,
			   const_double_from_real_value (offset, fmode),
			   target, 0, OPTAB_LIB_WIDEN);
      if (temp != target)
	emit_move_insn (target, temp);

      do_pending_stack_adjust ();
      emit_label (label);
      goto done;
    }

  /* No hardware instruction available; call a library routine.  */
    {
      rtx libfunc;
      rtx_insn *insns;
      rtx value;
      convert_optab tab = unsignedp ? ufloat_optab : sfloat_optab;

      if (GET_MODE_PRECISION (GET_MODE (from)) < GET_MODE_PRECISION (SImode))
	from = convert_to_mode (SImode, from, unsignedp);

      libfunc = convert_optab_libfunc (tab, GET_MODE (to), GET_MODE (from));
      gcc_assert (libfunc);

      start_sequence ();

      value = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST,
				       GET_MODE (to), 1, from,
				       GET_MODE (from));
      insns = get_insns ();
      end_sequence ();

      emit_libcall_block (insns, target, value,
			  gen_rtx_fmt_e (unsignedp ? UNSIGNED_FLOAT : FLOAT,
					 GET_MODE (to), from));
    }

 done:

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

/* Generate code to convert FROM to fixed point and store in TO.  FROM
   must be floating point.  */

void
expand_fix (rtx to, rtx from, int unsignedp)
{
  enum insn_code icode;
  rtx target = to;
  machine_mode fmode, imode;
  bool must_trunc = false;

  /* We first try to find a pair of modes, one real and one integer, at
     least as wide as FROM and TO, respectively, in which we can open-code
     this conversion.  If the integer mode is wider than the mode of TO,
     we can do the conversion either signed or unsigned.  */

  FOR_EACH_MODE_FROM (fmode, GET_MODE (from))
    FOR_EACH_MODE_FROM (imode, GET_MODE (to))
      {
	int doing_unsigned = unsignedp;

	icode = can_fix_p (imode, fmode, unsignedp, &must_trunc);
	if (icode == CODE_FOR_nothing && imode != GET_MODE (to) && unsignedp)
	  icode = can_fix_p (imode, fmode, 0, &must_trunc), doing_unsigned = 0;

	if (icode != CODE_FOR_nothing)
	  {
	    rtx_insn *last = get_last_insn ();
	    if (fmode != GET_MODE (from))
	      from = convert_to_mode (fmode, from, 0);

	    if (must_trunc)
	      {
		rtx temp = gen_reg_rtx (GET_MODE (from));
		from = expand_unop (GET_MODE (from), ftrunc_optab, from,
				    temp, 0);
	      }

	    if (imode != GET_MODE (to))
	      target = gen_reg_rtx (imode);

	    if (maybe_emit_unop_insn (icode, target, from,
				      doing_unsigned ? UNSIGNED_FIX : FIX))
	      {
		if (target != to)
		  convert_move (to, target, unsignedp);
		return;
	      }
	    delete_insns_since (last);
	  }
      }

  /* For an unsigned conversion, there is one more way to do it.
     If we have a signed conversion, we generate code that compares
     the real value to the largest representable positive number.  If if
     is smaller, the conversion is done normally.  Otherwise, subtract
     one plus the highest signed number, convert, and add it back.

     We only need to check all real modes, since we know we didn't find
     anything with a wider integer mode.

     This code used to extend FP value into mode wider than the destination.
     This is needed for decimal float modes which cannot accurately
     represent one plus the highest signed number of the same size, but
     not for binary modes.  Consider, for instance conversion from SFmode
     into DImode.

     The hot path through the code is dealing with inputs smaller than 2^63
     and doing just the conversion, so there is no bits to lose.

     In the other path we know the value is positive in the range 2^63..2^64-1
     inclusive.  (as for other input overflow happens and result is undefined)
     So we know that the most important bit set in mantissa corresponds to
     2^63.  The subtraction of 2^63 should not generate any rounding as it
     simply clears out that bit.  The rest is trivial.  */

  scalar_int_mode to_mode;
  if (unsignedp
      && is_a <scalar_int_mode> (GET_MODE (to), &to_mode)
      && HWI_COMPUTABLE_MODE_P (to_mode))
    FOR_EACH_MODE_FROM (fmode, GET_MODE (from))
      if (CODE_FOR_nothing != can_fix_p (to_mode, fmode, 0, &must_trunc)
	  && (!DECIMAL_FLOAT_MODE_P (fmode)
	      || GET_MODE_BITSIZE (fmode) > GET_MODE_PRECISION (to_mode)))
	{
	  int bitsize;
	  REAL_VALUE_TYPE offset;
	  rtx limit;
	  rtx_code_label *lab1, *lab2;
	  rtx_insn *insn;

	  bitsize = GET_MODE_PRECISION (to_mode);
	  real_2expN (&offset, bitsize - 1, fmode);
	  limit = const_double_from_real_value (offset, fmode);
	  lab1 = gen_label_rtx ();
	  lab2 = gen_label_rtx ();

	  if (fmode != GET_MODE (from))
	    from = convert_to_mode (fmode, from, 0);

	  /* See if we need to do the subtraction.  */
	  do_pending_stack_adjust ();
	  emit_cmp_and_jump_insns (from, limit, GE, NULL_RTX, GET_MODE (from),
				   0, lab1);

	  /* If not, do the signed "fix" and branch around fixup code.  */
	  expand_fix (to, from, 0);
	  emit_jump_insn (targetm.gen_jump (lab2));
	  emit_barrier ();

	  /* Otherwise, subtract 2**(N-1), convert to signed number,
	     then add 2**(N-1).  Do the addition using XOR since this
	     will often generate better code.  */
	  emit_label (lab1);
	  target = expand_binop (GET_MODE (from), sub_optab, from, limit,
				 NULL_RTX, 0, OPTAB_LIB_WIDEN);
	  expand_fix (to, target, 0);
	  target = expand_binop (to_mode, xor_optab, to,
				 gen_int_mode
				 (HOST_WIDE_INT_1 << (bitsize - 1),
				  to_mode),
				 to, 1, OPTAB_LIB_WIDEN);

	  if (target != to)
	    emit_move_insn (to, target);

	  emit_label (lab2);

	  if (optab_handler (mov_optab, to_mode) != CODE_FOR_nothing)
	    {
	      /* Make a place for a REG_NOTE and add it.  */
	      insn = emit_move_insn (to, to);
	      set_dst_reg_note (insn, REG_EQUAL,
				gen_rtx_fmt_e (UNSIGNED_FIX, to_mode,
					       copy_rtx (from)),
				to);
	    }

	  return;
	}

  /* We can't do it with an insn, so use a library call.  But first ensure
     that the mode of TO is at least as wide as SImode, since those are the
     only library calls we know about.  */

  if (GET_MODE_PRECISION (GET_MODE (to)) < GET_MODE_PRECISION (SImode))
    {
      target = gen_reg_rtx (SImode);

      expand_fix (target, from, unsignedp);
    }
  else
    {
      rtx_insn *insns;
      rtx value;
      rtx libfunc;

      convert_optab tab = unsignedp ? ufix_optab : sfix_optab;
      libfunc = convert_optab_libfunc (tab, GET_MODE (to), GET_MODE (from));
      gcc_assert (libfunc);

      start_sequence ();

      value = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST,
				       GET_MODE (to), 1, from,
				       GET_MODE (from));
      insns = get_insns ();
      end_sequence ();

      emit_libcall_block (insns, target, value,
			  gen_rtx_fmt_e (unsignedp ? UNSIGNED_FIX : FIX,
					 GET_MODE (to), from));
    }

  if (target != to)
    {
      if (GET_MODE (to) == GET_MODE (target))
        emit_move_insn (to, target);
      else
        convert_move (to, target, 0);
    }
}


/* Promote integer arguments for a libcall if necessary.
   emit_library_call_value cannot do the promotion because it does not
   know if it should do a signed or unsigned promotion.  This is because
   there are no tree types defined for libcalls.  */

static rtx
prepare_libcall_arg (rtx arg, int uintp)
{
  scalar_int_mode mode;
  machine_mode arg_mode;
  if (is_a <scalar_int_mode> (GET_MODE (arg), &mode))
    {
      /*  If we need to promote the integer function argument we need to do
	  it here instead of inside emit_library_call_value because in
	  emit_library_call_value we don't know if we should do a signed or
	  unsigned promotion.  */

      int unsigned_p = 0;
      arg_mode = promote_function_mode (NULL_TREE, mode,
					&unsigned_p, NULL_TREE, 0);
      if (arg_mode != mode)
	return convert_to_mode (arg_mode, arg, uintp);
    }
    return arg;
}

/* Generate code to convert FROM or TO a fixed-point.
   If UINTP is true, either TO or FROM is an unsigned integer.
   If SATP is true, we need to saturate the result.  */

void
expand_fixed_convert (rtx to, rtx from, int uintp, int satp)
{
  machine_mode to_mode = GET_MODE (to);
  machine_mode from_mode = GET_MODE (from);
  convert_optab tab;
  enum rtx_code this_code;
  enum insn_code code;
  rtx_insn *insns;
  rtx value;
  rtx libfunc;

  if (to_mode == from_mode)
    {
      emit_move_insn (to, from);
      return;
    }

  if (uintp)
    {
      tab = satp ? satfractuns_optab : fractuns_optab;
      this_code = satp ? UNSIGNED_SAT_FRACT : UNSIGNED_FRACT_CONVERT;
    }
  else
    {
      tab = satp ? satfract_optab : fract_optab;
      this_code = satp ? SAT_FRACT : FRACT_CONVERT;
    }
  code = convert_optab_handler (tab, to_mode, from_mode);
  if (code != CODE_FOR_nothing)
    {
      emit_unop_insn (code, to, from, this_code);
      return;
    }

  libfunc = convert_optab_libfunc (tab, to_mode, from_mode);
  gcc_assert (libfunc);

  from = prepare_libcall_arg (from, uintp);
  from_mode = GET_MODE (from);

  start_sequence ();
  value = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST, to_mode,
				   1, from, from_mode);
  insns = get_insns ();
  end_sequence ();

  emit_libcall_block (insns, to, value,
		      gen_rtx_fmt_e (optab_to_code (tab), to_mode, from));
}

/* Generate code to convert FROM to fixed point and store in TO.  FROM
   must be floating point, TO must be signed.  Use the conversion optab
   TAB to do the conversion.  */

bool
expand_sfix_optab (rtx to, rtx from, convert_optab tab)
{
  enum insn_code icode;
  rtx target = to;
  machine_mode fmode, imode;

  /* We first try to find a pair of modes, one real and one integer, at
     least as wide as FROM and TO, respectively, in which we can open-code
     this conversion.  If the integer mode is wider than the mode of TO,
     we can do the conversion either signed or unsigned.  */

  FOR_EACH_MODE_FROM (fmode, GET_MODE (from))
    FOR_EACH_MODE_FROM (imode, GET_MODE (to))
      {
	icode = convert_optab_handler (tab, imode, fmode);
	if (icode != CODE_FOR_nothing)
	  {
	    rtx_insn *last = get_last_insn ();
	    if (fmode != GET_MODE (from))
	      from = convert_to_mode (fmode, from, 0);

	    if (imode != GET_MODE (to))
	      target = gen_reg_rtx (imode);

	    if (!maybe_emit_unop_insn (icode, target, from, UNKNOWN))
	      {
	        delete_insns_since (last);
		continue;
	      }
	    if (target != to)
	      convert_move (to, target, 0);
	    return true;
	  }
      }

  return false;
}

/* Report whether we have an instruction to perform the operation
   specified by CODE on operands of mode MODE.  */
int
have_insn_for (enum rtx_code code, machine_mode mode)
{
  return (code_to_optab (code)
	  && (optab_handler (code_to_optab (code), mode)
	      != CODE_FOR_nothing));
}

/* Print information about the current contents of the optabs on
   STDERR.  */

DEBUG_FUNCTION void
debug_optab_libfuncs (void)
{
  int i, j, k;

  /* Dump the arithmetic optabs.  */
  for (i = FIRST_NORM_OPTAB; i <= LAST_NORMLIB_OPTAB; ++i)
    for (j = 0; j < NUM_MACHINE_MODES; ++j)
      {
	rtx l = optab_libfunc ((optab) i, (machine_mode) j);
	if (l)
	  {
	    gcc_assert (GET_CODE (l) == SYMBOL_REF);
	    fprintf (stderr, "%s\t%s:\t%s\n",
		     GET_RTX_NAME (optab_to_code ((optab) i)),
		     GET_MODE_NAME (j),
		     XSTR (l, 0));
	  }
      }

  /* Dump the conversion optabs.  */
  for (i = FIRST_CONV_OPTAB; i <= LAST_CONVLIB_OPTAB; ++i)
    for (j = 0; j < NUM_MACHINE_MODES; ++j)
      for (k = 0; k < NUM_MACHINE_MODES; ++k)
	{
	  rtx l = convert_optab_libfunc ((optab) i, (machine_mode) j,
					 (machine_mode) k);
	  if (l)
	    {
	      gcc_assert (GET_CODE (l) == SYMBOL_REF);
	      fprintf (stderr, "%s\t%s\t%s:\t%s\n",
		       GET_RTX_NAME (optab_to_code ((optab) i)),
		       GET_MODE_NAME (j),
		       GET_MODE_NAME (k),
		       XSTR (l, 0));
	    }
	}
}

/* Generate insns to trap with code TCODE if OP1 and OP2 satisfy condition
   CODE.  Return 0 on failure.  */

rtx_insn *
gen_cond_trap (enum rtx_code code, rtx op1, rtx op2, rtx tcode)
{
  machine_mode mode = GET_MODE (op1);
  enum insn_code icode;
  rtx_insn *insn;
  rtx trap_rtx;

  if (mode == VOIDmode)
    return 0;

  icode = optab_handler (ctrap_optab, mode);
  if (icode == CODE_FOR_nothing)
    return 0;

  /* Some targets only accept a zero trap code.  */
  if (!insn_operand_matches (icode, 3, tcode))
    return 0;

  do_pending_stack_adjust ();
  start_sequence ();
  prepare_cmp_insn (op1, op2, code, NULL_RTX, false, OPTAB_DIRECT,
		    &trap_rtx, &mode);
  if (!trap_rtx)
    insn = NULL;
  else
    insn = GEN_FCN (icode) (trap_rtx, XEXP (trap_rtx, 0), XEXP (trap_rtx, 1),
			    tcode);

  /* If that failed, then give up.  */
  if (insn == 0)
    {
      end_sequence ();
      return 0;
    }

  emit_insn (insn);
  insn = get_insns ();
  end_sequence ();
  return insn;
}

/* Return rtx code for TCODE. Use UNSIGNEDP to select signed
   or unsigned operation code.  */

enum rtx_code
get_rtx_code (enum tree_code tcode, bool unsignedp)
{
  enum rtx_code code;
  switch (tcode)
    {
    case EQ_EXPR:
      code = EQ;
      break;
    case NE_EXPR:
      code = NE;
      break;
    case LT_EXPR:
      code = unsignedp ? LTU : LT;
      break;
    case LE_EXPR:
      code = unsignedp ? LEU : LE;
      break;
    case GT_EXPR:
      code = unsignedp ? GTU : GT;
      break;
    case GE_EXPR:
      code = unsignedp ? GEU : GE;
      break;

    case UNORDERED_EXPR:
      code = UNORDERED;
      break;
    case ORDERED_EXPR:
      code = ORDERED;
      break;
    case UNLT_EXPR:
      code = UNLT;
      break;
    case UNLE_EXPR:
      code = UNLE;
      break;
    case UNGT_EXPR:
      code = UNGT;
      break;
    case UNGE_EXPR:
      code = UNGE;
      break;
    case UNEQ_EXPR:
      code = UNEQ;
      break;
    case LTGT_EXPR:
      code = LTGT;
      break;

    case BIT_AND_EXPR:
      code = AND;
      break;

    case BIT_IOR_EXPR:
      code = IOR;
      break;

    default:
      gcc_unreachable ();
    }
  return code;
}

/* Return a comparison rtx of mode CMP_MODE for COND.  Use UNSIGNEDP to
   select signed or unsigned operators.  OPNO holds the index of the
   first comparison operand for insn ICODE.  Do not generate the
   compare instruction itself.  */

static rtx
vector_compare_rtx (machine_mode cmp_mode, enum tree_code tcode,
		    tree t_op0, tree t_op1, bool unsignedp,
		    enum insn_code icode, unsigned int opno)
{
  struct expand_operand ops[2];
  rtx rtx_op0, rtx_op1;
  machine_mode m0, m1;
  enum rtx_code rcode = get_rtx_code (tcode, unsignedp);

  gcc_assert (TREE_CODE_CLASS (tcode) == tcc_comparison);

  /* Expand operands.  For vector types with scalar modes, e.g. where int64x1_t
     has mode DImode, this can produce a constant RTX of mode VOIDmode; in such
     cases, use the original mode.  */
  rtx_op0 = expand_expr (t_op0, NULL_RTX, TYPE_MODE (TREE_TYPE (t_op0)),
			 EXPAND_STACK_PARM);
  m0 = GET_MODE (rtx_op0);
  if (m0 == VOIDmode)
    m0 = TYPE_MODE (TREE_TYPE (t_op0));

  rtx_op1 = expand_expr (t_op1, NULL_RTX, TYPE_MODE (TREE_TYPE (t_op1)),
			 EXPAND_STACK_PARM);
  m1 = GET_MODE (rtx_op1);
  if (m1 == VOIDmode)
    m1 = TYPE_MODE (TREE_TYPE (t_op1));

  create_input_operand (&ops[0], rtx_op0, m0);
  create_input_operand (&ops[1], rtx_op1, m1);
  if (!maybe_legitimize_operands (icode, opno, 2, ops))
    gcc_unreachable ();
  return gen_rtx_fmt_ee (rcode, cmp_mode, ops[0].value, ops[1].value);
}

/* Checks if vec_perm mask SEL is a constant equivalent to a shift of the first
   vec_perm operand, assuming the second operand is a constant vector of zeroes.
   Return the shift distance in bits if so, or NULL_RTX if the vec_perm is not a
   shift.  */
static rtx
shift_amt_for_vec_perm_mask (rtx sel)
{
  unsigned int i, first, nelt = GET_MODE_NUNITS (GET_MODE (sel));
  unsigned int bitsize = GET_MODE_UNIT_BITSIZE (GET_MODE (sel));

  if (GET_CODE (sel) != CONST_VECTOR)
    return NULL_RTX;

  first = INTVAL (CONST_VECTOR_ELT (sel, 0));
  if (first >= nelt)
    return NULL_RTX;
  for (i = 1; i < nelt; i++)
    {
      int idx = INTVAL (CONST_VECTOR_ELT (sel, i));
      unsigned int expected = i + first;
      /* Indices into the second vector are all equivalent.  */
      if (idx < 0 || (MIN (nelt, (unsigned) idx) != MIN (nelt, expected)))
	return NULL_RTX;
    }

  return GEN_INT (first * bitsize);
}

/* A subroutine of expand_vec_perm for expanding one vec_perm insn.  */

static rtx
expand_vec_perm_1 (enum insn_code icode, rtx target,
		   rtx v0, rtx v1, rtx sel)
{
  machine_mode tmode = GET_MODE (target);
  machine_mode smode = GET_MODE (sel);
  struct expand_operand ops[4];

  create_output_operand (&ops[0], target, tmode);
  create_input_operand (&ops[3], sel, smode);

  /* Make an effort to preserve v0 == v1.  The target expander is able to
     rely on this to determine if we're permuting a single input operand.  */
  if (rtx_equal_p (v0, v1))
    {
      if (!insn_operand_matches (icode, 1, v0))
        v0 = force_reg (tmode, v0);
      gcc_checking_assert (insn_operand_matches (icode, 1, v0));
      gcc_checking_assert (insn_operand_matches (icode, 2, v0));

      create_fixed_operand (&ops[1], v0);
      create_fixed_operand (&ops[2], v0);
    }
  else
    {
      create_input_operand (&ops[1], v0, tmode);
      create_input_operand (&ops[2], v1, tmode);
    }

  if (maybe_expand_insn (icode, 4, ops))
    return ops[0].value;
  return NULL_RTX;
}

/* Generate instructions for vec_perm optab given its mode
   and three operands.  */

rtx
expand_vec_perm (machine_mode mode, rtx v0, rtx v1, rtx sel, rtx target)
{
  enum insn_code icode;
  machine_mode qimode;
  unsigned int i, w, e, u;
  rtx tmp, sel_qi = NULL;
  rtvec vec;

  if (!target || GET_MODE (target) != mode)
    target = gen_reg_rtx (mode);

  w = GET_MODE_SIZE (mode);
  e = GET_MODE_NUNITS (mode);
  u = GET_MODE_UNIT_SIZE (mode);

  /* Set QIMODE to a different vector mode with byte elements.
     If no such mode, or if MODE already has byte elements, use VOIDmode.  */
  qimode = VOIDmode;
  if (GET_MODE_INNER (mode) != QImode)
    {
      qimode = mode_for_vector (QImode, w);
      if (!VECTOR_MODE_P (qimode))
	qimode = VOIDmode;
    }

  /* If the input is a constant, expand it specially.  */
  gcc_assert (GET_MODE_CLASS (GET_MODE (sel)) == MODE_VECTOR_INT);
  if (GET_CODE (sel) == CONST_VECTOR)
    {
      /* See if this can be handled with a vec_shr.  We only do this if the
	 second vector is all zeroes.  */
      enum insn_code shift_code = optab_handler (vec_shr_optab, mode);
      enum insn_code shift_code_qi = ((qimode != VOIDmode && qimode != mode)
				      ? optab_handler (vec_shr_optab, qimode)
				      : CODE_FOR_nothing);
      rtx shift_amt = NULL_RTX;
      if (v1 == CONST0_RTX (GET_MODE (v1))
	  && (shift_code != CODE_FOR_nothing
	      || shift_code_qi != CODE_FOR_nothing))
	{
	  shift_amt = shift_amt_for_vec_perm_mask (sel);
	  if (shift_amt)
	    {
	      struct expand_operand ops[3];
	      if (shift_code != CODE_FOR_nothing)
		{
		  create_output_operand (&ops[0], target, mode);
		  create_input_operand (&ops[1], v0, mode);
		  create_convert_operand_from_type (&ops[2], shift_amt,
						    sizetype);
		  if (maybe_expand_insn (shift_code, 3, ops))
		    return ops[0].value;
		}
	      if (shift_code_qi != CODE_FOR_nothing)
		{
		  tmp = gen_reg_rtx (qimode);
		  create_output_operand (&ops[0], tmp, qimode);
		  create_input_operand (&ops[1], gen_lowpart (qimode, v0),
					qimode);
		  create_convert_operand_from_type (&ops[2], shift_amt,
						    sizetype);
		  if (maybe_expand_insn (shift_code_qi, 3, ops))
		    return gen_lowpart (mode, ops[0].value);
		}
	    }
	}

      icode = direct_optab_handler (vec_perm_const_optab, mode);
      if (icode != CODE_FOR_nothing)
	{
	  tmp = expand_vec_perm_1 (icode, target, v0, v1, sel);
	  if (tmp)
	    return tmp;
	}

      /* Fall back to a constant byte-based permutation.  */
      if (qimode != VOIDmode)
	{
	  vec = rtvec_alloc (w);
	  for (i = 0; i < e; ++i)
	    {
	      unsigned int j, this_e;

	      this_e = INTVAL (CONST_VECTOR_ELT (sel, i));
	      this_e &= 2 * e - 1;
	      this_e *= u;

	      for (j = 0; j < u; ++j)
		RTVEC_ELT (vec, i * u + j) = GEN_INT (this_e + j);
	    }
	  sel_qi = gen_rtx_CONST_VECTOR (qimode, vec);

	  icode = direct_optab_handler (vec_perm_const_optab, qimode);
	  if (icode != CODE_FOR_nothing)
	    {
	      tmp = mode != qimode ? gen_reg_rtx (qimode) : target;
	      tmp = expand_vec_perm_1 (icode, tmp, gen_lowpart (qimode, v0),
				       gen_lowpart (qimode, v1), sel_qi);
	      if (tmp)
		return gen_lowpart (mode, tmp);
	    }
	}
    }

  /* Otherwise expand as a fully variable permuation.  */
  icode = direct_optab_handler (vec_perm_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      tmp = expand_vec_perm_1 (icode, target, v0, v1, sel);
      if (tmp)
	return tmp;
    }

  /* As a special case to aid several targets, lower the element-based
     permutation to a byte-based permutation and try again.  */
  if (qimode == VOIDmode)
    return NULL_RTX;
  icode = direct_optab_handler (vec_perm_optab, qimode);
  if (icode == CODE_FOR_nothing)
    return NULL_RTX;

  if (sel_qi == NULL)
    {
      /* Multiply each element by its byte size.  */
      machine_mode selmode = GET_MODE (sel);
      if (u == 2)
	sel = expand_simple_binop (selmode, PLUS, sel, sel,
				   NULL, 0, OPTAB_DIRECT);
      else
	sel = expand_simple_binop (selmode, ASHIFT, sel,
				   GEN_INT (exact_log2 (u)),
				   NULL, 0, OPTAB_DIRECT);
      gcc_assert (sel != NULL);

      /* Broadcast the low byte each element into each of its bytes.  */
      vec = rtvec_alloc (w);
      for (i = 0; i < w; ++i)
	{
	  int this_e = i / u * u;
	  if (BYTES_BIG_ENDIAN)
	    this_e += u - 1;
	  RTVEC_ELT (vec, i) = GEN_INT (this_e);
	}
      tmp = gen_rtx_CONST_VECTOR (qimode, vec);
      sel = gen_lowpart (qimode, sel);
      sel = expand_vec_perm (qimode, sel, sel, tmp, NULL);
      gcc_assert (sel != NULL);

      /* Add the byte offset to each byte element.  */
      /* Note that the definition of the indicies here is memory ordering,
	 so there should be no difference between big and little endian.  */
      vec = rtvec_alloc (w);
      for (i = 0; i < w; ++i)
	RTVEC_ELT (vec, i) = GEN_INT (i % u);
      tmp = gen_rtx_CONST_VECTOR (qimode, vec);
      sel_qi = expand_simple_binop (qimode, PLUS, sel, tmp,
				    sel, 0, OPTAB_DIRECT);
      gcc_assert (sel_qi != NULL);
    }

  tmp = mode != qimode ? gen_reg_rtx (qimode) : target;
  tmp = expand_vec_perm_1 (icode, tmp, gen_lowpart (qimode, v0),
			   gen_lowpart (qimode, v1), sel_qi);
  if (tmp)
    tmp = gen_lowpart (mode, tmp);
  return tmp;
}

/* Generate insns for a VEC_COND_EXPR with mask, given its TYPE and its
   three operands.  */

rtx
expand_vec_cond_mask_expr (tree vec_cond_type, tree op0, tree op1, tree op2,
			   rtx target)
{
  struct expand_operand ops[4];
  machine_mode mode = TYPE_MODE (vec_cond_type);
  machine_mode mask_mode = TYPE_MODE (TREE_TYPE (op0));
  enum insn_code icode = get_vcond_mask_icode (mode, mask_mode);
  rtx mask, rtx_op1, rtx_op2;

  if (icode == CODE_FOR_nothing)
    return 0;

  mask = expand_normal (op0);
  rtx_op1 = expand_normal (op1);
  rtx_op2 = expand_normal (op2);

  mask = force_reg (mask_mode, mask);
  rtx_op1 = force_reg (GET_MODE (rtx_op1), rtx_op1);

  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], rtx_op1, mode);
  create_input_operand (&ops[2], rtx_op2, mode);
  create_input_operand (&ops[3], mask, mask_mode);
  expand_insn (icode, 4, ops);

  return ops[0].value;
}

/* Generate insns for a VEC_COND_EXPR, given its TYPE and its
   three operands.  */

rtx
expand_vec_cond_expr (tree vec_cond_type, tree op0, tree op1, tree op2,
		      rtx target)
{
  struct expand_operand ops[6];
  enum insn_code icode;
  rtx comparison, rtx_op1, rtx_op2;
  machine_mode mode = TYPE_MODE (vec_cond_type);
  machine_mode cmp_op_mode;
  bool unsignedp;
  tree op0a, op0b;
  enum tree_code tcode;

  if (COMPARISON_CLASS_P (op0))
    {
      op0a = TREE_OPERAND (op0, 0);
      op0b = TREE_OPERAND (op0, 1);
      tcode = TREE_CODE (op0);
    }
  else
    {
      gcc_assert (VECTOR_BOOLEAN_TYPE_P (TREE_TYPE (op0)));
      if (get_vcond_mask_icode (mode, TYPE_MODE (TREE_TYPE (op0)))
	  != CODE_FOR_nothing)
	return expand_vec_cond_mask_expr (vec_cond_type, op0, op1,
					  op2, target);
      /* Fake op0 < 0.  */
      else
	{
	  gcc_assert (GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (op0)))
		      == MODE_VECTOR_INT);
	  op0a = op0;
	  op0b = build_zero_cst (TREE_TYPE (op0));
	  tcode = LT_EXPR;
	}
    }
  cmp_op_mode = TYPE_MODE (TREE_TYPE (op0a));
  unsignedp = TYPE_UNSIGNED (TREE_TYPE (op0a));


  gcc_assert (GET_MODE_SIZE (mode) == GET_MODE_SIZE (cmp_op_mode)
	      && GET_MODE_NUNITS (mode) == GET_MODE_NUNITS (cmp_op_mode));

  icode = get_vcond_icode (mode, cmp_op_mode, unsignedp);
  if (icode == CODE_FOR_nothing)
    {
      if (tcode == EQ_EXPR || tcode == NE_EXPR)
	icode = get_vcond_eq_icode (mode, cmp_op_mode);
      if (icode == CODE_FOR_nothing)
	return 0;
    }

  comparison = vector_compare_rtx (VOIDmode, tcode, op0a, op0b, unsignedp,
				   icode, 4);
  rtx_op1 = expand_normal (op1);
  rtx_op2 = expand_normal (op2);

  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], rtx_op1, mode);
  create_input_operand (&ops[2], rtx_op2, mode);
  create_fixed_operand (&ops[3], comparison);
  create_fixed_operand (&ops[4], XEXP (comparison, 0));
  create_fixed_operand (&ops[5], XEXP (comparison, 1));
  expand_insn (icode, 6, ops);
  return ops[0].value;
}

/* Generate insns for a vector comparison into a mask.  */

rtx
expand_vec_cmp_expr (tree type, tree exp, rtx target)
{
  struct expand_operand ops[4];
  enum insn_code icode;
  rtx comparison;
  machine_mode mask_mode = TYPE_MODE (type);
  machine_mode vmode;
  bool unsignedp;
  tree op0a, op0b;
  enum tree_code tcode;

  op0a = TREE_OPERAND (exp, 0);
  op0b = TREE_OPERAND (exp, 1);
  tcode = TREE_CODE (exp);

  unsignedp = TYPE_UNSIGNED (TREE_TYPE (op0a));
  vmode = TYPE_MODE (TREE_TYPE (op0a));

  icode = get_vec_cmp_icode (vmode, mask_mode, unsignedp);
  if (icode == CODE_FOR_nothing)
    {
      if (tcode == EQ_EXPR || tcode == NE_EXPR)
	icode = get_vec_cmp_eq_icode (vmode, mask_mode);
      if (icode == CODE_FOR_nothing)
	return 0;
    }

  comparison = vector_compare_rtx (mask_mode, tcode, op0a, op0b,
				   unsignedp, icode, 2);
  create_output_operand (&ops[0], target, mask_mode);
  create_fixed_operand (&ops[1], comparison);
  create_fixed_operand (&ops[2], XEXP (comparison, 0));
  create_fixed_operand (&ops[3], XEXP (comparison, 1));
  expand_insn (icode, 4, ops);
  return ops[0].value;
}

/* Expand a highpart multiply.  */

rtx
expand_mult_highpart (machine_mode mode, rtx op0, rtx op1,
		      rtx target, bool uns_p)
{
  struct expand_operand eops[3];
  enum insn_code icode;
  int method, i, nunits;
  machine_mode wmode;
  rtx m1, m2, perm;
  optab tab1, tab2;
  rtvec v;

  method = can_mult_highpart_p (mode, uns_p);
  switch (method)
    {
    case 0:
      return NULL_RTX;
    case 1:
      tab1 = uns_p ? umul_highpart_optab : smul_highpart_optab;
      return expand_binop (mode, tab1, op0, op1, target, uns_p,
			   OPTAB_LIB_WIDEN);
    case 2:
      tab1 = uns_p ? vec_widen_umult_even_optab : vec_widen_smult_even_optab;
      tab2 = uns_p ? vec_widen_umult_odd_optab : vec_widen_smult_odd_optab;
      break;
    case 3:
      tab1 = uns_p ? vec_widen_umult_lo_optab : vec_widen_smult_lo_optab;
      tab2 = uns_p ? vec_widen_umult_hi_optab : vec_widen_smult_hi_optab;
      if (BYTES_BIG_ENDIAN)
	std::swap (tab1, tab2);
      break;
    default:
      gcc_unreachable ();
    }

  icode = optab_handler (tab1, mode);
  nunits = GET_MODE_NUNITS (mode);
  wmode = insn_data[icode].operand[0].mode;
  gcc_checking_assert (2 * GET_MODE_NUNITS (wmode) == nunits);
  gcc_checking_assert (GET_MODE_SIZE (wmode) == GET_MODE_SIZE (mode));

  create_output_operand (&eops[0], gen_reg_rtx (wmode), wmode);
  create_input_operand (&eops[1], op0, mode);
  create_input_operand (&eops[2], op1, mode);
  expand_insn (icode, 3, eops);
  m1 = gen_lowpart (mode, eops[0].value);

  create_output_operand (&eops[0], gen_reg_rtx (wmode), wmode);
  create_input_operand (&eops[1], op0, mode);
  create_input_operand (&eops[2], op1, mode);
  expand_insn (optab_handler (tab2, mode), 3, eops);
  m2 = gen_lowpart (mode, eops[0].value);

  v = rtvec_alloc (nunits);
  if (method == 2)
    {
      for (i = 0; i < nunits; ++i)
	RTVEC_ELT (v, i) = GEN_INT (!BYTES_BIG_ENDIAN + (i & ~1)
				    + ((i & 1) ? nunits : 0));
    }
  else
    {
      for (i = 0; i < nunits; ++i)
	RTVEC_ELT (v, i) = GEN_INT (2 * i + (BYTES_BIG_ENDIAN ? 0 : 1));
    }
  perm = gen_rtx_CONST_VECTOR (mode, v);

  return expand_vec_perm (mode, m1, m2, perm, target);
}

/* Helper function to find the MODE_CC set in a sync_compare_and_swap
   pattern.  */

static void
find_cc_set (rtx x, const_rtx pat, void *data)
{
  if (REG_P (x) && GET_MODE_CLASS (GET_MODE (x)) == MODE_CC
      && GET_CODE (pat) == SET)
    {
      rtx *p_cc_reg = (rtx *) data;
      gcc_assert (!*p_cc_reg);
      *p_cc_reg = x;
    }
}

/* This is a helper function for the other atomic operations.  This function
   emits a loop that contains SEQ that iterates until a compare-and-swap
   operation at the end succeeds.  MEM is the memory to be modified.  SEQ is
   a set of instructions that takes a value from OLD_REG as an input and
   produces a value in NEW_REG as an output.  Before SEQ, OLD_REG will be
   set to the current contents of MEM.  After SEQ, a compare-and-swap will
   attempt to update MEM with NEW_REG.  The function returns true when the
   loop was generated successfully.  */

static bool
expand_compare_and_swap_loop (rtx mem, rtx old_reg, rtx new_reg, rtx seq)
{
  machine_mode mode = GET_MODE (mem);
  rtx_code_label *label;
  rtx cmp_reg, success, oldval;

  /* The loop we want to generate looks like

	cmp_reg = mem;
      label:
        old_reg = cmp_reg;
	seq;
	(success, cmp_reg) = compare-and-swap(mem, old_reg, new_reg)
	if (success)
	  goto label;

     Note that we only do the plain load from memory once.  Subsequent
     iterations use the value loaded by the compare-and-swap pattern.  */

  label = gen_label_rtx ();
  cmp_reg = gen_reg_rtx (mode);

  emit_move_insn (cmp_reg, mem);
  emit_label (label);
  emit_move_insn (old_reg, cmp_reg);
  if (seq)
    emit_insn (seq);

  success = NULL_RTX;
  oldval = cmp_reg;
  if (!expand_atomic_compare_and_swap (&success, &oldval, mem, old_reg,
				       new_reg, false, MEMMODEL_SYNC_SEQ_CST,
				       MEMMODEL_RELAXED))
    return false;

  if (oldval != cmp_reg)
    emit_move_insn (cmp_reg, oldval);

  /* Mark this jump predicted not taken.  */
  emit_cmp_and_jump_insns (success, const0_rtx, EQ, const0_rtx,
			   GET_MODE (success), 1, label,
			   profile_probability::guessed_never ());
  return true;
}


/* This function tries to emit an atomic_exchange intruction.  VAL is written
   to *MEM using memory model MODEL. The previous contents of *MEM are returned,
   using TARGET if possible.  */
   
static rtx
maybe_emit_atomic_exchange (rtx target, rtx mem, rtx val, enum memmodel model)
{
  machine_mode mode = GET_MODE (mem);
  enum insn_code icode;

  /* If the target supports the exchange directly, great.  */
  icode = direct_optab_handler (atomic_exchange_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand ops[4];

      create_output_operand (&ops[0], target, mode);
      create_fixed_operand (&ops[1], mem);
      create_input_operand (&ops[2], val, mode);
      create_integer_operand (&ops[3], model);
      if (maybe_expand_insn (icode, 4, ops))
	return ops[0].value;
    }

  return NULL_RTX;
}

/* This function tries to implement an atomic exchange operation using
   __sync_lock_test_and_set. VAL is written to *MEM using memory model MODEL.
   The previous contents of *MEM are returned, using TARGET if possible.
   Since this instructionn is an acquire barrier only, stronger memory
   models may require additional barriers to be emitted.  */

static rtx
maybe_emit_sync_lock_test_and_set (rtx target, rtx mem, rtx val,
				   enum memmodel model)
{
  machine_mode mode = GET_MODE (mem);
  enum insn_code icode;
  rtx_insn *last_insn = get_last_insn ();

  icode = optab_handler (sync_lock_test_and_set_optab, mode);

  /* Legacy sync_lock_test_and_set is an acquire barrier.  If the pattern
     exists, and the memory model is stronger than acquire, add a release 
     barrier before the instruction.  */

  if (is_mm_seq_cst (model) || is_mm_release (model) || is_mm_acq_rel (model))
    expand_mem_thread_fence (model);

  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand ops[3];
      create_output_operand (&ops[0], target, mode);
      create_fixed_operand (&ops[1], mem);
      create_input_operand (&ops[2], val, mode);
      if (maybe_expand_insn (icode, 3, ops))
	return ops[0].value;
    }

  /* If an external test-and-set libcall is provided, use that instead of
     any external compare-and-swap that we might get from the compare-and-
     swap-loop expansion later.  */
  if (!can_compare_and_swap_p (mode, false))
    {
      rtx libfunc = optab_libfunc (sync_lock_test_and_set_optab, mode);
      if (libfunc != NULL)
	{
	  rtx addr;

	  addr = convert_memory_address (ptr_mode, XEXP (mem, 0));
	  return emit_library_call_value (libfunc, NULL_RTX, LCT_NORMAL,
					  mode, 2, addr, ptr_mode,
					  val, mode);
	}
    }

  /* If the test_and_set can't be emitted, eliminate any barrier that might
     have been emitted.  */
  delete_insns_since (last_insn);
  return NULL_RTX;
}

/* This function tries to implement an atomic exchange operation using a 
   compare_and_swap loop. VAL is written to *MEM.  The previous contents of
   *MEM are returned, using TARGET if possible.  No memory model is required
   since a compare_and_swap loop is seq-cst.  */

static rtx 
maybe_emit_compare_and_swap_exchange_loop (rtx target, rtx mem, rtx val)
{
  machine_mode mode = GET_MODE (mem);

  if (can_compare_and_swap_p (mode, true))
    {
      if (!target || !register_operand (target, mode))
	target = gen_reg_rtx (mode);
      if (expand_compare_and_swap_loop (mem, target, val, NULL_RTX))
	return target;
    }

  return NULL_RTX;
}

/* This function tries to implement an atomic test-and-set operation
   using the atomic_test_and_set instruction pattern.  A boolean value
   is returned from the operation, using TARGET if possible.  */

static rtx
maybe_emit_atomic_test_and_set (rtx target, rtx mem, enum memmodel model)
{
  machine_mode pat_bool_mode;
  struct expand_operand ops[3];

  if (!targetm.have_atomic_test_and_set ())
    return NULL_RTX;

  /* While we always get QImode from __atomic_test_and_set, we get
     other memory modes from __sync_lock_test_and_set.  Note that we
     use no endian adjustment here.  This matches the 4.6 behavior
     in the Sparc backend.  */
  enum insn_code icode = targetm.code_for_atomic_test_and_set;
  gcc_checking_assert (insn_data[icode].operand[1].mode == QImode);
  if (GET_MODE (mem) != QImode)
    mem = adjust_address_nv (mem, QImode, 0);

  pat_bool_mode = insn_data[icode].operand[0].mode;
  create_output_operand (&ops[0], target, pat_bool_mode);
  create_fixed_operand (&ops[1], mem);
  create_integer_operand (&ops[2], model);

  if (maybe_expand_insn (icode, 3, ops))
    return ops[0].value;
  return NULL_RTX;
}

/* This function expands the legacy _sync_lock test_and_set operation which is
   generally an atomic exchange.  Some limited targets only allow the
   constant 1 to be stored.  This is an ACQUIRE operation. 

   TARGET is an optional place to stick the return value.  
   MEM is where VAL is stored.  */

rtx
expand_sync_lock_test_and_set (rtx target, rtx mem, rtx val)
{
  rtx ret;

  /* Try an atomic_exchange first.  */
  ret = maybe_emit_atomic_exchange (target, mem, val, MEMMODEL_SYNC_ACQUIRE);
  if (ret)
    return ret;

  ret = maybe_emit_sync_lock_test_and_set (target, mem, val,
					   MEMMODEL_SYNC_ACQUIRE);
  if (ret)
    return ret;

  ret = maybe_emit_compare_and_swap_exchange_loop (target, mem, val);
  if (ret)
    return ret;

  /* If there are no other options, try atomic_test_and_set if the value
     being stored is 1.  */
  if (val == const1_rtx)
    ret = maybe_emit_atomic_test_and_set (target, mem, MEMMODEL_SYNC_ACQUIRE);

  return ret;
}

/* This function expands the atomic test_and_set operation:
   atomically store a boolean TRUE into MEM and return the previous value.

   MEMMODEL is the memory model variant to use.
   TARGET is an optional place to stick the return value.  */

rtx
expand_atomic_test_and_set (rtx target, rtx mem, enum memmodel model)
{
  machine_mode mode = GET_MODE (mem);
  rtx ret, trueval, subtarget;

  ret = maybe_emit_atomic_test_and_set (target, mem, model);
  if (ret)
    return ret;

  /* Be binary compatible with non-default settings of trueval, and different
     cpu revisions.  E.g. one revision may have atomic-test-and-set, but
     another only has atomic-exchange.  */
  if (targetm.atomic_test_and_set_trueval == 1)
    {
      trueval = const1_rtx;
      subtarget = target ? target : gen_reg_rtx (mode);
    }
  else
    {
      trueval = gen_int_mode (targetm.atomic_test_and_set_trueval, mode);
      subtarget = gen_reg_rtx (mode);
    }

  /* Try the atomic-exchange optab...  */
  ret = maybe_emit_atomic_exchange (subtarget, mem, trueval, model);

  /* ... then an atomic-compare-and-swap loop ... */
  if (!ret)
    ret = maybe_emit_compare_and_swap_exchange_loop (subtarget, mem, trueval);

  /* ... before trying the vaguely defined legacy lock_test_and_set. */
  if (!ret)
    ret = maybe_emit_sync_lock_test_and_set (subtarget, mem, trueval, model);

  /* Recall that the legacy lock_test_and_set optab was allowed to do magic
     things with the value 1.  Thus we try again without trueval.  */
  if (!ret && targetm.atomic_test_and_set_trueval != 1)
    ret = maybe_emit_sync_lock_test_and_set (subtarget, mem, const1_rtx, model);

  /* Failing all else, assume a single threaded environment and simply
     perform the operation.  */
  if (!ret)
    {
      /* If the result is ignored skip the move to target.  */
      if (subtarget != const0_rtx)
        emit_move_insn (subtarget, mem);

      emit_move_insn (mem, trueval);
      ret = subtarget;
    }

  /* Recall that have to return a boolean value; rectify if trueval
     is not exactly one.  */
  if (targetm.atomic_test_and_set_trueval != 1)
    ret = emit_store_flag_force (target, NE, ret, const0_rtx, mode, 0, 1);
  
  return ret;
}

/* This function expands the atomic exchange operation:
   atomically store VAL in MEM and return the previous value in MEM.

   MEMMODEL is the memory model variant to use.
   TARGET is an optional place to stick the return value.  */

rtx
expand_atomic_exchange (rtx target, rtx mem, rtx val, enum memmodel model)
{
  machine_mode mode = GET_MODE (mem);
  rtx ret;

  /* If loads are not atomic for the required size and we are not called to
     provide a __sync builtin, do not do anything so that we stay consistent
     with atomic loads of the same size.  */
  if (!can_atomic_load_p (mode) && !is_mm_sync (model))
    return NULL_RTX;

  ret = maybe_emit_atomic_exchange (target, mem, val, model);

  /* Next try a compare-and-swap loop for the exchange.  */
  if (!ret)
    ret = maybe_emit_compare_and_swap_exchange_loop (target, mem, val);

  return ret;
}

/* This function expands the atomic compare exchange operation:

   *PTARGET_BOOL is an optional place to store the boolean success/failure.
   *PTARGET_OVAL is an optional place to store the old value from memory.
   Both target parameters may be NULL or const0_rtx to indicate that we do
   not care about that return value.  Both target parameters are updated on
   success to the actual location of the corresponding result.

   MEMMODEL is the memory model variant to use.

   The return value of the function is true for success.  */

bool
expand_atomic_compare_and_swap (rtx *ptarget_bool, rtx *ptarget_oval,
				rtx mem, rtx expected, rtx desired,
				bool is_weak, enum memmodel succ_model,
				enum memmodel fail_model)
{
  machine_mode mode = GET_MODE (mem);
  struct expand_operand ops[8];
  enum insn_code icode;
  rtx target_oval, target_bool = NULL_RTX;
  rtx libfunc;

  /* If loads are not atomic for the required size and we are not called to
     provide a __sync builtin, do not do anything so that we stay consistent
     with atomic loads of the same size.  */
  if (!can_atomic_load_p (mode) && !is_mm_sync (succ_model))
    return false;

  /* Load expected into a register for the compare and swap.  */
  if (MEM_P (expected))
    expected = copy_to_reg (expected);

  /* Make sure we always have some place to put the return oldval.
     Further, make sure that place is distinct from the input expected,
     just in case we need that path down below.  */
  if (ptarget_oval && *ptarget_oval == const0_rtx)
    ptarget_oval = NULL;

  if (ptarget_oval == NULL
      || (target_oval = *ptarget_oval) == NULL
      || reg_overlap_mentioned_p (expected, target_oval))
    target_oval = gen_reg_rtx (mode);

  icode = direct_optab_handler (atomic_compare_and_swap_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      machine_mode bool_mode = insn_data[icode].operand[0].mode;

      if (ptarget_bool && *ptarget_bool == const0_rtx)
	ptarget_bool = NULL;

      /* Make sure we always have a place for the bool operand.  */
      if (ptarget_bool == NULL
	  || (target_bool = *ptarget_bool) == NULL
	  || GET_MODE (target_bool) != bool_mode)
	target_bool = gen_reg_rtx (bool_mode);

      /* Emit the compare_and_swap.  */
      create_output_operand (&ops[0], target_bool, bool_mode);
      create_output_operand (&ops[1], target_oval, mode);
      create_fixed_operand (&ops[2], mem);
      create_input_operand (&ops[3], expected, mode);
      create_input_operand (&ops[4], desired, mode);
      create_integer_operand (&ops[5], is_weak);
      create_integer_operand (&ops[6], succ_model);
      create_integer_operand (&ops[7], fail_model);
      if (maybe_expand_insn (icode, 8, ops))
	{
	  /* Return success/failure.  */
	  target_bool = ops[0].value;
	  target_oval = ops[1].value;
	  goto success;
	}
    }

  /* Otherwise fall back to the original __sync_val_compare_and_swap
     which is always seq-cst.  */
  icode = optab_handler (sync_compare_and_swap_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      rtx cc_reg;

      create_output_operand (&ops[0], target_oval, mode);
      create_fixed_operand (&ops[1], mem);
      create_input_operand (&ops[2], expected, mode);
      create_input_operand (&ops[3], desired, mode);
      if (!maybe_expand_insn (icode, 4, ops))
	return false;

      target_oval = ops[0].value;

      /* If the caller isn't interested in the boolean return value,
	 skip the computation of it.  */
      if (ptarget_bool == NULL)
	goto success;

      /* Otherwise, work out if the compare-and-swap succeeded.  */
      cc_reg = NULL_RTX;
      if (have_insn_for (COMPARE, CCmode))
	note_stores (PATTERN (get_last_insn ()), find_cc_set, &cc_reg);
      if (cc_reg)
	{
	  target_bool = emit_store_flag_force (target_bool, EQ, cc_reg,
					       const0_rtx, VOIDmode, 0, 1);
	  goto success;
	}
      goto success_bool_from_val;
    }

  /* Also check for library support for __sync_val_compare_and_swap.  */
  libfunc = optab_libfunc (sync_compare_and_swap_optab, mode);
  if (libfunc != NULL)
    {
      rtx addr = convert_memory_address (ptr_mode, XEXP (mem, 0));
      rtx target = emit_library_call_value (libfunc, NULL_RTX, LCT_NORMAL,
					    mode, 3, addr, ptr_mode,
					    expected, mode, desired, mode);
      emit_move_insn (target_oval, target);

      /* Compute the boolean return value only if requested.  */
      if (ptarget_bool)
	goto success_bool_from_val;
      else
	goto success;
    }

  /* Failure.  */
  return false;

 success_bool_from_val:
   target_bool = emit_store_flag_force (target_bool, EQ, target_oval,
					expected, VOIDmode, 1, 1);
 success:
  /* Make sure that the oval output winds up where the caller asked.  */
  if (ptarget_oval)
    *ptarget_oval = target_oval;
  if (ptarget_bool)
    *ptarget_bool = target_bool;
  return true;
}

/* Generate asm volatile("" : : : "memory") as the memory barrier.  */

static void
expand_asm_memory_barrier (void)
{
  rtx asm_op, clob;

  asm_op = gen_rtx_ASM_OPERANDS (VOIDmode, "", "", 0,
				 rtvec_alloc (0), rtvec_alloc (0),
				 rtvec_alloc (0), UNKNOWN_LOCATION);
  MEM_VOLATILE_P (asm_op) = 1;

  clob = gen_rtx_SCRATCH (VOIDmode);
  clob = gen_rtx_MEM (BLKmode, clob);
  clob = gen_rtx_CLOBBER (VOIDmode, clob);

  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, asm_op, clob)));
}

/* This routine will either emit the mem_thread_fence pattern or issue a 
   sync_synchronize to generate a fence for memory model MEMMODEL.  */

void
expand_mem_thread_fence (enum memmodel model)
{
  if (is_mm_relaxed (model))
    return;
  if (targetm.have_mem_thread_fence ())
    {
      emit_insn (targetm.gen_mem_thread_fence (GEN_INT (model)));
      expand_asm_memory_barrier ();
    }
  else if (targetm.have_memory_barrier ())
    emit_insn (targetm.gen_memory_barrier ());
  else if (synchronize_libfunc != NULL_RTX)
    emit_library_call (synchronize_libfunc, LCT_NORMAL, VOIDmode, 0);
  else
    expand_asm_memory_barrier ();
}

/* This routine will either emit the mem_signal_fence pattern or issue a 
   sync_synchronize to generate a fence for memory model MEMMODEL.  */

void
expand_mem_signal_fence (enum memmodel model)
{
  if (targetm.have_mem_signal_fence ())
    emit_insn (targetm.gen_mem_signal_fence (GEN_INT (model)));
  else if (!is_mm_relaxed (model))
    {
      /* By default targets are coherent between a thread and the signal
	 handler running on the same thread.  Thus this really becomes a
	 compiler barrier, in that stores must not be sunk past
	 (or raised above) a given point.  */
      expand_asm_memory_barrier ();
    }
}

/* This function expands the atomic load operation:
   return the atomically loaded value in MEM.

   MEMMODEL is the memory model variant to use.
   TARGET is an option place to stick the return value.  */

rtx
expand_atomic_load (rtx target, rtx mem, enum memmodel model)
{
  machine_mode mode = GET_MODE (mem);
  enum insn_code icode;

  /* If the target supports the load directly, great.  */
  icode = direct_optab_handler (atomic_load_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand ops[3];

      create_output_operand (&ops[0], target, mode);
      create_fixed_operand (&ops[1], mem);
      create_integer_operand (&ops[2], model);
      if (maybe_expand_insn (icode, 3, ops))
	return ops[0].value;
    }

  /* If the size of the object is greater than word size on this target,
     then we assume that a load will not be atomic.  We could try to
     emulate a load with a compare-and-swap operation, but the store that
     doing this could result in would be incorrect if this is a volatile
     atomic load or targetting read-only-mapped memory.  */
  if (GET_MODE_PRECISION (mode) > BITS_PER_WORD)
    /* If there is no atomic load, leave the library call.  */
    return NULL_RTX;

  /* Otherwise assume loads are atomic, and emit the proper barriers.  */
  if (!target || target == const0_rtx)
    target = gen_reg_rtx (mode);

  /* For SEQ_CST, emit a barrier before the load.  */
  if (is_mm_seq_cst (model))
    expand_mem_thread_fence (model);

  emit_move_insn (target, mem);

  /* Emit the appropriate barrier after the load.  */
  expand_mem_thread_fence (model);

  return target;
}

/* This function expands the atomic store operation:
   Atomically store VAL in MEM.
   MEMMODEL is the memory model variant to use.
   USE_RELEASE is true if __sync_lock_release can be used as a fall back.
   function returns const0_rtx if a pattern was emitted.  */

rtx
expand_atomic_store (rtx mem, rtx val, enum memmodel model, bool use_release)
{
  machine_mode mode = GET_MODE (mem);
  enum insn_code icode;
  struct expand_operand ops[3];

  /* If the target supports the store directly, great.  */
  icode = direct_optab_handler (atomic_store_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      create_fixed_operand (&ops[0], mem);
      create_input_operand (&ops[1], val, mode);
      create_integer_operand (&ops[2], model);
      if (maybe_expand_insn (icode, 3, ops))
	return const0_rtx;
    }

  /* If using __sync_lock_release is a viable alternative, try it.
     Note that this will not be set to true if we are expanding a generic
     __atomic_store_n.  */
  if (use_release)
    {
      icode = direct_optab_handler (sync_lock_release_optab, mode);
      if (icode != CODE_FOR_nothing)
	{
	  create_fixed_operand (&ops[0], mem);
	  create_input_operand (&ops[1], const0_rtx, mode);
	  if (maybe_expand_insn (icode, 2, ops))
	    {
	      /* lock_release is only a release barrier.  */
	      if (is_mm_seq_cst (model))
		expand_mem_thread_fence (model);
	      return const0_rtx;
	    }
	}
    }

  /* If the size of the object is greater than word size on this target,
     a default store will not be atomic.  */
  if (GET_MODE_PRECISION (mode) > BITS_PER_WORD)
    {
      /* If loads are atomic or we are called to provide a __sync builtin,
	 we can try a atomic_exchange and throw away the result.  Otherwise,
	 don't do anything so that we do not create an inconsistency between
	 loads and stores.  */
      if (can_atomic_load_p (mode) || is_mm_sync (model))
	{
	  rtx target = maybe_emit_atomic_exchange (NULL_RTX, mem, val, model);
	  if (!target)
	    target = maybe_emit_compare_and_swap_exchange_loop (NULL_RTX, mem,
								val);
	  if (target)
	    return const0_rtx;
	}
        return NULL_RTX;
    }

  /* Otherwise assume stores are atomic, and emit the proper barriers.  */
  expand_mem_thread_fence (model);

  emit_move_insn (mem, val);

  /* For SEQ_CST, also emit a barrier after the store.  */
  if (is_mm_seq_cst (model))
    expand_mem_thread_fence (model);

  return const0_rtx;
}


/* Structure containing the pointers and values required to process the
   various forms of the atomic_fetch_op and atomic_op_fetch builtins.  */

struct atomic_op_functions
{
  direct_optab mem_fetch_before;
  direct_optab mem_fetch_after;
  direct_optab mem_no_result;
  optab fetch_before;
  optab fetch_after;
  direct_optab no_result;
  enum rtx_code reverse_code;
};


/* Fill in structure pointed to by OP with the various optab entries for an 
   operation of type CODE.  */

static void
get_atomic_op_for_code (struct atomic_op_functions *op, enum rtx_code code)
{
  gcc_assert (op!= NULL);

  /* If SWITCHABLE_TARGET is defined, then subtargets can be switched
     in the source code during compilation, and the optab entries are not
     computable until runtime.  Fill in the values at runtime.  */
  switch (code)
    {
    case PLUS:
      op->mem_fetch_before = atomic_fetch_add_optab;
      op->mem_fetch_after = atomic_add_fetch_optab;
      op->mem_no_result = atomic_add_optab;
      op->fetch_before = sync_old_add_optab;
      op->fetch_after = sync_new_add_optab;
      op->no_result = sync_add_optab;
      op->reverse_code = MINUS;
      break;
    case MINUS:
      op->mem_fetch_before = atomic_fetch_sub_optab;
      op->mem_fetch_after = atomic_sub_fetch_optab;
      op->mem_no_result = atomic_sub_optab;
      op->fetch_before = sync_old_sub_optab;
      op->fetch_after = sync_new_sub_optab;
      op->no_result = sync_sub_optab;
      op->reverse_code = PLUS;
      break;
    case XOR:
      op->mem_fetch_before = atomic_fetch_xor_optab;
      op->mem_fetch_after = atomic_xor_fetch_optab;
      op->mem_no_result = atomic_xor_optab;
      op->fetch_before = sync_old_xor_optab;
      op->fetch_after = sync_new_xor_optab;
      op->no_result = sync_xor_optab;
      op->reverse_code = XOR;
      break;
    case AND:
      op->mem_fetch_before = atomic_fetch_and_optab;
      op->mem_fetch_after = atomic_and_fetch_optab;
      op->mem_no_result = atomic_and_optab;
      op->fetch_before = sync_old_and_optab;
      op->fetch_after = sync_new_and_optab;
      op->no_result = sync_and_optab;
      op->reverse_code = UNKNOWN;
      break;
    case IOR:
      op->mem_fetch_before = atomic_fetch_or_optab;
      op->mem_fetch_after = atomic_or_fetch_optab;
      op->mem_no_result = atomic_or_optab;
      op->fetch_before = sync_old_ior_optab;
      op->fetch_after = sync_new_ior_optab;
      op->no_result = sync_ior_optab;
      op->reverse_code = UNKNOWN;
      break;
    case NOT:
      op->mem_fetch_before = atomic_fetch_nand_optab;
      op->mem_fetch_after = atomic_nand_fetch_optab;
      op->mem_no_result = atomic_nand_optab;
      op->fetch_before = sync_old_nand_optab;
      op->fetch_after = sync_new_nand_optab;
      op->no_result = sync_nand_optab;
      op->reverse_code = UNKNOWN;
      break;
    default:
      gcc_unreachable ();
    }
}

/* See if there is a more optimal way to implement the operation "*MEM CODE VAL"
   using memory order MODEL.  If AFTER is true the operation needs to return
   the value of *MEM after the operation, otherwise the previous value.  
   TARGET is an optional place to place the result.  The result is unused if
   it is const0_rtx.
   Return the result if there is a better sequence, otherwise NULL_RTX.  */

static rtx
maybe_optimize_fetch_op (rtx target, rtx mem, rtx val, enum rtx_code code,
			 enum memmodel model, bool after)
{
  /* If the value is prefetched, or not used, it may be possible to replace
     the sequence with a native exchange operation.  */
  if (!after || target == const0_rtx)
    {
      /* fetch_and (&x, 0, m) can be replaced with exchange (&x, 0, m).  */
      if (code == AND && val == const0_rtx)
        {
	  if (target == const0_rtx)
	    target = gen_reg_rtx (GET_MODE (mem));
	  return maybe_emit_atomic_exchange (target, mem, val, model);
	}

      /* fetch_or (&x, -1, m) can be replaced with exchange (&x, -1, m).  */
      if (code == IOR && val == constm1_rtx)
        {
	  if (target == const0_rtx)
	    target = gen_reg_rtx (GET_MODE (mem));
	  return maybe_emit_atomic_exchange (target, mem, val, model);
	}
    }

  return NULL_RTX;
}

/* Try to emit an instruction for a specific operation varaition. 
   OPTAB contains the OP functions.
   TARGET is an optional place to return the result. const0_rtx means unused.
   MEM is the memory location to operate on.
   VAL is the value to use in the operation.
   USE_MEMMODEL is TRUE if the variation with a memory model should be tried.
   MODEL is the memory model, if used.
   AFTER is true if the returned result is the value after the operation.  */

static rtx 
maybe_emit_op (const struct atomic_op_functions *optab, rtx target, rtx mem,
	       rtx val, bool use_memmodel, enum memmodel model, bool after)
{
  machine_mode mode = GET_MODE (mem);
  struct expand_operand ops[4];
  enum insn_code icode;
  int op_counter = 0;
  int num_ops;

  /* Check to see if there is a result returned.  */
  if (target == const0_rtx)
    {
      if (use_memmodel)
        {
	  icode = direct_optab_handler (optab->mem_no_result, mode);
	  create_integer_operand (&ops[2], model);
	  num_ops = 3;
	}
      else
        {
	  icode = direct_optab_handler (optab->no_result, mode);
	  num_ops = 2;
	}
    }
  /* Otherwise, we need to generate a result.  */
  else
    {
      if (use_memmodel)
        {
	  icode = direct_optab_handler (after ? optab->mem_fetch_after
					: optab->mem_fetch_before, mode);
	  create_integer_operand (&ops[3], model);
	  num_ops = 4;
	}
      else
	{
	  icode = optab_handler (after ? optab->fetch_after
				 : optab->fetch_before, mode);
	  num_ops = 3;
	}
      create_output_operand (&ops[op_counter++], target, mode);
    }
  if (icode == CODE_FOR_nothing)
    return NULL_RTX;

  create_fixed_operand (&ops[op_counter++], mem);
  /* VAL may have been promoted to a wider mode.  Shrink it if so.  */
  create_convert_operand_to (&ops[op_counter++], val, mode, true);

  if (maybe_expand_insn (icode, num_ops, ops))
    return (target == const0_rtx ? const0_rtx : ops[0].value);

  return NULL_RTX;
} 


/* This function expands an atomic fetch_OP or OP_fetch operation:
   TARGET is an option place to stick the return value.  const0_rtx indicates
   the result is unused. 
   atomically fetch MEM, perform the operation with VAL and return it to MEM.
   CODE is the operation being performed (OP)
   MEMMODEL is the memory model variant to use.
   AFTER is true to return the result of the operation (OP_fetch).
   AFTER is false to return the value before the operation (fetch_OP).  

   This function will *only* generate instructions if there is a direct
   optab. No compare and swap loops or libcalls will be generated. */

static rtx
expand_atomic_fetch_op_no_fallback (rtx target, rtx mem, rtx val,
				    enum rtx_code code, enum memmodel model,
				    bool after)
{
  machine_mode mode = GET_MODE (mem);
  struct atomic_op_functions optab;
  rtx result;
  bool unused_result = (target == const0_rtx);

  get_atomic_op_for_code (&optab, code);

  /* Check to see if there are any better instructions.  */
  result = maybe_optimize_fetch_op (target, mem, val, code, model, after);
  if (result)
    return result;

  /* Check for the case where the result isn't used and try those patterns.  */
  if (unused_result)
    {
      /* Try the memory model variant first.  */
      result = maybe_emit_op (&optab, target, mem, val, true, model, true);
      if (result)
        return result;

      /* Next try the old style withuot a memory model.  */
      result = maybe_emit_op (&optab, target, mem, val, false, model, true);
      if (result)
        return result;

      /* There is no no-result pattern, so try patterns with a result.  */
      target = NULL_RTX;
    }

  /* Try the __atomic version.  */
  result = maybe_emit_op (&optab, target, mem, val, true, model, after);
  if (result)
    return result;

  /* Try the older __sync version.  */
  result = maybe_emit_op (&optab, target, mem, val, false, model, after);
  if (result)
    return result;

  /* If the fetch value can be calculated from the other variation of fetch,
     try that operation.  */
  if (after || unused_result || optab.reverse_code != UNKNOWN)
    {
      /* Try the __atomic version, then the older __sync version.  */
      result = maybe_emit_op (&optab, target, mem, val, true, model, !after);
      if (!result)
	result = maybe_emit_op (&optab, target, mem, val, false, model, !after);

      if (result)
	{
	  /* If the result isn't used, no need to do compensation code.  */
	  if (unused_result)
	    return result;

	  /* Issue compensation code.  Fetch_after  == fetch_before OP val.
	     Fetch_before == after REVERSE_OP val.  */
	  if (!after)
	    code = optab.reverse_code;
	  if (code == NOT)
	    {
	      result = expand_simple_binop (mode, AND, result, val, NULL_RTX,
					    true, OPTAB_LIB_WIDEN);
	      result = expand_simple_unop (mode, NOT, result, target, true);
	    }
	  else
	    result = expand_simple_binop (mode, code, result, val, target,
					  true, OPTAB_LIB_WIDEN);
	  return result;
	}
    }

  /* No direct opcode can be generated.  */
  return NULL_RTX;
}



/* This function expands an atomic fetch_OP or OP_fetch operation:
   TARGET is an option place to stick the return value.  const0_rtx indicates
   the result is unused. 
   atomically fetch MEM, perform the operation with VAL and return it to MEM.
   CODE is the operation being performed (OP)
   MEMMODEL is the memory model variant to use.
   AFTER is true to return the result of the operation (OP_fetch).
   AFTER is false to return the value before the operation (fetch_OP).  */
rtx
expand_atomic_fetch_op (rtx target, rtx mem, rtx val, enum rtx_code code,
			enum memmodel model, bool after)
{
  machine_mode mode = GET_MODE (mem);
  rtx result;
  bool unused_result = (target == const0_rtx);

  /* If loads are not atomic for the required size and we are not called to
     provide a __sync builtin, do not do anything so that we stay consistent
     with atomic loads of the same size.  */
  if (!can_atomic_load_p (mode) && !is_mm_sync (model))
    return NULL_RTX;

  result = expand_atomic_fetch_op_no_fallback (target, mem, val, code, model,
					       after);
  
  if (result)
    return result;

  /* Add/sub can be implemented by doing the reverse operation with -(val).  */
  if (code == PLUS || code == MINUS)
    {
      rtx tmp;
      enum rtx_code reverse = (code == PLUS ? MINUS : PLUS);

      start_sequence ();
      tmp = expand_simple_unop (mode, NEG, val, NULL_RTX, true);
      result = expand_atomic_fetch_op_no_fallback (target, mem, tmp, reverse,
						   model, after);
      if (result)
	{
	  /* PLUS worked so emit the insns and return.  */
	  tmp = get_insns ();
	  end_sequence ();
	  emit_insn (tmp);
          return result;
	}

      /* PLUS did not work, so throw away the negation code and continue.  */
      end_sequence ();
    }

  /* Try the __sync libcalls only if we can't do compare-and-swap inline.  */
  if (!can_compare_and_swap_p (mode, false))
    {
      rtx libfunc;
      bool fixup = false;
      enum rtx_code orig_code = code;
      struct atomic_op_functions optab;

      get_atomic_op_for_code (&optab, code);
      libfunc = optab_libfunc (after ? optab.fetch_after
			       : optab.fetch_before, mode);
      if (libfunc == NULL
	  && (after || unused_result || optab.reverse_code != UNKNOWN))
	{
	  fixup = true;
	  if (!after)
	    code = optab.reverse_code;
	  libfunc = optab_libfunc (after ? optab.fetch_before
				   : optab.fetch_after, mode);
	}
      if (libfunc != NULL)
	{
	  rtx addr = convert_memory_address (ptr_mode, XEXP (mem, 0));
	  result = emit_library_call_value (libfunc, NULL, LCT_NORMAL, mode,
					    2, addr, ptr_mode, val, mode);

	  if (!unused_result && fixup)
	    result = expand_simple_binop (mode, code, result, val, target,
					  true, OPTAB_LIB_WIDEN);
	  return result;
	}

      /* We need the original code for any further attempts.  */
      code = orig_code;
    }

  /* If nothing else has succeeded, default to a compare and swap loop.  */
  if (can_compare_and_swap_p (mode, true))
    {
      rtx_insn *insn;
      rtx t0 = gen_reg_rtx (mode), t1;

      start_sequence ();

      /* If the result is used, get a register for it.  */
      if (!unused_result) 
        {
	  if (!target || !register_operand (target, mode))
	    target = gen_reg_rtx (mode);
	  /* If fetch_before, copy the value now.  */
	  if (!after)
	    emit_move_insn (target, t0);
	}
      else
        target = const0_rtx;

      t1 = t0;
      if (code == NOT)
        {
	  t1 = expand_simple_binop (mode, AND, t1, val, NULL_RTX,
				    true, OPTAB_LIB_WIDEN);
	  t1 = expand_simple_unop (mode, code, t1, NULL_RTX, true);
	}
      else
	t1 = expand_simple_binop (mode, code, t1, val, NULL_RTX, true, 
				  OPTAB_LIB_WIDEN);

      /* For after, copy the value now.  */
      if (!unused_result && after)
        emit_move_insn (target, t1);
      insn = get_insns ();
      end_sequence ();

      if (t1 != NULL && expand_compare_and_swap_loop (mem, t0, t1, insn))
        return target;
    }

  return NULL_RTX;
}

/* Return true if OPERAND is suitable for operand number OPNO of
   instruction ICODE.  */

bool
insn_operand_matches (enum insn_code icode, unsigned int opno, rtx operand)
{
  return (!insn_data[(int) icode].operand[opno].predicate
	  || (insn_data[(int) icode].operand[opno].predicate
	      (operand, insn_data[(int) icode].operand[opno].mode)));
}

/* TARGET is a target of a multiword operation that we are going to
   implement as a series of word-mode operations.  Return true if
   TARGET is suitable for this purpose.  */

bool
valid_multiword_target_p (rtx target)
{
  machine_mode mode;
  int i;

  mode = GET_MODE (target);
  for (i = 0; i < GET_MODE_SIZE (mode); i += UNITS_PER_WORD)
    if (!validate_subreg (word_mode, mode, target, i))
      return false;
  return true;
}

/* Like maybe_legitimize_operand, but do not change the code of the
   current rtx value.  */

static bool
maybe_legitimize_operand_same_code (enum insn_code icode, unsigned int opno,
				    struct expand_operand *op)
{
  /* See if the operand matches in its current form.  */
  if (insn_operand_matches (icode, opno, op->value))
    return true;

  /* If the operand is a memory whose address has no side effects,
     try forcing the address into a non-virtual pseudo register.
     The check for side effects is important because copy_to_mode_reg
     cannot handle things like auto-modified addresses.  */
  if (insn_data[(int) icode].operand[opno].allows_mem && MEM_P (op->value))
    {
      rtx addr, mem;

      mem = op->value;
      addr = XEXP (mem, 0);
      if (!(REG_P (addr) && REGNO (addr) > LAST_VIRTUAL_REGISTER)
	  && !side_effects_p (addr))
	{
	  rtx_insn *last;
	  machine_mode mode;

	  last = get_last_insn ();
	  mode = get_address_mode (mem);
	  mem = replace_equiv_address (mem, copy_to_mode_reg (mode, addr));
	  if (insn_operand_matches (icode, opno, mem))
	    {
	      op->value = mem;
	      return true;
	    }
	  delete_insns_since (last);
	}
    }

  return false;
}

/* Try to make OP match operand OPNO of instruction ICODE.  Return true
   on success, storing the new operand value back in OP.  */

static bool
maybe_legitimize_operand (enum insn_code icode, unsigned int opno,
			  struct expand_operand *op)
{
  machine_mode mode, imode;
  bool old_volatile_ok, result;

  mode = op->mode;
  switch (op->type)
    {
    case EXPAND_FIXED:
      old_volatile_ok = volatile_ok;
      volatile_ok = true;
      result = maybe_legitimize_operand_same_code (icode, opno, op);
      volatile_ok = old_volatile_ok;
      return result;

    case EXPAND_OUTPUT:
      gcc_assert (mode != VOIDmode);
      if (op->value
	  && op->value != const0_rtx
	  && GET_MODE (op->value) == mode
	  && maybe_legitimize_operand_same_code (icode, opno, op))
	return true;

      op->value = gen_reg_rtx (mode);
      op->target = 0;
      break;

    case EXPAND_INPUT:
    input:
      gcc_assert (mode != VOIDmode);
      gcc_assert (GET_MODE (op->value) == VOIDmode
		  || GET_MODE (op->value) == mode);
      if (maybe_legitimize_operand_same_code (icode, opno, op))
	return true;

      op->value = copy_to_mode_reg (mode, op->value);
      break;

    case EXPAND_CONVERT_TO:
      gcc_assert (mode != VOIDmode);
      op->value = convert_to_mode (mode, op->value, op->unsigned_p);
      goto input;

    case EXPAND_CONVERT_FROM:
      if (GET_MODE (op->value) != VOIDmode)
	mode = GET_MODE (op->value);
      else
	/* The caller must tell us what mode this value has.  */
	gcc_assert (mode != VOIDmode);

      imode = insn_data[(int) icode].operand[opno].mode;
      if (imode != VOIDmode && imode != mode)
	{
	  op->value = convert_modes (imode, mode, op->value, op->unsigned_p);
	  mode = imode;
	}
      goto input;

    case EXPAND_ADDRESS:
      op->value = convert_memory_address (as_a <scalar_int_mode> (mode),
					  op->value);
      goto input;

    case EXPAND_INTEGER:
      mode = insn_data[(int) icode].operand[opno].mode;
      if (mode != VOIDmode && const_int_operand (op->value, mode))
	goto input;
      break;
    }
  return insn_operand_matches (icode, opno, op->value);
}

/* Make OP describe an input operand that should have the same value
   as VALUE, after any mode conversion that the target might request.
   TYPE is the type of VALUE.  */

void
create_convert_operand_from_type (struct expand_operand *op,
				  rtx value, tree type)
{
  create_convert_operand_from (op, value, TYPE_MODE (type),
			       TYPE_UNSIGNED (type));
}

/* Try to make operands [OPS, OPS + NOPS) match operands [OPNO, OPNO + NOPS)
   of instruction ICODE.  Return true on success, leaving the new operand
   values in the OPS themselves.  Emit no code on failure.  */

bool
maybe_legitimize_operands (enum insn_code icode, unsigned int opno,
			   unsigned int nops, struct expand_operand *ops)
{
  rtx_insn *last;
  unsigned int i;

  last = get_last_insn ();
  for (i = 0; i < nops; i++)
    if (!maybe_legitimize_operand (icode, opno + i, &ops[i]))
      {
	delete_insns_since (last);
	return false;
      }
  return true;
}

/* Try to generate instruction ICODE, using operands [OPS, OPS + NOPS)
   as its operands.  Return the instruction pattern on success,
   and emit any necessary set-up code.  Return null and emit no
   code on failure.  */

rtx_insn *
maybe_gen_insn (enum insn_code icode, unsigned int nops,
		struct expand_operand *ops)
{
  gcc_assert (nops == (unsigned int) insn_data[(int) icode].n_generator_args);
  if (!maybe_legitimize_operands (icode, 0, nops, ops))
    return NULL;

  switch (nops)
    {
    case 1:
      return GEN_FCN (icode) (ops[0].value);
    case 2:
      return GEN_FCN (icode) (ops[0].value, ops[1].value);
    case 3:
      return GEN_FCN (icode) (ops[0].value, ops[1].value, ops[2].value);
    case 4:
      return GEN_FCN (icode) (ops[0].value, ops[1].value, ops[2].value,
			      ops[3].value);
    case 5:
      return GEN_FCN (icode) (ops[0].value, ops[1].value, ops[2].value,
			      ops[3].value, ops[4].value);
    case 6:
      return GEN_FCN (icode) (ops[0].value, ops[1].value, ops[2].value,
			      ops[3].value, ops[4].value, ops[5].value);
    case 7:
      return GEN_FCN (icode) (ops[0].value, ops[1].value, ops[2].value,
			      ops[3].value, ops[4].value, ops[5].value,
			      ops[6].value);
    case 8:
      return GEN_FCN (icode) (ops[0].value, ops[1].value, ops[2].value,
			      ops[3].value, ops[4].value, ops[5].value,
			      ops[6].value, ops[7].value);
    case 9:
      return GEN_FCN (icode) (ops[0].value, ops[1].value, ops[2].value,
			      ops[3].value, ops[4].value, ops[5].value,
			      ops[6].value, ops[7].value, ops[8].value);
    }
  gcc_unreachable ();
}

/* Try to emit instruction ICODE, using operands [OPS, OPS + NOPS)
   as its operands.  Return true on success and emit no code on failure.  */

bool
maybe_expand_insn (enum insn_code icode, unsigned int nops,
		   struct expand_operand *ops)
{
  rtx_insn *pat = maybe_gen_insn (icode, nops, ops);
  if (pat)
    {
      emit_insn (pat);
      return true;
    }
  return false;
}

/* Like maybe_expand_insn, but for jumps.  */

bool
maybe_expand_jump_insn (enum insn_code icode, unsigned int nops,
			struct expand_operand *ops)
{
  rtx_insn *pat = maybe_gen_insn (icode, nops, ops);
  if (pat)
    {
      emit_jump_insn (pat);
      return true;
    }
  return false;
}

/* Emit instruction ICODE, using operands [OPS, OPS + NOPS)
   as its operands.  */

void
expand_insn (enum insn_code icode, unsigned int nops,
	     struct expand_operand *ops)
{
  if (!maybe_expand_insn (icode, nops, ops))
    gcc_unreachable ();
}

/* Like expand_insn, but for jumps.  */

void
expand_jump_insn (enum insn_code icode, unsigned int nops,
		  struct expand_operand *ops)
{
  if (!maybe_expand_jump_insn (icode, nops, ops))
    gcc_unreachable ();
}
