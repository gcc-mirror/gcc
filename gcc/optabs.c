/* Expand the basic unary and binary arithmetic operations, for GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "toplev.h"

/* Include insn-config.h before expr.h so that HAVE_conditional_move
   is properly defined.  */
#include "insn-config.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "function.h"
#include "except.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "recog.h"
#include "reload.h"
#include "ggc.h"
#include "real.h"
#include "basic-block.h"
#include "target.h"

/* Each optab contains info on how this target machine
   can perform a particular operation
   for all sizes and kinds of operands.

   The operation to be performed is often specified
   by passing one of these optabs as an argument.

   See expr.h for documentation of these optabs.  */

optab optab_table[OTI_MAX];

rtx libfunc_table[LTI_MAX];

/* Tables of patterns for converting one mode to another.  */
convert_optab convert_optab_table[CTI_MAX];

/* Contains the optab used for each rtx code.  */
optab code_to_optab[NUM_RTX_CODE + 1];

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the gen_function to make a branch to test that condition.  */

rtxfun bcc_gen_fctn[NUM_RTX_CODE];

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the insn code to make a store-condition insn
   to test that condition.  */

enum insn_code setcc_gen_code[NUM_RTX_CODE];

#ifdef HAVE_conditional_move
/* Indexed by the machine mode, gives the insn code to make a conditional
   move insn.  This is not indexed by the rtx-code like bcc_gen_fctn and
   setcc_gen_code to cut down on the number of named patterns.  Consider a day
   when a lot more rtx codes are conditional (eg: for the ARM).  */

enum insn_code movcc_gen_code[NUM_MACHINE_MODES];
#endif

/* The insn generating function can not take an rtx_code argument.
   TRAP_RTX is used as an rtx argument.  Its code is replaced with
   the code to be used in the trap insn and all other fields are ignored.  */
static GTY(()) rtx trap_rtx;

static int add_equal_note (rtx, rtx, enum rtx_code, rtx, rtx);
static rtx widen_operand (rtx, enum machine_mode, enum machine_mode, int,
			  int);
static int expand_cmplxdiv_straight (rtx, rtx, rtx, rtx, rtx, rtx,
				     enum machine_mode, int,
				     enum optab_methods, enum mode_class,
				     optab);
static int expand_cmplxdiv_wide (rtx, rtx, rtx, rtx, rtx, rtx,
				 enum machine_mode, int, enum optab_methods,
				 enum mode_class, optab);
static void prepare_cmp_insn (rtx *, rtx *, enum rtx_code *, rtx,
			      enum machine_mode *, int *,
			      enum can_compare_purpose);
static enum insn_code can_fix_p (enum machine_mode, enum machine_mode, int,
				 int *);
static enum insn_code can_float_p (enum machine_mode, enum machine_mode, int);
static rtx ftruncify (rtx);
static optab new_optab (void);
static convert_optab new_convert_optab (void);
static inline optab init_optab (enum rtx_code);
static inline optab init_optabv (enum rtx_code);
static inline convert_optab init_convert_optab (enum rtx_code);
static void init_libfuncs (optab, int, int, const char *, int);
static void init_integral_libfuncs (optab, const char *, int);
static void init_floating_libfuncs (optab, const char *, int);
static void init_interclass_conv_libfuncs (convert_optab, const char *,
					   enum mode_class, enum mode_class);
static void init_intraclass_conv_libfuncs (convert_optab, const char *,
					   enum mode_class, bool);
static void emit_cmp_and_jump_insn_1 (rtx, rtx, enum machine_mode,
				      enum rtx_code, int, rtx);
static void prepare_float_lib_cmp (rtx *, rtx *, enum rtx_code *,
				   enum machine_mode *, int *);
static rtx expand_vector_binop (enum machine_mode, optab, rtx, rtx, rtx, int,
				enum optab_methods);
static rtx expand_vector_unop (enum machine_mode, optab, rtx, rtx, int);
static rtx widen_clz (enum machine_mode, rtx, rtx);
static rtx expand_parity (enum machine_mode, rtx, rtx);

#ifndef HAVE_conditional_trap
#define HAVE_conditional_trap 0
#define gen_conditional_trap(a,b) (abort (), NULL_RTX)
#endif

/* Add a REG_EQUAL note to the last insn in INSNS.  TARGET is being set to
   the result of operation CODE applied to OP0 (and OP1 if it is a binary
   operation).

   If the last insn does not set TARGET, don't do anything, but return 1.

   If a previous insn sets TARGET and TARGET is one of OP0 or OP1,
   don't add the REG_EQUAL note but return 0.  Our caller can then try
   again, ensuring that TARGET is not one of the operands.  */

static int
add_equal_note (rtx insns, rtx target, enum rtx_code code, rtx op0, rtx op1)
{
  rtx last_insn, insn, set;
  rtx note;

  if (! insns
      || ! INSN_P (insns)
      || NEXT_INSN (insns) == NULL_RTX)
    abort ();

  if (GET_RTX_CLASS (code) != '1' && GET_RTX_CLASS (code) != '2'
      && GET_RTX_CLASS (code) != 'c' && GET_RTX_CLASS (code) != '<')
    return 1;

  if (GET_CODE (target) == ZERO_EXTRACT)
    return 1;

  for (last_insn = insns;
       NEXT_INSN (last_insn) != NULL_RTX;
       last_insn = NEXT_INSN (last_insn))
    ;

  set = single_set (last_insn);
  if (set == NULL_RTX)
    return 1;

  if (! rtx_equal_p (SET_DEST (set), target)
      /* For a STRICT_LOW_PART, the REG_NOTE applies to what is inside it.  */
      && (GET_CODE (SET_DEST (set)) != STRICT_LOW_PART
	  || ! rtx_equal_p (XEXP (SET_DEST (set), 0), target)))
    return 1;

  /* If TARGET is in OP0 or OP1, check if anything in SEQ sets TARGET
     besides the last insn.  */
  if (reg_overlap_mentioned_p (target, op0)
      || (op1 && reg_overlap_mentioned_p (target, op1)))
    {
      insn = PREV_INSN (last_insn);
      while (insn != NULL_RTX)
	{
	  if (reg_set_p (target, insn))
	    return 0;

	  insn = PREV_INSN (insn);
	}
    }

  if (GET_RTX_CLASS (code) == '1')
    note = gen_rtx_fmt_e (code, GET_MODE (target), copy_rtx (op0));
  else
    note = gen_rtx_fmt_ee (code, GET_MODE (target), copy_rtx (op0), copy_rtx (op1));

  set_unique_reg_note (last_insn, REG_EQUAL, note);

  return 1;
}

/* Widen OP to MODE and return the rtx for the widened operand.  UNSIGNEDP
   says whether OP is signed or unsigned.  NO_EXTEND is nonzero if we need
   not actually do a sign-extend or zero-extend, but can leave the
   higher-order bits of the result rtx undefined, for example, in the case
   of logical operations, but not right shifts.  */

static rtx
widen_operand (rtx op, enum machine_mode mode, enum machine_mode oldmode,
	       int unsignedp, int no_extend)
{
  rtx result;

  /* If we don't have to extend and this is a constant, return it.  */
  if (no_extend && GET_MODE (op) == VOIDmode)
    return op;

  /* If we must extend do so.  If OP is a SUBREG for a promoted object, also
     extend since it will be more efficient to do so unless the signedness of
     a promoted object differs from our extension.  */
  if (! no_extend
      || (GET_CODE (op) == SUBREG && SUBREG_PROMOTED_VAR_P (op)
	  && SUBREG_PROMOTED_UNSIGNED_P (op) == unsignedp))
    return convert_modes (mode, oldmode, op, unsignedp);

  /* If MODE is no wider than a single word, we return a paradoxical
     SUBREG.  */
  if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
    return gen_rtx_SUBREG (mode, force_reg (GET_MODE (op), op), 0);

  /* Otherwise, get an object of MODE, clobber it, and set the low-order
     part to OP.  */

  result = gen_reg_rtx (mode);
  emit_insn (gen_rtx_CLOBBER (VOIDmode, result));
  emit_move_insn (gen_lowpart (GET_MODE (op), result), op);
  return result;
}

/* Generate code to perform a straightforward complex divide.  */

static int
expand_cmplxdiv_straight (rtx real0, rtx real1, rtx imag0, rtx imag1,
			  rtx realr, rtx imagr, enum machine_mode submode,
			  int unsignedp, enum optab_methods methods,
			  enum mode_class class, optab binoptab)
{
  rtx divisor;
  rtx real_t, imag_t;
  rtx temp1, temp2;
  rtx res;
  optab this_add_optab = add_optab;
  optab this_sub_optab = sub_optab;
  optab this_neg_optab = neg_optab;
  optab this_mul_optab = smul_optab;

  if (binoptab == sdivv_optab)
    {
      this_add_optab = addv_optab;
      this_sub_optab = subv_optab;
      this_neg_optab = negv_optab;
      this_mul_optab = smulv_optab;
    }

  /* Don't fetch these from memory more than once.  */
  real0 = force_reg (submode, real0);
  real1 = force_reg (submode, real1);

  if (imag0 != 0)
    imag0 = force_reg (submode, imag0);

  imag1 = force_reg (submode, imag1);

  /* Divisor: c*c + d*d.  */
  temp1 = expand_binop (submode, this_mul_optab, real1, real1,
			NULL_RTX, unsignedp, methods);

  temp2 = expand_binop (submode, this_mul_optab, imag1, imag1,
			NULL_RTX, unsignedp, methods);

  if (temp1 == 0 || temp2 == 0)
    return 0;

  divisor = expand_binop (submode, this_add_optab, temp1, temp2,
			  NULL_RTX, unsignedp, methods);
  if (divisor == 0)
    return 0;

  if (imag0 == 0)
    {
      /* Mathematically, ((a)(c-id))/divisor.  */
      /* Computationally, (a+i0) / (c+id) = (ac/(cc+dd)) + i(-ad/(cc+dd)).  */

      /* Calculate the dividend.  */
      real_t = expand_binop (submode, this_mul_optab, real0, real1,
			     NULL_RTX, unsignedp, methods);

      imag_t = expand_binop (submode, this_mul_optab, real0, imag1,
			     NULL_RTX, unsignedp, methods);

      if (real_t == 0 || imag_t == 0)
	return 0;

      imag_t = expand_unop (submode, this_neg_optab, imag_t,
			    NULL_RTX, unsignedp);
    }
  else
    {
      /* Mathematically, ((a+ib)(c-id))/divider.  */
      /* Calculate the dividend.  */
      temp1 = expand_binop (submode, this_mul_optab, real0, real1,
			    NULL_RTX, unsignedp, methods);

      temp2 = expand_binop (submode, this_mul_optab, imag0, imag1,
			    NULL_RTX, unsignedp, methods);

      if (temp1 == 0 || temp2 == 0)
	return 0;

      real_t = expand_binop (submode, this_add_optab, temp1, temp2,
			     NULL_RTX, unsignedp, methods);

      temp1 = expand_binop (submode, this_mul_optab, imag0, real1,
			    NULL_RTX, unsignedp, methods);

      temp2 = expand_binop (submode, this_mul_optab, real0, imag1,
			    NULL_RTX, unsignedp, methods);

      if (temp1 == 0 || temp2 == 0)
	return 0;

      imag_t = expand_binop (submode, this_sub_optab, temp1, temp2,
			     NULL_RTX, unsignedp, methods);

      if (real_t == 0 || imag_t == 0)
	return 0;
    }

  if (class == MODE_COMPLEX_FLOAT)
    res = expand_binop (submode, binoptab, real_t, divisor,
			realr, unsignedp, methods);
  else
    res = expand_divmod (0, TRUNC_DIV_EXPR, submode,
			 real_t, divisor, realr, unsignedp);

  if (res == 0)
    return 0;

  if (res != realr)
    emit_move_insn (realr, res);

  if (class == MODE_COMPLEX_FLOAT)
    res = expand_binop (submode, binoptab, imag_t, divisor,
			imagr, unsignedp, methods);
  else
    res = expand_divmod (0, TRUNC_DIV_EXPR, submode,
			 imag_t, divisor, imagr, unsignedp);

  if (res == 0)
    return 0;

  if (res != imagr)
    emit_move_insn (imagr, res);

  return 1;
}

/* Generate code to perform a wide-input-range-acceptable complex divide.  */

static int
expand_cmplxdiv_wide (rtx real0, rtx real1, rtx imag0, rtx imag1, rtx realr,
		      rtx imagr, enum machine_mode submode, int unsignedp,
		      enum optab_methods methods, enum mode_class class,
		      optab binoptab)
{
  rtx ratio, divisor;
  rtx real_t, imag_t;
  rtx temp1, temp2, lab1, lab2;
  enum machine_mode mode;
  rtx res;
  optab this_add_optab = add_optab;
  optab this_sub_optab = sub_optab;
  optab this_neg_optab = neg_optab;
  optab this_mul_optab = smul_optab;

  if (binoptab == sdivv_optab)
    {
      this_add_optab = addv_optab;
      this_sub_optab = subv_optab;
      this_neg_optab = negv_optab;
      this_mul_optab = smulv_optab;
    }

  /* Don't fetch these from memory more than once.  */
  real0 = force_reg (submode, real0);
  real1 = force_reg (submode, real1);

  if (imag0 != 0)
    imag0 = force_reg (submode, imag0);

  imag1 = force_reg (submode, imag1);

  /* XXX What's an "unsigned" complex number?  */
  if (unsignedp)
    {
      temp1 = real1;
      temp2 = imag1;
    }
  else
    {
      temp1 = expand_abs (submode, real1, NULL_RTX, unsignedp, 1);
      temp2 = expand_abs (submode, imag1, NULL_RTX, unsignedp, 1);
    }

  if (temp1 == 0 || temp2 == 0)
    return 0;

  mode = GET_MODE (temp1);
  lab1 = gen_label_rtx ();
  emit_cmp_and_jump_insns (temp1, temp2, LT, NULL_RTX,
			   mode, unsignedp, lab1);

  /* |c| >= |d|; use ratio d/c to scale dividend and divisor.  */

  if (class == MODE_COMPLEX_FLOAT)
    ratio = expand_binop (submode, binoptab, imag1, real1,
			  NULL_RTX, unsignedp, methods);
  else
    ratio = expand_divmod (0, TRUNC_DIV_EXPR, submode,
			   imag1, real1, NULL_RTX, unsignedp);

  if (ratio == 0)
    return 0;

  /* Calculate divisor.  */

  temp1 = expand_binop (submode, this_mul_optab, imag1, ratio,
			NULL_RTX, unsignedp, methods);

  if (temp1 == 0)
    return 0;

  divisor = expand_binop (submode, this_add_optab, temp1, real1,
			  NULL_RTX, unsignedp, methods);

  if (divisor == 0)
    return 0;

  /* Calculate dividend.  */

  if (imag0 == 0)
    {
      real_t = real0;

      /* Compute a / (c+id) as a / (c+d(d/c)) + i (-a(d/c)) / (c+d(d/c)).  */

      imag_t = expand_binop (submode, this_mul_optab, real0, ratio,
			     NULL_RTX, unsignedp, methods);

      if (imag_t == 0)
	return 0;

      imag_t = expand_unop (submode, this_neg_optab, imag_t,
			    NULL_RTX, unsignedp);

      if (real_t == 0 || imag_t == 0)
	return 0;
    }
  else
    {
      /* Compute (a+ib)/(c+id) as
	 (a+b(d/c))/(c+d(d/c) + i(b-a(d/c))/(c+d(d/c)).  */

      temp1 = expand_binop (submode, this_mul_optab, imag0, ratio,
			    NULL_RTX, unsignedp, methods);

      if (temp1 == 0)
	return 0;

      real_t = expand_binop (submode, this_add_optab, temp1, real0,
			     NULL_RTX, unsignedp, methods);

      temp1 = expand_binop (submode, this_mul_optab, real0, ratio,
			    NULL_RTX, unsignedp, methods);

      if (temp1 == 0)
	return 0;

      imag_t = expand_binop (submode, this_sub_optab, imag0, temp1,
			     NULL_RTX, unsignedp, methods);

      if (real_t == 0 || imag_t == 0)
	return 0;
    }

  if (class == MODE_COMPLEX_FLOAT)
    res = expand_binop (submode, binoptab, real_t, divisor,
			realr, unsignedp, methods);
  else
    res = expand_divmod (0, TRUNC_DIV_EXPR, submode,
			 real_t, divisor, realr, unsignedp);

  if (res == 0)
    return 0;

  if (res != realr)
    emit_move_insn (realr, res);

  if (class == MODE_COMPLEX_FLOAT)
    res = expand_binop (submode, binoptab, imag_t, divisor,
			imagr, unsignedp, methods);
  else
    res = expand_divmod (0, TRUNC_DIV_EXPR, submode,
			 imag_t, divisor, imagr, unsignedp);

  if (res == 0)
    return 0;

  if (res != imagr)
    emit_move_insn (imagr, res);

  lab2 = gen_label_rtx ();
  emit_jump_insn (gen_jump (lab2));
  emit_barrier ();

  emit_label (lab1);

  /* |d| > |c|; use ratio c/d to scale dividend and divisor.  */

  if (class == MODE_COMPLEX_FLOAT)
    ratio = expand_binop (submode, binoptab, real1, imag1,
			  NULL_RTX, unsignedp, methods);
  else
    ratio = expand_divmod (0, TRUNC_DIV_EXPR, submode,
			   real1, imag1, NULL_RTX, unsignedp);

  if (ratio == 0)
    return 0;

  /* Calculate divisor.  */

  temp1 = expand_binop (submode, this_mul_optab, real1, ratio,
			NULL_RTX, unsignedp, methods);

  if (temp1 == 0)
    return 0;

  divisor = expand_binop (submode, this_add_optab, temp1, imag1,
			  NULL_RTX, unsignedp, methods);

  if (divisor == 0)
    return 0;

  /* Calculate dividend.  */

  if (imag0 == 0)
    {
      /* Compute a / (c+id) as a(c/d) / (c(c/d)+d) + i (-a) / (c(c/d)+d).  */

      real_t = expand_binop (submode, this_mul_optab, real0, ratio,
			     NULL_RTX, unsignedp, methods);

      imag_t = expand_unop (submode, this_neg_optab, real0,
			    NULL_RTX, unsignedp);

      if (real_t == 0 || imag_t == 0)
	return 0;
    }
  else
    {
      /* Compute (a+ib)/(c+id) as
	 (a(c/d)+b)/(c(c/d)+d) + i (b(c/d)-a)/(c(c/d)+d).  */

      temp1 = expand_binop (submode, this_mul_optab, real0, ratio,
			    NULL_RTX, unsignedp, methods);

      if (temp1 == 0)
	return 0;

      real_t = expand_binop (submode, this_add_optab, temp1, imag0,
			     NULL_RTX, unsignedp, methods);

      temp1 = expand_binop (submode, this_mul_optab, imag0, ratio,
			    NULL_RTX, unsignedp, methods);

      if (temp1 == 0)
	return 0;

      imag_t = expand_binop (submode, this_sub_optab, temp1, real0,
			     NULL_RTX, unsignedp, methods);

      if (real_t == 0 || imag_t == 0)
	return 0;
    }

  if (class == MODE_COMPLEX_FLOAT)
    res = expand_binop (submode, binoptab, real_t, divisor,
			realr, unsignedp, methods);
  else
    res = expand_divmod (0, TRUNC_DIV_EXPR, submode,
			 real_t, divisor, realr, unsignedp);

  if (res == 0)
    return 0;

  if (res != realr)
    emit_move_insn (realr, res);

  if (class == MODE_COMPLEX_FLOAT)
    res = expand_binop (submode, binoptab, imag_t, divisor,
			imagr, unsignedp, methods);
  else
    res = expand_divmod (0, TRUNC_DIV_EXPR, submode,
			 imag_t, divisor, imagr, unsignedp);

  if (res == 0)
    return 0;

  if (res != imagr)
    emit_move_insn (imagr, res);

  emit_label (lab2);

  return 1;
}

/* Wrapper around expand_binop which takes an rtx code to specify
   the operation to perform, not an optab pointer.  All other
   arguments are the same.  */
rtx
expand_simple_binop (enum machine_mode mode, enum rtx_code code, rtx op0,
		     rtx op1, rtx target, int unsignedp,
		     enum optab_methods methods)
{
  optab binop = code_to_optab[(int) code];
  if (binop == 0)
    abort ();

  return expand_binop (mode, binop, op0, op1, target, unsignedp, methods);
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
expand_binop (enum machine_mode mode, optab binoptab, rtx op0, rtx op1,
	      rtx target, int unsignedp, enum optab_methods methods)
{
  enum optab_methods next_methods
    = (methods == OPTAB_LIB || methods == OPTAB_LIB_WIDEN
       ? OPTAB_WIDEN : methods);
  enum mode_class class;
  enum machine_mode wider_mode;
  rtx temp;
  int commutative_op = 0;
  int shift_op = (binoptab->code == ASHIFT
		  || binoptab->code == ASHIFTRT
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
      /* Load duplicate non-volatile operands once.  */
      if (rtx_equal_p (op0, op1) && ! volatile_refs_p (op0))
	{
	  op0 = force_not_mem (op0);
	  op1 = op0;
	}
      else
	{
	  op0 = force_not_mem (op0);
	  op1 = force_not_mem (op1);
	}
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
      && rtx_cost (op0, binoptab->code) > COSTS_N_INSNS (1))
    op0 = force_reg (mode, op0);

  if (CONSTANT_P (op1) && preserve_subexpressions_p ()
      && ! shift_op && rtx_cost (op1, binoptab->code) > COSTS_N_INSNS (1))
    op1 = force_reg (mode, op1);

  /* Record where to delete back to if we backtrack.  */
  last = get_last_insn ();

  /* If operation is commutative,
     try to make the first operand a register.
     Even better, try to make it the same as the target.
     Also try to make the last operand a constant.  */
  if (GET_RTX_CLASS (binoptab->code) == 'c'
      || binoptab == smul_widen_optab
      || binoptab == umul_widen_optab
      || binoptab == smul_highpart_optab
      || binoptab == umul_highpart_optab)
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
      enum machine_mode mode0 = insn_data[icode].operand[1].mode;
      enum machine_mode mode1 = insn_data[icode].operand[2].mode;
      rtx pat;
      rtx xop0 = op0, xop1 = op1;

      if (target)
	temp = target;
      else
	temp = gen_reg_rtx (mode);

      /* If it is a commutative operator and the modes would match
	 if we would swap the operands, we can save the conversions.  */
      if (commutative_op)
	{
	  if (GET_MODE (op0) != mode0 && GET_MODE (op1) != mode1
	      && GET_MODE (op0) == mode1 && GET_MODE (op1) == mode0)
	    {
	      rtx tmp;

	      tmp = op0; op0 = op1; op1 = tmp;
	      tmp = xop0; xop0 = xop1; xop1 = tmp;
	    }
	}

      /* In case the insn wants input operands in modes different from
	 those of the actual operands, convert the operands.  It would
	 seem that we don't need to convert CONST_INTs, but we do, so
	 that they're properly zero-extended, sign-extended or truncated
	 for their mode.  */

      if (GET_MODE (op0) != mode0 && mode0 != VOIDmode)
	xop0 = convert_modes (mode0,
			      GET_MODE (op0) != VOIDmode
			      ? GET_MODE (op0)
			      : mode,
			      xop0, unsignedp);

      if (GET_MODE (op1) != mode1 && mode1 != VOIDmode)
	xop1 = convert_modes (mode1,
			      GET_MODE (op1) != VOIDmode
			      ? GET_MODE (op1)
			      : mode,
			      xop1, unsignedp);

      /* Now, if insn's predicates don't allow our operands, put them into
	 pseudo regs.  */

      if (! (*insn_data[icode].operand[1].predicate) (xop0, mode0)
	  && mode0 != VOIDmode)
	xop0 = copy_to_mode_reg (mode0, xop0);

      if (! (*insn_data[icode].operand[2].predicate) (xop1, mode1)
	  && mode1 != VOIDmode)
	xop1 = copy_to_mode_reg (mode1, xop1);

      if (! (*insn_data[icode].operand[0].predicate) (temp, mode))
	temp = gen_reg_rtx (mode);

      pat = GEN_FCN (icode) (temp, xop0, xop1);
      if (pat)
	{
	  /* If PAT is composed of more than one insn, try to add an appropriate
	     REG_EQUAL note to it.  If we can't because TEMP conflicts with an
	     operand, call ourselves again, this time without a target.  */
	  if (INSN_P (pat) && NEXT_INSN (pat) != NULL_RTX
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
			   op0, op1, NULL_RTX, unsignedp, OPTAB_DIRECT);

      if (temp != 0)
	{
	  if (GET_MODE_CLASS (mode) == MODE_INT)
	    return gen_lowpart (mode, temp);
	  else
	    return convert_to_mode (mode, temp, unsignedp);
	}
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
	       the results to the same narrowness.  */

	    if ((binoptab == ior_optab || binoptab == and_optab
		 || binoptab == xor_optab
		 || binoptab == add_optab || binoptab == sub_optab
		 || binoptab == smul_optab || binoptab == ashl_optab)
		&& class == MODE_INT)
	      no_extend = 1;

	    xop0 = widen_operand (xop0, wider_mode, mode, unsignedp, no_extend);

	    /* The second operand of a shift must always be extended.  */
	    xop1 = widen_operand (xop1, wider_mode, mode, unsignedp,
				  no_extend && binoptab != ashl_optab);

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
				target_piece, unsignedp, next_methods);

	  if (x == 0)
	    break;

	  if (target_piece != x)
	    emit_move_insn (target_piece, x);
	}

      insns = get_insns ();
      end_sequence ();

      if (i == GET_MODE_BITSIZE (mode) / BITS_PER_WORD)
	{
	  if (binoptab->code != UNKNOWN)
	    equiv_value
	      = gen_rtx_fmt_ee (binoptab->code, mode,
				copy_rtx (op0), copy_rtx (op1));
	  else
	    equiv_value = 0;

	  emit_no_conflict_block (insns, target, op0, op1, equiv_value);
	  return target;
	}
    }

  /* Synthesize double word shifts from single word shifts.  */
  if ((binoptab == lshr_optab || binoptab == ashl_optab
       || binoptab == ashr_optab)
      && class == MODE_INT
      && GET_CODE (op1) == CONST_INT
      && GET_MODE_SIZE (mode) == 2 * UNITS_PER_WORD
      && binoptab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing
      && ashl_optab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing
      && lshr_optab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing)
    {
      rtx insns, inter, equiv_value;
      rtx into_target, outof_target;
      rtx into_input, outof_input;
      int shift_count, left_shift, outof_word;

      /* If TARGET is the same as one of the operands, the REG_EQUAL note
	 won't be accurate, so use a new target.  */
      if (target == 0 || target == op0 || target == op1)
	target = gen_reg_rtx (mode);

      start_sequence ();

      shift_count = INTVAL (op1);

      /* OUTOF_* is the word we are shifting bits away from, and
	 INTO_* is the word that we are shifting bits towards, thus
	 they differ depending on the direction of the shift and
	 WORDS_BIG_ENDIAN.  */

      left_shift = binoptab == ashl_optab;
      outof_word = left_shift ^ ! WORDS_BIG_ENDIAN;

      outof_target = operand_subword (target, outof_word, 1, mode);
      into_target = operand_subword (target, 1 - outof_word, 1, mode);

      outof_input = operand_subword_force (op0, outof_word, mode);
      into_input = operand_subword_force (op0, 1 - outof_word, mode);

      if (shift_count >= BITS_PER_WORD)
	{
	  inter = expand_binop (word_mode, binoptab,
			       outof_input,
			       GEN_INT (shift_count - BITS_PER_WORD),
			       into_target, unsignedp, next_methods);

	  if (inter != 0 && inter != into_target)
	    emit_move_insn (into_target, inter);

	  /* For a signed right shift, we must fill the word we are shifting
	     out of with copies of the sign bit.  Otherwise it is zeroed.  */
	  if (inter != 0 && binoptab != ashr_optab)
	    inter = CONST0_RTX (word_mode);
	  else if (inter != 0)
	    inter = expand_binop (word_mode, binoptab,
				  outof_input,
				  GEN_INT (BITS_PER_WORD - 1),
				  outof_target, unsignedp, next_methods);

	  if (inter != 0 && inter != outof_target)
	    emit_move_insn (outof_target, inter);
	}
      else
	{
	  rtx carries;
	  optab reverse_unsigned_shift, unsigned_shift;

	  /* For a shift of less then BITS_PER_WORD, to compute the carry,
	     we must do a logical shift in the opposite direction of the
	     desired shift.  */

	  reverse_unsigned_shift = (left_shift ? lshr_optab : ashl_optab);

	  /* For a shift of less than BITS_PER_WORD, to compute the word
	     shifted towards, we need to unsigned shift the orig value of
	     that word.  */

	  unsigned_shift = (left_shift ? ashl_optab : lshr_optab);

	  carries = expand_binop (word_mode, reverse_unsigned_shift,
				  outof_input,
				  GEN_INT (BITS_PER_WORD - shift_count),
				  0, unsignedp, next_methods);

	  if (carries == 0)
	    inter = 0;
	  else
	    inter = expand_binop (word_mode, unsigned_shift, into_input,
				  op1, 0, unsignedp, next_methods);

	  if (inter != 0)
	    inter = expand_binop (word_mode, ior_optab, carries, inter,
				  into_target, unsignedp, next_methods);

	  if (inter != 0 && inter != into_target)
	    emit_move_insn (into_target, inter);

	  if (inter != 0)
	    inter = expand_binop (word_mode, binoptab, outof_input,
				  op1, outof_target, unsignedp, next_methods);

	  if (inter != 0 && inter != outof_target)
	    emit_move_insn (outof_target, inter);
	}

      insns = get_insns ();
      end_sequence ();

      if (inter != 0)
	{
	  if (binoptab->code != UNKNOWN)
	    equiv_value = gen_rtx_fmt_ee (binoptab->code, mode, op0, op1);
	  else
	    equiv_value = 0;

	  emit_no_conflict_block (insns, target, op0, op1, equiv_value);
	  return target;
	}
    }

  /* Synthesize double word rotates from single word shifts.  */
  if ((binoptab == rotl_optab || binoptab == rotr_optab)
      && class == MODE_INT
      && GET_CODE (op1) == CONST_INT
      && GET_MODE_SIZE (mode) == 2 * UNITS_PER_WORD
      && ashl_optab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing
      && lshr_optab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing)
    {
      rtx insns, equiv_value;
      rtx into_target, outof_target;
      rtx into_input, outof_input;
      rtx inter;
      int shift_count, left_shift, outof_word;

      /* If TARGET is the same as one of the operands, the REG_EQUAL note
	 won't be accurate, so use a new target.  */
      if (target == 0 || target == op0 || target == op1)
	target = gen_reg_rtx (mode);

      start_sequence ();

      shift_count = INTVAL (op1);

      /* OUTOF_* is the word we are shifting bits away from, and
	 INTO_* is the word that we are shifting bits towards, thus
	 they differ depending on the direction of the shift and
	 WORDS_BIG_ENDIAN.  */

      left_shift = (binoptab == rotl_optab);
      outof_word = left_shift ^ ! WORDS_BIG_ENDIAN;

      outof_target = operand_subword (target, outof_word, 1, mode);
      into_target = operand_subword (target, 1 - outof_word, 1, mode);

      outof_input = operand_subword_force (op0, outof_word, mode);
      into_input = operand_subword_force (op0, 1 - outof_word, mode);

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
	  if (binoptab->code != UNKNOWN)
	    equiv_value = gen_rtx_fmt_ee (binoptab->code, mode, op0, op1);
	  else
	    equiv_value = 0;

	  /* We can't make this a no conflict block if this is a word swap,
	     because the word swap case fails if the input and output values
	     are in the same register.  */
	  if (shift_count != BITS_PER_WORD)
	    emit_no_conflict_block (insns, target, op0, op1, equiv_value);
	  else
	    emit_insn (insns);


	  return target;
	}
    }

  /* These can be done a word at a time by propagating carries.  */
  if ((binoptab == add_optab || binoptab == sub_optab)
      && class == MODE_INT
      && GET_MODE_SIZE (mode) >= 2 * UNITS_PER_WORD
      && binoptab->handlers[(int) word_mode].insn_code != CODE_FOR_nothing)
    {
      unsigned int i;
      optab otheroptab = binoptab == add_optab ? sub_optab : add_optab;
      const unsigned int nwords = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;
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
      xop0 = force_reg (mode, op0);
      xop1 = force_reg (mode, op1);

      xtarget = gen_reg_rtx (mode);

      if (target == 0 || GET_CODE (target) != REG)
	target = xtarget;

      /* Indicate for flow that the entire target reg is being set.  */
      if (GET_CODE (target) == REG)
	emit_insn (gen_rtx_CLOBBER (VOIDmode, xtarget));

      /* Do the actual arithmetic.  */
      for (i = 0; i < nwords; i++)
	{
	  int index = (WORDS_BIG_ENDIAN ? nwords - i - 1 : i);
	  rtx target_piece = operand_subword (xtarget, index, 1, mode);
	  rtx op0_piece = operand_subword_force (xop0, index, mode);
	  rtx op1_piece = operand_subword_force (xop1, index, mode);
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

	  carry_in = carry_out;
	}

      if (i == GET_MODE_BITSIZE (mode) / (unsigned) BITS_PER_WORD)
	{
	  if (mov_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing
	      || ! rtx_equal_p (target, xtarget))
	    {
	      rtx temp = emit_move_insn (target, xtarget);

	      set_unique_reg_note (temp,
				   REG_EQUAL,
				   gen_rtx_fmt_ee (binoptab->code, mode,
						   copy_rtx (xop0),
						   copy_rtx (xop1)));
	    }
	  else
	    target = xtarget;

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
      rtx op0_xhigh = NULL_RTX;
      rtx op1_xhigh = NULL_RTX;

      /* If the target is the same as one of the inputs, don't use it.  This
	 prevents problems with the REG_EQUAL note.  */
      if (target == op0 || target == op1
	  || (target != 0 && GET_CODE (target) != REG))
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
				    NULL_RTX, 1, next_methods);
	  if (op0_xhigh)
	    op0_xhigh = expand_binop (word_mode, add_optab, op0_high,
				      op0_xhigh, op0_xhigh, 0, next_methods);
	  else
	    {
	      op0_xhigh = expand_binop (word_mode, ashr_optab, op0_low, wordm1,
					NULL_RTX, 0, next_methods);
	      if (op0_xhigh)
		op0_xhigh = expand_binop (word_mode, sub_optab, op0_high,
					  op0_xhigh, op0_xhigh, 0,
					  next_methods);
	    }

	  op1_xhigh = expand_binop (word_mode, lshr_optab, op1_low, wordm1,
				    NULL_RTX, 1, next_methods);
	  if (op1_xhigh)
	    op1_xhigh = expand_binop (word_mode, add_optab, op1_high,
				      op1_xhigh, op1_xhigh, 0, next_methods);
	  else
	    {
	      op1_xhigh = expand_binop (word_mode, ashr_optab, op1_low, wordm1,
					NULL_RTX, 0, next_methods);
	      if (op1_xhigh)
		op1_xhigh = expand_binop (word_mode, sub_optab, op1_high,
					  op1_xhigh, op1_xhigh, 0,
					  next_methods);
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
	  rtx product_high = operand_subword (product, high, 1, mode);
	  rtx temp = expand_binop (word_mode, binoptab, op0_low, op1_xhigh,
				   NULL_RTX, 0, OPTAB_DIRECT);

	  if (!REG_P (product_high))
	    product_high = force_reg (word_mode, product_high);

	  if (temp != 0)
	    temp = expand_binop (word_mode, add_optab, temp, product_high,
				 product_high, 0, next_methods);

	  if (temp != 0 && temp != product_high)
	    emit_move_insn (product_high, temp);

	  if (temp != 0)
	    temp = expand_binop (word_mode, binoptab, op1_low, op0_xhigh,
				 NULL_RTX, 0, OPTAB_DIRECT);

	  if (temp != 0)
	    temp = expand_binop (word_mode, add_optab, temp,
				 product_high, product_high,
				 0, next_methods);

	  if (temp != 0 && temp != product_high)
	    emit_move_insn (product_high, temp);

	  emit_move_insn (operand_subword (product, high, 1, mode), product_high);

	  if (temp != 0)
	    {
	      if (mov_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
		{
		  temp = emit_move_insn (product, product);
		  set_unique_reg_note (temp,
				       REG_EQUAL,
				       gen_rtx_fmt_ee (MULT, mode,
						       copy_rtx (op0),
						       copy_rtx (op1)));
		}

	      return product;
	    }
	}

      /* If we get here, we couldn't do it for some reason even though we
	 originally thought we could.  Delete anything we've emitted in
	 trying to do it.  */

      delete_insns_since (last);
    }

  /* Open-code the vector operations if we have no hardware support
     for them.  */
  if (class == MODE_VECTOR_INT || class == MODE_VECTOR_FLOAT)
    return expand_vector_binop (mode, binoptab, op0, op1, target,
				unsignedp, methods);

  /* We need to open-code the complex type operations: '+, -, * and /' */

  /* At this point we allow operations between two similar complex
     numbers, and also if one of the operands is not a complex number
     but rather of MODE_FLOAT or MODE_INT. However, the caller
     must make sure that the MODE of the non-complex operand matches
     the SUBMODE of the complex operand.  */

  if (class == MODE_COMPLEX_FLOAT || class == MODE_COMPLEX_INT)
    {
      rtx real0 = 0, imag0 = 0;
      rtx real1 = 0, imag1 = 0;
      rtx realr, imagr, res;
      rtx seq, result;
      int ok = 0;

      /* Find the correct mode for the real and imaginary parts.  */
      enum machine_mode submode = GET_MODE_INNER (mode);

      if (submode == BLKmode)
	abort ();

      start_sequence ();

      if (GET_MODE (op0) == mode)
	{
	  real0 = gen_realpart (submode, op0);
	  imag0 = gen_imagpart (submode, op0);
	}
      else
	real0 = op0;

      if (GET_MODE (op1) == mode)
	{
	  real1 = gen_realpart (submode, op1);
	  imag1 = gen_imagpart (submode, op1);
	}
      else
	real1 = op1;

      if (real0 == 0 || real1 == 0 || ! (imag0 != 0 || imag1 != 0))
	abort ();

      result = gen_reg_rtx (mode);
      realr = gen_realpart (submode, result);
      imagr = gen_imagpart (submode, result);

      switch (binoptab->code)
	{
	case PLUS:
	  /* (a+ib) + (c+id) = (a+c) + i(b+d) */
	case MINUS:
	  /* (a+ib) - (c+id) = (a-c) + i(b-d) */
	  res = expand_binop (submode, binoptab, real0, real1,
			      realr, unsignedp, methods);

	  if (res == 0)
	    break;
	  else if (res != realr)
	    emit_move_insn (realr, res);

	  if (imag0 != 0 && imag1 != 0)
	    res = expand_binop (submode, binoptab, imag0, imag1,
				imagr, unsignedp, methods);
	  else if (imag0 != 0)
	    res = imag0;
	  else if (binoptab->code == MINUS)
            res = expand_unop (submode,
                                binoptab == subv_optab ? negv_optab : neg_optab,
                                imag1, imagr, unsignedp);
	  else
	    res = imag1;

	  if (res == 0)
	    break;
	  else if (res != imagr)
	    emit_move_insn (imagr, res);

	  ok = 1;
	  break;

	case MULT:
	  /* (a+ib) * (c+id) = (ac-bd) + i(ad+cb) */

	  if (imag0 != 0 && imag1 != 0)
	    {
	      rtx temp1, temp2;

	      /* Don't fetch these from memory more than once.  */
	      real0 = force_reg (submode, real0);
	      real1 = force_reg (submode, real1);
	      imag0 = force_reg (submode, imag0);
	      imag1 = force_reg (submode, imag1);

	      temp1 = expand_binop (submode, binoptab, real0, real1, NULL_RTX,
				    unsignedp, methods);

	      temp2 = expand_binop (submode, binoptab, imag0, imag1, NULL_RTX,
				    unsignedp, methods);

	      if (temp1 == 0 || temp2 == 0)
		break;

	      res = (expand_binop
                     (submode,
                      binoptab == smulv_optab ? subv_optab : sub_optab,
                      temp1, temp2, realr, unsignedp, methods));

	      if (res == 0)
		break;
	      else if (res != realr)
		emit_move_insn (realr, res);

	      temp1 = expand_binop (submode, binoptab, real0, imag1,
				    NULL_RTX, unsignedp, methods);

	      /* Avoid expanding redundant multiplication for the common
		 case of squaring a complex number.  */
	      if (rtx_equal_p (real0, real1) && rtx_equal_p (imag0, imag1))
		temp2 = temp1;
	      else
		temp2 = expand_binop (submode, binoptab, real1, imag0,
				      NULL_RTX, unsignedp, methods);

	      if (temp1 == 0 || temp2 == 0)
		break;

	      res = (expand_binop
                     (submode,
                      binoptab == smulv_optab ? addv_optab : add_optab,
                      temp1, temp2, imagr, unsignedp, methods));

	      if (res == 0)
		break;
	      else if (res != imagr)
		emit_move_insn (imagr, res);

	      ok = 1;
	    }
	  else
	    {
	      /* Don't fetch these from memory more than once.  */
	      real0 = force_reg (submode, real0);
	      real1 = force_reg (submode, real1);

	      res = expand_binop (submode, binoptab, real0, real1,
				  realr, unsignedp, methods);
	      if (res == 0)
		break;
	      else if (res != realr)
		emit_move_insn (realr, res);

	      if (imag0 != 0)
		res = expand_binop (submode, binoptab,
				    real1, imag0, imagr, unsignedp, methods);
	      else
		res = expand_binop (submode, binoptab,
				    real0, imag1, imagr, unsignedp, methods);

	      if (res == 0)
		break;
	      else if (res != imagr)
		emit_move_insn (imagr, res);

	      ok = 1;
	    }
	  break;

	case DIV:
	  /* (a+ib) / (c+id) = ((ac+bd)/(cc+dd)) + i((bc-ad)/(cc+dd)) */

	  if (imag1 == 0)
	    {
	      /* (a+ib) / (c+i0) = (a/c) + i(b/c) */

	      /* Don't fetch these from memory more than once.  */
	      real1 = force_reg (submode, real1);

	      /* Simply divide the real and imaginary parts by `c' */
	      if (class == MODE_COMPLEX_FLOAT)
		res = expand_binop (submode, binoptab, real0, real1,
				    realr, unsignedp, methods);
	      else
		res = expand_divmod (0, TRUNC_DIV_EXPR, submode,
				     real0, real1, realr, unsignedp);

	      if (res == 0)
		break;
	      else if (res != realr)
		emit_move_insn (realr, res);

	      if (class == MODE_COMPLEX_FLOAT)
		res = expand_binop (submode, binoptab, imag0, real1,
				    imagr, unsignedp, methods);
	      else
		res = expand_divmod (0, TRUNC_DIV_EXPR, submode,
				     imag0, real1, imagr, unsignedp);

	      if (res == 0)
		break;
	      else if (res != imagr)
		emit_move_insn (imagr, res);

	      ok = 1;
	    }
	  else
	    {
	      switch (flag_complex_divide_method)
		{
		case 0:
		  ok = expand_cmplxdiv_straight (real0, real1, imag0, imag1,
						 realr, imagr, submode,
						 unsignedp, methods,
						 class, binoptab);
		  break;

		case 1:
		  ok = expand_cmplxdiv_wide (real0, real1, imag0, imag1,
					     realr, imagr, submode,
					     unsignedp, methods,
					     class, binoptab);
		  break;

		default:
		  abort ();
		}
	    }
	  break;

	default:
	  abort ();
	}

      seq = get_insns ();
      end_sequence ();

      if (ok)
	{
	  rtx equiv = gen_rtx_fmt_ee (binoptab->code, mode,
				      copy_rtx (op0), copy_rtx (op1));
	  emit_no_conflict_block (seq, result, op0, op1, equiv);
	  return result;
	}
    }

  /* It can't be open-coded in this mode.
     Use a library call if one is available and caller says that's ok.  */

  if (binoptab->handlers[(int) mode].libfunc
      && (methods == OPTAB_LIB || methods == OPTAB_LIB_WIDEN))
    {
      rtx insns;
      rtx op1x = op1;
      enum machine_mode op1_mode = mode;
      rtx value;

      start_sequence ();

      if (shift_op)
	{
	  op1_mode = word_mode;
	  /* Specify unsigned here,
	     since negative shift counts are meaningless.  */
	  op1x = convert_to_mode (word_mode, op1, 1);
	}

      if (GET_MODE (op0) != VOIDmode
	  && GET_MODE (op0) != mode)
	op0 = convert_to_mode (mode, op0, unsignedp);

      /* Pass 1 for NO_QUEUE so we don't lose any increments
	 if the libcall is cse'd or moved.  */
      value = emit_library_call_value (binoptab->handlers[(int) mode].libfunc,
				       NULL_RTX, LCT_CONST, mode, 2,
				       op0, mode, op1x, op1_mode);

      insns = get_insns ();
      end_sequence ();

      target = gen_reg_rtx (mode);
      emit_libcall_block (insns, target, value,
			  gen_rtx_fmt_ee (binoptab->code, mode, op0, op1));

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
		 the results to the same narrowness.  */

	      if ((binoptab == ior_optab || binoptab == and_optab
		   || binoptab == xor_optab
		   || binoptab == add_optab || binoptab == sub_optab
		   || binoptab == smul_optab || binoptab == ashl_optab)
		  && class == MODE_INT)
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

/* Like expand_binop, but for open-coding vectors binops.  */

static rtx
expand_vector_binop (enum machine_mode mode, optab binoptab, rtx op0,
		     rtx op1, rtx target, int unsignedp,
		     enum optab_methods methods)
{
  enum machine_mode submode, tmode;
  int size, elts, subsize, subbitsize, i;
  rtx t, a, b, res, seq;
  enum mode_class class;

  class = GET_MODE_CLASS (mode);

  size = GET_MODE_SIZE (mode);
  submode = GET_MODE_INNER (mode);

  /* Search for the widest vector mode with the same inner mode that is
     still narrower than MODE and that allows to open-code this operator.
     Note, if we find such a mode and the handler later decides it can't
     do the expansion, we'll be called recursively with the narrower mode.  */
  for (tmode = GET_CLASS_NARROWEST_MODE (class);
       GET_MODE_SIZE (tmode) < GET_MODE_SIZE (mode);
       tmode = GET_MODE_WIDER_MODE (tmode))
    {
      if (GET_MODE_INNER (tmode) == GET_MODE_INNER (mode)
	  && binoptab->handlers[(int) tmode].insn_code != CODE_FOR_nothing)
	submode = tmode;
    }

  switch (binoptab->code)
    {
    case AND:
    case IOR:
    case XOR:
      tmode = int_mode_for_mode (mode);
      if (tmode != BLKmode)
	submode = tmode;
    case PLUS:
    case MINUS:
    case MULT:
    case DIV:
      subsize = GET_MODE_SIZE (submode);
      subbitsize = GET_MODE_BITSIZE (submode);
      elts = size / subsize;

      /* If METHODS is OPTAB_DIRECT, we don't insist on the exact mode,
	 but that we operate on more than one element at a time.  */
      if (subsize == GET_MODE_UNIT_SIZE (mode) && methods == OPTAB_DIRECT)
	return 0;

      start_sequence ();

      /* Errors can leave us with a const0_rtx as operand.  */
      if (GET_MODE (op0) != mode)
	op0 = copy_to_mode_reg (mode, op0);
      if (GET_MODE (op1) != mode)
	op1 = copy_to_mode_reg (mode, op1);

      if (!target)
	target = gen_reg_rtx (mode);

      for (i = 0; i < elts; ++i)
	{
	  /* If this is part of a register, and not the first item in the
	     word, we can't store using a SUBREG - that would clobber
	     previous results.
	     And storing with a SUBREG is only possible for the least
	     significant part, hence we can't do it for big endian
	     (unless we want to permute the evaluation order.  */
	  if (GET_CODE (target) == REG
	      && (BYTES_BIG_ENDIAN
		  ? subsize < UNITS_PER_WORD
		  : ((i * subsize) % UNITS_PER_WORD) != 0))
	    t = NULL_RTX;
	  else
	    t = simplify_gen_subreg (submode, target, mode, i * subsize);
	  if (CONSTANT_P (op0))
	    a = simplify_gen_subreg (submode, op0, mode, i * subsize);
	  else
	    a = extract_bit_field (op0, subbitsize, i * subbitsize, unsignedp,
				   NULL_RTX, submode, submode, size);
	  if (CONSTANT_P (op1))
	    b = simplify_gen_subreg (submode, op1, mode, i * subsize);
	  else
	    b = extract_bit_field (op1, subbitsize, i * subbitsize, unsignedp,
				   NULL_RTX, submode, submode, size);

	  if (binoptab->code == DIV)
	    {
	      if (class == MODE_VECTOR_FLOAT)
		res = expand_binop (submode, binoptab, a, b, t,
				    unsignedp, methods);
	      else
		res = expand_divmod (0, TRUNC_DIV_EXPR, submode,
				     a, b, t, unsignedp);
	    }
	  else
	    res = expand_binop (submode, binoptab, a, b, t,
				unsignedp, methods);

	  if (res == 0)
	    break;

	  if (t)
	    emit_move_insn (t, res);
	  else
	    store_bit_field (target, subbitsize, i * subbitsize, submode, res,
			     size);
	}
      break;

    default:
      abort ();
    }

  seq = get_insns ();
  end_sequence ();
  emit_insn (seq);

  return target;
}

/* Like expand_unop but for open-coding vector unops.  */

static rtx
expand_vector_unop (enum machine_mode mode, optab unoptab, rtx op0,
		    rtx target, int unsignedp)
{
  enum machine_mode submode, tmode;
  int size, elts, subsize, subbitsize, i;
  rtx t, a, res, seq;

  size = GET_MODE_SIZE (mode);
  submode = GET_MODE_INNER (mode);

  /* Search for the widest vector mode with the same inner mode that is
     still narrower than MODE and that allows to open-code this operator.
     Note, if we find such a mode and the handler later decides it can't
     do the expansion, we'll be called recursively with the narrower mode.  */
  for (tmode = GET_CLASS_NARROWEST_MODE (GET_MODE_CLASS (mode));
       GET_MODE_SIZE (tmode) < GET_MODE_SIZE (mode);
       tmode = GET_MODE_WIDER_MODE (tmode))
    {
      if (GET_MODE_INNER (tmode) == GET_MODE_INNER (mode)
	  && unoptab->handlers[(int) tmode].insn_code != CODE_FOR_nothing)
	submode = tmode;
    }
  /* If there is no negate operation, try doing a subtract from zero.  */
  if (unoptab == neg_optab && GET_MODE_CLASS (submode) == MODE_INT
      /* Avoid infinite recursion when an
	 error has left us with the wrong mode.  */
      && GET_MODE (op0) == mode)
    {
      rtx temp;
      temp = expand_binop (mode, sub_optab, CONST0_RTX (mode), op0,
                           target, unsignedp, OPTAB_DIRECT);
      if (temp)
	return temp;
    }

  if (unoptab == one_cmpl_optab)
    {
      tmode = int_mode_for_mode (mode);
      if (tmode != BLKmode)
	submode = tmode;
    }

  subsize = GET_MODE_SIZE (submode);
  subbitsize = GET_MODE_BITSIZE (submode);
  elts = size / subsize;

  /* Errors can leave us with a const0_rtx as operand.  */
  if (GET_MODE (op0) != mode)
    op0 = copy_to_mode_reg (mode, op0);

  if (!target)
    target = gen_reg_rtx (mode);

  start_sequence ();

  for (i = 0; i < elts; ++i)
    {
      /* If this is part of a register, and not the first item in the
	 word, we can't store using a SUBREG - that would clobber
	 previous results.
	 And storing with a SUBREG is only possible for the least
	 significant part, hence we can't do it for big endian
	 (unless we want to permute the evaluation order.  */
      if (GET_CODE (target) == REG
	  && (BYTES_BIG_ENDIAN
	      ?  subsize < UNITS_PER_WORD
	      : ((i * subsize) % UNITS_PER_WORD) != 0))
	t = NULL_RTX;
      else
	t = simplify_gen_subreg (submode, target, mode, i * subsize);
      if (CONSTANT_P (op0))
	a = simplify_gen_subreg (submode, op0, mode, i * subsize);
      else
	a = extract_bit_field (op0, subbitsize, i * subbitsize, unsignedp,
			       t, submode, submode, size);

      res = expand_unop (submode, unoptab, a, t, unsignedp);

      if (t)
	emit_move_insn (t, res);
      else
	store_bit_field (target, subbitsize, i * subbitsize, submode, res,
			 size);
    }

  seq = get_insns ();
  end_sequence ();
  emit_insn (seq);

  return target;
}

/* Expand a binary operator which has both signed and unsigned forms.
   UOPTAB is the optab for unsigned operations, and SOPTAB is for
   signed operations.

   If we widen unsigned operands, we may use a signed wider operation instead
   of an unsigned wider operation, since the result would be the same.  */

rtx
sign_expand_binop (enum machine_mode mode, optab uoptab, optab soptab,
		   rtx op0, rtx op1, rtx target, int unsignedp,
		   enum optab_methods methods)
{
  rtx temp;
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
   the result is not actually wanted.  We will generate it into
   a dummy pseudo-reg and discard it.  They may not both be zero.

   Returns 1 if this operation can be performed; 0 if not.  */

int
expand_twoval_binop (optab binoptab, rtx op0, rtx op1, rtx targ0, rtx targ1,
		     int unsignedp)
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
      && rtx_cost (op0, binoptab->code) > COSTS_N_INSNS (1))
    op0 = force_reg (mode, op0);

  if (CONSTANT_P (op1) && preserve_subexpressions_p ()
      && rtx_cost (op1, binoptab->code) > COSTS_N_INSNS (1))
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
      enum machine_mode mode0 = insn_data[icode].operand[1].mode;
      enum machine_mode mode1 = insn_data[icode].operand[2].mode;
      rtx pat;
      rtx xop0 = op0, xop1 = op1;

      /* In case the insn wants input operands in modes different from
	 those of the actual operands, convert the operands.  It would
	 seem that we don't need to convert CONST_INTs, but we do, so
	 that they're properly zero-extended, sign-extended or truncated
	 for their mode.  */

      if (GET_MODE (op0) != mode0 && mode0 != VOIDmode)
	xop0 = convert_modes (mode0,
			      GET_MODE (op0) != VOIDmode
			      ? GET_MODE (op0)
			      : mode,
			      xop0, unsignedp);

      if (GET_MODE (op1) != mode1 && mode1 != VOIDmode)
	xop1 = convert_modes (mode1,
			      GET_MODE (op1) != VOIDmode
			      ? GET_MODE (op1)
			      : mode,
			      xop1, unsignedp);

      /* Now, if insn doesn't accept these operands, put them into pseudos.  */
      if (! (*insn_data[icode].operand[1].predicate) (xop0, mode0))
	xop0 = copy_to_mode_reg (mode0, xop0);

      if (! (*insn_data[icode].operand[2].predicate) (xop1, mode1))
	xop1 = copy_to_mode_reg (mode1, xop1);

      /* We could handle this, but we should always be called with a pseudo
	 for our targets and all insns should take them as outputs.  */
      if (! (*insn_data[icode].operand[0].predicate) (targ0, mode)
	  || ! (*insn_data[icode].operand[3].predicate) (targ1, mode))
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

/* Wrapper around expand_unop which takes an rtx code to specify
   the operation to perform, not an optab pointer.  All other
   arguments are the same.  */
rtx
expand_simple_unop (enum machine_mode mode, enum rtx_code code, rtx op0,
		    rtx target, int unsignedp)
{
  optab unop = code_to_optab[(int) code];
  if (unop == 0)
    abort ();

  return expand_unop (mode, unop, op0, target, unsignedp);
}

/* Try calculating
	(clz:narrow x)
   as
	(clz:wide (zero_extend:wide x)) - ((width wide) - (width narrow)).  */
static rtx
widen_clz (enum machine_mode mode, rtx op0, rtx target)
{
  enum mode_class class = GET_MODE_CLASS (mode);
  if (class == MODE_INT || class == MODE_FLOAT || class == MODE_COMPLEX_FLOAT)
    {
      enum machine_mode wider_mode;
      for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
	   wider_mode = GET_MODE_WIDER_MODE (wider_mode))
	{
	  if (clz_optab->handlers[(int) wider_mode].insn_code
	      != CODE_FOR_nothing)
	    {
	      rtx xop0, temp, last;

	      last = get_last_insn ();

	      if (target == 0)
		target = gen_reg_rtx (mode);
	      xop0 = widen_operand (op0, wider_mode, mode, true, false);
	      temp = expand_unop (wider_mode, clz_optab, xop0, NULL_RTX, true);
	      if (temp != 0)
		temp = expand_binop (wider_mode, sub_optab, temp,
				     GEN_INT (GET_MODE_BITSIZE (wider_mode)
					      - GET_MODE_BITSIZE (mode)),
				     target, true, OPTAB_DIRECT);
	      if (temp == 0)
		delete_insns_since (last);

	      return temp;
	    }
	}
    }
  return 0;
}

/* Try calculating (parity x) as (and (popcount x) 1), where
   popcount can also be done in a wider mode.  */
static rtx
expand_parity (enum machine_mode mode, rtx op0, rtx target)
{
  enum mode_class class = GET_MODE_CLASS (mode);
  if (class == MODE_INT || class == MODE_FLOAT || class == MODE_COMPLEX_FLOAT)
    {
      enum machine_mode wider_mode;
      for (wider_mode = mode; wider_mode != VOIDmode;
	   wider_mode = GET_MODE_WIDER_MODE (wider_mode))
	{
	  if (popcount_optab->handlers[(int) wider_mode].insn_code
	      != CODE_FOR_nothing)
	    {
	      rtx xop0, temp, last;

	      last = get_last_insn ();

	      if (target == 0)
		target = gen_reg_rtx (mode);
	      xop0 = widen_operand (op0, wider_mode, mode, true, false);
	      temp = expand_unop (wider_mode, popcount_optab, xop0, NULL_RTX,
				  true);
	      if (temp != 0)
		temp = expand_binop (wider_mode, and_optab, temp, GEN_INT (1),
				     target, true, OPTAB_DIRECT);
	      if (temp == 0)
		delete_insns_since (last);

	      return temp;
	    }
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
expand_unop (enum machine_mode mode, optab unoptab, rtx op0, rtx target,
	     int unsignedp)
{
  enum mode_class class;
  enum machine_mode wider_mode;
  rtx temp;
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
      enum machine_mode mode0 = insn_data[icode].operand[1].mode;
      rtx xop0 = op0;

      if (target)
	temp = target;
      else
	temp = gen_reg_rtx (mode);

      if (GET_MODE (xop0) != VOIDmode
	  && GET_MODE (xop0) != mode0)
	xop0 = convert_to_mode (mode0, xop0, unsignedp);

      /* Now, if insn doesn't accept our operand, put it into a pseudo.  */

      if (! (*insn_data[icode].operand[1].predicate) (xop0, mode0))
	xop0 = copy_to_mode_reg (mode0, xop0);

      if (! (*insn_data[icode].operand[0].predicate) (temp, mode))
	temp = gen_reg_rtx (mode);

      pat = GEN_FCN (icode) (temp, xop0);
      if (pat)
	{
	  if (INSN_P (pat) && NEXT_INSN (pat) != NULL_RTX
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

  /* Widening clz needs special treatment.  */
  if (unoptab == clz_optab)
    {
      temp = widen_clz (mode, op0, target);
      if (temp)
	return temp;
      else
	goto try_libcall;
    }

  if (class == MODE_INT || class == MODE_FLOAT || class == MODE_COMPLEX_FLOAT)
    for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
	 wider_mode = GET_MODE_WIDER_MODE (wider_mode))
      {
	if (unoptab->handlers[(int) wider_mode].insn_code != CODE_FOR_nothing)
	  {
	    rtx xop0 = op0;

	    /* For certain operations, we need not actually extend
	       the narrow operand, as long as we will truncate the
	       results to the same narrowness.  */

	    xop0 = widen_operand (xop0, wider_mode, mode, unsignedp,
				  (unoptab == neg_optab
				   || unoptab == one_cmpl_optab)
				  && class == MODE_INT);

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
			      gen_rtx_fmt_e (unoptab->code, mode,
					     copy_rtx (op0)));
      return target;
    }

  /* Open-code the complex negation operation.  */
  else if (unoptab->code == NEG
	   && (class == MODE_COMPLEX_FLOAT || class == MODE_COMPLEX_INT))
    {
      rtx target_piece;
      rtx x;
      rtx seq;

      /* Find the correct mode for the real and imaginary parts.  */
      enum machine_mode submode = GET_MODE_INNER (mode);

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
			      gen_rtx_fmt_e (unoptab->code, mode,
					     copy_rtx (op0)));
      return target;
    }

  /* Try negating floating point values by flipping the sign bit.  */
  if (unoptab->code == NEG && class == MODE_FLOAT
      && GET_MODE_BITSIZE (mode) <= 2 * HOST_BITS_PER_WIDE_INT)
    {
      const struct real_format *fmt = REAL_MODE_FORMAT (mode);
      enum machine_mode imode = int_mode_for_mode (mode);
      int bitpos = (fmt != 0) ? fmt->signbit : -1;

      if (imode != BLKmode && bitpos >= 0 && fmt->has_signed_zero)
	{
	  HOST_WIDE_INT hi, lo;
	  rtx last = get_last_insn ();

	  /* Handle targets with different FP word orders.  */
	  if (FLOAT_WORDS_BIG_ENDIAN != WORDS_BIG_ENDIAN)
	    {
	      int nwords = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;
	      int word = nwords - (bitpos / BITS_PER_WORD) - 1;
	      bitpos = word * BITS_PER_WORD + bitpos % BITS_PER_WORD;
	    }

	  if (bitpos < HOST_BITS_PER_WIDE_INT)
	    {
	      hi = 0;
	      lo = (HOST_WIDE_INT) 1 << bitpos;
	    }
	  else
	    {
	      hi = (HOST_WIDE_INT) 1 << (bitpos - HOST_BITS_PER_WIDE_INT);
	      lo = 0;
	    }
	  temp = expand_binop (imode, xor_optab,
			       gen_lowpart (imode, op0),
			       immed_double_const (lo, hi, imode),
			       NULL_RTX, 1, OPTAB_LIB_WIDEN);
	  if (temp != 0)
	    {
	      rtx insn;
	      if (target == 0)
		target = gen_reg_rtx (mode);
	      insn = emit_move_insn (target, gen_lowpart (mode, temp));
	      set_unique_reg_note (insn, REG_EQUAL,
				   gen_rtx_fmt_e (NEG, mode,
						  copy_rtx (op0)));
	      return target;
	    }
	  delete_insns_since (last);
        }
    }

  /* Try calculating parity (x) as popcount (x) % 2.  */
  if (unoptab == parity_optab)
    {
      temp = expand_parity (mode, op0, target);
      if (temp)
	return temp;
    }

 try_libcall:
  /* Now try a library call in this mode.  */
  if (unoptab->handlers[(int) mode].libfunc)
    {
      rtx insns;
      rtx value;
      enum machine_mode outmode = mode;

      /* All of these functions return small values.  Thus we choose to
	 have them return something that isn't a double-word.  */
      if (unoptab == ffs_optab || unoptab == clz_optab || unoptab == ctz_optab
	  || unoptab == popcount_optab || unoptab == parity_optab)
	outmode
	    = GET_MODE (hard_libcall_value (TYPE_MODE (integer_type_node)));

      start_sequence ();

      /* Pass 1 for NO_QUEUE so we don't lose any increments
	 if the libcall is cse'd or moved.  */
      value = emit_library_call_value (unoptab->handlers[(int) mode].libfunc,
				       NULL_RTX, LCT_CONST, outmode,
				       1, op0, mode);
      insns = get_insns ();
      end_sequence ();

      target = gen_reg_rtx (outmode);
      emit_libcall_block (insns, target, value,
			  gen_rtx_fmt_e (unoptab->code, mode, op0));

      return target;
    }

  if (class == MODE_VECTOR_FLOAT || class == MODE_VECTOR_INT)
    return expand_vector_unop (mode, unoptab, op0, target, unsignedp);

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

	      xop0 = widen_operand (xop0, wider_mode, mode, unsignedp,
				    (unoptab == neg_optab
				     || unoptab == one_cmpl_optab)
				    && class == MODE_INT);

	      temp = expand_unop (wider_mode, unoptab, xop0, NULL_RTX,
				  unsignedp);

	      /* If we are generating clz using wider mode, adjust the
		 result.  */
	      if (unoptab == clz_optab && temp != 0)
		temp = expand_binop (wider_mode, sub_optab, temp,
				     GEN_INT (GET_MODE_BITSIZE (wider_mode)
					      - GET_MODE_BITSIZE (mode)),
				     target, true, OPTAB_DIRECT);

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

  /* If there is no negate operation, try doing a subtract from zero.
     The US Software GOFAST library needs this.  */
  if (unoptab->code == NEG)
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
expand_abs_nojump (enum machine_mode mode, rtx op0, rtx target,
		   int result_unsignedp)
{
  rtx temp;

  if (! flag_trapv)
    result_unsignedp = 1;

  /* First try to do it with a special abs instruction.  */
  temp = expand_unop (mode, result_unsignedp ? abs_optab : absv_optab,
                      op0, target, 0);
  if (temp != 0)
    return temp;

  /* For floating point modes, try clearing the sign bit.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_MODE_BITSIZE (mode) <= 2 * HOST_BITS_PER_WIDE_INT)
    {
      const struct real_format *fmt = REAL_MODE_FORMAT (mode);
      enum machine_mode imode = int_mode_for_mode (mode);
      int bitpos = (fmt != 0) ? fmt->signbit : -1;

      if (imode != BLKmode && bitpos >= 0)
	{
	  HOST_WIDE_INT hi, lo;
	  rtx last = get_last_insn ();

	  /* Handle targets with different FP word orders.  */
	  if (FLOAT_WORDS_BIG_ENDIAN != WORDS_BIG_ENDIAN)
	    {
	      int nwords = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;
	      int word = nwords - (bitpos / BITS_PER_WORD) - 1;
	      bitpos = word * BITS_PER_WORD + bitpos % BITS_PER_WORD;
	    }

	  if (bitpos < HOST_BITS_PER_WIDE_INT)
	    {
	      hi = 0;
	      lo = (HOST_WIDE_INT) 1 << bitpos;
	    }
	  else
	    {
	      hi = (HOST_WIDE_INT) 1 << (bitpos - HOST_BITS_PER_WIDE_INT);
	      lo = 0;
	    }
	  temp = expand_binop (imode, and_optab,
			       gen_lowpart (imode, op0),
			       immed_double_const (~lo, ~hi, imode),
			       NULL_RTX, 1, OPTAB_LIB_WIDEN);
	  if (temp != 0)
	    {
	      rtx insn;
	      if (target == 0)
		target = gen_reg_rtx (mode);
	      insn = emit_move_insn (target, gen_lowpart (mode, temp));
	      set_unique_reg_note (insn, REG_EQUAL,
				   gen_rtx_fmt_e (ABS, mode,
						  copy_rtx (op0)));
	      return target;
	    }
	  delete_insns_since (last);
	}
    }

  /* If we have a MAX insn, we can do this as MAX (x, -x).  */
  if (smax_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      rtx last = get_last_insn ();

      temp = expand_unop (mode, neg_optab, op0, NULL_RTX, 0);
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

  if (GET_MODE_CLASS (mode) == MODE_INT && BRANCH_COST >= 2)
    {
      rtx extended = expand_shift (RSHIFT_EXPR, mode, op0,
				   size_int (GET_MODE_BITSIZE (mode) - 1),
				   NULL_RTX, 0);

      temp = expand_binop (mode, xor_optab, extended, op0, target, 0,
			   OPTAB_LIB_WIDEN);
      if (temp != 0)
	temp = expand_binop (mode, result_unsignedp ? sub_optab : subv_optab,
                             temp, extended, target, 0, OPTAB_LIB_WIDEN);

      if (temp != 0)
	return temp;
    }

  return NULL_RTX;
}

rtx
expand_abs (enum machine_mode mode, rtx op0, rtx target,
	    int result_unsignedp, int safe)
{
  rtx temp, op1;

  if (! flag_trapv)
    result_unsignedp = 1;

  temp = expand_abs_nojump (mode, op0, target, result_unsignedp);
  if (temp != 0)
    return temp;

  /* If that does not win, use conditional jump and negate.  */

  /* It is safe to use the target if it is the same
     as the source if this is also a pseudo register */
  if (op0 == target && GET_CODE (op0) == REG
      && REGNO (op0) >= FIRST_PSEUDO_REGISTER)
    safe = 1;

  op1 = gen_label_rtx ();
  if (target == 0 || ! safe
      || GET_MODE (target) != mode
      || (GET_CODE (target) == MEM && MEM_VOLATILE_P (target))
      || (GET_CODE (target) == REG
	  && REGNO (target) < FIRST_PSEUDO_REGISTER))
    target = gen_reg_rtx (mode);

  emit_move_insn (target, op0);
  NO_DEFER_POP;

  /* If this mode is an integer too wide to compare properly,
     compare word by word.  Rely on CSE to optimize constant cases.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && ! can_compare_p (GE, mode, ccp_jump))
    do_jump_by_parts_greater_rtx (mode, 0, target, const0_rtx,
				  NULL_RTX, op1);
  else
    do_compare_rtx_and_jump (target, CONST0_RTX (mode), GE, 0, mode,
			     NULL_RTX, NULL_RTX, op1);

  op0 = expand_unop (mode, result_unsignedp ? neg_optab : negv_optab,
                     target, target, 0);
  if (op0 != target)
    emit_move_insn (target, op0);
  emit_label (op1);
  OK_DEFER_POP;
  return target;
}

/* Emit code to compute the absolute value of OP0, with result to
   TARGET if convenient.  (TARGET may be 0.)  The return value says
   where the result actually is to be found.

   MODE is the mode of the operand; the mode of the result is
   different but can be deduced from MODE.

   UNSIGNEDP is relevant for complex integer modes.  */

rtx
expand_complex_abs (enum machine_mode mode, rtx op0, rtx target,
		    int unsignedp)
{
  enum mode_class class = GET_MODE_CLASS (mode);
  enum machine_mode wider_mode;
  rtx temp;
  rtx entry_last = get_last_insn ();
  rtx last;
  rtx pat;
  optab this_abs_optab;

  /* Find the correct mode for the real and imaginary parts.  */
  enum machine_mode submode = GET_MODE_INNER (mode);

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

  this_abs_optab = ! unsignedp && flag_trapv
                   && (GET_MODE_CLASS(mode) == MODE_INT)
                   ? absv_optab : abs_optab;

  if (this_abs_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) this_abs_optab->handlers[(int) mode].insn_code;
      enum machine_mode mode0 = insn_data[icode].operand[1].mode;
      rtx xop0 = op0;

      if (target)
	temp = target;
      else
	temp = gen_reg_rtx (submode);

      if (GET_MODE (xop0) != VOIDmode
	  && GET_MODE (xop0) != mode0)
	xop0 = convert_to_mode (mode0, xop0, unsignedp);

      /* Now, if insn doesn't accept our operand, put it into a pseudo.  */

      if (! (*insn_data[icode].operand[1].predicate) (xop0, mode0))
	xop0 = copy_to_mode_reg (mode0, xop0);

      if (! (*insn_data[icode].operand[0].predicate) (temp, submode))
	temp = gen_reg_rtx (submode);

      pat = GEN_FCN (icode) (temp, xop0);
      if (pat)
	{
	  if (INSN_P (pat) && NEXT_INSN (pat) != NULL_RTX
	      && ! add_equal_note (pat, temp, this_abs_optab->code, xop0,
				   NULL_RTX))
	    {
	      delete_insns_since (last);
	      return expand_unop (mode, this_abs_optab, op0, NULL_RTX,
				  unsignedp);
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
      if (this_abs_optab->handlers[(int) wider_mode].insn_code
	  != CODE_FOR_nothing)
	{
	  rtx xop0 = op0;

	  xop0 = convert_modes (wider_mode, mode, xop0, unsignedp);
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
  if (sqrt_optab->handlers[(int) submode].insn_code != CODE_FOR_nothing
      && ! flag_trapv)
    {
      rtx real, imag, total;

      real = gen_realpart (submode, op0);
      imag = gen_imagpart (submode, op0);

      /* Square both parts.  */
      real = expand_mult (submode, real, real, NULL_RTX, 0);
      imag = expand_mult (submode, imag, imag, NULL_RTX, 0);

      /* Sum the parts.  */
      total = expand_binop (submode, add_optab, real, imag, NULL_RTX,
			    0, OPTAB_LIB_WIDEN);

      /* Get sqrt in TARGET.  Set TARGET to where the result is.  */
      target = expand_unop (submode, sqrt_optab, total, target, 0);
      if (target == 0)
	delete_insns_since (last);
      else
	return target;
    }

  /* Now try a library call in this mode.  */
  if (this_abs_optab->handlers[(int) mode].libfunc)
    {
      rtx insns;
      rtx value;

      start_sequence ();

      /* Pass 1 for NO_QUEUE so we don't lose any increments
	 if the libcall is cse'd or moved.  */
      value = emit_library_call_value (abs_optab->handlers[(int) mode].libfunc,
				       NULL_RTX, LCT_CONST, submode, 1, op0, mode);
      insns = get_insns ();
      end_sequence ();

      target = gen_reg_rtx (submode);
      emit_libcall_block (insns, target, value,
			  gen_rtx_fmt_e (this_abs_optab->code, mode, op0));

      return target;
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  for (wider_mode = GET_MODE_WIDER_MODE (mode); wider_mode != VOIDmode;
       wider_mode = GET_MODE_WIDER_MODE (wider_mode))
    {
      if ((this_abs_optab->handlers[(int) wider_mode].insn_code
	   != CODE_FOR_nothing)
	  || this_abs_optab->handlers[(int) wider_mode].libfunc)
	{
	  rtx xop0 = op0;

	  xop0 = convert_modes (wider_mode, mode, xop0, unsignedp);

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
emit_unop_insn (int icode, rtx target, rtx op0, enum rtx_code code)
{
  rtx temp;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  rtx pat;

  temp = target = protect_from_queue (target, 1);

  op0 = protect_from_queue (op0, 0);

  /* Sign and zero extension from memory is often done specially on
     RISC machines, so forcing into a register here can pessimize
     code.  */
  if (flag_force_mem && code != SIGN_EXTEND && code != ZERO_EXTEND)
    op0 = force_not_mem (op0);

  /* Now, if insn does not accept our operands, put them into pseudos.  */

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  if (! (*insn_data[icode].operand[0].predicate) (temp, GET_MODE (temp))
      || (flag_force_mem && GET_CODE (temp) == MEM))
    temp = gen_reg_rtx (GET_MODE (temp));

  pat = GEN_FCN (icode) (temp, op0);

  if (INSN_P (pat) && NEXT_INSN (pat) != NULL_RTX && code != UNKNOWN)
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
   are first emitted, followed by the block as described above.

   TARGET, OP0, and OP1 are the output and inputs of the operations,
   respectively.  OP1 may be zero for a unary operation.

   EQUIV, if nonzero, is an expression to be placed into a REG_EQUAL note
   on the last insn.

   If TARGET is not a register, INSNS is simply emitted with no special
   processing.  Likewise if anything in INSNS is not an INSN or if
   there is a libcall block inside INSNS.

   The final insn emitted is returned.  */

rtx
emit_no_conflict_block (rtx insns, rtx target, rtx op0, rtx op1, rtx equiv)
{
  rtx prev, next, first, last, insn;

  if (GET_CODE (target) != REG || reload_in_progress)
    return emit_insn (insns);
  else
    for (insn = insns; insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) != INSN
	  || find_reg_note (insn, REG_LIBCALL, NULL_RTX))
	return emit_insn (insns);

  /* First emit all insns that do not store into words of the output and remove
     these from the list.  */
  for (insn = insns; insn; insn = next)
    {
      rtx set = 0, note;
      int i;

      next = NEXT_INSN (insn);

      /* Some ports (cris) create a libcall regions at their own.  We must
	 avoid any potential nesting of LIBCALLs.  */
      if ((note = find_reg_note (insn, REG_LIBCALL, NULL)) != NULL)
	remove_note (insn, note);
      if ((note = find_reg_note (insn, REG_RETVAL, NULL)) != NULL)
	remove_note (insn, note);

      if (GET_CODE (PATTERN (insn)) == SET || GET_CODE (PATTERN (insn)) == USE
	  || GET_CODE (PATTERN (insn)) == CLOBBER)
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
    emit_insn (gen_rtx_CLOBBER (VOIDmode, target));

  for (insn = insns; insn; insn = next)
    {
      next = NEXT_INSN (insn);
      add_insn (insn);

      if (op1 && GET_CODE (op1) == REG)
	REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_NO_CONFLICT, op1,
					      REG_NOTES (insn));

      if (op0 && GET_CODE (op0) == REG)
	REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_NO_CONFLICT, op0,
					      REG_NOTES (insn));
    }

  if (mov_optab->handlers[(int) GET_MODE (target)].insn_code
      != CODE_FOR_nothing)
    {
      last = emit_move_insn (target, target);
      if (equiv)
	set_unique_reg_note (last, REG_EQUAL, equiv);
    }
  else
    {
      last = get_last_insn ();

      /* Remove any existing REG_EQUAL note from "last", or else it will
	 be mistaken for a note referring to the full contents of the
	 alleged libcall value when found together with the REG_RETVAL
	 note added below.  An existing note can come from an insn
	 expansion at "last".  */
      remove_note (last, find_reg_note (last, REG_EQUAL, NULL_RTX));
    }

  if (prev == 0)
    first = get_insns ();
  else
    first = NEXT_INSN (prev);

  /* Encapsulate the block so it gets manipulated as a unit.  */
  REG_NOTES (first) = gen_rtx_INSN_LIST (REG_LIBCALL, last,
					 REG_NOTES (first));
  REG_NOTES (last) = gen_rtx_INSN_LIST (REG_RETVAL, first, REG_NOTES (last));

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
emit_libcall_block (rtx insns, rtx target, rtx result, rtx equiv)
{
  rtx final_dest = target;
  rtx prev, next, first, last, insn;

  /* If this is a reg with REG_USERVAR_P set, then it could possibly turn
     into a MEM later.  Protect the libcall block from this change.  */
  if (! REG_P (target) || REG_USERVAR_P (target))
    target = gen_reg_rtx (GET_MODE (target));

  /* If we're using non-call exceptions, a libcall corresponding to an
     operation that may trap may also trap.  */
  if (flag_non_call_exceptions && may_trap_p (equiv))
    {
      for (insn = insns; insn; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == CALL_INSN)
	  {
	    rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);

	    if (note != 0 && INTVAL (XEXP (note, 0)) <= 0)
	      remove_note (insn, note);
	  }
    }
  else
  /* look for any CALL_INSNs in this sequence, and attach a REG_EH_REGION
     reg note to indicate that this call cannot throw or execute a nonlocal
     goto (unless there is already a REG_EH_REGION note, in which case
     we update it).  */
    for (insn = insns; insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == CALL_INSN)
	{
	  rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);

	  if (note != 0)
	    XEXP (note, 0) = GEN_INT (-1);
	  else
	    REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EH_REGION, GEN_INT (-1),
						  REG_NOTES (insn));
	}

  /* First emit all insns that set pseudos.  Remove them from the list as
     we go.  Avoid insns that set pseudos which were referenced in previous
     insns.  These can be generated by move_by_pieces, for example,
     to update an address.  Similarly, avoid insns that reference things
     set in previous insns.  */

  for (insn = insns; insn; insn = next)
    {
      rtx set = single_set (insn);
      rtx note;

      /* Some ports (cris) create a libcall regions at their own.  We must
	 avoid any potential nesting of LIBCALLs.  */
      if ((note = find_reg_note (insn, REG_LIBCALL, NULL)) != NULL)
	remove_note (insn, note);
      if ((note = find_reg_note (insn, REG_RETVAL, NULL)) != NULL)
	remove_note (insn, note);

      next = NEXT_INSN (insn);

      if (set != 0 && GET_CODE (SET_DEST (set)) == REG
	  && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER
	  && (insn == insns
	      || ((! INSN_P(insns)
		   || ! reg_mentioned_p (SET_DEST (set), PATTERN (insns)))
		  && ! reg_used_between_p (SET_DEST (set), insns, insn)
		  && ! modified_in_p (SET_SRC (set), insns)
		  && ! modified_between_p (SET_SRC (set), insns, insn))))
	{
	  if (PREV_INSN (insn))
	    NEXT_INSN (PREV_INSN (insn)) = next;
	  else
	    insns = next;

	  if (next)
	    PREV_INSN (next) = PREV_INSN (insn);

	  add_insn (insn);
	}

      /* Some ports use a loop to copy large arguments onto the stack.
	 Don't move anything outside such a loop.  */
      if (GET_CODE (insn) == CODE_LABEL)
	break;
    }

  prev = get_last_insn ();

  /* Write the remaining insns followed by the final copy.  */

  for (insn = insns; insn; insn = next)
    {
      next = NEXT_INSN (insn);

      add_insn (insn);
    }

  last = emit_move_insn (target, result);
  if (mov_optab->handlers[(int) GET_MODE (target)].insn_code
      != CODE_FOR_nothing)
    set_unique_reg_note (last, REG_EQUAL, copy_rtx (equiv));
  else
    {
      /* Remove any existing REG_EQUAL note from "last", or else it will
	 be mistaken for a note referring to the full contents of the
	 libcall value when found together with the REG_RETVAL note added
	 below.  An existing note can come from an insn expansion at
	 "last".  */
      remove_note (last, find_reg_note (last, REG_EQUAL, NULL_RTX));
    }

  if (final_dest != target)
    emit_move_insn (final_dest, target);

  if (prev == 0)
    first = get_insns ();
  else
    first = NEXT_INSN (prev);

  /* Encapsulate the block so it gets manipulated as a unit.  */
  if (!flag_non_call_exceptions || !may_trap_p (equiv))
    {
      /* We can't attach the REG_LIBCALL and REG_RETVAL notes
	 when the encapsulated region would not be in one basic block,
	 i.e. when there is a control_flow_insn_p insn between FIRST and LAST.
       */
      bool attach_libcall_retval_notes = true;
      next = NEXT_INSN (last);
      for (insn = first; insn != next; insn = NEXT_INSN (insn))
	if (control_flow_insn_p (insn))
	  {
	    attach_libcall_retval_notes = false;
	    break;
	  }

      if (attach_libcall_retval_notes)
	{
	  REG_NOTES (first) = gen_rtx_INSN_LIST (REG_LIBCALL, last,
						 REG_NOTES (first));
	  REG_NOTES (last) = gen_rtx_INSN_LIST (REG_RETVAL, first,
						REG_NOTES (last));
	}
    }
}

/* Generate code to store zero in X.  */

void
emit_clr_insn (rtx x)
{
  emit_move_insn (x, const0_rtx);
}

/* Generate code to store 1 in X
   assuming it contains zero beforehand.  */

void
emit_0_to_1_insn (rtx x)
{
  emit_move_insn (x, const1_rtx);
}

/* Nonzero if we can perform a comparison of mode MODE straightforwardly.
   PURPOSE describes how this comparison will be used.  CODE is the rtx
   comparison code we will be using.

   ??? Actually, CODE is slightly weaker than that.  A target is still
   required to implement all of the normal bcc operations, but not
   required to implement all (or any) of the unordered bcc operations.  */

int
can_compare_p (enum rtx_code code, enum machine_mode mode,
	       enum can_compare_purpose purpose)
{
  do
    {
      if (cmp_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
	{
	  if (purpose == ccp_jump)
	    return bcc_gen_fctn[(int) code] != NULL;
	  else if (purpose == ccp_store_flag)
	    return setcc_gen_code[(int) code] != CODE_FOR_nothing;
	  else
	    /* There's only one cmov entry point, and it's allowed to fail.  */
	    return 1;
	}
      if (purpose == ccp_jump
	  && cbranch_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
	return 1;
      if (purpose == ccp_cmov
	  && cmov_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
	return 1;
      if (purpose == ccp_store_flag
	  && cstore_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
	return 1;

      mode = GET_MODE_WIDER_MODE (mode);
    }
  while (mode != VOIDmode);

  return 0;
}

/* This function is called when we are going to emit a compare instruction that
   compares the values found in *PX and *PY, using the rtl operator COMPARISON.

   *PMODE is the mode of the inputs (in case they are const_int).
   *PUNSIGNEDP nonzero says that the operands are unsigned;
   this matters if they need to be widened.

   If they have mode BLKmode, then SIZE specifies the size of both operands.

   This function performs all the setup necessary so that the caller only has
   to emit a single comparison insn.  This setup can involve doing a BLKmode
   comparison or emitting a library call to perform the comparison if no insn
   is available to handle it.
   The values which are passed in through pointers can be modified; the caller
   should perform the comparison on the modified values.  */

static void
prepare_cmp_insn (rtx *px, rtx *py, enum rtx_code *pcomparison, rtx size,
		  enum machine_mode *pmode, int *punsignedp,
		  enum can_compare_purpose purpose)
{
  enum machine_mode mode = *pmode;
  rtx x = *px, y = *py;
  int unsignedp = *punsignedp;
  enum mode_class class;

  class = GET_MODE_CLASS (mode);

  /* They could both be VOIDmode if both args are immediate constants,
     but we should fold that at an earlier stage.
     With no special code here, this will call abort,
     reminding the programmer to implement such folding.  */

  if (mode != BLKmode && flag_force_mem)
    {
      /* Load duplicate non-volatile operands once.  */
      if (rtx_equal_p (x, y) && ! volatile_refs_p (x))
	{
	  x = force_not_mem (x);
	  y = x;
	}
      else
	{
	  x = force_not_mem (x);
	  y = force_not_mem (y);
	}
    }

  /* If we are inside an appropriately-short loop and one operand is an
     expensive constant, force it into a register.  */
  if (CONSTANT_P (x) && preserve_subexpressions_p ()
      && rtx_cost (x, COMPARE) > COSTS_N_INSNS (1))
    x = force_reg (mode, x);

  if (CONSTANT_P (y) && preserve_subexpressions_p ()
      && rtx_cost (y, COMPARE) > COSTS_N_INSNS (1))
    y = force_reg (mode, y);

#ifdef HAVE_cc0
  /* Abort if we have a non-canonical comparison.  The RTL documentation
     states that canonical comparisons are required only for targets which
     have cc0.  */
  if (CONSTANT_P (x) && ! CONSTANT_P (y))
    abort ();
#endif

  /* Don't let both operands fail to indicate the mode.  */
  if (GET_MODE (x) == VOIDmode && GET_MODE (y) == VOIDmode)
    x = force_reg (mode, x);

  /* Handle all BLKmode compares.  */

  if (mode == BLKmode)
    {
      enum machine_mode cmp_mode, result_mode;
      enum insn_code cmp_code;
      tree length_type;
      rtx libfunc;
      rtx result;
      rtx opalign
	= GEN_INT (MIN (MEM_ALIGN (x), MEM_ALIGN (y)) / BITS_PER_UNIT);

      if (size == 0)
	abort ();

      emit_queue ();
      x = protect_from_queue (x, 0);
      y = protect_from_queue (y, 0);
      size = protect_from_queue (size, 0);

      /* Try to use a memory block compare insn - either cmpstr
	 or cmpmem will do.  */
      for (cmp_mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   cmp_mode != VOIDmode;
	   cmp_mode = GET_MODE_WIDER_MODE (cmp_mode))
	{
	  cmp_code = cmpmem_optab[cmp_mode];
	  if (cmp_code == CODE_FOR_nothing)
	    cmp_code = cmpstr_optab[cmp_mode];
	  if (cmp_code == CODE_FOR_nothing)
	    continue;

	  /* Must make sure the size fits the insn's mode.  */
	  if ((GET_CODE (size) == CONST_INT
	       && INTVAL (size) >= (1 << GET_MODE_BITSIZE (cmp_mode)))
	      || (GET_MODE_BITSIZE (GET_MODE (size))
		  > GET_MODE_BITSIZE (cmp_mode)))
	    continue;

	  result_mode = insn_data[cmp_code].operand[0].mode;
	  result = gen_reg_rtx (result_mode);
	  size = convert_to_mode (cmp_mode, size, 1);
	  emit_insn (GEN_FCN (cmp_code) (result, x, y, size, opalign));

	  *px = result;
	  *py = const0_rtx;
	  *pmode = result_mode;
	  return;
	}

      /* Otherwise call a library function, memcmp if we've got it,
	 bcmp otherwise.  */
#ifdef TARGET_MEM_FUNCTIONS
      libfunc = memcmp_libfunc;
      length_type = sizetype;
#else
      libfunc = bcmp_libfunc;
      length_type = integer_type_node;
#endif
      result_mode = TYPE_MODE (integer_type_node);
      cmp_mode = TYPE_MODE (length_type);
      size = convert_to_mode (TYPE_MODE (length_type), size,
			      TREE_UNSIGNED (length_type));

      result = emit_library_call_value (libfunc, 0, LCT_PURE_MAKE_BLOCK,
					result_mode, 3,
					XEXP (x, 0), Pmode,
					XEXP (y, 0), Pmode,
					size, cmp_mode);
      *px = result;
      *py = const0_rtx;
      *pmode = result_mode;
      return;
    }

  /* Don't allow operands to the compare to trap, as that can put the
     compare and branch in different basic blocks.  */
  if (flag_non_call_exceptions)
    {
      if (may_trap_p (x))
	x = force_reg (mode, x);
      if (may_trap_p (y))
	y = force_reg (mode, y);
    }

  *px = x;
  *py = y;
  if (can_compare_p (*pcomparison, mode, purpose))
    return;

  /* Handle a lib call just for the mode we are using.  */

  if (cmp_optab->handlers[(int) mode].libfunc && class != MODE_FLOAT)
    {
      rtx libfunc = cmp_optab->handlers[(int) mode].libfunc;
      rtx result;

      /* If we want unsigned, and this mode has a distinct unsigned
	 comparison routine, use that.  */
      if (unsignedp && ucmp_optab->handlers[(int) mode].libfunc)
	libfunc = ucmp_optab->handlers[(int) mode].libfunc;

      result = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST_MAKE_BLOCK,
					word_mode, 2, x, mode, y, mode);

      /* Integer comparison returns a result that must be compared against 1,
	 so that even if we do an unsigned compare afterward,
	 there is still a value that can represent the result "less than".  */
      *px = result;
      *py = const1_rtx;
      *pmode = word_mode;
      return;
    }

  if (class == MODE_FLOAT)
    prepare_float_lib_cmp (px, py, pcomparison, pmode, punsignedp);

  else
    abort ();
}

/* Before emitting an insn with code ICODE, make sure that X, which is going
   to be used for operand OPNUM of the insn, is converted from mode MODE to
   WIDER_MODE (UNSIGNEDP determines whether it is an unsigned conversion), and
   that it is accepted by the operand predicate.  Return the new value.  */

rtx
prepare_operand (int icode, rtx x, int opnum, enum machine_mode mode,
		 enum machine_mode wider_mode, int unsignedp)
{
  x = protect_from_queue (x, 0);

  if (mode != wider_mode)
    x = convert_modes (wider_mode, mode, x, unsignedp);

  if (! (*insn_data[icode].operand[opnum].predicate)
      (x, insn_data[icode].operand[opnum].mode))
    {
      if (no_new_pseudos)
	return NULL_RTX;
      x = copy_to_mode_reg (insn_data[icode].operand[opnum].mode, x);
    }

  return x;
}

/* Subroutine of emit_cmp_and_jump_insns; this function is called when we know
   we can do the comparison.
   The arguments are the same as for emit_cmp_and_jump_insns; but LABEL may
   be NULL_RTX which indicates that only a comparison is to be generated.  */

static void
emit_cmp_and_jump_insn_1 (rtx x, rtx y, enum machine_mode mode,
			  enum rtx_code comparison, int unsignedp, rtx label)
{
  rtx test = gen_rtx_fmt_ee (comparison, mode, x, y);
  enum mode_class class = GET_MODE_CLASS (mode);
  enum machine_mode wider_mode = mode;

  /* Try combined insns first.  */
  do
    {
      enum insn_code icode;
      PUT_MODE (test, wider_mode);

      if (label)
	{
	  icode = cbranch_optab->handlers[(int) wider_mode].insn_code;

	  if (icode != CODE_FOR_nothing
	      && (*insn_data[icode].operand[0].predicate) (test, wider_mode))
	    {
	      x = prepare_operand (icode, x, 1, mode, wider_mode, unsignedp);
	      y = prepare_operand (icode, y, 2, mode, wider_mode, unsignedp);
	      emit_jump_insn (GEN_FCN (icode) (test, x, y, label));
	      return;
	    }
	}

      /* Handle some compares against zero.  */
      icode = (int) tst_optab->handlers[(int) wider_mode].insn_code;
      if (y == CONST0_RTX (mode) && icode != CODE_FOR_nothing)
	{
	  x = prepare_operand (icode, x, 0, mode, wider_mode, unsignedp);
	  emit_insn (GEN_FCN (icode) (x));
	  if (label)
	    emit_jump_insn ((*bcc_gen_fctn[(int) comparison]) (label));
	  return;
	}

      /* Handle compares for which there is a directly suitable insn.  */

      icode = (int) cmp_optab->handlers[(int) wider_mode].insn_code;
      if (icode != CODE_FOR_nothing)
	{
	  x = prepare_operand (icode, x, 0, mode, wider_mode, unsignedp);
	  y = prepare_operand (icode, y, 1, mode, wider_mode, unsignedp);
	  emit_insn (GEN_FCN (icode) (x, y));
	  if (label)
	    emit_jump_insn ((*bcc_gen_fctn[(int) comparison]) (label));
	  return;
	}

      if (class != MODE_INT && class != MODE_FLOAT
	  && class != MODE_COMPLEX_FLOAT)
	break;

      wider_mode = GET_MODE_WIDER_MODE (wider_mode);
    }
  while (wider_mode != VOIDmode);

  abort ();
}

/* Generate code to compare X with Y so that the condition codes are
   set and to jump to LABEL if the condition is true.  If X is a
   constant and Y is not a constant, then the comparison is swapped to
   ensure that the comparison RTL has the canonical form.

   UNSIGNEDP nonzero says that X and Y are unsigned; this matters if they
   need to be widened by emit_cmp_insn.  UNSIGNEDP is also used to select
   the proper branch condition code.

   If X and Y have mode BLKmode, then SIZE specifies the size of both X and Y.

   MODE is the mode of the inputs (in case they are const_int).

   COMPARISON is the rtl operator to compare with (EQ, NE, GT, etc.).  It will
   be passed unchanged to emit_cmp_insn, then potentially converted into an
   unsigned variant based on UNSIGNEDP to select a proper jump instruction.  */

void
emit_cmp_and_jump_insns (rtx x, rtx y, enum rtx_code comparison, rtx size,
			 enum machine_mode mode, int unsignedp, rtx label)
{
  rtx op0 = x, op1 = y;

  /* Swap operands and condition to ensure canonical RTL.  */
  if (swap_commutative_operands_p (x, y))
    {
      /* If we're not emitting a branch, this means some caller
         is out of sync.  */
      if (! label)
	abort ();

      op0 = y, op1 = x;
      comparison = swap_condition (comparison);
    }

#ifdef HAVE_cc0
  /* If OP0 is still a constant, then both X and Y must be constants.  Force
     X into a register to avoid aborting in emit_cmp_insn due to non-canonical
     RTL.  */
  if (CONSTANT_P (op0))
    op0 = force_reg (mode, op0);
#endif

  emit_queue ();
  if (unsignedp)
    comparison = unsigned_condition (comparison);

  prepare_cmp_insn (&op0, &op1, &comparison, size, &mode, &unsignedp,
		    ccp_jump);
  emit_cmp_and_jump_insn_1 (op0, op1, mode, comparison, unsignedp, label);
}

/* Like emit_cmp_and_jump_insns, but generate only the comparison.  */

void
emit_cmp_insn (rtx x, rtx y, enum rtx_code comparison, rtx size,
	       enum machine_mode mode, int unsignedp)
{
  emit_cmp_and_jump_insns (x, y, comparison, size, mode, unsignedp, 0);
}

/* Emit a library call comparison between floating point X and Y.
   COMPARISON is the rtl operator to compare with (EQ, NE, GT, etc.).  */

static void
prepare_float_lib_cmp (rtx *px, rtx *py, enum rtx_code *pcomparison,
		       enum machine_mode *pmode, int *punsignedp)
{
  enum rtx_code comparison = *pcomparison;
  enum rtx_code swapped = swap_condition (comparison);
  rtx x = protect_from_queue (*px, 0);
  rtx y = protect_from_queue (*py, 0);
  enum machine_mode orig_mode = GET_MODE (x);
  enum machine_mode mode;
  rtx value, target, insns, equiv;
  rtx libfunc = 0;

  for (mode = orig_mode; mode != VOIDmode; mode = GET_MODE_WIDER_MODE (mode))
    {
      if ((libfunc = code_to_optab[comparison]->handlers[mode].libfunc))
	break;

      if ((libfunc = code_to_optab[swapped]->handlers[mode].libfunc))
	{
	  rtx tmp;
	  tmp = x; x = y; y = tmp;
	  comparison = swapped;
	  break;
	}
    }

  if (mode == VOIDmode)
    abort ();

  if (mode != orig_mode)
    {
      x = convert_to_mode (mode, x, 0);
      y = convert_to_mode (mode, y, 0);
    }

  /* Attach a REG_EQUAL note describing the semantics of the libcall to
     the RTL.  The allows the RTL optimizers to delete the libcall if the
     condition can be determined at compile-time.  */
  if (comparison == UNORDERED)
    {
      rtx temp = simplify_gen_relational (NE, word_mode, mode, x, x);
      equiv = simplify_gen_relational (NE, word_mode, mode, y, y);
      equiv = simplify_gen_ternary (IF_THEN_ELSE, word_mode, word_mode,
				    temp, const_true_rtx, equiv);
    }
  else
    {
      equiv = simplify_gen_relational (comparison, word_mode, mode, x, y);
      if (! FLOAT_LIB_COMPARE_RETURNS_BOOL (mode, comparison))
	{
	  rtx true_rtx, false_rtx;

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
	      abort ();
	    }
	  equiv = simplify_gen_ternary (IF_THEN_ELSE, word_mode, word_mode,
					equiv, true_rtx, false_rtx);
	}
    }

  start_sequence ();
  value = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST,
				   word_mode, 2, x, mode, y, mode);
  insns = get_insns ();
  end_sequence ();

  target = gen_reg_rtx (word_mode);
  emit_libcall_block (insns, target, value, equiv);


  if (comparison == UNORDERED
      || FLOAT_LIB_COMPARE_RETURNS_BOOL (mode, comparison))
    comparison = NE;

  *px = target;
  *py = const0_rtx;
  *pmode = word_mode;
  *pcomparison = comparison;
  *punsignedp = 0;
}

/* Generate code to indirectly jump to a location given in the rtx LOC.  */

void
emit_indirect_jump (rtx loc)
{
  if (! ((*insn_data[(int) CODE_FOR_indirect_jump].operand[0].predicate)
	 (loc, Pmode)))
    loc = copy_to_mode_reg (Pmode, loc);

  emit_jump_insn (gen_indirect_jump (loc));
  emit_barrier ();
}

#ifdef HAVE_conditional_move

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
		       enum machine_mode cmode, rtx op2, rtx op3,
		       enum machine_mode mode, int unsignedp)
{
  rtx tem, subtarget, comparison, insn;
  enum insn_code icode;
  enum rtx_code reversed;

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
    {
      tem = op0;
      op0 = op1;
      op1 = tem;
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

  if (swap_commutative_operands_p (op2, op3)
      && ((reversed = reversed_comparison_code_parts (code, op0, op1, NULL))
          != UNKNOWN))
    {
      tem = op2;
      op2 = op3;
      op3 = tem;
      code = reversed;
    }

  if (mode == VOIDmode)
    mode = GET_MODE (op2);

  icode = movcc_gen_code[mode];

  if (icode == CODE_FOR_nothing)
    return 0;

  if (flag_force_mem)
    {
      op2 = force_not_mem (op2);
      op3 = force_not_mem (op3);
    }

  if (target)
    target = protect_from_queue (target, 1);
  else
    target = gen_reg_rtx (mode);

  subtarget = target;

  emit_queue ();

  op2 = protect_from_queue (op2, 0);
  op3 = protect_from_queue (op3, 0);

  /* If the insn doesn't accept these operands, put them in pseudos.  */

  if (! (*insn_data[icode].operand[0].predicate)
      (subtarget, insn_data[icode].operand[0].mode))
    subtarget = gen_reg_rtx (insn_data[icode].operand[0].mode);

  if (! (*insn_data[icode].operand[2].predicate)
      (op2, insn_data[icode].operand[2].mode))
    op2 = copy_to_mode_reg (insn_data[icode].operand[2].mode, op2);

  if (! (*insn_data[icode].operand[3].predicate)
      (op3, insn_data[icode].operand[3].mode))
    op3 = copy_to_mode_reg (insn_data[icode].operand[3].mode, op3);

  /* Everything should now be in the suitable form, so emit the compare insn
     and then the conditional move.  */

  comparison
    = compare_from_rtx (op0, op1, code, unsignedp, cmode, NULL_RTX);

  /* ??? Watch for const0_rtx (nop) and const_true_rtx (unconditional)?  */
  /* We can get const0_rtx or const_true_rtx in some circumstances.  Just
     return NULL and let the caller figure out how best to deal with this
     situation.  */
  if (GET_CODE (comparison) != code)
    return NULL_RTX;

  insn = GEN_FCN (icode) (subtarget, comparison, op2, op3);

  /* If that failed, then give up.  */
  if (insn == 0)
    return 0;

  emit_insn (insn);

  if (subtarget != target)
    convert_move (target, subtarget, 0);

  return target;
}

/* Return nonzero if a conditional move of mode MODE is supported.

   This function is for combine so it can tell whether an insn that looks
   like a conditional move is actually supported by the hardware.  If we
   guess wrong we lose a bit on optimization, but that's it.  */
/* ??? sparc64 supports conditionally moving integers values based on fp
   comparisons, and vice versa.  How do we handle them?  */

int
can_conditionally_move_p (enum machine_mode mode)
{
  if (movcc_gen_code[mode] != CODE_FOR_nothing)
    return 1;

  return 0;
}

#endif /* HAVE_conditional_move */

/* Emit a conditional addition instruction if the machine supports one for that
   condition and machine mode.

   OP0 and OP1 are the operands that should be compared using CODE.  CMODE is
   the mode to use should they be constants.  If it is VOIDmode, they cannot
   both be constants.

   OP2 should be stored in TARGET if the comparison is true, otherwise OP2+OP3
   should be stored there.  MODE is the mode to use should they be constants.
   If it is VOIDmode, they cannot both be constants.

   The result is either TARGET (perhaps modified) or NULL_RTX if the operation
   is not supported.  */

rtx
emit_conditional_add (rtx target, enum rtx_code code, rtx op0, rtx op1,
		      enum machine_mode cmode, rtx op2, rtx op3,
		      enum machine_mode mode, int unsignedp)
{
  rtx tem, subtarget, comparison, insn;
  enum insn_code icode;
  enum rtx_code reversed;

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
    {
      tem = op0;
      op0 = op1;
      op1 = tem;
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

  if (swap_commutative_operands_p (op2, op3)
      && ((reversed = reversed_comparison_code_parts (code, op0, op1, NULL))
          != UNKNOWN))
    {
      tem = op2;
      op2 = op3;
      op3 = tem;
      code = reversed;
    }

  if (mode == VOIDmode)
    mode = GET_MODE (op2);

  icode = addcc_optab->handlers[(int) mode].insn_code;

  if (icode == CODE_FOR_nothing)
    return 0;

  if (flag_force_mem)
    {
      op2 = force_not_mem (op2);
      op3 = force_not_mem (op3);
    }

  if (target)
    target = protect_from_queue (target, 1);
  else
    target = gen_reg_rtx (mode);

  subtarget = target;

  emit_queue ();

  op2 = protect_from_queue (op2, 0);
  op3 = protect_from_queue (op3, 0);

  /* If the insn doesn't accept these operands, put them in pseudos.  */

  if (! (*insn_data[icode].operand[0].predicate)
      (subtarget, insn_data[icode].operand[0].mode))
    subtarget = gen_reg_rtx (insn_data[icode].operand[0].mode);

  if (! (*insn_data[icode].operand[2].predicate)
      (op2, insn_data[icode].operand[2].mode))
    op2 = copy_to_mode_reg (insn_data[icode].operand[2].mode, op2);

  if (! (*insn_data[icode].operand[3].predicate)
      (op3, insn_data[icode].operand[3].mode))
    op3 = copy_to_mode_reg (insn_data[icode].operand[3].mode, op3);

  /* Everything should now be in the suitable form, so emit the compare insn
     and then the conditional move.  */

  comparison
    = compare_from_rtx (op0, op1, code, unsignedp, cmode, NULL_RTX);

  /* ??? Watch for const0_rtx (nop) and const_true_rtx (unconditional)?  */
  /* We can get const0_rtx or const_true_rtx in some circumstances.  Just
     return NULL and let the caller figure out how best to deal with this
     situation.  */
  if (GET_CODE (comparison) != code)
    return NULL_RTX;

  insn = GEN_FCN (icode) (subtarget, comparison, op2, op3);

  /* If that failed, then give up.  */
  if (insn == 0)
    return 0;

  emit_insn (insn);

  if (subtarget != target)
    convert_move (target, subtarget, 0);

  return target;
}

/* These functions attempt to generate an insn body, rather than
   emitting the insn, but if the gen function already emits them, we
   make no attempt to turn them back into naked patterns.

   They do not protect from queued increments,
   because they may be used 1) in protect_from_queue itself
   and 2) in other passes where there is no queue.  */

/* Generate and return an insn body to add Y to X.  */

rtx
gen_add2_insn (rtx x, rtx y)
{
  int icode = (int) add_optab->handlers[(int) GET_MODE (x)].insn_code;

  if (! ((*insn_data[icode].operand[0].predicate)
	 (x, insn_data[icode].operand[0].mode))
      || ! ((*insn_data[icode].operand[1].predicate)
	    (x, insn_data[icode].operand[1].mode))
      || ! ((*insn_data[icode].operand[2].predicate)
	    (y, insn_data[icode].operand[2].mode)))
    abort ();

  return (GEN_FCN (icode) (x, x, y));
}

/* Generate and return an insn body to add r1 and c,
   storing the result in r0.  */
rtx
gen_add3_insn (rtx r0, rtx r1, rtx c)
{
  int icode = (int) add_optab->handlers[(int) GET_MODE (r0)].insn_code;

  if (icode == CODE_FOR_nothing
      || ! ((*insn_data[icode].operand[0].predicate)
	    (r0, insn_data[icode].operand[0].mode))
      || ! ((*insn_data[icode].operand[1].predicate)
	    (r1, insn_data[icode].operand[1].mode))
      || ! ((*insn_data[icode].operand[2].predicate)
	    (c, insn_data[icode].operand[2].mode)))
    return NULL_RTX;

  return (GEN_FCN (icode) (r0, r1, c));
}

int
have_add2_insn (rtx x, rtx y)
{
  int icode;

  if (GET_MODE (x) == VOIDmode)
    abort ();

  icode = (int) add_optab->handlers[(int) GET_MODE (x)].insn_code;

  if (icode == CODE_FOR_nothing)
    return 0;

  if (! ((*insn_data[icode].operand[0].predicate)
	 (x, insn_data[icode].operand[0].mode))
      || ! ((*insn_data[icode].operand[1].predicate)
	    (x, insn_data[icode].operand[1].mode))
      || ! ((*insn_data[icode].operand[2].predicate)
	    (y, insn_data[icode].operand[2].mode)))
    return 0;

  return 1;
}

/* Generate and return an insn body to subtract Y from X.  */

rtx
gen_sub2_insn (rtx x, rtx y)
{
  int icode = (int) sub_optab->handlers[(int) GET_MODE (x)].insn_code;

  if (! ((*insn_data[icode].operand[0].predicate)
	 (x, insn_data[icode].operand[0].mode))
      || ! ((*insn_data[icode].operand[1].predicate)
	    (x, insn_data[icode].operand[1].mode))
      || ! ((*insn_data[icode].operand[2].predicate)
	    (y, insn_data[icode].operand[2].mode)))
    abort ();

  return (GEN_FCN (icode) (x, x, y));
}

/* Generate and return an insn body to subtract r1 and c,
   storing the result in r0.  */
rtx
gen_sub3_insn (rtx r0, rtx r1, rtx c)
{
  int icode = (int) sub_optab->handlers[(int) GET_MODE (r0)].insn_code;

  if (icode == CODE_FOR_nothing
      || ! ((*insn_data[icode].operand[0].predicate)
	    (r0, insn_data[icode].operand[0].mode))
      || ! ((*insn_data[icode].operand[1].predicate)
	    (r1, insn_data[icode].operand[1].mode))
      || ! ((*insn_data[icode].operand[2].predicate)
	    (c, insn_data[icode].operand[2].mode)))
    return NULL_RTX;

  return (GEN_FCN (icode) (r0, r1, c));
}

int
have_sub2_insn (rtx x, rtx y)
{
  int icode;

  if (GET_MODE (x) == VOIDmode)
    abort ();

  icode = (int) sub_optab->handlers[(int) GET_MODE (x)].insn_code;

  if (icode == CODE_FOR_nothing)
    return 0;

  if (! ((*insn_data[icode].operand[0].predicate)
	 (x, insn_data[icode].operand[0].mode))
      || ! ((*insn_data[icode].operand[1].predicate)
	    (x, insn_data[icode].operand[1].mode))
      || ! ((*insn_data[icode].operand[2].predicate)
	    (y, insn_data[icode].operand[2].mode)))
    return 0;

  return 1;
}

/* Generate the body of an instruction to copy Y into X.
   It may be a list of insns, if one insn isn't enough.  */

rtx
gen_move_insn (rtx x, rtx y)
{
  rtx seq;

  start_sequence ();
  emit_move_insn_1 (x, y);
  seq = get_insns ();
  end_sequence ();
  return seq;
}

/* Return the insn code used to extend FROM_MODE to TO_MODE.
   UNSIGNEDP specifies zero-extension instead of sign-extension.  If
   no such operation exists, CODE_FOR_nothing will be returned.  */

enum insn_code
can_extend_p (enum machine_mode to_mode, enum machine_mode from_mode,
	      int unsignedp)
{
  convert_optab tab;
#ifdef HAVE_ptr_extend
  if (unsignedp < 0)
    return CODE_FOR_ptr_extend;
#endif

  tab = unsignedp ? zext_optab : sext_optab;
  return tab->handlers[to_mode][from_mode].insn_code;
}

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */

rtx
gen_extend_insn (rtx x, rtx y, enum machine_mode mto,
		 enum machine_mode mfrom, int unsignedp)
{
  enum insn_code icode = can_extend_p (mto, mfrom, unsignedp);
  return GEN_FCN (icode) (x, y);
}

/* can_fix_p and can_float_p say whether the target machine
   can directly convert a given fixed point type to
   a given floating point type, or vice versa.
   The returned value is the CODE_FOR_... value to use,
   or CODE_FOR_nothing if these modes cannot be directly converted.

   *TRUNCP_PTR is set to 1 if it is necessary to output
   an explicit FTRUNC insn before the fix insn; otherwise 0.  */

static enum insn_code
can_fix_p (enum machine_mode fixmode, enum machine_mode fltmode,
	   int unsignedp, int *truncp_ptr)
{
  convert_optab tab;
  enum insn_code icode;

  tab = unsignedp ? ufixtrunc_optab : sfixtrunc_optab;
  icode = tab->handlers[fixmode][fltmode].insn_code;
  if (icode != CODE_FOR_nothing)
    {
      *truncp_ptr = 0;
      return icode;
    }

  tab = unsignedp ? ufix_optab : sfix_optab;
  icode = tab->handlers[fixmode][fltmode].insn_code;
  if (icode != CODE_FOR_nothing
      && ftrunc_optab->handlers[fltmode].insn_code != CODE_FOR_nothing)
    {
      *truncp_ptr = 1;
      return icode;
    }

  *truncp_ptr = 0;
  return CODE_FOR_nothing;
}

static enum insn_code
can_float_p (enum machine_mode fltmode, enum machine_mode fixmode,
	     int unsignedp)
{
  convert_optab tab;

  tab = unsignedp ? ufloat_optab : sfloat_optab;
  return tab->handlers[fltmode][fixmode].insn_code;
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
  enum machine_mode fmode, imode;

  /* Crash now, because we won't be able to decide which mode to use.  */
  if (GET_MODE (from) == VOIDmode)
    abort ();

  /* Look for an insn to do the conversion.  Do it in the specified
     modes if possible; otherwise convert either input, output or both to
     wider mode.  If the integer mode is wider than the mode of FROM,
     we can do the conversion signed even if the input is unsigned.  */

  for (fmode = GET_MODE (to); fmode != VOIDmode;
       fmode = GET_MODE_WIDER_MODE (fmode))
    for (imode = GET_MODE (from); imode != VOIDmode;
	 imode = GET_MODE_WIDER_MODE (imode))
      {
	int doing_unsigned = unsignedp;

	if (fmode != GET_MODE (to)
	    && significand_size (fmode) < GET_MODE_BITSIZE (GET_MODE (from)))
	  continue;

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

      /* Look for a usable floating mode FMODE wider than the source and at
	 least as wide as the target.  Using FMODE will avoid rounding woes
	 with unsigned values greater than the signed maximum value.  */

      for (fmode = GET_MODE (to);  fmode != VOIDmode;
	   fmode = GET_MODE_WIDER_MODE (fmode))
	if (GET_MODE_BITSIZE (GET_MODE (from)) < GET_MODE_BITSIZE (fmode)
	    && can_float_p (fmode, GET_MODE (from), 0) != CODE_FOR_nothing)
	  break;

      if (fmode == VOIDmode)
	{
	  /* There is no such mode.  Pretend the target is wide enough.  */
	  fmode = GET_MODE (to);

	  /* Avoid double-rounding when TO is narrower than FROM.  */
	  if ((significand_size (fmode) + 1)
	      < GET_MODE_BITSIZE (GET_MODE (from)))
	    {
	      rtx temp1;
	      rtx neglabel = gen_label_rtx ();

	      /* Don't use TARGET if it isn't a register, is a hard register,
		 or is the wrong mode.  */
	      if (GET_CODE (target) != REG
		  || REGNO (target) < FIRST_PSEUDO_REGISTER
		  || GET_MODE (target) != fmode)
		target = gen_reg_rtx (fmode);

	      imode = GET_MODE (from);
	      do_pending_stack_adjust ();

	      /* Test whether the sign bit is set.  */
	      emit_cmp_and_jump_insns (from, const0_rtx, LT, NULL_RTX, imode,
				       0, neglabel);

	      /* The sign bit is not set.  Convert as signed.  */
	      expand_float (target, from, 0);
	      emit_jump_insn (gen_jump (label));
	      emit_barrier ();

	      /* The sign bit is set.
		 Convert to a usable (positive signed) value by shifting right
		 one bit, while remembering if a nonzero bit was shifted
		 out; i.e., compute  (from & 1) | (from >> 1).  */

	      emit_label (neglabel);
	      temp = expand_binop (imode, and_optab, from, const1_rtx,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
	      temp1 = expand_shift (RSHIFT_EXPR, imode, from, integer_one_node,
				    NULL_RTX, 1);
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

      if (GET_MODE (to) != fmode
	  || GET_CODE (to) != REG || REGNO (to) < FIRST_PSEUDO_REGISTER)
	target = gen_reg_rtx (fmode);

      /* Convert as signed integer to floating.  */
      expand_float (target, from, 0);

      /* If FROM is negative (and therefore TO is negative),
	 correct its value by 2**bitwidth.  */

      do_pending_stack_adjust ();
      emit_cmp_and_jump_insns (from, const0_rtx, GE, NULL_RTX, GET_MODE (from),
			       0, label);


      real_2expN (&offset, GET_MODE_BITSIZE (GET_MODE (from)));
      temp = expand_binop (fmode, add_optab, target,
			   CONST_DOUBLE_FROM_REAL_VALUE (offset, fmode),
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
      rtx insns;
      rtx value;
      convert_optab tab = unsignedp ? ufloat_optab : sfloat_optab;

      to = protect_from_queue (to, 1);
      from = protect_from_queue (from, 0);

      if (GET_MODE_SIZE (GET_MODE (from)) < GET_MODE_SIZE (SImode))
	from = convert_to_mode (SImode, from, unsignedp);

      if (flag_force_mem)
	from = force_not_mem (from);

      libfunc = tab->handlers[GET_MODE (to)][GET_MODE (from)].libfunc;
      if (!libfunc)
	abort ();

      start_sequence ();

      value = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST,
				       GET_MODE (to), 1, from,
				       GET_MODE (from));
      insns = get_insns ();
      end_sequence ();

      emit_libcall_block (insns, target, value,
			  gen_rtx_FLOAT (GET_MODE (to), from));
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

/* expand_fix: generate code to convert FROM to fixed point
   and store in TO.  FROM must be floating point.  */

static rtx
ftruncify (rtx x)
{
  rtx temp = gen_reg_rtx (GET_MODE (x));
  return expand_unop (GET_MODE (x), ftrunc_optab, x, temp, 0);
}

void
expand_fix (rtx to, rtx from, int unsignedp)
{
  enum insn_code icode;
  rtx target = to;
  enum machine_mode fmode, imode;
  int must_trunc = 0;

  /* We first try to find a pair of modes, one real and one integer, at
     least as wide as FROM and TO, respectively, in which we can open-code
     this conversion.  If the integer mode is wider than the mode of TO,
     we can do the conversion either signed or unsigned.  */

  for (fmode = GET_MODE (from); fmode != VOIDmode;
       fmode = GET_MODE_WIDER_MODE (fmode))
    for (imode = GET_MODE (to); imode != VOIDmode;
	 imode = GET_MODE_WIDER_MODE (imode))
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

  /* For an unsigned conversion, there is one more way to do it.
     If we have a signed conversion, we generate code that compares
     the real value to the largest representable positive number.  If if
     is smaller, the conversion is done normally.  Otherwise, subtract
     one plus the highest signed number, convert, and add it back.

     We only need to check all real modes, since we know we didn't find
     anything with a wider integer mode.

     This code used to extend FP value into mode wider than the destination.
     This is not needed.  Consider, for instance conversion from SFmode
     into DImode.

     The hot path trought the code is dealing with inputs smaller than 2^63
     and doing just the conversion, so there is no bits to lose.

     In the other path we know the value is positive in the range 2^63..2^64-1
     inclusive.  (as for other imput overflow happens and result is undefined)
     So we know that the most important bit set in mantissa corresponds to
     2^63.  The subtraction of 2^63 should not generate any rounding as it
     simply clears out that bit.  The rest is trivial.  */

  if (unsignedp && GET_MODE_BITSIZE (GET_MODE (to)) <= HOST_BITS_PER_WIDE_INT)
    for (fmode = GET_MODE (from); fmode != VOIDmode;
	 fmode = GET_MODE_WIDER_MODE (fmode))
      if (CODE_FOR_nothing != can_fix_p (GET_MODE (to), fmode, 0,
					 &must_trunc))
	{
	  int bitsize;
	  REAL_VALUE_TYPE offset;
	  rtx limit, lab1, lab2, insn;

	  bitsize = GET_MODE_BITSIZE (GET_MODE (to));
	  real_2expN (&offset, bitsize - 1);
	  limit = CONST_DOUBLE_FROM_REAL_VALUE (offset, fmode);
	  lab1 = gen_label_rtx ();
	  lab2 = gen_label_rtx ();

	  emit_queue ();
	  to = protect_from_queue (to, 1);
	  from = protect_from_queue (from, 0);

	  if (flag_force_mem)
	    from = force_not_mem (from);

	  if (fmode != GET_MODE (from))
	    from = convert_to_mode (fmode, from, 0);

	  /* See if we need to do the subtraction.  */
	  do_pending_stack_adjust ();
	  emit_cmp_and_jump_insns (from, limit, GE, NULL_RTX, GET_MODE (from),
				   0, lab1);

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
				 gen_int_mode
				 ((HOST_WIDE_INT) 1 << (bitsize - 1),
				  GET_MODE (to)),
				 to, 1, OPTAB_LIB_WIDEN);

	  if (target != to)
	    emit_move_insn (to, target);

	  emit_label (lab2);

	  if (mov_optab->handlers[(int) GET_MODE (to)].insn_code
	      != CODE_FOR_nothing)
	    {
	      /* Make a place for a REG_NOTE and add it.  */
	      insn = emit_move_insn (to, to);
	      set_unique_reg_note (insn,
	                           REG_EQUAL,
				   gen_rtx_fmt_e (UNSIGNED_FIX,
						  GET_MODE (to),
						  copy_rtx (from)));
	    }

	  return;
	}

  /* We can't do it with an insn, so use a library call.  But first ensure
     that the mode of TO is at least as wide as SImode, since those are the
     only library calls we know about.  */

  if (GET_MODE_SIZE (GET_MODE (to)) < GET_MODE_SIZE (SImode))
    {
      target = gen_reg_rtx (SImode);

      expand_fix (target, from, unsignedp);
    }
  else
    {
      rtx insns;
      rtx value;
      rtx libfunc;
      
      convert_optab tab = unsignedp ? ufix_optab : sfix_optab;
      libfunc = tab->handlers[GET_MODE (to)][GET_MODE (from)].libfunc;
      if (!libfunc)
	abort ();

      to = protect_from_queue (to, 1);
      from = protect_from_queue (from, 0);

      if (flag_force_mem)
	from = force_not_mem (from);

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

/* Report whether we have an instruction to perform the operation
   specified by CODE on operands of mode MODE.  */
int
have_insn_for (enum rtx_code code, enum machine_mode mode)
{
  return (code_to_optab[(int) code] != 0
	  && (code_to_optab[(int) code]->handlers[(int) mode].insn_code
	      != CODE_FOR_nothing));
}

/* Create a blank optab.  */
static optab
new_optab (void)
{
  int i;
  optab op = ggc_alloc (sizeof (struct optab));
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      op->handlers[i].insn_code = CODE_FOR_nothing;
      op->handlers[i].libfunc = 0;
    }

  return op;
}

static convert_optab
new_convert_optab (void)
{
  int i, j;
  convert_optab op = ggc_alloc (sizeof (struct convert_optab));
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    for (j = 0; j < NUM_MACHINE_MODES; j++)
      {
	op->handlers[i][j].insn_code = CODE_FOR_nothing;
	op->handlers[i][j].libfunc = 0;
      }
  return op;
}

/* Same, but fill in its code as CODE, and write it into the
   code_to_optab table.  */
static inline optab
init_optab (enum rtx_code code)
{
  optab op = new_optab ();
  op->code = code;
  code_to_optab[(int) code] = op;
  return op;
}

/* Same, but fill in its code as CODE, and do _not_ write it into
   the code_to_optab table.  */
static inline optab
init_optabv (enum rtx_code code)
{
  optab op = new_optab ();
  op->code = code;
  return op;
}

/* Conversion optabs never go in the code_to_optab table.  */
static inline convert_optab
init_convert_optab (enum rtx_code code)
{
  convert_optab op = new_convert_optab ();
  op->code = code;
  return op;
}

/* Initialize the libfunc fields of an entire group of entries in some
   optab.  Each entry is set equal to a string consisting of a leading
   pair of underscores followed by a generic operation name followed by
   a mode name (downshifted to lowercase) followed by a single character
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
init_libfuncs (optab optable, int first_mode, int last_mode,
	       const char *opname, int suffix)
{
  int mode;
  unsigned opname_len = strlen (opname);

  for (mode = first_mode; (int) mode <= (int) last_mode;
       mode = (enum machine_mode) ((int) mode + 1))
    {
      const char *mname = GET_MODE_NAME (mode);
      unsigned mname_len = strlen (mname);
      char *libfunc_name = alloca (2 + opname_len + mname_len + 1 + 1);
      char *p;
      const char *q;

      p = libfunc_name;
      *p++ = '_';
      *p++ = '_';
      for (q = opname; *q; )
	*p++ = *q++;
      for (q = mname; *q; q++)
	*p++ = TOLOWER (*q);
      *p++ = suffix;
      *p = '\0';

      optable->handlers[(int) mode].libfunc
	= init_one_libfunc (ggc_alloc_string (libfunc_name, p - libfunc_name));
    }
}

/* Initialize the libfunc fields of an entire group of entries in some
   optab which correspond to all integer mode operations.  The parameters
   have the same meaning as similarly named ones for the `init_libfuncs'
   routine.  (See above).  */

static void
init_integral_libfuncs (optab optable, const char *opname, int suffix)
{
  int maxsize = 2*BITS_PER_WORD;
  if (maxsize < LONG_LONG_TYPE_SIZE)
    maxsize = LONG_LONG_TYPE_SIZE;
  init_libfuncs (optable, word_mode,
		 mode_for_size (maxsize, MODE_INT, 0),
		 opname, suffix);
}

/* Initialize the libfunc fields of an entire group of entries in some
   optab which correspond to all real mode operations.  The parameters
   have the same meaning as similarly named ones for the `init_libfuncs'
   routine.  (See above).  */

static void
init_floating_libfuncs (optab optable, const char *opname, int suffix)
{
  init_libfuncs (optable, MIN_MODE_FLOAT, MAX_MODE_FLOAT, opname, suffix);
}

/* Initialize the libfunc fields of an entire group of entries of an
   inter-mode-class conversion optab.  The string formation rules are
   similar to the ones for init_libfuncs, above, but instead of having
   a mode name and an operand count these functions have two mode names
   and no operand count.  */
static void
init_interclass_conv_libfuncs (convert_optab tab, const char *opname,
			       enum mode_class from_class,
			       enum mode_class to_class)
{
  enum machine_mode first_from_mode = GET_CLASS_NARROWEST_MODE (from_class);
  enum machine_mode first_to_mode = GET_CLASS_NARROWEST_MODE (to_class);
  size_t opname_len = strlen (opname);
  size_t max_mname_len = 0;

  enum machine_mode fmode, tmode;
  const char *fname, *tname;
  const char *q;
  char *libfunc_name, *suffix;
  char *p;

  for (fmode = first_from_mode;
       fmode != VOIDmode;
       fmode = GET_MODE_WIDER_MODE (fmode))
    max_mname_len = MAX (max_mname_len, strlen (GET_MODE_NAME (fmode)));

  for (tmode = first_to_mode;
       tmode != VOIDmode;
       tmode = GET_MODE_WIDER_MODE (tmode))
    max_mname_len = MAX (max_mname_len, strlen (GET_MODE_NAME (tmode)));

  libfunc_name = alloca (2 + opname_len + 2*max_mname_len + 1 + 1);
  libfunc_name[0] = '_';
  libfunc_name[1] = '_';
  memcpy (&libfunc_name[2], opname, opname_len);
  suffix = libfunc_name + opname_len + 2;

  for (fmode = first_from_mode; fmode != VOIDmode;
       fmode = GET_MODE_WIDER_MODE (fmode))
    for (tmode = first_to_mode; tmode != VOIDmode;
	 tmode = GET_MODE_WIDER_MODE (tmode))
      {
	fname = GET_MODE_NAME (fmode);
	tname = GET_MODE_NAME (tmode);

	p = suffix;
	for (q = fname; *q; p++, q++)
	  *p = TOLOWER (*q);
	for (q = tname; *q; p++, q++)
	  *p = TOLOWER (*q);

	*p = '\0';

	tab->handlers[tmode][fmode].libfunc
	  = init_one_libfunc (ggc_alloc_string (libfunc_name,
						p - libfunc_name));
      }
}

/* Initialize the libfunc fields of an entire group of entries of an
   intra-mode-class conversion optab.  The string formation rules are
   similar to the ones for init_libfunc, above.  WIDENING says whether
   the optab goes from narrow to wide modes or vice versa.  These functions
   have two mode names _and_ an operand count.  */
static void
init_intraclass_conv_libfuncs (convert_optab tab, const char *opname,
			       enum mode_class class, bool widening)
{
  enum machine_mode first_mode = GET_CLASS_NARROWEST_MODE (class);
  size_t opname_len = strlen (opname);
  size_t max_mname_len = 0;

  enum machine_mode nmode, wmode;
  const char *nname, *wname;
  const char *q;
  char *libfunc_name, *suffix;
  char *p;

  for (nmode = first_mode; nmode != VOIDmode;
       nmode = GET_MODE_WIDER_MODE (nmode))
    max_mname_len = MAX (max_mname_len, strlen (GET_MODE_NAME (nmode)));

  libfunc_name = alloca (2 + opname_len + 2*max_mname_len + 1 + 1);
  libfunc_name[0] = '_';
  libfunc_name[1] = '_';
  memcpy (&libfunc_name[2], opname, opname_len);
  suffix = libfunc_name + opname_len + 2;

  for (nmode = first_mode; nmode != VOIDmode;
       nmode = GET_MODE_WIDER_MODE (nmode))
    for (wmode = GET_MODE_WIDER_MODE (nmode); wmode != VOIDmode;
	 wmode = GET_MODE_WIDER_MODE (wmode))
      {
	nname = GET_MODE_NAME (nmode);
	wname = GET_MODE_NAME (wmode);

	p = suffix;
	for (q = widening ? nname : wname; *q; p++, q++)
	  *p = TOLOWER (*q);
	for (q = widening ? wname : nname; *q; p++, q++)
	  *p = TOLOWER (*q);

	*p++ = '2';
	*p = '\0';

	tab->handlers[widening ? wmode : nmode]
	             [widening ? nmode : wmode].libfunc
	  = init_one_libfunc (ggc_alloc_string (libfunc_name,
						p - libfunc_name));
      }
}


rtx
init_one_libfunc (const char *name)
{
  rtx symbol;

  /* Create a FUNCTION_DECL that can be passed to
     targetm.encode_section_info.  */
  /* ??? We don't have any type information except for this is
     a function.  Pretend this is "int foo()".  */
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name),
			  build_function_type (integer_type_node, NULL_TREE));
  DECL_ARTIFICIAL (decl) = 1;
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  symbol = XEXP (DECL_RTL (decl), 0);

  /* Zap the nonsensical SYMBOL_REF_DECL for this.  What we're left with
     are the flags assigned by targetm.encode_section_info.  */
  SYMBOL_REF_DECL (symbol) = 0;

  return symbol;
}

/* Call this to reset the function entry for one optab (OPTABLE) in mode
   MODE to NAME, which should be either 0 or a string constant.  */
void
set_optab_libfunc (optab optable, enum machine_mode mode, const char *name)
{
  if (name)
    optable->handlers[mode].libfunc = init_one_libfunc (name);
  else
    optable->handlers[mode].libfunc = 0;
}

/* Call this to reset the function entry for one conversion optab
   (OPTABLE) from mode FMODE to mode TMODE to NAME, which should be
   either 0 or a string constant.  */
void
set_conv_libfunc (convert_optab optable, enum machine_mode tmode,
		  enum machine_mode fmode, const char *name)
{
  if (name)
    optable->handlers[tmode][fmode].libfunc = init_one_libfunc (name);
  else
    optable->handlers[tmode][fmode].libfunc = 0;
}

/* Call this once to initialize the contents of the optabs
   appropriately for the current target machine.  */

void
init_optabs (void)
{
  unsigned int i;

  /* Start by initializing all tables to contain CODE_FOR_nothing.  */

  for (i = 0; i < NUM_RTX_CODE; i++)
    setcc_gen_code[i] = CODE_FOR_nothing;

#ifdef HAVE_conditional_move
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    movcc_gen_code[i] = CODE_FOR_nothing;
#endif

  add_optab = init_optab (PLUS);
  addv_optab = init_optabv (PLUS);
  sub_optab = init_optab (MINUS);
  subv_optab = init_optabv (MINUS);
  smul_optab = init_optab (MULT);
  smulv_optab = init_optabv (MULT);
  smul_highpart_optab = init_optab (UNKNOWN);
  umul_highpart_optab = init_optab (UNKNOWN);
  smul_widen_optab = init_optab (UNKNOWN);
  umul_widen_optab = init_optab (UNKNOWN);
  sdiv_optab = init_optab (DIV);
  sdivv_optab = init_optabv (DIV);
  sdivmod_optab = init_optab (UNKNOWN);
  udiv_optab = init_optab (UDIV);
  udivmod_optab = init_optab (UNKNOWN);
  smod_optab = init_optab (MOD);
  umod_optab = init_optab (UMOD);
  ftrunc_optab = init_optab (UNKNOWN);
  and_optab = init_optab (AND);
  ior_optab = init_optab (IOR);
  xor_optab = init_optab (XOR);
  ashl_optab = init_optab (ASHIFT);
  ashr_optab = init_optab (ASHIFTRT);
  lshr_optab = init_optab (LSHIFTRT);
  rotl_optab = init_optab (ROTATE);
  rotr_optab = init_optab (ROTATERT);
  smin_optab = init_optab (SMIN);
  smax_optab = init_optab (SMAX);
  umin_optab = init_optab (UMIN);
  umax_optab = init_optab (UMAX);
  pow_optab = init_optab (UNKNOWN);
  atan2_optab = init_optab (UNKNOWN);

  /* These three have codes assigned exclusively for the sake of
     have_insn_for.  */
  mov_optab = init_optab (SET);
  movstrict_optab = init_optab (STRICT_LOW_PART);
  cmp_optab = init_optab (COMPARE);

  ucmp_optab = init_optab (UNKNOWN);
  tst_optab = init_optab (UNKNOWN);

  eq_optab = init_optab (EQ);
  ne_optab = init_optab (NE);
  gt_optab = init_optab (GT);
  ge_optab = init_optab (GE);
  lt_optab = init_optab (LT);
  le_optab = init_optab (LE);
  unord_optab = init_optab (UNORDERED);

  neg_optab = init_optab (NEG);
  negv_optab = init_optabv (NEG);
  abs_optab = init_optab (ABS);
  absv_optab = init_optabv (ABS);
  addcc_optab = init_optab (UNKNOWN);
  one_cmpl_optab = init_optab (NOT);
  ffs_optab = init_optab (FFS);
  clz_optab = init_optab (CLZ);
  ctz_optab = init_optab (CTZ);
  popcount_optab = init_optab (POPCOUNT);
  parity_optab = init_optab (PARITY);
  sqrt_optab = init_optab (SQRT);
  floor_optab = init_optab (UNKNOWN);
  ceil_optab = init_optab (UNKNOWN);
  round_optab = init_optab (UNKNOWN);
  btrunc_optab = init_optab (UNKNOWN);
  nearbyint_optab = init_optab (UNKNOWN);
  sin_optab = init_optab (UNKNOWN);
  cos_optab = init_optab (UNKNOWN);
  exp_optab = init_optab (UNKNOWN);
  log_optab = init_optab (UNKNOWN);
  tan_optab = init_optab (UNKNOWN);
  atan_optab = init_optab (UNKNOWN);
  strlen_optab = init_optab (UNKNOWN);
  cbranch_optab = init_optab (UNKNOWN);
  cmov_optab = init_optab (UNKNOWN);
  cstore_optab = init_optab (UNKNOWN);
  push_optab = init_optab (UNKNOWN);

  vec_extract_optab = init_optab (UNKNOWN);
  vec_set_optab = init_optab (UNKNOWN);
  vec_init_optab = init_optab (UNKNOWN);
  /* Conversions.  */
  sext_optab = init_convert_optab (SIGN_EXTEND);
  zext_optab = init_convert_optab (ZERO_EXTEND);
  trunc_optab = init_convert_optab (TRUNCATE);
  sfix_optab = init_convert_optab (FIX);
  ufix_optab = init_convert_optab (UNSIGNED_FIX);
  sfixtrunc_optab = init_convert_optab (UNKNOWN);
  ufixtrunc_optab = init_convert_optab (UNKNOWN);
  sfloat_optab = init_convert_optab (FLOAT);
  ufloat_optab = init_convert_optab (UNSIGNED_FLOAT);

  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      movstr_optab[i] = CODE_FOR_nothing;
      clrstr_optab[i] = CODE_FOR_nothing;
      cmpstr_optab[i] = CODE_FOR_nothing;
      cmpmem_optab[i] = CODE_FOR_nothing;

#ifdef HAVE_SECONDARY_RELOADS
      reload_in_optab[i] = reload_out_optab[i] = CODE_FOR_nothing;
#endif
    }

  /* Fill in the optabs with the insns we support.  */
  init_all_optabs ();

  /* Initialize the optabs with the names of the library functions.  */
  init_integral_libfuncs (add_optab, "add", '3');
  init_floating_libfuncs (add_optab, "add", '3');
  init_integral_libfuncs (addv_optab, "addv", '3');
  init_floating_libfuncs (addv_optab, "add", '3');
  init_integral_libfuncs (sub_optab, "sub", '3');
  init_floating_libfuncs (sub_optab, "sub", '3');
  init_integral_libfuncs (subv_optab, "subv", '3');
  init_floating_libfuncs (subv_optab, "sub", '3');
  init_integral_libfuncs (smul_optab, "mul", '3');
  init_floating_libfuncs (smul_optab, "mul", '3');
  init_integral_libfuncs (smulv_optab, "mulv", '3');
  init_floating_libfuncs (smulv_optab, "mul", '3');
  init_integral_libfuncs (sdiv_optab, "div", '3');
  init_floating_libfuncs (sdiv_optab, "div", '3');
  init_integral_libfuncs (sdivv_optab, "divv", '3');
  init_integral_libfuncs (udiv_optab, "udiv", '3');
  init_integral_libfuncs (sdivmod_optab, "divmod", '4');
  init_integral_libfuncs (udivmod_optab, "udivmod", '4');
  init_integral_libfuncs (smod_optab, "mod", '3');
  init_integral_libfuncs (umod_optab, "umod", '3');
  init_floating_libfuncs (ftrunc_optab, "ftrunc", '2');
  init_integral_libfuncs (and_optab, "and", '3');
  init_integral_libfuncs (ior_optab, "ior", '3');
  init_integral_libfuncs (xor_optab, "xor", '3');
  init_integral_libfuncs (ashl_optab, "ashl", '3');
  init_integral_libfuncs (ashr_optab, "ashr", '3');
  init_integral_libfuncs (lshr_optab, "lshr", '3');
  init_integral_libfuncs (smin_optab, "min", '3');
  init_floating_libfuncs (smin_optab, "min", '3');
  init_integral_libfuncs (smax_optab, "max", '3');
  init_floating_libfuncs (smax_optab, "max", '3');
  init_integral_libfuncs (umin_optab, "umin", '3');
  init_integral_libfuncs (umax_optab, "umax", '3');
  init_integral_libfuncs (neg_optab, "neg", '2');
  init_floating_libfuncs (neg_optab, "neg", '2');
  init_integral_libfuncs (negv_optab, "negv", '2');
  init_floating_libfuncs (negv_optab, "neg", '2');
  init_integral_libfuncs (one_cmpl_optab, "one_cmpl", '2');
  init_integral_libfuncs (ffs_optab, "ffs", '2');
  init_integral_libfuncs (clz_optab, "clz", '2');
  init_integral_libfuncs (ctz_optab, "ctz", '2');
  init_integral_libfuncs (popcount_optab, "popcount", '2');
  init_integral_libfuncs (parity_optab, "parity", '2');

  /* Comparison libcalls for integers MUST come in pairs, signed/unsigned.  */
  init_integral_libfuncs (cmp_optab, "cmp", '2');
  init_integral_libfuncs (ucmp_optab, "ucmp", '2');
  init_floating_libfuncs (cmp_optab, "cmp", '2');

  /* EQ etc are floating point only.  */
  init_floating_libfuncs (eq_optab, "eq", '2');
  init_floating_libfuncs (ne_optab, "ne", '2');
  init_floating_libfuncs (gt_optab, "gt", '2');
  init_floating_libfuncs (ge_optab, "ge", '2');
  init_floating_libfuncs (lt_optab, "lt", '2');
  init_floating_libfuncs (le_optab, "le", '2');
  init_floating_libfuncs (unord_optab, "unord", '2');

  /* Conversions.  */
  init_interclass_conv_libfuncs (sfloat_optab, "float", MODE_INT, MODE_FLOAT);
  init_interclass_conv_libfuncs (sfix_optab, "fix",     MODE_FLOAT, MODE_INT);
  init_interclass_conv_libfuncs (ufix_optab, "fixuns",  MODE_FLOAT, MODE_INT);

  /* sext_optab is also used for FLOAT_EXTEND.  */
  init_intraclass_conv_libfuncs (sext_optab, "extend", MODE_FLOAT, true);
  init_intraclass_conv_libfuncs (trunc_optab, "trunc", MODE_FLOAT, false);

  /* Use cabs for double complex abs, since systems generally have cabs.
     Don't define any libcall for float complex, so that cabs will be used.  */
  if (complex_double_type_node)
    abs_optab->handlers[TYPE_MODE (complex_double_type_node)].libfunc
      = init_one_libfunc ("cabs");

  /* The ffs function operates on `int'.  */
  ffs_optab->handlers[(int) mode_for_size (INT_TYPE_SIZE, MODE_INT, 0)].libfunc
    = init_one_libfunc ("ffs");

  abort_libfunc = init_one_libfunc ("abort");
  memcpy_libfunc = init_one_libfunc ("memcpy");
  memmove_libfunc = init_one_libfunc ("memmove");
  bcopy_libfunc = init_one_libfunc ("bcopy");
  memcmp_libfunc = init_one_libfunc ("memcmp");
  bcmp_libfunc = init_one_libfunc ("__gcc_bcmp");
  memset_libfunc = init_one_libfunc ("memset");
  bzero_libfunc = init_one_libfunc ("bzero");
  setbits_libfunc = init_one_libfunc ("__setbits");

  unwind_resume_libfunc = init_one_libfunc (USING_SJLJ_EXCEPTIONS
					    ? "_Unwind_SjLj_Resume"
					    : "_Unwind_Resume");
#ifndef DONT_USE_BUILTIN_SETJMP
  setjmp_libfunc = init_one_libfunc ("__builtin_setjmp");
  longjmp_libfunc = init_one_libfunc ("__builtin_longjmp");
#else
  setjmp_libfunc = init_one_libfunc ("setjmp");
  longjmp_libfunc = init_one_libfunc ("longjmp");
#endif
  unwind_sjlj_register_libfunc = init_one_libfunc ("_Unwind_SjLj_Register");
  unwind_sjlj_unregister_libfunc
    = init_one_libfunc ("_Unwind_SjLj_Unregister");

  /* For function entry/exit instrumentation.  */
  profile_function_entry_libfunc
    = init_one_libfunc ("__cyg_profile_func_enter");
  profile_function_exit_libfunc
    = init_one_libfunc ("__cyg_profile_func_exit");

  gcov_flush_libfunc = init_one_libfunc ("__gcov_flush");
  gcov_init_libfunc = init_one_libfunc ("__gcov_init");

  if (HAVE_conditional_trap)
    trap_rtx = gen_rtx_fmt_ee (EQ, VOIDmode, NULL_RTX, NULL_RTX);

  /* Allow the target to add more libcalls or rename some, etc.  */
  targetm.init_libfuncs ();
}

/* Generate insns to trap with code TCODE if OP1 and OP2 satisfy condition
   CODE.  Return 0 on failure.  */

rtx
gen_cond_trap (enum rtx_code code ATTRIBUTE_UNUSED, rtx op1,
	       rtx op2 ATTRIBUTE_UNUSED, rtx tcode ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = GET_MODE (op1);
  enum insn_code icode;
  rtx insn;

  if (!HAVE_conditional_trap)
    return 0;

  if (mode == VOIDmode)
    return 0;

  icode = cmp_optab->handlers[(int) mode].insn_code;
  if (icode == CODE_FOR_nothing)
    return 0;

  start_sequence ();
  op1 = prepare_operand (icode, op1, 0, mode, mode, 0);
  op2 = prepare_operand (icode, op2, 1, mode, mode, 0);
  if (!op1 || !op2)
    {
      end_sequence ();
      return 0;
    }
  emit_insn (GEN_FCN (icode) (op1, op2));

  PUT_CODE (trap_rtx, code);
  insn = gen_conditional_trap (trap_rtx, tcode);
  if (insn)
    {
      emit_insn (insn);
      insn = get_insns ();
    }
  end_sequence ();

  return insn;
}

#include "gt-optabs.h"
