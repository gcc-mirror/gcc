/* Common subexpression elimination for GNU compiler.
   Copyright (C) 1987, 88, 89, 92-7, 1998, 1999 Free Software Foundation, Inc.

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
/* stdio.h must precede rtl.h for FFS.  */
#include "system.h"
#include <setjmp.h>

#include "rtl.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "real.h"
#include "insn-config.h"
#include "recog.h"
#include "function.h"
#include "expr.h"
#include "toplev.h"
#include "output.h"

/* Simplification and canonicalization of RTL.  */

/* Nonzero if X has the form (PLUS frame-pointer integer).  We check for
   virtual regs here because the simplify_*_operation routines are called
   by integrate.c, which is called before virtual register instantiation.

   ?!? FIXED_BASE_PLUS_P and NONZERO_BASE_PLUS_P need to move into 
   a header file so that their definitions can be shared with the
   simplification routines in simplify-rtx.c.  Until then, do not
   change these macros without also changing the copy in simplify-rtx.c.  */

#define FIXED_BASE_PLUS_P(X)					\
  ((X) == frame_pointer_rtx || (X) == hard_frame_pointer_rtx	\
   || ((X) == arg_pointer_rtx && fixed_regs[ARG_POINTER_REGNUM])\
   || (X) == virtual_stack_vars_rtx				\
   || (X) == virtual_incoming_args_rtx				\
   || (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == CONST_INT \
       && (XEXP (X, 0) == frame_pointer_rtx			\
	   || XEXP (X, 0) == hard_frame_pointer_rtx		\
	   || ((X) == arg_pointer_rtx				\
	       && fixed_regs[ARG_POINTER_REGNUM])		\
	   || XEXP (X, 0) == virtual_stack_vars_rtx		\
	   || XEXP (X, 0) == virtual_incoming_args_rtx))	\
   || GET_CODE (X) == ADDRESSOF)

/* Similar, but also allows reference to the stack pointer.

   This used to include FIXED_BASE_PLUS_P, however, we can't assume that
   arg_pointer_rtx by itself is nonzero, because on at least one machine,
   the i960, the arg pointer is zero when it is unused.  */

#define NONZERO_BASE_PLUS_P(X)					\
  ((X) == frame_pointer_rtx || (X) == hard_frame_pointer_rtx	\
   || (X) == virtual_stack_vars_rtx				\
   || (X) == virtual_incoming_args_rtx				\
   || (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == CONST_INT \
       && (XEXP (X, 0) == frame_pointer_rtx			\
	   || XEXP (X, 0) == hard_frame_pointer_rtx		\
	   || ((X) == arg_pointer_rtx				\
	       && fixed_regs[ARG_POINTER_REGNUM])		\
	   || XEXP (X, 0) == virtual_stack_vars_rtx		\
	   || XEXP (X, 0) == virtual_incoming_args_rtx))	\
   || (X) == stack_pointer_rtx					\
   || (X) == virtual_stack_dynamic_rtx				\
   || (X) == virtual_outgoing_args_rtx				\
   || (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == CONST_INT \
       && (XEXP (X, 0) == stack_pointer_rtx			\
	   || XEXP (X, 0) == virtual_stack_dynamic_rtx		\
	   || XEXP (X, 0) == virtual_outgoing_args_rtx))	\
   || GET_CODE (X) == ADDRESSOF)


static rtx simplify_plus_minus	PARAMS ((enum rtx_code, enum machine_mode,
				       rtx, rtx));
static void check_fold_consts	PARAMS ((PTR));

/* Make a binary operation by properly ordering the operands and 
   seeing if the expression folds.  */

rtx
simplify_gen_binary (code, mode, op0, op1)
     enum rtx_code code;
     enum machine_mode mode;
     rtx op0, op1;
{
  rtx tem;

  /* Put complex operands first and constants second if commutative.  */
  if (GET_RTX_CLASS (code) == 'c'
      && ((CONSTANT_P (op0) && GET_CODE (op1) != CONST_INT)
	  || (GET_RTX_CLASS (GET_CODE (op0)) == 'o'
	      && GET_RTX_CLASS (GET_CODE (op1)) != 'o')
	  || (GET_CODE (op0) == SUBREG
	      && GET_RTX_CLASS (GET_CODE (SUBREG_REG (op0))) == 'o'
	      && GET_RTX_CLASS (GET_CODE (op1)) != 'o')))
    tem = op0, op0 = op1, op1 = tem;

  /* If this simplifies, do it.  */
  tem = simplify_binary_operation (code, mode, op0, op1);

  if (tem)
    return tem;

  /* Handle addition and subtraction of CONST_INT specially.  Otherwise,
     just form the operation.  */

  if (code == PLUS && GET_CODE (op1) == CONST_INT
      && GET_MODE (op0) != VOIDmode)
    return plus_constant (op0, INTVAL (op1));
  else if (code == MINUS && GET_CODE (op1) == CONST_INT
	   && GET_MODE (op0) != VOIDmode)
    return plus_constant (op0, - INTVAL (op1));
  else
    return gen_rtx_fmt_ee (code, mode, op0, op1);
}

/* Try to simplify a unary operation CODE whose output mode is to be
   MODE with input operand OP whose mode was originally OP_MODE.
   Return zero if no simplification can be made.  */

rtx
simplify_unary_operation (code, mode, op, op_mode)
     enum rtx_code code;
     enum machine_mode mode;
     rtx op;
     enum machine_mode op_mode;
{
  register int width = GET_MODE_BITSIZE (mode);

  /* The order of these tests is critical so that, for example, we don't
     check the wrong mode (input vs. output) for a conversion operation,
     such as FIX.  At some point, this should be simplified.  */

#if !defined(REAL_IS_NOT_DOUBLE) || defined(REAL_ARITHMETIC)

  if (code == FLOAT && GET_MODE (op) == VOIDmode
      && (GET_CODE (op) == CONST_DOUBLE || GET_CODE (op) == CONST_INT))
    {
      HOST_WIDE_INT hv, lv;
      REAL_VALUE_TYPE d;

      if (GET_CODE (op) == CONST_INT)
	lv = INTVAL (op), hv = INTVAL (op) < 0 ? -1 : 0;
      else
	lv = CONST_DOUBLE_LOW (op),  hv = CONST_DOUBLE_HIGH (op);

#ifdef REAL_ARITHMETIC
      REAL_VALUE_FROM_INT (d, lv, hv, mode);
#else
      if (hv < 0)
	{
	  d = (double) (~ hv);
	  d *= ((double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2))
		* (double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2)));
	  d += (double) (unsigned HOST_WIDE_INT) (~ lv);
	  d = (- d - 1.0);
	}
      else
	{
	  d = (double) hv;
	  d *= ((double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2))
		* (double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2)));
	  d += (double) (unsigned HOST_WIDE_INT) lv;
	}
#endif  /* REAL_ARITHMETIC */
      d = real_value_truncate (mode, d);
      return CONST_DOUBLE_FROM_REAL_VALUE (d, mode);
    }
  else if (code == UNSIGNED_FLOAT && GET_MODE (op) == VOIDmode
	   && (GET_CODE (op) == CONST_DOUBLE || GET_CODE (op) == CONST_INT))
    {
      HOST_WIDE_INT hv, lv;
      REAL_VALUE_TYPE d;

      if (GET_CODE (op) == CONST_INT)
	lv = INTVAL (op), hv = INTVAL (op) < 0 ? -1 : 0;
      else
	lv = CONST_DOUBLE_LOW (op),  hv = CONST_DOUBLE_HIGH (op);

      if (op_mode == VOIDmode)
	{
	  /* We don't know how to interpret negative-looking numbers in
	     this case, so don't try to fold those.  */
	  if (hv < 0)
	    return 0;
	}
      else if (GET_MODE_BITSIZE (op_mode) >= HOST_BITS_PER_WIDE_INT * 2)
	;
      else
	hv = 0, lv &= GET_MODE_MASK (op_mode);

#ifdef REAL_ARITHMETIC
      REAL_VALUE_FROM_UNSIGNED_INT (d, lv, hv, mode);
#else

      d = (double) (unsigned HOST_WIDE_INT) hv;
      d *= ((double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2))
	    * (double) ((HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2)));
      d += (double) (unsigned HOST_WIDE_INT) lv;
#endif  /* REAL_ARITHMETIC */
      d = real_value_truncate (mode, d);
      return CONST_DOUBLE_FROM_REAL_VALUE (d, mode);
    }
#endif

  if (GET_CODE (op) == CONST_INT
      && width <= HOST_BITS_PER_WIDE_INT && width > 0)
    {
      register HOST_WIDE_INT arg0 = INTVAL (op);
      register HOST_WIDE_INT val;

      switch (code)
	{
	case NOT:
	  val = ~ arg0;
	  break;

	case NEG:
	  val = - arg0;
	  break;

	case ABS:
	  val = (arg0 >= 0 ? arg0 : - arg0);
	  break;

	case FFS:
	  /* Don't use ffs here.  Instead, get low order bit and then its
	     number.  If arg0 is zero, this will return 0, as desired.  */
	  arg0 &= GET_MODE_MASK (mode);
	  val = exact_log2 (arg0 & (- arg0)) + 1;
	  break;

	case TRUNCATE:
	  val = arg0;
	  break;

	case ZERO_EXTEND:
	  if (op_mode == VOIDmode)
	    op_mode = mode;
	  if (GET_MODE_BITSIZE (op_mode) == HOST_BITS_PER_WIDE_INT)
	    {
	      /* If we were really extending the mode,
		 we would have to distinguish between zero-extension
		 and sign-extension.  */
	      if (width != GET_MODE_BITSIZE (op_mode))
		abort ();
	      val = arg0;
	    }
	  else if (GET_MODE_BITSIZE (op_mode) < HOST_BITS_PER_WIDE_INT)
	    val = arg0 & ~((HOST_WIDE_INT) (-1) << GET_MODE_BITSIZE (op_mode));
	  else
	    return 0;
	  break;

	case SIGN_EXTEND:
	  if (op_mode == VOIDmode)
	    op_mode = mode;
	  if (GET_MODE_BITSIZE (op_mode) == HOST_BITS_PER_WIDE_INT)
	    {
	      /* If we were really extending the mode,
		 we would have to distinguish between zero-extension
		 and sign-extension.  */
	      if (width != GET_MODE_BITSIZE (op_mode))
		abort ();
	      val = arg0;
	    }
	  else if (GET_MODE_BITSIZE (op_mode) < HOST_BITS_PER_WIDE_INT)
	    {
	      val
		= arg0 & ~((HOST_WIDE_INT) (-1) << GET_MODE_BITSIZE (op_mode));
	      if (val
		  & ((HOST_WIDE_INT) 1 << (GET_MODE_BITSIZE (op_mode) - 1)))
		val -= (HOST_WIDE_INT) 1 << GET_MODE_BITSIZE (op_mode);
	    }
	  else
	    return 0;
	  break;

	case SQRT:
	  return 0;

	default:
	  abort ();
	}

      val = trunc_int_for_mode (val, mode);

      return GEN_INT (val);
    }

  /* We can do some operations on integer CONST_DOUBLEs.  Also allow
     for a DImode operation on a CONST_INT.  */
  else if (GET_MODE (op) == VOIDmode && width <= HOST_BITS_PER_INT * 2
	   && (GET_CODE (op) == CONST_DOUBLE || GET_CODE (op) == CONST_INT))
    {
      HOST_WIDE_INT l1, h1, lv, hv;

      if (GET_CODE (op) == CONST_DOUBLE)
	l1 = CONST_DOUBLE_LOW (op), h1 = CONST_DOUBLE_HIGH (op);
      else
	l1 = INTVAL (op), h1 = l1 < 0 ? -1 : 0;

      switch (code)
	{
	case NOT:
	  lv = ~ l1;
	  hv = ~ h1;
	  break;

	case NEG:
	  neg_double (l1, h1, &lv, &hv);
	  break;

	case ABS:
	  if (h1 < 0)
	    neg_double (l1, h1, &lv, &hv);
	  else
	    lv = l1, hv = h1;
	  break;

	case FFS:
	  hv = 0;
	  if (l1 == 0)
	    lv = HOST_BITS_PER_WIDE_INT + exact_log2 (h1 & (-h1)) + 1;
	  else
	    lv = exact_log2 (l1 & (-l1)) + 1;
	  break;

	case TRUNCATE:
	  /* This is just a change-of-mode, so do nothing.  */
	  lv = l1, hv = h1;
	  break;

	case ZERO_EXTEND:
	  if (op_mode == VOIDmode
	      || GET_MODE_BITSIZE (op_mode) > HOST_BITS_PER_WIDE_INT)
	    return 0;

	  hv = 0;
	  lv = l1 & GET_MODE_MASK (op_mode);
	  break;

	case SIGN_EXTEND:
	  if (op_mode == VOIDmode
	      || GET_MODE_BITSIZE (op_mode) > HOST_BITS_PER_WIDE_INT)
	    return 0;
	  else
	    {
	      lv = l1 & GET_MODE_MASK (op_mode);
	      if (GET_MODE_BITSIZE (op_mode) < HOST_BITS_PER_WIDE_INT
		  && (lv & ((HOST_WIDE_INT) 1
			    << (GET_MODE_BITSIZE (op_mode) - 1))) != 0)
		lv -= (HOST_WIDE_INT) 1 << GET_MODE_BITSIZE (op_mode);

	      hv = (lv < 0) ? ~ (HOST_WIDE_INT) 0 : 0;
	    }
	  break;

	case SQRT:
	  return 0;

	default:
	  return 0;
	}

      return immed_double_const (lv, hv, mode);
    }

#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
  else if (GET_CODE (op) == CONST_DOUBLE
	   && GET_MODE_CLASS (mode) == MODE_FLOAT)
    {
      REAL_VALUE_TYPE d;
      jmp_buf handler;
      rtx x;

      if (setjmp (handler))
	/* There used to be a warning here, but that is inadvisable.
	   People may want to cause traps, and the natural way
	   to do it should not get a warning.  */
	return 0;

      set_float_handler (handler);

      REAL_VALUE_FROM_CONST_DOUBLE (d, op);

      switch (code)
	{
	case NEG:
	  d = REAL_VALUE_NEGATE (d);
	  break;

	case ABS:
	  if (REAL_VALUE_NEGATIVE (d))
	    d = REAL_VALUE_NEGATE (d);
	  break;

	case FLOAT_TRUNCATE:
	  d = real_value_truncate (mode, d);
	  break;

	case FLOAT_EXTEND:
	  /* All this does is change the mode.  */
	  break;

	case FIX:
	  d = REAL_VALUE_RNDZINT (d);
	  break;

	case UNSIGNED_FIX:
	  d = REAL_VALUE_UNSIGNED_RNDZINT (d);
	  break;

	case SQRT:
	  return 0;

	default:
	  abort ();
	}

      x = CONST_DOUBLE_FROM_REAL_VALUE (d, mode);
      set_float_handler (NULL_PTR);
      return x;
    }

  else if (GET_CODE (op) == CONST_DOUBLE
	   && GET_MODE_CLASS (GET_MODE (op)) == MODE_FLOAT
	   && GET_MODE_CLASS (mode) == MODE_INT
	   && width <= HOST_BITS_PER_WIDE_INT && width > 0)
    {
      REAL_VALUE_TYPE d;
      jmp_buf handler;
      HOST_WIDE_INT val;

      if (setjmp (handler))
	return 0;

      set_float_handler (handler);

      REAL_VALUE_FROM_CONST_DOUBLE (d, op);

      switch (code)
	{
	case FIX:
	  val = REAL_VALUE_FIX (d);
	  break;

	case UNSIGNED_FIX:
	  val = REAL_VALUE_UNSIGNED_FIX (d);
	  break;

	default:
	  abort ();
	}

      set_float_handler (NULL_PTR);

      val = trunc_int_for_mode (val, mode);

      return GEN_INT (val);
    }
#endif
  /* This was formerly used only for non-IEEE float.
     eggert@twinsun.com says it is safe for IEEE also.  */
  else
    {
      /* There are some simplifications we can do even if the operands
	 aren't constant.  */
      switch (code)
	{
	case NEG:
	case NOT:
	  /* (not (not X)) == X, similarly for NEG.  */
	  if (GET_CODE (op) == code)
	    return XEXP (op, 0);
	  break;

	case SIGN_EXTEND:
	  /* (sign_extend (truncate (minus (label_ref L1) (label_ref L2))))
	     becomes just the MINUS if its mode is MODE.  This allows
	     folding switch statements on machines using casesi (such as
	     the Vax).  */
	  if (GET_CODE (op) == TRUNCATE
	      && GET_MODE (XEXP (op, 0)) == mode
	      && GET_CODE (XEXP (op, 0)) == MINUS
	      && GET_CODE (XEXP (XEXP (op, 0), 0)) == LABEL_REF
	      && GET_CODE (XEXP (XEXP (op, 0), 1)) == LABEL_REF)
	    return XEXP (op, 0);

#ifdef POINTERS_EXTEND_UNSIGNED
	  if (! POINTERS_EXTEND_UNSIGNED
	      && mode == Pmode && GET_MODE (op) == ptr_mode
	      && CONSTANT_P (op))
	    return convert_memory_address (Pmode, op);
#endif
	  break;

#ifdef POINTERS_EXTEND_UNSIGNED
	case ZERO_EXTEND:
	  if (POINTERS_EXTEND_UNSIGNED
	      && mode == Pmode && GET_MODE (op) == ptr_mode
	      && CONSTANT_P (op))
	    return convert_memory_address (Pmode, op);
	  break;
#endif
	  
	default:
	  break;
	}

      return 0;
    }
}

/* Simplify a binary operation CODE with result mode MODE, operating on OP0
   and OP1.  Return 0 if no simplification is possible.

   Don't use this for relational operations such as EQ or LT.
   Use simplify_relational_operation instead.  */

rtx
simplify_binary_operation (code, mode, op0, op1)
     enum rtx_code code;
     enum machine_mode mode;
     rtx op0, op1;
{
  register HOST_WIDE_INT arg0, arg1, arg0s, arg1s;
  HOST_WIDE_INT val;
  int width = GET_MODE_BITSIZE (mode);
  rtx tem;

  /* Relational operations don't work here.  We must know the mode
     of the operands in order to do the comparison correctly.
     Assuming a full word can give incorrect results.
     Consider comparing 128 with -128 in QImode.  */

  if (GET_RTX_CLASS (code) == '<')
    abort ();

#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_CODE (op0) == CONST_DOUBLE && GET_CODE (op1) == CONST_DOUBLE
      && mode == GET_MODE (op0) && mode == GET_MODE (op1))
    {
      REAL_VALUE_TYPE f0, f1, value;
      jmp_buf handler;

      if (setjmp (handler))
	return 0;

      set_float_handler (handler);

      REAL_VALUE_FROM_CONST_DOUBLE (f0, op0);
      REAL_VALUE_FROM_CONST_DOUBLE (f1, op1);
      f0 = real_value_truncate (mode, f0);
      f1 = real_value_truncate (mode, f1);

#ifdef REAL_ARITHMETIC
#ifndef REAL_INFINITY
      if (code == DIV && REAL_VALUES_EQUAL (f1, dconst0))
	return 0;
#endif
      REAL_ARITHMETIC (value, rtx_to_tree_code (code), f0, f1);
#else
      switch (code)
	{
	case PLUS:
	  value = f0 + f1;
	  break;
	case MINUS:
	  value = f0 - f1;
	  break;
	case MULT:
	  value = f0 * f1;
	  break;
	case DIV:
#ifndef REAL_INFINITY
	  if (f1 == 0)
	    return 0;
#endif
	  value = f0 / f1;
	  break;
	case SMIN:
	  value = MIN (f0, f1);
	  break;
	case SMAX:
	  value = MAX (f0, f1);
	  break;
	default:
	  abort ();
	}
#endif

      value = real_value_truncate (mode, value);
      set_float_handler (NULL_PTR);
      return CONST_DOUBLE_FROM_REAL_VALUE (value, mode);
    }
#endif  /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */

  /* We can fold some multi-word operations.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && width == HOST_BITS_PER_WIDE_INT * 2
      && (GET_CODE (op0) == CONST_DOUBLE || GET_CODE (op0) == CONST_INT)
      && (GET_CODE (op1) == CONST_DOUBLE || GET_CODE (op1) == CONST_INT))
    {
      HOST_WIDE_INT l1, l2, h1, h2, lv, hv;

      if (GET_CODE (op0) == CONST_DOUBLE)
	l1 = CONST_DOUBLE_LOW (op0), h1 = CONST_DOUBLE_HIGH (op0);
      else
	l1 = INTVAL (op0), h1 = l1 < 0 ? -1 : 0;

      if (GET_CODE (op1) == CONST_DOUBLE)
	l2 = CONST_DOUBLE_LOW (op1), h2 = CONST_DOUBLE_HIGH (op1);
      else
	l2 = INTVAL (op1), h2 = l2 < 0 ? -1 : 0;

      switch (code)
	{
	case MINUS:
	  /* A - B == A + (-B).  */
	  neg_double (l2, h2, &lv, &hv);
	  l2 = lv, h2 = hv;

	  /* .. fall through ...  */

	case PLUS:
	  add_double (l1, h1, l2, h2, &lv, &hv);
	  break;

	case MULT:
	  mul_double (l1, h1, l2, h2, &lv, &hv);
	  break;

	case DIV:  case MOD:   case UDIV:  case UMOD:
	  /* We'd need to include tree.h to do this and it doesn't seem worth
	     it.  */
	  return 0;

	case AND:
	  lv = l1 & l2, hv = h1 & h2;
	  break;

	case IOR:
	  lv = l1 | l2, hv = h1 | h2;
	  break;

	case XOR:
	  lv = l1 ^ l2, hv = h1 ^ h2;
	  break;

	case SMIN:
	  if (h1 < h2
	      || (h1 == h2
		  && ((unsigned HOST_WIDE_INT) l1
		      < (unsigned HOST_WIDE_INT) l2)))
	    lv = l1, hv = h1;
	  else
	    lv = l2, hv = h2;
	  break;

	case SMAX:
	  if (h1 > h2
	      || (h1 == h2
		  && ((unsigned HOST_WIDE_INT) l1
		      > (unsigned HOST_WIDE_INT) l2)))
	    lv = l1, hv = h1;
	  else
	    lv = l2, hv = h2;
	  break;

	case UMIN:
	  if ((unsigned HOST_WIDE_INT) h1 < (unsigned HOST_WIDE_INT) h2
	      || (h1 == h2
		  && ((unsigned HOST_WIDE_INT) l1
		      < (unsigned HOST_WIDE_INT) l2)))
	    lv = l1, hv = h1;
	  else
	    lv = l2, hv = h2;
	  break;

	case UMAX:
	  if ((unsigned HOST_WIDE_INT) h1 > (unsigned HOST_WIDE_INT) h2
	      || (h1 == h2
		  && ((unsigned HOST_WIDE_INT) l1
		      > (unsigned HOST_WIDE_INT) l2)))
	    lv = l1, hv = h1;
	  else
	    lv = l2, hv = h2;
	  break;

	case LSHIFTRT:   case ASHIFTRT:
	case ASHIFT:
	case ROTATE:     case ROTATERT:
#ifdef SHIFT_COUNT_TRUNCATED
	  if (SHIFT_COUNT_TRUNCATED)
	    l2 &= (GET_MODE_BITSIZE (mode) - 1), h2 = 0;
#endif

	  if (h2 != 0 || l2 < 0 || l2 >= GET_MODE_BITSIZE (mode))
	    return 0;

	  if (code == LSHIFTRT || code == ASHIFTRT)
	    rshift_double (l1, h1, l2, GET_MODE_BITSIZE (mode), &lv, &hv,
			   code == ASHIFTRT);
	  else if (code == ASHIFT)
	    lshift_double (l1, h1, l2, GET_MODE_BITSIZE (mode), &lv, &hv, 1);
	  else if (code == ROTATE)
	    lrotate_double (l1, h1, l2, GET_MODE_BITSIZE (mode), &lv, &hv);
	  else /* code == ROTATERT */
	    rrotate_double (l1, h1, l2, GET_MODE_BITSIZE (mode), &lv, &hv);
	  break;

	default:
	  return 0;
	}

      return immed_double_const (lv, hv, mode);
    }

  if (GET_CODE (op0) != CONST_INT || GET_CODE (op1) != CONST_INT
      || width > HOST_BITS_PER_WIDE_INT || width == 0)
    {
      /* Even if we can't compute a constant result,
	 there are some cases worth simplifying.  */

      switch (code)
	{
	case PLUS:
	  /* In IEEE floating point, x+0 is not the same as x.  Similarly
	     for the other optimizations below.  */
	  if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
	      && FLOAT_MODE_P (mode) && ! flag_fast_math)
	    break;

	  if (op1 == CONST0_RTX (mode))
	    return op0;

	  /* ((-a) + b) -> (b - a) and similarly for (a + (-b)) */
	  if (GET_CODE (op0) == NEG)
	    return simplify_gen_binary (MINUS, mode, op1, XEXP (op0, 0));
	  else if (GET_CODE (op1) == NEG)
	    return simplify_gen_binary (MINUS, mode, op0, XEXP (op1, 0));

	  /* Handle both-operands-constant cases.  We can only add
	     CONST_INTs to constants since the sum of relocatable symbols
	     can't be handled by most assemblers.  Don't add CONST_INT
	     to CONST_INT since overflow won't be computed properly if wider
	     than HOST_BITS_PER_WIDE_INT.  */

	  if (CONSTANT_P (op0) && GET_MODE (op0) != VOIDmode
	      && GET_CODE (op1) == CONST_INT)
	    return plus_constant (op0, INTVAL (op1));
	  else if (CONSTANT_P (op1) && GET_MODE (op1) != VOIDmode
		   && GET_CODE (op0) == CONST_INT)
	    return plus_constant (op1, INTVAL (op0));

	  /* See if this is something like X * C - X or vice versa or
	     if the multiplication is written as a shift.  If so, we can
	     distribute and make a new multiply, shift, or maybe just
	     have X (if C is 2 in the example above).  But don't make
	     real multiply if we didn't have one before.  */

	  if (! FLOAT_MODE_P (mode))
	    {
	      HOST_WIDE_INT coeff0 = 1, coeff1 = 1;
	      rtx lhs = op0, rhs = op1;
	      int had_mult = 0;

	      if (GET_CODE (lhs) == NEG)
		coeff0 = -1, lhs = XEXP (lhs, 0);
	      else if (GET_CODE (lhs) == MULT
		       && GET_CODE (XEXP (lhs, 1)) == CONST_INT)
		{
		  coeff0 = INTVAL (XEXP (lhs, 1)), lhs = XEXP (lhs, 0);
		  had_mult = 1;
		}
	      else if (GET_CODE (lhs) == ASHIFT
		       && GET_CODE (XEXP (lhs, 1)) == CONST_INT
		       && INTVAL (XEXP (lhs, 1)) >= 0
		       && INTVAL (XEXP (lhs, 1)) < HOST_BITS_PER_WIDE_INT)
		{
		  coeff0 = ((HOST_WIDE_INT) 1) << INTVAL (XEXP (lhs, 1));
		  lhs = XEXP (lhs, 0);
		}

	      if (GET_CODE (rhs) == NEG)
		coeff1 = -1, rhs = XEXP (rhs, 0);
	      else if (GET_CODE (rhs) == MULT
		       && GET_CODE (XEXP (rhs, 1)) == CONST_INT)
		{
		  coeff1 = INTVAL (XEXP (rhs, 1)), rhs = XEXP (rhs, 0);
		  had_mult = 1;
		}
	      else if (GET_CODE (rhs) == ASHIFT
		       && GET_CODE (XEXP (rhs, 1)) == CONST_INT
		       && INTVAL (XEXP (rhs, 1)) >= 0
		       && INTVAL (XEXP (rhs, 1)) < HOST_BITS_PER_WIDE_INT)
		{
		  coeff1 = ((HOST_WIDE_INT) 1) << INTVAL (XEXP (rhs, 1));
		  rhs = XEXP (rhs, 0);
		}

	      if (rtx_equal_p (lhs, rhs))
		{
		  tem = simplify_gen_binary (MULT, mode, lhs,
					GEN_INT (coeff0 + coeff1));
		  return (GET_CODE (tem) == MULT && ! had_mult) ? 0 : tem;
		}
	    }

	  /* If one of the operands is a PLUS or a MINUS, see if we can
	     simplify this by the associative law. 
	     Don't use the associative law for floating point.
	     The inaccuracy makes it nonassociative,
	     and subtle programs can break if operations are associated.  */

	  if (INTEGRAL_MODE_P (mode)
	      && (GET_CODE (op0) == PLUS || GET_CODE (op0) == MINUS
		  || GET_CODE (op1) == PLUS || GET_CODE (op1) == MINUS)
	      && (tem = simplify_plus_minus (code, mode, op0, op1)) != 0)
	    return tem;
	  break;

	case COMPARE:
#ifdef HAVE_cc0
	  /* Convert (compare FOO (const_int 0)) to FOO unless we aren't
	     using cc0, in which case we want to leave it as a COMPARE
	     so we can distinguish it from a register-register-copy.

	     In IEEE floating point, x-0 is not the same as x.  */

	  if ((TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	       || ! FLOAT_MODE_P (mode) || flag_fast_math)
	      && op1 == CONST0_RTX (mode))
	    return op0;
#else
	  /* Do nothing here.  */
#endif
	  break;
	      
	case MINUS:
	  /* None of these optimizations can be done for IEEE
	     floating point.  */
	  if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
	      && FLOAT_MODE_P (mode) && ! flag_fast_math)
	    break;

	  /* We can't assume x-x is 0 even with non-IEEE floating point,
	     but since it is zero except in very strange circumstances, we
	     will treat it as zero with -ffast-math.  */
	  if (rtx_equal_p (op0, op1)
	      && ! side_effects_p (op0)
	      && (! FLOAT_MODE_P (mode) || flag_fast_math))
	    return CONST0_RTX (mode);

	  /* Change subtraction from zero into negation.  */
	  if (op0 == CONST0_RTX (mode))
	    return gen_rtx_NEG (mode, op1);

	  /* (-1 - a) is ~a.  */
	  if (op0 == constm1_rtx)
	    return gen_rtx_NOT (mode, op1);

	  /* Subtracting 0 has no effect.  */
	  if (op1 == CONST0_RTX (mode))
	    return op0;

	  /* See if this is something like X * C - X or vice versa or
	     if the multiplication is written as a shift.  If so, we can
	     distribute and make a new multiply, shift, or maybe just
	     have X (if C is 2 in the example above).  But don't make
	     real multiply if we didn't have one before.  */

	  if (! FLOAT_MODE_P (mode))
	    {
	      HOST_WIDE_INT coeff0 = 1, coeff1 = 1;
	      rtx lhs = op0, rhs = op1;
	      int had_mult = 0;

	      if (GET_CODE (lhs) == NEG)
		coeff0 = -1, lhs = XEXP (lhs, 0);
	      else if (GET_CODE (lhs) == MULT
		       && GET_CODE (XEXP (lhs, 1)) == CONST_INT)
		{
		  coeff0 = INTVAL (XEXP (lhs, 1)), lhs = XEXP (lhs, 0);
		  had_mult = 1;
		}
	      else if (GET_CODE (lhs) == ASHIFT
		       && GET_CODE (XEXP (lhs, 1)) == CONST_INT
		       && INTVAL (XEXP (lhs, 1)) >= 0
		       && INTVAL (XEXP (lhs, 1)) < HOST_BITS_PER_WIDE_INT)
		{
		  coeff0 = ((HOST_WIDE_INT) 1) << INTVAL (XEXP (lhs, 1));
		  lhs = XEXP (lhs, 0);
		}

	      if (GET_CODE (rhs) == NEG)
		coeff1 = - 1, rhs = XEXP (rhs, 0);
	      else if (GET_CODE (rhs) == MULT
		       && GET_CODE (XEXP (rhs, 1)) == CONST_INT)
		{
		  coeff1 = INTVAL (XEXP (rhs, 1)), rhs = XEXP (rhs, 0);
		  had_mult = 1;
		}
	      else if (GET_CODE (rhs) == ASHIFT
		       && GET_CODE (XEXP (rhs, 1)) == CONST_INT
		       && INTVAL (XEXP (rhs, 1)) >= 0
		       && INTVAL (XEXP (rhs, 1)) < HOST_BITS_PER_WIDE_INT)
		{
		  coeff1 = ((HOST_WIDE_INT) 1) << INTVAL (XEXP (rhs, 1));
		  rhs = XEXP (rhs, 0);
		}

	      if (rtx_equal_p (lhs, rhs))
		{
		  tem = simplify_gen_binary (MULT, mode, lhs,
					     GEN_INT (coeff0 - coeff1));
		  return (GET_CODE (tem) == MULT && ! had_mult) ? 0 : tem;
		}
	    }

	  /* (a - (-b)) -> (a + b).  */
	  if (GET_CODE (op1) == NEG)
	    return simplify_gen_binary (PLUS, mode, op0, XEXP (op1, 0));

	  /* If one of the operands is a PLUS or a MINUS, see if we can
	     simplify this by the associative law. 
	     Don't use the associative law for floating point.
	     The inaccuracy makes it nonassociative,
	     and subtle programs can break if operations are associated.  */

	  if (INTEGRAL_MODE_P (mode)
	      && (GET_CODE (op0) == PLUS || GET_CODE (op0) == MINUS
		  || GET_CODE (op1) == PLUS || GET_CODE (op1) == MINUS)
	      && (tem = simplify_plus_minus (code, mode, op0, op1)) != 0)
	    return tem;

	  /* Don't let a relocatable value get a negative coeff.  */
	  if (GET_CODE (op1) == CONST_INT && GET_MODE (op0) != VOIDmode)
	    return plus_constant (op0, - INTVAL (op1));

	  /* (x - (x & y)) -> (x & ~y) */
	  if (GET_CODE (op1) == AND)
	    {
	     if (rtx_equal_p (op0, XEXP (op1, 0)))
	       return simplify_gen_binary (AND, mode, op0,
					   gen_rtx_NOT (mode, XEXP (op1, 1)));
	     if (rtx_equal_p (op0, XEXP (op1, 1)))
	       return simplify_gen_binary (AND, mode, op0,
					   gen_rtx_NOT (mode, XEXP (op1, 0)));
	   }
	  break;

	case MULT:
	  if (op1 == constm1_rtx)
	    {
	      tem = simplify_unary_operation (NEG, mode, op0, mode);

	      return tem ? tem : gen_rtx_NEG (mode, op0);
	    }

	  /* In IEEE floating point, x*0 is not always 0.  */
	  if ((TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	       || ! FLOAT_MODE_P (mode) || flag_fast_math)
	      && op1 == CONST0_RTX (mode)
	      && ! side_effects_p (op0))
	    return op1;

	  /* In IEEE floating point, x*1 is not equivalent to x for nans.
	     However, ANSI says we can drop signals,
	     so we can do this anyway.  */
	  if (op1 == CONST1_RTX (mode))
	    return op0;

	  /* Convert multiply by constant power of two into shift unless
	     we are still generating RTL.  This test is a kludge.  */
	  if (GET_CODE (op1) == CONST_INT
	      && (val = exact_log2 (INTVAL (op1))) >= 0
	      /* If the mode is larger than the host word size, and the
		 uppermost bit is set, then this isn't a power of two due
		 to implicit sign extension.  */
	      && (width <= HOST_BITS_PER_WIDE_INT
		  || val != HOST_BITS_PER_WIDE_INT - 1)
	      && ! rtx_equal_function_value_matters)
	    return gen_rtx_ASHIFT (mode, op0, GEN_INT (val));

	  if (GET_CODE (op1) == CONST_DOUBLE
	      && GET_MODE_CLASS (GET_MODE (op1)) == MODE_FLOAT)
	    {
	      REAL_VALUE_TYPE d;
	      jmp_buf handler;
	      int op1is2, op1ism1;

	      if (setjmp (handler))
		return 0;

	      set_float_handler (handler);
	      REAL_VALUE_FROM_CONST_DOUBLE (d, op1);
	      op1is2 = REAL_VALUES_EQUAL (d, dconst2);
	      op1ism1 = REAL_VALUES_EQUAL (d, dconstm1);
	      set_float_handler (NULL_PTR);

	      /* x*2 is x+x and x*(-1) is -x */
	      if (op1is2 && GET_MODE (op0) == mode)
		return gen_rtx_PLUS (mode, op0, copy_rtx (op0));

	      else if (op1ism1 && GET_MODE (op0) == mode)
		return gen_rtx_NEG (mode, op0);
	    }
	  break;

	case IOR:
	  if (op1 == const0_rtx)
	    return op0;
	  if (GET_CODE (op1) == CONST_INT
	      && (INTVAL (op1) & GET_MODE_MASK (mode)) == GET_MODE_MASK (mode))
	    return op1;
	  if (rtx_equal_p (op0, op1) && ! side_effects_p (op0))
	    return op0;
	  /* A | (~A) -> -1 */
	  if (((GET_CODE (op0) == NOT && rtx_equal_p (XEXP (op0, 0), op1))
	       || (GET_CODE (op1) == NOT && rtx_equal_p (XEXP (op1, 0), op0)))
	      && ! side_effects_p (op0)
	      && GET_MODE_CLASS (mode) != MODE_CC)
	    return constm1_rtx;
	  break;

	case XOR:
	  if (op1 == const0_rtx)
	    return op0;
	  if (GET_CODE (op1) == CONST_INT
	      && (INTVAL (op1) & GET_MODE_MASK (mode)) == GET_MODE_MASK (mode))
	    return gen_rtx_NOT (mode, op0);
	  if (op0 == op1 && ! side_effects_p (op0)
	      && GET_MODE_CLASS (mode) != MODE_CC)
	    return const0_rtx;
	  break;

	case AND:
	  if (op1 == const0_rtx && ! side_effects_p (op0))
	    return const0_rtx;
	  if (GET_CODE (op1) == CONST_INT
	      && (INTVAL (op1) & GET_MODE_MASK (mode)) == GET_MODE_MASK (mode))
	    return op0;
	  if (op0 == op1 && ! side_effects_p (op0)
	      && GET_MODE_CLASS (mode) != MODE_CC)
	    return op0;
	  /* A & (~A) -> 0 */
	  if (((GET_CODE (op0) == NOT && rtx_equal_p (XEXP (op0, 0), op1))
	       || (GET_CODE (op1) == NOT && rtx_equal_p (XEXP (op1, 0), op0)))
	      && ! side_effects_p (op0)
	      && GET_MODE_CLASS (mode) != MODE_CC)
	    return const0_rtx;
	  break;

	case UDIV:
	  /* Convert divide by power of two into shift (divide by 1 handled
	     below).  */
	  if (GET_CODE (op1) == CONST_INT
	      && (arg1 = exact_log2 (INTVAL (op1))) > 0)
	    return gen_rtx_LSHIFTRT (mode, op0, GEN_INT (arg1));

	  /* ... fall through ...  */

	case DIV:
	  if (op1 == CONST1_RTX (mode))
	    return op0;

	  /* In IEEE floating point, 0/x is not always 0.  */
	  if ((TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	       || ! FLOAT_MODE_P (mode) || flag_fast_math)
	      && op0 == CONST0_RTX (mode)
	      && ! side_effects_p (op1))
	    return op0;

#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
	  /* Change division by a constant into multiplication.  Only do
	     this with -ffast-math until an expert says it is safe in
	     general.  */
	  else if (GET_CODE (op1) == CONST_DOUBLE
		   && GET_MODE_CLASS (GET_MODE (op1)) == MODE_FLOAT
		   && op1 != CONST0_RTX (mode)
		   && flag_fast_math)
	    {
	      REAL_VALUE_TYPE d;
	      REAL_VALUE_FROM_CONST_DOUBLE (d, op1);

	      if (! REAL_VALUES_EQUAL (d, dconst0))
		{
#if defined (REAL_ARITHMETIC)
		  REAL_ARITHMETIC (d, rtx_to_tree_code (DIV), dconst1, d);
		  return gen_rtx_MULT (mode, op0, 
				       CONST_DOUBLE_FROM_REAL_VALUE (d, mode));
#else
		  return
		    gen_rtx_MULT (mode, op0, 
				  CONST_DOUBLE_FROM_REAL_VALUE (1./d, mode));
#endif
		}
	    }
#endif
	  break;

	case UMOD:
	  /* Handle modulus by power of two (mod with 1 handled below).  */
	  if (GET_CODE (op1) == CONST_INT
	      && exact_log2 (INTVAL (op1)) > 0)
	    return gen_rtx_AND (mode, op0, GEN_INT (INTVAL (op1) - 1));

	  /* ... fall through ...  */

	case MOD:
	  if ((op0 == const0_rtx || op1 == const1_rtx)
	      && ! side_effects_p (op0) && ! side_effects_p (op1))
	    return const0_rtx;
	  break;

	case ROTATERT:
	case ROTATE:
	  /* Rotating ~0 always results in ~0.  */
	  if (GET_CODE (op0) == CONST_INT && width <= HOST_BITS_PER_WIDE_INT
	      && (unsigned HOST_WIDE_INT) INTVAL (op0) == GET_MODE_MASK (mode)
	      && ! side_effects_p (op1))
	    return op0;

	  /* ... fall through ...  */

	case ASHIFT:
	case ASHIFTRT:
	case LSHIFTRT:
	  if (op1 == const0_rtx)
	    return op0;
	  if (op0 == const0_rtx && ! side_effects_p (op1))
	    return op0;
	  break;

	case SMIN:
	  if (width <= HOST_BITS_PER_WIDE_INT && GET_CODE (op1) == CONST_INT 
	      && INTVAL (op1) == (HOST_WIDE_INT) 1 << (width -1)
	      && ! side_effects_p (op0))
	    return op1;
	  else if (rtx_equal_p (op0, op1) && ! side_effects_p (op0))
	    return op0;
	  break;
	   
	case SMAX:
	  if (width <= HOST_BITS_PER_WIDE_INT && GET_CODE (op1) == CONST_INT
	      && ((unsigned HOST_WIDE_INT) INTVAL (op1)
		  == (unsigned HOST_WIDE_INT) GET_MODE_MASK (mode) >> 1)
	      && ! side_effects_p (op0))
	    return op1;
	  else if (rtx_equal_p (op0, op1) && ! side_effects_p (op0))
	    return op0;
	  break;

	case UMIN:
	  if (op1 == const0_rtx && ! side_effects_p (op0))
	    return op1;
	  else if (rtx_equal_p (op0, op1) && ! side_effects_p (op0))
	    return op0;
	  break;
	    
	case UMAX:
	  if (op1 == constm1_rtx && ! side_effects_p (op0))
	    return op1;
	  else if (rtx_equal_p (op0, op1) && ! side_effects_p (op0))
	    return op0;
	  break;

	default:
	  abort ();
	}
      
      return 0;
    }

  /* Get the integer argument values in two forms:
     zero-extended in ARG0, ARG1 and sign-extended in ARG0S, ARG1S.  */

  arg0 = INTVAL (op0);
  arg1 = INTVAL (op1);

  if (width < HOST_BITS_PER_WIDE_INT)
    {
      arg0 &= ((HOST_WIDE_INT) 1 << width) - 1;
      arg1 &= ((HOST_WIDE_INT) 1 << width) - 1;

      arg0s = arg0;
      if (arg0s & ((HOST_WIDE_INT) 1 << (width - 1)))
	arg0s |= ((HOST_WIDE_INT) (-1) << width);

      arg1s = arg1;
      if (arg1s & ((HOST_WIDE_INT) 1 << (width - 1)))
	arg1s |= ((HOST_WIDE_INT) (-1) << width);
    }
  else
    {
      arg0s = arg0;
      arg1s = arg1;
    }

  /* Compute the value of the arithmetic.  */

  switch (code)
    {
    case PLUS:
      val = arg0s + arg1s;
      break;

    case MINUS:
      val = arg0s - arg1s;
      break;

    case MULT:
      val = arg0s * arg1s;
      break;

    case DIV:
      if (arg1s == 0)
	return 0;
      val = arg0s / arg1s;
      break;

    case MOD:
      if (arg1s == 0)
	return 0;
      val = arg0s % arg1s;
      break;

    case UDIV:
      if (arg1 == 0)
	return 0;
      val = (unsigned HOST_WIDE_INT) arg0 / arg1;
      break;

    case UMOD:
      if (arg1 == 0)
	return 0;
      val = (unsigned HOST_WIDE_INT) arg0 % arg1;
      break;

    case AND:
      val = arg0 & arg1;
      break;

    case IOR:
      val = arg0 | arg1;
      break;

    case XOR:
      val = arg0 ^ arg1;
      break;

    case LSHIFTRT:
      /* If shift count is undefined, don't fold it; let the machine do
	 what it wants.  But truncate it if the machine will do that.  */
      if (arg1 < 0)
	return 0;

#ifdef SHIFT_COUNT_TRUNCATED
      if (SHIFT_COUNT_TRUNCATED)
	arg1 %= width;
#endif

      val = ((unsigned HOST_WIDE_INT) arg0) >> arg1;
      break;

    case ASHIFT:
      if (arg1 < 0)
	return 0;

#ifdef SHIFT_COUNT_TRUNCATED
      if (SHIFT_COUNT_TRUNCATED)
	arg1 %= width;
#endif

      val = ((unsigned HOST_WIDE_INT) arg0) << arg1;
      break;

    case ASHIFTRT:
      if (arg1 < 0)
	return 0;

#ifdef SHIFT_COUNT_TRUNCATED
      if (SHIFT_COUNT_TRUNCATED)
	arg1 %= width;
#endif

      val = arg0s >> arg1;

      /* Bootstrap compiler may not have sign extended the right shift.
	 Manually extend the sign to insure bootstrap cc matches gcc.  */
      if (arg0s < 0 && arg1 > 0)
	val |= ((HOST_WIDE_INT) -1) << (HOST_BITS_PER_WIDE_INT - arg1);

      break;

    case ROTATERT:
      if (arg1 < 0)
	return 0;

      arg1 %= width;
      val = ((((unsigned HOST_WIDE_INT) arg0) << (width - arg1))
	     | (((unsigned HOST_WIDE_INT) arg0) >> arg1));
      break;

    case ROTATE:
      if (arg1 < 0)
	return 0;

      arg1 %= width;
      val = ((((unsigned HOST_WIDE_INT) arg0) << arg1)
	     | (((unsigned HOST_WIDE_INT) arg0) >> (width - arg1)));
      break;

    case COMPARE:
      /* Do nothing here.  */
      return 0;

    case SMIN:
      val = arg0s <= arg1s ? arg0s : arg1s;
      break;

    case UMIN:
      val = ((unsigned HOST_WIDE_INT) arg0
	     <= (unsigned HOST_WIDE_INT) arg1 ? arg0 : arg1);
      break;

    case SMAX:
      val = arg0s > arg1s ? arg0s : arg1s;
      break;

    case UMAX:
      val = ((unsigned HOST_WIDE_INT) arg0
	     > (unsigned HOST_WIDE_INT) arg1 ? arg0 : arg1);
      break;

    default:
      abort ();
    }

  val = trunc_int_for_mode (val, mode);

  return GEN_INT (val);
}

/* Simplify a PLUS or MINUS, at least one of whose operands may be another
   PLUS or MINUS.

   Rather than test for specific case, we do this by a brute-force method
   and do all possible simplifications until no more changes occur.  Then
   we rebuild the operation.  */

static rtx
simplify_plus_minus (code, mode, op0, op1)
     enum rtx_code code;
     enum machine_mode mode;
     rtx op0, op1;
{
  rtx ops[8];
  int negs[8];
  rtx result, tem;
  int n_ops = 2, input_ops = 2, input_consts = 0, n_consts = 0;
  int first = 1, negate = 0, changed;
  int i, j;

  bzero ((char *) ops, sizeof ops);
  
  /* Set up the two operands and then expand them until nothing has been
     changed.  If we run out of room in our array, give up; this should
     almost never happen.  */

  ops[0] = op0, ops[1] = op1, negs[0] = 0, negs[1] = (code == MINUS);

  changed = 1;
  while (changed)
    {
      changed = 0;

      for (i = 0; i < n_ops; i++)
	switch (GET_CODE (ops[i]))
	  {
	  case PLUS:
	  case MINUS:
	    if (n_ops == 7)
	      return 0;

	    ops[n_ops] = XEXP (ops[i], 1);
	    negs[n_ops++] = GET_CODE (ops[i]) == MINUS ? !negs[i] : negs[i];
	    ops[i] = XEXP (ops[i], 0);
	    input_ops++;
	    changed = 1;
	    break;

	  case NEG:
	    ops[i] = XEXP (ops[i], 0);
	    negs[i] = ! negs[i];
	    changed = 1;
	    break;

	  case CONST:
	    ops[i] = XEXP (ops[i], 0);
	    input_consts++;
	    changed = 1;
	    break;

	  case NOT:
	    /* ~a -> (-a - 1) */
	    if (n_ops != 7)
	      {
		ops[n_ops] = constm1_rtx;
		negs[n_ops++] = negs[i];
		ops[i] = XEXP (ops[i], 0);
		negs[i] = ! negs[i];
		changed = 1;
	      }
	    break;

	  case CONST_INT:
	    if (negs[i])
	      ops[i] = GEN_INT (- INTVAL (ops[i])), negs[i] = 0, changed = 1;
	    break;

	  default:
	    break;
	  }
    }

  /* If we only have two operands, we can't do anything.  */
  if (n_ops <= 2)
    return 0;

  /* Now simplify each pair of operands until nothing changes.  The first
     time through just simplify constants against each other.  */

  changed = 1;
  while (changed)
    {
      changed = first;

      for (i = 0; i < n_ops - 1; i++)
	for (j = i + 1; j < n_ops; j++)
	  if (ops[i] != 0 && ops[j] != 0
	      && (! first || (CONSTANT_P (ops[i]) && CONSTANT_P (ops[j]))))
	    {
	      rtx lhs = ops[i], rhs = ops[j];
	      enum rtx_code ncode = PLUS;

	      if (negs[i] && ! negs[j])
		lhs = ops[j], rhs = ops[i], ncode = MINUS;
	      else if (! negs[i] && negs[j])
		ncode = MINUS;

	      tem = simplify_binary_operation (ncode, mode, lhs, rhs);
	      if (tem)
		{
		  ops[i] = tem, ops[j] = 0;
		  negs[i] = negs[i] && negs[j];
		  if (GET_CODE (tem) == NEG)
		    ops[i] = XEXP (tem, 0), negs[i] = ! negs[i];

		  if (GET_CODE (ops[i]) == CONST_INT && negs[i])
		    ops[i] = GEN_INT (- INTVAL (ops[i])), negs[i] = 0;
		  changed = 1;
		}
	    }

      first = 0;
    }

  /* Pack all the operands to the lower-numbered entries and give up if
     we didn't reduce the number of operands we had.  Make sure we
     count a CONST as two operands.  If we have the same number of
     operands, but have made more CONSTs than we had, this is also
     an improvement, so accept it.  */

  for (i = 0, j = 0; j < n_ops; j++)
    if (ops[j] != 0)
      {
	ops[i] = ops[j], negs[i++] = negs[j];
	if (GET_CODE (ops[j]) == CONST)
	  n_consts++;
      }

  if (i + n_consts > input_ops
      || (i + n_consts == input_ops && n_consts <= input_consts))
    return 0;

  n_ops = i;

  /* If we have a CONST_INT, put it last.  */
  for (i = 0; i < n_ops - 1; i++)
    if (GET_CODE (ops[i]) == CONST_INT)
      {
	tem = ops[n_ops - 1], ops[n_ops - 1] = ops[i] , ops[i] = tem;
	j = negs[n_ops - 1], negs[n_ops - 1] = negs[i], negs[i] = j;
      }

  /* Put a non-negated operand first.  If there aren't any, make all
     operands positive and negate the whole thing later.  */
  for (i = 0; i < n_ops && negs[i]; i++)
    ;

  if (i == n_ops)
    {
      for (i = 0; i < n_ops; i++)
	negs[i] = 0;
      negate = 1;
    }
  else if (i != 0)
    {
      tem = ops[0], ops[0] = ops[i], ops[i] = tem;
      j = negs[0], negs[0] = negs[i], negs[i] = j;
    }

  /* Now make the result by performing the requested operations.  */
  result = ops[0];
  for (i = 1; i < n_ops; i++)
    result = simplify_gen_binary (negs[i] ? MINUS : PLUS, mode, result, ops[i]);

  return negate ? gen_rtx_NEG (mode, result) : result;
}

struct cfc_args
{
  rtx op0, op1;			/* Input */
  int equal, op0lt, op1lt;	/* Output */
};

static void
check_fold_consts (data)
  PTR data;
{
  struct cfc_args *args = (struct cfc_args *) data;
  REAL_VALUE_TYPE d0, d1;

  REAL_VALUE_FROM_CONST_DOUBLE (d0, args->op0);
  REAL_VALUE_FROM_CONST_DOUBLE (d1, args->op1);
  args->equal = REAL_VALUES_EQUAL (d0, d1);
  args->op0lt = REAL_VALUES_LESS (d0, d1);
  args->op1lt = REAL_VALUES_LESS (d1, d0);
}

/* Like simplify_binary_operation except used for relational operators.
   MODE is the mode of the operands, not that of the result.  If MODE
   is VOIDmode, both operands must also be VOIDmode and we compare the
   operands in "infinite precision".

   If no simplification is possible, this function returns zero.  Otherwise,
   it returns either const_true_rtx or const0_rtx.  */

rtx
simplify_relational_operation (code, mode, op0, op1)
     enum rtx_code code;
     enum machine_mode mode;
     rtx op0, op1;
{
  int equal, op0lt, op0ltu, op1lt, op1ltu;
  rtx tem;

  /* If op0 is a compare, extract the comparison arguments from it.  */
  if (GET_CODE (op0) == COMPARE && op1 == const0_rtx)
    op1 = XEXP (op0, 1), op0 = XEXP (op0, 0);

  /* We can't simplify MODE_CC values since we don't know what the
     actual comparison is.  */
  if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_CC
#ifdef HAVE_cc0
      || op0 == cc0_rtx
#endif
      )
    return 0;

  /* For integer comparisons of A and B maybe we can simplify A - B and can
     then simplify a comparison of that with zero.  If A and B are both either
     a register or a CONST_INT, this can't help; testing for these cases will
     prevent infinite recursion here and speed things up.

     If CODE is an unsigned comparison, then we can never do this optimization,
     because it gives an incorrect result if the subtraction wraps around zero.
     ANSI C defines unsigned operations such that they never overflow, and
     thus such cases can not be ignored.  */

  if (INTEGRAL_MODE_P (mode) && op1 != const0_rtx
      && ! ((GET_CODE (op0) == REG || GET_CODE (op0) == CONST_INT)
	    && (GET_CODE (op1) == REG || GET_CODE (op1) == CONST_INT))
      && 0 != (tem = simplify_binary_operation (MINUS, mode, op0, op1))
      && code != GTU && code != GEU && code != LTU && code != LEU)
    return simplify_relational_operation (signed_condition (code),
					  mode, tem, const0_rtx);

  /* For non-IEEE floating-point, if the two operands are equal, we know the
     result.  */
  if (rtx_equal_p (op0, op1)
      && (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	  || ! FLOAT_MODE_P (GET_MODE (op0)) || flag_fast_math))
    equal = 1, op0lt = 0, op0ltu = 0, op1lt = 0, op1ltu = 0;

  /* If the operands are floating-point constants, see if we can fold
     the result.  */
#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
  else if (GET_CODE (op0) == CONST_DOUBLE && GET_CODE (op1) == CONST_DOUBLE
	   && GET_MODE_CLASS (GET_MODE (op0)) == MODE_FLOAT)
    {
      struct cfc_args args;

      /* Setup input for check_fold_consts() */
      args.op0 = op0;
      args.op1 = op1;
      
      if (do_float_handler(check_fold_consts, (PTR) &args) == 0)
	/* We got an exception from check_fold_consts() */
	return 0;

      /* Receive output from check_fold_consts() */
      equal = args.equal;
      op0lt = op0ltu = args.op0lt;
      op1lt = op1ltu = args.op1lt;
    }
#endif  /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */

  /* Otherwise, see if the operands are both integers.  */
  else if ((GET_MODE_CLASS (mode) == MODE_INT || mode == VOIDmode)
	   && (GET_CODE (op0) == CONST_DOUBLE || GET_CODE (op0) == CONST_INT)
	   && (GET_CODE (op1) == CONST_DOUBLE || GET_CODE (op1) == CONST_INT))
    {
      int width = GET_MODE_BITSIZE (mode);
      HOST_WIDE_INT l0s, h0s, l1s, h1s;
      unsigned HOST_WIDE_INT l0u, h0u, l1u, h1u;

      /* Get the two words comprising each integer constant.  */
      if (GET_CODE (op0) == CONST_DOUBLE)
	{
	  l0u = l0s = CONST_DOUBLE_LOW (op0);
	  h0u = h0s = CONST_DOUBLE_HIGH (op0);
	}
      else
	{
	  l0u = l0s = INTVAL (op0);
	  h0u = h0s = l0s < 0 ? -1 : 0;
	}
	  
      if (GET_CODE (op1) == CONST_DOUBLE)
	{
	  l1u = l1s = CONST_DOUBLE_LOW (op1);
	  h1u = h1s = CONST_DOUBLE_HIGH (op1);
	}
      else
	{
	  l1u = l1s = INTVAL (op1);
	  h1u = h1s = l1s < 0 ? -1 : 0;
	}

      /* If WIDTH is nonzero and smaller than HOST_BITS_PER_WIDE_INT,
	 we have to sign or zero-extend the values.  */
      if (width != 0 && width <= HOST_BITS_PER_WIDE_INT)
	h0u = h1u = 0, h0s = l0s < 0 ? -1 : 0, h1s = l1s < 0 ? -1 : 0;

      if (width != 0 && width < HOST_BITS_PER_WIDE_INT)
	{
	  l0u &= ((HOST_WIDE_INT) 1 << width) - 1;
	  l1u &= ((HOST_WIDE_INT) 1 << width) - 1;

	  if (l0s & ((HOST_WIDE_INT) 1 << (width - 1)))
	    l0s |= ((HOST_WIDE_INT) (-1) << width);

	  if (l1s & ((HOST_WIDE_INT) 1 << (width - 1)))
	    l1s |= ((HOST_WIDE_INT) (-1) << width);
	}

      equal = (h0u == h1u && l0u == l1u);
      op0lt = (h0s < h1s || (h0s == h1s && l0s < l1s));
      op1lt = (h1s < h0s || (h1s == h0s && l1s < l0s));
      op0ltu = (h0u < h1u || (h0u == h1u && l0u < l1u));
      op1ltu = (h1u < h0u || (h1u == h0u && l1u < l0u));
    }

  /* Otherwise, there are some code-specific tests we can make.  */
  else
    {
      switch (code)
	{
	case EQ:
	  /* References to the frame plus a constant or labels cannot
	     be zero, but a SYMBOL_REF can due to #pragma weak.  */
	  if (((NONZERO_BASE_PLUS_P (op0) && op1 == const0_rtx)
	       || GET_CODE (op0) == LABEL_REF)
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	      /* On some machines, the ap reg can be 0 sometimes.  */
	      && op0 != arg_pointer_rtx
#endif
		)
	    return const0_rtx;
	  break;

	case NE:
	  if (((NONZERO_BASE_PLUS_P (op0) && op1 == const0_rtx)
	       || GET_CODE (op0) == LABEL_REF)
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	      && op0 != arg_pointer_rtx
#endif
	      )
	    return const_true_rtx;
	  break;

	case GEU:
	  /* Unsigned values are never negative.  */
	  if (op1 == const0_rtx)
	    return const_true_rtx;
	  break;

	case LTU:
	  if (op1 == const0_rtx)
	    return const0_rtx;
	  break;

	case LEU:
	  /* Unsigned values are never greater than the largest
	     unsigned value.  */
	  if (GET_CODE (op1) == CONST_INT
	      && (unsigned HOST_WIDE_INT) INTVAL (op1) == GET_MODE_MASK (mode)
	    && INTEGRAL_MODE_P (mode))
	  return const_true_rtx;
	  break;

	case GTU:
	  if (GET_CODE (op1) == CONST_INT
	      && (unsigned HOST_WIDE_INT) INTVAL (op1) == GET_MODE_MASK (mode)
	      && INTEGRAL_MODE_P (mode))
	    return const0_rtx;
	  break;
	  
	default:
	  break;
	}

      return 0;
    }

  /* If we reach here, EQUAL, OP0LT, OP0LTU, OP1LT, and OP1LTU are set
     as appropriate.  */
  switch (code)
    {
    case EQ:
      return equal ? const_true_rtx : const0_rtx;
    case NE:
      return ! equal ? const_true_rtx : const0_rtx;
    case LT:
      return op0lt ? const_true_rtx : const0_rtx;
    case GT:
      return op1lt ? const_true_rtx : const0_rtx;
    case LTU:
      return op0ltu ? const_true_rtx : const0_rtx;
    case GTU:
      return op1ltu ? const_true_rtx : const0_rtx;
    case LE:
      return equal || op0lt ? const_true_rtx : const0_rtx;
    case GE:
      return equal || op1lt ? const_true_rtx : const0_rtx;
    case LEU:
      return equal || op0ltu ? const_true_rtx : const0_rtx;
    case GEU:
      return equal || op1ltu ? const_true_rtx : const0_rtx;
    default:
      abort ();
    }
}

/* Simplify CODE, an operation with result mode MODE and three operands,
   OP0, OP1, and OP2.  OP0_MODE was the mode of OP0 before it became
   a constant.  Return 0 if no simplifications is possible.  */

rtx
simplify_ternary_operation (code, mode, op0_mode, op0, op1, op2)
     enum rtx_code code;
     enum machine_mode mode, op0_mode;
     rtx op0, op1, op2;
{
  int width = GET_MODE_BITSIZE (mode);

  /* VOIDmode means "infinite" precision.  */
  if (width == 0)
    width = HOST_BITS_PER_WIDE_INT;

  switch (code)
    {
    case SIGN_EXTRACT:
    case ZERO_EXTRACT:
      if (GET_CODE (op0) == CONST_INT
	  && GET_CODE (op1) == CONST_INT
	  && GET_CODE (op2) == CONST_INT
	  && INTVAL (op1) + INTVAL (op2) <= GET_MODE_BITSIZE (op0_mode)
	  && width <= HOST_BITS_PER_WIDE_INT)
	{
	  /* Extracting a bit-field from a constant */
	  HOST_WIDE_INT val = INTVAL (op0);

	  if (BITS_BIG_ENDIAN)
	    val >>= (GET_MODE_BITSIZE (op0_mode)
		     - INTVAL (op2) - INTVAL (op1));
	  else
	    val >>= INTVAL (op2);

	  if (HOST_BITS_PER_WIDE_INT != INTVAL (op1))
	    {
	      /* First zero-extend.  */
	      val &= ((HOST_WIDE_INT) 1 << INTVAL (op1)) - 1;
	      /* If desired, propagate sign bit.  */
	      if (code == SIGN_EXTRACT
		  && (val & ((HOST_WIDE_INT) 1 << (INTVAL (op1) - 1))))
		val |= ~ (((HOST_WIDE_INT) 1 << INTVAL (op1)) - 1);
	    }

	  /* Clear the bits that don't belong in our mode,
	     unless they and our sign bit are all one.
	     So we get either a reasonable negative value or a reasonable
	     unsigned value for this mode.  */
	  if (width < HOST_BITS_PER_WIDE_INT
	      && ((val & ((HOST_WIDE_INT) (-1) << (width - 1)))
		  != ((HOST_WIDE_INT) (-1) << (width - 1))))
	    val &= ((HOST_WIDE_INT) 1 << width) - 1;

	  return GEN_INT (val);
	}
      break;

    case IF_THEN_ELSE:
      if (GET_CODE (op0) == CONST_INT)
	return op0 != const0_rtx ? op1 : op2;

      /* Convert a == b ? b : a to "a".  */
      if (GET_CODE (op0) == NE && ! side_effects_p (op0)
	  && rtx_equal_p (XEXP (op0, 0), op1)
	  && rtx_equal_p (XEXP (op0, 1), op2))
	return op1;
      else if (GET_CODE (op0) == EQ && ! side_effects_p (op0)
	  && rtx_equal_p (XEXP (op0, 1), op1)
	  && rtx_equal_p (XEXP (op0, 0), op2))
	return op2;
      else if (GET_RTX_CLASS (GET_CODE (op0)) == '<' && ! side_effects_p (op0))
	{
	  rtx temp;
	  temp = simplify_relational_operation (GET_CODE (op0), op0_mode,
						XEXP (op0, 0), XEXP (op0, 1));
	  /* See if any simplifications were possible.  */
	  if (temp == const0_rtx)
	    return op2;
	  else if (temp == const1_rtx)
	    return op1;
	}
      break;

    default:
      abort ();
    }

  return 0;
}

/* Simplify X, an rtx expression.

   Return the simplified expression or NULL if no simplifications
   were possible.

   This is the preferred entry point into the simplification routines;
   however, we still allow passes to call the more specific routines.

   Right now GCC has three (yes, three) major bodies of RTL simplficiation
   code that need to be unified.

	1. fold_rtx in cse.c.  This code uses various CSE specific
	   information to aid in RTL simplification.

	2. simplify_rtx in combine.c.  Similar to fold_rtx, except that
	   it uses combine specific information to aid in RTL
	   simplification.

	3. The routines in this file.


   Long term we want to only have one body of simplification code; to
   get to that state I recommend the following steps:

	1. Pour over fold_rtx & simplify_rtx and move any simplifications
	   which are not pass dependent state into these routines.

	2. As code is moved by #1, change fold_rtx & simplify_rtx to
	   use this routine whenever possible.

	3. Allow for pass dependent state to be provided to these
	   routines and add simplifications based on the pass dependent
	   state.  Remove code from cse.c & combine.c that becomes
	   redundant/dead.

    It will take time, but ultimately the compiler will be easier to
    maintain and improve.  It's totally silly that when we add a
    simplification that it needs to be added to 4 places (3 for RTL
    simplification and 1 for tree simplification.  */
	   
rtx
simplify_rtx (x)
     rtx x;
{
  enum rtx_code code;
  enum machine_mode mode;

  mode = GET_MODE (x);
  code = GET_CODE (x);

  switch (GET_RTX_CLASS (code))
    {
    case '1':
      return simplify_unary_operation (code, mode,
				       XEXP (x, 0), GET_MODE (XEXP (x, 0)));
    case '2':
    case 'c':
      return simplify_binary_operation (code, mode, XEXP (x, 0), XEXP (x, 1));

    case '3':
    case 'b':
      return simplify_ternary_operation (code, mode, GET_MODE (XEXP (x, 0)),
					 XEXP (x, 0), XEXP (x, 1), XEXP (x, 2));

    case '<':
      return simplify_relational_operation (code, GET_MODE (XEXP (x, 0)),
					    XEXP (x, 0), XEXP (x, 1));
    default:
      return NULL;
    }
}
