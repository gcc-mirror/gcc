/* RTL simplification functions for GNU compiler.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "tree.h"
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
#include "ggc.h"

/* Simplification and canonicalization of RTL.  */

/* Nonzero if X has the form (PLUS frame-pointer integer).  We check for
   virtual regs here because the simplify_*_operation routines are called
   by integrate.c, which is called before virtual register instantiation.

   ?!? NONZERO_BASE_PLUS_P needs to move into
   a header file so that their definitions can be shared with the
   simplification routines in simplify-rtx.c.  Until then, do not
   change this macro without also changing the copy in simplify-rtx.c.  */

/* Allows reference to the stack pointer.

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

/* Much code operates on (low, high) pairs; the low value is an
   unsigned wide int, the high value a signed wide int.  We
   occasionally need to sign extend from low to high as if low were a
   signed wide int.  */
#define HWI_SIGN_EXTEND(low) \
 ((((HOST_WIDE_INT) low) < 0) ? ((HOST_WIDE_INT) -1) : ((HOST_WIDE_INT) 0))

static rtx neg_const_int PARAMS ((enum machine_mode, rtx));
static int simplify_plus_minus_op_data_cmp PARAMS ((const void *,
						    const void *));
static rtx simplify_plus_minus		PARAMS ((enum rtx_code,
						 enum machine_mode, rtx,
						 rtx, int));

/* Negate a CONST_INT rtx, truncating (because a conversion from a
   maximally negative number can overflow).  */
static rtx
neg_const_int (mode, i)
     enum machine_mode mode;
     rtx i;
{
  return gen_int_mode (- INTVAL (i), mode);
}


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
      && swap_commutative_operands_p (op0, op1))
    tem = op0, op0 = op1, op1 = tem;

  /* If this simplifies, do it.  */
  tem = simplify_binary_operation (code, mode, op0, op1);
  if (tem)
    return tem;

  /* Handle addition and subtraction specially.  Otherwise, just form
     the operation.  */

  if (code == PLUS || code == MINUS)
    {
      tem = simplify_plus_minus (code, mode, op0, op1, 1);
      if (tem)
	return tem;
    }

  return gen_rtx_fmt_ee (code, mode, op0, op1);
}

/* If X is a MEM referencing the constant pool, return the real value.
   Otherwise return X.  */
rtx
avoid_constant_pool_reference (x)
     rtx x;
{
  rtx c, addr;
  enum machine_mode cmode;

  if (GET_CODE (x) != MEM)
    return x;
  addr = XEXP (x, 0);

  if (GET_CODE (addr) == LO_SUM)
    addr = XEXP (addr, 1);

  if (GET_CODE (addr) != SYMBOL_REF
      || ! CONSTANT_POOL_ADDRESS_P (addr))
    return x;

  c = get_pool_constant (addr);
  cmode = get_pool_mode (addr);

  /* If we're accessing the constant in a different mode than it was
     originally stored, attempt to fix that up via subreg simplifications.
     If that fails we have no choice but to return the original memory.  */
  if (cmode != GET_MODE (x))
    {
      c = simplify_subreg (GET_MODE (x), c, cmode, 0);
      return c ? c : x;
    }

  return c;
}

/* Make a unary operation by first seeing if it folds and otherwise making
   the specified operation.  */

rtx
simplify_gen_unary (code, mode, op, op_mode)
     enum rtx_code code;
     enum machine_mode mode;
     rtx op;
     enum machine_mode op_mode;
{
  rtx tem;

  /* If this simplifies, use it.  */
  if ((tem = simplify_unary_operation (code, mode, op, op_mode)) != 0)
    return tem;

  return gen_rtx_fmt_e (code, mode, op);
}

/* Likewise for ternary operations.  */

rtx
simplify_gen_ternary (code, mode, op0_mode, op0, op1, op2)
     enum rtx_code code;
     enum machine_mode mode, op0_mode;
     rtx op0, op1, op2;
{
  rtx tem;

  /* If this simplifies, use it.  */
  if (0 != (tem = simplify_ternary_operation (code, mode, op0_mode,
					      op0, op1, op2)))
    return tem;

  return gen_rtx_fmt_eee (code, mode, op0, op1, op2);
}

/* Likewise, for relational operations.
   CMP_MODE specifies mode comparison is done in.
  */

rtx
simplify_gen_relational (code, mode, cmp_mode, op0, op1)
     enum rtx_code code;
     enum machine_mode mode;
     enum machine_mode cmp_mode;
     rtx op0, op1;
{
  rtx tem;

  if ((tem = simplify_relational_operation (code, cmp_mode, op0, op1)) != 0)
    return tem;

  /* For the following tests, ensure const0_rtx is op1.  */
  if (op0 == const0_rtx && swap_commutative_operands_p (op0, op1))
    tem = op0, op0 = op1, op1 = tem, code = swap_condition (code);

  /* If op0 is a compare, extract the comparison arguments from it.  */
  if (GET_CODE (op0) == COMPARE && op1 == const0_rtx)
    op1 = XEXP (op0, 1), op0 = XEXP (op0, 0);

  /* If op0 is a comparison, extract the comparison arguments form it.  */
  if (code == NE && op1 == const0_rtx
      && GET_RTX_CLASS (GET_CODE (op0)) == '<')
    return op0;
  else if (code == EQ && op1 == const0_rtx)
    {
      /* The following tests GET_RTX_CLASS (GET_CODE (op0)) == '<'.  */
      enum rtx_code new = reversed_comparison_code (op0, NULL_RTX);
      if (new != UNKNOWN)
        {
	  code = new;
	  mode = cmp_mode;
	  op1 = XEXP (op0, 1);
	  op0 = XEXP (op0, 0);
        }
    }

  /* Put complex operands first and constants second.  */
  if (swap_commutative_operands_p (op0, op1))
    tem = op0, op0 = op1, op1 = tem, code = swap_condition (code);

  return gen_rtx_fmt_ee (code, mode, op0, op1);
}

/* Replace all occurrences of OLD in X with NEW and try to simplify the
   resulting RTX.  Return a new RTX which is as simplified as possible.  */

rtx
simplify_replace_rtx (x, old, new)
     rtx x;
     rtx old;
     rtx new;
{
  enum rtx_code code = GET_CODE (x);
  enum machine_mode mode = GET_MODE (x);

  /* If X is OLD, return NEW.  Otherwise, if this is an expression, try
     to build a new expression substituting recursively.  If we can't do
     anything, return our input.  */

  if (x == old)
    return new;

  switch (GET_RTX_CLASS (code))
    {
    case '1':
      {
	enum machine_mode op_mode = GET_MODE (XEXP (x, 0));
	rtx op = (XEXP (x, 0) == old
		  ? new : simplify_replace_rtx (XEXP (x, 0), old, new));

	return simplify_gen_unary (code, mode, op, op_mode);
      }

    case '2':
    case 'c':
      return
	simplify_gen_binary (code, mode,
			     simplify_replace_rtx (XEXP (x, 0), old, new),
			     simplify_replace_rtx (XEXP (x, 1), old, new));
    case '<':
      {
	enum machine_mode op_mode = (GET_MODE (XEXP (x, 0)) != VOIDmode
				     ? GET_MODE (XEXP (x, 0))
				     : GET_MODE (XEXP (x, 1)));
	rtx op0 = simplify_replace_rtx (XEXP (x, 0), old, new);
	rtx op1 = simplify_replace_rtx (XEXP (x, 1), old, new);

	return
	  simplify_gen_relational (code, mode,
				   (op_mode != VOIDmode
				    ? op_mode
				    : GET_MODE (op0) != VOIDmode
				    ? GET_MODE (op0)
				    : GET_MODE (op1)),
				   op0, op1);
      }

    case '3':
    case 'b':
      {
	enum machine_mode op_mode = GET_MODE (XEXP (x, 0));
	rtx op0 = simplify_replace_rtx (XEXP (x, 0), old, new);

	return
	  simplify_gen_ternary (code, mode,
				(op_mode != VOIDmode
				 ? op_mode
				 : GET_MODE (op0)),
				op0,
				simplify_replace_rtx (XEXP (x, 1), old, new),
				simplify_replace_rtx (XEXP (x, 2), old, new));
      }

    case 'x':
      /* The only case we try to handle is a SUBREG.  */
      if (code == SUBREG)
	{
	  rtx exp;
	  exp = simplify_gen_subreg (GET_MODE (x),
				     simplify_replace_rtx (SUBREG_REG (x),
				     			   old, new),
				     GET_MODE (SUBREG_REG (x)),
				     SUBREG_BYTE (x));
	  if (exp)
	   x = exp;
	}
      return x;

    case 'o':
      if (code == MEM)
	return replace_equiv_address_nv (x,
					 simplify_replace_rtx (XEXP (x, 0),
							       old, new));
      else if (code == LO_SUM)
	{
	  rtx op0 = simplify_replace_rtx (XEXP (x, 0), old, new);
	  rtx op1 = simplify_replace_rtx (XEXP (x, 1), old, new);

	  /* (lo_sum (high x) x) -> x  */
	  if (GET_CODE (op0) == HIGH && rtx_equal_p (XEXP (op0, 0), op1))
	    return op1;

	  return gen_rtx_LO_SUM (mode, op0, op1);
	}
      else if (code == REG)
	{
	  if (REG_P (old) && REGNO (x) == REGNO (old))
	    return new;
	}

      return x;

    default:
      return x;
    }
  return x;
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
  unsigned int width = GET_MODE_BITSIZE (mode);
  rtx trueop = avoid_constant_pool_reference (op);

  if (code == VEC_DUPLICATE)
    {
      if (!VECTOR_MODE_P (mode))
	abort ();
      if (GET_MODE (trueop) != VOIDmode
	  && !VECTOR_MODE_P (GET_MODE (trueop))
	  && GET_MODE_INNER (mode) != GET_MODE (trueop))
	abort ();
      if (GET_MODE (trueop) != VOIDmode
	  && VECTOR_MODE_P (GET_MODE (trueop))
	  && GET_MODE_INNER (mode) != GET_MODE_INNER (GET_MODE (trueop)))
	abort ();
      if (GET_CODE (trueop) == CONST_INT || GET_CODE (trueop) == CONST_DOUBLE
	  || GET_CODE (trueop) == CONST_VECTOR)
	{
          int elt_size = GET_MODE_SIZE (GET_MODE_INNER (mode));
          unsigned n_elts = (GET_MODE_SIZE (mode) / elt_size);
	  rtvec v = rtvec_alloc (n_elts);
	  unsigned int i;

	  if (GET_CODE (trueop) != CONST_VECTOR)
	    for (i = 0; i < n_elts; i++)
	      RTVEC_ELT (v, i) = trueop;
	  else
	    {
	      enum machine_mode inmode = GET_MODE (trueop);
              int in_elt_size = GET_MODE_SIZE (GET_MODE_INNER (inmode));
              unsigned in_n_elts = (GET_MODE_SIZE (inmode) / in_elt_size);

	      if (in_n_elts >= n_elts || n_elts % in_n_elts)
		abort ();
	      for (i = 0; i < n_elts; i++)
	        RTVEC_ELT (v, i) = CONST_VECTOR_ELT (trueop, i % in_n_elts);
	    }
	  return gen_rtx_CONST_VECTOR (mode, v);
	}
    }

  /* The order of these tests is critical so that, for example, we don't
     check the wrong mode (input vs. output) for a conversion operation,
     such as FIX.  At some point, this should be simplified.  */

  if (code == FLOAT && GET_MODE (trueop) == VOIDmode
      && (GET_CODE (trueop) == CONST_DOUBLE || GET_CODE (trueop) == CONST_INT))
    {
      HOST_WIDE_INT hv, lv;
      REAL_VALUE_TYPE d;

      if (GET_CODE (trueop) == CONST_INT)
	lv = INTVAL (trueop), hv = HWI_SIGN_EXTEND (lv);
      else
	lv = CONST_DOUBLE_LOW (trueop),  hv = CONST_DOUBLE_HIGH (trueop);

      REAL_VALUE_FROM_INT (d, lv, hv, mode);
      d = real_value_truncate (mode, d);
      return CONST_DOUBLE_FROM_REAL_VALUE (d, mode);
    }
  else if (code == UNSIGNED_FLOAT && GET_MODE (trueop) == VOIDmode
	   && (GET_CODE (trueop) == CONST_DOUBLE
	       || GET_CODE (trueop) == CONST_INT))
    {
      HOST_WIDE_INT hv, lv;
      REAL_VALUE_TYPE d;

      if (GET_CODE (trueop) == CONST_INT)
	lv = INTVAL (trueop), hv = HWI_SIGN_EXTEND (lv);
      else
	lv = CONST_DOUBLE_LOW (trueop),  hv = CONST_DOUBLE_HIGH (trueop);

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

      REAL_VALUE_FROM_UNSIGNED_INT (d, lv, hv, mode);
      d = real_value_truncate (mode, d);
      return CONST_DOUBLE_FROM_REAL_VALUE (d, mode);
    }

  if (GET_CODE (trueop) == CONST_INT
      && width <= HOST_BITS_PER_WIDE_INT && width > 0)
    {
      HOST_WIDE_INT arg0 = INTVAL (trueop);
      HOST_WIDE_INT val;

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
	  /* When zero-extending a CONST_INT, we need to know its
             original mode.  */
	  if (op_mode == VOIDmode)
	    abort ();
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
	case FLOAT_EXTEND:
	case FLOAT_TRUNCATE:
	case SS_TRUNCATE:
	case US_TRUNCATE:
	  return 0;

	default:
	  abort ();
	}

      val = trunc_int_for_mode (val, mode);

      return GEN_INT (val);
    }

  /* We can do some operations on integer CONST_DOUBLEs.  Also allow
     for a DImode operation on a CONST_INT.  */
  else if (GET_MODE (trueop) == VOIDmode
	   && width <= HOST_BITS_PER_WIDE_INT * 2
	   && (GET_CODE (trueop) == CONST_DOUBLE
	       || GET_CODE (trueop) == CONST_INT))
    {
      unsigned HOST_WIDE_INT l1, lv;
      HOST_WIDE_INT h1, hv;

      if (GET_CODE (trueop) == CONST_DOUBLE)
	l1 = CONST_DOUBLE_LOW (trueop), h1 = CONST_DOUBLE_HIGH (trueop);
      else
	l1 = INTVAL (trueop), h1 = HWI_SIGN_EXTEND (l1);

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
	  if (op_mode == VOIDmode)
	    abort ();

	  if (GET_MODE_BITSIZE (op_mode) > HOST_BITS_PER_WIDE_INT)
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

	      hv = HWI_SIGN_EXTEND (lv);
	    }
	  break;

	case SQRT:
	  return 0;

	default:
	  return 0;
	}

      return immed_double_const (lv, hv, mode);
    }

  else if (GET_CODE (trueop) == CONST_DOUBLE
	   && GET_MODE_CLASS (mode) == MODE_FLOAT)
    {
      REAL_VALUE_TYPE d;
      REAL_VALUE_FROM_CONST_DOUBLE (d, trueop);

      switch (code)
	{
	case SQRT:
	  /* We don't attempt to optimize this.  */
	  return 0;

	case ABS:
	  d = REAL_VALUE_ABS (d);
	  break;
	case NEG:
	  d = REAL_VALUE_NEGATE (d);
	  break;
	case FLOAT_TRUNCATE:
	  d = real_value_truncate (mode, d);
	  break;
	case FLOAT_EXTEND:
	  /* All this does is change the mode.  */
	  break;
	case FIX:
	  real_arithmetic (&d, FIX_TRUNC_EXPR, &d, NULL);
	  break;

	default:
	  abort ();
	}
      return CONST_DOUBLE_FROM_REAL_VALUE (d, mode);
    }

  else if (GET_CODE (trueop) == CONST_DOUBLE
	   && GET_MODE_CLASS (GET_MODE (trueop)) == MODE_FLOAT
	   && GET_MODE_CLASS (mode) == MODE_INT
	   && width <= HOST_BITS_PER_WIDE_INT && width > 0)
    {
      HOST_WIDE_INT i;
      REAL_VALUE_TYPE d;
      REAL_VALUE_FROM_CONST_DOUBLE (d, trueop);
      switch (code)
	{
	case FIX:		i = REAL_VALUE_FIX (d);		  break;
	case UNSIGNED_FIX:	i = REAL_VALUE_UNSIGNED_FIX (d);  break;
	default:
	  abort ();
	}
      return gen_int_mode (i, mode);
    }

  /* This was formerly used only for non-IEEE float.
     eggert@twinsun.com says it is safe for IEEE also.  */
  else
    {
      enum rtx_code reversed;
      /* There are some simplifications we can do even if the operands
	 aren't constant.  */
      switch (code)
	{
	case NOT:
	  /* (not (not X)) == X.  */
	  if (GET_CODE (op) == NOT)
	    return XEXP (op, 0);

	  /* (not (eq X Y)) == (ne X Y), etc.  */
	  if (mode == BImode && GET_RTX_CLASS (GET_CODE (op)) == '<'
	      && ((reversed = reversed_comparison_code (op, NULL_RTX))
		  != UNKNOWN))
	    return gen_rtx_fmt_ee (reversed,
				   op_mode, XEXP (op, 0), XEXP (op, 1));
	  break;

	case NEG:
	  /* (neg (neg X)) == X.  */
	  if (GET_CODE (op) == NEG)
	    return XEXP (op, 0);
	  break;

	case SIGN_EXTEND:
	  /* (sign_extend (truncate (minus (label_ref L1) (label_ref L2))))
	     becomes just the MINUS if its mode is MODE.  This allows
	     folding switch statements on machines using casesi (such as
	     the VAX).  */
	  if (GET_CODE (op) == TRUNCATE
	      && GET_MODE (XEXP (op, 0)) == mode
	      && GET_CODE (XEXP (op, 0)) == MINUS
	      && GET_CODE (XEXP (XEXP (op, 0), 0)) == LABEL_REF
	      && GET_CODE (XEXP (XEXP (op, 0), 1)) == LABEL_REF)
	    return XEXP (op, 0);

#if defined(POINTERS_EXTEND_UNSIGNED) && !defined(HAVE_ptr_extend)
	  if (! POINTERS_EXTEND_UNSIGNED
	      && mode == Pmode && GET_MODE (op) == ptr_mode
	      && (CONSTANT_P (op)
		  || (GET_CODE (op) == SUBREG
		      && GET_CODE (SUBREG_REG (op)) == REG
		      && REG_POINTER (SUBREG_REG (op))
		      && GET_MODE (SUBREG_REG (op)) == Pmode)))
	    return convert_memory_address (Pmode, op);
#endif
	  break;

#if defined(POINTERS_EXTEND_UNSIGNED) && !defined(HAVE_ptr_extend)
	case ZERO_EXTEND:
	  if (POINTERS_EXTEND_UNSIGNED > 0
	      && mode == Pmode && GET_MODE (op) == ptr_mode
	      && (CONSTANT_P (op)
		  || (GET_CODE (op) == SUBREG
		      && GET_CODE (SUBREG_REG (op)) == REG
		      && REG_POINTER (SUBREG_REG (op))
		      && GET_MODE (SUBREG_REG (op)) == Pmode)))
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
  HOST_WIDE_INT arg0, arg1, arg0s, arg1s;
  HOST_WIDE_INT val;
  unsigned int width = GET_MODE_BITSIZE (mode);
  rtx tem;
  rtx trueop0 = avoid_constant_pool_reference (op0);
  rtx trueop1 = avoid_constant_pool_reference (op1);

  /* Relational operations don't work here.  We must know the mode
     of the operands in order to do the comparison correctly.
     Assuming a full word can give incorrect results.
     Consider comparing 128 with -128 in QImode.  */

  if (GET_RTX_CLASS (code) == '<')
    abort ();

  /* Make sure the constant is second.  */
  if (GET_RTX_CLASS (code) == 'c'
      && swap_commutative_operands_p (trueop0, trueop1))
    {
      tem = op0, op0 = op1, op1 = tem;
      tem = trueop0, trueop0 = trueop1, trueop1 = tem;
    }

  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_CODE (trueop0) == CONST_DOUBLE
      && GET_CODE (trueop1) == CONST_DOUBLE
      && mode == GET_MODE (op0) && mode == GET_MODE (op1))
    {
      REAL_VALUE_TYPE f0, f1, value;

      REAL_VALUE_FROM_CONST_DOUBLE (f0, trueop0);
      REAL_VALUE_FROM_CONST_DOUBLE (f1, trueop1);
      f0 = real_value_truncate (mode, f0);
      f1 = real_value_truncate (mode, f1);

      if (code == DIV
	  && !MODE_HAS_INFINITIES (mode)
	  && REAL_VALUES_EQUAL (f1, dconst0))
	return 0;

      REAL_ARITHMETIC (value, rtx_to_tree_code (code), f0, f1);

      value = real_value_truncate (mode, value);
      return CONST_DOUBLE_FROM_REAL_VALUE (value, mode);
    }

  /* We can fold some multi-word operations.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && width == HOST_BITS_PER_WIDE_INT * 2
      && (GET_CODE (trueop0) == CONST_DOUBLE
	  || GET_CODE (trueop0) == CONST_INT)
      && (GET_CODE (trueop1) == CONST_DOUBLE
	  || GET_CODE (trueop1) == CONST_INT))
    {
      unsigned HOST_WIDE_INT l1, l2, lv;
      HOST_WIDE_INT h1, h2, hv;

      if (GET_CODE (trueop0) == CONST_DOUBLE)
	l1 = CONST_DOUBLE_LOW (trueop0), h1 = CONST_DOUBLE_HIGH (trueop0);
      else
	l1 = INTVAL (trueop0), h1 = HWI_SIGN_EXTEND (l1);

      if (GET_CODE (trueop1) == CONST_DOUBLE)
	l2 = CONST_DOUBLE_LOW (trueop1), h2 = CONST_DOUBLE_HIGH (trueop1);
      else
	l2 = INTVAL (trueop1), h2 = HWI_SIGN_EXTEND (l2);

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

	  if (h2 != 0 || l2 >= GET_MODE_BITSIZE (mode))
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
	  /* Maybe simplify x + 0 to x.  The two expressions are equivalent
	     when x is NaN, infinite, or finite and nonzero.  They aren't
	     when x is -0 and the rounding mode is not towards -infinity,
	     since (-0) + 0 is then 0.  */
	  if (!HONOR_SIGNED_ZEROS (mode) && trueop1 == CONST0_RTX (mode))
	    return op0;

	  /* ((-a) + b) -> (b - a) and similarly for (a + (-b)).  These
	     transformations are safe even for IEEE.  */
	  if (GET_CODE (op0) == NEG)
	    return simplify_gen_binary (MINUS, mode, op1, XEXP (op0, 0));
	  else if (GET_CODE (op1) == NEG)
	    return simplify_gen_binary (MINUS, mode, op0, XEXP (op1, 0));

	  /* (~a) + 1 -> -a */
	  if (INTEGRAL_MODE_P (mode)
	      && GET_CODE (op0) == NOT
	      && trueop1 == const1_rtx)
	    return gen_rtx_NEG (mode, XEXP (op0, 0));

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
		  || GET_CODE (op1) == PLUS || GET_CODE (op1) == MINUS
		  || (GET_CODE (op0) == CONST
		      && GET_CODE (XEXP (op0, 0)) == PLUS)
		  || (GET_CODE (op1) == CONST
		      && GET_CODE (XEXP (op1, 0)) == PLUS))
	      && (tem = simplify_plus_minus (code, mode, op0, op1, 0)) != 0)
	    return tem;
	  break;

	case COMPARE:
#ifdef HAVE_cc0
	  /* Convert (compare FOO (const_int 0)) to FOO unless we aren't
	     using cc0, in which case we want to leave it as a COMPARE
	     so we can distinguish it from a register-register-copy.

	     In IEEE floating point, x-0 is not the same as x.  */

	  if ((TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	       || ! FLOAT_MODE_P (mode) || flag_unsafe_math_optimizations)
	      && trueop1 == CONST0_RTX (mode))
	    return op0;
#endif

	  /* Convert (compare (gt (flags) 0) (lt (flags) 0)) to (flags).  */
	  if (((GET_CODE (op0) == GT && GET_CODE (op1) == LT)
	       || (GET_CODE (op0) == GTU && GET_CODE (op1) == LTU))
	      && XEXP (op0, 1) == const0_rtx && XEXP (op1, 1) == const0_rtx)
	    {
	      rtx xop00 = XEXP (op0, 0);
	      rtx xop10 = XEXP (op1, 0);

#ifdef HAVE_cc0
	      if (GET_CODE (xop00) == CC0 && GET_CODE (xop10) == CC0)
#else
	      if (GET_CODE (xop00) == REG && GET_CODE (xop10) == REG
		  && GET_MODE (xop00) == GET_MODE (xop10)
		  && REGNO (xop00) == REGNO (xop10)
		  && GET_MODE_CLASS (GET_MODE (xop00)) == MODE_CC
		  && GET_MODE_CLASS (GET_MODE (xop10)) == MODE_CC)
#endif
		return xop00;
	    }
	  break;

	case MINUS:
	  /* We can't assume x-x is 0 even with non-IEEE floating point,
	     but since it is zero except in very strange circumstances, we
	     will treat it as zero with -funsafe-math-optimizations.  */
	  if (rtx_equal_p (trueop0, trueop1)
	      && ! side_effects_p (op0)
	      && (! FLOAT_MODE_P (mode) || flag_unsafe_math_optimizations))
	    return CONST0_RTX (mode);

	  /* Change subtraction from zero into negation.  (0 - x) is the
	     same as -x when x is NaN, infinite, or finite and nonzero.
	     But if the mode has signed zeros, and does not round towards
	     -infinity, then 0 - 0 is 0, not -0.  */
	  if (!HONOR_SIGNED_ZEROS (mode) && trueop0 == CONST0_RTX (mode))
	    return gen_rtx_NEG (mode, op1);

	  /* (-1 - a) is ~a.  */
	  if (trueop0 == constm1_rtx)
	    return gen_rtx_NOT (mode, op1);

	  /* Subtracting 0 has no effect unless the mode has signed zeros
	     and supports rounding towards -infinity.  In such a case,
	     0 - 0 is -0.  */
	  if (!(HONOR_SIGNED_ZEROS (mode)
		&& HONOR_SIGN_DEPENDENT_ROUNDING (mode))
	      && trueop1 == CONST0_RTX (mode))
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

	  /* (a - (-b)) -> (a + b).  True even for IEEE.  */
	  if (GET_CODE (op1) == NEG)
	    return simplify_gen_binary (PLUS, mode, op0, XEXP (op1, 0));

	  /* If one of the operands is a PLUS or a MINUS, see if we can
	     simplify this by the associative law.
	     Don't use the associative law for floating point.
	     The inaccuracy makes it nonassociative,
	     and subtle programs can break if operations are associated.  */

	  if (INTEGRAL_MODE_P (mode)
	      && (GET_CODE (op0) == PLUS || GET_CODE (op0) == MINUS
		  || GET_CODE (op1) == PLUS || GET_CODE (op1) == MINUS
		  || (GET_CODE (op0) == CONST
		      && GET_CODE (XEXP (op0, 0)) == PLUS)
		  || (GET_CODE (op1) == CONST
		      && GET_CODE (XEXP (op1, 0)) == PLUS))
	      && (tem = simplify_plus_minus (code, mode, op0, op1, 0)) != 0)
	    return tem;

	  /* Don't let a relocatable value get a negative coeff.  */
	  if (GET_CODE (op1) == CONST_INT && GET_MODE (op0) != VOIDmode)
	    return simplify_gen_binary (PLUS, mode,
					op0,
					neg_const_int (mode, op1));

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
	  if (trueop1 == constm1_rtx)
	    {
	      tem = simplify_unary_operation (NEG, mode, op0, mode);

	      return tem ? tem : gen_rtx_NEG (mode, op0);
	    }

	  /* Maybe simplify x * 0 to 0.  The reduction is not valid if
	     x is NaN, since x * 0 is then also NaN.  Nor is it valid
	     when the mode has signed zeros, since multiplying a negative
	     number by 0 will give -0, not 0.  */
	  if (!HONOR_NANS (mode)
	      && !HONOR_SIGNED_ZEROS (mode)
	      && trueop1 == CONST0_RTX (mode)
	      && ! side_effects_p (op0))
	    return op1;

	  /* In IEEE floating point, x*1 is not equivalent to x for
	     signalling NaNs.  */
	  if (!HONOR_SNANS (mode)
	      && trueop1 == CONST1_RTX (mode))
	    return op0;

	  /* Convert multiply by constant power of two into shift unless
	     we are still generating RTL.  This test is a kludge.  */
	  if (GET_CODE (trueop1) == CONST_INT
	      && (val = exact_log2 (INTVAL (trueop1))) >= 0
	      /* If the mode is larger than the host word size, and the
		 uppermost bit is set, then this isn't a power of two due
		 to implicit sign extension.  */
	      && (width <= HOST_BITS_PER_WIDE_INT
		  || val != HOST_BITS_PER_WIDE_INT - 1)
	      && ! rtx_equal_function_value_matters)
	    return gen_rtx_ASHIFT (mode, op0, GEN_INT (val));

	  /* x*2 is x+x and x*(-1) is -x */
	  if (GET_CODE (trueop1) == CONST_DOUBLE
	      && GET_MODE_CLASS (GET_MODE (trueop1)) == MODE_FLOAT
	      && GET_MODE (op0) == mode)
	    {
	      REAL_VALUE_TYPE d;
	      REAL_VALUE_FROM_CONST_DOUBLE (d, trueop1);

	      if (REAL_VALUES_EQUAL (d, dconst2))
		return gen_rtx_PLUS (mode, op0, copy_rtx (op0));

	      if (REAL_VALUES_EQUAL (d, dconstm1))
		return gen_rtx_NEG (mode, op0);
	    }
	  break;

	case IOR:
	  if (trueop1 == const0_rtx)
	    return op0;
	  if (GET_CODE (trueop1) == CONST_INT
	      && ((INTVAL (trueop1) & GET_MODE_MASK (mode))
	          == GET_MODE_MASK (mode)))
	    return op1;
	  if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0))
	    return op0;
	  /* A | (~A) -> -1 */
	  if (((GET_CODE (op0) == NOT && rtx_equal_p (XEXP (op0, 0), op1))
	       || (GET_CODE (op1) == NOT && rtx_equal_p (XEXP (op1, 0), op0)))
	      && ! side_effects_p (op0)
	      && GET_MODE_CLASS (mode) != MODE_CC)
	    return constm1_rtx;
	  break;

	case XOR:
	  if (trueop1 == const0_rtx)
	    return op0;
	  if (GET_CODE (trueop1) == CONST_INT
	      && ((INTVAL (trueop1) & GET_MODE_MASK (mode))
		  == GET_MODE_MASK (mode)))
	    return gen_rtx_NOT (mode, op0);
	  if (trueop0 == trueop1 && ! side_effects_p (op0)
	      && GET_MODE_CLASS (mode) != MODE_CC)
	    return const0_rtx;
	  break;

	case AND:
	  if (trueop1 == const0_rtx && ! side_effects_p (op0))
	    return const0_rtx;
	  if (GET_CODE (trueop1) == CONST_INT
	      && ((INTVAL (trueop1) & GET_MODE_MASK (mode))
		  == GET_MODE_MASK (mode)))
	    return op0;
	  if (trueop0 == trueop1 && ! side_effects_p (op0)
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
	  if (GET_CODE (trueop1) == CONST_INT
	      && (arg1 = exact_log2 (INTVAL (trueop1))) > 0)
	    return gen_rtx_LSHIFTRT (mode, op0, GEN_INT (arg1));

	  /* ... fall through ...  */

	case DIV:
	  if (trueop1 == CONST1_RTX (mode))
	    {
	      /* On some platforms DIV uses narrower mode than its
		 operands.  */
	      rtx x = gen_lowpart_common (mode, op0);
	      if (x)
		return x;
	      else if (mode != GET_MODE (op0) && GET_MODE (op0) != VOIDmode)
		return gen_lowpart_SUBREG (mode, op0);
	      else
		return op0;
	    }

	  /* Maybe change 0 / x to 0.  This transformation isn't safe for
	     modes with NaNs, since 0 / 0 will then be NaN rather than 0.
	     Nor is it safe for modes with signed zeros, since dividing
	     0 by a negative number gives -0, not 0.  */
	  if (!HONOR_NANS (mode)
	      && !HONOR_SIGNED_ZEROS (mode)
	      && trueop0 == CONST0_RTX (mode)
	      && ! side_effects_p (op1))
	    return op0;

	  /* Change division by a constant into multiplication.  Only do
	     this with -funsafe-math-optimizations.  */
	  else if (GET_CODE (trueop1) == CONST_DOUBLE
		   && GET_MODE_CLASS (GET_MODE (trueop1)) == MODE_FLOAT
		   && trueop1 != CONST0_RTX (mode)
		   && flag_unsafe_math_optimizations)
	    {
	      REAL_VALUE_TYPE d;
	      REAL_VALUE_FROM_CONST_DOUBLE (d, trueop1);

	      if (! REAL_VALUES_EQUAL (d, dconst0))
		{
		  REAL_ARITHMETIC (d, rtx_to_tree_code (DIV), dconst1, d);
		  return gen_rtx_MULT (mode, op0,
				       CONST_DOUBLE_FROM_REAL_VALUE (d, mode));
		}
	    }
	  break;

	case UMOD:
	  /* Handle modulus by power of two (mod with 1 handled below).  */
	  if (GET_CODE (trueop1) == CONST_INT
	      && exact_log2 (INTVAL (trueop1)) > 0)
	    return gen_rtx_AND (mode, op0, GEN_INT (INTVAL (op1) - 1));

	  /* ... fall through ...  */

	case MOD:
	  if ((trueop0 == const0_rtx || trueop1 == const1_rtx)
	      && ! side_effects_p (op0) && ! side_effects_p (op1))
	    return const0_rtx;
	  break;

	case ROTATERT:
	case ROTATE:
	  /* Rotating ~0 always results in ~0.  */
	  if (GET_CODE (trueop0) == CONST_INT && width <= HOST_BITS_PER_WIDE_INT
	      && (unsigned HOST_WIDE_INT) INTVAL (trueop0) == GET_MODE_MASK (mode)
	      && ! side_effects_p (op1))
	    return op0;

	  /* ... fall through ...  */

	case ASHIFT:
	case ASHIFTRT:
	case LSHIFTRT:
	  if (trueop1 == const0_rtx)
	    return op0;
	  if (trueop0 == const0_rtx && ! side_effects_p (op1))
	    return op0;
	  break;

	case SMIN:
	  if (width <= HOST_BITS_PER_WIDE_INT && GET_CODE (trueop1) == CONST_INT
	      && INTVAL (trueop1) == (HOST_WIDE_INT) 1 << (width -1)
	      && ! side_effects_p (op0))
	    return op1;
	  else if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0))
	    return op0;
	  break;

	case SMAX:
	  if (width <= HOST_BITS_PER_WIDE_INT && GET_CODE (trueop1) == CONST_INT
	      && ((unsigned HOST_WIDE_INT) INTVAL (trueop1)
		  == (unsigned HOST_WIDE_INT) GET_MODE_MASK (mode) >> 1)
	      && ! side_effects_p (op0))
	    return op1;
	  else if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0))
	    return op0;
	  break;

	case UMIN:
	  if (trueop1 == const0_rtx && ! side_effects_p (op0))
	    return op1;
	  else if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0))
	    return op0;
	  break;

	case UMAX:
	  if (trueop1 == constm1_rtx && ! side_effects_p (op0))
	    return op1;
	  else if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0))
	    return op0;
	  break;

	case SS_PLUS:
	case US_PLUS:
	case SS_MINUS:
	case US_MINUS:
	  /* ??? There are simplifications that can be done.  */
	  return 0;

	case VEC_SELECT:
	case VEC_CONCAT:
	  return 0;

	default:
	  abort ();
	}

      return 0;
    }

  /* Get the integer argument values in two forms:
     zero-extended in ARG0, ARG1 and sign-extended in ARG0S, ARG1S.  */

  arg0 = INTVAL (trueop0);
  arg1 = INTVAL (trueop1);

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
      if (arg1s == 0
	  || (arg0s == (HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT - 1)
	      && arg1s == -1))
	return 0;
      val = arg0s / arg1s;
      break;

    case MOD:
      if (arg1s == 0
	  || (arg0s == (HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT - 1)
	      && arg1s == -1))
	return 0;
      val = arg0s % arg1s;
      break;

    case UDIV:
      if (arg1 == 0
	  || (arg0s == (HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT - 1)
	      && arg1s == -1))
	return 0;
      val = (unsigned HOST_WIDE_INT) arg0 / arg1;
      break;

    case UMOD:
      if (arg1 == 0
	  || (arg0s == (HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT - 1)
	      && arg1s == -1))
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
   we rebuild the operation.

   If FORCE is true, then always generate the rtx.  This is used to
   canonicalize stuff emitted from simplify_gen_binary.  Note that this
   can still fail if the rtx is too complex.  It won't fail just because
   the result is not 'simpler' than the input, however.  */

struct simplify_plus_minus_op_data
{
  rtx op;
  int neg;
};

static int
simplify_plus_minus_op_data_cmp (p1, p2)
     const void *p1;
     const void *p2;
{
  const struct simplify_plus_minus_op_data *d1 = p1;
  const struct simplify_plus_minus_op_data *d2 = p2;

  return (commutative_operand_precedence (d2->op)
	  - commutative_operand_precedence (d1->op));
}

static rtx
simplify_plus_minus (code, mode, op0, op1, force)
     enum rtx_code code;
     enum machine_mode mode;
     rtx op0, op1;
     int force;
{
  struct simplify_plus_minus_op_data ops[8];
  rtx result, tem;
  int n_ops = 2, input_ops = 2, input_consts = 0, n_consts;
  int first, negate, changed;
  int i, j;

  memset ((char *) ops, 0, sizeof ops);

  /* Set up the two operands and then expand them until nothing has been
     changed.  If we run out of room in our array, give up; this should
     almost never happen.  */

  ops[0].op = op0;
  ops[0].neg = 0;
  ops[1].op = op1;
  ops[1].neg = (code == MINUS);

  do
    {
      changed = 0;

      for (i = 0; i < n_ops; i++)
	{
	  rtx this_op = ops[i].op;
	  int this_neg = ops[i].neg;
	  enum rtx_code this_code = GET_CODE (this_op);

	  switch (this_code)
	    {
	    case PLUS:
	    case MINUS:
	      if (n_ops == 7)
		return NULL_RTX;

	      ops[n_ops].op = XEXP (this_op, 1);
	      ops[n_ops].neg = (this_code == MINUS) ^ this_neg;
	      n_ops++;

	      ops[i].op = XEXP (this_op, 0);
	      input_ops++;
	      changed = 1;
	      break;

	    case NEG:
	      ops[i].op = XEXP (this_op, 0);
	      ops[i].neg = ! this_neg;
	      changed = 1;
	      break;

	    case CONST:
	      if (n_ops < 7
		  && GET_CODE (XEXP (this_op, 0)) == PLUS
		  && CONSTANT_P (XEXP (XEXP (this_op, 0), 0))
		  && CONSTANT_P (XEXP (XEXP (this_op, 0), 1)))
		{
		  ops[i].op = XEXP (XEXP (this_op, 0), 0);
		  ops[n_ops].op = XEXP (XEXP (this_op, 0), 1);
		  ops[n_ops].neg = this_neg;
		  n_ops++;
		  input_consts++;
		  changed = 1;
		}
	      break;

	    case NOT:
	      /* ~a -> (-a - 1) */
	      if (n_ops != 7)
		{
		  ops[n_ops].op = constm1_rtx;
		  ops[n_ops++].neg = this_neg;
		  ops[i].op = XEXP (this_op, 0);
		  ops[i].neg = !this_neg;
		  changed = 1;
		}
	      break;

	    case CONST_INT:
	      if (this_neg)
		{
		  ops[i].op = neg_const_int (mode, this_op);
		  ops[i].neg = 0;
		  changed = 1;
		}
	      break;

	    default:
	      break;
	    }
	}
    }
  while (changed);

  /* If we only have two operands, we can't do anything.  */
  if (n_ops <= 2 && !force)
    return NULL_RTX;

  /* Count the number of CONSTs we didn't split above.  */
  for (i = 0; i < n_ops; i++)
    if (GET_CODE (ops[i].op) == CONST)
      input_consts++;

  /* Now simplify each pair of operands until nothing changes.  The first
     time through just simplify constants against each other.  */

  first = 1;
  do
    {
      changed = first;

      for (i = 0; i < n_ops - 1; i++)
	for (j = i + 1; j < n_ops; j++)
	  {
	    rtx lhs = ops[i].op, rhs = ops[j].op;
	    int lneg = ops[i].neg, rneg = ops[j].neg;

	    if (lhs != 0 && rhs != 0
		&& (! first || (CONSTANT_P (lhs) && CONSTANT_P (rhs))))
	      {
		enum rtx_code ncode = PLUS;

		if (lneg != rneg)
		  {
		    ncode = MINUS;
		    if (lneg)
		      tem = lhs, lhs = rhs, rhs = tem;
		  }
		else if (swap_commutative_operands_p (lhs, rhs))
		  tem = lhs, lhs = rhs, rhs = tem;

		tem = simplify_binary_operation (ncode, mode, lhs, rhs);

		/* Reject "simplifications" that just wrap the two
		   arguments in a CONST.  Failure to do so can result
		   in infinite recursion with simplify_binary_operation
		   when it calls us to simplify CONST operations.  */
		if (tem
		    && ! (GET_CODE (tem) == CONST
			  && GET_CODE (XEXP (tem, 0)) == ncode
			  && XEXP (XEXP (tem, 0), 0) == lhs
			  && XEXP (XEXP (tem, 0), 1) == rhs)
		    /* Don't allow -x + -1 -> ~x simplifications in the
		       first pass.  This allows us the chance to combine
		       the -1 with other constants.  */
		    && ! (first
			  && GET_CODE (tem) == NOT
			  && XEXP (tem, 0) == rhs))
		  {
		    lneg &= rneg;
		    if (GET_CODE (tem) == NEG)
		      tem = XEXP (tem, 0), lneg = !lneg;
		    if (GET_CODE (tem) == CONST_INT && lneg)
		      tem = neg_const_int (mode, tem), lneg = 0;

		    ops[i].op = tem;
		    ops[i].neg = lneg;
		    ops[j].op = NULL_RTX;
		    changed = 1;
		  }
	      }
	  }

      first = 0;
    }
  while (changed);

  /* Pack all the operands to the lower-numbered entries.  */
  for (i = 0, j = 0; j < n_ops; j++)
    if (ops[j].op)
      ops[i++] = ops[j];
  n_ops = i;

  /* Sort the operations based on swap_commutative_operands_p.  */
  qsort (ops, n_ops, sizeof (*ops), simplify_plus_minus_op_data_cmp);

  /* We suppressed creation of trivial CONST expressions in the
     combination loop to avoid recursion.  Create one manually now.
     The combination loop should have ensured that there is exactly
     one CONST_INT, and the sort will have ensured that it is last
     in the array and that any other constant will be next-to-last.  */

  if (n_ops > 1
      && GET_CODE (ops[n_ops - 1].op) == CONST_INT
      && CONSTANT_P (ops[n_ops - 2].op))
    {
      rtx value = ops[n_ops - 1].op;
      if (ops[n_ops - 1].neg ^ ops[n_ops - 2].neg)
	value = neg_const_int (mode, value);
      ops[n_ops - 2].op = plus_constant (ops[n_ops - 2].op, INTVAL (value));
      n_ops--;
    }

  /* Count the number of CONSTs that we generated.  */
  n_consts = 0;
  for (i = 0; i < n_ops; i++)
    if (GET_CODE (ops[i].op) == CONST)
      n_consts++;

  /* Give up if we didn't reduce the number of operands we had.  Make
     sure we count a CONST as two operands.  If we have the same
     number of operands, but have made more CONSTs than before, this
     is also an improvement, so accept it.  */
  if (!force
      && (n_ops + n_consts > input_ops
	  || (n_ops + n_consts == input_ops && n_consts <= input_consts)))
    return NULL_RTX;

  /* Put a non-negated operand first.  If there aren't any, make all
     operands positive and negate the whole thing later.  */

  negate = 0;
  for (i = 0; i < n_ops && ops[i].neg; i++)
    continue;
  if (i == n_ops)
    {
      for (i = 0; i < n_ops; i++)
	ops[i].neg = 0;
      negate = 1;
    }
  else if (i != 0)
    {
      tem = ops[0].op;
      ops[0] = ops[i];
      ops[i].op = tem;
      ops[i].neg = 1;
    }

  /* Now make the result by performing the requested operations.  */
  result = ops[0].op;
  for (i = 1; i < n_ops; i++)
    result = gen_rtx_fmt_ee (ops[i].neg ? MINUS : PLUS,
			     mode, result, ops[i].op);

  return negate ? gen_rtx_NEG (mode, result) : result;
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
  rtx trueop0;
  rtx trueop1;

  if (mode == VOIDmode
      && (GET_MODE (op0) != VOIDmode
	  || GET_MODE (op1) != VOIDmode))
    abort ();

  /* If op0 is a compare, extract the comparison arguments from it.  */
  if (GET_CODE (op0) == COMPARE && op1 == const0_rtx)
    op1 = XEXP (op0, 1), op0 = XEXP (op0, 0);

  trueop0 = avoid_constant_pool_reference (op0);
  trueop1 = avoid_constant_pool_reference (op1);

  /* We can't simplify MODE_CC values since we don't know what the
     actual comparison is.  */
  if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_CC
#ifdef HAVE_cc0
      || op0 == cc0_rtx
#endif
      )
    return 0;

  /* Make sure the constant is second.  */
  if (swap_commutative_operands_p (trueop0, trueop1))
    {
      tem = op0, op0 = op1, op1 = tem;
      tem = trueop0, trueop0 = trueop1, trueop1 = tem;
      code = swap_condition (code);
    }

  /* For integer comparisons of A and B maybe we can simplify A - B and can
     then simplify a comparison of that with zero.  If A and B are both either
     a register or a CONST_INT, this can't help; testing for these cases will
     prevent infinite recursion here and speed things up.

     If CODE is an unsigned comparison, then we can never do this optimization,
     because it gives an incorrect result if the subtraction wraps around zero.
     ANSI C defines unsigned operations such that they never overflow, and
     thus such cases can not be ignored.  */

  if (INTEGRAL_MODE_P (mode) && trueop1 != const0_rtx
      && ! ((GET_CODE (op0) == REG || GET_CODE (trueop0) == CONST_INT)
	    && (GET_CODE (op1) == REG || GET_CODE (trueop1) == CONST_INT))
      && 0 != (tem = simplify_binary_operation (MINUS, mode, op0, op1))
      && code != GTU && code != GEU && code != LTU && code != LEU)
    return simplify_relational_operation (signed_condition (code),
					  mode, tem, const0_rtx);

  if (flag_unsafe_math_optimizations && code == ORDERED)
    return const_true_rtx;

  if (flag_unsafe_math_optimizations && code == UNORDERED)
    return const0_rtx;

  /* For modes without NaNs, if the two operands are equal, we know the
     result.  Nevertheless, don't discard them if they have side-effects.  */
  if (!HONOR_NANS (GET_MODE (trueop0))
      && rtx_equal_p (trueop0, trueop1)
      && ! side_effects_p (trueop0))
    equal = 1, op0lt = 0, op0ltu = 0, op1lt = 0, op1ltu = 0;

  /* If the operands are floating-point constants, see if we can fold
     the result.  */
  else if (GET_CODE (trueop0) == CONST_DOUBLE
	   && GET_CODE (trueop1) == CONST_DOUBLE
	   && GET_MODE_CLASS (GET_MODE (trueop0)) == MODE_FLOAT)
    {
      REAL_VALUE_TYPE d0, d1;

      REAL_VALUE_FROM_CONST_DOUBLE (d0, trueop0);
      REAL_VALUE_FROM_CONST_DOUBLE (d1, trueop1);

      /* Comparisons are unordered iff at least one of the values is NaN.  */
      if (REAL_VALUE_ISNAN (d0) || REAL_VALUE_ISNAN (d1))
	switch (code)
	  {
	  case UNEQ:
	  case UNLT:
	  case UNGT:
	  case UNLE:
	  case UNGE:
	  case NE:
	  case UNORDERED:
	    return const_true_rtx;
	  case EQ:
	  case LT:
	  case GT:
	  case LE:
	  case GE:
	  case LTGT:
	  case ORDERED:
	    return const0_rtx;
	  default:
	    return 0;
	  }

      equal = REAL_VALUES_EQUAL (d0, d1);
      op0lt = op0ltu = REAL_VALUES_LESS (d0, d1);
      op1lt = op1ltu = REAL_VALUES_LESS (d1, d0);
    }

  /* Otherwise, see if the operands are both integers.  */
  else if ((GET_MODE_CLASS (mode) == MODE_INT || mode == VOIDmode)
	   && (GET_CODE (trueop0) == CONST_DOUBLE
	       || GET_CODE (trueop0) == CONST_INT)
	   && (GET_CODE (trueop1) == CONST_DOUBLE
	       || GET_CODE (trueop1) == CONST_INT))
    {
      int width = GET_MODE_BITSIZE (mode);
      HOST_WIDE_INT l0s, h0s, l1s, h1s;
      unsigned HOST_WIDE_INT l0u, h0u, l1u, h1u;

      /* Get the two words comprising each integer constant.  */
      if (GET_CODE (trueop0) == CONST_DOUBLE)
	{
	  l0u = l0s = CONST_DOUBLE_LOW (trueop0);
	  h0u = h0s = CONST_DOUBLE_HIGH (trueop0);
	}
      else
	{
	  l0u = l0s = INTVAL (trueop0);
	  h0u = h0s = HWI_SIGN_EXTEND (l0s);
	}

      if (GET_CODE (trueop1) == CONST_DOUBLE)
	{
	  l1u = l1s = CONST_DOUBLE_LOW (trueop1);
	  h1u = h1s = CONST_DOUBLE_HIGH (trueop1);
	}
      else
	{
	  l1u = l1s = INTVAL (trueop1);
	  h1u = h1s = HWI_SIGN_EXTEND (l1s);
	}

      /* If WIDTH is nonzero and smaller than HOST_BITS_PER_WIDE_INT,
	 we have to sign or zero-extend the values.  */
      if (width != 0 && width < HOST_BITS_PER_WIDE_INT)
	{
	  l0u &= ((HOST_WIDE_INT) 1 << width) - 1;
	  l1u &= ((HOST_WIDE_INT) 1 << width) - 1;

	  if (l0s & ((HOST_WIDE_INT) 1 << (width - 1)))
	    l0s |= ((HOST_WIDE_INT) (-1) << width);

	  if (l1s & ((HOST_WIDE_INT) 1 << (width - 1)))
	    l1s |= ((HOST_WIDE_INT) (-1) << width);
	}
      if (width != 0 && width <= HOST_BITS_PER_WIDE_INT)
	h0u = h1u = 0, h0s = HWI_SIGN_EXTEND (l0s), h1s = HWI_SIGN_EXTEND (l1s);

      equal = (h0u == h1u && l0u == l1u);
      op0lt = (h0s < h1s || (h0s == h1s && l0u < l1u));
      op1lt = (h1s < h0s || (h1s == h0s && l1u < l0u));
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
	  if (((NONZERO_BASE_PLUS_P (op0) && trueop1 == const0_rtx)
	       || GET_CODE (trueop0) == LABEL_REF)
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	      /* On some machines, the ap reg can be 0 sometimes.  */
	      && op0 != arg_pointer_rtx
#endif
		)
	    return const0_rtx;
	  break;

	case NE:
	  if (((NONZERO_BASE_PLUS_P (op0) && trueop1 == const0_rtx)
	       || GET_CODE (trueop0) == LABEL_REF)
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	      && op0 != arg_pointer_rtx
#endif
	      )
	    return const_true_rtx;
	  break;

	case GEU:
	  /* Unsigned values are never negative.  */
	  if (trueop1 == const0_rtx)
	    return const_true_rtx;
	  break;

	case LTU:
	  if (trueop1 == const0_rtx)
	    return const0_rtx;
	  break;

	case LEU:
	  /* Unsigned values are never greater than the largest
	     unsigned value.  */
	  if (GET_CODE (trueop1) == CONST_INT
	      && (unsigned HOST_WIDE_INT) INTVAL (trueop1) == GET_MODE_MASK (mode)
	    && INTEGRAL_MODE_P (mode))
	  return const_true_rtx;
	  break;

	case GTU:
	  if (GET_CODE (trueop1) == CONST_INT
	      && (unsigned HOST_WIDE_INT) INTVAL (trueop1) == GET_MODE_MASK (mode)
	      && INTEGRAL_MODE_P (mode))
	    return const0_rtx;
	  break;

	case LT:
	  /* Optimize abs(x) < 0.0.  */
	  if (trueop1 == CONST0_RTX (mode) && !HONOR_SNANS (mode))
	    {
	      tem = GET_CODE (trueop0) == FLOAT_EXTEND ? XEXP (trueop0, 0)
						       : trueop0;
	      if (GET_CODE (tem) == ABS)
		return const0_rtx;
	    }
	  break;

	case GE:
	  /* Optimize abs(x) >= 0.0.  */
	  if (trueop1 == CONST0_RTX (mode) && !HONOR_NANS (mode))
	    {
	      tem = GET_CODE (trueop0) == FLOAT_EXTEND ? XEXP (trueop0, 0)
						       : trueop0;
	      if (GET_CODE (tem) == ABS)
		return const1_rtx;
	    }
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
    case UNEQ:
      return equal ? const_true_rtx : const0_rtx;
    case NE:
    case LTGT:
      return ! equal ? const_true_rtx : const0_rtx;
    case LT:
    case UNLT:
      return op0lt ? const_true_rtx : const0_rtx;
    case GT:
    case UNGT:
      return op1lt ? const_true_rtx : const0_rtx;
    case LTU:
      return op0ltu ? const_true_rtx : const0_rtx;
    case GTU:
      return op1ltu ? const_true_rtx : const0_rtx;
    case LE:
    case UNLE:
      return equal || op0lt ? const_true_rtx : const0_rtx;
    case GE:
    case UNGE:
      return equal || op1lt ? const_true_rtx : const0_rtx;
    case LEU:
      return equal || op0ltu ? const_true_rtx : const0_rtx;
    case GEU:
      return equal || op1ltu ? const_true_rtx : const0_rtx;
    case ORDERED:
      return const_true_rtx;
    case UNORDERED:
      return const0_rtx;
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
  unsigned int width = GET_MODE_BITSIZE (mode);

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
	  && ((unsigned) INTVAL (op1) + (unsigned) INTVAL (op2) <= width)
	  && width <= (unsigned) HOST_BITS_PER_WIDE_INT)
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
	  && !HONOR_NANS (mode)
	  && rtx_equal_p (XEXP (op0, 0), op1)
	  && rtx_equal_p (XEXP (op0, 1), op2))
	return op1;
      else if (GET_CODE (op0) == EQ && ! side_effects_p (op0)
	  && !HONOR_NANS (mode)
	  && rtx_equal_p (XEXP (op0, 1), op1)
	  && rtx_equal_p (XEXP (op0, 0), op2))
	return op2;
      else if (GET_RTX_CLASS (GET_CODE (op0)) == '<' && ! side_effects_p (op0))
	{
	  enum machine_mode cmp_mode = (GET_MODE (XEXP (op0, 0)) == VOIDmode
					? GET_MODE (XEXP (op0, 1))
					: GET_MODE (XEXP (op0, 0)));
	  rtx temp;
	  if (cmp_mode == VOIDmode)
	    cmp_mode = op0_mode;
	  temp = simplify_relational_operation (GET_CODE (op0), cmp_mode,
					        XEXP (op0, 0), XEXP (op0, 1));

	  /* See if any simplifications were possible.  */
	  if (temp == const0_rtx)
	    return op2;
	  else if (temp == const1_rtx)
	    return op1;
	  else if (temp)
	    op0 = temp;

	  /* Look for happy constants in op1 and op2.  */
	  if (GET_CODE (op1) == CONST_INT && GET_CODE (op2) == CONST_INT)
	    {
	      HOST_WIDE_INT t = INTVAL (op1);
	      HOST_WIDE_INT f = INTVAL (op2);

	      if (t == STORE_FLAG_VALUE && f == 0)
	        code = GET_CODE (op0);
	      else if (t == 0 && f == STORE_FLAG_VALUE)
		{
		  enum rtx_code tmp;
		  tmp = reversed_comparison_code (op0, NULL_RTX);
		  if (tmp == UNKNOWN)
		    break;
		  code = tmp;
		}
	      else
		break;

	      return gen_rtx_fmt_ee (code, mode, XEXP (op0, 0), XEXP (op0, 1));
	    }
	}
      break;
    case VEC_MERGE:
      if (GET_MODE (op0) != mode
	  || GET_MODE (op1) != mode
	  || !VECTOR_MODE_P (mode))
	abort ();
      op0 = avoid_constant_pool_reference (op0);
      op1 = avoid_constant_pool_reference (op1);
      op2 = avoid_constant_pool_reference (op2);
      if (GET_CODE (op0) == CONST_VECTOR
	  && GET_CODE (op1) == CONST_VECTOR
	  && GET_CODE (op2) == CONST_INT)
	{
          int elt_size = GET_MODE_SIZE (GET_MODE_INNER (mode));
	  unsigned n_elts = (GET_MODE_SIZE (mode) / elt_size);
	  rtvec v = rtvec_alloc (n_elts);
	  unsigned int i;

	  for (i = 0; i < n_elts; i++)
	    RTVEC_ELT (v, i) = (INTVAL (op2) & (1 << i)
				? CONST_VECTOR_ELT (op0, i)
				: CONST_VECTOR_ELT (op1, i));
	  return gen_rtx_CONST_VECTOR (mode, v);
	}
      break;

    default:
      abort ();
    }

  return 0;
}

/* Simplify SUBREG:OUTERMODE(OP:INNERMODE, BYTE)
   Return 0 if no simplifications is possible.  */
rtx
simplify_subreg (outermode, op, innermode, byte)
     rtx op;
     unsigned int byte;
     enum machine_mode outermode, innermode;
{
  /* Little bit of sanity checking.  */
  if (innermode == VOIDmode || outermode == VOIDmode
      || innermode == BLKmode || outermode == BLKmode)
    abort ();

  if (GET_MODE (op) != innermode
      && GET_MODE (op) != VOIDmode)
    abort ();

  if (byte % GET_MODE_SIZE (outermode)
      || byte >= GET_MODE_SIZE (innermode))
    abort ();

  if (outermode == innermode && !byte)
    return op;

  /* Simplify subregs of vector constants.  */
  if (GET_CODE (op) == CONST_VECTOR)
    {
      int elt_size = GET_MODE_SIZE (GET_MODE_INNER (innermode));
      const unsigned int offset = byte / elt_size;
      rtx elt;

      if (GET_MODE_INNER (innermode) == outermode)
	{
	  elt = CONST_VECTOR_ELT (op, offset);

	  /* ?? We probably don't need this copy_rtx because constants
	     can be shared.  ?? */

	  return copy_rtx (elt);
	}
      else if (GET_MODE_INNER (innermode) == GET_MODE_INNER (outermode)
	       && GET_MODE_SIZE (innermode) > GET_MODE_SIZE (outermode))
	{
	  return (gen_rtx_CONST_VECTOR
		  (outermode,
		   gen_rtvec_v (GET_MODE_NUNITS (outermode),
				&CONST_VECTOR_ELT (op, offset))));
	}
      else if (GET_MODE_CLASS (outermode) == MODE_INT
	       && (GET_MODE_SIZE (outermode) % elt_size == 0))
	{
	  /* This happens when the target register size is smaller then
	     the vector mode, and we synthesize operations with vectors
	     of elements that are smaller than the register size.  */
	  HOST_WIDE_INT sum = 0, high = 0;
	  unsigned n_elts = (GET_MODE_SIZE (outermode) / elt_size);
	  unsigned i = BYTES_BIG_ENDIAN ? offset : offset + n_elts - 1;
	  unsigned step = BYTES_BIG_ENDIAN ? 1 : -1;
	  int shift = BITS_PER_UNIT * elt_size;

	  for (; n_elts--; i += step)
	    {
	      elt = CONST_VECTOR_ELT (op, i);
	      if (GET_CODE (elt) == CONST_DOUBLE
		  && GET_MODE_CLASS (GET_MODE (elt)) == MODE_FLOAT)
		{
		  elt = gen_lowpart_common (int_mode_for_mode (GET_MODE (elt)),
					    elt);
		  if (! elt)
		    return NULL_RTX;
		}
	      if (GET_CODE (elt) != CONST_INT)
		return NULL_RTX;
	      /* Avoid overflow.  */
	      if (high >> (HOST_BITS_PER_WIDE_INT - shift))
		return NULL_RTX;
	      high = high << shift | sum >> (HOST_BITS_PER_WIDE_INT - shift);
	      sum = (sum << shift) + INTVAL (elt);
	    }
	  if (GET_MODE_BITSIZE (outermode) <= HOST_BITS_PER_WIDE_INT)
	    return GEN_INT (trunc_int_for_mode (sum, outermode));
	  else if (GET_MODE_BITSIZE (outermode) == 2* HOST_BITS_PER_WIDE_INT)
	    return immed_double_const (sum, high, outermode);
	  else
	    return NULL_RTX;
	}
      else if (GET_MODE_CLASS (outermode) == MODE_INT
	       && (elt_size % GET_MODE_SIZE (outermode) == 0))
	{
	  enum machine_mode new_mode
	    = int_mode_for_mode (GET_MODE_INNER (innermode));
	  int subbyte = byte % elt_size;

	  op = simplify_subreg (new_mode, op, innermode, byte - subbyte);
	    if (! op)
	      return NULL_RTX;
	  return simplify_subreg (outermode, op, new_mode, subbyte);
	}
      else if (GET_MODE_CLASS (outermode) == MODE_INT)
        /* This shouldn't happen, but let's not do anything stupid.  */
	return NULL_RTX;
    }

  /* Attempt to simplify constant to non-SUBREG expression.  */
  if (CONSTANT_P (op))
    {
      int offset, part;
      unsigned HOST_WIDE_INT val = 0;

      if (GET_MODE_CLASS (outermode) == MODE_VECTOR_INT
	  || GET_MODE_CLASS (outermode) == MODE_VECTOR_FLOAT)
	{
	  /* Construct a CONST_VECTOR from individual subregs.  */
	  enum machine_mode submode = GET_MODE_INNER (outermode);
	  int subsize = GET_MODE_UNIT_SIZE (outermode);
	  int i, elts = GET_MODE_NUNITS (outermode);
	  rtvec v = rtvec_alloc (elts);
	  rtx elt;

	  for (i = 0; i < elts; i++, byte += subsize)
	    {
	      /* This might fail, e.g. if taking a subreg from a SYMBOL_REF.  */
	      /* ??? It would be nice if we could actually make such subregs
		 on targets that allow such relocations.  */
	      if (byte >= GET_MODE_UNIT_SIZE (innermode))
		elt = CONST0_RTX (submode);
	      else
	        elt = simplify_subreg (submode, op, innermode, byte);
	      if (! elt)
		return NULL_RTX;
	      RTVEC_ELT (v, i) = elt;
	    }
	  return gen_rtx_CONST_VECTOR (outermode, v);
	}

      /* ??? This code is partly redundant with code below, but can handle
	 the subregs of floats and similar corner cases.
	 Later it we should move all simplification code here and rewrite
	 GEN_LOWPART_IF_POSSIBLE, GEN_HIGHPART, OPERAND_SUBWORD and friends
	 using SIMPLIFY_SUBREG.  */
      if (subreg_lowpart_offset (outermode, innermode) == byte
	  && GET_CODE (op) != CONST_VECTOR)
	{
	  rtx new = gen_lowpart_if_possible (outermode, op);
	  if (new)
	    return new;
	}

      /* Similar comment as above apply here.  */
      if (GET_MODE_SIZE (outermode) == UNITS_PER_WORD
	  && GET_MODE_SIZE (innermode) > UNITS_PER_WORD
	  && GET_MODE_CLASS (outermode) == MODE_INT)
	{
	  rtx new = constant_subword (op,
				      (byte / UNITS_PER_WORD),
				      innermode);
	  if (new)
	    return new;
	}

      if (GET_MODE_CLASS (outermode) != MODE_INT
	  && GET_MODE_CLASS (outermode) != MODE_CC)
	{
	  enum machine_mode new_mode = int_mode_for_mode (outermode);

	  if (new_mode != innermode || byte != 0)
	    {
	      op = simplify_subreg (new_mode, op, innermode, byte);
	      if (! op)
		return NULL_RTX;
	      return simplify_subreg (outermode, op, new_mode, 0);
	    }
	}

      offset = byte * BITS_PER_UNIT;
      switch (GET_CODE (op))
	{
	case CONST_DOUBLE:
	  if (GET_MODE (op) != VOIDmode)
	    break;

	  /* We can't handle this case yet.  */
	  if (GET_MODE_BITSIZE (outermode) >= HOST_BITS_PER_WIDE_INT)
	    return NULL_RTX;

	  part = offset >= HOST_BITS_PER_WIDE_INT;
	  if ((BITS_PER_WORD > HOST_BITS_PER_WIDE_INT
	       && BYTES_BIG_ENDIAN)
	      || (BITS_PER_WORD <= HOST_BITS_PER_WIDE_INT
		  && WORDS_BIG_ENDIAN))
	    part = !part;
	  val = part ? CONST_DOUBLE_HIGH (op) : CONST_DOUBLE_LOW (op);
	  offset %= HOST_BITS_PER_WIDE_INT;

	  /* We've already picked the word we want from a double, so
	     pretend this is actually an integer.  */
	  innermode = mode_for_size (HOST_BITS_PER_WIDE_INT, MODE_INT, 0);

	  /* FALLTHROUGH */
	case CONST_INT:
	  if (GET_CODE (op) == CONST_INT)
	    val = INTVAL (op);

	  /* We don't handle synthetizing of non-integral constants yet.  */
	  if (GET_MODE_CLASS (outermode) != MODE_INT)
	    return NULL_RTX;

	  if (BYTES_BIG_ENDIAN || WORDS_BIG_ENDIAN)
	    {
	      if (WORDS_BIG_ENDIAN)
		offset = (GET_MODE_BITSIZE (innermode)
			  - GET_MODE_BITSIZE (outermode) - offset);
	      if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN
		  && GET_MODE_SIZE (outermode) < UNITS_PER_WORD)
		offset = (offset + BITS_PER_WORD - GET_MODE_BITSIZE (outermode)
			  - 2 * (offset % BITS_PER_WORD));
	    }

	  if (offset >= HOST_BITS_PER_WIDE_INT)
	    return ((HOST_WIDE_INT) val < 0) ? constm1_rtx : const0_rtx;
	  else
	    {
	      val >>= offset;
	      if (GET_MODE_BITSIZE (outermode) < HOST_BITS_PER_WIDE_INT)
		val = trunc_int_for_mode (val, outermode);
	      return GEN_INT (val);
	    }
	default:
	  break;
	}
    }

  /* Changing mode twice with SUBREG => just change it once,
     or not at all if changing back op starting mode.  */
  if (GET_CODE (op) == SUBREG)
    {
      enum machine_mode innermostmode = GET_MODE (SUBREG_REG (op));
      int final_offset = byte + SUBREG_BYTE (op);
      rtx new;

      if (outermode == innermostmode
	  && byte == 0 && SUBREG_BYTE (op) == 0)
	return SUBREG_REG (op);

      /* The SUBREG_BYTE represents offset, as if the value were stored
	 in memory.  Irritating exception is paradoxical subreg, where
	 we define SUBREG_BYTE to be 0.  On big endian machines, this
	 value should be negative.  For a moment, undo this exception.  */
      if (byte == 0 && GET_MODE_SIZE (innermode) < GET_MODE_SIZE (outermode))
	{
	  int difference = (GET_MODE_SIZE (innermode) - GET_MODE_SIZE (outermode));
	  if (WORDS_BIG_ENDIAN)
	    final_offset += (difference / UNITS_PER_WORD) * UNITS_PER_WORD;
	  if (BYTES_BIG_ENDIAN)
	    final_offset += difference % UNITS_PER_WORD;
	}
      if (SUBREG_BYTE (op) == 0
	  && GET_MODE_SIZE (innermostmode) < GET_MODE_SIZE (innermode))
	{
	  int difference = (GET_MODE_SIZE (innermostmode) - GET_MODE_SIZE (innermode));
	  if (WORDS_BIG_ENDIAN)
	    final_offset += (difference / UNITS_PER_WORD) * UNITS_PER_WORD;
	  if (BYTES_BIG_ENDIAN)
	    final_offset += difference % UNITS_PER_WORD;
	}

      /* See whether resulting subreg will be paradoxical.  */
      if (GET_MODE_SIZE (innermostmode) > GET_MODE_SIZE (outermode))
	{
	  /* In nonparadoxical subregs we can't handle negative offsets.  */
	  if (final_offset < 0)
	    return NULL_RTX;
	  /* Bail out in case resulting subreg would be incorrect.  */
	  if (final_offset % GET_MODE_SIZE (outermode)
	      || (unsigned) final_offset >= GET_MODE_SIZE (innermostmode))
	    return NULL_RTX;
	}
      else
	{
	  int offset = 0;
	  int difference = (GET_MODE_SIZE (innermostmode) - GET_MODE_SIZE (outermode));

	  /* In paradoxical subreg, see if we are still looking on lower part.
	     If so, our SUBREG_BYTE will be 0.  */
	  if (WORDS_BIG_ENDIAN)
	    offset += (difference / UNITS_PER_WORD) * UNITS_PER_WORD;
	  if (BYTES_BIG_ENDIAN)
	    offset += difference % UNITS_PER_WORD;
	  if (offset == final_offset)
	    final_offset = 0;
	  else
	    return NULL_RTX;
	}

      /* Recurse for futher possible simplifications.  */
      new = simplify_subreg (outermode, SUBREG_REG (op),
			     GET_MODE (SUBREG_REG (op)),
			     final_offset);
      if (new)
	return new;
      return gen_rtx_SUBREG (outermode, SUBREG_REG (op), final_offset);
    }

  /* SUBREG of a hard register => just change the register number
     and/or mode.  If the hard register is not valid in that mode,
     suppress this simplification.  If the hard register is the stack,
     frame, or argument pointer, leave this as a SUBREG.  */

  if (REG_P (op)
      && (! REG_FUNCTION_VALUE_P (op)
	  || ! rtx_equal_function_value_matters)
      && REGNO (op) < FIRST_PSEUDO_REGISTER
#ifdef CANNOT_CHANGE_MODE_CLASS
      && ! (REG_CANNOT_CHANGE_MODE_P (REGNO (op), innermode, outermode)
	    && GET_MODE_CLASS (innermode) != MODE_COMPLEX_INT
	    && GET_MODE_CLASS (innermode) != MODE_COMPLEX_FLOAT)
#endif
      && ((reload_completed && !frame_pointer_needed)
	  || (REGNO (op) != FRAME_POINTER_REGNUM
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
	      && REGNO (op) != HARD_FRAME_POINTER_REGNUM
#endif
	     ))
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
      && REGNO (op) != ARG_POINTER_REGNUM
#endif
      && REGNO (op) != STACK_POINTER_REGNUM)
    {
      int final_regno = subreg_hard_regno (gen_rtx_SUBREG (outermode, op, byte),
					   0);

      /* ??? We do allow it if the current REG is not valid for
	 its mode.  This is a kludge to work around how float/complex
	 arguments are passed on 32-bit SPARC and should be fixed.  */
      if (HARD_REGNO_MODE_OK (final_regno, outermode)
	  || ! HARD_REGNO_MODE_OK (REGNO (op), innermode))
	{
	  rtx x = gen_rtx_REG (outermode, final_regno);

	  /* Propagate original regno.  We don't have any way to specify
	     the offset inside orignal regno, so do so only for lowpart.
	     The information is used only by alias analysis that can not
	     grog partial register anyway.  */

	  if (subreg_lowpart_offset (outermode, innermode) == byte)
	    ORIGINAL_REGNO (x) = ORIGINAL_REGNO (op);
	  return x;
	}
    }

  /* If we have a SUBREG of a register that we are replacing and we are
     replacing it with a MEM, make a new MEM and try replacing the
     SUBREG with it.  Don't do this if the MEM has a mode-dependent address
     or if we would be widening it.  */

  if (GET_CODE (op) == MEM
      && ! mode_dependent_address_p (XEXP (op, 0))
      /* Allow splitting of volatile memory references in case we don't
         have instruction to move the whole thing.  */
      && (! MEM_VOLATILE_P (op)
	  || ! have_insn_for (SET, innermode))
      && GET_MODE_SIZE (outermode) <= GET_MODE_SIZE (GET_MODE (op)))
    return adjust_address_nv (op, outermode, byte);

  /* Handle complex values represented as CONCAT
     of real and imaginary part.  */
  if (GET_CODE (op) == CONCAT)
    {
      int is_realpart = byte < GET_MODE_UNIT_SIZE (innermode);
      rtx part = is_realpart ? XEXP (op, 0) : XEXP (op, 1);
      unsigned int final_offset;
      rtx res;

      final_offset = byte % (GET_MODE_UNIT_SIZE (innermode));
      res = simplify_subreg (outermode, part, GET_MODE (part), final_offset);
      if (res)
	return res;
      /* We can at least simplify it by referring directly to the relevant part.  */
      return gen_rtx_SUBREG (outermode, part, final_offset);
    }

  return NULL_RTX;
}
/* Make a SUBREG operation or equivalent if it folds.  */

rtx
simplify_gen_subreg (outermode, op, innermode, byte)
     rtx op;
     unsigned int byte;
     enum machine_mode outermode, innermode;
{
  rtx new;
  /* Little bit of sanity checking.  */
  if (innermode == VOIDmode || outermode == VOIDmode
      || innermode == BLKmode || outermode == BLKmode)
    abort ();

  if (GET_MODE (op) != innermode
      && GET_MODE (op) != VOIDmode)
    abort ();

  if (byte % GET_MODE_SIZE (outermode)
      || byte >= GET_MODE_SIZE (innermode))
    abort ();

  if (GET_CODE (op) == QUEUED)
    return NULL_RTX;

  new = simplify_subreg (outermode, op, innermode, byte);
  if (new)
    return new;

  if (GET_CODE (op) == SUBREG || GET_MODE (op) == VOIDmode)
    return NULL_RTX;

  return gen_rtx_SUBREG (outermode, op, byte);
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
  enum rtx_code code = GET_CODE (x);
  enum machine_mode mode = GET_MODE (x);

  switch (GET_RTX_CLASS (code))
    {
    case '1':
      return simplify_unary_operation (code, mode,
				       XEXP (x, 0), GET_MODE (XEXP (x, 0)));
    case 'c':
      if (swap_commutative_operands_p (XEXP (x, 0), XEXP (x, 1)))
	{
	  rtx tem;

	  tem = XEXP (x, 0);
	  XEXP (x, 0) = XEXP (x, 1);
	  XEXP (x, 1) = tem;
	  return simplify_binary_operation (code, mode,
					    XEXP (x, 0), XEXP (x, 1));
	}

    case '2':
      return simplify_binary_operation (code, mode, XEXP (x, 0), XEXP (x, 1));

    case '3':
    case 'b':
      return simplify_ternary_operation (code, mode, GET_MODE (XEXP (x, 0)),
					 XEXP (x, 0), XEXP (x, 1),
					 XEXP (x, 2));

    case '<':
      return simplify_relational_operation (code,
					    ((GET_MODE (XEXP (x, 0))
					      != VOIDmode)
					     ? GET_MODE (XEXP (x, 0))
					     : GET_MODE (XEXP (x, 1))),
					    XEXP (x, 0), XEXP (x, 1));
    case 'x':
      /* The only case we try to handle is a SUBREG.  */
      if (code == SUBREG)
	return simplify_gen_subreg (mode, SUBREG_REG (x),
				    GET_MODE (SUBREG_REG (x)),
				    SUBREG_BYTE (x));
      return NULL;
    default:
      return NULL;
    }
}
