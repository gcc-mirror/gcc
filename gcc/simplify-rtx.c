/* RTL simplification functions for GNU compiler.
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
#include "predict.h"
#include "memmodel.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "varasm.h"
#include "flags.h"

/* Simplification and canonicalization of RTL.  */

/* Much code operates on (low, high) pairs; the low value is an
   unsigned wide int, the high value a signed wide int.  We
   occasionally need to sign extend from low to high as if low were a
   signed wide int.  */
#define HWI_SIGN_EXTEND(low) \
  ((((HOST_WIDE_INT) low) < 0) ? HOST_WIDE_INT_M1 : HOST_WIDE_INT_0)

static rtx neg_const_int (machine_mode, const_rtx);
static bool plus_minus_operand_p (const_rtx);
static rtx simplify_plus_minus (enum rtx_code, machine_mode, rtx, rtx);
static rtx simplify_immed_subreg (machine_mode, rtx, machine_mode,
				  unsigned int);
static rtx simplify_associative_operation (enum rtx_code, machine_mode,
					   rtx, rtx);
static rtx simplify_relational_operation_1 (enum rtx_code, machine_mode,
					    machine_mode, rtx, rtx);
static rtx simplify_unary_operation_1 (enum rtx_code, machine_mode, rtx);
static rtx simplify_binary_operation_1 (enum rtx_code, machine_mode,
					rtx, rtx, rtx, rtx);

/* Negate a CONST_INT rtx.  */
static rtx
neg_const_int (machine_mode mode, const_rtx i)
{
  unsigned HOST_WIDE_INT val = -UINTVAL (i);
  
  if (!HWI_COMPUTABLE_MODE_P (mode)
      && val == UINTVAL (i))
    return simplify_const_unary_operation (NEG, mode, CONST_CAST_RTX (i),
					   mode);
  return gen_int_mode (val, mode);
}

/* Test whether expression, X, is an immediate constant that represents
   the most significant bit of machine mode MODE.  */

bool
mode_signbit_p (machine_mode mode, const_rtx x)
{
  unsigned HOST_WIDE_INT val;
  unsigned int width;
  scalar_int_mode int_mode;

  if (!is_int_mode (mode, &int_mode))
    return false;

  width = GET_MODE_PRECISION (int_mode);
  if (width == 0)
    return false;

  if (width <= HOST_BITS_PER_WIDE_INT
      && CONST_INT_P (x))
    val = INTVAL (x);
#if TARGET_SUPPORTS_WIDE_INT
  else if (CONST_WIDE_INT_P (x))
    {
      unsigned int i;
      unsigned int elts = CONST_WIDE_INT_NUNITS (x);
      if (elts != (width + HOST_BITS_PER_WIDE_INT - 1) / HOST_BITS_PER_WIDE_INT)
	return false;
      for (i = 0; i < elts - 1; i++)
	if (CONST_WIDE_INT_ELT (x, i) != 0)
	  return false;
      val = CONST_WIDE_INT_ELT (x, elts - 1);
      width %= HOST_BITS_PER_WIDE_INT;
      if (width == 0)
	width = HOST_BITS_PER_WIDE_INT;
    }
#else
  else if (width <= HOST_BITS_PER_DOUBLE_INT
	   && CONST_DOUBLE_AS_INT_P (x)
	   && CONST_DOUBLE_LOW (x) == 0)
    {
      val = CONST_DOUBLE_HIGH (x);
      width -= HOST_BITS_PER_WIDE_INT;
    }
#endif
  else
    /* X is not an integer constant.  */
    return false;

  if (width < HOST_BITS_PER_WIDE_INT)
    val &= (HOST_WIDE_INT_1U << width) - 1;
  return val == (HOST_WIDE_INT_1U << (width - 1));
}

/* Test whether VAL is equal to the most significant bit of mode MODE
   (after masking with the mode mask of MODE).  Returns false if the
   precision of MODE is too large to handle.  */

bool
val_signbit_p (machine_mode mode, unsigned HOST_WIDE_INT val)
{
  unsigned int width;
  scalar_int_mode int_mode;

  if (!is_int_mode (mode, &int_mode))
    return false;

  width = GET_MODE_PRECISION (int_mode);
  if (width == 0 || width > HOST_BITS_PER_WIDE_INT)
    return false;

  val &= GET_MODE_MASK (int_mode);
  return val == (HOST_WIDE_INT_1U << (width - 1));
}

/* Test whether the most significant bit of mode MODE is set in VAL.
   Returns false if the precision of MODE is too large to handle.  */
bool
val_signbit_known_set_p (machine_mode mode, unsigned HOST_WIDE_INT val)
{
  unsigned int width;

  scalar_int_mode int_mode;
  if (!is_int_mode (mode, &int_mode))
    return false;

  width = GET_MODE_PRECISION (int_mode);
  if (width == 0 || width > HOST_BITS_PER_WIDE_INT)
    return false;

  val &= HOST_WIDE_INT_1U << (width - 1);
  return val != 0;
}

/* Test whether the most significant bit of mode MODE is clear in VAL.
   Returns false if the precision of MODE is too large to handle.  */
bool
val_signbit_known_clear_p (machine_mode mode, unsigned HOST_WIDE_INT val)
{
  unsigned int width;

  scalar_int_mode int_mode;
  if (!is_int_mode (mode, &int_mode))
    return false;

  width = GET_MODE_PRECISION (int_mode);
  if (width == 0 || width > HOST_BITS_PER_WIDE_INT)
    return false;

  val &= HOST_WIDE_INT_1U << (width - 1);
  return val == 0;
}

/* Make a binary operation by properly ordering the operands and
   seeing if the expression folds.  */

rtx
simplify_gen_binary (enum rtx_code code, machine_mode mode, rtx op0,
		     rtx op1)
{
  rtx tem;

  /* If this simplifies, do it.  */
  tem = simplify_binary_operation (code, mode, op0, op1);
  if (tem)
    return tem;

  /* Put complex operands first and constants second if commutative.  */
  if (GET_RTX_CLASS (code) == RTX_COMM_ARITH
      && swap_commutative_operands_p (op0, op1))
    std::swap (op0, op1);

  return gen_rtx_fmt_ee (code, mode, op0, op1);
}

/* If X is a MEM referencing the constant pool, return the real value.
   Otherwise return X.  */
rtx
avoid_constant_pool_reference (rtx x)
{
  rtx c, tmp, addr;
  machine_mode cmode;
  HOST_WIDE_INT offset = 0;

  switch (GET_CODE (x))
    {
    case MEM:
      break;

    case FLOAT_EXTEND:
      /* Handle float extensions of constant pool references.  */
      tmp = XEXP (x, 0);
      c = avoid_constant_pool_reference (tmp);
      if (c != tmp && CONST_DOUBLE_AS_FLOAT_P (c))
	return const_double_from_real_value (*CONST_DOUBLE_REAL_VALUE (c),
					     GET_MODE (x));
      return x;

    default:
      return x;
    }

  if (GET_MODE (x) == BLKmode)
    return x;

  addr = XEXP (x, 0);

  /* Call target hook to avoid the effects of -fpic etc....  */
  addr = targetm.delegitimize_address (addr);

  /* Split the address into a base and integer offset.  */
  if (GET_CODE (addr) == CONST
      && GET_CODE (XEXP (addr, 0)) == PLUS
      && CONST_INT_P (XEXP (XEXP (addr, 0), 1)))
    {
      offset = INTVAL (XEXP (XEXP (addr, 0), 1));
      addr = XEXP (XEXP (addr, 0), 0);
    }

  if (GET_CODE (addr) == LO_SUM)
    addr = XEXP (addr, 1);

  /* If this is a constant pool reference, we can turn it into its
     constant and hope that simplifications happen.  */
  if (GET_CODE (addr) == SYMBOL_REF
      && CONSTANT_POOL_ADDRESS_P (addr))
    {
      c = get_pool_constant (addr);
      cmode = get_pool_mode (addr);

      /* If we're accessing the constant in a different mode than it was
         originally stored, attempt to fix that up via subreg simplifications.
         If that fails we have no choice but to return the original memory.  */
      if (offset == 0 && cmode == GET_MODE (x))
	return c;
      else if (offset >= 0 && offset < GET_MODE_SIZE (cmode))
        {
          rtx tem = simplify_subreg (GET_MODE (x), c, cmode, offset);
          if (tem && CONSTANT_P (tem))
            return tem;
        }
    }

  return x;
}

/* Simplify a MEM based on its attributes.  This is the default
   delegitimize_address target hook, and it's recommended that every
   overrider call it.  */

rtx
delegitimize_mem_from_attrs (rtx x)
{
  /* MEMs without MEM_OFFSETs may have been offset, so we can't just
     use their base addresses as equivalent.  */
  if (MEM_P (x)
      && MEM_EXPR (x)
      && MEM_OFFSET_KNOWN_P (x))
    {
      tree decl = MEM_EXPR (x);
      machine_mode mode = GET_MODE (x);
      HOST_WIDE_INT offset = 0;

      switch (TREE_CODE (decl))
	{
	default:
	  decl = NULL;
	  break;

	case VAR_DECL:
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	case COMPONENT_REF:
	case BIT_FIELD_REF:
	case REALPART_EXPR:
	case IMAGPART_EXPR:
	case VIEW_CONVERT_EXPR:
	  {
	    HOST_WIDE_INT bitsize, bitpos;
	    tree toffset;
	    int unsignedp, reversep, volatilep = 0;

	    decl
	      = get_inner_reference (decl, &bitsize, &bitpos, &toffset, &mode,
				     &unsignedp, &reversep, &volatilep);
	    if (bitsize != GET_MODE_BITSIZE (mode)
		|| (bitpos % BITS_PER_UNIT)
		|| (toffset && !tree_fits_shwi_p (toffset)))
	      decl = NULL;
	    else
	      {
		offset += bitpos / BITS_PER_UNIT;
		if (toffset)
		  offset += tree_to_shwi (toffset);
	      }
	    break;
	  }
	}

      if (decl
	  && mode == GET_MODE (x)
	  && VAR_P (decl)
	  && (TREE_STATIC (decl)
	      || DECL_THREAD_LOCAL_P (decl))
	  && DECL_RTL_SET_P (decl)
	  && MEM_P (DECL_RTL (decl)))
	{
	  rtx newx;

	  offset += MEM_OFFSET (x);

	  newx = DECL_RTL (decl);

	  if (MEM_P (newx))
	    {
	      rtx n = XEXP (newx, 0), o = XEXP (x, 0);

	      /* Avoid creating a new MEM needlessly if we already had
		 the same address.  We do if there's no OFFSET and the
		 old address X is identical to NEWX, or if X is of the
		 form (plus NEWX OFFSET), or the NEWX is of the form
		 (plus Y (const_int Z)) and X is that with the offset
		 added: (plus Y (const_int Z+OFFSET)).  */
	      if (!((offset == 0
		     || (GET_CODE (o) == PLUS
			 && GET_CODE (XEXP (o, 1)) == CONST_INT
			 && (offset == INTVAL (XEXP (o, 1))
			     || (GET_CODE (n) == PLUS
				 && GET_CODE (XEXP (n, 1)) == CONST_INT
				 && (INTVAL (XEXP (n, 1)) + offset
				     == INTVAL (XEXP (o, 1)))
				 && (n = XEXP (n, 0))))
			 && (o = XEXP (o, 0))))
		    && rtx_equal_p (o, n)))
		x = adjust_address_nv (newx, mode, offset);
	    }
	  else if (GET_MODE (x) == GET_MODE (newx)
		   && offset == 0)
	    x = newx;
	}
    }

  return x;
}

/* Make a unary operation by first seeing if it folds and otherwise making
   the specified operation.  */

rtx
simplify_gen_unary (enum rtx_code code, machine_mode mode, rtx op,
		    machine_mode op_mode)
{
  rtx tem;

  /* If this simplifies, use it.  */
  if ((tem = simplify_unary_operation (code, mode, op, op_mode)) != 0)
    return tem;

  return gen_rtx_fmt_e (code, mode, op);
}

/* Likewise for ternary operations.  */

rtx
simplify_gen_ternary (enum rtx_code code, machine_mode mode,
		      machine_mode op0_mode, rtx op0, rtx op1, rtx op2)
{
  rtx tem;

  /* If this simplifies, use it.  */
  if (0 != (tem = simplify_ternary_operation (code, mode, op0_mode,
					      op0, op1, op2)))
    return tem;

  return gen_rtx_fmt_eee (code, mode, op0, op1, op2);
}

/* Likewise, for relational operations.
   CMP_MODE specifies mode comparison is done in.  */

rtx
simplify_gen_relational (enum rtx_code code, machine_mode mode,
			 machine_mode cmp_mode, rtx op0, rtx op1)
{
  rtx tem;

  if (0 != (tem = simplify_relational_operation (code, mode, cmp_mode,
						 op0, op1)))
    return tem;

  return gen_rtx_fmt_ee (code, mode, op0, op1);
}

/* If FN is NULL, replace all occurrences of OLD_RTX in X with copy_rtx (DATA)
   and simplify the result.  If FN is non-NULL, call this callback on each
   X, if it returns non-NULL, replace X with its return value and simplify the
   result.  */

rtx
simplify_replace_fn_rtx (rtx x, const_rtx old_rtx,
			 rtx (*fn) (rtx, const_rtx, void *), void *data)
{
  enum rtx_code code = GET_CODE (x);
  machine_mode mode = GET_MODE (x);
  machine_mode op_mode;
  const char *fmt;
  rtx op0, op1, op2, newx, op;
  rtvec vec, newvec;
  int i, j;

  if (__builtin_expect (fn != NULL, 0))
    {
      newx = fn (x, old_rtx, data);
      if (newx)
	return newx;
    }
  else if (rtx_equal_p (x, old_rtx))
    return copy_rtx ((rtx) data);

  switch (GET_RTX_CLASS (code))
    {
    case RTX_UNARY:
      op0 = XEXP (x, 0);
      op_mode = GET_MODE (op0);
      op0 = simplify_replace_fn_rtx (op0, old_rtx, fn, data);
      if (op0 == XEXP (x, 0))
	return x;
      return simplify_gen_unary (code, mode, op0, op_mode);

    case RTX_BIN_ARITH:
    case RTX_COMM_ARITH:
      op0 = simplify_replace_fn_rtx (XEXP (x, 0), old_rtx, fn, data);
      op1 = simplify_replace_fn_rtx (XEXP (x, 1), old_rtx, fn, data);
      if (op0 == XEXP (x, 0) && op1 == XEXP (x, 1))
	return x;
      return simplify_gen_binary (code, mode, op0, op1);

    case RTX_COMPARE:
    case RTX_COMM_COMPARE:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      op_mode = GET_MODE (op0) != VOIDmode ? GET_MODE (op0) : GET_MODE (op1);
      op0 = simplify_replace_fn_rtx (op0, old_rtx, fn, data);
      op1 = simplify_replace_fn_rtx (op1, old_rtx, fn, data);
      if (op0 == XEXP (x, 0) && op1 == XEXP (x, 1))
	return x;
      return simplify_gen_relational (code, mode, op_mode, op0, op1);

    case RTX_TERNARY:
    case RTX_BITFIELD_OPS:
      op0 = XEXP (x, 0);
      op_mode = GET_MODE (op0);
      op0 = simplify_replace_fn_rtx (op0, old_rtx, fn, data);
      op1 = simplify_replace_fn_rtx (XEXP (x, 1), old_rtx, fn, data);
      op2 = simplify_replace_fn_rtx (XEXP (x, 2), old_rtx, fn, data);
      if (op0 == XEXP (x, 0) && op1 == XEXP (x, 1) && op2 == XEXP (x, 2))
	return x;
      if (op_mode == VOIDmode)
	op_mode = GET_MODE (op0);
      return simplify_gen_ternary (code, mode, op_mode, op0, op1, op2);

    case RTX_EXTRA:
      if (code == SUBREG)
	{
	  op0 = simplify_replace_fn_rtx (SUBREG_REG (x), old_rtx, fn, data);
	  if (op0 == SUBREG_REG (x))
	    return x;
	  op0 = simplify_gen_subreg (GET_MODE (x), op0,
				     GET_MODE (SUBREG_REG (x)),
				     SUBREG_BYTE (x));
	  return op0 ? op0 : x;
	}
      break;

    case RTX_OBJ:
      if (code == MEM)
	{
	  op0 = simplify_replace_fn_rtx (XEXP (x, 0), old_rtx, fn, data);
	  if (op0 == XEXP (x, 0))
	    return x;
	  return replace_equiv_address_nv (x, op0);
	}
      else if (code == LO_SUM)
	{
	  op0 = simplify_replace_fn_rtx (XEXP (x, 0), old_rtx, fn, data);
	  op1 = simplify_replace_fn_rtx (XEXP (x, 1), old_rtx, fn, data);

	  /* (lo_sum (high x) y) -> y where x and y have the same base.  */
	  if (GET_CODE (op0) == HIGH)
	    {
	      rtx base0, base1, offset0, offset1;
	      split_const (XEXP (op0, 0), &base0, &offset0);
	      split_const (op1, &base1, &offset1);
	      if (rtx_equal_p (base0, base1))
		return op1;
	    }

	  if (op0 == XEXP (x, 0) && op1 == XEXP (x, 1))
	    return x;
	  return gen_rtx_LO_SUM (mode, op0, op1);
	}
      break;

    default:
      break;
    }

  newx = x;
  fmt = GET_RTX_FORMAT (code);
  for (i = 0; fmt[i]; i++)
    switch (fmt[i])
      {
      case 'E':
	vec = XVEC (x, i);
	newvec = XVEC (newx, i);
	for (j = 0; j < GET_NUM_ELEM (vec); j++)
	  {
	    op = simplify_replace_fn_rtx (RTVEC_ELT (vec, j),
					  old_rtx, fn, data);
	    if (op != RTVEC_ELT (vec, j))
	      {
		if (newvec == vec)
		  {
		    newvec = shallow_copy_rtvec (vec);
		    if (x == newx)
		      newx = shallow_copy_rtx (x);
		    XVEC (newx, i) = newvec;
		  }
		RTVEC_ELT (newvec, j) = op;
	      }
	  }
	break;

      case 'e':
	if (XEXP (x, i))
	  {
	    op = simplify_replace_fn_rtx (XEXP (x, i), old_rtx, fn, data);
	    if (op != XEXP (x, i))
	      {
		if (x == newx)
		  newx = shallow_copy_rtx (x);
		XEXP (newx, i) = op;
	      }
	  }
	break;
      }
  return newx;
}

/* Replace all occurrences of OLD_RTX in X with NEW_RTX and try to simplify the
   resulting RTX.  Return a new RTX which is as simplified as possible.  */

rtx
simplify_replace_rtx (rtx x, const_rtx old_rtx, rtx new_rtx)
{
  return simplify_replace_fn_rtx (x, old_rtx, 0, new_rtx);
}

/* Try to simplify a MODE truncation of OP, which has OP_MODE.
   Only handle cases where the truncated value is inherently an rvalue.

   RTL provides two ways of truncating a value:

   1. a lowpart subreg.  This form is only a truncation when both
      the outer and inner modes (here MODE and OP_MODE respectively)
      are scalar integers, and only then when the subreg is used as
      an rvalue.

      It is only valid to form such truncating subregs if the
      truncation requires no action by the target.  The onus for
      proving this is on the creator of the subreg -- e.g. the
      caller to simplify_subreg or simplify_gen_subreg -- and typically
      involves either TRULY_NOOP_TRUNCATION_MODES_P or truncated_to_mode.

   2. a TRUNCATE.  This form handles both scalar and compound integers.

   The first form is preferred where valid.  However, the TRUNCATE
   handling in simplify_unary_operation turns the second form into the
   first form when TRULY_NOOP_TRUNCATION_MODES_P or truncated_to_mode allow,
   so it is generally safe to form rvalue truncations using:

      simplify_gen_unary (TRUNCATE, ...)

   and leave simplify_unary_operation to work out which representation
   should be used.

   Because of the proof requirements on (1), simplify_truncation must
   also use simplify_gen_unary (TRUNCATE, ...) to truncate parts of OP,
   regardless of whether the outer truncation came from a SUBREG or a
   TRUNCATE.  For example, if the caller has proven that an SImode
   truncation of:

      (and:DI X Y)

   is a no-op and can be represented as a subreg, it does not follow
   that SImode truncations of X and Y are also no-ops.  On a target
   like 64-bit MIPS that requires SImode values to be stored in
   sign-extended form, an SImode truncation of:

      (and:DI (reg:DI X) (const_int 63))

   is trivially a no-op because only the lower 6 bits can be set.
   However, X is still an arbitrary 64-bit number and so we cannot
   assume that truncating it too is a no-op.  */

static rtx
simplify_truncation (machine_mode mode, rtx op,
		     machine_mode op_mode)
{
  unsigned int precision = GET_MODE_UNIT_PRECISION (mode);
  unsigned int op_precision = GET_MODE_UNIT_PRECISION (op_mode);
  scalar_int_mode int_mode, int_op_mode, subreg_mode;

  gcc_assert (precision <= op_precision);

  /* Optimize truncations of zero and sign extended values.  */
  if (GET_CODE (op) == ZERO_EXTEND
      || GET_CODE (op) == SIGN_EXTEND)
    {
      /* There are three possibilities.  If MODE is the same as the
	 origmode, we can omit both the extension and the subreg.
	 If MODE is not larger than the origmode, we can apply the
	 truncation without the extension.  Finally, if the outermode
	 is larger than the origmode, we can just extend to the appropriate
	 mode.  */
      machine_mode origmode = GET_MODE (XEXP (op, 0));
      if (mode == origmode)
	return XEXP (op, 0);
      else if (precision <= GET_MODE_UNIT_PRECISION (origmode))
	return simplify_gen_unary (TRUNCATE, mode,
				   XEXP (op, 0), origmode);
      else
	return simplify_gen_unary (GET_CODE (op), mode,
				   XEXP (op, 0), origmode);
    }

  /* If the machine can perform operations in the truncated mode, distribute
     the truncation, i.e. simplify (truncate:QI (op:SI (x:SI) (y:SI))) into
     (op:QI (truncate:QI (x:SI)) (truncate:QI (y:SI))).  */
  if (1
      && (!WORD_REGISTER_OPERATIONS || precision >= BITS_PER_WORD)
      && (GET_CODE (op) == PLUS
	  || GET_CODE (op) == MINUS
	  || GET_CODE (op) == MULT))
    {
      rtx op0 = simplify_gen_unary (TRUNCATE, mode, XEXP (op, 0), op_mode);
      if (op0)
	{
	  rtx op1 = simplify_gen_unary (TRUNCATE, mode, XEXP (op, 1), op_mode);
	  if (op1)
	    return simplify_gen_binary (GET_CODE (op), mode, op0, op1);
	}
    }

  /* Simplify (truncate:QI (lshiftrt:SI (sign_extend:SI (x:QI)) C)) into
     to (ashiftrt:QI (x:QI) C), where C is a suitable small constant and
     the outer subreg is effectively a truncation to the original mode.  */
  if ((GET_CODE (op) == LSHIFTRT
       || GET_CODE (op) == ASHIFTRT)
      /* Ensure that OP_MODE is at least twice as wide as MODE
	 to avoid the possibility that an outer LSHIFTRT shifts by more
	 than the sign extension's sign_bit_copies and introduces zeros
	 into the high bits of the result.  */
      && 2 * precision <= op_precision
      && CONST_INT_P (XEXP (op, 1))
      && GET_CODE (XEXP (op, 0)) == SIGN_EXTEND
      && GET_MODE (XEXP (XEXP (op, 0), 0)) == mode
      && UINTVAL (XEXP (op, 1)) < precision)
    return simplify_gen_binary (ASHIFTRT, mode,
				XEXP (XEXP (op, 0), 0), XEXP (op, 1));

  /* Likewise (truncate:QI (lshiftrt:SI (zero_extend:SI (x:QI)) C)) into
     to (lshiftrt:QI (x:QI) C), where C is a suitable small constant and
     the outer subreg is effectively a truncation to the original mode.  */
  if ((GET_CODE (op) == LSHIFTRT
       || GET_CODE (op) == ASHIFTRT)
      && CONST_INT_P (XEXP (op, 1))
      && GET_CODE (XEXP (op, 0)) == ZERO_EXTEND
      && GET_MODE (XEXP (XEXP (op, 0), 0)) == mode
      && UINTVAL (XEXP (op, 1)) < precision)
    return simplify_gen_binary (LSHIFTRT, mode,
				XEXP (XEXP (op, 0), 0), XEXP (op, 1));

  /* Likewise (truncate:QI (ashift:SI (zero_extend:SI (x:QI)) C)) into
     to (ashift:QI (x:QI) C), where C is a suitable small constant and
     the outer subreg is effectively a truncation to the original mode.  */
  if (GET_CODE (op) == ASHIFT
      && CONST_INT_P (XEXP (op, 1))
      && (GET_CODE (XEXP (op, 0)) == ZERO_EXTEND
	  || GET_CODE (XEXP (op, 0)) == SIGN_EXTEND)
      && GET_MODE (XEXP (XEXP (op, 0), 0)) == mode
      && UINTVAL (XEXP (op, 1)) < precision)
    return simplify_gen_binary (ASHIFT, mode,
				XEXP (XEXP (op, 0), 0), XEXP (op, 1));

  /* Likewise (truncate:QI (and:SI (lshiftrt:SI (x:SI) C) C2)) into
     (and:QI (lshiftrt:QI (truncate:QI (x:SI)) C) C2) for suitable C
     and C2.  */
  if (GET_CODE (op) == AND
      && (GET_CODE (XEXP (op, 0)) == LSHIFTRT
	  || GET_CODE (XEXP (op, 0)) == ASHIFTRT)
      && CONST_INT_P (XEXP (XEXP (op, 0), 1))
      && CONST_INT_P (XEXP (op, 1)))
    {
      rtx op0 = (XEXP (XEXP (op, 0), 0));
      rtx shift_op = XEXP (XEXP (op, 0), 1);
      rtx mask_op = XEXP (op, 1);
      unsigned HOST_WIDE_INT shift = UINTVAL (shift_op);
      unsigned HOST_WIDE_INT mask = UINTVAL (mask_op);

      if (shift < precision
	  /* If doing this transform works for an X with all bits set,
	     it works for any X.  */
	  && ((GET_MODE_MASK (mode) >> shift) & mask)
	     == ((GET_MODE_MASK (op_mode) >> shift) & mask)
	  && (op0 = simplify_gen_unary (TRUNCATE, mode, op0, op_mode))
	  && (op0 = simplify_gen_binary (LSHIFTRT, mode, op0, shift_op)))
	{
	  mask_op = GEN_INT (trunc_int_for_mode (mask, mode));
	  return simplify_gen_binary (AND, mode, op0, mask_op);
	}
    }

  /* Turn (truncate:M1 (*_extract:M2 (reg:M2) (len) (pos))) into
     (*_extract:M1 (truncate:M1 (reg:M2)) (len) (pos')) if possible without
     changing len.  */
  if ((GET_CODE (op) == ZERO_EXTRACT || GET_CODE (op) == SIGN_EXTRACT)
      && REG_P (XEXP (op, 0))
      && GET_MODE (XEXP (op, 0)) == GET_MODE (op)
      && CONST_INT_P (XEXP (op, 1))
      && CONST_INT_P (XEXP (op, 2)))
    {
      rtx op0 = XEXP (op, 0);
      unsigned HOST_WIDE_INT len = UINTVAL (XEXP (op, 1));
      unsigned HOST_WIDE_INT pos = UINTVAL (XEXP (op, 2));
      if (BITS_BIG_ENDIAN && pos >= op_precision - precision)
	{
	  op0 = simplify_gen_unary (TRUNCATE, mode, op0, GET_MODE (op0));
	  if (op0)
	    {
	      pos -= op_precision - precision;
	      return simplify_gen_ternary (GET_CODE (op), mode, mode, op0,
					   XEXP (op, 1), GEN_INT (pos));
	    }
	}
      else if (!BITS_BIG_ENDIAN && precision >= len + pos)
	{
	  op0 = simplify_gen_unary (TRUNCATE, mode, op0, GET_MODE (op0));
	  if (op0)
	    return simplify_gen_ternary (GET_CODE (op), mode, mode, op0,
					 XEXP (op, 1), XEXP (op, 2));
	}
    }

  /* Recognize a word extraction from a multi-word subreg.  */
  if ((GET_CODE (op) == LSHIFTRT
       || GET_CODE (op) == ASHIFTRT)
      && SCALAR_INT_MODE_P (mode)
      && SCALAR_INT_MODE_P (op_mode)
      && precision >= BITS_PER_WORD
      && 2 * precision <= op_precision
      && CONST_INT_P (XEXP (op, 1))
      && (INTVAL (XEXP (op, 1)) & (precision - 1)) == 0
      && UINTVAL (XEXP (op, 1)) < op_precision)
    {
      int byte = subreg_lowpart_offset (mode, op_mode);
      int shifted_bytes = INTVAL (XEXP (op, 1)) / BITS_PER_UNIT;
      return simplify_gen_subreg (mode, XEXP (op, 0), op_mode,
				  (WORDS_BIG_ENDIAN
				   ? byte - shifted_bytes
				   : byte + shifted_bytes));
    }

  /* If we have a TRUNCATE of a right shift of MEM, make a new MEM
     and try replacing the TRUNCATE and shift with it.  Don't do this
     if the MEM has a mode-dependent address.  */
  if ((GET_CODE (op) == LSHIFTRT
       || GET_CODE (op) == ASHIFTRT)
      && is_a <scalar_int_mode> (mode, &int_mode)
      && is_a <scalar_int_mode> (op_mode, &int_op_mode)
      && MEM_P (XEXP (op, 0))
      && CONST_INT_P (XEXP (op, 1))
      && INTVAL (XEXP (op, 1)) % GET_MODE_BITSIZE (int_mode) == 0
      && INTVAL (XEXP (op, 1)) > 0
      && INTVAL (XEXP (op, 1)) < GET_MODE_BITSIZE (int_op_mode)
      && ! mode_dependent_address_p (XEXP (XEXP (op, 0), 0),
				     MEM_ADDR_SPACE (XEXP (op, 0)))
      && ! MEM_VOLATILE_P (XEXP (op, 0))
      && (GET_MODE_SIZE (int_mode) >= UNITS_PER_WORD
	  || WORDS_BIG_ENDIAN == BYTES_BIG_ENDIAN))
    {
      int byte = subreg_lowpart_offset (int_mode, int_op_mode);
      int shifted_bytes = INTVAL (XEXP (op, 1)) / BITS_PER_UNIT;
      return adjust_address_nv (XEXP (op, 0), int_mode,
				(WORDS_BIG_ENDIAN
				 ? byte - shifted_bytes
				 : byte + shifted_bytes));
    }

  /* (truncate:SI (OP:DI ({sign,zero}_extend:DI foo:SI))) is
     (OP:SI foo:SI) if OP is NEG or ABS.  */
  if ((GET_CODE (op) == ABS
       || GET_CODE (op) == NEG)
      && (GET_CODE (XEXP (op, 0)) == SIGN_EXTEND
	  || GET_CODE (XEXP (op, 0)) == ZERO_EXTEND)
      && GET_MODE (XEXP (XEXP (op, 0), 0)) == mode)
    return simplify_gen_unary (GET_CODE (op), mode,
			       XEXP (XEXP (op, 0), 0), mode);

  /* (truncate:A (subreg:B (truncate:C X) 0)) is
     (truncate:A X).  */
  if (GET_CODE (op) == SUBREG
      && is_a <scalar_int_mode> (mode, &int_mode)
      && SCALAR_INT_MODE_P (op_mode)
      && is_a <scalar_int_mode> (GET_MODE (SUBREG_REG (op)), &subreg_mode)
      && GET_CODE (SUBREG_REG (op)) == TRUNCATE
      && subreg_lowpart_p (op))
    {
      rtx inner = XEXP (SUBREG_REG (op), 0);
      if (GET_MODE_PRECISION (int_mode) <= GET_MODE_PRECISION (subreg_mode))
	return simplify_gen_unary (TRUNCATE, int_mode, inner,
				   GET_MODE (inner));
      else
	/* If subreg above is paradoxical and C is narrower
	   than A, return (subreg:A (truncate:C X) 0).  */
	return simplify_gen_subreg (int_mode, SUBREG_REG (op), subreg_mode, 0);
    }

  /* (truncate:A (truncate:B X)) is (truncate:A X).  */
  if (GET_CODE (op) == TRUNCATE)
    return simplify_gen_unary (TRUNCATE, mode, XEXP (op, 0),
			       GET_MODE (XEXP (op, 0)));

  /* (truncate:A (ior X C)) is (const_int -1) if C is equal to that already,
     in mode A.  */
  if (GET_CODE (op) == IOR
      && SCALAR_INT_MODE_P (mode)
      && SCALAR_INT_MODE_P (op_mode)
      && CONST_INT_P (XEXP (op, 1))
      && trunc_int_for_mode (INTVAL (XEXP (op, 1)), mode) == -1)
    return constm1_rtx;

  return NULL_RTX;
}

/* Try to simplify a unary operation CODE whose output mode is to be
   MODE with input operand OP whose mode was originally OP_MODE.
   Return zero if no simplification can be made.  */
rtx
simplify_unary_operation (enum rtx_code code, machine_mode mode,
			  rtx op, machine_mode op_mode)
{
  rtx trueop, tem;

  trueop = avoid_constant_pool_reference (op);

  tem = simplify_const_unary_operation (code, mode, trueop, op_mode);
  if (tem)
    return tem;

  return simplify_unary_operation_1 (code, mode, op);
}

/* Return true if FLOAT or UNSIGNED_FLOAT operation OP is known
   to be exact.  */

static bool
exact_int_to_float_conversion_p (const_rtx op)
{
  int out_bits = significand_size (GET_MODE_INNER (GET_MODE (op)));
  machine_mode op0_mode = GET_MODE (XEXP (op, 0));
  /* Constants shouldn't reach here.  */
  gcc_assert (op0_mode != VOIDmode);
  int in_prec = GET_MODE_UNIT_PRECISION (op0_mode);
  int in_bits = in_prec;
  if (HWI_COMPUTABLE_MODE_P (op0_mode))
    {
      unsigned HOST_WIDE_INT nonzero = nonzero_bits (XEXP (op, 0), op0_mode);
      if (GET_CODE (op) == FLOAT)
	in_bits -= num_sign_bit_copies (XEXP (op, 0), op0_mode);
      else if (GET_CODE (op) == UNSIGNED_FLOAT)
	in_bits = wi::min_precision (wi::uhwi (nonzero, in_prec), UNSIGNED);
      else
	gcc_unreachable ();
      in_bits -= wi::ctz (wi::uhwi (nonzero, in_prec));
    }
  return in_bits <= out_bits;
}

/* Perform some simplifications we can do even if the operands
   aren't constant.  */
static rtx
simplify_unary_operation_1 (enum rtx_code code, machine_mode mode, rtx op)
{
  enum rtx_code reversed;
  rtx temp;
  scalar_int_mode inner, int_mode, op_mode, op0_mode;

  switch (code)
    {
    case NOT:
      /* (not (not X)) == X.  */
      if (GET_CODE (op) == NOT)
	return XEXP (op, 0);

      /* (not (eq X Y)) == (ne X Y), etc. if BImode or the result of the
	 comparison is all ones.   */
      if (COMPARISON_P (op)
	  && (mode == BImode || STORE_FLAG_VALUE == -1)
	  && ((reversed = reversed_comparison_code (op, NULL)) != UNKNOWN))
	return simplify_gen_relational (reversed, mode, VOIDmode,
					XEXP (op, 0), XEXP (op, 1));

      /* (not (plus X -1)) can become (neg X).  */
      if (GET_CODE (op) == PLUS
	  && XEXP (op, 1) == constm1_rtx)
	return simplify_gen_unary (NEG, mode, XEXP (op, 0), mode);

      /* Similarly, (not (neg X)) is (plus X -1).  Only do this for
	 modes that have CONSTM1_RTX, i.e. MODE_INT, MODE_PARTIAL_INT
	 and MODE_VECTOR_INT.  */
      if (GET_CODE (op) == NEG && CONSTM1_RTX (mode))
	return simplify_gen_binary (PLUS, mode, XEXP (op, 0),
				    CONSTM1_RTX (mode));

      /* (not (xor X C)) for C constant is (xor X D) with D = ~C.  */
      if (GET_CODE (op) == XOR
	  && CONST_INT_P (XEXP (op, 1))
	  && (temp = simplify_unary_operation (NOT, mode,
					       XEXP (op, 1), mode)) != 0)
	return simplify_gen_binary (XOR, mode, XEXP (op, 0), temp);

      /* (not (plus X C)) for signbit C is (xor X D) with D = ~C.  */
      if (GET_CODE (op) == PLUS
	  && CONST_INT_P (XEXP (op, 1))
	  && mode_signbit_p (mode, XEXP (op, 1))
	  && (temp = simplify_unary_operation (NOT, mode,
					       XEXP (op, 1), mode)) != 0)
	return simplify_gen_binary (XOR, mode, XEXP (op, 0), temp);


      /* (not (ashift 1 X)) is (rotate ~1 X).  We used to do this for
	 operands other than 1, but that is not valid.  We could do a
	 similar simplification for (not (lshiftrt C X)) where C is
	 just the sign bit, but this doesn't seem common enough to
	 bother with.  */
      if (GET_CODE (op) == ASHIFT
	  && XEXP (op, 0) == const1_rtx)
	{
	  temp = simplify_gen_unary (NOT, mode, const1_rtx, mode);
	  return simplify_gen_binary (ROTATE, mode, temp, XEXP (op, 1));
	}

      /* (not (ashiftrt foo C)) where C is the number of bits in FOO
	 minus 1 is (ge foo (const_int 0)) if STORE_FLAG_VALUE is -1,
	 so we can perform the above simplification.  */
      if (STORE_FLAG_VALUE == -1
	  && is_a <scalar_int_mode> (mode, &int_mode)
	  && GET_CODE (op) == ASHIFTRT
	  && CONST_INT_P (XEXP (op, 1))
	  && INTVAL (XEXP (op, 1)) == GET_MODE_PRECISION (int_mode) - 1)
	return simplify_gen_relational (GE, int_mode, VOIDmode,
					XEXP (op, 0), const0_rtx);


      if (partial_subreg_p (op)
	  && subreg_lowpart_p (op)
	  && GET_CODE (SUBREG_REG (op)) == ASHIFT
	  && XEXP (SUBREG_REG (op), 0) == const1_rtx)
	{
	  machine_mode inner_mode = GET_MODE (SUBREG_REG (op));
	  rtx x;

	  x = gen_rtx_ROTATE (inner_mode,
			      simplify_gen_unary (NOT, inner_mode, const1_rtx,
						  inner_mode),
			      XEXP (SUBREG_REG (op), 1));
	  temp = rtl_hooks.gen_lowpart_no_emit (mode, x);
	  if (temp)
	    return temp;
	}

      /* Apply De Morgan's laws to reduce number of patterns for machines
	 with negating logical insns (and-not, nand, etc.).  If result has
	 only one NOT, put it first, since that is how the patterns are
	 coded.  */
      if (GET_CODE (op) == IOR || GET_CODE (op) == AND)
	{
	  rtx in1 = XEXP (op, 0), in2 = XEXP (op, 1);
	  machine_mode op_mode;

	  op_mode = GET_MODE (in1);
	  in1 = simplify_gen_unary (NOT, op_mode, in1, op_mode);

	  op_mode = GET_MODE (in2);
	  if (op_mode == VOIDmode)
	    op_mode = mode;
	  in2 = simplify_gen_unary (NOT, op_mode, in2, op_mode);

	  if (GET_CODE (in2) == NOT && GET_CODE (in1) != NOT)
	    std::swap (in1, in2);

	  return gen_rtx_fmt_ee (GET_CODE (op) == IOR ? AND : IOR,
				 mode, in1, in2);
	}

      /* (not (bswap x)) -> (bswap (not x)).  */
      if (GET_CODE (op) == BSWAP)
	{
	  rtx x = simplify_gen_unary (NOT, mode, XEXP (op, 0), mode);
	  return simplify_gen_unary (BSWAP, mode, x, mode);
	}
      break;

    case NEG:
      /* (neg (neg X)) == X.  */
      if (GET_CODE (op) == NEG)
	return XEXP (op, 0);

      /* (neg (x ? (neg y) : y)) == !x ? (neg y) : y.
	 If comparison is not reversible use
	 x ? y : (neg y).  */
      if (GET_CODE (op) == IF_THEN_ELSE)
	{
	  rtx cond = XEXP (op, 0);
	  rtx true_rtx = XEXP (op, 1);
	  rtx false_rtx = XEXP (op, 2);

	  if ((GET_CODE (true_rtx) == NEG
	       && rtx_equal_p (XEXP (true_rtx, 0), false_rtx))
	       || (GET_CODE (false_rtx) == NEG
		   && rtx_equal_p (XEXP (false_rtx, 0), true_rtx)))
	    {
	      if (reversed_comparison_code (cond, NULL) != UNKNOWN)
		temp = reversed_comparison (cond, mode);
	      else
		{
		  temp = cond;
		  std::swap (true_rtx, false_rtx);
		}
	      return simplify_gen_ternary (IF_THEN_ELSE, mode,
					    mode, temp, true_rtx, false_rtx);
	    }
	}

      /* (neg (plus X 1)) can become (not X).  */
      if (GET_CODE (op) == PLUS
	  && XEXP (op, 1) == const1_rtx)
	return simplify_gen_unary (NOT, mode, XEXP (op, 0), mode);

      /* Similarly, (neg (not X)) is (plus X 1).  */
      if (GET_CODE (op) == NOT)
	return simplify_gen_binary (PLUS, mode, XEXP (op, 0),
				    CONST1_RTX (mode));

      /* (neg (minus X Y)) can become (minus Y X).  This transformation
	 isn't safe for modes with signed zeros, since if X and Y are
	 both +0, (minus Y X) is the same as (minus X Y).  If the
	 rounding mode is towards +infinity (or -infinity) then the two
	 expressions will be rounded differently.  */
      if (GET_CODE (op) == MINUS
	  && !HONOR_SIGNED_ZEROS (mode)
	  && !HONOR_SIGN_DEPENDENT_ROUNDING (mode))
	return simplify_gen_binary (MINUS, mode, XEXP (op, 1), XEXP (op, 0));

      if (GET_CODE (op) == PLUS
	  && !HONOR_SIGNED_ZEROS (mode)
	  && !HONOR_SIGN_DEPENDENT_ROUNDING (mode))
	{
	  /* (neg (plus A C)) is simplified to (minus -C A).  */
	  if (CONST_SCALAR_INT_P (XEXP (op, 1))
	      || CONST_DOUBLE_AS_FLOAT_P (XEXP (op, 1)))
	    {
	      temp = simplify_unary_operation (NEG, mode, XEXP (op, 1), mode);
	      if (temp)
		return simplify_gen_binary (MINUS, mode, temp, XEXP (op, 0));
	    }

	  /* (neg (plus A B)) is canonicalized to (minus (neg A) B).  */
	  temp = simplify_gen_unary (NEG, mode, XEXP (op, 0), mode);
	  return simplify_gen_binary (MINUS, mode, temp, XEXP (op, 1));
	}

      /* (neg (mult A B)) becomes (mult A (neg B)).
	 This works even for floating-point values.  */
      if (GET_CODE (op) == MULT
	  && !HONOR_SIGN_DEPENDENT_ROUNDING (mode))
	{
	  temp = simplify_gen_unary (NEG, mode, XEXP (op, 1), mode);
	  return simplify_gen_binary (MULT, mode, XEXP (op, 0), temp);
	}

      /* NEG commutes with ASHIFT since it is multiplication.  Only do
	 this if we can then eliminate the NEG (e.g., if the operand
	 is a constant).  */
      if (GET_CODE (op) == ASHIFT)
	{
	  temp = simplify_unary_operation (NEG, mode, XEXP (op, 0), mode);
	  if (temp)
	    return simplify_gen_binary (ASHIFT, mode, temp, XEXP (op, 1));
	}

      /* (neg (ashiftrt X C)) can be replaced by (lshiftrt X C) when
	 C is equal to the width of MODE minus 1.  */
      if (GET_CODE (op) == ASHIFTRT
	  && CONST_INT_P (XEXP (op, 1))
	  && INTVAL (XEXP (op, 1)) == GET_MODE_PRECISION (mode) - 1)
	return simplify_gen_binary (LSHIFTRT, mode,
				    XEXP (op, 0), XEXP (op, 1));

      /* (neg (lshiftrt X C)) can be replaced by (ashiftrt X C) when
	 C is equal to the width of MODE minus 1.  */
      if (GET_CODE (op) == LSHIFTRT
	  && CONST_INT_P (XEXP (op, 1))
	  && INTVAL (XEXP (op, 1)) == GET_MODE_PRECISION (mode) - 1)
	return simplify_gen_binary (ASHIFTRT, mode,
				    XEXP (op, 0), XEXP (op, 1));

      /* (neg (xor A 1)) is (plus A -1) if A is known to be either 0 or 1.  */
      if (GET_CODE (op) == XOR
	  && XEXP (op, 1) == const1_rtx
	  && nonzero_bits (XEXP (op, 0), mode) == 1)
	return plus_constant (mode, XEXP (op, 0), -1);

      /* (neg (lt x 0)) is (ashiftrt X C) if STORE_FLAG_VALUE is 1.  */
      /* (neg (lt x 0)) is (lshiftrt X C) if STORE_FLAG_VALUE is -1.  */
      if (GET_CODE (op) == LT
	  && XEXP (op, 1) == const0_rtx
	  && is_a <scalar_int_mode> (GET_MODE (XEXP (op, 0)), &inner))
	{
	  int_mode = as_a <scalar_int_mode> (mode);
	  int isize = GET_MODE_PRECISION (inner);
	  if (STORE_FLAG_VALUE == 1)
	    {
	      temp = simplify_gen_binary (ASHIFTRT, inner, XEXP (op, 0),
					  GEN_INT (isize - 1));
	      if (int_mode == inner)
		return temp;
	      if (GET_MODE_PRECISION (int_mode) > isize)
		return simplify_gen_unary (SIGN_EXTEND, int_mode, temp, inner);
	      return simplify_gen_unary (TRUNCATE, int_mode, temp, inner);
	    }
	  else if (STORE_FLAG_VALUE == -1)
	    {
	      temp = simplify_gen_binary (LSHIFTRT, inner, XEXP (op, 0),
					  GEN_INT (isize - 1));
	      if (int_mode == inner)
		return temp;
	      if (GET_MODE_PRECISION (int_mode) > isize)
		return simplify_gen_unary (ZERO_EXTEND, int_mode, temp, inner);
	      return simplify_gen_unary (TRUNCATE, int_mode, temp, inner);
	    }
	}
      break;

    case TRUNCATE:
      /* Don't optimize (lshiftrt (mult ...)) as it would interfere
	 with the umulXi3_highpart patterns.  */
      if (GET_CODE (op) == LSHIFTRT
	  && GET_CODE (XEXP (op, 0)) == MULT)
	break;

      if (GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
	{
	  if (TRULY_NOOP_TRUNCATION_MODES_P (mode, GET_MODE (op)))
	    {
	      temp = rtl_hooks.gen_lowpart_no_emit (mode, op);
	      if (temp)
		return temp;
	    }
	  /* We can't handle truncation to a partial integer mode here
	     because we don't know the real bitsize of the partial
	     integer mode.  */
	  break;
	}

      if (GET_MODE (op) != VOIDmode)
	{
	  temp = simplify_truncation (mode, op, GET_MODE (op));
	  if (temp)
	    return temp;
	}

      /* If we know that the value is already truncated, we can
	 replace the TRUNCATE with a SUBREG.  */
      if (GET_MODE_NUNITS (mode) == 1
	  && (TRULY_NOOP_TRUNCATION_MODES_P (mode, GET_MODE (op))
	      || truncated_to_mode (mode, op)))
	{
	  temp = rtl_hooks.gen_lowpart_no_emit (mode, op);
	  if (temp)
	    return temp;
	}

      /* A truncate of a comparison can be replaced with a subreg if
         STORE_FLAG_VALUE permits.  This is like the previous test,
         but it works even if the comparison is done in a mode larger
         than HOST_BITS_PER_WIDE_INT.  */
      if (HWI_COMPUTABLE_MODE_P (mode)
	  && COMPARISON_P (op)
	  && (STORE_FLAG_VALUE & ~GET_MODE_MASK (mode)) == 0)
	{
	  temp = rtl_hooks.gen_lowpart_no_emit (mode, op);
	  if (temp)
	    return temp;
	}

      /* A truncate of a memory is just loading the low part of the memory
	 if we are not changing the meaning of the address. */
      if (GET_CODE (op) == MEM
	  && !VECTOR_MODE_P (mode)
	  && !MEM_VOLATILE_P (op)
	  && !mode_dependent_address_p (XEXP (op, 0), MEM_ADDR_SPACE (op)))
	{
	  temp = rtl_hooks.gen_lowpart_no_emit (mode, op);
	  if (temp)
	    return temp;
	}

      break;

    case FLOAT_TRUNCATE:
      if (DECIMAL_FLOAT_MODE_P (mode))
	break;

      /* (float_truncate:SF (float_extend:DF foo:SF)) = foo:SF.  */
      if (GET_CODE (op) == FLOAT_EXTEND
	  && GET_MODE (XEXP (op, 0)) == mode)
	return XEXP (op, 0);

      /* (float_truncate:SF (float_truncate:DF foo:XF))
         = (float_truncate:SF foo:XF).
	 This may eliminate double rounding, so it is unsafe.

         (float_truncate:SF (float_extend:XF foo:DF))
         = (float_truncate:SF foo:DF).

         (float_truncate:DF (float_extend:XF foo:SF))
         = (float_extend:DF foo:SF).  */
      if ((GET_CODE (op) == FLOAT_TRUNCATE
	   && flag_unsafe_math_optimizations)
	  || GET_CODE (op) == FLOAT_EXTEND)
	return simplify_gen_unary (GET_MODE_UNIT_SIZE (GET_MODE (XEXP (op, 0)))
	  			   > GET_MODE_UNIT_SIZE (mode)
	  			   ? FLOAT_TRUNCATE : FLOAT_EXTEND,
				   mode,
				   XEXP (op, 0), mode);

      /*  (float_truncate (float x)) is (float x)  */
      if ((GET_CODE (op) == FLOAT || GET_CODE (op) == UNSIGNED_FLOAT)
	  && (flag_unsafe_math_optimizations
	      || exact_int_to_float_conversion_p (op)))
	return simplify_gen_unary (GET_CODE (op), mode,
				   XEXP (op, 0),
				   GET_MODE (XEXP (op, 0)));

      /* (float_truncate:SF (OP:DF (float_extend:DF foo:sf))) is
	 (OP:SF foo:SF) if OP is NEG or ABS.  */
      if ((GET_CODE (op) == ABS
	   || GET_CODE (op) == NEG)
	  && GET_CODE (XEXP (op, 0)) == FLOAT_EXTEND
	  && GET_MODE (XEXP (XEXP (op, 0), 0)) == mode)
	return simplify_gen_unary (GET_CODE (op), mode,
				   XEXP (XEXP (op, 0), 0), mode);

      /* (float_truncate:SF (subreg:DF (float_truncate:SF X) 0))
	 is (float_truncate:SF x).  */
      if (GET_CODE (op) == SUBREG
	  && subreg_lowpart_p (op)
	  && GET_CODE (SUBREG_REG (op)) == FLOAT_TRUNCATE)
	return SUBREG_REG (op);
      break;

    case FLOAT_EXTEND:
      if (DECIMAL_FLOAT_MODE_P (mode))
	break;

      /*  (float_extend (float_extend x)) is (float_extend x)

	  (float_extend (float x)) is (float x) assuming that double
	  rounding can't happen.
          */
      if (GET_CODE (op) == FLOAT_EXTEND
	  || ((GET_CODE (op) == FLOAT || GET_CODE (op) == UNSIGNED_FLOAT)
	      && exact_int_to_float_conversion_p (op)))
	return simplify_gen_unary (GET_CODE (op), mode,
				   XEXP (op, 0),
				   GET_MODE (XEXP (op, 0)));

      break;

    case ABS:
      /* (abs (neg <foo>)) -> (abs <foo>) */
      if (GET_CODE (op) == NEG)
	return simplify_gen_unary (ABS, mode, XEXP (op, 0),
				   GET_MODE (XEXP (op, 0)));

      /* If the mode of the operand is VOIDmode (i.e. if it is ASM_OPERANDS),
         do nothing.  */
      if (GET_MODE (op) == VOIDmode)
	break;

      /* If operand is something known to be positive, ignore the ABS.  */
      if (GET_CODE (op) == FFS || GET_CODE (op) == ABS
	  || val_signbit_known_clear_p (GET_MODE (op),
					nonzero_bits (op, GET_MODE (op))))
	return op;

      /* If operand is known to be only -1 or 0, convert ABS to NEG.  */
      if (is_a <scalar_int_mode> (mode, &int_mode)
	  && (num_sign_bit_copies (op, int_mode)
	      == GET_MODE_PRECISION (int_mode)))
	return gen_rtx_NEG (int_mode, op);

      break;

    case FFS:
      /* (ffs (*_extend <X>)) = (ffs <X>) */
      if (GET_CODE (op) == SIGN_EXTEND
	  || GET_CODE (op) == ZERO_EXTEND)
	return simplify_gen_unary (FFS, mode, XEXP (op, 0),
				   GET_MODE (XEXP (op, 0)));
      break;

    case POPCOUNT:
      switch (GET_CODE (op))
	{
	case BSWAP:
	case ZERO_EXTEND:
	  /* (popcount (zero_extend <X>)) = (popcount <X>) */
	  return simplify_gen_unary (POPCOUNT, mode, XEXP (op, 0),
				     GET_MODE (XEXP (op, 0)));

	case ROTATE:
	case ROTATERT:
	  /* Rotations don't affect popcount.  */
	  if (!side_effects_p (XEXP (op, 1)))
	    return simplify_gen_unary (POPCOUNT, mode, XEXP (op, 0),
				       GET_MODE (XEXP (op, 0)));
	  break;

	default:
	  break;
	}
      break;

    case PARITY:
      switch (GET_CODE (op))
	{
	case NOT:
	case BSWAP:
	case ZERO_EXTEND:
	case SIGN_EXTEND:
	  return simplify_gen_unary (PARITY, mode, XEXP (op, 0),
				     GET_MODE (XEXP (op, 0)));

	case ROTATE:
	case ROTATERT:
	  /* Rotations don't affect parity.  */
	  if (!side_effects_p (XEXP (op, 1)))
	    return simplify_gen_unary (PARITY, mode, XEXP (op, 0),
				       GET_MODE (XEXP (op, 0)));
	  break;

	default:
	  break;
	}
      break;

    case BSWAP:
      /* (bswap (bswap x)) -> x.  */
      if (GET_CODE (op) == BSWAP)
	return XEXP (op, 0);
      break;

    case FLOAT:
      /* (float (sign_extend <X>)) = (float <X>).  */
      if (GET_CODE (op) == SIGN_EXTEND)
	return simplify_gen_unary (FLOAT, mode, XEXP (op, 0),
				   GET_MODE (XEXP (op, 0)));
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

      /* Extending a widening multiplication should be canonicalized to
	 a wider widening multiplication.  */
      if (GET_CODE (op) == MULT)
	{
	  rtx lhs = XEXP (op, 0);
	  rtx rhs = XEXP (op, 1);
	  enum rtx_code lcode = GET_CODE (lhs);
	  enum rtx_code rcode = GET_CODE (rhs);

	  /* Widening multiplies usually extend both operands, but sometimes
	     they use a shift to extract a portion of a register.  */
	  if ((lcode == SIGN_EXTEND
	       || (lcode == ASHIFTRT && CONST_INT_P (XEXP (lhs, 1))))
	      && (rcode == SIGN_EXTEND
		  || (rcode == ASHIFTRT && CONST_INT_P (XEXP (rhs, 1)))))
	    {
	      machine_mode lmode = GET_MODE (lhs);
	      machine_mode rmode = GET_MODE (rhs);
	      int bits;

	      if (lcode == ASHIFTRT)
		/* Number of bits not shifted off the end.  */
		bits = GET_MODE_PRECISION (lmode) - INTVAL (XEXP (lhs, 1));
	      else /* lcode == SIGN_EXTEND */
		/* Size of inner mode.  */
		bits = GET_MODE_PRECISION (GET_MODE (XEXP (lhs, 0)));

	      if (rcode == ASHIFTRT)
		bits += GET_MODE_PRECISION (rmode) - INTVAL (XEXP (rhs, 1));
	      else /* rcode == SIGN_EXTEND */
		bits += GET_MODE_PRECISION (GET_MODE (XEXP (rhs, 0)));

	      /* We can only widen multiplies if the result is mathematiclly
		 equivalent.  I.e. if overflow was impossible.  */
	      if (bits <= GET_MODE_PRECISION (GET_MODE (op)))
		return simplify_gen_binary
			 (MULT, mode,
			  simplify_gen_unary (SIGN_EXTEND, mode, lhs, lmode),
			  simplify_gen_unary (SIGN_EXTEND, mode, rhs, rmode));
	    }
	}

      /* Check for a sign extension of a subreg of a promoted
	 variable, where the promotion is sign-extended, and the
	 target mode is the same as the variable's promotion.  */
      if (GET_CODE (op) == SUBREG
	  && SUBREG_PROMOTED_VAR_P (op)
	  && SUBREG_PROMOTED_SIGNED_P (op)
	  && !paradoxical_subreg_p (mode, GET_MODE (SUBREG_REG (op))))
	{
	  temp = rtl_hooks.gen_lowpart_no_emit (mode, op);
	  if (temp)
	    return temp;
	}

      /* (sign_extend:M (sign_extend:N <X>)) is (sign_extend:M <X>).
	 (sign_extend:M (zero_extend:N <X>)) is (zero_extend:M <X>).  */
      if (GET_CODE (op) == SIGN_EXTEND || GET_CODE (op) == ZERO_EXTEND)
	{
	  gcc_assert (GET_MODE_PRECISION (mode)
		      > GET_MODE_PRECISION (GET_MODE (op)));
	  return simplify_gen_unary (GET_CODE (op), mode, XEXP (op, 0),
				     GET_MODE (XEXP (op, 0)));
	}

      /* (sign_extend:M (ashiftrt:N (ashift <X> (const_int I)) (const_int I)))
	 is (sign_extend:M (subreg:O <X>)) if there is mode with
	 GET_MODE_BITSIZE (N) - I bits.
	 (sign_extend:M (lshiftrt:N (ashift <X> (const_int I)) (const_int I)))
	 is similarly (zero_extend:M (subreg:O <X>)).  */
      if ((GET_CODE (op) == ASHIFTRT || GET_CODE (op) == LSHIFTRT)
	  && GET_CODE (XEXP (op, 0)) == ASHIFT
	  && is_a <scalar_int_mode> (mode, &int_mode)
	  && CONST_INT_P (XEXP (op, 1))
	  && XEXP (XEXP (op, 0), 1) == XEXP (op, 1)
	  && (op_mode = as_a <scalar_int_mode> (GET_MODE (op)),
	      GET_MODE_BITSIZE (op_mode) > INTVAL (XEXP (op, 1))))
	{
	  scalar_int_mode tmode;
	  gcc_assert (GET_MODE_BITSIZE (int_mode)
		      > GET_MODE_BITSIZE (op_mode));
	  if (int_mode_for_size (GET_MODE_BITSIZE (op_mode)
				 - INTVAL (XEXP (op, 1)), 1).exists (&tmode))
	    {
	      rtx inner =
		rtl_hooks.gen_lowpart_no_emit (tmode, XEXP (XEXP (op, 0), 0));
	      if (inner)
		return simplify_gen_unary (GET_CODE (op) == ASHIFTRT
					   ? SIGN_EXTEND : ZERO_EXTEND,
					   int_mode, inner, tmode);
	    }
	}

      /* (sign_extend:M (lshiftrt:N <X> (const_int I))) is better as
         (zero_extend:M (lshiftrt:N <X> (const_int I))) if I is not 0.  */
      if (GET_CODE (op) == LSHIFTRT
	  && CONST_INT_P (XEXP (op, 1))
	  && XEXP (op, 1) != const0_rtx)
	return simplify_gen_unary (ZERO_EXTEND, mode, op, GET_MODE (op));

#if defined(POINTERS_EXTEND_UNSIGNED)
      /* As we do not know which address space the pointer is referring to,
	 we can do this only if the target does not support different pointer
	 or address modes depending on the address space.  */
      if (target_default_pointer_address_modes_p ()
	  && ! POINTERS_EXTEND_UNSIGNED
	  && mode == Pmode && GET_MODE (op) == ptr_mode
	  && (CONSTANT_P (op)
	      || (GET_CODE (op) == SUBREG
		  && REG_P (SUBREG_REG (op))
		  && REG_POINTER (SUBREG_REG (op))
		  && GET_MODE (SUBREG_REG (op)) == Pmode))
	  && !targetm.have_ptr_extend ())
	{
	  temp
	    = convert_memory_address_addr_space_1 (Pmode, op,
						   ADDR_SPACE_GENERIC, false,
						   true);
	  if (temp)
	    return temp;
	}
#endif
      break;

    case ZERO_EXTEND:
      /* Check for a zero extension of a subreg of a promoted
	 variable, where the promotion is zero-extended, and the
	 target mode is the same as the variable's promotion.  */
      if (GET_CODE (op) == SUBREG
	  && SUBREG_PROMOTED_VAR_P (op)
	  && SUBREG_PROMOTED_UNSIGNED_P (op)
	  && !paradoxical_subreg_p (mode, GET_MODE (SUBREG_REG (op))))
	{
	  temp = rtl_hooks.gen_lowpart_no_emit (mode, op);
	  if (temp)
	    return temp;
	}

      /* Extending a widening multiplication should be canonicalized to
	 a wider widening multiplication.  */
      if (GET_CODE (op) == MULT)
	{
	  rtx lhs = XEXP (op, 0);
	  rtx rhs = XEXP (op, 1);
	  enum rtx_code lcode = GET_CODE (lhs);
	  enum rtx_code rcode = GET_CODE (rhs);

	  /* Widening multiplies usually extend both operands, but sometimes
	     they use a shift to extract a portion of a register.  */
	  if ((lcode == ZERO_EXTEND
	       || (lcode == LSHIFTRT && CONST_INT_P (XEXP (lhs, 1))))
	      && (rcode == ZERO_EXTEND
		  || (rcode == LSHIFTRT && CONST_INT_P (XEXP (rhs, 1)))))
	    {
	      machine_mode lmode = GET_MODE (lhs);
	      machine_mode rmode = GET_MODE (rhs);
	      int bits;

	      if (lcode == LSHIFTRT)
		/* Number of bits not shifted off the end.  */
		bits = GET_MODE_PRECISION (lmode) - INTVAL (XEXP (lhs, 1));
	      else /* lcode == ZERO_EXTEND */
		/* Size of inner mode.  */
		bits = GET_MODE_PRECISION (GET_MODE (XEXP (lhs, 0)));

	      if (rcode == LSHIFTRT)
		bits += GET_MODE_PRECISION (rmode) - INTVAL (XEXP (rhs, 1));
	      else /* rcode == ZERO_EXTEND */
		bits += GET_MODE_PRECISION (GET_MODE (XEXP (rhs, 0)));

	      /* We can only widen multiplies if the result is mathematiclly
		 equivalent.  I.e. if overflow was impossible.  */
	      if (bits <= GET_MODE_PRECISION (GET_MODE (op)))
		return simplify_gen_binary
			 (MULT, mode,
			  simplify_gen_unary (ZERO_EXTEND, mode, lhs, lmode),
			  simplify_gen_unary (ZERO_EXTEND, mode, rhs, rmode));
	    }
	}

      /* (zero_extend:M (zero_extend:N <X>)) is (zero_extend:M <X>).  */
      if (GET_CODE (op) == ZERO_EXTEND)
	return simplify_gen_unary (ZERO_EXTEND, mode, XEXP (op, 0),
				   GET_MODE (XEXP (op, 0)));

      /* (zero_extend:M (lshiftrt:N (ashift <X> (const_int I)) (const_int I)))
	 is (zero_extend:M (subreg:O <X>)) if there is mode with
	 GET_MODE_PRECISION (N) - I bits.  */
      if (GET_CODE (op) == LSHIFTRT
	  && GET_CODE (XEXP (op, 0)) == ASHIFT
	  && is_a <scalar_int_mode> (mode, &int_mode)
	  && CONST_INT_P (XEXP (op, 1))
	  && XEXP (XEXP (op, 0), 1) == XEXP (op, 1)
	  && (op_mode = as_a <scalar_int_mode> (GET_MODE (op)),
	      GET_MODE_PRECISION (op_mode) > INTVAL (XEXP (op, 1))))
	{
	  scalar_int_mode tmode;
	  if (int_mode_for_size (GET_MODE_PRECISION (op_mode)
				 - INTVAL (XEXP (op, 1)), 1).exists (&tmode))
	    {
	      rtx inner =
		rtl_hooks.gen_lowpart_no_emit (tmode, XEXP (XEXP (op, 0), 0));
	      if (inner)
		return simplify_gen_unary (ZERO_EXTEND, int_mode,
					   inner, tmode);
	    }
	}

      /* (zero_extend:M (subreg:N <X:O>)) is <X:O> (for M == O) or
	 (zero_extend:M <X:O>), if X doesn't have any non-zero bits outside
	 of mode N.  E.g.
	 (zero_extend:SI (subreg:QI (and:SI (reg:SI) (const_int 63)) 0)) is
	 (and:SI (reg:SI) (const_int 63)).  */
      if (partial_subreg_p (op)
	  && is_a <scalar_int_mode> (mode, &int_mode)
	  && is_a <scalar_int_mode> (GET_MODE (SUBREG_REG (op)), &op0_mode)
	  && GET_MODE_PRECISION (op0_mode) <= HOST_BITS_PER_WIDE_INT
	  && GET_MODE_PRECISION (int_mode) >= GET_MODE_PRECISION (op0_mode)
	  && subreg_lowpart_p (op)
	  && (nonzero_bits (SUBREG_REG (op), op0_mode)
	      & ~GET_MODE_MASK (GET_MODE (op))) == 0)
	{
	  if (GET_MODE_PRECISION (int_mode) == GET_MODE_PRECISION (op0_mode))
	    return SUBREG_REG (op);
	  return simplify_gen_unary (ZERO_EXTEND, int_mode, SUBREG_REG (op),
				     op0_mode);
	}

#if defined(POINTERS_EXTEND_UNSIGNED)
      /* As we do not know which address space the pointer is referring to,
	 we can do this only if the target does not support different pointer
	 or address modes depending on the address space.  */
      if (target_default_pointer_address_modes_p ()
	  && POINTERS_EXTEND_UNSIGNED > 0
	  && mode == Pmode && GET_MODE (op) == ptr_mode
	  && (CONSTANT_P (op)
	      || (GET_CODE (op) == SUBREG
		  && REG_P (SUBREG_REG (op))
		  && REG_POINTER (SUBREG_REG (op))
		  && GET_MODE (SUBREG_REG (op)) == Pmode))
	  && !targetm.have_ptr_extend ())
	{
	  temp
	    = convert_memory_address_addr_space_1 (Pmode, op,
						   ADDR_SPACE_GENERIC, false,
						   true);
	  if (temp)
	    return temp;
	}
#endif
      break;

    default:
      break;
    }

  return 0;
}

/* Try to compute the value of a unary operation CODE whose output mode is to
   be MODE with input operand OP whose mode was originally OP_MODE.
   Return zero if the value cannot be computed.  */
rtx
simplify_const_unary_operation (enum rtx_code code, machine_mode mode,
				rtx op, machine_mode op_mode)
{
  scalar_int_mode result_mode;

  if (code == VEC_DUPLICATE)
    {
      gcc_assert (VECTOR_MODE_P (mode));
      if (GET_MODE (op) != VOIDmode)
      {
	if (!VECTOR_MODE_P (GET_MODE (op)))
	  gcc_assert (GET_MODE_INNER (mode) == GET_MODE (op));
	else
	  gcc_assert (GET_MODE_INNER (mode) == GET_MODE_INNER
						(GET_MODE (op)));
      }
      if (CONST_SCALAR_INT_P (op) || CONST_DOUBLE_AS_FLOAT_P (op)
	  || GET_CODE (op) == CONST_VECTOR)
	{
	  int elt_size = GET_MODE_UNIT_SIZE (mode);
          unsigned n_elts = (GET_MODE_SIZE (mode) / elt_size);
	  rtvec v = rtvec_alloc (n_elts);
	  unsigned int i;

	  if (GET_CODE (op) != CONST_VECTOR)
	    for (i = 0; i < n_elts; i++)
	      RTVEC_ELT (v, i) = op;
	  else
	    {
	      machine_mode inmode = GET_MODE (op);
	      int in_elt_size = GET_MODE_UNIT_SIZE (inmode);
              unsigned in_n_elts = (GET_MODE_SIZE (inmode) / in_elt_size);

	      gcc_assert (in_n_elts < n_elts);
	      gcc_assert ((n_elts % in_n_elts) == 0);
	      for (i = 0; i < n_elts; i++)
	        RTVEC_ELT (v, i) = CONST_VECTOR_ELT (op, i % in_n_elts);
	    }
	  return gen_rtx_CONST_VECTOR (mode, v);
	}
    }

  if (VECTOR_MODE_P (mode) && GET_CODE (op) == CONST_VECTOR)
    {
      int elt_size = GET_MODE_UNIT_SIZE (mode);
      unsigned n_elts = (GET_MODE_SIZE (mode) / elt_size);
      machine_mode opmode = GET_MODE (op);
      int op_elt_size = GET_MODE_UNIT_SIZE (opmode);
      unsigned op_n_elts = (GET_MODE_SIZE (opmode) / op_elt_size);
      rtvec v = rtvec_alloc (n_elts);
      unsigned int i;

      gcc_assert (op_n_elts == n_elts);
      for (i = 0; i < n_elts; i++)
	{
	  rtx x = simplify_unary_operation (code, GET_MODE_INNER (mode),
					    CONST_VECTOR_ELT (op, i),
					    GET_MODE_INNER (opmode));
	  if (!x)
	    return 0;
	  RTVEC_ELT (v, i) = x;
	}
      return gen_rtx_CONST_VECTOR (mode, v);
    }

  /* The order of these tests is critical so that, for example, we don't
     check the wrong mode (input vs. output) for a conversion operation,
     such as FIX.  At some point, this should be simplified.  */

  if (code == FLOAT && CONST_SCALAR_INT_P (op))
    {
      REAL_VALUE_TYPE d;

      if (op_mode == VOIDmode)
	{
	  /* CONST_INT have VOIDmode as the mode.  We assume that all
	     the bits of the constant are significant, though, this is
	     a dangerous assumption as many times CONST_INTs are
	     created and used with garbage in the bits outside of the
	     precision of the implied mode of the const_int.  */
	  op_mode = MAX_MODE_INT;
	}

      real_from_integer (&d, mode, rtx_mode_t (op, op_mode), SIGNED);

      /* Avoid the folding if flag_signaling_nans is on and
         operand is a signaling NaN.  */
      if (HONOR_SNANS (mode) && REAL_VALUE_ISSIGNALING_NAN (d))
        return 0;

      d = real_value_truncate (mode, d);
      return const_double_from_real_value (d, mode);
    }
  else if (code == UNSIGNED_FLOAT && CONST_SCALAR_INT_P (op))
    {
      REAL_VALUE_TYPE d;

      if (op_mode == VOIDmode)
	{
	  /* CONST_INT have VOIDmode as the mode.  We assume that all
	     the bits of the constant are significant, though, this is
	     a dangerous assumption as many times CONST_INTs are
	     created and used with garbage in the bits outside of the
	     precision of the implied mode of the const_int.  */
	  op_mode = MAX_MODE_INT;
	}

      real_from_integer (&d, mode, rtx_mode_t (op, op_mode), UNSIGNED);

      /* Avoid the folding if flag_signaling_nans is on and
         operand is a signaling NaN.  */
      if (HONOR_SNANS (mode) && REAL_VALUE_ISSIGNALING_NAN (d))
        return 0;

      d = real_value_truncate (mode, d);
      return const_double_from_real_value (d, mode);
    }

  if (CONST_SCALAR_INT_P (op) && is_a <scalar_int_mode> (mode, &result_mode))
    {
      unsigned int width = GET_MODE_PRECISION (result_mode);
      wide_int result;
      scalar_int_mode imode = (op_mode == VOIDmode
			       ? result_mode
			       : as_a <scalar_int_mode> (op_mode));
      rtx_mode_t op0 = rtx_mode_t (op, imode);
      int int_value;

#if TARGET_SUPPORTS_WIDE_INT == 0
      /* This assert keeps the simplification from producing a result
	 that cannot be represented in a CONST_DOUBLE but a lot of
	 upstream callers expect that this function never fails to
	 simplify something and so you if you added this to the test
	 above the code would die later anyway.  If this assert
	 happens, you just need to make the port support wide int.  */
      gcc_assert (width <= HOST_BITS_PER_DOUBLE_INT);
#endif

      switch (code)
	{
	case NOT:
	  result = wi::bit_not (op0);
	  break;

	case NEG:
	  result = wi::neg (op0);
	  break;

	case ABS:
	  result = wi::abs (op0);
	  break;

	case FFS:
	  result = wi::shwi (wi::ffs (op0), result_mode);
	  break;

	case CLZ:
	  if (wi::ne_p (op0, 0))
	    int_value = wi::clz (op0);
	  else if (! CLZ_DEFINED_VALUE_AT_ZERO (imode, int_value))
	    int_value = GET_MODE_PRECISION (imode);
	  result = wi::shwi (int_value, result_mode);
	  break;

	case CLRSB:
	  result = wi::shwi (wi::clrsb (op0), result_mode);
	  break;

	case CTZ:
	  if (wi::ne_p (op0, 0))
	    int_value = wi::ctz (op0);
	  else if (! CTZ_DEFINED_VALUE_AT_ZERO (imode, int_value))
	    int_value = GET_MODE_PRECISION (imode);
	  result = wi::shwi (int_value, result_mode);
	  break;

	case POPCOUNT:
	  result = wi::shwi (wi::popcount (op0), result_mode);
	  break;

	case PARITY:
	  result = wi::shwi (wi::parity (op0), result_mode);
	  break;

	case BSWAP:
	  result = wide_int (op0).bswap ();
	  break;

	case TRUNCATE:
	case ZERO_EXTEND:
	  result = wide_int::from (op0, width, UNSIGNED);
	  break;

	case SIGN_EXTEND:
	  result = wide_int::from (op0, width, SIGNED);
	  break;

	case SQRT:
	default:
	  return 0;
	}

      return immed_wide_int_const (result, result_mode);
    }

  else if (CONST_DOUBLE_AS_FLOAT_P (op) 
	   && SCALAR_FLOAT_MODE_P (mode)
	   && SCALAR_FLOAT_MODE_P (GET_MODE (op)))
    {
      REAL_VALUE_TYPE d = *CONST_DOUBLE_REAL_VALUE (op);
      switch (code)
	{
	case SQRT:
	  return 0;
	case ABS:
	  d = real_value_abs (&d);
	  break;
	case NEG:
	  d = real_value_negate (&d);
	  break;
	case FLOAT_TRUNCATE:
	  /* Don't perform the operation if flag_signaling_nans is on
	     and the operand is a signaling NaN.  */
	  if (HONOR_SNANS (mode) && REAL_VALUE_ISSIGNALING_NAN (d))
	    return NULL_RTX;
	  d = real_value_truncate (mode, d);
	  break;
	case FLOAT_EXTEND:
	  /* Don't perform the operation if flag_signaling_nans is on
	     and the operand is a signaling NaN.  */
	  if (HONOR_SNANS (mode) && REAL_VALUE_ISSIGNALING_NAN (d))
	    return NULL_RTX;
	  /* All this does is change the mode, unless changing
	     mode class.  */
	  if (GET_MODE_CLASS (mode) != GET_MODE_CLASS (GET_MODE (op)))
	    real_convert (&d, mode, &d);
	  break;
	case FIX:
	  /* Don't perform the operation if flag_signaling_nans is on
	     and the operand is a signaling NaN.  */
	  if (HONOR_SNANS (mode) && REAL_VALUE_ISSIGNALING_NAN (d))
	    return NULL_RTX;
	  real_arithmetic (&d, FIX_TRUNC_EXPR, &d, NULL);
	  break;
	case NOT:
	  {
	    long tmp[4];
	    int i;

	    real_to_target (tmp, &d, GET_MODE (op));
	    for (i = 0; i < 4; i++)
	      tmp[i] = ~tmp[i];
	    real_from_target (&d, tmp, mode);
	    break;
	  }
	default:
	  gcc_unreachable ();
	}
      return const_double_from_real_value (d, mode);
    }
  else if (CONST_DOUBLE_AS_FLOAT_P (op)
	   && SCALAR_FLOAT_MODE_P (GET_MODE (op))
	   && is_int_mode (mode, &result_mode))
    {
      unsigned int width = GET_MODE_PRECISION (result_mode);
      /* Although the overflow semantics of RTL's FIX and UNSIGNED_FIX
	 operators are intentionally left unspecified (to ease implementation
	 by target backends), for consistency, this routine implements the
	 same semantics for constant folding as used by the middle-end.  */

      /* This was formerly used only for non-IEEE float.
	 eggert@twinsun.com says it is safe for IEEE also.  */
      REAL_VALUE_TYPE t;
      const REAL_VALUE_TYPE *x = CONST_DOUBLE_REAL_VALUE (op);
      wide_int wmax, wmin;
      /* This is part of the abi to real_to_integer, but we check
	 things before making this call.  */
      bool fail;

      switch (code)
	{
	case FIX:
	  if (REAL_VALUE_ISNAN (*x))
	    return const0_rtx;

	  /* Test against the signed upper bound.  */
	  wmax = wi::max_value (width, SIGNED);
	  real_from_integer (&t, VOIDmode, wmax, SIGNED);
	  if (real_less (&t, x))
	    return immed_wide_int_const (wmax, mode);

	  /* Test against the signed lower bound.  */
	  wmin = wi::min_value (width, SIGNED);
	  real_from_integer (&t, VOIDmode, wmin, SIGNED);
	  if (real_less (x, &t))
	    return immed_wide_int_const (wmin, mode);

	  return immed_wide_int_const (real_to_integer (x, &fail, width),
				       mode);

	case UNSIGNED_FIX:
	  if (REAL_VALUE_ISNAN (*x) || REAL_VALUE_NEGATIVE (*x))
	    return const0_rtx;

	  /* Test against the unsigned upper bound.  */
	  wmax = wi::max_value (width, UNSIGNED);
	  real_from_integer (&t, VOIDmode, wmax, UNSIGNED);
	  if (real_less (&t, x))
	    return immed_wide_int_const (wmax, mode);

	  return immed_wide_int_const (real_to_integer (x, &fail, width),
				       mode);

	default:
	  gcc_unreachable ();
	}
    }

  return NULL_RTX;
}

/* Subroutine of simplify_binary_operation to simplify a binary operation
   CODE that can commute with byte swapping, with result mode MODE and
   operating on OP0 and OP1.  CODE is currently one of AND, IOR or XOR.
   Return zero if no simplification or canonicalization is possible.  */

static rtx
simplify_byte_swapping_operation (enum rtx_code code, machine_mode mode,
				  rtx op0, rtx op1)
{
  rtx tem;

  /* (op (bswap x) C1)) -> (bswap (op x C2)) with C2 swapped.  */
  if (GET_CODE (op0) == BSWAP && CONST_SCALAR_INT_P (op1))
    {
      tem = simplify_gen_binary (code, mode, XEXP (op0, 0),
				 simplify_gen_unary (BSWAP, mode, op1, mode));
      return simplify_gen_unary (BSWAP, mode, tem, mode);
    }

  /* (op (bswap x) (bswap y)) -> (bswap (op x y)).  */
  if (GET_CODE (op0) == BSWAP && GET_CODE (op1) == BSWAP)
    {
      tem = simplify_gen_binary (code, mode, XEXP (op0, 0), XEXP (op1, 0));
      return simplify_gen_unary (BSWAP, mode, tem, mode);
    }

  return NULL_RTX;
}

/* Subroutine of simplify_binary_operation to simplify a commutative,
   associative binary operation CODE with result mode MODE, operating
   on OP0 and OP1.  CODE is currently one of PLUS, MULT, AND, IOR, XOR,
   SMIN, SMAX, UMIN or UMAX.  Return zero if no simplification or
   canonicalization is possible.  */

static rtx
simplify_associative_operation (enum rtx_code code, machine_mode mode,
				rtx op0, rtx op1)
{
  rtx tem;

  /* Linearize the operator to the left.  */
  if (GET_CODE (op1) == code)
    {
      /* "(a op b) op (c op d)" becomes "((a op b) op c) op d)".  */
      if (GET_CODE (op0) == code)
	{
	  tem = simplify_gen_binary (code, mode, op0, XEXP (op1, 0));
	  return simplify_gen_binary (code, mode, tem, XEXP (op1, 1));
	}

      /* "a op (b op c)" becomes "(b op c) op a".  */
      if (! swap_commutative_operands_p (op1, op0))
	return simplify_gen_binary (code, mode, op1, op0);

      std::swap (op0, op1);
    }

  if (GET_CODE (op0) == code)
    {
      /* Canonicalize "(x op c) op y" as "(x op y) op c".  */
      if (swap_commutative_operands_p (XEXP (op0, 1), op1))
	{
	  tem = simplify_gen_binary (code, mode, XEXP (op0, 0), op1);
	  return simplify_gen_binary (code, mode, tem, XEXP (op0, 1));
	}

      /* Attempt to simplify "(a op b) op c" as "a op (b op c)".  */
      tem = simplify_binary_operation (code, mode, XEXP (op0, 1), op1);
      if (tem != 0)
        return simplify_gen_binary (code, mode, XEXP (op0, 0), tem);

      /* Attempt to simplify "(a op b) op c" as "(a op c) op b".  */
      tem = simplify_binary_operation (code, mode, XEXP (op0, 0), op1);
      if (tem != 0)
        return simplify_gen_binary (code, mode, tem, XEXP (op0, 1));
    }

  return 0;
}


/* Simplify a binary operation CODE with result mode MODE, operating on OP0
   and OP1.  Return 0 if no simplification is possible.

   Don't use this for relational operations such as EQ or LT.
   Use simplify_relational_operation instead.  */
rtx
simplify_binary_operation (enum rtx_code code, machine_mode mode,
			   rtx op0, rtx op1)
{
  rtx trueop0, trueop1;
  rtx tem;

  /* Relational operations don't work here.  We must know the mode
     of the operands in order to do the comparison correctly.
     Assuming a full word can give incorrect results.
     Consider comparing 128 with -128 in QImode.  */
  gcc_assert (GET_RTX_CLASS (code) != RTX_COMPARE);
  gcc_assert (GET_RTX_CLASS (code) != RTX_COMM_COMPARE);

  /* Make sure the constant is second.  */
  if (GET_RTX_CLASS (code) == RTX_COMM_ARITH
      && swap_commutative_operands_p (op0, op1))
    std::swap (op0, op1);

  trueop0 = avoid_constant_pool_reference (op0);
  trueop1 = avoid_constant_pool_reference (op1);

  tem = simplify_const_binary_operation (code, mode, trueop0, trueop1);
  if (tem)
    return tem;
  tem = simplify_binary_operation_1 (code, mode, op0, op1, trueop0, trueop1);

  if (tem)
    return tem;

  /* If the above steps did not result in a simplification and op0 or op1
     were constant pool references, use the referenced constants directly.  */
  if (trueop0 != op0 || trueop1 != op1)
    return simplify_gen_binary (code, mode, trueop0, trueop1);

  return NULL_RTX;
}

/* Subroutine of simplify_binary_operation.  Simplify a binary operation
   CODE with result mode MODE, operating on OP0 and OP1.  If OP0 and/or
   OP1 are constant pool references, TRUEOP0 and TRUEOP1 represent the
   actual constants.  */

static rtx
simplify_binary_operation_1 (enum rtx_code code, machine_mode mode,
			     rtx op0, rtx op1, rtx trueop0, rtx trueop1)
{
  rtx tem, reversed, opleft, opright;
  HOST_WIDE_INT val;
  unsigned int width = GET_MODE_PRECISION (mode);
  scalar_int_mode int_mode, inner_mode;

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
	return simplify_gen_unary (NEG, mode, XEXP (op0, 0), mode);

      /* Handle both-operands-constant cases.  We can only add
	 CONST_INTs to constants since the sum of relocatable symbols
	 can't be handled by most assemblers.  Don't add CONST_INT
	 to CONST_INT since overflow won't be computed properly if wider
	 than HOST_BITS_PER_WIDE_INT.  */

      if ((GET_CODE (op0) == CONST
	   || GET_CODE (op0) == SYMBOL_REF
	   || GET_CODE (op0) == LABEL_REF)
	  && CONST_INT_P (op1))
	return plus_constant (mode, op0, INTVAL (op1));
      else if ((GET_CODE (op1) == CONST
		|| GET_CODE (op1) == SYMBOL_REF
		|| GET_CODE (op1) == LABEL_REF)
	       && CONST_INT_P (op0))
	return plus_constant (mode, op1, INTVAL (op0));

      /* See if this is something like X * C - X or vice versa or
	 if the multiplication is written as a shift.  If so, we can
	 distribute and make a new multiply, shift, or maybe just
	 have X (if C is 2 in the example above).  But don't make
	 something more expensive than we had before.  */

      if (is_a <scalar_int_mode> (mode, &int_mode))
	{
	  rtx lhs = op0, rhs = op1;

	  wide_int coeff0 = wi::one (GET_MODE_PRECISION (int_mode));
	  wide_int coeff1 = wi::one (GET_MODE_PRECISION (int_mode));

	  if (GET_CODE (lhs) == NEG)
	    {
	      coeff0 = wi::minus_one (GET_MODE_PRECISION (int_mode));
	      lhs = XEXP (lhs, 0);
	    }
	  else if (GET_CODE (lhs) == MULT
		   && CONST_SCALAR_INT_P (XEXP (lhs, 1)))
	    {
	      coeff0 = rtx_mode_t (XEXP (lhs, 1), int_mode);
	      lhs = XEXP (lhs, 0);
	    }
	  else if (GET_CODE (lhs) == ASHIFT
		   && CONST_INT_P (XEXP (lhs, 1))
                   && INTVAL (XEXP (lhs, 1)) >= 0
		   && INTVAL (XEXP (lhs, 1)) < GET_MODE_PRECISION (int_mode))
	    {
	      coeff0 = wi::set_bit_in_zero (INTVAL (XEXP (lhs, 1)),
					    GET_MODE_PRECISION (int_mode));
	      lhs = XEXP (lhs, 0);
	    }

	  if (GET_CODE (rhs) == NEG)
	    {
	      coeff1 = wi::minus_one (GET_MODE_PRECISION (int_mode));
	      rhs = XEXP (rhs, 0);
	    }
	  else if (GET_CODE (rhs) == MULT
		   && CONST_INT_P (XEXP (rhs, 1)))
	    {
	      coeff1 = rtx_mode_t (XEXP (rhs, 1), int_mode);
	      rhs = XEXP (rhs, 0);
	    }
	  else if (GET_CODE (rhs) == ASHIFT
		   && CONST_INT_P (XEXP (rhs, 1))
		   && INTVAL (XEXP (rhs, 1)) >= 0
		   && INTVAL (XEXP (rhs, 1)) < GET_MODE_PRECISION (int_mode))
	    {
	      coeff1 = wi::set_bit_in_zero (INTVAL (XEXP (rhs, 1)),
					    GET_MODE_PRECISION (int_mode));
	      rhs = XEXP (rhs, 0);
	    }

	  if (rtx_equal_p (lhs, rhs))
	    {
	      rtx orig = gen_rtx_PLUS (int_mode, op0, op1);
	      rtx coeff;
	      bool speed = optimize_function_for_speed_p (cfun);

	      coeff = immed_wide_int_const (coeff0 + coeff1, int_mode);

	      tem = simplify_gen_binary (MULT, int_mode, lhs, coeff);
	      return (set_src_cost (tem, int_mode, speed)
		      <= set_src_cost (orig, int_mode, speed) ? tem : 0);
	    }
	}

      /* (plus (xor X C1) C2) is (xor X (C1^C2)) if C2 is signbit.  */
      if (CONST_SCALAR_INT_P (op1)
	  && GET_CODE (op0) == XOR
	  && CONST_SCALAR_INT_P (XEXP (op0, 1))
	  && mode_signbit_p (mode, op1))
	return simplify_gen_binary (XOR, mode, XEXP (op0, 0),
				    simplify_gen_binary (XOR, mode, op1,
							 XEXP (op0, 1)));

      /* Canonicalize (plus (mult (neg B) C) A) to (minus A (mult B C)).  */
      if (!HONOR_SIGN_DEPENDENT_ROUNDING (mode)
	  && GET_CODE (op0) == MULT
	  && GET_CODE (XEXP (op0, 0)) == NEG)
	{
	  rtx in1, in2;

	  in1 = XEXP (XEXP (op0, 0), 0);
	  in2 = XEXP (op0, 1);
	  return simplify_gen_binary (MINUS, mode, op1,
				      simplify_gen_binary (MULT, mode,
							   in1, in2));
	}

      /* (plus (comparison A B) C) can become (neg (rev-comp A B)) if
	 C is 1 and STORE_FLAG_VALUE is -1 or if C is -1 and STORE_FLAG_VALUE
	 is 1.  */
      if (COMPARISON_P (op0)
	  && ((STORE_FLAG_VALUE == -1 && trueop1 == const1_rtx)
	      || (STORE_FLAG_VALUE == 1 && trueop1 == constm1_rtx))
	  && (reversed = reversed_comparison (op0, mode)))
	return
	  simplify_gen_unary (NEG, mode, reversed, mode);

      /* If one of the operands is a PLUS or a MINUS, see if we can
	 simplify this by the associative law.
	 Don't use the associative law for floating point.
	 The inaccuracy makes it nonassociative,
	 and subtle programs can break if operations are associated.  */

      if (INTEGRAL_MODE_P (mode)
	  && (plus_minus_operand_p (op0)
	      || plus_minus_operand_p (op1))
	  && (tem = simplify_plus_minus (code, mode, op0, op1)) != 0)
	return tem;

      /* Reassociate floating point addition only when the user
	 specifies associative math operations.  */
      if (FLOAT_MODE_P (mode)
	  && flag_associative_math)
	{
	  tem = simplify_associative_operation (code, mode, op0, op1);
	  if (tem)
	    return tem;
	}
      break;

    case COMPARE:
      /* Convert (compare (gt (flags) 0) (lt (flags) 0)) to (flags).  */
      if (((GET_CODE (op0) == GT && GET_CODE (op1) == LT)
	   || (GET_CODE (op0) == GTU && GET_CODE (op1) == LTU))
	  && XEXP (op0, 1) == const0_rtx && XEXP (op1, 1) == const0_rtx)
	{
	  rtx xop00 = XEXP (op0, 0);
	  rtx xop10 = XEXP (op1, 0);

	  if (GET_CODE (xop00) == CC0 && GET_CODE (xop10) == CC0)
	      return xop00;

	    if (REG_P (xop00) && REG_P (xop10)
		&& REGNO (xop00) == REGNO (xop10)
		&& GET_MODE (xop00) == mode
		&& GET_MODE (xop10) == mode
		&& GET_MODE_CLASS (mode) == MODE_CC)
	      return xop00;
	}
      break;

    case MINUS:
      /* We can't assume x-x is 0 even with non-IEEE floating point,
	 but since it is zero except in very strange circumstances, we
	 will treat it as zero with -ffinite-math-only.  */
      if (rtx_equal_p (trueop0, trueop1)
	  && ! side_effects_p (op0)
	  && (!FLOAT_MODE_P (mode) || !HONOR_NANS (mode)))
	return CONST0_RTX (mode);

      /* Change subtraction from zero into negation.  (0 - x) is the
	 same as -x when x is NaN, infinite, or finite and nonzero.
	 But if the mode has signed zeros, and does not round towards
	 -infinity, then 0 - 0 is 0, not -0.  */
      if (!HONOR_SIGNED_ZEROS (mode) && trueop0 == CONST0_RTX (mode))
	return simplify_gen_unary (NEG, mode, op1, mode);

      /* (-1 - a) is ~a, unless the expression contains symbolic
	 constants, in which case not retaining additions and
	 subtractions could cause invalid assembly to be produced.  */
      if (trueop0 == constm1_rtx
	  && !contains_symbolic_reference_p (op1))
	return simplify_gen_unary (NOT, mode, op1, mode);

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
	 something more expensive than we had before.  */

      if (is_a <scalar_int_mode> (mode, &int_mode))
	{
	  rtx lhs = op0, rhs = op1;

	  wide_int coeff0 = wi::one (GET_MODE_PRECISION (int_mode));
	  wide_int negcoeff1 = wi::minus_one (GET_MODE_PRECISION (int_mode));

	  if (GET_CODE (lhs) == NEG)
	    {
	      coeff0 = wi::minus_one (GET_MODE_PRECISION (int_mode));
	      lhs = XEXP (lhs, 0);
	    }
	  else if (GET_CODE (lhs) == MULT
		   && CONST_SCALAR_INT_P (XEXP (lhs, 1)))
	    {
	      coeff0 = rtx_mode_t (XEXP (lhs, 1), int_mode);
	      lhs = XEXP (lhs, 0);
	    }
	  else if (GET_CODE (lhs) == ASHIFT
		   && CONST_INT_P (XEXP (lhs, 1))
		   && INTVAL (XEXP (lhs, 1)) >= 0
		   && INTVAL (XEXP (lhs, 1)) < GET_MODE_PRECISION (int_mode))
	    {
	      coeff0 = wi::set_bit_in_zero (INTVAL (XEXP (lhs, 1)),
					    GET_MODE_PRECISION (int_mode));
	      lhs = XEXP (lhs, 0);
	    }

	  if (GET_CODE (rhs) == NEG)
	    {
	      negcoeff1 = wi::one (GET_MODE_PRECISION (int_mode));
	      rhs = XEXP (rhs, 0);
	    }
	  else if (GET_CODE (rhs) == MULT
		   && CONST_INT_P (XEXP (rhs, 1)))
	    {
	      negcoeff1 = wi::neg (rtx_mode_t (XEXP (rhs, 1), int_mode));
	      rhs = XEXP (rhs, 0);
	    }
	  else if (GET_CODE (rhs) == ASHIFT
		   && CONST_INT_P (XEXP (rhs, 1))
		   && INTVAL (XEXP (rhs, 1)) >= 0
		   && INTVAL (XEXP (rhs, 1)) < GET_MODE_PRECISION (int_mode))
	    {
	      negcoeff1 = wi::set_bit_in_zero (INTVAL (XEXP (rhs, 1)),
					       GET_MODE_PRECISION (int_mode));
	      negcoeff1 = -negcoeff1;
	      rhs = XEXP (rhs, 0);
	    }

	  if (rtx_equal_p (lhs, rhs))
	    {
	      rtx orig = gen_rtx_MINUS (int_mode, op0, op1);
	      rtx coeff;
	      bool speed = optimize_function_for_speed_p (cfun);

	      coeff = immed_wide_int_const (coeff0 + negcoeff1, int_mode);

	      tem = simplify_gen_binary (MULT, int_mode, lhs, coeff);
	      return (set_src_cost (tem, int_mode, speed)
		      <= set_src_cost (orig, int_mode, speed) ? tem : 0);
	    }
	}

      /* (a - (-b)) -> (a + b).  True even for IEEE.  */
      if (GET_CODE (op1) == NEG)
	return simplify_gen_binary (PLUS, mode, op0, XEXP (op1, 0));

      /* (-x - c) may be simplified as (-c - x).  */
      if (GET_CODE (op0) == NEG
	  && (CONST_SCALAR_INT_P (op1) || CONST_DOUBLE_AS_FLOAT_P (op1)))
	{
	  tem = simplify_unary_operation (NEG, mode, op1, mode);
	  if (tem)
	    return simplify_gen_binary (MINUS, mode, tem, XEXP (op0, 0));
	}

      /* Don't let a relocatable value get a negative coeff.  */
      if (CONST_INT_P (op1) && GET_MODE (op0) != VOIDmode)
	return simplify_gen_binary (PLUS, mode,
				    op0,
				    neg_const_int (mode, op1));

      /* (x - (x & y)) -> (x & ~y) */
      if (INTEGRAL_MODE_P (mode) && GET_CODE (op1) == AND)
	{
	  if (rtx_equal_p (op0, XEXP (op1, 0)))
	    {
	      tem = simplify_gen_unary (NOT, mode, XEXP (op1, 1),
					GET_MODE (XEXP (op1, 1)));
	      return simplify_gen_binary (AND, mode, op0, tem);
	    }
	  if (rtx_equal_p (op0, XEXP (op1, 1)))
	    {
	      tem = simplify_gen_unary (NOT, mode, XEXP (op1, 0),
					GET_MODE (XEXP (op1, 0)));
	      return simplify_gen_binary (AND, mode, op0, tem);
	    }
	}

      /* If STORE_FLAG_VALUE is 1, (minus 1 (comparison foo bar)) can be done
	 by reversing the comparison code if valid.  */
      if (STORE_FLAG_VALUE == 1
	  && trueop0 == const1_rtx
	  && COMPARISON_P (op1)
	  && (reversed = reversed_comparison (op1, mode)))
	return reversed;

      /* Canonicalize (minus A (mult (neg B) C)) to (plus (mult B C) A).  */
      if (!HONOR_SIGN_DEPENDENT_ROUNDING (mode)
	  && GET_CODE (op1) == MULT
	  && GET_CODE (XEXP (op1, 0)) == NEG)
	{
	  rtx in1, in2;

	  in1 = XEXP (XEXP (op1, 0), 0);
	  in2 = XEXP (op1, 1);
	  return simplify_gen_binary (PLUS, mode,
				      simplify_gen_binary (MULT, mode,
							   in1, in2),
				      op0);
	}

      /* Canonicalize (minus (neg A) (mult B C)) to
	 (minus (mult (neg B) C) A).  */
      if (!HONOR_SIGN_DEPENDENT_ROUNDING (mode)
	  && GET_CODE (op1) == MULT
	  && GET_CODE (op0) == NEG)
	{
	  rtx in1, in2;

	  in1 = simplify_gen_unary (NEG, mode, XEXP (op1, 0), mode);
	  in2 = XEXP (op1, 1);
	  return simplify_gen_binary (MINUS, mode,
				      simplify_gen_binary (MULT, mode,
							   in1, in2),
				      XEXP (op0, 0));
	}

      /* If one of the operands is a PLUS or a MINUS, see if we can
	 simplify this by the associative law.  This will, for example,
         canonicalize (minus A (plus B C)) to (minus (minus A B) C).
	 Don't use the associative law for floating point.
	 The inaccuracy makes it nonassociative,
	 and subtle programs can break if operations are associated.  */

      if (INTEGRAL_MODE_P (mode)
	  && (plus_minus_operand_p (op0)
	      || plus_minus_operand_p (op1))
	  && (tem = simplify_plus_minus (code, mode, op0, op1)) != 0)
	return tem;
      break;

    case MULT:
      if (trueop1 == constm1_rtx)
	return simplify_gen_unary (NEG, mode, op0, mode);

      if (GET_CODE (op0) == NEG)
	{
	  rtx temp = simplify_unary_operation (NEG, mode, op1, mode);
	  /* If op1 is a MULT as well and simplify_unary_operation
	     just moved the NEG to the second operand, simplify_gen_binary
	     below could through simplify_associative_operation move
	     the NEG around again and recurse endlessly.  */
	  if (temp
	      && GET_CODE (op1) == MULT
	      && GET_CODE (temp) == MULT
	      && XEXP (op1, 0) == XEXP (temp, 0)
	      && GET_CODE (XEXP (temp, 1)) == NEG
	      && XEXP (op1, 1) == XEXP (XEXP (temp, 1), 0))
	    temp = NULL_RTX;
	  if (temp)
	    return simplify_gen_binary (MULT, mode, XEXP (op0, 0), temp);
	}
      if (GET_CODE (op1) == NEG)
	{
	  rtx temp = simplify_unary_operation (NEG, mode, op0, mode);
	  /* If op0 is a MULT as well and simplify_unary_operation
	     just moved the NEG to the second operand, simplify_gen_binary
	     below could through simplify_associative_operation move
	     the NEG around again and recurse endlessly.  */
	  if (temp
	      && GET_CODE (op0) == MULT
	      && GET_CODE (temp) == MULT
	      && XEXP (op0, 0) == XEXP (temp, 0)
	      && GET_CODE (XEXP (temp, 1)) == NEG
	      && XEXP (op0, 1) == XEXP (XEXP (temp, 1), 0))
	    temp = NULL_RTX;
	  if (temp)
	    return simplify_gen_binary (MULT, mode, temp, XEXP (op1, 0));
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

      /* Convert multiply by constant power of two into shift.  */
      if (CONST_SCALAR_INT_P (trueop1))
	{
	  val = wi::exact_log2 (rtx_mode_t (trueop1, mode));
	  if (val >= 0)
	    return simplify_gen_binary (ASHIFT, mode, op0, GEN_INT (val));
	}

      /* x*2 is x+x and x*(-1) is -x */
      if (CONST_DOUBLE_AS_FLOAT_P (trueop1)
	  && SCALAR_FLOAT_MODE_P (GET_MODE (trueop1))
	  && !DECIMAL_FLOAT_MODE_P (GET_MODE (trueop1))
	  && GET_MODE (op0) == mode)
	{
	  const REAL_VALUE_TYPE *d1 = CONST_DOUBLE_REAL_VALUE (trueop1);

	  if (real_equal (d1, &dconst2))
	    return simplify_gen_binary (PLUS, mode, op0, copy_rtx (op0));

	  if (!HONOR_SNANS (mode)
	      && real_equal (d1, &dconstm1))
	    return simplify_gen_unary (NEG, mode, op0, mode);
	}

      /* Optimize -x * -x as x * x.  */
      if (FLOAT_MODE_P (mode)
	  && GET_CODE (op0) == NEG
	  && GET_CODE (op1) == NEG
	  && rtx_equal_p (XEXP (op0, 0), XEXP (op1, 0))
	  && !side_effects_p (XEXP (op0, 0)))
	return simplify_gen_binary (MULT, mode, XEXP (op0, 0), XEXP (op1, 0));

      /* Likewise, optimize abs(x) * abs(x) as x * x.  */
      if (SCALAR_FLOAT_MODE_P (mode)
	  && GET_CODE (op0) == ABS
	  && GET_CODE (op1) == ABS
	  && rtx_equal_p (XEXP (op0, 0), XEXP (op1, 0))
	  && !side_effects_p (XEXP (op0, 0)))
	return simplify_gen_binary (MULT, mode, XEXP (op0, 0), XEXP (op1, 0));

      /* Reassociate multiplication, but for floating point MULTs
	 only when the user specifies unsafe math optimizations.  */
      if (! FLOAT_MODE_P (mode)
	  || flag_unsafe_math_optimizations)
	{
	  tem = simplify_associative_operation (code, mode, op0, op1);
	  if (tem)
	    return tem;
	}
      break;

    case IOR:
      if (trueop1 == CONST0_RTX (mode))
	return op0;
      if (INTEGRAL_MODE_P (mode)
	  && trueop1 == CONSTM1_RTX (mode)
	  && !side_effects_p (op0))
	return op1;
      if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0))
	return op0;
      /* A | (~A) -> -1 */
      if (((GET_CODE (op0) == NOT && rtx_equal_p (XEXP (op0, 0), op1))
	   || (GET_CODE (op1) == NOT && rtx_equal_p (XEXP (op1, 0), op0)))
	  && ! side_effects_p (op0)
	  && SCALAR_INT_MODE_P (mode))
	return constm1_rtx;

      /* (ior A C) is C if all bits of A that might be nonzero are on in C.  */
      if (CONST_INT_P (op1)
	  && HWI_COMPUTABLE_MODE_P (mode)
	  && (nonzero_bits (op0, mode) & ~UINTVAL (op1)) == 0
	  && !side_effects_p (op0))
	return op1;

      /* Canonicalize (X & C1) | C2.  */
      if (GET_CODE (op0) == AND
	  && CONST_INT_P (trueop1)
	  && CONST_INT_P (XEXP (op0, 1)))
	{
	  HOST_WIDE_INT mask = GET_MODE_MASK (mode);
	  HOST_WIDE_INT c1 = INTVAL (XEXP (op0, 1));
	  HOST_WIDE_INT c2 = INTVAL (trueop1);

	  /* If (C1&C2) == C1, then (X&C1)|C2 becomes C2.  */
	  if ((c1 & c2) == c1
	      && !side_effects_p (XEXP (op0, 0)))
	    return trueop1;

	  /* If (C1|C2) == ~0 then (X&C1)|C2 becomes X|C2.  */
	  if (((c1|c2) & mask) == mask)
	    return simplify_gen_binary (IOR, mode, XEXP (op0, 0), op1);
	}

      /* Convert (A & B) | A to A.  */
      if (GET_CODE (op0) == AND
	  && (rtx_equal_p (XEXP (op0, 0), op1)
	      || rtx_equal_p (XEXP (op0, 1), op1))
	  && ! side_effects_p (XEXP (op0, 0))
	  && ! side_effects_p (XEXP (op0, 1)))
	return op1;

      /* Convert (ior (ashift A CX) (lshiftrt A CY)) where CX+CY equals the
         mode size to (rotate A CX).  */

      if (GET_CODE (op1) == ASHIFT
          || GET_CODE (op1) == SUBREG)
        {
	  opleft = op1;
	  opright = op0;
	}
      else
        {
	  opright = op1;
	  opleft = op0;
	}

      if (GET_CODE (opleft) == ASHIFT && GET_CODE (opright) == LSHIFTRT
          && rtx_equal_p (XEXP (opleft, 0), XEXP (opright, 0))
          && CONST_INT_P (XEXP (opleft, 1))
          && CONST_INT_P (XEXP (opright, 1))
          && (INTVAL (XEXP (opleft, 1)) + INTVAL (XEXP (opright, 1))
              == GET_MODE_PRECISION (mode)))
        return gen_rtx_ROTATE (mode, XEXP (opright, 0), XEXP (opleft, 1));

      /* Same, but for ashift that has been "simplified" to a wider mode
        by simplify_shift_const.  */

      if (GET_CODE (opleft) == SUBREG
	  && is_a <scalar_int_mode> (mode, &int_mode)
	  && is_a <scalar_int_mode> (GET_MODE (SUBREG_REG (opleft)),
				     &inner_mode)
          && GET_CODE (SUBREG_REG (opleft)) == ASHIFT
          && GET_CODE (opright) == LSHIFTRT
          && GET_CODE (XEXP (opright, 0)) == SUBREG
          && SUBREG_BYTE (opleft) == SUBREG_BYTE (XEXP (opright, 0))
	  && GET_MODE_SIZE (int_mode) < GET_MODE_SIZE (inner_mode)
          && rtx_equal_p (XEXP (SUBREG_REG (opleft), 0),
                          SUBREG_REG (XEXP (opright, 0)))
          && CONST_INT_P (XEXP (SUBREG_REG (opleft), 1))
          && CONST_INT_P (XEXP (opright, 1))
	  && (INTVAL (XEXP (SUBREG_REG (opleft), 1))
	      + INTVAL (XEXP (opright, 1))
	      == GET_MODE_PRECISION (int_mode)))
	return gen_rtx_ROTATE (int_mode, XEXP (opright, 0),
			       XEXP (SUBREG_REG (opleft), 1));

      /* If OP0 is (ashiftrt (plus ...) C), it might actually be
         a (sign_extend (plus ...)).  Then check if OP1 is a CONST_INT and
	 the PLUS does not affect any of the bits in OP1: then we can do
	 the IOR as a PLUS and we can associate.  This is valid if OP1
         can be safely shifted left C bits.  */
      if (CONST_INT_P (trueop1) && GET_CODE (op0) == ASHIFTRT
          && GET_CODE (XEXP (op0, 0)) == PLUS
          && CONST_INT_P (XEXP (XEXP (op0, 0), 1))
          && CONST_INT_P (XEXP (op0, 1))
          && INTVAL (XEXP (op0, 1)) < HOST_BITS_PER_WIDE_INT)
        {
	  int count = INTVAL (XEXP (op0, 1));
	  HOST_WIDE_INT mask = UINTVAL (trueop1) << count;

          if (mask >> count == INTVAL (trueop1)
	      && trunc_int_for_mode (mask, mode) == mask
              && (mask & nonzero_bits (XEXP (op0, 0), mode)) == 0)
	    return simplify_gen_binary (ASHIFTRT, mode,
					plus_constant (mode, XEXP (op0, 0),
						       mask),
					XEXP (op0, 1));
        }

      tem = simplify_byte_swapping_operation (code, mode, op0, op1);
      if (tem)
	return tem;

      tem = simplify_associative_operation (code, mode, op0, op1);
      if (tem)
	return tem;
      break;

    case XOR:
      if (trueop1 == CONST0_RTX (mode))
	return op0;
      if (INTEGRAL_MODE_P (mode) && trueop1 == CONSTM1_RTX (mode))
	return simplify_gen_unary (NOT, mode, op0, mode);
      if (rtx_equal_p (trueop0, trueop1)
	  && ! side_effects_p (op0)
	  && GET_MODE_CLASS (mode) != MODE_CC)
	 return CONST0_RTX (mode);

      /* Canonicalize XOR of the most significant bit to PLUS.  */
      if (CONST_SCALAR_INT_P (op1)
	  && mode_signbit_p (mode, op1))
	return simplify_gen_binary (PLUS, mode, op0, op1);
      /* (xor (plus X C1) C2) is (xor X (C1^C2)) if C1 is signbit.  */
      if (CONST_SCALAR_INT_P (op1)
	  && GET_CODE (op0) == PLUS
	  && CONST_SCALAR_INT_P (XEXP (op0, 1))
	  && mode_signbit_p (mode, XEXP (op0, 1)))
	return simplify_gen_binary (XOR, mode, XEXP (op0, 0),
				    simplify_gen_binary (XOR, mode, op1,
							 XEXP (op0, 1)));

      /* If we are XORing two things that have no bits in common,
	 convert them into an IOR.  This helps to detect rotation encoded
	 using those methods and possibly other simplifications.  */

      if (HWI_COMPUTABLE_MODE_P (mode)
	  && (nonzero_bits (op0, mode)
	      & nonzero_bits (op1, mode)) == 0)
	return (simplify_gen_binary (IOR, mode, op0, op1));

      /* Convert (XOR (NOT x) (NOT y)) to (XOR x y).
	 Also convert (XOR (NOT x) y) to (NOT (XOR x y)), similarly for
	 (NOT y).  */
      {
	int num_negated = 0;

	if (GET_CODE (op0) == NOT)
	  num_negated++, op0 = XEXP (op0, 0);
	if (GET_CODE (op1) == NOT)
	  num_negated++, op1 = XEXP (op1, 0);

	if (num_negated == 2)
	  return simplify_gen_binary (XOR, mode, op0, op1);
	else if (num_negated == 1)
	  return simplify_gen_unary (NOT, mode,
				     simplify_gen_binary (XOR, mode, op0, op1),
				     mode);
      }

      /* Convert (xor (and A B) B) to (and (not A) B).  The latter may
	 correspond to a machine insn or result in further simplifications
	 if B is a constant.  */

      if (GET_CODE (op0) == AND
	  && rtx_equal_p (XEXP (op0, 1), op1)
	  && ! side_effects_p (op1))
	return simplify_gen_binary (AND, mode,
				    simplify_gen_unary (NOT, mode,
							XEXP (op0, 0), mode),
				    op1);

      else if (GET_CODE (op0) == AND
	       && rtx_equal_p (XEXP (op0, 0), op1)
	       && ! side_effects_p (op1))
	return simplify_gen_binary (AND, mode,
				    simplify_gen_unary (NOT, mode,
							XEXP (op0, 1), mode),
				    op1);

      /* Given (xor (ior (xor A B) C) D), where B, C and D are
	 constants, simplify to (xor (ior A C) (B&~C)^D), canceling
	 out bits inverted twice and not set by C.  Similarly, given
	 (xor (and (xor A B) C) D), simplify without inverting C in
	 the xor operand: (xor (and A C) (B&C)^D).
      */
      else if ((GET_CODE (op0) == IOR || GET_CODE (op0) == AND)
	       && GET_CODE (XEXP (op0, 0)) == XOR
	       && CONST_INT_P (op1)
	       && CONST_INT_P (XEXP (op0, 1))
	       && CONST_INT_P (XEXP (XEXP (op0, 0), 1)))
	{
	  enum rtx_code op = GET_CODE (op0);
	  rtx a = XEXP (XEXP (op0, 0), 0);
	  rtx b = XEXP (XEXP (op0, 0), 1);
	  rtx c = XEXP (op0, 1);
	  rtx d = op1;
	  HOST_WIDE_INT bval = INTVAL (b);
	  HOST_WIDE_INT cval = INTVAL (c);
	  HOST_WIDE_INT dval = INTVAL (d);
	  HOST_WIDE_INT xcval;

	  if (op == IOR)
	    xcval = ~cval;
	  else
	    xcval = cval;

	  return simplify_gen_binary (XOR, mode,
				      simplify_gen_binary (op, mode, a, c),
				      gen_int_mode ((bval & xcval) ^ dval,
						    mode));
	}

      /* Given (xor (and A B) C), using P^Q == (~P&Q) | (~Q&P),
	 we can transform like this:
            (A&B)^C == ~(A&B)&C | ~C&(A&B)
                    == (~A|~B)&C | ~C&(A&B)    * DeMorgan's Law
                    == ~A&C | ~B&C | A&(~C&B)  * Distribute and re-order
	 Attempt a few simplifications when B and C are both constants.  */
      if (GET_CODE (op0) == AND
	  && CONST_INT_P (op1)
	  && CONST_INT_P (XEXP (op0, 1)))
	{
	  rtx a = XEXP (op0, 0);
	  rtx b = XEXP (op0, 1);
	  rtx c = op1;
	  HOST_WIDE_INT bval = INTVAL (b);
	  HOST_WIDE_INT cval = INTVAL (c);

	  /* Instead of computing ~A&C, we compute its negated value,
	     ~(A|~C).  If it yields -1, ~A&C is zero, so we can
	     optimize for sure.  If it does not simplify, we still try
	     to compute ~A&C below, but since that always allocates
	     RTL, we don't try that before committing to returning a
	     simplified expression.  */
	  rtx n_na_c = simplify_binary_operation (IOR, mode, a,
						  GEN_INT (~cval));

	  if ((~cval & bval) == 0)
	    {
	      rtx na_c = NULL_RTX;
	      if (n_na_c)
		na_c = simplify_gen_unary (NOT, mode, n_na_c, mode);
	      else
		{
		  /* If ~A does not simplify, don't bother: we don't
		     want to simplify 2 operations into 3, and if na_c
		     were to simplify with na, n_na_c would have
		     simplified as well.  */
		  rtx na = simplify_unary_operation (NOT, mode, a, mode);
		  if (na)
		    na_c = simplify_gen_binary (AND, mode, na, c);
		}

	      /* Try to simplify ~A&C | ~B&C.  */
	      if (na_c != NULL_RTX)
		return simplify_gen_binary (IOR, mode, na_c,
					    gen_int_mode (~bval & cval, mode));
	    }
	  else
	    {
	      /* If ~A&C is zero, simplify A&(~C&B) | ~B&C.  */
	      if (n_na_c == CONSTM1_RTX (mode))
		{
		  rtx a_nc_b = simplify_gen_binary (AND, mode, a,
						    gen_int_mode (~cval & bval,
								  mode));
		  return simplify_gen_binary (IOR, mode, a_nc_b,
					      gen_int_mode (~bval & cval,
							    mode));
		}
	    }
	}

      /* If we have (xor (and (xor A B) C) A) with C a constant we can instead
	 do (ior (and A ~C) (and B C)) which is a machine instruction on some
	 machines, and also has shorter instruction path length.  */
      if (GET_CODE (op0) == AND
	  && GET_CODE (XEXP (op0, 0)) == XOR
	  && CONST_INT_P (XEXP (op0, 1))
	  && rtx_equal_p (XEXP (XEXP (op0, 0), 0), trueop1))
	{
	  rtx a = trueop1;
	  rtx b = XEXP (XEXP (op0, 0), 1);
	  rtx c = XEXP (op0, 1);
	  rtx nc = simplify_gen_unary (NOT, mode, c, mode);
	  rtx a_nc = simplify_gen_binary (AND, mode, a, nc);
	  rtx bc = simplify_gen_binary (AND, mode, b, c);
	  return simplify_gen_binary (IOR, mode, a_nc, bc);
	}
      /* Similarly, (xor (and (xor A B) C) B) as (ior (and A C) (and B ~C))  */
      else if (GET_CODE (op0) == AND
	  && GET_CODE (XEXP (op0, 0)) == XOR
	  && CONST_INT_P (XEXP (op0, 1))
	  && rtx_equal_p (XEXP (XEXP (op0, 0), 1), trueop1))
	{
	  rtx a = XEXP (XEXP (op0, 0), 0);
	  rtx b = trueop1;
	  rtx c = XEXP (op0, 1);
	  rtx nc = simplify_gen_unary (NOT, mode, c, mode);
	  rtx b_nc = simplify_gen_binary (AND, mode, b, nc);
	  rtx ac = simplify_gen_binary (AND, mode, a, c);
	  return simplify_gen_binary (IOR, mode, ac, b_nc);
	}

      /* (xor (comparison foo bar) (const_int 1)) can become the reversed
	 comparison if STORE_FLAG_VALUE is 1.  */
      if (STORE_FLAG_VALUE == 1
	  && trueop1 == const1_rtx
	  && COMPARISON_P (op0)
	  && (reversed = reversed_comparison (op0, mode)))
	return reversed;

      /* (lshiftrt foo C) where C is the number of bits in FOO minus 1
	 is (lt foo (const_int 0)), so we can perform the above
	 simplification if STORE_FLAG_VALUE is 1.  */

      if (is_a <scalar_int_mode> (mode, &int_mode)
	  && STORE_FLAG_VALUE == 1
	  && trueop1 == const1_rtx
	  && GET_CODE (op0) == LSHIFTRT
	  && CONST_INT_P (XEXP (op0, 1))
	  && INTVAL (XEXP (op0, 1)) == GET_MODE_PRECISION (int_mode) - 1)
	return gen_rtx_GE (int_mode, XEXP (op0, 0), const0_rtx);

      /* (xor (comparison foo bar) (const_int sign-bit))
	 when STORE_FLAG_VALUE is the sign bit.  */
      if (is_a <scalar_int_mode> (mode, &int_mode)
	  && val_signbit_p (int_mode, STORE_FLAG_VALUE)
	  && trueop1 == const_true_rtx
	  && COMPARISON_P (op0)
	  && (reversed = reversed_comparison (op0, int_mode)))
	return reversed;

      tem = simplify_byte_swapping_operation (code, mode, op0, op1);
      if (tem)
	return tem;

      tem = simplify_associative_operation (code, mode, op0, op1);
      if (tem)
	return tem;
      break;

    case AND:
      if (trueop1 == CONST0_RTX (mode) && ! side_effects_p (op0))
	return trueop1;
      if (INTEGRAL_MODE_P (mode) && trueop1 == CONSTM1_RTX (mode))
	return op0;
      if (HWI_COMPUTABLE_MODE_P (mode))
	{
	  HOST_WIDE_INT nzop0 = nonzero_bits (trueop0, mode);
	  HOST_WIDE_INT nzop1;
	  if (CONST_INT_P (trueop1))
	    {
	      HOST_WIDE_INT val1 = INTVAL (trueop1);
	      /* If we are turning off bits already known off in OP0, we need
		 not do an AND.  */
	      if ((nzop0 & ~val1) == 0)
		return op0;
	    }
	  nzop1 = nonzero_bits (trueop1, mode);
	  /* If we are clearing all the nonzero bits, the result is zero.  */
	  if ((nzop1 & nzop0) == 0
	      && !side_effects_p (op0) && !side_effects_p (op1))
	    return CONST0_RTX (mode);
	}
      if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0)
	  && GET_MODE_CLASS (mode) != MODE_CC)
	return op0;
      /* A & (~A) -> 0 */
      if (((GET_CODE (op0) == NOT && rtx_equal_p (XEXP (op0, 0), op1))
	   || (GET_CODE (op1) == NOT && rtx_equal_p (XEXP (op1, 0), op0)))
	  && ! side_effects_p (op0)
	  && GET_MODE_CLASS (mode) != MODE_CC)
	return CONST0_RTX (mode);

      /* Transform (and (extend X) C) into (zero_extend (and X C)) if
	 there are no nonzero bits of C outside of X's mode.  */
      if ((GET_CODE (op0) == SIGN_EXTEND
	   || GET_CODE (op0) == ZERO_EXTEND)
	  && CONST_INT_P (trueop1)
	  && HWI_COMPUTABLE_MODE_P (mode)
	  && (~GET_MODE_MASK (GET_MODE (XEXP (op0, 0)))
	      & UINTVAL (trueop1)) == 0)
	{
	  machine_mode imode = GET_MODE (XEXP (op0, 0));
	  tem = simplify_gen_binary (AND, imode, XEXP (op0, 0),
				     gen_int_mode (INTVAL (trueop1),
						   imode));
	  return simplify_gen_unary (ZERO_EXTEND, mode, tem, imode);
	}

      /* Transform (and (truncate X) C) into (truncate (and X C)).  This way
	 we might be able to further simplify the AND with X and potentially
	 remove the truncation altogether.  */
      if (GET_CODE (op0) == TRUNCATE && CONST_INT_P (trueop1))
	{
	  rtx x = XEXP (op0, 0);
	  machine_mode xmode = GET_MODE (x);
	  tem = simplify_gen_binary (AND, xmode, x,
				     gen_int_mode (INTVAL (trueop1), xmode));
	  return simplify_gen_unary (TRUNCATE, mode, tem, xmode);
	}

      /* Canonicalize (A | C1) & C2 as (A & C2) | (C1 & C2).  */
      if (GET_CODE (op0) == IOR
	  && CONST_INT_P (trueop1)
	  && CONST_INT_P (XEXP (op0, 1)))
	{
	  HOST_WIDE_INT tmp = INTVAL (trueop1) & INTVAL (XEXP (op0, 1));
	  return simplify_gen_binary (IOR, mode,
				      simplify_gen_binary (AND, mode,
							   XEXP (op0, 0), op1),
				      gen_int_mode (tmp, mode));
	}

      /* Convert (A ^ B) & A to A & (~B) since the latter is often a single
	 insn (and may simplify more).  */
      if (GET_CODE (op0) == XOR
	  && rtx_equal_p (XEXP (op0, 0), op1)
	  && ! side_effects_p (op1))
	return simplify_gen_binary (AND, mode,
				    simplify_gen_unary (NOT, mode,
							XEXP (op0, 1), mode),
				    op1);

      if (GET_CODE (op0) == XOR
	  && rtx_equal_p (XEXP (op0, 1), op1)
	  && ! side_effects_p (op1))
	return simplify_gen_binary (AND, mode,
				    simplify_gen_unary (NOT, mode,
							XEXP (op0, 0), mode),
				    op1);

      /* Similarly for (~(A ^ B)) & A.  */
      if (GET_CODE (op0) == NOT
	  && GET_CODE (XEXP (op0, 0)) == XOR
	  && rtx_equal_p (XEXP (XEXP (op0, 0), 0), op1)
	  && ! side_effects_p (op1))
	return simplify_gen_binary (AND, mode, XEXP (XEXP (op0, 0), 1), op1);

      if (GET_CODE (op0) == NOT
	  && GET_CODE (XEXP (op0, 0)) == XOR
	  && rtx_equal_p (XEXP (XEXP (op0, 0), 1), op1)
	  && ! side_effects_p (op1))
	return simplify_gen_binary (AND, mode, XEXP (XEXP (op0, 0), 0), op1);

      /* Convert (A | B) & A to A.  */
      if (GET_CODE (op0) == IOR
	  && (rtx_equal_p (XEXP (op0, 0), op1)
	      || rtx_equal_p (XEXP (op0, 1), op1))
	  && ! side_effects_p (XEXP (op0, 0))
	  && ! side_effects_p (XEXP (op0, 1)))
	return op1;

      /* For constants M and N, if M == (1LL << cst) - 1 && (N & M) == M,
	 ((A & N) + B) & M -> (A + B) & M
	 Similarly if (N & M) == 0,
	 ((A | N) + B) & M -> (A + B) & M
	 and for - instead of + and/or ^ instead of |.
         Also, if (N & M) == 0, then
	 (A +- N) & M -> A & M.  */
      if (CONST_INT_P (trueop1)
	  && HWI_COMPUTABLE_MODE_P (mode)
	  && ~UINTVAL (trueop1)
	  && (UINTVAL (trueop1) & (UINTVAL (trueop1) + 1)) == 0
	  && (GET_CODE (op0) == PLUS || GET_CODE (op0) == MINUS))
	{
	  rtx pmop[2];
	  int which;

	  pmop[0] = XEXP (op0, 0);
	  pmop[1] = XEXP (op0, 1);

	  if (CONST_INT_P (pmop[1])
	      && (UINTVAL (pmop[1]) & UINTVAL (trueop1)) == 0)
	    return simplify_gen_binary (AND, mode, pmop[0], op1);

	  for (which = 0; which < 2; which++)
	    {
	      tem = pmop[which];
	      switch (GET_CODE (tem))
		{
		case AND:
		  if (CONST_INT_P (XEXP (tem, 1))
		      && (UINTVAL (XEXP (tem, 1)) & UINTVAL (trueop1))
		      == UINTVAL (trueop1))
		    pmop[which] = XEXP (tem, 0);
		  break;
		case IOR:
		case XOR:
		  if (CONST_INT_P (XEXP (tem, 1))
		      && (UINTVAL (XEXP (tem, 1)) & UINTVAL (trueop1)) == 0)
		    pmop[which] = XEXP (tem, 0);
		  break;
		default:
		  break;
		}
	    }

	  if (pmop[0] != XEXP (op0, 0) || pmop[1] != XEXP (op0, 1))
	    {
	      tem = simplify_gen_binary (GET_CODE (op0), mode,
					 pmop[0], pmop[1]);
	      return simplify_gen_binary (code, mode, tem, op1);
	    }
	}

      /* (and X (ior (not X) Y) -> (and X Y) */
      if (GET_CODE (op1) == IOR
	  && GET_CODE (XEXP (op1, 0)) == NOT
	  && rtx_equal_p (op0, XEXP (XEXP (op1, 0), 0)))
       return simplify_gen_binary (AND, mode, op0, XEXP (op1, 1));

      /* (and (ior (not X) Y) X) -> (and X Y) */
      if (GET_CODE (op0) == IOR
	  && GET_CODE (XEXP (op0, 0)) == NOT
	  && rtx_equal_p (op1, XEXP (XEXP (op0, 0), 0)))
	return simplify_gen_binary (AND, mode, op1, XEXP (op0, 1));

      /* (and X (ior Y (not X)) -> (and X Y) */
      if (GET_CODE (op1) == IOR
	  && GET_CODE (XEXP (op1, 1)) == NOT
	  && rtx_equal_p (op0, XEXP (XEXP (op1, 1), 0)))
       return simplify_gen_binary (AND, mode, op0, XEXP (op1, 0));

      /* (and (ior Y (not X)) X) -> (and X Y) */
      if (GET_CODE (op0) == IOR
	  && GET_CODE (XEXP (op0, 1)) == NOT
	  && rtx_equal_p (op1, XEXP (XEXP (op0, 1), 0)))
	return simplify_gen_binary (AND, mode, op1, XEXP (op0, 0));

      tem = simplify_byte_swapping_operation (code, mode, op0, op1);
      if (tem)
	return tem;

      tem = simplify_associative_operation (code, mode, op0, op1);
      if (tem)
	return tem;
      break;

    case UDIV:
      /* 0/x is 0 (or x&0 if x has side-effects).  */
      if (trueop0 == CONST0_RTX (mode)
	  && !cfun->can_throw_non_call_exceptions)
	{
	  if (side_effects_p (op1))
	    return simplify_gen_binary (AND, mode, op1, trueop0);
	  return trueop0;
	}
      /* x/1 is x.  */
      if (trueop1 == CONST1_RTX (mode))
	{
	  tem = rtl_hooks.gen_lowpart_no_emit (mode, op0);
	  if (tem)
	    return tem;
	}
      /* Convert divide by power of two into shift.  */
      if (CONST_INT_P (trueop1)
	  && (val = exact_log2 (UINTVAL (trueop1))) > 0)
	return simplify_gen_binary (LSHIFTRT, mode, op0, GEN_INT (val));
      break;

    case DIV:
      /* Handle floating point and integers separately.  */
      if (SCALAR_FLOAT_MODE_P (mode))
	{
	  /* Maybe change 0.0 / x to 0.0.  This transformation isn't
	     safe for modes with NaNs, since 0.0 / 0.0 will then be
	     NaN rather than 0.0.  Nor is it safe for modes with signed
	     zeros, since dividing 0 by a negative number gives -0.0  */
	  if (trueop0 == CONST0_RTX (mode)
	      && !HONOR_NANS (mode)
	      && !HONOR_SIGNED_ZEROS (mode)
	      && ! side_effects_p (op1))
	    return op0;
	  /* x/1.0 is x.  */
	  if (trueop1 == CONST1_RTX (mode)
	      && !HONOR_SNANS (mode))
	    return op0;

	  if (CONST_DOUBLE_AS_FLOAT_P (trueop1)
	      && trueop1 != CONST0_RTX (mode))
	    {
	      const REAL_VALUE_TYPE *d1 = CONST_DOUBLE_REAL_VALUE (trueop1);

	      /* x/-1.0 is -x.  */
	      if (real_equal (d1, &dconstm1)
		  && !HONOR_SNANS (mode))
		return simplify_gen_unary (NEG, mode, op0, mode);

	      /* Change FP division by a constant into multiplication.
		 Only do this with -freciprocal-math.  */
	      if (flag_reciprocal_math
		  && !real_equal (d1, &dconst0))
		{
		  REAL_VALUE_TYPE d;
		  real_arithmetic (&d, RDIV_EXPR, &dconst1, d1);
		  tem = const_double_from_real_value (d, mode);
		  return simplify_gen_binary (MULT, mode, op0, tem);
		}
	    }
	}
      else if (SCALAR_INT_MODE_P (mode))
	{
	  /* 0/x is 0 (or x&0 if x has side-effects).  */
	  if (trueop0 == CONST0_RTX (mode)
	      && !cfun->can_throw_non_call_exceptions)
	    {
	      if (side_effects_p (op1))
		return simplify_gen_binary (AND, mode, op1, trueop0);
	      return trueop0;
	    }
	  /* x/1 is x.  */
	  if (trueop1 == CONST1_RTX (mode))
	    {
	      tem = rtl_hooks.gen_lowpart_no_emit (mode, op0);
	      if (tem)
		return tem;
	    }
	  /* x/-1 is -x.  */
	  if (trueop1 == constm1_rtx)
	    {
	      rtx x = rtl_hooks.gen_lowpart_no_emit (mode, op0);
	      if (x)
		return simplify_gen_unary (NEG, mode, x, mode);
	    }
	}
      break;

    case UMOD:
      /* 0%x is 0 (or x&0 if x has side-effects).  */
      if (trueop0 == CONST0_RTX (mode))
	{
	  if (side_effects_p (op1))
	    return simplify_gen_binary (AND, mode, op1, trueop0);
	  return trueop0;
	}
      /* x%1 is 0 (of x&0 if x has side-effects).  */
      if (trueop1 == CONST1_RTX (mode))
	{
	  if (side_effects_p (op0))
	    return simplify_gen_binary (AND, mode, op0, CONST0_RTX (mode));
	  return CONST0_RTX (mode);
	}
      /* Implement modulus by power of two as AND.  */
      if (CONST_INT_P (trueop1)
	  && exact_log2 (UINTVAL (trueop1)) > 0)
	return simplify_gen_binary (AND, mode, op0,
				    gen_int_mode (INTVAL (op1) - 1, mode));
      break;

    case MOD:
      /* 0%x is 0 (or x&0 if x has side-effects).  */
      if (trueop0 == CONST0_RTX (mode))
	{
	  if (side_effects_p (op1))
	    return simplify_gen_binary (AND, mode, op1, trueop0);
	  return trueop0;
	}
      /* x%1 and x%-1 is 0 (or x&0 if x has side-effects).  */
      if (trueop1 == CONST1_RTX (mode) || trueop1 == constm1_rtx)
	{
	  if (side_effects_p (op0))
	    return simplify_gen_binary (AND, mode, op0, CONST0_RTX (mode));
	  return CONST0_RTX (mode);
	}
      break;

    case ROTATERT:
    case ROTATE:
      /* Canonicalize rotates by constant amount.  If op1 is bitsize / 2,
	 prefer left rotation, if op1 is from bitsize / 2 + 1 to
	 bitsize - 1, use other direction of rotate with 1 .. bitsize / 2 - 1
	 amount instead.  */
#if defined(HAVE_rotate) && defined(HAVE_rotatert)
      if (CONST_INT_P (trueop1)
	  && IN_RANGE (INTVAL (trueop1),
		       GET_MODE_PRECISION (mode) / 2 + (code == ROTATE),
		       GET_MODE_PRECISION (mode) - 1))
	return simplify_gen_binary (code == ROTATE ? ROTATERT : ROTATE,
				    mode, op0, GEN_INT (GET_MODE_PRECISION (mode)
							- INTVAL (trueop1)));
#endif
      /* FALLTHRU */
    case ASHIFTRT:
      if (trueop1 == CONST0_RTX (mode))
	return op0;
      if (trueop0 == CONST0_RTX (mode) && ! side_effects_p (op1))
	return op0;
      /* Rotating ~0 always results in ~0.  */
      if (CONST_INT_P (trueop0)
	  && HWI_COMPUTABLE_MODE_P (mode)
	  && UINTVAL (trueop0) == GET_MODE_MASK (mode)
	  && ! side_effects_p (op1))
	return op0;

    canonicalize_shift:
      /* Given:
	 scalar modes M1, M2
	 scalar constants c1, c2
	 size (M2) > size (M1)
	 c1 == size (M2) - size (M1)
	 optimize:
	 ([a|l]shiftrt:M1 (subreg:M1 (lshiftrt:M2 (reg:M2) (const_int <c1>))
				 <low_part>)
		      (const_int <c2>))
	 to:
	 (subreg:M1 ([a|l]shiftrt:M2 (reg:M2) (const_int <c1 + c2>))
		    <low_part>).  */
      if ((code == ASHIFTRT || code == LSHIFTRT)
	  && is_a <scalar_int_mode> (mode, &int_mode)
	  && SUBREG_P (op0)
	  && CONST_INT_P (op1)
	  && GET_CODE (SUBREG_REG (op0)) == LSHIFTRT
	  && is_a <scalar_int_mode> (GET_MODE (SUBREG_REG (op0)),
				     &inner_mode)
	  && CONST_INT_P (XEXP (SUBREG_REG (op0), 1))
	  && GET_MODE_BITSIZE (inner_mode) > GET_MODE_BITSIZE (int_mode)
	  && (INTVAL (XEXP (SUBREG_REG (op0), 1))
	      == GET_MODE_BITSIZE (inner_mode) - GET_MODE_BITSIZE (int_mode))
	  && subreg_lowpart_p (op0))
	{
	  rtx tmp = GEN_INT (INTVAL (XEXP (SUBREG_REG (op0), 1))
			     + INTVAL (op1));
	  tmp = simplify_gen_binary (code, inner_mode,
				     XEXP (SUBREG_REG (op0), 0),
				     tmp);
	  return lowpart_subreg (int_mode, tmp, inner_mode);
	}

      if (SHIFT_COUNT_TRUNCATED && CONST_INT_P (op1))
	{
	  val = INTVAL (op1) & (GET_MODE_PRECISION (mode) - 1);
	  if (val != INTVAL (op1))
	    return simplify_gen_binary (code, mode, op0, GEN_INT (val));
	}
      break;

    case ASHIFT:
    case SS_ASHIFT:
    case US_ASHIFT:
      if (trueop1 == CONST0_RTX (mode))
	return op0;
      if (trueop0 == CONST0_RTX (mode) && ! side_effects_p (op1))
	return op0;
      goto canonicalize_shift;

    case LSHIFTRT:
      if (trueop1 == CONST0_RTX (mode))
	return op0;
      if (trueop0 == CONST0_RTX (mode) && ! side_effects_p (op1))
	return op0;
      /* Optimize (lshiftrt (clz X) C) as (eq X 0).  */
      if (GET_CODE (op0) == CLZ
	  && is_a <scalar_int_mode> (GET_MODE (XEXP (op0, 0)), &inner_mode)
	  && CONST_INT_P (trueop1)
	  && STORE_FLAG_VALUE == 1
	  && INTVAL (trueop1) < (HOST_WIDE_INT)width)
	{
	  unsigned HOST_WIDE_INT zero_val = 0;

	  if (CLZ_DEFINED_VALUE_AT_ZERO (inner_mode, zero_val)
	      && zero_val == GET_MODE_PRECISION (inner_mode)
	      && INTVAL (trueop1) == exact_log2 (zero_val))
	    return simplify_gen_relational (EQ, mode, inner_mode,
					    XEXP (op0, 0), const0_rtx);
	}
      goto canonicalize_shift;

    case SMIN:
      if (HWI_COMPUTABLE_MODE_P (mode)
	  && mode_signbit_p (mode, trueop1)
	  && ! side_effects_p (op0))
	return op1;
      if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0))
	return op0;
      tem = simplify_associative_operation (code, mode, op0, op1);
      if (tem)
	return tem;
      break;

    case SMAX:
      if (HWI_COMPUTABLE_MODE_P (mode)
	  && CONST_INT_P (trueop1)
	  && (UINTVAL (trueop1) == GET_MODE_MASK (mode) >> 1)
	  && ! side_effects_p (op0))
	return op1;
      if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0))
	return op0;
      tem = simplify_associative_operation (code, mode, op0, op1);
      if (tem)
	return tem;
      break;

    case UMIN:
      if (trueop1 == CONST0_RTX (mode) && ! side_effects_p (op0))
	return op1;
      if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0))
	return op0;
      tem = simplify_associative_operation (code, mode, op0, op1);
      if (tem)
	return tem;
      break;

    case UMAX:
      if (trueop1 == constm1_rtx && ! side_effects_p (op0))
	return op1;
      if (rtx_equal_p (trueop0, trueop1) && ! side_effects_p (op0))
	return op0;
      tem = simplify_associative_operation (code, mode, op0, op1);
      if (tem)
	return tem;
      break;

    case SS_PLUS:
    case US_PLUS:
    case SS_MINUS:
    case US_MINUS:
    case SS_MULT:
    case US_MULT:
    case SS_DIV:
    case US_DIV:
      /* ??? There are simplifications that can be done.  */
      return 0;

    case VEC_SELECT:
      if (!VECTOR_MODE_P (mode))
	{
	  gcc_assert (VECTOR_MODE_P (GET_MODE (trueop0)));
	  gcc_assert (mode == GET_MODE_INNER (GET_MODE (trueop0)));
	  gcc_assert (GET_CODE (trueop1) == PARALLEL);
	  gcc_assert (XVECLEN (trueop1, 0) == 1);
	  gcc_assert (CONST_INT_P (XVECEXP (trueop1, 0, 0)));

	  if (GET_CODE (trueop0) == CONST_VECTOR)
	    return CONST_VECTOR_ELT (trueop0, INTVAL (XVECEXP
						      (trueop1, 0, 0)));

	  /* Extract a scalar element from a nested VEC_SELECT expression
	     (with optional nested VEC_CONCAT expression).  Some targets
	     (i386) extract scalar element from a vector using chain of
	     nested VEC_SELECT expressions.  When input operand is a memory
	     operand, this operation can be simplified to a simple scalar
	     load from an offseted memory address.  */
	  if (GET_CODE (trueop0) == VEC_SELECT)
	    {
	      rtx op0 = XEXP (trueop0, 0);
	      rtx op1 = XEXP (trueop0, 1);

	      machine_mode opmode = GET_MODE (op0);
	      int elt_size = GET_MODE_UNIT_SIZE (opmode);
	      int n_elts = GET_MODE_SIZE (opmode) / elt_size;

	      int i = INTVAL (XVECEXP (trueop1, 0, 0));
	      int elem;

	      rtvec vec;
	      rtx tmp_op, tmp;

	      gcc_assert (GET_CODE (op1) == PARALLEL);
	      gcc_assert (i < n_elts);

	      /* Select element, pointed by nested selector.  */
	      elem = INTVAL (XVECEXP (op1, 0, i));

	      /* Handle the case when nested VEC_SELECT wraps VEC_CONCAT.  */
	      if (GET_CODE (op0) == VEC_CONCAT)
		{
		  rtx op00 = XEXP (op0, 0);
		  rtx op01 = XEXP (op0, 1);

		  machine_mode mode00, mode01;
		  int n_elts00, n_elts01;

		  mode00 = GET_MODE (op00);
		  mode01 = GET_MODE (op01);

		  /* Find out number of elements of each operand.  */
		  if (VECTOR_MODE_P (mode00))
		    {
		      elt_size = GET_MODE_UNIT_SIZE (mode00);
		      n_elts00 = GET_MODE_SIZE (mode00) / elt_size;
		    }
		  else
		    n_elts00 = 1;

		  if (VECTOR_MODE_P (mode01))
		    {
		      elt_size = GET_MODE_UNIT_SIZE (mode01);
		      n_elts01 = GET_MODE_SIZE (mode01) / elt_size;
		    }
		  else
		    n_elts01 = 1;

		  gcc_assert (n_elts == n_elts00 + n_elts01);

		  /* Select correct operand of VEC_CONCAT
		     and adjust selector. */
		  if (elem < n_elts01)
		    tmp_op = op00;
		  else
		    {
		      tmp_op = op01;
		      elem -= n_elts00;
		    }
		}
	      else
		tmp_op = op0;

	      vec = rtvec_alloc (1);
	      RTVEC_ELT (vec, 0) = GEN_INT (elem);

	      tmp = gen_rtx_fmt_ee (code, mode,
				    tmp_op, gen_rtx_PARALLEL (VOIDmode, vec));
	      return tmp;
	    }
	  if (GET_CODE (trueop0) == VEC_DUPLICATE
	      && GET_MODE (XEXP (trueop0, 0)) == mode)
	    return XEXP (trueop0, 0);
	}
      else
	{
	  gcc_assert (VECTOR_MODE_P (GET_MODE (trueop0)));
	  gcc_assert (GET_MODE_INNER (mode)
		      == GET_MODE_INNER (GET_MODE (trueop0)));
	  gcc_assert (GET_CODE (trueop1) == PARALLEL);

	  if (GET_CODE (trueop0) == CONST_VECTOR)
	    {
	      int elt_size = GET_MODE_UNIT_SIZE (mode);
	      unsigned n_elts = (GET_MODE_SIZE (mode) / elt_size);
	      rtvec v = rtvec_alloc (n_elts);
	      unsigned int i;

	      gcc_assert (XVECLEN (trueop1, 0) == (int) n_elts);
	      for (i = 0; i < n_elts; i++)
		{
		  rtx x = XVECEXP (trueop1, 0, i);

		  gcc_assert (CONST_INT_P (x));
		  RTVEC_ELT (v, i) = CONST_VECTOR_ELT (trueop0,
						       INTVAL (x));
		}

	      return gen_rtx_CONST_VECTOR (mode, v);
	    }

	  /* Recognize the identity.  */
	  if (GET_MODE (trueop0) == mode)
	    {
	      bool maybe_ident = true;
	      for (int i = 0; i < XVECLEN (trueop1, 0); i++)
		{
		  rtx j = XVECEXP (trueop1, 0, i);
		  if (!CONST_INT_P (j) || INTVAL (j) != i)
		    {
		      maybe_ident = false;
		      break;
		    }
		}
	      if (maybe_ident)
		return trueop0;
	    }

	  /* If we build {a,b} then permute it, build the result directly.  */
	  if (XVECLEN (trueop1, 0) == 2
	      && CONST_INT_P (XVECEXP (trueop1, 0, 0))
	      && CONST_INT_P (XVECEXP (trueop1, 0, 1))
	      && GET_CODE (trueop0) == VEC_CONCAT
	      && GET_CODE (XEXP (trueop0, 0)) == VEC_CONCAT
	      && GET_MODE (XEXP (trueop0, 0)) == mode
	      && GET_CODE (XEXP (trueop0, 1)) == VEC_CONCAT
	      && GET_MODE (XEXP (trueop0, 1)) == mode)
	    {
	      unsigned int i0 = INTVAL (XVECEXP (trueop1, 0, 0));
	      unsigned int i1 = INTVAL (XVECEXP (trueop1, 0, 1));
	      rtx subop0, subop1;

	      gcc_assert (i0 < 4 && i1 < 4);
	      subop0 = XEXP (XEXP (trueop0, i0 / 2), i0 % 2);
	      subop1 = XEXP (XEXP (trueop0, i1 / 2), i1 % 2);

	      return simplify_gen_binary (VEC_CONCAT, mode, subop0, subop1);
	    }

	  if (XVECLEN (trueop1, 0) == 2
	      && CONST_INT_P (XVECEXP (trueop1, 0, 0))
	      && CONST_INT_P (XVECEXP (trueop1, 0, 1))
	      && GET_CODE (trueop0) == VEC_CONCAT
	      && GET_MODE (trueop0) == mode)
	    {
	      unsigned int i0 = INTVAL (XVECEXP (trueop1, 0, 0));
	      unsigned int i1 = INTVAL (XVECEXP (trueop1, 0, 1));
	      rtx subop0, subop1;

	      gcc_assert (i0 < 2 && i1 < 2);
	      subop0 = XEXP (trueop0, i0);
	      subop1 = XEXP (trueop0, i1);

	      return simplify_gen_binary (VEC_CONCAT, mode, subop0, subop1);
	    }

	  /* If we select one half of a vec_concat, return that.  */
	  if (GET_CODE (trueop0) == VEC_CONCAT
	      && CONST_INT_P (XVECEXP (trueop1, 0, 0)))
	    {
	      rtx subop0 = XEXP (trueop0, 0);
	      rtx subop1 = XEXP (trueop0, 1);
	      machine_mode mode0 = GET_MODE (subop0);
	      machine_mode mode1 = GET_MODE (subop1);
	      int li = GET_MODE_UNIT_SIZE (mode0);
	      int l0 = GET_MODE_SIZE (mode0) / li;
	      int l1 = GET_MODE_SIZE (mode1) / li;
	      int i0 = INTVAL (XVECEXP (trueop1, 0, 0));
	      if (i0 == 0 && !side_effects_p (op1) && mode == mode0)
		{
		  bool success = true;
		  for (int i = 1; i < l0; ++i)
		    {
		      rtx j = XVECEXP (trueop1, 0, i);
		      if (!CONST_INT_P (j) || INTVAL (j) != i)
			{
			  success = false;
			  break;
			}
		    }
		  if (success)
		    return subop0;
		}
	      if (i0 == l0 && !side_effects_p (op0) && mode == mode1)
		{
		  bool success = true;
		  for (int i = 1; i < l1; ++i)
		    {
		      rtx j = XVECEXP (trueop1, 0, i);
		      if (!CONST_INT_P (j) || INTVAL (j) != i0 + i)
			{
			  success = false;
			  break;
			}
		    }
		  if (success)
		    return subop1;
		}
	    }
	}

      if (XVECLEN (trueop1, 0) == 1
	  && CONST_INT_P (XVECEXP (trueop1, 0, 0))
	  && GET_CODE (trueop0) == VEC_CONCAT)
	{
	  rtx vec = trueop0;
	  int offset = INTVAL (XVECEXP (trueop1, 0, 0)) * GET_MODE_SIZE (mode);

	  /* Try to find the element in the VEC_CONCAT.  */
	  while (GET_MODE (vec) != mode
		 && GET_CODE (vec) == VEC_CONCAT)
	    {
	      HOST_WIDE_INT vec_size;

	      if (CONST_INT_P (XEXP (vec, 0)))
	        {
	          /* vec_concat of two const_ints doesn't make sense with
	             respect to modes.  */
	          if (CONST_INT_P (XEXP (vec, 1)))
	            return 0;

	          vec_size = GET_MODE_SIZE (GET_MODE (trueop0))
	                     - GET_MODE_SIZE (GET_MODE (XEXP (vec, 1)));
	        }
	      else
	        vec_size = GET_MODE_SIZE (GET_MODE (XEXP (vec, 0)));

	      if (offset < vec_size)
		vec = XEXP (vec, 0);
	      else
		{
		  offset -= vec_size;
		  vec = XEXP (vec, 1);
		}
	      vec = avoid_constant_pool_reference (vec);
	    }

	  if (GET_MODE (vec) == mode)
	    return vec;
	}

      /* If we select elements in a vec_merge that all come from the same
	 operand, select from that operand directly.  */
      if (GET_CODE (op0) == VEC_MERGE)
	{
	  rtx trueop02 = avoid_constant_pool_reference (XEXP (op0, 2));
	  if (CONST_INT_P (trueop02))
	    {
	      unsigned HOST_WIDE_INT sel = UINTVAL (trueop02);
	      bool all_operand0 = true;
	      bool all_operand1 = true;
	      for (int i = 0; i < XVECLEN (trueop1, 0); i++)
		{
		  rtx j = XVECEXP (trueop1, 0, i);
		  if (sel & (HOST_WIDE_INT_1U << UINTVAL (j)))
		    all_operand1 = false;
		  else
		    all_operand0 = false;
		}
	      if (all_operand0 && !side_effects_p (XEXP (op0, 1)))
		return simplify_gen_binary (VEC_SELECT, mode, XEXP (op0, 0), op1);
	      if (all_operand1 && !side_effects_p (XEXP (op0, 0)))
		return simplify_gen_binary (VEC_SELECT, mode, XEXP (op0, 1), op1);
	    }
	}

      /* If we have two nested selects that are inverses of each
	 other, replace them with the source operand.  */
      if (GET_CODE (trueop0) == VEC_SELECT
	  && GET_MODE (XEXP (trueop0, 0)) == mode)
	{
	  rtx op0_subop1 = XEXP (trueop0, 1);
	  gcc_assert (GET_CODE (op0_subop1) == PARALLEL);
	  gcc_assert (XVECLEN (trueop1, 0) == GET_MODE_NUNITS (mode));

	  /* Apply the outer ordering vector to the inner one.  (The inner
	     ordering vector is expressly permitted to be of a different
	     length than the outer one.)  If the result is { 0, 1, ..., n-1 }
	     then the two VEC_SELECTs cancel.  */
	  for (int i = 0; i < XVECLEN (trueop1, 0); ++i)
	    {
	      rtx x = XVECEXP (trueop1, 0, i);
	      if (!CONST_INT_P (x))
		return 0;
	      rtx y = XVECEXP (op0_subop1, 0, INTVAL (x));
	      if (!CONST_INT_P (y) || i != INTVAL (y))
		return 0;
	    }
	  return XEXP (trueop0, 0);
	}

      return 0;
    case VEC_CONCAT:
      {
	machine_mode op0_mode = (GET_MODE (trueop0) != VOIDmode
				      ? GET_MODE (trueop0)
				      : GET_MODE_INNER (mode));
	machine_mode op1_mode = (GET_MODE (trueop1) != VOIDmode
				      ? GET_MODE (trueop1)
				      : GET_MODE_INNER (mode));

	gcc_assert (VECTOR_MODE_P (mode));
	gcc_assert (GET_MODE_SIZE (op0_mode) + GET_MODE_SIZE (op1_mode)
		    == GET_MODE_SIZE (mode));

	if (VECTOR_MODE_P (op0_mode))
	  gcc_assert (GET_MODE_INNER (mode)
		      == GET_MODE_INNER (op0_mode));
	else
	  gcc_assert (GET_MODE_INNER (mode) == op0_mode);

	if (VECTOR_MODE_P (op1_mode))
	  gcc_assert (GET_MODE_INNER (mode)
		      == GET_MODE_INNER (op1_mode));
	else
	  gcc_assert (GET_MODE_INNER (mode) == op1_mode);

	if ((GET_CODE (trueop0) == CONST_VECTOR
	     || CONST_SCALAR_INT_P (trueop0) 
	     || CONST_DOUBLE_AS_FLOAT_P (trueop0))
	    && (GET_CODE (trueop1) == CONST_VECTOR
		|| CONST_SCALAR_INT_P (trueop1) 
		|| CONST_DOUBLE_AS_FLOAT_P (trueop1)))
	  {
	    int elt_size = GET_MODE_UNIT_SIZE (mode);
	    unsigned n_elts = (GET_MODE_SIZE (mode) / elt_size);
	    rtvec v = rtvec_alloc (n_elts);
	    unsigned int i;
	    unsigned in_n_elts = 1;

	    if (VECTOR_MODE_P (op0_mode))
	      in_n_elts = (GET_MODE_SIZE (op0_mode) / elt_size);
	    for (i = 0; i < n_elts; i++)
	      {
		if (i < in_n_elts)
		  {
		    if (!VECTOR_MODE_P (op0_mode))
		      RTVEC_ELT (v, i) = trueop0;
		    else
		      RTVEC_ELT (v, i) = CONST_VECTOR_ELT (trueop0, i);
		  }
		else
		  {
		    if (!VECTOR_MODE_P (op1_mode))
		      RTVEC_ELT (v, i) = trueop1;
		    else
		      RTVEC_ELT (v, i) = CONST_VECTOR_ELT (trueop1,
							   i - in_n_elts);
		  }
	      }

	    return gen_rtx_CONST_VECTOR (mode, v);
	  }

	/* Try to merge two VEC_SELECTs from the same vector into a single one.
	   Restrict the transformation to avoid generating a VEC_SELECT with a
	   mode unrelated to its operand.  */
	if (GET_CODE (trueop0) == VEC_SELECT
	    && GET_CODE (trueop1) == VEC_SELECT
	    && rtx_equal_p (XEXP (trueop0, 0), XEXP (trueop1, 0))
	    && GET_MODE (XEXP (trueop0, 0)) == mode)
	  {
	    rtx par0 = XEXP (trueop0, 1);
	    rtx par1 = XEXP (trueop1, 1);
	    int len0 = XVECLEN (par0, 0);
	    int len1 = XVECLEN (par1, 0);
	    rtvec vec = rtvec_alloc (len0 + len1);
	    for (int i = 0; i < len0; i++)
	      RTVEC_ELT (vec, i) = XVECEXP (par0, 0, i);
	    for (int i = 0; i < len1; i++)
	      RTVEC_ELT (vec, len0 + i) = XVECEXP (par1, 0, i);
	    return simplify_gen_binary (VEC_SELECT, mode, XEXP (trueop0, 0),
					gen_rtx_PARALLEL (VOIDmode, vec));
	  }
      }
      return 0;

    default:
      gcc_unreachable ();
    }

  return 0;
}

rtx
simplify_const_binary_operation (enum rtx_code code, machine_mode mode,
				 rtx op0, rtx op1)
{
  if (VECTOR_MODE_P (mode)
      && code != VEC_CONCAT
      && GET_CODE (op0) == CONST_VECTOR
      && GET_CODE (op1) == CONST_VECTOR)
    {
      unsigned n_elts = GET_MODE_NUNITS (mode);
      machine_mode op0mode = GET_MODE (op0);
      unsigned op0_n_elts = GET_MODE_NUNITS (op0mode);
      machine_mode op1mode = GET_MODE (op1);
      unsigned op1_n_elts = GET_MODE_NUNITS (op1mode);
      rtvec v = rtvec_alloc (n_elts);
      unsigned int i;

      gcc_assert (op0_n_elts == n_elts);
      gcc_assert (op1_n_elts == n_elts);
      for (i = 0; i < n_elts; i++)
	{
	  rtx x = simplify_binary_operation (code, GET_MODE_INNER (mode),
					     CONST_VECTOR_ELT (op0, i),
					     CONST_VECTOR_ELT (op1, i));
	  if (!x)
	    return 0;
	  RTVEC_ELT (v, i) = x;
	}

      return gen_rtx_CONST_VECTOR (mode, v);
    }

  if (VECTOR_MODE_P (mode)
      && code == VEC_CONCAT
      && (CONST_SCALAR_INT_P (op0)
	  || GET_CODE (op0) == CONST_FIXED
	  || CONST_DOUBLE_AS_FLOAT_P (op0))
      && (CONST_SCALAR_INT_P (op1)
	  || CONST_DOUBLE_AS_FLOAT_P (op1)
	  || GET_CODE (op1) == CONST_FIXED))
    {
      unsigned n_elts = GET_MODE_NUNITS (mode);
      rtvec v = rtvec_alloc (n_elts);

      gcc_assert (n_elts >= 2);
      if (n_elts == 2)
	{
	  gcc_assert (GET_CODE (op0) != CONST_VECTOR);
	  gcc_assert (GET_CODE (op1) != CONST_VECTOR);

	  RTVEC_ELT (v, 0) = op0;
	  RTVEC_ELT (v, 1) = op1;
	}
      else
	{
	  unsigned op0_n_elts = GET_MODE_NUNITS (GET_MODE (op0));
	  unsigned op1_n_elts = GET_MODE_NUNITS (GET_MODE (op1));
	  unsigned i;

	  gcc_assert (GET_CODE (op0) == CONST_VECTOR);
	  gcc_assert (GET_CODE (op1) == CONST_VECTOR);
	  gcc_assert (op0_n_elts + op1_n_elts == n_elts);

	  for (i = 0; i < op0_n_elts; ++i)
	    RTVEC_ELT (v, i) = XVECEXP (op0, 0, i);
	  for (i = 0; i < op1_n_elts; ++i)
	    RTVEC_ELT (v, op0_n_elts+i) = XVECEXP (op1, 0, i);
	}

      return gen_rtx_CONST_VECTOR (mode, v);
    }

  if (SCALAR_FLOAT_MODE_P (mode)
      && CONST_DOUBLE_AS_FLOAT_P (op0) 
      && CONST_DOUBLE_AS_FLOAT_P (op1)
      && mode == GET_MODE (op0) && mode == GET_MODE (op1))
    {
      if (code == AND
	  || code == IOR
	  || code == XOR)
	{
	  long tmp0[4];
	  long tmp1[4];
	  REAL_VALUE_TYPE r;
	  int i;

	  real_to_target (tmp0, CONST_DOUBLE_REAL_VALUE (op0),
			  GET_MODE (op0));
	  real_to_target (tmp1, CONST_DOUBLE_REAL_VALUE (op1),
			  GET_MODE (op1));
	  for (i = 0; i < 4; i++)
	    {
	      switch (code)
	      {
	      case AND:
		tmp0[i] &= tmp1[i];
		break;
	      case IOR:
		tmp0[i] |= tmp1[i];
		break;
	      case XOR:
		tmp0[i] ^= tmp1[i];
		break;
	      default:
		gcc_unreachable ();
	      }
	    }
	   real_from_target (&r, tmp0, mode);
	   return const_double_from_real_value (r, mode);
	}
      else
	{
	  REAL_VALUE_TYPE f0, f1, value, result;
	  const REAL_VALUE_TYPE *opr0, *opr1;
	  bool inexact;

	  opr0 = CONST_DOUBLE_REAL_VALUE (op0);
	  opr1 = CONST_DOUBLE_REAL_VALUE (op1);

	  if (HONOR_SNANS (mode)
	      && (REAL_VALUE_ISSIGNALING_NAN (*opr0)
	          || REAL_VALUE_ISSIGNALING_NAN (*opr1)))
	    return 0;

	  real_convert (&f0, mode, opr0);
	  real_convert (&f1, mode, opr1);

	  if (code == DIV
	      && real_equal (&f1, &dconst0)
	      && (flag_trapping_math || ! MODE_HAS_INFINITIES (mode)))
	    return 0;

	  if (MODE_HAS_INFINITIES (mode) && HONOR_NANS (mode)
	      && flag_trapping_math
	      && REAL_VALUE_ISINF (f0) && REAL_VALUE_ISINF (f1))
	    {
	      int s0 = REAL_VALUE_NEGATIVE (f0);
	      int s1 = REAL_VALUE_NEGATIVE (f1);

	      switch (code)
		{
		case PLUS:
		  /* Inf + -Inf = NaN plus exception.  */
		  if (s0 != s1)
		    return 0;
		  break;
		case MINUS:
		  /* Inf - Inf = NaN plus exception.  */
		  if (s0 == s1)
		    return 0;
		  break;
		case DIV:
		  /* Inf / Inf = NaN plus exception.  */
		  return 0;
		default:
		  break;
		}
	    }

	  if (code == MULT && MODE_HAS_INFINITIES (mode) && HONOR_NANS (mode)
	      && flag_trapping_math
	      && ((REAL_VALUE_ISINF (f0) && real_equal (&f1, &dconst0))
		  || (REAL_VALUE_ISINF (f1)
		      && real_equal (&f0, &dconst0))))
	    /* Inf * 0 = NaN plus exception.  */
	    return 0;

	  inexact = real_arithmetic (&value, rtx_to_tree_code (code),
				     &f0, &f1);
	  real_convert (&result, mode, &value);

	  /* Don't constant fold this floating point operation if
	     the result has overflowed and flag_trapping_math.  */

	  if (flag_trapping_math
	      && MODE_HAS_INFINITIES (mode)
	      && REAL_VALUE_ISINF (result)
	      && !REAL_VALUE_ISINF (f0)
	      && !REAL_VALUE_ISINF (f1))
	    /* Overflow plus exception.  */
	    return 0;

	  /* Don't constant fold this floating point operation if the
	     result may dependent upon the run-time rounding mode and
	     flag_rounding_math is set, or if GCC's software emulation
	     is unable to accurately represent the result.  */

	  if ((flag_rounding_math
	       || (MODE_COMPOSITE_P (mode) && !flag_unsafe_math_optimizations))
	      && (inexact || !real_identical (&result, &value)))
	    return NULL_RTX;

	  return const_double_from_real_value (result, mode);
	}
    }

  /* We can fold some multi-word operations.  */
  scalar_int_mode int_mode;
  if (is_a <scalar_int_mode> (mode, &int_mode)
      && CONST_SCALAR_INT_P (op0)
      && CONST_SCALAR_INT_P (op1))
    {
      wide_int result;
      bool overflow;
      rtx_mode_t pop0 = rtx_mode_t (op0, int_mode);
      rtx_mode_t pop1 = rtx_mode_t (op1, int_mode);

#if TARGET_SUPPORTS_WIDE_INT == 0
      /* This assert keeps the simplification from producing a result
	 that cannot be represented in a CONST_DOUBLE but a lot of
	 upstream callers expect that this function never fails to
	 simplify something and so you if you added this to the test
	 above the code would die later anyway.  If this assert
	 happens, you just need to make the port support wide int.  */
      gcc_assert (GET_MODE_PRECISION (int_mode) <= HOST_BITS_PER_DOUBLE_INT);
#endif
      switch (code)
	{
	case MINUS:
	  result = wi::sub (pop0, pop1);
	  break;

	case PLUS:
	  result = wi::add (pop0, pop1);
	  break;

	case MULT:
	  result = wi::mul (pop0, pop1);
	  break;

	case DIV:
	  result = wi::div_trunc (pop0, pop1, SIGNED, &overflow);
	  if (overflow)
	    return NULL_RTX;
	  break;

	case MOD:
	  result = wi::mod_trunc (pop0, pop1, SIGNED, &overflow);
	  if (overflow)
	    return NULL_RTX;
	  break;

	case UDIV:
	  result = wi::div_trunc (pop0, pop1, UNSIGNED, &overflow);
	  if (overflow)
	    return NULL_RTX;
	  break;

	case UMOD:
	  result = wi::mod_trunc (pop0, pop1, UNSIGNED, &overflow);
	  if (overflow)
	    return NULL_RTX;
	  break;

	case AND:
	  result = wi::bit_and (pop0, pop1);
	  break;

	case IOR:
	  result = wi::bit_or (pop0, pop1);
	  break;

	case XOR:
	  result = wi::bit_xor (pop0, pop1);
	  break;

	case SMIN:
	  result = wi::smin (pop0, pop1);
	  break;

	case SMAX:
	  result = wi::smax (pop0, pop1);
	  break;

	case UMIN:
	  result = wi::umin (pop0, pop1);
	  break;

	case UMAX:
	  result = wi::umax (pop0, pop1);
	  break;

	case LSHIFTRT:
	case ASHIFTRT:
	case ASHIFT:
	  {
	    wide_int wop1 = pop1;
	    if (SHIFT_COUNT_TRUNCATED)
	      wop1 = wi::umod_trunc (wop1, GET_MODE_PRECISION (int_mode));
	    else if (wi::geu_p (wop1, GET_MODE_PRECISION (int_mode)))
	      return NULL_RTX;

	    switch (code)
	      {
	      case LSHIFTRT:
		result = wi::lrshift (pop0, wop1);
		break;

	      case ASHIFTRT:
		result = wi::arshift (pop0, wop1);
		break;

	      case ASHIFT:
		result = wi::lshift (pop0, wop1);
		break;

	      default:
		gcc_unreachable ();
	      }
	    break;
	  }
	case ROTATE:
	case ROTATERT:
	  {
	    if (wi::neg_p (pop1))
	      return NULL_RTX;

	    switch (code)
	      {
	      case ROTATE:
		result = wi::lrotate (pop0, pop1);
		break;

	      case ROTATERT:
		result = wi::rrotate (pop0, pop1);
		break;

	      default:
		gcc_unreachable ();
	      }
	    break;
	  }
	default:
	  return NULL_RTX;
	}
      return immed_wide_int_const (result, int_mode);
    }

  return NULL_RTX;
}



/* Return a positive integer if X should sort after Y.  The value
   returned is 1 if and only if X and Y are both regs.  */

static int
simplify_plus_minus_op_data_cmp (rtx x, rtx y)
{
  int result;

  result = (commutative_operand_precedence (y)
	    - commutative_operand_precedence (x));
  if (result)
    return result + result;

  /* Group together equal REGs to do more simplification.  */
  if (REG_P (x) && REG_P (y))
    return REGNO (x) > REGNO (y);

  return 0;
}

/* Simplify and canonicalize a PLUS or MINUS, at least one of whose
   operands may be another PLUS or MINUS.

   Rather than test for specific case, we do this by a brute-force method
   and do all possible simplifications until no more changes occur.  Then
   we rebuild the operation.

   May return NULL_RTX when no changes were made.  */

static rtx
simplify_plus_minus (enum rtx_code code, machine_mode mode, rtx op0,
		     rtx op1)
{
  struct simplify_plus_minus_op_data
  {
    rtx op;
    short neg;
  } ops[16];
  rtx result, tem;
  int n_ops = 2;
  int changed, n_constants, canonicalized = 0;
  int i, j;

  memset (ops, 0, sizeof ops);

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
      n_constants = 0;

      for (i = 0; i < n_ops; i++)
	{
	  rtx this_op = ops[i].op;
	  int this_neg = ops[i].neg;
	  enum rtx_code this_code = GET_CODE (this_op);

	  switch (this_code)
	    {
	    case PLUS:
	    case MINUS:
	      if (n_ops == ARRAY_SIZE (ops))
		return NULL_RTX;

	      ops[n_ops].op = XEXP (this_op, 1);
	      ops[n_ops].neg = (this_code == MINUS) ^ this_neg;
	      n_ops++;

	      ops[i].op = XEXP (this_op, 0);
	      changed = 1;
	      /* If this operand was negated then we will potentially
		 canonicalize the expression.  Similarly if we don't
		 place the operands adjacent we're re-ordering the
		 expression and thus might be performing a
		 canonicalization.  Ignore register re-ordering.
		 ??? It might be better to shuffle the ops array here,
		 but then (plus (plus (A, B), plus (C, D))) wouldn't
		 be seen as non-canonical.  */
	      if (this_neg
		  || (i != n_ops - 2
		      && !(REG_P (ops[i].op) && REG_P (ops[n_ops - 1].op))))
		canonicalized = 1;
	      break;

	    case NEG:
	      ops[i].op = XEXP (this_op, 0);
	      ops[i].neg = ! this_neg;
	      changed = 1;
	      canonicalized = 1;
	      break;

	    case CONST:
	      if (n_ops != ARRAY_SIZE (ops)
		  && GET_CODE (XEXP (this_op, 0)) == PLUS
		  && CONSTANT_P (XEXP (XEXP (this_op, 0), 0))
		  && CONSTANT_P (XEXP (XEXP (this_op, 0), 1)))
		{
		  ops[i].op = XEXP (XEXP (this_op, 0), 0);
		  ops[n_ops].op = XEXP (XEXP (this_op, 0), 1);
		  ops[n_ops].neg = this_neg;
		  n_ops++;
		  changed = 1;
		  canonicalized = 1;
		}
	      break;

	    case NOT:
	      /* ~a -> (-a - 1) */
	      if (n_ops != ARRAY_SIZE (ops))
		{
		  ops[n_ops].op = CONSTM1_RTX (mode);
		  ops[n_ops++].neg = this_neg;
		  ops[i].op = XEXP (this_op, 0);
		  ops[i].neg = !this_neg;
		  changed = 1;
		  canonicalized = 1;
		}
	      break;

	    case CONST_INT:
	      n_constants++;
	      if (this_neg)
		{
		  ops[i].op = neg_const_int (mode, this_op);
		  ops[i].neg = 0;
		  changed = 1;
		  canonicalized = 1;
		}
	      break;

	    default:
	      break;
	    }
	}
    }
  while (changed);

  if (n_constants > 1)
    canonicalized = 1;

  gcc_assert (n_ops >= 2);

  /* If we only have two operands, we can avoid the loops.  */
  if (n_ops == 2)
    {
      enum rtx_code code = ops[0].neg || ops[1].neg ? MINUS : PLUS;
      rtx lhs, rhs;

      /* Get the two operands.  Be careful with the order, especially for
	 the cases where code == MINUS.  */
      if (ops[0].neg && ops[1].neg)
	{
	  lhs = gen_rtx_NEG (mode, ops[0].op);
	  rhs = ops[1].op;
	}
      else if (ops[0].neg)
	{
	  lhs = ops[1].op;
	  rhs = ops[0].op;
	}
      else
	{
	  lhs = ops[0].op;
	  rhs = ops[1].op;
	}

      return simplify_const_binary_operation (code, mode, lhs, rhs);
    }

  /* Now simplify each pair of operands until nothing changes.  */
  while (1)
    {
      /* Insertion sort is good enough for a small array.  */
      for (i = 1; i < n_ops; i++)
	{
	  struct simplify_plus_minus_op_data save;
	  int cmp;

	  j = i - 1;
	  cmp = simplify_plus_minus_op_data_cmp (ops[j].op, ops[i].op);
	  if (cmp <= 0)
	    continue;
	  /* Just swapping registers doesn't count as canonicalization.  */
	  if (cmp != 1)
	    canonicalized = 1;

	  save = ops[i];
	  do
	    ops[j + 1] = ops[j];
	  while (j--
		 && simplify_plus_minus_op_data_cmp (ops[j].op, save.op) > 0);
	  ops[j + 1] = save;
	}

      changed = 0;
      for (i = n_ops - 1; i > 0; i--)
	for (j = i - 1; j >= 0; j--)
	  {
	    rtx lhs = ops[j].op, rhs = ops[i].op;
	    int lneg = ops[j].neg, rneg = ops[i].neg;

	    if (lhs != 0 && rhs != 0)
	      {
		enum rtx_code ncode = PLUS;

		if (lneg != rneg)
		  {
		    ncode = MINUS;
		    if (lneg)
		      std::swap (lhs, rhs);
		  }
		else if (swap_commutative_operands_p (lhs, rhs))
		  std::swap (lhs, rhs);

		if ((GET_CODE (lhs) == CONST || CONST_INT_P (lhs))
		    && (GET_CODE (rhs) == CONST || CONST_INT_P (rhs)))
		  {
		    rtx tem_lhs, tem_rhs;

		    tem_lhs = GET_CODE (lhs) == CONST ? XEXP (lhs, 0) : lhs;
		    tem_rhs = GET_CODE (rhs) == CONST ? XEXP (rhs, 0) : rhs;
		    tem = simplify_binary_operation (ncode, mode, tem_lhs,
						     tem_rhs);

		    if (tem && !CONSTANT_P (tem))
		      tem = gen_rtx_CONST (GET_MODE (tem), tem);
		  }
		else
		  tem = simplify_binary_operation (ncode, mode, lhs, rhs);

		if (tem)
		  {
		    /* Reject "simplifications" that just wrap the two
		       arguments in a CONST.  Failure to do so can result
		       in infinite recursion with simplify_binary_operation
		       when it calls us to simplify CONST operations.
		       Also, if we find such a simplification, don't try
		       any more combinations with this rhs:  We must have
		       something like symbol+offset, ie. one of the
		       trivial CONST expressions we handle later.  */
		    if (GET_CODE (tem) == CONST
			&& GET_CODE (XEXP (tem, 0)) == ncode
			&& XEXP (XEXP (tem, 0), 0) == lhs
			&& XEXP (XEXP (tem, 0), 1) == rhs)
		      break;
		    lneg &= rneg;
		    if (GET_CODE (tem) == NEG)
		      tem = XEXP (tem, 0), lneg = !lneg;
		    if (CONST_INT_P (tem) && lneg)
		      tem = neg_const_int (mode, tem), lneg = 0;

		    ops[i].op = tem;
		    ops[i].neg = lneg;
		    ops[j].op = NULL_RTX;
		    changed = 1;
		    canonicalized = 1;
		  }
	      }
	  }

      if (!changed)
	break;

      /* Pack all the operands to the lower-numbered entries.  */
      for (i = 0, j = 0; j < n_ops; j++)
	if (ops[j].op)
	  {
	    ops[i] = ops[j];
	    i++;
	  }
      n_ops = i;
    }

  /* If nothing changed, check that rematerialization of rtl instructions
     is still required.  */
  if (!canonicalized)
    {
      /* Perform rematerialization if only all operands are registers and
	 all operations are PLUS.  */
      /* ??? Also disallow (non-global, non-frame) fixed registers to work
	 around rs6000 and how it uses the CA register.  See PR67145.  */
      for (i = 0; i < n_ops; i++)
	if (ops[i].neg
	    || !REG_P (ops[i].op)
	    || (REGNO (ops[i].op) < FIRST_PSEUDO_REGISTER
		&& fixed_regs[REGNO (ops[i].op)]
		&& !global_regs[REGNO (ops[i].op)]
		&& ops[i].op != frame_pointer_rtx
		&& ops[i].op != arg_pointer_rtx
		&& ops[i].op != stack_pointer_rtx))
	  return NULL_RTX;
      goto gen_result;
    }

  /* Create (minus -C X) instead of (neg (const (plus X C))).  */
  if (n_ops == 2
      && CONST_INT_P (ops[1].op)
      && CONSTANT_P (ops[0].op)
      && ops[0].neg)
    return gen_rtx_fmt_ee (MINUS, mode, ops[1].op, ops[0].op);

  /* We suppressed creation of trivial CONST expressions in the
     combination loop to avoid recursion.  Create one manually now.
     The combination loop should have ensured that there is exactly
     one CONST_INT, and the sort will have ensured that it is last
     in the array and that any other constant will be next-to-last.  */

  if (n_ops > 1
      && CONST_INT_P (ops[n_ops - 1].op)
      && CONSTANT_P (ops[n_ops - 2].op))
    {
      rtx value = ops[n_ops - 1].op;
      if (ops[n_ops - 1].neg ^ ops[n_ops - 2].neg)
	value = neg_const_int (mode, value);
      if (CONST_INT_P (value))
	{
	  ops[n_ops - 2].op = plus_constant (mode, ops[n_ops - 2].op,
					     INTVAL (value));
	  n_ops--;
	}
    }

  /* Put a non-negated operand first, if possible.  */

  for (i = 0; i < n_ops && ops[i].neg; i++)
    continue;
  if (i == n_ops)
    ops[0].op = gen_rtx_NEG (mode, ops[0].op);
  else if (i != 0)
    {
      tem = ops[0].op;
      ops[0] = ops[i];
      ops[i].op = tem;
      ops[i].neg = 1;
    }

  /* Now make the result by performing the requested operations.  */
 gen_result:
  result = ops[0].op;
  for (i = 1; i < n_ops; i++)
    result = gen_rtx_fmt_ee (ops[i].neg ? MINUS : PLUS,
			     mode, result, ops[i].op);

  return result;
}

/* Check whether an operand is suitable for calling simplify_plus_minus.  */
static bool
plus_minus_operand_p (const_rtx x)
{
  return GET_CODE (x) == PLUS
         || GET_CODE (x) == MINUS
	 || (GET_CODE (x) == CONST
	     && GET_CODE (XEXP (x, 0)) == PLUS
	     && CONSTANT_P (XEXP (XEXP (x, 0), 0))
	     && CONSTANT_P (XEXP (XEXP (x, 0), 1)));
}

/* Like simplify_binary_operation except used for relational operators.
   MODE is the mode of the result. If MODE is VOIDmode, both operands must
   not also be VOIDmode.

   CMP_MODE specifies in which mode the comparison is done in, so it is
   the mode of the operands.  If CMP_MODE is VOIDmode, it is taken from
   the operands or, if both are VOIDmode, the operands are compared in
   "infinite precision".  */
rtx
simplify_relational_operation (enum rtx_code code, machine_mode mode,
			       machine_mode cmp_mode, rtx op0, rtx op1)
{
  rtx tem, trueop0, trueop1;

  if (cmp_mode == VOIDmode)
    cmp_mode = GET_MODE (op0);
  if (cmp_mode == VOIDmode)
    cmp_mode = GET_MODE (op1);

  tem = simplify_const_relational_operation (code, cmp_mode, op0, op1);
  if (tem)
    {
      if (SCALAR_FLOAT_MODE_P (mode))
	{
          if (tem == const0_rtx)
            return CONST0_RTX (mode);
#ifdef FLOAT_STORE_FLAG_VALUE
	  {
	    REAL_VALUE_TYPE val;
	    val = FLOAT_STORE_FLAG_VALUE (mode);
	    return const_double_from_real_value (val, mode);
	  }
#else
	  return NULL_RTX;
#endif
	}
      if (VECTOR_MODE_P (mode))
	{
	  if (tem == const0_rtx)
	    return CONST0_RTX (mode);
#ifdef VECTOR_STORE_FLAG_VALUE
	  {
	    int i, units;
	    rtvec v;

	    rtx val = VECTOR_STORE_FLAG_VALUE (mode);
	    if (val == NULL_RTX)
	      return NULL_RTX;
	    if (val == const1_rtx)
	      return CONST1_RTX (mode);

	    units = GET_MODE_NUNITS (mode);
	    v = rtvec_alloc (units);
	    for (i = 0; i < units; i++)
	      RTVEC_ELT (v, i) = val;
	    return gen_rtx_raw_CONST_VECTOR (mode, v);
	  }
#else
	  return NULL_RTX;
#endif
	}

      return tem;
    }

  /* For the following tests, ensure const0_rtx is op1.  */
  if (swap_commutative_operands_p (op0, op1)
      || (op0 == const0_rtx && op1 != const0_rtx))
    std::swap (op0, op1), code = swap_condition (code);

  /* If op0 is a compare, extract the comparison arguments from it.  */
  if (GET_CODE (op0) == COMPARE && op1 == const0_rtx)
    return simplify_gen_relational (code, mode, VOIDmode,
				    XEXP (op0, 0), XEXP (op0, 1));

  if (GET_MODE_CLASS (cmp_mode) == MODE_CC
      || CC0_P (op0))
    return NULL_RTX;

  trueop0 = avoid_constant_pool_reference (op0);
  trueop1 = avoid_constant_pool_reference (op1);
  return simplify_relational_operation_1 (code, mode, cmp_mode,
		  			  trueop0, trueop1);
}

/* This part of simplify_relational_operation is only used when CMP_MODE
   is not in class MODE_CC (i.e. it is a real comparison).

   MODE is the mode of the result, while CMP_MODE specifies in which
   mode the comparison is done in, so it is the mode of the operands.  */

static rtx
simplify_relational_operation_1 (enum rtx_code code, machine_mode mode,
				 machine_mode cmp_mode, rtx op0, rtx op1)
{
  enum rtx_code op0code = GET_CODE (op0);

  if (op1 == const0_rtx && COMPARISON_P (op0))
    {
      /* If op0 is a comparison, extract the comparison arguments
         from it.  */
      if (code == NE)
	{
	  if (GET_MODE (op0) == mode)
	    return simplify_rtx (op0);
	  else
	    return simplify_gen_relational (GET_CODE (op0), mode, VOIDmode,
					    XEXP (op0, 0), XEXP (op0, 1));
	}
      else if (code == EQ)
	{
	  enum rtx_code new_code = reversed_comparison_code (op0, NULL);
	  if (new_code != UNKNOWN)
	    return simplify_gen_relational (new_code, mode, VOIDmode,
					    XEXP (op0, 0), XEXP (op0, 1));
	}
    }

  /* (LTU/GEU (PLUS a C) C), where C is constant, can be simplified to
     (GEU/LTU a -C).  Likewise for (LTU/GEU (PLUS a C) a).  */
  if ((code == LTU || code == GEU)
      && GET_CODE (op0) == PLUS
      && CONST_INT_P (XEXP (op0, 1))
      && (rtx_equal_p (op1, XEXP (op0, 0))
	  || rtx_equal_p (op1, XEXP (op0, 1)))
      /* (LTU/GEU (PLUS a 0) 0) is not the same as (GEU/LTU a 0). */
      && XEXP (op0, 1) != const0_rtx)
    {
      rtx new_cmp
	= simplify_gen_unary (NEG, cmp_mode, XEXP (op0, 1), cmp_mode);
      return simplify_gen_relational ((code == LTU ? GEU : LTU), mode,
				      cmp_mode, XEXP (op0, 0), new_cmp);
    }

  /* (GTU (PLUS a C) (C - 1)) where C is a non-zero constant can be
     transformed into (LTU a -C).  */
  if (code == GTU && GET_CODE (op0) == PLUS && CONST_INT_P (op1)
      && CONST_INT_P (XEXP (op0, 1))
      && (UINTVAL (op1) == UINTVAL (XEXP (op0, 1)) - 1)
      && XEXP (op0, 1) != const0_rtx)
    {
      rtx new_cmp
	= simplify_gen_unary (NEG, cmp_mode, XEXP (op0, 1), cmp_mode);
      return simplify_gen_relational (LTU, mode, cmp_mode,
				       XEXP (op0, 0), new_cmp);
    }

  /* Canonicalize (LTU/GEU (PLUS a b) b) as (LTU/GEU (PLUS a b) a).  */
  if ((code == LTU || code == GEU)
      && GET_CODE (op0) == PLUS
      && rtx_equal_p (op1, XEXP (op0, 1))
      /* Don't recurse "infinitely" for (LTU/GEU (PLUS b b) b).  */
      && !rtx_equal_p (op1, XEXP (op0, 0)))
    return simplify_gen_relational (code, mode, cmp_mode, op0,
				    copy_rtx (XEXP (op0, 0)));

  if (op1 == const0_rtx)
    {
      /* Canonicalize (GTU x 0) as (NE x 0).  */
      if (code == GTU)
        return simplify_gen_relational (NE, mode, cmp_mode, op0, op1);
      /* Canonicalize (LEU x 0) as (EQ x 0).  */
      if (code == LEU)
        return simplify_gen_relational (EQ, mode, cmp_mode, op0, op1);
    }
  else if (op1 == const1_rtx)
    {
      switch (code)
        {
        case GE:
	  /* Canonicalize (GE x 1) as (GT x 0).  */
	  return simplify_gen_relational (GT, mode, cmp_mode,
					  op0, const0_rtx);
	case GEU:
	  /* Canonicalize (GEU x 1) as (NE x 0).  */
	  return simplify_gen_relational (NE, mode, cmp_mode,
					  op0, const0_rtx);
	case LT:
	  /* Canonicalize (LT x 1) as (LE x 0).  */
	  return simplify_gen_relational (LE, mode, cmp_mode,
					  op0, const0_rtx);
	case LTU:
	  /* Canonicalize (LTU x 1) as (EQ x 0).  */
	  return simplify_gen_relational (EQ, mode, cmp_mode,
					  op0, const0_rtx);
	default:
	  break;
	}
    }
  else if (op1 == constm1_rtx)
    {
      /* Canonicalize (LE x -1) as (LT x 0).  */
      if (code == LE)
        return simplify_gen_relational (LT, mode, cmp_mode, op0, const0_rtx);
      /* Canonicalize (GT x -1) as (GE x 0).  */
      if (code == GT)
        return simplify_gen_relational (GE, mode, cmp_mode, op0, const0_rtx);
    }

  /* (eq/ne (plus x cst1) cst2) simplifies to (eq/ne x (cst2 - cst1))  */
  if ((code == EQ || code == NE)
      && (op0code == PLUS || op0code == MINUS)
      && CONSTANT_P (op1)
      && CONSTANT_P (XEXP (op0, 1))
      && (INTEGRAL_MODE_P (cmp_mode) || flag_unsafe_math_optimizations))
    {
      rtx x = XEXP (op0, 0);
      rtx c = XEXP (op0, 1);
      enum rtx_code invcode = op0code == PLUS ? MINUS : PLUS;
      rtx tem = simplify_gen_binary (invcode, cmp_mode, op1, c);

      /* Detect an infinite recursive condition, where we oscillate at this
	 simplification case between:
	    A + B == C  <--->  C - B == A,
	 where A, B, and C are all constants with non-simplifiable expressions,
	 usually SYMBOL_REFs.  */
      if (GET_CODE (tem) == invcode
	  && CONSTANT_P (x)
	  && rtx_equal_p (c, XEXP (tem, 1)))
	return NULL_RTX;

      return simplify_gen_relational (code, mode, cmp_mode, x, tem);
    }

  /* (ne:SI (zero_extract:SI FOO (const_int 1) BAR) (const_int 0))) is
     the same as (zero_extract:SI FOO (const_int 1) BAR).  */
  scalar_int_mode int_mode, int_cmp_mode;
  if (code == NE
      && op1 == const0_rtx
      && is_int_mode (mode, &int_mode)
      && is_a <scalar_int_mode> (cmp_mode, &int_cmp_mode)
      /* ??? Work-around BImode bugs in the ia64 backend.  */
      && int_mode != BImode
      && int_cmp_mode != BImode
      && nonzero_bits (op0, int_cmp_mode) == 1
      && STORE_FLAG_VALUE == 1)
    return GET_MODE_SIZE (int_mode) > GET_MODE_SIZE (int_cmp_mode)
	   ? simplify_gen_unary (ZERO_EXTEND, int_mode, op0, int_cmp_mode)
	   : lowpart_subreg (int_mode, op0, int_cmp_mode);

  /* (eq/ne (xor x y) 0) simplifies to (eq/ne x y).  */
  if ((code == EQ || code == NE)
      && op1 == const0_rtx
      && op0code == XOR)
    return simplify_gen_relational (code, mode, cmp_mode,
				    XEXP (op0, 0), XEXP (op0, 1));

  /* (eq/ne (xor x y) x) simplifies to (eq/ne y 0).  */
  if ((code == EQ || code == NE)
      && op0code == XOR
      && rtx_equal_p (XEXP (op0, 0), op1)
      && !side_effects_p (XEXP (op0, 0)))
    return simplify_gen_relational (code, mode, cmp_mode, XEXP (op0, 1),
				    CONST0_RTX (mode));

  /* Likewise (eq/ne (xor x y) y) simplifies to (eq/ne x 0).  */
  if ((code == EQ || code == NE)
      && op0code == XOR
      && rtx_equal_p (XEXP (op0, 1), op1)
      && !side_effects_p (XEXP (op0, 1)))
    return simplify_gen_relational (code, mode, cmp_mode, XEXP (op0, 0),
				    CONST0_RTX (mode));

  /* (eq/ne (xor x C1) C2) simplifies to (eq/ne x (C1^C2)).  */
  if ((code == EQ || code == NE)
      && op0code == XOR
      && CONST_SCALAR_INT_P (op1)
      && CONST_SCALAR_INT_P (XEXP (op0, 1)))
    return simplify_gen_relational (code, mode, cmp_mode, XEXP (op0, 0),
				    simplify_gen_binary (XOR, cmp_mode,
							 XEXP (op0, 1), op1));

  /* (eq/ne (and x y) x) simplifies to (eq/ne (and (not y) x) 0), which
     can be implemented with a BICS instruction on some targets, or
     constant-folded if y is a constant.  */
  if ((code == EQ || code == NE)
      && op0code == AND
      && rtx_equal_p (XEXP (op0, 0), op1)
      && !side_effects_p (op1)
      && op1 != CONST0_RTX (cmp_mode))
    {
      rtx not_y = simplify_gen_unary (NOT, cmp_mode, XEXP (op0, 1), cmp_mode);
      rtx lhs = simplify_gen_binary (AND, cmp_mode, not_y, XEXP (op0, 0));

      return simplify_gen_relational (code, mode, cmp_mode, lhs,
				      CONST0_RTX (cmp_mode));
    }

  /* Likewise for (eq/ne (and x y) y).  */
  if ((code == EQ || code == NE)
      && op0code == AND
      && rtx_equal_p (XEXP (op0, 1), op1)
      && !side_effects_p (op1)
      && op1 != CONST0_RTX (cmp_mode))
    {
      rtx not_x = simplify_gen_unary (NOT, cmp_mode, XEXP (op0, 0), cmp_mode);
      rtx lhs = simplify_gen_binary (AND, cmp_mode, not_x, XEXP (op0, 1));

      return simplify_gen_relational (code, mode, cmp_mode, lhs,
				      CONST0_RTX (cmp_mode));
    }

  /* (eq/ne (bswap x) C1) simplifies to (eq/ne x C2) with C2 swapped.  */
  if ((code == EQ || code == NE)
      && GET_CODE (op0) == BSWAP
      && CONST_SCALAR_INT_P (op1))
    return simplify_gen_relational (code, mode, cmp_mode, XEXP (op0, 0),
				    simplify_gen_unary (BSWAP, cmp_mode,
							op1, cmp_mode));

  /* (eq/ne (bswap x) (bswap y)) simplifies to (eq/ne x y).  */
  if ((code == EQ || code == NE)
      && GET_CODE (op0) == BSWAP
      && GET_CODE (op1) == BSWAP)
    return simplify_gen_relational (code, mode, cmp_mode,
				    XEXP (op0, 0), XEXP (op1, 0));

  if (op0code == POPCOUNT && op1 == const0_rtx)
    switch (code)
      {
      case EQ:
      case LE:
      case LEU:
	/* (eq (popcount x) (const_int 0)) -> (eq x (const_int 0)).  */
	return simplify_gen_relational (EQ, mode, GET_MODE (XEXP (op0, 0)),
					XEXP (op0, 0), const0_rtx);

      case NE:
      case GT:
      case GTU:
	/* (ne (popcount x) (const_int 0)) -> (ne x (const_int 0)).  */
	return simplify_gen_relational (NE, mode, GET_MODE (XEXP (op0, 0)),
					XEXP (op0, 0), const0_rtx);

      default:
	break;
      }

  return NULL_RTX;
}

enum
{
  CMP_EQ = 1,
  CMP_LT = 2,
  CMP_GT = 4,
  CMP_LTU = 8,
  CMP_GTU = 16
};


/* Convert the known results for EQ, LT, GT, LTU, GTU contained in
   KNOWN_RESULT to a CONST_INT, based on the requested comparison CODE
   For KNOWN_RESULT to make sense it should be either CMP_EQ, or the
   logical OR of one of (CMP_LT, CMP_GT) and one of (CMP_LTU, CMP_GTU).
   For floating-point comparisons, assume that the operands were ordered.  */

static rtx
comparison_result (enum rtx_code code, int known_results)
{
  switch (code)
    {
    case EQ:
    case UNEQ:
      return (known_results & CMP_EQ) ? const_true_rtx : const0_rtx;
    case NE:
    case LTGT:
      return (known_results & CMP_EQ) ? const0_rtx : const_true_rtx;

    case LT:
    case UNLT:
      return (known_results & CMP_LT) ? const_true_rtx : const0_rtx;
    case GE:
    case UNGE:
      return (known_results & CMP_LT) ? const0_rtx : const_true_rtx;

    case GT:
    case UNGT:
      return (known_results & CMP_GT) ? const_true_rtx : const0_rtx;
    case LE:
    case UNLE:
      return (known_results & CMP_GT) ? const0_rtx : const_true_rtx;

    case LTU:
      return (known_results & CMP_LTU) ? const_true_rtx : const0_rtx;
    case GEU:
      return (known_results & CMP_LTU) ? const0_rtx : const_true_rtx;

    case GTU:
      return (known_results & CMP_GTU) ? const_true_rtx : const0_rtx;
    case LEU:
      return (known_results & CMP_GTU) ? const0_rtx : const_true_rtx;

    case ORDERED:
      return const_true_rtx;
    case UNORDERED:
      return const0_rtx;
    default:
      gcc_unreachable ();
    }
}

/* Check if the given comparison (done in the given MODE) is actually
   a tautology or a contradiction.  If the mode is VOID_mode, the
   comparison is done in "infinite precision".  If no simplification
   is possible, this function returns zero.  Otherwise, it returns
   either const_true_rtx or const0_rtx.  */

rtx
simplify_const_relational_operation (enum rtx_code code,
				     machine_mode mode,
				     rtx op0, rtx op1)
{
  rtx tem;
  rtx trueop0;
  rtx trueop1;

  gcc_assert (mode != VOIDmode
	      || (GET_MODE (op0) == VOIDmode
		  && GET_MODE (op1) == VOIDmode));

  /* If op0 is a compare, extract the comparison arguments from it.  */
  if (GET_CODE (op0) == COMPARE && op1 == const0_rtx)
    {
      op1 = XEXP (op0, 1);
      op0 = XEXP (op0, 0);

      if (GET_MODE (op0) != VOIDmode)
	mode = GET_MODE (op0);
      else if (GET_MODE (op1) != VOIDmode)
	mode = GET_MODE (op1);
      else
	return 0;
    }

  /* We can't simplify MODE_CC values since we don't know what the
     actual comparison is.  */
  if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_CC || CC0_P (op0))
    return 0;

  /* Make sure the constant is second.  */
  if (swap_commutative_operands_p (op0, op1))
    {
      std::swap (op0, op1);
      code = swap_condition (code);
    }

  trueop0 = avoid_constant_pool_reference (op0);
  trueop1 = avoid_constant_pool_reference (op1);

  /* For integer comparisons of A and B maybe we can simplify A - B and can
     then simplify a comparison of that with zero.  If A and B are both either
     a register or a CONST_INT, this can't help; testing for these cases will
     prevent infinite recursion here and speed things up.

     We can only do this for EQ and NE comparisons as otherwise we may
     lose or introduce overflow which we cannot disregard as undefined as
     we do not know the signedness of the operation on either the left or
     the right hand side of the comparison.  */

  if (INTEGRAL_MODE_P (mode) && trueop1 != const0_rtx
      && (code == EQ || code == NE)
      && ! ((REG_P (op0) || CONST_INT_P (trueop0))
	    && (REG_P (op1) || CONST_INT_P (trueop1)))
      && 0 != (tem = simplify_binary_operation (MINUS, mode, op0, op1))
      /* We cannot do this if tem is a nonzero address.  */
      && ! nonzero_address_p (tem))
    return simplify_const_relational_operation (signed_condition (code),
						mode, tem, const0_rtx);

  if (! HONOR_NANS (mode) && code == ORDERED)
    return const_true_rtx;

  if (! HONOR_NANS (mode) && code == UNORDERED)
    return const0_rtx;

  /* For modes without NaNs, if the two operands are equal, we know the
     result except if they have side-effects.  Even with NaNs we know
     the result of unordered comparisons and, if signaling NaNs are
     irrelevant, also the result of LT/GT/LTGT.  */
  if ((! HONOR_NANS (trueop0)
       || code == UNEQ || code == UNLE || code == UNGE
       || ((code == LT || code == GT || code == LTGT)
	   && ! HONOR_SNANS (trueop0)))
      && rtx_equal_p (trueop0, trueop1)
      && ! side_effects_p (trueop0))
    return comparison_result (code, CMP_EQ);

  /* If the operands are floating-point constants, see if we can fold
     the result.  */
  if (CONST_DOUBLE_AS_FLOAT_P (trueop0)
      && CONST_DOUBLE_AS_FLOAT_P (trueop1)
      && SCALAR_FLOAT_MODE_P (GET_MODE (trueop0)))
    {
      const REAL_VALUE_TYPE *d0 = CONST_DOUBLE_REAL_VALUE (trueop0);
      const REAL_VALUE_TYPE *d1 = CONST_DOUBLE_REAL_VALUE (trueop1);

      /* Comparisons are unordered iff at least one of the values is NaN.  */
      if (REAL_VALUE_ISNAN (*d0) || REAL_VALUE_ISNAN (*d1))
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

      return comparison_result (code,
				(real_equal (d0, d1) ? CMP_EQ :
				 real_less (d0, d1) ? CMP_LT : CMP_GT));
    }

  /* Otherwise, see if the operands are both integers.  */
  if ((GET_MODE_CLASS (mode) == MODE_INT || mode == VOIDmode)
      && CONST_SCALAR_INT_P (trueop0) && CONST_SCALAR_INT_P (trueop1))
    {
      /* It would be nice if we really had a mode here.  However, the
	 largest int representable on the target is as good as
	 infinite.  */
      machine_mode cmode = (mode == VOIDmode) ? MAX_MODE_INT : mode;
      rtx_mode_t ptrueop0 = rtx_mode_t (trueop0, cmode);
      rtx_mode_t ptrueop1 = rtx_mode_t (trueop1, cmode);

      if (wi::eq_p (ptrueop0, ptrueop1))
	return comparison_result (code, CMP_EQ);
      else
	{
	  int cr = wi::lts_p (ptrueop0, ptrueop1) ? CMP_LT : CMP_GT;
	  cr |= wi::ltu_p (ptrueop0, ptrueop1) ? CMP_LTU : CMP_GTU;
	  return comparison_result (code, cr);
	}
    }

  /* Optimize comparisons with upper and lower bounds.  */
  scalar_int_mode int_mode;
  if (CONST_INT_P (trueop1)
      && is_a <scalar_int_mode> (mode, &int_mode)
      && HWI_COMPUTABLE_MODE_P (int_mode)
      && !side_effects_p (trueop0))
    {
      int sign;
      unsigned HOST_WIDE_INT nonzero = nonzero_bits (trueop0, int_mode);
      HOST_WIDE_INT val = INTVAL (trueop1);
      HOST_WIDE_INT mmin, mmax;

      if (code == GEU
	  || code == LEU
	  || code == GTU
	  || code == LTU)
	sign = 0;
      else
	sign = 1;

      /* Get a reduced range if the sign bit is zero.  */
      if (nonzero <= (GET_MODE_MASK (int_mode) >> 1))
	{
	  mmin = 0;
	  mmax = nonzero;
	}
      else
	{
	  rtx mmin_rtx, mmax_rtx;
	  get_mode_bounds (int_mode, sign, int_mode, &mmin_rtx, &mmax_rtx);

	  mmin = INTVAL (mmin_rtx);
	  mmax = INTVAL (mmax_rtx);
	  if (sign)
	    {
	      unsigned int sign_copies
		= num_sign_bit_copies (trueop0, int_mode);

	      mmin >>= (sign_copies - 1);
	      mmax >>= (sign_copies - 1);
	    }
	}

      switch (code)
	{
	/* x >= y is always true for y <= mmin, always false for y > mmax.  */
	case GEU:
	  if ((unsigned HOST_WIDE_INT) val <= (unsigned HOST_WIDE_INT) mmin)
	    return const_true_rtx;
	  if ((unsigned HOST_WIDE_INT) val > (unsigned HOST_WIDE_INT) mmax)
	    return const0_rtx;
	  break;
	case GE:
	  if (val <= mmin)
	    return const_true_rtx;
	  if (val > mmax)
	    return const0_rtx;
	  break;

	/* x <= y is always true for y >= mmax, always false for y < mmin.  */
	case LEU:
	  if ((unsigned HOST_WIDE_INT) val >= (unsigned HOST_WIDE_INT) mmax)
	    return const_true_rtx;
	  if ((unsigned HOST_WIDE_INT) val < (unsigned HOST_WIDE_INT) mmin)
	    return const0_rtx;
	  break;
	case LE:
	  if (val >= mmax)
	    return const_true_rtx;
	  if (val < mmin)
	    return const0_rtx;
	  break;

	case EQ:
	  /* x == y is always false for y out of range.  */
	  if (val < mmin || val > mmax)
	    return const0_rtx;
	  break;

	/* x > y is always false for y >= mmax, always true for y < mmin.  */
	case GTU:
	  if ((unsigned HOST_WIDE_INT) val >= (unsigned HOST_WIDE_INT) mmax)
	    return const0_rtx;
	  if ((unsigned HOST_WIDE_INT) val < (unsigned HOST_WIDE_INT) mmin)
	    return const_true_rtx;
	  break;
	case GT:
	  if (val >= mmax)
	    return const0_rtx;
	  if (val < mmin)
	    return const_true_rtx;
	  break;

	/* x < y is always false for y <= mmin, always true for y > mmax.  */
	case LTU:
	  if ((unsigned HOST_WIDE_INT) val <= (unsigned HOST_WIDE_INT) mmin)
	    return const0_rtx;
	  if ((unsigned HOST_WIDE_INT) val > (unsigned HOST_WIDE_INT) mmax)
	    return const_true_rtx;
	  break;
	case LT:
	  if (val <= mmin)
	    return const0_rtx;
	  if (val > mmax)
	    return const_true_rtx;
	  break;

	case NE:
	  /* x != y is always true for y out of range.  */
	  if (val < mmin || val > mmax)
	    return const_true_rtx;
	  break;

	default:
	  break;
	}
    }

  /* Optimize integer comparisons with zero.  */
  if (is_a <scalar_int_mode> (mode, &int_mode)
      && trueop1 == const0_rtx
      && !side_effects_p (trueop0))
    {
      /* Some addresses are known to be nonzero.  We don't know
	 their sign, but equality comparisons are known.  */
      if (nonzero_address_p (trueop0))
	{
	  if (code == EQ || code == LEU)
	    return const0_rtx;
	  if (code == NE || code == GTU)
	    return const_true_rtx;
	}

      /* See if the first operand is an IOR with a constant.  If so, we
	 may be able to determine the result of this comparison.  */
      if (GET_CODE (op0) == IOR)
	{
	  rtx inner_const = avoid_constant_pool_reference (XEXP (op0, 1));
	  if (CONST_INT_P (inner_const) && inner_const != const0_rtx)
	    {
	      int sign_bitnum = GET_MODE_PRECISION (int_mode) - 1;
	      int has_sign = (HOST_BITS_PER_WIDE_INT >= sign_bitnum
			      && (UINTVAL (inner_const)
				  & (HOST_WIDE_INT_1U
				     << sign_bitnum)));

	      switch (code)
		{
		case EQ:
		case LEU:
		  return const0_rtx;
		case NE:
		case GTU:
		  return const_true_rtx;
		case LT:
		case LE:
		  if (has_sign)
		    return const_true_rtx;
		  break;
		case GT:
		case GE:
		  if (has_sign)
		    return const0_rtx;
		  break;
		default:
		  break;
		}
	    }
	}
    }

  /* Optimize comparison of ABS with zero.  */
  if (trueop1 == CONST0_RTX (mode) && !side_effects_p (trueop0)
      && (GET_CODE (trueop0) == ABS
	  || (GET_CODE (trueop0) == FLOAT_EXTEND
	      && GET_CODE (XEXP (trueop0, 0)) == ABS)))
    {
      switch (code)
	{
	case LT:
	  /* Optimize abs(x) < 0.0.  */
	  if (!INTEGRAL_MODE_P (mode) && !HONOR_SNANS (mode))
	    return const0_rtx;
	  break;

	case GE:
	  /* Optimize abs(x) >= 0.0.  */
	  if (!INTEGRAL_MODE_P (mode) && !HONOR_NANS (mode))
	    return const_true_rtx;
	  break;

	case UNGE:
	  /* Optimize ! (abs(x) < 0.0).  */
	  return const_true_rtx;

	default:
	  break;
	}
    }

  return 0;
}

/* Recognize expressions of the form (X CMP 0) ? VAL : OP (X)
   where OP is CLZ or CTZ and VAL is the value from CLZ_DEFINED_VALUE_AT_ZERO
   or CTZ_DEFINED_VALUE_AT_ZERO respectively and return OP (X) if the expression
   can be simplified to that or NULL_RTX if not.
   Assume X is compared against zero with CMP_CODE and the true
   arm is TRUE_VAL and the false arm is FALSE_VAL.  */

static rtx
simplify_cond_clz_ctz (rtx x, rtx_code cmp_code, rtx true_val, rtx false_val)
{
  if (cmp_code != EQ && cmp_code != NE)
    return NULL_RTX;

  /* Result on X == 0 and X !=0 respectively.  */
  rtx on_zero, on_nonzero;
  if (cmp_code == EQ)
    {
      on_zero = true_val;
      on_nonzero = false_val;
    }
  else
    {
      on_zero = false_val;
      on_nonzero = true_val;
    }

  rtx_code op_code = GET_CODE (on_nonzero);
  if ((op_code != CLZ && op_code != CTZ)
      || !rtx_equal_p (XEXP (on_nonzero, 0), x)
      || !CONST_INT_P (on_zero))
    return NULL_RTX;

  HOST_WIDE_INT op_val;
  scalar_int_mode mode ATTRIBUTE_UNUSED
    = as_a <scalar_int_mode> (GET_MODE (XEXP (on_nonzero, 0)));
  if (((op_code == CLZ && CLZ_DEFINED_VALUE_AT_ZERO (mode, op_val))
       || (op_code == CTZ && CTZ_DEFINED_VALUE_AT_ZERO (mode, op_val)))
      && op_val == INTVAL (on_zero))
    return on_nonzero;

  return NULL_RTX;
}


/* Simplify CODE, an operation with result mode MODE and three operands,
   OP0, OP1, and OP2.  OP0_MODE was the mode of OP0 before it became
   a constant.  Return 0 if no simplifications is possible.  */

rtx
simplify_ternary_operation (enum rtx_code code, machine_mode mode,
			    machine_mode op0_mode, rtx op0, rtx op1,
			    rtx op2)
{
  bool any_change = false;
  rtx tem, trueop2;
  scalar_int_mode int_mode, int_op0_mode;

  switch (code)
    {
    case FMA:
      /* Simplify negations around the multiplication.  */
      /* -a * -b + c  =>  a * b + c.  */
      if (GET_CODE (op0) == NEG)
	{
	  tem = simplify_unary_operation (NEG, mode, op1, mode);
	  if (tem)
	    op1 = tem, op0 = XEXP (op0, 0), any_change = true;
	}
      else if (GET_CODE (op1) == NEG)
	{
	  tem = simplify_unary_operation (NEG, mode, op0, mode);
	  if (tem)
	    op0 = tem, op1 = XEXP (op1, 0), any_change = true;
	}

      /* Canonicalize the two multiplication operands.  */
      /* a * -b + c  =>  -b * a + c.  */
      if (swap_commutative_operands_p (op0, op1))
	std::swap (op0, op1), any_change = true;

      if (any_change)
	return gen_rtx_FMA (mode, op0, op1, op2);
      return NULL_RTX;

    case SIGN_EXTRACT:
    case ZERO_EXTRACT:
      if (CONST_INT_P (op0)
	  && CONST_INT_P (op1)
	  && CONST_INT_P (op2)
	  && is_a <scalar_int_mode> (mode, &int_mode)
	  && INTVAL (op1) + INTVAL (op2) <= GET_MODE_PRECISION (int_mode)
	  && HWI_COMPUTABLE_MODE_P (int_mode))
	{
	  /* Extracting a bit-field from a constant */
	  unsigned HOST_WIDE_INT val = UINTVAL (op0);
	  HOST_WIDE_INT op1val = INTVAL (op1);
	  HOST_WIDE_INT op2val = INTVAL (op2);
	  if (!BITS_BIG_ENDIAN)
	    val >>= op2val;
	  else if (is_a <scalar_int_mode> (op0_mode, &int_op0_mode))
	    val >>= GET_MODE_PRECISION (int_op0_mode) - op2val - op1val;
	  else
	    /* Not enough information to calculate the bit position.  */
	    break;

	  if (HOST_BITS_PER_WIDE_INT != op1val)
	    {
	      /* First zero-extend.  */
	      val &= (HOST_WIDE_INT_1U << op1val) - 1;
	      /* If desired, propagate sign bit.  */
	      if (code == SIGN_EXTRACT
		  && (val & (HOST_WIDE_INT_1U << (op1val - 1)))
		     != 0)
		val |= ~ ((HOST_WIDE_INT_1U << op1val) - 1);
	    }

	  return gen_int_mode (val, int_mode);
	}
      break;

    case IF_THEN_ELSE:
      if (CONST_INT_P (op0))
	return op0 != const0_rtx ? op1 : op2;

      /* Convert c ? a : a into "a".  */
      if (rtx_equal_p (op1, op2) && ! side_effects_p (op0))
	return op1;

      /* Convert a != b ? a : b into "a".  */
      if (GET_CODE (op0) == NE
	  && ! side_effects_p (op0)
	  && ! HONOR_NANS (mode)
	  && ! HONOR_SIGNED_ZEROS (mode)
	  && ((rtx_equal_p (XEXP (op0, 0), op1)
	       && rtx_equal_p (XEXP (op0, 1), op2))
	      || (rtx_equal_p (XEXP (op0, 0), op2)
		  && rtx_equal_p (XEXP (op0, 1), op1))))
	return op1;

      /* Convert a == b ? a : b into "b".  */
      if (GET_CODE (op0) == EQ
	  && ! side_effects_p (op0)
	  && ! HONOR_NANS (mode)
	  && ! HONOR_SIGNED_ZEROS (mode)
	  && ((rtx_equal_p (XEXP (op0, 0), op1)
	       && rtx_equal_p (XEXP (op0, 1), op2))
	      || (rtx_equal_p (XEXP (op0, 0), op2)
		  && rtx_equal_p (XEXP (op0, 1), op1))))
	return op2;

      /* Convert (!c) != {0,...,0} ? a : b into
         c != {0,...,0} ? b : a for vector modes.  */
      if (VECTOR_MODE_P (GET_MODE (op1))
	  && GET_CODE (op0) == NE
	  && GET_CODE (XEXP (op0, 0)) == NOT
	  && GET_CODE (XEXP (op0, 1)) == CONST_VECTOR)
	{
	  rtx cv = XEXP (op0, 1);
	  int nunits = CONST_VECTOR_NUNITS (cv);
	  bool ok = true;
	  for (int i = 0; i < nunits; ++i)
	    if (CONST_VECTOR_ELT (cv, i) != const0_rtx)
	      {
		ok = false;
		break;
	      }
	  if (ok)
	    {
	      rtx new_op0 = gen_rtx_NE (GET_MODE (op0),
					XEXP (XEXP (op0, 0), 0),
					XEXP (op0, 1));
	      rtx retval = gen_rtx_IF_THEN_ELSE (mode, new_op0, op2, op1);
	      return retval;
	    }
	}

      /* Convert x == 0 ? N : clz (x) into clz (x) when
	 CLZ_DEFINED_VALUE_AT_ZERO is defined to N for the mode of x.
	 Similarly for ctz (x).  */
      if (COMPARISON_P (op0) && !side_effects_p (op0)
	  && XEXP (op0, 1) == const0_rtx)
	{
	  rtx simplified
	    = simplify_cond_clz_ctz (XEXP (op0, 0), GET_CODE (op0),
				     op1, op2);
	  if (simplified)
	    return simplified;
	}

      if (COMPARISON_P (op0) && ! side_effects_p (op0))
	{
	  machine_mode cmp_mode = (GET_MODE (XEXP (op0, 0)) == VOIDmode
					? GET_MODE (XEXP (op0, 1))
					: GET_MODE (XEXP (op0, 0)));
	  rtx temp;

	  /* Look for happy constants in op1 and op2.  */
	  if (CONST_INT_P (op1) && CONST_INT_P (op2))
	    {
	      HOST_WIDE_INT t = INTVAL (op1);
	      HOST_WIDE_INT f = INTVAL (op2);

	      if (t == STORE_FLAG_VALUE && f == 0)
	        code = GET_CODE (op0);
	      else if (t == 0 && f == STORE_FLAG_VALUE)
		{
		  enum rtx_code tmp;
		  tmp = reversed_comparison_code (op0, NULL);
		  if (tmp == UNKNOWN)
		    break;
		  code = tmp;
		}
	      else
		break;

	      return simplify_gen_relational (code, mode, cmp_mode,
					      XEXP (op0, 0), XEXP (op0, 1));
	    }

	  if (cmp_mode == VOIDmode)
	    cmp_mode = op0_mode;
	  temp = simplify_relational_operation (GET_CODE (op0), op0_mode,
			  			cmp_mode, XEXP (op0, 0),
						XEXP (op0, 1));

	  /* See if any simplifications were possible.  */
	  if (temp)
	    {
	      if (CONST_INT_P (temp))
		return temp == const0_rtx ? op2 : op1;
	      else if (temp)
	        return gen_rtx_IF_THEN_ELSE (mode, temp, op1, op2);
	    }
	}
      break;

    case VEC_MERGE:
      gcc_assert (GET_MODE (op0) == mode);
      gcc_assert (GET_MODE (op1) == mode);
      gcc_assert (VECTOR_MODE_P (mode));
      trueop2 = avoid_constant_pool_reference (op2);
      if (CONST_INT_P (trueop2))
	{
	  int elt_size = GET_MODE_UNIT_SIZE (mode);
	  unsigned n_elts = (GET_MODE_SIZE (mode) / elt_size);
	  unsigned HOST_WIDE_INT sel = UINTVAL (trueop2);
	  unsigned HOST_WIDE_INT mask;
	  if (n_elts == HOST_BITS_PER_WIDE_INT)
	    mask = -1;
	  else
	    mask = (HOST_WIDE_INT_1U << n_elts) - 1;

	  if (!(sel & mask) && !side_effects_p (op0))
	    return op1;
	  if ((sel & mask) == mask && !side_effects_p (op1))
	    return op0;

	  rtx trueop0 = avoid_constant_pool_reference (op0);
	  rtx trueop1 = avoid_constant_pool_reference (op1);
	  if (GET_CODE (trueop0) == CONST_VECTOR
	      && GET_CODE (trueop1) == CONST_VECTOR)
	    {
	      rtvec v = rtvec_alloc (n_elts);
	      unsigned int i;

	      for (i = 0; i < n_elts; i++)
		RTVEC_ELT (v, i) = ((sel & (HOST_WIDE_INT_1U << i))
				    ? CONST_VECTOR_ELT (trueop0, i)
				    : CONST_VECTOR_ELT (trueop1, i));
	      return gen_rtx_CONST_VECTOR (mode, v);
	    }

	  /* Replace (vec_merge (vec_merge a b m) c n) with (vec_merge b c n)
	     if no element from a appears in the result.  */
	  if (GET_CODE (op0) == VEC_MERGE)
	    {
	      tem = avoid_constant_pool_reference (XEXP (op0, 2));
	      if (CONST_INT_P (tem))
		{
		  unsigned HOST_WIDE_INT sel0 = UINTVAL (tem);
		  if (!(sel & sel0 & mask) && !side_effects_p (XEXP (op0, 0)))
		    return simplify_gen_ternary (code, mode, mode,
						 XEXP (op0, 1), op1, op2);
		  if (!(sel & ~sel0 & mask) && !side_effects_p (XEXP (op0, 1)))
		    return simplify_gen_ternary (code, mode, mode,
						 XEXP (op0, 0), op1, op2);
		}
	    }
	  if (GET_CODE (op1) == VEC_MERGE)
	    {
	      tem = avoid_constant_pool_reference (XEXP (op1, 2));
	      if (CONST_INT_P (tem))
		{
		  unsigned HOST_WIDE_INT sel1 = UINTVAL (tem);
		  if (!(~sel & sel1 & mask) && !side_effects_p (XEXP (op1, 0)))
		    return simplify_gen_ternary (code, mode, mode,
						 op0, XEXP (op1, 1), op2);
		  if (!(~sel & ~sel1 & mask) && !side_effects_p (XEXP (op1, 1)))
		    return simplify_gen_ternary (code, mode, mode,
						 op0, XEXP (op1, 0), op2);
		}
	    }

	  /* Replace (vec_merge (vec_duplicate (vec_select a parallel (i))) a 1 << i)
	     with a.  */
	  if (GET_CODE (op0) == VEC_DUPLICATE
	      && GET_CODE (XEXP (op0, 0)) == VEC_SELECT
	      && GET_CODE (XEXP (XEXP (op0, 0), 1)) == PARALLEL
	      && mode_nunits[GET_MODE (XEXP (op0, 0))] == 1)
	    {
	      tem = XVECEXP ((XEXP (XEXP (op0, 0), 1)), 0, 0);
	      if (CONST_INT_P (tem) && CONST_INT_P (op2))
		{
		  if (XEXP (XEXP (op0, 0), 0) == op1
		      && UINTVAL (op2) == HOST_WIDE_INT_1U << UINTVAL (tem))
		    return op1;
		}
	    }
	}

      if (rtx_equal_p (op0, op1)
	  && !side_effects_p (op2) && !side_effects_p (op1))
	return op0;

      break;

    default:
      gcc_unreachable ();
    }

  return 0;
}

/* Evaluate a SUBREG of a CONST_INT or CONST_WIDE_INT or CONST_DOUBLE
   or CONST_FIXED or CONST_VECTOR, returning another CONST_INT or
   CONST_WIDE_INT or CONST_DOUBLE or CONST_FIXED or CONST_VECTOR.

   Works by unpacking OP into a collection of 8-bit values
   represented as a little-endian array of 'unsigned char', selecting by BYTE,
   and then repacking them again for OUTERMODE.  */

static rtx
simplify_immed_subreg (machine_mode outermode, rtx op,
		       machine_mode innermode, unsigned int byte)
{
  enum {
    value_bit = 8,
    value_mask = (1 << value_bit) - 1
  };
  unsigned char value[MAX_BITSIZE_MODE_ANY_MODE / value_bit];
  int value_start;
  int i;
  int elem;

  int num_elem;
  rtx * elems;
  int elem_bitsize;
  rtx result_s = NULL;
  rtvec result_v = NULL;
  enum mode_class outer_class;
  scalar_mode outer_submode;
  int max_bitsize;

  /* Some ports misuse CCmode.  */
  if (GET_MODE_CLASS (outermode) == MODE_CC && CONST_INT_P (op))
    return op;

  /* We have no way to represent a complex constant at the rtl level.  */
  if (COMPLEX_MODE_P (outermode))
    return NULL_RTX;

  /* We support any size mode.  */
  max_bitsize = MAX (GET_MODE_BITSIZE (outermode),
		     GET_MODE_BITSIZE (innermode));

  /* Unpack the value.  */

  if (GET_CODE (op) == CONST_VECTOR)
    {
      num_elem = CONST_VECTOR_NUNITS (op);
      elems = &CONST_VECTOR_ELT (op, 0);
      elem_bitsize = GET_MODE_UNIT_BITSIZE (innermode);
    }
  else
    {
      num_elem = 1;
      elems = &op;
      elem_bitsize = max_bitsize;
    }
  /* If this asserts, it is too complicated; reducing value_bit may help.  */
  gcc_assert (BITS_PER_UNIT % value_bit == 0);
  /* I don't know how to handle endianness of sub-units.  */
  gcc_assert (elem_bitsize % BITS_PER_UNIT == 0);

  for (elem = 0; elem < num_elem; elem++)
    {
      unsigned char * vp;
      rtx el = elems[elem];

      /* Vectors are kept in target memory order.  (This is probably
	 a mistake.)  */
      {
	unsigned byte = (elem * elem_bitsize) / BITS_PER_UNIT;
	unsigned ibyte = (((num_elem - 1 - elem) * elem_bitsize)
			  / BITS_PER_UNIT);
	unsigned word_byte = WORDS_BIG_ENDIAN ? ibyte : byte;
	unsigned subword_byte = BYTES_BIG_ENDIAN ? ibyte : byte;
	unsigned bytele = (subword_byte % UNITS_PER_WORD
			 + (word_byte / UNITS_PER_WORD) * UNITS_PER_WORD);
	vp = value + (bytele * BITS_PER_UNIT) / value_bit;
      }

      switch (GET_CODE (el))
	{
	case CONST_INT:
	  for (i = 0;
	       i < HOST_BITS_PER_WIDE_INT && i < elem_bitsize;
	       i += value_bit)
	    *vp++ = INTVAL (el) >> i;
	  /* CONST_INTs are always logically sign-extended.  */
	  for (; i < elem_bitsize; i += value_bit)
	    *vp++ = INTVAL (el) < 0 ? -1 : 0;
	  break;

	case CONST_WIDE_INT:
	  {
	    rtx_mode_t val = rtx_mode_t (el, GET_MODE_INNER (innermode));
	    unsigned char extend = wi::sign_mask (val);
	    int prec = wi::get_precision (val);

	    for (i = 0; i < prec && i < elem_bitsize; i += value_bit)
	      *vp++ = wi::extract_uhwi (val, i, value_bit);
	    for (; i < elem_bitsize; i += value_bit)
	      *vp++ = extend;
	  }
	  break;

	case CONST_DOUBLE:
	  if (TARGET_SUPPORTS_WIDE_INT == 0 && GET_MODE (el) == VOIDmode)
	    {
	      unsigned char extend = 0;
	      /* If this triggers, someone should have generated a
		 CONST_INT instead.  */
	      gcc_assert (elem_bitsize > HOST_BITS_PER_WIDE_INT);

	      for (i = 0; i < HOST_BITS_PER_WIDE_INT; i += value_bit)
		*vp++ = CONST_DOUBLE_LOW (el) >> i;
	      while (i < HOST_BITS_PER_DOUBLE_INT && i < elem_bitsize)
		{
		  *vp++
		    = CONST_DOUBLE_HIGH (el) >> (i - HOST_BITS_PER_WIDE_INT);
		  i += value_bit;
		}

	      if (CONST_DOUBLE_HIGH (el) >> (HOST_BITS_PER_WIDE_INT - 1))
		extend = -1;
	      for (; i < elem_bitsize; i += value_bit)
		*vp++ = extend;
	    }
	  else
	    {
	      /* This is big enough for anything on the platform.  */
	      long tmp[MAX_BITSIZE_MODE_ANY_MODE / 32];
	      scalar_float_mode el_mode;

	      el_mode = as_a <scalar_float_mode> (GET_MODE (el));
	      int bitsize = GET_MODE_BITSIZE (el_mode);

	      gcc_assert (bitsize <= elem_bitsize);
	      gcc_assert (bitsize % value_bit == 0);

	      real_to_target (tmp, CONST_DOUBLE_REAL_VALUE (el),
			      GET_MODE (el));

	      /* real_to_target produces its result in words affected by
		 FLOAT_WORDS_BIG_ENDIAN.  However, we ignore this,
		 and use WORDS_BIG_ENDIAN instead; see the documentation
	         of SUBREG in rtl.texi.  */
	      for (i = 0; i < bitsize; i += value_bit)
		{
		  int ibase;
		  if (WORDS_BIG_ENDIAN)
		    ibase = bitsize - 1 - i;
		  else
		    ibase = i;
		  *vp++ = tmp[ibase / 32] >> i % 32;
		}

	      /* It shouldn't matter what's done here, so fill it with
		 zero.  */
	      for (; i < elem_bitsize; i += value_bit)
		*vp++ = 0;
	    }
	  break;

        case CONST_FIXED:
	  if (elem_bitsize <= HOST_BITS_PER_WIDE_INT)
	    {
	      for (i = 0; i < elem_bitsize; i += value_bit)
		*vp++ = CONST_FIXED_VALUE_LOW (el) >> i;
	    }
	  else
	    {
	      for (i = 0; i < HOST_BITS_PER_WIDE_INT; i += value_bit)
		*vp++ = CONST_FIXED_VALUE_LOW (el) >> i;
              for (; i < HOST_BITS_PER_DOUBLE_INT && i < elem_bitsize;
		   i += value_bit)
		*vp++ = CONST_FIXED_VALUE_HIGH (el)
			>> (i - HOST_BITS_PER_WIDE_INT);
	      for (; i < elem_bitsize; i += value_bit)
		*vp++ = 0;
	    }
          break;

	default:
	  gcc_unreachable ();
	}
    }

  /* Now, pick the right byte to start with.  */
  /* Renumber BYTE so that the least-significant byte is byte 0.  A special
     case is paradoxical SUBREGs, which shouldn't be adjusted since they
     will already have offset 0.  */
  if (GET_MODE_SIZE (innermode) >= GET_MODE_SIZE (outermode))
    {
      unsigned ibyte = (GET_MODE_SIZE (innermode) - GET_MODE_SIZE (outermode)
			- byte);
      unsigned word_byte = WORDS_BIG_ENDIAN ? ibyte : byte;
      unsigned subword_byte = BYTES_BIG_ENDIAN ? ibyte : byte;
      byte = (subword_byte % UNITS_PER_WORD
	      + (word_byte / UNITS_PER_WORD) * UNITS_PER_WORD);
    }

  /* BYTE should still be inside OP.  (Note that BYTE is unsigned,
     so if it's become negative it will instead be very large.)  */
  gcc_assert (byte < GET_MODE_SIZE (innermode));

  /* Convert from bytes to chunks of size value_bit.  */
  value_start = byte * (BITS_PER_UNIT / value_bit);

  /* Re-pack the value.  */
  num_elem = GET_MODE_NUNITS (outermode);

  if (VECTOR_MODE_P (outermode))
    {
      result_v = rtvec_alloc (num_elem);
      elems = &RTVEC_ELT (result_v, 0);
    }
  else
    elems = &result_s;

  outer_submode = GET_MODE_INNER (outermode);
  outer_class = GET_MODE_CLASS (outer_submode);
  elem_bitsize = GET_MODE_BITSIZE (outer_submode);

  gcc_assert (elem_bitsize % value_bit == 0);
  gcc_assert (elem_bitsize + value_start * value_bit <= max_bitsize);

  for (elem = 0; elem < num_elem; elem++)
    {
      unsigned char *vp;

      /* Vectors are stored in target memory order.  (This is probably
	 a mistake.)  */
      {
	unsigned byte = (elem * elem_bitsize) / BITS_PER_UNIT;
	unsigned ibyte = (((num_elem - 1 - elem) * elem_bitsize)
			  / BITS_PER_UNIT);
	unsigned word_byte = WORDS_BIG_ENDIAN ? ibyte : byte;
	unsigned subword_byte = BYTES_BIG_ENDIAN ? ibyte : byte;
	unsigned bytele = (subword_byte % UNITS_PER_WORD
			 + (word_byte / UNITS_PER_WORD) * UNITS_PER_WORD);
	vp = value + value_start + (bytele * BITS_PER_UNIT) / value_bit;
      }

      switch (outer_class)
	{
	case MODE_INT:
	case MODE_PARTIAL_INT:
	  {
	    int u;
	    int base = 0;
	    int units
	      = (GET_MODE_BITSIZE (outer_submode) + HOST_BITS_PER_WIDE_INT - 1)
	      / HOST_BITS_PER_WIDE_INT;
	    HOST_WIDE_INT tmp[MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_WIDE_INT];
	    wide_int r;

	    if (GET_MODE_PRECISION (outer_submode) > MAX_BITSIZE_MODE_ANY_INT)
	      return NULL_RTX;
	    for (u = 0; u < units; u++)
	      {
		unsigned HOST_WIDE_INT buf = 0;
		for (i = 0;
		     i < HOST_BITS_PER_WIDE_INT && base + i < elem_bitsize;
		     i += value_bit)
		  buf |= (unsigned HOST_WIDE_INT)(*vp++ & value_mask) << i;

		tmp[u] = buf;
		base += HOST_BITS_PER_WIDE_INT;
	      }
	    r = wide_int::from_array (tmp, units,
				      GET_MODE_PRECISION (outer_submode));
#if TARGET_SUPPORTS_WIDE_INT == 0
	    /* Make sure r will fit into CONST_INT or CONST_DOUBLE.  */
	    if (wi::min_precision (r, SIGNED) > HOST_BITS_PER_DOUBLE_INT)
	      return NULL_RTX;
#endif
	    elems[elem] = immed_wide_int_const (r, outer_submode);
	  }
	  break;

	case MODE_FLOAT:
	case MODE_DECIMAL_FLOAT:
	  {
	    REAL_VALUE_TYPE r;
	    long tmp[MAX_BITSIZE_MODE_ANY_MODE / 32] = { 0 };

	    /* real_from_target wants its input in words affected by
	       FLOAT_WORDS_BIG_ENDIAN.  However, we ignore this,
	       and use WORDS_BIG_ENDIAN instead; see the documentation
	       of SUBREG in rtl.texi.  */
	    for (i = 0; i < elem_bitsize; i += value_bit)
	      {
		int ibase;
		if (WORDS_BIG_ENDIAN)
		  ibase = elem_bitsize - 1 - i;
		else
		  ibase = i;
		tmp[ibase / 32] |= (*vp++ & value_mask) << i % 32;
	      }

	    real_from_target (&r, tmp, outer_submode);
	    elems[elem] = const_double_from_real_value (r, outer_submode);
	  }
	  break;

	case MODE_FRACT:
	case MODE_UFRACT:
	case MODE_ACCUM:
	case MODE_UACCUM:
	  {
	    FIXED_VALUE_TYPE f;
	    f.data.low = 0;
	    f.data.high = 0;
	    f.mode = outer_submode;

	    for (i = 0;
		 i < HOST_BITS_PER_WIDE_INT && i < elem_bitsize;
		 i += value_bit)
	      f.data.low |= (unsigned HOST_WIDE_INT)(*vp++ & value_mask) << i;
	    for (; i < elem_bitsize; i += value_bit)
	      f.data.high |= ((unsigned HOST_WIDE_INT)(*vp++ & value_mask)
			     << (i - HOST_BITS_PER_WIDE_INT));

	    elems[elem] = CONST_FIXED_FROM_FIXED_VALUE (f, outer_submode);
          }
          break;

	default:
	  gcc_unreachable ();
	}
    }
  if (VECTOR_MODE_P (outermode))
    return gen_rtx_CONST_VECTOR (outermode, result_v);
  else
    return result_s;
}

/* Simplify SUBREG:OUTERMODE(OP:INNERMODE, BYTE)
   Return 0 if no simplifications are possible.  */
rtx
simplify_subreg (machine_mode outermode, rtx op,
		 machine_mode innermode, unsigned int byte)
{
  /* Little bit of sanity checking.  */
  gcc_assert (innermode != VOIDmode);
  gcc_assert (outermode != VOIDmode);
  gcc_assert (innermode != BLKmode);
  gcc_assert (outermode != BLKmode);

  gcc_assert (GET_MODE (op) == innermode
	      || GET_MODE (op) == VOIDmode);

  if ((byte % GET_MODE_SIZE (outermode)) != 0)
    return NULL_RTX;

  if (byte >= GET_MODE_SIZE (innermode))
    return NULL_RTX;

  if (outermode == innermode && !byte)
    return op;

  if (CONST_SCALAR_INT_P (op)
      || CONST_DOUBLE_AS_FLOAT_P (op)
      || GET_CODE (op) == CONST_FIXED
      || GET_CODE (op) == CONST_VECTOR)
    return simplify_immed_subreg (outermode, op, innermode, byte);

  /* Changing mode twice with SUBREG => just change it once,
     or not at all if changing back op starting mode.  */
  if (GET_CODE (op) == SUBREG)
    {
      machine_mode innermostmode = GET_MODE (SUBREG_REG (op));
      rtx newx;

      if (outermode == innermostmode
	  && byte == 0 && SUBREG_BYTE (op) == 0)
	return SUBREG_REG (op);

      /* Work out the memory offset of the final OUTERMODE value relative
	 to the inner value of OP.  */
      HOST_WIDE_INT mem_offset = subreg_memory_offset (outermode,
						       innermode, byte);
      HOST_WIDE_INT op_mem_offset = subreg_memory_offset (op);
      HOST_WIDE_INT final_offset = mem_offset + op_mem_offset;

      /* See whether resulting subreg will be paradoxical.  */
      if (!paradoxical_subreg_p (outermode, innermostmode))
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
	  HOST_WIDE_INT required_offset
	    = subreg_memory_offset (outermode, innermostmode, 0);
	  if (final_offset != required_offset)
	    return NULL_RTX;
	  /* Paradoxical subregs always have byte offset 0.  */
	  final_offset = 0;
	}

      /* Recurse for further possible simplifications.  */
      newx = simplify_subreg (outermode, SUBREG_REG (op), innermostmode,
			      final_offset);
      if (newx)
	return newx;
      if (validate_subreg (outermode, innermostmode,
			   SUBREG_REG (op), final_offset))
	{
	  newx = gen_rtx_SUBREG (outermode, SUBREG_REG (op), final_offset);
	  if (SUBREG_PROMOTED_VAR_P (op)
	      && SUBREG_PROMOTED_SIGN (op) >= 0
	      && GET_MODE_CLASS (outermode) == MODE_INT
	      && IN_RANGE (GET_MODE_SIZE (outermode),
			   GET_MODE_SIZE (innermode),
			   GET_MODE_SIZE (innermostmode))
	      && subreg_lowpart_p (newx))
	    {
	      SUBREG_PROMOTED_VAR_P (newx) = 1;
	      SUBREG_PROMOTED_SET (newx, SUBREG_PROMOTED_GET (op));
	    }
	  return newx;
	}
      return NULL_RTX;
    }

  /* SUBREG of a hard register => just change the register number
     and/or mode.  If the hard register is not valid in that mode,
     suppress this simplification.  If the hard register is the stack,
     frame, or argument pointer, leave this as a SUBREG.  */

  if (REG_P (op) && HARD_REGISTER_P (op))
    {
      unsigned int regno, final_regno;

      regno = REGNO (op);
      final_regno = simplify_subreg_regno (regno, innermode, byte, outermode);
      if (HARD_REGISTER_NUM_P (final_regno))
	{
	  rtx x = gen_rtx_REG_offset (op, outermode, final_regno,
				      subreg_memory_offset (outermode,
							    innermode, byte));

	  /* Propagate original regno.  We don't have any way to specify
	     the offset inside original regno, so do so only for lowpart.
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

  if (MEM_P (op)
      && ! mode_dependent_address_p (XEXP (op, 0), MEM_ADDR_SPACE (op))
      /* Allow splitting of volatile memory references in case we don't
         have instruction to move the whole thing.  */
      && (! MEM_VOLATILE_P (op)
	  || ! have_insn_for (SET, innermode))
      && GET_MODE_SIZE (outermode) <= GET_MODE_SIZE (GET_MODE (op)))
    return adjust_address_nv (op, outermode, byte);

  /* Handle complex or vector values represented as CONCAT or VEC_CONCAT
     of two parts.  */
  if (GET_CODE (op) == CONCAT
      || GET_CODE (op) == VEC_CONCAT)
    {
      unsigned int part_size, final_offset;
      rtx part, res;

      machine_mode part_mode = GET_MODE (XEXP (op, 0));
      if (part_mode == VOIDmode)
	part_mode = GET_MODE_INNER (GET_MODE (op));
      part_size = GET_MODE_SIZE (part_mode);
      if (byte < part_size)
	{
	  part = XEXP (op, 0);
	  final_offset = byte;
	}
      else
	{
	  part = XEXP (op, 1);
	  final_offset = byte - part_size;
	}

      if (final_offset + GET_MODE_SIZE (outermode) > part_size)
	return NULL_RTX;

      part_mode = GET_MODE (part);
      if (part_mode == VOIDmode)
	part_mode = GET_MODE_INNER (GET_MODE (op));
      res = simplify_subreg (outermode, part, part_mode, final_offset);
      if (res)
	return res;
      if (validate_subreg (outermode, part_mode, part, final_offset))
	return gen_rtx_SUBREG (outermode, part, final_offset);
      return NULL_RTX;
    }

  /* A SUBREG resulting from a zero extension may fold to zero if
     it extracts higher bits that the ZERO_EXTEND's source bits.  */
  if (GET_CODE (op) == ZERO_EXTEND && SCALAR_INT_MODE_P (innermode))
    {
      unsigned int bitpos = subreg_lsb_1 (outermode, innermode, byte);
      if (bitpos >= GET_MODE_PRECISION (GET_MODE (XEXP (op, 0))))
	return CONST0_RTX (outermode);
    }

  scalar_int_mode int_outermode, int_innermode;
  if (is_a <scalar_int_mode> (outermode, &int_outermode)
      && is_a <scalar_int_mode> (innermode, &int_innermode)
      && (GET_MODE_PRECISION (int_outermode)
	  < GET_MODE_PRECISION (int_innermode))
      && byte == subreg_lowpart_offset (int_outermode, int_innermode))
    {
      rtx tem = simplify_truncation (int_outermode, op, int_innermode);
      if (tem)
	return tem;
    }

  return NULL_RTX;
}

/* Make a SUBREG operation or equivalent if it folds.  */

rtx
simplify_gen_subreg (machine_mode outermode, rtx op,
		     machine_mode innermode, unsigned int byte)
{
  rtx newx;

  newx = simplify_subreg (outermode, op, innermode, byte);
  if (newx)
    return newx;

  if (GET_CODE (op) == SUBREG
      || GET_CODE (op) == CONCAT
      || GET_MODE (op) == VOIDmode)
    return NULL_RTX;

  if (validate_subreg (outermode, innermode, op, byte))
    return gen_rtx_SUBREG (outermode, op, byte);

  return NULL_RTX;
}

/* Generates a subreg to get the least significant part of EXPR (in mode
   INNER_MODE) to OUTER_MODE.  */

rtx
lowpart_subreg (machine_mode outer_mode, rtx expr,
			     machine_mode inner_mode)
{
  return simplify_gen_subreg (outer_mode, expr, inner_mode,
			      subreg_lowpart_offset (outer_mode, inner_mode));
}

/* Simplify X, an rtx expression.

   Return the simplified expression or NULL if no simplifications
   were possible.

   This is the preferred entry point into the simplification routines;
   however, we still allow passes to call the more specific routines.

   Right now GCC has three (yes, three) major bodies of RTL simplification
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
simplify_rtx (const_rtx x)
{
  const enum rtx_code code = GET_CODE (x);
  const machine_mode mode = GET_MODE (x);

  switch (GET_RTX_CLASS (code))
    {
    case RTX_UNARY:
      return simplify_unary_operation (code, mode,
				       XEXP (x, 0), GET_MODE (XEXP (x, 0)));
    case RTX_COMM_ARITH:
      if (swap_commutative_operands_p (XEXP (x, 0), XEXP (x, 1)))
	return simplify_gen_binary (code, mode, XEXP (x, 1), XEXP (x, 0));

      /* Fall through.  */

    case RTX_BIN_ARITH:
      return simplify_binary_operation (code, mode, XEXP (x, 0), XEXP (x, 1));

    case RTX_TERNARY:
    case RTX_BITFIELD_OPS:
      return simplify_ternary_operation (code, mode, GET_MODE (XEXP (x, 0)),
					 XEXP (x, 0), XEXP (x, 1),
					 XEXP (x, 2));

    case RTX_COMPARE:
    case RTX_COMM_COMPARE:
      return simplify_relational_operation (code, mode,
                                            ((GET_MODE (XEXP (x, 0))
                                             != VOIDmode)
                                            ? GET_MODE (XEXP (x, 0))
                                            : GET_MODE (XEXP (x, 1))),
                                            XEXP (x, 0),
                                            XEXP (x, 1));

    case RTX_EXTRA:
      if (code == SUBREG)
	return simplify_subreg (mode, SUBREG_REG (x),
				GET_MODE (SUBREG_REG (x)),
				SUBREG_BYTE (x));
      break;

    case RTX_OBJ:
      if (code == LO_SUM)
	{
	  /* Convert (lo_sum (high FOO) FOO) to FOO.  */
	  if (GET_CODE (XEXP (x, 0)) == HIGH
	      && rtx_equal_p (XEXP (XEXP (x, 0), 0), XEXP (x, 1)))
	  return XEXP (x, 1);
	}
      break;

    default:
      break;
    }
  return NULL;
}
