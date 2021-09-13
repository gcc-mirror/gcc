/* intrinsics.cc -- D language compiler intrinsics.
   Copyright (C) 2006-2021 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/declaration.h"
#include "dmd/identifier.h"
#include "dmd/mangle.h"
#include "dmd/mangle.h"
#include "dmd/module.h"
#include "dmd/template.h"

#include "tm.h"
#include "function.h"
#include "tree.h"
#include "fold-const.h"
#include "stringpool.h"
#include "builtins.h"

#include "d-tree.h"


/* An internal struct used to hold information on D intrinsics.  */

struct intrinsic_decl
{
  /* The DECL_INTRINSIC_CODE of this decl.  */
  intrinsic_code code;

  /* The DECL_FUNCTION_CODE of this decl, if it directly maps to any.  */
  built_in_function built_in;

  /* The name of the intrinsic.  */
  const char *name;

  /* The module where the intrinsic is located.  */
  const char *module;

  /* The mangled signature decoration of the intrinsic.  */
  const char *deco;

  /* True if the intrinsic is only handled in CTFE.  */
  bool ctfeonly;
};

static const intrinsic_decl intrinsic_decls[] =
{
#define DEF_D_INTRINSIC(CODE, BUILTIN, NAME, MODULE, DECO, CTFE) \
    { CODE, BUILTIN, NAME, MODULE, DECO, CTFE },

#include "intrinsics.def"

#undef DEF_D_INTRINSIC
};

/* Checks if DECL is an intrinsic or run time library function that requires
   special processing.  Sets DECL_INTRINSIC_CODE so it can be identified
   later in maybe_expand_intrinsic.  */

void
maybe_set_intrinsic (FuncDeclaration *decl)
{
  if (!decl->ident || decl->builtin != BUILTINunknown)
    return;

  /* The builtin flag is updated only if we can evaluate the intrinsic
     at compile-time.  Such as the math or bitop intrinsics.  */
  decl->builtin = BUILTINunimp;

  /* Check if it's a compiler intrinsic.  We only require that any
     internally recognised intrinsics are declared in a module with
     an explicit module declaration.  */
  Module *m = decl->getModule ();

  if (!m || !m->md)
    return;

  TemplateInstance *ti = decl->isInstantiated ();
  TemplateDeclaration *td = ti ? ti->tempdecl->isTemplateDeclaration () : NULL;

  const char *tname = decl->ident->toChars ();
  const char *tmodule = m->md->toChars ();
  const char *tdeco = (td == NULL) ? decl->type->deco : NULL;

  /* Look through all D intrinsics.  */
  for (size_t i = 0; i < (int) INTRINSIC_LAST; i++)
    {
      if (!intrinsic_decls[i].name)
	continue;

      if (strcmp (intrinsic_decls[i].name, tname) != 0
	  || strcmp (intrinsic_decls[i].module, tmodule) != 0)
	continue;

      /* Instantiated functions would have the wrong type deco, get it from the
	 template member instead.  */
      if (tdeco == NULL)
	{
	  if (!td || !td->onemember)
	    return;

	  FuncDeclaration *fd = td->onemember->isFuncDeclaration ();
	  if (fd == NULL)
	    return;

	  OutBuffer buf;
	  mangleToBuffer (fd->type, &buf);
	  tdeco = buf.extractChars ();
	}

      /* Matching the type deco may be a bit too strict, as it means that all
	 function attributes that end up in the signature must be kept aligned
	 between the compiler and library declaration.  */
      if (strcmp (intrinsic_decls[i].deco, tdeco) == 0)
	{
	  intrinsic_code code = intrinsic_decls[i].code;

	  if (decl->csym == NULL)
	    get_symbol_decl (decl);

	  /* If there is no function body, then the implementation is always
	     provided by the compiler.  */
	  if (!decl->fbody)
	    set_decl_built_in_function (decl->csym, BUILT_IN_FRONTEND, code);

	  /* Infer whether the intrinsic can be used for CTFE, let the
	     front-end know that it can be evaluated at compile-time.  */
	  switch (code)
	    {
	    case INTRINSIC_VA_ARG:
	    case INTRINSIC_C_VA_ARG:
	    case INTRINSIC_VASTART:
	    case INTRINSIC_ADDS:
	    case INTRINSIC_ADDSL:
	    case INTRINSIC_ADDU:
	    case INTRINSIC_ADDUL:
	    case INTRINSIC_SUBS:
	    case INTRINSIC_SUBSL:
	    case INTRINSIC_SUBU:
	    case INTRINSIC_SUBUL:
	    case INTRINSIC_MULS:
	    case INTRINSIC_MULSL:
	    case INTRINSIC_MULU:
	    case INTRINSIC_MULUI:
	    case INTRINSIC_MULUL:
	    case INTRINSIC_NEGS:
	    case INTRINSIC_NEGSL:
	    case INTRINSIC_VLOAD8:
	    case INTRINSIC_VLOAD16:
	    case INTRINSIC_VLOAD32:
	    case INTRINSIC_VLOAD64:
	    case INTRINSIC_VSTORE8:
	    case INTRINSIC_VSTORE16:
	    case INTRINSIC_VSTORE32:
	    case INTRINSIC_VSTORE64:
	      break;

	    case INTRINSIC_POW:
	    {
	      /* Check that this overload of pow() is has an equivalent
		 built-in function.  It could be `int pow(int, int)'.  */
	      tree rettype = TREE_TYPE (TREE_TYPE (decl->csym));
	      if (mathfn_built_in (rettype, BUILT_IN_POW) != NULL_TREE)
		decl->builtin = BUILTINgcc;
	      break;
	    }

	    default:
	      decl->builtin = BUILTINgcc;
	      break;
	    }

	  /* The intrinsic was marked as CTFE-only.  */
	  if (intrinsic_decls[i].ctfeonly)
	    DECL_BUILT_IN_CTFE (decl->csym) = 1;

	  DECL_INTRINSIC_CODE (decl->csym) = code;
	  break;
	}
    }
}

/* Construct a function call to the built-in function CODE, N is the number of
   arguments, and the `...' parameters are the argument expressions.
   The original call expression is held in CALLEXP.  */

static tree
call_builtin_fn (tree callexp, built_in_function code, int n, ...)
{
  tree *argarray = XALLOCAVEC (tree, n);
  va_list ap;

  va_start (ap, n);
  for (int i = 0; i < n; i++)
    argarray[i] = va_arg (ap, tree);
  va_end (ap);

  tree exp = build_call_expr_loc_array (EXPR_LOCATION (callexp),
					builtin_decl_explicit (code),
					n, argarray);
  return convert (TREE_TYPE (callexp), fold (exp));
}

/* Expand a front-end instrinsic call to bsf().  This takes one argument,
   the signature to which can be either:

	int bsf (uint arg);
	int bsf (ulong arg);

   This scans all bits in the given argument starting with the first,
   returning the bit number of the first bit set.  The original call
   expression is held in CALLEXP.  */

static tree
expand_intrinsic_bsf (tree callexp)
{
  /* The bsr() intrinsic gets turned into __builtin_ctz(arg).
     The return value is supposed to be undefined if arg is zero.  */
  tree arg = CALL_EXPR_ARG (callexp, 0);
  int argsize = TYPE_PRECISION (TREE_TYPE (arg));

  /* Which variant of __builtin_ctz* should we call?  */
  built_in_function code = (argsize <= INT_TYPE_SIZE) ? BUILT_IN_CTZ
    : (argsize <= LONG_TYPE_SIZE) ? BUILT_IN_CTZL
    : (argsize <= LONG_LONG_TYPE_SIZE) ? BUILT_IN_CTZLL
    : END_BUILTINS;

  gcc_assert (code != END_BUILTINS);

  return call_builtin_fn (callexp, code, 1, arg);
}

/* Expand a front-end instrinsic call to bsr().  This takes one argument,
   the signature to which can be either:

	int bsr (uint arg);
	int bsr (ulong arg);

   This scans all bits in the given argument from the most significant bit
   to the least significant, returning the bit number of the first bit set.
   The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_bsr (tree callexp)
{
  /* The bsr() intrinsic gets turned into (size - 1) - __builtin_clz(arg).
     The return value is supposed to be undefined if arg is zero.  */
  tree arg = CALL_EXPR_ARG (callexp, 0);
  tree type = TREE_TYPE (arg);
  int argsize = TYPE_PRECISION (type);

  /* Which variant of __builtin_clz* should we call?  */
  built_in_function code = (argsize <= INT_TYPE_SIZE) ? BUILT_IN_CLZ
    : (argsize <= LONG_TYPE_SIZE) ? BUILT_IN_CLZL
    : (argsize <= LONG_LONG_TYPE_SIZE) ? BUILT_IN_CLZLL
    : END_BUILTINS;

  gcc_assert (code != END_BUILTINS);

  tree result = call_builtin_fn (callexp, code, 1, arg);

  /* Handle int -> long conversions.  */
  if (TREE_TYPE (result) != type)
    result = fold_convert (type, result);

  result = fold_build2 (MINUS_EXPR, type,
			build_integer_cst (argsize - 1, type), result);
  return fold_convert (TREE_TYPE (callexp), result);
}

/* Expand a front-end intrinsic call to INTRINSIC, which is either a call to
   bt(), btc(), btr(), or bts().  These intrinsics expect to take two arguments,
   the signature to which is:

	int bt (size_t* ptr, size_t bitnum);

   All intrinsics test if a bit is set and return the result of that condition.
   Variants of `bt' will then update that bit. `btc' compliments the bit, `bts'
   sets the bit, and `btr' resets the bit.  The original call expression is
   held in CALLEXP.  */

static tree
expand_intrinsic_bt (intrinsic_code intrinsic, tree callexp)
{
  tree ptr = CALL_EXPR_ARG (callexp, 0);
  tree bitnum = CALL_EXPR_ARG (callexp, 1);
  tree type = TREE_TYPE (TREE_TYPE (ptr));

  /* size_t bitsize = sizeof(*ptr) * BITS_PER_UNIT;  */
  tree bitsize = fold_convert (type, TYPE_SIZE (TREE_TYPE (ptr)));

  /* ptr[bitnum / bitsize]  */
  ptr = build_array_index (ptr, fold_build2 (TRUNC_DIV_EXPR, type,
					     bitnum, bitsize));
  ptr = indirect_ref (type, ptr);

  /* mask = 1 << (bitnum % bitsize);  */
  bitnum = fold_build2 (TRUNC_MOD_EXPR, type, bitnum, bitsize);
  bitnum = fold_build2 (LSHIFT_EXPR, type, build_one_cst (type), bitnum);

  /* cond = ptr[bitnum / size] & mask;  */
  tree cond = fold_build2 (BIT_AND_EXPR, type, ptr, bitnum);

  /* cond ? -1 : 0;  */
  cond = build_condition (TREE_TYPE (callexp), d_truthvalue_conversion (cond),
			  build_minus_one_cst (TREE_TYPE (callexp)),
			  build_zero_cst (TREE_TYPE (callexp)));

  /* Update the bit as needed, only testing the bit for bt().  */
  tree_code code;

  switch (intrinsic)
    {
    case INTRINSIC_BT:
    case INTRINSIC_BT64:
      return cond;

    case INTRINSIC_BTC:
    case INTRINSIC_BTC64:
      code = BIT_XOR_EXPR;
      break;

    case INTRINSIC_BTR:
    case INTRINSIC_BTR64:
      bitnum = fold_build1 (BIT_NOT_EXPR, TREE_TYPE (bitnum), bitnum);
      code = BIT_AND_EXPR;
      break;

    case INTRINSIC_BTS:
    case INTRINSIC_BTS64:
      code = BIT_IOR_EXPR;
      break;

    default:
      gcc_unreachable ();
    }

  /* ptr[bitnum / size] op= mask;  */
  ptr = modify_expr (ptr, fold_build2 (code, TREE_TYPE (ptr), ptr, bitnum));

  /* Store the condition result in a temporary, and return expressions in
     correct order of evaluation.  */
  tree tmp = build_local_temp (TREE_TYPE (callexp));
  cond = modify_expr (tmp, cond);

  return compound_expr (cond, compound_expr (ptr, tmp));
}

/* Expand a front-end intrinsic call to popcnt().  This takes one argument, the
   signature to which can be either:

	int popcnt (uint arg);
	int popcnt (ulong arg);

   Calculates the number of set bits in an integer.  The original call
   expression is held in CALLEXP.  */

static tree
expand_intrinsic_popcnt (tree callexp)
{
  tree arg = CALL_EXPR_ARG (callexp, 0);
  int argsize = TYPE_PRECISION (TREE_TYPE (arg));

  /* Which variant of __builtin_popcount* should we call?  */
  built_in_function code = (argsize <= INT_TYPE_SIZE) ? BUILT_IN_POPCOUNT
    : (argsize <= LONG_TYPE_SIZE) ? BUILT_IN_POPCOUNTL
    : (argsize <= LONG_LONG_TYPE_SIZE) ? BUILT_IN_POPCOUNTLL
    : END_BUILTINS;

  gcc_assert (code != END_BUILTINS);

  return call_builtin_fn (callexp, code, 1, arg);
}

/* Expand a front-end intrinsic call to INTRINSIC, which is either a call to
   rol() or ror().  These intrinsics expect to take one or two arguments,
   the signature to which can be either:

	T rol(T) (const T value, const uint count);
	T rol(uint count, T) (const T value);
	T ror(T) (const T value, const uint count);
	T ror(uint count, T) (const T value);

   This bitwise rotates VALUE left or right by COUNT bit positions.  */

static tree
expand_intrinsic_rotate (intrinsic_code intrinsic, tree callexp)
{
  tree type = TREE_TYPE (callexp);
  tree value = CALL_EXPR_ARG (callexp, 0);
  tree count;
  tree_code code;

  /* Get the equivalent tree code for the intrinsic.  */
  if (intrinsic == INTRINSIC_ROL || intrinsic == INTRINSIC_ROL_TIARG)
    code = LROTATE_EXPR;
  else if (intrinsic == INTRINSIC_ROR || intrinsic == INTRINSIC_ROR_TIARG)
    code = RROTATE_EXPR;
  else
    gcc_unreachable ();

  /* Get the COUNT parameter.  Either from the call expression arguments or the
     template instantiation arguments.  */
  if (intrinsic == INTRINSIC_ROL || intrinsic == INTRINSIC_ROR)
    count = CALL_EXPR_ARG (callexp, 1);
  else
    {
      tree callee = CALL_EXPR_FN (callexp);

      if (TREE_CODE (callee) == ADDR_EXPR)
	callee = TREE_OPERAND (callee, 0);

      /* Retrieve from the encoded template instantation.  */
      TemplateInstance *ti = DECL_LANG_FRONTEND (callee)->isInstantiated ();
      gcc_assert (ti && ti->tiargs && ti->tiargs->length == 2);

      Expression *e = isExpression ((*ti->tiargs)[0]);
      gcc_assert (e && e->op == TOKint64);
      count = build_expr (e, true);
    }

  return fold_build2 (code, type, value, count);
}

/* Expand a front-end intrinsic call to copysign().  This takes two arguments,
   the signature to which can be either:

	float copysign (T to, float from);
	double copysign (T to, double from);
	real copysign (T to, real from);

   This computes a value composed of TO with the sign bit of FROM.  The original
   call expression is held in CALLEXP.  */

static tree
expand_intrinsic_copysign (tree callexp)
{
  tree to = CALL_EXPR_ARG (callexp, 0);
  tree from = CALL_EXPR_ARG (callexp, 1);
  tree type = TREE_TYPE (to);

  /* Convert parameters to the same type.  Prefer the first parameter unless it
     is an integral type.  */
  if (INTEGRAL_TYPE_P (type))
    {
      to = fold_convert (TREE_TYPE (from), to);
      type = TREE_TYPE (to);
    }
  else
    from = fold_convert (type, from);

  /* Which variant of __builtin_copysign* should we call?  */
  built_in_function code = (type == float_type_node) ? BUILT_IN_COPYSIGNF
    : (type == double_type_node) ? BUILT_IN_COPYSIGN
    : (type == long_double_type_node) ? BUILT_IN_COPYSIGNL
    : END_BUILTINS;

  gcc_assert (code != END_BUILTINS);

  return call_builtin_fn (callexp, code, 2, to, from);
}

/* Expand a front-end intrinsic call to pow().  This takes two arguments, the
   signature to which can be either:

	float pow (float base, T exponent);
	double pow (double base, T exponent);
	real pow (real base, T exponent);

   This computes the value of BASE raised to the power of EXPONENT.
   The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_pow (tree callexp)
{
  tree base = CALL_EXPR_ARG (callexp, 0);
  tree exponent = CALL_EXPR_ARG (callexp, 1);
  tree exptype = TREE_TYPE (exponent);

  /* Which variant of __builtin_pow* should we call?  */
  built_in_function code = SCALAR_FLOAT_TYPE_P (exptype) ? BUILT_IN_POW
    : INTEGRAL_TYPE_P (exptype) ? BUILT_IN_POWI
    : END_BUILTINS;
  gcc_assert (code != END_BUILTINS);

  tree builtin = mathfn_built_in (TREE_TYPE (base), code);
  gcc_assert (builtin != NULL_TREE);

  return call_builtin_fn (callexp, DECL_FUNCTION_CODE (builtin), 2,
			  base, exponent);
}

/* Expand a front-end intrinsic call to toPrec().  This takes one argument, the
   signature to which can be either:

	T toPrec(T)(float f);
	T toPrec(T)(double f);
	T toPrec(T)(real f);

    This rounds the argument F to the precision of the specified floating
    point type T.  The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_toprec (tree callexp)
{
  tree f = CALL_EXPR_ARG (callexp, 0);
  tree type = TREE_TYPE (callexp);

  return convert (type, f);
}

/* Expand a front-end intrinsic call to va_arg().  This takes either one or two
   arguments, the signature to which can be either:

	T va_arg(T) (ref va_list ap);
	void va_arg(T) (va_list ap, ref T parmn);

   This retrieves the next variadic parameter that is type T from the given
   va_list.  If also given, store the value into parmn, otherwise return it.
   The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_vaarg (tree callexp)
{
  tree ap = CALL_EXPR_ARG (callexp, 0);
  tree parmn = NULL_TREE;
  tree type;

  STRIP_NOPS (ap);

  if (call_expr_nargs (callexp) == 1)
    type = TREE_TYPE (callexp);
  else
    {
      parmn = CALL_EXPR_ARG (callexp, 1);
      STRIP_NOPS (parmn);

      /* The `ref' argument to va_arg is either an address or reference,
	 get the value of it.  */
      if (TREE_CODE (parmn) == PARM_DECL && POINTER_TYPE_P (TREE_TYPE (parmn)))
	parmn = build_deref (parmn);
      else
	{
	  gcc_assert (TREE_CODE (parmn) == ADDR_EXPR);
	  parmn = TREE_OPERAND (parmn, 0);
	}

      type = TREE_TYPE (parmn);
    }

  /* (T) VA_ARG_EXP<ap>;  */
  tree exp = build1_loc (EXPR_LOCATION (callexp), VA_ARG_EXPR, type, ap);

  /* parmn = (T) VA_ARG_EXP<ap>;  */
  if (parmn != NULL_TREE)
    exp = modify_expr (parmn, exp);

  return exp;
}

/* Expand a front-end intrinsic call to va_start(), which takes two arguments,
   the signature to which is:

	void va_start(T) (out va_list ap, ref T parmn);

   This initializes the va_list type, where parmn should be the last named
   parameter.  The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_vastart (tree callexp)
{
  tree ap = CALL_EXPR_ARG (callexp, 0);
  tree parmn = CALL_EXPR_ARG (callexp, 1);

  STRIP_NOPS (ap);
  STRIP_NOPS (parmn);

  /* The va_list argument should already have its address taken.  The second
     argument, however, is inout and that needs to be fixed to prevent a
     warning.  Could be casting, so need to check type too?  */
  gcc_assert (TREE_CODE (ap) == ADDR_EXPR
	      || (TREE_CODE (ap) == PARM_DECL
		  && POINTER_TYPE_P (TREE_TYPE (ap))));

  /* Assuming nobody tries to change the return type.  */
  if (TREE_CODE (parmn) != PARM_DECL)
    {
      gcc_assert (TREE_CODE (parmn) == ADDR_EXPR);
      parmn = TREE_OPERAND (parmn, 0);
    }

  return call_builtin_fn (callexp, BUILT_IN_VA_START, 2, ap, parmn);
}

/* Expand a front-end instrinsic call to INTRINSIC, which is either a call to
   adds(), addu(), subs(), subu(), negs(), muls(), or mulu().  These intrinsics
   expect to take two or three arguments, the signature to which can be either:

	int adds (int x, int y, ref bool overflow);
	long adds (long x, long y, ref bool overflow);
	int negs (int x, ref bool overflow);
	long negs (long x, ref bool overflow);

   This performs an operation on two signed or unsigned integers, checking for
   overflow.  The overflow is sticky, meaning that a sequence of operations
   can be done and overflow need only be checked at the end.  The original call
   expression is held in CALLEXP.  */

static tree
expand_intrinsic_checkedint (intrinsic_code intrinsic, tree callexp)
{
  tree type = TREE_TYPE (callexp);
  tree x;
  tree y;
  tree overflow;
  internal_fn icode;

  /* Which variant of *_OVERFLOW should we generate?  */
  switch (intrinsic)
    {
    case INTRINSIC_ADDS:
    case INTRINSIC_ADDSL:
    case INTRINSIC_ADDU:
    case INTRINSIC_ADDUL:
      x = CALL_EXPR_ARG (callexp, 0);
      y = CALL_EXPR_ARG (callexp, 1);
      overflow = CALL_EXPR_ARG (callexp, 2);
      icode = IFN_ADD_OVERFLOW;
      break;

    case INTRINSIC_SUBS:
    case INTRINSIC_SUBSL:
    case INTRINSIC_SUBU:
    case INTRINSIC_SUBUL:
      x = CALL_EXPR_ARG (callexp, 0);
      y = CALL_EXPR_ARG (callexp, 1);
      overflow = CALL_EXPR_ARG (callexp, 2);
      icode = IFN_SUB_OVERFLOW;
      break;

    case INTRINSIC_MULS:
    case INTRINSIC_MULSL:
    case INTRINSIC_MULU:
    case INTRINSIC_MULUI:
    case INTRINSIC_MULUL:
      x = CALL_EXPR_ARG (callexp, 0);
      y = CALL_EXPR_ARG (callexp, 1);
      overflow = CALL_EXPR_ARG (callexp, 2);
      icode = IFN_MUL_OVERFLOW;
      break;

    case INTRINSIC_NEGS:
    case INTRINSIC_NEGSL:
      /* The negs() intrinsic gets turned into SUB_OVERFLOW (0, y).  */
      x = fold_convert (type, integer_zero_node);
      y = CALL_EXPR_ARG (callexp, 0);
      overflow = CALL_EXPR_ARG (callexp, 1);
      icode = IFN_SUB_OVERFLOW;
      break;

    default:
      gcc_unreachable ();
    }

  tree result
    = build_call_expr_internal_loc (EXPR_LOCATION (callexp), icode,
				    build_complex_type (type), 2, x, y);

  STRIP_NOPS (overflow);
  overflow = build_deref (overflow);

  /* Assign returned result to overflow parameter, however if overflow is
     already true, maintain its value.  */
  type = TREE_TYPE (overflow);
  result = save_expr (result);

  tree exp = fold_build2 (BIT_IOR_EXPR, type, overflow,
			  fold_convert (type, imaginary_part (result)));
  exp = modify_expr (overflow, exp);

  /* Return the value of result.  */
  return compound_expr (exp, real_part (result));
}

/* Expand a front-end instrinsic call to volatileLoad().  This takes one
   argument, the signature to which can be either:

	ubyte volatileLoad (ubyte* ptr);
	ushort volatileLoad (ushort* ptr);
	uint volatileLoad (uint* ptr);
	ulong volatileLoad (ulong* ptr);

   This reads a value from the memory location indicated by ptr.  Calls to
   them are be guaranteed to not be removed (such as during DCE) or reordered
   in the same thread.  The original call expression is held in CALLEXP.  */

static tree
expand_volatile_load (tree callexp)
{
  tree ptr = CALL_EXPR_ARG (callexp, 0);
  tree ptrtype = TREE_TYPE (ptr);
  gcc_assert (POINTER_TYPE_P (ptrtype));

  /* (T) *(volatile T *) ptr;  */
  tree type = build_qualified_type (TREE_TYPE (ptrtype), TYPE_QUAL_VOLATILE);
  tree result = indirect_ref (type, ptr);
  TREE_THIS_VOLATILE (result) = 1;

  return result;
}

/* Expand a front-end instrinsic call to volatileStore().  This takes two
   arguments, the signature to which can be either:

	void volatileStore (ubyte* ptr, ubyte value);
	void volatileStore (ushort* ptr, ushort value);
	void volatileStore (uint* ptr, uint value);
	void volatileStore (ulong* ptr, ulong value);

   This writes a value to the memory location indicated by ptr.  Calls to
   them are be guaranteed to not be removed (such as during DCE) or reordered
   in the same thread.  The original call expression is held in CALLEXP.  */

static tree
expand_volatile_store (tree callexp)
{
  tree ptr = CALL_EXPR_ARG (callexp, 0);
  tree ptrtype = TREE_TYPE (ptr);
  gcc_assert (POINTER_TYPE_P (ptrtype));

  /* (T) *(volatile T *) ptr;  */
  tree type = build_qualified_type (TREE_TYPE (ptrtype), TYPE_QUAL_VOLATILE);
  tree result = indirect_ref (type, ptr);
  TREE_THIS_VOLATILE (result) = 1;

  /* (*(volatile T *) ptr) = value;  */
  tree value = CALL_EXPR_ARG (callexp, 1);
  return modify_expr (result, value);
}

/* If CALLEXP is for an intrinsic , expand and return inlined compiler
   generated instructions.  Most map directly to GCC builtins, others
   require a little extra work around them.  */

tree
maybe_expand_intrinsic (tree callexp)
{
  tree callee = CALL_EXPR_FN (callexp);

  if (TREE_CODE (callee) == ADDR_EXPR)
    callee = TREE_OPERAND (callee, 0);

  if (TREE_CODE (callee) != FUNCTION_DECL)
    return callexp;

  /* Don't expand CTFE-only intrinsics outside of semantic processing.  */
  if (DECL_BUILT_IN_CTFE (callee) && !doing_semantic_analysis_p)
    return callexp;

  intrinsic_code intrinsic = DECL_INTRINSIC_CODE (callee);
  built_in_function code;

  switch (intrinsic)
    {
    case INTRINSIC_NONE:
      return callexp;

    case INTRINSIC_BSF:
    case INTRINSIC_BSF64:
      return expand_intrinsic_bsf (callexp);

    case INTRINSIC_BSR:
    case INTRINSIC_BSR64:
      return expand_intrinsic_bsr (callexp);

    case INTRINSIC_BT:
    case INTRINSIC_BT64:
    case INTRINSIC_BTC:
    case INTRINSIC_BTC64:
    case INTRINSIC_BTR:
    case INTRINSIC_BTR64:
    case INTRINSIC_BTS:
    case INTRINSIC_BTS64:
      return expand_intrinsic_bt (intrinsic, callexp);

    case INTRINSIC_POPCNT32:
    case INTRINSIC_POPCNT64:
      return expand_intrinsic_popcnt (callexp);

    case INTRINSIC_ROL:
    case INTRINSIC_ROL_TIARG:
    case INTRINSIC_ROR:
    case INTRINSIC_ROR_TIARG:
      return expand_intrinsic_rotate (intrinsic, callexp);

    case INTRINSIC_BSWAP16:
    case INTRINSIC_BSWAP32:
    case INTRINSIC_BSWAP64:
    case INTRINSIC_CEIL:
    case INTRINSIC_CEILF:
    case INTRINSIC_CEILL:
    case INTRINSIC_COS:
    case INTRINSIC_COSF:
    case INTRINSIC_COSL:
    case INTRINSIC_EXP:
    case INTRINSIC_EXP2:
    case INTRINSIC_EXPM1:
    case INTRINSIC_FABS:
    case INTRINSIC_FABSF:
    case INTRINSIC_FABSL:
    case INTRINSIC_FLOOR:
    case INTRINSIC_FLOORF:
    case INTRINSIC_FLOORL:
    case INTRINSIC_ISFINITE:
    case INTRINSIC_ISINFINITY:
    case INTRINSIC_ISNAN:
    case INTRINSIC_LOG:
    case INTRINSIC_LOG10:
    case INTRINSIC_LOG2:
    case INTRINSIC_RINT:
    case INTRINSIC_RINTF:
    case INTRINSIC_RINTL:
    case INTRINSIC_RNDTOL:
    case INTRINSIC_RNDTOLF:
    case INTRINSIC_RNDTOLL:
    case INTRINSIC_ROUND:
    case INTRINSIC_SIN:
    case INTRINSIC_SINF:
    case INTRINSIC_SINL:
    case INTRINSIC_SQRT:
    case INTRINSIC_SQRTF:
    case INTRINSIC_SQRTL:
    case INTRINSIC_TAN:
    case INTRINSIC_TRUNC:
      code = intrinsic_decls[intrinsic].built_in;
      gcc_assert (code != BUILT_IN_NONE);
      return call_builtin_fn (callexp, code, 1,
			      CALL_EXPR_ARG (callexp, 0));

    case INTRINSIC_FMAX:
    case INTRINSIC_FMIN:
    case INTRINSIC_LDEXP:
    case INTRINSIC_LDEXPF:
    case INTRINSIC_LDEXPL:
      code = intrinsic_decls[intrinsic].built_in;
      gcc_assert (code != BUILT_IN_NONE);
      return call_builtin_fn (callexp, code, 2,
			      CALL_EXPR_ARG (callexp, 0),
			      CALL_EXPR_ARG (callexp, 1));

    case INTRINSIC_FMA:
      code = intrinsic_decls[intrinsic].built_in;
      gcc_assert (code != BUILT_IN_NONE);
      return call_builtin_fn (callexp, code, 3,
			      CALL_EXPR_ARG (callexp, 0),
			      CALL_EXPR_ARG (callexp, 1),
			      CALL_EXPR_ARG (callexp, 2));

    case INTRINSIC_COPYSIGN:
    case INTRINSIC_COPYSIGNI:
      return expand_intrinsic_copysign (callexp);

    case INTRINSIC_POW:
      return expand_intrinsic_pow (callexp);

    case INTRINSIC_TOPREC:
    case INTRINSIC_TOPRECF:
    case INTRINSIC_TOPRECL:
      return expand_intrinsic_toprec (callexp);

    case INTRINSIC_VA_ARG:
    case INTRINSIC_C_VA_ARG:
      return expand_intrinsic_vaarg (callexp);

    case INTRINSIC_VASTART:
      return expand_intrinsic_vastart (callexp);

    case INTRINSIC_ADDS:
    case INTRINSIC_ADDSL:
    case INTRINSIC_ADDU:
    case INTRINSIC_ADDUL:
    case INTRINSIC_SUBS:
    case INTRINSIC_SUBSL:
    case INTRINSIC_SUBU:
    case INTRINSIC_SUBUL:
    case INTRINSIC_MULS:
    case INTRINSIC_MULSL:
    case INTRINSIC_MULU:
    case INTRINSIC_MULUI:
    case INTRINSIC_MULUL:
    case INTRINSIC_NEGS:
    case INTRINSIC_NEGSL:
      return expand_intrinsic_checkedint (intrinsic, callexp);

    case INTRINSIC_VLOAD8:
    case INTRINSIC_VLOAD16:
    case INTRINSIC_VLOAD32:
    case INTRINSIC_VLOAD64:
      return expand_volatile_load (callexp);

    case INTRINSIC_VSTORE8:
    case INTRINSIC_VSTORE16:
    case INTRINSIC_VSTORE32:
    case INTRINSIC_VSTORE64:
      return expand_volatile_store (callexp);

    default:
      gcc_unreachable ();
    }
}
