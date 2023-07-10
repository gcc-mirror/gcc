/* intrinsics.cc -- D language compiler intrinsics.
   Copyright (C) 2006-2023 Free Software Foundation, Inc.

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
#include "dmd/expression.h"
#include "dmd/identifier.h"
#include "dmd/mangle.h"
#include "dmd/module.h"
#include "dmd/template.h"

#include "tm.h"
#include "function.h"
#include "tree.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "fold-const.h"
#include "stringpool.h"
#include "builtins.h"
#include "vec-perm-indices.h"

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
  if (!decl->ident || decl->builtin != BUILTIN::unknown)
    return;

  /* The builtin flag is updated only if we can evaluate the intrinsic
     at compile-time.  Such as the math or bitop intrinsics.  */
  decl->builtin = BUILTIN::unimp;

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
	    case INTRINSIC_LOADUNALIGNED:
	    case INTRINSIC_STOREUNALIGNED:
	    case INTRINSIC_SHUFFLE:
	    case INTRINSIC_SHUFFLEVECTOR:
	    case INTRINSIC_CONVERTVECTOR:
	    case INTRINSIC_BLENDVECTOR:
	    case INTRINSIC_VLOAD8:
	    case INTRINSIC_VLOAD16:
	    case INTRINSIC_VLOAD32:
	    case INTRINSIC_VLOAD64:
	    case INTRINSIC_VSTORE8:
	    case INTRINSIC_VSTORE16:
	    case INTRINSIC_VSTORE32:
	    case INTRINSIC_VSTORE64:
	      /* Cannot interpret function during CTFE.  If the library
		 provides a definition, its body will be used instead.  */
	      break;

	    case INTRINSIC_POW:
	    {
	      /* Check that this overload of pow() is has an equivalent
		 built-in function.  It could be `int pow(int, int)'.  */
	      tree rettype = TREE_TYPE (TREE_TYPE (decl->csym));
	      if (mathfn_built_in (rettype, BUILT_IN_POW) != NULL_TREE)
		decl->builtin = BUILTIN::gcc;
	      break;
	    }

	    default:
	      decl->builtin = BUILTIN::gcc;
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

/* Helper function for maybe_warn_intrinsic_mismatch.  Issue warning about
   mismatch in the EXPECTED return type in call to the intrinsic function in
   CALLEXP, and return TRUE.  */

static bool
warn_mismatched_return_type (tree callexp, const char *expected)
{
  warning_at (EXPR_LOCATION (callexp), OPT_Wbuiltin_declaration_mismatch,
	      "mismatch in return type of intrinsic function %qD "
	      "(%qT, should be %qs)", get_callee_fndecl (callexp),
	      TREE_TYPE (callexp), expected);
  return true;
}

/* Helper function for maybe_warn_intrinsic_mismatch.  Issue warning or error
   about mismatch in the EXPECTED argument type at ARGNO in call to the
   intrinsic function in CALLEXP, and return TRUE.  */

static bool
warn_mismatched_argument (tree callexp, unsigned argno, const char *expected)
{
  warning_at (EXPR_LOCATION (callexp), OPT_Wbuiltin_declaration_mismatch,
	      "mismatch in argument %u type of intrinsic function %qD "
	      "(%qT, should be %qs)", argno + 1, get_callee_fndecl (callexp),
	      TREE_TYPE (CALL_EXPR_ARG (callexp, argno)), expected);
  return true;
}

static bool
warn_mismatched_argument (tree callexp, unsigned argno, tree expected,
			  bool error_p = false)
{
  if (error_p)
    error_at (EXPR_LOCATION (callexp),
	      "mismatch in argument %u type of intrinsic function %qD "
	      "(%qT, should be %qT)", argno + 1, get_callee_fndecl (callexp),
	      TREE_TYPE (CALL_EXPR_ARG (callexp, argno)), expected);
  else
    warning_at (EXPR_LOCATION (callexp), OPT_Wbuiltin_declaration_mismatch,
		"mismatch in argument %u type of intrinsic function %qD "
		"(%qT, should be %qT)", argno + 1, get_callee_fndecl (callexp),
		TREE_TYPE (CALL_EXPR_ARG (callexp, argno)), expected);

  return true;
}

/* Helper function for maybe_warn_intrinsic_mismatch.  Builds a vector integer
   type suitable for the mask argument of INTRINSIC_SHUFFLE from the given
   input argument TYPE.  */

static tree
build_shuffle_mask_type (tree type)
{
  const unsigned bits = GET_MODE_BITSIZE (SCALAR_TYPE_MODE (TREE_TYPE (type)));
  const int unsignedp = TYPE_UNSIGNED (TREE_TYPE (type));
  tree inner = lang_hooks.types.type_for_size (bits, unsignedp);
  gcc_assert (inner && TREE_CODE (inner) == INTEGER_TYPE);

  /* %% Get the front-end type for the vector so the D type will be
     printed (this should really be handled by a D tree printer).  */
  Type *t = build_frontend_type (inner);
  gcc_assert (t != NULL);
  unsigned HOST_WIDE_INT nunits = TYPE_VECTOR_SUBPARTS (type).to_constant ();

  return build_ctype (TypeVector::create (t->sarrayOf (nunits)));
}

/* Checks if call to intrinsic FUNCTION in CALLEXP matches the internal
   type and value constraints that we expect from the library definitions.
   Returns TRUE and issues a warning if there is a mismatch.

   Note: The return type and parameters are encoded into the signature `deco'
   string that we match on in maybe_set_intrinsic(), so if the deco mangle
   string has 'i' in the part that specifies the return type, then the matched
   intrinsic will always have the return type `int'.

   For templated intrinsics however, we rely on template constraints to ensure
   that the generic type matches what we expect it to be.  There is still an
   enforced relationship between a template argument and its instantiated type.
   For example: `T func(T)(T*)' would have the generic return type `@1T' and
   generic parameter type `@1PT', so it can be assumed that if the return type
   matches what we expect then all parameters are fine as well.  Otherwise it
   can be assumed that some internal_error has occurred for this to be the case.
   Where a templated intrinsic has multiple template arguments, each generic
   type will need to be checked for its validity.  */

static bool
maybe_warn_intrinsic_mismatch (tree function, tree callexp)
{
  switch (DECL_INTRINSIC_CODE (function))
    {
    case INTRINSIC_NONE:
    default:
      return false;

    case INTRINSIC_LOADUNALIGNED:
      {
	/* Expects the signature:
	   vector(T) loadUnaligned (vector(T)*);  */
	gcc_assert (call_expr_nargs (callexp) == 1);

	tree ptr = TREE_TYPE (CALL_EXPR_ARG (callexp, 0));
	if (!VECTOR_TYPE_P (TREE_TYPE (callexp))
	    || !POINTER_TYPE_P (ptr) || !VECTOR_TYPE_P (TREE_TYPE (ptr)))
	  return warn_mismatched_return_type (callexp, "__vector(T)");

	return false;
      }

    case INTRINSIC_STOREUNALIGNED:
      {
	/* Expects the signature:
	   vector(T) storeUnaligned (vector(T)*, vector(T));  */
	gcc_assert (call_expr_nargs (callexp) == 2);

	tree ptr = TREE_TYPE (CALL_EXPR_ARG (callexp, 0));
	tree val = TREE_TYPE (CALL_EXPR_ARG (callexp, 1));
	if (!VECTOR_TYPE_P (TREE_TYPE (callexp))
	    || !POINTER_TYPE_P (ptr) || !VECTOR_TYPE_P (TREE_TYPE (ptr))
	    || !VECTOR_TYPE_P (val))
	  return warn_mismatched_return_type (callexp, "__vector(T)");

	return false;
      }

    case INTRINSIC_SHUFFLE:
    case INTRINSIC_BLENDVECTOR:
      {
	/* Expects the signature:
	   vector(T) shuffle (vector(T), vector(U), vector(V));
	   vector(T) blendvector (vector(T), vector(U), vector(V));  */
	gcc_assert (call_expr_nargs (callexp) == 3);

	tree vec0 = TREE_TYPE (CALL_EXPR_ARG (callexp, 0));
	if (!VECTOR_TYPE_P (TREE_TYPE (callexp))
	    || !VECTOR_TYPE_P (vec0))
	  return warn_mismatched_return_type (callexp, "__vector(T)");

	tree vec1 = TREE_TYPE (CALL_EXPR_ARG (callexp, 1));
	if (!VECTOR_TYPE_P (vec1))
	  return warn_mismatched_argument (callexp, 1, vec0);

	tree mask = TREE_TYPE (CALL_EXPR_ARG (callexp, 2));
	if (!VECTOR_TYPE_P (mask) || !VECTOR_INTEGER_TYPE_P (mask))
	  {
	    tree expected = build_shuffle_mask_type (vec0);
	    return warn_mismatched_argument (callexp, 2, expected,
					     VECTOR_TYPE_P (mask));
	  }

	/* Types have been validated, now issue errors about violations on the
	   constraints of the intrinsic.  */
	if (TYPE_MAIN_VARIANT (vec0) != TYPE_MAIN_VARIANT (vec1))
	  return warn_mismatched_argument (callexp, 1, vec0, true);

	/* Vector element sizes should be equal between arguments and mask.  */
	if (GET_MODE_BITSIZE (SCALAR_TYPE_MODE (TREE_TYPE (vec0)))
	    != GET_MODE_BITSIZE (SCALAR_TYPE_MODE (TREE_TYPE (mask)))
	    || maybe_ne (TYPE_VECTOR_SUBPARTS (vec0),
			 TYPE_VECTOR_SUBPARTS (mask))
	    || maybe_ne (TYPE_VECTOR_SUBPARTS (vec1),
			 TYPE_VECTOR_SUBPARTS (mask)))
	  {
	    tree expected = build_shuffle_mask_type (vec0);
	    return warn_mismatched_argument (callexp, 2, expected, true);
	  }

	return false;
      }

    case INTRINSIC_SHUFFLEVECTOR:
      {
	/* Expects the signature:
	   vector(T[N]) shufflevector (vector(T), vector(U), N...);  */
	gcc_assert (call_expr_nargs (callexp) >= 3);
	gcc_assert (VECTOR_TYPE_P (TREE_TYPE (callexp)));

	tree vec0 = TREE_TYPE (CALL_EXPR_ARG (callexp, 0));
	if (!VECTOR_TYPE_P (vec0))
	  return warn_mismatched_argument (callexp, 0, "__vector(T)");

	tree vec1 = TREE_TYPE (CALL_EXPR_ARG (callexp, 1));
	if (!VECTOR_TYPE_P (vec1))
	  return warn_mismatched_argument (callexp, 1, vec0);

	for (int i = 2; i < call_expr_nargs (callexp); i++)
	  {
	    tree idx = TREE_TYPE (CALL_EXPR_ARG (callexp, i));
	    if (TREE_CODE (idx) != INTEGER_TYPE)
	      return warn_mismatched_argument (callexp, i, d_int_type);
	  }

	/* Types have been validated, now issue errors about violations on the
	   constraints of the intrinsic.  */
	if (TYPE_MAIN_VARIANT (TREE_TYPE (vec0))
	    != TYPE_MAIN_VARIANT (TREE_TYPE (vec1)))
	  {
	    /* %% Get the front-end type for the vector so the D type will be
	       printed (this should really be handled by a D tree printer).  */
	    unsigned HOST_WIDE_INT nunits;
	    if (!TYPE_VECTOR_SUBPARTS (vec1).is_constant (&nunits))
	      break;

	    Type *inner = build_frontend_type (TREE_TYPE (vec0));
	    Type *vector = TypeVector::create (inner->sarrayOf (nunits));
	    return warn_mismatched_argument (callexp, 1,
					     build_ctype (vector), true);
	  }

	/* Vector sizes should be known, and number of indices a power of 2.  */
	unsigned HOST_WIDE_INT vec0_length;
	unsigned HOST_WIDE_INT vec1_length;
	if (!TYPE_VECTOR_SUBPARTS (vec0).is_constant (&vec0_length)
	    || !TYPE_VECTOR_SUBPARTS (vec1).is_constant (&vec1_length)
	    || !pow2p_hwi (call_expr_nargs (callexp) - 2))
	  break;

	/* All index arguments must be valid constants as well.  */
	for (int i = 2; i < call_expr_nargs (callexp); i++)
	  {
	    tree idx = CALL_EXPR_ARG (callexp, i);
	    if (!tree_fits_shwi_p (idx))
	      {
		error_at (EXPR_LOCATION (callexp),
			  "argument %qE cannot be read at compile time", idx);
		return true;
	      }

	    HOST_WIDE_INT iidx = tree_to_shwi (idx);
	    if (iidx < 0
		|| (unsigned HOST_WIDE_INT) iidx >= vec0_length + vec1_length)
	      {
		error_at (EXPR_LOCATION (callexp),
			  "element index %qE is out of bounds %<[0 .. %E]%>",
			  idx, build_integer_cst (vec0_length + vec1_length));
		return true;
	      }
	  }

	return false;
      }

    case INTRINSIC_CONVERTVECTOR:
      {
	/* Expects the signature:
	   vector(T) convertvector (vector(U));  */
	gcc_assert (call_expr_nargs (callexp) == 1);

	tree ret = TREE_TYPE (callexp);
	if (!VECTOR_TYPE_P (ret)
	    || (!VECTOR_INTEGER_TYPE_P (ret) && !VECTOR_FLOAT_TYPE_P (ret)))
	  return warn_mismatched_return_type (callexp, "__vector(T)");

	tree arg = TREE_TYPE (CALL_EXPR_ARG (callexp, 0));
	if (!VECTOR_TYPE_P (arg)
	    || (!VECTOR_INTEGER_TYPE_P (arg) && !VECTOR_FLOAT_TYPE_P (arg)))
	  return warn_mismatched_argument (callexp, 0, "__vector(T)");

	/* Types have been validated, now issue errors about violations on the
	   constraints of the intrinsic.  */
	if (maybe_ne (TYPE_VECTOR_SUBPARTS (ret), TYPE_VECTOR_SUBPARTS (arg)))
	  {
	    /* %% Get the front-end type for the vector so the D type will be
	       printed (this should really be handled by a D tree printer).  */
	    unsigned HOST_WIDE_INT nunits;
	    if (!TYPE_VECTOR_SUBPARTS (ret).is_constant (&nunits))
	      break;

	    Type *inner = build_frontend_type (TREE_TYPE (arg));
	    Type *vector = TypeVector::create (inner->sarrayOf (nunits));
	    return warn_mismatched_argument (callexp, 0,
					     build_ctype (vector), true);
	  }

	return false;
      }
    }

  /* Generic mismatch warning if it hasn't already been handled.  */
  warning_at (EXPR_LOCATION (callexp), OPT_Wbuiltin_declaration_mismatch,
	      "mismatch in call of intrinsic function %qD",  function);
  return true;
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
  /* The bsf() intrinsic gets turned into __builtin_ctz(arg).
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
  /* The bsr() intrinsic gets turned into __builtin_clz(arg) ^ (size - 1).
     The return value is supposed to be undefined if arg is zero.  */
  tree arg = CALL_EXPR_ARG (callexp, 0);
  tree type = TREE_TYPE (callexp);
  int argsize = TYPE_PRECISION (TREE_TYPE (arg));

  /* Which variant of __builtin_clz* should we call?  */
  built_in_function code = (argsize <= INT_TYPE_SIZE) ? BUILT_IN_CLZ
    : (argsize <= LONG_TYPE_SIZE) ? BUILT_IN_CLZL
    : (argsize <= LONG_LONG_TYPE_SIZE) ? BUILT_IN_CLZLL
    : END_BUILTINS;

  gcc_assert (code != END_BUILTINS);

  tree result = call_builtin_fn (callexp, code, 1, arg);

  return fold_build2 (BIT_XOR_EXPR, type, result,
		      build_integer_cst (argsize - 1, type));
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
  ptr = build_pointer_index (ptr, fold_build2 (TRUNC_DIV_EXPR, type,
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
      /* Retrieve from the encoded template instantation.  */
      tree callee = get_callee_fndecl (callexp);
      TemplateInstance *ti = DECL_LANG_FRONTEND (callee)->isInstantiated ();
      gcc_assert (ti && ti->tiargs && ti->tiargs->length == 2);

      Expression *e = isExpression ((*ti->tiargs)[0]);
      gcc_assert (e && e->op == EXP::int64);
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
  TREE_SIDE_EFFECTS (result) = 1;

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
  TREE_SIDE_EFFECTS (result) = 1;

  /* (*(volatile T *) ptr) = value;  */
  tree value = CALL_EXPR_ARG (callexp, 1);
  return modify_expr (result, value);
}

/* Expand a front-end instrinsic call to convertvector().  This takes one
   argument, the signature to which is:

	vector(T) convertvector (vector(F) vec);

   This converts a vector VEC to TYPE by casting every element in VEC to the
   element type of TYPE.  The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_vec_convert (tree callexp)
{
  tree vec = CALL_EXPR_ARG (callexp, 0);
  tree type = TREE_TYPE (callexp);

  /* Use VIEW_CONVERT for simple vector conversions.  */
  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (vec)))
       == TYPE_MAIN_VARIANT (TREE_TYPE (type)))
      || (VECTOR_INTEGER_TYPE_P (TREE_TYPE (vec))
	  && VECTOR_INTEGER_TYPE_P (type)
	  && (TYPE_PRECISION (TREE_TYPE (TREE_TYPE (vec)))
	      == TYPE_PRECISION (TREE_TYPE (type)))))
    return build1_loc (EXPR_LOCATION (callexp), VIEW_CONVERT_EXPR, type, vec);

  return build_call_expr_internal_loc (EXPR_LOCATION (callexp), IFN_VEC_CONVERT,
				       type, 1, vec);
}

/* Expand a front-end instrinsic call to blendvector().  This expects to take
   three arguments, the signature to which is:

	vector(T) blendvector (vector(T) vec0, vector(U) vec1, vector(M) mask);

   This builds a VEC_COND_EXPR if VEC0, VEC1, and MASK are vector types, VEC0
   has the same type as VEC1, and the number of elements of VEC0, VEC1, and MASK
   are the same.  The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_vec_blend (tree callexp)
{
  tree vec0 = CALL_EXPR_ARG (callexp, 0);
  tree vec1 = CALL_EXPR_ARG (callexp, 1);
  tree mask = CALL_EXPR_ARG (callexp, 2);

  tree cmp = fold_build2_loc (EXPR_LOCATION (callexp), NE_EXPR,
			      truth_type_for (TREE_TYPE (mask)),
			      mask, build_zero_cst (TREE_TYPE (mask)));

  tree ret = fold_build3_loc (EXPR_LOCATION (callexp), VEC_COND_EXPR,
			      TREE_TYPE (callexp), cmp, vec0, vec1);

  if (!CONSTANT_CLASS_P (vec0) || !CONSTANT_CLASS_P (vec1))
    ret = force_target_expr (ret);

  return ret;
}

/* Expand a front-end instrinsic call to shuffle().  This expects to take three
   arguments, the signature to which is:

	vector(T) shuffle (vector(T) vec0, vector(T) vec1, vector(M) mask);

   This builds a VEC_PERM_EXPR if VEC0, VEC1, and MASK are vector types, VEC0
   has the same type as VEC1, and the number of elements of VEC0, VEC1, and MASK
   are the same.  The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_vec_shuffle (tree callexp)
{
  tree vec0 = CALL_EXPR_ARG (callexp, 0);
  tree vec1 = CALL_EXPR_ARG (callexp, 1);
  tree mask = CALL_EXPR_ARG (callexp, 2);

  return build3_loc (EXPR_LOCATION (callexp), VEC_PERM_EXPR,
		     TREE_TYPE (callexp), vec0, vec1, mask);
}

/* Expand a front-end instrinsic call to shufflevector().  This takes two
   positional arguments and a variadic list, the signature to which is:

	vector(TM) shuffle (vector(T) vec1, vector(T) vec2, index...);

   This builds a VEC_PERM_EXPR if VEC0 and VEC1 are vector types, VEC0 has the
   same element type as VEC1, and the number of elements in INDEX is a valid
   power of two.  The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_vec_shufflevector (tree callexp)
{
  tree vec0 = CALL_EXPR_ARG (callexp, 0);
  tree vec1 = CALL_EXPR_ARG (callexp, 1);

  unsigned HOST_WIDE_INT v0elems =
    TYPE_VECTOR_SUBPARTS (TREE_TYPE (vec0)).to_constant ();
  unsigned HOST_WIDE_INT v1elems =
    TYPE_VECTOR_SUBPARTS (TREE_TYPE (vec1)).to_constant ();

  unsigned HOST_WIDE_INT num_indices = call_expr_nargs (callexp) - 2;
  unsigned HOST_WIDE_INT masklen = MAX (num_indices, MAX (v0elems, v1elems));
  unsigned HOST_WIDE_INT pad_size = (v0elems < masklen ? masklen - v0elems : 0);
  vec_perm_builder sel (masklen, masklen, 1);

  unsigned n = 0;
  for (; n < num_indices; ++n)
    {
      tree idx = CALL_EXPR_ARG (callexp, n + 2);
      HOST_WIDE_INT iidx = tree_to_shwi (idx);
      /* VEC_PERM_EXPR does not allow different sized inputs.  */
      if ((unsigned HOST_WIDE_INT) iidx >= v0elems)
	iidx += pad_size;

      sel.quick_push (iidx);
    }

  /* VEC_PERM_EXPR does not support a result that is smaller than the inputs.  */
  for (; n < masklen; ++n)
    sel.quick_push (n);

  vec_perm_indices indices (sel, 2, masklen);

  /* Pad out arguments to the common vector size.  */
  tree ret_type = build_vector_type (TREE_TYPE (TREE_TYPE (vec0)), masklen);
  if (v0elems < masklen)
    {
      constructor_elt elt = { NULL_TREE, build_zero_cst (TREE_TYPE (vec0)) };
      vec0 = build_constructor_single (ret_type, NULL_TREE, vec0);
      for (unsigned i = 1; i < masklen / v0elems; ++i)
        vec_safe_push (CONSTRUCTOR_ELTS (vec0), elt);
    }

  if (v1elems < masklen)
    {
      constructor_elt elt = { NULL_TREE, build_zero_cst (TREE_TYPE (vec1)) };
      vec1 = build_constructor_single (ret_type, NULL_TREE, vec1);
      for (unsigned i = 1; i < masklen / v1elems; ++i)
        vec_safe_push (CONSTRUCTOR_ELTS (vec1), elt);
    }

  tree mask_type = build_vector_type (build_nonstandard_integer_type
                (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (ret_type))), 1),
                masklen);
  tree ret = build3_loc (EXPR_LOCATION (callexp), VEC_PERM_EXPR, ret_type, vec0,
			 vec1, vec_perm_indices_to_tree (mask_type, indices));

  /* Get the low part we are interested in.  */
  if (num_indices < masklen)
    {
      ret = build3_loc (EXPR_LOCATION (callexp), BIT_FIELD_REF,
			TREE_TYPE (callexp), ret,
			TYPE_SIZE (TREE_TYPE (callexp)), bitsize_zero_node);
      /* Wrap the low part operation in a TARGET_EXPR so it gets a separate
         temporary during gimplification.  */
      ret = force_target_expr (ret);
    }

  return ret;
}

/* Expand a front-end instrinsic call to loadUnaligned().  This takes one
   argument, the signature to which is:

	vector(T) loadUnaligned (vector(T)* ptr)

   This generates a load of a vector from an unaligned address PTR.
   The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_vec_load_unaligned (tree callexp)
{
  tree ptr = CALL_EXPR_ARG (callexp, 0);

  tree unaligned_type = build_variant_type_copy (TREE_TYPE (TREE_TYPE (ptr)));
  SET_TYPE_ALIGN (unaligned_type, 1 * BITS_PER_UNIT);
  TYPE_USER_ALIGN (unaligned_type) = 1;

  tree load = indirect_ref (unaligned_type, ptr);
  return convert (TREE_TYPE (callexp), load);
}

/* Expand a front-end instrinsic call to storeUnaligned().  This takes two
   arguments, the signature to which is:

	vector(T) storeUnaligned (vector(T)* ptr, vector(T) value)

   This generates an assignment of a vector VALUE to an unaligned address PTR.
   The original call expression is held in CALLEXP.  */

static tree
expand_intrinsic_vec_store_unaligned (tree callexp)
{
  tree ptr = CALL_EXPR_ARG (callexp, 0);
  tree vec = CALL_EXPR_ARG (callexp, 1);

  tree unaligned_type = build_variant_type_copy (TREE_TYPE (TREE_TYPE (ptr)));
  SET_TYPE_ALIGN (unaligned_type, 1 * BITS_PER_UNIT);
  TYPE_USER_ALIGN (unaligned_type) = 1;

  tree load = indirect_ref (unaligned_type, ptr);
  return build_assign (MODIFY_EXPR, load, vec);
}

/* If CALLEXP is for an intrinsic , expand and return inlined compiler
   generated instructions.  Most map directly to GCC builtins, others
   require a little extra work around them.  */

tree
maybe_expand_intrinsic (tree callexp)
{
  tree callee = get_callee_fndecl (callexp);

  if (callee == NULL_TREE || TREE_CODE (callee) != FUNCTION_DECL)
    return callexp;

  /* Don't expand CTFE-only intrinsics outside of semantic processing.  */
  if (DECL_BUILT_IN_CTFE (callee) && !doing_semantic_analysis_p)
    return callexp;

  /* Gate the expansion of the intrinsic with constraint checks, if any fail
     then bail out without any lowering.  */
  if (maybe_warn_intrinsic_mismatch (callee, callexp))
    {
      /* Reset the built-in flag so that we don't trip fold_builtin.  */
      set_decl_built_in_function (callee, NOT_BUILT_IN, 0);
      return callexp;
    }

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

    case INTRINSIC_LOADUNALIGNED:
      return expand_intrinsic_vec_load_unaligned (callexp);

    case INTRINSIC_STOREUNALIGNED:
      return expand_intrinsic_vec_store_unaligned (callexp);

    case INTRINSIC_SHUFFLE:
      return expand_intrinsic_vec_shuffle (callexp);

    case INTRINSIC_SHUFFLEVECTOR:
      return expand_intrinsic_vec_shufflevector (callexp);

    case INTRINSIC_CONVERTVECTOR:
      return expand_intrinsic_vec_convert (callexp);

    case INTRINSIC_BLENDVECTOR:
      return expand_intrinsic_vec_blend (callexp);

    default:
      gcc_unreachable ();
    }
}
