/* Intrinsic translation
   Copyright (C) 2002-2025 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

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

/* trans-intrinsic.cc-- generate GENERIC trees for calls to intrinsics.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "memmodel.h"
#include "tm.h"		/* For UNITS_PER_WORD.  */
#include "tree.h"
#include "gfortran.h"
#include "trans.h"
#include "stringpool.h"
#include "fold-const.h"
#include "internal-fn.h"
#include "tree-nested.h"
#include "stor-layout.h"
#include "toplev.h"	/* For rest_of_decl_compilation.  */
#include "arith.h"
#include "trans-const.h"
#include "trans-types.h"
#include "trans-array.h"
#include "dependency.h"	/* For CAF array alias analysis.  */
#include "attribs.h"
#include "realmpfr.h"
#include "constructor.h"

/* Only for gfc_trans_assign and gfc_trans_pointer_assign.  */

/* This maps Fortran intrinsic math functions to external library or GCC
   builtin functions.  */
typedef struct GTY(()) gfc_intrinsic_map_t {
  /* The explicit enum is required to work around inadequacies in the
     garbage collection/gengtype parsing mechanism.  */
  enum gfc_isym_id id;

  /* Enum value from the "language-independent", aka C-centric, part
     of gcc, or END_BUILTINS of no such value set.  */
  enum built_in_function float_built_in;
  enum built_in_function double_built_in;
  enum built_in_function long_double_built_in;
  enum built_in_function complex_float_built_in;
  enum built_in_function complex_double_built_in;
  enum built_in_function complex_long_double_built_in;

  /* True if the naming pattern is to prepend "c" for complex and
     append "f" for kind=4.  False if the naming pattern is to
     prepend "_gfortran_" and append "[rc](4|8|10|16)".  */
  bool libm_name;

  /* True if a complex version of the function exists.  */
  bool complex_available;

  /* True if the function should be marked const.  */
  bool is_constant;

  /* The base library name of this function.  */
  const char *name;

  /* Cache decls created for the various operand types.  */
  tree real4_decl;
  tree real8_decl;
  tree real10_decl;
  tree real16_decl;
  tree complex4_decl;
  tree complex8_decl;
  tree complex10_decl;
  tree complex16_decl;
}
gfc_intrinsic_map_t;

/* ??? The NARGS==1 hack here is based on the fact that (c99 at least)
   defines complex variants of all of the entries in mathbuiltins.def
   except for atan2.  */
#define DEFINE_MATH_BUILTIN(ID, NAME, ARGTYPE) \
  { GFC_ISYM_ ## ID, BUILT_IN_ ## ID ## F, BUILT_IN_ ## ID, \
    BUILT_IN_ ## ID ## L, END_BUILTINS, END_BUILTINS, END_BUILTINS, \
    true, false, true, NAME, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, \
    NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE},

#define DEFINE_MATH_BUILTIN_C(ID, NAME, ARGTYPE) \
  { GFC_ISYM_ ## ID, BUILT_IN_ ## ID ## F, BUILT_IN_ ## ID, \
    BUILT_IN_ ## ID ## L, BUILT_IN_C ## ID ## F, BUILT_IN_C ## ID, \
    BUILT_IN_C ## ID ## L, true, true, true, NAME, NULL_TREE, NULL_TREE, \
    NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE},

#define LIB_FUNCTION(ID, NAME, HAVE_COMPLEX) \
  { GFC_ISYM_ ## ID, END_BUILTINS, END_BUILTINS, END_BUILTINS, \
    END_BUILTINS, END_BUILTINS, END_BUILTINS, \
    false, HAVE_COMPLEX, true, NAME, NULL_TREE, NULL_TREE, NULL_TREE, \
    NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE }

#define OTHER_BUILTIN(ID, NAME, TYPE, CONST) \
  { GFC_ISYM_NONE, BUILT_IN_ ## ID ## F, BUILT_IN_ ## ID, \
    BUILT_IN_ ## ID ## L, END_BUILTINS, END_BUILTINS, END_BUILTINS, \
    true, false, CONST, NAME, NULL_TREE, NULL_TREE, \
    NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE},

static GTY(()) gfc_intrinsic_map_t gfc_intrinsic_map[] =
{
  /* Functions built into gcc itself (DEFINE_MATH_BUILTIN and
     DEFINE_MATH_BUILTIN_C), then the built-ins that don't correspond
     to any GFC_ISYM id directly, which use the OTHER_BUILTIN macro.  */
#include "mathbuiltins.def"

  /* Functions in libgfortran.  */
  LIB_FUNCTION (ERFC_SCALED, "erfc_scaled", false),
  LIB_FUNCTION (SIND, "sind", false),
  LIB_FUNCTION (COSD, "cosd", false),
  LIB_FUNCTION (TAND, "tand", false),

  /* End the list.  */
  LIB_FUNCTION (NONE, NULL, false)

};
#undef OTHER_BUILTIN
#undef LIB_FUNCTION
#undef DEFINE_MATH_BUILTIN
#undef DEFINE_MATH_BUILTIN_C


enum rounding_mode { RND_ROUND, RND_TRUNC, RND_CEIL, RND_FLOOR };


/* Find the correct variant of a given builtin from its argument.  */
static tree
builtin_decl_for_precision (enum built_in_function base_built_in,
			    int precision)
{
  enum built_in_function i = END_BUILTINS;

  gfc_intrinsic_map_t *m;
  for (m = gfc_intrinsic_map; m->double_built_in != base_built_in ; m++)
    ;

  if (precision == TYPE_PRECISION (float_type_node))
    i = m->float_built_in;
  else if (precision == TYPE_PRECISION (double_type_node))
    i = m->double_built_in;
  else if (precision == TYPE_PRECISION (long_double_type_node)
	   && (!gfc_real16_is_float128
	       || long_double_type_node != gfc_float128_type_node))
    i = m->long_double_built_in;
  else if (precision == TYPE_PRECISION (gfc_float128_type_node))
    {
      /* Special treatment, because it is not exactly a built-in, but
	 a library function.  */
      return m->real16_decl;
    }

  return (i == END_BUILTINS ? NULL_TREE : builtin_decl_explicit (i));
}


tree
gfc_builtin_decl_for_float_kind (enum built_in_function double_built_in,
				 int kind)
{
  int i = gfc_validate_kind (BT_REAL, kind, false);

  if (gfc_real_kinds[i].c_float128)
    {
      /* For _Float128, the story is a bit different, because we return
	 a decl to a library function rather than a built-in.  */
      gfc_intrinsic_map_t *m;
      for (m = gfc_intrinsic_map; m->double_built_in != double_built_in ; m++)
	;

      return m->real16_decl;
    }

  return builtin_decl_for_precision (double_built_in,
				     gfc_real_kinds[i].mode_precision);
}


/* Evaluate the arguments to an intrinsic function.  The value
   of NARGS may be less than the actual number of arguments in EXPR
   to allow optional "KIND" arguments that are not included in the
   generated code to be ignored.  */

static void
gfc_conv_intrinsic_function_args (gfc_se *se, gfc_expr *expr,
				  tree *argarray, int nargs)
{
  gfc_actual_arglist *actual;
  gfc_expr *e;
  gfc_intrinsic_arg  *formal;
  gfc_se argse;
  int curr_arg;

  formal = expr->value.function.isym->formal;
  actual = expr->value.function.actual;

   for (curr_arg = 0; curr_arg < nargs; curr_arg++,
	actual = actual->next,
	formal = formal ? formal->next : NULL)
    {
      gcc_assert (actual);
      e = actual->expr;
      /* Skip omitted optional arguments.  */
      if (!e)
	{
	  --curr_arg;
	  continue;
	}

      /* Evaluate the parameter.  This will substitute scalarized
         references automatically.  */
      gfc_init_se (&argse, se);

      if (e->ts.type == BT_CHARACTER)
	{
	  gfc_conv_expr (&argse, e);
	  gfc_conv_string_parameter (&argse);
          argarray[curr_arg++] = argse.string_length;
	  gcc_assert (curr_arg < nargs);
	}
      else
        gfc_conv_expr_val (&argse, e);

      /* If an optional argument is itself an optional dummy argument,
	 check its presence and substitute a null if absent.  */
      if (e->expr_type == EXPR_VARIABLE
	    && e->symtree->n.sym->attr.optional
	    && formal
	    && formal->optional)
	gfc_conv_missing_dummy (&argse, e, formal->ts, 0);

      gfc_add_block_to_block (&se->pre, &argse.pre);
      gfc_add_block_to_block (&se->post, &argse.post);
      argarray[curr_arg] = argse.expr;
    }
}

/* Count the number of actual arguments to the intrinsic function EXPR
   including any "hidden" string length arguments.  */

static unsigned int
gfc_intrinsic_argument_list_length (gfc_expr *expr)
{
  int n = 0;
  gfc_actual_arglist *actual;

  for (actual = expr->value.function.actual; actual; actual = actual->next)
    {
      if (!actual->expr)
	continue;

      if (actual->expr->ts.type == BT_CHARACTER)
	n += 2;
      else
	n++;
    }

  return n;
}


/* Conversions between different types are output by the frontend as
   intrinsic functions.  We implement these directly with inline code.  */

static void
gfc_conv_intrinsic_conversion (gfc_se * se, gfc_expr * expr)
{
  tree type;
  tree *args;
  int nargs;

  nargs = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, nargs);

  /* Evaluate all the arguments passed. Whilst we're only interested in the
     first one here, there are other parts of the front-end that assume this
     and will trigger an ICE if it's not the case.  */
  type = gfc_typenode_for_spec (&expr->ts);
  gcc_assert (expr->value.function.actual->expr);
  gfc_conv_intrinsic_function_args (se, expr, args, nargs);

  /* Conversion between character kinds involves a call to a library
     function.  */
  if (expr->ts.type == BT_CHARACTER)
    {
      tree fndecl, var, addr, tmp;

      if (expr->ts.kind == 1
	  && expr->value.function.actual->expr->ts.kind == 4)
	fndecl = gfor_fndecl_convert_char4_to_char1;
      else if (expr->ts.kind == 4
	       && expr->value.function.actual->expr->ts.kind == 1)
	fndecl = gfor_fndecl_convert_char1_to_char4;
      else
	gcc_unreachable ();

      /* Create the variable storing the converted value.  */
      type = gfc_get_pchar_type (expr->ts.kind);
      var = gfc_create_var (type, "str");
      addr = gfc_build_addr_expr (build_pointer_type (type), var);

      /* Call the library function that will perform the conversion.  */
      gcc_assert (nargs >= 2);
      tmp = build_call_expr_loc (input_location,
			     fndecl, 3, addr, args[0], args[1]);
      gfc_add_expr_to_block (&se->pre, tmp);

      /* Free the temporary afterwards.  */
      tmp = gfc_call_free (var);
      gfc_add_expr_to_block (&se->post, tmp);

      se->expr = var;
      se->string_length = args[0];

      return;
    }

  /* Conversion from complex to non-complex involves taking the real
     component of the value.  */
  if (TREE_CODE (TREE_TYPE (args[0])) == COMPLEX_TYPE
      && expr->ts.type != BT_COMPLEX)
    {
      tree artype;

      artype = TREE_TYPE (TREE_TYPE (args[0]));
      args[0] = fold_build1_loc (input_location, REALPART_EXPR, artype,
				 args[0]);
    }

  se->expr = convert (type, args[0]);
}

/* This is needed because the gcc backend only implements
   FIX_TRUNC_EXPR, which is the same as INT() in Fortran.
   FLOOR(x) = INT(x) <= x ? INT(x) : INT(x) - 1
   Similarly for CEILING.  */

static tree
build_fixbound_expr (stmtblock_t * pblock, tree arg, tree type, int up)
{
  tree tmp;
  tree cond;
  tree argtype;
  tree intval;

  argtype = TREE_TYPE (arg);
  arg = gfc_evaluate_now (arg, pblock);

  intval = convert (type, arg);
  intval = gfc_evaluate_now (intval, pblock);

  tmp = convert (argtype, intval);
  cond = fold_build2_loc (input_location, up ? GE_EXPR : LE_EXPR,
			  logical_type_node, tmp, arg);

  tmp = fold_build2_loc (input_location, up ? PLUS_EXPR : MINUS_EXPR, type,
			 intval, build_int_cst (type, 1));
  tmp = fold_build3_loc (input_location, COND_EXPR, type, cond, intval, tmp);
  return tmp;
}


/* Round to nearest integer, away from zero.  */

static tree
build_round_expr (tree arg, tree restype)
{
  tree argtype;
  tree fn;
  int argprec, resprec;

  argtype = TREE_TYPE (arg);
  argprec = TYPE_PRECISION (argtype);
  resprec = TYPE_PRECISION (restype);

  /* Depending on the type of the result, choose the int intrinsic (iround,
     available only as a builtin, therefore cannot use it for _Float128), long
     int intrinsic (lround family) or long long intrinsic (llround).  If we
     don't have an appropriate function that converts directly to the integer
     type (such as kind == 16), just use ROUND, and then convert the result to
     an integer.  We might also need to convert the result afterwards.  */
  if (resprec <= INT_TYPE_SIZE
      && argprec <= TYPE_PRECISION (long_double_type_node))
    fn = builtin_decl_for_precision (BUILT_IN_IROUND, argprec);
  else if (resprec <= LONG_TYPE_SIZE)
    fn = builtin_decl_for_precision (BUILT_IN_LROUND, argprec);
  else if (resprec <= LONG_LONG_TYPE_SIZE)
    fn = builtin_decl_for_precision (BUILT_IN_LLROUND, argprec);
  else if (resprec >= argprec)
    fn = builtin_decl_for_precision (BUILT_IN_ROUND, argprec);
  else
    gcc_unreachable ();

  return convert (restype, build_call_expr_loc (input_location,
						fn, 1, arg));
}


/* Convert a real to an integer using a specific rounding mode.
   Ideally we would just build the corresponding GENERIC node,
   however the RTL expander only actually supports FIX_TRUNC_EXPR.  */

static tree
build_fix_expr (stmtblock_t * pblock, tree arg, tree type,
               enum rounding_mode op)
{
  switch (op)
    {
    case RND_FLOOR:
      return build_fixbound_expr (pblock, arg, type, 0);

    case RND_CEIL:
      return build_fixbound_expr (pblock, arg, type, 1);

    case RND_ROUND:
      return build_round_expr (arg, type);

    case RND_TRUNC:
      return fold_build1_loc (input_location, FIX_TRUNC_EXPR, type, arg);

    default:
      gcc_unreachable ();
    }
}


/* Round a real value using the specified rounding mode.
   We use a temporary integer of that same kind size as the result.
   Values larger than those that can be represented by this kind are
   unchanged, as they will not be accurate enough to represent the
   rounding.
    huge = HUGE (KIND (a))
    aint (a) = ((a > huge) || (a < -huge)) ? a : (real)(int)a
   */

static void
gfc_conv_intrinsic_aint (gfc_se * se, gfc_expr * expr, enum rounding_mode op)
{
  tree type;
  tree itype;
  tree arg[2];
  tree tmp;
  tree cond;
  tree decl;
  mpfr_t huge;
  int n, nargs;
  int kind;

  kind = expr->ts.kind;
  nargs = gfc_intrinsic_argument_list_length (expr);

  decl = NULL_TREE;
  /* We have builtin functions for some cases.  */
  switch (op)
    {
    case RND_ROUND:
      decl = gfc_builtin_decl_for_float_kind (BUILT_IN_ROUND, kind);
      break;

    case RND_TRUNC:
      decl = gfc_builtin_decl_for_float_kind (BUILT_IN_TRUNC, kind);
      break;

    default:
      gcc_unreachable ();
    }

  /* Evaluate the argument.  */
  gcc_assert (expr->value.function.actual->expr);
  gfc_conv_intrinsic_function_args (se, expr, arg, nargs);

  /* Use a builtin function if one exists.  */
  if (decl != NULL_TREE)
    {
      se->expr = build_call_expr_loc (input_location, decl, 1, arg[0]);
      return;
    }

  /* This code is probably redundant, but we'll keep it lying around just
     in case.  */
  type = gfc_typenode_for_spec (&expr->ts);
  arg[0] = gfc_evaluate_now (arg[0], &se->pre);

  /* Test if the value is too large to handle sensibly.  */
  gfc_set_model_kind (kind);
  mpfr_init (huge);
  n = gfc_validate_kind (BT_INTEGER, kind, false);
  mpfr_set_z (huge, gfc_integer_kinds[n].huge, GFC_RND_MODE);
  tmp = gfc_conv_mpfr_to_tree (huge, kind, 0);
  cond = fold_build2_loc (input_location, LT_EXPR, logical_type_node, arg[0],
			  tmp);

  mpfr_neg (huge, huge, GFC_RND_MODE);
  tmp = gfc_conv_mpfr_to_tree (huge, kind, 0);
  tmp = fold_build2_loc (input_location, GT_EXPR, logical_type_node, arg[0],
			 tmp);
  cond = fold_build2_loc (input_location, TRUTH_AND_EXPR, logical_type_node,
			  cond, tmp);
  itype = gfc_get_int_type (kind);

  tmp = build_fix_expr (&se->pre, arg[0], itype, op);
  tmp = convert (type, tmp);
  se->expr = fold_build3_loc (input_location, COND_EXPR, type, cond, tmp,
			      arg[0]);
  mpfr_clear (huge);
}


/* Convert to an integer using the specified rounding mode.  */

static void
gfc_conv_intrinsic_int (gfc_se * se, gfc_expr * expr, enum rounding_mode op)
{
  tree type;
  tree *args;
  int nargs;

  nargs = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, nargs);

  /* Evaluate the argument, we process all arguments even though we only
     use the first one for code generation purposes.  */
  type = gfc_typenode_for_spec (&expr->ts);
  gcc_assert (expr->value.function.actual->expr);
  gfc_conv_intrinsic_function_args (se, expr, args, nargs);

  if (TREE_CODE (TREE_TYPE (args[0])) == INTEGER_TYPE)
    {
      /* Conversion to a different integer kind.  */
      se->expr = convert (type, args[0]);
    }
  else
    {
      /* Conversion from complex to non-complex involves taking the real
         component of the value.  */
      if (TREE_CODE (TREE_TYPE (args[0])) == COMPLEX_TYPE
	  && expr->ts.type != BT_COMPLEX)
	{
	  tree artype;

	  artype = TREE_TYPE (TREE_TYPE (args[0]));
	  args[0] = fold_build1_loc (input_location, REALPART_EXPR, artype,
				     args[0]);
	}

      se->expr = build_fix_expr (&se->pre, args[0], type, op);
    }
}


/* Get the imaginary component of a value.  */

static void
gfc_conv_intrinsic_imagpart (gfc_se * se, gfc_expr * expr)
{
  tree arg;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  se->expr = fold_build1_loc (input_location, IMAGPART_EXPR,
			      TREE_TYPE (TREE_TYPE (arg)), arg);
}


/* Get the complex conjugate of a value.  */

static void
gfc_conv_intrinsic_conjg (gfc_se * se, gfc_expr * expr)
{
  tree arg;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  se->expr = fold_build1_loc (input_location, CONJ_EXPR, TREE_TYPE (arg), arg);
}



static tree
define_quad_builtin (const char *name, tree type, bool is_const)
{
  tree fndecl;
  fndecl = build_decl (input_location, FUNCTION_DECL, get_identifier (name),
		       type);

  /* Mark the decl as external.  */
  DECL_EXTERNAL (fndecl) = 1;
  TREE_PUBLIC (fndecl) = 1;

  /* Mark it __attribute__((const)).  */
  TREE_READONLY (fndecl) = is_const;

  rest_of_decl_compilation (fndecl, 1, 0);

  return fndecl;
}

/* Add SIMD attribute for FNDECL built-in if the built-in
   name is in VECTORIZED_BUILTINS.  */

static void
add_simd_flag_for_built_in (tree fndecl)
{
  if (gfc_vectorized_builtins == NULL
      || fndecl == NULL_TREE)
    return;

  const char *name = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  int *clauses = gfc_vectorized_builtins->get (name);
  if (clauses)
    {
      for (unsigned i = 0; i < 3; i++)
	if (*clauses & (1 << i))
	  {
	    gfc_simd_clause simd_type = (gfc_simd_clause)*clauses;
	    tree omp_clause = NULL_TREE;
	    if (simd_type == SIMD_NONE)
	      ; /* No SIMD clause.  */
	    else
	      {
		omp_clause_code code
		  = (simd_type == SIMD_INBRANCH
		     ? OMP_CLAUSE_INBRANCH : OMP_CLAUSE_NOTINBRANCH);
		omp_clause = build_omp_clause (UNKNOWN_LOCATION, code);
		omp_clause = build_tree_list (NULL_TREE, omp_clause);
	      }

	    DECL_ATTRIBUTES (fndecl)
	      = tree_cons (get_identifier ("omp declare simd"), omp_clause,
			   DECL_ATTRIBUTES (fndecl));
	  }
    }
}

  /* Set SIMD attribute to all built-in functions that are mentioned
     in gfc_vectorized_builtins vector.  */

void
gfc_adjust_builtins (void)
{
  gfc_intrinsic_map_t *m;
  for (m = gfc_intrinsic_map;
       m->id != GFC_ISYM_NONE || m->double_built_in != END_BUILTINS; m++)
    {
      add_simd_flag_for_built_in (m->real4_decl);
      add_simd_flag_for_built_in (m->complex4_decl);
      add_simd_flag_for_built_in (m->real8_decl);
      add_simd_flag_for_built_in (m->complex8_decl);
      add_simd_flag_for_built_in (m->real10_decl);
      add_simd_flag_for_built_in (m->complex10_decl);
      add_simd_flag_for_built_in (m->real16_decl);
      add_simd_flag_for_built_in (m->complex16_decl);
      add_simd_flag_for_built_in (m->real16_decl);
      add_simd_flag_for_built_in (m->complex16_decl);
    }

  /* Release all strings.  */
  if (gfc_vectorized_builtins != NULL)
    {
      for (hash_map<nofree_string_hash, int>::iterator it
	   = gfc_vectorized_builtins->begin ();
	   it != gfc_vectorized_builtins->end (); ++it)
	free (CONST_CAST (char *, (*it).first));

      delete gfc_vectorized_builtins;
      gfc_vectorized_builtins = NULL;
    }
}

/* Initialize function decls for library functions.  The external functions
   are created as required.  Builtin functions are added here.  */

void
gfc_build_intrinsic_lib_fndecls (void)
{
  gfc_intrinsic_map_t *m;
  tree quad_decls[END_BUILTINS + 1];

  if (gfc_real16_is_float128)
  {
    /* If we have soft-float types, we create the decls for their
       C99-like library functions.  For now, we only handle _Float128
       q-suffixed or IEC 60559 f128-suffixed functions.  */

    tree type, complex_type, func_1, func_2, func_3, func_cabs, func_frexp;
    tree func_iround, func_lround, func_llround, func_scalbn, func_cpow;

    memset (quad_decls, 0, sizeof(tree) * (END_BUILTINS + 1));

    type = gfc_float128_type_node;
    complex_type = gfc_complex_float128_type_node;
    /* type (*) (type) */
    func_1 = build_function_type_list (type, type, NULL_TREE);
    /* int (*) (type) */
    func_iround = build_function_type_list (integer_type_node,
					    type, NULL_TREE);
    /* long (*) (type) */
    func_lround = build_function_type_list (long_integer_type_node,
					    type, NULL_TREE);
    /* long long (*) (type) */
    func_llround = build_function_type_list (long_long_integer_type_node,
					     type, NULL_TREE);
    /* type (*) (type, type) */
    func_2 = build_function_type_list (type, type, type, NULL_TREE);
    /* type (*) (type, type, type) */
    func_3 = build_function_type_list (type, type, type, type, NULL_TREE);
    /* type (*) (type, &int) */
    func_frexp
      = build_function_type_list (type,
				  type,
				  build_pointer_type (integer_type_node),
				  NULL_TREE);
    /* type (*) (type, int) */
    func_scalbn = build_function_type_list (type,
					    type, integer_type_node, NULL_TREE);
    /* type (*) (complex type) */
    func_cabs = build_function_type_list (type, complex_type, NULL_TREE);
    /* complex type (*) (complex type, complex type) */
    func_cpow
      = build_function_type_list (complex_type,
				  complex_type, complex_type, NULL_TREE);

#define DEFINE_MATH_BUILTIN(ID, NAME, ARGTYPE)
#define DEFINE_MATH_BUILTIN_C(ID, NAME, ARGTYPE)
#define LIB_FUNCTION(ID, NAME, HAVE_COMPLEX)

    /* Only these built-ins are actually needed here. These are used directly
       from the code, when calling builtin_decl_for_precision() or
       builtin_decl_for_float_type(). The others are all constructed by
       gfc_get_intrinsic_lib_fndecl().  */
#define OTHER_BUILTIN(ID, NAME, TYPE, CONST) \
    quad_decls[BUILT_IN_ ## ID]						\
      = define_quad_builtin (gfc_real16_use_iec_60559			\
			     ? NAME "f128" : NAME "q", func_ ## TYPE,	\
			     CONST);

#include "mathbuiltins.def"

#undef OTHER_BUILTIN
#undef LIB_FUNCTION
#undef DEFINE_MATH_BUILTIN
#undef DEFINE_MATH_BUILTIN_C

    /* There is one built-in we defined manually, because it gets called
       with builtin_decl_for_precision() or builtin_decl_for_float_type()
       even though it is not an OTHER_BUILTIN: it is SQRT.  */
    quad_decls[BUILT_IN_SQRT]
      = define_quad_builtin (gfc_real16_use_iec_60559
			     ? "sqrtf128" : "sqrtq", func_1, true);
  }

  /* Add GCC builtin functions.  */
  for (m = gfc_intrinsic_map;
       m->id != GFC_ISYM_NONE || m->double_built_in != END_BUILTINS; m++)
    {
      if (m->float_built_in != END_BUILTINS)
	m->real4_decl = builtin_decl_explicit (m->float_built_in);
      if (m->complex_float_built_in != END_BUILTINS)
	m->complex4_decl = builtin_decl_explicit (m->complex_float_built_in);
      if (m->double_built_in != END_BUILTINS)
	m->real8_decl = builtin_decl_explicit (m->double_built_in);
      if (m->complex_double_built_in != END_BUILTINS)
	m->complex8_decl = builtin_decl_explicit (m->complex_double_built_in);

      /* If real(kind=10) exists, it is always long double.  */
      if (m->long_double_built_in != END_BUILTINS)
	m->real10_decl = builtin_decl_explicit (m->long_double_built_in);
      if (m->complex_long_double_built_in != END_BUILTINS)
	m->complex10_decl
	  = builtin_decl_explicit (m->complex_long_double_built_in);

      if (!gfc_real16_is_float128)
	{
	  if (m->long_double_built_in != END_BUILTINS)
	    m->real16_decl = builtin_decl_explicit (m->long_double_built_in);
	  if (m->complex_long_double_built_in != END_BUILTINS)
	    m->complex16_decl
	      = builtin_decl_explicit (m->complex_long_double_built_in);
	}
      else if (quad_decls[m->double_built_in] != NULL_TREE)
        {
	  /* Quad-precision function calls are constructed when first
	     needed by builtin_decl_for_precision(), except for those
	     that will be used directly (define by OTHER_BUILTIN).  */
	  m->real16_decl = quad_decls[m->double_built_in];
	}
      else if (quad_decls[m->complex_double_built_in] != NULL_TREE)
        {
	  /* Same thing for the complex ones.  */
	  m->complex16_decl = quad_decls[m->double_built_in];
	}
    }
}


/* Create a fndecl for a simple intrinsic library function.  */

static tree
gfc_get_intrinsic_lib_fndecl (gfc_intrinsic_map_t * m, gfc_expr * expr)
{
  tree type;
  vec<tree, va_gc> *argtypes;
  tree fndecl;
  gfc_actual_arglist *actual;
  tree *pdecl;
  gfc_typespec *ts;
  char name[GFC_MAX_SYMBOL_LEN + 3];

  ts = &expr->ts;
  if (ts->type == BT_REAL)
    {
      switch (ts->kind)
	{
	case 4:
	  pdecl = &m->real4_decl;
	  break;
	case 8:
	  pdecl = &m->real8_decl;
	  break;
	case 10:
	  pdecl = &m->real10_decl;
	  break;
	case 16:
	  pdecl = &m->real16_decl;
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  else if (ts->type == BT_COMPLEX)
    {
      gcc_assert (m->complex_available);

      switch (ts->kind)
	{
	case 4:
	  pdecl = &m->complex4_decl;
	  break;
	case 8:
	  pdecl = &m->complex8_decl;
	  break;
	case 10:
	  pdecl = &m->complex10_decl;
	  break;
	case 16:
	  pdecl = &m->complex16_decl;
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  else
    gcc_unreachable ();

  if (*pdecl)
    return *pdecl;

  if (m->libm_name)
    {
      int n = gfc_validate_kind (BT_REAL, ts->kind, false);
      if (gfc_real_kinds[n].c_float)
	snprintf (name, sizeof (name), "%s%s%s",
		  ts->type == BT_COMPLEX ? "c" : "", m->name, "f");
      else if (gfc_real_kinds[n].c_double)
	snprintf (name, sizeof (name), "%s%s",
		  ts->type == BT_COMPLEX ? "c" : "", m->name);
      else if (gfc_real_kinds[n].c_long_double)
	snprintf (name, sizeof (name), "%s%s%s",
		  ts->type == BT_COMPLEX ? "c" : "", m->name, "l");
      else if (gfc_real_kinds[n].c_float128)
	snprintf (name, sizeof (name), "%s%s%s",
		  ts->type == BT_COMPLEX ? "c" : "", m->name,
		  gfc_real_kinds[n].use_iec_60559 ? "f128" : "q");
      else
	gcc_unreachable ();
    }
  else
    {
      snprintf (name, sizeof (name), PREFIX ("%s_%c%d"), m->name,
		ts->type == BT_COMPLEX ? 'c' : 'r',
		gfc_type_abi_kind (ts));
    }

  argtypes = NULL;
  for (actual = expr->value.function.actual; actual; actual = actual->next)
    {
      type = gfc_typenode_for_spec (&actual->expr->ts);
      vec_safe_push (argtypes, type);
    }
  type = build_function_type_vec (gfc_typenode_for_spec (ts), argtypes);
  fndecl = build_decl (input_location,
		       FUNCTION_DECL, get_identifier (name), type);

  /* Mark the decl as external.  */
  DECL_EXTERNAL (fndecl) = 1;
  TREE_PUBLIC (fndecl) = 1;

  /* Mark it __attribute__((const)), if possible.  */
  TREE_READONLY (fndecl) = m->is_constant;

  rest_of_decl_compilation (fndecl, 1, 0);

  (*pdecl) = fndecl;
  return fndecl;
}


/* Convert an intrinsic function into an external or builtin call.  */

static void
gfc_conv_intrinsic_lib_function (gfc_se * se, gfc_expr * expr)
{
  gfc_intrinsic_map_t *m;
  tree fndecl;
  tree rettype;
  tree *args;
  unsigned int num_args;
  gfc_isym_id id;

  id = expr->value.function.isym->id;
  /* Find the entry for this function.  */
  for (m = gfc_intrinsic_map;
       m->id != GFC_ISYM_NONE || m->double_built_in != END_BUILTINS; m++)
    {
      if (id == m->id)
	break;
    }

  if (m->id == GFC_ISYM_NONE)
    {
      gfc_internal_error ("Intrinsic function %qs (%d) not recognized",
			  expr->value.function.name, id);
    }

  /* Get the decl and generate the call.  */
  num_args = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, num_args);

  gfc_conv_intrinsic_function_args (se, expr, args, num_args);
  fndecl = gfc_get_intrinsic_lib_fndecl (m, expr);
  rettype = TREE_TYPE (TREE_TYPE (fndecl));

  fndecl = build_addr (fndecl);
  se->expr = build_call_array_loc (input_location, rettype, fndecl, num_args, args);
}


/* If bounds-checking is enabled, create code to verify at runtime that the
   string lengths for both expressions are the same (needed for e.g. MERGE).
   If bounds-checking is not enabled, does nothing.  */

void
gfc_trans_same_strlen_check (const char* intr_name, locus* where,
			     tree a, tree b, stmtblock_t* target)
{
  tree cond;
  tree name;

  /* If bounds-checking is disabled, do nothing.  */
  if (!(gfc_option.rtcheck & GFC_RTCHECK_BOUNDS))
    return;

  /* Compare the two string lengths.  */
  cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node, a, b);

  /* Output the runtime-check.  */
  name = gfc_build_cstring_const (intr_name);
  name = gfc_build_addr_expr (pchar_type_node, name);
  gfc_trans_runtime_check (true, false, cond, target, where,
			   "Unequal character lengths (%ld/%ld) in %s",
			   fold_convert (long_integer_type_node, a),
			   fold_convert (long_integer_type_node, b), name);
}


/* The EXPONENT(X) intrinsic function is translated into
       int ret;
       return isfinite(X) ? (frexp (X, &ret) , ret) : huge
   so that if X is a NaN or infinity, the result is HUGE(0).
 */

static void
gfc_conv_intrinsic_exponent (gfc_se *se, gfc_expr *expr)
{
  tree arg, type, res, tmp, frexp, cond, huge;
  int i;

  frexp = gfc_builtin_decl_for_float_kind (BUILT_IN_FREXP,
				       expr->value.function.actual->expr->ts.kind);

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  arg = gfc_evaluate_now (arg, &se->pre);

  i = gfc_validate_kind (BT_INTEGER, gfc_c_int_kind, false);
  huge = gfc_conv_mpz_to_tree (gfc_integer_kinds[i].huge, gfc_c_int_kind);
  cond = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_ISFINITE),
			      1, arg);

  res = gfc_create_var (integer_type_node, NULL);
  tmp = build_call_expr_loc (input_location, frexp, 2, arg,
			     gfc_build_addr_expr (NULL_TREE, res));
  tmp = fold_build2_loc (input_location, COMPOUND_EXPR, integer_type_node,
			 tmp, res);
  se->expr = fold_build3_loc (input_location, COND_EXPR, integer_type_node,
			      cond, tmp, huge);

  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = fold_convert (type, se->expr);
}


static int caf_call_cnt = 0;

static tree
conv_caf_func_index (stmtblock_t *block, gfc_namespace *ns, const char *pat,
		     gfc_expr *hash)
{
  char *name;
  gfc_se argse;
  gfc_expr func_index;
  gfc_symtree *index_st;
  tree func_index_tree;
  stmtblock_t blk;

  /* Need to get namespace where static variables are possible.  */
  while (ns && ns->proc_name && ns->proc_name->attr.flavor == FL_LABEL)
    ns = ns->parent;
  gcc_assert (ns);

  name = xasprintf (pat, caf_call_cnt);
  gcc_assert (!gfc_get_sym_tree (name, ns, &index_st, false));
  free (name);

  index_st->n.sym->attr.flavor = FL_VARIABLE;
  index_st->n.sym->attr.save = SAVE_EXPLICIT;
  index_st->n.sym->value
    = gfc_get_constant_expr (BT_INTEGER, gfc_default_integer_kind,
			     &gfc_current_locus);
  mpz_init_set_si (index_st->n.sym->value->value.integer, -1);
  index_st->n.sym->ts.type = BT_INTEGER;
  index_st->n.sym->ts.kind = gfc_default_integer_kind;
  gfc_set_sym_referenced (index_st->n.sym);
  memset (&func_index, 0, sizeof (gfc_expr));
  gfc_clear_ts (&func_index.ts);
  func_index.expr_type = EXPR_VARIABLE;
  func_index.symtree = index_st;
  func_index.ts = index_st->n.sym->ts;
  gfc_commit_symbol (index_st->n.sym);

  gfc_init_se (&argse, NULL);
  gfc_conv_expr (&argse, &func_index);
  gfc_add_block_to_block (block, &argse.pre);
  func_index_tree = argse.expr;

  gfc_init_se (&argse, NULL);
  gfc_conv_expr (&argse, hash);

  gfc_init_block (&blk);
  gfc_add_modify (&blk, func_index_tree,
		  build_call_expr (gfor_fndecl_caf_get_remote_function_index, 1,
				   argse.expr));
  gfc_add_expr_to_block (
    block,
    build3 (COND_EXPR, void_type_node,
	    gfc_likely (build2 (EQ_EXPR, logical_type_node, func_index_tree,
				build_int_cst (integer_type_node, -1)),
			PRED_FIRST_MATCH),
	    gfc_finish_block (&blk), NULL_TREE));

  return func_index_tree;
}

static tree
conv_caf_add_call_data (stmtblock_t *blk, gfc_namespace *ns, const char *pat,
			gfc_symbol *data_sym, tree *data_size)
{
  char *name;
  gfc_symtree *data_st;
  gfc_constructor *con;
  gfc_expr data, data_init;
  gfc_se argse;
  tree data_tree;

  memset (&data, 0, sizeof (gfc_expr));
  gfc_clear_ts (&data.ts);
  data.expr_type = EXPR_VARIABLE;
  name = xasprintf (pat, caf_call_cnt);
  gcc_assert (!gfc_get_sym_tree (name, ns, &data_st, false));
  free (name);
  data_st->n.sym->attr.flavor = FL_VARIABLE;
  data_st->n.sym->ts = data_sym->ts;
  data.symtree = data_st;
  gfc_set_sym_referenced (data.symtree->n.sym);
  data.ts = data_st->n.sym->ts;
  gfc_commit_symbol (data_st->n.sym);

  memset (&data_init, 0, sizeof (gfc_expr));
  gfc_clear_ts (&data_init.ts);
  data_init.expr_type = EXPR_STRUCTURE;
  data_init.ts = data.ts;
  for (gfc_component *comp = data.ts.u.derived->components; comp;
       comp = comp->next)
    {
      con = gfc_constructor_get ();
      con->expr = comp->initializer;
      comp->initializer = NULL;
      gfc_constructor_append (&data_init.value.constructor, con);
    }

  if (data.ts.u.derived->components)
    {
      gfc_init_se (&argse, NULL);
      gfc_conv_expr (&argse, &data);
      data_tree = argse.expr;
      gfc_add_expr_to_block (blk,
			     gfc_trans_structure_assign (data_tree, &data_init,
							 true, true));
      gfc_constructor_free (data_init.value.constructor);
      *data_size = TREE_TYPE (data_tree)->type_common.size_unit;
      data_tree = gfc_build_addr_expr (pvoid_type_node, data_tree);
    }
  else
    {
      data_tree = build_zero_cst (pvoid_type_node);
      *data_size = build_zero_cst (size_type_node);
    }

  return data_tree;
}

static tree
conv_shape_to_cst (gfc_expr *e)
{
  tree tmp = NULL;
  for (int d = 0; d < e->rank; ++d)
    {
      if (!tmp)
	tmp = gfc_conv_mpz_to_tree (e->shape[d], gfc_size_kind);
      else
	tmp = fold_build2 (MULT_EXPR, TREE_TYPE (tmp), tmp,
			   gfc_conv_mpz_to_tree (e->shape[d], gfc_size_kind));
    }
  return fold_convert (size_type_node, tmp);
}

static void
conv_stat_and_team (stmtblock_t *block, gfc_expr *expr, tree *stat, tree *team)
{
  gfc_expr *stat_e, *team_e;

  stat_e = gfc_find_stat_co (expr);
  if (stat_e)
    {
      gfc_se stat_se;
      gfc_init_se (&stat_se, NULL);
      gfc_conv_expr_reference (&stat_se, stat_e);
      *stat = stat_se.expr;
      gfc_add_block_to_block (block, &stat_se.pre);
      gfc_add_block_to_block (block, &stat_se.post);
    }
  else
    *stat = null_pointer_node;

  team_e = gfc_find_team_co (expr);
  if (team_e)
    {
      gfc_se team_se;
      gfc_init_se (&team_se, NULL);
      gfc_conv_expr_reference (&team_se, team_e);
      *team = team_se.expr;
      gfc_add_block_to_block (block, &team_se.pre);
      gfc_add_block_to_block (block, &team_se.post);
    }
  else
    *team = null_pointer_node;
}

/* Get data from a remote coarray.  */

static void
gfc_conv_intrinsic_caf_get (gfc_se *se, gfc_expr *expr, tree lhs,
			    bool may_realloc, symbol_attribute *caf_attr)
{
  gfc_expr *array_expr;
  tree caf_decl, token, image_index, tmp, res_var, type, stat, dest_size,
    dest_data, opt_dest_desc, get_fn_index_tree, add_data_tree, add_data_size,
    opt_src_desc, opt_src_charlen, opt_dest_charlen, team;
  symbol_attribute caf_attr_store;
  gfc_namespace *ns;
  gfc_expr *get_fn_hash = expr->value.function.actual->next->expr,
	   *get_fn_expr = expr->value.function.actual->next->next->expr;
  gfc_symbol *add_data_sym = get_fn_expr->symtree->n.sym->formal->sym;

  gcc_assert (flag_coarray == GFC_FCOARRAY_LIB);

  if (se->ss && se->ss->info->useflags)
    {
      /* Access the previously obtained result.  */
      gfc_conv_tmp_array_ref (se);
      return;
    }

  array_expr = expr->value.function.actual->expr;
  ns = array_expr->expr_type == EXPR_VARIABLE
	   && !array_expr->symtree->n.sym->attr.associate_var
	 ? array_expr->symtree->n.sym->ns
	 : gfc_current_ns;
  type = gfc_typenode_for_spec (&array_expr->ts);

  if (caf_attr == NULL)
    {
      caf_attr_store = gfc_caf_attr (array_expr);
      caf_attr = &caf_attr_store;
    }

  res_var = lhs;

  conv_stat_and_team (&se->pre, expr, &stat, &team);

  get_fn_index_tree
    = conv_caf_func_index (&se->pre, ns, "__caf_get_from_remote_fn_index_%d",
			   get_fn_hash);
  add_data_tree
    = conv_caf_add_call_data (&se->pre, ns, "__caf_get_from_remote_add_data_%d",
			      add_data_sym, &add_data_size);
  ++caf_call_cnt;

  if (array_expr->rank == 0)
    {
      res_var = gfc_create_var (type, "caf_res");
      if (array_expr->ts.type == BT_CHARACTER)
	{
	  gfc_conv_string_length (array_expr->ts.u.cl, array_expr, &se->pre);
	  se->string_length = array_expr->ts.u.cl->backend_decl;
	  opt_src_charlen = gfc_build_addr_expr (
	    NULL_TREE, gfc_trans_force_lval (&se->pre, se->string_length));
	  dest_size = build_int_cstu (size_type_node, array_expr->ts.kind);
	}
      else
	{
	  dest_size = res_var->typed.type->type_common.size_unit;
	  opt_src_charlen
	    = build_zero_cst (build_pointer_type (size_type_node));
	}
      dest_data
	= gfc_evaluate_now (gfc_build_addr_expr (NULL_TREE, res_var), &se->pre);
      res_var = build_fold_indirect_ref (dest_data);
      dest_data = gfc_build_addr_expr (pvoid_type_node, dest_data);
      opt_dest_desc = build_zero_cst (pvoid_type_node);
    }
  else
    {
      /* Create temporary.  */
      may_realloc = gfc_trans_create_temp_array (&se->pre, &se->post, se->ss,
						 type, NULL_TREE, false, false,
						 false, &array_expr->where)
		    == NULL_TREE;
      res_var = se->ss->info->data.array.descriptor;
      if (array_expr->ts.type == BT_CHARACTER)
	{
	  se->string_length = array_expr->ts.u.cl->backend_decl;
	  opt_src_charlen = gfc_build_addr_expr (
	    NULL_TREE, gfc_trans_force_lval (&se->pre, se->string_length));
	  dest_size = build_int_cstu (size_type_node, array_expr->ts.kind);
	}
      else
	{
	  opt_src_charlen
	    = build_zero_cst (build_pointer_type (size_type_node));
	  dest_size = fold_build2 (
	    MULT_EXPR, size_type_node,
	    fold_convert (size_type_node,
			  array_expr->shape
			    ? conv_shape_to_cst (array_expr)
			    : gfc_conv_descriptor_size (res_var,
							array_expr->rank)),
	    fold_convert (size_type_node,
			  gfc_conv_descriptor_span_get (res_var)));
	}
      opt_dest_desc = res_var;
      dest_data = gfc_conv_descriptor_data_get (res_var);
      opt_dest_desc = gfc_build_addr_expr (NULL_TREE, opt_dest_desc);
      if (may_realloc)
	{
	  tmp = gfc_conv_descriptor_data_get (res_var);
	  tmp = gfc_deallocate_with_status (tmp, NULL_TREE, NULL_TREE,
					    NULL_TREE, NULL_TREE, true, NULL,
					    GFC_CAF_COARRAY_NOCOARRAY);
	  gfc_add_expr_to_block (&se->post, tmp);
	}
      dest_data
	= gfc_build_addr_expr (NULL_TREE,
			       gfc_trans_force_lval (&se->pre, dest_data));
    }

  opt_dest_charlen = opt_src_charlen;
  caf_decl = gfc_get_tree_for_caf_expr (array_expr);
  if (TREE_CODE (TREE_TYPE (caf_decl)) == REFERENCE_TYPE)
    caf_decl = build_fold_indirect_ref_loc (input_location, caf_decl);

  if (!TYPE_LANG_SPECIFIC (TREE_TYPE (caf_decl))->rank
      || GFC_ARRAY_TYPE_P (TREE_TYPE (caf_decl)))
    opt_src_desc = build_zero_cst (pvoid_type_node);
  else
    opt_src_desc = gfc_build_addr_expr (pvoid_type_node, caf_decl);

  image_index = gfc_caf_get_image_index (&se->pre, array_expr, caf_decl);
  gfc_get_caf_token_offset (se, &token, NULL, caf_decl, NULL, array_expr);

  /* It guarantees memory consistency within the same segment.  */
  tmp = gfc_build_string_const (strlen ("memory") + 1, "memory");
  tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
		    gfc_build_string_const (1, ""), NULL_TREE, NULL_TREE,
		    tree_cons (NULL_TREE, tmp, NULL_TREE), NULL_TREE);
  ASM_VOLATILE_P (tmp) = 1;
  gfc_add_expr_to_block (&se->pre, tmp);

  tmp = build_call_expr_loc (
    input_location, gfor_fndecl_caf_get_from_remote, 15, token, opt_src_desc,
    opt_src_charlen, image_index, dest_size, dest_data, opt_dest_charlen,
    opt_dest_desc, constant_boolean_node (may_realloc, boolean_type_node),
    get_fn_index_tree, add_data_tree, add_data_size, stat, team,
    null_pointer_node);

  gfc_add_expr_to_block (&se->pre, tmp);

  if (se->ss)
    gfc_advance_se_ss_chain (se);

  se->expr = res_var;

  return;
}

/* Generate call to caf_is_present_on_remote for allocated (coarrary[...])
   calls.  */

static void
gfc_conv_intrinsic_caf_is_present_remote (gfc_se *se, gfc_expr *e)
{
  gfc_expr *caf_expr, *hash, *present_fn;
  gfc_symbol *add_data_sym;
  tree fn_index, add_data_tree, add_data_size, caf_decl, image_index, token;

  gcc_assert (e->expr_type == EXPR_FUNCTION
	      && e->value.function.isym->id
		   == GFC_ISYM_CAF_IS_PRESENT_ON_REMOTE);
  caf_expr = e->value.function.actual->expr;
  hash = e->value.function.actual->next->expr;
  present_fn = e->value.function.actual->next->next->expr;
  add_data_sym = present_fn->symtree->n.sym->formal->sym;

  fn_index = conv_caf_func_index (&se->pre, gfc_current_ns,
				  "__caf_present_on_remote_fn_index_%d", hash);
  add_data_tree = conv_caf_add_call_data (&se->pre, gfc_current_ns,
					  "__caf_present_on_remote_add_data_%d",
					  add_data_sym, &add_data_size);
  ++caf_call_cnt;

  caf_decl = gfc_get_tree_for_caf_expr (caf_expr);
  if (TREE_CODE (TREE_TYPE (caf_decl)) == REFERENCE_TYPE)
    caf_decl = build_fold_indirect_ref_loc (input_location, caf_decl);

  image_index = gfc_caf_get_image_index (&se->pre, caf_expr, caf_decl);
  gfc_get_caf_token_offset (se, &token, NULL, caf_decl, NULL, caf_expr);

  se->expr
    = fold_convert (logical_type_node,
		    build_call_expr_loc (input_location,
					 gfor_fndecl_caf_is_present_on_remote,
					 5, token, image_index, fn_index,
					 add_data_tree, add_data_size));
}

static tree
conv_caf_send_to_remote (gfc_code *code)
{
  gfc_expr *lhs_expr, *rhs_expr, *lhs_hash, *receiver_fn_expr;
  gfc_symbol *add_data_sym;
  gfc_se lhs_se, rhs_se;
  stmtblock_t block;
  gfc_namespace *ns;
  tree caf_decl, token, rhs_size, image_index, tmp, rhs_data;
  tree lhs_stat, lhs_team, opt_lhs_charlen, opt_rhs_charlen;
  tree opt_lhs_desc = NULL_TREE, opt_rhs_desc = NULL_TREE;
  tree receiver_fn_index_tree, add_data_tree, add_data_size;

  gcc_assert (flag_coarray == GFC_FCOARRAY_LIB);
  gcc_assert (code->resolved_isym->id == GFC_ISYM_CAF_SEND);

  lhs_expr = code->ext.actual->expr;
  rhs_expr = code->ext.actual->next->expr;
  lhs_hash = code->ext.actual->next->next->expr;
  receiver_fn_expr = code->ext.actual->next->next->next->expr;
  add_data_sym = receiver_fn_expr->symtree->n.sym->formal->sym;

  ns = lhs_expr->expr_type == EXPR_VARIABLE
	   && !lhs_expr->symtree->n.sym->attr.associate_var
	 ? lhs_expr->symtree->n.sym->ns
	 : gfc_current_ns;

  gfc_init_block (&block);

  /* LHS.  */
  gfc_init_se (&lhs_se, NULL);
  caf_decl = gfc_get_tree_for_caf_expr (lhs_expr);
  if (TREE_CODE (TREE_TYPE (caf_decl)) == REFERENCE_TYPE)
    caf_decl = build_fold_indirect_ref_loc (input_location, caf_decl);
  if (lhs_expr->rank == 0)
    {
      if (lhs_expr->ts.type == BT_CHARACTER)
	{
	  gfc_conv_string_length (lhs_expr->ts.u.cl, lhs_expr, &block);
	  lhs_se.string_length = lhs_expr->ts.u.cl->backend_decl;
	  opt_lhs_charlen = gfc_build_addr_expr (
	    NULL_TREE, gfc_trans_force_lval (&block, lhs_se.string_length));
	}
      else
	opt_lhs_charlen = build_zero_cst (build_pointer_type (size_type_node));
      opt_lhs_desc = null_pointer_node;
    }
  else
    {
      gfc_conv_expr_descriptor (&lhs_se, lhs_expr);
      gfc_add_block_to_block (&block, &lhs_se.pre);
      opt_lhs_desc = lhs_se.expr;
      if (lhs_expr->ts.type == BT_CHARACTER)
	opt_lhs_charlen = gfc_build_addr_expr (
	  NULL_TREE, gfc_trans_force_lval (&block, lhs_se.string_length));
      else
	opt_lhs_charlen = build_zero_cst (build_pointer_type (size_type_node));
      /* Get the third formal argument of the receiver function.  (This is the
	 location where to put the data on the remote image.)  Need to look at
	 the argument in the function decl, because in the gfc_symbol's formal
	 argument an array may have no descriptor while in the generated
	 function decl it has.  */
      tmp = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (TYPE_ARG_TYPES (
	TREE_TYPE (receiver_fn_expr->symtree->n.sym->backend_decl)))));
      if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (tmp)))
	opt_lhs_desc = null_pointer_node;
      else
	opt_lhs_desc
	  = gfc_build_addr_expr (NULL_TREE,
				 gfc_trans_force_lval (&block, opt_lhs_desc));
    }

  /* Obtain token, offset and image index for the LHS.  */
  image_index = gfc_caf_get_image_index (&block, lhs_expr, caf_decl);
  gfc_get_caf_token_offset (&lhs_se, &token, NULL, caf_decl, NULL, lhs_expr);

  /* RHS.  */
  gfc_init_se (&rhs_se, NULL);
  if (rhs_expr->rank == 0)
    {
      rhs_se.want_pointer = rhs_expr->ts.type == BT_CHARACTER;
      gfc_conv_expr (&rhs_se, rhs_expr);
      gfc_add_block_to_block (&block, &rhs_se.pre);
      opt_rhs_desc = null_pointer_node;
      if (rhs_expr->ts.type == BT_CHARACTER)
	{
	  rhs_data
	    = rhs_expr->expr_type == EXPR_CONSTANT
		? gfc_build_addr_expr (NULL_TREE,
				       gfc_trans_force_lval (&block,
							     rhs_se.expr))
		: rhs_se.expr;
	  opt_rhs_charlen = gfc_build_addr_expr (
	    NULL_TREE, gfc_trans_force_lval (&block, rhs_se.string_length));
	  rhs_size = build_int_cstu (size_type_node, rhs_expr->ts.kind);
	}
      else
	{
	  rhs_data
	    = gfc_build_addr_expr (NULL_TREE,
				   gfc_trans_force_lval (&block, rhs_se.expr));
	  opt_rhs_charlen
	    = build_zero_cst (build_pointer_type (size_type_node));
	  rhs_size = TREE_TYPE (rhs_se.expr)->type_common.size_unit;
	}
    }
  else
    {
      rhs_se.force_tmp = rhs_expr->shape == NULL
			 || !gfc_is_simply_contiguous (rhs_expr, false, false);
      gfc_conv_expr_descriptor (&rhs_se, rhs_expr);
      gfc_add_block_to_block (&block, &rhs_se.pre);
      opt_rhs_desc = rhs_se.expr;
      if (rhs_expr->ts.type == BT_CHARACTER)
	{
	  opt_rhs_charlen = gfc_build_addr_expr (
	    NULL_TREE, gfc_trans_force_lval (&block, rhs_se.string_length));
	  rhs_size = build_int_cstu (size_type_node, rhs_expr->ts.kind);
	}
      else
	{
	  opt_rhs_charlen
	    = build_zero_cst (build_pointer_type (size_type_node));
	  rhs_size = fold_build2 (
	    MULT_EXPR, size_type_node,
	    fold_convert (size_type_node,
			  rhs_expr->shape
			    ? conv_shape_to_cst (rhs_expr)
			    : gfc_conv_descriptor_size (rhs_se.expr,
							rhs_expr->rank)),
	    fold_convert (size_type_node,
			  gfc_conv_descriptor_span_get (rhs_se.expr)));
	}

      rhs_data = gfc_build_addr_expr (
	NULL_TREE, gfc_trans_force_lval (&block, gfc_conv_descriptor_data_get (
						   opt_rhs_desc)));
      opt_rhs_desc = gfc_build_addr_expr (NULL_TREE, opt_rhs_desc);
    }
  gfc_add_block_to_block (&block, &rhs_se.pre);

  conv_stat_and_team (&block, lhs_expr, &lhs_stat, &lhs_team);

  receiver_fn_index_tree
    = conv_caf_func_index (&block, ns, "__caf_send_to_remote_fn_index_%d",
			   lhs_hash);
  add_data_tree
    = conv_caf_add_call_data (&block, ns, "__caf_send_to_remote_add_data_%d",
			      add_data_sym, &add_data_size);
  ++caf_call_cnt;

  tmp
    = build_call_expr_loc (input_location, gfor_fndecl_caf_send_to_remote, 14,
			   token, opt_lhs_desc, opt_lhs_charlen, image_index,
			   rhs_size, rhs_data, opt_rhs_charlen, opt_rhs_desc,
			   receiver_fn_index_tree, add_data_tree, add_data_size,
			   lhs_stat, lhs_team, null_pointer_node);

  gfc_add_expr_to_block (&block, tmp);
  gfc_add_block_to_block (&block, &lhs_se.post);
  gfc_add_block_to_block (&block, &rhs_se.post);

  /* It guarantees memory consistency within the same segment.  */
  tmp = gfc_build_string_const (strlen ("memory") + 1, "memory");
  tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
		    gfc_build_string_const (1, ""), NULL_TREE, NULL_TREE,
		    tree_cons (NULL_TREE, tmp, NULL_TREE), NULL_TREE);
  ASM_VOLATILE_P (tmp) = 1;
  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}

/* Send-get data to a remote coarray.  */

static tree
conv_caf_sendget (gfc_code *code)
{
  /* lhs stuff  */
  gfc_expr *lhs_expr, *lhs_hash, *receiver_fn_expr;
  gfc_symbol *lhs_add_data_sym;
  gfc_se lhs_se;
  tree lhs_caf_decl, lhs_token, opt_lhs_charlen,
    opt_lhs_desc = NULL_TREE, receiver_fn_index_tree, lhs_image_index,
    lhs_add_data_tree, lhs_add_data_size, lhs_stat, lhs_team;
  int transfer_rank;

  /* rhs stuff  */
  gfc_expr *rhs_expr, *rhs_hash, *sender_fn_expr;
  gfc_symbol *rhs_add_data_sym;
  gfc_se rhs_se;
  tree rhs_caf_decl, rhs_token, opt_rhs_charlen,
    opt_rhs_desc = NULL_TREE, sender_fn_index_tree, rhs_image_index,
    rhs_add_data_tree, rhs_add_data_size, rhs_stat, rhs_team;

  /* shared  */
  stmtblock_t block;
  gfc_namespace *ns;
  tree tmp, rhs_size;

  gcc_assert (flag_coarray == GFC_FCOARRAY_LIB);
  gcc_assert (code->resolved_isym->id == GFC_ISYM_CAF_SENDGET);

  lhs_expr = code->ext.actual->expr;
  rhs_expr = code->ext.actual->next->expr;
  lhs_hash = code->ext.actual->next->next->expr;
  receiver_fn_expr = code->ext.actual->next->next->next->expr;
  rhs_hash = code->ext.actual->next->next->next->next->expr;
  sender_fn_expr = code->ext.actual->next->next->next->next->next->expr;

  lhs_add_data_sym = receiver_fn_expr->symtree->n.sym->formal->sym;
  rhs_add_data_sym = sender_fn_expr->symtree->n.sym->formal->sym;

  ns = lhs_expr->expr_type == EXPR_VARIABLE
	   && !lhs_expr->symtree->n.sym->attr.associate_var
	 ? lhs_expr->symtree->n.sym->ns
	 : gfc_current_ns;

  gfc_init_block (&block);

  lhs_stat = null_pointer_node;
  lhs_team = null_pointer_node;
  rhs_stat = null_pointer_node;
  rhs_team = null_pointer_node;

  /* LHS.  */
  gfc_init_se (&lhs_se, NULL);
  lhs_caf_decl = gfc_get_tree_for_caf_expr (lhs_expr);
  if (TREE_CODE (TREE_TYPE (lhs_caf_decl)) == REFERENCE_TYPE)
    lhs_caf_decl = build_fold_indirect_ref_loc (input_location, lhs_caf_decl);
  if (lhs_expr->rank == 0)
    {
      if (lhs_expr->ts.type == BT_CHARACTER)
	{
	  gfc_conv_string_length (lhs_expr->ts.u.cl, lhs_expr, &block);
	  lhs_se.string_length = lhs_expr->ts.u.cl->backend_decl;
	  opt_lhs_charlen = gfc_build_addr_expr (
	    NULL_TREE, gfc_trans_force_lval (&block, lhs_se.string_length));
	}
      else
	opt_lhs_charlen = build_zero_cst (build_pointer_type (size_type_node));
      opt_lhs_desc = null_pointer_node;
    }
  else
    {
      gfc_conv_expr_descriptor (&lhs_se, lhs_expr);
      gfc_add_block_to_block (&block, &lhs_se.pre);
      opt_lhs_desc = lhs_se.expr;
      if (lhs_expr->ts.type == BT_CHARACTER)
	opt_lhs_charlen = gfc_build_addr_expr (
	  NULL_TREE, gfc_trans_force_lval (&block, lhs_se.string_length));
      else
	opt_lhs_charlen = build_zero_cst (build_pointer_type (size_type_node));
      /* Get the third formal argument of the receiver function.  (This is the
	 location where to put the data on the remote image.)  Need to look at
	 the argument in the function decl, because in the gfc_symbol's formal
	 argument an array may have no descriptor while in the generated
	 function decl it has.  */
      tmp = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (TYPE_ARG_TYPES (
	TREE_TYPE (receiver_fn_expr->symtree->n.sym->backend_decl)))));
      if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (tmp)))
	opt_lhs_desc = null_pointer_node;
      else
	opt_lhs_desc
	  = gfc_build_addr_expr (NULL_TREE,
				 gfc_trans_force_lval (&block, opt_lhs_desc));
    }

  /* Obtain token, offset and image index for the LHS.  */
  lhs_image_index = gfc_caf_get_image_index (&block, lhs_expr, lhs_caf_decl);
  gfc_get_caf_token_offset (&lhs_se, &lhs_token, NULL, lhs_caf_decl, NULL,
			    lhs_expr);

  /* RHS.  */
  rhs_caf_decl = gfc_get_tree_for_caf_expr (rhs_expr);
  if (TREE_CODE (TREE_TYPE (rhs_caf_decl)) == REFERENCE_TYPE)
    rhs_caf_decl = build_fold_indirect_ref_loc (input_location, rhs_caf_decl);
  transfer_rank = rhs_expr->rank;
  gfc_expression_rank (rhs_expr);
  gfc_init_se (&rhs_se, NULL);
  if (rhs_expr->rank == 0)
    {
      opt_rhs_desc = null_pointer_node;
      if (rhs_expr->ts.type == BT_CHARACTER)
	{
	  gfc_conv_expr (&rhs_se, rhs_expr);
	  gfc_add_block_to_block (&block, &rhs_se.pre);
	  opt_rhs_charlen = gfc_build_addr_expr (
	    NULL_TREE, gfc_trans_force_lval (&block, rhs_se.string_length));
	  rhs_size = build_int_cstu (size_type_node, rhs_expr->ts.kind);
	}
      else
	{
	  gfc_typespec *ts
	    = &sender_fn_expr->symtree->n.sym->formal->next->next->sym->ts;

	  opt_rhs_charlen
	    = build_zero_cst (build_pointer_type (size_type_node));
	  rhs_size = gfc_typenode_for_spec (ts)->type_common.size_unit;
	}
    }
  /* Get the fifth formal argument of the getter function.  This is the argument
     pointing to the data to get on the remote image.  Need to look at the
     argument in the function decl, because in the gfc_symbol's formal argument
     an array may have no descriptor while in the generated function decl it
     has.  */
  else if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (TREE_VALUE (
	     TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (TYPE_ARG_TYPES (
	       TREE_TYPE (sender_fn_expr->symtree->n.sym->backend_decl))))))))))
    {
      rhs_se.data_not_needed = 1;
      gfc_conv_expr_descriptor (&rhs_se, rhs_expr);
      gfc_add_block_to_block (&block, &rhs_se.pre);
      if (rhs_expr->ts.type == BT_CHARACTER)
	{
	  opt_rhs_charlen = gfc_build_addr_expr (
	    NULL_TREE, gfc_trans_force_lval (&block, rhs_se.string_length));
	  rhs_size = build_int_cstu (size_type_node, rhs_expr->ts.kind);
	}
      else
	{
	  opt_rhs_charlen
	    = build_zero_cst (build_pointer_type (size_type_node));
	  rhs_size = TREE_TYPE (rhs_se.expr)->type_common.size_unit;
	}
      opt_rhs_desc = null_pointer_node;
    }
  else
    {
      gfc_ref *arr_ref = rhs_expr->ref;
      while (arr_ref && arr_ref->type != REF_ARRAY)
	arr_ref = arr_ref->next;
      rhs_se.force_tmp
	= (rhs_expr->shape == NULL
	   && (!arr_ref || !gfc_full_array_ref_p (arr_ref, nullptr)))
	  || !gfc_is_simply_contiguous (rhs_expr, false, false);
      gfc_conv_expr_descriptor (&rhs_se, rhs_expr);
      gfc_add_block_to_block (&block, &rhs_se.pre);
      opt_rhs_desc = rhs_se.expr;
      if (rhs_expr->ts.type == BT_CHARACTER)
	{
	  opt_rhs_charlen = gfc_build_addr_expr (
	    NULL_TREE, gfc_trans_force_lval (&block, rhs_se.string_length));
	  rhs_size = build_int_cstu (size_type_node, rhs_expr->ts.kind);
	}
      else
	{
	  opt_rhs_charlen
	    = build_zero_cst (build_pointer_type (size_type_node));
	  rhs_size = fold_build2 (
	    MULT_EXPR, size_type_node,
	    fold_convert (size_type_node,
			  rhs_expr->shape
			    ? conv_shape_to_cst (rhs_expr)
			    : gfc_conv_descriptor_size (rhs_se.expr,
							rhs_expr->rank)),
	    fold_convert (size_type_node,
			  gfc_conv_descriptor_span_get (rhs_se.expr)));
	}

      opt_rhs_desc = gfc_build_addr_expr (NULL_TREE, opt_rhs_desc);
    }
  gfc_add_block_to_block (&block, &rhs_se.pre);

  /* Obtain token, offset and image index for the RHS.  */
  rhs_image_index = gfc_caf_get_image_index (&block, rhs_expr, rhs_caf_decl);
  gfc_get_caf_token_offset (&rhs_se, &rhs_token, NULL, rhs_caf_decl, NULL,
			    rhs_expr);

  /* stat and team.  */
  conv_stat_and_team (&block, lhs_expr, &lhs_stat, &lhs_team);
  conv_stat_and_team (&block, rhs_expr, &rhs_stat, &rhs_team);

  sender_fn_index_tree
    = conv_caf_func_index (&block, ns, "__caf_transfer_from_fn_index_%d",
			   rhs_hash);
  rhs_add_data_tree
    = conv_caf_add_call_data (&block, ns,
			      "__caf_transfer_from_remote_add_data_%d",
			      rhs_add_data_sym, &rhs_add_data_size);
  receiver_fn_index_tree
    = conv_caf_func_index (&block, ns, "__caf_transfer_to_remote_fn_index_%d",
			   lhs_hash);
  lhs_add_data_tree
    = conv_caf_add_call_data (&block, ns,
			      "__caf_transfer_to_remote_add_data_%d",
			      lhs_add_data_sym, &lhs_add_data_size);
  ++caf_call_cnt;

  tmp = build_call_expr_loc (
    input_location, gfor_fndecl_caf_transfer_between_remotes, 20, lhs_token,
    opt_lhs_desc, opt_lhs_charlen, lhs_image_index, receiver_fn_index_tree,
    lhs_add_data_tree, lhs_add_data_size, rhs_token, opt_rhs_desc,
    opt_rhs_charlen, rhs_image_index, sender_fn_index_tree, rhs_add_data_tree,
    rhs_add_data_size, rhs_size,
    transfer_rank == 0 ? boolean_true_node : boolean_false_node, lhs_stat,
    lhs_team, null_pointer_node, rhs_stat, rhs_team, null_pointer_node);

  gfc_add_expr_to_block (&block, tmp);
  gfc_add_block_to_block (&block, &lhs_se.post);
  gfc_add_block_to_block (&block, &rhs_se.post);

  /* It guarantees memory consistency within the same segment.  */
  tmp = gfc_build_string_const (strlen ("memory") + 1, "memory");
  tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
		    gfc_build_string_const (1, ""), NULL_TREE, NULL_TREE,
		    tree_cons (NULL_TREE, tmp, NULL_TREE), NULL_TREE);
  ASM_VOLATILE_P (tmp) = 1;
  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}


static void
trans_this_image (gfc_se * se, gfc_expr *expr)
{
  stmtblock_t loop;
  tree type, desc, dim_arg, cond, tmp, m, loop_var, exit_label, min_var,
       lbound, ubound, extent, ml;
  gfc_se argse;
  int rank, corank;
  gfc_expr *distance = expr->value.function.actual->next->next->expr;

  if (expr->value.function.actual->expr
      && !gfc_is_coarray (expr->value.function.actual->expr))
    distance = expr->value.function.actual->expr;

  /* The case -fcoarray=single is handled elsewhere.  */
  gcc_assert (flag_coarray != GFC_FCOARRAY_SINGLE);

  /* Argument-free version: THIS_IMAGE().  */
  if (distance || expr->value.function.actual->expr == NULL)
    {
      if (distance)
	{
	  gfc_init_se (&argse, NULL);
	  gfc_conv_expr_val (&argse, distance);
	  gfc_add_block_to_block (&se->pre, &argse.pre);
	  gfc_add_block_to_block (&se->post, &argse.post);
	  tmp = fold_convert (integer_type_node, argse.expr);
	}
      else
	tmp = integer_zero_node;
      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_this_image, 1,
				 tmp);
      se->expr = fold_convert (gfc_get_int_type (gfc_default_integer_kind),
			       tmp);
      return;
    }

  /* Coarray-argument version: THIS_IMAGE(coarray [, dim]).  */

  type = gfc_get_int_type (gfc_default_integer_kind);
  corank = expr->value.function.actual->expr->corank;
  rank = expr->value.function.actual->expr->rank;

  /* Obtain the descriptor of the COARRAY.  */
  gfc_init_se (&argse, NULL);
  argse.want_coarray = 1;
  gfc_conv_expr_descriptor (&argse, expr->value.function.actual->expr);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);
  desc = argse.expr;

  if (se->ss)
    {
      /* Create an implicit second parameter from the loop variable.  */
      gcc_assert (!expr->value.function.actual->next->expr);
      gcc_assert (corank > 0);
      gcc_assert (se->loop->dimen == 1);
      gcc_assert (se->ss->info->expr == expr);

      dim_arg = se->loop->loopvar[0];
      dim_arg = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, dim_arg,
				 build_int_cst (TREE_TYPE (dim_arg), 1));
      gfc_advance_se_ss_chain (se);
    }
  else
    {
      /* Use the passed DIM= argument.  */
      gcc_assert (expr->value.function.actual->next->expr);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_type (&argse, expr->value.function.actual->next->expr,
			  gfc_array_index_type);
      gfc_add_block_to_block (&se->pre, &argse.pre);
      dim_arg = argse.expr;

      if (INTEGER_CST_P (dim_arg))
	{
	  if (wi::ltu_p (wi::to_wide (dim_arg), 1)
	      || wi::gtu_p (wi::to_wide (dim_arg),
			    GFC_TYPE_ARRAY_CORANK (TREE_TYPE (desc))))
	    gfc_error ("%<dim%> argument of %s intrinsic at %L is not a valid "
		       "dimension index", expr->value.function.isym->name,
		       &expr->where);
	}
     else if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
	{
	  dim_arg = gfc_evaluate_now (dim_arg, &se->pre);
	  cond = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
				  dim_arg,
				  build_int_cst (TREE_TYPE (dim_arg), 1));
	  tmp = gfc_rank_cst[GFC_TYPE_ARRAY_CORANK (TREE_TYPE (desc))];
	  tmp = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
				 dim_arg, tmp);
	  cond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
				  logical_type_node, cond, tmp);
	  gfc_trans_runtime_check (true, false, cond, &se->pre, &expr->where,
			           gfc_msg_fault);
	}
    }

  /* Used algorithm; cf. Fortran 2008, C.10. Note, due to the scalarizer,
     one always has a dim_arg argument.

     m = this_image() - 1
     if (corank == 1)
       {
	 sub(1) = m + lcobound(corank)
	 return;
       }
     i = rank
     min_var = min (rank + corank - 2, rank + dim_arg - 1)
     for (;;)
       {
	 extent = gfc_extent(i)
	 ml = m
	 m  = m/extent
	 if (i >= min_var)
	   goto exit_label
	 i++
       }
     exit_label:
     sub(dim_arg) = (dim_arg < corank) ? ml - m*extent + lcobound(dim_arg)
				       : m + lcobound(corank)
  */

  /* this_image () - 1.  */
  tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_this_image, 1,
			     integer_zero_node);
  tmp = fold_build2_loc (input_location, MINUS_EXPR, type,
			 fold_convert (type, tmp), build_int_cst (type, 1));
  if (corank == 1)
    {
      /* sub(1) = m + lcobound(corank).  */
      lbound = gfc_conv_descriptor_lbound_get (desc,
			build_int_cst (TREE_TYPE (gfc_array_index_type),
				       corank+rank-1));
      lbound = fold_convert (type, lbound);
      tmp = fold_build2_loc (input_location, PLUS_EXPR, type, tmp, lbound);

      se->expr = tmp;
      return;
    }

  m = gfc_create_var (type, NULL);
  ml = gfc_create_var (type, NULL);
  loop_var = gfc_create_var (integer_type_node, NULL);
  min_var = gfc_create_var (integer_type_node, NULL);

  /* m = this_image () - 1.  */
  gfc_add_modify (&se->pre, m, tmp);

  /* min_var = min (rank + corank-2, rank + dim_arg - 1).  */
  tmp = fold_build2_loc (input_location, PLUS_EXPR, integer_type_node,
			 fold_convert (integer_type_node, dim_arg),
			 build_int_cst (integer_type_node, rank - 1));
  tmp = fold_build2_loc (input_location, MIN_EXPR, integer_type_node,
			 build_int_cst (integer_type_node, rank + corank - 2),
			 tmp);
  gfc_add_modify (&se->pre, min_var, tmp);

  /* i = rank.  */
  tmp = build_int_cst (integer_type_node, rank);
  gfc_add_modify (&se->pre, loop_var, tmp);

  exit_label = gfc_build_label_decl (NULL_TREE);
  TREE_USED (exit_label) = 1;

  /* Loop body.  */
  gfc_init_block (&loop);

  /* ml = m.  */
  gfc_add_modify (&loop, ml, m);

  /* extent = ...  */
  lbound = gfc_conv_descriptor_lbound_get (desc, loop_var);
  ubound = gfc_conv_descriptor_ubound_get (desc, loop_var);
  extent = gfc_conv_array_extent_dim (lbound, ubound, NULL);
  extent = fold_convert (type, extent);

  /* m = m/extent.  */
  gfc_add_modify (&loop, m,
		  fold_build2_loc (input_location, TRUNC_DIV_EXPR, type,
			  m, extent));

  /* Exit condition:  if (i >= min_var) goto exit_label.  */
  cond = fold_build2_loc (input_location, GE_EXPR, logical_type_node, loop_var,
		  min_var);
  tmp = build1_v (GOTO_EXPR, exit_label);
  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, cond, tmp,
                         build_empty_stmt (input_location));
  gfc_add_expr_to_block (&loop, tmp);

  /* Increment loop variable: i++.  */
  gfc_add_modify (&loop, loop_var,
                  fold_build2_loc (input_location, PLUS_EXPR, integer_type_node,
				   loop_var,
				   integer_one_node));

  /* Making the loop... actually loop!  */
  tmp = gfc_finish_block (&loop);
  tmp = build1_v (LOOP_EXPR, tmp);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* The exit label.  */
  tmp = build1_v (LABEL_EXPR, exit_label);
  gfc_add_expr_to_block (&se->pre, tmp);

  /*  sub(co_dim) = (co_dim < corank) ? ml - m*extent + lcobound(dim_arg)
				      : m + lcobound(corank) */

  cond = fold_build2_loc (input_location, LT_EXPR, logical_type_node, dim_arg,
			  build_int_cst (TREE_TYPE (dim_arg), corank));

  lbound = gfc_conv_descriptor_lbound_get (desc,
		fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, dim_arg,
				 build_int_cst (TREE_TYPE (dim_arg), rank-1)));
  lbound = fold_convert (type, lbound);

  tmp = fold_build2_loc (input_location, MINUS_EXPR, type, ml,
			 fold_build2_loc (input_location, MULT_EXPR, type,
					  m, extent));
  tmp = fold_build2_loc (input_location, PLUS_EXPR, type, tmp, lbound);

  se->expr = fold_build3_loc (input_location, COND_EXPR, type, cond, tmp,
			      fold_build2_loc (input_location, PLUS_EXPR, type,
					       m, lbound));
}


/* Convert a call to image_status.  */

static void
conv_intrinsic_image_status (gfc_se *se, gfc_expr *expr)
{
  unsigned int num_args;
  tree *args, tmp;

  num_args = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, num_args);
  gfc_conv_intrinsic_function_args (se, expr, args, num_args);
  /* In args[0] the number of the image the status is desired for has to be
     given.  */

  if (flag_coarray == GFC_FCOARRAY_SINGLE)
    {
      tree arg;
      arg = gfc_evaluate_now (args[0], &se->pre);
      tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			     fold_convert (integer_type_node, arg),
			     integer_one_node);
      tmp = fold_build3_loc (input_location, COND_EXPR, integer_type_node,
			     tmp, integer_zero_node,
			     build_int_cst (integer_type_node,
					    GFC_STAT_STOPPED_IMAGE));
    }
  else if (flag_coarray == GFC_FCOARRAY_LIB)
    tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_image_status, 2,
			       args[0], build_int_cst (integer_type_node, -1));
  else
    gcc_unreachable ();

  se->expr = fold_convert (gfc_get_int_type (gfc_default_integer_kind), tmp);
}

static void
conv_intrinsic_team_number (gfc_se *se, gfc_expr *expr)
{
  unsigned int num_args;

  tree *args, tmp;

  num_args = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, num_args);
  gfc_conv_intrinsic_function_args (se, expr, args, num_args);

  if (flag_coarray ==
      GFC_FCOARRAY_SINGLE && expr->value.function.actual->expr)
    {
      tree arg;

      arg = gfc_evaluate_now (args[0], &se->pre);
      tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
      			     fold_convert (integer_type_node, arg),
      			     integer_one_node);
      tmp = fold_build3_loc (input_location, COND_EXPR, integer_type_node,
      			     tmp, integer_zero_node,
      			     build_int_cst (integer_type_node,
      					    GFC_STAT_STOPPED_IMAGE));
    }
  else if (flag_coarray == GFC_FCOARRAY_SINGLE)
    {
      // the value -1 represents that no team has been created yet
      tmp = build_int_cst (integer_type_node, -1);
    }
  else if (flag_coarray == GFC_FCOARRAY_LIB && expr->value.function.actual->expr)
    tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_team_number, 1,
			       args[0], build_int_cst (integer_type_node, -1));
  else if (flag_coarray == GFC_FCOARRAY_LIB)
    tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_team_number, 1,
		integer_zero_node, build_int_cst (integer_type_node, -1));
  else
    gcc_unreachable ();

  se->expr = fold_convert (gfc_get_int_type (gfc_default_integer_kind), tmp);
}


static void
trans_image_index (gfc_se * se, gfc_expr *expr)
{
  tree num_images, cond, coindex, type, lbound, ubound, desc, subdesc,
       tmp, invalid_bound;
  gfc_se argse, subse;
  int rank, corank, codim;

  type = gfc_get_int_type (gfc_default_integer_kind);
  corank = expr->value.function.actual->expr->corank;
  rank = expr->value.function.actual->expr->rank;

  /* Obtain the descriptor of the COARRAY.  */
  gfc_init_se (&argse, NULL);
  argse.want_coarray = 1;
  gfc_conv_expr_descriptor (&argse, expr->value.function.actual->expr);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);
  desc = argse.expr;

  /* Obtain a handle to the SUB argument.  */
  gfc_init_se (&subse, NULL);
  gfc_conv_expr_descriptor (&subse, expr->value.function.actual->next->expr);
  gfc_add_block_to_block (&se->pre, &subse.pre);
  gfc_add_block_to_block (&se->post, &subse.post);
  subdesc = build_fold_indirect_ref_loc (input_location,
			gfc_conv_descriptor_data_get (subse.expr));

  /* Fortran 2008 does not require that the values remain in the cobounds,
     thus we need explicitly check this - and return 0 if they are exceeded.  */

  lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[rank+corank-1]);
  tmp = gfc_build_array_ref (subdesc, gfc_rank_cst[corank-1], NULL);
  invalid_bound = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
				 fold_convert (gfc_array_index_type, tmp),
				 lbound);

  for (codim = corank + rank - 2; codim >= rank; codim--)
    {
      lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[codim]);
      ubound = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[codim]);
      tmp = gfc_build_array_ref (subdesc, gfc_rank_cst[codim-rank], NULL);
      cond = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
			      fold_convert (gfc_array_index_type, tmp),
			      lbound);
      invalid_bound = fold_build2_loc (input_location, TRUTH_OR_EXPR,
				       logical_type_node, invalid_bound, cond);
      cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			      fold_convert (gfc_array_index_type, tmp),
			      ubound);
      invalid_bound = fold_build2_loc (input_location, TRUTH_OR_EXPR,
				       logical_type_node, invalid_bound, cond);
    }

  invalid_bound = gfc_unlikely (invalid_bound, PRED_FORTRAN_INVALID_BOUND);

  /* See Fortran 2008, C.10 for the following algorithm.  */

  /* coindex = sub(corank) - lcobound(n).  */
  coindex = fold_convert (gfc_array_index_type,
			  gfc_build_array_ref (subdesc, gfc_rank_cst[corank-1],
					       NULL));
  lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[rank+corank-1]);
  coindex = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			     fold_convert (gfc_array_index_type, coindex),
			     lbound);

  for (codim = corank + rank - 2; codim >= rank; codim--)
    {
      tree extent, ubound;

      /* coindex = coindex*extent(codim) + sub(codim) - lcobound(codim).  */
      lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[codim]);
      ubound = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[codim]);
      extent = gfc_conv_array_extent_dim (lbound, ubound, NULL);

      /* coindex *= extent.  */
      coindex = fold_build2_loc (input_location, MULT_EXPR,
				 gfc_array_index_type, coindex, extent);

      /* coindex += sub(codim).  */
      tmp = gfc_build_array_ref (subdesc, gfc_rank_cst[codim-rank], NULL);
      coindex = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, coindex,
				 fold_convert (gfc_array_index_type, tmp));

      /* coindex -= lbound(codim).  */
      lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[codim]);
      coindex = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type, coindex, lbound);
    }

  coindex = fold_build2_loc (input_location, PLUS_EXPR, type,
			     fold_convert(type, coindex),
			     build_int_cst (type, 1));

  /* Return 0 if "coindex" exceeds num_images().  */

  if (flag_coarray == GFC_FCOARRAY_SINGLE)
    num_images = build_int_cst (type, 1);
  else
    {
      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_num_images, 2,
				 integer_zero_node,
				 build_int_cst (integer_type_node, -1));
      num_images = fold_convert (type, tmp);
    }

  tmp = gfc_create_var (type, NULL);
  gfc_add_modify (&se->pre, tmp, coindex);

  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node, tmp,
			  num_images);
  cond = fold_build2_loc (input_location, TRUTH_OR_EXPR, logical_type_node,
			  cond,
			  fold_convert (logical_type_node, invalid_bound));
  se->expr = fold_build3_loc (input_location, COND_EXPR, type, cond,
			      build_int_cst (type, 0), tmp);
}

static void
trans_num_images (gfc_se * se, gfc_expr *expr)
{
  tree tmp, distance, failed;
  gfc_se argse;

  if (expr->value.function.actual->expr)
    {
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, expr->value.function.actual->expr);
      gfc_add_block_to_block (&se->pre, &argse.pre);
      gfc_add_block_to_block (&se->post, &argse.post);
      distance = fold_convert (integer_type_node, argse.expr);
    }
  else
    distance = integer_zero_node;

  if (expr->value.function.actual->next->expr)
    {
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, expr->value.function.actual->next->expr);
      gfc_add_block_to_block (&se->pre, &argse.pre);
      gfc_add_block_to_block (&se->post, &argse.post);
      failed = fold_convert (integer_type_node, argse.expr);
    }
  else
    failed = build_int_cst (integer_type_node, -1);
  tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_num_images, 2,
			     distance, failed);
  se->expr = fold_convert (gfc_get_int_type (gfc_default_integer_kind), tmp);
}


static void
gfc_conv_intrinsic_rank (gfc_se *se, gfc_expr *expr)
{
  gfc_se argse;

  gfc_init_se (&argse, NULL);
  argse.data_not_needed = 1;
  argse.descriptor_only = 1;

  gfc_conv_expr_descriptor (&argse, expr->value.function.actual->expr);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);

  se->expr = gfc_conv_descriptor_rank (argse.expr);
  se->expr = fold_convert (gfc_get_int_type (gfc_default_integer_kind),
			   se->expr);
}


static void
gfc_conv_intrinsic_is_contiguous (gfc_se * se, gfc_expr * expr)
{
  gfc_expr *arg;
  arg = expr->value.function.actual->expr;
  gfc_conv_is_contiguous_expr (se, arg);
  se->expr = fold_convert (gfc_typenode_for_spec (&expr->ts), se->expr);
}

/* This function does the work for gfc_conv_intrinsic_is_contiguous,
   plus it can be called directly.  */

void
gfc_conv_is_contiguous_expr (gfc_se *se, gfc_expr *arg)
{
  gfc_ss *ss;
  gfc_se argse;
  tree desc, tmp, stride, extent, cond;
  int i;
  tree fncall0;
  gfc_array_spec *as;

  if (arg->ts.type == BT_CLASS)
    gfc_add_class_array_ref (arg);

  ss = gfc_walk_expr (arg);
  gcc_assert (ss != gfc_ss_terminator);
  gfc_init_se (&argse, NULL);
  argse.data_not_needed = 1;
  gfc_conv_expr_descriptor (&argse, arg);

  as = gfc_get_full_arrayspec_from_expr (arg);

  /* Create:  stride[0] == 1 && stride[1] == extend[0]*stride[0] && ...
     Note in addition that zero-sized arrays don't count as contiguous.  */

  if (as && as->type == AS_ASSUMED_RANK)
    {
      /* Build the call to is_contiguous0.  */
      argse.want_pointer = 1;
      gfc_conv_expr_descriptor (&argse, arg);
      gfc_add_block_to_block (&se->pre, &argse.pre);
      gfc_add_block_to_block (&se->post, &argse.post);
      desc = gfc_evaluate_now (argse.expr, &se->pre);
      fncall0 = build_call_expr_loc (input_location,
				     gfor_fndecl_is_contiguous0, 1, desc);
      se->expr = fncall0;
      se->expr = convert (logical_type_node, se->expr);
    }
  else
    {
      gfc_add_block_to_block (&se->pre, &argse.pre);
      gfc_add_block_to_block (&se->post, &argse.post);
      desc = gfc_evaluate_now (argse.expr, &se->pre);

      stride = gfc_conv_descriptor_stride_get (desc, gfc_rank_cst[0]);
      cond = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
			      stride, build_int_cst (TREE_TYPE (stride), 1));

      for (i = 0; i < arg->rank - 1; i++)
	{
	  tmp = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[i]);
	  extent = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[i]);
	  extent = fold_build2_loc (input_location, MINUS_EXPR,
				    gfc_array_index_type, extent, tmp);
	  extent = fold_build2_loc (input_location, PLUS_EXPR,
				    gfc_array_index_type, extent,
				    gfc_index_one_node);
	  tmp = gfc_conv_descriptor_stride_get (desc, gfc_rank_cst[i]);
	  tmp = fold_build2_loc (input_location, MULT_EXPR, TREE_TYPE (tmp),
				 tmp, extent);
	  stride = gfc_conv_descriptor_stride_get (desc, gfc_rank_cst[i+1]);
	  tmp = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
				 stride, tmp);
	  cond = fold_build2_loc (input_location, TRUTH_AND_EXPR,
				  boolean_type_node, cond, tmp);
	}
      se->expr = cond;
    }
}


/* Evaluate a single upper or lower bound.  */
/* TODO: bound intrinsic generates way too much unnecessary code.  */

static void
gfc_conv_intrinsic_bound (gfc_se * se, gfc_expr * expr, enum gfc_isym_id op)
{
  gfc_actual_arglist *arg;
  gfc_actual_arglist *arg2;
  tree desc;
  tree type;
  tree bound;
  tree tmp;
  tree cond, cond1;
  tree ubound;
  tree lbound;
  tree size;
  gfc_se argse;
  gfc_array_spec * as;
  bool assumed_rank_lb_one;

  arg = expr->value.function.actual;
  arg2 = arg->next;

  if (se->ss)
    {
      /* Create an implicit second parameter from the loop variable.  */
      gcc_assert (!arg2->expr || op == GFC_ISYM_SHAPE);
      gcc_assert (se->loop->dimen == 1);
      gcc_assert (se->ss->info->expr == expr);
      gfc_advance_se_ss_chain (se);
      bound = se->loop->loopvar[0];
      bound = fold_build2_loc (input_location, MINUS_EXPR,
			       gfc_array_index_type, bound,
			       se->loop->from[0]);
    }
  else
    {
      /* use the passed argument.  */
      gcc_assert (arg2->expr);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_type (&argse, arg2->expr, gfc_array_index_type);
      gfc_add_block_to_block (&se->pre, &argse.pre);
      bound = argse.expr;
      /* Convert from one based to zero based.  */
      bound = fold_build2_loc (input_location, MINUS_EXPR,
			       gfc_array_index_type, bound,
			       gfc_index_one_node);
    }

  /* TODO: don't re-evaluate the descriptor on each iteration.  */
  /* Get a descriptor for the first parameter.  */
  gfc_init_se (&argse, NULL);
  gfc_conv_expr_descriptor (&argse, arg->expr);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);

  desc = argse.expr;

  as = gfc_get_full_arrayspec_from_expr (arg->expr);

  if (INTEGER_CST_P (bound))
    {
      gcc_assert (op != GFC_ISYM_SHAPE);
      if (((!as || as->type != AS_ASSUMED_RANK)
	   && wi::geu_p (wi::to_wide (bound),
			 GFC_TYPE_ARRAY_RANK (TREE_TYPE (desc))))
	  || wi::gtu_p (wi::to_wide (bound), GFC_MAX_DIMENSIONS))
	gfc_error ("%<dim%> argument of %s intrinsic at %L is not a valid "
		   "dimension index",
		   (op == GFC_ISYM_UBOUND) ? "UBOUND" : "LBOUND",
		   &expr->where);
    }

  if (!INTEGER_CST_P (bound) || (as && as->type == AS_ASSUMED_RANK))
    {
      if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
        {
          bound = gfc_evaluate_now (bound, &se->pre);
          cond = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
				  bound, build_int_cst (TREE_TYPE (bound), 0));
	  if (as && as->type == AS_ASSUMED_RANK)
	    tmp = gfc_conv_descriptor_rank (desc);
	  else
	    tmp = gfc_rank_cst[GFC_TYPE_ARRAY_RANK (TREE_TYPE (desc))];
          tmp = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
				 bound, fold_convert(TREE_TYPE (bound), tmp));
          cond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
				  logical_type_node, cond, tmp);
          gfc_trans_runtime_check (true, false, cond, &se->pre, &expr->where,
				   gfc_msg_fault);
        }
    }

  /* Take care of the lbound shift for assumed-rank arrays that are
     nonallocatable and nonpointers. Those have a lbound of 1.  */
  assumed_rank_lb_one = as && as->type == AS_ASSUMED_RANK
			&& ((arg->expr->ts.type != BT_CLASS
			     && !arg->expr->symtree->n.sym->attr.allocatable
			     && !arg->expr->symtree->n.sym->attr.pointer)
			    || (arg->expr->ts.type == BT_CLASS
			     && !CLASS_DATA (arg->expr)->attr.allocatable
			     && !CLASS_DATA (arg->expr)->attr.class_pointer));

  ubound = gfc_conv_descriptor_ubound_get (desc, bound);
  lbound = gfc_conv_descriptor_lbound_get (desc, bound);
  size = fold_build2_loc (input_location, MINUS_EXPR,
			  gfc_array_index_type, ubound, lbound);
  size = fold_build2_loc (input_location, PLUS_EXPR,
			  gfc_array_index_type, size, gfc_index_one_node);

  /* 13.14.53: Result value for LBOUND

     Case (i): For an array section or for an array expression other than a
               whole array or array structure component, LBOUND(ARRAY, DIM)
               has the value 1.  For a whole array or array structure
               component, LBOUND(ARRAY, DIM) has the value:
                 (a) equal to the lower bound for subscript DIM of ARRAY if
                     dimension DIM of ARRAY does not have extent zero
                     or if ARRAY is an assumed-size array of rank DIM,
              or (b) 1 otherwise.

     13.14.113: Result value for UBOUND

     Case (i): For an array section or for an array expression other than a
               whole array or array structure component, UBOUND(ARRAY, DIM)
               has the value equal to the number of elements in the given
               dimension; otherwise, it has a value equal to the upper bound
               for subscript DIM of ARRAY if dimension DIM of ARRAY does
               not have size zero and has value zero if dimension DIM has
               size zero.  */

  if (op == GFC_ISYM_LBOUND && assumed_rank_lb_one)
    se->expr = gfc_index_one_node;
  else if (as)
    {
      if (op == GFC_ISYM_UBOUND)
	{
	  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
				  size, gfc_index_zero_node);
	  se->expr = fold_build3_loc (input_location, COND_EXPR,
				      gfc_array_index_type, cond,
				      (assumed_rank_lb_one ? size : ubound),
				      gfc_index_zero_node);
	}
      else if (op == GFC_ISYM_LBOUND)
	{
	  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
				  size, gfc_index_zero_node);
	  if (as->type == AS_ASSUMED_SIZE)
	    {
	      cond1 = fold_build2_loc (input_location, EQ_EXPR,
				       logical_type_node, bound,
				       build_int_cst (TREE_TYPE (bound),
						      arg->expr->rank - 1));
	      cond = fold_build2_loc (input_location, TRUTH_OR_EXPR,
				      logical_type_node, cond, cond1);
	    }
	  se->expr = fold_build3_loc (input_location, COND_EXPR,
				      gfc_array_index_type, cond,
				      lbound, gfc_index_one_node);
	}
      else if (op == GFC_ISYM_SHAPE)
	se->expr = fold_build2_loc (input_location, MAX_EXPR,
				    gfc_array_index_type, size,
				    gfc_index_zero_node);
      else
	gcc_unreachable ();

      /* According to F2018 16.9.172, para 5, an assumed rank object,
	 argument associated with and assumed size array, has the ubound
	 of the final dimension set to -1 and UBOUND must return this.
	 Similarly for the SHAPE intrinsic.  */
      if (op != GFC_ISYM_LBOUND && assumed_rank_lb_one)
	{
	  tree minus_one = build_int_cst (gfc_array_index_type, -1);
	  tree rank = fold_convert (gfc_array_index_type,
				    gfc_conv_descriptor_rank (desc));
	  rank = fold_build2_loc (input_location, PLUS_EXPR,
				  gfc_array_index_type, rank, minus_one);

	  /* Fix the expression to stop it from becoming even more
	     complicated.  */
	  se->expr = gfc_evaluate_now (se->expr, &se->pre);

	  /* Descriptors for assumed-size arrays have ubound = -1
	     in the last dimension.  */
	  cond1 = fold_build2_loc (input_location, EQ_EXPR,
				   logical_type_node, ubound, minus_one);
	  cond = fold_build2_loc (input_location, EQ_EXPR,
				  logical_type_node, bound, rank);
	  cond = fold_build2_loc (input_location, TRUTH_AND_EXPR,
				  logical_type_node, cond, cond1);
	  se->expr = fold_build3_loc (input_location, COND_EXPR,
				      gfc_array_index_type, cond,
				      minus_one, se->expr);
	}
    }
  else   /* as is null; this is an old-fashioned 1-based array.  */
    {
      if (op != GFC_ISYM_LBOUND)
        {
	  se->expr = fold_build2_loc (input_location, MAX_EXPR,
				      gfc_array_index_type, size,
				      gfc_index_zero_node);
	}
      else
	se->expr = gfc_index_one_node;
    }


  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = convert (type, se->expr);
}


static void
conv_intrinsic_cobound (gfc_se * se, gfc_expr * expr)
{
  gfc_actual_arglist *arg;
  gfc_actual_arglist *arg2;
  gfc_se argse;
  tree bound, resbound, resbound2, desc, cond, tmp;
  tree type;
  int corank;

  gcc_assert (expr->value.function.isym->id == GFC_ISYM_LCOBOUND
	      || expr->value.function.isym->id == GFC_ISYM_UCOBOUND
	      || expr->value.function.isym->id == GFC_ISYM_THIS_IMAGE);

  arg = expr->value.function.actual;
  arg2 = arg->next;

  gcc_assert (arg->expr->expr_type == EXPR_VARIABLE);
  corank = arg->expr->corank;

  gfc_init_se (&argse, NULL);
  argse.want_coarray = 1;

  gfc_conv_expr_descriptor (&argse, arg->expr);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);
  desc = argse.expr;

  if (se->ss)
    {
      /* Create an implicit second parameter from the loop variable.  */
      gcc_assert (!arg2->expr);
      gcc_assert (corank > 0);
      gcc_assert (se->loop->dimen == 1);
      gcc_assert (se->ss->info->expr == expr);

      bound = se->loop->loopvar[0];
      bound = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			       bound, gfc_rank_cst[arg->expr->rank]);
      gfc_advance_se_ss_chain (se);
    }
  else
    {
      /* use the passed argument.  */
      gcc_assert (arg2->expr);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_type (&argse, arg2->expr, gfc_array_index_type);
      gfc_add_block_to_block (&se->pre, &argse.pre);
      bound = argse.expr;

      if (INTEGER_CST_P (bound))
	{
	  if (wi::ltu_p (wi::to_wide (bound), 1)
	      || wi::gtu_p (wi::to_wide (bound),
			    GFC_TYPE_ARRAY_CORANK (TREE_TYPE (desc))))
	    gfc_error ("%<dim%> argument of %s intrinsic at %L is not a valid "
		       "dimension index", expr->value.function.isym->name,
		       &expr->where);
	}
      else if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
        {
	  bound = gfc_evaluate_now (bound, &se->pre);
	  cond = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
				  bound, build_int_cst (TREE_TYPE (bound), 1));
	  tmp = gfc_rank_cst[GFC_TYPE_ARRAY_CORANK (TREE_TYPE (desc))];
	  tmp = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
				 bound, tmp);
	  cond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
				  logical_type_node, cond, tmp);
	  gfc_trans_runtime_check (true, false, cond, &se->pre, &expr->where,
				   gfc_msg_fault);
	}


      /* Subtract 1 to get to zero based and add dimensions.  */
      switch (arg->expr->rank)
	{
	case 0:
	  bound = fold_build2_loc (input_location, MINUS_EXPR,
				   gfc_array_index_type, bound,
				   gfc_index_one_node);
	case 1:
	  break;
	default:
	  bound = fold_build2_loc (input_location, PLUS_EXPR,
				   gfc_array_index_type, bound,
				   gfc_rank_cst[arg->expr->rank - 1]);
	}
    }

  resbound = gfc_conv_descriptor_lbound_get (desc, bound);

  /* Handle UCOBOUND with special handling of the last codimension.  */
  if (expr->value.function.isym->id == GFC_ISYM_UCOBOUND)
    {
      /* Last codimension: For -fcoarray=single just return
	 the lcobound - otherwise add
	   ceiling (real (num_images ()) / real (size)) - 1
	 = (num_images () + size - 1) / size - 1
	 = (num_images - 1) / size(),
         where size is the product of the extent of all but the last
	 codimension.  */

      if (flag_coarray != GFC_FCOARRAY_SINGLE && corank > 1)
	{
          tree cosize;

	  cosize = gfc_conv_descriptor_cosize (desc, arg->expr->rank, corank);
	  tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_num_images,
				     2, integer_zero_node,
				     build_int_cst (integer_type_node, -1));
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type,
				 fold_convert (gfc_array_index_type, tmp),
				 build_int_cst (gfc_array_index_type, 1));
	  tmp = fold_build2_loc (input_location, TRUNC_DIV_EXPR,
				 gfc_array_index_type, tmp,
				 fold_convert (gfc_array_index_type, cosize));
	  resbound = fold_build2_loc (input_location, PLUS_EXPR,
				      gfc_array_index_type, resbound, tmp);
	}
      else if (flag_coarray != GFC_FCOARRAY_SINGLE)
	{
	  /* ubound = lbound + num_images() - 1.  */
	  tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_num_images,
				     2, integer_zero_node,
				     build_int_cst (integer_type_node, -1));
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type,
				 fold_convert (gfc_array_index_type, tmp),
				 build_int_cst (gfc_array_index_type, 1));
	  resbound = fold_build2_loc (input_location, PLUS_EXPR,
				      gfc_array_index_type, resbound, tmp);
	}

      if (corank > 1)
	{
	  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
				  bound,
				  build_int_cst (TREE_TYPE (bound),
						 arg->expr->rank + corank - 1));

	  resbound2 = gfc_conv_descriptor_ubound_get (desc, bound);
	  se->expr = fold_build3_loc (input_location, COND_EXPR,
				      gfc_array_index_type, cond,
				      resbound, resbound2);
	}
      else
	se->expr = resbound;
    }
  else
    se->expr = resbound;

  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = convert (type, se->expr);
}


static void
conv_intrinsic_stride (gfc_se * se, gfc_expr * expr)
{
  gfc_actual_arglist *array_arg;
  gfc_actual_arglist *dim_arg;
  gfc_se argse;
  tree desc, tmp;

  array_arg = expr->value.function.actual;
  dim_arg = array_arg->next;

  gcc_assert (array_arg->expr->expr_type == EXPR_VARIABLE);

  gfc_init_se (&argse, NULL);
  gfc_conv_expr_descriptor (&argse, array_arg->expr);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);
  desc = argse.expr;

  gcc_assert (dim_arg->expr);
  gfc_init_se (&argse, NULL);
  gfc_conv_expr_type (&argse, dim_arg->expr, gfc_array_index_type);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			 argse.expr, gfc_index_one_node);
  se->expr = gfc_conv_descriptor_stride_get (desc, tmp);
}

static void
gfc_conv_intrinsic_abs (gfc_se * se, gfc_expr * expr)
{
  tree arg, cabs;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);

  switch (expr->value.function.actual->expr->ts.type)
    {
    case BT_INTEGER:
    case BT_REAL:
      se->expr = fold_build1_loc (input_location, ABS_EXPR, TREE_TYPE (arg),
				  arg);
      break;

    case BT_COMPLEX:
      cabs = gfc_builtin_decl_for_float_kind (BUILT_IN_CABS, expr->ts.kind);
      se->expr = build_call_expr_loc (input_location, cabs, 1, arg);
      break;

    default:
      gcc_unreachable ();
    }
}


/* Create a complex value from one or two real components.  */

static void
gfc_conv_intrinsic_cmplx (gfc_se * se, gfc_expr * expr, int both)
{
  tree real;
  tree imag;
  tree type;
  tree *args;
  unsigned int num_args;

  num_args = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, num_args);

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, args, num_args);
  real = convert (TREE_TYPE (type), args[0]);
  if (both)
    imag = convert (TREE_TYPE (type), args[1]);
  else if (TREE_CODE (TREE_TYPE (args[0])) == COMPLEX_TYPE)
    {
      imag = fold_build1_loc (input_location, IMAGPART_EXPR,
			      TREE_TYPE (TREE_TYPE (args[0])), args[0]);
      imag = convert (TREE_TYPE (type), imag);
    }
  else
    imag = build_real_from_int_cst (TREE_TYPE (type), integer_zero_node);

  se->expr = fold_build2_loc (input_location, COMPLEX_EXPR, type, real, imag);
}


/* Remainder function MOD(A, P) = A - INT(A / P) * P
                      MODULO(A, P) = A - FLOOR (A / P) * P

   The obvious algorithms above are numerically instable for large
   arguments, hence these intrinsics are instead implemented via calls
   to the fmod family of functions.  It is the responsibility of the
   user to ensure that the second argument is non-zero.  */

static void
gfc_conv_intrinsic_mod (gfc_se * se, gfc_expr * expr, int modulo)
{
  tree type;
  tree tmp;
  tree test;
  tree test2;
  tree fmod;
  tree zero;
  tree args[2];

  gfc_conv_intrinsic_function_args (se, expr, args, 2);

  switch (expr->ts.type)
    {
    case BT_INTEGER:
      /* Integer case is easy, we've got a builtin op.  */
      type = TREE_TYPE (args[0]);

      if (modulo)
       se->expr = fold_build2_loc (input_location, FLOOR_MOD_EXPR, type,
				   args[0], args[1]);
      else
       se->expr = fold_build2_loc (input_location, TRUNC_MOD_EXPR, type,
				   args[0], args[1]);
      break;

    case BT_UNSIGNED:
      /* Even easier, we only need one.  */
      type = TREE_TYPE (args[0]);
      se->expr = fold_build2_loc (input_location, TRUNC_MOD_EXPR, type,
				  args[0], args[1]);
      break;

    case BT_REAL:
      fmod = NULL_TREE;
      /* Check if we have a builtin fmod.  */
      fmod = gfc_builtin_decl_for_float_kind (BUILT_IN_FMOD, expr->ts.kind);

      /* The builtin should always be available.  */
      gcc_assert (fmod != NULL_TREE);

      tmp = build_addr (fmod);
      se->expr = build_call_array_loc (input_location,
				       TREE_TYPE (TREE_TYPE (fmod)),
                                       tmp, 2, args);
      if (modulo == 0)
	return;

      type = TREE_TYPE (args[0]);

      args[0] = gfc_evaluate_now (args[0], &se->pre);
      args[1] = gfc_evaluate_now (args[1], &se->pre);

      /* Definition:
	 modulo = arg - floor (arg/arg2) * arg2

	 In order to calculate the result accurately, we use the fmod
	 function as follows.

	 res = fmod (arg, arg2);
	 if (res)
	   {
	     if ((arg < 0) xor (arg2 < 0))
	       res += arg2;
	   }
	 else
	   res = copysign (0., arg2);

	 => As two nested ternary exprs:

	 res = res ? (((arg < 0) xor (arg2 < 0)) ? res + arg2 : res)
	       : copysign (0., arg2);

      */

      zero = gfc_build_const (type, integer_zero_node);
      tmp = gfc_evaluate_now (se->expr, &se->pre);
      if (!flag_signed_zeros)
	{
	  test = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
				  args[0], zero);
	  test2 = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
				   args[1], zero);
	  test2 = fold_build2_loc (input_location, TRUTH_XOR_EXPR,
				   logical_type_node, test, test2);
	  test = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				  tmp, zero);
	  test = fold_build2_loc (input_location, TRUTH_AND_EXPR,
				  logical_type_node, test, test2);
	  test = gfc_evaluate_now (test, &se->pre);
	  se->expr = fold_build3_loc (input_location, COND_EXPR, type, test,
				      fold_build2_loc (input_location,
						       PLUS_EXPR,
						       type, tmp, args[1]),
				      tmp);
	}
      else
	{
	  tree expr1, copysign, cscall;
	  copysign = gfc_builtin_decl_for_float_kind (BUILT_IN_COPYSIGN,
						      expr->ts.kind);
	  test = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
				  args[0], zero);
	  test2 = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
				   args[1], zero);
	  test2 = fold_build2_loc (input_location, TRUTH_XOR_EXPR,
				   logical_type_node, test, test2);
	  expr1 = fold_build3_loc (input_location, COND_EXPR, type, test2,
				   fold_build2_loc (input_location,
						    PLUS_EXPR,
						    type, tmp, args[1]),
				   tmp);
	  test = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				  tmp, zero);
	  cscall = build_call_expr_loc (input_location, copysign, 2, zero,
					args[1]);
	  se->expr = fold_build3_loc (input_location, COND_EXPR, type, test,
				      expr1, cscall);
	}
      return;

    default:
      gcc_unreachable ();
    }
}

/* DSHIFTL(I,J,S) = (I << S) | (J >> (BITSIZE(J) - S))
   DSHIFTR(I,J,S) = (I << (BITSIZE(I) - S)) | (J >> S)
   where the right shifts are logical (i.e. 0's are shifted in).
   Because SHIFT_EXPR's want shifts strictly smaller than the integral
   type width, we have to special-case both S == 0 and S == BITSIZE(J):
     DSHIFTL(I,J,0) = I
     DSHIFTL(I,J,BITSIZE) = J
     DSHIFTR(I,J,0) = J
     DSHIFTR(I,J,BITSIZE) = I.  */

static void
gfc_conv_intrinsic_dshift (gfc_se * se, gfc_expr * expr, bool dshiftl)
{
  tree type, utype, stype, arg1, arg2, shift, res, left, right;
  tree args[3], cond, tmp;
  int bitsize;

  gfc_conv_intrinsic_function_args (se, expr, args, 3);

  gcc_assert (TREE_TYPE (args[0]) == TREE_TYPE (args[1]));
  type = TREE_TYPE (args[0]);
  bitsize = TYPE_PRECISION (type);
  utype = unsigned_type_for (type);
  stype = TREE_TYPE (args[2]);

  arg1 = gfc_evaluate_now (args[0], &se->pre);
  arg2 = gfc_evaluate_now (args[1], &se->pre);
  shift = gfc_evaluate_now (args[2], &se->pre);

  /* The generic case.  */
  tmp = fold_build2_loc (input_location, MINUS_EXPR, stype,
			 build_int_cst (stype, bitsize), shift);
  left = fold_build2_loc (input_location, LSHIFT_EXPR, type,
			  arg1, dshiftl ? shift : tmp);

  right = fold_build2_loc (input_location, RSHIFT_EXPR, utype,
			   fold_convert (utype, arg2), dshiftl ? tmp : shift);
  right = fold_convert (type, right);

  res = fold_build2_loc (input_location, BIT_IOR_EXPR, type, left, right);

  /* Special cases.  */
  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, shift,
			  build_int_cst (stype, 0));
  res = fold_build3_loc (input_location, COND_EXPR, type, cond,
			 dshiftl ? arg1 : arg2, res);

  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, shift,
			  build_int_cst (stype, bitsize));
  res = fold_build3_loc (input_location, COND_EXPR, type, cond,
			 dshiftl ? arg2 : arg1, res);

  se->expr = res;
}


/* Positive difference DIM (x, y) = ((x - y) < 0) ? 0 : x - y.  */

static void
gfc_conv_intrinsic_dim (gfc_se * se, gfc_expr * expr)
{
  tree val;
  tree tmp;
  tree type;
  tree zero;
  tree args[2];

  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  type = TREE_TYPE (args[0]);

  val = fold_build2_loc (input_location, MINUS_EXPR, type, args[0], args[1]);
  val = gfc_evaluate_now (val, &se->pre);

  zero = gfc_build_const (type, integer_zero_node);
  tmp = fold_build2_loc (input_location, LE_EXPR, logical_type_node, val, zero);
  se->expr = fold_build3_loc (input_location, COND_EXPR, type, tmp, zero, val);
}


/* SIGN(A, B) is absolute value of A times sign of B.
   The real value versions use library functions to ensure the correct
   handling of negative zero.  Integer case implemented as:
   SIGN(A, B) = { tmp = (A ^ B) >> C; (A + tmp) ^ tmp }
  */

static void
gfc_conv_intrinsic_sign (gfc_se * se, gfc_expr * expr)
{
  tree tmp;
  tree type;
  tree args[2];

  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  if (expr->ts.type == BT_REAL)
    {
      tree abs;

      tmp = gfc_builtin_decl_for_float_kind (BUILT_IN_COPYSIGN, expr->ts.kind);
      abs = gfc_builtin_decl_for_float_kind (BUILT_IN_FABS, expr->ts.kind);

      /* We explicitly have to ignore the minus sign. We do so by using
	 result = (arg1 == 0) ? abs(arg0) : copysign(arg0, arg1).  */
      if (!flag_sign_zero
	  && MODE_HAS_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (args[1]))))
	{
	  tree cond, zero;
	  zero = build_real_from_int_cst (TREE_TYPE (args[1]), integer_zero_node);
	  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
				  args[1], zero);
	  se->expr = fold_build3_loc (input_location, COND_EXPR,
				  TREE_TYPE (args[0]), cond,
				  build_call_expr_loc (input_location, abs, 1,
						       args[0]),
				  build_call_expr_loc (input_location, tmp, 2,
						       args[0], args[1]));
	}
      else
        se->expr = build_call_expr_loc (input_location, tmp, 2,
					args[0], args[1]);
      return;
    }

  /* Having excluded floating point types, we know we are now dealing
     with signed integer types.  */
  type = TREE_TYPE (args[0]);

  /* Args[0] is used multiple times below.  */
  args[0] = gfc_evaluate_now (args[0], &se->pre);

  /* Construct (A ^ B) >> 31, which generates a bit mask of all zeros if
     the signs of A and B are the same, and of all ones if they differ.  */
  tmp = fold_build2_loc (input_location, BIT_XOR_EXPR, type, args[0], args[1]);
  tmp = fold_build2_loc (input_location, RSHIFT_EXPR, type, tmp,
			 build_int_cst (type, TYPE_PRECISION (type) - 1));
  tmp = gfc_evaluate_now (tmp, &se->pre);

  /* Construct (A + tmp) ^ tmp, which is A if tmp is zero, and -A if tmp]
     is all ones (i.e. -1).  */
  se->expr = fold_build2_loc (input_location, BIT_XOR_EXPR, type,
			      fold_build2_loc (input_location, PLUS_EXPR,
					       type, args[0], tmp), tmp);
}


/* Test for the presence of an optional argument.  */

static void
gfc_conv_intrinsic_present (gfc_se * se, gfc_expr * expr)
{
  gfc_expr *arg;

  arg = expr->value.function.actual->expr;
  gcc_assert (arg->expr_type == EXPR_VARIABLE);
  se->expr = gfc_conv_expr_present (arg->symtree->n.sym);
  se->expr = convert (gfc_typenode_for_spec (&expr->ts), se->expr);
}


/* Calculate the double precision product of two single precision values.  */

static void
gfc_conv_intrinsic_dprod (gfc_se * se, gfc_expr * expr)
{
  tree type;
  tree args[2];

  gfc_conv_intrinsic_function_args (se, expr, args, 2);

  /* Convert the args to double precision before multiplying.  */
  type = gfc_typenode_for_spec (&expr->ts);
  args[0] = convert (type, args[0]);
  args[1] = convert (type, args[1]);
  se->expr = fold_build2_loc (input_location, MULT_EXPR, type, args[0],
			      args[1]);
}


/* Return a length one character string containing an ascii character.  */

static void
gfc_conv_intrinsic_char (gfc_se * se, gfc_expr * expr)
{
  tree arg[2];
  tree var;
  tree type;
  unsigned int num_args;

  num_args = gfc_intrinsic_argument_list_length (expr);
  gfc_conv_intrinsic_function_args (se, expr, arg, num_args);

  type = gfc_get_char_type (expr->ts.kind);
  var = gfc_create_var (type, "char");

  arg[0] = fold_build1_loc (input_location, NOP_EXPR, type, arg[0]);
  gfc_add_modify (&se->pre, var, arg[0]);
  se->expr = gfc_build_addr_expr (build_pointer_type (type), var);
  se->string_length = build_int_cst (gfc_charlen_type_node, 1);
}


static void
gfc_conv_intrinsic_ctime (gfc_se * se, gfc_expr * expr)
{
  tree var;
  tree len;
  tree tmp;
  tree cond;
  tree fndecl;
  tree *args;
  unsigned int num_args;

  num_args = gfc_intrinsic_argument_list_length (expr) + 2;
  args = XALLOCAVEC (tree, num_args);

  var = gfc_create_var (pchar_type_node, "pstr");
  len = gfc_create_var (gfc_charlen_type_node, "len");

  gfc_conv_intrinsic_function_args (se, expr, &args[2], num_args - 2);
  args[0] = gfc_build_addr_expr (NULL_TREE, var);
  args[1] = gfc_build_addr_expr (NULL_TREE, len);

  fndecl = build_addr (gfor_fndecl_ctime);
  tmp = build_call_array_loc (input_location,
			  TREE_TYPE (TREE_TYPE (gfor_fndecl_ctime)),
			  fndecl, num_args, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			  len, build_int_cst (TREE_TYPE (len), 0));
  tmp = gfc_call_free (var);
  tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&se->post, tmp);

  se->expr = var;
  se->string_length = len;
}


static void
gfc_conv_intrinsic_fdate (gfc_se * se, gfc_expr * expr)
{
  tree var;
  tree len;
  tree tmp;
  tree cond;
  tree fndecl;
  tree *args;
  unsigned int num_args;

  num_args = gfc_intrinsic_argument_list_length (expr) + 2;
  args = XALLOCAVEC (tree, num_args);

  var = gfc_create_var (pchar_type_node, "pstr");
  len = gfc_create_var (gfc_charlen_type_node, "len");

  gfc_conv_intrinsic_function_args (se, expr, &args[2], num_args - 2);
  args[0] = gfc_build_addr_expr (NULL_TREE, var);
  args[1] = gfc_build_addr_expr (NULL_TREE, len);

  fndecl = build_addr (gfor_fndecl_fdate);
  tmp = build_call_array_loc (input_location,
			  TREE_TYPE (TREE_TYPE (gfor_fndecl_fdate)),
			  fndecl, num_args, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			  len, build_int_cst (TREE_TYPE (len), 0));
  tmp = gfc_call_free (var);
  tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&se->post, tmp);

  se->expr = var;
  se->string_length = len;
}


/* Generate a direct call to free() for the FREE subroutine.  */

static tree
conv_intrinsic_free (gfc_code *code)
{
  stmtblock_t block;
  gfc_se argse;
  tree arg, call;

  gfc_init_se (&argse, NULL);
  gfc_conv_expr (&argse, code->ext.actual->expr);
  arg = fold_convert (ptr_type_node, argse.expr);

  gfc_init_block (&block);
  call = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_FREE), 1, arg);
  gfc_add_expr_to_block (&block, call);
  return gfc_finish_block (&block);
}


/* Call the RANDOM_INIT library subroutine with a hidden argument for
   handling seeding on coarray images.  */

static tree
conv_intrinsic_random_init (gfc_code *code)
{
  stmtblock_t block;
  gfc_se se;
  tree arg1, arg2, tmp;
  /* On none coarray == lib compiles use LOGICAL(4) else regular LOGICAL.  */
  tree used_bool_type_node = flag_coarray == GFC_FCOARRAY_LIB
			     ? logical_type_node
			     : gfc_get_logical_type (4);

  /* Make the function call.  */
  gfc_init_block (&block);
  gfc_init_se (&se, NULL);

  /* Convert REPEATABLE to the desired LOGICAL entity.  */
  gfc_conv_expr (&se, code->ext.actual->expr);
  gfc_add_block_to_block (&block, &se.pre);
  arg1 = fold_convert (used_bool_type_node, gfc_evaluate_now (se.expr, &block));
  gfc_add_block_to_block (&block, &se.post);

  /* Convert IMAGE_DISTINCT to the desired LOGICAL entity.  */
  gfc_conv_expr (&se, code->ext.actual->next->expr);
  gfc_add_block_to_block (&block, &se.pre);
  arg2 = fold_convert (used_bool_type_node, gfc_evaluate_now (se.expr, &block));
  gfc_add_block_to_block (&block, &se.post);

  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_random_init,
				 2, arg1, arg2);
    }
  else
    {
      /* The ABI for libgfortran needs to be maintained, so a hidden
	 argument must be include if code is compiled with -fcoarray=single
	 or without the option.  Set to 0.  */
      tree arg3 = build_int_cst (gfc_get_int_type (4), 0);
      tmp = build_call_expr_loc (input_location, gfor_fndecl_random_init,
				 3, arg1, arg2, arg3);
    }

  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}


/* Call the SYSTEM_CLOCK library functions, handling the type and kind
   conversions.  */

static tree
conv_intrinsic_system_clock (gfc_code *code)
{
  stmtblock_t block;
  gfc_se count_se, count_rate_se, count_max_se;
  tree arg1 = NULL_TREE, arg2 = NULL_TREE, arg3 = NULL_TREE;
  tree tmp;
  int least;

  gfc_expr *count = code->ext.actual->expr;
  gfc_expr *count_rate = code->ext.actual->next->expr;
  gfc_expr *count_max = code->ext.actual->next->next->expr;

  /* Evaluate our arguments.  */
  if (count)
    {
      gfc_init_se (&count_se, NULL);
      gfc_conv_expr (&count_se, count);
    }

  if (count_rate)
    {
      gfc_init_se (&count_rate_se, NULL);
      gfc_conv_expr (&count_rate_se, count_rate);
    }

  if (count_max)
    {
      gfc_init_se (&count_max_se, NULL);
      gfc_conv_expr (&count_max_se, count_max);
    }

  /* Find the smallest kind found of the arguments.  */
  least = 16;
  least = (count && count->ts.kind < least) ? count->ts.kind : least;
  least = (count_rate && count_rate->ts.kind < least) ? count_rate->ts.kind
						      : least;
  least = (count_max && count_max->ts.kind < least) ? count_max->ts.kind
						    : least;

  /* Prepare temporary variables.  */

  if (count)
    {
      if (least >= 8)
	arg1 = gfc_create_var (gfc_get_int_type (8), "count");
      else if (least == 4)
	arg1 = gfc_create_var (gfc_get_int_type (4), "count");
      else if (count->ts.kind == 1)
        arg1 = gfc_conv_mpz_to_tree (gfc_integer_kinds[0].pedantic_min_int,
				     count->ts.kind);
      else
        arg1 = gfc_conv_mpz_to_tree (gfc_integer_kinds[1].pedantic_min_int,
				     count->ts.kind);
    }

  if (count_rate)
    {
      if (least >= 8)
	arg2 = gfc_create_var (gfc_get_int_type (8), "count_rate");
      else if (least == 4)
	arg2 = gfc_create_var (gfc_get_int_type (4), "count_rate");
      else
        arg2 = integer_zero_node;
    }

  if (count_max)
    {
      if (least >= 8)
	arg3 = gfc_create_var (gfc_get_int_type (8), "count_max");
      else if (least == 4)
	arg3 = gfc_create_var (gfc_get_int_type (4), "count_max");
      else
        arg3 = integer_zero_node;
    }

  /* Make the function call.  */
  gfc_init_block (&block);

if (least <= 2)
  {
    if (least == 1)
      {
	arg1 ? gfc_build_addr_expr (NULL_TREE, arg1)
	       : null_pointer_node;
	arg2 ? gfc_build_addr_expr (NULL_TREE, arg2)
	       : null_pointer_node;
	arg3 ? gfc_build_addr_expr (NULL_TREE, arg3)
	       : null_pointer_node;
      }

    if (least == 2)
      {
	arg1 ? gfc_build_addr_expr (NULL_TREE, arg1)
	       : null_pointer_node;
	arg2 ? gfc_build_addr_expr (NULL_TREE, arg2)
	       : null_pointer_node;
	arg3 ? gfc_build_addr_expr (NULL_TREE, arg3)
	       : null_pointer_node;
      }
  }
else
  {
    if (least == 4)
      {
	tmp = build_call_expr_loc (input_location,
		gfor_fndecl_system_clock4, 3,
		arg1 ? gfc_build_addr_expr (NULL_TREE, arg1)
		       : null_pointer_node,
		arg2 ? gfc_build_addr_expr (NULL_TREE, arg2)
		       : null_pointer_node,
		arg3 ? gfc_build_addr_expr (NULL_TREE, arg3)
		       : null_pointer_node);
	gfc_add_expr_to_block (&block, tmp);
      }
    /* Handle kind>=8, 10, or 16 arguments */
    if (least >= 8)
      {
	tmp = build_call_expr_loc (input_location,
		gfor_fndecl_system_clock8, 3,
		arg1 ? gfc_build_addr_expr (NULL_TREE, arg1)
		       : null_pointer_node,
		arg2 ? gfc_build_addr_expr (NULL_TREE, arg2)
		       : null_pointer_node,
		arg3 ? gfc_build_addr_expr (NULL_TREE, arg3)
		       : null_pointer_node);
	gfc_add_expr_to_block (&block, tmp);
      }
  }

  /* And store values back if needed.  */
  if (arg1 && arg1 != count_se.expr)
    gfc_add_modify (&block, count_se.expr,
		    fold_convert (TREE_TYPE (count_se.expr), arg1));
  if (arg2 && arg2 != count_rate_se.expr)
    gfc_add_modify (&block, count_rate_se.expr,
		    fold_convert (TREE_TYPE (count_rate_se.expr), arg2));
  if (arg3 && arg3 != count_max_se.expr)
    gfc_add_modify (&block, count_max_se.expr,
		    fold_convert (TREE_TYPE (count_max_se.expr), arg3));

  return gfc_finish_block (&block);
}


/* Return a character string containing the tty name.  */

static void
gfc_conv_intrinsic_ttynam (gfc_se * se, gfc_expr * expr)
{
  tree var;
  tree len;
  tree tmp;
  tree cond;
  tree fndecl;
  tree *args;
  unsigned int num_args;

  num_args = gfc_intrinsic_argument_list_length (expr) + 2;
  args = XALLOCAVEC (tree, num_args);

  var = gfc_create_var (pchar_type_node, "pstr");
  len = gfc_create_var (gfc_charlen_type_node, "len");

  gfc_conv_intrinsic_function_args (se, expr, &args[2], num_args - 2);
  args[0] = gfc_build_addr_expr (NULL_TREE, var);
  args[1] = gfc_build_addr_expr (NULL_TREE, len);

  fndecl = build_addr (gfor_fndecl_ttynam);
  tmp = build_call_array_loc (input_location,
			  TREE_TYPE (TREE_TYPE (gfor_fndecl_ttynam)),
			  fndecl, num_args, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			  len, build_int_cst (TREE_TYPE (len), 0));
  tmp = gfc_call_free (var);
  tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&se->post, tmp);

  se->expr = var;
  se->string_length = len;
}


/* Get the minimum/maximum value of all the parameters.
    minmax (a1, a2, a3, ...)
    {
      mvar = a1;
      mvar = COMP (mvar, a2)
      mvar = COMP (mvar, a3)
      ...
      return mvar;
    }
    Where COMP is MIN/MAX_EXPR for integral types or when we don't
    care about NaNs, or IFN_FMIN/MAX when the target has support for
    fast NaN-honouring min/max.  When neither holds expand a sequence
    of explicit comparisons.  */

/* TODO: Mismatching types can occur when specific names are used.
   These should be handled during resolution.  */
static void
gfc_conv_intrinsic_minmax (gfc_se * se, gfc_expr * expr, enum tree_code op)
{
  tree tmp;
  tree mvar;
  tree val;
  tree *args;
  tree type;
  tree argtype;
  gfc_actual_arglist *argexpr;
  unsigned int i, nargs;

  nargs = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, nargs);

  gfc_conv_intrinsic_function_args (se, expr, args, nargs);
  type = gfc_typenode_for_spec (&expr->ts);

  /* Only evaluate the argument once.  */
  if (!VAR_P (args[0]) && !TREE_CONSTANT (args[0]))
    args[0] = gfc_evaluate_now (args[0], &se->pre);

  /* Determine suitable type of temporary, as a GNU extension allows
     different argument kinds.  */
  argtype = TREE_TYPE (args[0]);
  argexpr = expr->value.function.actual;
  for (i = 1, argexpr = argexpr->next; i < nargs; i++, argexpr = argexpr->next)
    {
      tree tmptype = TREE_TYPE (args[i]);
      if (TYPE_PRECISION (tmptype) > TYPE_PRECISION (argtype))
	argtype = tmptype;
    }
  mvar = gfc_create_var (argtype, "M");
  gfc_add_modify (&se->pre, mvar, convert (argtype, args[0]));

  argexpr = expr->value.function.actual;
  for (i = 1, argexpr = argexpr->next; i < nargs; i++, argexpr = argexpr->next)
    {
      tree cond = NULL_TREE;
      val = args[i];

      /* Handle absent optional arguments by ignoring the comparison.  */
      if (argexpr->expr->expr_type == EXPR_VARIABLE
	  && argexpr->expr->symtree->n.sym->attr.optional
	  && INDIRECT_REF_P (val))
	{
	  cond = fold_build2_loc (input_location,
				NE_EXPR, logical_type_node,
				TREE_OPERAND (val, 0),
			build_int_cst (TREE_TYPE (TREE_OPERAND (val, 0)), 0));
	}
      else if (!VAR_P (val) && !TREE_CONSTANT (val))
	/* Only evaluate the argument once.  */
	val = gfc_evaluate_now (val, &se->pre);

      tree calc;
      /* For floating point types, the question is what MAX(a, NaN) or
	 MIN(a, NaN) should return (where "a" is a normal number).
	 There are valid use case for returning either one, but the
	 Fortran standard doesn't specify which one should be chosen.
	 Also, there is no consensus among other tested compilers.  In
	 short, it's a mess.  So lets just do whatever is fastest.  */
      tree_code code = op == GT_EXPR ? MAX_EXPR : MIN_EXPR;
      calc = fold_build2_loc (input_location, code, argtype,
			      convert (argtype, val), mvar);
      tmp = build2_v (MODIFY_EXPR, mvar, calc);

      if (cond != NULL_TREE)
	tmp = build3_v (COND_EXPR, cond, tmp,
			build_empty_stmt (input_location));
      gfc_add_expr_to_block (&se->pre, tmp);
    }
  se->expr = convert (type, mvar);
}


/* Generate library calls for MIN and MAX intrinsics for character
   variables.  */
static void
gfc_conv_intrinsic_minmax_char (gfc_se * se, gfc_expr * expr, int op)
{
  tree *args;
  tree var, len, fndecl, tmp, cond, function;
  unsigned int nargs;

  nargs = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, nargs + 4);
  gfc_conv_intrinsic_function_args (se, expr, &args[4], nargs);

  /* Create the result variables.  */
  len = gfc_create_var (gfc_charlen_type_node, "len");
  args[0] = gfc_build_addr_expr (NULL_TREE, len);
  var = gfc_create_var (gfc_get_pchar_type (expr->ts.kind), "pstr");
  args[1] = gfc_build_addr_expr (ppvoid_type_node, var);
  args[2] = build_int_cst (integer_type_node, op);
  args[3] = build_int_cst (integer_type_node, nargs / 2);

  if (expr->ts.kind == 1)
    function = gfor_fndecl_string_minmax;
  else if (expr->ts.kind == 4)
    function = gfor_fndecl_string_minmax_char4;
  else
    gcc_unreachable ();

  /* Make the function call.  */
  fndecl = build_addr (function);
  tmp = build_call_array_loc (input_location,
			  TREE_TYPE (TREE_TYPE (function)), fndecl,
			  nargs + 4, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			  len, build_int_cst (TREE_TYPE (len), 0));
  tmp = gfc_call_free (var);
  tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&se->post, tmp);

  se->expr = var;
  se->string_length = len;
}


/* Create a symbol node for this intrinsic.  The symbol from the frontend
   has the generic name.  */

static gfc_symbol *
gfc_get_symbol_for_expr (gfc_expr * expr, bool ignore_optional)
{
  gfc_symbol *sym;

  /* TODO: Add symbols for intrinsic function to the global namespace.  */
  gcc_assert (strlen (expr->value.function.name) <= GFC_MAX_SYMBOL_LEN - 5);
  sym = gfc_new_symbol (expr->value.function.name, NULL);

  sym->ts = expr->ts;
  if (sym->ts.type == BT_CHARACTER)
    sym->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
  sym->attr.external = 1;
  sym->attr.function = 1;
  sym->attr.always_explicit = 1;
  sym->attr.proc = PROC_INTRINSIC;
  sym->attr.flavor = FL_PROCEDURE;
  sym->result = sym;
  if (expr->rank > 0)
    {
      sym->attr.dimension = 1;
      sym->as = gfc_get_array_spec ();
      sym->as->type = AS_ASSUMED_SHAPE;
      sym->as->rank = expr->rank;
    }

  gfc_copy_formal_args_intr (sym, expr->value.function.isym,
			     ignore_optional ? expr->value.function.actual
					     : NULL);

  return sym;
}

/* Remove empty actual arguments.  */

static void
remove_empty_actual_arguments (gfc_actual_arglist **ap)
{
  while (*ap)
    {
      if ((*ap)->expr == NULL)
	{
	  gfc_actual_arglist *r = *ap;
	  *ap = r->next;
	  r->next = NULL;
	  gfc_free_actual_arglist (r);
	}
      else
	ap = &((*ap)->next);
    }
}

#define MAX_SPEC_ARG 12

/* Make up an fn spec that's right for intrinsic functions that we
   want to call.  */

static char *
intrinsic_fnspec (gfc_expr *expr)
{
  static char fnspec_buf[MAX_SPEC_ARG*2+1];
  char *fp;
  int i;
  int num_char_args;

#define ADD_CHAR(c) do { *fp++ = c; *fp++ = ' '; } while(0)

  /* Set the fndecl.  */
  fp = fnspec_buf;
  /* Function return value.  FIXME: Check if the second letter could
     be something other than a space, for further optimization.  */
  ADD_CHAR ('.');
  if (expr->rank == 0)
    {
      if (expr->ts.type == BT_CHARACTER)
	{
	  ADD_CHAR ('w');  /* Address of character.  */
	  ADD_CHAR ('.');  /* Length of character.  */
	}
    }
  else
    ADD_CHAR ('w');  /* Return value is a descriptor.  */

  num_char_args = 0;
  for (gfc_actual_arglist *a = expr->value.function.actual; a; a = a->next)
    {
      if (a->expr == NULL)
	continue;

      if (a->name && strcmp (a->name,"%VAL") == 0)
	ADD_CHAR ('.');
      else
	{
	  if (a->expr->rank > 0)
	    ADD_CHAR ('r');
	  else
	    ADD_CHAR ('R');
	}
      num_char_args += a->expr->ts.type == BT_CHARACTER;
      gcc_assert (fp - fnspec_buf + num_char_args <= MAX_SPEC_ARG*2);
    }

  for (i = 0; i < num_char_args; i++)
    ADD_CHAR ('.');

  *fp = '\0';
  return fnspec_buf;
}

#undef MAX_SPEC_ARG
#undef ADD_CHAR

/* Generate the right symbol for the specific intrinsic function and
 modify the expr accordingly.  This assumes that absent optional
 arguments should be removed.  */

gfc_symbol *
specific_intrinsic_symbol (gfc_expr *expr)
{
  gfc_symbol *sym;

  sym = gfc_find_intrinsic_symbol (expr);
  if (sym == NULL)
    {
      sym = gfc_get_intrinsic_function_symbol (expr);
      sym->ts = expr->ts;
      if (sym->ts.type == BT_CHARACTER && sym->ts.u.cl)
	sym->ts.u.cl = gfc_new_charlen (sym->ns, NULL);

      gfc_copy_formal_args_intr (sym, expr->value.function.isym,
				 expr->value.function.actual, true);
      sym->backend_decl
	= gfc_get_extern_function_decl (sym, expr->value.function.actual,
					intrinsic_fnspec (expr));
    }

  remove_empty_actual_arguments (&(expr->value.function.actual));

  return sym;
}

/* Generate a call to an external intrinsic function.  FIXME: So far,
   this only works for functions which are called with well-defined
   types; CSHIFT and friends will come later.  */

static void
gfc_conv_intrinsic_funcall (gfc_se * se, gfc_expr * expr)
{
  gfc_symbol *sym;
  vec<tree, va_gc> *append_args;
  bool specific_symbol;

  gcc_assert (!se->ss || se->ss->info->expr == expr);

  if (se->ss)
    gcc_assert (expr->rank > 0);
  else
    gcc_assert (expr->rank == 0);

  switch (expr->value.function.isym->id)
    {
    case GFC_ISYM_ANY:
    case GFC_ISYM_ALL:
    case GFC_ISYM_FINDLOC:
    case GFC_ISYM_MAXLOC:
    case GFC_ISYM_MINLOC:
    case GFC_ISYM_MAXVAL:
    case GFC_ISYM_MINVAL:
    case GFC_ISYM_NORM2:
    case GFC_ISYM_PRODUCT:
    case GFC_ISYM_SUM:
      specific_symbol = true;
      break;
    default:
      specific_symbol = false;
    }

  if (specific_symbol)
    {
      /* Need to copy here because specific_intrinsic_symbol modifies
	 expr to omit the absent optional arguments.  */
      expr = gfc_copy_expr (expr);
      sym = specific_intrinsic_symbol (expr);
    }
  else
    sym = gfc_get_symbol_for_expr (expr, se->ignore_optional);

  /* Calls to libgfortran_matmul need to be appended special arguments,
     to be able to call the BLAS ?gemm functions if required and possible.  */
  append_args = NULL;
  if (expr->value.function.isym->id == GFC_ISYM_MATMUL
      && !expr->external_blas
      && sym->ts.type != BT_LOGICAL)
    {
      tree cint = gfc_get_int_type (gfc_c_int_kind);

      if (flag_external_blas
	  && (sym->ts.type == BT_REAL || sym->ts.type == BT_COMPLEX)
	  && (sym->ts.kind == 4 || sym->ts.kind == 8))
	{
	  tree gemm_fndecl;

	  if (sym->ts.type == BT_REAL)
	    {
	      if (sym->ts.kind == 4)
		gemm_fndecl = gfor_fndecl_sgemm;
	      else
		gemm_fndecl = gfor_fndecl_dgemm;
	    }
	  else
	    {
	      if (sym->ts.kind == 4)
		gemm_fndecl = gfor_fndecl_cgemm;
	      else
		gemm_fndecl = gfor_fndecl_zgemm;
	    }

	  vec_alloc (append_args, 3);
	  append_args->quick_push (build_int_cst (cint, 1));
	  append_args->quick_push (build_int_cst (cint,
						  flag_blas_matmul_limit));
	  append_args->quick_push (gfc_build_addr_expr (NULL_TREE,
							gemm_fndecl));
	}
      else
	{
	  vec_alloc (append_args, 3);
	  append_args->quick_push (build_int_cst (cint, 0));
	  append_args->quick_push (build_int_cst (cint, 0));
	  append_args->quick_push (null_pointer_node);
	}
    }

  gfc_conv_procedure_call (se, sym, expr->value.function.actual, expr,
			  append_args);

  if (specific_symbol)
    gfc_free_expr (expr);
  else
    gfc_free_symbol (sym);
}

/* ANY and ALL intrinsics. ANY->op == NE_EXPR, ALL->op == EQ_EXPR.
   Implemented as
    any(a)
    {
      forall (i=...)
        if (a[i] != 0)
          return 1
      end forall
      return 0
    }
    all(a)
    {
      forall (i=...)
        if (a[i] == 0)
          return 0
      end forall
      return 1
    }
 */
static void
gfc_conv_intrinsic_anyall (gfc_se * se, gfc_expr * expr, enum tree_code op)
{
  tree resvar;
  stmtblock_t block;
  stmtblock_t body;
  tree type;
  tree tmp;
  tree found;
  gfc_loopinfo loop;
  gfc_actual_arglist *actual;
  gfc_ss *arrayss;
  gfc_se arrayse;
  tree exit_label;

  if (se->ss)
    {
      gfc_conv_intrinsic_funcall (se, expr);
      return;
    }

  actual = expr->value.function.actual;
  type = gfc_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  resvar = gfc_create_var (type, "test");
  if (op == EQ_EXPR)
    tmp = convert (type, boolean_true_node);
  else
    tmp = convert (type, boolean_false_node);
  gfc_add_modify (&se->pre, resvar, tmp);

  /* Walk the arguments.  */
  arrayss = gfc_walk_expr (actual->expr);
  gcc_assert (arrayss != gfc_ss_terminator);

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);
  exit_label = gfc_build_label_decl (NULL_TREE);
  TREE_USED (exit_label) = 1;
  gfc_add_ss_to_loop (&loop, arrayss);

  /* Initialize the loop.  */
  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop, &expr->where);

  gfc_mark_ss_chain_used (arrayss, 1);
  /* Generate the loop body.  */
  gfc_start_scalarized_body (&loop, &body);

  /* If the condition matches then set the return value.  */
  gfc_start_block (&block);
  if (op == EQ_EXPR)
    tmp = convert (type, boolean_false_node);
  else
    tmp = convert (type, boolean_true_node);
  gfc_add_modify (&block, resvar, tmp);

  /* And break out of the loop.  */
  tmp = build1_v (GOTO_EXPR, exit_label);
  gfc_add_expr_to_block (&block, tmp);

  found = gfc_finish_block (&block);

  /* Check this element.  */
  gfc_init_se (&arrayse, NULL);
  gfc_copy_loopinfo_to_se (&arrayse, &loop);
  arrayse.ss = arrayss;
  gfc_conv_expr_val (&arrayse, actual->expr);

  gfc_add_block_to_block (&body, &arrayse.pre);
  tmp = fold_build2_loc (input_location, op, logical_type_node, arrayse.expr,
			 build_int_cst (TREE_TYPE (arrayse.expr), 0));
  tmp = build3_v (COND_EXPR, tmp, found, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&body, tmp);
  gfc_add_block_to_block (&body, &arrayse.post);

  gfc_trans_scalarizing_loops (&loop, &body);

  /* Add the exit label.  */
  tmp = build1_v (LABEL_EXPR, exit_label);
  gfc_add_expr_to_block (&loop.pre, tmp);

  gfc_add_block_to_block (&se->pre, &loop.pre);
  gfc_add_block_to_block (&se->pre, &loop.post);
  gfc_cleanup_loop (&loop);

  se->expr = resvar;
}


/* Generate the constant 180 / pi, which is used in the conversion
   of acosd(), asind(), atand(), atan2d().  */

static tree
rad2deg (int kind)
{
  tree retval;
  mpfr_t pi, t0;

  gfc_set_model_kind (kind);
  mpfr_init (pi);
  mpfr_init (t0);
  mpfr_set_si (t0, 180, GFC_RND_MODE);
  mpfr_const_pi (pi, GFC_RND_MODE);
  mpfr_div (t0, t0, pi, GFC_RND_MODE);
  retval = gfc_conv_mpfr_to_tree (t0, kind, 0);
  mpfr_clear (t0);
  mpfr_clear (pi);
  return retval;
}


static gfc_intrinsic_map_t *
gfc_lookup_intrinsic (gfc_isym_id id)
{
  gfc_intrinsic_map_t *m = gfc_intrinsic_map;
  for (; m->id != GFC_ISYM_NONE || m->double_built_in != END_BUILTINS; m++)
    if (id == m->id)
      break;
  gcc_assert (id == m->id);
  return m;
}


/* ACOSD(x) is translated into ACOS(x) * 180 / pi.
   ASIND(x) is translated into ASIN(x) * 180 / pi.
   ATAND(x) is translated into ATAN(x) * 180 / pi.  */

static void
gfc_conv_intrinsic_atrigd (gfc_se * se, gfc_expr * expr, gfc_isym_id id)
{
  tree arg;
  tree atrigd;
  tree type;
  gfc_intrinsic_map_t *m;

  type = gfc_typenode_for_spec (&expr->ts);

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);

  switch (id)
    {
    case GFC_ISYM_ACOSD:
      m = gfc_lookup_intrinsic (GFC_ISYM_ACOS);
      break;
    case GFC_ISYM_ASIND:
      m = gfc_lookup_intrinsic (GFC_ISYM_ASIN);
      break;
    case GFC_ISYM_ATAND:
      m = gfc_lookup_intrinsic (GFC_ISYM_ATAN);
      break;
    default:
      gcc_unreachable ();
    }
  atrigd = gfc_get_intrinsic_lib_fndecl (m, expr);
  atrigd = build_call_expr_loc (input_location, atrigd, 1, arg);

  se->expr = fold_build2_loc (input_location, MULT_EXPR, type, atrigd,
			      fold_convert (type, rad2deg (expr->ts.kind)));
}


/* COTAN(X) is translated into -TAN(X+PI/2) for REAL argument and
   COS(X) / SIN(X) for COMPLEX argument.  */

static void
gfc_conv_intrinsic_cotan (gfc_se *se, gfc_expr *expr)
{
  gfc_intrinsic_map_t *m;
  tree arg;
  tree type;

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);

  if (expr->ts.type == BT_REAL)
    {
      tree tan;
      tree tmp;
      mpfr_t pio2;

      /* Create pi/2.  */
      gfc_set_model_kind (expr->ts.kind);
      mpfr_init (pio2);
      mpfr_const_pi (pio2, GFC_RND_MODE);
      mpfr_div_ui (pio2, pio2, 2, GFC_RND_MODE);
      tmp = gfc_conv_mpfr_to_tree (pio2, expr->ts.kind, 0);
      mpfr_clear (pio2);

      /* Find tan builtin function.  */
      m = gfc_lookup_intrinsic (GFC_ISYM_TAN);
      tan = gfc_get_intrinsic_lib_fndecl (m, expr);
      tmp = fold_build2_loc (input_location, PLUS_EXPR, type, arg, tmp);
      tan = build_call_expr_loc (input_location, tan, 1, tmp);
      se->expr = fold_build1_loc (input_location, NEGATE_EXPR, type, tan);
    }
  else
    {
      tree sin;
      tree cos;

      /* Find cos builtin function.  */
      m = gfc_lookup_intrinsic (GFC_ISYM_COS);
      cos = gfc_get_intrinsic_lib_fndecl (m, expr);
      cos = build_call_expr_loc (input_location, cos, 1, arg);

      /* Find sin builtin function.  */
      m = gfc_lookup_intrinsic (GFC_ISYM_SIN);
      sin = gfc_get_intrinsic_lib_fndecl (m, expr);
      sin = build_call_expr_loc (input_location, sin, 1, arg);

      /* Divide cos by sin. */
      se->expr = fold_build2_loc (input_location, RDIV_EXPR, type, cos, sin);
   }
}


/* COTAND(X) is translated into -TAND(X+90) for REAL argument.  */

static void
gfc_conv_intrinsic_cotand (gfc_se *se, gfc_expr *expr)
{
  tree arg;
  tree type;
  tree ninety_tree;
  mpfr_t ninety;

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);

  gfc_set_model_kind (expr->ts.kind);

  /* Build the tree for x + 90.  */
  mpfr_init_set_ui (ninety, 90, GFC_RND_MODE);
  ninety_tree = gfc_conv_mpfr_to_tree (ninety, expr->ts.kind, 0);
  arg = fold_build2_loc (input_location, PLUS_EXPR, type, arg, ninety_tree);
  mpfr_clear (ninety);

  /* Find tand.  */
  gfc_intrinsic_map_t *m = gfc_lookup_intrinsic (GFC_ISYM_TAND);
  tree tand = gfc_get_intrinsic_lib_fndecl (m, expr);
  tand = build_call_expr_loc (input_location, tand, 1, arg);

  se->expr = fold_build1_loc (input_location, NEGATE_EXPR, type, tand);
}


/* ATAN2D(Y,X) is translated into ATAN2(Y,X) * 180 / PI. */

static void
gfc_conv_intrinsic_atan2d (gfc_se *se, gfc_expr *expr)
{
  tree args[2];
  tree atan2d;
  tree type;

  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  type = TREE_TYPE (args[0]);

  gfc_intrinsic_map_t *m = gfc_lookup_intrinsic (GFC_ISYM_ATAN2);
  atan2d = gfc_get_intrinsic_lib_fndecl (m, expr);
  atan2d = build_call_expr_loc (input_location, atan2d, 2, args[0], args[1]);

  se->expr = fold_build2_loc (input_location, MULT_EXPR, type, atan2d,
			      rad2deg (expr->ts.kind));
}


/* COUNT(A) = Number of true elements in A.  */
static void
gfc_conv_intrinsic_count (gfc_se * se, gfc_expr * expr)
{
  tree resvar;
  tree type;
  stmtblock_t body;
  tree tmp;
  gfc_loopinfo loop;
  gfc_actual_arglist *actual;
  gfc_ss *arrayss;
  gfc_se arrayse;

  if (se->ss)
    {
      gfc_conv_intrinsic_funcall (se, expr);
      return;
    }

  actual = expr->value.function.actual;

  type = gfc_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  resvar = gfc_create_var (type, "count");
  gfc_add_modify (&se->pre, resvar, build_int_cst (type, 0));

  /* Walk the arguments.  */
  arrayss = gfc_walk_expr (actual->expr);
  gcc_assert (arrayss != gfc_ss_terminator);

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);
  gfc_add_ss_to_loop (&loop, arrayss);

  /* Initialize the loop.  */
  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop, &expr->where);

  gfc_mark_ss_chain_used (arrayss, 1);
  /* Generate the loop body.  */
  gfc_start_scalarized_body (&loop, &body);

  tmp = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (resvar),
			 resvar, build_int_cst (TREE_TYPE (resvar), 1));
  tmp = build2_v (MODIFY_EXPR, resvar, tmp);

  gfc_init_se (&arrayse, NULL);
  gfc_copy_loopinfo_to_se (&arrayse, &loop);
  arrayse.ss = arrayss;
  gfc_conv_expr_val (&arrayse, actual->expr);
  tmp = build3_v (COND_EXPR, arrayse.expr, tmp,
		  build_empty_stmt (input_location));

  gfc_add_block_to_block (&body, &arrayse.pre);
  gfc_add_expr_to_block (&body, tmp);
  gfc_add_block_to_block (&body, &arrayse.post);

  gfc_trans_scalarizing_loops (&loop, &body);

  gfc_add_block_to_block (&se->pre, &loop.pre);
  gfc_add_block_to_block (&se->pre, &loop.post);
  gfc_cleanup_loop (&loop);

  se->expr = resvar;
}


/* Update given gfc_se to have ss component pointing to the nested gfc_ss
   struct and return the corresponding loopinfo.  */

static gfc_loopinfo *
enter_nested_loop (gfc_se *se)
{
  se->ss = se->ss->nested_ss;
  gcc_assert (se->ss == se->ss->loop->ss);

  return se->ss->loop;
}

/* Build the condition for a mask, which may be optional.  */

static tree
conv_mask_condition (gfc_se *maskse, gfc_expr *maskexpr,
			 bool optional_mask)
{
  tree present;
  tree type;

  if (optional_mask)
    {
      type = TREE_TYPE (maskse->expr);
      present = gfc_conv_expr_present (maskexpr->symtree->n.sym);
      present = convert (type, present);
      present = fold_build1_loc (input_location, TRUTH_NOT_EXPR, type,
				 present);
      return fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
			      type, present, maskse->expr);
    }
  else
    return maskse->expr;
}

/* Inline implementation of the sum and product intrinsics.  */
static void
gfc_conv_intrinsic_arith (gfc_se * se, gfc_expr * expr, enum tree_code op,
			  bool norm2)
{
  tree resvar;
  tree scale = NULL_TREE;
  tree type;
  stmtblock_t body;
  stmtblock_t block;
  tree tmp;
  gfc_loopinfo loop, *ploop;
  gfc_actual_arglist *arg_array, *arg_mask;
  gfc_ss *arrayss = NULL;
  gfc_ss *maskss = NULL;
  gfc_se arrayse;
  gfc_se maskse;
  gfc_se *parent_se;
  gfc_expr *arrayexpr;
  gfc_expr *maskexpr;
  bool optional_mask;

  if (expr->rank > 0)
    {
      gcc_assert (gfc_inline_intrinsic_function_p (expr));
      parent_se = se;
    }
  else
    parent_se = NULL;

  type = gfc_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  resvar = gfc_create_var (type, "val");
  if (norm2)
    {
      /* result = 0.0;
	 scale = 1.0.  */
      scale = gfc_create_var (type, "scale");
      gfc_add_modify (&se->pre, scale,
		      gfc_build_const (type, integer_one_node));
      tmp = gfc_build_const (type, integer_zero_node);
    }
  else if (op == PLUS_EXPR || op == BIT_IOR_EXPR || op == BIT_XOR_EXPR)
    tmp = gfc_build_const (type, integer_zero_node);
  else if (op == NE_EXPR)
    /* PARITY.  */
    tmp = convert (type, boolean_false_node);
  else if (op == BIT_AND_EXPR)
    tmp = gfc_build_const (type, fold_build1_loc (input_location, NEGATE_EXPR,
						  type, integer_one_node));
  else
    tmp = gfc_build_const (type, integer_one_node);

  gfc_add_modify (&se->pre, resvar, tmp);

  arg_array = expr->value.function.actual;

  arrayexpr = arg_array->expr;

  if (op == NE_EXPR || norm2)
    {
      /* PARITY and NORM2.  */
      maskexpr = NULL;
      optional_mask = false;
    }
  else
    {
      arg_mask  = arg_array->next->next;
      gcc_assert (arg_mask != NULL);
      maskexpr = arg_mask->expr;
      optional_mask = maskexpr && maskexpr->expr_type == EXPR_VARIABLE
	&& maskexpr->symtree->n.sym->attr.dummy
	&& maskexpr->symtree->n.sym->attr.optional;
    }

  if (expr->rank == 0)
    {
      /* Walk the arguments.  */
      arrayss = gfc_walk_expr (arrayexpr);
      gcc_assert (arrayss != gfc_ss_terminator);

      if (maskexpr && maskexpr->rank > 0)
	{
	  maskss = gfc_walk_expr (maskexpr);
	  gcc_assert (maskss != gfc_ss_terminator);
	}
      else
	maskss = NULL;

      /* Initialize the scalarizer.  */
      gfc_init_loopinfo (&loop);

      /* We add the mask first because the number of iterations is
	 taken from the last ss, and this breaks if an absent
	 optional argument is used for mask.  */

      if (maskexpr && maskexpr->rank > 0)
	gfc_add_ss_to_loop (&loop, maskss);
      gfc_add_ss_to_loop (&loop, arrayss);

      /* Initialize the loop.  */
      gfc_conv_ss_startstride (&loop);
      gfc_conv_loop_setup (&loop, &expr->where);

      if (maskexpr && maskexpr->rank > 0)
	gfc_mark_ss_chain_used (maskss, 1);
      gfc_mark_ss_chain_used (arrayss, 1);

      ploop = &loop;
    }
  else
    /* All the work has been done in the parent loops.  */
    ploop = enter_nested_loop (se);

  gcc_assert (ploop);

  /* Generate the loop body.  */
  gfc_start_scalarized_body (ploop, &body);

  /* If we have a mask, only add this element if the mask is set.  */
  if (maskexpr && maskexpr->rank > 0)
    {
      gfc_init_se (&maskse, parent_se);
      gfc_copy_loopinfo_to_se (&maskse, ploop);
      if (expr->rank == 0)
	maskse.ss = maskss;
      gfc_conv_expr_val (&maskse, maskexpr);
      gfc_add_block_to_block (&body, &maskse.pre);

      gfc_start_block (&block);
    }
  else
    gfc_init_block (&block);

  /* Do the actual summation/product.  */
  gfc_init_se (&arrayse, parent_se);
  gfc_copy_loopinfo_to_se (&arrayse, ploop);
  if (expr->rank == 0)
    arrayse.ss = arrayss;
  gfc_conv_expr_val (&arrayse, arrayexpr);
  gfc_add_block_to_block (&block, &arrayse.pre);

  if (norm2)
    {
      /* if (x (i) != 0.0)
	   {
	     absX = abs(x(i))
	     if (absX > scale)
	       {
                 val = scale/absX;
		 result = 1.0 + result * val * val;
		 scale = absX;
	       }
	     else
	       {
                 val = absX/scale;
	         result += val * val;
	       }
	   }  */
      tree res1, res2, cond, absX, val;
      stmtblock_t ifblock1, ifblock2, ifblock3;

      gfc_init_block (&ifblock1);

      absX = gfc_create_var (type, "absX");
      gfc_add_modify (&ifblock1, absX,
		      fold_build1_loc (input_location, ABS_EXPR, type,
				       arrayse.expr));
      val = gfc_create_var (type, "val");
      gfc_add_expr_to_block (&ifblock1, val);

      gfc_init_block (&ifblock2);
      gfc_add_modify (&ifblock2, val,
		      fold_build2_loc (input_location, RDIV_EXPR, type, scale,
				       absX));
      res1 = fold_build2_loc (input_location, MULT_EXPR, type, val, val);
      res1 = fold_build2_loc (input_location, MULT_EXPR, type, resvar, res1);
      res1 = fold_build2_loc (input_location, PLUS_EXPR, type, res1,
			      gfc_build_const (type, integer_one_node));
      gfc_add_modify (&ifblock2, resvar, res1);
      gfc_add_modify (&ifblock2, scale, absX);
      res1 = gfc_finish_block (&ifblock2);

      gfc_init_block (&ifblock3);
      gfc_add_modify (&ifblock3, val,
		      fold_build2_loc (input_location, RDIV_EXPR, type, absX,
				       scale));
      res2 = fold_build2_loc (input_location, MULT_EXPR, type, val, val);
      res2 = fold_build2_loc (input_location, PLUS_EXPR, type, resvar, res2);
      gfc_add_modify (&ifblock3, resvar, res2);
      res2 = gfc_finish_block (&ifblock3);

      cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			      absX, scale);
      tmp = build3_v (COND_EXPR, cond, res1, res2);
      gfc_add_expr_to_block (&ifblock1, tmp);
      tmp = gfc_finish_block (&ifblock1);

      cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			      arrayse.expr,
			      gfc_build_const (type, integer_zero_node));

      tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);
    }
  else
    {
      tmp = fold_build2_loc (input_location, op, type, resvar, arrayse.expr);
      gfc_add_modify (&block, resvar, tmp);
    }

  gfc_add_block_to_block (&block, &arrayse.post);

  if (maskexpr && maskexpr->rank > 0)
    {
      /* We enclose the above in if (mask) {...} .  If the mask is an
	 optional argument, generate
	 IF (.NOT. PRESENT(MASK) .OR. MASK(I)).  */
      tree ifmask;
      tmp = gfc_finish_block (&block);
      ifmask = conv_mask_condition (&maskse, maskexpr, optional_mask);
      tmp = build3_v (COND_EXPR, ifmask, tmp,
		      build_empty_stmt (input_location));
    }
  else
    tmp = gfc_finish_block (&block);
  gfc_add_expr_to_block (&body, tmp);

  gfc_trans_scalarizing_loops (ploop, &body);

  /* For a scalar mask, enclose the loop in an if statement.  */
  if (maskexpr && maskexpr->rank == 0)
    {
      gfc_init_block (&block);
      gfc_add_block_to_block (&block, &ploop->pre);
      gfc_add_block_to_block (&block, &ploop->post);
      tmp = gfc_finish_block (&block);

      if (expr->rank > 0)
	{
	  tmp = build3_v (COND_EXPR, se->ss->info->data.scalar.value, tmp,
			  build_empty_stmt (input_location));
	  gfc_advance_se_ss_chain (se);
	}
      else
	{
	  tree ifmask;

	  gcc_assert (expr->rank == 0);
	  gfc_init_se (&maskse, NULL);
	  gfc_conv_expr_val (&maskse, maskexpr);
	  ifmask = conv_mask_condition (&maskse, maskexpr, optional_mask);
	  tmp = build3_v (COND_EXPR, ifmask, tmp,
			  build_empty_stmt (input_location));
	}

      gfc_add_expr_to_block (&block, tmp);
      gfc_add_block_to_block (&se->pre, &block);
      gcc_assert (se->post.head == NULL);
    }
  else
    {
      gfc_add_block_to_block (&se->pre, &ploop->pre);
      gfc_add_block_to_block (&se->pre, &ploop->post);
    }

  if (expr->rank == 0)
    gfc_cleanup_loop (ploop);

  if (norm2)
    {
      /* result = scale * sqrt(result).  */
      tree sqrt;
      sqrt = gfc_builtin_decl_for_float_kind (BUILT_IN_SQRT, expr->ts.kind);
      resvar = build_call_expr_loc (input_location,
				    sqrt, 1, resvar);
      resvar = fold_build2_loc (input_location, MULT_EXPR, type, scale, resvar);
    }

  se->expr = resvar;
}


/* Inline implementation of the dot_product intrinsic. This function
   is based on gfc_conv_intrinsic_arith (the previous function).  */
static void
gfc_conv_intrinsic_dot_product (gfc_se * se, gfc_expr * expr)
{
  tree resvar;
  tree type;
  stmtblock_t body;
  stmtblock_t block;
  tree tmp;
  gfc_loopinfo loop;
  gfc_actual_arglist *actual;
  gfc_ss *arrayss1, *arrayss2;
  gfc_se arrayse1, arrayse2;
  gfc_expr *arrayexpr1, *arrayexpr2;

  type = gfc_typenode_for_spec (&expr->ts);

  /* Initialize the result.  */
  resvar = gfc_create_var (type, "val");
  if (expr->ts.type == BT_LOGICAL)
    tmp = build_int_cst (type, 0);
  else
    tmp = gfc_build_const (type, integer_zero_node);

  gfc_add_modify (&se->pre, resvar, tmp);

  /* Walk argument #1.  */
  actual = expr->value.function.actual;
  arrayexpr1 = actual->expr;
  arrayss1 = gfc_walk_expr (arrayexpr1);
  gcc_assert (arrayss1 != gfc_ss_terminator);

  /* Walk argument #2.  */
  actual = actual->next;
  arrayexpr2 = actual->expr;
  arrayss2 = gfc_walk_expr (arrayexpr2);
  gcc_assert (arrayss2 != gfc_ss_terminator);

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);
  gfc_add_ss_to_loop (&loop, arrayss1);
  gfc_add_ss_to_loop (&loop, arrayss2);

  /* Initialize the loop.  */
  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop, &expr->where);

  gfc_mark_ss_chain_used (arrayss1, 1);
  gfc_mark_ss_chain_used (arrayss2, 1);

  /* Generate the loop body.  */
  gfc_start_scalarized_body (&loop, &body);
  gfc_init_block (&block);

  /* Make the tree expression for [conjg(]array1[)].  */
  gfc_init_se (&arrayse1, NULL);
  gfc_copy_loopinfo_to_se (&arrayse1, &loop);
  arrayse1.ss = arrayss1;
  gfc_conv_expr_val (&arrayse1, arrayexpr1);
  if (expr->ts.type == BT_COMPLEX)
    arrayse1.expr = fold_build1_loc (input_location, CONJ_EXPR, type,
				     arrayse1.expr);
  gfc_add_block_to_block (&block, &arrayse1.pre);

  /* Make the tree expression for array2.  */
  gfc_init_se (&arrayse2, NULL);
  gfc_copy_loopinfo_to_se (&arrayse2, &loop);
  arrayse2.ss = arrayss2;
  gfc_conv_expr_val (&arrayse2, arrayexpr2);
  gfc_add_block_to_block (&block, &arrayse2.pre);

  /* Do the actual product and sum.  */
  if (expr->ts.type == BT_LOGICAL)
    {
      tmp = fold_build2_loc (input_location, TRUTH_AND_EXPR, type,
			     arrayse1.expr, arrayse2.expr);
      tmp = fold_build2_loc (input_location, TRUTH_OR_EXPR, type, resvar, tmp);
    }
  else
    {
      tmp = fold_build2_loc (input_location, MULT_EXPR, type, arrayse1.expr,
			     arrayse2.expr);
      tmp = fold_build2_loc (input_location, PLUS_EXPR, type, resvar, tmp);
    }
  gfc_add_modify (&block, resvar, tmp);

  /* Finish up the loop block and the loop.  */
  tmp = gfc_finish_block (&block);
  gfc_add_expr_to_block (&body, tmp);

  gfc_trans_scalarizing_loops (&loop, &body);
  gfc_add_block_to_block (&se->pre, &loop.pre);
  gfc_add_block_to_block (&se->pre, &loop.post);
  gfc_cleanup_loop (&loop);

  se->expr = resvar;
}


/* Tells whether the expression E is a reference to an optional variable whose
   presence is not known at compile time.  Those are variable references without
   subreference; if there is a subreference, we can assume the variable is
   present.  We have to special case full arrays, which we represent with a fake
   "full" reference, and class descriptors for which a reference to data is not
   really a subreference.  */

bool
maybe_absent_optional_variable (gfc_expr *e)
{
  if (!(e && e->expr_type == EXPR_VARIABLE))
    return false;

  gfc_symbol *sym = e->symtree->n.sym;
  if (!sym->attr.optional)
    return false;

  gfc_ref *ref = e->ref;
  if (ref == nullptr)
    return true;

  if (ref->type == REF_ARRAY
      && ref->u.ar.type == AR_FULL
      && ref->next == nullptr)
    return true;

  if (!(sym->ts.type == BT_CLASS
	&& ref->type == REF_COMPONENT
	&& ref->u.c.component == CLASS_DATA (sym)))
    return false;

  gfc_ref *next_ref = ref->next;
  if (next_ref == nullptr)
    return true;

  if (next_ref->type == REF_ARRAY
      && next_ref->u.ar.type == AR_FULL
      && next_ref->next == nullptr)
    return true;

  return false;
}


/* Remove unneeded kind= argument from actual argument list when the
   result conversion is dealt with in a different place.  */

static void
strip_kind_from_actual (gfc_actual_arglist * actual)
{
  for (gfc_actual_arglist *a = actual; a; a = a->next)
    {
      if (a && a->name && strcmp (a->name, "kind") == 0)
	{
	  gfc_free_expr (a->expr);
	  a->expr = NULL;
	}
    }
}

/* Emit code for minloc or maxloc intrinsic.  There are many different cases
   we need to handle.  For performance reasons we sometimes create two
   loops instead of one, where the second one is much simpler.
   Examples for minloc intrinsic:
   A: Result is scalar.
      1) Array mask is used and NaNs need to be supported:
	 limit = Infinity;
	 pos = 0;
	 S = from;
	 while (S <= to) {
	   if (mask[S]) {
	     if (pos == 0) pos = S + (1 - from);
	     if (a[S] <= limit) {
	       limit = a[S];
	       pos = S + (1 - from);
	       goto lab1;
	     }
	   }
	   S++;
	 }
	 goto lab2;
	 lab1:;
	 while (S <= to) {
	   if (mask[S])
	     if (a[S] < limit) {
	       limit = a[S];
	       pos = S + (1 - from);
	     }
	   S++;
	 }
	 lab2:;
      2) NaNs need to be supported, but it is known at compile time or cheaply
	 at runtime whether array is nonempty or not:
	 limit = Infinity;
	 pos = 0;
	 S = from;
	 while (S <= to) {
	   if (a[S] <= limit) {
	     limit = a[S];
	     pos = S + (1 - from);
	     goto lab1;
	   }
	   S++;
	 }
	 if (from <= to) pos = 1;
	 goto lab2;
	 lab1:;
	 while (S <= to) {
	   if (a[S] < limit) {
	     limit = a[S];
	     pos = S + (1 - from);
	   }
	   S++;
	 }
	 lab2:;
      3) NaNs aren't supported, array mask is used:
	 limit = infinities_supported ? Infinity : huge (limit);
	 pos = 0;
	 S = from;
	 while (S <= to) {
	   if (mask[S]) {
	     limit = a[S];
	     pos = S + (1 - from);
	     goto lab1;
	   }
	   S++;
	 }
	 goto lab2;
	 lab1:;
	 while (S <= to) {
	   if (mask[S])
	     if (a[S] < limit) {
	       limit = a[S];
	       pos = S + (1 - from);
	     }
	   S++;
	 }
	 lab2:;
      4) Same without array mask:
	 limit = infinities_supported ? Infinity : huge (limit);
	 pos = (from <= to) ? 1 : 0;
	 S = from;
	 while (S <= to) {
	   if (a[S] < limit) {
	     limit = a[S];
	     pos = S + (1 - from);
	   }
	   S++;
	 }
   B: Array result, non-CHARACTER type, DIM absent
      Generate similar code as in the scalar case, using a collection of
      variables (one per dimension) instead of a single variable as result.
      Picking only cases 1) and 4) with ARRAY of rank 2, the generated code
      becomes:
      1) Array mask is used and NaNs need to be supported:
	 limit = Infinity;
	 pos0 = 0;
	 pos1 = 0;
	 S1 = from1;
	 second_loop_entry = false;
	 while (S1 <= to1) {
	   S0 = from0;
	   while (s0 <= to0 {
	     if (mask[S1][S0]) {
	       if (pos0 == 0) {
		 pos0 = S0 + (1 - from0);
		 pos1 = S1 + (1 - from1);
	       }
	       if (a[S1][S0] <= limit) {
		 limit = a[S1][S0];
		 pos0 = S0 + (1 - from0);
		 pos1 = S1 + (1 - from1);
		 second_loop_entry = true;
		 goto lab1;
	       }
	     }
	     S0++;
	   }
	   S1++;
	 }
	 goto lab2;
	 lab1:;
	 S1 = second_loop_entry ? S1 : from1;
	 while (S1 <= to1) {
	   S0 = second_loop_entry ? S0 : from0;
	   while (S0 <= to0) {
	     if (mask[S1][S0])
	       if (a[S1][S0] < limit) {
		 limit = a[S1][S0];
		 pos0 = S + (1 - from0);
		 pos1 = S + (1 - from1);
	       }
	     second_loop_entry = false;
	     S0++;
	   }
	   S1++;
	 }
	 lab2:;
	 result = { pos0, pos1 };
      ...
      4) NANs aren't supported, no array mask.
	 limit = infinities_supported ? Infinity : huge (limit);
	 pos0 = (from0 <= to0 && from1 <= to1) ? 1 : 0;
	 pos1 = (from0 <= to0 && from1 <= to1) ? 1 : 0;
	 S1 = from1;
	 while (S1 <= to1) {
	   S0 = from0;
	   while (S0 <= to0) {
	     if (a[S1][S0] < limit) {
	       limit = a[S1][S0];
	       pos0 = S + (1 - from0);
	       pos1 = S + (1 - from1);
	     }
	     S0++;
	   }
	   S1++;
	 }
	 result = { pos0, pos1 };
   C: Otherwise, a call is generated.
   For 2) and 4), if mask is scalar, this all goes into a conditional,
   setting pos = 0; in the else branch.

   Since we now also support the BACK argument, instead of using
   if (a[S] < limit), we now use

   if (back)
     cond = a[S] <= limit;
   else
     cond = a[S] < limit;
   if (cond) {
     ....

   The optimizer is smart enough to move the condition out of the loop.
   They are now marked as unlikely too for further speedup.  */

static void
gfc_conv_intrinsic_minmaxloc (gfc_se * se, gfc_expr * expr, enum tree_code op)
{
  stmtblock_t body;
  stmtblock_t block;
  stmtblock_t ifblock;
  stmtblock_t elseblock;
  tree limit;
  tree type;
  tree tmp;
  tree cond;
  tree elsetmp;
  tree ifbody;
  tree offset[GFC_MAX_DIMENSIONS];
  tree nonempty;
  tree lab1, lab2;
  tree b_if, b_else;
  tree back;
  gfc_loopinfo loop, *ploop;
  gfc_actual_arglist *actual, *array_arg, *dim_arg, *mask_arg, *kind_arg;
  gfc_actual_arglist *back_arg;
  gfc_ss *arrayss = nullptr;
  gfc_ss *maskss = nullptr;
  gfc_ss *orig_ss = nullptr;
  gfc_se arrayse;
  gfc_se maskse;
  gfc_se nested_se;
  gfc_se *base_se;
  gfc_expr *arrayexpr;
  gfc_expr *maskexpr;
  gfc_expr *backexpr;
  gfc_se backse;
  tree pos[GFC_MAX_DIMENSIONS];
  tree idx[GFC_MAX_DIMENSIONS];
  tree result_var = NULL_TREE;
  int n;
  bool optional_mask;

  actual = expr->value.function.actual;
  array_arg = actual;
  dim_arg = array_arg->next;
  mask_arg = dim_arg->next;
  kind_arg = mask_arg->next;
  back_arg = kind_arg->next;

  bool dim_present = dim_arg->expr != nullptr;
  bool nested_loop = dim_present && expr->rank > 0;

  /* The last argument, BACK, is passed by value. Ensure that
     by setting its name to %VAL. */
  for (gfc_actual_arglist *a = actual; a; a = a->next)
    {
      if (a->next == NULL)
	a->name = "%VAL";
    }

  if (se->ss)
    {
      if (se->ss->info->useflags)
	{
	  if (!dim_present || !gfc_inline_intrinsic_function_p (expr))
	    {
	      /* The code generating and initializing the result array has been
		 generated already before the scalarization loop, either with a
		 library function call or with inline code; now we can just use
		 the result.  */
	      gfc_conv_tmp_array_ref (se);
	      return;
	    }
	}
      else if (!gfc_inline_intrinsic_function_p (expr))
	{
	  gfc_conv_intrinsic_funcall (se, expr);
	  return;
	}
    }

  arrayexpr = actual->expr;

  /* Special case for character maxloc.  Remove unneeded actual
     arguments, then call a library function.  */

  if (arrayexpr->ts.type == BT_CHARACTER)
    {
      gcc_assert (expr->rank == 0);

      gfc_actual_arglist *a = actual;
      strip_kind_from_actual (a);
      while (a)
	{
	  if (a->name && strcmp (a->name, "dim") == 0)
	    {
	      gfc_free_expr (a->expr);
	      a->expr = NULL;
	    }
	  a = a->next;
	}
      gfc_conv_intrinsic_funcall (se, expr);
      return;
    }

  type = gfc_typenode_for_spec (&expr->ts);

  if (expr->rank > 0 && !dim_present)
    {
      gfc_array_spec as;
      memset (&as, 0, sizeof (as));

      as.rank = 1;
      as.lower[0] = gfc_get_int_expr (gfc_index_integer_kind,
				      &arrayexpr->where,
				      HOST_WIDE_INT_1);
      as.upper[0] = gfc_get_int_expr (gfc_index_integer_kind,
				      &arrayexpr->where,
				      arrayexpr->rank);

      tree array = gfc_get_nodesc_array_type (type, &as, PACKED_STATIC, true);

      result_var = gfc_create_var (array, "loc_result");
    }

  const int reduction_dimensions = dim_present ? 1 : arrayexpr->rank;

  /* Initialize the result.  */
  for (int i = 0; i < reduction_dimensions; i++)
    {
      pos[i] = gfc_create_var (gfc_array_index_type,
			       gfc_get_string ("pos%d", i));
      offset[i] = gfc_create_var (gfc_array_index_type,
				  gfc_get_string ("offset%d", i));
      idx[i] = gfc_create_var (gfc_array_index_type,
			       gfc_get_string ("idx%d", i));
    }

  maskexpr = mask_arg->expr;
  optional_mask = maskexpr && maskexpr->expr_type == EXPR_VARIABLE
    && maskexpr->symtree->n.sym->attr.dummy
    && maskexpr->symtree->n.sym->attr.optional;
  backexpr = back_arg->expr;

  gfc_init_se (&backse, nested_loop ? se : nullptr);
  if (backexpr == nullptr)
    back = logical_false_node;
  else if (maybe_absent_optional_variable (backexpr))
    {
      /* This should have been checked already by
	 maybe_absent_optional_variable.  */
      gcc_checking_assert (backexpr->expr_type == EXPR_VARIABLE);

      gfc_conv_expr (&backse, backexpr);
      tree present = gfc_conv_expr_present (backexpr->symtree->n.sym, false);
      back = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			      logical_type_node, present, backse.expr);
    }
  else
    {
      gfc_conv_expr (&backse, backexpr);
      back = backse.expr;
    }
  gfc_add_block_to_block (&se->pre, &backse.pre);
  back = gfc_evaluate_now_loc (input_location, back, &se->pre);
  gfc_add_block_to_block (&se->pre, &backse.post);

  if (nested_loop)
    {
      gfc_init_se (&nested_se, se);
      base_se = &nested_se;
    }
  else
    {
      /* Walk the arguments.  */
      arrayss = gfc_walk_expr (arrayexpr);
      gcc_assert (arrayss != gfc_ss_terminator);

      if (maskexpr && maskexpr->rank != 0)
	{
	  maskss = gfc_walk_expr (maskexpr);
	  gcc_assert (maskss != gfc_ss_terminator);
	}

      base_se = nullptr;
    }

  nonempty = nullptr;
  if (!(maskexpr && maskexpr->rank > 0))
    {
      mpz_t asize;
      bool reduction_size_known;

      if (dim_present)
	{
	  int reduction_dim;
	  if (dim_arg->expr->expr_type == EXPR_CONSTANT)
	    reduction_dim = mpz_get_si (dim_arg->expr->value.integer) - 1;
	  else if (arrayexpr->rank == 1)
	    reduction_dim = 0;
	  else
	    gcc_unreachable ();
	  reduction_size_known = gfc_array_dimen_size (arrayexpr, reduction_dim,
						       &asize);
	}
      else
	reduction_size_known = gfc_array_size (arrayexpr, &asize);

      if (reduction_size_known)
	{
	  nonempty = gfc_conv_mpz_to_tree (asize, gfc_index_integer_kind);
	  mpz_clear (asize);
	  nonempty = fold_build2_loc (input_location, GT_EXPR,
				      logical_type_node, nonempty,
				      gfc_index_zero_node);
	}
      maskss = NULL;
    }

  limit = gfc_create_var (gfc_typenode_for_spec (&arrayexpr->ts), "limit");
  switch (arrayexpr->ts.type)
    {
    case BT_REAL:
      tmp = gfc_build_inf_or_huge (TREE_TYPE (limit), arrayexpr->ts.kind);
      break;

    case BT_INTEGER:
      n = gfc_validate_kind (arrayexpr->ts.type, arrayexpr->ts.kind, false);
      tmp = gfc_conv_mpz_to_tree (gfc_integer_kinds[n].huge,
				  arrayexpr->ts.kind);
      break;

    case BT_UNSIGNED:
      /* For MAXVAL, the minimum is zero, for MINVAL it is HUGE().  */
      if (op == GT_EXPR)
	{
	  tmp = gfc_get_unsigned_type (arrayexpr->ts.kind);
	  tmp = build_int_cst (tmp, 0);
	}
      else
	{
	  n = gfc_validate_kind (arrayexpr->ts.type, arrayexpr->ts.kind, false);
	  tmp = gfc_conv_mpz_unsigned_to_tree (gfc_unsigned_kinds[n].huge,
					       expr->ts.kind);
	}
      break;

    default:
      gcc_unreachable ();
    }

  /* We start with the most negative possible value for MAXLOC, and the most
     positive possible value for MINLOC. The most negative possible value is
     -HUGE for BT_REAL and (-HUGE - 1) for BT_INTEGER; the most positive
     possible value is HUGE in both cases.  BT_UNSIGNED has already been dealt
     with above.  */
  if (op == GT_EXPR && expr->ts.type != BT_UNSIGNED)
    tmp = fold_build1_loc (input_location, NEGATE_EXPR, TREE_TYPE (tmp), tmp);
  if (op == GT_EXPR && arrayexpr->ts.type == BT_INTEGER)
    tmp = fold_build2_loc (input_location, MINUS_EXPR, TREE_TYPE (tmp), tmp,
			   build_int_cst (TREE_TYPE (tmp), 1));

  gfc_add_modify (&se->pre, limit, tmp);

  /* If we are in a case where we generate two sets of loops, the second one
     should continue where the first stopped instead of restarting from the
     beginning.  So nested loops in the second set should have a partial range
     on the first iteration, but they should start from the beginning and span
     their full range on the following iterations.  So we use conditionals in
     the loops lower bounds, and use the following variable in those
     conditionals to decide whether to use the original loop bound or to use
     the index at which the loop from the first set stopped.  */
  tree second_loop_entry = gfc_create_var (logical_type_node,
					   "second_loop_entry");
  gfc_add_modify (&se->pre, second_loop_entry, logical_false_node);

  if (nested_loop)
    {
      ploop = enter_nested_loop (&nested_se);
      orig_ss = nested_se.ss;
      ploop->temp_dim = 1;
    }
  else
    {
      /* Initialize the scalarizer.  */
      gfc_init_loopinfo (&loop);

      /* We add the mask first because the number of iterations is taken
	 from the last ss, and this breaks if an absent optional argument
	 is used for mask.  */

      if (maskss)
	gfc_add_ss_to_loop (&loop, maskss);

      gfc_add_ss_to_loop (&loop, arrayss);

      /* Initialize the loop.  */
      gfc_conv_ss_startstride (&loop);

      /* The code generated can have more than one loop in sequence (see the
	 comment at the function header).  This doesn't work well with the
	 scalarizer, which changes arrays' offset when the scalarization loops
	 are generated (see gfc_trans_preloop_setup).  Fortunately, we can use
	 the scalarizer temporary code to handle multiple loops.  Thus, we set
	 temp_dim here, we call gfc_mark_ss_chain_used with flag=3 later, and
	 we use gfc_trans_scalarized_loop_boundary even later to restore
	 offset.  */
      loop.temp_dim = loop.dimen;
      gfc_conv_loop_setup (&loop, &expr->where);

      ploop = &loop;
    }

  gcc_assert (reduction_dimensions == ploop->dimen);

  if (nonempty == NULL && !(maskexpr && maskexpr->rank > 0))
    {
      nonempty = logical_true_node;

      for (int i = 0; i < ploop->dimen; i++)
	{
	  if (!(ploop->from[i] && ploop->to[i]))
	    {
	      nonempty = NULL;
	      break;
	    }

	  tree tmp = fold_build2_loc (input_location, LE_EXPR,
				      logical_type_node, ploop->from[i],
				      ploop->to[i]);

	  nonempty = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
				      logical_type_node, nonempty, tmp);
	}
    }

  lab1 = NULL;
  lab2 = NULL;
  /* Initialize the position to zero, following Fortran 2003.  We are free
     to do this because Fortran 95 allows the result of an entirely false
     mask to be processor dependent.  If we know at compile time the array
     is non-empty and no MASK is used, we can initialize to 1 to simplify
     the inner loop.  */
  if (nonempty != NULL && !HONOR_NANS (DECL_MODE (limit)))
    {
      tree init = fold_build3_loc (input_location, COND_EXPR,
				   gfc_array_index_type, nonempty,
				   gfc_index_one_node,
				   gfc_index_zero_node);
      for (int i = 0; i < ploop->dimen; i++)
	gfc_add_modify (&ploop->pre, pos[i], init);
    }
  else
    {
      for (int i = 0; i < ploop->dimen; i++)
	gfc_add_modify (&ploop->pre, pos[i], gfc_index_zero_node);
      lab1 = gfc_build_label_decl (NULL_TREE);
      TREE_USED (lab1) = 1;
      lab2 = gfc_build_label_decl (NULL_TREE);
      TREE_USED (lab2) = 1;
    }

  /* An offset must be added to the loop
     counter to obtain the required position.  */
  for (int i = 0; i < ploop->dimen; i++)
    {
      gcc_assert (ploop->from[i]);

      tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			     gfc_index_one_node, ploop->from[i]);
      gfc_add_modify (&ploop->pre, offset[i], tmp);
    }

  if (!nested_loop)
    {
      gfc_mark_ss_chain_used (arrayss, lab1 ? 3 : 1);
      if (maskss)
	gfc_mark_ss_chain_used (maskss, lab1 ? 3 : 1);
    }

  /* Generate the loop body.  */
  gfc_start_scalarized_body (ploop, &body);

  /* If we have a mask, only check this element if the mask is set.  */
  if (maskexpr && maskexpr->rank > 0)
    {
      gfc_init_se (&maskse, base_se);
      gfc_copy_loopinfo_to_se (&maskse, ploop);
      if (!nested_loop)
	maskse.ss = maskss;
      gfc_conv_expr_val (&maskse, maskexpr);
      gfc_add_block_to_block (&body, &maskse.pre);

      gfc_start_block (&block);
    }
  else
    gfc_init_block (&block);

  /* Compare with the current limit.  */
  gfc_init_se (&arrayse, base_se);
  gfc_copy_loopinfo_to_se (&arrayse, ploop);
  if (!nested_loop)
    arrayse.ss = arrayss;
  gfc_conv_expr_val (&arrayse, arrayexpr);
  gfc_add_block_to_block (&block, &arrayse.pre);

  /* We do the following if this is a more extreme value.  */
  gfc_start_block (&ifblock);

  /* Assign the value to the limit...  */
  gfc_add_modify (&ifblock, limit, arrayse.expr);

  if (nonempty == NULL && HONOR_NANS (DECL_MODE (limit)))
    {
      stmtblock_t ifblock2;
      tree ifbody2;

      gfc_start_block (&ifblock2);
      for (int i = 0; i < ploop->dimen; i++)
	{
	  tmp = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (pos[i]),
				 ploop->loopvar[i], offset[i]);
	  gfc_add_modify (&ifblock2, pos[i], tmp);
	}
      ifbody2 = gfc_finish_block (&ifblock2);

      cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			      pos[0], gfc_index_zero_node);
      tmp = build3_v (COND_EXPR, cond, ifbody2,
		      build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);
    }

  for (int i = 0; i < ploop->dimen; i++)
    {
      tmp = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (pos[i]),
			     ploop->loopvar[i], offset[i]);
      gfc_add_modify (&ifblock, pos[i], tmp);
      gfc_add_modify (&ifblock, idx[i], ploop->loopvar[i]);
    }

  gfc_add_modify (&ifblock, second_loop_entry, logical_true_node);

  if (lab1)
    gfc_add_expr_to_block (&ifblock, build1_v (GOTO_EXPR, lab1));

  ifbody = gfc_finish_block (&ifblock);

  if (!lab1 || HONOR_NANS (DECL_MODE (limit)))
    {
      if (lab1)
	cond = fold_build2_loc (input_location,
				op == GT_EXPR ? GE_EXPR : LE_EXPR,
				logical_type_node, arrayse.expr, limit);
      else
	{
	  tree ifbody2, elsebody2;

	  /* We switch to > or >= depending on the value of the BACK argument. */
	  cond = gfc_create_var (logical_type_node, "cond");

	  gfc_start_block (&ifblock);
	  b_if = fold_build2_loc (input_location, op == GT_EXPR ? GE_EXPR : LE_EXPR,
				  logical_type_node, arrayse.expr, limit);

	  gfc_add_modify (&ifblock, cond, b_if);
	  ifbody2 = gfc_finish_block (&ifblock);

	  gfc_start_block (&elseblock);
	  b_else = fold_build2_loc (input_location, op, logical_type_node,
				    arrayse.expr, limit);

	  gfc_add_modify (&elseblock, cond, b_else);
	  elsebody2 = gfc_finish_block (&elseblock);

	  tmp = fold_build3_loc (input_location, COND_EXPR, logical_type_node,
				 back, ifbody2, elsebody2);

	  gfc_add_expr_to_block (&block, tmp);
	}

      cond = gfc_unlikely (cond, PRED_BUILTIN_EXPECT);
      ifbody = build3_v (COND_EXPR, cond, ifbody,
			 build_empty_stmt (input_location));
    }
  gfc_add_expr_to_block (&block, ifbody);

  if (maskexpr && maskexpr->rank > 0)
    {
      /* We enclose the above in if (mask) {...}.  If the mask is an
	 optional argument, generate IF (.NOT. PRESENT(MASK)
	 .OR. MASK(I)). */

      tree ifmask;
      ifmask = conv_mask_condition (&maskse, maskexpr, optional_mask);
      tmp = gfc_finish_block (&block);
      tmp = build3_v (COND_EXPR, ifmask, tmp,
		      build_empty_stmt (input_location));
    }
  else
    tmp = gfc_finish_block (&block);
  gfc_add_expr_to_block (&body, tmp);

  if (lab1)
    {
      for (int i = 0; i < ploop->dimen; i++)
	ploop->from[i] = fold_build3_loc (input_location, COND_EXPR,
					  TREE_TYPE (ploop->from[i]),
					  second_loop_entry, idx[i],
					  ploop->from[i]);

      gfc_trans_scalarized_loop_boundary (ploop, &body);

      if (nested_loop)
	{
	  /* The first loop already advanced the parent se'ss chain, so clear
	     the parent now to avoid doing it a second time, making the chain
	     out of sync.  */
	  nested_se.parent = nullptr;
	  nested_se.ss = orig_ss;
	}

      stmtblock_t * const outer_block = &ploop->code[ploop->dimen - 1];

      if (HONOR_NANS (DECL_MODE (limit)))
	{
	  if (nonempty != NULL)
	    {
	      stmtblock_t init_block;
	      gfc_init_block (&init_block);

	      for (int i = 0; i < ploop->dimen; i++)
		gfc_add_modify (&init_block, pos[i], gfc_index_one_node);

	      tree ifbody = gfc_finish_block (&init_block);
	      tmp = build3_v (COND_EXPR, nonempty, ifbody,
			      build_empty_stmt (input_location));
	      gfc_add_expr_to_block (outer_block, tmp);
	    }
	}

      gfc_add_expr_to_block (outer_block, build1_v (GOTO_EXPR, lab2));
      gfc_add_expr_to_block (outer_block, build1_v (LABEL_EXPR, lab1));

      /* If we have a mask, only check this element if the mask is set.  */
      if (maskexpr && maskexpr->rank > 0)
	{
	  gfc_init_se (&maskse, base_se);
	  gfc_copy_loopinfo_to_se (&maskse, ploop);
	  if (!nested_loop)
	    maskse.ss = maskss;
	  gfc_conv_expr_val (&maskse, maskexpr);
	  gfc_add_block_to_block (&body, &maskse.pre);

	  gfc_start_block (&block);
	}
      else
	gfc_init_block (&block);

      /* Compare with the current limit.  */
      gfc_init_se (&arrayse, base_se);
      gfc_copy_loopinfo_to_se (&arrayse, ploop);
      if (!nested_loop)
	arrayse.ss = arrayss;
      gfc_conv_expr_val (&arrayse, arrayexpr);
      gfc_add_block_to_block (&block, &arrayse.pre);

      /* We do the following if this is a more extreme value.  */
      gfc_start_block (&ifblock);

      /* Assign the value to the limit...  */
      gfc_add_modify (&ifblock, limit, arrayse.expr);

      for (int i = 0; i < ploop->dimen; i++)
	{
	  tmp = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (pos[i]),
				 ploop->loopvar[i], offset[i]);
	  gfc_add_modify (&ifblock, pos[i], tmp);
	}

      ifbody = gfc_finish_block (&ifblock);

      /* We switch to > or >= depending on the value of the BACK argument. */
      {
	tree ifbody2, elsebody2;

	cond = gfc_create_var (logical_type_node, "cond");

	gfc_start_block (&ifblock);
	b_if = fold_build2_loc (input_location, op == GT_EXPR ? GE_EXPR : LE_EXPR,
				logical_type_node, arrayse.expr, limit);

	gfc_add_modify (&ifblock, cond, b_if);
	ifbody2 = gfc_finish_block (&ifblock);

	gfc_start_block (&elseblock);
	b_else = fold_build2_loc (input_location, op, logical_type_node,
				  arrayse.expr, limit);

	gfc_add_modify (&elseblock, cond, b_else);
	elsebody2 = gfc_finish_block (&elseblock);

	tmp = fold_build3_loc (input_location, COND_EXPR, logical_type_node,
			       back, ifbody2, elsebody2);
      }

      gfc_add_expr_to_block (&block, tmp);
      cond = gfc_unlikely (cond, PRED_BUILTIN_EXPECT);
      tmp = build3_v (COND_EXPR, cond, ifbody,
		      build_empty_stmt (input_location));

      gfc_add_expr_to_block (&block, tmp);

      if (maskexpr && maskexpr->rank > 0)
	{
	  /* We enclose the above in if (mask) {...}.  If the mask is
	 an optional argument, generate IF (.NOT. PRESENT(MASK)
	 .OR. MASK(I)).*/

	  tree ifmask;
	  ifmask = conv_mask_condition (&maskse, maskexpr, optional_mask);
	  tmp = gfc_finish_block (&block);
	  tmp = build3_v (COND_EXPR, ifmask, tmp,
			  build_empty_stmt (input_location));
	}
      else
	tmp = gfc_finish_block (&block);

      gfc_add_expr_to_block (&body, tmp);
      gfc_add_modify (&body, second_loop_entry, logical_false_node);
    }

  gfc_trans_scalarizing_loops (ploop, &body);

  if (lab2)
    gfc_add_expr_to_block (&ploop->pre, build1_v (LABEL_EXPR, lab2));

  /* For a scalar mask, enclose the loop in an if statement.  */
  if (maskexpr && maskexpr->rank == 0)
    {
      tree ifmask;

      gfc_init_se (&maskse, nested_loop ? se : nullptr);
      gfc_conv_expr_val (&maskse, maskexpr);
      gfc_add_block_to_block (&se->pre, &maskse.pre);
      gfc_init_block (&block);
      gfc_add_block_to_block (&block, &ploop->pre);
      gfc_add_block_to_block (&block, &ploop->post);
      tmp = gfc_finish_block (&block);

      /* For the else part of the scalar mask, just initialize
	 the pos variable the same way as above.  */

      gfc_init_block (&elseblock);
      for (int i = 0; i < ploop->dimen; i++)
	gfc_add_modify (&elseblock, pos[i], gfc_index_zero_node);
      elsetmp = gfc_finish_block (&elseblock);
      ifmask = conv_mask_condition (&maskse, maskexpr, optional_mask);
      tmp = build3_v (COND_EXPR, ifmask, tmp, elsetmp);
      gfc_add_expr_to_block (&block, tmp);
      gfc_add_block_to_block (&se->pre, &block);
    }
  else
    {
      gfc_add_block_to_block (&se->pre, &ploop->pre);
      gfc_add_block_to_block (&se->pre, &ploop->post);
    }

  if (!nested_loop)
    gfc_cleanup_loop (&loop);

  if (!dim_present)
    {
      for (int i = 0; i < arrayexpr->rank; i++)
	{
	  tree res_idx = build_int_cst (gfc_array_index_type, i);
	  tree res_arr_ref = gfc_build_array_ref (result_var, res_idx,
						  NULL_TREE, true);

	  tree value = convert (type, pos[i]);
	  gfc_add_modify (&se->pre, res_arr_ref, value);
	}

      se->expr = result_var;
    }
  else
    se->expr = convert (type, pos[0]);
}

/* Emit code for findloc.  */

static void
gfc_conv_intrinsic_findloc (gfc_se *se, gfc_expr *expr)
{
  gfc_actual_arglist *array_arg, *value_arg, *dim_arg, *mask_arg,
    *kind_arg, *back_arg;
  gfc_expr *value_expr;
  int ikind;
  tree resvar;
  stmtblock_t block;
  stmtblock_t body;
  stmtblock_t loopblock;
  tree type;
  tree tmp;
  tree found;
  tree forward_branch = NULL_TREE;
  tree back_branch;
  gfc_loopinfo loop;
  gfc_ss *arrayss;
  gfc_ss *maskss;
  gfc_se arrayse;
  gfc_se valuese;
  gfc_se maskse;
  gfc_se backse;
  tree exit_label;
  gfc_expr *maskexpr;
  tree offset;
  int i;
  bool optional_mask;

  array_arg = expr->value.function.actual;
  value_arg = array_arg->next;
  dim_arg   = value_arg->next;
  mask_arg  = dim_arg->next;
  kind_arg  = mask_arg->next;
  back_arg  = kind_arg->next;

  /* Remove kind and set ikind.  */
  if (kind_arg->expr)
    {
      ikind = mpz_get_si (kind_arg->expr->value.integer);
      gfc_free_expr (kind_arg->expr);
      kind_arg->expr = NULL;
    }
  else
    ikind = gfc_default_integer_kind;

  value_expr = value_arg->expr;

  /* Unless it's a string, pass VALUE by value.  */
  if (value_expr->ts.type != BT_CHARACTER)
    value_arg->name = "%VAL";

  /* Pass BACK argument by value.  */
  back_arg->name = "%VAL";

  /* Call the library if we have a character function or if
     rank > 0.  */
  if (se->ss || array_arg->expr->ts.type == BT_CHARACTER)
    {
      se->ignore_optional = 1;
      if (expr->rank == 0)
	{
	  /* Remove dim argument.  */
	  gfc_free_expr (dim_arg->expr);
	  dim_arg->expr = NULL;
	}
      gfc_conv_intrinsic_funcall (se, expr);
      return;
    }

  type = gfc_get_int_type (ikind);

  /* Initialize the result.  */
  resvar = gfc_create_var (gfc_array_index_type, "pos");
  gfc_add_modify (&se->pre, resvar, build_int_cst (gfc_array_index_type, 0));
  offset = gfc_create_var (gfc_array_index_type, "offset");

  maskexpr = mask_arg->expr;
  optional_mask = maskexpr && maskexpr->expr_type == EXPR_VARIABLE
    && maskexpr->symtree->n.sym->attr.dummy
    && maskexpr->symtree->n.sym->attr.optional;

  /*  Generate two loops, one for BACK=.true. and one for BACK=.false.  */

  for (i = 0 ; i < 2; i++)
    {
      /* Walk the arguments.  */
      arrayss = gfc_walk_expr (array_arg->expr);
      gcc_assert (arrayss != gfc_ss_terminator);

      if (maskexpr && maskexpr->rank != 0)
	{
	  maskss = gfc_walk_expr (maskexpr);
	  gcc_assert (maskss != gfc_ss_terminator);
	}
      else
	maskss = NULL;

      /* Initialize the scalarizer.  */
      gfc_init_loopinfo (&loop);
      exit_label = gfc_build_label_decl (NULL_TREE);
      TREE_USED (exit_label) = 1;

      /* We add the mask first because the number of iterations is
	 taken from the last ss, and this breaks if an absent
	 optional argument is used for mask.  */

      if (maskss)
	gfc_add_ss_to_loop (&loop, maskss);
      gfc_add_ss_to_loop (&loop, arrayss);

      /* Initialize the loop.  */
      gfc_conv_ss_startstride (&loop);
      gfc_conv_loop_setup (&loop, &expr->where);

      /* Calculate the offset.  */
      tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			     gfc_index_one_node, loop.from[0]);
      gfc_add_modify (&loop.pre, offset, tmp);

      gfc_mark_ss_chain_used (arrayss, 1);
      if (maskss)
	gfc_mark_ss_chain_used (maskss, 1);

      /* The first loop is for BACK=.true.  */
      if (i == 0)
	loop.reverse[0] = GFC_REVERSE_SET;

      /* Generate the loop body.  */
      gfc_start_scalarized_body (&loop, &body);

      /* If we have an array mask, only add the element if it is
	 set.  */
      if (maskss)
	{
	  gfc_init_se (&maskse, NULL);
	  gfc_copy_loopinfo_to_se (&maskse, &loop);
	  maskse.ss = maskss;
	  gfc_conv_expr_val (&maskse, maskexpr);
	  gfc_add_block_to_block (&body, &maskse.pre);
	}

      /* If the condition matches then set the return value.  */
      gfc_start_block (&block);

      /* Add the offset.  */
      tmp = fold_build2_loc (input_location, PLUS_EXPR,
			     TREE_TYPE (resvar),
			     loop.loopvar[0], offset);
      gfc_add_modify (&block, resvar, tmp);
      /* And break out of the loop.  */
      tmp = build1_v (GOTO_EXPR, exit_label);
      gfc_add_expr_to_block (&block, tmp);

      found = gfc_finish_block (&block);

      /* Check this element.  */
      gfc_init_se (&arrayse, NULL);
      gfc_copy_loopinfo_to_se (&arrayse, &loop);
      arrayse.ss = arrayss;
      gfc_conv_expr_val (&arrayse, array_arg->expr);
      gfc_add_block_to_block (&body, &arrayse.pre);

      gfc_init_se (&valuese, NULL);
      gfc_conv_expr_val (&valuese, value_arg->expr);
      gfc_add_block_to_block (&body, &valuese.pre);

      tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			     arrayse.expr, valuese.expr);

      tmp = build3_v (COND_EXPR, tmp, found, build_empty_stmt (input_location));
      if (maskss)
	{
	  /* We enclose the above in if (mask) {...}.  If the mask is
	     an optional argument, generate IF (.NOT. PRESENT(MASK)
	     .OR. MASK(I)). */

	  tree ifmask;
	  ifmask = conv_mask_condition (&maskse, maskexpr, optional_mask);
	  tmp = build3_v (COND_EXPR, ifmask, tmp,
			  build_empty_stmt (input_location));
	}

      gfc_add_expr_to_block (&body, tmp);
      gfc_add_block_to_block (&body, &arrayse.post);

      gfc_trans_scalarizing_loops (&loop, &body);

      /* Add the exit label.  */
      tmp = build1_v (LABEL_EXPR, exit_label);
      gfc_add_expr_to_block (&loop.pre, tmp);
      gfc_start_block (&loopblock);
      gfc_add_block_to_block (&loopblock, &loop.pre);
      gfc_add_block_to_block (&loopblock, &loop.post);
      if (i == 0)
	forward_branch = gfc_finish_block (&loopblock);
      else
	back_branch = gfc_finish_block (&loopblock);

      gfc_cleanup_loop (&loop);
    }

  /* Enclose the two loops in an IF statement.  */

  gfc_init_se (&backse, NULL);
  gfc_conv_expr_val (&backse, back_arg->expr);
  gfc_add_block_to_block (&se->pre, &backse.pre);
  tmp = build3_v (COND_EXPR, backse.expr, forward_branch, back_branch);

  /* For a scalar mask, enclose the loop in an if statement.  */
  if (maskexpr && maskss == NULL)
    {
      tree ifmask;
      tree if_stmt;

      gfc_init_se (&maskse, NULL);
      gfc_conv_expr_val (&maskse, maskexpr);
      gfc_init_block (&block);
      gfc_add_expr_to_block (&block, maskse.expr);
      ifmask = conv_mask_condition (&maskse, maskexpr, optional_mask);
      if_stmt = build3_v (COND_EXPR, ifmask, tmp,
			  build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, if_stmt);
      tmp = gfc_finish_block (&block);
    }

  gfc_add_expr_to_block (&se->pre, tmp);
  se->expr = convert (type, resvar);

}

/* Emit code for minval or maxval intrinsic.  There are many different cases
   we need to handle.  For performance reasons we sometimes create two
   loops instead of one, where the second one is much simpler.
   Examples for minval intrinsic:
   1) Result is an array, a call is generated
   2) Array mask is used and NaNs need to be supported, rank 1:
      limit = Infinity;
      nonempty = false;
      S = from;
      while (S <= to) {
	if (mask[S]) {
	  nonempty = true;
	  if (a[S] <= limit) {
	    limit = a[S];
	    S++;
	    goto lab;
	  }
	else
	  S++;
	}
      }
      limit = nonempty ? NaN : huge (limit);
      lab:
      while (S <= to) { if(mask[S]) limit = min (a[S], limit); S++; }
   3) NaNs need to be supported, but it is known at compile time or cheaply
      at runtime whether array is nonempty or not, rank 1:
      limit = Infinity;
      S = from;
      while (S <= to) {
	if (a[S] <= limit) {
	  limit = a[S];
	  S++;
	  goto lab;
	  }
	else
	  S++;
      }
      limit = (from <= to) ? NaN : huge (limit);
      lab:
      while (S <= to) { limit = min (a[S], limit); S++; }
   4) Array mask is used and NaNs need to be supported, rank > 1:
      limit = Infinity;
      nonempty = false;
      fast = false;
      S1 = from1;
      while (S1 <= to1) {
	S2 = from2;
	while (S2 <= to2) {
	  if (mask[S1][S2]) {
	    if (fast) limit = min (a[S1][S2], limit);
	    else {
	      nonempty = true;
	      if (a[S1][S2] <= limit) {
		limit = a[S1][S2];
		fast = true;
	      }
	    }
	  }
	  S2++;
	}
	S1++;
      }
      if (!fast)
	limit = nonempty ? NaN : huge (limit);
   5) NaNs need to be supported, but it is known at compile time or cheaply
      at runtime whether array is nonempty or not, rank > 1:
      limit = Infinity;
      fast = false;
      S1 = from1;
      while (S1 <= to1) {
	S2 = from2;
	while (S2 <= to2) {
	  if (fast) limit = min (a[S1][S2], limit);
	  else {
	    if (a[S1][S2] <= limit) {
	      limit = a[S1][S2];
	      fast = true;
	    }
	  }
	  S2++;
	}
	S1++;
      }
      if (!fast)
	limit = (nonempty_array) ? NaN : huge (limit);
   6) NaNs aren't supported, but infinities are.  Array mask is used:
      limit = Infinity;
      nonempty = false;
      S = from;
      while (S <= to) {
	if (mask[S]) { nonempty = true; limit = min (a[S], limit); }
	S++;
      }
      limit = nonempty ? limit : huge (limit);
   7) Same without array mask:
      limit = Infinity;
      S = from;
      while (S <= to) { limit = min (a[S], limit); S++; }
      limit = (from <= to) ? limit : huge (limit);
   8) Neither NaNs nor infinities are supported (-ffast-math or BT_INTEGER):
      limit = huge (limit);
      S = from;
      while (S <= to) { limit = min (a[S], limit); S++); }
      (or
      while (S <= to) { if (mask[S]) limit = min (a[S], limit); S++; }
      with array mask instead).
   For 3), 5), 7) and 8), if mask is scalar, this all goes into a conditional,
   setting limit = huge (limit); in the else branch.  */

static void
gfc_conv_intrinsic_minmaxval (gfc_se * se, gfc_expr * expr, enum tree_code op)
{
  tree limit;
  tree type;
  tree tmp;
  tree ifbody;
  tree nonempty;
  tree nonempty_var;
  tree lab;
  tree fast;
  tree huge_cst = NULL, nan_cst = NULL;
  stmtblock_t body;
  stmtblock_t block, block2;
  gfc_loopinfo loop;
  gfc_actual_arglist *actual;
  gfc_ss *arrayss;
  gfc_ss *maskss;
  gfc_se arrayse;
  gfc_se maskse;
  gfc_expr *arrayexpr;
  gfc_expr *maskexpr;
  int n;
  bool optional_mask;

  if (se->ss)
    {
      gfc_conv_intrinsic_funcall (se, expr);
      return;
    }

  actual = expr->value.function.actual;
  arrayexpr = actual->expr;

  if (arrayexpr->ts.type == BT_CHARACTER)
    {
      gfc_actual_arglist *dim = actual->next;
      if (expr->rank == 0 && dim->expr != 0)
	{
	  gfc_free_expr (dim->expr);
	  dim->expr = NULL;
	}
      gfc_conv_intrinsic_funcall (se, expr);
      return;
    }

  type = gfc_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  limit = gfc_create_var (type, "limit");
  n = gfc_validate_kind (expr->ts.type, expr->ts.kind, false);
  switch (expr->ts.type)
    {
    case BT_REAL:
      huge_cst = gfc_conv_mpfr_to_tree (gfc_real_kinds[n].huge,
					expr->ts.kind, 0);
      if (HONOR_INFINITIES (DECL_MODE (limit)))
	{
	  REAL_VALUE_TYPE real;
	  real_inf (&real);
	  tmp = build_real (type, real);
	}
      else
	tmp = huge_cst;
      if (HONOR_NANS (DECL_MODE (limit)))
	nan_cst = gfc_build_nan (type, "");
      break;

    case BT_INTEGER:
      tmp = gfc_conv_mpz_to_tree (gfc_integer_kinds[n].huge, expr->ts.kind);
      break;

    case BT_UNSIGNED:
      /* For MAXVAL, the minimum is zero, for MINVAL it is HUGE().  */
      if (op == GT_EXPR)
	tmp = build_int_cst (type, 0);
      else
	tmp = gfc_conv_mpz_unsigned_to_tree (gfc_unsigned_kinds[n].huge,
					     expr->ts.kind);
      break;

    default:
      gcc_unreachable ();
    }

  /* We start with the most negative possible value for MAXVAL, and the most
     positive possible value for MINVAL. The most negative possible value is
     -HUGE for BT_REAL and (-HUGE - 1) for BT_INTEGER; the most positive
     possible value is HUGE in both cases.   BT_UNSIGNED has already been dealt
     with above.  */
  if (op == GT_EXPR && expr->ts.type != BT_UNSIGNED)
    {
      tmp = fold_build1_loc (input_location, NEGATE_EXPR, TREE_TYPE (tmp), tmp);
      if (huge_cst)
	huge_cst = fold_build1_loc (input_location, NEGATE_EXPR,
				    TREE_TYPE (huge_cst), huge_cst);
    }

  if (op == GT_EXPR && expr->ts.type == BT_INTEGER)
    tmp = fold_build2_loc (input_location, MINUS_EXPR, TREE_TYPE (tmp),
			   tmp, build_int_cst (type, 1));

  gfc_add_modify (&se->pre, limit, tmp);

  /* Walk the arguments.  */
  arrayss = gfc_walk_expr (arrayexpr);
  gcc_assert (arrayss != gfc_ss_terminator);

  actual = actual->next->next;
  gcc_assert (actual);
  maskexpr = actual->expr;
  optional_mask = maskexpr && maskexpr->expr_type == EXPR_VARIABLE
    && maskexpr->symtree->n.sym->attr.dummy
    && maskexpr->symtree->n.sym->attr.optional;
  nonempty = NULL;
  if (maskexpr && maskexpr->rank != 0)
    {
      maskss = gfc_walk_expr (maskexpr);
      gcc_assert (maskss != gfc_ss_terminator);
    }
  else
    {
      mpz_t asize;
      if (gfc_array_size (arrayexpr, &asize))
	{
	  nonempty = gfc_conv_mpz_to_tree (asize, gfc_index_integer_kind);
	  mpz_clear (asize);
	  nonempty = fold_build2_loc (input_location, GT_EXPR,
				      logical_type_node, nonempty,
				      gfc_index_zero_node);
	}
      maskss = NULL;
    }

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);

  /* We add the mask first because the number of iterations is taken
     from the last ss, and this breaks if an absent optional argument
     is used for mask.  */

  if (maskss)
    gfc_add_ss_to_loop (&loop, maskss);
  gfc_add_ss_to_loop (&loop, arrayss);

  /* Initialize the loop.  */
  gfc_conv_ss_startstride (&loop);

  /* The code generated can have more than one loop in sequence (see the
     comment at the function header).  This doesn't work well with the
     scalarizer, which changes arrays' offset when the scalarization loops
     are generated (see gfc_trans_preloop_setup).  Fortunately, {min,max}val
     are  currently inlined in the scalar case only.  As there is no dependency
     to care about in that case, there is no temporary, so that we can use the
     scalarizer temporary code to handle multiple loops.  Thus, we set temp_dim
     here, we call gfc_mark_ss_chain_used with flag=3 later, and we use
     gfc_trans_scalarized_loop_boundary even later to restore offset.
     TODO: this prevents inlining of rank > 0 minmaxval calls, so this
     should eventually go away.  We could either create two loops properly,
     or find another way to save/restore the array offsets between the two
     loops (without conflicting with temporary management), or use a single
     loop minmaxval implementation.  See PR 31067.  */
  loop.temp_dim = loop.dimen;
  gfc_conv_loop_setup (&loop, &expr->where);

  if (nonempty == NULL && maskss == NULL
      && loop.dimen == 1 && loop.from[0] && loop.to[0])
    nonempty = fold_build2_loc (input_location, LE_EXPR, logical_type_node,
				loop.from[0], loop.to[0]);
  nonempty_var = NULL;
  if (nonempty == NULL
      && (HONOR_INFINITIES (DECL_MODE (limit))
	  || HONOR_NANS (DECL_MODE (limit))))
    {
      nonempty_var = gfc_create_var (logical_type_node, "nonempty");
      gfc_add_modify (&se->pre, nonempty_var, logical_false_node);
      nonempty = nonempty_var;
    }
  lab = NULL;
  fast = NULL;
  if (HONOR_NANS (DECL_MODE (limit)))
    {
      if (loop.dimen == 1)
	{
	  lab = gfc_build_label_decl (NULL_TREE);
	  TREE_USED (lab) = 1;
	}
      else
	{
	  fast = gfc_create_var (logical_type_node, "fast");
	  gfc_add_modify (&se->pre, fast, logical_false_node);
	}
    }

  gfc_mark_ss_chain_used (arrayss, lab ? 3 : 1);
  if (maskss)
    gfc_mark_ss_chain_used (maskss, lab ? 3 : 1);
  /* Generate the loop body.  */
  gfc_start_scalarized_body (&loop, &body);

  /* If we have a mask, only add this element if the mask is set.  */
  if (maskss)
    {
      gfc_init_se (&maskse, NULL);
      gfc_copy_loopinfo_to_se (&maskse, &loop);
      maskse.ss = maskss;
      gfc_conv_expr_val (&maskse, maskexpr);
      gfc_add_block_to_block (&body, &maskse.pre);

      gfc_start_block (&block);
    }
  else
    gfc_init_block (&block);

  /* Compare with the current limit.  */
  gfc_init_se (&arrayse, NULL);
  gfc_copy_loopinfo_to_se (&arrayse, &loop);
  arrayse.ss = arrayss;
  gfc_conv_expr_val (&arrayse, arrayexpr);
  arrayse.expr = gfc_evaluate_now (arrayse.expr, &arrayse.pre);
  gfc_add_block_to_block (&block, &arrayse.pre);

  gfc_init_block (&block2);

  if (nonempty_var)
    gfc_add_modify (&block2, nonempty_var, logical_true_node);

  if (HONOR_NANS (DECL_MODE (limit)))
    {
      tmp = fold_build2_loc (input_location, op == GT_EXPR ? GE_EXPR : LE_EXPR,
			     logical_type_node, arrayse.expr, limit);
      if (lab)
	{
	  stmtblock_t ifblock;
	  tree inc_loop;
	  inc_loop = fold_build2_loc (input_location, PLUS_EXPR,
				      TREE_TYPE (loop.loopvar[0]),
				      loop.loopvar[0], gfc_index_one_node);
	  gfc_init_block (&ifblock);
	  gfc_add_modify (&ifblock, limit, arrayse.expr);
	  gfc_add_modify (&ifblock, loop.loopvar[0], inc_loop);
	  gfc_add_expr_to_block (&ifblock, build1_v (GOTO_EXPR, lab));
	  ifbody = gfc_finish_block (&ifblock);
	}
      else
	{
	  stmtblock_t ifblock;

	  gfc_init_block (&ifblock);
	  gfc_add_modify (&ifblock, limit, arrayse.expr);
	  gfc_add_modify (&ifblock, fast, logical_true_node);
	  ifbody = gfc_finish_block (&ifblock);
	}
      tmp = build3_v (COND_EXPR, tmp, ifbody,
		      build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block2, tmp);
    }
  else
    {
      /* MIN_EXPR/MAX_EXPR has unspecified behavior with NaNs or
	 signed zeros.  */
      tmp = fold_build2_loc (input_location,
			     op == GT_EXPR ? MAX_EXPR : MIN_EXPR,
			     type, arrayse.expr, limit);
      gfc_add_modify (&block2, limit, tmp);
    }

  if (fast)
    {
      tree elsebody = gfc_finish_block (&block2);

      /* MIN_EXPR/MAX_EXPR has unspecified behavior with NaNs or
	 signed zeros.  */
      if (HONOR_NANS (DECL_MODE (limit)))
	{
	  tmp = fold_build2_loc (input_location, op, logical_type_node,
				 arrayse.expr, limit);
	  ifbody = build2_v (MODIFY_EXPR, limit, arrayse.expr);
	  ifbody = build3_v (COND_EXPR, tmp, ifbody,
			     build_empty_stmt (input_location));
	}
      else
	{
	  tmp = fold_build2_loc (input_location,
				 op == GT_EXPR ? MAX_EXPR : MIN_EXPR,
				 type, arrayse.expr, limit);
	  ifbody = build2_v (MODIFY_EXPR, limit, tmp);
	}
      tmp = build3_v (COND_EXPR, fast, ifbody, elsebody);
      gfc_add_expr_to_block (&block, tmp);
    }
  else
    gfc_add_block_to_block (&block, &block2);

  gfc_add_block_to_block (&block, &arrayse.post);

  tmp = gfc_finish_block (&block);
  if (maskss)
    {
      /* We enclose the above in if (mask) {...}.  If the mask is an
	 optional argument, generate IF (.NOT. PRESENT(MASK)
	 .OR. MASK(I)).  */
      tree ifmask;
      ifmask = conv_mask_condition (&maskse, maskexpr, optional_mask);
      tmp = build3_v (COND_EXPR, ifmask, tmp,
		      build_empty_stmt (input_location));
    }
  gfc_add_expr_to_block (&body, tmp);

  if (lab)
    {
      gfc_trans_scalarized_loop_boundary (&loop, &body);

      tmp = fold_build3_loc (input_location, COND_EXPR, type, nonempty,
			     nan_cst, huge_cst);
      gfc_add_modify (&loop.code[0], limit, tmp);
      gfc_add_expr_to_block (&loop.code[0], build1_v (LABEL_EXPR, lab));

      /* If we have a mask, only add this element if the mask is set.  */
      if (maskss)
	{
	  gfc_init_se (&maskse, NULL);
	  gfc_copy_loopinfo_to_se (&maskse, &loop);
	  maskse.ss = maskss;
	  gfc_conv_expr_val (&maskse, maskexpr);
	  gfc_add_block_to_block (&body, &maskse.pre);

	  gfc_start_block (&block);
	}
      else
	gfc_init_block (&block);

      /* Compare with the current limit.  */
      gfc_init_se (&arrayse, NULL);
      gfc_copy_loopinfo_to_se (&arrayse, &loop);
      arrayse.ss = arrayss;
      gfc_conv_expr_val (&arrayse, arrayexpr);
      arrayse.expr = gfc_evaluate_now (arrayse.expr, &arrayse.pre);
      gfc_add_block_to_block (&block, &arrayse.pre);

      /* MIN_EXPR/MAX_EXPR has unspecified behavior with NaNs or
	 signed zeros.  */
      if (HONOR_NANS (DECL_MODE (limit)))
	{
	  tmp = fold_build2_loc (input_location, op, logical_type_node,
				 arrayse.expr, limit);
	  ifbody = build2_v (MODIFY_EXPR, limit, arrayse.expr);
	  tmp = build3_v (COND_EXPR, tmp, ifbody,
			  build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&block, tmp);
	}
      else
	{
	  tmp = fold_build2_loc (input_location,
				 op == GT_EXPR ? MAX_EXPR : MIN_EXPR,
				 type, arrayse.expr, limit);
	  gfc_add_modify (&block, limit, tmp);
	}

      gfc_add_block_to_block (&block, &arrayse.post);

      tmp = gfc_finish_block (&block);
      if (maskss)
	/* We enclose the above in if (mask) {...}.  */
	{
	  tree ifmask;
	  ifmask = conv_mask_condition (&maskse, maskexpr, optional_mask);
	  tmp = build3_v (COND_EXPR, ifmask, tmp,
			  build_empty_stmt (input_location));
	}

      gfc_add_expr_to_block (&body, tmp);
      /* Avoid initializing loopvar[0] again, it should be left where
	 it finished by the first loop.  */
      loop.from[0] = loop.loopvar[0];
    }
  gfc_trans_scalarizing_loops (&loop, &body);

  if (fast)
    {
      tmp = fold_build3_loc (input_location, COND_EXPR, type, nonempty,
			     nan_cst, huge_cst);
      ifbody = build2_v (MODIFY_EXPR, limit, tmp);
      tmp = build3_v (COND_EXPR, fast, build_empty_stmt (input_location),
		      ifbody);
      gfc_add_expr_to_block (&loop.pre, tmp);
    }
  else if (HONOR_INFINITIES (DECL_MODE (limit)) && !lab)
    {
      tmp = fold_build3_loc (input_location, COND_EXPR, type, nonempty, limit,
			     huge_cst);
      gfc_add_modify (&loop.pre, limit, tmp);
    }

  /* For a scalar mask, enclose the loop in an if statement.  */
  if (maskexpr && maskss == NULL)
    {
      tree else_stmt;
      tree ifmask;

      gfc_init_se (&maskse, NULL);
      gfc_conv_expr_val (&maskse, maskexpr);
      gfc_init_block (&block);
      gfc_add_block_to_block (&block, &loop.pre);
      gfc_add_block_to_block (&block, &loop.post);
      tmp = gfc_finish_block (&block);

      if (HONOR_INFINITIES (DECL_MODE (limit)))
	else_stmt = build2_v (MODIFY_EXPR, limit, huge_cst);
      else
	else_stmt = build_empty_stmt (input_location);

      ifmask = conv_mask_condition (&maskse, maskexpr, optional_mask);
      tmp = build3_v (COND_EXPR, ifmask, tmp, else_stmt);
      gfc_add_expr_to_block (&block, tmp);
      gfc_add_block_to_block (&se->pre, &block);
    }
  else
    {
      gfc_add_block_to_block (&se->pre, &loop.pre);
      gfc_add_block_to_block (&se->pre, &loop.post);
    }

  gfc_cleanup_loop (&loop);

  se->expr = limit;
}

/* BTEST (i, pos) = (i & (1 << pos)) != 0.  */
static void
gfc_conv_intrinsic_btest (gfc_se * se, gfc_expr * expr)
{
  tree args[2];
  tree type;
  tree tmp;

  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  type = TREE_TYPE (args[0]);

  /* Optionally generate code for runtime argument check.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_BITS)
    {
      tree below = fold_build2_loc (input_location, LT_EXPR,
				    logical_type_node, args[1],
				    build_int_cst (TREE_TYPE (args[1]), 0));
      tree nbits = build_int_cst (TREE_TYPE (args[1]), TYPE_PRECISION (type));
      tree above = fold_build2_loc (input_location, GE_EXPR,
				    logical_type_node, args[1], nbits);
      tree scond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
				    logical_type_node, below, above);
      gfc_trans_runtime_check (true, false, scond, &se->pre, &expr->where,
			       "POS argument (%ld) out of range 0:%ld "
			       "in intrinsic BTEST",
			       fold_convert (long_integer_type_node, args[1]),
			       fold_convert (long_integer_type_node, nbits));
    }

  tmp = fold_build2_loc (input_location, LSHIFT_EXPR, type,
			 build_int_cst (type, 1), args[1]);
  tmp = fold_build2_loc (input_location, BIT_AND_EXPR, type, args[0], tmp);
  tmp = fold_build2_loc (input_location, NE_EXPR, logical_type_node, tmp,
			 build_int_cst (type, 0));
  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = convert (type, tmp);
}


/* Generate code for BGE, BGT, BLE and BLT intrinsics.  */
static void
gfc_conv_intrinsic_bitcomp (gfc_se * se, gfc_expr * expr, enum tree_code op)
{
  tree args[2];

  gfc_conv_intrinsic_function_args (se, expr, args, 2);

  /* Convert both arguments to the unsigned type of the same size.  */
  args[0] = fold_convert (unsigned_type_for (TREE_TYPE (args[0])), args[0]);
  args[1] = fold_convert (unsigned_type_for (TREE_TYPE (args[1])), args[1]);

  /* If they have unequal type size, convert to the larger one.  */
  if (TYPE_PRECISION (TREE_TYPE (args[0]))
      > TYPE_PRECISION (TREE_TYPE (args[1])))
    args[1] = fold_convert (TREE_TYPE (args[0]), args[1]);
  else if (TYPE_PRECISION (TREE_TYPE (args[1]))
	   > TYPE_PRECISION (TREE_TYPE (args[0])))
    args[0] = fold_convert (TREE_TYPE (args[1]), args[0]);

  /* Now, we compare them.  */
  se->expr = fold_build2_loc (input_location, op, logical_type_node,
			      args[0], args[1]);
}


/* Generate code to perform the specified operation.  */
static void
gfc_conv_intrinsic_bitop (gfc_se * se, gfc_expr * expr, enum tree_code op)
{
  tree args[2];

  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  se->expr = fold_build2_loc (input_location, op, TREE_TYPE (args[0]),
			      args[0], args[1]);
}

/* Bitwise not.  */
static void
gfc_conv_intrinsic_not (gfc_se * se, gfc_expr * expr)
{
  tree arg;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  se->expr = fold_build1_loc (input_location, BIT_NOT_EXPR,
			      TREE_TYPE (arg), arg);
}


/* Generate code for OUT_OF_RANGE.  */
static void
gfc_conv_intrinsic_out_of_range (gfc_se * se, gfc_expr * expr)
{
  tree *args;
  tree type;
  tree tmp = NULL_TREE, tmp1, tmp2;
  unsigned int num_args;
  int k;
  gfc_se rnd_se;
  gfc_actual_arglist *arg = expr->value.function.actual;
  gfc_expr *x = arg->expr;
  gfc_expr *mold = arg->next->expr;

  num_args = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, num_args);

  gfc_conv_intrinsic_function_args (se, expr, args, num_args);

  gfc_init_se (&rnd_se, NULL);

  if (num_args == 3)
    {
      /* The ROUND argument is optional and shall appear only if X is
	 of type real and MOLD is of type integer (see edit F23/004).  */
      gfc_expr *round = arg->next->next->expr;
      gfc_conv_expr (&rnd_se, round);

      if (round->expr_type == EXPR_VARIABLE
	  && round->symtree->n.sym->attr.dummy
	  && round->symtree->n.sym->attr.optional)
	{
	  tree present = gfc_conv_expr_present (round->symtree->n.sym);
	  rnd_se.expr = build3_loc (input_location, COND_EXPR,
				    logical_type_node, present,
				    rnd_se.expr, logical_false_node);
	  gfc_add_block_to_block (&se->pre, &rnd_se.pre);
	}
    }
  else
    {
      /* If ROUND is absent, it is equivalent to having the value false.  */
      rnd_se.expr = logical_false_node;
    }

  type = TREE_TYPE (args[0]);
  k = gfc_validate_kind (mold->ts.type, mold->ts.kind, false);

  switch (x->ts.type)
    {
    case BT_REAL:
      /* X may be IEEE infinity or NaN, but the representation of MOLD may not
	 support infinity or NaN.  */
      tree finite;
      finite = build_call_expr_loc (input_location,
				    builtin_decl_explicit (BUILT_IN_ISFINITE),
				    1,  args[0]);
      finite = convert (logical_type_node, finite);

      if (mold->ts.type == BT_REAL)
	{
	  tmp1 = build1 (ABS_EXPR, type, args[0]);
	  tmp2 = gfc_conv_mpfr_to_tree (gfc_real_kinds[k].huge,
					mold->ts.kind, 0);
	  tmp = build2 (GT_EXPR, logical_type_node, tmp1,
			convert (type, tmp2));

	  /* Check if MOLD representation supports infinity or NaN.  */
	  bool infnan = (HONOR_INFINITIES (TREE_TYPE (args[1]))
			 || HONOR_NANS (TREE_TYPE (args[1])));
	  tmp = build3 (COND_EXPR, logical_type_node, finite, tmp,
			infnan ? logical_false_node : logical_true_node);
	}
      else
	{
	  tree rounded;
	  tree decl;

	  decl = gfc_builtin_decl_for_float_kind (BUILT_IN_TRUNC, x->ts.kind);
	  gcc_assert (decl != NULL_TREE);

	  /* Round or truncate argument X, depending on the optional argument
	     ROUND (default: .false.).  */
	  tmp1 = build_round_expr (args[0], type);
	  tmp2 = build_call_expr_loc (input_location, decl, 1, args[0]);
	  rounded = build3 (COND_EXPR, type, rnd_se.expr, tmp1, tmp2);

	  if (mold->ts.type == BT_INTEGER)
	    {
	      tmp1 = gfc_conv_mpz_to_tree (gfc_integer_kinds[k].min_int,
					   x->ts.kind);
	      tmp2 = gfc_conv_mpz_to_tree (gfc_integer_kinds[k].huge,
					   x->ts.kind);
	    }
	  else if (mold->ts.type == BT_UNSIGNED)
	    {
	      tmp1 = build_real_from_int_cst (type, integer_zero_node);
	      tmp2 = gfc_conv_mpz_to_tree (gfc_unsigned_kinds[k].huge,
					   x->ts.kind);
	    }
	  else
	    gcc_unreachable ();

	  tmp1 = build2 (LT_EXPR, logical_type_node, rounded,
			 convert (type, tmp1));
	  tmp2 = build2 (GT_EXPR, logical_type_node, rounded,
			 convert (type, tmp2));
	  tmp = build2 (TRUTH_ORIF_EXPR, logical_type_node, tmp1, tmp2);
	  tmp = build2 (TRUTH_ORIF_EXPR, logical_type_node,
			build1 (TRUTH_NOT_EXPR, logical_type_node, finite),
			tmp);
	}
      break;

    case BT_INTEGER:
      if (mold->ts.type == BT_INTEGER)
	{
	  tmp1 = gfc_conv_mpz_to_tree (gfc_integer_kinds[k].min_int,
				       x->ts.kind);
	  tmp2 = gfc_conv_mpz_to_tree (gfc_integer_kinds[k].huge,
				       x->ts.kind);
	  tmp1 = build2 (LT_EXPR, logical_type_node, args[0],
			 convert (type, tmp1));
	  tmp2 = build2 (GT_EXPR, logical_type_node, args[0],
			 convert (type, tmp2));
	  tmp = build2 (TRUTH_ORIF_EXPR, logical_type_node, tmp1, tmp2);
	}
      else if (mold->ts.type == BT_UNSIGNED)
	{
	  int i = gfc_validate_kind (x->ts.type, x->ts.kind, false);
	  tmp = build_int_cst (type, 0);
	  tmp = build2 (LT_EXPR, logical_type_node, args[0], tmp);
	  if (mpz_cmp (gfc_integer_kinds[i].huge,
		       gfc_unsigned_kinds[k].huge) > 0)
	    {
	      tmp2 = gfc_conv_mpz_to_tree (gfc_unsigned_kinds[k].huge,
					   x->ts.kind);
	      tmp2 = build2 (GT_EXPR, logical_type_node, args[0],
			     convert (type, tmp2));
	      tmp = build2 (TRUTH_ORIF_EXPR, logical_type_node, tmp, tmp2);
	    }
	}
      else if (mold->ts.type == BT_REAL)
	{
	  tmp2 = gfc_conv_mpfr_to_tree (gfc_real_kinds[k].huge,
					mold->ts.kind, 0);
	  tmp1 = build1 (NEGATE_EXPR, TREE_TYPE (tmp2), tmp2);
	  tmp1 = build2 (LT_EXPR, logical_type_node, args[0],
			 convert (type, tmp1));
	  tmp2 = build2 (GT_EXPR, logical_type_node, args[0],
			 convert (type, tmp2));
	  tmp = build2 (TRUTH_ORIF_EXPR, logical_type_node, tmp1, tmp2);
	}
      else
	gcc_unreachable ();
      break;

    case BT_UNSIGNED:
      if (mold->ts.type == BT_UNSIGNED)
	{
	  tmp = gfc_conv_mpz_to_tree (gfc_unsigned_kinds[k].huge,
				      x->ts.kind);
	  tmp = build2 (GT_EXPR, logical_type_node, args[0],
			convert (type, tmp));
	}
      else if (mold->ts.type == BT_INTEGER)
	{
	  tmp = gfc_conv_mpz_to_tree (gfc_integer_kinds[k].huge,
				      x->ts.kind);
	  tmp = build2 (GT_EXPR, logical_type_node, args[0],
			convert (type, tmp));
	}
      else if (mold->ts.type == BT_REAL)
	{
	  tmp = gfc_conv_mpfr_to_tree (gfc_real_kinds[k].huge,
				       mold->ts.kind, 0);
	  tmp = build2 (GT_EXPR, logical_type_node, args[0],
			convert (type, tmp));
	}
      else
	gcc_unreachable ();
      break;

    default:
      gcc_unreachable ();
    }

  se->expr = convert (gfc_typenode_for_spec (&expr->ts), tmp);
}


/* Set or clear a single bit.  */
static void
gfc_conv_intrinsic_singlebitop (gfc_se * se, gfc_expr * expr, int set)
{
  tree args[2];
  tree type;
  tree tmp;
  enum tree_code op;

  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  type = TREE_TYPE (args[0]);

  /* Optionally generate code for runtime argument check.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_BITS)
    {
      tree below = fold_build2_loc (input_location, LT_EXPR,
				    logical_type_node, args[1],
				    build_int_cst (TREE_TYPE (args[1]), 0));
      tree nbits = build_int_cst (TREE_TYPE (args[1]), TYPE_PRECISION (type));
      tree above = fold_build2_loc (input_location, GE_EXPR,
				    logical_type_node, args[1], nbits);
      tree scond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
				    logical_type_node, below, above);
      size_t len_name = strlen (expr->value.function.isym->name);
      char *name = XALLOCAVEC (char, len_name + 1);
      for (size_t i = 0; i < len_name; i++)
	name[i] = TOUPPER (expr->value.function.isym->name[i]);
      name[len_name] = '\0';
      tree iname = gfc_build_addr_expr (pchar_type_node,
					gfc_build_cstring_const (name));
      gfc_trans_runtime_check (true, false, scond, &se->pre, &expr->where,
			       "POS argument (%ld) out of range 0:%ld "
			       "in intrinsic %s",
			       fold_convert (long_integer_type_node, args[1]),
			       fold_convert (long_integer_type_node, nbits),
			       iname);
    }

  tmp = fold_build2_loc (input_location, LSHIFT_EXPR, type,
			 build_int_cst (type, 1), args[1]);
  if (set)
    op = BIT_IOR_EXPR;
  else
    {
      op = BIT_AND_EXPR;
      tmp = fold_build1_loc (input_location, BIT_NOT_EXPR, type, tmp);
    }
  se->expr = fold_build2_loc (input_location, op, type, args[0], tmp);
}

/* Extract a sequence of bits.
    IBITS(I, POS, LEN) = (I >> POS) & ~((~0) << LEN).  */
static void
gfc_conv_intrinsic_ibits (gfc_se * se, gfc_expr * expr)
{
  tree args[3];
  tree type;
  tree tmp;
  tree mask;
  tree num_bits, cond;

  gfc_conv_intrinsic_function_args (se, expr, args, 3);
  type = TREE_TYPE (args[0]);

  /* Optionally generate code for runtime argument check.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_BITS)
    {
      tree tmp1 = fold_convert (long_integer_type_node, args[1]);
      tree tmp2 = fold_convert (long_integer_type_node, args[2]);
      tree nbits = build_int_cst (long_integer_type_node,
				  TYPE_PRECISION (type));
      tree below = fold_build2_loc (input_location, LT_EXPR,
				    logical_type_node, args[1],
				    build_int_cst (TREE_TYPE (args[1]), 0));
      tree above = fold_build2_loc (input_location, GT_EXPR,
				    logical_type_node, tmp1, nbits);
      tree scond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
				    logical_type_node, below, above);
      gfc_trans_runtime_check (true, false, scond, &se->pre, &expr->where,
			       "POS argument (%ld) out of range 0:%ld "
			       "in intrinsic IBITS", tmp1, nbits);
      below = fold_build2_loc (input_location, LT_EXPR,
			       logical_type_node, args[2],
			       build_int_cst (TREE_TYPE (args[2]), 0));
      above = fold_build2_loc (input_location, GT_EXPR,
			       logical_type_node, tmp2, nbits);
      scond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
			       logical_type_node, below, above);
      gfc_trans_runtime_check (true, false, scond, &se->pre, &expr->where,
			       "LEN argument (%ld) out of range 0:%ld "
			       "in intrinsic IBITS", tmp2, nbits);
      above = fold_build2_loc (input_location, PLUS_EXPR,
			       long_integer_type_node, tmp1, tmp2);
      scond = fold_build2_loc (input_location, GT_EXPR,
			       logical_type_node, above, nbits);
      gfc_trans_runtime_check (true, false, scond, &se->pre, &expr->where,
			       "POS(%ld)+LEN(%ld)>BIT_SIZE(%ld) "
			       "in intrinsic IBITS", tmp1, tmp2, nbits);
    }

  /* The Fortran standard allows (shift width) LEN <= BIT_SIZE(I), whereas
     gcc requires a shift width < BIT_SIZE(I), so we have to catch this
     special case.  See also gfc_conv_intrinsic_ishft ().  */
  num_bits = build_int_cst (TREE_TYPE (args[2]), TYPE_PRECISION (type));

  mask = build_int_cst (type, -1);
  mask = fold_build2_loc (input_location, LSHIFT_EXPR, type, mask, args[2]);
  cond = fold_build2_loc (input_location, GE_EXPR, logical_type_node, args[2],
			  num_bits);
  mask = fold_build3_loc (input_location, COND_EXPR, type, cond,
			  build_int_cst (type, 0), mask);
  mask = fold_build1_loc (input_location, BIT_NOT_EXPR, type, mask);

  tmp = fold_build2_loc (input_location, RSHIFT_EXPR, type, args[0], args[1]);

  se->expr = fold_build2_loc (input_location, BIT_AND_EXPR, type, tmp, mask);
}

static void
gfc_conv_intrinsic_shift (gfc_se * se, gfc_expr * expr, bool right_shift,
			  bool arithmetic)
{
  tree args[2], type, num_bits, cond;
  tree bigshift;
  bool do_convert = false;

  gfc_conv_intrinsic_function_args (se, expr, args, 2);

  args[0] = gfc_evaluate_now (args[0], &se->pre);
  args[1] = gfc_evaluate_now (args[1], &se->pre);
  type = TREE_TYPE (args[0]);

  if (!arithmetic)
    {
      args[0] = fold_convert (unsigned_type_for (type), args[0]);
      do_convert = true;
    }
  else
    gcc_assert (right_shift);

  if (flag_unsigned && arithmetic && expr->ts.type == BT_UNSIGNED)
    {
      do_convert = true;
      args[0] = fold_convert (signed_type_for (type), args[0]);
    }

  se->expr = fold_build2_loc (input_location,
			      right_shift ? RSHIFT_EXPR : LSHIFT_EXPR,
			      TREE_TYPE (args[0]), args[0], args[1]);

  if (do_convert)
    se->expr = fold_convert (type, se->expr);

  if (!arithmetic)
    bigshift = build_int_cst (type, 0);
  else
    {
      tree nonneg = fold_build2_loc (input_location, GE_EXPR,
				     logical_type_node, args[0],
				     build_int_cst (TREE_TYPE (args[0]), 0));
      bigshift = fold_build3_loc (input_location, COND_EXPR, type, nonneg,
				  build_int_cst (type, 0),
				  build_int_cst (type, -1));
    }

  /* The Fortran standard allows shift widths <= BIT_SIZE(I), whereas
     gcc requires a shift width < BIT_SIZE(I), so we have to catch this
     special case.  */
  num_bits = build_int_cst (TREE_TYPE (args[1]), TYPE_PRECISION (type));

  /* Optionally generate code for runtime argument check.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_BITS)
    {
      tree below = fold_build2_loc (input_location, LT_EXPR,
				    logical_type_node, args[1],
				    build_int_cst (TREE_TYPE (args[1]), 0));
      tree above = fold_build2_loc (input_location, GT_EXPR,
				    logical_type_node, args[1], num_bits);
      tree scond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
				    logical_type_node, below, above);
      size_t len_name = strlen (expr->value.function.isym->name);
      char *name = XALLOCAVEC (char, len_name + 1);
      for (size_t i = 0; i < len_name; i++)
	name[i] = TOUPPER (expr->value.function.isym->name[i]);
      name[len_name] = '\0';
      tree iname = gfc_build_addr_expr (pchar_type_node,
					gfc_build_cstring_const (name));
      gfc_trans_runtime_check (true, false, scond, &se->pre, &expr->where,
			       "SHIFT argument (%ld) out of range 0:%ld "
			       "in intrinsic %s",
			       fold_convert (long_integer_type_node, args[1]),
			       fold_convert (long_integer_type_node, num_bits),
			       iname);
    }

  cond = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
			  args[1], num_bits);

  se->expr = fold_build3_loc (input_location, COND_EXPR, type, cond,
			      bigshift, se->expr);
}

/* ISHFT (I, SHIFT) = (abs (shift) >= BIT_SIZE (i))
                        ? 0
	 	        : ((shift >= 0) ? i << shift : i >> -shift)
   where all shifts are logical shifts.  */
static void
gfc_conv_intrinsic_ishft (gfc_se * se, gfc_expr * expr)
{
  tree args[2];
  tree type;
  tree utype;
  tree tmp;
  tree width;
  tree num_bits;
  tree cond;
  tree lshift;
  tree rshift;

  gfc_conv_intrinsic_function_args (se, expr, args, 2);

  args[0] = gfc_evaluate_now (args[0], &se->pre);
  args[1] = gfc_evaluate_now (args[1], &se->pre);

  type = TREE_TYPE (args[0]);
  utype = unsigned_type_for (type);

  width = fold_build1_loc (input_location, ABS_EXPR, TREE_TYPE (args[1]),
			   args[1]);

  /* Left shift if positive.  */
  lshift = fold_build2_loc (input_location, LSHIFT_EXPR, type, args[0], width);

  /* Right shift if negative.
     We convert to an unsigned type because we want a logical shift.
     The standard doesn't define the case of shifting negative
     numbers, and we try to be compatible with other compilers, most
     notably g77, here.  */
  rshift = fold_convert (type, fold_build2_loc (input_location, RSHIFT_EXPR,
				    utype, convert (utype, args[0]), width));

  tmp = fold_build2_loc (input_location, GE_EXPR, logical_type_node, args[1],
			 build_int_cst (TREE_TYPE (args[1]), 0));
  tmp = fold_build3_loc (input_location, COND_EXPR, type, tmp, lshift, rshift);

  /* The Fortran standard allows shift widths <= BIT_SIZE(I), whereas
     gcc requires a shift width < BIT_SIZE(I), so we have to catch this
     special case.  */
  num_bits = build_int_cst (TREE_TYPE (args[1]), TYPE_PRECISION (type));

  /* Optionally generate code for runtime argument check.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_BITS)
    {
      tree outside = fold_build2_loc (input_location, GT_EXPR,
				    logical_type_node, width, num_bits);
      gfc_trans_runtime_check (true, false, outside, &se->pre, &expr->where,
			       "SHIFT argument (%ld) out of range -%ld:%ld "
			       "in intrinsic ISHFT",
			       fold_convert (long_integer_type_node, args[1]),
			       fold_convert (long_integer_type_node, num_bits),
			       fold_convert (long_integer_type_node, num_bits));
    }

  cond = fold_build2_loc (input_location, GE_EXPR, logical_type_node, width,
			  num_bits);
  se->expr = fold_build3_loc (input_location, COND_EXPR, type, cond,
			      build_int_cst (type, 0), tmp);
}


/* Circular shift.  AKA rotate or barrel shift.  */

static void
gfc_conv_intrinsic_ishftc (gfc_se * se, gfc_expr * expr)
{
  tree *args;
  tree type;
  tree tmp;
  tree lrot;
  tree rrot;
  tree zero;
  tree nbits;
  unsigned int num_args;

  num_args = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, num_args);

  gfc_conv_intrinsic_function_args (se, expr, args, num_args);

  type = TREE_TYPE (args[0]);
  nbits = build_int_cst (long_integer_type_node, TYPE_PRECISION (type));

  if (num_args == 3)
    {
      gfc_expr *size = expr->value.function.actual->next->next->expr;

      /* Use a library function for the 3 parameter version.  */
      tree int4type = gfc_get_int_type (4);

      /* Treat optional SIZE argument when it is passed as an optional
	 dummy.  If SIZE is absent, the default value is BIT_SIZE(I).  */
      if (size->expr_type == EXPR_VARIABLE
	  && size->symtree->n.sym->attr.dummy
	  && size->symtree->n.sym->attr.optional)
	{
	  tree type_of_size = TREE_TYPE (args[2]);
	  args[2] = build3_loc (input_location, COND_EXPR, type_of_size,
				gfc_conv_expr_present (size->symtree->n.sym),
				args[2], fold_convert (type_of_size, nbits));
	}

      /* We convert the first argument to at least 4 bytes, and
	 convert back afterwards.  This removes the need for library
	 functions for all argument sizes, and function will be
	 aligned to at least 32 bits, so there's no loss.  */
      if (expr->ts.kind < 4)
	args[0] = convert (int4type, args[0]);

      /* Convert the SHIFT and SIZE args to INTEGER*4 otherwise we would
         need loads of library  functions.  They cannot have values >
	 BIT_SIZE (I) so the conversion is safe.  */
      args[1] = convert (int4type, args[1]);
      args[2] = convert (int4type, args[2]);

      /* Optionally generate code for runtime argument check.  */
      if (gfc_option.rtcheck & GFC_RTCHECK_BITS)
	{
	  tree size = fold_convert (long_integer_type_node, args[2]);
	  tree below = fold_build2_loc (input_location, LE_EXPR,
					logical_type_node, size,
					build_int_cst (TREE_TYPE (args[1]), 0));
	  tree above = fold_build2_loc (input_location, GT_EXPR,
					logical_type_node, size, nbits);
	  tree scond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
					logical_type_node, below, above);
	  gfc_trans_runtime_check (true, false, scond, &se->pre, &expr->where,
				   "SIZE argument (%ld) out of range 1:%ld "
				   "in intrinsic ISHFTC", size, nbits);
	  tree width = fold_convert (long_integer_type_node, args[1]);
	  width = fold_build1_loc (input_location, ABS_EXPR,
				   long_integer_type_node, width);
	  scond = fold_build2_loc (input_location, GT_EXPR,
				   logical_type_node, width, size);
	  gfc_trans_runtime_check (true, false, scond, &se->pre, &expr->where,
				   "SHIFT argument (%ld) out of range -%ld:%ld "
				   "in intrinsic ISHFTC",
				   fold_convert (long_integer_type_node, args[1]),
				   size, size);
	}

      switch (expr->ts.kind)
	{
	case 1:
	case 2:
	case 4:
	  tmp = gfor_fndecl_math_ishftc4;
	  break;
	case 8:
	  tmp = gfor_fndecl_math_ishftc8;
	  break;
	case 16:
	  tmp = gfor_fndecl_math_ishftc16;
	  break;
	default:
	  gcc_unreachable ();
	}
      se->expr = build_call_expr_loc (input_location,
				      tmp, 3, args[0], args[1], args[2]);
      /* Convert the result back to the original type, if we extended
	 the first argument's width above.  */
      if (expr->ts.kind < 4)
	se->expr = convert (type, se->expr);

      return;
    }

  /* Evaluate arguments only once.  */
  args[0] = gfc_evaluate_now (args[0], &se->pre);
  args[1] = gfc_evaluate_now (args[1], &se->pre);

  /* Optionally generate code for runtime argument check.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_BITS)
    {
      tree width = fold_convert (long_integer_type_node, args[1]);
      width = fold_build1_loc (input_location, ABS_EXPR,
			       long_integer_type_node, width);
      tree outside = fold_build2_loc (input_location, GT_EXPR,
				      logical_type_node, width, nbits);
      gfc_trans_runtime_check (true, false, outside, &se->pre, &expr->where,
			       "SHIFT argument (%ld) out of range -%ld:%ld "
			       "in intrinsic ISHFTC",
			       fold_convert (long_integer_type_node, args[1]),
			       nbits, nbits);
    }

  /* Rotate left if positive.  */
  lrot = fold_build2_loc (input_location, LROTATE_EXPR, type, args[0], args[1]);

  /* Rotate right if negative.  */
  tmp = fold_build1_loc (input_location, NEGATE_EXPR, TREE_TYPE (args[1]),
			 args[1]);
  rrot = fold_build2_loc (input_location,RROTATE_EXPR, type, args[0], tmp);

  zero = build_int_cst (TREE_TYPE (args[1]), 0);
  tmp = fold_build2_loc (input_location, GT_EXPR, logical_type_node, args[1],
			 zero);
  rrot = fold_build3_loc (input_location, COND_EXPR, type, tmp, lrot, rrot);

  /* Do nothing if shift == 0.  */
  tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, args[1],
			 zero);
  se->expr = fold_build3_loc (input_location, COND_EXPR, type, tmp, args[0],
			      rrot);
}


/* LEADZ (i) = (i == 0) ? BIT_SIZE (i)
			: __builtin_clz(i) - (BIT_SIZE('int') - BIT_SIZE(i))

   The conditional expression is necessary because the result of LEADZ(0)
   is defined, but the result of __builtin_clz(0) is undefined for most
   targets.

   For INTEGER kinds smaller than the C 'int' type, we have to subtract the
   difference in bit size between the argument of LEADZ and the C int.  */

static void
gfc_conv_intrinsic_leadz (gfc_se * se, gfc_expr * expr)
{
  tree arg;
  tree arg_type;
  tree cond;
  tree result_type;
  tree leadz;
  tree bit_size;
  tree tmp;
  tree func;
  int s, argsize;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  argsize = TYPE_PRECISION (TREE_TYPE (arg));

  /* Which variant of __builtin_clz* should we call?  */
  if (argsize <= INT_TYPE_SIZE)
    {
      arg_type = unsigned_type_node;
      func = builtin_decl_explicit (BUILT_IN_CLZ);
    }
  else if (argsize <= LONG_TYPE_SIZE)
    {
      arg_type = long_unsigned_type_node;
      func = builtin_decl_explicit (BUILT_IN_CLZL);
    }
  else if (argsize <= LONG_LONG_TYPE_SIZE)
    {
      arg_type = long_long_unsigned_type_node;
      func = builtin_decl_explicit (BUILT_IN_CLZLL);
    }
  else
    {
      gcc_assert (argsize == 2 * LONG_LONG_TYPE_SIZE);
      arg_type = gfc_build_uint_type (argsize);
      func = NULL_TREE;
    }

  /* Convert the actual argument twice: first, to the unsigned type of the
     same size; then, to the proper argument type for the built-in
     function.  But the return type is of the default INTEGER kind.  */
  arg = fold_convert (gfc_build_uint_type (argsize), arg);
  arg = fold_convert (arg_type, arg);
  arg = gfc_evaluate_now (arg, &se->pre);
  result_type = gfc_get_int_type (gfc_default_integer_kind);

  /* Compute LEADZ for the case i .ne. 0.  */
  if (func)
    {
      s = TYPE_PRECISION (arg_type) - argsize;
      tmp = fold_convert (result_type,
			  build_call_expr_loc (input_location, func,
					       1, arg));
      leadz = fold_build2_loc (input_location, MINUS_EXPR, result_type,
			       tmp, build_int_cst (result_type, s));
    }
  else
    {
      /* We end up here if the argument type is larger than 'long long'.
	 We generate this code:

	    if (x & (ULL_MAX << ULL_SIZE) != 0)
	      return clzll ((unsigned long long) (x >> ULLSIZE));
	    else
	      return ULL_SIZE + clzll ((unsigned long long) x);
	 where ULL_MAX is the largest value that a ULL_MAX can hold
	 (0xFFFFFFFFFFFFFFFF for a 64-bit long long type), and ULLSIZE
	 is the bit-size of the long long type (64 in this example).  */
      tree ullsize, ullmax, tmp1, tmp2, btmp;

      ullsize = build_int_cst (result_type, LONG_LONG_TYPE_SIZE);
      ullmax = fold_build1_loc (input_location, BIT_NOT_EXPR,
				long_long_unsigned_type_node,
				build_int_cst (long_long_unsigned_type_node,
					       0));

      cond = fold_build2_loc (input_location, LSHIFT_EXPR, arg_type,
			      fold_convert (arg_type, ullmax), ullsize);
      cond = fold_build2_loc (input_location, BIT_AND_EXPR, arg_type,
			      arg, cond);
      cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			      cond, build_int_cst (arg_type, 0));

      tmp1 = fold_build2_loc (input_location, RSHIFT_EXPR, arg_type,
			      arg, ullsize);
      tmp1 = fold_convert (long_long_unsigned_type_node, tmp1);
      btmp = builtin_decl_explicit (BUILT_IN_CLZLL);
      tmp1 = fold_convert (result_type,
			   build_call_expr_loc (input_location, btmp, 1, tmp1));

      tmp2 = fold_convert (long_long_unsigned_type_node, arg);
      btmp = builtin_decl_explicit (BUILT_IN_CLZLL);
      tmp2 = fold_convert (result_type,
			   build_call_expr_loc (input_location, btmp, 1, tmp2));
      tmp2 = fold_build2_loc (input_location, PLUS_EXPR, result_type,
			      tmp2, ullsize);

      leadz = fold_build3_loc (input_location, COND_EXPR, result_type,
			       cond, tmp1, tmp2);
    }

  /* Build BIT_SIZE.  */
  bit_size = build_int_cst (result_type, argsize);

  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			  arg, build_int_cst (arg_type, 0));
  se->expr = fold_build3_loc (input_location, COND_EXPR, result_type, cond,
			      bit_size, leadz);
}


/* TRAILZ(i) = (i == 0) ? BIT_SIZE (i) : __builtin_ctz(i)

   The conditional expression is necessary because the result of TRAILZ(0)
   is defined, but the result of __builtin_ctz(0) is undefined for most
   targets.  */

static void
gfc_conv_intrinsic_trailz (gfc_se * se, gfc_expr *expr)
{
  tree arg;
  tree arg_type;
  tree cond;
  tree result_type;
  tree trailz;
  tree bit_size;
  tree func;
  int argsize;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  argsize = TYPE_PRECISION (TREE_TYPE (arg));

  /* Which variant of __builtin_ctz* should we call?  */
  if (argsize <= INT_TYPE_SIZE)
    {
      arg_type = unsigned_type_node;
      func = builtin_decl_explicit (BUILT_IN_CTZ);
    }
  else if (argsize <= LONG_TYPE_SIZE)
    {
      arg_type = long_unsigned_type_node;
      func = builtin_decl_explicit (BUILT_IN_CTZL);
    }
  else if (argsize <= LONG_LONG_TYPE_SIZE)
    {
      arg_type = long_long_unsigned_type_node;
      func = builtin_decl_explicit (BUILT_IN_CTZLL);
    }
  else
    {
      gcc_assert (argsize == 2 * LONG_LONG_TYPE_SIZE);
      arg_type = gfc_build_uint_type (argsize);
      func = NULL_TREE;
    }

  /* Convert the actual argument twice: first, to the unsigned type of the
     same size; then, to the proper argument type for the built-in
     function.  But the return type is of the default INTEGER kind.  */
  arg = fold_convert (gfc_build_uint_type (argsize), arg);
  arg = fold_convert (arg_type, arg);
  arg = gfc_evaluate_now (arg, &se->pre);
  result_type = gfc_get_int_type (gfc_default_integer_kind);

  /* Compute TRAILZ for the case i .ne. 0.  */
  if (func)
    trailz = fold_convert (result_type, build_call_expr_loc (input_location,
							     func, 1, arg));
  else
    {
      /* We end up here if the argument type is larger than 'long long'.
	 We generate this code:

	    if ((x & ULL_MAX) == 0)
	      return ULL_SIZE + ctzll ((unsigned long long) (x >> ULLSIZE));
	    else
	      return ctzll ((unsigned long long) x);

	 where ULL_MAX is the largest value that a ULL_MAX can hold
	 (0xFFFFFFFFFFFFFFFF for a 64-bit long long type), and ULLSIZE
	 is the bit-size of the long long type (64 in this example).  */
      tree ullsize, ullmax, tmp1, tmp2, btmp;

      ullsize = build_int_cst (result_type, LONG_LONG_TYPE_SIZE);
      ullmax = fold_build1_loc (input_location, BIT_NOT_EXPR,
				long_long_unsigned_type_node,
				build_int_cst (long_long_unsigned_type_node, 0));

      cond = fold_build2_loc (input_location, BIT_AND_EXPR, arg_type, arg,
			      fold_convert (arg_type, ullmax));
      cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, cond,
			      build_int_cst (arg_type, 0));

      tmp1 = fold_build2_loc (input_location, RSHIFT_EXPR, arg_type,
			      arg, ullsize);
      tmp1 = fold_convert (long_long_unsigned_type_node, tmp1);
      btmp = builtin_decl_explicit (BUILT_IN_CTZLL);
      tmp1 = fold_convert (result_type,
			   build_call_expr_loc (input_location, btmp, 1, tmp1));
      tmp1 = fold_build2_loc (input_location, PLUS_EXPR, result_type,
			      tmp1, ullsize);

      tmp2 = fold_convert (long_long_unsigned_type_node, arg);
      btmp = builtin_decl_explicit (BUILT_IN_CTZLL);
      tmp2 = fold_convert (result_type,
			   build_call_expr_loc (input_location, btmp, 1, tmp2));

      trailz = fold_build3_loc (input_location, COND_EXPR, result_type,
				cond, tmp1, tmp2);
    }

  /* Build BIT_SIZE.  */
  bit_size = build_int_cst (result_type, argsize);

  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			  arg, build_int_cst (arg_type, 0));
  se->expr = fold_build3_loc (input_location, COND_EXPR, result_type, cond,
			      bit_size, trailz);
}

/* Using __builtin_popcount for POPCNT and __builtin_parity for POPPAR;
   for types larger than "long long", we call the long long built-in for
   the lower and higher bits and combine the result.  */

static void
gfc_conv_intrinsic_popcnt_poppar (gfc_se * se, gfc_expr *expr, int parity)
{
  tree arg;
  tree arg_type;
  tree result_type;
  tree func;
  int argsize;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  argsize = TYPE_PRECISION (TREE_TYPE (arg));
  result_type = gfc_get_int_type (gfc_default_integer_kind);

  /* Which variant of the builtin should we call?  */
  if (argsize <= INT_TYPE_SIZE)
    {
      arg_type = unsigned_type_node;
      func = builtin_decl_explicit (parity
				    ? BUILT_IN_PARITY
				    : BUILT_IN_POPCOUNT);
    }
  else if (argsize <= LONG_TYPE_SIZE)
    {
      arg_type = long_unsigned_type_node;
      func = builtin_decl_explicit (parity
				    ? BUILT_IN_PARITYL
				    : BUILT_IN_POPCOUNTL);
    }
  else if (argsize <= LONG_LONG_TYPE_SIZE)
    {
      arg_type = long_long_unsigned_type_node;
      func = builtin_decl_explicit (parity
				    ? BUILT_IN_PARITYLL
				    : BUILT_IN_POPCOUNTLL);
    }
  else
    {
      /* Our argument type is larger than 'long long', which mean none
	 of the POPCOUNT builtins covers it.  We thus call the 'long long'
	 variant multiple times, and add the results.  */
      tree utype, arg2, call1, call2;

      /* For now, we only cover the case where argsize is twice as large
	 as 'long long'.  */
      gcc_assert (argsize == 2 * LONG_LONG_TYPE_SIZE);

      func = builtin_decl_explicit (parity
				    ? BUILT_IN_PARITYLL
				    : BUILT_IN_POPCOUNTLL);

      /* Convert it to an integer, and store into a variable.  */
      utype = gfc_build_uint_type (argsize);
      arg = fold_convert (utype, arg);
      arg = gfc_evaluate_now (arg, &se->pre);

      /* Call the builtin twice.  */
      call1 = build_call_expr_loc (input_location, func, 1,
				   fold_convert (long_long_unsigned_type_node,
						 arg));

      arg2 = fold_build2_loc (input_location, RSHIFT_EXPR, utype, arg,
			      build_int_cst (utype, LONG_LONG_TYPE_SIZE));
      call2 = build_call_expr_loc (input_location, func, 1,
				   fold_convert (long_long_unsigned_type_node,
						 arg2));

      /* Combine the results.  */
      if (parity)
	se->expr = fold_build2_loc (input_location, BIT_XOR_EXPR,
				    integer_type_node, call1, call2);
      else
	se->expr = fold_build2_loc (input_location, PLUS_EXPR,
				    integer_type_node, call1, call2);

      se->expr = convert (result_type, se->expr);
      return;
    }

  /* Convert the actual argument twice: first, to the unsigned type of the
     same size; then, to the proper argument type for the built-in
     function.  */
  arg = fold_convert (gfc_build_uint_type (argsize), arg);
  arg = fold_convert (arg_type, arg);

  se->expr = fold_convert (result_type,
			   build_call_expr_loc (input_location, func, 1, arg));
}


/* Process an intrinsic with unspecified argument-types that has an optional
   argument (which could be of type character), e.g. EOSHIFT.  For those, we
   need to append the string length of the optional argument if it is not
   present and the type is really character.
   primary specifies the position (starting at 1) of the non-optional argument
   specifying the type and optional gives the position of the optional
   argument in the arglist.  */

static void
conv_generic_with_optional_char_arg (gfc_se* se, gfc_expr* expr,
				     unsigned primary, unsigned optional)
{
  gfc_actual_arglist* prim_arg;
  gfc_actual_arglist* opt_arg;
  unsigned cur_pos;
  gfc_actual_arglist* arg;
  gfc_symbol* sym;
  vec<tree, va_gc> *append_args;

  /* Find the two arguments given as position.  */
  cur_pos = 0;
  prim_arg = NULL;
  opt_arg = NULL;
  for (arg = expr->value.function.actual; arg; arg = arg->next)
    {
      ++cur_pos;

      if (cur_pos == primary)
	prim_arg = arg;
      if (cur_pos == optional)
	opt_arg = arg;

      if (cur_pos >= primary && cur_pos >= optional)
	break;
    }
  gcc_assert (prim_arg);
  gcc_assert (prim_arg->expr);
  gcc_assert (opt_arg);

  /* If we do have type CHARACTER and the optional argument is really absent,
     append a dummy 0 as string length.  */
  append_args = NULL;
  if (prim_arg->expr->ts.type == BT_CHARACTER && !opt_arg->expr)
    {
      tree dummy;

      dummy = build_int_cst (gfc_charlen_type_node, 0);
      vec_alloc (append_args, 1);
      append_args->quick_push (dummy);
    }

  /* Build the call itself.  */
  gcc_assert (!se->ignore_optional);
  sym = gfc_get_symbol_for_expr (expr, false);
  gfc_conv_procedure_call (se, sym, expr->value.function.actual, expr,
			  append_args);
  gfc_free_symbol (sym);
}

/* The length of a character string.  */
static void
gfc_conv_intrinsic_len (gfc_se * se, gfc_expr * expr)
{
  tree len;
  tree type;
  tree decl;
  gfc_symbol *sym;
  gfc_se argse;
  gfc_expr *arg;

  gcc_assert (!se->ss);

  arg = expr->value.function.actual->expr;

  type = gfc_typenode_for_spec (&expr->ts);
  switch (arg->expr_type)
    {
    case EXPR_CONSTANT:
      len = build_int_cst (gfc_charlen_type_node, arg->value.character.length);
      break;

    case EXPR_ARRAY:
      /* Obtain the string length from the function used by
         trans-array.cc(gfc_trans_array_constructor).  */
      len = NULL_TREE;
      get_array_ctor_strlen (&se->pre, arg->value.constructor, &len);
      break;

    case EXPR_VARIABLE:
      if (arg->ref == NULL
	    || (arg->ref->next == NULL && arg->ref->type == REF_ARRAY))
	{
	  /* This doesn't catch all cases.
	     See http://gcc.gnu.org/ml/fortran/2004-06/msg00165.html
	     and the surrounding thread.  */
	  sym = arg->symtree->n.sym;
	  decl = gfc_get_symbol_decl (sym);
	  if (decl == current_function_decl && sym->attr.function
		&& (sym->result == sym))
	    decl = gfc_get_fake_result_decl (sym, 0);

	  len = sym->ts.u.cl->backend_decl;
	  gcc_assert (len);
	  break;
	}

      /* Fall through.  */

    default:
      gfc_init_se (&argse, se);
      if (arg->rank == 0)
	gfc_conv_expr (&argse, arg);
      else
	gfc_conv_expr_descriptor (&argse, arg);
      gfc_add_block_to_block (&se->pre, &argse.pre);
      gfc_add_block_to_block (&se->post, &argse.post);
      len = argse.string_length;
      break;
    }
  se->expr = convert (type, len);
}

/* The length of a character string not including trailing blanks.  */
static void
gfc_conv_intrinsic_len_trim (gfc_se * se, gfc_expr * expr)
{
  int kind = expr->value.function.actual->expr->ts.kind;
  tree args[2], type, fndecl;

  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  type = gfc_typenode_for_spec (&expr->ts);

  if (kind == 1)
    fndecl = gfor_fndecl_string_len_trim;
  else if (kind == 4)
    fndecl = gfor_fndecl_string_len_trim_char4;
  else
    gcc_unreachable ();

  se->expr = build_call_expr_loc (input_location,
			      fndecl, 2, args[0], args[1]);
  se->expr = convert (type, se->expr);
}


/* Returns the starting position of a substring within a string.  */

static void
gfc_conv_intrinsic_index_scan_verify (gfc_se * se, gfc_expr * expr,
				      tree function)
{
  tree logical4_type_node = gfc_get_logical_type (4);
  tree type;
  tree fndecl;
  tree *args;
  unsigned int num_args;

  args = XALLOCAVEC (tree, 5);

  /* Get number of arguments; characters count double due to the
     string length argument. Kind= is not passed to the library
     and thus ignored.  */
  if (expr->value.function.actual->next->next->expr == NULL)
    num_args = 4;
  else
    num_args = 5;

  gfc_conv_intrinsic_function_args (se, expr, args, num_args);
  type = gfc_typenode_for_spec (&expr->ts);

  if (num_args == 4)
    args[4] = build_int_cst (logical4_type_node, 0);
  else
    args[4] = convert (logical4_type_node, args[4]);

  fndecl = build_addr (function);
  se->expr = build_call_array_loc (input_location,
			       TREE_TYPE (TREE_TYPE (function)), fndecl,
			       5, args);
  se->expr = convert (type, se->expr);

}

/* The ascii value for a single character.  */
static void
gfc_conv_intrinsic_ichar (gfc_se * se, gfc_expr * expr)
{
  tree args[3], type, pchartype;
  int nargs;

  nargs = gfc_intrinsic_argument_list_length (expr);
  gfc_conv_intrinsic_function_args (se, expr, args, nargs);
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (args[1])));
  pchartype = gfc_get_pchar_type (expr->value.function.actual->expr->ts.kind);
  args[1] = fold_build1_loc (input_location, NOP_EXPR, pchartype, args[1]);
  type = gfc_typenode_for_spec (&expr->ts);

  se->expr = build_fold_indirect_ref_loc (input_location,
				      args[1]);
  se->expr = convert (type, se->expr);
}


/* Intrinsic ISNAN calls __builtin_isnan.  */

static void
gfc_conv_intrinsic_isnan (gfc_se * se, gfc_expr * expr)
{
  tree arg;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  se->expr = build_call_expr_loc (input_location,
				  builtin_decl_explicit (BUILT_IN_ISNAN),
				  1, arg);
  STRIP_TYPE_NOPS (se->expr);
  se->expr = fold_convert (gfc_typenode_for_spec (&expr->ts), se->expr);
}


/* Intrinsics IS_IOSTAT_END and IS_IOSTAT_EOR just need to compare
   their argument against a constant integer value.  */

static void
gfc_conv_has_intvalue (gfc_se * se, gfc_expr * expr, const int value)
{
  tree arg;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  se->expr = fold_build2_loc (input_location, EQ_EXPR,
			      gfc_typenode_for_spec (&expr->ts),
			      arg, build_int_cst (TREE_TYPE (arg), value));
}



/* MERGE (tsource, fsource, mask) = mask ? tsource : fsource.  */

static void
gfc_conv_intrinsic_merge (gfc_se * se, gfc_expr * expr)
{
  tree tsource;
  tree fsource;
  tree mask;
  tree type;
  tree len, len2;
  tree *args;
  unsigned int num_args;

  num_args = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, num_args);

  gfc_conv_intrinsic_function_args (se, expr, args, num_args);
  if (expr->ts.type != BT_CHARACTER)
    {
      tsource = args[0];
      fsource = args[1];
      mask = args[2];
    }
  else
    {
      /* We do the same as in the non-character case, but the argument
	 list is different because of the string length arguments. We
	 also have to set the string length for the result.  */
      len = args[0];
      tsource = args[1];
      len2 = args[2];
      fsource = args[3];
      mask = args[4];

      gfc_trans_same_strlen_check ("MERGE intrinsic", &expr->where, len, len2,
				   &se->pre);
      se->string_length = len;
    }
  tsource = gfc_evaluate_now (tsource, &se->pre);
  fsource = gfc_evaluate_now (fsource, &se->pre);
  mask = gfc_evaluate_now (mask, &se->pre);
  type = TREE_TYPE (tsource);
  se->expr = fold_build3_loc (input_location, COND_EXPR, type, mask, tsource,
			      fold_convert (type, fsource));
}


/* MERGE_BITS (I, J, MASK) = (I & MASK) | (I & (~MASK)).  */

static void
gfc_conv_intrinsic_merge_bits (gfc_se * se, gfc_expr * expr)
{
  tree args[3], mask, type;

  gfc_conv_intrinsic_function_args (se, expr, args, 3);
  mask = gfc_evaluate_now (args[2], &se->pre);

  type = TREE_TYPE (args[0]);
  gcc_assert (TREE_TYPE (args[1]) == type);
  gcc_assert (TREE_TYPE (mask) == type);

  args[0] = fold_build2_loc (input_location, BIT_AND_EXPR, type, args[0], mask);
  args[1] = fold_build2_loc (input_location, BIT_AND_EXPR, type, args[1],
			     fold_build1_loc (input_location, BIT_NOT_EXPR,
					      type, mask));
  se->expr = fold_build2_loc (input_location, BIT_IOR_EXPR, type,
			      args[0], args[1]);
}


/* MASKL(n)  =  n == 0 ? 0 : (~0) << (BIT_SIZE - n)
   MASKR(n)  =  n == BIT_SIZE ? ~0 : ~((~0) << n)  */

static void
gfc_conv_intrinsic_mask (gfc_se * se, gfc_expr * expr, int left)
{
  tree arg, allones, type, utype, res, cond, bitsize;
  int i;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  arg = gfc_evaluate_now (arg, &se->pre);

  type = gfc_get_int_type (expr->ts.kind);
  utype = unsigned_type_for (type);

  i = gfc_validate_kind (BT_INTEGER, expr->ts.kind, false);
  bitsize = build_int_cst (TREE_TYPE (arg), gfc_integer_kinds[i].bit_size);

  allones = fold_build1_loc (input_location, BIT_NOT_EXPR, utype,
			     build_int_cst (utype, 0));

  if (left)
    {
      /* Left-justified mask.  */
      res = fold_build2_loc (input_location, MINUS_EXPR, TREE_TYPE (arg),
			     bitsize, arg);
      res = fold_build2_loc (input_location, LSHIFT_EXPR, utype, allones,
			     fold_convert (utype, res));

      /* Special case arg == 0, because SHIFT_EXPR wants a shift strictly
	 smaller than type width.  */
      cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, arg,
			      build_int_cst (TREE_TYPE (arg), 0));
      res = fold_build3_loc (input_location, COND_EXPR, utype, cond,
			     build_int_cst (utype, 0), res);
    }
  else
    {
      /* Right-justified mask.  */
      res = fold_build2_loc (input_location, LSHIFT_EXPR, utype, allones,
			     fold_convert (utype, arg));
      res = fold_build1_loc (input_location, BIT_NOT_EXPR, utype, res);

      /* Special case agr == bit_size, because SHIFT_EXPR wants a shift
	 strictly smaller than type width.  */
      cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			      arg, bitsize);
      res = fold_build3_loc (input_location, COND_EXPR, utype,
			     cond, allones, res);
    }

  se->expr = fold_convert (type, res);
}


/* FRACTION (s) is translated into:
     isfinite (s) ? frexp (s, &dummy_int) : NaN  */
static void
gfc_conv_intrinsic_fraction (gfc_se * se, gfc_expr * expr)
{
  tree arg, type, tmp, res, frexp, cond;

  frexp = gfc_builtin_decl_for_float_kind (BUILT_IN_FREXP, expr->ts.kind);

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  arg = gfc_evaluate_now (arg, &se->pre);

  cond = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_ISFINITE),
			      1, arg);

  tmp = gfc_create_var (integer_type_node, NULL);
  res = build_call_expr_loc (input_location, frexp, 2,
			     fold_convert (type, arg),
			     gfc_build_addr_expr (NULL_TREE, tmp));
  res = fold_convert (type, res);

  se->expr = fold_build3_loc (input_location, COND_EXPR, type,
			      cond, res, gfc_build_nan (type, ""));
}


/* NEAREST (s, dir) is translated into
     tmp = copysign (HUGE_VAL, dir);
     return nextafter (s, tmp);
 */
static void
gfc_conv_intrinsic_nearest (gfc_se * se, gfc_expr * expr)
{
  tree args[2], type, tmp, nextafter, copysign, huge_val;

  nextafter = gfc_builtin_decl_for_float_kind (BUILT_IN_NEXTAFTER, expr->ts.kind);
  copysign = gfc_builtin_decl_for_float_kind (BUILT_IN_COPYSIGN, expr->ts.kind);

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, args, 2);

  huge_val = gfc_build_inf_or_huge (type, expr->ts.kind);
  tmp = build_call_expr_loc (input_location, copysign, 2, huge_val,
			     fold_convert (type, args[1]));
  se->expr = build_call_expr_loc (input_location, nextafter, 2,
				  fold_convert (type, args[0]), tmp);
  se->expr = fold_convert (type, se->expr);
}


/* SPACING (s) is translated into
    int e;
    if (!isfinite (s))
      res = NaN;
    else if (s == 0)
      res = tiny;
    else
    {
      frexp (s, &e);
      e = e - prec;
      e = MAX_EXPR (e, emin);
      res = scalbn (1., e);
    }
    return res;

 where prec is the precision of s, gfc_real_kinds[k].digits,
       emin is min_exponent - 1, gfc_real_kinds[k].min_exponent - 1,
   and tiny is tiny(s), gfc_real_kinds[k].tiny.  */

static void
gfc_conv_intrinsic_spacing (gfc_se * se, gfc_expr * expr)
{
  tree arg, type, prec, emin, tiny, res, e;
  tree cond, nan, tmp, frexp, scalbn;
  int k;
  stmtblock_t block;

  k = gfc_validate_kind (BT_REAL, expr->ts.kind, false);
  prec = build_int_cst (integer_type_node, gfc_real_kinds[k].digits);
  emin = build_int_cst (integer_type_node, gfc_real_kinds[k].min_exponent - 1);
  tiny = gfc_conv_mpfr_to_tree (gfc_real_kinds[k].tiny, expr->ts.kind, 0);

  frexp = gfc_builtin_decl_for_float_kind (BUILT_IN_FREXP, expr->ts.kind);
  scalbn = gfc_builtin_decl_for_float_kind (BUILT_IN_SCALBN, expr->ts.kind);

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  arg = gfc_evaluate_now (arg, &se->pre);

  type = gfc_typenode_for_spec (&expr->ts);
  e = gfc_create_var (integer_type_node, NULL);
  res = gfc_create_var (type, NULL);


  /* Build the block for s /= 0.  */
  gfc_start_block (&block);
  tmp = build_call_expr_loc (input_location, frexp, 2, arg,
			     gfc_build_addr_expr (NULL_TREE, e));
  gfc_add_expr_to_block (&block, tmp);

  tmp = fold_build2_loc (input_location, MINUS_EXPR, integer_type_node, e,
			 prec);
  gfc_add_modify (&block, e, fold_build2_loc (input_location, MAX_EXPR,
					      integer_type_node, tmp, emin));

  tmp = build_call_expr_loc (input_location, scalbn, 2,
			 build_real_from_int_cst (type, integer_one_node), e);
  gfc_add_modify (&block, res, tmp);

  /* Finish by building the IF statement for value zero.  */
  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, arg,
			  build_real_from_int_cst (type, integer_zero_node));
  tmp = build3_v (COND_EXPR, cond, build2_v (MODIFY_EXPR, res, tiny),
		  gfc_finish_block (&block));

  /* And deal with infinities and NaNs.  */
  cond = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_ISFINITE),
			      1, arg);
  nan = gfc_build_nan (type, "");
  tmp = build3_v (COND_EXPR, cond, tmp, build2_v (MODIFY_EXPR, res, nan));

  gfc_add_expr_to_block (&se->pre, tmp);
  se->expr = res;
}


/* RRSPACING (s) is translated into
      int e;
      real x;
      x = fabs (s);
      if (isfinite (x))
      {
	if (x != 0)
	{
	  frexp (s, &e);
	  x = scalbn (x, precision - e);
	}
      }
      else
        x = NaN;
      return x;

 where precision is gfc_real_kinds[k].digits.  */

static void
gfc_conv_intrinsic_rrspacing (gfc_se * se, gfc_expr * expr)
{
  tree arg, type, e, x, cond, nan, stmt, tmp, frexp, scalbn, fabs;
  int prec, k;
  stmtblock_t block;

  k = gfc_validate_kind (BT_REAL, expr->ts.kind, false);
  prec = gfc_real_kinds[k].digits;

  frexp = gfc_builtin_decl_for_float_kind (BUILT_IN_FREXP, expr->ts.kind);
  scalbn = gfc_builtin_decl_for_float_kind (BUILT_IN_SCALBN, expr->ts.kind);
  fabs = gfc_builtin_decl_for_float_kind (BUILT_IN_FABS, expr->ts.kind);

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  arg = gfc_evaluate_now (arg, &se->pre);

  e = gfc_create_var (integer_type_node, NULL);
  x = gfc_create_var (type, NULL);
  gfc_add_modify (&se->pre, x,
		  build_call_expr_loc (input_location, fabs, 1, arg));


  gfc_start_block (&block);
  tmp = build_call_expr_loc (input_location, frexp, 2, arg,
			     gfc_build_addr_expr (NULL_TREE, e));
  gfc_add_expr_to_block (&block, tmp);

  tmp = fold_build2_loc (input_location, MINUS_EXPR, integer_type_node,
			 build_int_cst (integer_type_node, prec), e);
  tmp = build_call_expr_loc (input_location, scalbn, 2, x, tmp);
  gfc_add_modify (&block, x, tmp);
  stmt = gfc_finish_block (&block);

  /* if (x != 0) */
  cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node, x,
			  build_real_from_int_cst (type, integer_zero_node));
  tmp = build3_v (COND_EXPR, cond, stmt, build_empty_stmt (input_location));

  /* And deal with infinities and NaNs.  */
  cond = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_ISFINITE),
			      1, x);
  nan = gfc_build_nan (type, "");
  tmp = build3_v (COND_EXPR, cond, tmp, build2_v (MODIFY_EXPR, x, nan));

  gfc_add_expr_to_block (&se->pre, tmp);
  se->expr = fold_convert (type, x);
}


/* SCALE (s, i) is translated into scalbn (s, i).  */
static void
gfc_conv_intrinsic_scale (gfc_se * se, gfc_expr * expr)
{
  tree args[2], type, scalbn;

  scalbn = gfc_builtin_decl_for_float_kind (BUILT_IN_SCALBN, expr->ts.kind);

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  se->expr = build_call_expr_loc (input_location, scalbn, 2,
				  fold_convert (type, args[0]),
				  fold_convert (integer_type_node, args[1]));
  se->expr = fold_convert (type, se->expr);
}


/* SET_EXPONENT (s, i) is translated into
   isfinite(s) ? scalbn (frexp (s, &dummy_int), i) : NaN  */
static void
gfc_conv_intrinsic_set_exponent (gfc_se * se, gfc_expr * expr)
{
  tree args[2], type, tmp, frexp, scalbn, cond, nan, res;

  frexp = gfc_builtin_decl_for_float_kind (BUILT_IN_FREXP, expr->ts.kind);
  scalbn = gfc_builtin_decl_for_float_kind (BUILT_IN_SCALBN, expr->ts.kind);

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  args[0] = gfc_evaluate_now (args[0], &se->pre);

  tmp = gfc_create_var (integer_type_node, NULL);
  tmp = build_call_expr_loc (input_location, frexp, 2,
			     fold_convert (type, args[0]),
			     gfc_build_addr_expr (NULL_TREE, tmp));
  res = build_call_expr_loc (input_location, scalbn, 2, tmp,
			     fold_convert (integer_type_node, args[1]));
  res = fold_convert (type, res);

  /* Call to isfinite */
  cond = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_ISFINITE),
			      1, args[0]);
  nan = gfc_build_nan (type, "");

  se->expr = fold_build3_loc (input_location, COND_EXPR, type, cond,
			      res, nan);
}


static void
gfc_conv_intrinsic_size (gfc_se * se, gfc_expr * expr)
{
  gfc_actual_arglist *actual;
  tree arg1;
  tree type;
  tree size;
  gfc_se argse;
  gfc_expr *e;
  gfc_symbol *sym = NULL;

  gfc_init_se (&argse, NULL);
  actual = expr->value.function.actual;

  if (actual->expr->ts.type == BT_CLASS)
    gfc_add_class_array_ref (actual->expr);

  e = actual->expr;

  /* These are emerging from the interface mapping, when a class valued
     function appears as the rhs in a realloc on assign statement, where
     the size of the result is that of one of the actual arguments.  */
  if (e->expr_type == EXPR_VARIABLE
      && e->symtree->n.sym->ns == NULL /* This is distinctive!  */
      && e->symtree->n.sym->ts.type == BT_CLASS
      && e->ref && e->ref->type == REF_COMPONENT
      && strcmp (e->ref->u.c.component->name, "_data") == 0)
    sym = e->symtree->n.sym;

  if ((gfc_option.rtcheck & GFC_RTCHECK_POINTER)
      && e
      && (e->expr_type == EXPR_VARIABLE || e->expr_type == EXPR_FUNCTION))
    {
      symbol_attribute attr;
      char *msg;
      tree temp;
      tree cond;

      if (e->symtree->n.sym && IS_CLASS_ARRAY (e->symtree->n.sym))
	{
	  attr = CLASS_DATA (e->symtree->n.sym)->attr;
	  attr.pointer = attr.class_pointer;
	}
      else
	attr = gfc_expr_attr (e);

      if (attr.allocatable)
	msg = xasprintf ("Allocatable argument '%s' is not allocated",
			 e->symtree->n.sym->name);
      else if (attr.pointer)
	msg = xasprintf ("Pointer argument '%s' is not associated",
			 e->symtree->n.sym->name);
      else
	goto end_arg_check;

      if (sym)
	{
	  temp = gfc_class_data_get (sym->backend_decl);
	  temp = gfc_conv_descriptor_data_get (temp);
	}
      else
	{
	  argse.descriptor_only = 1;
	  gfc_conv_expr_descriptor (&argse, actual->expr);
	  temp = gfc_conv_descriptor_data_get (argse.expr);
	}

      cond = fold_build2_loc (input_location, EQ_EXPR,
			      logical_type_node, temp,
			      fold_convert (TREE_TYPE (temp),
					    null_pointer_node));
      gfc_trans_runtime_check (true, false, cond, &argse.pre, &e->where, msg);

      free (msg);
    }
 end_arg_check:

  argse.data_not_needed = 1;
  if (gfc_is_class_array_function (e))
    {
      /* For functions that return a class array conv_expr_descriptor is not
	 able to get the descriptor right.  Therefore this special case.  */
      gfc_conv_expr_reference (&argse, e);
      argse.expr = gfc_class_data_get (argse.expr);
    }
  else if (sym && sym->backend_decl)
    {
      gcc_assert (GFC_CLASS_TYPE_P (TREE_TYPE (sym->backend_decl)));
      argse.expr = gfc_class_data_get (sym->backend_decl);
    }
  else
    gfc_conv_expr_descriptor (&argse, actual->expr);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);
  arg1 = argse.expr;

  actual = actual->next;
  if (actual->expr)
    {
      stmtblock_t block;
      gfc_init_block (&block);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_type (&argse, actual->expr,
			  gfc_array_index_type);
      gfc_add_block_to_block (&block, &argse.pre);
      tree tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			     argse.expr, gfc_index_one_node);
      size = gfc_tree_array_size (&block, arg1, e, tmp);

      /* Unusually, for an intrinsic, size does not exclude
	 an optional arg2, so we must test for it.  */
      if (actual->expr->expr_type == EXPR_VARIABLE
	    && actual->expr->symtree->n.sym->attr.dummy
	    && actual->expr->symtree->n.sym->attr.optional)
	{
	  tree cond;
	  stmtblock_t block2;
	  gfc_init_block (&block2);
	  gfc_init_se (&argse, NULL);
	  argse.want_pointer = 1;
	  argse.data_not_needed = 1;
	  gfc_conv_expr (&argse, actual->expr);
	  gfc_add_block_to_block (&se->pre, &argse.pre);
	  /* 'block2' contains the arg2 absent case, 'block' the arg2 present
	      case; size_var can be used in both blocks. */
	  tree size_var = gfc_create_var (TREE_TYPE (size), "size");
	  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
				 TREE_TYPE (size_var), size_var, size);
	  gfc_add_expr_to_block (&block, tmp);
	  size = gfc_tree_array_size (&block2, arg1, e, NULL_TREE);
	  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
				 TREE_TYPE (size_var), size_var, size);
	  gfc_add_expr_to_block (&block2, tmp);
	  cond = gfc_conv_expr_present (actual->expr->symtree->n.sym);
	  tmp = build3_v (COND_EXPR, cond, gfc_finish_block (&block),
			  gfc_finish_block (&block2));
	  gfc_add_expr_to_block (&se->pre, tmp);
	  size = size_var;
	}
      else
	gfc_add_block_to_block (&se->pre, &block);
    }
  else
    size = gfc_tree_array_size (&se->pre, arg1, e, NULL_TREE);
  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = convert (type, size);
}


/* Helper function to compute the size of a character variable,
   excluding the terminating null characters.  The result has
   gfc_array_index_type type.  */

tree
size_of_string_in_bytes (int kind, tree string_length)
{
  tree bytesize;
  int i = gfc_validate_kind (BT_CHARACTER, kind, false);

  bytesize = build_int_cst (gfc_array_index_type,
			    gfc_character_kinds[i].bit_size / 8);

  return fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			  bytesize,
			  fold_convert (gfc_array_index_type, string_length));
}


static void
gfc_conv_intrinsic_sizeof (gfc_se *se, gfc_expr *expr)
{
  gfc_expr *arg;
  gfc_se argse;
  tree source_bytes;
  tree tmp;
  tree lower;
  tree upper;
  tree byte_size;
  tree field;
  int n;

  gfc_init_se (&argse, NULL);
  arg = expr->value.function.actual->expr;

  if (arg->rank || arg->ts.type == BT_ASSUMED)
    gfc_conv_expr_descriptor (&argse, arg);
  else
    gfc_conv_expr_reference (&argse, arg);

  if (arg->ts.type == BT_ASSUMED)
    {
      /* This only works if an array descriptor has been passed; thus, extract
	 the size from the descriptor.  */
      gcc_assert (TYPE_PRECISION (gfc_array_index_type)
		  == TYPE_PRECISION (size_type_node));
      tmp = arg->symtree->n.sym->backend_decl;
      tmp = DECL_LANG_SPECIFIC (tmp)
	    && GFC_DECL_SAVED_DESCRIPTOR (tmp) != NULL_TREE
	    ? GFC_DECL_SAVED_DESCRIPTOR (tmp) : tmp;
      if (POINTER_TYPE_P (TREE_TYPE (tmp)))
	tmp = build_fold_indirect_ref_loc (input_location, tmp);

      tmp = gfc_conv_descriptor_dtype (tmp);
      field = gfc_advance_chain (TYPE_FIELDS (get_dtype_type_node ()),
				 GFC_DTYPE_ELEM_LEN);
      tmp = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			     tmp, field, NULL_TREE);

      byte_size = fold_convert (gfc_array_index_type, tmp);
    }
  else if (arg->ts.type == BT_CLASS)
    {
      /* Conv_expr_descriptor returns a component_ref to _data component of the
	 class object.  The class object may be a non-pointer object, e.g.
	 located on the stack, or a memory location pointed to, e.g. a
	 parameter, i.e., an indirect_ref.  */
      if (POINTER_TYPE_P (TREE_TYPE (argse.expr))
	  && GFC_CLASS_TYPE_P (TREE_TYPE (TREE_TYPE (argse.expr))))
	byte_size
	  = gfc_class_vtab_size_get (build_fold_indirect_ref (argse.expr));
      else if (GFC_CLASS_TYPE_P (TREE_TYPE (argse.expr)))
	byte_size = gfc_class_vtab_size_get (argse.expr);
      else if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (argse.expr))
	       && TREE_CODE (argse.expr) == COMPONENT_REF)
	byte_size = gfc_class_vtab_size_get (TREE_OPERAND (argse.expr, 0));
      else if (arg->rank > 0
	       || (arg->rank == 0
		   && arg->ref && arg->ref->type == REF_COMPONENT))
	{
	  /* The scalarizer added an additional temp.  To get the class' vptr
	     one has to look at the original backend_decl.  */
	  if (argse.class_container)
	    byte_size = gfc_class_vtab_size_get (argse.class_container);
	  else if (DECL_LANG_SPECIFIC (arg->symtree->n.sym->backend_decl))
	    byte_size = gfc_class_vtab_size_get (
	      GFC_DECL_SAVED_DESCRIPTOR (arg->symtree->n.sym->backend_decl));
	  else
	    gcc_unreachable ();
	}
      else
	gcc_unreachable ();
    }
  else
    {
      if (arg->ts.type == BT_CHARACTER)
	byte_size = size_of_string_in_bytes (arg->ts.kind, argse.string_length);
      else
	{
	  if (arg->rank == 0)
	    byte_size = TREE_TYPE (build_fold_indirect_ref_loc (input_location,
								argse.expr));
	  else
	    byte_size = gfc_get_element_type (TREE_TYPE (argse.expr));
	  byte_size = fold_convert (gfc_array_index_type,
				    size_in_bytes (byte_size));
	}
    }

  if (arg->rank == 0)
    se->expr = byte_size;
  else
    {
      source_bytes = gfc_create_var (gfc_array_index_type, "bytes");
      gfc_add_modify (&argse.pre, source_bytes, byte_size);

      if (arg->rank == -1)
	{
	  tree cond, loop_var, exit_label;
          stmtblock_t body;

	  tmp = fold_convert (gfc_array_index_type,
			      gfc_conv_descriptor_rank (argse.expr));
	  loop_var = gfc_create_var (gfc_array_index_type, "i");
	  gfc_add_modify (&argse.pre, loop_var, gfc_index_zero_node);
          exit_label = gfc_build_label_decl (NULL_TREE);

	  /* Create loop:
	     for (;;)
		{
		  if (i >= rank)
		    goto exit;
		  source_bytes = source_bytes * array.dim[i].extent;
		  i = i + 1;
		}
	      exit:  */
	  gfc_start_block (&body);
	  cond = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
				  loop_var, tmp);
	  tmp = build1_v (GOTO_EXPR, exit_label);
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 cond, tmp, build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&body, tmp);

	  lower = gfc_conv_descriptor_lbound_get (argse.expr, loop_var);
	  upper = gfc_conv_descriptor_ubound_get (argse.expr, loop_var);
	  tmp = gfc_conv_array_extent_dim (lower, upper, NULL);
	  tmp = fold_build2_loc (input_location, MULT_EXPR,
				 gfc_array_index_type, tmp, source_bytes);
	  gfc_add_modify (&body, source_bytes, tmp);

	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, loop_var,
				 gfc_index_one_node);
	  gfc_add_modify_loc (input_location, &body, loop_var, tmp);

	  tmp = gfc_finish_block (&body);

	  tmp = fold_build1_loc (input_location, LOOP_EXPR, void_type_node,
				 tmp);
	  gfc_add_expr_to_block (&argse.pre, tmp);

	  tmp = build1_v (LABEL_EXPR, exit_label);
	  gfc_add_expr_to_block (&argse.pre, tmp);
	}
      else
	{
	  /* Obtain the size of the array in bytes.  */
	  for (n = 0; n < arg->rank; n++)
	    {
	      tree idx;
	      idx = gfc_rank_cst[n];
	      lower = gfc_conv_descriptor_lbound_get (argse.expr, idx);
	      upper = gfc_conv_descriptor_ubound_get (argse.expr, idx);
	      tmp = gfc_conv_array_extent_dim (lower, upper, NULL);
	      tmp = fold_build2_loc (input_location, MULT_EXPR,
				     gfc_array_index_type, tmp, source_bytes);
	      gfc_add_modify (&argse.pre, source_bytes, tmp);
	    }
	}
      se->expr = source_bytes;
    }

  gfc_add_block_to_block (&se->pre, &argse.pre);
}


static void
gfc_conv_intrinsic_storage_size (gfc_se *se, gfc_expr *expr)
{
  gfc_expr *arg;
  gfc_se argse;
  tree type, result_type, tmp, class_decl = NULL;
  gfc_symbol *sym;
  bool unlimited = false;

  arg = expr->value.function.actual->expr;

  gfc_init_se (&argse, NULL);
  result_type = gfc_get_int_type (expr->ts.kind);

  if (arg->rank == 0)
    {
      if (arg->ts.type == BT_CLASS)
	{
	  unlimited = UNLIMITED_POLY (arg);
	  gfc_add_vptr_component (arg);
	  gfc_add_size_component (arg);
	  gfc_conv_expr (&argse, arg);
	  tmp = fold_convert (result_type, argse.expr);
	  class_decl = gfc_get_class_from_expr (argse.expr);
	  goto done;
	}

      gfc_conv_expr_reference (&argse, arg);
      type = TREE_TYPE (build_fold_indirect_ref_loc (input_location,
						     argse.expr));
    }
  else
    {
      argse.want_pointer = 0;
      gfc_conv_expr_descriptor (&argse, arg);
      sym = arg->expr_type == EXPR_VARIABLE ? arg->symtree->n.sym : NULL;
      if (arg->ts.type == BT_CLASS)
	{
	  unlimited = UNLIMITED_POLY (arg);
	  if (TREE_CODE (argse.expr) == COMPONENT_REF)
	    tmp = gfc_class_vtab_size_get (TREE_OPERAND (argse.expr, 0));
	  else if (arg->rank > 0 && sym
		   && DECL_LANG_SPECIFIC (sym->backend_decl))
	    tmp = gfc_class_vtab_size_get (
		 GFC_DECL_SAVED_DESCRIPTOR (sym->backend_decl));
	  else
	    gcc_unreachable ();
	  tmp = fold_convert (result_type, tmp);
	  class_decl = gfc_get_class_from_expr (argse.expr);
	  goto done;
	}
      type = gfc_get_element_type (TREE_TYPE (argse.expr));
    }

  /* Obtain the argument's word length.  */
  if (arg->ts.type == BT_CHARACTER)
    tmp = size_of_string_in_bytes (arg->ts.kind, argse.string_length);
  else
    tmp = size_in_bytes (type);
  tmp = fold_convert (result_type, tmp);

done:
  if (unlimited && class_decl)
    tmp = gfc_resize_class_size_with_len (NULL, class_decl, tmp);

  se->expr = fold_build2_loc (input_location, MULT_EXPR, result_type, tmp,
			      build_int_cst (result_type, BITS_PER_UNIT));
  gfc_add_block_to_block (&se->pre, &argse.pre);
}


/* Intrinsic string comparison functions.  */

static void
gfc_conv_intrinsic_strcmp (gfc_se * se, gfc_expr * expr, enum tree_code op)
{
  tree args[4];

  gfc_conv_intrinsic_function_args (se, expr, args, 4);

  se->expr
    = gfc_build_compare_string (args[0], args[1], args[2], args[3],
				expr->value.function.actual->expr->ts.kind,
				op);
  se->expr = fold_build2_loc (input_location, op,
			      gfc_typenode_for_spec (&expr->ts), se->expr,
			      build_int_cst (TREE_TYPE (se->expr), 0));
}

/* Generate a call to the adjustl/adjustr library function.  */
static void
gfc_conv_intrinsic_adjust (gfc_se * se, gfc_expr * expr, tree fndecl)
{
  tree args[3];
  tree len;
  tree type;
  tree var;
  tree tmp;

  gfc_conv_intrinsic_function_args (se, expr, &args[1], 2);
  len = args[1];

  type = TREE_TYPE (args[2]);
  var = gfc_conv_string_tmp (se, type, len);
  args[0] = var;

  tmp = build_call_expr_loc (input_location,
			 fndecl, 3, args[0], args[1], args[2]);
  gfc_add_expr_to_block (&se->pre, tmp);
  se->expr = var;
  se->string_length = len;
}


/* Generate code for the TRANSFER intrinsic:
	For scalar results:
	  DEST = TRANSFER (SOURCE, MOLD)
	where:
	  typeof<DEST> = typeof<MOLD>
	and:
	  MOLD is scalar.

	For array results:
	  DEST(1:N) = TRANSFER (SOURCE, MOLD[, SIZE])
	where:
	  typeof<DEST> = typeof<MOLD>
	and:
	  N = min (sizeof (SOURCE(:)), sizeof (DEST(:)),
	      sizeof (DEST(0) * SIZE).  */
static void
gfc_conv_intrinsic_transfer (gfc_se * se, gfc_expr * expr)
{
  tree tmp;
  tree tmpdecl;
  tree ptr;
  tree extent;
  tree source;
  tree source_type;
  tree source_bytes;
  tree mold_type;
  tree dest_word_len;
  tree size_words;
  tree size_bytes;
  tree upper;
  tree lower;
  tree stmt;
  tree class_ref = NULL_TREE;
  gfc_actual_arglist *arg;
  gfc_se argse;
  gfc_array_info *info;
  stmtblock_t block;
  int n;
  bool scalar_mold;
  gfc_expr *source_expr, *mold_expr, *class_expr;

  info = NULL;
  if (se->loop)
    info = &se->ss->info->data.array;

  /* Convert SOURCE.  The output from this stage is:-
	source_bytes = length of the source in bytes
	source = pointer to the source data.  */
  arg = expr->value.function.actual;
  source_expr = arg->expr;

  /* Ensure double transfer through LOGICAL preserves all
     the needed bits.  */
  if (arg->expr->expr_type == EXPR_FUNCTION
	&& arg->expr->value.function.esym == NULL
	&& arg->expr->value.function.isym != NULL
	&& arg->expr->value.function.isym->id == GFC_ISYM_TRANSFER
	&& arg->expr->ts.type == BT_LOGICAL
	&& expr->ts.type != arg->expr->ts.type)
    arg->expr->value.function.name = "__transfer_in_transfer";

  gfc_init_se (&argse, NULL);

  source_bytes = gfc_create_var (gfc_array_index_type, NULL);

  /* Obtain the pointer to source and the length of source in bytes.  */
  if (arg->expr->rank == 0)
    {
      gfc_conv_expr_reference (&argse, arg->expr);
      if (arg->expr->ts.type == BT_CLASS)
	{
	  tmp = build_fold_indirect_ref_loc (input_location, argse.expr);
	  if (GFC_CLASS_TYPE_P (TREE_TYPE (tmp)))
	    {
	      source = gfc_class_data_get (tmp);
	      class_ref = tmp;
	    }
	  else
	    {
	      /* Array elements are evaluated as a reference to the data.
		 To obtain the vptr for the element size, the argument
		 expression must be stripped to the class reference and
		 re-evaluated. The pre and post blocks are not needed.  */
	      gcc_assert (arg->expr->expr_type == EXPR_VARIABLE);
	      source = argse.expr;
	      class_expr = gfc_find_and_cut_at_last_class_ref (arg->expr);
	      gfc_init_se (&argse, NULL);
	      gfc_conv_expr (&argse, class_expr);
	      class_ref = argse.expr;
	    }
	}
      else
	source = argse.expr;

      /* Obtain the source word length.  */
      switch (arg->expr->ts.type)
	{
	case BT_CHARACTER:
	  tmp = size_of_string_in_bytes (arg->expr->ts.kind,
					 argse.string_length);
	  break;
	case BT_CLASS:
	  if (class_ref != NULL_TREE)
	    {
	      tmp = gfc_class_vtab_size_get (class_ref);
	      if (UNLIMITED_POLY (source_expr))
		tmp = gfc_resize_class_size_with_len (NULL, class_ref, tmp);
	    }
	  else
	    {
	      tmp = gfc_class_vtab_size_get (argse.expr);
	      if (UNLIMITED_POLY (source_expr))
		tmp = gfc_resize_class_size_with_len (NULL, argse.expr, tmp);
	    }
	  break;
	default:
	  source_type = TREE_TYPE (build_fold_indirect_ref_loc (input_location,
								source));
	  tmp = fold_convert (gfc_array_index_type,
			      size_in_bytes (source_type));
	  break;
	}
    }
  else
    {
      argse.want_pointer = 0;
      gfc_conv_expr_descriptor (&argse, arg->expr);
      source = gfc_conv_descriptor_data_get (argse.expr);
      source_type = gfc_get_element_type (TREE_TYPE (argse.expr));

      /* Repack the source if not simply contiguous.  */
      if (!gfc_is_simply_contiguous (arg->expr, false, true))
	{
	  tmp = gfc_build_addr_expr (NULL_TREE, argse.expr);

	  if (warn_array_temporaries)
	    gfc_warning (OPT_Warray_temporaries,
			 "Creating array temporary at %L", &expr->where);

	  source = build_call_expr_loc (input_location,
				    gfor_fndecl_in_pack, 1, tmp);
	  source = gfc_evaluate_now (source, &argse.pre);

	  /* Free the temporary.  */
	  gfc_start_block (&block);
	  tmp = gfc_call_free (source);
	  gfc_add_expr_to_block (&block, tmp);
	  stmt = gfc_finish_block (&block);

	  /* Clean up if it was repacked.  */
	  gfc_init_block (&block);
	  tmp = gfc_conv_array_data (argse.expr);
	  tmp = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				 source, tmp);
	  tmp = build3_v (COND_EXPR, tmp, stmt,
			  build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&block, tmp);
	  gfc_add_block_to_block (&block, &se->post);
	  gfc_init_block (&se->post);
	  gfc_add_block_to_block (&se->post, &block);
	}

      /* Obtain the source word length.  */
      if (arg->expr->ts.type == BT_CHARACTER)
	tmp = size_of_string_in_bytes (arg->expr->ts.kind,
				       argse.string_length);
      else if (arg->expr->ts.type == BT_CLASS)
	{
	  class_ref = TREE_OPERAND (argse.expr, 0);
	  tmp = gfc_class_vtab_size_get (class_ref);
	  if (UNLIMITED_POLY (arg->expr))
	    tmp = gfc_resize_class_size_with_len (&argse.pre, class_ref, tmp);
	}
      else
	tmp = fold_convert (gfc_array_index_type,
			    size_in_bytes (source_type));

      /* Obtain the size of the array in bytes.  */
      extent = gfc_create_var (gfc_array_index_type, NULL);
      for (n = 0; n < arg->expr->rank; n++)
	{
	  tree idx;
	  idx = gfc_rank_cst[n];
	  gfc_add_modify (&argse.pre, source_bytes, tmp);
	  lower = gfc_conv_descriptor_lbound_get (argse.expr, idx);
	  upper = gfc_conv_descriptor_ubound_get (argse.expr, idx);
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type, upper, lower);
	  gfc_add_modify (&argse.pre, extent, tmp);
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, extent,
				 gfc_index_one_node);
	  tmp = fold_build2_loc (input_location, MULT_EXPR,
				 gfc_array_index_type, tmp, source_bytes);
	}
    }

  gfc_add_modify (&argse.pre, source_bytes, tmp);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);

  /* Now convert MOLD.  The outputs are:
	mold_type = the TREE type of MOLD
	dest_word_len = destination word length in bytes.  */
  arg = arg->next;
  mold_expr = arg->expr;

  gfc_init_se (&argse, NULL);

  scalar_mold = arg->expr->rank == 0;

  if (arg->expr->rank == 0)
    {
      gfc_conv_expr_reference (&argse, mold_expr);
      mold_type = TREE_TYPE (build_fold_indirect_ref_loc (input_location,
							  argse.expr));
    }
  else
    {
      argse.want_pointer = 0;
      gfc_conv_expr_descriptor (&argse, mold_expr);
      mold_type = gfc_get_element_type (TREE_TYPE (argse.expr));
    }

  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);

  if (strcmp (expr->value.function.name, "__transfer_in_transfer") == 0)
    {
      /* If this TRANSFER is nested in another TRANSFER, use a type
	 that preserves all bits.  */
      if (mold_expr->ts.type == BT_LOGICAL)
	mold_type = gfc_get_int_type (mold_expr->ts.kind);
    }

  /* Obtain the destination word length.  */
  switch (mold_expr->ts.type)
    {
    case BT_CHARACTER:
      tmp = size_of_string_in_bytes (mold_expr->ts.kind, argse.string_length);
      mold_type = gfc_get_character_type_len (mold_expr->ts.kind,
					      argse.string_length);
      break;
    case BT_CLASS:
      if (scalar_mold)
	class_ref = argse.expr;
      else
	class_ref = TREE_OPERAND (argse.expr, 0);
      tmp = gfc_class_vtab_size_get (class_ref);
      if (UNLIMITED_POLY (arg->expr))
	tmp = gfc_resize_class_size_with_len (&argse.pre, class_ref, tmp);
      break;
    default:
      tmp = fold_convert (gfc_array_index_type, size_in_bytes (mold_type));
      break;
    }

  /* Do not fix dest_word_len if it is a variable, since the temporary can wind
     up being used before the assignment.  */
  if (mold_expr->ts.type == BT_CHARACTER && mold_expr->ts.deferred)
    dest_word_len = tmp;
  else
    {
      dest_word_len = gfc_create_var (gfc_array_index_type, NULL);
      gfc_add_modify (&se->pre, dest_word_len, tmp);
    }

  /* Finally convert SIZE, if it is present.  */
  arg = arg->next;
  size_words = gfc_create_var (gfc_array_index_type, NULL);

  if (arg->expr)
    {
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_reference (&argse, arg->expr);
      tmp = convert (gfc_array_index_type,
		     build_fold_indirect_ref_loc (input_location,
					      argse.expr));
      gfc_add_block_to_block (&se->pre, &argse.pre);
      gfc_add_block_to_block (&se->post, &argse.post);
    }
  else
    tmp = NULL_TREE;

  /* Separate array and scalar results.  */
  if (scalar_mold && tmp == NULL_TREE)
    goto scalar_transfer;

  size_bytes = gfc_create_var (gfc_array_index_type, NULL);
  if (tmp != NULL_TREE)
    tmp = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			   tmp, dest_word_len);
  else
    tmp = source_bytes;

  gfc_add_modify (&se->pre, size_bytes, tmp);
  gfc_add_modify (&se->pre, size_words,
		       fold_build2_loc (input_location, CEIL_DIV_EXPR,
					gfc_array_index_type,
					size_bytes, dest_word_len));

  /* Evaluate the bounds of the result.  If the loop range exists, we have
     to check if it is too large.  If so, we modify loop->to be consistent
     with min(size, size(source)).  Otherwise, size is made consistent with
     the loop range, so that the right number of bytes is transferred.*/
  n = se->loop->order[0];
  if (se->loop->to[n] != NULL_TREE)
    {
      tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			     se->loop->to[n], se->loop->from[n]);
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			     tmp, gfc_index_one_node);
      tmp = fold_build2_loc (input_location, MIN_EXPR, gfc_array_index_type,
			 tmp, size_words);
      gfc_add_modify (&se->pre, size_words, tmp);
      gfc_add_modify (&se->pre, size_bytes,
			   fold_build2_loc (input_location, MULT_EXPR,
					    gfc_array_index_type,
					    size_words, dest_word_len));
      upper = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			       size_words, se->loop->from[n]);
      upper = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			       upper, gfc_index_one_node);
    }
  else
    {
      upper = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			       size_words, gfc_index_one_node);
      se->loop->from[n] = gfc_index_zero_node;
    }

  se->loop->to[n] = upper;

  /* Build a destination descriptor, using the pointer, source, as the
     data field.  */
  gfc_trans_create_temp_array (&se->pre, &se->post, se->ss, mold_type,
			       NULL_TREE, false, true, false, &expr->where);

  /* Cast the pointer to the result.  */
  tmp = gfc_conv_descriptor_data_get (info->descriptor);
  tmp = fold_convert (pvoid_type_node, tmp);

  /* Use memcpy to do the transfer.  */
  tmp
    = build_call_expr_loc (input_location,
			   builtin_decl_explicit (BUILT_IN_MEMCPY), 3, tmp,
			   fold_convert (pvoid_type_node, source),
			   fold_convert (size_type_node,
					 fold_build2_loc (input_location,
							  MIN_EXPR,
							  gfc_array_index_type,
							  size_bytes,
							  source_bytes)));
  gfc_add_expr_to_block (&se->pre, tmp);

  se->expr = info->descriptor;
  if (expr->ts.type == BT_CHARACTER)
    {
      tmp = fold_convert (gfc_charlen_type_node,
			  TYPE_SIZE_UNIT (gfc_get_char_type (expr->ts.kind)));
      se->string_length = fold_build2_loc (input_location, TRUNC_DIV_EXPR,
					   gfc_charlen_type_node,
					   dest_word_len, tmp);
    }

  return;

/* Deal with scalar results.  */
scalar_transfer:
  extent = fold_build2_loc (input_location, MIN_EXPR, gfc_array_index_type,
			    dest_word_len, source_bytes);
  extent = fold_build2_loc (input_location, MAX_EXPR, gfc_array_index_type,
			    extent, gfc_index_zero_node);

  if (expr->ts.type == BT_CHARACTER)
    {
      tree direct, indirect, free;

      ptr = convert (gfc_get_pchar_type (expr->ts.kind), source);
      tmpdecl = gfc_create_var (gfc_get_pchar_type (expr->ts.kind),
				"transfer");

      /* If source is longer than the destination, use a pointer to
	 the source directly.  */
      gfc_init_block (&block);
      gfc_add_modify (&block, tmpdecl, ptr);
      direct = gfc_finish_block (&block);

      /* Otherwise, allocate a string with the length of the destination
	 and copy the source into it.  */
      gfc_init_block (&block);
      tmp = gfc_get_pchar_type (expr->ts.kind);
      tmp = gfc_call_malloc (&block, tmp, dest_word_len);
      gfc_add_modify (&block, tmpdecl,
		      fold_convert (TREE_TYPE (ptr), tmp));
      tmp = build_call_expr_loc (input_location,
			     builtin_decl_explicit (BUILT_IN_MEMCPY), 3,
			     fold_convert (pvoid_type_node, tmpdecl),
			     fold_convert (pvoid_type_node, ptr),
			     fold_convert (size_type_node, extent));
      gfc_add_expr_to_block (&block, tmp);
      indirect = gfc_finish_block (&block);

      /* Wrap it up with the condition.  */
      tmp = fold_build2_loc (input_location, LE_EXPR, logical_type_node,
			     dest_word_len, source_bytes);
      tmp = build3_v (COND_EXPR, tmp, direct, indirect);
      gfc_add_expr_to_block (&se->pre, tmp);

      /* Free the temporary string, if necessary.  */
      free = gfc_call_free (tmpdecl);
      tmp = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			     dest_word_len, source_bytes);
      tmp = build3_v (COND_EXPR, tmp, free, build_empty_stmt (input_location));
      gfc_add_expr_to_block (&se->post, tmp);

      se->expr = tmpdecl;
      tmp = fold_convert (gfc_charlen_type_node,
			  TYPE_SIZE_UNIT (gfc_get_char_type (expr->ts.kind)));
      se->string_length = fold_build2_loc (input_location, TRUNC_DIV_EXPR,
					   gfc_charlen_type_node,
					   dest_word_len, tmp);
    }
  else
    {
      tmpdecl = gfc_create_var (mold_type, "transfer");

      ptr = convert (build_pointer_type (mold_type), source);

      /* For CLASS results, allocate the needed memory first.  */
      if (mold_expr->ts.type == BT_CLASS)
	{
	  tree cdata;
	  cdata = gfc_class_data_get (tmpdecl);
	  tmp = gfc_call_malloc (&se->pre, TREE_TYPE (cdata), dest_word_len);
	  gfc_add_modify (&se->pre, cdata, tmp);
	}

      /* Use memcpy to do the transfer.  */
      if (mold_expr->ts.type == BT_CLASS)
	tmp = gfc_class_data_get (tmpdecl);
      else
	tmp = gfc_build_addr_expr (NULL_TREE, tmpdecl);

      tmp = build_call_expr_loc (input_location,
			     builtin_decl_explicit (BUILT_IN_MEMCPY), 3,
			     fold_convert (pvoid_type_node, tmp),
			     fold_convert (pvoid_type_node, ptr),
			     fold_convert (size_type_node, extent));
      gfc_add_expr_to_block (&se->pre, tmp);

      /* For CLASS results, set the _vptr.  */
      if (mold_expr->ts.type == BT_CLASS)
	gfc_reset_vptr (&se->pre, nullptr, tmpdecl, source_expr->ts.u.derived);

      se->expr = tmpdecl;
    }
}


/* Generate code for the ALLOCATED intrinsic.
   Generate inline code that directly check the address of the argument.  */

static void
gfc_conv_allocated (gfc_se *se, gfc_expr *expr)
{
  gfc_se arg1se;
  tree tmp;
  gfc_expr *e = expr->value.function.actual->expr;

  gfc_init_se (&arg1se, NULL);
  if (e->ts.type == BT_CLASS)
    {
      /* Make sure that class array expressions have both a _data
	 component reference and an array reference....  */
      if (CLASS_DATA (e)->attr.dimension)
	gfc_add_class_array_ref (e);
      /* .... whilst scalars only need the _data component.  */
      else
	gfc_add_data_component (e);
    }

  gcc_assert (flag_coarray != GFC_FCOARRAY_LIB || !gfc_is_coindexed (e));

  if (e->rank == 0)
    {
      /* Allocatable scalar.  */
      arg1se.want_pointer = 1;
      gfc_conv_expr (&arg1se, e);
      tmp = arg1se.expr;
    }
  else
    {
      /* Allocatable array.  */
      arg1se.descriptor_only = 1;
      gfc_conv_expr_descriptor (&arg1se, e);
      tmp = gfc_conv_descriptor_data_get (arg1se.expr);
    }

  tmp = fold_build2_loc (input_location, NE_EXPR, logical_type_node, tmp,
			 fold_convert (TREE_TYPE (tmp), null_pointer_node));

  /* Components of pointer array references sometimes come back with a pre block.  */
  if (arg1se.pre.head)
    gfc_add_block_to_block (&se->pre, &arg1se.pre);

  se->expr = convert (gfc_typenode_for_spec (&expr->ts), tmp);
}


/* Generate code for the ASSOCIATED intrinsic.
   If both POINTER and TARGET are arrays, generate a call to library function
   _gfor_associated, and pass descriptors of POINTER and TARGET to it.
   In other cases, generate inline code that directly compare the address of
   POINTER with the address of TARGET.  */

static void
gfc_conv_associated (gfc_se *se, gfc_expr *expr)
{
  gfc_actual_arglist *arg1;
  gfc_actual_arglist *arg2;
  gfc_se arg1se;
  gfc_se arg2se;
  tree tmp2;
  tree tmp;
  tree nonzero_arraylen = NULL_TREE;
  gfc_ss *ss;
  bool scalar;

  gfc_init_se (&arg1se, NULL);
  gfc_init_se (&arg2se, NULL);
  arg1 = expr->value.function.actual;
  arg2 = arg1->next;

  /* Check whether the expression is a scalar or not; we cannot use
     arg1->expr->rank as it can be nonzero for proc pointers.  */
  ss = gfc_walk_expr (arg1->expr);
  scalar = ss == gfc_ss_terminator;
  if (!scalar)
    gfc_free_ss_chain (ss);

  if (!arg2->expr)
    {
      /* No optional target.  */
      if (scalar)
        {
	  /* A pointer to a scalar.  */
	  arg1se.want_pointer = 1;
	  gfc_conv_expr (&arg1se, arg1->expr);
	  if (arg1->expr->symtree->n.sym->attr.proc_pointer
	      && arg1->expr->symtree->n.sym->attr.dummy)
	    arg1se.expr = build_fold_indirect_ref_loc (input_location,
						       arg1se.expr);
  	  if (arg1->expr->ts.type == BT_CLASS)
	    {
	      tmp2 = gfc_class_data_get (arg1se.expr);
	      if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (tmp2)))
		tmp2 = gfc_conv_descriptor_data_get (tmp2);
	    }
	  else
	    tmp2 = arg1se.expr;
        }
      else
        {
          /* A pointer to an array.  */
          gfc_conv_expr_descriptor (&arg1se, arg1->expr);
          tmp2 = gfc_conv_descriptor_data_get (arg1se.expr);
        }
      gfc_add_block_to_block (&se->pre, &arg1se.pre);
      gfc_add_block_to_block (&se->post, &arg1se.post);
      tmp = fold_build2_loc (input_location, NE_EXPR, logical_type_node, tmp2,
			     fold_convert (TREE_TYPE (tmp2), null_pointer_node));
      se->expr = tmp;
    }
  else
    {
      /* An optional target.  */
      if (arg2->expr->ts.type == BT_CLASS
	  && arg2->expr->expr_type != EXPR_FUNCTION)
	gfc_add_data_component (arg2->expr);

      if (scalar)
        {
	  /* A pointer to a scalar.  */
	  arg1se.want_pointer = 1;
	  gfc_conv_expr (&arg1se, arg1->expr);
	  if (arg1->expr->symtree->n.sym->attr.proc_pointer
	      && arg1->expr->symtree->n.sym->attr.dummy)
	    arg1se.expr = build_fold_indirect_ref_loc (input_location,
						       arg1se.expr);
	  if (arg1->expr->ts.type == BT_CLASS)
	    arg1se.expr = gfc_class_data_get (arg1se.expr);

	  arg2se.want_pointer = 1;
	  gfc_conv_expr (&arg2se, arg2->expr);
	  if (arg2->expr->symtree->n.sym->attr.proc_pointer
	      && arg2->expr->symtree->n.sym->attr.dummy)
	    arg2se.expr = build_fold_indirect_ref_loc (input_location,
						       arg2se.expr);
	  if (arg2->expr->ts.type == BT_CLASS)
	    {
	      arg2se.expr = gfc_evaluate_now (arg2se.expr, &arg2se.pre);
	      arg2se.expr = gfc_class_data_get (arg2se.expr);
	    }
	  gfc_add_block_to_block (&se->pre, &arg1se.pre);
	  gfc_add_block_to_block (&se->post, &arg1se.post);
	  gfc_add_block_to_block (&se->pre, &arg2se.pre);
	  gfc_add_block_to_block (&se->post, &arg2se.post);
          tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
				 arg1se.expr, arg2se.expr);
          tmp2 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				  arg1se.expr, null_pointer_node);
          se->expr = fold_build2_loc (input_location, TRUTH_AND_EXPR,
				      logical_type_node, tmp, tmp2);
        }
      else
        {
	  /* An array pointer of zero length is not associated if target is
	     present.  */
	  arg1se.descriptor_only = 1;
	  gfc_conv_expr_lhs (&arg1se, arg1->expr);
	  if (arg1->expr->rank == -1)
	    {
	      tmp = gfc_conv_descriptor_rank (arg1se.expr);
	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     TREE_TYPE (tmp), tmp,
				     build_int_cst (TREE_TYPE (tmp), 1));
	    }
	  else
	    tmp = gfc_rank_cst[arg1->expr->rank - 1];
	  tmp = gfc_conv_descriptor_stride_get (arg1se.expr, tmp);
	  if (arg2->expr->rank != 0)
	    nonzero_arraylen = fold_build2_loc (input_location, NE_EXPR,
						logical_type_node, tmp,
						build_int_cst (TREE_TYPE (tmp), 0));

	  /* A pointer to an array, call library function _gfor_associated.  */
	  arg1se.want_pointer = 1;
	  gfc_conv_expr_descriptor (&arg1se, arg1->expr);
	  gfc_add_block_to_block (&se->pre, &arg1se.pre);
	  gfc_add_block_to_block (&se->post, &arg1se.post);

	  arg2se.want_pointer = 1;
	  arg2se.force_no_tmp = 1;
	  if (arg2->expr->rank != 0)
	    gfc_conv_expr_descriptor (&arg2se, arg2->expr);
	  else
	    {
	      gfc_conv_expr (&arg2se, arg2->expr);
	      arg2se.expr
		= gfc_conv_scalar_to_descriptor (&arg2se, arg2se.expr,
						 gfc_expr_attr (arg2->expr));
	      arg2se.expr = gfc_build_addr_expr (NULL_TREE, arg2se.expr);
	    }
	  gfc_add_block_to_block (&se->pre, &arg2se.pre);
	  gfc_add_block_to_block (&se->post, &arg2se.post);
	  se->expr = build_call_expr_loc (input_location,
				      gfor_fndecl_associated, 2,
				      arg1se.expr, arg2se.expr);
	  se->expr = convert (logical_type_node, se->expr);
	  if (arg2->expr->rank != 0)
	    se->expr = fold_build2_loc (input_location, TRUTH_AND_EXPR,
					logical_type_node, se->expr,
					nonzero_arraylen);
        }

      /* If target is present zero character length pointers cannot
	 be associated.  */
      if (arg1->expr->ts.type == BT_CHARACTER)
	{
	  tmp = arg1se.string_length;
	  tmp = fold_build2_loc (input_location, NE_EXPR,
				 logical_type_node, tmp,
				 build_zero_cst (TREE_TYPE (tmp)));
	  se->expr = fold_build2_loc (input_location, TRUTH_AND_EXPR,
				      logical_type_node, se->expr, tmp);
	}
    }

  se->expr = convert (gfc_typenode_for_spec (&expr->ts), se->expr);
}


/* Generate code for the SAME_TYPE_AS intrinsic.
   Generate inline code that directly checks the vindices.  */

static void
gfc_conv_same_type_as (gfc_se *se, gfc_expr *expr)
{
  gfc_expr *a, *b;
  gfc_se se1, se2;
  tree tmp;
  tree conda = NULL_TREE, condb = NULL_TREE;

  gfc_init_se (&se1, NULL);
  gfc_init_se (&se2, NULL);

  a = expr->value.function.actual->expr;
  b = expr->value.function.actual->next->expr;

  bool unlimited_poly_a = UNLIMITED_POLY (a);
  bool unlimited_poly_b = UNLIMITED_POLY (b);
  if (unlimited_poly_a)
    {
      se1.want_pointer = 1;
      gfc_add_vptr_component (a);
    }
  else if (a->ts.type == BT_CLASS)
    {
      gfc_add_vptr_component (a);
      gfc_add_hash_component (a);
    }
  else if (a->ts.type == BT_DERIVED)
    a = gfc_get_int_expr (gfc_default_integer_kind, NULL,
			  a->ts.u.derived->hash_value);

  if (unlimited_poly_b)
    {
      se2.want_pointer = 1;
      gfc_add_vptr_component (b);
    }
  else if (b->ts.type == BT_CLASS)
    {
      gfc_add_vptr_component (b);
      gfc_add_hash_component (b);
    }
  else if (b->ts.type == BT_DERIVED)
    b = gfc_get_int_expr (gfc_default_integer_kind, NULL,
			  b->ts.u.derived->hash_value);

  gfc_conv_expr (&se1, a);
  gfc_conv_expr (&se2, b);

  if (unlimited_poly_a)
    {
      conda = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			       se1.expr,
			       build_int_cst (TREE_TYPE (se1.expr), 0));
      se1.expr = gfc_vptr_hash_get (se1.expr);
    }

  if (unlimited_poly_b)
    {
      condb = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			       se2.expr,
			       build_int_cst (TREE_TYPE (se2.expr), 0));
      se2.expr = gfc_vptr_hash_get (se2.expr);
    }

  tmp = fold_build2_loc (input_location, EQ_EXPR,
			 logical_type_node, se1.expr,
			 fold_convert (TREE_TYPE (se1.expr), se2.expr));

  if (conda)
    tmp = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			   logical_type_node, conda, tmp);

  if (condb)
    tmp = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			   logical_type_node, condb, tmp);

  se->expr = convert (gfc_typenode_for_spec (&expr->ts), tmp);
}


/* Generate code for SELECTED_CHAR_KIND (NAME) intrinsic function.  */

static void
gfc_conv_intrinsic_sc_kind (gfc_se *se, gfc_expr *expr)
{
  tree args[2];

  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  se->expr = build_call_expr_loc (input_location,
			      gfor_fndecl_sc_kind, 2, args[0], args[1]);
  se->expr = fold_convert (gfc_typenode_for_spec (&expr->ts), se->expr);
}


/* Generate code for SELECTED_INT_KIND (R) intrinsic function.  */

static void
gfc_conv_intrinsic_si_kind (gfc_se *se, gfc_expr *expr)
{
  tree arg, type;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);

  /* The argument to SELECTED_INT_KIND is INTEGER(4).  */
  type = gfc_get_int_type (4);
  arg = gfc_build_addr_expr (NULL_TREE, fold_convert (type, arg));

  /* Convert it to the required type.  */
  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = build_call_expr_loc (input_location,
			      gfor_fndecl_si_kind, 1, arg);
  se->expr = fold_convert (type, se->expr);
}


/* Generate code for SELECTED_LOGICAL_KIND (BITS) intrinsic function.  */

static void
gfc_conv_intrinsic_sl_kind (gfc_se *se, gfc_expr *expr)
{
  tree arg, type;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);

  /* The argument to SELECTED_LOGICAL_KIND is INTEGER(4).  */
  type = gfc_get_int_type (4);
  arg = gfc_build_addr_expr (NULL_TREE, fold_convert (type, arg));

  /* Convert it to the required type.  */
  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = build_call_expr_loc (input_location,
			      gfor_fndecl_sl_kind, 1, arg);
  se->expr = fold_convert (type, se->expr);
}


/* Generate code for SELECTED_REAL_KIND (P, R, RADIX) intrinsic function.  */

static void
gfc_conv_intrinsic_sr_kind (gfc_se *se, gfc_expr *expr)
{
  gfc_actual_arglist *actual;
  tree type;
  gfc_se argse;
  vec<tree, va_gc> *args = NULL;

  for (actual = expr->value.function.actual; actual; actual = actual->next)
    {
      gfc_init_se (&argse, se);

      /* Pass a NULL pointer for an absent arg.  */
      if (actual->expr == NULL)
        argse.expr = null_pointer_node;
      else
	{
	  gfc_typespec ts;
          gfc_clear_ts (&ts);

	  if (actual->expr->ts.kind != gfc_c_int_kind)
	    {
  	      /* The arguments to SELECTED_REAL_KIND are INTEGER(4).  */
	      ts.type = BT_INTEGER;
	      ts.kind = gfc_c_int_kind;
	      gfc_convert_type (actual->expr, &ts, 2);
	    }
	  gfc_conv_expr_reference (&argse, actual->expr);
	}

      gfc_add_block_to_block (&se->pre, &argse.pre);
      gfc_add_block_to_block (&se->post, &argse.post);
      vec_safe_push (args, argse.expr);
    }

  /* Convert it to the required type.  */
  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = build_call_expr_loc_vec (input_location,
				      gfor_fndecl_sr_kind, args);
  se->expr = fold_convert (type, se->expr);
}


/* Generate code for TRIM (A) intrinsic function.  */

static void
gfc_conv_intrinsic_trim (gfc_se * se, gfc_expr * expr)
{
  tree var;
  tree len;
  tree addr;
  tree tmp;
  tree cond;
  tree fndecl;
  tree function;
  tree *args;
  unsigned int num_args;

  num_args = gfc_intrinsic_argument_list_length (expr) + 2;
  args = XALLOCAVEC (tree, num_args);

  var = gfc_create_var (gfc_get_pchar_type (expr->ts.kind), "pstr");
  addr = gfc_build_addr_expr (ppvoid_type_node, var);
  len = gfc_create_var (gfc_charlen_type_node, "len");

  gfc_conv_intrinsic_function_args (se, expr, &args[2], num_args - 2);
  args[0] = gfc_build_addr_expr (NULL_TREE, len);
  args[1] = addr;

  if (expr->ts.kind == 1)
    function = gfor_fndecl_string_trim;
  else if (expr->ts.kind == 4)
    function = gfor_fndecl_string_trim_char4;
  else
    gcc_unreachable ();

  fndecl = build_addr (function);
  tmp = build_call_array_loc (input_location,
			  TREE_TYPE (TREE_TYPE (function)), fndecl,
			  num_args, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			  len, build_int_cst (TREE_TYPE (len), 0));
  tmp = gfc_call_free (var);
  tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&se->post, tmp);

  se->expr = var;
  se->string_length = len;
}


/* Generate code for REPEAT (STRING, NCOPIES) intrinsic function.  */

static void
gfc_conv_intrinsic_repeat (gfc_se * se, gfc_expr * expr)
{
  tree args[3], ncopies, dest, dlen, src, slen, ncopies_type;
  tree type, cond, tmp, count, exit_label, n, max, largest;
  tree size;
  stmtblock_t block, body;
  int i;

  /* We store in charsize the size of a character.  */
  i = gfc_validate_kind (BT_CHARACTER, expr->ts.kind, false);
  size = build_int_cst (sizetype, gfc_character_kinds[i].bit_size / 8);

  /* Get the arguments.  */
  gfc_conv_intrinsic_function_args (se, expr, args, 3);
  slen = fold_convert (sizetype, gfc_evaluate_now (args[0], &se->pre));
  src = args[1];
  ncopies = gfc_evaluate_now (args[2], &se->pre);
  ncopies_type = TREE_TYPE (ncopies);

  /* Check that NCOPIES is not negative.  */
  cond = fold_build2_loc (input_location, LT_EXPR, logical_type_node, ncopies,
			  build_int_cst (ncopies_type, 0));
  gfc_trans_runtime_check (true, false, cond, &se->pre, &expr->where,
			   "Argument NCOPIES of REPEAT intrinsic is negative "
			   "(its value is %ld)",
			   fold_convert (long_integer_type_node, ncopies));

  /* If the source length is zero, any non negative value of NCOPIES
     is valid, and nothing happens.  */
  n = gfc_create_var (ncopies_type, "ncopies");
  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, slen,
			  size_zero_node);
  tmp = fold_build3_loc (input_location, COND_EXPR, ncopies_type, cond,
			 build_int_cst (ncopies_type, 0), ncopies);
  gfc_add_modify (&se->pre, n, tmp);
  ncopies = n;

  /* Check that ncopies is not too large: ncopies should be less than
     (or equal to) MAX / slen, where MAX is the maximal integer of
     the gfc_charlen_type_node type.  If slen == 0, we need a special
     case to avoid the division by zero.  */
  max = fold_build2_loc (input_location, TRUNC_DIV_EXPR, sizetype,
			 fold_convert (sizetype,
				       TYPE_MAX_VALUE (gfc_charlen_type_node)),
			 slen);
  largest = TYPE_PRECISION (sizetype) > TYPE_PRECISION (ncopies_type)
	      ? sizetype : ncopies_type;
  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			  fold_convert (largest, ncopies),
			  fold_convert (largest, max));
  tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, slen,
			 size_zero_node);
  cond = fold_build3_loc (input_location, COND_EXPR, logical_type_node, tmp,
			  logical_false_node, cond);
  gfc_trans_runtime_check (true, false, cond, &se->pre, &expr->where,
			   "Argument NCOPIES of REPEAT intrinsic is too large");

  /* Compute the destination length.  */
  dlen = fold_build2_loc (input_location, MULT_EXPR, gfc_charlen_type_node,
			  fold_convert (gfc_charlen_type_node, slen),
			  fold_convert (gfc_charlen_type_node, ncopies));
  type = gfc_get_character_type (expr->ts.kind, expr->ts.u.cl);
  dest = gfc_conv_string_tmp (se, build_pointer_type (type), dlen);

  /* Generate the code to do the repeat operation:
       for (i = 0; i < ncopies; i++)
         memmove (dest + (i * slen * size), src, slen*size);  */
  gfc_start_block (&block);
  count = gfc_create_var (sizetype, "count");
  gfc_add_modify (&block, count, size_zero_node);
  exit_label = gfc_build_label_decl (NULL_TREE);

  /* Start the loop body.  */
  gfc_start_block (&body);

  /* Exit the loop if count >= ncopies.  */
  cond = fold_build2_loc (input_location, GE_EXPR, logical_type_node, count,
			  fold_convert (sizetype, ncopies));
  tmp = build1_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, cond, tmp,
			 build_empty_stmt (input_location));
  gfc_add_expr_to_block (&body, tmp);

  /* Call memmove (dest + (i*slen*size), src, slen*size).  */
  tmp = fold_build2_loc (input_location, MULT_EXPR, sizetype, slen,
			 count);
  tmp = fold_build2_loc (input_location, MULT_EXPR, sizetype, tmp,
			 size);
  tmp = fold_build_pointer_plus_loc (input_location,
				     fold_convert (pvoid_type_node, dest), tmp);
  tmp = build_call_expr_loc (input_location,
			     builtin_decl_explicit (BUILT_IN_MEMMOVE),
			     3, tmp, src,
			     fold_build2_loc (input_location, MULT_EXPR,
					      size_type_node, slen, size));
  gfc_add_expr_to_block (&body, tmp);

  /* Increment count.  */
  tmp = fold_build2_loc (input_location, PLUS_EXPR, sizetype,
			 count, size_one_node);
  gfc_add_modify (&body, count, tmp);

  /* Build the loop.  */
  tmp = build1_v (LOOP_EXPR, gfc_finish_block (&body));
  gfc_add_expr_to_block (&block, tmp);

  /* Add the exit label.  */
  tmp = build1_v (LABEL_EXPR, exit_label);
  gfc_add_expr_to_block (&block, tmp);

  /* Finish the block.  */
  tmp = gfc_finish_block (&block);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Set the result value.  */
  se->expr = dest;
  se->string_length = dlen;
}


/* Generate code for the IARGC intrinsic.  */

static void
gfc_conv_intrinsic_iargc (gfc_se * se, gfc_expr * expr)
{
  tree tmp;
  tree fndecl;
  tree type;

  /* Call the library function.  This always returns an INTEGER(4).  */
  fndecl = gfor_fndecl_iargc;
  tmp = build_call_expr_loc (input_location,
			 fndecl, 0);

  /* Convert it to the required type.  */
  type = gfc_typenode_for_spec (&expr->ts);
  tmp = fold_convert (type, tmp);

  se->expr = tmp;
}


/* Generate code for the KILL intrinsic.  */

static void
conv_intrinsic_kill (gfc_se *se, gfc_expr *expr)
{
  tree *args;
  tree int4_type_node = gfc_get_int_type (4);
  tree pid;
  tree sig;
  tree tmp;
  unsigned int num_args;

  num_args = gfc_intrinsic_argument_list_length (expr);
  args = XALLOCAVEC (tree, num_args);
  gfc_conv_intrinsic_function_args (se, expr, args, num_args);

  /* Convert PID to a INTEGER(4) entity.  */
  pid = convert (int4_type_node, args[0]);

  /* Convert SIG to a INTEGER(4) entity.  */
  sig = convert (int4_type_node, args[1]);

  tmp = build_call_expr_loc (input_location, gfor_fndecl_kill, 2, pid, sig);

  se->expr = fold_convert (TREE_TYPE (args[0]), tmp);
}


static tree
conv_intrinsic_kill_sub (gfc_code *code)
{
  stmtblock_t block;
  gfc_se se, se_stat;
  tree int4_type_node = gfc_get_int_type (4);
  tree pid;
  tree sig;
  tree statp;
  tree tmp;

  /* Make the function call.  */
  gfc_init_block (&block);
  gfc_init_se (&se, NULL);

  /* Convert PID to a INTEGER(4) entity.  */
  gfc_conv_expr (&se, code->ext.actual->expr);
  gfc_add_block_to_block (&block, &se.pre);
  pid = fold_convert (int4_type_node, gfc_evaluate_now (se.expr, &block));
  gfc_add_block_to_block (&block, &se.post);

  /* Convert SIG to a INTEGER(4) entity.  */
  gfc_conv_expr (&se, code->ext.actual->next->expr);
  gfc_add_block_to_block (&block, &se.pre);
  sig = fold_convert (int4_type_node, gfc_evaluate_now (se.expr, &block));
  gfc_add_block_to_block (&block, &se.post);

  /* Deal with an optional STATUS.  */
  if (code->ext.actual->next->next->expr)
    {
      gfc_init_se (&se_stat, NULL);
      gfc_conv_expr (&se_stat, code->ext.actual->next->next->expr);
      statp = gfc_create_var (gfc_get_int_type (4), "_statp");
    }
  else
    statp = NULL_TREE;

  tmp = build_call_expr_loc (input_location, gfor_fndecl_kill_sub, 3, pid, sig,
	statp ? gfc_build_addr_expr (NULL_TREE, statp) : null_pointer_node);

  gfc_add_expr_to_block (&block, tmp);

  if (statp && statp != se_stat.expr)
    gfc_add_modify (&block, se_stat.expr,
		    fold_convert (TREE_TYPE (se_stat.expr), statp));

  return gfc_finish_block (&block);
}



/* The loc intrinsic returns the address of its argument as
   gfc_index_integer_kind integer.  */

static void
gfc_conv_intrinsic_loc (gfc_se * se, gfc_expr * expr)
{
  tree temp_var;
  gfc_expr *arg_expr;

  gcc_assert (!se->ss);

  arg_expr = expr->value.function.actual->expr;
  if (arg_expr->rank == 0)
    {
      if (arg_expr->ts.type == BT_CLASS)
	gfc_add_data_component (arg_expr);
      gfc_conv_expr_reference (se, arg_expr);
    }
  else
    gfc_conv_array_parameter (se, arg_expr, true, NULL, NULL, NULL);
  se->expr = convert (gfc_get_int_type (gfc_index_integer_kind), se->expr);

  /* Create a temporary variable for loc return value.  Without this,
     we get an error an ICE in gcc/expr.cc(expand_expr_addr_expr_1).  */
  temp_var = gfc_create_var (gfc_get_int_type (gfc_index_integer_kind), NULL);
  gfc_add_modify (&se->pre, temp_var, se->expr);
  se->expr = temp_var;
}


/* Specialized trim for f_c_string.  */

static void
conv_trim (gfc_se *tse, gfc_se *str)
{
  tree cond, plen, pvar, tlen, ttmp, tvar;

  tlen = gfc_create_var (gfc_charlen_type_node, "tlen");
  plen = gfc_build_addr_expr (NULL_TREE, tlen);

  tvar = gfc_create_var (pchar_type_node, "tstr");
  pvar = gfc_build_addr_expr (ppvoid_type_node, tvar);

  ttmp = build_call_expr_loc (input_location, gfor_fndecl_string_trim, 4,
			      plen, pvar, str->string_length, str->expr);

  gfc_add_expr_to_block (&tse->pre, ttmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			  tlen, build_int_cst (TREE_TYPE (tlen), 0));
  ttmp = gfc_call_free (tvar);
  ttmp = build3_v (COND_EXPR, cond, ttmp, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&tse->post, ttmp);

  tse->expr = tvar;
  tse->string_length = tlen;
}


/* The following routine generates code for the intrinsic functions from
   the ISO_C_BINDING module: C_LOC, C_FUNLOC, C_ASSOCIATED, and
   F_C_STRING.  */

static void
conv_isocbinding_function (gfc_se *se, gfc_expr *expr)
{
  gfc_actual_arglist *arg = expr->value.function.actual;

  if (expr->value.function.isym->id == GFC_ISYM_C_LOC)
    {
      if (arg->expr->rank == 0)
	gfc_conv_expr_reference (se, arg->expr);
      else if (gfc_is_simply_contiguous (arg->expr, false, false))
	gfc_conv_array_parameter (se, arg->expr, true, NULL, NULL, NULL);
      else
	{
	  gfc_conv_expr_descriptor (se, arg->expr);
	  se->expr = gfc_conv_descriptor_data_get (se->expr);
	}

      /* TODO -- the following two lines shouldn't be necessary, but if
	 they're removed, a bug is exposed later in the code path.
	 This workaround was thus introduced, but will have to be
	 removed; please see PR 35150 for details about the issue.  */
      se->expr = convert (pvoid_type_node, se->expr);
      se->expr = gfc_evaluate_now (se->expr, &se->pre);
    }
  else if (expr->value.function.isym->id == GFC_ISYM_C_FUNLOC)
    gfc_conv_expr_reference (se, arg->expr);
  else if (expr->value.function.isym->id == GFC_ISYM_C_ASSOCIATED)
    {
      gfc_se arg1se;
      gfc_se arg2se;

      /* Build the addr_expr for the first argument.  The argument is
	 already an *address* so we don't need to set want_pointer in
	 the gfc_se.  */
      gfc_init_se (&arg1se, NULL);
      gfc_conv_expr (&arg1se, arg->expr);
      gfc_add_block_to_block (&se->pre, &arg1se.pre);
      gfc_add_block_to_block (&se->post, &arg1se.post);

      /* See if we were given two arguments.  */
      if (arg->next->expr == NULL)
	/* Only given one arg so generate a null and do a
	   not-equal comparison against the first arg.  */
	se->expr = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				    arg1se.expr,
				    fold_convert (TREE_TYPE (arg1se.expr),
						  null_pointer_node));
      else
	{
	  tree eq_expr;
	  tree not_null_expr;

	  /* Given two arguments so build the arg2se from second arg.  */
	  gfc_init_se (&arg2se, NULL);
	  gfc_conv_expr (&arg2se, arg->next->expr);
	  gfc_add_block_to_block (&se->pre, &arg2se.pre);
	  gfc_add_block_to_block (&se->post, &arg2se.post);

	  /* Generate test to compare that the two args are equal.  */
	  eq_expr = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
				     arg1se.expr, arg2se.expr);
	  /* Generate test to ensure that the first arg is not null.  */
	  not_null_expr = fold_build2_loc (input_location, NE_EXPR,
					   logical_type_node,
					   arg1se.expr, null_pointer_node);

	  /* Finally, the generated test must check that both arg1 is not
	     NULL and that it is equal to the second arg.  */
	  se->expr = fold_build2_loc (input_location, TRUTH_AND_EXPR,
				      logical_type_node,
				      not_null_expr, eq_expr);
	}
    }
  else if (expr->value.function.isym->id == GFC_ISYM_F_C_STRING)
    {
      /* There are three cases:
	 f_c_string(string)          -> trim(string) // c_null_char
	 f_c_string(string, .false.) -> trim(string) // c_null_char
	 f_c_string(string, .true.)  -> string       // c_null_char  */

      gfc_se lse, rse, tse;
      tree len, tmp, var;
      gfc_expr *string = arg->expr;
      gfc_expr *asis = arg->next->expr;
      gfc_expr *cnc;

      /* Convert string. */
      gfc_init_se (&lse, se);
      gfc_conv_expr (&lse, string);
      gfc_conv_string_parameter (&lse);

      /* Create a string for C_NULL_CHAR and convert it.  */
      cnc = gfc_get_character_expr (gfc_default_character_kind,
				    &string->where, "\0", 1);
      gfc_init_se (&rse, se);
      gfc_conv_expr (&rse, cnc);
      gfc_conv_string_parameter (&rse);
      gfc_free_expr (cnc);

#ifdef cnode
#undef cnode
#endif
#define cnode gfc_charlen_type_node
      if (asis)
	{
	  stmtblock_t block;
	  gfc_se asis_se, vse;
	  tree elen, evar, tlen, tvar;
	  tree else_branch, then_branch;

	  elen = evar = tlen = tvar = NULL_TREE;

	  /* f_c_string(string, .true.) -> string // c_null_char  */

	  gfc_init_block (&block);

	  gfc_add_block_to_block (&block, &lse.pre);
	  gfc_add_block_to_block (&block, &rse.pre);

	  tlen = fold_build2_loc (input_location, PLUS_EXPR, cnode,
				  fold_convert (cnode, lse.string_length),
				  fold_convert (cnode, rse.string_length));

	  gfc_init_se (&vse, se);
	  tvar = gfc_conv_string_tmp (&vse, pchar_type_node, tlen);
	  gfc_add_block_to_block (&block, &vse.pre);

	  tmp = build_call_expr_loc (input_location, gfor_fndecl_concat_string,
				     6, tlen, tvar,
				     lse.string_length, lse.expr,
				     rse.string_length, rse.expr);
	  gfc_add_expr_to_block (&block, tmp);

	  then_branch = gfc_finish_block (&block);

	  /* f_c_string(string, .false.) = trim(string) // c_null_char  */

	  gfc_init_block (&block);

	  gfc_init_se (&tse, se);
	  conv_trim (&tse, &lse);
	  gfc_add_block_to_block (&block, &tse.pre);
	  gfc_add_block_to_block (&block, &rse.pre);

	  elen = fold_build2_loc (input_location, PLUS_EXPR, cnode,
				  fold_convert (cnode, tse.string_length),
				  fold_convert (cnode, rse.string_length));

	  gfc_init_se (&vse, se);
	  evar = gfc_conv_string_tmp (&vse, pchar_type_node, elen);
	  gfc_add_block_to_block (&block, &vse.pre);

	  tmp = build_call_expr_loc (input_location, gfor_fndecl_concat_string,
				     6, elen, evar,
				     tse.string_length, tse.expr,
				     rse.string_length, rse.expr);
	  gfc_add_expr_to_block (&block, tmp);

	  else_branch = gfc_finish_block (&block);

	  gfc_init_se (&asis_se, se);
	  gfc_conv_expr (&asis_se, asis);
	  if (asis->expr_type == EXPR_VARIABLE
	    && asis->symtree->n.sym->attr.dummy
	    && asis->symtree->n.sym->attr.optional)
	    {
	      tree present = gfc_conv_expr_present (asis->symtree->n.sym);
	      asis_se.expr = build3_loc (input_location, COND_EXPR,
					 logical_type_node, present,
					 asis_se.expr,
					 build_int_cst (logical_type_node, 0));
	    }
	  gfc_add_block_to_block (&se->pre, &asis_se.pre);
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 asis_se.expr, then_branch, else_branch);

	  gfc_add_expr_to_block (&se->pre, tmp);

	  var = fold_build3_loc (input_location, COND_EXPR, pchar_type_node,
				 asis_se.expr, tvar, evar);
	  gfc_add_expr_to_block (&se->pre, var);

	  len = fold_build3_loc (input_location, COND_EXPR, cnode,
				 asis_se.expr, tlen, elen);
	  gfc_add_expr_to_block (&se->pre, len);
	}
      else
	{
	  /* f_c_string(string) = trim(string) // c_null_char  */

	  gfc_add_block_to_block (&se->pre, &lse.pre);
	  gfc_add_block_to_block (&se->pre, &rse.pre);

	  gfc_init_se (&tse, se);
	  conv_trim (&tse, &lse);
	  gfc_add_block_to_block (&se->pre, &tse.pre);
	  gfc_add_block_to_block (&se->post, &tse.post);

	  len = fold_build2_loc (input_location, PLUS_EXPR, cnode,
				 fold_convert (cnode, tse.string_length),
				 fold_convert (cnode, rse.string_length));

	  var = gfc_conv_string_tmp (se, pchar_type_node, len);

	  tmp = build_call_expr_loc (input_location, gfor_fndecl_concat_string,
				     6, len, var,
				     tse.string_length, tse.expr,
				     rse.string_length, rse.expr);
	  gfc_add_expr_to_block (&se->pre, tmp);
	}

      se->expr = var;
      se->string_length = len;

#undef cnode
    }
  else
    gcc_unreachable ();
}


/* The following routine generates code for the intrinsic
   subroutines from the ISO_C_BINDING module:
    * C_F_POINTER
    * C_F_PROCPOINTER.  */

static tree
conv_isocbinding_subroutine (gfc_code *code)
{
  gfc_se se;
  gfc_se cptrse;
  gfc_se fptrse;
  gfc_se shapese;
  gfc_ss *shape_ss;
  tree desc, dim, tmp, stride, offset;
  stmtblock_t body, block;
  gfc_loopinfo loop;
  gfc_actual_arglist *arg = code->ext.actual;

  gfc_init_se (&se, NULL);
  gfc_init_se (&cptrse, NULL);
  gfc_conv_expr (&cptrse, arg->expr);
  gfc_add_block_to_block (&se.pre, &cptrse.pre);
  gfc_add_block_to_block (&se.post, &cptrse.post);

  gfc_init_se (&fptrse, NULL);
  if (arg->next->expr->rank == 0)
    {
      fptrse.want_pointer = 1;
      gfc_conv_expr (&fptrse, arg->next->expr);
      gfc_add_block_to_block (&se.pre, &fptrse.pre);
      gfc_add_block_to_block (&se.post, &fptrse.post);
      if (arg->next->expr->symtree->n.sym->attr.proc_pointer
	  && arg->next->expr->symtree->n.sym->attr.dummy)
	fptrse.expr = build_fold_indirect_ref_loc (input_location,
						       fptrse.expr);
      se.expr = fold_build2_loc (input_location, MODIFY_EXPR,
				 TREE_TYPE (fptrse.expr),
				 fptrse.expr,
				 fold_convert (TREE_TYPE (fptrse.expr),
					       cptrse.expr));
      gfc_add_expr_to_block (&se.pre, se.expr);
      gfc_add_block_to_block (&se.pre, &se.post);
      return gfc_finish_block (&se.pre);
    }

  gfc_start_block (&block);

  /* Get the descriptor of the Fortran pointer.  */
  fptrse.descriptor_only = 1;
  gfc_conv_expr_descriptor (&fptrse, arg->next->expr);
  gfc_add_block_to_block (&block, &fptrse.pre);
  desc = fptrse.expr;

  /* Set the span field.  */
  tmp = TYPE_SIZE_UNIT (gfc_get_element_type (TREE_TYPE (desc)));
  tmp = fold_convert (gfc_array_index_type, tmp);
  gfc_conv_descriptor_span_set (&block, desc, tmp);

  /* Set data value, dtype, and offset.  */
  tmp = GFC_TYPE_ARRAY_DATAPTR_TYPE (TREE_TYPE (desc));
  gfc_conv_descriptor_data_set (&block, desc, fold_convert (tmp, cptrse.expr));
  gfc_add_modify (&block, gfc_conv_descriptor_dtype (desc),
		  gfc_get_dtype (TREE_TYPE (desc)));

  /* Start scalarization of the bounds, using the shape argument.  */

  shape_ss = gfc_walk_expr (arg->next->next->expr);
  gcc_assert (shape_ss != gfc_ss_terminator);
  gfc_init_se (&shapese, NULL);

  gfc_init_loopinfo (&loop);
  gfc_add_ss_to_loop (&loop, shape_ss);
  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop, &arg->next->expr->where);
  gfc_mark_ss_chain_used (shape_ss, 1);

  gfc_copy_loopinfo_to_se (&shapese, &loop);
  shapese.ss = shape_ss;

  stride = gfc_create_var (gfc_array_index_type, "stride");
  offset = gfc_create_var (gfc_array_index_type, "offset");
  gfc_add_modify (&block, stride, gfc_index_one_node);
  gfc_add_modify (&block, offset, gfc_index_zero_node);

  /* Loop body.  */
  gfc_start_scalarized_body (&loop, &body);

  dim = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			     loop.loopvar[0], loop.from[0]);

  /* Set bounds and stride.  */
  gfc_conv_descriptor_lbound_set (&body, desc, dim, gfc_index_one_node);
  gfc_conv_descriptor_stride_set (&body, desc, dim, stride);

  gfc_conv_expr (&shapese, arg->next->next->expr);
  gfc_add_block_to_block (&body, &shapese.pre);
  gfc_conv_descriptor_ubound_set (&body, desc, dim, shapese.expr);
  gfc_add_block_to_block (&body, &shapese.post);

  /* Calculate offset.  */
  gfc_add_modify (&body, offset,
		  fold_build2_loc (input_location, PLUS_EXPR,
				   gfc_array_index_type, offset, stride));
  /* Update stride.  */
  gfc_add_modify (&body, stride,
		  fold_build2_loc (input_location, MULT_EXPR,
				   gfc_array_index_type, stride,
				   fold_convert (gfc_array_index_type,
						 shapese.expr)));
  /* Finish scalarization loop.  */
  gfc_trans_scalarizing_loops (&loop, &body);
  gfc_add_block_to_block (&block, &loop.pre);
  gfc_add_block_to_block (&block, &loop.post);
  gfc_add_block_to_block (&block, &fptrse.post);
  gfc_cleanup_loop (&loop);

  gfc_add_modify (&block, offset,
		  fold_build1_loc (input_location, NEGATE_EXPR,
				   gfc_array_index_type, offset));
  gfc_conv_descriptor_offset_set (&block, desc, offset);

  gfc_add_expr_to_block (&se.pre, gfc_finish_block (&block));
  gfc_add_block_to_block (&se.pre, &se.post);
  return gfc_finish_block (&se.pre);
}


/* Save and restore floating-point state.  */

tree
gfc_save_fp_state (stmtblock_t *block)
{
  tree type, fpstate, tmp;

  type = build_array_type (char_type_node,
	                   build_range_type (size_type_node, size_zero_node,
					     size_int (GFC_FPE_STATE_BUFFER_SIZE)));
  fpstate = gfc_create_var (type, "fpstate");
  fpstate = gfc_build_addr_expr (pvoid_type_node, fpstate);

  tmp = build_call_expr_loc (input_location, gfor_fndecl_ieee_procedure_entry,
			     1, fpstate);
  gfc_add_expr_to_block (block, tmp);

  return fpstate;
}


void
gfc_restore_fp_state (stmtblock_t *block, tree fpstate)
{
  tree tmp;

  tmp = build_call_expr_loc (input_location, gfor_fndecl_ieee_procedure_exit,
			     1, fpstate);
  gfc_add_expr_to_block (block, tmp);
}


/* Generate code for arguments of IEEE functions.  */

static void
conv_ieee_function_args (gfc_se *se, gfc_expr *expr, tree *argarray,
			 int nargs)
{
  gfc_actual_arglist *actual;
  gfc_expr *e;
  gfc_se argse;
  int arg;

  actual = expr->value.function.actual;
  for (arg = 0; arg < nargs; arg++, actual = actual->next)
    {
      gcc_assert (actual);
      e = actual->expr;

      gfc_init_se (&argse, se);
      gfc_conv_expr_val (&argse, e);

      gfc_add_block_to_block (&se->pre, &argse.pre);
      gfc_add_block_to_block (&se->post, &argse.post);
      argarray[arg] = argse.expr;
    }
}


/* Generate code for intrinsics IEEE_IS_NAN, IEEE_IS_FINITE
   and IEEE_UNORDERED, which translate directly to GCC type-generic
   built-ins.  */

static void
conv_intrinsic_ieee_builtin (gfc_se * se, gfc_expr * expr,
			     enum built_in_function code, int nargs)
{
  tree args[2];
  gcc_assert ((unsigned) nargs <= ARRAY_SIZE (args));

  conv_ieee_function_args (se, expr, args, nargs);
  se->expr = build_call_expr_loc_array (input_location,
					builtin_decl_explicit (code),
					nargs, args);
  STRIP_TYPE_NOPS (se->expr);
  se->expr = fold_convert (gfc_typenode_for_spec (&expr->ts), se->expr);
}


/* Generate code for intrinsics IEEE_SIGNBIT.  */

static void
conv_intrinsic_ieee_signbit (gfc_se * se, gfc_expr * expr)
{
  tree arg, signbit;

  conv_ieee_function_args (se, expr, &arg, 1);
  signbit = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_SIGNBIT),
				 1, arg);
  signbit = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			     signbit, integer_zero_node);
  se->expr = fold_convert (gfc_typenode_for_spec (&expr->ts), signbit);
}


/* Generate code for IEEE_IS_NORMAL intrinsic:
     IEEE_IS_NORMAL(x) --> (__builtin_isnormal(x) || x == 0)  */

static void
conv_intrinsic_ieee_is_normal (gfc_se * se, gfc_expr * expr)
{
  tree arg, isnormal, iszero;

  /* Convert arg, evaluate it only once.  */
  conv_ieee_function_args (se, expr, &arg, 1);
  arg = gfc_evaluate_now (arg, &se->pre);

  isnormal = build_call_expr_loc (input_location,
				  builtin_decl_explicit (BUILT_IN_ISNORMAL),
				  1, arg);
  iszero = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, arg,
			    build_real_from_int_cst (TREE_TYPE (arg),
						     integer_zero_node));
  se->expr = fold_build2_loc (input_location, TRUTH_OR_EXPR,
			      logical_type_node, isnormal, iszero);
  se->expr = fold_convert (gfc_typenode_for_spec (&expr->ts), se->expr);
}


/* Generate code for IEEE_IS_NEGATIVE intrinsic:
     IEEE_IS_NEGATIVE(x) --> (__builtin_signbit(x) && !__builtin_isnan(x))  */

static void
conv_intrinsic_ieee_is_negative (gfc_se * se, gfc_expr * expr)
{
  tree arg, signbit, isnan;

  /* Convert arg, evaluate it only once.  */
  conv_ieee_function_args (se, expr, &arg, 1);
  arg = gfc_evaluate_now (arg, &se->pre);

  isnan = build_call_expr_loc (input_location,
			       builtin_decl_explicit (BUILT_IN_ISNAN),
			       1, arg);
  STRIP_TYPE_NOPS (isnan);

  signbit = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_SIGNBIT),
				 1, arg);
  signbit = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			     signbit, integer_zero_node);

  se->expr = fold_build2_loc (input_location, TRUTH_AND_EXPR,
			      logical_type_node, signbit,
			      fold_build1_loc (input_location, TRUTH_NOT_EXPR,
					       TREE_TYPE(isnan), isnan));

  se->expr = fold_convert (gfc_typenode_for_spec (&expr->ts), se->expr);
}


/* Generate code for IEEE_LOGB and IEEE_RINT.  */

static void
conv_intrinsic_ieee_logb_rint (gfc_se * se, gfc_expr * expr,
			       enum built_in_function code)
{
  tree arg, decl, call, fpstate;
  int argprec;

  conv_ieee_function_args (se, expr, &arg, 1);
  argprec = TYPE_PRECISION (TREE_TYPE (arg));
  decl = builtin_decl_for_precision (code, argprec);

  /* Save floating-point state.  */
  fpstate = gfc_save_fp_state (&se->pre);

  /* Make the function call.  */
  call = build_call_expr_loc (input_location, decl, 1, arg);
  se->expr = fold_convert (gfc_typenode_for_spec (&expr->ts), call);

  /* Restore floating-point state.  */
  gfc_restore_fp_state (&se->post, fpstate);
}


/* Generate code for IEEE_REM.  */

static void
conv_intrinsic_ieee_rem (gfc_se * se, gfc_expr * expr)
{
  tree args[2], decl, call, fpstate;
  int argprec;

  conv_ieee_function_args (se, expr, args, 2);

  /* If arguments have unequal size, convert them to the larger.  */
  if (TYPE_PRECISION (TREE_TYPE (args[0]))
      > TYPE_PRECISION (TREE_TYPE (args[1])))
    args[1] = fold_convert (TREE_TYPE (args[0]), args[1]);
  else if (TYPE_PRECISION (TREE_TYPE (args[1]))
	   > TYPE_PRECISION (TREE_TYPE (args[0])))
    args[0] = fold_convert (TREE_TYPE (args[1]), args[0]);

  argprec = TYPE_PRECISION (TREE_TYPE (args[0]));
  decl = builtin_decl_for_precision (BUILT_IN_REMAINDER, argprec);

  /* Save floating-point state.  */
  fpstate = gfc_save_fp_state (&se->pre);

  /* Make the function call.  */
  call = build_call_expr_loc_array (input_location, decl, 2, args);
  se->expr = fold_convert (TREE_TYPE (args[0]), call);

  /* Restore floating-point state.  */
  gfc_restore_fp_state (&se->post, fpstate);
}


/* Generate code for IEEE_NEXT_AFTER.  */

static void
conv_intrinsic_ieee_next_after (gfc_se * se, gfc_expr * expr)
{
  tree args[2], decl, call, fpstate;
  int argprec;

  conv_ieee_function_args (se, expr, args, 2);

  /* Result has the characteristics of first argument.  */
  args[1] = fold_convert (TREE_TYPE (args[0]), args[1]);
  argprec = TYPE_PRECISION (TREE_TYPE (args[0]));
  decl = builtin_decl_for_precision (BUILT_IN_NEXTAFTER, argprec);

  /* Save floating-point state.  */
  fpstate = gfc_save_fp_state (&se->pre);

  /* Make the function call.  */
  call = build_call_expr_loc_array (input_location, decl, 2, args);
  se->expr = fold_convert (TREE_TYPE (args[0]), call);

  /* Restore floating-point state.  */
  gfc_restore_fp_state (&se->post, fpstate);
}


/* Generate code for IEEE_SCALB.  */

static void
conv_intrinsic_ieee_scalb (gfc_se * se, gfc_expr * expr)
{
  tree args[2], decl, call, huge, type;
  int argprec, n;

  conv_ieee_function_args (se, expr, args, 2);

  /* Result has the characteristics of first argument.  */
  argprec = TYPE_PRECISION (TREE_TYPE (args[0]));
  decl = builtin_decl_for_precision (BUILT_IN_SCALBN, argprec);

  if (TYPE_PRECISION (TREE_TYPE (args[1])) > TYPE_PRECISION (integer_type_node))
    {
      /* We need to fold the integer into the range of a C int.  */
      args[1] = gfc_evaluate_now (args[1], &se->pre);
      type = TREE_TYPE (args[1]);

      n = gfc_validate_kind (BT_INTEGER, gfc_c_int_kind, false);
      huge = gfc_conv_mpz_to_tree (gfc_integer_kinds[n].huge,
				   gfc_c_int_kind);
      huge = fold_convert (type, huge);
      args[1] = fold_build2_loc (input_location, MIN_EXPR, type, args[1],
				 huge);
      args[1] = fold_build2_loc (input_location, MAX_EXPR, type, args[1],
				 fold_build1_loc (input_location, NEGATE_EXPR,
						  type, huge));
    }

  args[1] = fold_convert (integer_type_node, args[1]);

  /* Make the function call.  */
  call = build_call_expr_loc_array (input_location, decl, 2, args);
  se->expr = fold_convert (TREE_TYPE (args[0]), call);
}


/* Generate code for IEEE_COPY_SIGN.  */

static void
conv_intrinsic_ieee_copy_sign (gfc_se * se, gfc_expr * expr)
{
  tree args[2], decl, sign;
  int argprec;

  conv_ieee_function_args (se, expr, args, 2);

  /* Get the sign of the second argument.  */
  sign = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_SIGNBIT),
			      1, args[1]);
  sign = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			  sign, integer_zero_node);

  /* Create a value of one, with the right sign.  */
  sign = fold_build3_loc (input_location, COND_EXPR, integer_type_node,
			  sign,
			  fold_build1_loc (input_location, NEGATE_EXPR,
					   integer_type_node,
					   integer_one_node),
			  integer_one_node);
  args[1] = fold_convert (TREE_TYPE (args[0]), sign);

  argprec = TYPE_PRECISION (TREE_TYPE (args[0]));
  decl = builtin_decl_for_precision (BUILT_IN_COPYSIGN, argprec);

  se->expr = build_call_expr_loc_array (input_location, decl, 2, args);
}


/* Generate code for IEEE_CLASS.  */

static void
conv_intrinsic_ieee_class (gfc_se *se, gfc_expr *expr)
{
  tree arg, c, t1, t2, t3, t4;

  /* Convert arg, evaluate it only once.  */
  conv_ieee_function_args (se, expr, &arg, 1);
  arg = gfc_evaluate_now (arg, &se->pre);

  c = build_call_expr_loc (input_location,
			   builtin_decl_explicit (BUILT_IN_FPCLASSIFY), 6,
			   build_int_cst (integer_type_node, IEEE_QUIET_NAN),
			   build_int_cst (integer_type_node,
					  IEEE_POSITIVE_INF),
			   build_int_cst (integer_type_node,
					  IEEE_POSITIVE_NORMAL),
			   build_int_cst (integer_type_node,
					  IEEE_POSITIVE_DENORMAL),
			   build_int_cst (integer_type_node,
					  IEEE_POSITIVE_ZERO),
			   arg);
  c = gfc_evaluate_now (c, &se->pre);
  t1 = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			c, build_int_cst (integer_type_node,
					  IEEE_QUIET_NAN));
  t2 = build_call_expr_loc (input_location,
			    builtin_decl_explicit (BUILT_IN_ISSIGNALING), 1,
			    arg);
  t2 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			t2, build_zero_cst (TREE_TYPE (t2)));
  t1 = fold_build2_loc (input_location, TRUTH_AND_EXPR,
			logical_type_node, t1, t2);
  t3 = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
			c, build_int_cst (integer_type_node,
					  IEEE_POSITIVE_ZERO));
  t4 = build_call_expr_loc (input_location,
			    builtin_decl_explicit (BUILT_IN_SIGNBIT), 1,
			    arg);
  t4 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			t4, build_zero_cst (TREE_TYPE (t4)));
  t3 = fold_build2_loc (input_location, TRUTH_AND_EXPR,
			logical_type_node, t3, t4);
  int s = IEEE_NEGATIVE_ZERO + IEEE_POSITIVE_ZERO;
  gcc_assert (IEEE_NEGATIVE_INF == s - IEEE_POSITIVE_INF);
  gcc_assert (IEEE_NEGATIVE_NORMAL == s - IEEE_POSITIVE_NORMAL);
  gcc_assert (IEEE_NEGATIVE_DENORMAL == s - IEEE_POSITIVE_DENORMAL);
  gcc_assert (IEEE_NEGATIVE_SUBNORMAL == s - IEEE_POSITIVE_SUBNORMAL);
  gcc_assert (IEEE_NEGATIVE_ZERO == s - IEEE_POSITIVE_ZERO);
  t4 = fold_build2_loc (input_location, MINUS_EXPR, TREE_TYPE (c),
			build_int_cst (TREE_TYPE (c), s), c);
  t3 = fold_build3_loc (input_location, COND_EXPR, TREE_TYPE (c),
			t3, t4, c);
  t1 = fold_build3_loc (input_location, COND_EXPR, TREE_TYPE (c), t1,
			build_int_cst (TREE_TYPE (c), IEEE_SIGNALING_NAN),
			t3);
  tree type = gfc_typenode_for_spec (&expr->ts);
  /* Perform a quick sanity check that the return type is
     IEEE_CLASS_TYPE derived type defined in
     libgfortran/ieee/ieee_arithmetic.F90
     Primarily check that it is a derived type with a single
     member in it.  */
  gcc_assert (TREE_CODE (type) == RECORD_TYPE);
  tree field = NULL_TREE;
  for (tree f = TYPE_FIELDS (type); f != NULL_TREE; f = DECL_CHAIN (f))
    if (TREE_CODE (f) == FIELD_DECL)
      {
	gcc_assert (field == NULL_TREE);
	field = f;
      }
  gcc_assert (field);
  t1 = fold_convert (TREE_TYPE (field), t1);
  se->expr = build_constructor_single (type, field, t1);
}


/* Generate code for IEEE_VALUE.  */

static void
conv_intrinsic_ieee_value (gfc_se *se, gfc_expr *expr)
{
  tree args[2], arg, ret, tmp;
  stmtblock_t body;

  /* Convert args, evaluate the second one only once.  */
  conv_ieee_function_args (se, expr, args, 2);
  arg = gfc_evaluate_now (args[1], &se->pre);

  tree type = TREE_TYPE (arg);
  /* Perform a quick sanity check that the second argument's type is
     IEEE_CLASS_TYPE derived type defined in
     libgfortran/ieee/ieee_arithmetic.F90
     Primarily check that it is a derived type with a single
     member in it.  */
  gcc_assert (TREE_CODE (type) == RECORD_TYPE);
  tree field = NULL_TREE;
  for (tree f = TYPE_FIELDS (type); f != NULL_TREE; f = DECL_CHAIN (f))
    if (TREE_CODE (f) == FIELD_DECL)
      {
	gcc_assert (field == NULL_TREE);
	field = f;
      }
  gcc_assert (field);
  arg = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			 arg, field, NULL_TREE);
  arg = gfc_evaluate_now (arg, &se->pre);

  type = gfc_typenode_for_spec (&expr->ts);
  gcc_assert (SCALAR_FLOAT_TYPE_P (type));
  ret = gfc_create_var (type, NULL);

  gfc_init_block (&body);

  tree end_label = gfc_build_label_decl (NULL_TREE);
  for (int c = IEEE_SIGNALING_NAN; c <= IEEE_POSITIVE_INF; ++c)
    {
      tree label = gfc_build_label_decl (NULL_TREE);
      tree low = build_int_cst (TREE_TYPE (arg), c);
      tmp = build_case_label (low, low, label);
      gfc_add_expr_to_block (&body, tmp);

      REAL_VALUE_TYPE real;
      int k;
      switch (c)
	{
	case IEEE_SIGNALING_NAN:
	  real_nan (&real, "", 0, TYPE_MODE (type));
	  break;
	case IEEE_QUIET_NAN:
	  real_nan (&real, "", 1, TYPE_MODE (type));
	  break;
	case IEEE_NEGATIVE_INF:
	  real_inf (&real);
	  real = real_value_negate (&real);
	  break;
	case IEEE_NEGATIVE_NORMAL:
	  real_from_integer (&real, TYPE_MODE (type), -42, SIGNED);
	  break;
	case IEEE_NEGATIVE_DENORMAL:
	  k = gfc_validate_kind (BT_REAL, expr->ts.kind, false);
	  real_from_mpfr (&real, gfc_real_kinds[k].tiny,
			  type, GFC_RND_MODE);
	  real_arithmetic (&real, RDIV_EXPR, &real, &dconst2);
	  real = real_value_negate (&real);
	  break;
	case IEEE_NEGATIVE_ZERO:
	  real_from_integer (&real, TYPE_MODE (type), 0, SIGNED);
	  real = real_value_negate (&real);
	  break;
	case IEEE_POSITIVE_ZERO:
	  /* Make this also the default: label.  The other possibility
	     would be to add a separate default: label followed by
	     __builtin_unreachable ().  */
	  label = gfc_build_label_decl (NULL_TREE);
	  tmp = build_case_label (NULL_TREE, NULL_TREE, label);
	  gfc_add_expr_to_block (&body, tmp);
	  real_from_integer (&real, TYPE_MODE (type), 0, SIGNED);
	  break;
	case IEEE_POSITIVE_DENORMAL:
	  k = gfc_validate_kind (BT_REAL, expr->ts.kind, false);
	  real_from_mpfr (&real, gfc_real_kinds[k].tiny,
			  type, GFC_RND_MODE);
	  real_arithmetic (&real, RDIV_EXPR, &real, &dconst2);
	  break;
	case IEEE_POSITIVE_NORMAL:
	  real_from_integer (&real, TYPE_MODE (type), 42, SIGNED);
	  break;
	case IEEE_POSITIVE_INF:
	  real_inf (&real);
	  break;
	default:
	  gcc_unreachable ();
	}

      tree val = build_real (type, real);
      gfc_add_modify (&body, ret, val);

      tmp = build1_v (GOTO_EXPR, end_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  tmp = gfc_finish_block (&body);
  tmp = fold_build2_loc (input_location, SWITCH_EXPR, NULL_TREE, arg, tmp);
  gfc_add_expr_to_block (&se->pre, tmp);

  tmp = build1_v (LABEL_EXPR, end_label);
  gfc_add_expr_to_block (&se->pre, tmp);

  se->expr = ret;
}


/* Generate code for IEEE_FMA.  */

static void
conv_intrinsic_ieee_fma (gfc_se * se, gfc_expr * expr)
{
  tree args[3], decl, call;
  int argprec;

  conv_ieee_function_args (se, expr, args, 3);

  /* All three arguments should have the same type.  */
  gcc_assert (TYPE_PRECISION (TREE_TYPE (args[0])) == TYPE_PRECISION (TREE_TYPE (args[1])));
  gcc_assert (TYPE_PRECISION (TREE_TYPE (args[0])) == TYPE_PRECISION (TREE_TYPE (args[2])));

  /* Call the type-generic FMA built-in.  */
  argprec = TYPE_PRECISION (TREE_TYPE (args[0]));
  decl = builtin_decl_for_precision (BUILT_IN_FMA, argprec);
  call = build_call_expr_loc_array (input_location, decl, 3, args);

  /* Convert to the final type.  */
  se->expr = fold_convert (TREE_TYPE (args[0]), call);
}


/* Generate code for IEEE_{MIN,MAX}_NUM{,_MAG}.  */

static void
conv_intrinsic_ieee_minmax (gfc_se * se, gfc_expr * expr, int max,
			    const char *name)
{
  tree args[2], func;
  built_in_function fn;

  conv_ieee_function_args (se, expr, args, 2);
  gcc_assert (TYPE_PRECISION (TREE_TYPE (args[0])) == TYPE_PRECISION (TREE_TYPE (args[1])));
  args[0] = gfc_evaluate_now (args[0], &se->pre);
  args[1] = gfc_evaluate_now (args[1], &se->pre);

  if (startswith (name, "mag"))
    {
      /* IEEE_MIN_NUM_MAG and IEEE_MAX_NUM_MAG translate to C functions
	 fminmag() and fmaxmag(), which do not exist as built-ins.

	 Following glibc, we emit this:

	   fminmag (x, y) {
	     ax = ABS (x);
	     ay = ABS (y);
	     if (isless (ax, ay))
	       return x;
	     else if (isgreater (ax, ay))
	       return y;
	     else if (ax == ay)
	       return x < y ? x : y;
	     else if (issignaling (x) || issignaling (y))
	       return x + y;
	     else
	       return isnan (y) ? x : y;
	   }

	   fmaxmag (x, y) {
	     ax = ABS (x);
	     ay = ABS (y);
	     if (isgreater (ax, ay))
	       return x;
	     else if (isless (ax, ay))
	       return y;
	     else if (ax == ay)
	       return x > y ? x : y;
	     else if (issignaling (x) || issignaling (y))
	       return x + y;
	     else
	       return isnan (y) ? x : y;
	   }

	 */

      tree abs0, abs1, sig0, sig1;
      tree cond1, cond2, cond3, cond4, cond5;
      tree res;
      tree type = TREE_TYPE (args[0]);

      func = gfc_builtin_decl_for_float_kind (BUILT_IN_FABS, expr->ts.kind);
      abs0 = build_call_expr_loc (input_location, func, 1, args[0]);
      abs1 = build_call_expr_loc (input_location, func, 1, args[1]);
      abs0 = gfc_evaluate_now (abs0, &se->pre);
      abs1 = gfc_evaluate_now (abs1, &se->pre);

      cond5 = build_call_expr_loc (input_location,
				   builtin_decl_explicit (BUILT_IN_ISNAN),
				   1, args[1]);
      res = fold_build3_loc (input_location, COND_EXPR, type, cond5,
			     args[0], args[1]);

      sig0 = build_call_expr_loc (input_location,
				  builtin_decl_explicit (BUILT_IN_ISSIGNALING),
				  1, args[0]);
      sig1 = build_call_expr_loc (input_location,
				  builtin_decl_explicit (BUILT_IN_ISSIGNALING),
				  1, args[1]);
      cond4 = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
			       logical_type_node, sig0, sig1);
      res = fold_build3_loc (input_location, COND_EXPR, type, cond4,
			     fold_build2_loc (input_location, PLUS_EXPR,
					      type, args[0], args[1]),
			     res);

      cond3 = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			       abs0, abs1);
      res = fold_build3_loc (input_location, COND_EXPR, type, cond3,
			     fold_build2_loc (input_location,
					      max ? MAX_EXPR : MIN_EXPR,
					      type, args[0], args[1]),
			     res);

      func = builtin_decl_explicit (max ? BUILT_IN_ISLESS : BUILT_IN_ISGREATER);
      cond2 = build_call_expr_loc (input_location, func, 2, abs0, abs1);
      res = fold_build3_loc (input_location, COND_EXPR, type, cond2,
			     args[1], res);

      func = builtin_decl_explicit (max ? BUILT_IN_ISGREATER : BUILT_IN_ISLESS);
      cond1 = build_call_expr_loc (input_location, func, 2, abs0, abs1);
      res = fold_build3_loc (input_location, COND_EXPR, type, cond1,
			     args[0], res);

      se->expr = res;
    }
  else
    {
      /* IEEE_MIN_NUM and IEEE_MAX_NUM translate to fmin() and fmax().  */
      fn = max ? BUILT_IN_FMAX : BUILT_IN_FMIN;
      func = gfc_builtin_decl_for_float_kind (fn, expr->ts.kind);
      se->expr = build_call_expr_loc_array (input_location, func, 2, args);
    }
}


/* Generate code for comparison functions IEEE_QUIET_* and
   IEEE_SIGNALING_*.  */

static void
conv_intrinsic_ieee_comparison (gfc_se * se, gfc_expr * expr, int signaling,
				const char *name)
{
  tree args[2];
  tree arg1, arg2, res;

  /* Evaluate arguments only once.  */
  conv_ieee_function_args (se, expr, args, 2);
  arg1 = gfc_evaluate_now (args[0], &se->pre);
  arg2 = gfc_evaluate_now (args[1], &se->pre);

  if (startswith (name, "eq"))
    {
      if (signaling)
	res = build_call_expr_loc (input_location,
				   builtin_decl_explicit (BUILT_IN_ISEQSIG),
				   2, arg1, arg2);
      else
	res = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			       arg1, arg2);
    }
  else if (startswith (name, "ne"))
    {
      if (signaling)
	{
	  res = build_call_expr_loc (input_location,
				     builtin_decl_explicit (BUILT_IN_ISEQSIG),
				     2, arg1, arg2);
	  res = fold_build1_loc (input_location, TRUTH_NOT_EXPR,
				 logical_type_node, res);
	}
      else
	res = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			       arg1, arg2);
    }
  else if (startswith (name, "ge"))
    {
      if (signaling)
	res = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
			       arg1, arg2);
      else
	res = build_call_expr_loc (input_location,
				   builtin_decl_explicit (BUILT_IN_ISGREATEREQUAL),
				   2, arg1, arg2);
    }
  else if (startswith (name, "gt"))
    {
      if (signaling)
	res = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			       arg1, arg2);
      else
	res = build_call_expr_loc (input_location,
				   builtin_decl_explicit (BUILT_IN_ISGREATER),
				   2, arg1, arg2);
    }
  else if (startswith (name, "le"))
    {
      if (signaling)
	res = fold_build2_loc (input_location, LE_EXPR, logical_type_node,
			       arg1, arg2);
      else
	res = build_call_expr_loc (input_location,
				   builtin_decl_explicit (BUILT_IN_ISLESSEQUAL),
				   2, arg1, arg2);
    }
  else if (startswith (name, "lt"))
    {
      if (signaling)
	res = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
			       arg1, arg2);
      else
	res = build_call_expr_loc (input_location,
				   builtin_decl_explicit (BUILT_IN_ISLESS),
				   2, arg1, arg2);
    }
  else
    gcc_unreachable ();

  se->expr = fold_convert (gfc_typenode_for_spec (&expr->ts), res);
}


/* Generate code for an intrinsic function from the IEEE_ARITHMETIC
   module.  */

bool
gfc_conv_ieee_arithmetic_function (gfc_se * se, gfc_expr * expr)
{
  const char *name = expr->value.function.name;

  if (startswith (name, "_gfortran_ieee_is_nan"))
    conv_intrinsic_ieee_builtin (se, expr, BUILT_IN_ISNAN, 1);
  else if (startswith (name, "_gfortran_ieee_is_finite"))
    conv_intrinsic_ieee_builtin (se, expr, BUILT_IN_ISFINITE, 1);
  else if (startswith (name, "_gfortran_ieee_unordered"))
    conv_intrinsic_ieee_builtin (se, expr, BUILT_IN_ISUNORDERED, 2);
  else if (startswith (name, "_gfortran_ieee_signbit"))
    conv_intrinsic_ieee_signbit (se, expr);
  else if (startswith (name, "_gfortran_ieee_is_normal"))
    conv_intrinsic_ieee_is_normal (se, expr);
  else if (startswith (name, "_gfortran_ieee_is_negative"))
    conv_intrinsic_ieee_is_negative (se, expr);
  else if (startswith (name, "_gfortran_ieee_copy_sign"))
    conv_intrinsic_ieee_copy_sign (se, expr);
  else if (startswith (name, "_gfortran_ieee_scalb"))
    conv_intrinsic_ieee_scalb (se, expr);
  else if (startswith (name, "_gfortran_ieee_next_after"))
    conv_intrinsic_ieee_next_after (se, expr);
  else if (startswith (name, "_gfortran_ieee_rem"))
    conv_intrinsic_ieee_rem (se, expr);
  else if (startswith (name, "_gfortran_ieee_logb"))
    conv_intrinsic_ieee_logb_rint (se, expr, BUILT_IN_LOGB);
  else if (startswith (name, "_gfortran_ieee_rint"))
    conv_intrinsic_ieee_logb_rint (se, expr, BUILT_IN_RINT);
  else if (startswith (name, "ieee_class_") && ISDIGIT (name[11]))
    conv_intrinsic_ieee_class (se, expr);
  else if (startswith (name, "ieee_value_") && ISDIGIT (name[11]))
    conv_intrinsic_ieee_value (se, expr);
  else if (startswith (name, "_gfortran_ieee_fma"))
    conv_intrinsic_ieee_fma (se, expr);
  else if (startswith (name, "_gfortran_ieee_min_num_"))
    conv_intrinsic_ieee_minmax (se, expr, 0, name + 23);
  else if (startswith (name, "_gfortran_ieee_max_num_"))
    conv_intrinsic_ieee_minmax (se, expr, 1, name + 23);
  else if (startswith (name, "_gfortran_ieee_quiet_"))
    conv_intrinsic_ieee_comparison (se, expr, 0, name + 21);
  else if (startswith (name, "_gfortran_ieee_signaling_"))
    conv_intrinsic_ieee_comparison (se, expr, 1, name + 25);
  else
    /* It is not among the functions we translate directly.  We return
       false, so a library function call is emitted.  */
    return false;

  return true;
}


/* Generate a direct call to malloc() for the MALLOC intrinsic.  */

static void
gfc_conv_intrinsic_malloc (gfc_se * se, gfc_expr * expr)
{
  tree arg, res, restype;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  arg = fold_convert (size_type_node, arg);
  res = build_call_expr_loc (input_location,
			     builtin_decl_explicit (BUILT_IN_MALLOC), 1, arg);
  restype = gfc_typenode_for_spec (&expr->ts);
  se->expr = fold_convert (restype, res);
}


/* Generate code for an intrinsic function.  Some map directly to library
   calls, others get special handling.  In some cases the name of the function
   used depends on the type specifiers.  */

void
gfc_conv_intrinsic_function (gfc_se * se, gfc_expr * expr)
{
  const char *name;
  int lib, kind;
  tree fndecl;

  name = &expr->value.function.name[2];

  if (expr->rank > 0)
    {
      lib = gfc_is_intrinsic_libcall (expr);
      if (lib != 0)
	{
	  if (lib == 1)
	    se->ignore_optional = 1;

	  switch (expr->value.function.isym->id)
	    {
	    case GFC_ISYM_EOSHIFT:
	    case GFC_ISYM_PACK:
	    case GFC_ISYM_RESHAPE:
	      /* For all of those the first argument specifies the type and the
		 third is optional.  */
	      conv_generic_with_optional_char_arg (se, expr, 1, 3);
	      break;

	    case GFC_ISYM_FINDLOC:
	      gfc_conv_intrinsic_findloc (se, expr);
	      break;

	    case GFC_ISYM_MINLOC:
	      gfc_conv_intrinsic_minmaxloc (se, expr, LT_EXPR);
	      break;

	    case GFC_ISYM_MAXLOC:
	      gfc_conv_intrinsic_minmaxloc (se, expr, GT_EXPR);
	      break;

	    default:
	      gfc_conv_intrinsic_funcall (se, expr);
	      break;
	    }

	  return;
	}
    }

  switch (expr->value.function.isym->id)
    {
    case GFC_ISYM_NONE:
      gcc_unreachable ();

    case GFC_ISYM_REPEAT:
      gfc_conv_intrinsic_repeat (se, expr);
      break;

    case GFC_ISYM_TRIM:
      gfc_conv_intrinsic_trim (se, expr);
      break;

    case GFC_ISYM_SC_KIND:
      gfc_conv_intrinsic_sc_kind (se, expr);
      break;

    case GFC_ISYM_SI_KIND:
      gfc_conv_intrinsic_si_kind (se, expr);
      break;

    case GFC_ISYM_SL_KIND:
      gfc_conv_intrinsic_sl_kind (se, expr);
      break;

    case GFC_ISYM_SR_KIND:
      gfc_conv_intrinsic_sr_kind (se, expr);
      break;

    case GFC_ISYM_EXPONENT:
      gfc_conv_intrinsic_exponent (se, expr);
      break;

    case GFC_ISYM_SCAN:
      kind = expr->value.function.actual->expr->ts.kind;
      if (kind == 1)
       fndecl = gfor_fndecl_string_scan;
      else if (kind == 4)
       fndecl = gfor_fndecl_string_scan_char4;
      else
       gcc_unreachable ();

      gfc_conv_intrinsic_index_scan_verify (se, expr, fndecl);
      break;

    case GFC_ISYM_VERIFY:
      kind = expr->value.function.actual->expr->ts.kind;
      if (kind == 1)
       fndecl = gfor_fndecl_string_verify;
      else if (kind == 4)
       fndecl = gfor_fndecl_string_verify_char4;
      else
       gcc_unreachable ();

      gfc_conv_intrinsic_index_scan_verify (se, expr, fndecl);
      break;

    case GFC_ISYM_ALLOCATED:
      gfc_conv_allocated (se, expr);
      break;

    case GFC_ISYM_ASSOCIATED:
      gfc_conv_associated(se, expr);
      break;

    case GFC_ISYM_SAME_TYPE_AS:
      gfc_conv_same_type_as (se, expr);
      break;

    case GFC_ISYM_ABS:
      gfc_conv_intrinsic_abs (se, expr);
      break;

    case GFC_ISYM_ADJUSTL:
      if (expr->ts.kind == 1)
       fndecl = gfor_fndecl_adjustl;
      else if (expr->ts.kind == 4)
       fndecl = gfor_fndecl_adjustl_char4;
      else
       gcc_unreachable ();

      gfc_conv_intrinsic_adjust (se, expr, fndecl);
      break;

    case GFC_ISYM_ADJUSTR:
      if (expr->ts.kind == 1)
       fndecl = gfor_fndecl_adjustr;
      else if (expr->ts.kind == 4)
       fndecl = gfor_fndecl_adjustr_char4;
      else
       gcc_unreachable ();

      gfc_conv_intrinsic_adjust (se, expr, fndecl);
      break;

    case GFC_ISYM_AIMAG:
      gfc_conv_intrinsic_imagpart (se, expr);
      break;

    case GFC_ISYM_AINT:
      gfc_conv_intrinsic_aint (se, expr, RND_TRUNC);
      break;

    case GFC_ISYM_ALL:
      gfc_conv_intrinsic_anyall (se, expr, EQ_EXPR);
      break;

    case GFC_ISYM_ANINT:
      gfc_conv_intrinsic_aint (se, expr, RND_ROUND);
      break;

    case GFC_ISYM_AND:
      gfc_conv_intrinsic_bitop (se, expr, BIT_AND_EXPR);
      break;

    case GFC_ISYM_ANY:
      gfc_conv_intrinsic_anyall (se, expr, NE_EXPR);
      break;

    case GFC_ISYM_ACOSD:
    case GFC_ISYM_ASIND:
    case GFC_ISYM_ATAND:
      gfc_conv_intrinsic_atrigd (se, expr, expr->value.function.isym->id);
      break;

    case GFC_ISYM_COTAN:
      gfc_conv_intrinsic_cotan (se, expr);
      break;

    case GFC_ISYM_COTAND:
      gfc_conv_intrinsic_cotand (se, expr);
      break;

    case GFC_ISYM_ATAN2D:
      gfc_conv_intrinsic_atan2d (se, expr);
      break;

    case GFC_ISYM_BTEST:
      gfc_conv_intrinsic_btest (se, expr);
      break;

    case GFC_ISYM_BGE:
      gfc_conv_intrinsic_bitcomp (se, expr, GE_EXPR);
      break;

    case GFC_ISYM_BGT:
      gfc_conv_intrinsic_bitcomp (se, expr, GT_EXPR);
      break;

    case GFC_ISYM_BLE:
      gfc_conv_intrinsic_bitcomp (se, expr, LE_EXPR);
      break;

    case GFC_ISYM_BLT:
      gfc_conv_intrinsic_bitcomp (se, expr, LT_EXPR);
      break;

    case GFC_ISYM_C_ASSOCIATED:
    case GFC_ISYM_C_FUNLOC:
    case GFC_ISYM_C_LOC:
    case GFC_ISYM_F_C_STRING:
      conv_isocbinding_function (se, expr);
      break;

    case GFC_ISYM_ACHAR:
    case GFC_ISYM_CHAR:
      gfc_conv_intrinsic_char (se, expr);
      break;

    case GFC_ISYM_CONVERSION:
    case GFC_ISYM_DBLE:
    case GFC_ISYM_DFLOAT:
    case GFC_ISYM_FLOAT:
    case GFC_ISYM_LOGICAL:
    case GFC_ISYM_REAL:
    case GFC_ISYM_REALPART:
    case GFC_ISYM_SNGL:
      gfc_conv_intrinsic_conversion (se, expr);
      break;

      /* Integer conversions are handled separately to make sure we get the
         correct rounding mode.  */
    case GFC_ISYM_INT:
    case GFC_ISYM_INT2:
    case GFC_ISYM_INT8:
    case GFC_ISYM_LONG:
    case GFC_ISYM_UINT:
      gfc_conv_intrinsic_int (se, expr, RND_TRUNC);
      break;

    case GFC_ISYM_NINT:
      gfc_conv_intrinsic_int (se, expr, RND_ROUND);
      break;

    case GFC_ISYM_CEILING:
      gfc_conv_intrinsic_int (se, expr, RND_CEIL);
      break;

    case GFC_ISYM_FLOOR:
      gfc_conv_intrinsic_int (se, expr, RND_FLOOR);
      break;

    case GFC_ISYM_MOD:
      gfc_conv_intrinsic_mod (se, expr, 0);
      break;

    case GFC_ISYM_MODULO:
      gfc_conv_intrinsic_mod (se, expr, 1);
      break;

    case GFC_ISYM_CAF_GET:
      gfc_conv_intrinsic_caf_get (se, expr, NULL_TREE, false, NULL);
      break;

    case GFC_ISYM_CAF_IS_PRESENT_ON_REMOTE:
      gfc_conv_intrinsic_caf_is_present_remote (se, expr);
      break;

    case GFC_ISYM_CMPLX:
      gfc_conv_intrinsic_cmplx (se, expr, name[5] == '1');
      break;

    case GFC_ISYM_COMMAND_ARGUMENT_COUNT:
      gfc_conv_intrinsic_iargc (se, expr);
      break;

    case GFC_ISYM_COMPLEX:
      gfc_conv_intrinsic_cmplx (se, expr, 1);
      break;

    case GFC_ISYM_CONJG:
      gfc_conv_intrinsic_conjg (se, expr);
      break;

    case GFC_ISYM_COUNT:
      gfc_conv_intrinsic_count (se, expr);
      break;

    case GFC_ISYM_CTIME:
      gfc_conv_intrinsic_ctime (se, expr);
      break;

    case GFC_ISYM_DIM:
      gfc_conv_intrinsic_dim (se, expr);
      break;

    case GFC_ISYM_DOT_PRODUCT:
      gfc_conv_intrinsic_dot_product (se, expr);
      break;

    case GFC_ISYM_DPROD:
      gfc_conv_intrinsic_dprod (se, expr);
      break;

    case GFC_ISYM_DSHIFTL:
      gfc_conv_intrinsic_dshift (se, expr, true);
      break;

    case GFC_ISYM_DSHIFTR:
      gfc_conv_intrinsic_dshift (se, expr, false);
      break;

    case GFC_ISYM_FDATE:
      gfc_conv_intrinsic_fdate (se, expr);
      break;

    case GFC_ISYM_FRACTION:
      gfc_conv_intrinsic_fraction (se, expr);
      break;

    case GFC_ISYM_IALL:
      gfc_conv_intrinsic_arith (se, expr, BIT_AND_EXPR, false);
      break;

    case GFC_ISYM_IAND:
      gfc_conv_intrinsic_bitop (se, expr, BIT_AND_EXPR);
      break;

    case GFC_ISYM_IANY:
      gfc_conv_intrinsic_arith (se, expr, BIT_IOR_EXPR, false);
      break;

    case GFC_ISYM_IBCLR:
      gfc_conv_intrinsic_singlebitop (se, expr, 0);
      break;

    case GFC_ISYM_IBITS:
      gfc_conv_intrinsic_ibits (se, expr);
      break;

    case GFC_ISYM_IBSET:
      gfc_conv_intrinsic_singlebitop (se, expr, 1);
      break;

    case GFC_ISYM_IACHAR:
    case GFC_ISYM_ICHAR:
      /* We assume ASCII character sequence.  */
      gfc_conv_intrinsic_ichar (se, expr);
      break;

    case GFC_ISYM_IARGC:
      gfc_conv_intrinsic_iargc (se, expr);
      break;

    case GFC_ISYM_IEOR:
      gfc_conv_intrinsic_bitop (se, expr, BIT_XOR_EXPR);
      break;

    case GFC_ISYM_INDEX:
      kind = expr->value.function.actual->expr->ts.kind;
      if (kind == 1)
       fndecl = gfor_fndecl_string_index;
      else if (kind == 4)
       fndecl = gfor_fndecl_string_index_char4;
      else
       gcc_unreachable ();

      gfc_conv_intrinsic_index_scan_verify (se, expr, fndecl);
      break;

    case GFC_ISYM_IOR:
      gfc_conv_intrinsic_bitop (se, expr, BIT_IOR_EXPR);
      break;

    case GFC_ISYM_IPARITY:
      gfc_conv_intrinsic_arith (se, expr, BIT_XOR_EXPR, false);
      break;

    case GFC_ISYM_IS_IOSTAT_END:
      gfc_conv_has_intvalue (se, expr, LIBERROR_END);
      break;

    case GFC_ISYM_IS_IOSTAT_EOR:
      gfc_conv_has_intvalue (se, expr, LIBERROR_EOR);
      break;

    case GFC_ISYM_IS_CONTIGUOUS:
      gfc_conv_intrinsic_is_contiguous (se, expr);
      break;

    case GFC_ISYM_ISNAN:
      gfc_conv_intrinsic_isnan (se, expr);
      break;

    case GFC_ISYM_KILL:
      conv_intrinsic_kill (se, expr);
      break;

    case GFC_ISYM_LSHIFT:
      gfc_conv_intrinsic_shift (se, expr, false, false);
      break;

    case GFC_ISYM_RSHIFT:
      gfc_conv_intrinsic_shift (se, expr, true, true);
      break;

    case GFC_ISYM_SHIFTA:
      gfc_conv_intrinsic_shift (se, expr, true, true);
      break;

    case GFC_ISYM_SHIFTL:
      gfc_conv_intrinsic_shift (se, expr, false, false);
      break;

    case GFC_ISYM_SHIFTR:
      gfc_conv_intrinsic_shift (se, expr, true, false);
      break;

    case GFC_ISYM_ISHFT:
      gfc_conv_intrinsic_ishft (se, expr);
      break;

    case GFC_ISYM_ISHFTC:
      gfc_conv_intrinsic_ishftc (se, expr);
      break;

    case GFC_ISYM_LEADZ:
      gfc_conv_intrinsic_leadz (se, expr);
      break;

    case GFC_ISYM_TRAILZ:
      gfc_conv_intrinsic_trailz (se, expr);
      break;

    case GFC_ISYM_POPCNT:
      gfc_conv_intrinsic_popcnt_poppar (se, expr, 0);
      break;

    case GFC_ISYM_POPPAR:
      gfc_conv_intrinsic_popcnt_poppar (se, expr, 1);
      break;

    case GFC_ISYM_LBOUND:
      gfc_conv_intrinsic_bound (se, expr, GFC_ISYM_LBOUND);
      break;

    case GFC_ISYM_LCOBOUND:
      conv_intrinsic_cobound (se, expr);
      break;

    case GFC_ISYM_TRANSPOSE:
      /* The scalarizer has already been set up for reversed dimension access
	 order ; now we just get the argument value normally.  */
      gfc_conv_expr (se, expr->value.function.actual->expr);
      break;

    case GFC_ISYM_LEN:
      gfc_conv_intrinsic_len (se, expr);
      break;

    case GFC_ISYM_LEN_TRIM:
      gfc_conv_intrinsic_len_trim (se, expr);
      break;

    case GFC_ISYM_LGE:
      gfc_conv_intrinsic_strcmp (se, expr, GE_EXPR);
      break;

    case GFC_ISYM_LGT:
      gfc_conv_intrinsic_strcmp (se, expr, GT_EXPR);
      break;

    case GFC_ISYM_LLE:
      gfc_conv_intrinsic_strcmp (se, expr, LE_EXPR);
      break;

    case GFC_ISYM_LLT:
      gfc_conv_intrinsic_strcmp (se, expr, LT_EXPR);
      break;

    case GFC_ISYM_MALLOC:
      gfc_conv_intrinsic_malloc (se, expr);
      break;

    case GFC_ISYM_MASKL:
      gfc_conv_intrinsic_mask (se, expr, 1);
      break;

    case GFC_ISYM_MASKR:
      gfc_conv_intrinsic_mask (se, expr, 0);
      break;

    case GFC_ISYM_MAX:
      if (expr->ts.type == BT_CHARACTER)
	gfc_conv_intrinsic_minmax_char (se, expr, 1);
      else
	gfc_conv_intrinsic_minmax (se, expr, GT_EXPR);
      break;

    case GFC_ISYM_MAXLOC:
      gfc_conv_intrinsic_minmaxloc (se, expr, GT_EXPR);
      break;

    case GFC_ISYM_FINDLOC:
      gfc_conv_intrinsic_findloc (se, expr);
      break;

    case GFC_ISYM_MAXVAL:
      gfc_conv_intrinsic_minmaxval (se, expr, GT_EXPR);
      break;

    case GFC_ISYM_MERGE:
      gfc_conv_intrinsic_merge (se, expr);
      break;

    case GFC_ISYM_MERGE_BITS:
      gfc_conv_intrinsic_merge_bits (se, expr);
      break;

    case GFC_ISYM_MIN:
      if (expr->ts.type == BT_CHARACTER)
	gfc_conv_intrinsic_minmax_char (se, expr, -1);
      else
	gfc_conv_intrinsic_minmax (se, expr, LT_EXPR);
      break;

    case GFC_ISYM_MINLOC:
      gfc_conv_intrinsic_minmaxloc (se, expr, LT_EXPR);
      break;

    case GFC_ISYM_MINVAL:
      gfc_conv_intrinsic_minmaxval (se, expr, LT_EXPR);
      break;

    case GFC_ISYM_NEAREST:
      gfc_conv_intrinsic_nearest (se, expr);
      break;

    case GFC_ISYM_NORM2:
      gfc_conv_intrinsic_arith (se, expr, PLUS_EXPR, true);
      break;

    case GFC_ISYM_NOT:
      gfc_conv_intrinsic_not (se, expr);
      break;

    case GFC_ISYM_OR:
      gfc_conv_intrinsic_bitop (se, expr, BIT_IOR_EXPR);
      break;

    case GFC_ISYM_OUT_OF_RANGE:
      gfc_conv_intrinsic_out_of_range (se, expr);
      break;

    case GFC_ISYM_PARITY:
      gfc_conv_intrinsic_arith (se, expr, NE_EXPR, false);
      break;

    case GFC_ISYM_PRESENT:
      gfc_conv_intrinsic_present (se, expr);
      break;

    case GFC_ISYM_PRODUCT:
      gfc_conv_intrinsic_arith (se, expr, MULT_EXPR, false);
      break;

    case GFC_ISYM_RANK:
      gfc_conv_intrinsic_rank (se, expr);
      break;

    case GFC_ISYM_RRSPACING:
      gfc_conv_intrinsic_rrspacing (se, expr);
      break;

    case GFC_ISYM_SET_EXPONENT:
      gfc_conv_intrinsic_set_exponent (se, expr);
      break;

    case GFC_ISYM_SCALE:
      gfc_conv_intrinsic_scale (se, expr);
      break;

    case GFC_ISYM_SHAPE:
      gfc_conv_intrinsic_bound (se, expr, GFC_ISYM_SHAPE);
      break;

    case GFC_ISYM_SIGN:
      gfc_conv_intrinsic_sign (se, expr);
      break;

    case GFC_ISYM_SIZE:
      gfc_conv_intrinsic_size (se, expr);
      break;

    case GFC_ISYM_SIZEOF:
    case GFC_ISYM_C_SIZEOF:
      gfc_conv_intrinsic_sizeof (se, expr);
      break;

    case GFC_ISYM_STORAGE_SIZE:
      gfc_conv_intrinsic_storage_size (se, expr);
      break;

    case GFC_ISYM_SPACING:
      gfc_conv_intrinsic_spacing (se, expr);
      break;

    case GFC_ISYM_STRIDE:
      conv_intrinsic_stride (se, expr);
      break;

    case GFC_ISYM_SUM:
      gfc_conv_intrinsic_arith (se, expr, PLUS_EXPR, false);
      break;

    case GFC_ISYM_TEAM_NUMBER:
      conv_intrinsic_team_number (se, expr);
      break;

    case GFC_ISYM_TRANSFER:
      if (se->ss && se->ss->info->useflags)
	/* Access the previously obtained result.  */
	gfc_conv_tmp_array_ref (se);
      else
	gfc_conv_intrinsic_transfer (se, expr);
      break;

    case GFC_ISYM_TTYNAM:
      gfc_conv_intrinsic_ttynam (se, expr);
      break;

    case GFC_ISYM_UBOUND:
      gfc_conv_intrinsic_bound (se, expr, GFC_ISYM_UBOUND);
      break;

    case GFC_ISYM_UCOBOUND:
      conv_intrinsic_cobound (se, expr);
      break;

    case GFC_ISYM_XOR:
      gfc_conv_intrinsic_bitop (se, expr, BIT_XOR_EXPR);
      break;

    case GFC_ISYM_LOC:
      gfc_conv_intrinsic_loc (se, expr);
      break;

    case GFC_ISYM_THIS_IMAGE:
      /* For num_images() == 1, handle as LCOBOUND.  */
      if (expr->value.function.actual->expr
	  && flag_coarray == GFC_FCOARRAY_SINGLE)
	conv_intrinsic_cobound (se, expr);
      else
	trans_this_image (se, expr);
      break;

    case GFC_ISYM_IMAGE_INDEX:
      trans_image_index (se, expr);
      break;

    case GFC_ISYM_IMAGE_STATUS:
      conv_intrinsic_image_status (se, expr);
      break;

    case GFC_ISYM_NUM_IMAGES:
      trans_num_images (se, expr);
      break;

    case GFC_ISYM_ACCESS:
    case GFC_ISYM_CHDIR:
    case GFC_ISYM_CHMOD:
    case GFC_ISYM_DTIME:
    case GFC_ISYM_ETIME:
    case GFC_ISYM_EXTENDS_TYPE_OF:
    case GFC_ISYM_FGET:
    case GFC_ISYM_FGETC:
    case GFC_ISYM_FNUM:
    case GFC_ISYM_FPUT:
    case GFC_ISYM_FPUTC:
    case GFC_ISYM_FSTAT:
    case GFC_ISYM_FTELL:
    case GFC_ISYM_GETCWD:
    case GFC_ISYM_GETGID:
    case GFC_ISYM_GETPID:
    case GFC_ISYM_GETUID:
    case GFC_ISYM_HOSTNM:
    case GFC_ISYM_IERRNO:
    case GFC_ISYM_IRAND:
    case GFC_ISYM_ISATTY:
    case GFC_ISYM_JN2:
    case GFC_ISYM_LINK:
    case GFC_ISYM_LSTAT:
    case GFC_ISYM_MATMUL:
    case GFC_ISYM_MCLOCK:
    case GFC_ISYM_MCLOCK8:
    case GFC_ISYM_RAND:
    case GFC_ISYM_RENAME:
    case GFC_ISYM_SECOND:
    case GFC_ISYM_SECNDS:
    case GFC_ISYM_SIGNAL:
    case GFC_ISYM_STAT:
    case GFC_ISYM_SYMLNK:
    case GFC_ISYM_SYSTEM:
    case GFC_ISYM_TIME:
    case GFC_ISYM_TIME8:
    case GFC_ISYM_UMASK:
    case GFC_ISYM_UNLINK:
    case GFC_ISYM_YN2:
      gfc_conv_intrinsic_funcall (se, expr);
      break;

    case GFC_ISYM_EOSHIFT:
    case GFC_ISYM_PACK:
    case GFC_ISYM_RESHAPE:
      /* For those, expr->rank should always be >0 and thus the if above the
	 switch should have matched.  */
      gcc_unreachable ();
      break;

    default:
      gfc_conv_intrinsic_lib_function (se, expr);
      break;
    }
}


static gfc_ss *
walk_inline_intrinsic_transpose (gfc_ss *ss, gfc_expr *expr)
{
  gfc_ss *arg_ss, *tmp_ss;
  gfc_actual_arglist *arg;

  arg = expr->value.function.actual;

  gcc_assert (arg->expr);

  arg_ss = gfc_walk_subexpr (gfc_ss_terminator, arg->expr);
  gcc_assert (arg_ss != gfc_ss_terminator);

  for (tmp_ss = arg_ss; ; tmp_ss = tmp_ss->next)
    {
      if (tmp_ss->info->type != GFC_SS_SCALAR
	  && tmp_ss->info->type != GFC_SS_REFERENCE)
	{
	  gcc_assert (tmp_ss->dimen == 2);

	  /* We just invert dimensions.  */
	  std::swap (tmp_ss->dim[0], tmp_ss->dim[1]);
	}

      /* Stop when tmp_ss points to the last valid element of the chain...  */
      if (tmp_ss->next == gfc_ss_terminator)
	break;
    }

  /* ... so that we can attach the rest of the chain to it.  */
  tmp_ss->next = ss;

  return arg_ss;
}


/* Move the given dimension of the given gfc_ss list to a nested gfc_ss list.
   This has the side effect of reversing the nested list, so there is no
   need to call gfc_reverse_ss on it (the given list is assumed not to be
   reversed yet).   */

static gfc_ss *
nest_loop_dimension (gfc_ss *ss, int dim)
{
  int ss_dim, i;
  gfc_ss *new_ss, *prev_ss = gfc_ss_terminator;
  gfc_loopinfo *new_loop;

  gcc_assert (ss != gfc_ss_terminator);

  for (; ss != gfc_ss_terminator; ss = ss->next)
    {
      new_ss = gfc_get_ss ();
      new_ss->next = prev_ss;
      new_ss->parent = ss;
      new_ss->info = ss->info;
      new_ss->info->refcount++;
      if (ss->dimen != 0)
	{
	  gcc_assert (ss->info->type != GFC_SS_SCALAR
		      && ss->info->type != GFC_SS_REFERENCE);

	  new_ss->dimen = 1;
	  new_ss->dim[0] = ss->dim[dim];

	  gcc_assert (dim < ss->dimen);

	  ss_dim = --ss->dimen;
	  for (i = dim; i < ss_dim; i++)
	    ss->dim[i] = ss->dim[i + 1];

	  ss->dim[ss_dim] = 0;
	}
      prev_ss = new_ss;

      if (ss->nested_ss)
	{
	  ss->nested_ss->parent = new_ss;
	  new_ss->nested_ss = ss->nested_ss;
	}
      ss->nested_ss = new_ss;
    }

  new_loop = gfc_get_loopinfo ();
  gfc_init_loopinfo (new_loop);

  gcc_assert (prev_ss != NULL);
  gcc_assert (prev_ss != gfc_ss_terminator);
  gfc_add_ss_to_loop (new_loop, prev_ss);
  return new_ss->parent;
}


/* Create the gfc_ss list for the SUM/PRODUCT arguments when the function
   is to be inlined.  */

static gfc_ss *
walk_inline_intrinsic_arith (gfc_ss *ss, gfc_expr *expr)
{
  gfc_ss *tmp_ss, *tail, *array_ss;
  gfc_actual_arglist *arg1, *arg2, *arg3;
  int sum_dim;
  bool scalar_mask = false;

  /* The rank of the result will be determined later.  */
  arg1 = expr->value.function.actual;
  arg2 = arg1->next;
  arg3 = arg2->next;
  gcc_assert (arg3 != NULL);

  if (expr->rank == 0)
    return ss;

  tmp_ss = gfc_ss_terminator;

  if (arg3->expr)
    {
      gfc_ss *mask_ss;

      mask_ss = gfc_walk_subexpr (tmp_ss, arg3->expr);
      if (mask_ss == tmp_ss)
	scalar_mask = 1;

      tmp_ss = mask_ss;
    }

  array_ss = gfc_walk_subexpr (tmp_ss, arg1->expr);
  gcc_assert (array_ss != tmp_ss);

  /* Odd thing: If the mask is scalar, it is used by the frontend after
     the array (to make an if around the nested loop). Thus it shall
     be after array_ss once the gfc_ss list is reversed.  */
  if (scalar_mask)
    tmp_ss = gfc_get_scalar_ss (array_ss, arg3->expr);
  else
    tmp_ss = array_ss;

  /* "Hide" the dimension on which we will sum in the first arg's scalarization
     chain.  */
  sum_dim = mpz_get_si (arg2->expr->value.integer) - 1;
  tail = nest_loop_dimension (tmp_ss, sum_dim);
  tail->next = ss;

  return tmp_ss;
}


/* Create the gfc_ss list for the arguments to MINLOC or MAXLOC when the
   function is to be inlined.  */

static gfc_ss *
walk_inline_intrinsic_minmaxloc (gfc_ss *ss, gfc_expr *expr ATTRIBUTE_UNUSED)
{
  if (expr->rank == 0)
    return ss;

  gfc_actual_arglist *array_arg = expr->value.function.actual;
  gfc_actual_arglist *dim_arg = array_arg->next;
  gfc_actual_arglist *mask_arg = dim_arg->next;
  gfc_actual_arglist *kind_arg = mask_arg->next;
  gfc_actual_arglist *back_arg = kind_arg->next;

  gfc_expr *array = array_arg->expr;
  gfc_expr *dim = dim_arg->expr;
  gfc_expr *mask = mask_arg->expr;
  gfc_expr *back = back_arg->expr;

  if (dim == nullptr)
    return gfc_get_array_ss (ss, expr, 1, GFC_SS_INTRINSIC);

  gfc_ss *tmp_ss = gfc_ss_terminator;

  bool scalar_mask = false;
  if (mask)
    {
      gfc_ss *mask_ss = gfc_walk_subexpr (tmp_ss, mask);
      if (mask_ss == tmp_ss)
	scalar_mask = true;
      else if (maybe_absent_optional_variable (mask))
	mask_ss->info->can_be_null_ref = true;

      tmp_ss = mask_ss;
    }

  gfc_ss *array_ss = gfc_walk_subexpr (tmp_ss, array);
  gcc_assert (array_ss != tmp_ss);

  tmp_ss = array_ss;

  /* Move the dimension on which we will sum to a separate nested scalarization
     chain, "hiding" that dimension from the outer scalarization.  */
  int dim_val = mpz_get_si (dim->value.integer);
  gfc_ss *tail = nest_loop_dimension (tmp_ss, dim_val - 1);

  if (back && array->rank > 1)
    {
      /* If there are nested scalarization loops, include BACK in the
	 scalarization chains to avoid evaluating it multiple times in a loop.
	 Otherwise, prefer to handle it outside of scalarization.  */
      gfc_ss *back_ss = gfc_get_scalar_ss (ss, back);
      back_ss->info->type = GFC_SS_REFERENCE;
      if (maybe_absent_optional_variable (back))
	back_ss->info->can_be_null_ref = true;

      tail->next = back_ss;
    }
  else
    tail->next = ss;

  if (scalar_mask)
    {
      tmp_ss = gfc_get_scalar_ss (tmp_ss, mask);
      /* MASK can be a forwarded optional argument, so make the necessary setup
	 to avoid the scalarizer generating any unguarded pointer dereference in
	 that case.  */
      tmp_ss->info->type = GFC_SS_REFERENCE;
      if (maybe_absent_optional_variable (mask))
	tmp_ss->info->can_be_null_ref = true;
    }

  return tmp_ss;
}


static gfc_ss *
walk_inline_intrinsic_function (gfc_ss * ss, gfc_expr * expr)
{

  switch (expr->value.function.isym->id)
    {
      case GFC_ISYM_PRODUCT:
      case GFC_ISYM_SUM:
	return walk_inline_intrinsic_arith (ss, expr);

      case GFC_ISYM_TRANSPOSE:
	return walk_inline_intrinsic_transpose (ss, expr);

      case GFC_ISYM_MAXLOC:
      case GFC_ISYM_MINLOC:
	return walk_inline_intrinsic_minmaxloc (ss, expr);

      default:
	gcc_unreachable ();
    }
  gcc_unreachable ();
}


/* This generates code to execute before entering the scalarization loop.
   Currently does nothing.  */

void
gfc_add_intrinsic_ss_code (gfc_loopinfo * loop ATTRIBUTE_UNUSED, gfc_ss * ss)
{
  switch (ss->info->expr->value.function.isym->id)
    {
    case GFC_ISYM_UBOUND:
    case GFC_ISYM_LBOUND:
    case GFC_ISYM_UCOBOUND:
    case GFC_ISYM_LCOBOUND:
    case GFC_ISYM_MAXLOC:
    case GFC_ISYM_MINLOC:
    case GFC_ISYM_THIS_IMAGE:
    case GFC_ISYM_SHAPE:
      break;

    default:
      gcc_unreachable ();
    }
}


/* The LBOUND, LCOBOUND, UBOUND, UCOBOUND, and SHAPE intrinsics with
   one parameter are expanded into code inside the scalarization loop.  */

static gfc_ss *
gfc_walk_intrinsic_bound (gfc_ss * ss, gfc_expr * expr)
{
  if (expr->value.function.actual->expr->ts.type == BT_CLASS)
    gfc_add_class_array_ref (expr->value.function.actual->expr);

  /* The two argument version returns a scalar.  */
  if (expr->value.function.isym->id != GFC_ISYM_SHAPE
      && expr->value.function.actual->next->expr)
    return ss;

  return gfc_get_array_ss (ss, expr, 1, GFC_SS_INTRINSIC);
}


/* Walk an intrinsic array libcall.  */

static gfc_ss *
gfc_walk_intrinsic_libfunc (gfc_ss * ss, gfc_expr * expr)
{
  gcc_assert (expr->rank > 0);
  return gfc_get_array_ss (ss, expr, expr->rank, GFC_SS_FUNCTION);
}


/* Return whether the function call expression EXPR will be expanded
   inline by gfc_conv_intrinsic_function.  */

bool
gfc_inline_intrinsic_function_p (gfc_expr *expr)
{
  gfc_actual_arglist *args, *dim_arg, *mask_arg;
  gfc_expr *maskexpr;

  gfc_intrinsic_sym *isym = expr->value.function.isym;
  if (!isym)
    return false;

  switch (isym->id)
    {
    case GFC_ISYM_PRODUCT:
    case GFC_ISYM_SUM:
      /* Disable inline expansion if code size matters.  */
      if (optimize_size)
	return false;

      args = expr->value.function.actual;
      dim_arg = args->next;

      /* We need to be able to subset the SUM argument at compile-time.  */
      if (dim_arg->expr && dim_arg->expr->expr_type != EXPR_CONSTANT)
	return false;

      /* FIXME: If MASK is optional for a more than two-dimensional
	 argument, the scalarizer gets confused if the mask is
	 absent.  See PR 82995.  For now, fall back to the library
	 function.  */

      mask_arg = dim_arg->next;
      maskexpr = mask_arg->expr;

      if (expr->rank > 0 && maskexpr && maskexpr->expr_type == EXPR_VARIABLE
	  && maskexpr->symtree->n.sym->attr.dummy
	  && maskexpr->symtree->n.sym->attr.optional)
	return false;

      return true;

    case GFC_ISYM_TRANSPOSE:
      return true;

    case GFC_ISYM_MINLOC:
    case GFC_ISYM_MAXLOC:
      {
	if ((isym->id == GFC_ISYM_MINLOC
	     && (flag_inline_intrinsics
		 & GFC_FLAG_INLINE_INTRINSIC_MINLOC) == 0)
	    || (isym->id == GFC_ISYM_MAXLOC
		&& (flag_inline_intrinsics
		    & GFC_FLAG_INLINE_INTRINSIC_MAXLOC) == 0))
	  return false;

	gfc_actual_arglist *array_arg = expr->value.function.actual;
	gfc_actual_arglist *dim_arg = array_arg->next;

	gfc_expr *array = array_arg->expr;
	gfc_expr *dim = dim_arg->expr;

	if (!(array->ts.type == BT_INTEGER
	      || array->ts.type == BT_REAL))
	  return false;

	if (array->rank == 1)
	  return true;

	if (dim != nullptr
	    && dim->expr_type != EXPR_CONSTANT)
	  return false;

	return true;
      }

    default:
      return false;
    }
}


/* Returns nonzero if the specified intrinsic function call maps directly to
   an external library call.  Should only be used for functions that return
   arrays.  */

int
gfc_is_intrinsic_libcall (gfc_expr * expr)
{
  gcc_assert (expr->expr_type == EXPR_FUNCTION && expr->value.function.isym);
  gcc_assert (expr->rank > 0);

  if (gfc_inline_intrinsic_function_p (expr))
    return 0;

  switch (expr->value.function.isym->id)
    {
    case GFC_ISYM_ALL:
    case GFC_ISYM_ANY:
    case GFC_ISYM_COUNT:
    case GFC_ISYM_FINDLOC:
    case GFC_ISYM_JN2:
    case GFC_ISYM_IANY:
    case GFC_ISYM_IALL:
    case GFC_ISYM_IPARITY:
    case GFC_ISYM_MATMUL:
    case GFC_ISYM_MAXLOC:
    case GFC_ISYM_MAXVAL:
    case GFC_ISYM_MINLOC:
    case GFC_ISYM_MINVAL:
    case GFC_ISYM_NORM2:
    case GFC_ISYM_PARITY:
    case GFC_ISYM_PRODUCT:
    case GFC_ISYM_SUM:
    case GFC_ISYM_SPREAD:
    case GFC_ISYM_YN2:
      /* Ignore absent optional parameters.  */
      return 1;

    case GFC_ISYM_CSHIFT:
    case GFC_ISYM_EOSHIFT:
    case GFC_ISYM_GET_TEAM:
    case GFC_ISYM_FAILED_IMAGES:
    case GFC_ISYM_STOPPED_IMAGES:
    case GFC_ISYM_PACK:
    case GFC_ISYM_RESHAPE:
    case GFC_ISYM_UNPACK:
      /* Pass absent optional parameters.  */
      return 2;

    default:
      return 0;
    }
}

/* Walk an intrinsic function.  */
gfc_ss *
gfc_walk_intrinsic_function (gfc_ss * ss, gfc_expr * expr,
			     gfc_intrinsic_sym * isym)
{
  gcc_assert (isym);

  if (isym->elemental)
    return gfc_walk_elemental_function_args (ss, expr->value.function.actual,
					     expr->value.function.isym,
					     GFC_SS_SCALAR);

  if (expr->rank == 0 && expr->corank == 0)
    return ss;

  if (gfc_inline_intrinsic_function_p (expr))
    return walk_inline_intrinsic_function (ss, expr);

  if (expr->rank != 0 && gfc_is_intrinsic_libcall (expr))
    return gfc_walk_intrinsic_libfunc (ss, expr);

  /* Special cases.  */
  switch (isym->id)
    {
    case GFC_ISYM_LBOUND:
    case GFC_ISYM_LCOBOUND:
    case GFC_ISYM_UBOUND:
    case GFC_ISYM_UCOBOUND:
    case GFC_ISYM_THIS_IMAGE:
    case GFC_ISYM_SHAPE:
      return gfc_walk_intrinsic_bound (ss, expr);

    case GFC_ISYM_TRANSFER:
    case GFC_ISYM_CAF_GET:
      return gfc_walk_intrinsic_libfunc (ss, expr);

    default:
      /* This probably meant someone forgot to add an intrinsic to the above
         list(s) when they implemented it, or something's gone horribly
	 wrong.  */
      gcc_unreachable ();
    }
}

static tree
conv_co_collective (gfc_code *code)
{
  gfc_se argse;
  stmtblock_t block, post_block;
  tree fndecl, array = NULL_TREE, strlen, image_index, stat, errmsg, errmsg_len;
  gfc_expr *image_idx_expr, *stat_expr, *errmsg_expr, *opr_expr;

  gfc_start_block (&block);
  gfc_init_block (&post_block);

  if (code->resolved_isym->id == GFC_ISYM_CO_REDUCE)
    {
      opr_expr = code->ext.actual->next->expr;
      image_idx_expr = code->ext.actual->next->next->expr;
      stat_expr = code->ext.actual->next->next->next->expr;
      errmsg_expr = code->ext.actual->next->next->next->next->expr;
    }
  else
    {
      opr_expr = NULL;
      image_idx_expr = code->ext.actual->next->expr;
      stat_expr = code->ext.actual->next->next->expr;
      errmsg_expr = code->ext.actual->next->next->next->expr;
    }

  /* stat.  */
  if (stat_expr)
    {
      gfc_init_se (&argse, NULL);
      gfc_conv_expr (&argse, stat_expr);
      gfc_add_block_to_block (&block, &argse.pre);
      gfc_add_block_to_block (&post_block, &argse.post);
      stat = argse.expr;
      if (flag_coarray != GFC_FCOARRAY_SINGLE)
	stat = gfc_build_addr_expr (NULL_TREE, stat);
    }
  else if (flag_coarray == GFC_FCOARRAY_SINGLE)
    stat = NULL_TREE;
  else
    stat = null_pointer_node;

  /* Early exit for GFC_FCOARRAY_SINGLE.  */
  if (flag_coarray == GFC_FCOARRAY_SINGLE)
    {
      if (stat != NULL_TREE)
	{
	  /* For optional stats, check the pointer is valid before zero'ing.  */
	  if (gfc_expr_attr (stat_expr).optional)
	    {
	      tree tmp;
	      stmtblock_t ass_block;
	      gfc_start_block (&ass_block);
	      gfc_add_modify (&ass_block, stat,
			      fold_convert (TREE_TYPE (stat),
					    integer_zero_node));
	      tmp = fold_build2 (NE_EXPR, logical_type_node,
				 gfc_build_addr_expr (NULL_TREE, stat),
				 null_pointer_node);
	      tmp = fold_build3 (COND_EXPR, void_type_node, tmp,
				 gfc_finish_block (&ass_block),
				 build_empty_stmt (input_location));
	      gfc_add_expr_to_block (&block, tmp);
	    }
	  else
	    gfc_add_modify (&block, stat,
			    fold_convert (TREE_TYPE (stat), integer_zero_node));
	}
      return gfc_finish_block (&block);
    }

  gfc_symbol *derived = code->ext.actual->expr->ts.type == BT_DERIVED
    ? code->ext.actual->expr->ts.u.derived : NULL;

  /* Handle the array.  */
  gfc_init_se (&argse, NULL);
  if (!derived || !derived->attr.alloc_comp
      || code->resolved_isym->id != GFC_ISYM_CO_BROADCAST)
    {
      if (code->ext.actual->expr->rank == 0)
	{
	  symbol_attribute attr;
	  gfc_clear_attr (&attr);
	  gfc_init_se (&argse, NULL);
	  gfc_conv_expr (&argse, code->ext.actual->expr);
	  gfc_add_block_to_block (&block, &argse.pre);
	  gfc_add_block_to_block (&post_block, &argse.post);
	  array = gfc_conv_scalar_to_descriptor (&argse, argse.expr, attr);
	  array = gfc_build_addr_expr (NULL_TREE, array);
	}
      else
	{
	  argse.want_pointer = 1;
	  gfc_conv_expr_descriptor (&argse, code->ext.actual->expr);
	  array = argse.expr;
	}
    }

  gfc_add_block_to_block (&block, &argse.pre);
  gfc_add_block_to_block (&post_block, &argse.post);

  if (code->ext.actual->expr->ts.type == BT_CHARACTER)
    strlen = argse.string_length;
  else
    strlen = integer_zero_node;

  /* image_index.  */
  if (image_idx_expr)
    {
      gfc_init_se (&argse, NULL);
      gfc_conv_expr (&argse, image_idx_expr);
      gfc_add_block_to_block (&block, &argse.pre);
      gfc_add_block_to_block (&post_block, &argse.post);
      image_index = fold_convert (integer_type_node, argse.expr);
    }
  else
    image_index = integer_zero_node;

  /* errmsg.  */
  if (errmsg_expr)
    {
      gfc_init_se (&argse, NULL);
      gfc_conv_expr (&argse, errmsg_expr);
      gfc_add_block_to_block (&block, &argse.pre);
      gfc_add_block_to_block (&post_block, &argse.post);
      errmsg = argse.expr;
      errmsg_len = fold_convert (size_type_node, argse.string_length);
    }
  else
    {
      errmsg = null_pointer_node;
      errmsg_len = build_zero_cst (size_type_node);
    }

  /* Generate the function call.  */
  switch (code->resolved_isym->id)
    {
    case GFC_ISYM_CO_BROADCAST:
      fndecl = gfor_fndecl_co_broadcast;
      break;
    case GFC_ISYM_CO_MAX:
      fndecl = gfor_fndecl_co_max;
      break;
    case GFC_ISYM_CO_MIN:
      fndecl = gfor_fndecl_co_min;
      break;
    case GFC_ISYM_CO_REDUCE:
      fndecl = gfor_fndecl_co_reduce;
      break;
    case GFC_ISYM_CO_SUM:
      fndecl = gfor_fndecl_co_sum;
      break;
    default:
      gcc_unreachable ();
    }

  if (derived && derived->attr.alloc_comp
      && code->resolved_isym->id == GFC_ISYM_CO_BROADCAST)
    /* The derived type has the attribute 'alloc_comp'.  */
    {
      tree tmp = gfc_bcast_alloc_comp (derived, code->ext.actual->expr,
				       code->ext.actual->expr->rank,
				       image_index, stat, errmsg, errmsg_len);
      gfc_add_expr_to_block (&block, tmp);
    }
  else
    {
      if (code->resolved_isym->id == GFC_ISYM_CO_SUM
	  || code->resolved_isym->id == GFC_ISYM_CO_BROADCAST)
	fndecl = build_call_expr_loc (input_location, fndecl, 5, array,
				      image_index, stat, errmsg, errmsg_len);
      else if (code->resolved_isym->id != GFC_ISYM_CO_REDUCE)
	fndecl = build_call_expr_loc (input_location, fndecl, 6, array,
				      image_index, stat, errmsg,
				      strlen, errmsg_len);
      else
	{
	  tree opr, opr_flags;

	  // FIXME: Handle TS29113's bind(C) strings with descriptor.
	  int opr_flag_int;
	  if (gfc_is_proc_ptr_comp (opr_expr))
	    {
	      gfc_symbol *sym = gfc_get_proc_ptr_comp (opr_expr)->ts.interface;
	      opr_flag_int = sym->attr.dimension
		|| (sym->ts.type == BT_CHARACTER
		    && !sym->attr.is_bind_c)
		? GFC_CAF_BYREF : 0;
	      opr_flag_int |= opr_expr->ts.type == BT_CHARACTER
		&& !sym->attr.is_bind_c
		? GFC_CAF_HIDDENLEN : 0;
	      opr_flag_int |= sym->formal->sym->attr.value
		? GFC_CAF_ARG_VALUE : 0;
	    }
	  else
	    {
	      opr_flag_int = gfc_return_by_reference (opr_expr->symtree->n.sym)
		? GFC_CAF_BYREF : 0;
	      opr_flag_int |= opr_expr->ts.type == BT_CHARACTER
		&& !opr_expr->symtree->n.sym->attr.is_bind_c
		? GFC_CAF_HIDDENLEN : 0;
	      opr_flag_int |= opr_expr->symtree->n.sym->formal->sym->attr.value
		? GFC_CAF_ARG_VALUE : 0;
	    }
	  opr_flags = build_int_cst (integer_type_node, opr_flag_int);
	  gfc_conv_expr (&argse, opr_expr);
	  opr = argse.expr;
	  fndecl = build_call_expr_loc (input_location, fndecl, 8, array, opr,
					opr_flags, image_index, stat, errmsg,
					strlen, errmsg_len);
	}
    }

  gfc_add_expr_to_block (&block, fndecl);
  gfc_add_block_to_block (&block, &post_block);

  return gfc_finish_block (&block);
}


static tree
conv_intrinsic_atomic_op (gfc_code *code)
{
  gfc_se argse;
  tree tmp, atom, value, old = NULL_TREE, stat = NULL_TREE;
  stmtblock_t block, post_block;
  gfc_expr *atom_expr = code->ext.actual->expr;
  gfc_expr *stat_expr;
  built_in_function fn;

  if (atom_expr->expr_type == EXPR_FUNCTION
      && atom_expr->value.function.isym
      && atom_expr->value.function.isym->id == GFC_ISYM_CAF_GET)
    atom_expr = atom_expr->value.function.actual->expr;

  gfc_start_block (&block);
  gfc_init_block (&post_block);

  gfc_init_se (&argse, NULL);
  argse.want_pointer = 1;
  gfc_conv_expr (&argse, atom_expr);
  gfc_add_block_to_block (&block, &argse.pre);
  gfc_add_block_to_block (&post_block, &argse.post);
  atom = argse.expr;

  gfc_init_se (&argse, NULL);
  if (flag_coarray == GFC_FCOARRAY_LIB
      && code->ext.actual->next->expr->ts.kind == atom_expr->ts.kind)
    argse.want_pointer = 1;
  gfc_conv_expr (&argse, code->ext.actual->next->expr);
  gfc_add_block_to_block (&block, &argse.pre);
  gfc_add_block_to_block (&post_block, &argse.post);
  value = argse.expr;

  switch (code->resolved_isym->id)
    {
    case GFC_ISYM_ATOMIC_ADD:
    case GFC_ISYM_ATOMIC_AND:
    case GFC_ISYM_ATOMIC_DEF:
    case GFC_ISYM_ATOMIC_OR:
    case GFC_ISYM_ATOMIC_XOR:
      stat_expr = code->ext.actual->next->next->expr;
      if (flag_coarray == GFC_FCOARRAY_LIB)
	old = null_pointer_node;
      break;
    default:
      gfc_init_se (&argse, NULL);
      if (flag_coarray == GFC_FCOARRAY_LIB)
	argse.want_pointer = 1;
      gfc_conv_expr (&argse, code->ext.actual->next->next->expr);
      gfc_add_block_to_block (&block, &argse.pre);
      gfc_add_block_to_block (&post_block, &argse.post);
      old = argse.expr;
      stat_expr = code->ext.actual->next->next->next->expr;
    }

  /* STAT=  */
  if (stat_expr != NULL)
    {
      gcc_assert (stat_expr->expr_type == EXPR_VARIABLE);
      gfc_init_se (&argse, NULL);
      if (flag_coarray == GFC_FCOARRAY_LIB)
	argse.want_pointer = 1;
      gfc_conv_expr_val (&argse, stat_expr);
      gfc_add_block_to_block (&block, &argse.pre);
      gfc_add_block_to_block (&post_block, &argse.post);
      stat = argse.expr;
    }
  else if (flag_coarray == GFC_FCOARRAY_LIB)
    stat = null_pointer_node;

  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tree image_index, caf_decl, offset, token;
      int op;

      switch (code->resolved_isym->id)
	{
	case GFC_ISYM_ATOMIC_ADD:
	case GFC_ISYM_ATOMIC_FETCH_ADD:
	  op = (int) GFC_CAF_ATOMIC_ADD;
	  break;
	case GFC_ISYM_ATOMIC_AND:
	case GFC_ISYM_ATOMIC_FETCH_AND:
	  op = (int) GFC_CAF_ATOMIC_AND;
	  break;
	case GFC_ISYM_ATOMIC_OR:
	case GFC_ISYM_ATOMIC_FETCH_OR:
	  op = (int) GFC_CAF_ATOMIC_OR;
	  break;
	case GFC_ISYM_ATOMIC_XOR:
	case GFC_ISYM_ATOMIC_FETCH_XOR:
	  op = (int) GFC_CAF_ATOMIC_XOR;
	  break;
	case GFC_ISYM_ATOMIC_DEF:
	  op = 0;  /* Unused.  */
	  break;
	default:
	  gcc_unreachable ();
	}

      caf_decl = gfc_get_tree_for_caf_expr (atom_expr);
      if (TREE_CODE (TREE_TYPE (caf_decl)) == REFERENCE_TYPE)
	caf_decl = build_fold_indirect_ref_loc (input_location, caf_decl);

      if (gfc_is_coindexed (atom_expr))
	image_index = gfc_caf_get_image_index (&block, atom_expr, caf_decl);
      else
	image_index = integer_zero_node;

      if (!POINTER_TYPE_P (TREE_TYPE (value)))
	{
	  tmp = gfc_create_var (TREE_TYPE (TREE_TYPE (atom)), "value");
	  gfc_add_modify (&block, tmp, fold_convert (TREE_TYPE (tmp), value));
          value = gfc_build_addr_expr (NULL_TREE, tmp);
	}

      gfc_init_se (&argse, NULL);
      gfc_get_caf_token_offset (&argse, &token, &offset, caf_decl, atom,
				atom_expr);

      gfc_add_block_to_block (&block, &argse.pre);
      if (code->resolved_isym->id == GFC_ISYM_ATOMIC_DEF)
	tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_atomic_def, 7,
				   token, offset, image_index, value, stat,
				   build_int_cst (integer_type_node,
						  (int) atom_expr->ts.type),
				   build_int_cst (integer_type_node,
						  (int) atom_expr->ts.kind));
      else
	tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_atomic_op, 9,
				   build_int_cst (integer_type_node, op),
				   token, offset, image_index, value, old, stat,
				   build_int_cst (integer_type_node,
						  (int) atom_expr->ts.type),
				   build_int_cst (integer_type_node,
						  (int) atom_expr->ts.kind));

      gfc_add_expr_to_block (&block, tmp);
      gfc_add_block_to_block (&block, &argse.post);
      gfc_add_block_to_block (&block, &post_block);
      return gfc_finish_block (&block);
    }


  switch (code->resolved_isym->id)
    {
    case GFC_ISYM_ATOMIC_ADD:
    case GFC_ISYM_ATOMIC_FETCH_ADD:
      fn = BUILT_IN_ATOMIC_FETCH_ADD_N;
      break;
    case GFC_ISYM_ATOMIC_AND:
    case GFC_ISYM_ATOMIC_FETCH_AND:
      fn = BUILT_IN_ATOMIC_FETCH_AND_N;
      break;
    case GFC_ISYM_ATOMIC_DEF:
      fn = BUILT_IN_ATOMIC_STORE_N;
      break;
    case GFC_ISYM_ATOMIC_OR:
    case GFC_ISYM_ATOMIC_FETCH_OR:
      fn = BUILT_IN_ATOMIC_FETCH_OR_N;
      break;
    case GFC_ISYM_ATOMIC_XOR:
    case GFC_ISYM_ATOMIC_FETCH_XOR:
      fn = BUILT_IN_ATOMIC_FETCH_XOR_N;
      break;
    default:
      gcc_unreachable ();
    }

  tmp = TREE_TYPE (TREE_TYPE (atom));
  fn = (built_in_function) ((int) fn
			    + exact_log2 (tree_to_uhwi (TYPE_SIZE_UNIT (tmp)))
			    + 1);
  tree itype = TREE_TYPE (TREE_TYPE (atom));
  tmp = builtin_decl_explicit (fn);

  switch (code->resolved_isym->id)
    {
    case GFC_ISYM_ATOMIC_ADD:
    case GFC_ISYM_ATOMIC_AND:
    case GFC_ISYM_ATOMIC_DEF:
    case GFC_ISYM_ATOMIC_OR:
    case GFC_ISYM_ATOMIC_XOR:
      tmp = build_call_expr_loc (input_location, tmp, 3, atom,
				 fold_convert (itype, value),
				 build_int_cst (NULL, MEMMODEL_RELAXED));
      gfc_add_expr_to_block (&block, tmp);
      break;
    default:
      tmp = build_call_expr_loc (input_location, tmp, 3, atom,
				 fold_convert (itype, value),
				 build_int_cst (NULL, MEMMODEL_RELAXED));
      gfc_add_modify (&block, old, fold_convert (TREE_TYPE (old), tmp));
      break;
    }

  if (stat != NULL_TREE)
    gfc_add_modify (&block, stat, build_int_cst (TREE_TYPE (stat), 0));
  gfc_add_block_to_block (&block, &post_block);
  return gfc_finish_block (&block);
}


static tree
conv_intrinsic_atomic_ref (gfc_code *code)
{
  gfc_se argse;
  tree tmp, atom, value, stat = NULL_TREE;
  stmtblock_t block, post_block;
  built_in_function fn;
  gfc_expr *atom_expr = code->ext.actual->next->expr;

  if (atom_expr->expr_type == EXPR_FUNCTION
      && atom_expr->value.function.isym
      && atom_expr->value.function.isym->id == GFC_ISYM_CAF_GET)
    atom_expr = atom_expr->value.function.actual->expr;

  gfc_start_block (&block);
  gfc_init_block (&post_block);
  gfc_init_se (&argse, NULL);
  argse.want_pointer = 1;
  gfc_conv_expr (&argse, atom_expr);
  gfc_add_block_to_block (&block, &argse.pre);
  gfc_add_block_to_block (&post_block, &argse.post);
  atom = argse.expr;

  gfc_init_se (&argse, NULL);
  if (flag_coarray == GFC_FCOARRAY_LIB
      && code->ext.actual->expr->ts.kind == atom_expr->ts.kind)
    argse.want_pointer = 1;
  gfc_conv_expr (&argse, code->ext.actual->expr);
  gfc_add_block_to_block (&block, &argse.pre);
  gfc_add_block_to_block (&post_block, &argse.post);
  value = argse.expr;

  /* STAT=  */
  if (code->ext.actual->next->next->expr != NULL)
    {
      gcc_assert (code->ext.actual->next->next->expr->expr_type
		  == EXPR_VARIABLE);
      gfc_init_se (&argse, NULL);
      if (flag_coarray == GFC_FCOARRAY_LIB)
	argse.want_pointer = 1;
      gfc_conv_expr_val (&argse, code->ext.actual->next->next->expr);
      gfc_add_block_to_block (&block, &argse.pre);
      gfc_add_block_to_block (&post_block, &argse.post);
      stat = argse.expr;
    }
  else if (flag_coarray == GFC_FCOARRAY_LIB)
    stat = null_pointer_node;

  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tree image_index, caf_decl, offset, token;
      tree orig_value = NULL_TREE, vardecl = NULL_TREE;

      caf_decl = gfc_get_tree_for_caf_expr (atom_expr);
      if (TREE_CODE (TREE_TYPE (caf_decl)) == REFERENCE_TYPE)
	caf_decl = build_fold_indirect_ref_loc (input_location, caf_decl);

      if (gfc_is_coindexed (atom_expr))
	image_index = gfc_caf_get_image_index (&block, atom_expr, caf_decl);
      else
	image_index = integer_zero_node;

      gfc_init_se (&argse, NULL);
      gfc_get_caf_token_offset (&argse, &token, &offset, caf_decl, atom,
				atom_expr);
      gfc_add_block_to_block (&block, &argse.pre);

      /* Different type, need type conversion.  */
      if (!POINTER_TYPE_P (TREE_TYPE (value)))
	{
	  vardecl = gfc_create_var (TREE_TYPE (TREE_TYPE (atom)), "value");
          orig_value = value;
          value = gfc_build_addr_expr (NULL_TREE, vardecl);
	}

      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_atomic_ref, 7,
				 token, offset, image_index, value, stat,
				 build_int_cst (integer_type_node,
						(int) atom_expr->ts.type),
				 build_int_cst (integer_type_node,
						(int) atom_expr->ts.kind));
      gfc_add_expr_to_block (&block, tmp);
      if (vardecl != NULL_TREE)
	gfc_add_modify (&block, orig_value,
			fold_convert (TREE_TYPE (orig_value), vardecl));
      gfc_add_block_to_block (&block, &argse.post);
      gfc_add_block_to_block (&block, &post_block);
      return gfc_finish_block (&block);
    }

  tmp = TREE_TYPE (TREE_TYPE (atom));
  fn = (built_in_function) ((int) BUILT_IN_ATOMIC_LOAD_N
			    + exact_log2 (tree_to_uhwi (TYPE_SIZE_UNIT (tmp)))
			    + 1);
  tmp = builtin_decl_explicit (fn);
  tmp = build_call_expr_loc (input_location, tmp, 2, atom,
			     build_int_cst (integer_type_node,
					    MEMMODEL_RELAXED));
  gfc_add_modify (&block, value, fold_convert (TREE_TYPE (value), tmp));

  if (stat != NULL_TREE)
    gfc_add_modify (&block, stat, build_int_cst (TREE_TYPE (stat), 0));
  gfc_add_block_to_block (&block, &post_block);
  return gfc_finish_block (&block);
}


static tree
conv_intrinsic_atomic_cas (gfc_code *code)
{
  gfc_se argse;
  tree tmp, atom, old, new_val, comp, stat = NULL_TREE;
  stmtblock_t block, post_block;
  built_in_function fn;
  gfc_expr *atom_expr = code->ext.actual->expr;

  if (atom_expr->expr_type == EXPR_FUNCTION
      && atom_expr->value.function.isym
      && atom_expr->value.function.isym->id == GFC_ISYM_CAF_GET)
    atom_expr = atom_expr->value.function.actual->expr;

  gfc_init_block (&block);
  gfc_init_block (&post_block);
  gfc_init_se (&argse, NULL);
  argse.want_pointer = 1;
  gfc_conv_expr (&argse, atom_expr);
  atom = argse.expr;

  gfc_init_se (&argse, NULL);
  if (flag_coarray == GFC_FCOARRAY_LIB)
    argse.want_pointer = 1;
  gfc_conv_expr (&argse, code->ext.actual->next->expr);
  gfc_add_block_to_block (&block, &argse.pre);
  gfc_add_block_to_block (&post_block, &argse.post);
  old = argse.expr;

  gfc_init_se (&argse, NULL);
  if (flag_coarray == GFC_FCOARRAY_LIB)
    argse.want_pointer = 1;
  gfc_conv_expr (&argse, code->ext.actual->next->next->expr);
  gfc_add_block_to_block (&block, &argse.pre);
  gfc_add_block_to_block (&post_block, &argse.post);
  comp = argse.expr;

  gfc_init_se (&argse, NULL);
  if (flag_coarray == GFC_FCOARRAY_LIB
      && code->ext.actual->next->next->next->expr->ts.kind
	 == atom_expr->ts.kind)
    argse.want_pointer = 1;
  gfc_conv_expr (&argse, code->ext.actual->next->next->next->expr);
  gfc_add_block_to_block (&block, &argse.pre);
  gfc_add_block_to_block (&post_block, &argse.post);
  new_val = argse.expr;

  /* STAT=  */
  if (code->ext.actual->next->next->next->next->expr != NULL)
    {
      gcc_assert (code->ext.actual->next->next->next->next->expr->expr_type
		  == EXPR_VARIABLE);
      gfc_init_se (&argse, NULL);
      if (flag_coarray == GFC_FCOARRAY_LIB)
	argse.want_pointer = 1;
      gfc_conv_expr_val (&argse,
			 code->ext.actual->next->next->next->next->expr);
      gfc_add_block_to_block (&block, &argse.pre);
      gfc_add_block_to_block (&post_block, &argse.post);
      stat = argse.expr;
    }
  else if (flag_coarray == GFC_FCOARRAY_LIB)
    stat = null_pointer_node;

  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tree image_index, caf_decl, offset, token;

      caf_decl = gfc_get_tree_for_caf_expr (atom_expr);
      if (TREE_CODE (TREE_TYPE (caf_decl)) == REFERENCE_TYPE)
	caf_decl = build_fold_indirect_ref_loc (input_location, caf_decl);

      if (gfc_is_coindexed (atom_expr))
	image_index = gfc_caf_get_image_index (&block, atom_expr, caf_decl);
      else
	image_index = integer_zero_node;

      if (TREE_TYPE (TREE_TYPE (new_val)) != TREE_TYPE (TREE_TYPE (old)))
	{
	  tmp = gfc_create_var (TREE_TYPE (TREE_TYPE (old)), "new");
	  gfc_add_modify (&block, tmp, fold_convert (TREE_TYPE (tmp), new_val));
          new_val = gfc_build_addr_expr (NULL_TREE, tmp);
	}

      /* Convert a constant to a pointer.  */
      if (!POINTER_TYPE_P (TREE_TYPE (comp)))
	{
	  tmp = gfc_create_var (TREE_TYPE (TREE_TYPE (old)), "comp");
	  gfc_add_modify (&block, tmp, fold_convert (TREE_TYPE (tmp), comp));
          comp = gfc_build_addr_expr (NULL_TREE, tmp);
	}

      gfc_init_se (&argse, NULL);
      gfc_get_caf_token_offset (&argse, &token, &offset, caf_decl, atom,
				atom_expr);
      gfc_add_block_to_block (&block, &argse.pre);

      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_atomic_cas, 9,
				 token, offset, image_index, old, comp, new_val,
				 stat, build_int_cst (integer_type_node,
						      (int) atom_expr->ts.type),
				 build_int_cst (integer_type_node,
						(int) atom_expr->ts.kind));
      gfc_add_expr_to_block (&block, tmp);
      gfc_add_block_to_block (&block, &argse.post);
      gfc_add_block_to_block (&block, &post_block);
      return gfc_finish_block (&block);
    }

  tmp = TREE_TYPE (TREE_TYPE (atom));
  fn = (built_in_function) ((int) BUILT_IN_ATOMIC_COMPARE_EXCHANGE_N
			    + exact_log2 (tree_to_uhwi (TYPE_SIZE_UNIT (tmp)))
			    + 1);
  tmp = builtin_decl_explicit (fn);

  gfc_add_modify (&block, old, comp);
  tmp = build_call_expr_loc (input_location, tmp, 6, atom,
			     gfc_build_addr_expr (NULL, old),
			     fold_convert (TREE_TYPE (old), new_val),
			     boolean_false_node,
			     build_int_cst (NULL, MEMMODEL_RELAXED),
			     build_int_cst (NULL, MEMMODEL_RELAXED));
  gfc_add_expr_to_block (&block, tmp);

  if (stat != NULL_TREE)
    gfc_add_modify (&block, stat, build_int_cst (TREE_TYPE (stat), 0));
  gfc_add_block_to_block (&block, &post_block);
  return gfc_finish_block (&block);
}

static tree
conv_intrinsic_event_query (gfc_code *code)
{
  gfc_se se, argse;
  tree stat = NULL_TREE, stat2 = NULL_TREE;
  tree count = NULL_TREE, count2 = NULL_TREE;

  gfc_expr *event_expr = code->ext.actual->expr;

  if (code->ext.actual->next->next->expr)
    {
      gcc_assert (code->ext.actual->next->next->expr->expr_type
		  == EXPR_VARIABLE);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, code->ext.actual->next->next->expr);
      stat = argse.expr;
    }
  else if (flag_coarray == GFC_FCOARRAY_LIB)
    stat = null_pointer_node;

  if (code->ext.actual->next->expr)
    {
      gcc_assert (code->ext.actual->next->expr->expr_type == EXPR_VARIABLE);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, code->ext.actual->next->expr);
      count = argse.expr;
    }

  gfc_start_block (&se.pre);
  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tree tmp, token, image_index;
      tree index = build_zero_cst (gfc_array_index_type);

      if (event_expr->expr_type == EXPR_FUNCTION
	  && event_expr->value.function.isym
	  && event_expr->value.function.isym->id == GFC_ISYM_CAF_GET)
	event_expr = event_expr->value.function.actual->expr;

      tree caf_decl = gfc_get_tree_for_caf_expr (event_expr);

      if (event_expr->symtree->n.sym->ts.type != BT_DERIVED
	  || event_expr->symtree->n.sym->ts.u.derived->from_intmod
	     != INTMOD_ISO_FORTRAN_ENV
	  || event_expr->symtree->n.sym->ts.u.derived->intmod_sym_id
	     != ISOFORTRAN_EVENT_TYPE)
	{
	  gfc_error ("Sorry, the event component of derived type at %L is not "
		     "yet supported", &event_expr->where);
	  return NULL_TREE;
	}

      if (gfc_is_coindexed (event_expr))
	{
	  gfc_error ("The event variable at %L shall not be coindexed",
		     &event_expr->where);
          return NULL_TREE;
	}

      image_index = integer_zero_node;

      gfc_get_caf_token_offset (&se, &token, NULL, caf_decl, NULL_TREE,
				event_expr);

      /* For arrays, obtain the array index.  */
      if (gfc_expr_attr (event_expr).dimension)
	{
	  tree desc, tmp, extent, lbound, ubound;
          gfc_array_ref *ar, ar2;
          int i;

	  /* TODO: Extend this, once DT components are supported.  */
	  ar = &event_expr->ref->u.ar;
	  ar2 = *ar;
	  memset (ar, '\0', sizeof (*ar));
	  ar->as = ar2.as;
	  ar->type = AR_FULL;

	  gfc_init_se (&argse, NULL);
	  argse.descriptor_only = 1;
	  gfc_conv_expr_descriptor (&argse, event_expr);
	  gfc_add_block_to_block (&se.pre, &argse.pre);
	  desc = argse.expr;
	  *ar = ar2;

	  extent = build_one_cst (gfc_array_index_type);
	  for (i = 0; i < ar->dimen; i++)
	    {
	      gfc_init_se (&argse, NULL);
	      gfc_conv_expr_type (&argse, ar->start[i], gfc_array_index_type);
	      gfc_add_block_to_block (&argse.pre, &argse.pre);
	      lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[i]);
	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     TREE_TYPE (lbound), argse.expr, lbound);
	      tmp = fold_build2_loc (input_location, MULT_EXPR,
				     TREE_TYPE (tmp), extent, tmp);
	      index = fold_build2_loc (input_location, PLUS_EXPR,
				       TREE_TYPE (tmp), index, tmp);
	      if (i < ar->dimen - 1)
		{
		  ubound = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[i]);
		  tmp = gfc_conv_array_extent_dim (lbound, ubound, NULL);
		  extent = fold_build2_loc (input_location, MULT_EXPR,
					    TREE_TYPE (tmp), extent, tmp);
		}
	    }
	}

      if (count != null_pointer_node && TREE_TYPE (count) != integer_type_node)
	{
	  count2 = count;
	  count = gfc_create_var (integer_type_node, "count");
	}

      if (stat != null_pointer_node && TREE_TYPE (stat) != integer_type_node)
	{
	  stat2 = stat;
	  stat = gfc_create_var (integer_type_node, "stat");
	}

      index = fold_convert (size_type_node, index);
      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_event_query, 5,
                                   token, index, image_index, count
				   ? gfc_build_addr_expr (NULL, count) : count,
				   stat != null_pointer_node
				   ? gfc_build_addr_expr (NULL, stat) : stat);
      gfc_add_expr_to_block (&se.pre, tmp);

      if (count2 != NULL_TREE)
	gfc_add_modify (&se.pre, count2,
			fold_convert (TREE_TYPE (count2), count));

      if (stat2 != NULL_TREE)
	gfc_add_modify (&se.pre, stat2,
			fold_convert (TREE_TYPE (stat2), stat));

      return gfc_finish_block (&se.pre);
    }

  gfc_init_se (&argse, NULL);
  gfc_conv_expr_val (&argse, code->ext.actual->expr);
  gfc_add_modify (&se.pre, count, fold_convert (TREE_TYPE (count), argse.expr));

  if (stat != NULL_TREE)
    gfc_add_modify (&se.pre, stat, build_int_cst (TREE_TYPE (stat), 0));

  return gfc_finish_block (&se.pre);
}


/* This is a peculiar case because of the need to do dependency checking.
   It is called via trans-stmt.cc(gfc_trans_call), where it is picked out as
   a special case and this function called instead of
   gfc_conv_procedure_call.  */
void
gfc_conv_intrinsic_mvbits (gfc_se *se, gfc_actual_arglist *actual_args,
			   gfc_loopinfo *loop)
{
  gfc_actual_arglist *actual;
  gfc_se argse[5];
  gfc_expr *arg[5];
  gfc_ss *lss;
  int n;

  tree from, frompos, len, to, topos;
  tree lenmask, oldbits, newbits, bitsize;
  tree type, utype, above, mask1, mask2;

  if (loop)
    lss = loop->ss;
  else
    lss = gfc_ss_terminator;

  actual = actual_args;
  for (n = 0; n < 5; n++, actual = actual->next)
    {
      arg[n] = actual->expr;
      gfc_init_se (&argse[n], NULL);

      if (lss != gfc_ss_terminator)
	{
	  gfc_copy_loopinfo_to_se (&argse[n], loop);
	  /* Find the ss for the expression if it is there.  */
	  argse[n].ss = lss;
	  gfc_mark_ss_chain_used (lss, 1);
	}

      gfc_conv_expr (&argse[n], arg[n]);

      if (loop)
	lss = argse[n].ss;
    }

  from    = argse[0].expr;
  frompos = argse[1].expr;
  len     = argse[2].expr;
  to      = argse[3].expr;
  topos   = argse[4].expr;

  /* The type of the result (TO).  */
  type    = TREE_TYPE (to);
  bitsize = build_int_cst (integer_type_node, TYPE_PRECISION (type));

  /* Optionally generate code for runtime argument check.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_BITS)
    {
      tree nbits, below, ccond;
      tree fp = fold_convert (long_integer_type_node, frompos);
      tree ln = fold_convert (long_integer_type_node, len);
      tree tp = fold_convert (long_integer_type_node, topos);
      below = fold_build2_loc (input_location, LT_EXPR,
			       logical_type_node, frompos,
			       build_int_cst (TREE_TYPE (frompos), 0));
      above = fold_build2_loc (input_location, GT_EXPR,
			       logical_type_node, frompos,
			       fold_convert (TREE_TYPE (frompos), bitsize));
      ccond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
			       logical_type_node, below, above);
      gfc_trans_runtime_check (true, false, ccond, &argse[1].pre,
			       &arg[1]->where,
			       "FROMPOS argument (%ld) out of range 0:%d "
			       "in intrinsic MVBITS", fp, bitsize);
      below = fold_build2_loc (input_location, LT_EXPR,
			       logical_type_node, len,
			       build_int_cst (TREE_TYPE (len), 0));
      above = fold_build2_loc (input_location, GT_EXPR,
			       logical_type_node, len,
			       fold_convert (TREE_TYPE (len), bitsize));
      ccond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
			       logical_type_node, below, above);
      gfc_trans_runtime_check (true, false, ccond, &argse[2].pre,
			       &arg[2]->where,
			       "LEN argument (%ld) out of range 0:%d "
			       "in intrinsic MVBITS", ln, bitsize);
      below = fold_build2_loc (input_location, LT_EXPR,
			       logical_type_node, topos,
			       build_int_cst (TREE_TYPE (topos), 0));
      above = fold_build2_loc (input_location, GT_EXPR,
			       logical_type_node, topos,
			       fold_convert (TREE_TYPE (topos), bitsize));
      ccond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
			       logical_type_node, below, above);
      gfc_trans_runtime_check (true, false, ccond, &argse[4].pre,
			       &arg[4]->where,
			       "TOPOS argument (%ld) out of range 0:%d "
			       "in intrinsic MVBITS", tp, bitsize);

      /* The tests above ensure that FROMPOS, LEN and TOPOS fit into short
	 integers.  Additions below cannot overflow.  */
      nbits = fold_convert (long_integer_type_node, bitsize);
      above = fold_build2_loc (input_location, PLUS_EXPR,
			       long_integer_type_node, fp, ln);
      ccond = fold_build2_loc (input_location, GT_EXPR,
			       logical_type_node, above, nbits);
      gfc_trans_runtime_check (true, false, ccond, &argse[1].pre,
			       &arg[1]->where,
			       "FROMPOS(%ld)+LEN(%ld)>BIT_SIZE(%d) "
			       "in intrinsic MVBITS", fp, ln, bitsize);
      above = fold_build2_loc (input_location, PLUS_EXPR,
			       long_integer_type_node, tp, ln);
      ccond = fold_build2_loc (input_location, GT_EXPR,
			       logical_type_node, above, nbits);
      gfc_trans_runtime_check (true, false, ccond, &argse[4].pre,
			       &arg[4]->where,
			       "TOPOS(%ld)+LEN(%ld)>BIT_SIZE(%d) "
			       "in intrinsic MVBITS", tp, ln, bitsize);
    }

  for (n = 0; n < 5; n++)
    {
      gfc_add_block_to_block (&se->pre, &argse[n].pre);
      gfc_add_block_to_block (&se->post, &argse[n].post);
    }

  /* lenmask = (LEN >= bit_size (TYPE)) ? ~(TYPE)0 : ((TYPE)1 << LEN) - 1  */
  above = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
			   len, fold_convert (TREE_TYPE (len), bitsize));
  mask1 = build_int_cst (type, -1);
  mask2 = fold_build2_loc (input_location, LSHIFT_EXPR, type,
			   build_int_cst (type, 1), len);
  mask2 = fold_build2_loc (input_location, MINUS_EXPR, type,
			   mask2, build_int_cst (type, 1));
  lenmask = fold_build3_loc (input_location, COND_EXPR, type,
			     above, mask1, mask2);

  /* newbits = (((UTYPE)(FROM) >> FROMPOS) & lenmask) << TOPOS.
   * For valid frompos+len <= bit_size(FROM) the conversion to unsigned is
   * not strictly necessary; artificial bits from rshift will be masked.  */
  utype = unsigned_type_for (type);
  newbits = fold_build2_loc (input_location, RSHIFT_EXPR, utype,
			     fold_convert (utype, from), frompos);
  newbits = fold_build2_loc (input_location, BIT_AND_EXPR, type,
			     fold_convert (type, newbits), lenmask);
  newbits = fold_build2_loc (input_location, LSHIFT_EXPR, type,
			     newbits, topos);

  /* oldbits = TO & (~(lenmask << TOPOS)).  */
  oldbits = fold_build2_loc (input_location, LSHIFT_EXPR, type,
			     lenmask, topos);
  oldbits = fold_build1_loc (input_location, BIT_NOT_EXPR, type, oldbits);
  oldbits = fold_build2_loc (input_location, BIT_AND_EXPR, type, oldbits, to);

  /* TO = newbits | oldbits.  */
  se->expr = fold_build2_loc (input_location, BIT_IOR_EXPR, type,
			      oldbits, newbits);

  /* Return the assignment.  */
  se->expr = fold_build2_loc (input_location, MODIFY_EXPR,
			      void_type_node, to, se->expr);
}


static tree
conv_intrinsic_move_alloc (gfc_code *code)
{
  stmtblock_t block;
  gfc_expr *from_expr, *to_expr;
  gfc_se from_se, to_se;
  tree tmp, to_tree, from_tree;
  bool coarray, from_is_class, from_is_scalar;

  gfc_start_block (&block);

  from_expr = code->ext.actual->expr;
  to_expr = code->ext.actual->next->expr;

  gfc_init_se (&from_se, NULL);
  gfc_init_se (&to_se, NULL);

  gcc_assert (from_expr->ts.type != BT_CLASS || to_expr->ts.type == BT_CLASS);
  coarray = from_expr->corank != 0;

  from_is_class = from_expr->ts.type == BT_CLASS;
  from_is_scalar = from_expr->rank == 0 && !coarray;
  if (to_expr->ts.type == BT_CLASS || from_is_scalar)
    {
      from_se.want_pointer = 1;
      if (from_is_scalar)
	gfc_conv_expr (&from_se, from_expr);
      else
	gfc_conv_expr_descriptor (&from_se, from_expr);
      if (from_is_class)
	from_tree = gfc_class_data_get (from_se.expr);
      else
	{
	  gfc_symbol *vtab;
	  from_tree = from_se.expr;

	  if (to_expr->ts.type == BT_CLASS)
	    {
	      vtab = gfc_find_vtab (&from_expr->ts);
	      gcc_assert (vtab);
	      from_se.expr = gfc_get_symbol_decl (vtab);
	    }
	}
      gfc_add_block_to_block (&block, &from_se.pre);

      to_se.want_pointer = 1;
      if (to_expr->rank == 0)
	gfc_conv_expr (&to_se, to_expr);
      else
	gfc_conv_expr_descriptor (&to_se, to_expr);
      if (to_expr->ts.type == BT_CLASS)
	to_tree = gfc_class_data_get (to_se.expr);
      else
	to_tree = to_se.expr;
      gfc_add_block_to_block (&block, &to_se.pre);

      /* Deallocate "to".  */
      if (to_expr->rank == 0)
	{
	  tmp
	    = gfc_deallocate_scalar_with_status (to_tree, NULL_TREE, NULL_TREE,
						 true, to_expr, to_expr->ts);
	  gfc_add_expr_to_block (&block, tmp);
	}

      if (from_is_scalar)
	{
	  /* Assign (_data) pointers.  */
	  gfc_add_modify_loc (input_location, &block, to_tree,
			      fold_convert (TREE_TYPE (to_tree), from_tree));

	  /* Set "from" to NULL.  */
	  gfc_add_modify_loc (input_location, &block, from_tree,
			      fold_convert (TREE_TYPE (from_tree),
					    null_pointer_node));

	  gfc_add_block_to_block (&block, &from_se.post);
	}
      gfc_add_block_to_block (&block, &to_se.post);

      /* Set _vptr.  */
      if (to_expr->ts.type == BT_CLASS)
	{
	  gfc_class_set_vptr (&block, to_se.expr, from_se.expr);
	  if (from_is_class)
	    gfc_reset_vptr (&block, from_expr);
	  if (UNLIMITED_POLY (to_expr))
	    {
	      tree to_len = gfc_class_len_get (to_se.class_container);
	      tmp = from_expr->ts.type == BT_CHARACTER && from_se.string_length
		      ? from_se.string_length
		      : size_zero_node;
	      gfc_add_modify_loc (input_location, &block, to_len,
				  fold_convert (TREE_TYPE (to_len), tmp));
	    }
	}

      if (from_is_scalar)
	{
	  if (to_expr->ts.type == BT_CHARACTER && to_expr->ts.deferred)
	    {
	      gfc_add_modify_loc (input_location, &block, to_se.string_length,
				  fold_convert (TREE_TYPE (to_se.string_length),
						from_se.string_length));
	      if (from_expr->ts.deferred)
		gfc_add_modify_loc (
		  input_location, &block, from_se.string_length,
		  build_int_cst (TREE_TYPE (from_se.string_length), 0));
	    }
	  if (UNLIMITED_POLY (from_expr))
	    gfc_reset_len (&block, from_expr);

	  return gfc_finish_block (&block);
	}

      gfc_init_se (&to_se, NULL);
      gfc_init_se (&from_se, NULL);
    }

  /* Deallocate "to".  */
  if (from_expr->rank == 0)
    {
      to_se.want_coarray = 1;
      from_se.want_coarray = 1;
    }
  gfc_conv_expr_descriptor (&to_se, to_expr);
  gfc_conv_expr_descriptor (&from_se, from_expr);

  /* For coarrays, call SYNC ALL if TO is already deallocated as MOVE_ALLOC
     is an image control "statement", cf. IR F08/0040 in 12-006A.  */
  if (coarray && flag_coarray == GFC_FCOARRAY_LIB)
    {
      tree cond;

      tmp = gfc_deallocate_with_status (to_se.expr, NULL_TREE, NULL_TREE,
					NULL_TREE, NULL_TREE, true, to_expr,
					GFC_CAF_COARRAY_DEALLOCATE_ONLY);
      gfc_add_expr_to_block (&block, tmp);

      tmp = gfc_conv_descriptor_data_get (to_se.expr);
      cond = fold_build2_loc (input_location, EQ_EXPR,
			      logical_type_node, tmp,
			      fold_convert (TREE_TYPE (tmp),
					    null_pointer_node));
      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_sync_all,
				 3, null_pointer_node, null_pointer_node,
				 integer_zero_node);

      tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, cond,
			     tmp, build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);
    }
  else
    {
      if (to_expr->ts.type == BT_DERIVED
	  && to_expr->ts.u.derived->attr.alloc_comp)
	{
	  tmp = gfc_deallocate_alloc_comp (to_expr->ts.u.derived,
					   to_se.expr, to_expr->rank);
	  gfc_add_expr_to_block (&block, tmp);
	}

      tmp = gfc_deallocate_with_status (to_se.expr, NULL_TREE, NULL_TREE,
					NULL_TREE, NULL_TREE, true, to_expr,
					GFC_CAF_COARRAY_NOCOARRAY);
      gfc_add_expr_to_block (&block, tmp);
    }

  /* Copy the array descriptor data.  */
  gfc_add_modify_loc (input_location, &block, to_se.expr, from_se.expr);

  /* Set "from" to NULL.  */
  tmp = gfc_conv_descriptor_data_get (from_se.expr);
  gfc_add_modify_loc (input_location, &block, tmp,
		      fold_convert (TREE_TYPE (tmp), null_pointer_node));


  if (to_expr->ts.type == BT_CHARACTER && to_expr->ts.deferred)
    {
      gfc_add_modify_loc (input_location, &block, to_se.string_length,
			  fold_convert (TREE_TYPE (to_se.string_length),
					from_se.string_length));
      if (from_expr->ts.deferred)
        gfc_add_modify_loc (input_location, &block, from_se.string_length,
			build_int_cst (TREE_TYPE (from_se.string_length), 0));
    }

  return gfc_finish_block (&block);
}


tree
gfc_conv_intrinsic_subroutine (gfc_code *code)
{
  tree res;

  gcc_assert (code->resolved_isym);

  switch (code->resolved_isym->id)
    {
    case GFC_ISYM_MOVE_ALLOC:
      res = conv_intrinsic_move_alloc (code);
      break;

    case GFC_ISYM_ATOMIC_CAS:
      res = conv_intrinsic_atomic_cas (code);
      break;

    case GFC_ISYM_ATOMIC_ADD:
    case GFC_ISYM_ATOMIC_AND:
    case GFC_ISYM_ATOMIC_DEF:
    case GFC_ISYM_ATOMIC_OR:
    case GFC_ISYM_ATOMIC_XOR:
    case GFC_ISYM_ATOMIC_FETCH_ADD:
    case GFC_ISYM_ATOMIC_FETCH_AND:
    case GFC_ISYM_ATOMIC_FETCH_OR:
    case GFC_ISYM_ATOMIC_FETCH_XOR:
      res = conv_intrinsic_atomic_op (code);
      break;

    case GFC_ISYM_ATOMIC_REF:
      res = conv_intrinsic_atomic_ref (code);
      break;

    case GFC_ISYM_EVENT_QUERY:
      res = conv_intrinsic_event_query (code);
      break;

    case GFC_ISYM_C_F_POINTER:
    case GFC_ISYM_C_F_PROCPOINTER:
      res = conv_isocbinding_subroutine (code);
      break;

    case GFC_ISYM_CAF_SEND:
      res = conv_caf_send_to_remote (code);
      break;

    case GFC_ISYM_CAF_SENDGET:
      res = conv_caf_sendget (code);
      break;

    case GFC_ISYM_CO_BROADCAST:
    case GFC_ISYM_CO_MIN:
    case GFC_ISYM_CO_MAX:
    case GFC_ISYM_CO_REDUCE:
    case GFC_ISYM_CO_SUM:
      res = conv_co_collective (code);
      break;

    case GFC_ISYM_FREE:
      res = conv_intrinsic_free (code);
      break;

    case GFC_ISYM_RANDOM_INIT:
      res = conv_intrinsic_random_init (code);
      break;

    case GFC_ISYM_KILL:
      res = conv_intrinsic_kill_sub (code);
      break;

    case GFC_ISYM_MVBITS:
      res = NULL_TREE;
      break;

    case GFC_ISYM_SYSTEM_CLOCK:
      res = conv_intrinsic_system_clock (code);
      break;

    default:
      res = NULL_TREE;
      break;
    }

  return res;
}

#include "gt-fortran-trans-intrinsic.h"
