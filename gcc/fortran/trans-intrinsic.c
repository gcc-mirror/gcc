/* Intrinsic translation
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
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

/* trans-intrinsic.c-- generate GENERIC trees for calls to intrinsics.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include "gimple.h"
#include "flags.h"
#include "gfortran.h"
#include "arith.h"
#include "intrinsic.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"
#include "trans-array.h"
#include "defaults.h"
/* Only for gfc_trans_assign and gfc_trans_pointer_assign.  */
#include "trans-stmt.h"

/* This maps fortran intrinsic math functions to external library or GCC
   builtin functions.  */
typedef struct GTY(()) gfc_intrinsic_map_t {
  /* The explicit enum is required to work around inadequacies in the
     garbage collection/gengtype parsing mechanism.  */
  enum gfc_isym_id id;

  /* Enum value from the "language-independent", aka C-centric, part
     of gcc, or END_BUILTINS of no such value set.  */
  enum built_in_function code_r4;
  enum built_in_function code_r8;
  enum built_in_function code_r10;
  enum built_in_function code_r16;
  enum built_in_function code_c4;
  enum built_in_function code_c8;
  enum built_in_function code_c10;
  enum built_in_function code_c16;

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
    BUILT_IN_ ## ID ## L, BUILT_IN_ ## ID ## L, (enum built_in_function) 0, \
    (enum built_in_function) 0, (enum built_in_function) 0, \
    (enum built_in_function) 0, true, false, true, NAME, NULL_TREE, \
    NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, \
    NULL_TREE},

#define DEFINE_MATH_BUILTIN_C(ID, NAME, ARGTYPE) \
  { GFC_ISYM_ ## ID, BUILT_IN_ ## ID ## F, BUILT_IN_ ## ID, \
    BUILT_IN_ ## ID ## L, BUILT_IN_ ## ID ## L, BUILT_IN_C ## ID ## F, \
    BUILT_IN_C ## ID, BUILT_IN_C ## ID ## L, BUILT_IN_C ## ID ## L, true, \
    true, true, NAME, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, \
    NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE},

#define LIB_FUNCTION(ID, NAME, HAVE_COMPLEX) \
  { GFC_ISYM_ ## ID, END_BUILTINS, END_BUILTINS, END_BUILTINS, END_BUILTINS, \
    END_BUILTINS, END_BUILTINS, END_BUILTINS, END_BUILTINS, \
    false, HAVE_COMPLEX, true, NAME, NULL_TREE, NULL_TREE, NULL_TREE, \
    NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE }

static GTY(()) gfc_intrinsic_map_t gfc_intrinsic_map[] =
{
  /* Functions built into gcc itself.  */
#include "mathbuiltins.def"

  /* Functions in libgfortran.  */
  LIB_FUNCTION (ERFC_SCALED, "erfc_scaled", false),

  /* End the list.  */
  LIB_FUNCTION (NONE, NULL, false)

};
#undef LIB_FUNCTION
#undef DEFINE_MATH_BUILTIN
#undef DEFINE_MATH_BUILTIN_C

/* Structure for storing components of a floating number to be used by
   elemental functions to manipulate reals.  */
typedef struct
{
  tree arg;     /* Variable tree to view convert to integer.  */
  tree expn;    /* Variable tree to save exponent.  */
  tree frac;    /* Variable tree to save fraction.  */
  tree smask;   /* Constant tree of sign's mask.  */
  tree emask;   /* Constant tree of exponent's mask.  */
  tree fmask;   /* Constant tree of fraction's mask.  */
  tree edigits; /* Constant tree of the number of exponent bits.  */
  tree fdigits; /* Constant tree of the number of fraction bits.  */
  tree f1;      /* Constant tree of the f1 defined in the real model.  */
  tree bias;    /* Constant tree of the bias of exponent in the memory.  */
  tree type;    /* Type tree of arg1.  */
  tree mtype;   /* Type tree of integer type. Kind is that of arg1.  */
}
real_compnt_info;

enum rounding_mode { RND_ROUND, RND_TRUNC, RND_CEIL, RND_FLOOR };

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
  args = (tree *) alloca (sizeof (tree) * nargs);

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
      args[0] = fold_build1 (REALPART_EXPR, artype, args[0]);
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
  cond = fold_build2 (up ? GE_EXPR : LE_EXPR, boolean_type_node, tmp, arg);

  tmp = fold_build2 (up ? PLUS_EXPR : MINUS_EXPR, type, intval,
		     build_int_cst (type, 1));
  tmp = fold_build3 (COND_EXPR, type, cond, intval, tmp);
  return tmp;
}


/* Round to nearest integer, away from zero.  */

static tree
build_round_expr (tree arg, tree restype)
{
  tree argtype;
  tree fn;
  bool longlong;
  int argprec, resprec;

  argtype = TREE_TYPE (arg);
  argprec = TYPE_PRECISION (argtype);
  resprec = TYPE_PRECISION (restype);

  /* Depending on the type of the result, choose the long int intrinsic
     (lround family) or long long intrinsic (llround).  We might also
     need to convert the result afterwards.  */
  if (resprec <= LONG_TYPE_SIZE)
    longlong = false;
  else if (resprec <= LONG_LONG_TYPE_SIZE)
    longlong = true;
  else
    gcc_unreachable ();

  /* Now, depending on the argument type, we choose between intrinsics.  */
  if (argprec == TYPE_PRECISION (float_type_node))
    fn = built_in_decls[longlong ? BUILT_IN_LLROUNDF : BUILT_IN_LROUNDF];
  else if (argprec == TYPE_PRECISION (double_type_node))
    fn = built_in_decls[longlong ? BUILT_IN_LLROUND : BUILT_IN_LROUND];
  else if (argprec == TYPE_PRECISION (long_double_type_node))
    fn = built_in_decls[longlong ? BUILT_IN_LLROUNDL : BUILT_IN_LROUNDL];
  else
    gcc_unreachable ();

  return fold_convert (restype, build_call_expr_loc (input_location,
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
      break;

    case RND_CEIL:
      return build_fixbound_expr (pblock, arg, type, 1);
      break;

    case RND_ROUND:
      return build_round_expr (arg, type);
      break;

    case RND_TRUNC:
      return fold_build1 (FIX_TRUNC_EXPR, type, arg);
      break;

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
  mpfr_t huge;
  int n, nargs;
  int kind;

  kind = expr->ts.kind;
  nargs =  gfc_intrinsic_argument_list_length (expr);

  n = END_BUILTINS;
  /* We have builtin functions for some cases.  */
  switch (op)
    {
    case RND_ROUND:
      switch (kind)
	{
	case 4:
	  n = BUILT_IN_ROUNDF;
	  break;

	case 8:
	  n = BUILT_IN_ROUND;
	  break;

	case 10:
	case 16:
	  n = BUILT_IN_ROUNDL;
	  break;
	}
      break;

    case RND_TRUNC:
      switch (kind)
	{
	case 4:
	  n = BUILT_IN_TRUNCF;
	  break;

	case 8:
	  n = BUILT_IN_TRUNC;
	  break;

	case 10:
	case 16:
	  n = BUILT_IN_TRUNCL;
	  break;
	}
      break;

    default:
      gcc_unreachable ();
    }

  /* Evaluate the argument.  */
  gcc_assert (expr->value.function.actual->expr);
  gfc_conv_intrinsic_function_args (se, expr, arg, nargs);

  /* Use a builtin function if one exists.  */
  if (n != END_BUILTINS)
    {
      tmp = built_in_decls[n];
      se->expr = build_call_expr_loc (input_location,
				  tmp, 1, arg[0]);
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
  cond = fold_build2 (LT_EXPR, boolean_type_node, arg[0], tmp);

  mpfr_neg (huge, huge, GFC_RND_MODE);
  tmp = gfc_conv_mpfr_to_tree (huge, kind, 0);
  tmp = fold_build2 (GT_EXPR, boolean_type_node, arg[0], tmp);
  cond = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, cond, tmp);
  itype = gfc_get_int_type (kind);

  tmp = build_fix_expr (&se->pre, arg[0], itype, op);
  tmp = convert (type, tmp);
  se->expr = fold_build3 (COND_EXPR, type, cond, tmp, arg[0]);
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
  args = (tree *) alloca (sizeof (tree) * nargs);

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
	  args[0] = fold_build1 (REALPART_EXPR, artype, args[0]);
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
  se->expr = fold_build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (arg)), arg);
}


/* Get the complex conjugate of a value.  */

static void
gfc_conv_intrinsic_conjg (gfc_se * se, gfc_expr * expr)
{
  tree arg;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  se->expr = fold_build1 (CONJ_EXPR, TREE_TYPE (arg), arg);
}


/* Initialize function decls for library functions.  The external functions
   are created as required.  Builtin functions are added here.  */

void
gfc_build_intrinsic_lib_fndecls (void)
{
  gfc_intrinsic_map_t *m;

  /* Add GCC builtin functions.  */
  for (m = gfc_intrinsic_map; m->id != GFC_ISYM_NONE; m++)
    {
      if (m->code_r4 != END_BUILTINS)
	m->real4_decl = built_in_decls[m->code_r4];
      if (m->code_r8 != END_BUILTINS)
	m->real8_decl = built_in_decls[m->code_r8];
      if (m->code_r10 != END_BUILTINS)
	m->real10_decl = built_in_decls[m->code_r10];
      if (m->code_r16 != END_BUILTINS)
	m->real16_decl = built_in_decls[m->code_r16];
      if (m->code_c4 != END_BUILTINS)
	m->complex4_decl = built_in_decls[m->code_c4];
      if (m->code_c8 != END_BUILTINS)
	m->complex8_decl = built_in_decls[m->code_c8];
      if (m->code_c10 != END_BUILTINS)
	m->complex10_decl = built_in_decls[m->code_c10];
      if (m->code_c16 != END_BUILTINS)
	m->complex16_decl = built_in_decls[m->code_c16];
    }
}


/* Create a fndecl for a simple intrinsic library function.  */

static tree
gfc_get_intrinsic_lib_fndecl (gfc_intrinsic_map_t * m, gfc_expr * expr)
{
  tree type;
  tree argtypes;
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
      if (ts->kind == 4)
	snprintf (name, sizeof (name), "%s%s%s",
		ts->type == BT_COMPLEX ? "c" : "", m->name, "f");
      else if (ts->kind == 8)
	snprintf (name, sizeof (name), "%s%s",
		ts->type == BT_COMPLEX ? "c" : "", m->name);
      else
	{
	  gcc_assert (ts->kind == 10 || ts->kind == 16);
	  snprintf (name, sizeof (name), "%s%s%s",
		ts->type == BT_COMPLEX ? "c" : "", m->name, "l");
	}
    }
  else
    {
      snprintf (name, sizeof (name), PREFIX ("%s_%c%d"), m->name,
		ts->type == BT_COMPLEX ? 'c' : 'r',
		ts->kind);
    }

  argtypes = NULL_TREE;
  for (actual = expr->value.function.actual; actual; actual = actual->next)
    {
      type = gfc_typenode_for_spec (&actual->expr->ts);
      argtypes = gfc_chainon_list (argtypes, type);
    }
  argtypes = gfc_chainon_list (argtypes, void_type_node);
  type = build_function_type (gfc_typenode_for_spec (ts), argtypes);
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
  for (m = gfc_intrinsic_map; m->id != GFC_ISYM_NONE; m++)
    {
      if (id == m->id)
	break;
    }

  if (m->id == GFC_ISYM_NONE)
    {
      internal_error ("Intrinsic function %s(%d) not recognized",
		      expr->value.function.name, id);
    }

  /* Get the decl and generate the call.  */
  num_args = gfc_intrinsic_argument_list_length (expr);
  args = (tree *) alloca (sizeof (tree) * num_args);

  gfc_conv_intrinsic_function_args (se, expr, args, num_args);
  fndecl = gfc_get_intrinsic_lib_fndecl (m, expr);
  rettype = TREE_TYPE (TREE_TYPE (fndecl));

  fndecl = build_addr (fndecl, current_function_decl);
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
  cond = fold_build2 (NE_EXPR, boolean_type_node, a, b);

  /* Output the runtime-check.  */
  name = gfc_build_cstring_const (intr_name);
  name = gfc_build_addr_expr (pchar_type_node, name);
  gfc_trans_runtime_check (true, false, cond, target, where,
			   "Unequal character lengths (%ld/%ld) in %s",
			   fold_convert (long_integer_type_node, a),
			   fold_convert (long_integer_type_node, b), name);
}


/* The EXPONENT(s) intrinsic function is translated into
       int ret;
       frexp (s, &ret);
       return ret;
 */

static void
gfc_conv_intrinsic_exponent (gfc_se *se, gfc_expr *expr)
{
  tree arg, type, res, tmp;
  int frexp;

  switch (expr->value.function.actual->expr->ts.kind)
    {
    case 4:
      frexp = BUILT_IN_FREXPF;
      break;
    case 8:
      frexp = BUILT_IN_FREXP;
      break;
    case 10:
    case 16:
      frexp = BUILT_IN_FREXPL;
      break;
    default:
      gcc_unreachable ();
    }

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);

  res = gfc_create_var (integer_type_node, NULL);
  tmp = build_call_expr_loc (input_location,
			 built_in_decls[frexp], 2, arg,
			 gfc_build_addr_expr (NULL_TREE, res));
  gfc_add_expr_to_block (&se->pre, tmp);

  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = fold_convert (type, res);
}

/* Evaluate a single upper or lower bound.  */
/* TODO: bound intrinsic generates way too much unnecessary code.  */

static void
gfc_conv_intrinsic_bound (gfc_se * se, gfc_expr * expr, int upper)
{
  gfc_actual_arglist *arg;
  gfc_actual_arglist *arg2;
  tree desc;
  tree type;
  tree bound;
  tree tmp;
  tree cond, cond1, cond2, cond3, cond4, size;
  tree ubound;
  tree lbound;
  gfc_se argse;
  gfc_ss *ss;
  gfc_array_spec * as;
  gfc_ref *ref;

  arg = expr->value.function.actual;
  arg2 = arg->next;

  if (se->ss)
    {
      /* Create an implicit second parameter from the loop variable.  */
      gcc_assert (!arg2->expr);
      gcc_assert (se->loop->dimen == 1);
      gcc_assert (se->ss->expr == expr);
      gfc_advance_se_ss_chain (se);
      bound = se->loop->loopvar[0];
      bound = fold_build2 (MINUS_EXPR, gfc_array_index_type, bound,
			   se->loop->from[0]);
    }
  else
    {
      /* use the passed argument.  */
      gcc_assert (arg->next->expr);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_type (&argse, arg->next->expr, gfc_array_index_type);
      gfc_add_block_to_block (&se->pre, &argse.pre);
      bound = argse.expr;
      /* Convert from one based to zero based.  */
      bound = fold_build2 (MINUS_EXPR, gfc_array_index_type, bound,
			   gfc_index_one_node);
    }

  /* TODO: don't re-evaluate the descriptor on each iteration.  */
  /* Get a descriptor for the first parameter.  */
  ss = gfc_walk_expr (arg->expr);
  gcc_assert (ss != gfc_ss_terminator);
  gfc_init_se (&argse, NULL);
  gfc_conv_expr_descriptor (&argse, arg->expr, ss);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);

  desc = argse.expr;

  if (INTEGER_CST_P (bound))
    {
      int hi, low;

      hi = TREE_INT_CST_HIGH (bound);
      low = TREE_INT_CST_LOW (bound);
      if (hi || low < 0 || low >= GFC_TYPE_ARRAY_RANK (TREE_TYPE (desc)))
	gfc_error ("'dim' argument of %s intrinsic at %L is not a valid "
		   "dimension index", upper ? "UBOUND" : "LBOUND",
		   &expr->where);
    }
  else
    {
      if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
        {
          bound = gfc_evaluate_now (bound, &se->pre);
          cond = fold_build2 (LT_EXPR, boolean_type_node,
			      bound, build_int_cst (TREE_TYPE (bound), 0));
          tmp = gfc_rank_cst[GFC_TYPE_ARRAY_RANK (TREE_TYPE (desc))];
          tmp = fold_build2 (GE_EXPR, boolean_type_node, bound, tmp);
          cond = fold_build2 (TRUTH_ORIF_EXPR, boolean_type_node, cond, tmp);
          gfc_trans_runtime_check (true, false, cond, &se->pre, &expr->where,
				   gfc_msg_fault);
        }
    }

  ubound = gfc_conv_descriptor_ubound_get (desc, bound);
  lbound = gfc_conv_descriptor_lbound_get (desc, bound);
  
  /* Follow any component references.  */
  if (arg->expr->expr_type == EXPR_VARIABLE
      || arg->expr->expr_type == EXPR_CONSTANT)
    {
      as = arg->expr->symtree->n.sym->as;
      for (ref = arg->expr->ref; ref; ref = ref->next)
	{
	  switch (ref->type)
	    {
	    case REF_COMPONENT:
	      as = ref->u.c.component->as;
	      continue;

	    case REF_SUBSTRING:
	      continue;

	    case REF_ARRAY:
	      {
		switch (ref->u.ar.type)
		  {
		  case AR_ELEMENT:
		  case AR_SECTION:
		  case AR_UNKNOWN:
		    as = NULL;
		    continue;

		  case AR_FULL:
		    break;
		  }
		break;
	      }
	    }
	}
    }
  else
    as = NULL;

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

  if (as)
    {
      tree stride = gfc_conv_descriptor_stride_get (desc, bound);

      cond1 = fold_build2 (GE_EXPR, boolean_type_node, ubound, lbound);
      cond2 = fold_build2 (LE_EXPR, boolean_type_node, ubound, lbound);

      cond3 = fold_build2 (GE_EXPR, boolean_type_node, stride,
			   gfc_index_zero_node);
      cond3 = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, cond3, cond1);

      cond4 = fold_build2 (LT_EXPR, boolean_type_node, stride,
			   gfc_index_zero_node);

      if (upper)
	{
	  tree cond5;
	  cond = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, cond3, cond4);

	  cond5 = fold_build2 (EQ_EXPR, boolean_type_node, gfc_index_one_node, lbound);
	  cond5 = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, cond4, cond5);

	  cond = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, cond, cond5);

	  se->expr = fold_build3 (COND_EXPR, gfc_array_index_type, cond,
				  ubound, gfc_index_zero_node);
	}
      else
	{
	  if (as->type == AS_ASSUMED_SIZE)
	    cond = fold_build2 (EQ_EXPR, boolean_type_node, bound,
				build_int_cst (TREE_TYPE (bound),
					       arg->expr->rank - 1));
	  else
	    cond = boolean_false_node;

	  cond1 = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, cond3, cond4);
	  cond = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, cond, cond1);

	  se->expr = fold_build3 (COND_EXPR, gfc_array_index_type, cond,
				  lbound, gfc_index_one_node);
	}
    }
  else
    {
      if (upper)
        {
	  size = fold_build2 (MINUS_EXPR, gfc_array_index_type, ubound, lbound);
	  se->expr = fold_build2 (PLUS_EXPR, gfc_array_index_type, size,
				  gfc_index_one_node);
	  se->expr = fold_build2 (MAX_EXPR, gfc_array_index_type, se->expr,
				  gfc_index_zero_node);
	}
      else
	se->expr = gfc_index_one_node;
    }

  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = convert (type, se->expr);
}


static void
gfc_conv_intrinsic_abs (gfc_se * se, gfc_expr * expr)
{
  tree arg;
  int n;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);

  switch (expr->value.function.actual->expr->ts.type)
    {
    case BT_INTEGER:
    case BT_REAL:
      se->expr = fold_build1 (ABS_EXPR, TREE_TYPE (arg), arg);
      break;

    case BT_COMPLEX:
      switch (expr->ts.kind)
	{
	case 4:
	  n = BUILT_IN_CABSF;
	  break;
	case 8:
	  n = BUILT_IN_CABS;
	  break;
	case 10:
	case 16:
	  n = BUILT_IN_CABSL;
	  break;
	default:
	  gcc_unreachable ();
	}
      se->expr = build_call_expr_loc (input_location,
				  built_in_decls[n], 1, arg);
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
  args = (tree *) alloca (sizeof (tree) * num_args);

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, args, num_args);
  real = convert (TREE_TYPE (type), args[0]);
  if (both)
    imag = convert (TREE_TYPE (type), args[1]);
  else if (TREE_CODE (TREE_TYPE (args[0])) == COMPLEX_TYPE)
    {
      imag = fold_build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (args[0])),
			  args[0]);
      imag = convert (TREE_TYPE (type), imag);
    }
  else
    imag = build_real_from_int_cst (TREE_TYPE (type), integer_zero_node);

  se->expr = fold_build2 (COMPLEX_EXPR, type, real, imag);
}

/* Remainder function MOD(A, P) = A - INT(A / P) * P
                      MODULO(A, P) = A - FLOOR (A / P) * P  */
/* TODO: MOD(x, 0)  */

static void
gfc_conv_intrinsic_mod (gfc_se * se, gfc_expr * expr, int modulo)
{
  tree type;
  tree itype;
  tree tmp;
  tree test;
  tree test2;
  mpfr_t huge;
  int n, ikind;
  tree args[2];

  gfc_conv_intrinsic_function_args (se, expr, args, 2);

  switch (expr->ts.type)
    {
    case BT_INTEGER:
      /* Integer case is easy, we've got a builtin op.  */
      type = TREE_TYPE (args[0]);

      if (modulo)
       se->expr = fold_build2 (FLOOR_MOD_EXPR, type, args[0], args[1]);
      else
       se->expr = fold_build2 (TRUNC_MOD_EXPR, type, args[0], args[1]);
      break;

    case BT_REAL:
      n = END_BUILTINS;
      /* Check if we have a builtin fmod.  */
      switch (expr->ts.kind)
	{
	case 4:
	  n = BUILT_IN_FMODF;
	  break;

	case 8:
	  n = BUILT_IN_FMOD;
	  break;

	case 10:
	case 16:
	  n = BUILT_IN_FMODL;
	  break;

	default:
	  break;
	}

      /* Use it if it exists.  */
      if (n != END_BUILTINS)
	{
  	  tmp = build_addr (built_in_decls[n], current_function_decl);
	  se->expr = build_call_array_loc (input_location,
				       TREE_TYPE (TREE_TYPE (built_in_decls[n])),
                                       tmp, 2, args);
	  if (modulo == 0)
	    return;
	}

      type = TREE_TYPE (args[0]);

      args[0] = gfc_evaluate_now (args[0], &se->pre);
      args[1] = gfc_evaluate_now (args[1], &se->pre);

      /* Definition:
	 modulo = arg - floor (arg/arg2) * arg2, so
		= test ? fmod (arg, arg2) : fmod (arg, arg2) + arg2, 
	 where
	  test  = (fmod (arg, arg2) != 0) && ((arg < 0) xor (arg2 < 0))
	 thereby avoiding another division and retaining the accuracy
	 of the builtin function.  */
      if (n != END_BUILTINS && modulo)
	{
	  tree zero = gfc_build_const (type, integer_zero_node);
	  tmp = gfc_evaluate_now (se->expr, &se->pre);
	  test = fold_build2 (LT_EXPR, boolean_type_node, args[0], zero);
	  test2 = fold_build2 (LT_EXPR, boolean_type_node, args[1], zero);
	  test2 = fold_build2 (TRUTH_XOR_EXPR, boolean_type_node, test, test2);
	  test = fold_build2 (NE_EXPR, boolean_type_node, tmp, zero);
	  test = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, test, test2);
	  test = gfc_evaluate_now (test, &se->pre);
	  se->expr = fold_build3 (COND_EXPR, type, test,
				  fold_build2 (PLUS_EXPR, type, tmp, args[1]),
				  tmp);
	  return;
	}

      /* If we do not have a built_in fmod, the calculation is going to
	 have to be done longhand.  */
      tmp = fold_build2 (RDIV_EXPR, type, args[0], args[1]);

      /* Test if the value is too large to handle sensibly.  */
      gfc_set_model_kind (expr->ts.kind);
      mpfr_init (huge);
      n = gfc_validate_kind (BT_INTEGER, expr->ts.kind, true);
      ikind = expr->ts.kind;
      if (n < 0)
	{
	  n = gfc_validate_kind (BT_INTEGER, gfc_max_integer_kind, false);
	  ikind = gfc_max_integer_kind;
	}
      mpfr_set_z (huge, gfc_integer_kinds[n].huge, GFC_RND_MODE);
      test = gfc_conv_mpfr_to_tree (huge, expr->ts.kind, 0);
      test2 = fold_build2 (LT_EXPR, boolean_type_node, tmp, test);

      mpfr_neg (huge, huge, GFC_RND_MODE);
      test = gfc_conv_mpfr_to_tree (huge, expr->ts.kind, 0);
      test = fold_build2 (GT_EXPR, boolean_type_node, tmp, test);
      test2 = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, test, test2);

      itype = gfc_get_int_type (ikind);
      if (modulo)
       tmp = build_fix_expr (&se->pre, tmp, itype, RND_FLOOR);
      else
       tmp = build_fix_expr (&se->pre, tmp, itype, RND_TRUNC);
      tmp = convert (type, tmp);
      tmp = fold_build3 (COND_EXPR, type, test2, tmp, args[0]);
      tmp = fold_build2 (MULT_EXPR, type, tmp, args[1]);
      se->expr = fold_build2 (MINUS_EXPR, type, args[0], tmp);
      mpfr_clear (huge);
      break;

    default:
      gcc_unreachable ();
    }
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

  val = fold_build2 (MINUS_EXPR, type, args[0], args[1]);
  val = gfc_evaluate_now (val, &se->pre);

  zero = gfc_build_const (type, integer_zero_node);
  tmp = fold_build2 (LE_EXPR, boolean_type_node, val, zero);
  se->expr = fold_build3 (COND_EXPR, type, tmp, zero, val);
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

      switch (expr->ts.kind)
	{
	case 4:
	  tmp = built_in_decls[BUILT_IN_COPYSIGNF];
	  abs = built_in_decls[BUILT_IN_FABSF];
	  break;
	case 8:
	  tmp = built_in_decls[BUILT_IN_COPYSIGN];
	  abs = built_in_decls[BUILT_IN_FABS];
	  break;
	case 10:
	case 16:
	  tmp = built_in_decls[BUILT_IN_COPYSIGNL];
	  abs = built_in_decls[BUILT_IN_FABSL];
	  break;
	default:
	  gcc_unreachable ();
	}

      /* We explicitly have to ignore the minus sign. We do so by using
	 result = (arg1 == 0) ? abs(arg0) : copysign(arg0, arg1).  */
      if (!gfc_option.flag_sign_zero
	  && MODE_HAS_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (args[1]))))
	{
	  tree cond, zero;
	  zero = build_real_from_int_cst (TREE_TYPE (args[1]), integer_zero_node);
	  cond = fold_build2 (EQ_EXPR, boolean_type_node, args[1], zero);
	  se->expr = fold_build3 (COND_EXPR, TREE_TYPE (args[0]), cond,
				  build_call_expr (abs, 1, args[0]),
				  build_call_expr (tmp, 2, args[0], args[1]));
	}
      else
        se->expr = build_call_expr_loc (input_location,
				  tmp, 2, args[0], args[1]);
      return;
    }

  /* Having excluded floating point types, we know we are now dealing
     with signed integer types.  */
  type = TREE_TYPE (args[0]);

  /* Args[0] is used multiple times below.  */
  args[0] = gfc_evaluate_now (args[0], &se->pre);

  /* Construct (A ^ B) >> 31, which generates a bit mask of all zeros if
     the signs of A and B are the same, and of all ones if they differ.  */
  tmp = fold_build2 (BIT_XOR_EXPR, type, args[0], args[1]);
  tmp = fold_build2 (RSHIFT_EXPR, type, tmp,
		     build_int_cst (type, TYPE_PRECISION (type) - 1));
  tmp = gfc_evaluate_now (tmp, &se->pre);

  /* Construct (A + tmp) ^ tmp, which is A if tmp is zero, and -A if tmp]
     is all ones (i.e. -1).  */
  se->expr = fold_build2 (BIT_XOR_EXPR, type,
			  fold_build2 (PLUS_EXPR, type, args[0], tmp),
			  tmp);
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
  se->expr = fold_build2 (MULT_EXPR, type, args[0], args[1]);
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

  arg[0] = fold_build1 (NOP_EXPR, type, arg[0]);
  gfc_add_modify (&se->pre, var, arg[0]);
  se->expr = gfc_build_addr_expr (build_pointer_type (type), var);
  se->string_length = integer_one_node;
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
  args = (tree *) alloca (sizeof (tree) * num_args);

  var = gfc_create_var (pchar_type_node, "pstr");
  len = gfc_create_var (gfc_get_int_type (8), "len");

  gfc_conv_intrinsic_function_args (se, expr, &args[2], num_args - 2);
  args[0] = gfc_build_addr_expr (NULL_TREE, var);
  args[1] = gfc_build_addr_expr (NULL_TREE, len);

  fndecl = build_addr (gfor_fndecl_ctime, current_function_decl);
  tmp = build_call_array_loc (input_location,
			  TREE_TYPE (TREE_TYPE (gfor_fndecl_ctime)),
			  fndecl, num_args, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2 (GT_EXPR, boolean_type_node,
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
  args = (tree *) alloca (sizeof (tree) * num_args);

  var = gfc_create_var (pchar_type_node, "pstr");
  len = gfc_create_var (gfc_get_int_type (4), "len");

  gfc_conv_intrinsic_function_args (se, expr, &args[2], num_args - 2);
  args[0] = gfc_build_addr_expr (NULL_TREE, var);
  args[1] = gfc_build_addr_expr (NULL_TREE, len);

  fndecl = build_addr (gfor_fndecl_fdate, current_function_decl);
  tmp = build_call_array_loc (input_location,
			  TREE_TYPE (TREE_TYPE (gfor_fndecl_fdate)),
			  fndecl, num_args, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2 (GT_EXPR, boolean_type_node,
		      len, build_int_cst (TREE_TYPE (len), 0));
  tmp = gfc_call_free (var);
  tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&se->post, tmp);

  se->expr = var;
  se->string_length = len;
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
  args = (tree *) alloca (sizeof (tree) * num_args);

  var = gfc_create_var (pchar_type_node, "pstr");
  len = gfc_create_var (gfc_get_int_type (4), "len");

  gfc_conv_intrinsic_function_args (se, expr, &args[2], num_args - 2);
  args[0] = gfc_build_addr_expr (NULL_TREE, var);
  args[1] = gfc_build_addr_expr (NULL_TREE, len);

  fndecl = build_addr (gfor_fndecl_ttynam, current_function_decl);
  tmp = build_call_array_loc (input_location,
			  TREE_TYPE (TREE_TYPE (gfor_fndecl_ttynam)),
			  fndecl, num_args, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2 (GT_EXPR, boolean_type_node,
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
      if (a2 .op. mvar || isnan(mvar))
        mvar = a2;
      if (a3 .op. mvar || isnan(mvar))
        mvar = a3;
      ...
      return mvar
    }
 */

/* TODO: Mismatching types can occur when specific names are used.
   These should be handled during resolution.  */
static void
gfc_conv_intrinsic_minmax (gfc_se * se, gfc_expr * expr, enum tree_code op)
{
  tree tmp;
  tree mvar;
  tree val;
  tree thencase;
  tree *args;
  tree type;
  gfc_actual_arglist *argexpr;
  unsigned int i, nargs;

  nargs = gfc_intrinsic_argument_list_length (expr);
  args = (tree *) alloca (sizeof (tree) * nargs);

  gfc_conv_intrinsic_function_args (se, expr, args, nargs);
  type = gfc_typenode_for_spec (&expr->ts);

  argexpr = expr->value.function.actual;
  if (TREE_TYPE (args[0]) != type)
    args[0] = convert (type, args[0]);
  /* Only evaluate the argument once.  */
  if (TREE_CODE (args[0]) != VAR_DECL && !TREE_CONSTANT (args[0]))
    args[0] = gfc_evaluate_now (args[0], &se->pre);

  mvar = gfc_create_var (type, "M");
  gfc_add_modify (&se->pre, mvar, args[0]);
  for (i = 1, argexpr = argexpr->next; i < nargs; i++)
    {
      tree cond, isnan;

      val = args[i]; 

      /* Handle absent optional arguments by ignoring the comparison.  */
      if (argexpr->expr->expr_type == EXPR_VARIABLE
	  && argexpr->expr->symtree->n.sym->attr.optional
	  && TREE_CODE (val) == INDIRECT_REF)
	cond = fold_build2_loc (input_location,
				NE_EXPR, boolean_type_node,
				TREE_OPERAND (val, 0), 
			build_int_cst (TREE_TYPE (TREE_OPERAND (val, 0)), 0));
      else
      {
	cond = NULL_TREE;

	/* Only evaluate the argument once.  */
	if (TREE_CODE (val) != VAR_DECL && !TREE_CONSTANT (val))
	  val = gfc_evaluate_now (val, &se->pre);
      }

      thencase = build2_v (MODIFY_EXPR, mvar, convert (type, val));

      tmp = fold_build2 (op, boolean_type_node, convert (type, val), mvar);

      /* FIXME: When the IEEE_ARITHMETIC module is implemented, the call to
	 __builtin_isnan might be made dependent on that module being loaded,
	 to help performance of programs that don't rely on IEEE semantics.  */
      if (FLOAT_TYPE_P (TREE_TYPE (mvar)))
	{
	  isnan = build_call_expr_loc (input_location,
				   built_in_decls[BUILT_IN_ISNAN], 1, mvar);
	  tmp = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, tmp,
			     fold_convert (boolean_type_node, isnan));
	}
      tmp = build3_v (COND_EXPR, tmp, thencase,
		      build_empty_stmt (input_location));

      if (cond != NULL_TREE)
	tmp = build3_v (COND_EXPR, cond, tmp,
			build_empty_stmt (input_location));

      gfc_add_expr_to_block (&se->pre, tmp);
      argexpr = argexpr->next;
    }
  se->expr = mvar;
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
  args = (tree *) alloca (sizeof (tree) * (nargs + 4));
  gfc_conv_intrinsic_function_args (se, expr, &args[4], nargs);

  /* Create the result variables.  */
  len = gfc_create_var (gfc_charlen_type_node, "len");
  args[0] = gfc_build_addr_expr (NULL_TREE, len);
  var = gfc_create_var (gfc_get_pchar_type (expr->ts.kind), "pstr");
  args[1] = gfc_build_addr_expr (ppvoid_type_node, var);
  args[2] = build_int_cst (NULL_TREE, op);
  args[3] = build_int_cst (NULL_TREE, nargs / 2);

  if (expr->ts.kind == 1)
    function = gfor_fndecl_string_minmax;
  else if (expr->ts.kind == 4)
    function = gfor_fndecl_string_minmax_char4;
  else
    gcc_unreachable ();

  /* Make the function call.  */
  fndecl = build_addr (function, current_function_decl);
  tmp = build_call_array_loc (input_location,
			  TREE_TYPE (TREE_TYPE (function)), fndecl,
			  nargs + 4, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2 (GT_EXPR, boolean_type_node,
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
gfc_get_symbol_for_expr (gfc_expr * expr)
{
  gfc_symbol *sym;

  /* TODO: Add symbols for intrinsic function to the global namespace.  */
  gcc_assert (strlen (expr->value.function.name) <= GFC_MAX_SYMBOL_LEN - 5);
  sym = gfc_new_symbol (expr->value.function.name, NULL);

  sym->ts = expr->ts;
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

  /* TODO: proper argument lists for external intrinsics.  */
  return sym;
}

/* Generate a call to an external intrinsic function.  */
static void
gfc_conv_intrinsic_funcall (gfc_se * se, gfc_expr * expr)
{
  gfc_symbol *sym;
  tree append_args;

  gcc_assert (!se->ss || se->ss->expr == expr);

  if (se->ss)
    gcc_assert (expr->rank > 0);
  else
    gcc_assert (expr->rank == 0);

  sym = gfc_get_symbol_for_expr (expr);

  /* Calls to libgfortran_matmul need to be appended special arguments,
     to be able to call the BLAS ?gemm functions if required and possible.  */
  append_args = NULL_TREE;
  if (expr->value.function.isym->id == GFC_ISYM_MATMUL
      && sym->ts.type != BT_LOGICAL)
    {
      tree cint = gfc_get_int_type (gfc_c_int_kind);

      if (gfc_option.flag_external_blas
	  && (sym->ts.type == BT_REAL || sym->ts.type == BT_COMPLEX)
	  && (sym->ts.kind == gfc_default_real_kind
	      || sym->ts.kind == gfc_default_double_kind))
	{
	  tree gemm_fndecl;

	  if (sym->ts.type == BT_REAL)
	    {
	      if (sym->ts.kind == gfc_default_real_kind)
		gemm_fndecl = gfor_fndecl_sgemm;
	      else
		gemm_fndecl = gfor_fndecl_dgemm;
	    }
	  else
	    {
	      if (sym->ts.kind == gfc_default_real_kind)
		gemm_fndecl = gfor_fndecl_cgemm;
	      else
		gemm_fndecl = gfor_fndecl_zgemm;
	    }

	  append_args = gfc_chainon_list (NULL_TREE, build_int_cst (cint, 1));
	  append_args = gfc_chainon_list
			  (append_args, build_int_cst
					  (cint, gfc_option.blas_matmul_limit));
	  append_args = gfc_chainon_list (append_args,
					  gfc_build_addr_expr (NULL_TREE,
							       gemm_fndecl));
	}
      else
	{
	  append_args = gfc_chainon_list (NULL_TREE, build_int_cst (cint, 0));
	  append_args = gfc_chainon_list (append_args, build_int_cst (cint, 0));
	  append_args = gfc_chainon_list (append_args, null_pointer_node);
	}
    }

  gfc_conv_procedure_call (se, sym, expr->value.function.actual, expr,
			  append_args);
  gfc_free (sym);
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
  tmp = fold_build2 (op, boolean_type_node, arrayse.expr,
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

  tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (resvar),
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

/* Inline implementation of the sum and product intrinsics.  */
static void
gfc_conv_intrinsic_arith (gfc_se * se, gfc_expr * expr, enum tree_code op)
{
  tree resvar;
  tree type;
  stmtblock_t body;
  stmtblock_t block;
  tree tmp;
  gfc_loopinfo loop;
  gfc_actual_arglist *actual;
  gfc_ss *arrayss;
  gfc_ss *maskss;
  gfc_se arrayse;
  gfc_se maskse;
  gfc_expr *arrayexpr;
  gfc_expr *maskexpr;

  if (se->ss)
    {
      gfc_conv_intrinsic_funcall (se, expr);
      return;
    }

  type = gfc_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  resvar = gfc_create_var (type, "val");
  if (op == PLUS_EXPR)
    tmp = gfc_build_const (type, integer_zero_node);
  else
    tmp = gfc_build_const (type, integer_one_node);

  gfc_add_modify (&se->pre, resvar, tmp);

  /* Walk the arguments.  */
  actual = expr->value.function.actual;
  arrayexpr = actual->expr;
  arrayss = gfc_walk_expr (arrayexpr);
  gcc_assert (arrayss != gfc_ss_terminator);

  actual = actual->next->next;
  gcc_assert (actual);
  maskexpr = actual->expr;
  if (maskexpr && maskexpr->rank != 0)
    {
      maskss = gfc_walk_expr (maskexpr);
      gcc_assert (maskss != gfc_ss_terminator);
    }
  else
    maskss = NULL;

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);
  gfc_add_ss_to_loop (&loop, arrayss);
  if (maskss)
    gfc_add_ss_to_loop (&loop, maskss);

  /* Initialize the loop.  */
  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop, &expr->where);

  gfc_mark_ss_chain_used (arrayss, 1);
  if (maskss)
    gfc_mark_ss_chain_used (maskss, 1);
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

  /* Do the actual summation/product.  */
  gfc_init_se (&arrayse, NULL);
  gfc_copy_loopinfo_to_se (&arrayse, &loop);
  arrayse.ss = arrayss;
  gfc_conv_expr_val (&arrayse, arrayexpr);
  gfc_add_block_to_block (&block, &arrayse.pre);

  tmp = fold_build2 (op, type, resvar, arrayse.expr);
  gfc_add_modify (&block, resvar, tmp);
  gfc_add_block_to_block (&block, &arrayse.post);

  if (maskss)
    {
      /* We enclose the above in if (mask) {...} .  */
      tmp = gfc_finish_block (&block);

      tmp = build3_v (COND_EXPR, maskse.expr, tmp,
		      build_empty_stmt (input_location));
    }
  else
    tmp = gfc_finish_block (&block);
  gfc_add_expr_to_block (&body, tmp);

  gfc_trans_scalarizing_loops (&loop, &body);

  /* For a scalar mask, enclose the loop in an if statement.  */
  if (maskexpr && maskss == NULL)
    {
      gfc_init_se (&maskse, NULL);
      gfc_conv_expr_val (&maskse, maskexpr);
      gfc_init_block (&block);
      gfc_add_block_to_block (&block, &loop.pre);
      gfc_add_block_to_block (&block, &loop.post);
      tmp = gfc_finish_block (&block);

      tmp = build3_v (COND_EXPR, maskse.expr, tmp,
		      build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);
      gfc_add_block_to_block (&se->pre, &block);
    }
  else
    {
      gfc_add_block_to_block (&se->pre, &loop.pre);
      gfc_add_block_to_block (&se->pre, &loop.post);
    }

  gfc_cleanup_loop (&loop);

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
    arrayse1.expr = fold_build1 (CONJ_EXPR, type, arrayse1.expr);
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
      tmp = fold_build2 (TRUTH_AND_EXPR, type, arrayse1.expr, arrayse2.expr);
      tmp = fold_build2 (TRUTH_OR_EXPR, type, resvar, tmp);
    }
  else
    {
      tmp = fold_build2 (MULT_EXPR, type, arrayse1.expr, arrayse2.expr);
      tmp = fold_build2 (PLUS_EXPR, type, resvar, tmp);
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


/* Emit code for minloc or maxloc intrinsic.  There are many different cases
   we need to handle.  For performance reasons we sometimes create two
   loops instead of one, where the second one is much simpler.
   Examples for minloc intrinsic:
   1) Result is an array, a call is generated
   2) Array mask is used and NaNs need to be supported:
      limit = Infinity;
      pos = 0;
      S = from;
      while (S <= to) {
	if (mask[S]) {
	  if (pos == 0) pos = S + (1 - from);
	  if (a[S] <= limit) { limit = a[S]; pos = S + (1 - from); goto lab1; }
	}
	S++;
      }
      goto lab2;
      lab1:;
      while (S <= to) {
	if (mask[S]) if (a[S] < limit) { limit = a[S]; pos = S + (1 - from); }
	S++;
      }
      lab2:;
   3) NaNs need to be supported, but it is known at compile time or cheaply
      at runtime whether array is nonempty or not:
      limit = Infinity;
      pos = 0;
      S = from;
      while (S <= to) {
	if (a[S] <= limit) { limit = a[S]; pos = S + (1 - from); goto lab1; }
	S++;
      }
      if (from <= to) pos = 1;
      goto lab2;
      lab1:;
      while (S <= to) {
	if (a[S] < limit) { limit = a[S]; pos = S + (1 - from); }
	S++;
      }
      lab2:;
   4) NaNs aren't supported, array mask is used:
      limit = infinities_supported ? Infinity : huge (limit);
      pos = 0;
      S = from;
      while (S <= to) {
	if (mask[S]) { limit = a[S]; pos = S + (1 - from); goto lab1; }
	S++;
      }
      goto lab2;
      lab1:;
      while (S <= to) {
	if (mask[S]) if (a[S] < limit) { limit = a[S]; pos = S + (1 - from); }
	S++;
      }
      lab2:;
   5) Same without array mask:
      limit = infinities_supported ? Infinity : huge (limit);
      pos = (from <= to) ? 1 : 0;
      S = from;
      while (S <= to) {
	if (a[S] < limit) { limit = a[S]; pos = S + (1 - from); }
	S++;
      }
   For 3) and 5), if mask is scalar, this all goes into a conditional,
   setting pos = 0; in the else branch.  */

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
  tree offset;
  tree nonempty;
  tree lab1, lab2;
  gfc_loopinfo loop;
  gfc_actual_arglist *actual;
  gfc_ss *arrayss;
  gfc_ss *maskss;
  gfc_se arrayse;
  gfc_se maskse;
  gfc_expr *arrayexpr;
  gfc_expr *maskexpr;
  tree pos;
  int n;

  if (se->ss)
    {
      gfc_conv_intrinsic_funcall (se, expr);
      return;
    }

  /* Initialize the result.  */
  pos = gfc_create_var (gfc_array_index_type, "pos");
  offset = gfc_create_var (gfc_array_index_type, "offset");
  type = gfc_typenode_for_spec (&expr->ts);

  /* Walk the arguments.  */
  actual = expr->value.function.actual;
  arrayexpr = actual->expr;
  arrayss = gfc_walk_expr (arrayexpr);
  gcc_assert (arrayss != gfc_ss_terminator);

  actual = actual->next->next;
  gcc_assert (actual);
  maskexpr = actual->expr;
  nonempty = NULL;
  if (maskexpr && maskexpr->rank != 0)
    {
      maskss = gfc_walk_expr (maskexpr);
      gcc_assert (maskss != gfc_ss_terminator);
    }
  else
    {
      mpz_t asize;
      if (gfc_array_size (arrayexpr, &asize) == SUCCESS)
	{
	  nonempty = gfc_conv_mpz_to_tree (asize, gfc_index_integer_kind);
	  mpz_clear (asize);
	  nonempty = fold_build2 (GT_EXPR, boolean_type_node, nonempty,
				  gfc_index_zero_node);
	}
      maskss = NULL;
    }

  limit = gfc_create_var (gfc_typenode_for_spec (&arrayexpr->ts), "limit");
  n = gfc_validate_kind (arrayexpr->ts.type, arrayexpr->ts.kind, false);
  switch (arrayexpr->ts.type)
    {
    case BT_REAL:
      if (HONOR_INFINITIES (DECL_MODE (limit)))
	{
	  REAL_VALUE_TYPE real;
	  real_inf (&real);
	  tmp = build_real (TREE_TYPE (limit), real);
	}
      else
	tmp = gfc_conv_mpfr_to_tree (gfc_real_kinds[n].huge,
				     arrayexpr->ts.kind, 0);
      break;

    case BT_INTEGER:
      tmp = gfc_conv_mpz_to_tree (gfc_integer_kinds[n].huge,
				  arrayexpr->ts.kind);
      break;

    default:
      gcc_unreachable ();
    }

  /* We start with the most negative possible value for MAXLOC, and the most
     positive possible value for MINLOC. The most negative possible value is
     -HUGE for BT_REAL and (-HUGE - 1) for BT_INTEGER; the most positive
     possible value is HUGE in both cases.  */
  if (op == GT_EXPR)
    tmp = fold_build1 (NEGATE_EXPR, TREE_TYPE (tmp), tmp);
  if (op == GT_EXPR && expr->ts.type == BT_INTEGER)
    tmp = fold_build2 (MINUS_EXPR, TREE_TYPE (tmp), tmp,
		       build_int_cst (type, 1));

  gfc_add_modify (&se->pre, limit, tmp);

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);
  gfc_add_ss_to_loop (&loop, arrayss);
  if (maskss)
    gfc_add_ss_to_loop (&loop, maskss);

  /* Initialize the loop.  */
  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop, &expr->where);

  gcc_assert (loop.dimen == 1);
  if (nonempty == NULL && maskss == NULL && loop.from[0] && loop.to[0])
    nonempty = fold_build2 (LE_EXPR, boolean_type_node, loop.from[0],
			    loop.to[0]);

  lab1 = NULL;
  lab2 = NULL;
  /* Initialize the position to zero, following Fortran 2003.  We are free
     to do this because Fortran 95 allows the result of an entirely false
     mask to be processor dependent.  If we know at compile time the array
     is non-empty and no MASK is used, we can initialize to 1 to simplify
     the inner loop.  */
  if (nonempty != NULL && !HONOR_NANS (DECL_MODE (limit)))
    gfc_add_modify (&loop.pre, pos,
		    fold_build3 (COND_EXPR, gfc_array_index_type,
				 nonempty, gfc_index_one_node,
				 gfc_index_zero_node));
  else
    {
      gfc_add_modify (&loop.pre, pos, gfc_index_zero_node);
      lab1 = gfc_build_label_decl (NULL_TREE);
      TREE_USED (lab1) = 1;
      lab2 = gfc_build_label_decl (NULL_TREE);
      TREE_USED (lab2) = 1;
    }

  gfc_mark_ss_chain_used (arrayss, 1);
  if (maskss)
    gfc_mark_ss_chain_used (maskss, 1);
  /* Generate the loop body.  */
  gfc_start_scalarized_body (&loop, &body);

  /* If we have a mask, only check this element if the mask is set.  */
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
  gfc_add_block_to_block (&block, &arrayse.pre);

  /* We do the following if this is a more extreme value.  */
  gfc_start_block (&ifblock);

  /* Assign the value to the limit...  */
  gfc_add_modify (&ifblock, limit, arrayse.expr);

  /* Remember where we are.  An offset must be added to the loop
     counter to obtain the required position.  */
  if (loop.from[0])
    tmp = fold_build2 (MINUS_EXPR, gfc_array_index_type,
		       gfc_index_one_node, loop.from[0]);
  else
    tmp = gfc_index_one_node;

  gfc_add_modify (&block, offset, tmp);

  if (nonempty == NULL && HONOR_NANS (DECL_MODE (limit)))
    {
      stmtblock_t ifblock2;
      tree ifbody2;

      gfc_start_block (&ifblock2);
      tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (pos),
			 loop.loopvar[0], offset);
      gfc_add_modify (&ifblock2, pos, tmp);
      ifbody2 = gfc_finish_block (&ifblock2);
      cond = fold_build2 (EQ_EXPR, boolean_type_node, pos,
			  gfc_index_zero_node);
      tmp = build3_v (COND_EXPR, cond, ifbody2,
		      build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);
    }

  tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (pos),
		     loop.loopvar[0], offset);
  gfc_add_modify (&ifblock, pos, tmp);

  if (lab1)
    gfc_add_expr_to_block (&ifblock, build1_v (GOTO_EXPR, lab1));

  ifbody = gfc_finish_block (&ifblock);

  if (!lab1 || HONOR_NANS (DECL_MODE (limit)))
    {
      if (lab1)
	cond = fold_build2 (op == GT_EXPR ? GE_EXPR : LE_EXPR,
			    boolean_type_node, arrayse.expr, limit);
      else
	cond = fold_build2 (op, boolean_type_node, arrayse.expr, limit);

      ifbody = build3_v (COND_EXPR, cond, ifbody,
			 build_empty_stmt (input_location));
    }
  gfc_add_expr_to_block (&block, ifbody);

  if (maskss)
    {
      /* We enclose the above in if (mask) {...}.  */
      tmp = gfc_finish_block (&block);

      tmp = build3_v (COND_EXPR, maskse.expr, tmp,
		      build_empty_stmt (input_location));
    }
  else
    tmp = gfc_finish_block (&block);
  gfc_add_expr_to_block (&body, tmp);

  if (lab1)
    {
      gfc_trans_scalarized_loop_end (&loop, 0, &body);

      if (HONOR_NANS (DECL_MODE (limit)))
	{
	  if (nonempty != NULL)
	    {
	      ifbody = build2_v (MODIFY_EXPR, pos, gfc_index_one_node);
	      tmp = build3_v (COND_EXPR, nonempty, ifbody,
			      build_empty_stmt (input_location));
	      gfc_add_expr_to_block (&loop.code[0], tmp);
	    }
	}

      gfc_add_expr_to_block (&loop.code[0], build1_v (GOTO_EXPR, lab2));
      gfc_add_expr_to_block (&loop.code[0], build1_v (LABEL_EXPR, lab1));
      gfc_start_block (&body);

      /* If we have a mask, only check this element if the mask is set.  */
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
      gfc_add_block_to_block (&block, &arrayse.pre);

      /* We do the following if this is a more extreme value.  */
      gfc_start_block (&ifblock);

      /* Assign the value to the limit...  */
      gfc_add_modify (&ifblock, limit, arrayse.expr);

      /* Remember where we are.  An offset must be added to the loop
	 counter to obtain the required position.  */
      if (loop.from[0])
	tmp = fold_build2 (MINUS_EXPR, gfc_array_index_type,
			   gfc_index_one_node, loop.from[0]);
      else
	tmp = gfc_index_one_node;

      gfc_add_modify (&block, offset, tmp);

      tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (pos),
			 loop.loopvar[0], offset);
      gfc_add_modify (&ifblock, pos, tmp);

      ifbody = gfc_finish_block (&ifblock);

      cond = fold_build2 (op, boolean_type_node, arrayse.expr, limit);

      tmp = build3_v (COND_EXPR, cond, ifbody,
		      build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);

      if (maskss)
	{
	  /* We enclose the above in if (mask) {...}.  */
	  tmp = gfc_finish_block (&block);

	  tmp = build3_v (COND_EXPR, maskse.expr, tmp,
			  build_empty_stmt (input_location));
	}
      else
	tmp = gfc_finish_block (&block);
      gfc_add_expr_to_block (&body, tmp);
      /* Avoid initializing loopvar[0] again, it should be left where
	 it finished by the first loop.  */
      loop.from[0] = loop.loopvar[0];
    }

  gfc_trans_scalarizing_loops (&loop, &body);

  if (lab2)
    gfc_add_expr_to_block (&loop.pre, build1_v (LABEL_EXPR, lab2));

  /* For a scalar mask, enclose the loop in an if statement.  */
  if (maskexpr && maskss == NULL)
    {
      gfc_init_se (&maskse, NULL);
      gfc_conv_expr_val (&maskse, maskexpr);
      gfc_init_block (&block);
      gfc_add_block_to_block (&block, &loop.pre);
      gfc_add_block_to_block (&block, &loop.post);
      tmp = gfc_finish_block (&block);

      /* For the else part of the scalar mask, just initialize
	 the pos variable the same way as above.  */

      gfc_init_block (&elseblock);
      gfc_add_modify (&elseblock, pos, gfc_index_zero_node);
      elsetmp = gfc_finish_block (&elseblock);

      tmp = build3_v (COND_EXPR, maskse.expr, tmp, elsetmp);
      gfc_add_expr_to_block (&block, tmp);
      gfc_add_block_to_block (&se->pre, &block);
    }
  else
    {
      gfc_add_block_to_block (&se->pre, &loop.pre);
      gfc_add_block_to_block (&se->pre, &loop.post);
    }
  gfc_cleanup_loop (&loop);

  se->expr = convert (type, pos);
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
	if (mask[S]) { nonempty = true; if (a[S] <= limit) goto lab; }
	S++;
      }
      limit = nonempty ? NaN : huge (limit);
      lab:
      while (S <= to) { if(mask[S]) limit = min (a[S], limit); S++; }
   3) NaNs need to be supported, but it is known at compile time or cheaply
      at runtime whether array is nonempty or not, rank 1:
      limit = Infinity;
      S = from;
      while (S <= to) { if (a[S] <= limit) goto lab; S++; }
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

  if (se->ss)
    {
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
	{
	  REAL_VALUE_TYPE real;
	  real_nan (&real, "", 1, DECL_MODE (limit));
	  nan_cst = build_real (type, real);
	}
      break;

    case BT_INTEGER:
      tmp = gfc_conv_mpz_to_tree (gfc_integer_kinds[n].huge, expr->ts.kind);
      break;

    default:
      gcc_unreachable ();
    }

  /* We start with the most negative possible value for MAXVAL, and the most
     positive possible value for MINVAL. The most negative possible value is
     -HUGE for BT_REAL and (-HUGE - 1) for BT_INTEGER; the most positive
     possible value is HUGE in both cases.  */
  if (op == GT_EXPR)
    {
      tmp = fold_build1 (NEGATE_EXPR, TREE_TYPE (tmp), tmp);
      if (huge_cst)
	huge_cst = fold_build1 (NEGATE_EXPR, TREE_TYPE (huge_cst), huge_cst);
    }

  if (op == GT_EXPR && expr->ts.type == BT_INTEGER)
    tmp = fold_build2 (MINUS_EXPR, TREE_TYPE (tmp),
		       tmp, build_int_cst (type, 1));

  gfc_add_modify (&se->pre, limit, tmp);

  /* Walk the arguments.  */
  actual = expr->value.function.actual;
  arrayexpr = actual->expr;
  arrayss = gfc_walk_expr (arrayexpr);
  gcc_assert (arrayss != gfc_ss_terminator);

  actual = actual->next->next;
  gcc_assert (actual);
  maskexpr = actual->expr;
  nonempty = NULL;
  if (maskexpr && maskexpr->rank != 0)
    {
      maskss = gfc_walk_expr (maskexpr);
      gcc_assert (maskss != gfc_ss_terminator);
    }
  else
    {
      mpz_t asize;
      if (gfc_array_size (arrayexpr, &asize) == SUCCESS)
	{
	  nonempty = gfc_conv_mpz_to_tree (asize, gfc_index_integer_kind);
	  mpz_clear (asize);
	  nonempty = fold_build2 (GT_EXPR, boolean_type_node, nonempty,
				  gfc_index_zero_node);
	}
      maskss = NULL;
    }

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);
  gfc_add_ss_to_loop (&loop, arrayss);
  if (maskss)
    gfc_add_ss_to_loop (&loop, maskss);

  /* Initialize the loop.  */
  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop, &expr->where);

  if (nonempty == NULL && maskss == NULL
      && loop.dimen == 1 && loop.from[0] && loop.to[0])
    nonempty = fold_build2 (LE_EXPR, boolean_type_node, loop.from[0],
			    loop.to[0]);
  nonempty_var = NULL;
  if (nonempty == NULL
      && (HONOR_INFINITIES (DECL_MODE (limit))
	  || HONOR_NANS (DECL_MODE (limit))))
    {
      nonempty_var = gfc_create_var (boolean_type_node, "nonempty");
      gfc_add_modify (&se->pre, nonempty_var, boolean_false_node);
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
	  fast = gfc_create_var (boolean_type_node, "fast");
	  gfc_add_modify (&se->pre, fast, boolean_false_node);
	}
    }

  gfc_mark_ss_chain_used (arrayss, 1);
  if (maskss)
    gfc_mark_ss_chain_used (maskss, 1);
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
  gfc_add_block_to_block (&block, &arrayse.pre);

  gfc_init_block (&block2);

  if (nonempty_var)
    gfc_add_modify (&block2, nonempty_var, boolean_true_node);

  if (HONOR_NANS (DECL_MODE (limit)))
    {
      tmp = fold_build2 (op == GT_EXPR ? GE_EXPR : LE_EXPR,
			 boolean_type_node, arrayse.expr, limit);
      if (lab)
	ifbody = build1_v (GOTO_EXPR, lab);
      else
	{
	  stmtblock_t ifblock;

	  gfc_init_block (&ifblock);
	  gfc_add_modify (&ifblock, limit, arrayse.expr);
	  gfc_add_modify (&ifblock, fast, boolean_true_node);
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
      if (HONOR_SIGNED_ZEROS (DECL_MODE (limit)))
	{
	  tmp = fold_build2 (op, boolean_type_node, arrayse.expr, limit);
	  ifbody = build2_v (MODIFY_EXPR, limit, arrayse.expr);
	  tmp = build3_v (COND_EXPR, tmp, ifbody,
			  build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&block2, tmp);
	}
      else
	{
	  tmp = fold_build2 (op == GT_EXPR ? MAX_EXPR : MIN_EXPR,
			     type, arrayse.expr, limit);
	  gfc_add_modify (&block2, limit, tmp);
	}
    }

  if (fast)
    {
      tree elsebody = gfc_finish_block (&block2);

      /* MIN_EXPR/MAX_EXPR has unspecified behavior with NaNs or
	 signed zeros.  */
      if (HONOR_NANS (DECL_MODE (limit))
	  || HONOR_SIGNED_ZEROS (DECL_MODE (limit)))
	{
	  tmp = fold_build2 (op, boolean_type_node, arrayse.expr, limit);
	  ifbody = build2_v (MODIFY_EXPR, limit, arrayse.expr);
	  ifbody = build3_v (COND_EXPR, tmp, ifbody,
			     build_empty_stmt (input_location));
	}
      else
	{
	  tmp = fold_build2 (op == GT_EXPR ? MAX_EXPR : MIN_EXPR,
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
    /* We enclose the above in if (mask) {...}.  */
    tmp = build3_v (COND_EXPR, maskse.expr, tmp,
		    build_empty_stmt (input_location));
  gfc_add_expr_to_block (&body, tmp);

  if (lab)
    {
      gfc_trans_scalarized_loop_end (&loop, 0, &body);

      tmp = fold_build3 (COND_EXPR, type, nonempty, nan_cst, huge_cst);
      gfc_add_modify (&loop.code[0], limit, tmp);
      gfc_add_expr_to_block (&loop.code[0], build1_v (LABEL_EXPR, lab));

      gfc_start_block (&body);

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
      gfc_add_block_to_block (&block, &arrayse.pre);

      /* MIN_EXPR/MAX_EXPR has unspecified behavior with NaNs or
	 signed zeros.  */
      if (HONOR_NANS (DECL_MODE (limit))
	  || HONOR_SIGNED_ZEROS (DECL_MODE (limit)))
	{
	  tmp = fold_build2 (op, boolean_type_node, arrayse.expr, limit);
	  ifbody = build2_v (MODIFY_EXPR, limit, arrayse.expr);
	  tmp = build3_v (COND_EXPR, tmp, ifbody,
			  build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&block, tmp);
	}
      else
	{
	  tmp = fold_build2 (op == GT_EXPR ? MAX_EXPR : MIN_EXPR,
			     type, arrayse.expr, limit);
	  gfc_add_modify (&block, limit, tmp);
	}

      gfc_add_block_to_block (&block, &arrayse.post);

      tmp = gfc_finish_block (&block);
      if (maskss)
	/* We enclose the above in if (mask) {...}.  */
	tmp = build3_v (COND_EXPR, maskse.expr, tmp,
			build_empty_stmt (input_location));
      gfc_add_expr_to_block (&body, tmp);
      /* Avoid initializing loopvar[0] again, it should be left where
	 it finished by the first loop.  */
      loop.from[0] = loop.loopvar[0];
    }
  gfc_trans_scalarizing_loops (&loop, &body);

  if (fast)
    {
      tmp = fold_build3 (COND_EXPR, type, nonempty, nan_cst, huge_cst);
      ifbody = build2_v (MODIFY_EXPR, limit, tmp);
      tmp = build3_v (COND_EXPR, fast, build_empty_stmt (input_location),
		      ifbody);
      gfc_add_expr_to_block (&loop.pre, tmp);
    }
  else if (HONOR_INFINITIES (DECL_MODE (limit)) && !lab)
    {
      tmp = fold_build3 (COND_EXPR, type, nonempty, limit, huge_cst);
      gfc_add_modify (&loop.pre, limit, tmp);
    }

  /* For a scalar mask, enclose the loop in an if statement.  */
  if (maskexpr && maskss == NULL)
    {
      tree else_stmt;

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
      tmp = build3_v (COND_EXPR, maskse.expr, tmp, else_stmt);
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

  tmp = fold_build2 (LSHIFT_EXPR, type, build_int_cst (type, 1), args[1]);
  tmp = fold_build2 (BIT_AND_EXPR, type, args[0], tmp);
  tmp = fold_build2 (NE_EXPR, boolean_type_node, tmp,
		     build_int_cst (type, 0));
  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = convert (type, tmp);
}

/* Generate code to perform the specified operation.  */
static void
gfc_conv_intrinsic_bitop (gfc_se * se, gfc_expr * expr, enum tree_code op)
{
  tree args[2];

  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  se->expr = fold_build2 (op, TREE_TYPE (args[0]), args[0], args[1]);
}

/* Bitwise not.  */
static void
gfc_conv_intrinsic_not (gfc_se * se, gfc_expr * expr)
{
  tree arg;

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  se->expr = fold_build1 (BIT_NOT_EXPR, TREE_TYPE (arg), arg);
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

  tmp = fold_build2 (LSHIFT_EXPR, type, build_int_cst (type, 1), args[1]);
  if (set)
    op = BIT_IOR_EXPR;
  else
    {
      op = BIT_AND_EXPR;
      tmp = fold_build1 (BIT_NOT_EXPR, type, tmp);
    }
  se->expr = fold_build2 (op, type, args[0], tmp);
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

  gfc_conv_intrinsic_function_args (se, expr, args, 3);
  type = TREE_TYPE (args[0]);

  mask = build_int_cst (type, -1);
  mask = fold_build2 (LSHIFT_EXPR, type, mask, args[2]);
  mask = fold_build1 (BIT_NOT_EXPR, type, mask);

  tmp = fold_build2 (RSHIFT_EXPR, type, args[0], args[1]);

  se->expr = fold_build2 (BIT_AND_EXPR, type, tmp, mask);
}

/* RSHIFT (I, SHIFT) = I >> SHIFT
   LSHIFT (I, SHIFT) = I << SHIFT  */
static void
gfc_conv_intrinsic_rlshift (gfc_se * se, gfc_expr * expr, int right_shift)
{
  tree args[2];

  gfc_conv_intrinsic_function_args (se, expr, args, 2);

  se->expr = fold_build2 (right_shift ? RSHIFT_EXPR : LSHIFT_EXPR,
			  TREE_TYPE (args[0]), args[0], args[1]);
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
  type = TREE_TYPE (args[0]);
  utype = unsigned_type_for (type);

  width = fold_build1 (ABS_EXPR, TREE_TYPE (args[1]), args[1]);

  /* Left shift if positive.  */
  lshift = fold_build2 (LSHIFT_EXPR, type, args[0], width);

  /* Right shift if negative.
     We convert to an unsigned type because we want a logical shift.
     The standard doesn't define the case of shifting negative
     numbers, and we try to be compatible with other compilers, most
     notably g77, here.  */
  rshift = fold_convert (type, fold_build2 (RSHIFT_EXPR, utype, 
					    convert (utype, args[0]), width));

  tmp = fold_build2 (GE_EXPR, boolean_type_node, args[1],
		     build_int_cst (TREE_TYPE (args[1]), 0));
  tmp = fold_build3 (COND_EXPR, type, tmp, lshift, rshift);

  /* The Fortran standard allows shift widths <= BIT_SIZE(I), whereas
     gcc requires a shift width < BIT_SIZE(I), so we have to catch this
     special case.  */
  num_bits = build_int_cst (TREE_TYPE (args[1]), TYPE_PRECISION (type));
  cond = fold_build2 (GE_EXPR, boolean_type_node, width, num_bits);

  se->expr = fold_build3 (COND_EXPR, type, cond,
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
  unsigned int num_args;

  num_args = gfc_intrinsic_argument_list_length (expr);
  args = (tree *) alloca (sizeof (tree) * num_args);

  gfc_conv_intrinsic_function_args (se, expr, args, num_args);

  if (num_args == 3)
    {
      /* Use a library function for the 3 parameter version.  */
      tree int4type = gfc_get_int_type (4);

      type = TREE_TYPE (args[0]);
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
  type = TREE_TYPE (args[0]);

  /* Rotate left if positive.  */
  lrot = fold_build2 (LROTATE_EXPR, type, args[0], args[1]);

  /* Rotate right if negative.  */
  tmp = fold_build1 (NEGATE_EXPR, TREE_TYPE (args[1]), args[1]);
  rrot = fold_build2 (RROTATE_EXPR, type, args[0], tmp);

  zero = build_int_cst (TREE_TYPE (args[1]), 0);
  tmp = fold_build2 (GT_EXPR, boolean_type_node, args[1], zero);
  rrot = fold_build3 (COND_EXPR, type, tmp, lrot, rrot);

  /* Do nothing if shift == 0.  */
  tmp = fold_build2 (EQ_EXPR, boolean_type_node, args[1], zero);
  se->expr = fold_build3 (COND_EXPR, type, tmp, args[0], rrot);
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
      func = built_in_decls[BUILT_IN_CLZ];
    }
  else if (argsize <= LONG_TYPE_SIZE)
    {
      arg_type = long_unsigned_type_node;
      func = built_in_decls[BUILT_IN_CLZL];
    }
  else if (argsize <= LONG_LONG_TYPE_SIZE)
    {
      arg_type = long_long_unsigned_type_node;
      func = built_in_decls[BUILT_IN_CLZLL];
    }
  else
    {
      gcc_assert (argsize == 128);
      arg_type = gfc_build_uint_type (argsize);
      func = gfor_fndecl_clz128;
    }

  /* Convert the actual argument twice: first, to the unsigned type of the
     same size; then, to the proper argument type for the built-in
     function.  But the return type is of the default INTEGER kind.  */
  arg = fold_convert (gfc_build_uint_type (argsize), arg);
  arg = fold_convert (arg_type, arg);
  result_type = gfc_get_int_type (gfc_default_integer_kind);

  /* Compute LEADZ for the case i .ne. 0.  */
  s = TYPE_PRECISION (arg_type) - argsize;
  tmp = fold_convert (result_type, build_call_expr (func, 1, arg));
  leadz = fold_build2 (MINUS_EXPR, result_type,
		       tmp, build_int_cst (result_type, s));

  /* Build BIT_SIZE.  */
  bit_size = build_int_cst (result_type, argsize);

  cond = fold_build2 (EQ_EXPR, boolean_type_node,
		      arg, build_int_cst (arg_type, 0));
  se->expr = fold_build3 (COND_EXPR, result_type, cond, bit_size, leadz);
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
      func = built_in_decls[BUILT_IN_CTZ];
    }
  else if (argsize <= LONG_TYPE_SIZE)
    {
      arg_type = long_unsigned_type_node;
      func = built_in_decls[BUILT_IN_CTZL];
    }
  else if (argsize <= LONG_LONG_TYPE_SIZE)
    {
      arg_type = long_long_unsigned_type_node;
      func = built_in_decls[BUILT_IN_CTZLL];
    }
  else
    {
      gcc_assert (argsize == 128);
      arg_type = gfc_build_uint_type (argsize);
      func = gfor_fndecl_ctz128;
    }

  /* Convert the actual argument twice: first, to the unsigned type of the
     same size; then, to the proper argument type for the built-in
     function.  But the return type is of the default INTEGER kind.  */
  arg = fold_convert (gfc_build_uint_type (argsize), arg);
  arg = fold_convert (arg_type, arg);
  result_type = gfc_get_int_type (gfc_default_integer_kind);

  /* Compute TRAILZ for the case i .ne. 0.  */
  trailz = fold_convert (result_type, build_call_expr_loc (input_location,
						       func, 1, arg));

  /* Build BIT_SIZE.  */
  bit_size = build_int_cst (result_type, argsize);

  cond = fold_build2 (EQ_EXPR, boolean_type_node,
		      arg, build_int_cst (arg_type, 0));
  se->expr = fold_build3 (COND_EXPR, result_type, cond, bit_size, trailz);
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
  tree append_args;

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
  append_args = NULL_TREE;
  if (prim_arg->expr->ts.type == BT_CHARACTER && !opt_arg->expr)
    {
      tree dummy;

      dummy = build_int_cst (gfc_charlen_type_node, 0);
      append_args = gfc_chainon_list (append_args, dummy);
    }

  /* Build the call itself.  */
  sym = gfc_get_symbol_for_expr (expr);
  gfc_conv_procedure_call (se, sym, expr->value.function.actual, expr,
			  append_args);
  gfc_free (sym);
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
  gfc_ss *ss;

  gcc_assert (!se->ss);

  arg = expr->value.function.actual->expr;

  type = gfc_typenode_for_spec (&expr->ts);
  switch (arg->expr_type)
    {
    case EXPR_CONSTANT:
      len = build_int_cst (NULL_TREE, arg->value.character.length);
      break;

    case EXPR_ARRAY:
      /* Obtain the string length from the function used by
         trans-array.c(gfc_trans_array_constructor).  */
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

      /* Otherwise fall through.  */

    default:
      /* Anybody stupid enough to do this deserves inefficient code.  */
      ss = gfc_walk_expr (arg);
      gfc_init_se (&argse, se);
      if (ss == gfc_ss_terminator)
	gfc_conv_expr (&argse, arg);
      else
	gfc_conv_expr_descriptor (&argse, arg, ss);
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

  args = (tree *) alloca (sizeof (tree) * 5);

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

  fndecl = build_addr (function, current_function_decl);
  se->expr = build_call_array_loc (input_location,
			       TREE_TYPE (TREE_TYPE (function)), fndecl,
			       5, args);
  se->expr = convert (type, se->expr);

}

/* The ascii value for a single character.  */
static void
gfc_conv_intrinsic_ichar (gfc_se * se, gfc_expr * expr)
{
  tree args[2], type, pchartype;

  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (args[1])));
  pchartype = gfc_get_pchar_type (expr->value.function.actual->expr->ts.kind);
  args[1] = fold_build1 (NOP_EXPR, pchartype, args[1]);
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
			      built_in_decls[BUILT_IN_ISNAN], 1, arg);
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
  se->expr = fold_build2 (EQ_EXPR, gfc_typenode_for_spec (&expr->ts),
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
  args = (tree *) alloca (sizeof (tree) * num_args);

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
  type = TREE_TYPE (tsource);
  se->expr = fold_build3 (COND_EXPR, type, mask, tsource,
			  fold_convert (type, fsource));
}


/* FRACTION (s) is translated into frexp (s, &dummy_int).  */
static void
gfc_conv_intrinsic_fraction (gfc_se * se, gfc_expr * expr)
{
  tree arg, type, tmp;
  int frexp;

  switch (expr->ts.kind)
    {
      case 4:
	frexp = BUILT_IN_FREXPF;
	break;
      case 8:
	frexp = BUILT_IN_FREXP;
	break;
      case 10:
      case 16:
	frexp = BUILT_IN_FREXPL;
	break;
      default:
	gcc_unreachable ();
    }

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  tmp = gfc_create_var (integer_type_node, NULL);
  se->expr = build_call_expr_loc (input_location,
			      built_in_decls[frexp], 2,
			      fold_convert (type, arg),
			      gfc_build_addr_expr (NULL_TREE, tmp));
  se->expr = fold_convert (type, se->expr);
}


/* NEAREST (s, dir) is translated into
     tmp = copysign (HUGE_VAL, dir);
     return nextafter (s, tmp);
 */
static void
gfc_conv_intrinsic_nearest (gfc_se * se, gfc_expr * expr)
{
  tree args[2], type, tmp;
  int nextafter, copysign, huge_val;

  switch (expr->ts.kind)
    {
      case 4:
	nextafter = BUILT_IN_NEXTAFTERF;
	copysign = BUILT_IN_COPYSIGNF;
	huge_val = BUILT_IN_HUGE_VALF;
	break;
      case 8:
	nextafter = BUILT_IN_NEXTAFTER;
	copysign = BUILT_IN_COPYSIGN;
	huge_val = BUILT_IN_HUGE_VAL;
	break;
      case 10:
      case 16:
	nextafter = BUILT_IN_NEXTAFTERL;
	copysign = BUILT_IN_COPYSIGNL;
	huge_val = BUILT_IN_HUGE_VALL;
	break;
      default:
	gcc_unreachable ();
    }

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  tmp = build_call_expr_loc (input_location,
			 built_in_decls[copysign], 2,
			 build_call_expr_loc (input_location,
					  built_in_decls[huge_val], 0),
			 fold_convert (type, args[1]));
  se->expr = build_call_expr_loc (input_location,
			      built_in_decls[nextafter], 2,
			      fold_convert (type, args[0]), tmp);
  se->expr = fold_convert (type, se->expr);
}


/* SPACING (s) is translated into
    int e;
    if (s == 0)
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
  tree cond, tmp;
  int frexp, scalbn, k;
  stmtblock_t block;

  k = gfc_validate_kind (BT_REAL, expr->ts.kind, false);
  prec = build_int_cst (NULL_TREE, gfc_real_kinds[k].digits);
  emin = build_int_cst (NULL_TREE, gfc_real_kinds[k].min_exponent - 1);
  tiny = gfc_conv_mpfr_to_tree (gfc_real_kinds[k].tiny, expr->ts.kind, 0);

  switch (expr->ts.kind)
    {
      case 4:
	frexp = BUILT_IN_FREXPF;
	scalbn = BUILT_IN_SCALBNF;
	break;
      case 8:
	frexp = BUILT_IN_FREXP;
	scalbn = BUILT_IN_SCALBN;
	break;
      case 10:
      case 16:
	frexp = BUILT_IN_FREXPL;
	scalbn = BUILT_IN_SCALBNL;
	break;
      default:
	gcc_unreachable ();
    }

  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  arg = gfc_evaluate_now (arg, &se->pre);

  type = gfc_typenode_for_spec (&expr->ts);
  e = gfc_create_var (integer_type_node, NULL);
  res = gfc_create_var (type, NULL);


  /* Build the block for s /= 0.  */
  gfc_start_block (&block);
  tmp = build_call_expr_loc (input_location,
			 built_in_decls[frexp], 2, arg,
			 gfc_build_addr_expr (NULL_TREE, e));
  gfc_add_expr_to_block (&block, tmp);

  tmp = fold_build2 (MINUS_EXPR, integer_type_node, e, prec);
  gfc_add_modify (&block, e, fold_build2 (MAX_EXPR, integer_type_node,
					  tmp, emin));

  tmp = build_call_expr_loc (input_location,
			 built_in_decls[scalbn], 2,
			 build_real_from_int_cst (type, integer_one_node), e);
  gfc_add_modify (&block, res, tmp);

  /* Finish by building the IF statement.  */
  cond = fold_build2 (EQ_EXPR, boolean_type_node, arg,
		      build_real_from_int_cst (type, integer_zero_node));
  tmp = build3_v (COND_EXPR, cond, build2_v (MODIFY_EXPR, res, tiny),
		  gfc_finish_block (&block));

  gfc_add_expr_to_block (&se->pre, tmp);
  se->expr = res;
}


/* RRSPACING (s) is translated into
      int e;
      real x;
      x = fabs (s);
      if (x != 0)
      {
	frexp (s, &e);
	x = scalbn (x, precision - e);
      }
      return x;

 where precision is gfc_real_kinds[k].digits.  */

static void
gfc_conv_intrinsic_rrspacing (gfc_se * se, gfc_expr * expr)
{
  tree arg, type, e, x, cond, stmt, tmp;
  int frexp, scalbn, fabs, prec, k;
  stmtblock_t block;

  k = gfc_validate_kind (BT_REAL, expr->ts.kind, false);
  prec = gfc_real_kinds[k].digits;
  switch (expr->ts.kind)
    {
      case 4:
	frexp = BUILT_IN_FREXPF;
	scalbn = BUILT_IN_SCALBNF;
	fabs = BUILT_IN_FABSF;
	break;
      case 8:
	frexp = BUILT_IN_FREXP;
	scalbn = BUILT_IN_SCALBN;
	fabs = BUILT_IN_FABS;
	break;
      case 10:
      case 16:
	frexp = BUILT_IN_FREXPL;
	scalbn = BUILT_IN_SCALBNL;
	fabs = BUILT_IN_FABSL;
	break;
      default:
	gcc_unreachable ();
    }

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, &arg, 1);
  arg = gfc_evaluate_now (arg, &se->pre);

  e = gfc_create_var (integer_type_node, NULL);
  x = gfc_create_var (type, NULL);
  gfc_add_modify (&se->pre, x,
		  build_call_expr_loc (input_location,
				   built_in_decls[fabs], 1, arg));


  gfc_start_block (&block);
  tmp = build_call_expr_loc (input_location,
			 built_in_decls[frexp], 2, arg,
			 gfc_build_addr_expr (NULL_TREE, e));
  gfc_add_expr_to_block (&block, tmp);

  tmp = fold_build2 (MINUS_EXPR, integer_type_node,
		     build_int_cst (NULL_TREE, prec), e);
  tmp = build_call_expr_loc (input_location,
			 built_in_decls[scalbn], 2, x, tmp);
  gfc_add_modify (&block, x, tmp);
  stmt = gfc_finish_block (&block);

  cond = fold_build2 (NE_EXPR, boolean_type_node, x,
		      build_real_from_int_cst (type, integer_zero_node));
  tmp = build3_v (COND_EXPR, cond, stmt, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&se->pre, tmp);

  se->expr = fold_convert (type, x);
}


/* SCALE (s, i) is translated into scalbn (s, i).  */
static void
gfc_conv_intrinsic_scale (gfc_se * se, gfc_expr * expr)
{
  tree args[2], type;
  int scalbn;

  switch (expr->ts.kind)
    {
      case 4:
	scalbn = BUILT_IN_SCALBNF;
	break;
      case 8:
	scalbn = BUILT_IN_SCALBN;
	break;
      case 10:
      case 16:
	scalbn = BUILT_IN_SCALBNL;
	break;
      default:
	gcc_unreachable ();
    }

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, args, 2);
  se->expr = build_call_expr_loc (input_location,
			      built_in_decls[scalbn], 2,
			      fold_convert (type, args[0]),
			      fold_convert (integer_type_node, args[1]));
  se->expr = fold_convert (type, se->expr);
}


/* SET_EXPONENT (s, i) is translated into
   scalbn (frexp (s, &dummy_int), i).  */
static void
gfc_conv_intrinsic_set_exponent (gfc_se * se, gfc_expr * expr)
{
  tree args[2], type, tmp;
  int frexp, scalbn;

  switch (expr->ts.kind)
    {
      case 4:
	frexp = BUILT_IN_FREXPF;
	scalbn = BUILT_IN_SCALBNF;
	break;
      case 8:
	frexp = BUILT_IN_FREXP;
	scalbn = BUILT_IN_SCALBN;
	break;
      case 10:
      case 16:
	frexp = BUILT_IN_FREXPL;
	scalbn = BUILT_IN_SCALBNL;
	break;
      default:
	gcc_unreachable ();
    }

  type = gfc_typenode_for_spec (&expr->ts);
  gfc_conv_intrinsic_function_args (se, expr, args, 2);

  tmp = gfc_create_var (integer_type_node, NULL);
  tmp = build_call_expr_loc (input_location,
			 built_in_decls[frexp], 2,
			 fold_convert (type, args[0]),
			 gfc_build_addr_expr (NULL_TREE, tmp));
  se->expr = build_call_expr_loc (input_location,
			      built_in_decls[scalbn], 2, tmp,
			      fold_convert (integer_type_node, args[1]));
  se->expr = fold_convert (type, se->expr);
}


static void
gfc_conv_intrinsic_size (gfc_se * se, gfc_expr * expr)
{
  gfc_actual_arglist *actual;
  tree arg1;
  tree type;
  tree fncall0;
  tree fncall1;
  gfc_se argse;
  gfc_ss *ss;

  gfc_init_se (&argse, NULL);
  actual = expr->value.function.actual;

  ss = gfc_walk_expr (actual->expr);
  gcc_assert (ss != gfc_ss_terminator);
  argse.want_pointer = 1;
  argse.data_not_needed = 1;
  gfc_conv_expr_descriptor (&argse, actual->expr, ss);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);
  arg1 = gfc_evaluate_now (argse.expr, &se->pre);

  /* Build the call to size0.  */
  fncall0 = build_call_expr_loc (input_location,
			     gfor_fndecl_size0, 1, arg1);

  actual = actual->next;

  if (actual->expr)
    {
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_type (&argse, actual->expr,
			  gfc_array_index_type);
      gfc_add_block_to_block (&se->pre, &argse.pre);

      /* Unusually, for an intrinsic, size does not exclude
	 an optional arg2, so we must test for it.  */  
      if (actual->expr->expr_type == EXPR_VARIABLE
	    && actual->expr->symtree->n.sym->attr.dummy
	    && actual->expr->symtree->n.sym->attr.optional)
	{
	  tree tmp;
	  /* Build the call to size1.  */
	  fncall1 = build_call_expr_loc (input_location,
				     gfor_fndecl_size1, 2,
				     arg1, argse.expr);

	  gfc_init_se (&argse, NULL);
	  argse.want_pointer = 1;
	  argse.data_not_needed = 1;
	  gfc_conv_expr (&argse, actual->expr);
	  gfc_add_block_to_block (&se->pre, &argse.pre);
	  tmp = fold_build2 (NE_EXPR, boolean_type_node,
			     argse.expr, null_pointer_node);
	  tmp = gfc_evaluate_now (tmp, &se->pre);
	  se->expr = fold_build3 (COND_EXPR, pvoid_type_node,
				  tmp, fncall1, fncall0);
	}
      else
	{
	  se->expr = NULL_TREE;
	  argse.expr = fold_build2 (MINUS_EXPR, gfc_array_index_type,
				    argse.expr, gfc_index_one_node);
	}
    }
  else if (expr->value.function.actual->expr->rank == 1)
    {
      argse.expr = gfc_index_zero_node;
      se->expr = NULL_TREE;
    }
  else
    se->expr = fncall0;

  if (se->expr == NULL_TREE)
    {
      tree ubound, lbound;

      arg1 = build_fold_indirect_ref_loc (input_location,
				      arg1);
      ubound = gfc_conv_descriptor_ubound_get (arg1, argse.expr);
      lbound = gfc_conv_descriptor_lbound_get (arg1, argse.expr);
      se->expr = fold_build2 (MINUS_EXPR, gfc_array_index_type,
			      ubound, lbound);
      se->expr = fold_build2 (PLUS_EXPR, gfc_array_index_type, se->expr,
			      gfc_index_one_node);
      se->expr = fold_build2 (MAX_EXPR, gfc_array_index_type, se->expr,
			      gfc_index_zero_node);
    }

  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = convert (type, se->expr);
}


/* Helper function to compute the size of a character variable,
   excluding the terminating null characters.  The result has
   gfc_array_index_type type.  */

static tree
size_of_string_in_bytes (int kind, tree string_length)
{
  tree bytesize;
  int i = gfc_validate_kind (BT_CHARACTER, kind, false);
 
  bytesize = build_int_cst (gfc_array_index_type,
			    gfc_character_kinds[i].bit_size / 8);

  return fold_build2 (MULT_EXPR, gfc_array_index_type, bytesize,
		      fold_convert (gfc_array_index_type, string_length));
}


static void
gfc_conv_intrinsic_sizeof (gfc_se *se, gfc_expr *expr)
{
  gfc_expr *arg;
  gfc_ss *ss;
  gfc_se argse;
  tree source;
  tree source_bytes;
  tree type;
  tree tmp;
  tree lower;
  tree upper;
  int n;

  arg = expr->value.function.actual->expr;

  gfc_init_se (&argse, NULL);
  ss = gfc_walk_expr (arg);

  if (ss == gfc_ss_terminator)
    {
      gfc_conv_expr_reference (&argse, arg);
      source = argse.expr;

      type = TREE_TYPE (build_fold_indirect_ref_loc (input_location,
						 argse.expr));

      /* Obtain the source word length.  */
      if (arg->ts.type == BT_CHARACTER)
	se->expr = size_of_string_in_bytes (arg->ts.kind,
					    argse.string_length);
      else
	se->expr = fold_convert (gfc_array_index_type, size_in_bytes (type)); 
    }
  else
    {
      source_bytes = gfc_create_var (gfc_array_index_type, "bytes");
      argse.want_pointer = 0;
      gfc_conv_expr_descriptor (&argse, arg, ss);
      source = gfc_conv_descriptor_data_get (argse.expr);
      type = gfc_get_element_type (TREE_TYPE (argse.expr));

      /* Obtain the argument's word length.  */
      if (arg->ts.type == BT_CHARACTER)
	tmp = size_of_string_in_bytes (arg->ts.kind, argse.string_length);
      else
	tmp = fold_convert (gfc_array_index_type,
			    size_in_bytes (type)); 
      gfc_add_modify (&argse.pre, source_bytes, tmp);

      /* Obtain the size of the array in bytes.  */
      for (n = 0; n < arg->rank; n++)
	{
	  tree idx;
	  idx = gfc_rank_cst[n];
	  lower = gfc_conv_descriptor_lbound_get (argse.expr, idx);
	  upper = gfc_conv_descriptor_ubound_get (argse.expr, idx);
	  tmp = fold_build2 (MINUS_EXPR, gfc_array_index_type,
			     upper, lower);
	  tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     tmp, gfc_index_one_node);
	  tmp = fold_build2 (MULT_EXPR, gfc_array_index_type,
			     tmp, source_bytes);
	  gfc_add_modify (&argse.pre, source_bytes, tmp);
	}
      se->expr = source_bytes;
    }

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
				expr->value.function.actual->expr->ts.kind);
  se->expr = fold_build2 (op, gfc_typenode_for_spec (&expr->ts), se->expr,
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
  tree stride;
  tree stmt;
  gfc_actual_arglist *arg;
  gfc_se argse;
  gfc_ss *ss;
  gfc_ss_info *info;
  stmtblock_t block;
  int n;
  bool scalar_mold;

  info = NULL;
  if (se->loop)
    info = &se->ss->data.info;

  /* Convert SOURCE.  The output from this stage is:-
	source_bytes = length of the source in bytes
	source = pointer to the source data.  */
  arg = expr->value.function.actual;

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
  ss = gfc_walk_expr (arg->expr);

  source_bytes = gfc_create_var (gfc_array_index_type, NULL);

  /* Obtain the pointer to source and the length of source in bytes.  */
  if (ss == gfc_ss_terminator)
    {
      gfc_conv_expr_reference (&argse, arg->expr);
      source = argse.expr;

      source_type = TREE_TYPE (build_fold_indirect_ref_loc (input_location,
							argse.expr));

      /* Obtain the source word length.  */
      if (arg->expr->ts.type == BT_CHARACTER)
	tmp = size_of_string_in_bytes (arg->expr->ts.kind,
				       argse.string_length);
      else
	tmp = fold_convert (gfc_array_index_type,
			    size_in_bytes (source_type)); 
    }
  else
    {
      argse.want_pointer = 0;
      gfc_conv_expr_descriptor (&argse, arg->expr, ss);
      source = gfc_conv_descriptor_data_get (argse.expr);
      source_type = gfc_get_element_type (TREE_TYPE (argse.expr));

      /* Repack the source if not a full variable array.  */
      if (arg->expr->expr_type == EXPR_VARIABLE
	      && arg->expr->ref->u.ar.type != AR_FULL)
	{
	  tmp = gfc_build_addr_expr (NULL_TREE, argse.expr);

	  if (gfc_option.warn_array_temp)
	    gfc_warning ("Creating array temporary at %L", &expr->where);

	  source = build_call_expr_loc (input_location,
				    gfor_fndecl_in_pack, 1, tmp);
	  source = gfc_evaluate_now (source, &argse.pre);

	  /* Free the temporary.  */
	  gfc_start_block (&block);
	  tmp = gfc_call_free (convert (pvoid_type_node, source));
	  gfc_add_expr_to_block (&block, tmp);
	  stmt = gfc_finish_block (&block);

	  /* Clean up if it was repacked.  */
	  gfc_init_block (&block);
	  tmp = gfc_conv_array_data (argse.expr);
	  tmp = fold_build2 (NE_EXPR, boolean_type_node, source, tmp);
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
	  stride = gfc_conv_descriptor_stride_get (argse.expr, idx);
	  lower = gfc_conv_descriptor_lbound_get (argse.expr, idx);
	  upper = gfc_conv_descriptor_ubound_get (argse.expr, idx);
	  tmp = fold_build2 (MINUS_EXPR, gfc_array_index_type,
			     upper, lower);
	  gfc_add_modify (&argse.pre, extent, tmp);
	  tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     extent, gfc_index_one_node);
	  tmp = fold_build2 (MULT_EXPR, gfc_array_index_type,
			     tmp, source_bytes);
	}
    }

  gfc_add_modify (&argse.pre, source_bytes, tmp);
  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);

  /* Now convert MOLD.  The outputs are:
	mold_type = the TREE type of MOLD
	dest_word_len = destination word length in bytes.  */
  arg = arg->next;

  gfc_init_se (&argse, NULL);
  ss = gfc_walk_expr (arg->expr);

  scalar_mold = arg->expr->rank == 0;

  if (ss == gfc_ss_terminator)
    {
      gfc_conv_expr_reference (&argse, arg->expr);
      mold_type = TREE_TYPE (build_fold_indirect_ref_loc (input_location,
						      argse.expr));
    }
  else
    {
      gfc_init_se (&argse, NULL);
      argse.want_pointer = 0;
      gfc_conv_expr_descriptor (&argse, arg->expr, ss);
      mold_type = gfc_get_element_type (TREE_TYPE (argse.expr));
    }

  gfc_add_block_to_block (&se->pre, &argse.pre);
  gfc_add_block_to_block (&se->post, &argse.post);

  if (strcmp (expr->value.function.name, "__transfer_in_transfer") == 0)
    {
      /* If this TRANSFER is nested in another TRANSFER, use a type
	 that preserves all bits.  */
      if (arg->expr->ts.type == BT_LOGICAL)
	mold_type = gfc_get_int_type (arg->expr->ts.kind);
    }

  if (arg->expr->ts.type == BT_CHARACTER)
    {
      tmp = size_of_string_in_bytes (arg->expr->ts.kind, argse.string_length);
      mold_type = gfc_get_character_type_len (arg->expr->ts.kind, tmp);
    }
  else
    tmp = fold_convert (gfc_array_index_type,
			size_in_bytes (mold_type)); 
 
  dest_word_len = gfc_create_var (gfc_array_index_type, NULL);
  gfc_add_modify (&se->pre, dest_word_len, tmp);

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
    tmp = fold_build2 (MULT_EXPR, gfc_array_index_type,
		       tmp, dest_word_len);
  else
    tmp = source_bytes;

  gfc_add_modify (&se->pre, size_bytes, tmp);
  gfc_add_modify (&se->pre, size_words,
		       fold_build2 (CEIL_DIV_EXPR, gfc_array_index_type,
				    size_bytes, dest_word_len));

  /* Evaluate the bounds of the result.  If the loop range exists, we have
     to check if it is too large.  If so, we modify loop->to be consistent
     with min(size, size(source)).  Otherwise, size is made consistent with
     the loop range, so that the right number of bytes is transferred.*/
  n = se->loop->order[0];
  if (se->loop->to[n] != NULL_TREE)
    {
      tmp = fold_build2 (MINUS_EXPR, gfc_array_index_type,
			 se->loop->to[n], se->loop->from[n]);
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 tmp, gfc_index_one_node);
      tmp = fold_build2 (MIN_EXPR, gfc_array_index_type,
			 tmp, size_words);
      gfc_add_modify (&se->pre, size_words, tmp);
      gfc_add_modify (&se->pre, size_bytes,
			   fold_build2 (MULT_EXPR, gfc_array_index_type,
					size_words, dest_word_len));
      upper = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			   size_words, se->loop->from[n]);
      upper = fold_build2 (MINUS_EXPR, gfc_array_index_type,
			   upper, gfc_index_one_node);
    }
  else
    {
      upper = fold_build2 (MINUS_EXPR, gfc_array_index_type,
			   size_words, gfc_index_one_node);
      se->loop->from[n] = gfc_index_zero_node;
    }

  se->loop->to[n] = upper;

  /* Build a destination descriptor, using the pointer, source, as the
     data field.  */
  gfc_trans_create_temp_array (&se->pre, &se->post, se->loop,
			       info, mold_type, NULL_TREE, false, true, false,
			       &expr->where);

  /* Cast the pointer to the result.  */
  tmp = gfc_conv_descriptor_data_get (info->descriptor);
  tmp = fold_convert (pvoid_type_node, tmp);

  /* Use memcpy to do the transfer.  */
  tmp = build_call_expr_loc (input_location,
			 built_in_decls[BUILT_IN_MEMCPY],
			 3,
			 tmp,
			 fold_convert (pvoid_type_node, source),
			 fold_build2 (MIN_EXPR, gfc_array_index_type,
				      size_bytes, source_bytes));
  gfc_add_expr_to_block (&se->pre, tmp);

  se->expr = info->descriptor;
  if (expr->ts.type == BT_CHARACTER)
    se->string_length = dest_word_len;

  return;

/* Deal with scalar results.  */
scalar_transfer:
  extent = fold_build2 (MIN_EXPR, gfc_array_index_type,
			dest_word_len, source_bytes);

  if (expr->ts.type == BT_CHARACTER)
    {
      tree direct;
      tree indirect;

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
			     built_in_decls[BUILT_IN_MEMCPY], 3,
			     fold_convert (pvoid_type_node, tmpdecl),
			     fold_convert (pvoid_type_node, ptr),
			     extent);
      gfc_add_expr_to_block (&block, tmp);
      indirect = gfc_finish_block (&block);

      /* Wrap it up with the condition.  */
      tmp = fold_build2 (LE_EXPR, boolean_type_node,
			 dest_word_len, source_bytes);
      tmp = build3_v (COND_EXPR, tmp, direct, indirect);
      gfc_add_expr_to_block (&se->pre, tmp);

      se->expr = tmpdecl;
      se->string_length = dest_word_len;
    }
  else
    {
      tmpdecl = gfc_create_var (mold_type, "transfer");

      ptr = convert (build_pointer_type (mold_type), source);

      /* Use memcpy to do the transfer.  */
      tmp = gfc_build_addr_expr (NULL_TREE, tmpdecl);
      tmp = build_call_expr_loc (input_location,
			     built_in_decls[BUILT_IN_MEMCPY], 3,
			     fold_convert (pvoid_type_node, tmp),
			     fold_convert (pvoid_type_node, ptr),
			     extent);
      gfc_add_expr_to_block (&se->pre, tmp);

      se->expr = tmpdecl;
    }
}


/* Generate code for the ALLOCATED intrinsic.
   Generate inline code that directly check the address of the argument.  */

static void
gfc_conv_allocated (gfc_se *se, gfc_expr *expr)
{
  gfc_actual_arglist *arg1;
  gfc_se arg1se;
  gfc_ss *ss1;
  tree tmp;

  gfc_init_se (&arg1se, NULL);
  arg1 = expr->value.function.actual;
  ss1 = gfc_walk_expr (arg1->expr);

  if (ss1 == gfc_ss_terminator)
    {
      /* Allocatable scalar.  */
      arg1se.want_pointer = 1;
      gfc_conv_expr (&arg1se, arg1->expr);
      tmp = arg1se.expr;
    }
  else
    {
      /* Allocatable array.  */
      arg1se.descriptor_only = 1;
      gfc_conv_expr_descriptor (&arg1se, arg1->expr, ss1);
      tmp = gfc_conv_descriptor_data_get (arg1se.expr);
    }

  tmp = fold_build2 (NE_EXPR, boolean_type_node,
		     tmp, fold_convert (TREE_TYPE (tmp), null_pointer_node));
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
  tree nonzero_charlen;
  tree nonzero_arraylen;
  gfc_ss *ss1, *ss2;

  gfc_init_se (&arg1se, NULL);
  gfc_init_se (&arg2se, NULL);
  arg1 = expr->value.function.actual;
  arg2 = arg1->next;
  ss1 = gfc_walk_expr (arg1->expr);

  if (!arg2->expr)
    {
      /* No optional target.  */
      if (ss1 == gfc_ss_terminator)
        {
          /* A pointer to a scalar.  */
          arg1se.want_pointer = 1;
          gfc_conv_expr (&arg1se, arg1->expr);
          tmp2 = arg1se.expr;
        }
      else
        {
          /* A pointer to an array.  */
          gfc_conv_expr_descriptor (&arg1se, arg1->expr, ss1);
          tmp2 = gfc_conv_descriptor_data_get (arg1se.expr);
        }
      gfc_add_block_to_block (&se->pre, &arg1se.pre);
      gfc_add_block_to_block (&se->post, &arg1se.post);
      tmp = fold_build2 (NE_EXPR, boolean_type_node, tmp2,
			 fold_convert (TREE_TYPE (tmp2), null_pointer_node));
      se->expr = tmp;
    }
  else
    {
      /* An optional target.  */
      ss2 = gfc_walk_expr (arg2->expr);

      nonzero_charlen = NULL_TREE;
      if (arg1->expr->ts.type == BT_CHARACTER)
	nonzero_charlen = fold_build2 (NE_EXPR, boolean_type_node,
				       arg1->expr->ts.u.cl->backend_decl,
				       integer_zero_node);

      if (ss1 == gfc_ss_terminator)
        {
          /* A pointer to a scalar.  */
          gcc_assert (ss2 == gfc_ss_terminator);
          arg1se.want_pointer = 1;
          gfc_conv_expr (&arg1se, arg1->expr);
          arg2se.want_pointer = 1;
          gfc_conv_expr (&arg2se, arg2->expr);
	  gfc_add_block_to_block (&se->pre, &arg1se.pre);
	  gfc_add_block_to_block (&se->post, &arg1se.post);
          tmp = fold_build2 (EQ_EXPR, boolean_type_node,
			     arg1se.expr, arg2se.expr);
          tmp2 = fold_build2 (NE_EXPR, boolean_type_node,
			      arg1se.expr, null_pointer_node);
          se->expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				  tmp, tmp2);
        }
      else
        {
	  /* An array pointer of zero length is not associated if target is
	     present.  */
	  arg1se.descriptor_only = 1;
	  gfc_conv_expr_lhs (&arg1se, arg1->expr);
	  tmp = gfc_conv_descriptor_stride_get (arg1se.expr,
					    gfc_rank_cst[arg1->expr->rank - 1]);
	  nonzero_arraylen = fold_build2 (NE_EXPR, boolean_type_node, tmp,
					  build_int_cst (TREE_TYPE (tmp), 0));

          /* A pointer to an array, call library function _gfor_associated.  */
          gcc_assert (ss2 != gfc_ss_terminator);
          arg1se.want_pointer = 1;
          gfc_conv_expr_descriptor (&arg1se, arg1->expr, ss1);

          arg2se.want_pointer = 1;
          gfc_conv_expr_descriptor (&arg2se, arg2->expr, ss2);
          gfc_add_block_to_block (&se->pre, &arg2se.pre);
          gfc_add_block_to_block (&se->post, &arg2se.post);
          se->expr = build_call_expr_loc (input_location,
				      gfor_fndecl_associated, 2,
				      arg1se.expr, arg2se.expr);
	  se->expr = convert (boolean_type_node, se->expr);
	  se->expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				  se->expr, nonzero_arraylen);
        }

      /* If target is present zero character length pointers cannot
	 be associated.  */
      if (nonzero_charlen != NULL_TREE)
	se->expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				se->expr, nonzero_charlen);
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

  gfc_init_se (&se1, NULL);
  gfc_init_se (&se2, NULL);

  a = expr->value.function.actual->expr;
  b = expr->value.function.actual->next->expr;

  if (a->ts.type == BT_CLASS)
    gfc_add_component_ref (a, "$vindex");
  else if (a->ts.type == BT_DERIVED)
    a = gfc_int_expr (a->ts.u.derived->vindex);

  if (b->ts.type == BT_CLASS)
    gfc_add_component_ref (b, "$vindex");
  else if (b->ts.type == BT_DERIVED)
    b = gfc_int_expr (b->ts.u.derived->vindex);

  gfc_conv_expr (&se1, a);
  gfc_conv_expr (&se2, b);

  tmp = fold_build2 (EQ_EXPR, boolean_type_node,
		     se1.expr, fold_convert (TREE_TYPE (se1.expr), se2.expr));
  se->expr = convert (gfc_typenode_for_spec (&expr->ts), tmp);
}


/* Generate code for the EXTENDS_TYPE_OF intrinsic.  */

static void
gfc_conv_extends_type_of (gfc_se *se, gfc_expr *expr)
{
  gfc_expr *e;
  /* TODO: Implement EXTENDS_TYPE_OF.  */
  gfc_error ("Intrinsic EXTENDS_TYPE_OF at %L not yet implemented",
	     &expr->where);
  /* Just return 'false' for now.  */
  e = gfc_logical_expr (false, &expr->where);
  gfc_conv_expr (se, e);
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


/* Generate code for SELECTED_REAL_KIND (P, R) intrinsic function.  */

static void
gfc_conv_intrinsic_sr_kind (gfc_se *se, gfc_expr *expr)
{
  gfc_actual_arglist *actual;
  tree args, type;
  gfc_se argse;

  args = NULL_TREE;
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
      args = gfc_chainon_list (args, argse.expr);
    }

  /* Convert it to the required type.  */
  type = gfc_typenode_for_spec (&expr->ts);
  se->expr = build_function_call_expr (input_location,
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
  args = (tree *) alloca (sizeof (tree) * num_args);

  var = gfc_create_var (gfc_get_pchar_type (expr->ts.kind), "pstr");
  addr = gfc_build_addr_expr (ppvoid_type_node, var);
  len = gfc_create_var (gfc_get_int_type (4), "len");

  gfc_conv_intrinsic_function_args (se, expr, &args[2], num_args - 2);
  args[0] = gfc_build_addr_expr (NULL_TREE, len);
  args[1] = addr;

  if (expr->ts.kind == 1)
    function = gfor_fndecl_string_trim;
  else if (expr->ts.kind == 4)
    function = gfor_fndecl_string_trim_char4;
  else
    gcc_unreachable ();

  fndecl = build_addr (function, current_function_decl);
  tmp = build_call_array_loc (input_location,
			  TREE_TYPE (TREE_TYPE (function)), fndecl,
			  num_args, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Free the temporary afterwards, if necessary.  */
  cond = fold_build2 (GT_EXPR, boolean_type_node,
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
  size = build_int_cst (size_type_node, gfc_character_kinds[i].bit_size / 8);

  /* Get the arguments.  */
  gfc_conv_intrinsic_function_args (se, expr, args, 3);
  slen = fold_convert (size_type_node, gfc_evaluate_now (args[0], &se->pre));
  src = args[1];
  ncopies = gfc_evaluate_now (args[2], &se->pre);
  ncopies_type = TREE_TYPE (ncopies);

  /* Check that NCOPIES is not negative.  */
  cond = fold_build2 (LT_EXPR, boolean_type_node, ncopies,
		      build_int_cst (ncopies_type, 0));
  gfc_trans_runtime_check (true, false, cond, &se->pre, &expr->where,
			   "Argument NCOPIES of REPEAT intrinsic is negative "
			   "(its value is %lld)",
			   fold_convert (long_integer_type_node, ncopies));

  /* If the source length is zero, any non negative value of NCOPIES
     is valid, and nothing happens.  */
  n = gfc_create_var (ncopies_type, "ncopies");
  cond = fold_build2 (EQ_EXPR, boolean_type_node, slen,
		      build_int_cst (size_type_node, 0));
  tmp = fold_build3 (COND_EXPR, ncopies_type, cond,
		     build_int_cst (ncopies_type, 0), ncopies);
  gfc_add_modify (&se->pre, n, tmp);
  ncopies = n;

  /* Check that ncopies is not too large: ncopies should be less than
     (or equal to) MAX / slen, where MAX is the maximal integer of
     the gfc_charlen_type_node type.  If slen == 0, we need a special
     case to avoid the division by zero.  */
  i = gfc_validate_kind (BT_INTEGER, gfc_charlen_int_kind, false);
  max = gfc_conv_mpz_to_tree (gfc_integer_kinds[i].huge, gfc_charlen_int_kind);
  max = fold_build2 (TRUNC_DIV_EXPR, size_type_node,
		     fold_convert (size_type_node, max), slen);
  largest = TYPE_PRECISION (size_type_node) > TYPE_PRECISION (ncopies_type)
	      ? size_type_node : ncopies_type;
  cond = fold_build2 (GT_EXPR, boolean_type_node,
		      fold_convert (largest, ncopies),
		      fold_convert (largest, max));
  tmp = fold_build2 (EQ_EXPR, boolean_type_node, slen,
		     build_int_cst (size_type_node, 0));
  cond = fold_build3 (COND_EXPR, boolean_type_node, tmp, boolean_false_node,
		      cond);
  gfc_trans_runtime_check (true, false, cond, &se->pre, &expr->where,
			   "Argument NCOPIES of REPEAT intrinsic is too large");

  /* Compute the destination length.  */
  dlen = fold_build2 (MULT_EXPR, gfc_charlen_type_node,
		      fold_convert (gfc_charlen_type_node, slen),
		      fold_convert (gfc_charlen_type_node, ncopies));
  type = gfc_get_character_type (expr->ts.kind, expr->ts.u.cl);
  dest = gfc_conv_string_tmp (se, build_pointer_type (type), dlen);

  /* Generate the code to do the repeat operation:
       for (i = 0; i < ncopies; i++)
         memmove (dest + (i * slen * size), src, slen*size);  */
  gfc_start_block (&block);
  count = gfc_create_var (ncopies_type, "count");
  gfc_add_modify (&block, count, build_int_cst (ncopies_type, 0));
  exit_label = gfc_build_label_decl (NULL_TREE);

  /* Start the loop body.  */
  gfc_start_block (&body);

  /* Exit the loop if count >= ncopies.  */
  cond = fold_build2 (GE_EXPR, boolean_type_node, count, ncopies);
  tmp = build1_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = fold_build3 (COND_EXPR, void_type_node, cond, tmp,
		     build_empty_stmt (input_location));
  gfc_add_expr_to_block (&body, tmp);

  /* Call memmove (dest + (i*slen*size), src, slen*size).  */
  tmp = fold_build2 (MULT_EXPR, gfc_charlen_type_node,
		     fold_convert (gfc_charlen_type_node, slen),
		     fold_convert (gfc_charlen_type_node, count));
  tmp = fold_build2 (MULT_EXPR, gfc_charlen_type_node,
		     tmp, fold_convert (gfc_charlen_type_node, size));
  tmp = fold_build2 (POINTER_PLUS_EXPR, pvoid_type_node,
		     fold_convert (pvoid_type_node, dest),
		     fold_convert (sizetype, tmp));
  tmp = build_call_expr_loc (input_location,
			 built_in_decls[BUILT_IN_MEMMOVE], 3, tmp, src,
			 fold_build2 (MULT_EXPR, size_type_node, slen,
				      fold_convert (size_type_node, size)));
  gfc_add_expr_to_block (&body, tmp);

  /* Increment count.  */
  tmp = fold_build2 (PLUS_EXPR, ncopies_type,
		     count, build_int_cst (TREE_TYPE (count), 1));
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


/* The loc intrinsic returns the address of its argument as
   gfc_index_integer_kind integer.  */

static void
gfc_conv_intrinsic_loc (gfc_se * se, gfc_expr * expr)
{
  tree temp_var;
  gfc_expr *arg_expr;
  gfc_ss *ss;

  gcc_assert (!se->ss);

  arg_expr = expr->value.function.actual->expr;
  ss = gfc_walk_expr (arg_expr);
  if (ss == gfc_ss_terminator)
    gfc_conv_expr_reference (se, arg_expr);
  else
    gfc_conv_array_parameter (se, arg_expr, ss, 1, NULL, NULL, NULL);
  se->expr= convert (gfc_get_int_type (gfc_index_integer_kind), se->expr);
   
  /* Create a temporary variable for loc return value.  Without this, 
     we get an error an ICE in gcc/expr.c(expand_expr_addr_expr_1).  */
  temp_var = gfc_create_var (gfc_get_int_type (gfc_index_integer_kind), NULL);
  gfc_add_modify (&se->pre, temp_var, se->expr);
  se->expr = temp_var;
}

/* Generate code for an intrinsic function.  Some map directly to library
   calls, others get special handling.  In some cases the name of the function
   used depends on the type specifiers.  */

void
gfc_conv_intrinsic_function (gfc_se * se, gfc_expr * expr)
{
  gfc_intrinsic_sym *isym;
  const char *name;
  int lib, kind;
  tree fndecl;

  isym = expr->value.function.isym;

  name = &expr->value.function.name[2];

  if (expr->rank > 0 && !expr->inline_noncopying_intrinsic)
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

    case GFC_ISYM_EXTENDS_TYPE_OF:
      gfc_conv_extends_type_of (se, expr);
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

    case GFC_ISYM_BTEST:
      gfc_conv_intrinsic_btest (se, expr);
      break;

    case GFC_ISYM_ACHAR:
    case GFC_ISYM_CHAR:
      gfc_conv_intrinsic_char (se, expr);
      break;

    case GFC_ISYM_CONVERSION:
    case GFC_ISYM_REAL:
    case GFC_ISYM_LOGICAL:
    case GFC_ISYM_DBLE:
      gfc_conv_intrinsic_conversion (se, expr);
      break;

      /* Integer conversions are handled separately to make sure we get the
         correct rounding mode.  */
    case GFC_ISYM_INT:
    case GFC_ISYM_INT2:
    case GFC_ISYM_INT8:
    case GFC_ISYM_LONG:
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

    case GFC_ISYM_FDATE:
      gfc_conv_intrinsic_fdate (se, expr);
      break;

    case GFC_ISYM_FRACTION:
      gfc_conv_intrinsic_fraction (se, expr);
      break;

    case GFC_ISYM_IAND:
      gfc_conv_intrinsic_bitop (se, expr, BIT_AND_EXPR);
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

    case GFC_ISYM_IS_IOSTAT_END:
      gfc_conv_has_intvalue (se, expr, LIBERROR_END);
      break;

    case GFC_ISYM_IS_IOSTAT_EOR:
      gfc_conv_has_intvalue (se, expr, LIBERROR_EOR);
      break;

    case GFC_ISYM_ISNAN:
      gfc_conv_intrinsic_isnan (se, expr);
      break;

    case GFC_ISYM_LSHIFT:
      gfc_conv_intrinsic_rlshift (se, expr, 0);
      break;

    case GFC_ISYM_RSHIFT:
      gfc_conv_intrinsic_rlshift (se, expr, 1);
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

    case GFC_ISYM_LBOUND:
      gfc_conv_intrinsic_bound (se, expr, 0);
      break;

    case GFC_ISYM_TRANSPOSE:
      if (se->ss && se->ss->useflags)
	{
	  gfc_conv_tmp_array_ref (se);
	  gfc_advance_se_ss_chain (se);
	}
      else
	gfc_conv_array_transpose (se, expr->value.function.actual->expr);
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

    case GFC_ISYM_MAX:
      if (expr->ts.type == BT_CHARACTER)
	gfc_conv_intrinsic_minmax_char (se, expr, 1);
      else
	gfc_conv_intrinsic_minmax (se, expr, GT_EXPR);
      break;

    case GFC_ISYM_MAXLOC:
      gfc_conv_intrinsic_minmaxloc (se, expr, GT_EXPR);
      break;

    case GFC_ISYM_MAXVAL:
      gfc_conv_intrinsic_minmaxval (se, expr, GT_EXPR);
      break;

    case GFC_ISYM_MERGE:
      gfc_conv_intrinsic_merge (se, expr);
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

    case GFC_ISYM_NOT:
      gfc_conv_intrinsic_not (se, expr);
      break;

    case GFC_ISYM_OR:
      gfc_conv_intrinsic_bitop (se, expr, BIT_IOR_EXPR);
      break;

    case GFC_ISYM_PRESENT:
      gfc_conv_intrinsic_present (se, expr);
      break;

    case GFC_ISYM_PRODUCT:
      gfc_conv_intrinsic_arith (se, expr, MULT_EXPR);
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

    case GFC_ISYM_SIGN:
      gfc_conv_intrinsic_sign (se, expr);
      break;

    case GFC_ISYM_SIZE:
      gfc_conv_intrinsic_size (se, expr);
      break;

    case GFC_ISYM_SIZEOF:
      gfc_conv_intrinsic_sizeof (se, expr);
      break;

    case GFC_ISYM_SPACING:
      gfc_conv_intrinsic_spacing (se, expr);
      break;

    case GFC_ISYM_SUM:
      gfc_conv_intrinsic_arith (se, expr, PLUS_EXPR);
      break;

    case GFC_ISYM_TRANSFER:
      if (se->ss && se->ss->useflags)
	{
	  /* Access the previously obtained result.  */
	  gfc_conv_tmp_array_ref (se);
	  gfc_advance_se_ss_chain (se);
	}
      else
	gfc_conv_intrinsic_transfer (se, expr);
      break;

    case GFC_ISYM_TTYNAM:
      gfc_conv_intrinsic_ttynam (se, expr);
      break;

    case GFC_ISYM_UBOUND:
      gfc_conv_intrinsic_bound (se, expr, 1);
      break;

    case GFC_ISYM_XOR:
      gfc_conv_intrinsic_bitop (se, expr, BIT_XOR_EXPR);
      break;

    case GFC_ISYM_LOC:
      gfc_conv_intrinsic_loc (se, expr);
      break;

    case GFC_ISYM_ACCESS:
    case GFC_ISYM_CHDIR:
    case GFC_ISYM_CHMOD:
    case GFC_ISYM_DTIME:
    case GFC_ISYM_ETIME:
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
    case GFC_ISYM_KILL:
    case GFC_ISYM_IERRNO:
    case GFC_ISYM_IRAND:
    case GFC_ISYM_ISATTY:
    case GFC_ISYM_LINK:
    case GFC_ISYM_LSTAT:
    case GFC_ISYM_MALLOC:
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


/* This generates code to execute before entering the scalarization loop.
   Currently does nothing.  */

void
gfc_add_intrinsic_ss_code (gfc_loopinfo * loop ATTRIBUTE_UNUSED, gfc_ss * ss)
{
  switch (ss->expr->value.function.isym->id)
    {
    case GFC_ISYM_UBOUND:
    case GFC_ISYM_LBOUND:
      break;

    default:
      gcc_unreachable ();
    }
}


/* UBOUND and LBOUND intrinsics with one parameter are expanded into code
   inside the scalarization loop.  */

static gfc_ss *
gfc_walk_intrinsic_bound (gfc_ss * ss, gfc_expr * expr)
{
  gfc_ss *newss;

  /* The two argument version returns a scalar.  */
  if (expr->value.function.actual->next->expr)
    return ss;

  newss = gfc_get_ss ();
  newss->type = GFC_SS_INTRINSIC;
  newss->expr = expr;
  newss->next = ss;
  newss->data.info.dimen = 1;

  return newss;
}


/* Walk an intrinsic array libcall.  */

static gfc_ss *
gfc_walk_intrinsic_libfunc (gfc_ss * ss, gfc_expr * expr)
{
  gfc_ss *newss;

  gcc_assert (expr->rank > 0);

  newss = gfc_get_ss ();
  newss->type = GFC_SS_FUNCTION;
  newss->expr = expr;
  newss->next = ss;
  newss->data.info.dimen = expr->rank;

  return newss;
}


/* Returns nonzero if the specified intrinsic function call maps directly to
   an external library call.  Should only be used for functions that return
   arrays.  */

int
gfc_is_intrinsic_libcall (gfc_expr * expr)
{
  gcc_assert (expr->expr_type == EXPR_FUNCTION && expr->value.function.isym);
  gcc_assert (expr->rank > 0);

  switch (expr->value.function.isym->id)
    {
    case GFC_ISYM_ALL:
    case GFC_ISYM_ANY:
    case GFC_ISYM_COUNT:
    case GFC_ISYM_MATMUL:
    case GFC_ISYM_MAXLOC:
    case GFC_ISYM_MAXVAL:
    case GFC_ISYM_MINLOC:
    case GFC_ISYM_MINVAL:
    case GFC_ISYM_PRODUCT:
    case GFC_ISYM_SUM:
    case GFC_ISYM_SHAPE:
    case GFC_ISYM_SPREAD:
    case GFC_ISYM_TRANSPOSE:
      /* Ignore absent optional parameters.  */
      return 1;

    case GFC_ISYM_RESHAPE:
    case GFC_ISYM_CSHIFT:
    case GFC_ISYM_EOSHIFT:
    case GFC_ISYM_PACK:
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
    return gfc_walk_elemental_function_args (ss, expr->value.function.actual, GFC_SS_SCALAR);

  if (expr->rank == 0)
    return ss;

  if (gfc_is_intrinsic_libcall (expr))
    return gfc_walk_intrinsic_libfunc (ss, expr);

  /* Special cases.  */
  switch (isym->id)
    {
    case GFC_ISYM_LBOUND:
    case GFC_ISYM_UBOUND:
      return gfc_walk_intrinsic_bound (ss, expr);

    case GFC_ISYM_TRANSFER:
      return gfc_walk_intrinsic_libfunc (ss, expr);

    default:
      /* This probably meant someone forgot to add an intrinsic to the above
         list(s) when they implemented it, or something's gone horribly
	 wrong.  */
      gcc_unreachable ();
    }
}

#include "gt-fortran-trans-intrinsic.h"
