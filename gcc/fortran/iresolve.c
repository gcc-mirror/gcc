/* Intrinsic function resolution.
   Copyright (C) 2000-2019 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Katherine Holcomb

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


/* Assign name and types to intrinsic procedures.  For functions, the
   first argument to a resolution function is an expression pointer to
   the original function node and the rest are pointers to the
   arguments of the function call.  For subroutines, a pointer to the
   code node is passed.  The result type and library subroutine name
   are generally set according to the function arguments.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "gfortran.h"
#include "stringpool.h"
#include "intrinsic.h"
#include "constructor.h"
#include "arith.h"
#include "trans.h"

/* Given printf-like arguments, return a stable version of the result string.

   We already have a working, optimized string hashing table in the form of
   the identifier table.  Reusing this table is likely not to be wasted,
   since if the function name makes it to the gimple output of the frontend,
   we'll have to create the identifier anyway.  */

const char *
gfc_get_string (const char *format, ...)
{
  char temp_name[128];
  const char *str;
  va_list ap;
  tree ident;

  /* Handle common case without vsnprintf and temporary buffer.  */
  if (format[0] == '%' && format[1] == 's' && format[2] == '\0')
    {
      va_start (ap, format);
      str = va_arg (ap, const char *);
      va_end (ap);
    }
  else
    {
      va_start (ap, format);
      vsnprintf (temp_name, sizeof (temp_name), format, ap);
      va_end (ap);
      temp_name[sizeof (temp_name) - 1] = 0;
      str = temp_name;
    }

  ident = get_identifier (str);
  return IDENTIFIER_POINTER (ident);
}

/* MERGE and SPREAD need to have source charlen's present for passing
   to the result expression.  */
static void
check_charlen_present (gfc_expr *source)
{
  if (source->ts.u.cl == NULL)
    source->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);

  if (source->expr_type == EXPR_CONSTANT)
    {
      source->ts.u.cl->length
		= gfc_get_int_expr (gfc_charlen_int_kind, NULL,
				    source->value.character.length);
      source->rank = 0;
    }
  else if (source->expr_type == EXPR_ARRAY)
    {
      gfc_constructor *c = gfc_constructor_first (source->value.constructor);
      source->ts.u.cl->length
		= gfc_get_int_expr (gfc_charlen_int_kind, NULL,
				    c->expr->value.character.length);
    }
}

/* Helper function for resolving the "mask" argument.  */

static void
resolve_mask_arg (gfc_expr *mask)
{

  gfc_typespec ts;
  gfc_clear_ts (&ts);

  if (mask->rank == 0)
    {
      /* For the scalar case, coerce the mask to kind=4 unconditionally
	 (because this is the only kind we have a library function
	 for).  */

      if (mask->ts.kind != 4)
	{
	  ts.type = BT_LOGICAL;
	  ts.kind = 4;
	  gfc_convert_type (mask, &ts, 2);
	}
    }
  else
    {
      /* In the library, we access the mask with a GFC_LOGICAL_1
	 argument.  No need to waste memory if we are about to create
	 a temporary array.  */
      if (mask->expr_type == EXPR_OP && mask->ts.kind != 1)
	{
	  ts.type = BT_LOGICAL;
	  ts.kind = 1;
	  gfc_convert_type_warn (mask, &ts, 2, 0);
	}
    }
}


static void
resolve_bound (gfc_expr *f, gfc_expr *array, gfc_expr *dim, gfc_expr *kind,
	       const char *name, bool coarray)
{
  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;

  if (dim == NULL)
    {
      f->rank = 1;
      if (array->rank != -1)
	{
	  f->shape = gfc_get_shape (1);
	  mpz_init_set_ui (f->shape[0], coarray ? gfc_get_corank (array)
						: array->rank);
	}
    }

  f->value.function.name = gfc_get_string ("%s", name);
}


static void
resolve_transformational (const char *name, gfc_expr *f, gfc_expr *array,
			  gfc_expr *dim, gfc_expr *mask)
{
  const char *prefix;

  f->ts = array->ts;

  if (mask)
    {
      if (mask->rank == 0)
	prefix = "s";
      else
	prefix = "m";

      resolve_mask_arg (mask);
    }
  else
    prefix = "";

  if (dim != NULL)
    {
      f->rank = array->rank - 1;
      f->shape = gfc_copy_shape_excluding (array->shape, array->rank, dim);
      gfc_resolve_dim_arg (dim);
    }

  f->value.function.name
    = gfc_get_string (PREFIX ("%s%s_%c%d"), prefix, name,
		      gfc_type_letter (array->ts.type), array->ts.kind);
}


/********************** Resolution functions **********************/


void
gfc_resolve_abs (gfc_expr *f, gfc_expr *a)
{
  f->ts = a->ts;
  if (f->ts.type == BT_COMPLEX)
    f->ts.type = BT_REAL;

  f->value.function.name
    = gfc_get_string ("__abs_%c%d", gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_access (gfc_expr *f, gfc_expr *name ATTRIBUTE_UNUSED,
		    gfc_expr *mode ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_c_int_kind;
  f->value.function.name = PREFIX ("access_func");
}


void
gfc_resolve_adjustl (gfc_expr *f, gfc_expr *string)
{
  f->ts.type = BT_CHARACTER;
  f->ts.kind = string->ts.kind;
  if (string->ts.u.cl)
    f->ts.u.cl = gfc_new_charlen (gfc_current_ns, string->ts.u.cl);

  f->value.function.name = gfc_get_string ("__adjustl_s%d", f->ts.kind);
}


void
gfc_resolve_adjustr (gfc_expr *f, gfc_expr *string)
{
  f->ts.type = BT_CHARACTER;
  f->ts.kind = string->ts.kind;
  if (string->ts.u.cl)
    f->ts.u.cl = gfc_new_charlen (gfc_current_ns, string->ts.u.cl);

  f->value.function.name = gfc_get_string ("__adjustr_s%d", f->ts.kind);
}


static void
gfc_resolve_char_achar (gfc_expr *f, gfc_expr *x, gfc_expr *kind,
			bool is_achar)
{
  f->ts.type = BT_CHARACTER;
  f->ts.kind = (kind == NULL)
	     ? gfc_default_character_kind : mpz_get_si (kind->value.integer);
  f->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
  f->ts.u.cl->length = gfc_get_int_expr (gfc_charlen_int_kind, NULL, 1);

  f->value.function.name
    = gfc_get_string ("__%schar_%d_%c%d", is_achar ? "a" : "", f->ts.kind,
		      gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_achar (gfc_expr *f, gfc_expr *x, gfc_expr *kind)
{
  gfc_resolve_char_achar (f, x, kind, true);
}


void
gfc_resolve_acos (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__acos_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_acosh (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__acosh_%c%d", gfc_type_letter (x->ts.type),
		      x->ts.kind);
}


void
gfc_resolve_aimag (gfc_expr *f, gfc_expr *x)
{
  f->ts.type = BT_REAL;
  f->ts.kind = x->ts.kind;
  f->value.function.name
    = gfc_get_string ("__aimag_%c%d", gfc_type_letter (x->ts.type),
		      x->ts.kind);
}


void
gfc_resolve_and (gfc_expr *f, gfc_expr *i, gfc_expr *j)
{
  f->ts.type = i->ts.type;
  f->ts.kind = gfc_kind_max (i, j);

  if (i->ts.kind != j->ts.kind)
    {
      if (i->ts.kind == gfc_kind_max (i, j))
	gfc_convert_type (j, &i->ts, 2);
      else
	gfc_convert_type (i, &j->ts, 2);
    }

  f->value.function.name
    = gfc_get_string ("__and_%c%d", gfc_type_letter (i->ts.type), f->ts.kind);
}


void
gfc_resolve_aint (gfc_expr *f, gfc_expr *a, gfc_expr *kind)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts.type = a->ts.type;
  f->ts.kind = (kind == NULL) ? a->ts.kind : mpz_get_si (kind->value.integer);

  if (a->ts.kind != f->ts.kind)
    {
      ts.type = f->ts.type;
      ts.kind = f->ts.kind;
      gfc_convert_type (a, &ts, 2);
    }
  /* The resolved name is only used for specific intrinsics where
     the return kind is the same as the arg kind.  */
  f->value.function.name
    = gfc_get_string ("__aint_%c%d", gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_dint (gfc_expr *f, gfc_expr *a)
{
  gfc_resolve_aint (f, a, NULL);
}


void
gfc_resolve_all (gfc_expr *f, gfc_expr *mask, gfc_expr *dim)
{
  f->ts = mask->ts;

  if (dim != NULL)
    {
      gfc_resolve_dim_arg (dim);
      f->rank = mask->rank - 1;
      f->shape = gfc_copy_shape_excluding (mask->shape, mask->rank, dim);
    }

  f->value.function.name
    = gfc_get_string (PREFIX ("all_%c%d"), gfc_type_letter (mask->ts.type),
		      mask->ts.kind);
}


void
gfc_resolve_anint (gfc_expr *f, gfc_expr *a, gfc_expr *kind)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts.type = a->ts.type;
  f->ts.kind = (kind == NULL) ? a->ts.kind : mpz_get_si (kind->value.integer);

  if (a->ts.kind != f->ts.kind)
    {
      ts.type = f->ts.type;
      ts.kind = f->ts.kind;
      gfc_convert_type (a, &ts, 2);
    }

  /* The resolved name is only used for specific intrinsics where
     the return kind is the same as the arg kind.  */
  f->value.function.name
    = gfc_get_string ("__anint_%c%d", gfc_type_letter (a->ts.type),
		      a->ts.kind);
}


void
gfc_resolve_dnint (gfc_expr *f, gfc_expr *a)
{
  gfc_resolve_anint (f, a, NULL);
}


void
gfc_resolve_any (gfc_expr *f, gfc_expr *mask, gfc_expr *dim)
{
  f->ts = mask->ts;

  if (dim != NULL)
    {
      gfc_resolve_dim_arg (dim);
      f->rank = mask->rank - 1;
      f->shape = gfc_copy_shape_excluding (mask->shape, mask->rank, dim);
    }

  f->value.function.name
    = gfc_get_string (PREFIX ("any_%c%d"), gfc_type_letter (mask->ts.type),
		      mask->ts.kind);
}


void
gfc_resolve_asin (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__asin_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}

void
gfc_resolve_asinh (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__asinh_%c%d", gfc_type_letter (x->ts.type),
		      x->ts.kind);
}

void
gfc_resolve_atan (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__atan_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}

void
gfc_resolve_atanh (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__atanh_%c%d", gfc_type_letter (x->ts.type),
		      x->ts.kind);
}

void
gfc_resolve_atan2 (gfc_expr *f, gfc_expr *x, gfc_expr *y ATTRIBUTE_UNUSED)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__atan2_%c%d", gfc_type_letter (x->ts.type),
		      x->ts.kind);
}


/* Resolve the BESYN and BESJN intrinsics.  */

void
gfc_resolve_besn (gfc_expr *f, gfc_expr *n, gfc_expr *x)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts = x->ts;
  if (n->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      gfc_convert_type (n, &ts, 2);
    }
  f->value.function.name = gfc_get_string ("<intrinsic>");
}


void
gfc_resolve_bessel_n2 (gfc_expr *f, gfc_expr *n1, gfc_expr *n2, gfc_expr *x)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts = x->ts;
  f->rank = 1;
  if (n1->expr_type == EXPR_CONSTANT && n2->expr_type == EXPR_CONSTANT)
    {
      f->shape = gfc_get_shape (1);
      mpz_init (f->shape[0]);
      mpz_sub (f->shape[0], n2->value.integer, n1->value.integer);
      mpz_add_ui (f->shape[0], f->shape[0], 1);
    }

  if (n1->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      gfc_convert_type (n1, &ts, 2);
    }

  if (n2->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      gfc_convert_type (n2, &ts, 2);
    }

  if (f->value.function.isym->id == GFC_ISYM_JN2)
    f->value.function.name = gfc_get_string (PREFIX ("bessel_jn_r%d"),
					     f->ts.kind);
  else
    f->value.function.name = gfc_get_string (PREFIX ("bessel_yn_r%d"),
					     f->ts.kind);
}


void
gfc_resolve_btest (gfc_expr *f, gfc_expr *i, gfc_expr *pos)
{
  f->ts.type = BT_LOGICAL;
  f->ts.kind = gfc_default_logical_kind;
  f->value.function.name
    = gfc_get_string ("__btest_%d_%d", i->ts.kind, pos->ts.kind);
}


void
gfc_resolve_c_loc (gfc_expr *f, gfc_expr *x ATTRIBUTE_UNUSED)
{
  f->ts = f->value.function.isym->ts;
}


void
gfc_resolve_c_funloc (gfc_expr *f, gfc_expr *x ATTRIBUTE_UNUSED)
{
  f->ts = f->value.function.isym->ts;
}


void
gfc_resolve_ceiling (gfc_expr *f, gfc_expr *a, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL)
	     ? gfc_default_integer_kind : mpz_get_si (kind->value.integer);
  f->value.function.name
    = gfc_get_string ("__ceiling_%d_%c%d", f->ts.kind,
		      gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_char (gfc_expr *f, gfc_expr *a, gfc_expr *kind)
{
  gfc_resolve_char_achar (f, a, kind, false);
}


void
gfc_resolve_chdir (gfc_expr *f, gfc_expr *d ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string (PREFIX ("chdir_i%d"), f->ts.kind);
}


void
gfc_resolve_chdir_sub (gfc_code *c)
{
  const char *name;
  int kind;

  if (c->ext.actual->next->expr != NULL)
    kind = c->ext.actual->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("chdir_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_chmod (gfc_expr *f, gfc_expr *name ATTRIBUTE_UNUSED,
		   gfc_expr *mode ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_c_int_kind;
  f->value.function.name = PREFIX ("chmod_func");
}


void
gfc_resolve_chmod_sub (gfc_code *c)
{
  const char *name;
  int kind;

  if (c->ext.actual->next->next->expr != NULL)
    kind = c->ext.actual->next->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("chmod_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_cmplx (gfc_expr *f, gfc_expr *x, gfc_expr *y, gfc_expr *kind)
{
  f->ts.type = BT_COMPLEX;
  f->ts.kind = (kind == NULL)
	     ? gfc_default_real_kind : mpz_get_si (kind->value.integer);

  if (y == NULL)
    f->value.function.name
      = gfc_get_string ("__cmplx0_%d_%c%d", f->ts.kind,
			gfc_type_letter (x->ts.type), x->ts.kind);
  else
    f->value.function.name
      = gfc_get_string ("__cmplx1_%d_%c%d_%c%d", f->ts.kind,
			gfc_type_letter (x->ts.type), x->ts.kind,
			gfc_type_letter (y->ts.type), y->ts.kind);
}


void
gfc_resolve_dcmplx (gfc_expr *f, gfc_expr *x, gfc_expr *y)
{
  gfc_resolve_cmplx (f, x, y, gfc_get_int_expr (gfc_default_integer_kind, NULL,
						gfc_default_double_kind));
}


void
gfc_resolve_complex (gfc_expr *f, gfc_expr *x, gfc_expr *y)
{
  int kind;

  if (x->ts.type == BT_INTEGER)
    {
      if (y->ts.type == BT_INTEGER)
	kind = gfc_default_real_kind;
      else
	kind = y->ts.kind;
    }
  else
    {
      if (y->ts.type == BT_REAL)
	kind = (x->ts.kind > y->ts.kind) ? x->ts.kind : y->ts.kind;
      else
	kind = x->ts.kind;
    }

  f->ts.type = BT_COMPLEX;
  f->ts.kind = kind;
  f->value.function.name
    = gfc_get_string ("__cmplx1_%d_%c%d_%c%d", f->ts.kind,
		      gfc_type_letter (x->ts.type), x->ts.kind,
		      gfc_type_letter (y->ts.type), y->ts.kind);
}


void
gfc_resolve_conjg (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__conjg_%d", x->ts.kind);
}


void
gfc_resolve_cos (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__cos_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_cosh (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__cosh_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


/* Our replacement of elements of a trig call with an EXPR_OP (e.g.
   multiplying the result or operands by a factor to convert to/from degrees)
   will cause the resolve_* function to be invoked again when resolving the
   freshly created EXPR_OP.  See gfc_resolve_trigd, gfc_resolve_atrigd,
   gfc_resolve_cotan.  We must observe this and avoid recursively creating
   layers of nested EXPR_OP expressions.  */

static bool
is_trig_resolved (gfc_expr *f)
{
  /* We know we've already resolved the function if we see the lib call
     starting with '__'.  */
  return (f->value.function.name != NULL
	  && gfc_str_startswith (f->value.function.name, "__"));
}

/* Return a shallow copy of the function expression f.  The original expression
   has its pointers cleared so that it may be freed without affecting the
   shallow copy.  This is similar to gfc_copy_expr, but doesn't perform a deep
   copy of the argument list, allowing it to be reused somewhere else,
   setting the expression up nicely for gfc_replace_expr.  */

static gfc_expr *
copy_replace_function_shallow (gfc_expr *f)
{
  gfc_expr *fcopy;
  gfc_actual_arglist *args;

  /* The only thing deep-copied in gfc_copy_expr is args.  */
  args = f->value.function.actual;
  f->value.function.actual = NULL;
  fcopy = gfc_copy_expr (f);
  fcopy->value.function.actual = args;

  /* Clear the old function so the shallow copy is not affected if the old
     expression is freed.  */
  f->value.function.name = NULL;
  f->value.function.isym = NULL;
  f->value.function.actual = NULL;
  f->value.function.esym = NULL;
  f->shape = NULL;
  f->ref = NULL;

  return fcopy;
}


/* Resolve cotan = cos / sin.  */

void
gfc_resolve_cotan (gfc_expr *f, gfc_expr *x)
{
  gfc_expr *result, *fcopy, *sin;
  gfc_actual_arglist *sin_args;

  if (is_trig_resolved (f))
    return;

  /* Compute cotan (x) = cos (x) / sin (x).  */
  f->value.function.isym = gfc_intrinsic_function_by_id (GFC_ISYM_COS);
  gfc_resolve_cos (f, x);

  sin_args = gfc_get_actual_arglist ();
  sin_args->expr = gfc_copy_expr (x);

  sin = gfc_get_expr ();
  sin->ts = f->ts;
  sin->where = f->where;
  sin->expr_type = EXPR_FUNCTION;
  sin->value.function.isym = gfc_intrinsic_function_by_id (GFC_ISYM_SIN);
  sin->value.function.actual = sin_args;
  gfc_resolve_sin (sin, sin_args->expr);

  /* Replace f with cos/sin - we do this in place in f for the caller.  */
  fcopy = copy_replace_function_shallow (f);
  result = gfc_divide (fcopy, sin);
  gfc_replace_expr (f, result);
}


void
gfc_resolve_count (gfc_expr *f, gfc_expr *mask, gfc_expr *dim, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;

  if (dim != NULL)
    {
      f->rank = mask->rank - 1;
      gfc_resolve_dim_arg (dim);
      f->shape = gfc_copy_shape_excluding (mask->shape, mask->rank, dim);
    }

  resolve_mask_arg (mask);

  f->value.function.name
    = gfc_get_string (PREFIX ("count_%d_%c"), f->ts.kind,
		      gfc_type_letter (mask->ts.type));
}


void
gfc_resolve_cshift (gfc_expr *f, gfc_expr *array, gfc_expr *shift,
		    gfc_expr *dim)
{
  int n, m;

  if (array->ts.type == BT_CHARACTER && array->ref)
    gfc_resolve_substring_charlen (array);

  f->ts = array->ts;
  f->rank = array->rank;
  f->shape = gfc_copy_shape (array->shape, array->rank);

  if (shift->rank > 0)
    n = 1;
  else
    n = 0;

  /* If dim kind is greater than default integer we need to use the larger.  */
  m = gfc_default_integer_kind;
  if (dim != NULL)
    m = m < dim->ts.kind ? dim->ts.kind : m;

  /* Convert shift to at least m, so we don't need
      kind=1 and kind=2 versions of the library functions.  */
  if (shift->ts.kind < m)
    {
      gfc_typespec ts;
      gfc_clear_ts (&ts);
      ts.type = BT_INTEGER;
      ts.kind = m;
      gfc_convert_type_warn (shift, &ts, 2, 0);
    }

  if (dim != NULL)
    {
      if (dim->expr_type != EXPR_CONSTANT && dim->symtree != NULL
	  && dim->symtree->n.sym->attr.optional)
	{
	  /* Mark this for later setting the type in gfc_conv_missing_dummy.  */
	  dim->representation.length = shift->ts.kind;
	}
      else
	{
	  gfc_resolve_dim_arg (dim);
	  /* Convert dim to shift's kind to reduce variations.  */
	  if (dim->ts.kind != shift->ts.kind)
	    gfc_convert_type_warn (dim, &shift->ts, 2, 0);
        }
    }

  if (array->ts.type == BT_CHARACTER)
    {
      if (array->ts.kind == gfc_default_character_kind)
	f->value.function.name
	  = gfc_get_string (PREFIX ("cshift%d_%d_char"), n, shift->ts.kind);
      else
	f->value.function.name
	  = gfc_get_string (PREFIX ("cshift%d_%d_char%d"), n, shift->ts.kind,
			    array->ts.kind);
    }
  else
    f->value.function.name
	= gfc_get_string (PREFIX ("cshift%d_%d"), n, shift->ts.kind);
}


void
gfc_resolve_ctime (gfc_expr *f, gfc_expr *time)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts.type = BT_CHARACTER;
  f->ts.kind = gfc_default_character_kind;

  /* ctime TIME argument is a INTEGER(KIND=8), says the doc */
  if (time->ts.kind != 8)
    {
      ts.type = BT_INTEGER;
      ts.kind = 8;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (time, &ts, 2);
    }

  f->value.function.name = gfc_get_string (PREFIX ("ctime"));
}


void
gfc_resolve_dble (gfc_expr *f, gfc_expr *a)
{
  f->ts.type = BT_REAL;
  f->ts.kind = gfc_default_double_kind;
  f->value.function.name
    = gfc_get_string ("__dble_%c%d", gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_dim (gfc_expr *f, gfc_expr *a, gfc_expr *p)
{
  f->ts.type = a->ts.type;
  if (p != NULL)
    f->ts.kind = gfc_kind_max (a,p);
  else
    f->ts.kind = a->ts.kind;

  if (p != NULL && a->ts.kind != p->ts.kind)
    {
      if (a->ts.kind == gfc_kind_max (a,p))
	gfc_convert_type (p, &a->ts, 2);
      else
	gfc_convert_type (a, &p->ts, 2);
    }

  f->value.function.name
    = gfc_get_string ("__dim_%c%d", gfc_type_letter (f->ts.type), f->ts.kind);
}


void
gfc_resolve_dot_product (gfc_expr *f, gfc_expr *a, gfc_expr *b)
{
  gfc_expr temp;

  temp.expr_type = EXPR_OP;
  gfc_clear_ts (&temp.ts);
  temp.value.op.op = INTRINSIC_NONE;
  temp.value.op.op1 = a;
  temp.value.op.op2 = b;
  gfc_type_convert_binary (&temp, 1);
  f->ts = temp.ts;
  f->value.function.name
    = gfc_get_string (PREFIX ("dot_product_%c%d"),
		      gfc_type_letter (f->ts.type), f->ts.kind);
}


void
gfc_resolve_dprod (gfc_expr *f, gfc_expr *a ATTRIBUTE_UNUSED,
		   gfc_expr *b ATTRIBUTE_UNUSED)
{
  f->ts.kind = gfc_default_double_kind;
  f->ts.type = BT_REAL;
  f->value.function.name = gfc_get_string ("__dprod_r%d", f->ts.kind);
}


void
gfc_resolve_dshift (gfc_expr *f, gfc_expr *i, gfc_expr *j ATTRIBUTE_UNUSED,
		    gfc_expr *shift ATTRIBUTE_UNUSED)
{
  f->ts = i->ts;
  if (f->value.function.isym->id == GFC_ISYM_DSHIFTL)
    f->value.function.name = gfc_get_string ("dshiftl_i%d", f->ts.kind);
  else if (f->value.function.isym->id == GFC_ISYM_DSHIFTR)
    f->value.function.name = gfc_get_string ("dshiftr_i%d", f->ts.kind);
  else
    gcc_unreachable ();
}


void
gfc_resolve_eoshift (gfc_expr *f, gfc_expr *array, gfc_expr *shift,
		     gfc_expr *boundary, gfc_expr *dim)
{
  int n, m;

  if (array->ts.type == BT_CHARACTER && array->ref)
    gfc_resolve_substring_charlen (array);

  f->ts = array->ts;
  f->rank = array->rank;
  f->shape = gfc_copy_shape (array->shape, array->rank);

  n = 0;
  if (shift->rank > 0)
    n = n | 1;
  if (boundary && boundary->rank > 0)
    n = n | 2;

  /* If dim kind is greater than default integer we need to use the larger.  */
  m = gfc_default_integer_kind;
  if (dim != NULL)
    m = m < dim->ts.kind ? dim->ts.kind : m;

  /* Convert shift to at least m, so we don't need
      kind=1 and kind=2 versions of the library functions.  */
  if (shift->ts.kind < m)
    {
      gfc_typespec ts;
      gfc_clear_ts (&ts);
      ts.type = BT_INTEGER;
      ts.kind = m;
      gfc_convert_type_warn (shift, &ts, 2, 0);
    }

  if (dim != NULL)
    {
      if (dim->expr_type != EXPR_CONSTANT && dim->symtree != NULL
	  && dim->symtree->n.sym->attr.optional)
	{
	  /* Mark this for later setting the type in gfc_conv_missing_dummy.  */
	  dim->representation.length = shift->ts.kind;
	}
      else
	{
	  gfc_resolve_dim_arg (dim);
	  /* Convert dim to shift's kind to reduce variations.  */
	  if (dim->ts.kind != shift->ts.kind)
	    gfc_convert_type_warn (dim, &shift->ts, 2, 0);
        }
    }

  if (array->ts.type == BT_CHARACTER)
    {
      if (array->ts.kind == gfc_default_character_kind)
	f->value.function.name
	  = gfc_get_string (PREFIX ("eoshift%d_%d_char"), n, shift->ts.kind);
      else
	f->value.function.name
	  = gfc_get_string (PREFIX ("eoshift%d_%d_char%d"), n, shift->ts.kind,
			    array->ts.kind);
    }
  else
    f->value.function.name
	= gfc_get_string (PREFIX ("eoshift%d_%d"), n, shift->ts.kind);
}


void
gfc_resolve_exp (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__exp_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_exponent (gfc_expr *f, gfc_expr *x)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__exponent_%d", x->ts.kind);
}


/* Resolve the EXTENDS_TYPE_OF intrinsic function.  */

void
gfc_resolve_extends_type_of (gfc_expr *f, gfc_expr *a, gfc_expr *mo)
{
  gfc_symbol *vtab;
  gfc_symtree *st;

  /* Prevent double resolution.  */
  if (f->ts.type == BT_LOGICAL)
    return;

  /* Replace the first argument with the corresponding vtab.  */
  if (a->ts.type == BT_CLASS)
    gfc_add_vptr_component (a);
  else if (a->ts.type == BT_DERIVED)
    {
      locus where;

      vtab = gfc_find_derived_vtab (a->ts.u.derived);
      /* Clear the old expr.  */
      gfc_free_ref_list (a->ref);
      where = a->where;
      memset (a, '\0', sizeof (gfc_expr));
      /* Construct a new one.  */
      a->expr_type = EXPR_VARIABLE;
      st = gfc_find_symtree (vtab->ns->sym_root, vtab->name);
      a->symtree = st;
      a->ts = vtab->ts;
      a->where = where;
    }

  /* Replace the second argument with the corresponding vtab.  */
  if (mo->ts.type == BT_CLASS)
    gfc_add_vptr_component (mo);
  else if (mo->ts.type == BT_DERIVED)
    {
      locus where;

      vtab = gfc_find_derived_vtab (mo->ts.u.derived);
      /* Clear the old expr.  */
      where = mo->where;
      gfc_free_ref_list (mo->ref);
      memset (mo, '\0', sizeof (gfc_expr));
      /* Construct a new one.  */
      mo->expr_type = EXPR_VARIABLE;
      st = gfc_find_symtree (vtab->ns->sym_root, vtab->name);
      mo->symtree = st;
      mo->ts = vtab->ts;
      mo->where = where;
    }

  f->ts.type = BT_LOGICAL;
  f->ts.kind = 4;

  f->value.function.isym->formal->ts = a->ts;
  f->value.function.isym->formal->next->ts = mo->ts;

  /* Call library function.  */
  f->value.function.name = gfc_get_string (PREFIX ("is_extension_of"));
}


void
gfc_resolve_fdate (gfc_expr *f)
{
  f->ts.type = BT_CHARACTER;
  f->ts.kind = gfc_default_character_kind;
  f->value.function.name = gfc_get_string (PREFIX ("fdate"));
}


void
gfc_resolve_floor (gfc_expr *f, gfc_expr *a, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL)
	     ? gfc_default_integer_kind : mpz_get_si (kind->value.integer);
  f->value.function.name
    = gfc_get_string ("__floor%d_%c%d", f->ts.kind,
		      gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_fnum (gfc_expr *f, gfc_expr *n)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  if (n->ts.kind != f->ts.kind)
    gfc_convert_type (n, &f->ts, 2);
  f->value.function.name = gfc_get_string (PREFIX ("fnum_i%d"), f->ts.kind);
}


void
gfc_resolve_fraction (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__fraction_%d", x->ts.kind);
}


/* Resolve single-argument g77 math intrinsics, eg BESY0, ERF.  */

void
gfc_resolve_g77_math1 (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("<intrinsic>");
}


void
gfc_resolve_gamma (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__tgamma_%d", x->ts.kind);
}


void
gfc_resolve_getcwd (gfc_expr *f, gfc_expr *n ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX ("getcwd"));
}


void
gfc_resolve_getgid (gfc_expr *f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX ("getgid"));
}


void
gfc_resolve_getpid (gfc_expr *f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX ("getpid"));
}


void
gfc_resolve_getuid (gfc_expr *f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX ("getuid"));
}


void
gfc_resolve_hostnm (gfc_expr *f, gfc_expr *n ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX ("hostnm"));
}


void
gfc_resolve_hypot (gfc_expr *f, gfc_expr *x, gfc_expr *y ATTRIBUTE_UNUSED)
{
  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__hypot_r%d", x->ts.kind);
}


void
gfc_resolve_iall (gfc_expr *f, gfc_expr *array, gfc_expr *dim, gfc_expr *mask)
{
  resolve_transformational ("iall", f, array, dim, mask);
}


void
gfc_resolve_iand (gfc_expr *f, gfc_expr *i, gfc_expr *j)
{
  /* If the kind of i and j are different, then g77 cross-promoted the
     kinds to the largest value.  The Fortran 95 standard requires the
     kinds to match.  */
  if (i->ts.kind != j->ts.kind)
    {
      if (i->ts.kind == gfc_kind_max (i, j))
	gfc_convert_type (j, &i->ts, 2);
      else
	gfc_convert_type (i, &j->ts, 2);
    }

  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__iand_%d", i->ts.kind);
}


void
gfc_resolve_iany (gfc_expr *f, gfc_expr *array, gfc_expr *dim, gfc_expr *mask)
{
  resolve_transformational ("iany", f, array, dim, mask);
}


void
gfc_resolve_ibclr (gfc_expr *f, gfc_expr *i, gfc_expr *pos ATTRIBUTE_UNUSED)
{
  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__ibclr_%d", i->ts.kind);
}


void
gfc_resolve_ibits (gfc_expr *f, gfc_expr *i, gfc_expr *pos ATTRIBUTE_UNUSED,
		   gfc_expr *len ATTRIBUTE_UNUSED)
{
  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__ibits_%d", i->ts.kind);
}


void
gfc_resolve_ibset (gfc_expr *f, gfc_expr *i, gfc_expr *pos ATTRIBUTE_UNUSED)
{
  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__ibset_%d", i->ts.kind);
}


void
gfc_resolve_iachar (gfc_expr *f, gfc_expr *c, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__ichar_%d", c->ts.kind);
}


void
gfc_resolve_ichar (gfc_expr *f, gfc_expr *c, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__ichar_%d", c->ts.kind);
}


void
gfc_resolve_idnint (gfc_expr *f, gfc_expr *a)
{
  gfc_resolve_nint (f, a, NULL);
}


void
gfc_resolve_ierrno (gfc_expr *f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string (PREFIX ("ierrno_i%d"), f->ts.kind);
}


void
gfc_resolve_ieor (gfc_expr *f, gfc_expr *i, gfc_expr *j)
{
  /* If the kind of i and j are different, then g77 cross-promoted the
     kinds to the largest value.  The Fortran 95 standard requires the
     kinds to match.  */
  if (i->ts.kind != j->ts.kind)
    {
      if (i->ts.kind == gfc_kind_max (i, j))
	gfc_convert_type (j, &i->ts, 2);
      else
	gfc_convert_type (i, &j->ts, 2);
    }

  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__ieor_%d", i->ts.kind);
}


void
gfc_resolve_ior (gfc_expr *f, gfc_expr *i, gfc_expr *j)
{
  /* If the kind of i and j are different, then g77 cross-promoted the
     kinds to the largest value.  The Fortran 95 standard requires the
     kinds to match.  */
  if (i->ts.kind != j->ts.kind)
    {
      if (i->ts.kind == gfc_kind_max (i, j))
	gfc_convert_type (j, &i->ts, 2);
      else
	gfc_convert_type (i, &j->ts, 2);
    }

  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__ior_%d", i->ts.kind);
}


void
gfc_resolve_index_func (gfc_expr *f, gfc_expr *str,
			gfc_expr *sub_str ATTRIBUTE_UNUSED, gfc_expr *back,
			gfc_expr *kind)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;

  if (back && back->ts.kind != gfc_default_integer_kind)
    {
      ts.type = BT_LOGICAL;
      ts.kind = gfc_default_integer_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (back, &ts, 2);
    }

  f->value.function.name
    = gfc_get_string ("__index_%d_i%d", str->ts.kind, f->ts.kind);
}


void
gfc_resolve_int (gfc_expr *f, gfc_expr *a, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL)
	     ? gfc_default_integer_kind : mpz_get_si (kind->value.integer);
  f->value.function.name
    = gfc_get_string ("__int_%d_%c%d", f->ts.kind,
		      gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_int2 (gfc_expr *f, gfc_expr *a)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 2;
  f->value.function.name
    = gfc_get_string ("__int_%d_%c%d", f->ts.kind,
		      gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_int8 (gfc_expr *f, gfc_expr *a)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 8;
  f->value.function.name
    = gfc_get_string ("__int_%d_%c%d", f->ts.kind,
		      gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_long (gfc_expr *f, gfc_expr *a)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name
    = gfc_get_string ("__int_%d_%c%d", f->ts.kind,
		      gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_iparity (gfc_expr *f, gfc_expr *array, gfc_expr *dim, gfc_expr *mask)
{
  resolve_transformational ("iparity", f, array, dim, mask);
}


void
gfc_resolve_isatty (gfc_expr *f, gfc_expr *u)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts.type = BT_LOGICAL;
  f->ts.kind = gfc_default_integer_kind;
  if (u->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (u, &ts, 2);
    }

  f->value.function.name = gfc_get_string (PREFIX ("isatty_l%d"), f->ts.kind);
}


void
gfc_resolve_is_contiguous (gfc_expr *f, gfc_expr *array ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_LOGICAL;
  f->ts.kind = gfc_default_logical_kind;
  f->value.function.name = gfc_get_string ("__is_contiguous");
}


void
gfc_resolve_ishft (gfc_expr *f, gfc_expr *i, gfc_expr *shift)
{
  f->ts = i->ts;
  f->value.function.name
    = gfc_get_string ("__ishft_%d_%d", i->ts.kind, shift->ts.kind);
}


void
gfc_resolve_rshift (gfc_expr *f, gfc_expr *i, gfc_expr *shift)
{
  f->ts = i->ts;
  f->value.function.name
    = gfc_get_string ("__rshift_%d_%d", i->ts.kind, shift->ts.kind);
}


void
gfc_resolve_lshift (gfc_expr *f, gfc_expr *i, gfc_expr *shift)
{
  f->ts = i->ts;
  f->value.function.name
    = gfc_get_string ("__lshift_%d_%d", i->ts.kind, shift->ts.kind);
}


void
gfc_resolve_ishftc (gfc_expr *f, gfc_expr *i, gfc_expr *shift, gfc_expr *size)
{
  int s_kind;

  s_kind = (size == NULL) ? gfc_default_integer_kind : size->ts.kind;

  f->ts = i->ts;
  f->value.function.name
    = gfc_get_string ("__ishftc_%d_%d_%d", i->ts.kind, shift->ts.kind, s_kind);
}


void
gfc_resolve_lbound (gfc_expr *f, gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  resolve_bound (f, array, dim, kind, "__lbound", false);
}


void
gfc_resolve_lcobound (gfc_expr *f, gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  resolve_bound (f, array, dim, kind, "__lcobound", true);
}


void
gfc_resolve_len (gfc_expr *f, gfc_expr *string, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;
  f->value.function.name
    = gfc_get_string ("__len_%d_i%d", string->ts.kind,
		      gfc_default_integer_kind);
}


void
gfc_resolve_len_trim (gfc_expr *f, gfc_expr *string, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__len_trim%d", string->ts.kind);
}


void
gfc_resolve_lgamma (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__lgamma_%d", x->ts.kind);
}


void
gfc_resolve_link (gfc_expr *f, gfc_expr *p1 ATTRIBUTE_UNUSED,
		  gfc_expr *p2 ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string (PREFIX ("link_i%d"), f->ts.kind);
}


void
gfc_resolve_loc (gfc_expr *f, gfc_expr *x)
{
  f->ts.type= BT_INTEGER;
  f->ts.kind = gfc_index_integer_kind;
  f->value.function.name = gfc_get_string ("__loc_%d", x->ts.kind);
}


void
gfc_resolve_log (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__log_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_log10 (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__log10_%c%d", gfc_type_letter (x->ts.type),
		      x->ts.kind);
}


void
gfc_resolve_logical (gfc_expr *f, gfc_expr *a, gfc_expr *kind)
{
  f->ts.type = BT_LOGICAL;
  f->ts.kind = (kind == NULL)
	     ? gfc_default_logical_kind : mpz_get_si (kind->value.integer);
  f->rank = a->rank;

  f->value.function.name
    = gfc_get_string ("__logical_%d_%c%d", f->ts.kind,
		      gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_matmul (gfc_expr *f, gfc_expr *a, gfc_expr *b)
{
  gfc_expr temp;

  if (a->ts.type == BT_LOGICAL && b->ts.type == BT_LOGICAL)
    {
      f->ts.type = BT_LOGICAL;
      f->ts.kind = gfc_default_logical_kind;
    }
  else
    {
      temp.expr_type = EXPR_OP;
      gfc_clear_ts (&temp.ts);
      temp.value.op.op = INTRINSIC_NONE;
      temp.value.op.op1 = a;
      temp.value.op.op2 = b;
      gfc_type_convert_binary (&temp, 1);
      f->ts = temp.ts;
    }

  f->rank = (a->rank == 2 && b->rank == 2) ? 2 : 1;

  if (a->rank == 2 && b->rank == 2)
    {
      if (a->shape && b->shape)
	{
	  f->shape = gfc_get_shape (f->rank);
	  mpz_init_set (f->shape[0], a->shape[0]);
	  mpz_init_set (f->shape[1], b->shape[1]);
	}
    }
  else if (a->rank == 1)
    {
      if (b->shape)
	{
	  f->shape = gfc_get_shape (f->rank);
	  mpz_init_set (f->shape[0], b->shape[1]);
	}
    }
  else
    {
      /* b->rank == 1 and a->rank == 2 here, all other cases have
	 been caught in check.c.   */
      if (a->shape)
	{
	  f->shape = gfc_get_shape (f->rank);
	  mpz_init_set (f->shape[0], a->shape[0]);
	}
    }

  f->value.function.name
    = gfc_get_string (PREFIX ("matmul_%c%d"), gfc_type_letter (f->ts.type),
		      f->ts.kind);
}


static void
gfc_resolve_minmax (const char *name, gfc_expr *f, gfc_actual_arglist *args)
{
  gfc_actual_arglist *a;

  f->ts.type = args->expr->ts.type;
  f->ts.kind = args->expr->ts.kind;
  /* Find the largest type kind.  */
  for (a = args->next; a; a = a->next)
    {
      if (a->expr->ts.kind > f->ts.kind)
	f->ts.kind = a->expr->ts.kind;
    }

  /* Convert all parameters to the required kind.  */
  for (a = args; a; a = a->next)
    {
      if (a->expr->ts.kind != f->ts.kind)
	gfc_convert_type (a->expr, &f->ts, 2);
    }

  f->value.function.name
    = gfc_get_string (name, gfc_type_letter (f->ts.type), f->ts.kind);
}


void
gfc_resolve_max (gfc_expr *f, gfc_actual_arglist *args)
{
  gfc_resolve_minmax ("__max_%c%d", f, args);
}

/* The smallest kind for which a minloc and maxloc implementation exists.  */

#define MINMAXLOC_MIN_KIND 4

void
gfc_resolve_maxloc (gfc_expr *f, gfc_expr *array, gfc_expr *dim,
		    gfc_expr *mask, gfc_expr *kind, gfc_expr *back)
{
  const char *name;
  int i, j, idim;
  int fkind;
  int d_num;

  f->ts.type = BT_INTEGER;

  /* The library versions only exist for kinds 4, 8 and 16. For smaller kinds,
     we do a type conversion further down.  */
  if (kind)
    fkind = mpz_get_si (kind->value.integer);
  else
    fkind = gfc_default_integer_kind;

  if (fkind < MINMAXLOC_MIN_KIND)
    f->ts.kind = MINMAXLOC_MIN_KIND;
  else
    f->ts.kind = fkind;

  if (dim == NULL)
    {
      f->rank = 1;
      f->shape = gfc_get_shape (1);
      mpz_init_set_si (f->shape[0], array->rank);
    }
  else
    {
      f->rank = array->rank - 1;
      gfc_resolve_dim_arg (dim);
      if (array->shape && dim->expr_type == EXPR_CONSTANT)
	{
	  idim = (int) mpz_get_si (dim->value.integer);
	  f->shape = gfc_get_shape (f->rank);
	  for (i = 0, j = 0; i < f->rank; i++, j++)
	    {
	      if (i == (idim - 1))
		j++;
	      mpz_init_set (f->shape[i], array->shape[j]);
	    }
	}
    }

  if (mask)
    {
      if (mask->rank == 0)
	name = "smaxloc";
      else
	name = "mmaxloc";

      resolve_mask_arg (mask);
    }
  else
    name = "maxloc";

  if (dim)
    {
      if (array->ts.type != BT_CHARACTER || f->rank != 0)
	d_num = 1;
      else
	d_num = 2;
    }
  else
    d_num = 0;

  f->value.function.name
    = gfc_get_string (PREFIX ("%s%d_%d_%c%d"), name, d_num, f->ts.kind,
		      gfc_type_letter (array->ts.type), array->ts.kind);

  if (kind)
    fkind = mpz_get_si (kind->value.integer);
  else
    fkind = gfc_default_integer_kind;

  if (fkind != f->ts.kind)
    {
      gfc_typespec ts;
      gfc_clear_ts (&ts);

      ts.type = BT_INTEGER;
      ts.kind = fkind;
      gfc_convert_type_warn (f, &ts, 2, 0);
    }

  if (back->ts.kind != gfc_logical_4_kind)
    {
      gfc_typespec ts;
      gfc_clear_ts (&ts);
      ts.type = BT_LOGICAL;
      ts.kind = gfc_logical_4_kind;
      gfc_convert_type_warn (back, &ts, 2, 0);
    }
}


void
gfc_resolve_findloc (gfc_expr *f, gfc_expr *array, gfc_expr *value,
		     gfc_expr *dim, gfc_expr *mask, gfc_expr *kind,
		     gfc_expr *back)
{
  const char *name;
  int i, j, idim;
  int fkind;
  int d_num;

  /* See at the end of the function for why this is necessary.  */

  if (f->do_not_resolve_again)
    return;

  f->ts.type = BT_INTEGER;

  /* We have a single library version, which uses index_type.  */

  if (kind)
    fkind = mpz_get_si (kind->value.integer);
  else
    fkind = gfc_default_integer_kind;

  f->ts.kind = gfc_index_integer_kind;

  /* Convert value.  If array is not LOGICAL and value is, we already
     issued an error earlier.  */

  if ((array->ts.type != value->ts.type && value->ts.type != BT_LOGICAL)
      || array->ts.kind != value->ts.kind)
    gfc_convert_type_warn (value, &array->ts, 2, 0);

  if (dim == NULL)
    {
      f->rank = 1;
      f->shape = gfc_get_shape (1);
      mpz_init_set_si (f->shape[0], array->rank);
    }
  else
    {
      f->rank = array->rank - 1;
      gfc_resolve_dim_arg (dim);
      if (array->shape && dim->expr_type == EXPR_CONSTANT)
	{
	  idim = (int) mpz_get_si (dim->value.integer);
	  f->shape = gfc_get_shape (f->rank);
	  for (i = 0, j = 0; i < f->rank; i++, j++)
	    {
	      if (i == (idim - 1))
		j++;
	      mpz_init_set (f->shape[i], array->shape[j]);
	    }
	}
    }

  if (mask)
    {
      if (mask->rank == 0)
	name = "sfindloc";
      else
	name = "mfindloc";

      resolve_mask_arg (mask);
    }
  else
    name = "findloc";

  if (dim)
    {
      if (f->rank > 0)
	d_num = 1;
      else
	d_num = 2;
    }
  else
    d_num = 0;

  if (back->ts.kind != gfc_logical_4_kind)
    {
      gfc_typespec ts;
      gfc_clear_ts (&ts);
      ts.type = BT_LOGICAL;
      ts.kind = gfc_logical_4_kind;
      gfc_convert_type_warn (back, &ts, 2, 0);
    }

  f->value.function.name
    = gfc_get_string (PREFIX ("%s%d_%c%d"), name, d_num,
		      gfc_type_letter (array->ts.type, true), array->ts.kind);

  /* We only have a single library function, so we need to convert
     here.  If the function is resolved from within a convert
     function generated on a previous round of resolution, endless
     recursion could occur.  Guard against that here.  */

  if (f->ts.kind != fkind)
    {
      f->do_not_resolve_again = 1;
      gfc_typespec ts;
      gfc_clear_ts (&ts);

      ts.type = BT_INTEGER;
      ts.kind = fkind;
      gfc_convert_type_warn (f, &ts, 2, 0);
    }

}

void
gfc_resolve_maxval (gfc_expr *f, gfc_expr *array, gfc_expr *dim,
		    gfc_expr *mask)
{
  const char *name;
  int i, j, idim;

  f->ts = array->ts;

  if (dim != NULL)
    {
      f->rank = array->rank - 1;
      gfc_resolve_dim_arg (dim);

      if (f->rank && array->shape && dim->expr_type == EXPR_CONSTANT)
	{
	  idim = (int) mpz_get_si (dim->value.integer);
	  f->shape = gfc_get_shape (f->rank);
	  for (i = 0, j = 0; i < f->rank; i++, j++)
	    {
	      if (i == (idim - 1))
		j++;
	      mpz_init_set (f->shape[i], array->shape[j]);
	    }
	}
    }

  if (mask)
    {
      if (mask->rank == 0)
	name = "smaxval";
      else
	name = "mmaxval";

      resolve_mask_arg (mask);
    }
  else
    name = "maxval";

  if (array->ts.type != BT_CHARACTER)
    f->value.function.name
      = gfc_get_string (PREFIX ("%s_%c%d"), name,
			gfc_type_letter (array->ts.type), array->ts.kind);
  else
    f->value.function.name
      = gfc_get_string (PREFIX ("%s%d_%c%d"), name, f->rank != 0,
			gfc_type_letter (array->ts.type), array->ts.kind);     
}


void
gfc_resolve_mclock (gfc_expr *f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = PREFIX ("mclock");
}


void
gfc_resolve_mclock8 (gfc_expr *f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 8;
  f->value.function.name = PREFIX ("mclock8");
}


void
gfc_resolve_mask (gfc_expr *f, gfc_expr *i ATTRIBUTE_UNUSED,
		  gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = kind ? mpz_get_si (kind->value.integer)
		    : gfc_default_integer_kind;

  if (f->value.function.isym->id == GFC_ISYM_MASKL)
    f->value.function.name = gfc_get_string ("__maskl_i%d", f->ts.kind);
  else
    f->value.function.name = gfc_get_string ("__maskr_i%d", f->ts.kind);
}


void
gfc_resolve_merge (gfc_expr *f, gfc_expr *tsource,
		   gfc_expr *fsource ATTRIBUTE_UNUSED,
		   gfc_expr *mask ATTRIBUTE_UNUSED)
{
  if (tsource->ts.type == BT_CHARACTER && tsource->ref)
    gfc_resolve_substring_charlen (tsource);

  if (fsource->ts.type == BT_CHARACTER && fsource->ref)
    gfc_resolve_substring_charlen (fsource);

  if (tsource->ts.type == BT_CHARACTER)
    check_charlen_present (tsource);

  f->ts = tsource->ts;
  f->value.function.name
    = gfc_get_string ("__merge_%c%d", gfc_type_letter (tsource->ts.type),
		      tsource->ts.kind);
}


void
gfc_resolve_merge_bits (gfc_expr *f, gfc_expr *i,
			gfc_expr *j ATTRIBUTE_UNUSED,
			gfc_expr *mask ATTRIBUTE_UNUSED)
{
  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__merge_bits_i%d", i->ts.kind);
}


void
gfc_resolve_min (gfc_expr *f, gfc_actual_arglist *args)
{
  gfc_resolve_minmax ("__min_%c%d", f, args);
}


void
gfc_resolve_minloc (gfc_expr *f, gfc_expr *array, gfc_expr *dim,
		    gfc_expr *mask, gfc_expr *kind, gfc_expr *back)
{
  const char *name;
  int i, j, idim;
  int fkind;
  int d_num;

  f->ts.type = BT_INTEGER;

  /* The library versions only exist for kinds 4, 8 and 16. For smaller kinds,
     we do a type conversion further down.  */
  if (kind)
    fkind = mpz_get_si (kind->value.integer);
  else
    fkind = gfc_default_integer_kind;

  if (fkind < MINMAXLOC_MIN_KIND)
    f->ts.kind = MINMAXLOC_MIN_KIND;
  else
    f->ts.kind = fkind;

  if (dim == NULL)
    {
      f->rank = 1;
      f->shape = gfc_get_shape (1);
      mpz_init_set_si (f->shape[0], array->rank);
    }
  else
    {
      f->rank = array->rank - 1;
      gfc_resolve_dim_arg (dim);
      if (array->shape && dim->expr_type == EXPR_CONSTANT)
	{
	  idim = (int) mpz_get_si (dim->value.integer);
	  f->shape = gfc_get_shape (f->rank);
	  for (i = 0, j = 0; i < f->rank; i++, j++)
	    {
	      if (i == (idim - 1))
		j++;
	      mpz_init_set (f->shape[i], array->shape[j]);
	    }
	}
    }

  if (mask)
    {
      if (mask->rank == 0)
	name = "sminloc";
      else
	name = "mminloc";

      resolve_mask_arg (mask);
    }
  else
    name = "minloc";

  if (dim)
    {
      if (array->ts.type != BT_CHARACTER || f->rank != 0)
	d_num = 1;
      else
	d_num = 2;
    }
  else
    d_num = 0;

  f->value.function.name
    = gfc_get_string (PREFIX ("%s%d_%d_%c%d"), name, d_num, f->ts.kind,
		      gfc_type_letter (array->ts.type), array->ts.kind);

  if (fkind != f->ts.kind)
    {
      gfc_typespec ts;
      gfc_clear_ts (&ts);

      ts.type = BT_INTEGER;
      ts.kind = fkind;
      gfc_convert_type_warn (f, &ts, 2, 0);
    }

  if (back->ts.kind != gfc_logical_4_kind)
    {
      gfc_typespec ts;
      gfc_clear_ts (&ts);
      ts.type = BT_LOGICAL;
      ts.kind = gfc_logical_4_kind;
      gfc_convert_type_warn (back, &ts, 2, 0);
    }
}


void
gfc_resolve_minval (gfc_expr *f, gfc_expr *array, gfc_expr *dim,
		    gfc_expr *mask)
{
  const char *name;
  int i, j, idim;

  f->ts = array->ts;

  if (dim != NULL)
    {
      f->rank = array->rank - 1;
      gfc_resolve_dim_arg (dim);

      if (f->rank && array->shape && dim->expr_type == EXPR_CONSTANT)
	{
	  idim = (int) mpz_get_si (dim->value.integer);
	  f->shape = gfc_get_shape (f->rank);
	  for (i = 0, j = 0; i < f->rank; i++, j++)
	    {
	      if (i == (idim - 1))
		j++;
	      mpz_init_set (f->shape[i], array->shape[j]);
	    }
	}
    }

  if (mask)
    {
      if (mask->rank == 0)
	name = "sminval";
      else
	name = "mminval";

      resolve_mask_arg (mask);
    }
  else
    name = "minval";

  if (array->ts.type != BT_CHARACTER)
    f->value.function.name
      = gfc_get_string (PREFIX ("%s_%c%d"), name,
			gfc_type_letter (array->ts.type), array->ts.kind);
  else
    f->value.function.name
      = gfc_get_string (PREFIX ("%s%d_%c%d"), name, f->rank != 0,
			gfc_type_letter (array->ts.type), array->ts.kind);     
}


void
gfc_resolve_mod (gfc_expr *f, gfc_expr *a, gfc_expr *p)
{
  f->ts.type = a->ts.type;
  if (p != NULL)
    f->ts.kind = gfc_kind_max (a,p);
  else
    f->ts.kind = a->ts.kind;

  if (p != NULL && a->ts.kind != p->ts.kind)
    {
      if (a->ts.kind == gfc_kind_max (a,p))
	gfc_convert_type (p, &a->ts, 2);
      else
	gfc_convert_type (a, &p->ts, 2);
    }

  f->value.function.name
    = gfc_get_string ("__mod_%c%d", gfc_type_letter (f->ts.type), f->ts.kind);
}


void
gfc_resolve_modulo (gfc_expr *f, gfc_expr *a, gfc_expr *p)
{
  f->ts.type = a->ts.type;
  if (p != NULL)
    f->ts.kind = gfc_kind_max (a,p);
  else
    f->ts.kind = a->ts.kind;

  if (p != NULL && a->ts.kind != p->ts.kind)
    {
      if (a->ts.kind == gfc_kind_max (a,p))
	gfc_convert_type (p, &a->ts, 2);
      else
	gfc_convert_type (a, &p->ts, 2);
    }

  f->value.function.name
    = gfc_get_string ("__modulo_%c%d", gfc_type_letter (f->ts.type),
		      f->ts.kind);
}

void
gfc_resolve_nearest (gfc_expr *f, gfc_expr *a, gfc_expr *p)
{
  if (p->ts.kind != a->ts.kind)
    gfc_convert_type (p, &a->ts, 2);

  f->ts = a->ts;
  f->value.function.name
    = gfc_get_string ("__nearest_%c%d", gfc_type_letter (a->ts.type),
		      a->ts.kind);
}

void
gfc_resolve_nint (gfc_expr *f, gfc_expr *a, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL)
	     ? gfc_default_integer_kind : mpz_get_si (kind->value.integer);
  f->value.function.name
    = gfc_get_string ("__nint_%d_%d", f->ts.kind, a->ts.kind);
}


void
gfc_resolve_norm2 (gfc_expr *f, gfc_expr *array, gfc_expr *dim)
{
  resolve_transformational ("norm2", f, array, dim, NULL);
}


void
gfc_resolve_not (gfc_expr *f, gfc_expr *i)
{
  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__not_%d", i->ts.kind);
}


void
gfc_resolve_or (gfc_expr *f, gfc_expr *i, gfc_expr *j)
{
  f->ts.type = i->ts.type;
  f->ts.kind = gfc_kind_max (i, j);

  if (i->ts.kind != j->ts.kind)
    {
      if (i->ts.kind == gfc_kind_max (i, j))
	gfc_convert_type (j, &i->ts, 2);
      else
	gfc_convert_type (i, &j->ts, 2);
    }

  f->value.function.name
    = gfc_get_string ("__or_%c%d", gfc_type_letter (i->ts.type), f->ts.kind);
}


void
gfc_resolve_pack (gfc_expr *f, gfc_expr *array, gfc_expr *mask,
		  gfc_expr *vector ATTRIBUTE_UNUSED)
{
  if (array->ts.type == BT_CHARACTER && array->ref)
    gfc_resolve_substring_charlen (array);

  f->ts = array->ts;
  f->rank = 1;

  resolve_mask_arg (mask);

  if (mask->rank != 0)
    {
      if (array->ts.type == BT_CHARACTER)
	f->value.function.name
	  = array->ts.kind == 1 ? PREFIX ("pack_char")
				: gfc_get_string
					(PREFIX ("pack_char%d"),
					 array->ts.kind);
      else
	f->value.function.name = PREFIX ("pack");
    }
  else
    {
      if (array->ts.type == BT_CHARACTER)
	f->value.function.name
	  = array->ts.kind == 1 ? PREFIX ("pack_s_char")
				: gfc_get_string
					(PREFIX ("pack_s_char%d"),
					 array->ts.kind);
      else
	f->value.function.name = PREFIX ("pack_s");
    }
}


void
gfc_resolve_parity (gfc_expr *f, gfc_expr *array, gfc_expr *dim)
{
  resolve_transformational ("parity", f, array, dim, NULL);
}


void
gfc_resolve_product (gfc_expr *f, gfc_expr *array, gfc_expr *dim,
		     gfc_expr *mask)
{
  resolve_transformational ("product", f, array, dim, mask);
}


void
gfc_resolve_rank (gfc_expr *f, gfc_expr *array ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__rank");
}


void
gfc_resolve_real (gfc_expr *f, gfc_expr *a, gfc_expr *kind)
{
  f->ts.type = BT_REAL;

  if (kind != NULL)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = (a->ts.type == BT_COMPLEX)
	       ? a->ts.kind : gfc_default_real_kind;

  f->value.function.name
    = gfc_get_string ("__real_%d_%c%d", f->ts.kind,
		      gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_realpart (gfc_expr *f, gfc_expr *a)
{
  f->ts.type = BT_REAL;
  f->ts.kind = a->ts.kind;
  f->value.function.name
    = gfc_get_string ("__real_%d_%c%d", f->ts.kind,
		      gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_rename (gfc_expr *f, gfc_expr *p1 ATTRIBUTE_UNUSED,
		    gfc_expr *p2 ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string (PREFIX ("rename_i%d"), f->ts.kind);
}


void
gfc_resolve_repeat (gfc_expr *f, gfc_expr *string,
		    gfc_expr *ncopies)
{
  gfc_expr *tmp;
  f->ts.type = BT_CHARACTER;
  f->ts.kind = string->ts.kind;
  f->value.function.name = gfc_get_string ("__repeat_%d", string->ts.kind);

  /* If possible, generate a character length.  */
  if (f->ts.u.cl == NULL)
    f->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);

  tmp = NULL;
  if (string->expr_type == EXPR_CONSTANT)
    {
      tmp = gfc_get_int_expr (gfc_charlen_int_kind, NULL,
			      string->value.character.length);
    }
  else if (string->ts.u.cl && string->ts.u.cl->length)
    {
      tmp = gfc_copy_expr (string->ts.u.cl->length);
    }

  if (tmp)
    f->ts.u.cl->length = gfc_multiply (tmp, gfc_copy_expr (ncopies));
}


void
gfc_resolve_reshape (gfc_expr *f, gfc_expr *source, gfc_expr *shape,
		     gfc_expr *pad ATTRIBUTE_UNUSED,
		     gfc_expr *order ATTRIBUTE_UNUSED)
{
  mpz_t rank;
  int kind;
  int i;

  if (source->ts.type == BT_CHARACTER && source->ref)
    gfc_resolve_substring_charlen (source);

  f->ts = source->ts;

  gfc_array_size (shape, &rank);
  f->rank = mpz_get_si (rank);
  mpz_clear (rank);
  switch (source->ts.type)
    {
    case BT_COMPLEX:
    case BT_REAL:
    case BT_INTEGER:
    case BT_LOGICAL:
    case BT_CHARACTER:
      kind = source->ts.kind;
      break;

    default:
      kind = 0;
      break;
    }

  switch (kind)
    {
    case 4:
    case 8:
    case 10:
    case 16:
      if (source->ts.type == BT_COMPLEX || source->ts.type == BT_REAL)
	f->value.function.name
	  = gfc_get_string (PREFIX ("reshape_%c%d"),
			    gfc_type_letter (source->ts.type),
			    source->ts.kind);
      else if (source->ts.type == BT_CHARACTER)
	f->value.function.name = gfc_get_string (PREFIX ("reshape_char%d"),
						 kind);
      else
	f->value.function.name
	  = gfc_get_string (PREFIX ("reshape_%d"), source->ts.kind);
      break;

    default:
      f->value.function.name = (source->ts.type == BT_CHARACTER
				? PREFIX ("reshape_char") : PREFIX ("reshape"));
      break;
    }

  if (shape->expr_type == EXPR_ARRAY && gfc_is_constant_expr (shape))
    {
      gfc_constructor *c;
      f->shape = gfc_get_shape (f->rank);
      c = gfc_constructor_first (shape->value.constructor);
      for (i = 0; i < f->rank; i++)
	{
	  mpz_init_set (f->shape[i], c->expr->value.integer);
	  c = gfc_constructor_next (c);
	}
    }

  /* Force-convert both SHAPE and ORDER to index_kind so that we don't need
     so many runtime variations.  */
  if (shape->ts.kind != gfc_index_integer_kind)
    {
      gfc_typespec ts = shape->ts;
      ts.kind = gfc_index_integer_kind;
      gfc_convert_type_warn (shape, &ts, 2, 0);
    }
  if (order && order->ts.kind != gfc_index_integer_kind)
    gfc_convert_type_warn (order, &shape->ts, 2, 0);
}


void
gfc_resolve_rrspacing (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__rrspacing_%d", x->ts.kind);
}

void
gfc_resolve_fe_runtime_error (gfc_code *c)
{
  const char *name;
  gfc_actual_arglist *a;

  name = gfc_get_string (PREFIX ("runtime_error"));

  for (a = c->ext.actual->next; a; a = a->next)
    a->name = "%VAL";

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
  /* We set the backend_decl here because runtime_error is a
     variadic function and we would use the wrong calling
     convention otherwise.  */
  c->resolved_sym->backend_decl = gfor_fndecl_runtime_error;
}

void
gfc_resolve_scale (gfc_expr *f, gfc_expr *x, gfc_expr *i ATTRIBUTE_UNUSED)
{
  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__scale_%d", x->ts.kind);
}


void
gfc_resolve_scan (gfc_expr *f, gfc_expr *string,
		  gfc_expr *set ATTRIBUTE_UNUSED,
		  gfc_expr *back ATTRIBUTE_UNUSED, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__scan_%d", string->ts.kind);
}


void
gfc_resolve_secnds (gfc_expr *t1, gfc_expr *t0)
{
  t1->ts = t0->ts;
  t1->value.function.name = gfc_get_string (PREFIX ("secnds"));
}


void
gfc_resolve_set_exponent (gfc_expr *f, gfc_expr *x,
			  gfc_expr *i ATTRIBUTE_UNUSED)
{
  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__set_exponent_%d", x->ts.kind);
}


void
gfc_resolve_shape (gfc_expr *f, gfc_expr *array, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;

  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;

  f->rank = 1;
  if (array->rank != -1)
    {
      f->shape = gfc_get_shape (1);
      mpz_init_set_ui (f->shape[0], array->rank);
    }

  f->value.function.name = gfc_get_string (PREFIX ("shape_%d"), f->ts.kind);
}


void
gfc_resolve_shift (gfc_expr *f, gfc_expr *i, gfc_expr *shift ATTRIBUTE_UNUSED)
{
  f->ts = i->ts;
  if (f->value.function.isym->id == GFC_ISYM_SHIFTA)
    f->value.function.name = gfc_get_string ("shifta_i%d", f->ts.kind);
  else if (f->value.function.isym->id == GFC_ISYM_SHIFTL)
    f->value.function.name = gfc_get_string ("shiftl_i%d", f->ts.kind);
  else if (f->value.function.isym->id == GFC_ISYM_SHIFTR)
    f->value.function.name = gfc_get_string ("shiftr_i%d", f->ts.kind);
  else
    gcc_unreachable ();
}


void
gfc_resolve_sign (gfc_expr *f, gfc_expr *a, gfc_expr *b ATTRIBUTE_UNUSED)
{
  f->ts = a->ts;
  f->value.function.name
    = gfc_get_string ("__sign_%c%d", gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_signal (gfc_expr *f, gfc_expr *number, gfc_expr *handler)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_c_int_kind;

  /* handler can be either BT_INTEGER or BT_PROCEDURE  */
  if (handler->ts.type == BT_INTEGER)
    {
      if (handler->ts.kind != gfc_c_int_kind)
	gfc_convert_type (handler, &f->ts, 2);
      f->value.function.name = gfc_get_string (PREFIX ("signal_func_int"));
    }
  else
    f->value.function.name = gfc_get_string (PREFIX ("signal_func"));

  if (number->ts.kind != gfc_c_int_kind)
    gfc_convert_type (number, &f->ts, 2);
}


void
gfc_resolve_sin (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__sin_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_sinh (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__sinh_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_size (gfc_expr *f, gfc_expr *array ATTRIBUTE_UNUSED,
		  gfc_expr *dim ATTRIBUTE_UNUSED, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;
}


void
gfc_resolve_stride (gfc_expr *f, gfc_expr *array ATTRIBUTE_UNUSED,
		  gfc_expr *dim ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_index_integer_kind;
}


void
gfc_resolve_spacing (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__spacing_%d", x->ts.kind);
}


void
gfc_resolve_spread (gfc_expr *f, gfc_expr *source, gfc_expr *dim,
		    gfc_expr *ncopies)
{
  if (source->ts.type == BT_CHARACTER && source->ref)
    gfc_resolve_substring_charlen (source);

  if (source->ts.type == BT_CHARACTER)
    check_charlen_present (source);

  f->ts = source->ts;
  f->rank = source->rank + 1;
  if (source->rank == 0)
    {
      if (source->ts.type == BT_CHARACTER)
	f->value.function.name
	  = source->ts.kind == 1 ? PREFIX ("spread_char_scalar")
				 : gfc_get_string
					(PREFIX ("spread_char%d_scalar"),
					 source->ts.kind);
      else
	f->value.function.name = PREFIX ("spread_scalar");
    }
  else
    {
      if (source->ts.type == BT_CHARACTER)
	f->value.function.name
	  = source->ts.kind == 1 ? PREFIX ("spread_char")
				 : gfc_get_string
					(PREFIX ("spread_char%d"),
					 source->ts.kind);
      else
	f->value.function.name = PREFIX ("spread");
    }

  if (dim && gfc_is_constant_expr (dim)
      && ncopies && gfc_is_constant_expr (ncopies) && source->shape[0])
    {
      int i, idim;
      idim = mpz_get_ui (dim->value.integer);
      f->shape = gfc_get_shape (f->rank);
      for (i = 0; i < (idim - 1); i++)
	mpz_init_set (f->shape[i], source->shape[i]);

      mpz_init_set (f->shape[idim - 1], ncopies->value.integer);

      for (i = idim; i < f->rank ; i++)
	mpz_init_set (f->shape[i], source->shape[i-1]);
    }


  gfc_resolve_dim_arg (dim);
  gfc_resolve_index (ncopies, 1);
}


void
gfc_resolve_sqrt (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__sqrt_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


/* Resolve the g77 compatibility function STAT AND FSTAT.  */

void
gfc_resolve_stat (gfc_expr *f, gfc_expr *n ATTRIBUTE_UNUSED,
		  gfc_expr *a ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string (PREFIX ("stat_i%d"), f->ts.kind);
}


void
gfc_resolve_lstat (gfc_expr *f, gfc_expr *n ATTRIBUTE_UNUSED,
		   gfc_expr *a ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string (PREFIX ("lstat_i%d"), f->ts.kind);
}


void
gfc_resolve_fstat (gfc_expr *f, gfc_expr *n, gfc_expr *a ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  if (n->ts.kind != f->ts.kind)
    gfc_convert_type (n, &f->ts, 2);

  f->value.function.name = gfc_get_string (PREFIX ("fstat_i%d"), f->ts.kind);
}


void
gfc_resolve_fgetc (gfc_expr *f, gfc_expr *u, gfc_expr *c ATTRIBUTE_UNUSED)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_c_int_kind;
  if (u->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (u, &ts, 2);
    }

  f->value.function.name = gfc_get_string (PREFIX ("fgetc"));
}


void
gfc_resolve_fget (gfc_expr *f, gfc_expr *c ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_c_int_kind;
  f->value.function.name = gfc_get_string (PREFIX ("fget"));
}


void
gfc_resolve_fputc (gfc_expr *f, gfc_expr *u, gfc_expr *c ATTRIBUTE_UNUSED)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_c_int_kind;
  if (u->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (u, &ts, 2);
    }

  f->value.function.name = gfc_get_string (PREFIX ("fputc"));
}


void
gfc_resolve_fput (gfc_expr *f, gfc_expr *c ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_c_int_kind;
  f->value.function.name = gfc_get_string (PREFIX ("fput"));
}


void
gfc_resolve_ftell (gfc_expr *f, gfc_expr *u)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_intio_kind;
  if (u->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (u, &ts, 2);
    }

  f->value.function.name = gfc_get_string (PREFIX ("ftell"));
}


void
gfc_resolve_storage_size (gfc_expr *f, gfc_expr *a ATTRIBUTE_UNUSED,
			  gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;
}


void
gfc_resolve_sum (gfc_expr *f, gfc_expr *array, gfc_expr *dim, gfc_expr *mask)
{
  resolve_transformational ("sum", f, array, dim, mask);
}


void
gfc_resolve_symlnk (gfc_expr *f, gfc_expr *p1 ATTRIBUTE_UNUSED,
		    gfc_expr *p2 ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string (PREFIX ("symlnk_i%d"), f->ts.kind);
}


/* Resolve the g77 compatibility function SYSTEM.  */

void
gfc_resolve_system (gfc_expr *f, gfc_expr *n ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX ("system"));
}


void
gfc_resolve_tan (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__tan_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_tanh (gfc_expr *f, gfc_expr *x)
{
  f->ts = x->ts;
  f->value.function.name
    = gfc_get_string ("__tanh_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


/* Build an expression for converting degrees to radians.  */

static gfc_expr *
get_radians (gfc_expr *deg)
{
  gfc_expr *result, *factor;
  gfc_actual_arglist *mod_args;

  gcc_assert (deg->ts.type == BT_REAL);

  /* Set deg = deg % 360 to avoid offsets from large angles.  */
  factor = gfc_get_constant_expr (deg->ts.type, deg->ts.kind, &deg->where);
  mpfr_set_d (factor->value.real, 360.0, GFC_RND_MODE);

  mod_args = gfc_get_actual_arglist ();
  mod_args->expr = deg;
  mod_args->next = gfc_get_actual_arglist ();
  mod_args->next->expr = factor;

  result = gfc_get_expr ();
  result->ts = deg->ts;
  result->where = deg->where;
  result->expr_type = EXPR_FUNCTION;
  result->value.function.isym = gfc_intrinsic_function_by_id (GFC_ISYM_MOD);
  result->value.function.actual = mod_args;

  /* Set factor = pi / 180.  */
  factor = gfc_get_constant_expr (deg->ts.type, deg->ts.kind, &deg->where);
  mpfr_const_pi (factor->value.real, GFC_RND_MODE);
  mpfr_div_ui (factor->value.real, factor->value.real, 180, GFC_RND_MODE);

  /* Result is rad = (deg % 360) * (pi / 180).  */
  result = gfc_multiply (result, factor);
  return result;
}


/* Build an expression for converting radians to degrees.  */

static gfc_expr *
get_degrees (gfc_expr *rad)
{
  gfc_expr *result, *factor;
  gfc_actual_arglist *mod_args;
  mpfr_t tmp;

  gcc_assert (rad->ts.type == BT_REAL);

  /* Set rad = rad % 2pi to avoid offsets from large angles.  */
  factor = gfc_get_constant_expr (rad->ts.type, rad->ts.kind, &rad->where);
  mpfr_const_pi (factor->value.real, GFC_RND_MODE);
  mpfr_mul_ui (factor->value.real, factor->value.real, 2, GFC_RND_MODE);

  mod_args = gfc_get_actual_arglist ();
  mod_args->expr = rad;
  mod_args->next = gfc_get_actual_arglist ();
  mod_args->next->expr = factor;

  result = gfc_get_expr ();
  result->ts = rad->ts;
  result->where = rad->where;
  result->expr_type = EXPR_FUNCTION;
  result->value.function.isym = gfc_intrinsic_function_by_id (GFC_ISYM_MOD);
  result->value.function.actual = mod_args;

  /* Set factor = 180 / pi.  */
  factor = gfc_get_constant_expr (rad->ts.type, rad->ts.kind, &rad->where);
  mpfr_set_ui (factor->value.real, 180, GFC_RND_MODE);
  mpfr_init (tmp);
  mpfr_const_pi (tmp, GFC_RND_MODE);
  mpfr_div (factor->value.real, factor->value.real, tmp, GFC_RND_MODE);
  mpfr_clear (tmp);

  /* Result is deg = (rad % 2pi) * (180 / pi).  */
  result = gfc_multiply (result, factor);
  return result;
}


/* Resolve a call to a trig function.  */

static void
resolve_trig_call (gfc_expr *f, gfc_expr *x)
{
  switch (f->value.function.isym->id)
    {
    case GFC_ISYM_ACOS:
      return gfc_resolve_acos (f, x);
    case GFC_ISYM_ASIN:
      return gfc_resolve_asin (f, x);
    case GFC_ISYM_ATAN:
      return gfc_resolve_atan (f, x);
    case GFC_ISYM_ATAN2:
      /* NB. arg3 is unused for atan2 */
      return gfc_resolve_atan2 (f, x, NULL);
    case GFC_ISYM_COS:
      return gfc_resolve_cos (f, x);
    case GFC_ISYM_COTAN:
      return gfc_resolve_cotan (f, x);
    case GFC_ISYM_SIN:
      return gfc_resolve_sin (f, x);
    case GFC_ISYM_TAN:
      return gfc_resolve_tan (f, x);
    default:
      gcc_unreachable ();
    }
}

/* Resolve degree trig function as trigd (x) = trig (radians (x)).  */

void
gfc_resolve_trigd (gfc_expr *f, gfc_expr *x)
{
  if (is_trig_resolved (f))
    return;

  x = get_radians (x);
  f->value.function.actual->expr = x;

  resolve_trig_call (f, x);
}


/* Resolve degree inverse trig function as atrigd (x) = degrees (atrig (x)).  */

void
gfc_resolve_atrigd (gfc_expr *f, gfc_expr *x)
{
  gfc_expr *result, *fcopy;

  if (is_trig_resolved (f))
    return;

  resolve_trig_call (f, x);

  fcopy = copy_replace_function_shallow (f);
  result = get_degrees (fcopy);
  gfc_replace_expr (f, result);
}


/* Resolve atan2d(x) = degrees(atan2(x)).  */

void
gfc_resolve_atan2d (gfc_expr *f, gfc_expr *x, gfc_expr *y ATTRIBUTE_UNUSED)
{
  /* Note that we lose the second arg here - that's okay because it is
     unused in gfc_resolve_atan2 anyway.  */
  gfc_resolve_atrigd (f, x);
}


/* Resolve failed_images (team, kind).  */

void
gfc_resolve_failed_images (gfc_expr *f, gfc_expr *team ATTRIBUTE_UNUSED,
			   gfc_expr *kind)
{
  static char failed_images[] = "_gfortran_caf_failed_images";
  f->rank = 1;
  f->ts.type = BT_INTEGER;
  if (kind == NULL)
    f->ts.kind = gfc_default_integer_kind;
  else
    gfc_extract_int (kind, &f->ts.kind);
  f->value.function.name = failed_images;
}


/* Resolve image_status (image, team).  */

void
gfc_resolve_image_status (gfc_expr *f, gfc_expr *image ATTRIBUTE_UNUSED,
			  gfc_expr *team ATTRIBUTE_UNUSED)
{
  static char image_status[] = "_gfortran_caf_image_status";
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = image_status;
}


/* Resolve get_team ().  */

void
gfc_resolve_get_team (gfc_expr *f, gfc_expr *level ATTRIBUTE_UNUSED)
{
  static char get_team[] = "_gfortran_caf_get_team";
  f->rank = 0;
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = get_team;
}


/* Resolve image_index (...).  */

void
gfc_resolve_image_index (gfc_expr *f, gfc_expr *array ATTRIBUTE_UNUSED,
			 gfc_expr *sub ATTRIBUTE_UNUSED)
{
  static char image_index[] = "__image_index";
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = image_index;
}


/* Resolve stopped_images (team, kind).  */

void
gfc_resolve_stopped_images (gfc_expr *f, gfc_expr *team ATTRIBUTE_UNUSED,
			    gfc_expr *kind)
{
  static char stopped_images[] = "_gfortran_caf_stopped_images";
  f->rank = 1;
  f->ts.type = BT_INTEGER;
  if (kind == NULL)
    f->ts.kind = gfc_default_integer_kind;
  else
    gfc_extract_int (kind, &f->ts.kind);
  f->value.function.name = stopped_images;
}


/* Resolve team_number (team).  */

void
gfc_resolve_team_number (gfc_expr *f, gfc_expr *team ATTRIBUTE_UNUSED)
{
  static char team_number[] = "_gfortran_caf_team_number";
  f->rank = 0;
  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = team_number;
}


void
gfc_resolve_this_image (gfc_expr *f, gfc_expr *array, gfc_expr *dim,
			gfc_expr *distance ATTRIBUTE_UNUSED)
{
  static char this_image[] = "__this_image";
  if (array && gfc_is_coarray (array))
    resolve_bound (f, array, dim, NULL, "__this_image", true);
  else
    {
      f->ts.type = BT_INTEGER;
      f->ts.kind = gfc_default_integer_kind;
      f->value.function.name = this_image;
    }
}


void
gfc_resolve_time (gfc_expr *f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX ("time_func"));
}


void
gfc_resolve_time8 (gfc_expr *f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 8;
  f->value.function.name = gfc_get_string (PREFIX ("time8_func"));
}


void
gfc_resolve_transfer (gfc_expr *f, gfc_expr *source ATTRIBUTE_UNUSED,
		      gfc_expr *mold, gfc_expr *size)
{
  /* TODO: Make this do something meaningful.  */
  static char transfer0[] = "__transfer0", transfer1[] = "__transfer1";

  if (mold->ts.type == BT_CHARACTER
	&& !mold->ts.u.cl->length
	&& gfc_is_constant_expr (mold))
    {
      int len;
      if (mold->expr_type == EXPR_CONSTANT)
        {
	  len = mold->value.character.length;
	  mold->ts.u.cl->length = gfc_get_int_expr (gfc_charlen_int_kind,
						    NULL, len);
	}
      else
	{
	  gfc_constructor *c = gfc_constructor_first (mold->value.constructor);
	  len = c->expr->value.character.length;
	  mold->ts.u.cl->length = gfc_get_int_expr (gfc_charlen_int_kind,
						    NULL, len);
	}
    }

  f->ts = mold->ts;

  if (size == NULL && mold->rank == 0)
    {
      f->rank = 0;
      f->value.function.name = transfer0;
    }
  else
    {
      f->rank = 1;
      f->value.function.name = transfer1;
      if (size && gfc_is_constant_expr (size))
	{
	  f->shape = gfc_get_shape (1);
	  mpz_init_set (f->shape[0], size->value.integer);
	}
    }
}


void
gfc_resolve_transpose (gfc_expr *f, gfc_expr *matrix)
{

  if (matrix->ts.type == BT_CHARACTER && matrix->ref)
    gfc_resolve_substring_charlen (matrix);

  f->ts = matrix->ts;
  f->rank = 2;
  if (matrix->shape)
    {
      f->shape = gfc_get_shape (2);
      mpz_init_set (f->shape[0], matrix->shape[1]);
      mpz_init_set (f->shape[1], matrix->shape[0]);
    }

  switch (matrix->ts.kind)
    {
    case 4:
    case 8:
    case 10:
    case 16:
      switch (matrix->ts.type)
	{
	case BT_REAL:
	case BT_COMPLEX:
	  f->value.function.name
	    = gfc_get_string (PREFIX ("transpose_%c%d"),
			      gfc_type_letter (matrix->ts.type),
			      matrix->ts.kind);
	  break;

	case BT_INTEGER:
	case BT_LOGICAL:
	  /* Use the integer routines for real and logical cases.  This
	     assumes they all have the same alignment requirements.  */
	  f->value.function.name
	    = gfc_get_string (PREFIX ("transpose_i%d"), matrix->ts.kind);
	  break;

	default:
	  if (matrix->ts.type == BT_CHARACTER && matrix->ts.kind == 4)
	    f->value.function.name = PREFIX ("transpose_char4");
	  else
	    f->value.function.name = PREFIX ("transpose");
	  break;
	}
      break;

    default:
      f->value.function.name = (matrix->ts.type == BT_CHARACTER
				? PREFIX ("transpose_char")
				: PREFIX ("transpose"));
      break;
    }
}


void
gfc_resolve_trim (gfc_expr *f, gfc_expr *string)
{
  f->ts.type = BT_CHARACTER;
  f->ts.kind = string->ts.kind;
  f->value.function.name = gfc_get_string ("__trim_%d", string->ts.kind);
}


void
gfc_resolve_ubound (gfc_expr *f, gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  resolve_bound (f, array, dim, kind, "__ubound", false);
}


void
gfc_resolve_ucobound (gfc_expr *f, gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  resolve_bound (f, array, dim, kind, "__ucobound", true);
}


/* Resolve the g77 compatibility function UMASK.  */

void
gfc_resolve_umask (gfc_expr *f, gfc_expr *n)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = n->ts.kind;
  f->value.function.name = gfc_get_string (PREFIX ("umask_i%d"), n->ts.kind);
}


/* Resolve the g77 compatibility function UNLINK.  */

void
gfc_resolve_unlink (gfc_expr *f, gfc_expr *n ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX ("unlink"));
}


void
gfc_resolve_ttynam (gfc_expr *f, gfc_expr *unit)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  f->ts.type = BT_CHARACTER;
  f->ts.kind = gfc_default_character_kind;

  if (unit->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (unit, &ts, 2);
    }

  f->value.function.name = gfc_get_string (PREFIX ("ttynam"));
}


void
gfc_resolve_unpack (gfc_expr *f, gfc_expr *vector, gfc_expr *mask,
		    gfc_expr *field ATTRIBUTE_UNUSED)
{
  if (vector->ts.type == BT_CHARACTER && vector->ref)
    gfc_resolve_substring_charlen (vector);

  f->ts = vector->ts;
  f->rank = mask->rank;
  resolve_mask_arg (mask);

  if (vector->ts.type == BT_CHARACTER)
    {
      if (vector->ts.kind == 1)
	f->value.function.name
	  = gfc_get_string (PREFIX ("unpack%d_char"), field->rank > 0 ? 1 : 0);
      else
	f->value.function.name
	  = gfc_get_string (PREFIX ("unpack%d_char%d"),
			    field->rank > 0 ? 1 : 0, vector->ts.kind);
    }
  else
    f->value.function.name
      = gfc_get_string (PREFIX ("unpack%d"), field->rank > 0 ? 1 : 0);
}


void
gfc_resolve_verify (gfc_expr *f, gfc_expr *string,
		    gfc_expr *set ATTRIBUTE_UNUSED,
		    gfc_expr *back ATTRIBUTE_UNUSED, gfc_expr *kind)
{
  f->ts.type = BT_INTEGER;
  if (kind)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__verify_%d", string->ts.kind);
}


void
gfc_resolve_xor (gfc_expr *f, gfc_expr *i, gfc_expr *j)
{
  f->ts.type = i->ts.type;
  f->ts.kind = gfc_kind_max (i, j);

  if (i->ts.kind != j->ts.kind)
    {
      if (i->ts.kind == gfc_kind_max (i, j))
	gfc_convert_type (j, &i->ts, 2);
      else
	gfc_convert_type (i, &j->ts, 2);
    }

  f->value.function.name
    = gfc_get_string ("__xor_%c%d", gfc_type_letter (i->ts.type), f->ts.kind);
}


/* Intrinsic subroutine resolution.  */

void
gfc_resolve_alarm_sub (gfc_code *c)
{
  const char *name;
  gfc_expr *seconds, *handler;
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  seconds = c->ext.actual->expr;
  handler = c->ext.actual->next->expr;
  ts.type = BT_INTEGER;
  ts.kind = gfc_c_int_kind;

  /* handler can be either BT_INTEGER or BT_PROCEDURE.
     In all cases, the status argument is of default integer kind
     (enforced in check.c) so that the function suffix is fixed.  */
  if (handler->ts.type == BT_INTEGER)
    {
      if (handler->ts.kind != gfc_c_int_kind)
	gfc_convert_type (handler, &ts, 2);
      name = gfc_get_string (PREFIX ("alarm_sub_int_i%d"),
			     gfc_default_integer_kind);
    }
  else
    name = gfc_get_string (PREFIX ("alarm_sub_i%d"),
			   gfc_default_integer_kind);

  if (seconds->ts.kind != gfc_c_int_kind)
    gfc_convert_type (seconds, &ts, 2);

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}

void
gfc_resolve_cpu_time (gfc_code *c)
{
  const char *name;
  name = gfc_get_string (PREFIX ("cpu_time_%d"), c->ext.actual->expr->ts.kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Create a formal arglist based on an actual one and set the INTENTs given.  */

static gfc_formal_arglist*
create_formal_for_intents (gfc_actual_arglist* actual, const sym_intent* ints)
{
  gfc_formal_arglist* head;
  gfc_formal_arglist* tail;
  int i;

  if (!actual)
    return NULL;

  head = tail = gfc_get_formal_arglist ();
  for (i = 0; actual; actual = actual->next, tail = tail->next, ++i)
    {
      gfc_symbol* sym;

      sym = gfc_new_symbol ("dummyarg", NULL);
      sym->ts = actual->expr->ts;

      sym->attr.intent = ints[i];
      tail->sym = sym;

      if (actual->next)
	tail->next = gfc_get_formal_arglist ();
    }

  return head;
}


void
gfc_resolve_atomic_def (gfc_code *c)
{
  const char *name = "atomic_define";
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_atomic_ref (gfc_code *c)
{
  const char *name = "atomic_ref";
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}

void
gfc_resolve_event_query (gfc_code *c)
{
  const char *name = "event_query";
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}

void
gfc_resolve_mvbits (gfc_code *c)
{
  static const sym_intent INTENTS[] = {INTENT_IN, INTENT_IN, INTENT_IN,
				       INTENT_INOUT, INTENT_IN};

  const char *name;
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  /* FROMPOS, LEN and TOPOS are restricted to small values.  As such,
     they will be converted so that they fit into a C int.  */
  ts.type = BT_INTEGER;
  ts.kind = gfc_c_int_kind;
  if (c->ext.actual->next->expr->ts.kind != gfc_c_int_kind)
    gfc_convert_type (c->ext.actual->next->expr, &ts, 2);
  if (c->ext.actual->next->next->expr->ts.kind != gfc_c_int_kind)
    gfc_convert_type (c->ext.actual->next->next->expr, &ts, 2);
  if (c->ext.actual->next->next->next->next->expr->ts.kind != gfc_c_int_kind)
    gfc_convert_type (c->ext.actual->next->next->next->next->expr, &ts, 2);

  /* TO and FROM are guaranteed to have the same kind parameter.  */
  name = gfc_get_string (PREFIX ("mvbits_i%d"),
			 c->ext.actual->expr->ts.kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
  /* Mark as elemental subroutine as this does not happen automatically.  */
  c->resolved_sym->attr.elemental = 1;

  /* Create a dummy formal arglist so the INTENTs are known later for purpose
     of creating temporaries.  */
  c->resolved_sym->formal = create_formal_for_intents (c->ext.actual, INTENTS);
}


/* Set up the call to RANDOM_INIT.  */ 

void
gfc_resolve_random_init (gfc_code *c)
{
  const char *name;
  name = gfc_get_string (PREFIX ("random_init"));
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_random_number (gfc_code *c)
{
  const char *name;
  int kind;

  kind = c->ext.actual->expr->ts.kind;
  if (c->ext.actual->expr->rank == 0)
    name = gfc_get_string (PREFIX ("random_r%d"), kind);
  else
    name = gfc_get_string (PREFIX ("arandom_r%d"), kind);

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_random_seed (gfc_code *c)
{
  const char *name;

  name = gfc_get_string (PREFIX ("random_seed_i%d"), gfc_default_integer_kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_rename_sub (gfc_code *c)
{
  const char *name;
  int kind;

  /* Find the type of status.  If not present use default integer kind.  */
  if (c->ext.actual->next->next->expr != NULL)
    kind = c->ext.actual->next->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("rename_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_link_sub (gfc_code *c)
{
  const char *name;
  int kind;

  if (c->ext.actual->next->next->expr != NULL)
    kind = c->ext.actual->next->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("link_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_symlnk_sub (gfc_code *c)
{
  const char *name;
  int kind;

  if (c->ext.actual->next->next->expr != NULL)
    kind = c->ext.actual->next->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("symlnk_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* G77 compatibility subroutines dtime() and etime().  */

void
gfc_resolve_dtime_sub (gfc_code *c)
{
  const char *name;
  name = gfc_get_string (PREFIX ("dtime_sub"));
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}

void
gfc_resolve_etime_sub (gfc_code *c)
{
  const char *name;
  name = gfc_get_string (PREFIX ("etime_sub"));
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* G77 compatibility subroutines itime(), idate(), ltime() and gmtime().  */

void
gfc_resolve_itime (gfc_code *c)
{
  c->resolved_sym
    = gfc_get_intrinsic_sub_symbol (gfc_get_string (PREFIX ("itime_i%d"),
						    gfc_default_integer_kind));
}

void
gfc_resolve_idate (gfc_code *c)
{
  c->resolved_sym
    = gfc_get_intrinsic_sub_symbol (gfc_get_string (PREFIX ("idate_i%d"),
						    gfc_default_integer_kind));
}

void
gfc_resolve_ltime (gfc_code *c)
{
  c->resolved_sym
    = gfc_get_intrinsic_sub_symbol (gfc_get_string (PREFIX ("ltime_i%d"),
						    gfc_default_integer_kind));
}

void
gfc_resolve_gmtime (gfc_code *c)
{
  c->resolved_sym
    = gfc_get_intrinsic_sub_symbol (gfc_get_string (PREFIX ("gmtime_i%d"),
						    gfc_default_integer_kind));
}


/* G77 compatibility subroutine second().  */

void
gfc_resolve_second_sub (gfc_code *c)
{
  const char *name;
  name = gfc_get_string (PREFIX ("second_sub"));
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_sleep_sub (gfc_code *c)
{
  const char *name;
  int kind;

  if (c->ext.actual->expr != NULL)
    kind = c->ext.actual->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("sleep_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* G77 compatibility function srand().  */

void
gfc_resolve_srand (gfc_code *c)
{
  const char *name;
  name = gfc_get_string (PREFIX ("srand"));
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the getarg intrinsic subroutine.  */

void
gfc_resolve_getarg (gfc_code *c)
{
  const char *name;

  if (c->ext.actual->expr->ts.kind != gfc_default_integer_kind)
    {
      gfc_typespec ts;
      gfc_clear_ts (&ts);

      ts.type = BT_INTEGER;
      ts.kind = gfc_default_integer_kind;

      gfc_convert_type (c->ext.actual->expr, &ts, 2);
    }

  name = gfc_get_string (PREFIX ("getarg_i%d"), gfc_default_integer_kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the getcwd intrinsic subroutine.  */

void
gfc_resolve_getcwd_sub (gfc_code *c)
{
  const char *name;
  int kind;

  if (c->ext.actual->next->expr != NULL)
    kind = c->ext.actual->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("getcwd_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the get_command intrinsic subroutine.  */

void
gfc_resolve_get_command (gfc_code *c)
{
  const char *name;
  int kind;
  kind = gfc_default_integer_kind;
  name = gfc_get_string (PREFIX ("get_command_i%d"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the get_command_argument intrinsic subroutine.  */

void
gfc_resolve_get_command_argument (gfc_code *c)
{
  const char *name;
  int kind;
  kind = gfc_default_integer_kind;
  name = gfc_get_string (PREFIX ("get_command_argument_i%d"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the get_environment_variable intrinsic subroutine.  */

void
gfc_resolve_get_environment_variable (gfc_code *code)
{
  const char *name;
  int kind;
  kind = gfc_default_integer_kind;
  name = gfc_get_string (PREFIX ("get_environment_variable_i%d"), kind);
  code->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_signal_sub (gfc_code *c)
{
  const char *name;
  gfc_expr *number, *handler, *status;
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  number = c->ext.actual->expr;
  handler = c->ext.actual->next->expr;
  status = c->ext.actual->next->next->expr;
  ts.type = BT_INTEGER;
  ts.kind = gfc_c_int_kind;

  /* handler can be either BT_INTEGER or BT_PROCEDURE  */
  if (handler->ts.type == BT_INTEGER)
    {
      if (handler->ts.kind != gfc_c_int_kind)
	gfc_convert_type (handler, &ts, 2);
      name = gfc_get_string (PREFIX ("signal_sub_int"));
    }
  else
    name = gfc_get_string (PREFIX ("signal_sub"));

  if (number->ts.kind != gfc_c_int_kind)
    gfc_convert_type (number, &ts, 2);
  if (status != NULL && status->ts.kind != gfc_c_int_kind)
    gfc_convert_type (status, &ts, 2);

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the SYSTEM intrinsic subroutine.  */

void
gfc_resolve_system_sub (gfc_code *c)
{
  const char *name;
  name = gfc_get_string (PREFIX ("system_sub"));
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Determine if the arguments to SYSTEM_CLOCK are INTEGER(4) or INTEGER(8) */

void
gfc_resolve_system_clock (gfc_code *c)
{
  const char *name;
  int kind;
  gfc_expr *count = c->ext.actual->expr;
  gfc_expr *count_max = c->ext.actual->next->next->expr;

  /* The INTEGER(8) version has higher precision, it is used if both COUNT
     and COUNT_MAX can hold 64-bit values, or are absent.  */
  if ((!count || count->ts.kind >= 8)
      && (!count_max || count_max->ts.kind >= 8))
    kind = 8;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("system_clock_%d"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the EXECUTE_COMMAND_LINE intrinsic subroutine.  */
void
gfc_resolve_execute_command_line (gfc_code *c)
{
  const char *name;
  name = gfc_get_string (PREFIX ("execute_command_line_i%d"),
			 gfc_default_integer_kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the EXIT intrinsic subroutine.  */

void
gfc_resolve_exit (gfc_code *c)
{
  const char *name;
  gfc_typespec ts;
  gfc_expr *n;
  gfc_clear_ts (&ts);

  /* The STATUS argument has to be of default kind.  If it is not,
     we convert it.  */
  ts.type = BT_INTEGER;
  ts.kind = gfc_default_integer_kind;
  n = c->ext.actual->expr;
  if (n != NULL && n->ts.kind != ts.kind)
    gfc_convert_type (n, &ts, 2);

  name = gfc_get_string (PREFIX ("exit_i%d"), ts.kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the FLUSH intrinsic subroutine.  */

void
gfc_resolve_flush (gfc_code *c)
{
  const char *name;
  gfc_typespec ts;
  gfc_expr *n;
  gfc_clear_ts (&ts);

  ts.type = BT_INTEGER;
  ts.kind = gfc_default_integer_kind;
  n = c->ext.actual->expr;
  if (n != NULL && n->ts.kind != ts.kind)
    gfc_convert_type (n, &ts, 2);

  name = gfc_get_string (PREFIX ("flush_i%d"), ts.kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_ctime_sub (gfc_code *c)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  /* ctime TIME argument is a INTEGER(KIND=8), says the doc */
  if (c->ext.actual->expr->ts.kind != 8)
    {
      ts.type = BT_INTEGER;
      ts.kind = 8;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (c->ext.actual->expr, &ts, 2);
    }

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (PREFIX ("ctime_sub"));
}


void
gfc_resolve_fdate_sub (gfc_code *c)
{
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (PREFIX ("fdate_sub"));
}


void
gfc_resolve_gerror (gfc_code *c)
{
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (PREFIX ("gerror"));
}


void
gfc_resolve_getlog (gfc_code *c)
{
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (PREFIX ("getlog"));
}


void
gfc_resolve_hostnm_sub (gfc_code *c)
{
  const char *name;
  int kind;

  if (c->ext.actual->next->expr != NULL)
    kind = c->ext.actual->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("hostnm_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_perror (gfc_code *c)
{
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (PREFIX ("perror_sub"));
}

/* Resolve the STAT and FSTAT intrinsic subroutines.  */

void
gfc_resolve_stat_sub (gfc_code *c)
{
  const char *name;
  name = gfc_get_string (PREFIX ("stat_i%d_sub"), gfc_default_integer_kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_lstat_sub (gfc_code *c)
{
  const char *name;
  name = gfc_get_string (PREFIX ("lstat_i%d_sub"), gfc_default_integer_kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_fstat_sub (gfc_code *c)
{
  const char *name;
  gfc_expr *u;
  gfc_typespec *ts;

  u = c->ext.actual->expr;
  ts = &c->ext.actual->next->expr->ts;
  if (u->ts.kind != ts->kind)
    gfc_convert_type (u, ts, 2);
  name = gfc_get_string (PREFIX ("fstat_i%d_sub"), ts->kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_fgetc_sub (gfc_code *c)
{
  const char *name;
  gfc_typespec ts;
  gfc_expr *u, *st;
  gfc_clear_ts (&ts);

  u = c->ext.actual->expr;
  st = c->ext.actual->next->next->expr;

  if (u->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (u, &ts, 2);
    }

  if (st != NULL)
    name = gfc_get_string (PREFIX ("fgetc_i%d_sub"), st->ts.kind);
  else
    name = gfc_get_string (PREFIX ("fgetc_i%d_sub"), gfc_default_integer_kind);

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_fget_sub (gfc_code *c)
{
  const char *name;
  gfc_expr *st;

  st = c->ext.actual->next->expr;
  if (st != NULL)
    name = gfc_get_string (PREFIX ("fget_i%d_sub"), st->ts.kind);
  else
    name = gfc_get_string (PREFIX ("fget_i%d_sub"), gfc_default_integer_kind);

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_fputc_sub (gfc_code *c)
{
  const char *name;
  gfc_typespec ts;
  gfc_expr *u, *st;
  gfc_clear_ts (&ts);

  u = c->ext.actual->expr;
  st = c->ext.actual->next->next->expr;

  if (u->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (u, &ts, 2);
    }

  if (st != NULL)
    name = gfc_get_string (PREFIX ("fputc_i%d_sub"), st->ts.kind);
  else
    name = gfc_get_string (PREFIX ("fputc_i%d_sub"), gfc_default_integer_kind);

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_fput_sub (gfc_code *c)
{
  const char *name;
  gfc_expr *st;

  st = c->ext.actual->next->expr;
  if (st != NULL)
    name = gfc_get_string (PREFIX ("fput_i%d_sub"), st->ts.kind);
  else
    name = gfc_get_string (PREFIX ("fput_i%d_sub"), gfc_default_integer_kind);

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_fseek_sub (gfc_code *c)
{
  gfc_expr *unit;
  gfc_expr *offset;
  gfc_expr *whence;
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  unit   = c->ext.actual->expr;
  offset = c->ext.actual->next->expr;
  whence = c->ext.actual->next->next->expr;

  if (unit->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (unit, &ts, 2);
    }

  if (offset->ts.kind != gfc_intio_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_intio_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (offset, &ts, 2);
    }

  if (whence->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (whence, &ts, 2);
    }

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (PREFIX ("fseek_sub"));
}

void
gfc_resolve_ftell_sub (gfc_code *c)
{
  const char *name;
  gfc_expr *unit;
  gfc_expr *offset;
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  unit = c->ext.actual->expr;
  offset = c->ext.actual->next->expr;

  if (unit->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (unit, &ts, 2);
    }

  name = gfc_get_string (PREFIX ("ftell_i%d_sub"), offset->ts.kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_ttynam_sub (gfc_code *c)
{
  gfc_typespec ts;
  gfc_clear_ts (&ts);

  if (c->ext.actual->expr->ts.kind != gfc_c_int_kind)
    {
      ts.type = BT_INTEGER;
      ts.kind = gfc_c_int_kind;
      ts.u.derived = NULL;
      ts.u.cl = NULL;
      gfc_convert_type (c->ext.actual->expr, &ts, 2);
    }

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (PREFIX ("ttynam_sub"));
}


/* Resolve the UMASK intrinsic subroutine.  */

void
gfc_resolve_umask_sub (gfc_code *c)
{
  const char *name;
  int kind;

  if (c->ext.actual->next->expr != NULL)
    kind = c->ext.actual->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("umask_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}

/* Resolve the UNLINK intrinsic subroutine.  */

void
gfc_resolve_unlink_sub (gfc_code *c)
{
  const char *name;
  int kind;

  if (c->ext.actual->next->expr != NULL)
    kind = c->ext.actual->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX ("unlink_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}
