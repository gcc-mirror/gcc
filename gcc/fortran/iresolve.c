/* Intrinsic function resolution.
   Copyright (C) 2000, 2001, 2002, 2003, 2004 Free Software Foundation,
   Inc.
   Contributed by Andy Vaught & Katherine Holcomb

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


/* Assign name and types to intrinsic procedures.  For functions, the
   first argument to a resolution function is an expression pointer to
   the original function node and the rest are pointers to the
   arguments of the function call.  For subroutines, a pointer to the
   code node is passed.  The result type and library subroutine name
   are generally set according to the function arguments.  */

#include "config.h"
#include <string.h>
#include <stdarg.h>

#include "gfortran.h"
#include "intrinsic.h"


/* String pool subroutines.  This are used to provide static locations
   for the string constants that represent library function names.  */

typedef struct string_node
{
  struct string_node *next;
  char string[1];
}
string_node;

#define HASH_SIZE 13

static string_node *string_head[HASH_SIZE];


/* Return a hash code based on the name.  */

static int
hash (const char *name)
{
  int h;

  h = 1;
  while (*name)
    h = 5311966 * h + *name++;

  if (h < 0)
    h = -h;
  return h % HASH_SIZE;
}


/* Given printf-like arguments, return a static address of the
   resulting string.  If the name is not in the table, it is added.  */

char *
gfc_get_string (const char *format, ...)
{
  char temp_name[50];
  string_node *p;
  va_list ap;
  int h;

  va_start (ap, format);
  vsprintf (temp_name, format, ap);
  va_end (ap);

  h = hash (temp_name);

  /* Search */
  for (p = string_head[h]; p; p = p->next)
    if (strcmp (p->string, temp_name) == 0)
      return p->string;

  /* Add */
  p = gfc_getmem (sizeof (string_node) + strlen (temp_name));

  strcpy (p->string, temp_name);

  p->next = string_head[h];
  string_head[h] = p;

  return p->string;
}



static void
free_strings (void)
{
  string_node *p, *q;
  int h;

  for (h = 0; h < HASH_SIZE; h++)
    {
      for (p = string_head[h]; p; p = q)
	{
	  q = p->next;
	  gfc_free (p);
	}
    }
}


/********************** Resolution functions **********************/


void
gfc_resolve_abs (gfc_expr * f, gfc_expr * a)
{

  f->ts = a->ts;
  if (f->ts.type == BT_COMPLEX)
    f->ts.type = BT_REAL;

  f->value.function.name =
    gfc_get_string ("__abs_%c%d", gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_acos (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__acos_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_aimag (gfc_expr * f, gfc_expr * x)
{

  f->ts.type = BT_REAL;
  f->ts.kind = x->ts.kind;
  f->value.function.name =
    gfc_get_string ("__aimag_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_aint (gfc_expr * f, gfc_expr * a, gfc_expr * kind)
{

  f->ts.type = a->ts.type;
  f->ts.kind = (kind == NULL) ? a->ts.kind : mpz_get_si (kind->value.integer);

  /* The resolved name is only used for specific intrinsics where
     the return kind is the same as the arg kind.  */
  f->value.function.name =
    gfc_get_string ("__aint_%c%d", gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_dint (gfc_expr * f, gfc_expr * a)
{
  gfc_resolve_aint (f, a, NULL);
}


void
gfc_resolve_all (gfc_expr * f, gfc_expr * mask, gfc_expr * dim)
{

  f->ts = mask->ts;

  if (dim != NULL)
    {
      gfc_resolve_index (dim, 1);
      f->rank = mask->rank - 1;
      f->shape = gfc_copy_shape_excluding (mask->shape, mask->rank, dim);
    }

  f->value.function.name =
    gfc_get_string ("__all_%c%d", gfc_type_letter (mask->ts.type),
		    mask->ts.kind);
}


void
gfc_resolve_anint (gfc_expr * f, gfc_expr * a, gfc_expr * kind)
{

  f->ts.type = a->ts.type;
  f->ts.kind = (kind == NULL) ? a->ts.kind : mpz_get_si (kind->value.integer);

  /* The resolved name is only used for specific intrinsics where
     the return kind is the same as the arg kind.  */
  f->value.function.name =
    gfc_get_string ("__anint_%c%d", gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_dnint (gfc_expr * f, gfc_expr * a)
{
  gfc_resolve_anint (f, a, NULL);
}


void
gfc_resolve_any (gfc_expr * f, gfc_expr * mask, gfc_expr * dim)
{

  f->ts = mask->ts;

  if (dim != NULL)
    {
      gfc_resolve_index (dim, 1);
      f->rank = mask->rank - 1;
      f->shape = gfc_copy_shape_excluding (mask->shape, mask->rank, dim);
    }

  f->value.function.name =
    gfc_get_string ("__any_%c%d", gfc_type_letter (mask->ts.type),
		    mask->ts.kind);
}


void
gfc_resolve_asin (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__asin_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_atan (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__atan_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_atan2 (gfc_expr * f, gfc_expr * x,
		   gfc_expr * y ATTRIBUTE_UNUSED)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__atan2_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


/* Resolve the BESYN and BESJN intrinsics.  */

void
gfc_resolve_besn (gfc_expr * f, gfc_expr * n, gfc_expr * x)
{
  gfc_typespec ts;
  
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
gfc_resolve_btest (gfc_expr * f, gfc_expr * i, gfc_expr * pos)
{

  f->ts.type = BT_LOGICAL;
  f->ts.kind = gfc_default_logical_kind;

  f->value.function.name = gfc_get_string ("__btest_%d_%d", i->ts.kind,
					   pos->ts.kind);
}


void
gfc_resolve_ceiling (gfc_expr * f, gfc_expr * a, gfc_expr * kind)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? gfc_default_integer_kind
    : mpz_get_si (kind->value.integer);

  f->value.function.name =
    gfc_get_string ("__ceiling_%d_%c%d", f->ts.kind,
		    gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_char (gfc_expr * f, gfc_expr * a, gfc_expr * kind)
{

  f->ts.type = BT_CHARACTER;
  f->ts.kind = (kind == NULL) ? gfc_default_character_kind
    : mpz_get_si (kind->value.integer);

  f->value.function.name =
    gfc_get_string ("__char_%d_%c%d", f->ts.kind,
		    gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_cmplx (gfc_expr * f, gfc_expr * x, gfc_expr * y, gfc_expr * kind)
{

  f->ts.type = BT_COMPLEX;
  f->ts.kind = (kind == NULL) ? gfc_default_real_kind
    : mpz_get_si (kind->value.integer);

  if (y == NULL)
    f->value.function.name =
      gfc_get_string ("__cmplx0_%d_%c%d", f->ts.kind,
		      gfc_type_letter (x->ts.type), x->ts.kind);
  else
    f->value.function.name =
      gfc_get_string ("__cmplx1_%d_%c%d_%c%d", f->ts.kind,
		      gfc_type_letter (x->ts.type), x->ts.kind,
		      gfc_type_letter (y->ts.type), y->ts.kind);
}

void
gfc_resolve_dcmplx (gfc_expr * f, gfc_expr * x, gfc_expr * y)
{
  gfc_resolve_cmplx (f, x, y, gfc_int_expr (gfc_default_double_kind));
}

void
gfc_resolve_conjg (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__conjg_%d", x->ts.kind);
}


void
gfc_resolve_cos (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__cos_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_cosh (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__cosh_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_count (gfc_expr * f, gfc_expr * mask, gfc_expr * dim)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;

  if (dim != NULL)
    {
      f->rank = mask->rank - 1;
      gfc_resolve_index (dim, 1);
      f->shape = gfc_copy_shape_excluding (mask->shape, mask->rank, dim);
    }

  f->value.function.name =
    gfc_get_string ("__count_%d_%c%d", f->ts.kind,
		    gfc_type_letter (mask->ts.type), mask->ts.kind);
}


void
gfc_resolve_cshift (gfc_expr * f, gfc_expr * array,
		    gfc_expr * shift,
		    gfc_expr * dim)
{
  int n;

  f->ts = array->ts;
  f->rank = array->rank;
  f->shape = gfc_copy_shape (array->shape, array->rank);

  if (shift->rank > 0)
    n = 1;
  else
    n = 0;

  if (dim != NULL)
    {
      gfc_resolve_index (dim, 1);
      /* Convert dim to shift's kind, so we don't need so many variations.  */
      if (dim->ts.kind != shift->ts.kind)
	gfc_convert_type (dim, &shift->ts, 2);
    }
  f->value.function.name =
    gfc_get_string ("__cshift%d_%d", n, shift->ts.kind);
}


void
gfc_resolve_dble (gfc_expr * f, gfc_expr * a)
{

  f->ts.type = BT_REAL;
  f->ts.kind = gfc_default_double_kind;
  f->value.function.name =
    gfc_get_string ("__dble_%c%d", gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_dim (gfc_expr * f, gfc_expr * x,
		 gfc_expr * y ATTRIBUTE_UNUSED)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__dim_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_dot_product (gfc_expr * f, gfc_expr * a, gfc_expr * b)
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
      temp.operator = INTRINSIC_NONE;
      temp.op1 = a;
      temp.op2 = b;
      gfc_type_convert_binary (&temp);
      f->ts = temp.ts;
    }

  f->value.function.name =
    gfc_get_string ("__dot_product_%c%d", gfc_type_letter (f->ts.type),
		    f->ts.kind);
}


void
gfc_resolve_dprod (gfc_expr * f,
		   gfc_expr * a ATTRIBUTE_UNUSED,
		   gfc_expr * b ATTRIBUTE_UNUSED)
{
  f->ts.kind = gfc_default_double_kind;
  f->ts.type = BT_REAL;

  f->value.function.name = gfc_get_string ("__dprod_r%d", f->ts.kind);
}


void
gfc_resolve_eoshift (gfc_expr * f, gfc_expr * array,
		     gfc_expr * shift,
		     gfc_expr * boundary,
		     gfc_expr * dim)
{
  int n;

  f->ts = array->ts;
  f->rank = array->rank;
  f->shape = gfc_copy_shape (array->shape, array->rank);

  n = 0;
  if (shift->rank > 0)
    n = n | 1;
  if (boundary && boundary->rank > 0)
    n = n | 2;

  /* Convert dim to the same type as shift, so we don't need quite so many
     variations.  */
  if (dim != NULL && dim->ts.kind != shift->ts.kind)
    gfc_convert_type (dim, &shift->ts, 2);

  f->value.function.name =
    gfc_get_string ("__eoshift%d_%d", n, shift->ts.kind);
}


void
gfc_resolve_exp (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__exp_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_exponent (gfc_expr * f, gfc_expr * x)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;

  f->value.function.name = gfc_get_string ("__exponent_%d", x->ts.kind);
}


void
gfc_resolve_floor (gfc_expr * f, gfc_expr * a, gfc_expr * kind)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? gfc_default_integer_kind
    : mpz_get_si (kind->value.integer);

  f->value.function.name =
    gfc_get_string ("__floor%d_%c%d", f->ts.kind,
		    gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_fraction (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__fraction_%d", x->ts.kind);
}


/* Resolve single-argument g77 math intrinsics, eg BESY0, ERF.  */

void
gfc_resolve_g77_math1 (gfc_expr * f, gfc_expr * x)
{
  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("<intrinsic>");
}


void
gfc_resolve_getcwd (gfc_expr * f, gfc_expr * n ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX("getcwd"));
}


void
gfc_resolve_getgid (gfc_expr * f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX("getgid"));
}


void
gfc_resolve_getpid (gfc_expr * f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX("getpid"));
}


void
gfc_resolve_getuid (gfc_expr * f)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX("getuid"));
}

void
gfc_resolve_iand (gfc_expr * f, gfc_expr * i, gfc_expr * j ATTRIBUTE_UNUSED)
{

  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__iand_%d", i->ts.kind);
}


void
gfc_resolve_ibclr (gfc_expr * f, gfc_expr * i, gfc_expr * pos ATTRIBUTE_UNUSED)
{

  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__ibclr_%d", i->ts.kind);
}


void
gfc_resolve_ibits (gfc_expr * f, gfc_expr * i,
		   gfc_expr * pos ATTRIBUTE_UNUSED,
		   gfc_expr * len ATTRIBUTE_UNUSED)
{

  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__ibits_%d", i->ts.kind);
}


void
gfc_resolve_ibset (gfc_expr * f, gfc_expr * i,
		   gfc_expr * pos ATTRIBUTE_UNUSED)
{

  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__ibset_%d", i->ts.kind);
}


void
gfc_resolve_ichar (gfc_expr * f, gfc_expr * c)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;

  f->value.function.name = gfc_get_string ("__ichar_%d", c->ts.kind);
}


void
gfc_resolve_idnint (gfc_expr * f, gfc_expr * a)
{
  gfc_resolve_nint (f, a, NULL);
}


void
gfc_resolve_ieor (gfc_expr * f, gfc_expr * i,
		  gfc_expr * j ATTRIBUTE_UNUSED)
{

  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__ieor_%d", i->ts.kind);
}


void
gfc_resolve_ior (gfc_expr * f, gfc_expr * i,
		 gfc_expr * j ATTRIBUTE_UNUSED)
{

  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__ior_%d", i->ts.kind);
}


void
gfc_resolve_int (gfc_expr * f, gfc_expr * a, gfc_expr * kind)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? gfc_default_integer_kind
    : mpz_get_si (kind->value.integer);

  f->value.function.name =
    gfc_get_string ("__int_%d_%c%d", f->ts.kind, gfc_type_letter (a->ts.type),
		    a->ts.kind);
}


void
gfc_resolve_ishft (gfc_expr * f, gfc_expr * i, gfc_expr * shift)
{

  f->ts = i->ts;
  f->value.function.name =
    gfc_get_string ("__ishft_%d_%d", i->ts.kind, shift->ts.kind);
}


void
gfc_resolve_ishftc (gfc_expr * f, gfc_expr * i, gfc_expr * shift,
		    gfc_expr * size)
{
  int s_kind;

  s_kind = (size == NULL) ? gfc_default_integer_kind : shift->ts.kind;

  f->ts = i->ts;
  f->value.function.name =
    gfc_get_string ("__ishftc_%d_%d_%d", i->ts.kind, shift->ts.kind, s_kind);
}


void
gfc_resolve_lbound (gfc_expr * f, gfc_expr * array,
		    gfc_expr * dim)
{
  static char lbound[] = "__lbound";

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;

  if (dim == NULL)
    {
      f->rank = 1;
      f->shape = gfc_get_shape (1);
      mpz_init_set_ui (f->shape[0], array->rank);
    }

  f->value.function.name = lbound;
}


void
gfc_resolve_len (gfc_expr * f, gfc_expr * string)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__len_%d", string->ts.kind);
}


void
gfc_resolve_len_trim (gfc_expr * f, gfc_expr * string)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__len_trim%d", string->ts.kind);
}


void
gfc_resolve_log (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__log_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_log10 (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__log10_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_logical (gfc_expr * f, gfc_expr * a, gfc_expr * kind)
{

  f->ts.type = BT_LOGICAL;
  f->ts.kind = (kind == NULL) ? gfc_default_logical_kind
    : mpz_get_si (kind->value.integer);
  f->rank = a->rank;

  f->value.function.name =
    gfc_get_string ("__logical_%d_%c%d", f->ts.kind,
		    gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_matmul (gfc_expr * f, gfc_expr * a, gfc_expr * b)
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
      temp.operator = INTRINSIC_NONE;
      temp.op1 = a;
      temp.op2 = b;
      gfc_type_convert_binary (&temp);
      f->ts = temp.ts;
    }

  f->rank = (a->rank == 2 && b->rank == 2) ? 2 : 1;

  f->value.function.name =
    gfc_get_string ("__matmul_%c%d", gfc_type_letter (f->ts.type),
		    f->ts.kind);
}


static void
gfc_resolve_minmax (const char * name, gfc_expr * f, gfc_actual_arglist * args)
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

  f->value.function.name =
    gfc_get_string (name, gfc_type_letter (f->ts.type), f->ts.kind);
}


void
gfc_resolve_max (gfc_expr * f, gfc_actual_arglist * args)
{
  gfc_resolve_minmax ("__max_%c%d", f, args);
}


void
gfc_resolve_maxloc (gfc_expr * f, gfc_expr * array, gfc_expr * dim,
		    gfc_expr * mask)
{
  const char *name;

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;

  if (dim == NULL)
    f->rank = 1;
  else
    {
      f->rank = array->rank - 1;
      gfc_resolve_index (dim, 1);
    }

  name = mask ? "mmaxloc" : "maxloc";
  f->value.function.name =
    gfc_get_string ("__%s%d_%d_%c%d", name, dim != NULL, f->ts.kind,
                    gfc_type_letter (array->ts.type), array->ts.kind);
}


void
gfc_resolve_maxval (gfc_expr * f, gfc_expr * array, gfc_expr * dim,
		    gfc_expr * mask)
{

  f->ts = array->ts;

  if (dim != NULL)
    {
      f->rank = array->rank - 1;
      gfc_resolve_index (dim, 1);
    }

  f->value.function.name =
    gfc_get_string ("__%s_%c%d", mask ? "mmaxval" : "maxval",
		    gfc_type_letter (array->ts.type), array->ts.kind);
}


void
gfc_resolve_merge (gfc_expr * f, gfc_expr * tsource,
		   gfc_expr * fsource ATTRIBUTE_UNUSED,
		   gfc_expr * mask ATTRIBUTE_UNUSED)
{

  f->ts = tsource->ts;
  f->value.function.name =
    gfc_get_string ("__merge_%c%d", gfc_type_letter (tsource->ts.type),
		    tsource->ts.kind);
}


void
gfc_resolve_min (gfc_expr * f, gfc_actual_arglist * args)
{
  gfc_resolve_minmax ("__min_%c%d", f, args);
}


void
gfc_resolve_minloc (gfc_expr * f, gfc_expr * array, gfc_expr * dim,
		    gfc_expr * mask)
{
  const char *name;

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;

  if (dim == NULL)
    f->rank = 1;
  else
    {
      f->rank = array->rank - 1;
      gfc_resolve_index (dim, 1);
    }

  name = mask ? "mminloc" : "minloc";
  f->value.function.name =
    gfc_get_string ("__%s%d_%d_%c%d", name, dim != NULL, f->ts.kind,
                    gfc_type_letter (array->ts.type), array->ts.kind);
}


void
gfc_resolve_minval (gfc_expr * f, gfc_expr * array, gfc_expr * dim,
		    gfc_expr * mask)
{

  f->ts = array->ts;

  if (dim != NULL)
    {
      f->rank = array->rank - 1;
      gfc_resolve_index (dim, 1);
    }

  f->value.function.name =
    gfc_get_string ("__%s_%c%d", mask ? "mminval" : "minval",
		    gfc_type_letter (array->ts.type), array->ts.kind);
}


void
gfc_resolve_mod (gfc_expr * f, gfc_expr * a,
		 gfc_expr * p ATTRIBUTE_UNUSED)
{

  f->ts = a->ts;
  f->value.function.name =
    gfc_get_string ("__mod_%c%d", gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_modulo (gfc_expr * f, gfc_expr * a,
		    gfc_expr * p ATTRIBUTE_UNUSED)
{

  f->ts = a->ts;
  f->value.function.name =
    gfc_get_string ("__modulo_%c%d", gfc_type_letter (a->ts.type),
		    a->ts.kind);
}

void
gfc_resolve_nearest (gfc_expr * f, gfc_expr * a,
             gfc_expr *p ATTRIBUTE_UNUSED)
{

  f->ts = a->ts;
  f->value.function.name =
    gfc_get_string ("__nearest_%c%d", gfc_type_letter (a->ts.type),
            a->ts.kind);
}

void
gfc_resolve_nint (gfc_expr * f, gfc_expr * a, gfc_expr * kind)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? gfc_default_integer_kind
    : mpz_get_si (kind->value.integer);

  f->value.function.name =
    gfc_get_string ("__nint_%d_%d", f->ts.kind, a->ts.kind);
}


void
gfc_resolve_not (gfc_expr * f, gfc_expr * i)
{

  f->ts = i->ts;
  f->value.function.name = gfc_get_string ("__not_%d", i->ts.kind);
}


void
gfc_resolve_pack (gfc_expr * f,
                  gfc_expr * array ATTRIBUTE_UNUSED,
		  gfc_expr * mask,
		  gfc_expr * vector ATTRIBUTE_UNUSED)
{
  static char pack[] = "__pack",
    pack_s[] = "__pack_s";

  f->ts = array->ts;
  f->rank = 1;

  if (mask->rank != 0)
    f->value.function.name = pack;
  else
    {
      /* We convert mask to default logical only in the scalar case.
	 In the array case we can simply read the array as if it were
	 of type default logical.  */
      if (mask->ts.kind != gfc_default_logical_kind)
	{
	  gfc_typespec ts;

	  ts.type = BT_LOGICAL;
	  ts.kind = gfc_default_logical_kind;
	  gfc_convert_type (mask, &ts, 2);
	}

      f->value.function.name = pack_s;
    }
}


void
gfc_resolve_product (gfc_expr * f, gfc_expr * array, gfc_expr * dim,
		     gfc_expr * mask)
{

  f->ts = array->ts;

  if (dim != NULL)
    {
      f->rank = array->rank - 1;
      gfc_resolve_index (dim, 1);
    }

  f->value.function.name =
    gfc_get_string ("__%s_%c%d", mask ? "mproduct" : "product",
		    gfc_type_letter (array->ts.type), array->ts.kind);
}


void
gfc_resolve_real (gfc_expr * f, gfc_expr * a, gfc_expr * kind)
{

  f->ts.type = BT_REAL;

  if (kind != NULL)
    f->ts.kind = mpz_get_si (kind->value.integer);
  else
    f->ts.kind = (a->ts.type == BT_COMPLEX) ?
      a->ts.kind : gfc_default_real_kind;

  f->value.function.name =
    gfc_get_string ("__real_%d_%c%d", f->ts.kind,
		    gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_repeat (gfc_expr * f, gfc_expr * string,
		   gfc_expr * ncopies ATTRIBUTE_UNUSED)
{

  f->ts.type = BT_CHARACTER;
  f->ts.kind = string->ts.kind;
  f->value.function.name = gfc_get_string ("__repeat_%d", string->ts.kind);
}


void
gfc_resolve_reshape (gfc_expr * f, gfc_expr * source, gfc_expr * shape,
		     gfc_expr * pad ATTRIBUTE_UNUSED,
		     gfc_expr * order ATTRIBUTE_UNUSED)
{
  static char reshape0[] = "__reshape";
  mpz_t rank;
  int kind;
  int i;

  f->ts = source->ts;

  gfc_array_size (shape, &rank);
  f->rank = mpz_get_si (rank);
  mpz_clear (rank);
  switch (source->ts.type)
    {
    case BT_COMPLEX:
      kind = source->ts.kind * 2;
      break;

    case BT_REAL:
    case BT_INTEGER:
    case BT_LOGICAL:
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
    /* case 16: */
      f->value.function.name =
	gfc_get_string ("__reshape_%d", source->ts.kind);
      break;

    default:
      f->value.function.name = reshape0;
      break;
    }

  /* TODO: Make this work with a constant ORDER parameter.  */
  if (shape->expr_type == EXPR_ARRAY
      && gfc_is_constant_expr (shape)
      && order == NULL)
    {
      gfc_constructor *c;
      f->shape = gfc_get_shape (f->rank);
      c = shape->value.constructor;
      for (i = 0; i < f->rank; i++)
	{
	  mpz_init_set (f->shape[i], c->expr->value.integer);
	  c = c->next;
	}
    }
}


void
gfc_resolve_rrspacing (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__rrspacing_%d", x->ts.kind);
}


void
gfc_resolve_scale (gfc_expr * f, gfc_expr * x,
		   gfc_expr * y ATTRIBUTE_UNUSED)
{

  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__scale_%d_%d", x->ts.kind,
					   x->ts.kind);
}


void
gfc_resolve_scan (gfc_expr * f, gfc_expr * string,
		  gfc_expr * set ATTRIBUTE_UNUSED,
		  gfc_expr * back ATTRIBUTE_UNUSED)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__scan_%d", string->ts.kind);
}


void
gfc_resolve_set_exponent (gfc_expr * f, gfc_expr * x, gfc_expr * i)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__set_exponent_%d_%d", x->ts.kind, i->ts.kind);
}


void
gfc_resolve_shape (gfc_expr * f, gfc_expr * array)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->rank = 1;
  f->value.function.name = gfc_get_string ("__shape_%d", f->ts.kind);
  f->shape = gfc_get_shape (1);
  mpz_init_set_ui (f->shape[0], array->rank);
}


void
gfc_resolve_sign (gfc_expr * f, gfc_expr * a, gfc_expr * b ATTRIBUTE_UNUSED)
{

  f->ts = a->ts;
  f->value.function.name =
    gfc_get_string ("__sign_%c%d", gfc_type_letter (a->ts.type), a->ts.kind);
}


void
gfc_resolve_sin (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__sin_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_sinh (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__sinh_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_spacing (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name = gfc_get_string ("__spacing_%d", x->ts.kind);
}


void
gfc_resolve_spread (gfc_expr * f, gfc_expr * source,
		    gfc_expr * dim,
		    gfc_expr * ncopies)
{
  static char spread[] = "__spread";

  f->ts = source->ts;
  f->rank = source->rank + 1;
  f->value.function.name = spread;

  gfc_resolve_index (dim, 1);
  gfc_resolve_index (ncopies, 1);
}


void
gfc_resolve_sqrt (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__sqrt_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_sum (gfc_expr * f, gfc_expr * array, gfc_expr * dim,
		 gfc_expr * mask)
{

  f->ts = array->ts;

  if (dim != NULL)
    {
      f->rank = array->rank - 1;
      gfc_resolve_index (dim, 1);
    }

  f->value.function.name =
    gfc_get_string ("__%s_%c%d", mask ? "msum" : "sum",
		    gfc_type_letter (array->ts.type), array->ts.kind);
}


/* Resolve the g77 compatibility function SYSTEM.  */

void
gfc_resolve_system (gfc_expr * f, gfc_expr * n ATTRIBUTE_UNUSED)
{
  f->ts.type = BT_INTEGER;
  f->ts.kind = 4;
  f->value.function.name = gfc_get_string (PREFIX("system"));
}


void
gfc_resolve_tan (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__tan_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_tanh (gfc_expr * f, gfc_expr * x)
{

  f->ts = x->ts;
  f->value.function.name =
    gfc_get_string ("__tanh_%c%d", gfc_type_letter (x->ts.type), x->ts.kind);
}


void
gfc_resolve_transfer (gfc_expr * f, gfc_expr * source ATTRIBUTE_UNUSED,
		      gfc_expr * mold, gfc_expr * size)
{
  /* TODO: Make this do something meaningful.  */
  static char transfer0[] = "__transfer0", transfer1[] = "__transfer1";

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
    }
}


void
gfc_resolve_transpose (gfc_expr * f, gfc_expr * matrix)
{
  static char transpose0[] = "__transpose";
  int kind;

  f->ts = matrix->ts;
  f->rank = 2;
  if (matrix->shape)
    {
      f->shape = gfc_get_shape (2);
      mpz_init_set (f->shape[0], matrix->shape[1]);
      mpz_init_set (f->shape[1], matrix->shape[0]);
    }

  switch (matrix->ts.type)
    {
    case BT_COMPLEX:
      kind = matrix->ts.kind * 2;
      break;

    case BT_REAL:
    case BT_INTEGER:
    case BT_LOGICAL:
      kind = matrix->ts.kind;
      break;

    default:
      kind = 0;
      break;

    }

  switch (kind)
    {
    case 4:
    case 8:
    /* case 16: */
      f->value.function.name =
        gfc_get_string ("__transpose_%d", kind);
      break;

    default:
      f->value.function.name = transpose0;
    }
}


void
gfc_resolve_trim (gfc_expr * f, gfc_expr * string)
{

  f->ts.type = BT_CHARACTER;
  f->ts.kind = string->ts.kind;
  f->value.function.name = gfc_get_string ("__trim_%d", string->ts.kind);
}


void
gfc_resolve_ubound (gfc_expr * f, gfc_expr * array,
		    gfc_expr * dim)
{
  static char ubound[] = "__ubound";

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;

  if (dim == NULL)
    {
      f->rank = 1;
      f->shape = gfc_get_shape (1);
      mpz_init_set_ui (f->shape[0], array->rank);
    }

  f->value.function.name = ubound;
}


void
gfc_resolve_unpack (gfc_expr * f, gfc_expr * vector, gfc_expr * mask,
		    gfc_expr * field ATTRIBUTE_UNUSED)
{

  f->ts.type = vector->ts.type;
  f->ts.kind = vector->ts.kind;
  f->rank = mask->rank;

  f->value.function.name =
    gfc_get_string ("__unpack%d", field->rank > 0 ? 1 : 0);
}


void
gfc_resolve_verify (gfc_expr * f, gfc_expr * string,
		    gfc_expr * set ATTRIBUTE_UNUSED,
		    gfc_expr * back ATTRIBUTE_UNUSED)
{

  f->ts.type = BT_INTEGER;
  f->ts.kind = gfc_default_integer_kind;
  f->value.function.name = gfc_get_string ("__verify_%d", string->ts.kind);
}


/* Intrinsic subroutine resolution.  */

void
gfc_resolve_cpu_time (gfc_code * c ATTRIBUTE_UNUSED)
{
  const char *name;

  name = gfc_get_string (PREFIX("cpu_time_%d"),
			 c->ext.actual->expr->ts.kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_mvbits (gfc_code * c)
{
  const char *name;
  int kind;

  kind = c->ext.actual->expr->ts.kind;
  name = gfc_get_string (PREFIX("mvbits_i%d"), kind);

  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


void
gfc_resolve_random_number (gfc_code * c ATTRIBUTE_UNUSED)
{
  const char *name;
  int kind;

  kind = c->ext.actual->expr->ts.kind;
  if (c->ext.actual->expr->rank == 0)
    name = gfc_get_string (PREFIX("random_r%d"), kind);
  else
    name = gfc_get_string (PREFIX("arandom_r%d"), kind);
  
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* G77 compatibility subroutines etime() and dtime().  */

void
gfc_resolve_etime_sub (gfc_code * c)
{
  const char *name;

  name = gfc_get_string (PREFIX("etime_sub"));
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* G77 compatibility subroutine second().  */

void
gfc_resolve_second_sub (gfc_code * c)
{
  const char *name;

  name = gfc_get_string (PREFIX("second_sub"));
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* G77 compatibility function srand().  */

void
gfc_resolve_srand (gfc_code * c)
{
  const char *name;
  name = gfc_get_string (PREFIX("srand"));
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the getarg intrinsic subroutine.  */

void
gfc_resolve_getarg (gfc_code * c)
{
  const char *name;
  int kind;

  kind = gfc_default_integer_kind;
  name = gfc_get_string (PREFIX("getarg_i%d"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}

/* Resolve the getcwd intrinsic subroutine.  */

void
gfc_resolve_getcwd_sub (gfc_code * c)
{
  const char *name;
  int kind;

  if (c->ext.actual->next->expr != NULL)
    kind = c->ext.actual->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX("getcwd_i%d_sub"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the get_command intrinsic subroutine.  */

void
gfc_resolve_get_command (gfc_code * c)
{
  const char *name;
  int kind;

  kind = gfc_default_integer_kind;
  name = gfc_get_string (PREFIX("get_command_i%d"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}


/* Resolve the get_command_argument intrinsic subroutine.  */

void
gfc_resolve_get_command_argument (gfc_code * c)
{
  const char *name;
  int kind;

  kind = gfc_default_integer_kind;
  name = gfc_get_string (PREFIX("get_command_argument_i%d"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}

/* Resolve the get_environment_variable intrinsic subroutine. */

void
gfc_resolve_get_environment_variable (gfc_code * code)
{
  const char *name;
  int kind;

  kind = gfc_default_integer_kind;
  name = gfc_get_string (PREFIX("get_environment_variable_i%d"), kind);
  code->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}

/* Resolve the SYSTEM intrinsic subroutine.  */

void
gfc_resolve_system_sub (gfc_code * c)
{
  const char *name;

  name = gfc_get_string (PREFIX("system_sub"));
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}

/* Determine if the arguments to SYSTEM_CLOCK are INTEGER(4) or INTEGER(8) */

void
gfc_resolve_system_clock (gfc_code * c)
{
  const char *name;
  int kind;

  if (c->ext.actual->expr != NULL)
    kind = c->ext.actual->expr->ts.kind;
  else if (c->ext.actual->next->expr != NULL)
      kind = c->ext.actual->next->expr->ts.kind;
  else if (c->ext.actual->next->next->expr != NULL)
      kind = c->ext.actual->next->next->expr->ts.kind;
  else
    kind = gfc_default_integer_kind;

  name = gfc_get_string (PREFIX("system_clock_%d"), kind);
  c->resolved_sym = gfc_get_intrinsic_sub_symbol (name);
}

void
gfc_iresolve_init_1 (void)
{
  int i;

  for (i = 0; i < HASH_SIZE; i++)
    string_head[i] = NULL;
}


void
gfc_iresolve_done_1 (void)
{

  free_strings ();
}
