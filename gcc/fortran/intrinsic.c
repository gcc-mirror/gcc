/* Build up a list of intrinsic subroutines and functions for the
   name-resolution stage.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation,
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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */


#include "config.h"
#include "system.h"
#include "flags.h"
#include "gfortran.h"
#include "intrinsic.h"


/* Namespace to hold the resolved symbols for intrinsic subroutines.  */
static gfc_namespace *gfc_intrinsic_namespace;

int gfc_init_expr = 0;

/* Pointers to an intrinsic function and its argument names that are being
   checked.  */

const char *gfc_current_intrinsic;
const char *gfc_current_intrinsic_arg[MAX_INTRINSIC_ARGS];
locus *gfc_current_intrinsic_where;

static gfc_intrinsic_sym *functions, *subroutines, *conversion, *next_sym;
static gfc_intrinsic_arg *next_arg;

static int nfunc, nsub, nargs, nconv;

static enum
{ SZ_NOTHING = 0, SZ_SUBS, SZ_FUNCS, SZ_CONVS }
sizing;

#define REQUIRED	0
#define OPTIONAL	1

/* Return a letter based on the passed type.  Used to construct the
   name of a type-dependent subroutine.  */

char
gfc_type_letter (bt type)
{
  char c;

  switch (type)
    {
    case BT_LOGICAL:
      c = 'l';
      break;
    case BT_CHARACTER:
      c = 's';
      break;
    case BT_INTEGER:
      c = 'i';
      break;
    case BT_REAL:
      c = 'r';
      break;
    case BT_COMPLEX:
      c = 'c';
      break;

    case BT_HOLLERITH:
      c = 'h';
      break;

    default:
      c = 'u';
      break;
    }

  return c;
}


/* Get a symbol for a resolved name.  */

gfc_symbol *
gfc_get_intrinsic_sub_symbol (const char * name)
{
  gfc_symbol *sym;

  gfc_get_symbol (name, gfc_intrinsic_namespace, &sym);
  sym->attr.always_explicit = 1;
  sym->attr.subroutine = 1;
  sym->attr.flavor = FL_PROCEDURE;
  sym->attr.proc = PROC_INTRINSIC;

  return sym;
}


/* Return a pointer to the name of a conversion function given two
   typespecs.  */

static const char *
conv_name (gfc_typespec * from, gfc_typespec * to)
{
  static char name[30];

  sprintf (name, "__convert_%c%d_%c%d", gfc_type_letter (from->type),
	   from->kind, gfc_type_letter (to->type), to->kind);

  return gfc_get_string (name);
}


/* Given a pair of typespecs, find the gfc_intrinsic_sym node that
   corresponds to the conversion.  Returns NULL if the conversion
   isn't found.  */

static gfc_intrinsic_sym *
find_conv (gfc_typespec * from, gfc_typespec * to)
{
  gfc_intrinsic_sym *sym;
  const char *target;
  int i;

  target = conv_name (from, to);
  sym = conversion;

  for (i = 0; i < nconv; i++, sym++)
    if (strcmp (target, sym->name) == 0)
      return sym;

  return NULL;
}


/* Interface to the check functions.  We break apart an argument list
   and call the proper check function rather than forcing each
   function to manipulate the argument list.  */

static try
do_check (gfc_intrinsic_sym * specific, gfc_actual_arglist * arg)
{
  gfc_expr *a1, *a2, *a3, *a4, *a5;

  if (arg == NULL)
    return (*specific->check.f0) ();

  a1 = arg->expr;
  arg = arg->next;
  if (arg == NULL)
    return (*specific->check.f1) (a1);

  a2 = arg->expr;
  arg = arg->next;
  if (arg == NULL)
    return (*specific->check.f2) (a1, a2);

  a3 = arg->expr;
  arg = arg->next;
  if (arg == NULL)
    return (*specific->check.f3) (a1, a2, a3);

  a4 = arg->expr;
  arg = arg->next;
  if (arg == NULL)
    return (*specific->check.f4) (a1, a2, a3, a4);

  a5 = arg->expr;
  arg = arg->next;
  if (arg == NULL)
    return (*specific->check.f5) (a1, a2, a3, a4, a5);

  gfc_internal_error ("do_check(): too many args");
}


/*********** Subroutines to build the intrinsic list ****************/

/* Add a single intrinsic symbol to the current list.

   Argument list:
      char *     name of function
      int        whether function is elemental
      int        If the function can be used as an actual argument
      bt         return type of function
      int        kind of return type of function
      int        Fortran standard version
      check      pointer to check function
      simplify   pointer to simplification function
      resolve    pointer to resolution function

   Optional arguments come in multiples of four:
      char *    name of argument
      bt        type of argument
      int       kind of argument
      int       arg optional flag (1=optional, 0=required)

   The sequence is terminated by a NULL name.

   TODO: Are checks on actual_ok implemented elsewhere, or is that just
   missing here?  */

static void
add_sym (const char *name, int elemental, int actual_ok ATTRIBUTE_UNUSED,
	 bt type, int kind, int standard, gfc_check_f check,
	 gfc_simplify_f simplify, gfc_resolve_f resolve, ...)
{
  char buf[GFC_MAX_SYMBOL_LEN + 11]; /* 10 for '_gfortran_', 1 for '\0'  */
  int optional, first_flag;
  va_list argp;

  /* First check that the intrinsic belongs to the selected standard.
     If not, don't add it to the symbol list.  */
  if (!(gfc_option.allow_std & standard))
    return;

  switch (sizing)
    {
    case SZ_SUBS:
      nsub++;
      break;

    case SZ_FUNCS:
      nfunc++;
      break;

    case SZ_NOTHING:
      next_sym->name = gfc_get_string (name);

      strcpy (buf, "_gfortran_");
      strcat (buf, name);
      next_sym->lib_name = gfc_get_string (buf);

      next_sym->elemental = elemental;
      next_sym->ts.type = type;
      next_sym->ts.kind = kind;
      next_sym->standard = standard;
      next_sym->simplify = simplify;
      next_sym->check = check;
      next_sym->resolve = resolve;
      next_sym->specific = 0;
      next_sym->generic = 0;
      break;

    default:
      gfc_internal_error ("add_sym(): Bad sizing mode");
    }

  va_start (argp, resolve);

  first_flag = 1;

  for (;;)
    {
      name = va_arg (argp, char *);
      if (name == NULL)
	break;

      type = (bt) va_arg (argp, int);
      kind = va_arg (argp, int);
      optional = va_arg (argp, int);

      if (sizing != SZ_NOTHING)
	nargs++;
      else
	{
	  next_arg++;

	  if (first_flag)
	    next_sym->formal = next_arg;
	  else
	    (next_arg - 1)->next = next_arg;

	  first_flag = 0;

	  strcpy (next_arg->name, name);
	  next_arg->ts.type = type;
	  next_arg->ts.kind = kind;
	  next_arg->optional = optional;
	}
    }

  va_end (argp);

  next_sym++;
}


/* Add a symbol to the function list where the function takes
   0 arguments.  */

static void
add_sym_0 (const char *name, int elemental, int actual_ok, bt type,
		       int kind, int standard,
		       try (*check)(void),
		       gfc_expr *(*simplify)(void),
	   void (*resolve)(gfc_expr *))
{
  gfc_simplify_f sf;
  gfc_check_f cf;
  gfc_resolve_f rf;

  cf.f0 = check;
  sf.f0 = simplify;
  rf.f0 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   (void*)0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   0 arguments.  */

static void
add_sym_0s (const char * name, int actual_ok, int standard,
	    void (*resolve)(gfc_code *))
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f1 = NULL;
  sf.f1 = NULL;
  rf.s1 = resolve;

  add_sym (name, 1, actual_ok, BT_UNKNOWN, 0, standard, cf, sf, rf,
	   (void*)0);
}


/* Add a symbol to the function list where the function takes
   1 arguments.  */

static void
add_sym_1 (const char *name, int elemental, int actual_ok, bt type,
	   int kind, int standard,
	   try (*check)(gfc_expr *),
	   gfc_expr *(*simplify)(gfc_expr *),
	   void (*resolve)(gfc_expr *,gfc_expr *),
	   const char* a1, bt type1, int kind1, int optional1)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f1 = check;
  sf.f1 = simplify;
  rf.f1 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   (void*)0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   1 arguments.  */

static void
add_sym_1s (const char *name, int elemental, int actual_ok, bt type,
			int kind, int standard,
			try (*check)(gfc_expr *),
			gfc_expr *(*simplify)(gfc_expr *),
			void (*resolve)(gfc_code *),
	    const char* a1, bt type1, int kind1, int optional1)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f1 = check;
  sf.f1 = simplify;
  rf.s1 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   (void*)0);
}


/* Add a symbol from the MAX/MIN family of intrinsic functions to the
   function.  MAX et al take 2 or more arguments.  */

static void
add_sym_1m (const char *name, int elemental, int actual_ok, bt type,
			int kind, int standard,
			try (*check)(gfc_actual_arglist *),
			gfc_expr *(*simplify)(gfc_expr *),
			void (*resolve)(gfc_expr *,gfc_actual_arglist *),
			const char* a1, bt type1, int kind1, int optional1,
	    const char* a2, bt type2, int kind2, int optional2)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f1m = check;
  sf.f1 = simplify;
  rf.f1m = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   a2, type2, kind2, optional2,
	   (void*)0);
}


/* Add a symbol to the function list where the function takes
   2 arguments.  */

static void
add_sym_2 (const char *name, int elemental, int actual_ok, bt type,
		       int kind, int standard,
		       try (*check)(gfc_expr *,gfc_expr *),
		       gfc_expr *(*simplify)(gfc_expr *,gfc_expr *),
		       void (*resolve)(gfc_expr *,gfc_expr *,gfc_expr *),
		       const char* a1, bt type1, int kind1, int optional1,
	   const char* a2, bt type2, int kind2, int optional2)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f2 = check;
  sf.f2 = simplify;
  rf.f2 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   a2, type2, kind2, optional2,
	   (void*)0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   2 arguments.  */

static void
add_sym_2s (const char *name, int elemental, int actual_ok, bt type,
			int kind, int standard,
		       try (*check)(gfc_expr *,gfc_expr *),
		       gfc_expr *(*simplify)(gfc_expr *,gfc_expr *),
		       void (*resolve)(gfc_code *),
		       const char* a1, bt type1, int kind1, int optional1,
	    const char* a2, bt type2, int kind2, int optional2)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f2 = check;
  sf.f2 = simplify;
  rf.s1 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   a2, type2, kind2, optional2,
	   (void*)0);
}


/* Add a symbol to the function list where the function takes
   3 arguments.  */

static void
add_sym_3 (const char *name, int elemental, int actual_ok, bt type,
		       int kind, int standard,
		       try (*check)(gfc_expr *,gfc_expr *,gfc_expr *),
		       gfc_expr *(*simplify)(gfc_expr *,gfc_expr *,gfc_expr *),
		       void (*resolve)(gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *),
		       const char* a1, bt type1, int kind1, int optional1,
		       const char* a2, bt type2, int kind2, int optional2,
	   const char* a3, bt type3, int kind3, int optional3)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f3 = check;
  sf.f3 = simplify;
  rf.f3 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   a2, type2, kind2, optional2,
	   a3, type3, kind3, optional3,
	   (void*)0);
}


/* MINLOC and MAXLOC get special treatment because their argument
   might have to be reordered.  */

static void
add_sym_3ml (const char *name, int elemental, 
			 int actual_ok, bt type, int kind, int standard,
			 try (*check)(gfc_actual_arglist *),
			 gfc_expr*(*simplify)(gfc_expr *,gfc_expr *,gfc_expr *),
			 void (*resolve)(gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *),
			 const char* a1, bt type1, int kind1, int optional1,
			 const char* a2, bt type2, int kind2, int optional2,
	     const char* a3, bt type3, int kind3, int optional3)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f3ml = check;
  sf.f3 = simplify;
  rf.f3 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   a2, type2, kind2, optional2,
	   a3, type3, kind3, optional3,
	   (void*)0);
}


/* MINVAL, MAXVAL, PRODUCT, and SUM also get special treatment because
   their argument also might have to be reordered.  */

static void
add_sym_3red (const char *name, int elemental, 
                          int actual_ok, bt type, int kind, int standard,
                          try (*check)(gfc_actual_arglist *),
                          gfc_expr*(*simplify)(gfc_expr *,gfc_expr *,gfc_expr *),
                          void (*resolve)(gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *),
                          const char* a1, bt type1, int kind1, int optional1,
                          const char* a2, bt type2, int kind2, int optional2,
	      const char* a3, bt type3, int kind3, int optional3)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f3red = check;
  sf.f3 = simplify;
  rf.f3 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   a2, type2, kind2, optional2,
	   a3, type3, kind3, optional3,
	   (void*)0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   3 arguments.  */

static void
add_sym_3s (const char *name, int elemental, int actual_ok, bt type,
			int kind, int standard,
		       try (*check)(gfc_expr *,gfc_expr *,gfc_expr *),
		       gfc_expr *(*simplify)(gfc_expr *,gfc_expr *,gfc_expr *),
		       void (*resolve)(gfc_code *),
		       const char* a1, bt type1, int kind1, int optional1,
		       const char* a2, bt type2, int kind2, int optional2,
	    const char* a3, bt type3, int kind3, int optional3)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f3 = check;
  sf.f3 = simplify;
  rf.s1 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   a2, type2, kind2, optional2,
	   a3, type3, kind3, optional3,
	   (void*)0);
}


/* Add a symbol to the function list where the function takes
   4 arguments.  */

static void
add_sym_4 (const char *name, int elemental, int actual_ok, bt type,
		       int kind, int standard,
		       try (*check)(gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *),
		       gfc_expr *(*simplify)(gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *),
		       void (*resolve)(gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *),
		       const char* a1, bt type1, int kind1, int optional1,
		       const char* a2, bt type2, int kind2, int optional2,
		       const char* a3, bt type3, int kind3, int optional3,
	   const char* a4, bt type4, int kind4, int optional4 )
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f4 = check;
  sf.f4 = simplify;
  rf.f4 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   a2, type2, kind2, optional2,
	   a3, type3, kind3, optional3,
	   a4, type4, kind4, optional4,
	   (void*)0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   4 arguments.  */

static void
add_sym_4s (const char *name, int elemental, int actual_ok,
			bt type, int kind, int standard,
    try (*check)(gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *),
    gfc_expr *(*simplify)(gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *),
    void (*resolve)(gfc_code *),
    const char* a1, bt type1, int kind1, int optional1,
    const char* a2, bt type2, int kind2, int optional2,
    const char* a3, bt type3, int kind3, int optional3,
    const char* a4, bt type4, int kind4, int optional4)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f4 = check;
  sf.f4 = simplify;
  rf.s1 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   a2, type2, kind2, optional2,
	   a3, type3, kind3, optional3,
	   a4, type4, kind4, optional4,
	   (void*)0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   5 arguments.  */

static void
add_sym_5s (const char *name, int elemental, int actual_ok, 
 bt type, int kind, int standard,
 try (*check)(gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *),
 gfc_expr *(*simplify)(gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *,gfc_expr *),
 void (*resolve)(gfc_code *),
 const char* a1, bt type1, int kind1, int optional1,
 const char* a2, bt type2, int kind2, int optional2,
 const char* a3, bt type3, int kind3, int optional3,
 const char* a4, bt type4, int kind4, int optional4,
 const char* a5, bt type5, int kind5, int optional5) 
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f5 = check;
  sf.f5 = simplify;
  rf.s1 = resolve;

  add_sym (name, elemental, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1,
	   a2, type2, kind2, optional2,
	   a3, type3, kind3, optional3,
	   a4, type4, kind4, optional4,
	   a5, type5, kind5, optional5,
	   (void*)0);
}


/* Locate an intrinsic symbol given a base pointer, number of elements
   in the table and a pointer to a name.  Returns the NULL pointer if
   a name is not found.  */

static gfc_intrinsic_sym *
find_sym (gfc_intrinsic_sym * start, int n, const char *name)
{

  while (n > 0)
    {
      if (strcmp (name, start->name) == 0)
	return start;

      start++;
      n--;
    }

  return NULL;
}


/* Given a name, find a function in the intrinsic function table.
   Returns NULL if not found.  */

gfc_intrinsic_sym *
gfc_find_function (const char *name)
{

  return find_sym (functions, nfunc, name);
}


/* Given a name, find a function in the intrinsic subroutine table.
   Returns NULL if not found.  */

static gfc_intrinsic_sym *
find_subroutine (const char *name)
{

  return find_sym (subroutines, nsub, name);
}


/* Given a string, figure out if it is the name of a generic intrinsic
   function or not.  */

int
gfc_generic_intrinsic (const char *name)
{
  gfc_intrinsic_sym *sym;

  sym = gfc_find_function (name);
  return (sym == NULL) ? 0 : sym->generic;
}


/* Given a string, figure out if it is the name of a specific
   intrinsic function or not.  */

int
gfc_specific_intrinsic (const char *name)
{
  gfc_intrinsic_sym *sym;

  sym = gfc_find_function (name);
  return (sym == NULL) ? 0 : sym->specific;
}


/* Given a string, figure out if it is the name of an intrinsic
   subroutine or function.  There are no generic intrinsic
   subroutines, they are all specific.  */

int
gfc_intrinsic_name (const char *name, int subroutine_flag)
{

  return subroutine_flag ?
    find_subroutine (name) != NULL : gfc_find_function (name) != NULL;
}


/* Collect a set of intrinsic functions into a generic collection.
   The first argument is the name of the generic function, which is
   also the name of a specific function.  The rest of the specifics
   currently in the table are placed into the list of specific
   functions associated with that generic.  */

static void
make_generic (const char *name, gfc_generic_isym_id generic_id, int standard)
{
  gfc_intrinsic_sym *g;

  if (!(gfc_option.allow_std & standard))
    return;

  if (sizing != SZ_NOTHING)
    return;

  g = gfc_find_function (name);
  if (g == NULL)
    gfc_internal_error ("make_generic(): Can't find generic symbol '%s'",
			name);

  g->generic = 1;
  g->specific = 1;
  g->generic_id = generic_id;
  if ((g + 1)->name != NULL)
    g->specific_head = g + 1;
  g++;

  while (g->name != NULL)
    {
      g->next = g + 1;
      g->specific = 1;
      g->generic_id = generic_id;
      g++;
    }

  g--;
  g->next = NULL;
}


/* Create a duplicate intrinsic function entry for the current
   function, the only difference being the alternate name.  Note that
   we use argument lists more than once, but all argument lists are
   freed as a single block.  */

static void
make_alias (const char *name, int standard)
{

  /* First check that the intrinsic belongs to the selected standard.
     If not, don't add it to the symbol list.  */
  if (!(gfc_option.allow_std & standard))
    return;

  switch (sizing)
    {
    case SZ_FUNCS:
      nfunc++;
      break;

    case SZ_SUBS:
      nsub++;
      break;

    case SZ_NOTHING:
      next_sym[0] = next_sym[-1];
      next_sym->name = gfc_get_string (name);
      next_sym++;
      break;

    default:
      break;
    }
}

/* Make the current subroutine noreturn.  */

static void
make_noreturn(void)
{
  if (sizing == SZ_NOTHING)
      next_sym[-1].noreturn = 1;
}

/* Add intrinsic functions.  */

static void
add_functions (void)
{

  /* Argument names as in the standard (to be used as argument keywords).  */
  const char
    *a = "a", *f = "field", *pt = "pointer", *tg = "target",
    *b = "b", *m = "matrix", *ma = "matrix_a", *mb = "matrix_b",
    *c = "c", *n = "ncopies", *pos = "pos", *bck = "back",
    *i = "i", *v = "vector", *va = "vector_a", *vb = "vector_b",
    *j = "j", *a1 = "a1", *fs = "fsource", *ts = "tsource",
    *l = "l", *a2 = "a2", *mo = "mold", *ord = "order",
    *p = "p", *ar = "array", *shp = "shape", *src = "source",
    *r = "r", *bd = "boundary", *pad = "pad", *set = "set",
    *s = "s", *dm = "dim", *kind = "kind", *msk = "mask",
    *x = "x", *sh = "shift", *stg = "string", *ssg = "substring",
    *y = "y", *sz = "size", *sta = "string_a", *stb = "string_b",
    *z = "z", *ln = "len", *ut = "unit", *han = "handler",
    *num = "number", *tm = "time";

  int di, dr, dd, dl, dc, dz, ii;

  di = gfc_default_integer_kind;
  dr = gfc_default_real_kind;
  dd = gfc_default_double_kind;
  dl = gfc_default_logical_kind;
  dc = gfc_default_character_kind;
  dz = gfc_default_complex_kind;
  ii = gfc_index_integer_kind;

  add_sym_1 ("abs", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_abs, gfc_simplify_abs, gfc_resolve_abs,
	     a, BT_REAL, dr, REQUIRED);

  add_sym_1 ("iabs", 1, 1, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_abs, gfc_resolve_abs,
	     a, BT_INTEGER, di, REQUIRED);

  add_sym_1 ("dabs", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_abs, gfc_resolve_abs,
	     a, BT_REAL, dd, REQUIRED);

  add_sym_1 ("cabs", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     NULL, gfc_simplify_abs, gfc_resolve_abs,
	     a, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zabs", 1, 1, BT_REAL, dd, GFC_STD_GNU, 
	     NULL, gfc_simplify_abs, gfc_resolve_abs, 
	     a, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdabs", GFC_STD_GNU);

  make_generic ("abs", GFC_ISYM_ABS, GFC_STD_F77);

  add_sym_1 ("achar", 1, 1, BT_CHARACTER, dc, GFC_STD_F95,
	     gfc_check_achar, gfc_simplify_achar, NULL,
	     i, BT_INTEGER, di, REQUIRED);

  make_generic ("achar", GFC_ISYM_ACHAR, GFC_STD_F95);

  add_sym_1 ("acos", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_r, gfc_simplify_acos, gfc_resolve_acos,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dacos", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_acos, gfc_resolve_acos,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("acos", GFC_ISYM_ACOS, GFC_STD_F77);

  add_sym_1 ("acosh", 1, 1, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_acosh, gfc_resolve_acosh,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dacosh", 1, 1, BT_REAL, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_acosh, gfc_resolve_acosh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("acosh", GFC_ISYM_ACOSH, GFC_STD_GNU);

  add_sym_1 ("adjustl", 1, 1, BT_CHARACTER, dc, GFC_STD_F95,
	     NULL, gfc_simplify_adjustl, NULL,
	     stg, BT_CHARACTER, dc, REQUIRED);

  make_generic ("adjustl", GFC_ISYM_ADJUSTL, GFC_STD_F95);

  add_sym_1 ("adjustr", 1, 1, BT_CHARACTER, dc, GFC_STD_F95,
	     NULL, gfc_simplify_adjustr, NULL,
	     stg, BT_CHARACTER, dc, REQUIRED);

  make_generic ("adjustr", GFC_ISYM_ADJUSTR, GFC_STD_F95);

  add_sym_1 ("aimag", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_c, gfc_simplify_aimag, gfc_resolve_aimag,
	     z, BT_COMPLEX, dz, REQUIRED);

  make_alias ("imag", GFC_STD_GNU);
  make_alias ("imagpart", GFC_STD_GNU);

  add_sym_1 ("dimag", 1, 1, BT_REAL, dd, GFC_STD_GNU, 
	     NULL, gfc_simplify_aimag, gfc_resolve_aimag, 
	     z, BT_COMPLEX, dd, REQUIRED);


  make_generic ("aimag", GFC_ISYM_AIMAG, GFC_STD_F77);

  add_sym_2 ("aint", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_a_xkind, gfc_simplify_aint, gfc_resolve_aint,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  add_sym_1 ("dint", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_dint, gfc_resolve_dint,
	     a, BT_REAL, dd, REQUIRED);

  make_generic ("aint", GFC_ISYM_AINT, GFC_STD_F77);

  add_sym_2 ("all", 0, 1, BT_UNKNOWN, 0, GFC_STD_F95,
	     gfc_check_all_any, NULL, gfc_resolve_all,
	     msk, BT_LOGICAL, dl, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("all", GFC_ISYM_ALL, GFC_STD_F95);

  add_sym_1 ("allocated", 0, 1, BT_LOGICAL, dl, GFC_STD_F95,
	     gfc_check_allocated, NULL, NULL,
	     ar, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("allocated", GFC_ISYM_ALLOCATED, GFC_STD_F95);

  add_sym_2 ("anint", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_a_xkind, gfc_simplify_anint, gfc_resolve_anint,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  add_sym_1 ("dnint", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_dnint, gfc_resolve_dnint,
	     a, BT_REAL, dd, REQUIRED);

  make_generic ("anint", GFC_ISYM_ANINT, GFC_STD_F77);

  add_sym_2 ("any", 0, 1, BT_UNKNOWN, 0, GFC_STD_F95,
	     gfc_check_all_any, NULL, gfc_resolve_any,
	     msk, BT_LOGICAL, dl, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("any", GFC_ISYM_ANY, GFC_STD_F95);

  add_sym_1 ("asin", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_r, gfc_simplify_asin, gfc_resolve_asin,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dasin", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_asin, gfc_resolve_asin,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("asin", GFC_ISYM_ASIN, GFC_STD_F77);
  
  add_sym_1 ("asinh", 1, 1, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_asinh, gfc_resolve_asinh,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dasinh", 1, 1, BT_REAL, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_asinh, gfc_resolve_asinh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("asinh", GFC_ISYM_ASINH, GFC_STD_GNU);

  add_sym_2 ("associated", 0, 1, BT_LOGICAL, dl, GFC_STD_F95,
	     gfc_check_associated, NULL, NULL,
	     pt, BT_UNKNOWN, 0, REQUIRED, tg, BT_UNKNOWN, 0, OPTIONAL);

  make_generic ("associated", GFC_ISYM_ASSOCIATED, GFC_STD_F95);

  add_sym_1 ("atan", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_r, gfc_simplify_atan, gfc_resolve_atan,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("datan", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_atan, gfc_resolve_atan,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("atan", GFC_ISYM_ATAN, GFC_STD_F77);
  
  add_sym_1 ("atanh", 1, 1, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_atanh, gfc_resolve_atanh,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("datanh", 1, 1, BT_REAL, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_atanh, gfc_resolve_atanh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("atanh", GFC_ISYM_ATANH, GFC_STD_GNU);

  add_sym_2 ("atan2", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_atan2, gfc_simplify_atan2, gfc_resolve_atan2,
	     y, BT_REAL, dr, REQUIRED, x, BT_REAL, dr, REQUIRED);

  add_sym_2 ("datan2", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_atan2, gfc_resolve_atan2,
	     y, BT_REAL, dd, REQUIRED, x, BT_REAL, dd, REQUIRED);

  make_generic ("atan2", GFC_ISYM_ATAN2, GFC_STD_F77);
  
  /* Bessel and Neumann functions for G77 compatibility.  */
  add_sym_1 ("besj0", 1, 0, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dbesj0", 1, 0, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("besj0", GFC_ISYM_J0, GFC_STD_GNU);

  add_sym_1 ("besj1", 1, 0, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dbesj1", 1, 0, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("besj1", GFC_ISYM_J1, GFC_STD_GNU);

  add_sym_2 ("besjn", 1, 0, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_besn, NULL, gfc_resolve_besn,
	     n, BT_INTEGER, di, REQUIRED, x, BT_REAL, dr, REQUIRED);

  add_sym_2 ("dbesjn", 1, 0, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_besn, NULL, gfc_resolve_besn,
	     n, BT_INTEGER, di, REQUIRED, x, BT_REAL, dd, REQUIRED);

  make_generic ("besjn", GFC_ISYM_JN, GFC_STD_GNU);

  add_sym_1 ("besy0", 1, 0, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dbesy0", 1, 0, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("besy0", GFC_ISYM_Y0, GFC_STD_GNU);

  add_sym_1 ("besy1", 1, 0, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dbesy1", 1, 0, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("besy1", GFC_ISYM_Y1, GFC_STD_GNU);

  add_sym_2 ("besyn", 1, 0, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_besn, NULL, gfc_resolve_besn,
	     n, BT_INTEGER, di, REQUIRED, x, BT_REAL, dr, REQUIRED);

  add_sym_2 ("dbesyn", 1, 0, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_besn, NULL, gfc_resolve_besn,
	     n, BT_INTEGER, di, REQUIRED, x, BT_REAL, dd, REQUIRED);

  make_generic ("besyn", GFC_ISYM_YN, GFC_STD_GNU);

  add_sym_1 ("bit_size", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_i, gfc_simplify_bit_size, NULL,
	     i, BT_INTEGER, di, REQUIRED);

  make_generic ("bit_size", GFC_ISYM_NONE, GFC_STD_F95);

  add_sym_2 ("btest", 1, 1, BT_LOGICAL, dl, GFC_STD_F95,
	     gfc_check_btest, gfc_simplify_btest, gfc_resolve_btest,
	     i, BT_INTEGER, di, REQUIRED, pos, BT_INTEGER, di, REQUIRED);

  make_generic ("btest", GFC_ISYM_BTEST, GFC_STD_F95);

  add_sym_2 ("ceiling", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_a_ikind, gfc_simplify_ceiling, gfc_resolve_ceiling,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("ceiling", GFC_ISYM_CEILING, GFC_STD_F95);

  add_sym_2 ("char", 1, 0, BT_CHARACTER, dc, GFC_STD_F77,
	     gfc_check_char, gfc_simplify_char, gfc_resolve_char,
	     i, BT_INTEGER, di, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("char", GFC_ISYM_CHAR, GFC_STD_F77);

  add_sym_1 ("chdir", 0, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_chdir, NULL, gfc_resolve_chdir,
	     a, BT_CHARACTER, dc, REQUIRED);

  make_generic ("chdir", GFC_ISYM_CHDIR, GFC_STD_GNU);
  
  add_sym_3 ("cmplx", 1, 1, BT_COMPLEX, dz, GFC_STD_F77,
	     gfc_check_cmplx, gfc_simplify_cmplx, gfc_resolve_cmplx,
	     x, BT_UNKNOWN, dr, REQUIRED, y, BT_UNKNOWN, dr, OPTIONAL,
	     kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("cmplx", GFC_ISYM_CMPLX, GFC_STD_F77);

  /* Making dcmplx a specific of cmplx causes cmplx to return a double
     complex instead of the default complex.  */

  add_sym_2 ("dcmplx", 1, 1, BT_COMPLEX, dd, GFC_STD_GNU,
	     gfc_check_dcmplx, gfc_simplify_dcmplx, gfc_resolve_dcmplx,
	     x, BT_REAL, dd, REQUIRED, y, BT_REAL, dd, OPTIONAL);

  make_generic ("dcmplx", GFC_ISYM_CMPLX, GFC_STD_GNU);

  add_sym_1 ("conjg", 1, 1, BT_COMPLEX, dz, GFC_STD_F77,
	     gfc_check_fn_c, gfc_simplify_conjg, gfc_resolve_conjg,
	     z, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("dconjg", 1, 1, BT_COMPLEX, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_conjg, gfc_resolve_conjg, 
	     z, BT_COMPLEX, dd, REQUIRED);

  make_generic ("conjg", GFC_ISYM_CONJG, GFC_STD_F77);

  add_sym_1 ("cos", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_cos, gfc_resolve_cos,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dcos", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_cos, gfc_resolve_cos,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("ccos", 1, 1, BT_COMPLEX, dz, GFC_STD_F77,
	     NULL, gfc_simplify_cos, gfc_resolve_cos,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zcos", 1, 1, BT_COMPLEX, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_cos, gfc_resolve_cos, 
	     x, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdcos", GFC_STD_GNU);

  make_generic ("cos", GFC_ISYM_COS, GFC_STD_F77);

  add_sym_1 ("cosh", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_r, gfc_simplify_cosh, gfc_resolve_cosh,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dcosh", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_cosh, gfc_resolve_cosh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("cosh", GFC_ISYM_COSH, GFC_STD_F77);

  add_sym_2 ("count", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_count, NULL, gfc_resolve_count,
	     msk, BT_LOGICAL, dl, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("count", GFC_ISYM_COUNT, GFC_STD_F95);

  add_sym_3 ("cshift", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_cshift, NULL, gfc_resolve_cshift,
	     ar, BT_REAL, dr, REQUIRED, sh, BT_INTEGER, di, REQUIRED,
	     dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("cshift", GFC_ISYM_CSHIFT, GFC_STD_F95);

  add_sym_1 ("ctime", 0, 1, BT_CHARACTER, 0, GFC_STD_GNU,
              gfc_check_ctime, NULL, gfc_resolve_ctime,
	      tm, BT_INTEGER, di, REQUIRED);

  make_generic ("ctime", GFC_ISYM_CTIME, GFC_STD_GNU);

  add_sym_1 ("dble", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_dble, gfc_simplify_dble, gfc_resolve_dble,
	     a, BT_REAL, dr, REQUIRED);

  make_alias ("dfloat", GFC_STD_GNU);

  make_generic ("dble", GFC_ISYM_DBLE, GFC_STD_F77);

  add_sym_1 ("digits", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_digits, gfc_simplify_digits, NULL,
	     x, BT_UNKNOWN, dr, REQUIRED);

  make_generic ("digits", GFC_ISYM_NONE, GFC_STD_F95);

  add_sym_2 ("dim", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_a_p, gfc_simplify_dim, gfc_resolve_dim,
	     x, BT_UNKNOWN, dr, REQUIRED, y, BT_UNKNOWN, dr, REQUIRED);

  add_sym_2 ("idim", 1, 1, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_dim, gfc_resolve_dim,
	     x, BT_INTEGER, di, REQUIRED, y, BT_INTEGER, di, REQUIRED);

  add_sym_2 ("ddim", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_dim, gfc_resolve_dim,
	     x, BT_REAL, dd, REQUIRED, y, BT_REAL, dd, REQUIRED);

  make_generic ("dim", GFC_ISYM_DIM, GFC_STD_F77);

  add_sym_2 ("dot_product", 0, 1, BT_UNKNOWN, 0, GFC_STD_F95,
	     gfc_check_dot_product, NULL, gfc_resolve_dot_product,
	     va, BT_REAL, dr, REQUIRED, vb, BT_REAL, dr, REQUIRED);

  make_generic ("dot_product", GFC_ISYM_DOT_PRODUCT, GFC_STD_F95);

  add_sym_2 ("dprod", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_dprod, gfc_resolve_dprod,
	     x, BT_REAL, dr, REQUIRED, y, BT_REAL, dr, REQUIRED);

  make_generic ("dprod", GFC_ISYM_DPROD, GFC_STD_F77);

  add_sym_1 ("dreal", 1, 0, BT_REAL, dd, GFC_STD_GNU,
	     NULL, NULL, NULL,
	     a, BT_COMPLEX, dd, REQUIRED);

  make_generic ("dreal", GFC_ISYM_REAL, GFC_STD_GNU);

  add_sym_4 ("eoshift", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_eoshift, NULL, gfc_resolve_eoshift,
	     ar, BT_REAL, dr, 0, sh, BT_INTEGER, ii, REQUIRED,
	     bd, BT_REAL, dr, 1, dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("eoshift", GFC_ISYM_EOSHIFT, GFC_STD_F95);

  add_sym_1 ("epsilon", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_x, gfc_simplify_epsilon, NULL,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("epsilon", GFC_ISYM_NONE, GFC_STD_F95);

  /* G77 compatibility for the ERF() and ERFC() functions.  */
  add_sym_1 ("erf", 1, 0, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("derf", 1, 0, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("erf", GFC_ISYM_ERF, GFC_STD_GNU);

  add_sym_1 ("erfc", 1, 0, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("derfc", 1, 0, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_g77_math1, NULL, gfc_resolve_g77_math1,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("erfc", GFC_ISYM_ERFC, GFC_STD_GNU);

  /* G77 compatibility */
  add_sym_1 ("etime", 0, 1, BT_REAL, 4,  GFC_STD_GNU,
	     gfc_check_etime, NULL, NULL,
	     x, BT_REAL, 4, REQUIRED);

  make_alias ("dtime", GFC_STD_GNU);

  make_generic ("etime", GFC_ISYM_ETIME, GFC_STD_GNU);

  add_sym_1 ("exp", 1, 1, BT_REAL, dr,  GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_exp, gfc_resolve_exp,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dexp", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_exp, gfc_resolve_exp,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("cexp", 1, 1, BT_COMPLEX, dz, GFC_STD_F77,
	     NULL, gfc_simplify_exp, gfc_resolve_exp,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zexp", 1, 1, BT_COMPLEX, dd,  GFC_STD_GNU,
	     NULL, gfc_simplify_exp, gfc_resolve_exp, 
	     x, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdexp", GFC_STD_GNU);

  make_generic ("exp", GFC_ISYM_EXP, GFC_STD_F77);

  add_sym_1 ("exponent", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_x, gfc_simplify_exponent, gfc_resolve_exponent,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("exponent", GFC_ISYM_EXPONENT, GFC_STD_F95);

  add_sym_0 ("fdate", 1, 0, BT_CHARACTER, dc, GFC_STD_GNU,
	     NULL, NULL, gfc_resolve_fdate);

  make_generic ("fdate", GFC_ISYM_FDATE, GFC_STD_GNU);

  add_sym_2 ("floor", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_a_ikind, gfc_simplify_floor, gfc_resolve_floor,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("floor", GFC_ISYM_FLOOR, GFC_STD_F95);

  /* G77 compatible fnum */
  add_sym_1 ("fnum", 0, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_fnum, NULL, gfc_resolve_fnum,
	     ut, BT_INTEGER, di, REQUIRED);

  make_generic ("fnum", GFC_ISYM_FNUM, GFC_STD_GNU);

  add_sym_1 ("fraction", 1, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_x, gfc_simplify_fraction, gfc_resolve_fraction,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("fraction", GFC_ISYM_FRACTION, GFC_STD_F95);

  add_sym_2 ("fstat", 0, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_fstat, NULL, gfc_resolve_fstat,
	     a, BT_INTEGER, di, REQUIRED, b, BT_INTEGER, di, REQUIRED);

  make_generic ("fstat", GFC_ISYM_FSTAT, GFC_STD_GNU);

  /* Unix IDs (g77 compatibility)  */
  add_sym_1 ("getcwd", 0, 1, BT_INTEGER, di,  GFC_STD_GNU,
	     NULL, NULL, gfc_resolve_getcwd,
	     c, BT_CHARACTER, dc, REQUIRED);

  make_generic ("getcwd", GFC_ISYM_GETCWD, GFC_STD_GNU);

  add_sym_0 ("getgid", 1, 0, BT_INTEGER, di, GFC_STD_GNU,
	     NULL, NULL, gfc_resolve_getgid);

  make_generic ("getgid", GFC_ISYM_GETGID, GFC_STD_GNU);

  add_sym_0 ("getpid", 1, 0, BT_INTEGER, di, GFC_STD_GNU, 
	     NULL, NULL, gfc_resolve_getpid);

  make_generic ("getpid", GFC_ISYM_GETPID, GFC_STD_GNU);

  add_sym_0 ("getuid", 1, 0, BT_INTEGER, di, GFC_STD_GNU, 
	     NULL, NULL, gfc_resolve_getuid);

  make_generic ("getuid", GFC_ISYM_GETUID, GFC_STD_GNU);

  add_sym_1 ("hostnm", 0, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_hostnm, NULL, gfc_resolve_hostnm,
	     a, BT_CHARACTER, dc, REQUIRED);

  make_generic ("hostnm", GFC_ISYM_HOSTNM, GFC_STD_GNU);

  add_sym_1 ("huge", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_huge, gfc_simplify_huge, NULL,
	     x, BT_UNKNOWN, dr, REQUIRED);

  make_generic ("huge", GFC_ISYM_NONE, GFC_STD_F95);

  add_sym_1 ("iachar", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ichar_iachar, gfc_simplify_iachar, NULL,
	     c, BT_CHARACTER, dc, REQUIRED);

  make_generic ("iachar", GFC_ISYM_IACHAR, GFC_STD_F95);

  add_sym_2 ("iand", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_iand, gfc_simplify_iand, gfc_resolve_iand,
	     i, BT_INTEGER, di, REQUIRED, j, BT_INTEGER, di, REQUIRED);

  make_generic ("iand", GFC_ISYM_IAND, GFC_STD_F95);

  add_sym_0 ("iargc", 1, 1, BT_INTEGER, di, GFC_STD_GNU,
	     NULL, NULL, NULL);

  make_generic ("iargc", GFC_ISYM_IARGC, GFC_STD_GNU);

  add_sym_0 ("command_argument_count", 1, 1, BT_INTEGER, di, GFC_STD_F2003,
	     NULL, NULL, NULL);

  make_generic ("command_argument_count", GFC_ISYM_COMMAND_ARGUMENT_COUNT,
	        GFC_STD_F2003);

  add_sym_2 ("ibclr", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ibclr, gfc_simplify_ibclr, gfc_resolve_ibclr,
	     i, BT_INTEGER, di, REQUIRED, pos, BT_INTEGER, di, REQUIRED);

  make_generic ("ibclr", GFC_ISYM_IBCLR, GFC_STD_F95);

  add_sym_3 ("ibits", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ibits, gfc_simplify_ibits, gfc_resolve_ibits,
	     i, BT_INTEGER, di, REQUIRED, pos, BT_INTEGER, di, REQUIRED,
	     ln, BT_INTEGER, di, REQUIRED);

  make_generic ("ibits", GFC_ISYM_IBITS, GFC_STD_F95);

  add_sym_2 ("ibset", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ibset, gfc_simplify_ibset, gfc_resolve_ibset,
	     i, BT_INTEGER, di, REQUIRED, pos, BT_INTEGER, di, REQUIRED);

  make_generic ("ibset", GFC_ISYM_IBSET, GFC_STD_F95);

  add_sym_1 ("ichar", 1, 0, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_ichar_iachar, gfc_simplify_ichar, gfc_resolve_ichar,
	     c, BT_CHARACTER, dc, REQUIRED);

  make_generic ("ichar", GFC_ISYM_ICHAR, GFC_STD_F77);

  add_sym_2 ("ieor", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ieor, gfc_simplify_ieor, gfc_resolve_ieor,
	     i, BT_INTEGER, di, REQUIRED, j, BT_INTEGER, di, REQUIRED);

  make_generic ("ieor", GFC_ISYM_IEOR, GFC_STD_F95);

  add_sym_0 ("ierrno", 1, 0, BT_INTEGER, di, GFC_STD_GNU,
	     NULL, NULL, gfc_resolve_ierrno);

  make_generic ("ierrno", GFC_ISYM_IERRNO, GFC_STD_GNU);

  add_sym_3 ("index", 1, 1, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_index, gfc_simplify_index, NULL,
	     stg, BT_CHARACTER, dc, REQUIRED, ssg, BT_CHARACTER, dc, REQUIRED,
	     bck, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("index", GFC_ISYM_INDEX, GFC_STD_F77);

  add_sym_2 ("int", 1, 1, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_int, gfc_simplify_int, gfc_resolve_int,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  add_sym_1 ("ifix", 1, 0, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_ifix, NULL,
	     a, BT_REAL, dr, REQUIRED);

  add_sym_1 ("idint", 1, 0, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_idint, NULL,
	     a, BT_REAL, dd, REQUIRED);

  make_generic ("int", GFC_ISYM_INT, GFC_STD_F77);

  add_sym_2 ("ior", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ior, gfc_simplify_ior, gfc_resolve_ior,
	     i, BT_INTEGER, di, REQUIRED, j, BT_INTEGER, di, REQUIRED);

  make_generic ("ior", GFC_ISYM_IOR, GFC_STD_F95);

  /* The following function is for G77 compatibility.  */
  add_sym_1 ("irand", 0, 1, BT_INTEGER, 4, GFC_STD_GNU,
             gfc_check_irand, NULL, NULL,
	     i, BT_INTEGER, 4, OPTIONAL);

  make_generic ("irand", GFC_ISYM_IRAND, GFC_STD_GNU);

  add_sym_1 ("isatty", 0, 0, BT_LOGICAL, dl, GFC_STD_GNU,
	     gfc_check_isatty, NULL, gfc_resolve_isatty,
	     ut, BT_INTEGER, di, REQUIRED);

  make_generic ("isatty", GFC_ISYM_ISATTY, GFC_STD_GNU);

  add_sym_2 ("ishft", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ishft, gfc_simplify_ishft, gfc_resolve_ishft,
	     i, BT_INTEGER, di, REQUIRED, sh, BT_INTEGER, di, REQUIRED);

  make_generic ("ishft", GFC_ISYM_ISHFT, GFC_STD_F95);

  add_sym_3 ("ishftc", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ishftc, gfc_simplify_ishftc, gfc_resolve_ishftc,
	     i, BT_INTEGER, di, REQUIRED, sh, BT_INTEGER, di, REQUIRED,
	     sz, BT_INTEGER, di, OPTIONAL);

  make_generic ("ishftc", GFC_ISYM_ISHFTC, GFC_STD_F95);

  add_sym_2 ("kill", 1, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_kill, NULL, gfc_resolve_kill,
	     a, BT_INTEGER, di, REQUIRED, b, BT_INTEGER, di, REQUIRED);

  make_generic ("kill", GFC_ISYM_KILL, GFC_STD_GNU);

  add_sym_1 ("kind", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_kind, gfc_simplify_kind, NULL,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("kind", GFC_ISYM_NONE, GFC_STD_F95);

  add_sym_2 ("lbound", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_lbound, gfc_simplify_lbound, gfc_resolve_lbound,
	     ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, di, OPTIONAL);

  make_generic ("lbound", GFC_ISYM_LBOUND, GFC_STD_F95);

  add_sym_1 ("len", 0, 1, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_len, gfc_resolve_len,
	     stg, BT_CHARACTER, dc, REQUIRED);

  make_generic ("len", GFC_ISYM_LEN, GFC_STD_F77);

  add_sym_1 ("len_trim", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     NULL, gfc_simplify_len_trim, gfc_resolve_len_trim,
	     stg, BT_CHARACTER, dc, REQUIRED);

  make_alias ("lnblnk", GFC_STD_GNU);

  make_generic ("len_trim", GFC_ISYM_LEN_TRIM, GFC_STD_F95);

  add_sym_2 ("lge", 1, 0, BT_LOGICAL, dl, GFC_STD_F77,
	     NULL, gfc_simplify_lge, NULL,
	     sta, BT_CHARACTER, dc, REQUIRED, stb, BT_CHARACTER, dc, REQUIRED);

  make_generic ("lge", GFC_ISYM_LGE, GFC_STD_F77);

  add_sym_2 ("lgt", 1, 0, BT_LOGICAL, dl, GFC_STD_F77,
	     NULL, gfc_simplify_lgt, NULL,
	     sta, BT_CHARACTER, dc, REQUIRED, stb, BT_CHARACTER, dc, REQUIRED);

  make_generic ("lgt", GFC_ISYM_LGT, GFC_STD_F77);

  add_sym_2 ("lle", 1, 0, BT_LOGICAL, dl, GFC_STD_F77,
	     NULL, gfc_simplify_lle, NULL,
	     sta, BT_CHARACTER, dc, REQUIRED, stb, BT_CHARACTER, dc, REQUIRED);

  make_generic ("lle", GFC_ISYM_LLE, GFC_STD_F77);

  add_sym_2 ("llt", 1, 0, BT_LOGICAL, dl, GFC_STD_F77,
	     NULL, gfc_simplify_llt, NULL,
	     sta, BT_CHARACTER, dc, REQUIRED, stb, BT_CHARACTER, dc, REQUIRED);

  make_generic ("llt", GFC_ISYM_LLT, GFC_STD_F77);

  add_sym_2 ("link", 0, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_link, NULL, gfc_resolve_link,
	     a, BT_CHARACTER, dc, REQUIRED, b, BT_CHARACTER, dc, REQUIRED);

  make_generic ("link", GFC_ISYM_LINK, GFC_STD_GNU);
  
  add_sym_1 ("log", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_log, gfc_resolve_log,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("alog", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     NULL, gfc_simplify_log, gfc_resolve_log,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dlog", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_log, gfc_resolve_log,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("clog", 1, 1, BT_COMPLEX, dz, GFC_STD_F77,
	     NULL, gfc_simplify_log, gfc_resolve_log,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zlog", 1, 1, BT_COMPLEX, dd,  GFC_STD_GNU,
	     NULL, gfc_simplify_log, gfc_resolve_log,
	     x, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdlog", GFC_STD_GNU);

  make_generic ("log", GFC_ISYM_LOG, GFC_STD_F77);

  add_sym_1 ("log10", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_r, gfc_simplify_log10, gfc_resolve_log10,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("alog10", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     NULL, gfc_simplify_log10, gfc_resolve_log10,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dlog10", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_log10, gfc_resolve_log10,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("log10", GFC_ISYM_LOG10, GFC_STD_F77);

  add_sym_2 ("logical", 0, 1, BT_LOGICAL, dl, GFC_STD_F95,
	     gfc_check_logical, gfc_simplify_logical, gfc_resolve_logical,
	     l, BT_LOGICAL, dl, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("logical", GFC_ISYM_LOGICAL, GFC_STD_F95);

  add_sym_1 ("malloc", 0, 1, BT_INTEGER, ii, GFC_STD_GNU, gfc_check_malloc,
	     NULL, gfc_resolve_malloc, a, BT_INTEGER, di, REQUIRED);

  make_generic ("malloc", GFC_ISYM_MALLOC, GFC_STD_GNU);

  add_sym_2 ("matmul", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_matmul, NULL, gfc_resolve_matmul,
	     ma, BT_REAL, dr, REQUIRED, mb, BT_REAL, dr, REQUIRED);

  make_generic ("matmul", GFC_ISYM_MATMUL, GFC_STD_F95);

  /* Note: amax0 is equivalent to real(max), max1 is equivalent to
     int(max).  The max function must take at least two arguments.  */

  add_sym_1m ("max", 1, 0, BT_UNKNOWN, 0, GFC_STD_F77,
	     gfc_check_min_max, gfc_simplify_max, gfc_resolve_max,
	     a1, BT_UNKNOWN, dr, REQUIRED, a2, BT_UNKNOWN, dr, REQUIRED);

  add_sym_1m ("max0", 1, 0, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_min_max_integer, gfc_simplify_max, NULL,
	     a1, BT_INTEGER, di, REQUIRED, a2, BT_INTEGER, di, REQUIRED);

  add_sym_1m ("amax0", 1, 0, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_min_max_integer, gfc_simplify_max, NULL,
	     a1, BT_INTEGER, di, REQUIRED, a2, BT_INTEGER, di, REQUIRED);

  add_sym_1m ("amax1", 1, 0, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_min_max_real, gfc_simplify_max, NULL,
	     a1, BT_REAL, dr, REQUIRED, a2, BT_REAL, dr, REQUIRED);

  add_sym_1m ("max1", 1, 0, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_min_max_real, gfc_simplify_max, NULL,
	     a1, BT_REAL, dr, REQUIRED, a2, BT_REAL, dr, REQUIRED);

  add_sym_1m ("dmax1", 1, 0, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_min_max_double, gfc_simplify_max, NULL,
	     a1, BT_REAL, dd, REQUIRED, a2, BT_REAL, dd, REQUIRED);

  make_generic ("max", GFC_ISYM_MAX, GFC_STD_F77);

  add_sym_1 ("maxexponent", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_x, gfc_simplify_maxexponent, NULL,
	     x, BT_UNKNOWN, dr, REQUIRED);

  make_generic ("maxexponent", GFC_ISYM_NONE, GFC_STD_F95);

  add_sym_3ml ("maxloc", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	       gfc_check_minloc_maxloc, NULL, gfc_resolve_maxloc,
	       ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
	       msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("maxloc", GFC_ISYM_MAXLOC, GFC_STD_F95);

  add_sym_3red ("maxval", 0, 1, BT_REAL, dr, GFC_STD_F95,
                gfc_check_minval_maxval, NULL, gfc_resolve_maxval,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("maxval", GFC_ISYM_MAXVAL, GFC_STD_F95);

  add_sym_3 ("merge", 1, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_merge, NULL, gfc_resolve_merge,
	     ts, BT_REAL, dr, REQUIRED, fs, BT_REAL, dr, REQUIRED,
	     msk, BT_LOGICAL, dl, REQUIRED);

  make_generic ("merge", GFC_ISYM_MERGE, GFC_STD_F95);

  /* Note: amin0 is equivalent to real(min), min1 is equivalent to
     int(min).  */

  add_sym_1m ("min", 1, 0, BT_UNKNOWN, 0, GFC_STD_F77,
	      gfc_check_min_max, gfc_simplify_min, gfc_resolve_min,
	     a1, BT_REAL, dr, REQUIRED, a2, BT_REAL, dr, REQUIRED);

  add_sym_1m ("min0", 1, 0, BT_INTEGER, di, GFC_STD_F77,
	      gfc_check_min_max_integer, gfc_simplify_min, NULL,
	     a1, BT_INTEGER, di, REQUIRED, a2, BT_INTEGER, di, REQUIRED);

  add_sym_1m ("amin0", 1, 0, BT_REAL, dr, GFC_STD_F77,
	      gfc_check_min_max_integer, gfc_simplify_min, NULL,
	     a1, BT_INTEGER, di, REQUIRED, a2, BT_INTEGER, di, REQUIRED);

  add_sym_1m ("amin1", 1, 0, BT_REAL, dr, GFC_STD_F77,
	      gfc_check_min_max_real, gfc_simplify_min, NULL,
	     a1, BT_REAL, dr, REQUIRED, a2, BT_REAL, dr, REQUIRED);

  add_sym_1m ("min1", 1, 0, BT_INTEGER, di, GFC_STD_F77,
	      gfc_check_min_max_real, gfc_simplify_min, NULL,
	     a1, BT_REAL, dr, REQUIRED, a2, BT_REAL, dr, REQUIRED);

  add_sym_1m ("dmin1", 1, 0, BT_REAL, dd, GFC_STD_F77,
	      gfc_check_min_max_double, gfc_simplify_min, NULL,
	     a1, BT_REAL, dd, REQUIRED, a2, BT_REAL, dd, REQUIRED);

  make_generic ("min", GFC_ISYM_MIN, GFC_STD_F77);

  add_sym_1 ("minexponent", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_x, gfc_simplify_minexponent, NULL,
	     x, BT_UNKNOWN, dr, REQUIRED);

  make_generic ("minexponent", GFC_ISYM_NONE, GFC_STD_F95);

  add_sym_3ml ("minloc", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	       gfc_check_minloc_maxloc, NULL, gfc_resolve_minloc,
	       ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
	       msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("minloc", GFC_ISYM_MINLOC, GFC_STD_F95);

  add_sym_3red ("minval", 0, 1, BT_REAL, dr, GFC_STD_F95,
                gfc_check_minval_maxval, NULL, gfc_resolve_minval,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("minval", GFC_ISYM_MINVAL, GFC_STD_F95);

  add_sym_2 ("mod", 1, 1, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_a_p, gfc_simplify_mod, gfc_resolve_mod,
	     a, BT_INTEGER, di, REQUIRED, p, BT_INTEGER, di, REQUIRED);

  add_sym_2 ("amod", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     NULL, gfc_simplify_mod, gfc_resolve_mod,
	     a, BT_REAL, dr, REQUIRED, p, BT_REAL, dr, REQUIRED);

  add_sym_2 ("dmod", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_mod, gfc_resolve_mod,
	     a, BT_REAL, dd, REQUIRED, p, BT_REAL, dd, REQUIRED);

  make_generic ("mod", GFC_ISYM_MOD, GFC_STD_F77);

  add_sym_2 ("modulo", 1, 1, BT_REAL, di, GFC_STD_F95,
	     gfc_check_a_p, gfc_simplify_modulo, gfc_resolve_modulo,
	     a, BT_REAL, di, REQUIRED, p, BT_REAL, di, REQUIRED);

  make_generic ("modulo", GFC_ISYM_MODULO, GFC_STD_F95);

  add_sym_2 ("nearest", 1, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_nearest, gfc_simplify_nearest, gfc_resolve_nearest,
	     x, BT_REAL, dr, REQUIRED, s, BT_REAL, dr, REQUIRED);

  make_generic ("nearest", GFC_ISYM_NEAREST, GFC_STD_F95);

  add_sym_2 ("nint", 1, 1, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_a_ikind, gfc_simplify_nint, gfc_resolve_nint,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  add_sym_1 ("idnint", 1, 1, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_idnint, gfc_simplify_idnint, gfc_resolve_idnint,
	     a, BT_REAL, dd, REQUIRED);

  make_generic ("nint", GFC_ISYM_NINT, GFC_STD_F77);

  add_sym_1 ("not", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_i, gfc_simplify_not, gfc_resolve_not,
	     i, BT_INTEGER, di, REQUIRED);

  make_generic ("not", GFC_ISYM_NOT, GFC_STD_F95);

  add_sym_1 ("null", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_null, gfc_simplify_null, NULL,
	     mo, BT_INTEGER, di, OPTIONAL);

  make_generic ("null", GFC_ISYM_NONE, GFC_STD_F95);

  add_sym_3 ("pack", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_pack, NULL, gfc_resolve_pack,
	     ar, BT_REAL, dr, REQUIRED, msk, BT_LOGICAL, dl, REQUIRED,
	     v, BT_REAL, dr, OPTIONAL);

  make_generic ("pack", GFC_ISYM_PACK, GFC_STD_F95);

  add_sym_1 ("precision", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_precision, gfc_simplify_precision, NULL,
	     x, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("precision", GFC_ISYM_NONE, GFC_STD_F95);

  add_sym_1 ("present", 0, 1, BT_LOGICAL, dl, GFC_STD_F95,
	     gfc_check_present, NULL, NULL,
	     a, BT_REAL, dr, REQUIRED);

  make_generic ("present", GFC_ISYM_PRESENT, GFC_STD_F95);

  add_sym_3red ("product", 0, 1, BT_REAL, dr, GFC_STD_F95,
                gfc_check_product_sum, NULL, gfc_resolve_product,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("product", GFC_ISYM_PRODUCT, GFC_STD_F95);

  add_sym_1 ("radix", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_radix, gfc_simplify_radix, NULL,
	     x, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("radix", GFC_ISYM_NONE, GFC_STD_F95);

  /* The following function is for G77 compatibility.  */
  add_sym_1 ("rand", 0, 1, BT_REAL, 4, GFC_STD_GNU,
             gfc_check_rand, NULL, NULL,
             i, BT_INTEGER, 4, OPTIONAL);

  /* Compatibility with HP FORTRAN 77/iX Reference.  Note, rand() and ran()
     use slightly different shoddy multiplicative congruential PRNG.  */
  make_alias ("ran", GFC_STD_GNU);

  make_generic ("rand", GFC_ISYM_RAND, GFC_STD_GNU);

  add_sym_1 ("range", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_range, gfc_simplify_range, NULL,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("range", GFC_ISYM_NONE, GFC_STD_F95);

  add_sym_2 ("real", 1, 0, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_real, gfc_simplify_real, gfc_resolve_real,
	     a, BT_UNKNOWN, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  /* This provides compatibility with g77.  */
  add_sym_1 ("realpart", 1, 0, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_c, gfc_simplify_realpart, gfc_resolve_realpart,
	     a, BT_UNKNOWN, dr, REQUIRED);

  add_sym_1 ("float", 1, 0, BT_REAL, dr, GFC_STD_F77,
	     NULL, gfc_simplify_float, NULL,
	     a, BT_INTEGER, di, REQUIRED);

  add_sym_1 ("sngl", 1, 0, BT_REAL, dr, GFC_STD_F77,
	     NULL, gfc_simplify_sngl, NULL,
	     a, BT_REAL, dd, REQUIRED);

  make_generic ("real", GFC_ISYM_REAL, GFC_STD_F77);

  add_sym_2 ("rename", 0, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_rename, NULL, gfc_resolve_rename,
	     a, BT_CHARACTER, dc, REQUIRED, b, BT_CHARACTER, dc, REQUIRED);

  make_generic ("rename", GFC_ISYM_RENAME, GFC_STD_GNU);
  
  add_sym_2 ("repeat", 0, 1, BT_CHARACTER, dc, GFC_STD_F95,
	     gfc_check_repeat, gfc_simplify_repeat, gfc_resolve_repeat,
	     stg, BT_CHARACTER, dc, REQUIRED, n, BT_INTEGER, di, REQUIRED);

  make_generic ("repeat", GFC_ISYM_REPEAT, GFC_STD_F95);

  add_sym_4 ("reshape", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_reshape, gfc_simplify_reshape, gfc_resolve_reshape,
	     src, BT_REAL, dr, REQUIRED, shp, BT_INTEGER, ii, REQUIRED,
	     pad, BT_REAL, dr, OPTIONAL, ord, BT_INTEGER, ii, OPTIONAL);

  make_generic ("reshape", GFC_ISYM_RESHAPE, GFC_STD_F95);

  add_sym_1 ("rrspacing", 1, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_x, gfc_simplify_rrspacing, gfc_resolve_rrspacing,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("rrspacing", GFC_ISYM_RRSPACING, GFC_STD_F95);

  add_sym_2 ("scale", 1, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_scale, gfc_simplify_scale, gfc_resolve_scale,
	     x, BT_REAL, dr, REQUIRED, i, BT_INTEGER, di, REQUIRED);

  make_generic ("scale", GFC_ISYM_SCALE, GFC_STD_F95);

  add_sym_3 ("scan", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_scan, gfc_simplify_scan, gfc_resolve_scan,
	     stg, BT_CHARACTER, dc, REQUIRED, set, BT_CHARACTER, dc, REQUIRED,
	     bck, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("scan", GFC_ISYM_SCAN, GFC_STD_F95);

  /* Added for G77 compatibility garbage.  */
  add_sym_0 ("second", 0, 1, BT_REAL, 4, GFC_STD_GNU,
	     NULL, NULL, NULL);

  make_generic ("second", GFC_ISYM_SECOND, GFC_STD_GNU);

  /* Added for G77 compatibility.  */
  add_sym_1 ("secnds", 0, 1, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_secnds, NULL, gfc_resolve_secnds,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("secnds", GFC_ISYM_SECNDS, GFC_STD_GNU);

  add_sym_1 ("selected_int_kind", 0, 1, BT_INTEGER, di,  GFC_STD_F95,
	     gfc_check_selected_int_kind, gfc_simplify_selected_int_kind, NULL,
	     r, BT_INTEGER, di, REQUIRED);

  make_generic ("selected_int_kind", GFC_ISYM_SI_KIND, GFC_STD_F95);

  add_sym_2 ("selected_real_kind", 0, 1, BT_INTEGER, di,  GFC_STD_F95,
	     gfc_check_selected_real_kind, gfc_simplify_selected_real_kind,
	     NULL,
	     p, BT_INTEGER, di, OPTIONAL, r, BT_INTEGER, di, OPTIONAL);

  make_generic ("selected_real_kind", GFC_ISYM_SR_KIND, GFC_STD_F95);

  add_sym_2 ("set_exponent", 1, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_set_exponent, gfc_simplify_set_exponent,
	     gfc_resolve_set_exponent,
	     x, BT_REAL, dr, REQUIRED, i, BT_INTEGER, di, REQUIRED);

  make_generic ("set_exponent", GFC_ISYM_SET_EXPONENT, GFC_STD_F95);

  add_sym_1 ("shape", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_shape, gfc_simplify_shape, gfc_resolve_shape,
	     src, BT_REAL, dr, REQUIRED);

  make_generic ("shape", GFC_ISYM_SHAPE, GFC_STD_F95);

  add_sym_2 ("sign", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_sign, gfc_simplify_sign, gfc_resolve_sign,
	     a, BT_REAL, dr, REQUIRED, b, BT_REAL, dr, REQUIRED);

  add_sym_2 ("isign", 1, 1, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_sign, gfc_resolve_sign,
	     a, BT_INTEGER, di, REQUIRED, b, BT_INTEGER, di, REQUIRED);

  add_sym_2 ("dsign", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_sign, gfc_resolve_sign,
	     a, BT_REAL, dd, REQUIRED, b, BT_REAL, dd, REQUIRED);

  make_generic ("sign", GFC_ISYM_SIGN, GFC_STD_F77);

  add_sym_2 ("signal", 1, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_signal, NULL, gfc_resolve_signal,
	     num, BT_INTEGER, di, REQUIRED, han, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("signal", GFC_ISYM_SIGNAL, GFC_STD_GNU);

  add_sym_1 ("sin", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_sin, gfc_resolve_sin,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dsin", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_sin, gfc_resolve_sin,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("csin", 1, 1, BT_COMPLEX, dz, GFC_STD_F77,
	     NULL, gfc_simplify_sin, gfc_resolve_sin,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zsin", 1, 1, BT_COMPLEX, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_sin, gfc_resolve_sin,
	     x, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdsin", GFC_STD_GNU);

  make_generic ("sin", GFC_ISYM_SIN, GFC_STD_F77);

  add_sym_1 ("sinh", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_r, gfc_simplify_sinh, gfc_resolve_sinh,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dsinh", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_sinh, gfc_resolve_sinh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("sinh", GFC_ISYM_SINH, GFC_STD_F77);

  add_sym_2 ("size", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_size, gfc_simplify_size, NULL,
	     ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("size", GFC_ISYM_SIZE, GFC_STD_F95);

  add_sym_1 ("spacing", 1, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_x, gfc_simplify_spacing, gfc_resolve_spacing,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("spacing", GFC_ISYM_SPACING, GFC_STD_F95);

  add_sym_3 ("spread", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_spread, NULL, gfc_resolve_spread,
	     src, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, REQUIRED,
	     n, BT_INTEGER, di, REQUIRED);

  make_generic ("spread", GFC_ISYM_SPREAD, GFC_STD_F95);

  add_sym_1 ("sqrt", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_sqrt, gfc_resolve_sqrt,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dsqrt", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_sqrt, gfc_resolve_sqrt,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("csqrt", 1, 1, BT_COMPLEX, dz, GFC_STD_F77,
	     NULL, gfc_simplify_sqrt, gfc_resolve_sqrt,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zsqrt", 1, 1, BT_COMPLEX, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_sqrt, gfc_resolve_sqrt,
	     x, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdsqrt", GFC_STD_GNU);

  make_generic ("sqrt", GFC_ISYM_SQRT, GFC_STD_F77);

  add_sym_2 ("stat", 0, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_stat, NULL, gfc_resolve_stat,
	     a, BT_CHARACTER, dc, REQUIRED, b, BT_INTEGER, di, REQUIRED);

  make_generic ("stat", GFC_ISYM_STAT, GFC_STD_GNU);

  add_sym_3red ("sum", 0, 1, BT_UNKNOWN, 0, GFC_STD_F95,
                gfc_check_product_sum, NULL, gfc_resolve_sum,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("sum", GFC_ISYM_SUM, GFC_STD_F95);

  add_sym_2 ("symlnk", 0, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_symlnk, NULL, gfc_resolve_symlnk,
	     a, BT_CHARACTER, dc, REQUIRED, b, BT_CHARACTER, dc, REQUIRED);

  make_generic ("symlnk", GFC_ISYM_SYMLNK, GFC_STD_GNU);

  add_sym_1 ("system", 1, 1, BT_INTEGER, di, GFC_STD_GNU,
	     NULL, NULL, NULL,
	     c, BT_CHARACTER, dc, REQUIRED);

  make_generic ("system", GFC_ISYM_SYSTEM, GFC_STD_GNU);

  add_sym_1 ("tan", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_r, gfc_simplify_tan, gfc_resolve_tan,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dtan", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_tan, gfc_resolve_tan,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("tan", GFC_ISYM_TAN, GFC_STD_F77);

  add_sym_1 ("tanh", 1, 1, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_r, gfc_simplify_tanh, gfc_resolve_tanh,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dtanh", 1, 1, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_tanh, gfc_resolve_tanh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("tanh", GFC_ISYM_TANH, GFC_STD_F77);

  add_sym_0 ("time", 1, 0, BT_INTEGER, di, GFC_STD_GNU, 
	     NULL, NULL, gfc_resolve_time);

  make_generic ("time", GFC_ISYM_TIME, GFC_STD_GNU);

  add_sym_0 ("time8", 1, 0, BT_INTEGER, di, GFC_STD_GNU, 
	     NULL, NULL, gfc_resolve_time8);

  make_generic ("time8", GFC_ISYM_TIME8, GFC_STD_GNU);

  add_sym_1 ("tiny", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_x, gfc_simplify_tiny, NULL,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("tiny", GFC_ISYM_NONE, GFC_STD_F95);

  add_sym_3 ("transfer", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_transfer, NULL, gfc_resolve_transfer,
	     src, BT_REAL, dr, REQUIRED, mo, BT_REAL, dr, REQUIRED,
	     sz, BT_INTEGER, di, OPTIONAL);

  make_generic ("transfer", GFC_ISYM_TRANSFER, GFC_STD_F95);

  add_sym_1 ("transpose", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_transpose, NULL, gfc_resolve_transpose,
	     m, BT_REAL, dr, REQUIRED);

  make_generic ("transpose", GFC_ISYM_TRANSPOSE, GFC_STD_F95);

  add_sym_1 ("trim", 0, 1, BT_CHARACTER, dc, GFC_STD_F95,
	     gfc_check_trim, gfc_simplify_trim, gfc_resolve_trim,
	     stg, BT_CHARACTER, dc, REQUIRED);

  make_generic ("trim", GFC_ISYM_TRIM, GFC_STD_F95);

  add_sym_1 ("ttynam", 0, 1, BT_CHARACTER, 0, GFC_STD_GNU,
              gfc_check_ttynam, NULL, gfc_resolve_ttynam,
	      ut, BT_INTEGER, di, REQUIRED);

  make_generic ("ttynam", GFC_ISYM_TTYNAM, GFC_STD_GNU);

  add_sym_2 ("ubound", 0, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ubound, gfc_simplify_ubound, gfc_resolve_ubound,
	     ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("ubound", GFC_ISYM_UBOUND, GFC_STD_F95);

  /* g77 compatibility for UMASK.  */
  add_sym_1 ("umask", 0, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_umask, NULL, gfc_resolve_umask,
	     a, BT_INTEGER, di, REQUIRED);

  make_generic ("umask", GFC_ISYM_UMASK, GFC_STD_GNU);

  /* g77 compatibility for UNLINK.  */
  add_sym_1 ("unlink", 0, 1, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_unlink, NULL, gfc_resolve_unlink,
	     a, BT_CHARACTER, dc, REQUIRED);

  make_generic ("unlink", GFC_ISYM_UNLINK, GFC_STD_GNU);

  add_sym_3 ("unpack", 0, 1, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_unpack, NULL, gfc_resolve_unpack,
	     v, BT_REAL, dr, REQUIRED, msk, BT_LOGICAL, dl, REQUIRED,
	     f, BT_REAL, dr, REQUIRED);

  make_generic ("unpack", GFC_ISYM_UNPACK, GFC_STD_F95);

  add_sym_3 ("verify", 1, 1, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_verify, gfc_simplify_verify, gfc_resolve_verify,
	     stg, BT_CHARACTER, dc, REQUIRED, set, BT_CHARACTER, dc, REQUIRED,
	     bck, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("verify", GFC_ISYM_VERIFY, GFC_STD_F95);
    
  add_sym_1 ("loc", 0, 1, BT_INTEGER, ii, GFC_STD_GNU,
	    gfc_check_loc, NULL, gfc_resolve_loc,
	    ar, BT_UNKNOWN, 0, REQUIRED);
		
  make_generic ("loc", GFC_ISYM_LOC, GFC_STD_GNU);

}


/* Add intrinsic subroutines.  */

static void
add_subroutines (void)
{
  /* Argument names as in the standard (to be used as argument keywords).  */
  const char
    *h = "harvest", *dt = "date", *vl = "values", *pt = "put",
    *c = "count", *tm = "time", *tp = "topos", *gt = "get",
    *t = "to", *zn = "zone", *fp = "frompos", *cm = "count_max",
    *f = "from", *sz = "size", *ln = "len", *cr = "count_rate",
    *com = "command", *length = "length", *st = "status",
    *val = "value", *num = "number", *name = "name",
    *trim_name = "trim_name", *ut = "unit", *han = "handler",
    *sec = "seconds", *res = "result";

  int di, dr, dc, dl, ii;

  di = gfc_default_integer_kind;
  dr = gfc_default_real_kind;
  dc = gfc_default_character_kind;
  dl = gfc_default_logical_kind;
  ii = gfc_index_integer_kind;

  add_sym_0s ("abort", 1, GFC_STD_GNU, NULL);

  make_noreturn();

  add_sym_1s ("cpu_time", 0, 1, BT_UNKNOWN, 0, GFC_STD_F95,
	      gfc_check_cpu_time, NULL, gfc_resolve_cpu_time,
	      tm, BT_REAL, dr, REQUIRED);

  /* More G77 compatibility garbage.  */
  add_sym_2s ("ctime", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	     gfc_check_ctime_sub, NULL, gfc_resolve_ctime_sub,
	     tm, BT_INTEGER, di, REQUIRED, res, BT_CHARACTER, dc, REQUIRED);

  add_sym_1s ("second", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_second_sub, NULL, gfc_resolve_second_sub,
	      tm, BT_REAL, dr, REQUIRED);

  add_sym_2s ("chdir", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
              gfc_check_chdir_sub, NULL, gfc_resolve_chdir_sub,
	      name, BT_CHARACTER, dc, REQUIRED, st, BT_INTEGER, di, OPTIONAL);

  add_sym_4s ("date_and_time", 0, 1, BT_UNKNOWN, 0, GFC_STD_F95,
	      gfc_check_date_and_time, NULL, NULL,
	      dt, BT_CHARACTER, dc, OPTIONAL, tm, BT_CHARACTER, dc, OPTIONAL,
	      zn, BT_CHARACTER, dc, OPTIONAL, vl, BT_INTEGER, di, OPTIONAL);

  /* More G77 compatibility garbage.  */
  add_sym_2s ("etime", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	     gfc_check_etime_sub, NULL, gfc_resolve_etime_sub,
	      vl, BT_REAL, 4, REQUIRED, tm, BT_REAL, 4, REQUIRED);

  add_sym_2s ("dtime", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	     gfc_check_etime_sub, NULL, gfc_resolve_etime_sub,
	      vl, BT_REAL, 4, REQUIRED, tm, BT_REAL, 4, REQUIRED);

  add_sym_1s ("fdate", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	     gfc_check_fdate_sub, NULL, gfc_resolve_fdate_sub,
	     dt, BT_CHARACTER, dc, REQUIRED);

  add_sym_1s ("gerror", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
              gfc_check_gerror, NULL, gfc_resolve_gerror, c, BT_CHARACTER,
	      dc, REQUIRED);

  add_sym_2s ("getcwd", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
          gfc_check_getcwd_sub, NULL, gfc_resolve_getcwd_sub,
	      c, BT_CHARACTER, dc, REQUIRED, st, BT_INTEGER, di, OPTIONAL);

  add_sym_2s ("getenv", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	      NULL, NULL, NULL,
	      name, BT_CHARACTER, dc, REQUIRED, val, BT_CHARACTER, dc, REQUIRED);

  add_sym_2s ("getarg", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	      NULL, NULL, gfc_resolve_getarg,
	      c, BT_INTEGER, di, REQUIRED, vl, BT_CHARACTER, dc, REQUIRED);

  add_sym_1s ("getlog", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
              gfc_check_getlog, NULL, gfc_resolve_getlog, c, BT_CHARACTER,
	      dc, REQUIRED);

  /* F2003 commandline routines.  */

  add_sym_3s ("get_command", 0, 1, BT_UNKNOWN, 0, GFC_STD_F2003,
	      NULL, NULL, gfc_resolve_get_command,
	      com, BT_CHARACTER, dc, OPTIONAL, length, BT_INTEGER, di, OPTIONAL,
	      st, BT_INTEGER, di, OPTIONAL);

  add_sym_4s ("get_command_argument", 0, 1, BT_UNKNOWN, 0, GFC_STD_F2003,
	      NULL, NULL, gfc_resolve_get_command_argument,
	      num, BT_INTEGER, di, REQUIRED, val, BT_CHARACTER, dc, OPTIONAL,
	      length, BT_INTEGER, di, OPTIONAL, st, BT_INTEGER, di, OPTIONAL);

  /* F2003 subroutine to get environment variables.  */

  add_sym_5s ("get_environment_variable", 0, 1, BT_UNKNOWN, 0, GFC_STD_F2003,
	     NULL, NULL, gfc_resolve_get_environment_variable,
	      name, BT_CHARACTER, dc, REQUIRED, val, BT_CHARACTER, dc, OPTIONAL,
	      length, BT_INTEGER, di, OPTIONAL, st, BT_INTEGER, di, OPTIONAL,
	      trim_name, BT_LOGICAL, dl, OPTIONAL);

  add_sym_5s ("mvbits", 1, 1, BT_UNKNOWN, 0, GFC_STD_F95,
	      gfc_check_mvbits, gfc_simplify_mvbits, gfc_resolve_mvbits,
	      f, BT_INTEGER, di, REQUIRED, fp, BT_INTEGER, di, REQUIRED,
	      ln, BT_INTEGER, di, REQUIRED, t, BT_INTEGER, di, REQUIRED,
	      tp, BT_INTEGER, di, REQUIRED);

  add_sym_1s ("random_number", 0, 1, BT_UNKNOWN, 0, GFC_STD_F95,
	      gfc_check_random_number, NULL, gfc_resolve_random_number,
	      h, BT_REAL, dr, REQUIRED);

  add_sym_3s ("random_seed", 0, 1, BT_UNKNOWN, 0, GFC_STD_F95,
	     gfc_check_random_seed, NULL, NULL,
	      sz, BT_INTEGER, di, OPTIONAL, pt, BT_INTEGER, di, OPTIONAL,
	      gt, BT_INTEGER, di, OPTIONAL);

  /* More G77 compatibility garbage.  */
  add_sym_3s ("alarm", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_alarm_sub, NULL, gfc_resolve_alarm_sub,
	      sec, BT_INTEGER, di, REQUIRED, han, BT_UNKNOWN, 0, REQUIRED,
	      st, BT_INTEGER, di, OPTIONAL);

  add_sym_1s ("srand", 0, 1, BT_UNKNOWN, di, GFC_STD_GNU,
             gfc_check_srand, NULL, gfc_resolve_srand,
	      c, BT_INTEGER, 4, REQUIRED);

  add_sym_1s ("exit", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
             gfc_check_exit, NULL, gfc_resolve_exit,
	      c, BT_INTEGER, di, OPTIONAL);

  make_noreturn();

  add_sym_1s ("flush", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_flush, NULL, gfc_resolve_flush,
	      c, BT_INTEGER, di, OPTIONAL);

  add_sym_1s ("free", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU, gfc_check_free,
	      NULL, gfc_resolve_free, c, BT_INTEGER, ii, REQUIRED);

  add_sym_2s ("hostnm", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
          gfc_check_hostnm_sub, NULL, gfc_resolve_hostnm_sub,
	      c, BT_CHARACTER, dc, REQUIRED, st, BT_INTEGER, di, OPTIONAL);

  add_sym_3s ("kill", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU, gfc_check_kill_sub,
	      NULL, gfc_resolve_kill_sub, c, BT_INTEGER, di, REQUIRED,
	      val, BT_INTEGER, di, REQUIRED, st, BT_INTEGER, di, OPTIONAL);

  add_sym_3s ("link", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
              gfc_check_link_sub, NULL, gfc_resolve_link_sub,
	      name, BT_CHARACTER, dc, REQUIRED, val, BT_CHARACTER,
	      dc, REQUIRED, st, BT_INTEGER, di, OPTIONAL);

  add_sym_1s ("perror", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
          gfc_check_perror, NULL, gfc_resolve_perror,
	      c, BT_CHARACTER, dc, REQUIRED);

  add_sym_3s ("rename", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
              gfc_check_rename_sub, NULL, gfc_resolve_rename_sub,
	      name, BT_CHARACTER, dc, REQUIRED, val, BT_CHARACTER,
	      dc, REQUIRED, st, BT_INTEGER, di, OPTIONAL);

  add_sym_1s ("sleep", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
              gfc_check_sleep_sub, NULL, gfc_resolve_sleep_sub,
	      val, BT_CHARACTER, dc, REQUIRED);

  add_sym_3s ("fstat", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_fstat_sub, NULL, gfc_resolve_fstat_sub,
	      ut, BT_INTEGER, di, REQUIRED, vl, BT_INTEGER, di, REQUIRED,
	      st, BT_INTEGER, di, OPTIONAL);

  add_sym_3s ("stat", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_stat_sub, NULL, gfc_resolve_stat_sub,
	      name, BT_CHARACTER, dc, REQUIRED, vl, BT_INTEGER, di, REQUIRED,
	      st, BT_INTEGER, di, OPTIONAL);

  add_sym_3s ("signal", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_signal_sub, NULL, gfc_resolve_signal_sub,
	      num, BT_INTEGER, di, REQUIRED, han, BT_UNKNOWN, 0, REQUIRED,
	      st, BT_INTEGER, di, OPTIONAL);

  add_sym_3s ("symlnk", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
              gfc_check_symlnk_sub, NULL, gfc_resolve_symlnk_sub,
	      name, BT_CHARACTER, dc, REQUIRED, val, BT_CHARACTER,
	      dc, REQUIRED, st, BT_INTEGER, di, OPTIONAL);

  add_sym_2s ("system", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
	      NULL, NULL, gfc_resolve_system_sub,
	      c, BT_CHARACTER, dc, REQUIRED, st, BT_INTEGER, di, OPTIONAL);

  add_sym_3s ("system_clock", 0, 1, BT_UNKNOWN, 0, GFC_STD_F95,
	     gfc_check_system_clock, NULL, gfc_resolve_system_clock,
	      c, BT_INTEGER, di, OPTIONAL, cr, BT_INTEGER, di, OPTIONAL,
	      cm, BT_INTEGER, di, OPTIONAL);

  add_sym_2s ("ttynam", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
              gfc_check_ttynam_sub, NULL, gfc_resolve_ttynam_sub,
	      ut, BT_INTEGER, di, REQUIRED, c, BT_CHARACTER, dc, REQUIRED);

  add_sym_2s ("umask", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
          gfc_check_umask_sub, NULL, gfc_resolve_umask_sub,
	      val, BT_INTEGER, di, REQUIRED, num, BT_INTEGER, di, OPTIONAL);

  add_sym_2s ("unlink", 0, 1, BT_UNKNOWN, 0, GFC_STD_GNU,
          gfc_check_unlink_sub, NULL, gfc_resolve_unlink_sub,
	      c, BT_CHARACTER, dc, REQUIRED, st, BT_INTEGER, di, OPTIONAL);

}


/* Add a function to the list of conversion symbols.  */

static void
add_conv (bt from_type, int from_kind, bt to_type, int to_kind, int standard)
{

  gfc_typespec from, to;
  gfc_intrinsic_sym *sym;

  if (sizing == SZ_CONVS)
    {
      nconv++;
      return;
    }

  gfc_clear_ts (&from);
  from.type = from_type;
  from.kind = from_kind;

  gfc_clear_ts (&to);
  to.type = to_type;
  to.kind = to_kind;

  sym = conversion + nconv;

  sym->name = conv_name (&from, &to);
  sym->lib_name = sym->name;
  sym->simplify.cc = gfc_convert_constant;
  sym->standard = standard;
  sym->elemental = 1;
  sym->ts = to;
  sym->generic_id = GFC_ISYM_CONVERSION;

  nconv++;
}


/* Create gfc_intrinsic_sym nodes for all intrinsic conversion
   functions by looping over the kind tables.  */

static void
add_conversions (void)
{
  int i, j;

  /* Integer-Integer conversions.  */
  for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
    for (j = 0; gfc_integer_kinds[j].kind != 0; j++)
      {
	if (i == j)
	  continue;

	add_conv (BT_INTEGER, gfc_integer_kinds[i].kind,
		  BT_INTEGER, gfc_integer_kinds[j].kind, GFC_STD_F77);
      }

  /* Integer-Real/Complex conversions.  */
  for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
    for (j = 0; gfc_real_kinds[j].kind != 0; j++)
      {
	add_conv (BT_INTEGER, gfc_integer_kinds[i].kind,
		  BT_REAL, gfc_real_kinds[j].kind, GFC_STD_F77);

	add_conv (BT_REAL, gfc_real_kinds[j].kind,
		  BT_INTEGER, gfc_integer_kinds[i].kind, GFC_STD_F77);

	add_conv (BT_INTEGER, gfc_integer_kinds[i].kind,
		  BT_COMPLEX, gfc_real_kinds[j].kind, GFC_STD_F77);

	add_conv (BT_COMPLEX, gfc_real_kinds[j].kind,
		  BT_INTEGER, gfc_integer_kinds[i].kind, GFC_STD_F77);
      }

  if ((gfc_option.allow_std & GFC_STD_LEGACY) != 0)
    {
      /* Hollerith-Integer conversions.  */
      for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
	add_conv (BT_HOLLERITH, gfc_default_character_kind,
		  BT_INTEGER, gfc_integer_kinds[i].kind, GFC_STD_LEGACY);
      /* Hollerith-Real conversions.  */
      for (i = 0; gfc_real_kinds[i].kind != 0; i++)
	add_conv (BT_HOLLERITH, gfc_default_character_kind,
		  BT_REAL, gfc_real_kinds[i].kind, GFC_STD_LEGACY);
      /* Hollerith-Complex conversions.  */
      for (i = 0; gfc_real_kinds[i].kind != 0; i++)
	add_conv (BT_HOLLERITH, gfc_default_character_kind,
		  BT_COMPLEX, gfc_real_kinds[i].kind, GFC_STD_LEGACY);

      /* Hollerith-Character conversions.  */
      add_conv (BT_HOLLERITH, gfc_default_character_kind, BT_CHARACTER,
		  gfc_default_character_kind, GFC_STD_LEGACY);

      /* Hollerith-Logical conversions.  */
      for (i = 0; gfc_logical_kinds[i].kind != 0; i++)
	add_conv (BT_HOLLERITH, gfc_default_character_kind,
		  BT_LOGICAL, gfc_logical_kinds[i].kind, GFC_STD_LEGACY);
    }

  /* Real/Complex - Real/Complex conversions.  */
  for (i = 0; gfc_real_kinds[i].kind != 0; i++)
    for (j = 0; gfc_real_kinds[j].kind != 0; j++)
      {
	if (i != j)
	  {
	    add_conv (BT_REAL, gfc_real_kinds[i].kind,
		      BT_REAL, gfc_real_kinds[j].kind, GFC_STD_F77);

	    add_conv (BT_COMPLEX, gfc_real_kinds[i].kind,
		      BT_COMPLEX, gfc_real_kinds[j].kind, GFC_STD_F77);
	  }

	add_conv (BT_REAL, gfc_real_kinds[i].kind,
		  BT_COMPLEX, gfc_real_kinds[j].kind, GFC_STD_F77);

	add_conv (BT_COMPLEX, gfc_real_kinds[i].kind,
		  BT_REAL, gfc_real_kinds[j].kind, GFC_STD_F77);
      }

  /* Logical/Logical kind conversion.  */
  for (i = 0; gfc_logical_kinds[i].kind; i++)
    for (j = 0; gfc_logical_kinds[j].kind; j++)
      {
	if (i == j)
	  continue;

	add_conv (BT_LOGICAL, gfc_logical_kinds[i].kind,
		  BT_LOGICAL, gfc_logical_kinds[j].kind, GFC_STD_F77);
      }

  /* Integer-Logical and Logical-Integer conversions.  */
  if ((gfc_option.allow_std & GFC_STD_LEGACY) != 0)
    for (i=0; gfc_integer_kinds[i].kind; i++)
      for (j=0; gfc_logical_kinds[j].kind; j++)
	{
	  add_conv (BT_INTEGER, gfc_integer_kinds[i].kind,
		    BT_LOGICAL, gfc_logical_kinds[j].kind, GFC_STD_LEGACY);
	  add_conv (BT_LOGICAL, gfc_logical_kinds[j].kind,
		    BT_INTEGER, gfc_integer_kinds[i].kind, GFC_STD_LEGACY);
	}
}


/* Initialize the table of intrinsics.  */
void
gfc_intrinsic_init_1 (void)
{
  int i;

  nargs = nfunc = nsub = nconv = 0;

  /* Create a namespace to hold the resolved intrinsic symbols.  */
  gfc_intrinsic_namespace = gfc_get_namespace (NULL, 0);

  sizing = SZ_FUNCS;
  add_functions ();
  sizing = SZ_SUBS;
  add_subroutines ();
  sizing = SZ_CONVS;
  add_conversions ();

  functions = gfc_getmem (sizeof (gfc_intrinsic_sym) * (nfunc + nsub)
			  + sizeof (gfc_intrinsic_arg) * nargs);

  next_sym = functions;
  subroutines = functions + nfunc;

  conversion = gfc_getmem (sizeof (gfc_intrinsic_sym) * nconv);

  next_arg = ((gfc_intrinsic_arg *) (subroutines + nsub)) - 1;

  sizing = SZ_NOTHING;
  nconv = 0;

  add_functions ();
  add_subroutines ();
  add_conversions ();

  /* Set the pure flag.  All intrinsic functions are pure, and
     intrinsic subroutines are pure if they are elemental.  */

  for (i = 0; i < nfunc; i++)
    functions[i].pure = 1;

  for (i = 0; i < nsub; i++)
    subroutines[i].pure = subroutines[i].elemental;
}


void
gfc_intrinsic_done_1 (void)
{
  gfc_free (functions);
  gfc_free (conversion);
  gfc_free_namespace (gfc_intrinsic_namespace);
}


/******** Subroutines to check intrinsic interfaces ***********/

/* Given a formal argument list, remove any NULL arguments that may
   have been left behind by a sort against some formal argument list.  */

static void
remove_nullargs (gfc_actual_arglist ** ap)
{
  gfc_actual_arglist *head, *tail, *next;

  tail = NULL;

  for (head = *ap; head; head = next)
    {
      next = head->next;

      if (head->expr == NULL)
	{
	  head->next = NULL;
	  gfc_free_actual_arglist (head);
	}
      else
	{
	  if (tail == NULL)
	    *ap = head;
	  else
	    tail->next = head;

	  tail = head;
	  tail->next = NULL;
	}
    }

  if (tail == NULL)
    *ap = NULL;
}


/* Given an actual arglist and a formal arglist, sort the actual
   arglist so that its arguments are in a one-to-one correspondence
   with the format arglist.  Arguments that are not present are given
   a blank gfc_actual_arglist structure.  If something is obviously
   wrong (say, a missing required argument) we abort sorting and
   return FAILURE.  */

static try
sort_actual (const char *name, gfc_actual_arglist ** ap,
	     gfc_intrinsic_arg * formal, locus * where)
{

  gfc_actual_arglist *actual, *a;
  gfc_intrinsic_arg *f;

  remove_nullargs (ap);
  actual = *ap;

  for (f = formal; f; f = f->next)
    f->actual = NULL;

  f = formal;
  a = actual;

  if (f == NULL && a == NULL)	/* No arguments */
    return SUCCESS;

  for (;;)
    {				/* Put the nonkeyword arguments in a 1:1 correspondence */
      if (f == NULL)
	break;
      if (a == NULL)
	goto optional;

      if (a->name != NULL)
	goto keywords;

      f->actual = a;

      f = f->next;
      a = a->next;
    }

  if (a == NULL)
    goto do_sort;

  gfc_error ("Too many arguments in call to '%s' at %L", name, where);
  return FAILURE;

keywords:
  /* Associate the remaining actual arguments, all of which have
     to be keyword arguments.  */
  for (; a; a = a->next)
    {
      for (f = formal; f; f = f->next)
	if (strcmp (a->name, f->name) == 0)
	  break;

      if (f == NULL)
	{
	  gfc_error ("Can't find keyword named '%s' in call to '%s' at %L",
		     a->name, name, where);
	  return FAILURE;
	}

      if (f->actual != NULL)
	{
	  gfc_error ("Argument '%s' is appears twice in call to '%s' at %L",
		     f->name, name, where);
	  return FAILURE;
	}

      f->actual = a;
    }

optional:
  /* At this point, all unmatched formal args must be optional.  */
  for (f = formal; f; f = f->next)
    {
      if (f->actual == NULL && f->optional == 0)
	{
	  gfc_error ("Missing actual argument '%s' in call to '%s' at %L",
		     f->name, name, where);
	  return FAILURE;
	}
    }

do_sort:
  /* Using the formal argument list, string the actual argument list
     together in a way that corresponds with the formal list.  */
  actual = NULL;

  for (f = formal; f; f = f->next)
    {
      if (f->actual == NULL)
	{
	  a = gfc_get_actual_arglist ();
	  a->missing_arg_type = f->ts.type;
	}
      else
	a = f->actual;

      if (actual == NULL)
	*ap = a;
      else
	actual->next = a;

      actual = a;
    }
  actual->next = NULL;		/* End the sorted argument list.  */

  return SUCCESS;
}


/* Compare an actual argument list with an intrinsic's formal argument
   list.  The lists are checked for agreement of type.  We don't check
   for arrayness here.  */

static try
check_arglist (gfc_actual_arglist ** ap, gfc_intrinsic_sym * sym,
	       int error_flag)
{
  gfc_actual_arglist *actual;
  gfc_intrinsic_arg *formal;
  int i;

  formal = sym->formal;
  actual = *ap;

  i = 0;
  for (; formal; formal = formal->next, actual = actual->next, i++)
    {
      if (actual->expr == NULL)
	continue;

      if (!gfc_compare_types (&formal->ts, &actual->expr->ts))
	{
	  if (error_flag)
	    gfc_error
	      ("Type of argument '%s' in call to '%s' at %L should be "
	       "%s, not %s", gfc_current_intrinsic_arg[i],
	       gfc_current_intrinsic, &actual->expr->where,
	       gfc_typename (&formal->ts), gfc_typename (&actual->expr->ts));
	  return FAILURE;
	}
    }

  return SUCCESS;
}


/* Given a pointer to an intrinsic symbol and an expression node that
   represent the function call to that subroutine, figure out the type
   of the result.  This may involve calling a resolution subroutine.  */

static void
resolve_intrinsic (gfc_intrinsic_sym * specific, gfc_expr * e)
{
  gfc_expr *a1, *a2, *a3, *a4, *a5;
  gfc_actual_arglist *arg;

  if (specific->resolve.f1 == NULL)
    {
      if (e->value.function.name == NULL)
	e->value.function.name = specific->lib_name;

      if (e->ts.type == BT_UNKNOWN)
	e->ts = specific->ts;
      return;
    }

  arg = e->value.function.actual;

  /* Special case hacks for MIN and MAX.  */
  if (specific->resolve.f1m == gfc_resolve_max
      || specific->resolve.f1m == gfc_resolve_min)
    {
      (*specific->resolve.f1m) (e, arg);
      return;
    }

  if (arg == NULL)
    {
      (*specific->resolve.f0) (e);
      return;
    }

  a1 = arg->expr;
  arg = arg->next;

  if (arg == NULL)
    {
      (*specific->resolve.f1) (e, a1);
      return;
    }

  a2 = arg->expr;
  arg = arg->next;

  if (arg == NULL)
    {
      (*specific->resolve.f2) (e, a1, a2);
      return;
    }

  a3 = arg->expr;
  arg = arg->next;

  if (arg == NULL)
    {
      (*specific->resolve.f3) (e, a1, a2, a3);
      return;
    }

  a4 = arg->expr;
  arg = arg->next;

  if (arg == NULL)
    {
      (*specific->resolve.f4) (e, a1, a2, a3, a4);
      return;
    }

  a5 = arg->expr;
  arg = arg->next;

  if (arg == NULL)
    {
      (*specific->resolve.f5) (e, a1, a2, a3, a4, a5);
      return;
    }

  gfc_internal_error ("resolve_intrinsic(): Too many args for intrinsic");
}


/* Given an intrinsic symbol node and an expression node, call the
   simplification function (if there is one), perhaps replacing the
   expression with something simpler.  We return FAILURE on an error
   of the simplification, SUCCESS if the simplification worked, even
   if nothing has changed in the expression itself.  */

static try
do_simplify (gfc_intrinsic_sym * specific, gfc_expr * e)
{
  gfc_expr *result, *a1, *a2, *a3, *a4, *a5;
  gfc_actual_arglist *arg;

  /* Check the arguments if there are Hollerith constants. We deal with
     them at run-time.  */
  for (arg = e->value.function.actual; arg != NULL; arg = arg->next)
    {
      if (arg->expr && arg->expr->from_H)
	{
	  result = NULL;
	  goto finish;
	}
    }
  /* Max and min require special handling due to the variable number
     of args.  */
  if (specific->simplify.f1 == gfc_simplify_min)
    {
      result = gfc_simplify_min (e);
      goto finish;
    }

  if (specific->simplify.f1 == gfc_simplify_max)
    {
      result = gfc_simplify_max (e);
      goto finish;
    }

  if (specific->simplify.f1 == NULL)
    {
      result = NULL;
      goto finish;
    }

  arg = e->value.function.actual;

  if (arg == NULL)
    {
      result = (*specific->simplify.f0) ();
      goto finish;
    }

  a1 = arg->expr;
  arg = arg->next;

  if (specific->simplify.cc == gfc_convert_constant)
    {
      result = gfc_convert_constant (a1, specific->ts.type, specific->ts.kind);
      goto finish;
    }

  /* TODO: Warn if -pedantic and initialization expression and arg
     types not integer or character */

  if (arg == NULL)
    result = (*specific->simplify.f1) (a1);
  else
    {
      a2 = arg->expr;
      arg = arg->next;

      if (arg == NULL)
	result = (*specific->simplify.f2) (a1, a2);
      else
	{
	  a3 = arg->expr;
	  arg = arg->next;

	  if (arg == NULL)
	    result = (*specific->simplify.f3) (a1, a2, a3);
	  else
	    {
	      a4 = arg->expr;
	      arg = arg->next;

	      if (arg == NULL)
		result = (*specific->simplify.f4) (a1, a2, a3, a4);
	      else
		{
		  a5 = arg->expr;
		  arg = arg->next;

		  if (arg == NULL)
		    result = (*specific->simplify.f5) (a1, a2, a3, a4, a5);
		  else
		    gfc_internal_error
		      ("do_simplify(): Too many args for intrinsic");
		}
	    }
	}
    }

finish:
  if (result == &gfc_bad_expr)
    return FAILURE;

  if (result == NULL)
    resolve_intrinsic (specific, e);	/* Must call at run-time */
  else
    {
      result->where = e->where;
      gfc_replace_expr (e, result);
    }

  return SUCCESS;
}


/* Initialize the gfc_current_intrinsic_arg[] array for the benefit of
   error messages.  This subroutine returns FAILURE if a subroutine
   has more than MAX_INTRINSIC_ARGS, in which case the actual argument
   list cannot match any intrinsic.  */

static void
init_arglist (gfc_intrinsic_sym * isym)
{
  gfc_intrinsic_arg *formal;
  int i;

  gfc_current_intrinsic = isym->name;

  i = 0;
  for (formal = isym->formal; formal; formal = formal->next)
    {
      if (i >= MAX_INTRINSIC_ARGS)
	gfc_internal_error ("init_arglist(): too many arguments");
      gfc_current_intrinsic_arg[i++] = formal->name;
    }
}


/* Given a pointer to an intrinsic symbol and an expression consisting
   of a function call, see if the function call is consistent with the
   intrinsic's formal argument list.  Return SUCCESS if the expression
   and intrinsic match, FAILURE otherwise.  */

static try
check_specific (gfc_intrinsic_sym * specific, gfc_expr * expr, int error_flag)
{
  gfc_actual_arglist *arg, **ap;
  int r;
  try t;

  ap = &expr->value.function.actual;

  init_arglist (specific);

  /* Don't attempt to sort the argument list for min or max.  */
  if (specific->check.f1m == gfc_check_min_max
      || specific->check.f1m == gfc_check_min_max_integer
      || specific->check.f1m == gfc_check_min_max_real
      || specific->check.f1m == gfc_check_min_max_double)
    return (*specific->check.f1m) (*ap);

  if (sort_actual (specific->name, ap, specific->formal,
		   &expr->where) == FAILURE)
    return FAILURE;

  if (specific->check.f3ml == gfc_check_minloc_maxloc)
    /* This is special because we might have to reorder the argument
       list.  */
    t = gfc_check_minloc_maxloc (*ap);
  else if (specific->check.f3red == gfc_check_minval_maxval)
    /* This is also special because we also might have to reorder the
       argument list.  */
    t = gfc_check_minval_maxval (*ap);
  else if (specific->check.f3red == gfc_check_product_sum)
    /* Same here. The difference to the previous case is that we allow a
       general numeric type.  */
    t = gfc_check_product_sum (*ap);
  else
     {
       if (specific->check.f1 == NULL)
	 {
	   t = check_arglist (ap, specific, error_flag);
	   if (t == SUCCESS)
	     expr->ts = specific->ts;
	 }
       else
	 t = do_check (specific, *ap);
     }

  /* Check ranks for elemental intrinsics.  */
  if (t == SUCCESS && specific->elemental)
    {
      r = 0;
      for (arg = expr->value.function.actual; arg; arg = arg->next)
	{
	  if (arg->expr == NULL || arg->expr->rank == 0)
	    continue;
	  if (r == 0)
	    {
	      r = arg->expr->rank;
	      continue;
	    }

	  if (arg->expr->rank != r)
	    {
	      gfc_error
		("Ranks of arguments to elemental intrinsic '%s' differ "
		 "at %L", specific->name, &arg->expr->where);
	      return FAILURE;
	    }
	}
    }

  if (t == FAILURE)
    remove_nullargs (ap);

  return t;
}


/* See if an intrinsic is one of the intrinsics we evaluate
   as an extension.  */

static int
gfc_init_expr_extensions (gfc_intrinsic_sym *isym)
{
  /* FIXME: This should be moved into the intrinsic definitions.  */
  static const char * const init_expr_extensions[] = {
    "digits", "epsilon", "huge", "kind", "maxexponent", "minexponent",
    "precision", "present", "radix", "range", "selected_real_kind",
    "tiny", NULL
  };

  int i;

  for (i = 0; init_expr_extensions[i]; i++)
    if (strcmp (init_expr_extensions[i], isym->name) == 0)
      return 0;

  return 1;
}


/* Check whether an intrinsic belongs to whatever standard the user
   has chosen.  */

static void
check_intrinsic_standard (const char *name, int standard, locus * where)
{
  if (!gfc_option.warn_nonstd_intrinsics)
    return;

  gfc_notify_std (standard, "Intrinsic '%s' at %L is not included "
		  "in the selected standard", name, where);
}


/* See if a function call corresponds to an intrinsic function call.
   We return:

    MATCH_YES    if the call corresponds to an intrinsic, simplification
                 is done if possible.

    MATCH_NO     if the call does not correspond to an intrinsic

    MATCH_ERROR  if the call corresponds to an intrinsic but there was an
                 error during the simplification process.

   The error_flag parameter enables an error reporting.  */

match
gfc_intrinsic_func_interface (gfc_expr * expr, int error_flag)
{
  gfc_intrinsic_sym *isym, *specific;
  gfc_actual_arglist *actual;
  const char *name;
  int flag;

  if (expr->value.function.isym != NULL)
    return (do_simplify (expr->value.function.isym, expr) == FAILURE)
      ? MATCH_ERROR : MATCH_YES;

  gfc_suppress_error = !error_flag;
  flag = 0;

  for (actual = expr->value.function.actual; actual; actual = actual->next)
    if (actual->expr != NULL)
      flag |= (actual->expr->ts.type != BT_INTEGER
	       && actual->expr->ts.type != BT_CHARACTER);

  name = expr->symtree->n.sym->name;

  isym = specific = gfc_find_function (name);
  if (isym == NULL)
    {
      gfc_suppress_error = 0;
      return MATCH_NO;
    }

  gfc_current_intrinsic_where = &expr->where;

  /* Bypass the generic list for min and max.  */
  if (isym->check.f1m == gfc_check_min_max)
    {
      init_arglist (isym);

      if (gfc_check_min_max (expr->value.function.actual) == SUCCESS)
	goto got_specific;

      gfc_suppress_error = 0;
      return MATCH_NO;
    }

  /* If the function is generic, check all of its specific
     incarnations.  If the generic name is also a specific, we check
     that name last, so that any error message will correspond to the
     specific.  */
  gfc_suppress_error = 1;

  if (isym->generic)
    {
      for (specific = isym->specific_head; specific;
	   specific = specific->next)
	{
	  if (specific == isym)
	    continue;
	  if (check_specific (specific, expr, 0) == SUCCESS)
	    goto got_specific;
	}
    }

  gfc_suppress_error = !error_flag;

  if (check_specific (isym, expr, error_flag) == FAILURE)
    {
      gfc_suppress_error = 0;
      return MATCH_NO;
    }

  specific = isym;

got_specific:
  expr->value.function.isym = specific;
  gfc_intrinsic_symbol (expr->symtree->n.sym);

  gfc_suppress_error = 0;
  if (do_simplify (specific, expr) == FAILURE)
    return MATCH_ERROR;

  /* TODO: We should probably only allow elemental functions here.  */
  flag |= (expr->ts.type != BT_INTEGER && expr->ts.type != BT_CHARACTER);

  if (pedantic && gfc_init_expr
      && flag && gfc_init_expr_extensions (specific))
    {
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Evaluation of "
	    "nonstandard initialization expression at %L", &expr->where)
	  == FAILURE)
	{
	  return MATCH_ERROR;
	}
    }

  check_intrinsic_standard (name, isym->standard, &expr->where);

  return MATCH_YES;
}


/* See if a CALL statement corresponds to an intrinsic subroutine.
   Returns MATCH_YES if the subroutine corresponds to an intrinsic,
   MATCH_NO if not, and MATCH_ERROR if there was an error (but did
   correspond).  */

match
gfc_intrinsic_sub_interface (gfc_code * c, int error_flag)
{
  gfc_intrinsic_sym *isym;
  const char *name;

  name = c->symtree->n.sym->name;

  isym = find_subroutine (name);
  if (isym == NULL)
    return MATCH_NO;

  gfc_suppress_error = !error_flag;

  init_arglist (isym);

  if (sort_actual (name, &c->ext.actual, isym->formal, &c->loc) == FAILURE)
    goto fail;

  if (isym->check.f1 != NULL)
    {
      if (do_check (isym, c->ext.actual) == FAILURE)
	goto fail;
    }
  else
    {
      if (check_arglist (&c->ext.actual, isym, 1) == FAILURE)
	goto fail;
    }

  /* The subroutine corresponds to an intrinsic.  Allow errors to be
     seen at this point.  */
  gfc_suppress_error = 0;

  if (isym->resolve.s1 != NULL)
    isym->resolve.s1 (c);
  else
    c->resolved_sym = gfc_get_intrinsic_sub_symbol (isym->lib_name);

  if (gfc_pure (NULL) && !isym->elemental)
    {
      gfc_error ("Subroutine call to intrinsic '%s' at %L is not PURE", name,
		 &c->loc);
      return MATCH_ERROR;
    }

  c->resolved_sym->attr.noreturn = isym->noreturn;
  check_intrinsic_standard (name, isym->standard, &c->loc);

  return MATCH_YES;

fail:
  gfc_suppress_error = 0;
  return MATCH_NO;
}


/* Call gfc_convert_type() with warning enabled.  */

try
gfc_convert_type (gfc_expr * expr, gfc_typespec * ts, int eflag)
{
  return gfc_convert_type_warn (expr, ts, eflag, 1);
}


/* Try to convert an expression (in place) from one type to another.
   'eflag' controls the behavior on error.

   The possible values are:

     1 Generate a gfc_error()
     2 Generate a gfc_internal_error().

   'wflag' controls the warning related to conversion.  */

try
gfc_convert_type_warn (gfc_expr * expr, gfc_typespec * ts, int eflag,
		       int wflag)
{
  gfc_intrinsic_sym *sym;
  gfc_typespec from_ts;
  locus old_where;
  gfc_expr *new;
  int rank;
  mpz_t *shape;

  from_ts = expr->ts;		/* expr->ts gets clobbered */

  if (ts->type == BT_UNKNOWN)
    goto bad;

  /* NULL and zero size arrays get their type here.  */
  if (expr->expr_type == EXPR_NULL
      || (expr->expr_type == EXPR_ARRAY
	  && expr->value.constructor == NULL))
    {
      /* Sometimes the RHS acquire the type.  */
      expr->ts = *ts;
      return SUCCESS;
    }

  if (expr->ts.type == BT_UNKNOWN)
    goto bad;

  if (expr->ts.type == BT_DERIVED
      && ts->type == BT_DERIVED
      && gfc_compare_types (&expr->ts, ts))
    return SUCCESS;

  sym = find_conv (&expr->ts, ts);
  if (sym == NULL)
    goto bad;

  /* At this point, a conversion is necessary. A warning may be needed.  */
  if ((gfc_option.warn_std & sym->standard) != 0)
    gfc_warning_now ("Extension: Conversion from %s to %s at %L",
		     gfc_typename (&from_ts), gfc_typename (ts), &expr->where);
  else if (wflag && gfc_option.warn_conversion)
    gfc_warning_now ("Conversion from %s to %s at %L",
		     gfc_typename (&from_ts), gfc_typename (ts), &expr->where);

  /* Insert a pre-resolved function call to the right function.  */
  old_where = expr->where;
  rank = expr->rank;
  shape = expr->shape;

  new = gfc_get_expr ();
  *new = *expr;

  new = gfc_build_conversion (new);
  new->value.function.name = sym->lib_name;
  new->value.function.isym = sym;
  new->where = old_where;
  new->rank = rank;
  new->shape = gfc_copy_shape (shape, rank);

  *expr = *new;

  gfc_free (new);
  expr->ts = *ts;

  if (gfc_is_constant_expr (expr->value.function.actual->expr)
      && do_simplify (sym, expr) == FAILURE)
    {

      if (eflag == 2)
	goto bad;
      return FAILURE;		/* Error already generated in do_simplify() */
    }

  return SUCCESS;

bad:
  if (eflag == 1)
    {
      gfc_error ("Can't convert %s to %s at %L",
		 gfc_typename (&from_ts), gfc_typename (ts), &expr->where);
      return FAILURE;
    }

  gfc_internal_error ("Can't convert %s to %s at %L",
		      gfc_typename (&from_ts), gfc_typename (ts),
		      &expr->where);
  /* Not reached */
}
