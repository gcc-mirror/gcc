/* Build up a list of intrinsic subroutines and functions for the
   name-resolution stage.
   Copyright (C) 2000-2021 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "gfortran.h"
#include "intrinsic.h"

/* Namespace to hold the resolved symbols for intrinsic subroutines.  */
static gfc_namespace *gfc_intrinsic_namespace;

bool gfc_init_expr_flag = false;

/* Pointers to an intrinsic function and its argument names that are being
   checked.  */

const char *gfc_current_intrinsic;
gfc_intrinsic_arg *gfc_current_intrinsic_arg[MAX_INTRINSIC_ARGS];
locus *gfc_current_intrinsic_where;

static gfc_intrinsic_sym *functions, *subroutines, *conversion, *next_sym;
static gfc_intrinsic_sym *char_conversions;
static gfc_intrinsic_arg *next_arg;

static int nfunc, nsub, nargs, nconv, ncharconv;

static enum
{ SZ_NOTHING = 0, SZ_SUBS, SZ_FUNCS, SZ_CONVS }
sizing;

enum klass
{ CLASS_IMPURE = 0, CLASS_PURE, CLASS_ELEMENTAL,
  CLASS_INQUIRY, CLASS_TRANSFORMATIONAL, CLASS_ATOMIC };

#define ACTUAL_NO	0
#define ACTUAL_YES	1

#define REQUIRED	0
#define OPTIONAL	1


/* Return a letter based on the passed type.  Used to construct the
   name of a type-dependent subroutine.  If logical_equals_int is
   true, we can treat a logical like an int.  */

char
gfc_type_letter (bt type, bool logical_equals_int)
{
  char c;

  switch (type)
    {
    case BT_LOGICAL:
      if (logical_equals_int)
	c = 'i';
      else
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


/* Get a symbol for a resolved name. Note, if needed be, the elemental
   attribute has be added afterwards.  */

gfc_symbol *
gfc_get_intrinsic_sub_symbol (const char *name)
{
  gfc_symbol *sym;

  gfc_get_symbol (name, gfc_intrinsic_namespace, &sym);
  sym->attr.always_explicit = 1;
  sym->attr.subroutine = 1;
  sym->attr.flavor = FL_PROCEDURE;
  sym->attr.proc = PROC_INTRINSIC;

  gfc_commit_symbol (sym);

  return sym;
}

/* Get a symbol for a resolved function, with its special name.  The
   actual argument list needs to be set by the caller.  */

gfc_symbol *
gfc_get_intrinsic_function_symbol (gfc_expr *expr)
{
  gfc_symbol *sym;

  gfc_get_symbol (expr->value.function.name, gfc_intrinsic_namespace, &sym);
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
  return sym;
}

/* Find a symbol for a resolved intrinsic procedure, return NULL if
   not found.  */

gfc_symbol *
gfc_find_intrinsic_symbol (gfc_expr *expr)
{
  gfc_symbol *sym;
  gfc_find_symbol (expr->value.function.name, gfc_intrinsic_namespace,
		   0, &sym);
  return sym;
}


/* Return a pointer to the name of a conversion function given two
   typespecs.  */

static const char *
conv_name (gfc_typespec *from, gfc_typespec *to)
{
  return gfc_get_string ("__convert_%c%d_%c%d",
			 gfc_type_letter (from->type), from->kind,
			 gfc_type_letter (to->type), to->kind);
}


/* Given a pair of typespecs, find the gfc_intrinsic_sym node that
   corresponds to the conversion.  Returns NULL if the conversion
   isn't found.  */

static gfc_intrinsic_sym *
find_conv (gfc_typespec *from, gfc_typespec *to)
{
  gfc_intrinsic_sym *sym;
  const char *target;
  int i;

  target = conv_name (from, to);
  sym = conversion;

  for (i = 0; i < nconv; i++, sym++)
    if (target == sym->name)
      return sym;

  return NULL;
}


/* Given a pair of CHARACTER typespecs, find the gfc_intrinsic_sym node
   that corresponds to the conversion.  Returns NULL if the conversion
   isn't found.  */

static gfc_intrinsic_sym *
find_char_conv (gfc_typespec *from, gfc_typespec *to)
{
  gfc_intrinsic_sym *sym;
  const char *target;
  int i;

  target = conv_name (from, to);
  sym = char_conversions;

  for (i = 0; i < ncharconv; i++, sym++)
    if (target == sym->name)
      return sym;

  return NULL;
}


/* Check TS29113, C407b for assumed type and C535b for assumed-rank,
   and a likewise check for NO_ARG_CHECK.  */

static bool
do_ts29113_check (gfc_intrinsic_sym *specific, gfc_actual_arglist *arg)
{
  gfc_actual_arglist *a;

  for (a = arg; a; a = a->next)
    {
      if (!a->expr)
	continue;

      if (a->expr->expr_type == EXPR_VARIABLE
	  && (a->expr->symtree->n.sym->attr.ext_attr
	      & (1 << EXT_ATTR_NO_ARG_CHECK))
	  && specific->id != GFC_ISYM_C_LOC
	  && specific->id != GFC_ISYM_PRESENT)
	{
	  gfc_error ("Variable with NO_ARG_CHECK attribute at %L is only "
		     "permitted as argument to the intrinsic functions "
		     "C_LOC and PRESENT", &a->expr->where);
	  return false;
	}
      else if (a->expr->ts.type == BT_ASSUMED
	       && specific->id != GFC_ISYM_LBOUND
	       && specific->id != GFC_ISYM_PRESENT
	       && specific->id != GFC_ISYM_RANK
	       && specific->id != GFC_ISYM_SHAPE
	       && specific->id != GFC_ISYM_SIZE
	       && specific->id != GFC_ISYM_SIZEOF
	       && specific->id != GFC_ISYM_UBOUND
	       && specific->id != GFC_ISYM_IS_CONTIGUOUS
	       && specific->id != GFC_ISYM_C_LOC)
	{
	  gfc_error ("Assumed-type argument at %L is not permitted as actual"
		     " argument to the intrinsic %s", &a->expr->where,
		     gfc_current_intrinsic);
	  return false;
	}
      else if (a->expr->ts.type == BT_ASSUMED && a != arg)
	{
	  gfc_error ("Assumed-type argument at %L is only permitted as "
		     "first actual argument to the intrinsic %s",
		     &a->expr->where, gfc_current_intrinsic);
	  return false;
	}
      if (a->expr->rank == -1 && !specific->inquiry)
	{
	  gfc_error ("Assumed-rank argument at %L is only permitted as actual "
		     "argument to intrinsic inquiry functions",
		     &a->expr->where);
	  return false;
	}
      if (a->expr->rank == -1 && arg != a)
	{
	  gfc_error ("Assumed-rank argument at %L is only permitted as first "
		     "actual argument to the intrinsic inquiry function %s",
		     &a->expr->where, gfc_current_intrinsic);
	  return false;
	}
    }

  return true;
}


/* Interface to the check functions.  We break apart an argument list
   and call the proper check function rather than forcing each
   function to manipulate the argument list.  */

static bool
do_check (gfc_intrinsic_sym *specific, gfc_actual_arglist *arg)
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
      int	whether function is elemental
      int	If the function can be used as an actual argument [1]
      bt	 return type of function
      int	kind of return type of function
      int	Fortran standard version
      check      pointer to check function
      simplify   pointer to simplification function
      resolve    pointer to resolution function

   Optional arguments come in multiples of five:
      char *      name of argument
      bt          type of argument
      int         kind of argument
      int         arg optional flag (1=optional, 0=required)
      sym_intent  intent of argument

   The sequence is terminated by a NULL name.


 [1] Whether a function can or cannot be used as an actual argument is
     determined by its presence on the 13.6 list in Fortran 2003.  The
     following intrinsics, which are GNU extensions, are considered allowed
     as actual arguments: ACOSH ATANH DACOSH DASINH DATANH DCONJG DIMAG
     ZABS ZCOS ZEXP ZLOG ZSIN ZSQRT.  */

static void
add_sym (const char *name, gfc_isym_id id, enum klass cl, int actual_ok, bt type, int kind,
	 int standard, gfc_check_f check, gfc_simplify_f simplify,
	 gfc_resolve_f resolve, ...)
{
  char buf[GFC_MAX_SYMBOL_LEN + 11]; /* 10 for '_gfortran_', 1 for '\0'  */
  int optional, first_flag;
  sym_intent intent;
  va_list argp;

  switch (sizing)
    {
    case SZ_SUBS:
      nsub++;
      break;

    case SZ_FUNCS:
      nfunc++;
      break;

    case SZ_NOTHING:
      next_sym->name = gfc_get_string ("%s", name);

      strcpy (buf, "_gfortran_");
      strcat (buf, name);
      next_sym->lib_name = gfc_get_string ("%s", buf);

      next_sym->pure = (cl != CLASS_IMPURE);
      next_sym->elemental = (cl == CLASS_ELEMENTAL);
      next_sym->inquiry = (cl == CLASS_INQUIRY);
      next_sym->transformational = (cl == CLASS_TRANSFORMATIONAL);
      next_sym->actual_ok = actual_ok;
      next_sym->ts.type = type;
      next_sym->ts.kind = kind;
      next_sym->standard = standard;
      next_sym->simplify = simplify;
      next_sym->check = check;
      next_sym->resolve = resolve;
      next_sym->specific = 0;
      next_sym->generic = 0;
      next_sym->conversion = 0;
      next_sym->id = id;
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
      intent = (sym_intent) va_arg (argp, int);

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
	  next_arg->value = 0;
	  next_arg->intent = intent;
	}
    }

  va_end (argp);

  next_sym++;
}


/* Add a symbol to the function list where the function takes
   0 arguments.  */

static void
add_sym_0 (const char *name, gfc_isym_id id, enum klass cl, int actual_ok, bt type,
	   int kind, int standard,
	   bool (*check) (void),
	   gfc_expr *(*simplify) (void),
	   void (*resolve) (gfc_expr *))
{
  gfc_simplify_f sf;
  gfc_check_f cf;
  gfc_resolve_f rf;

  cf.f0 = check;
  sf.f0 = simplify;
  rf.f0 = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   (void *) 0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   0 arguments.  */

static void
add_sym_0s (const char *name, gfc_isym_id id, int standard,
	    void (*resolve) (gfc_code *))
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f1 = NULL;
  sf.f1 = NULL;
  rf.s1 = resolve;

  add_sym (name, id, CLASS_IMPURE, ACTUAL_NO, BT_UNKNOWN, 0, standard, cf, sf,
	   rf, (void *) 0);
}


/* Add a symbol to the function list where the function takes
   1 arguments.  */

static void
add_sym_1 (const char *name, gfc_isym_id id, enum klass cl, int actual_ok, bt type,
	   int kind, int standard,
	   bool (*check) (gfc_expr *),
	   gfc_expr *(*simplify) (gfc_expr *),
	   void (*resolve) (gfc_expr *, gfc_expr *),
	   const char *a1, bt type1, int kind1, int optional1)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f1 = check;
  sf.f1 = simplify;
  rf.f1 = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, INTENT_IN,
	   (void *) 0);
}


/* Add a symbol to the function list where the function takes
   1 arguments, specifying the intent of the argument.  */

static void
add_sym_1_intent (const char *name, gfc_isym_id id, enum klass cl,
		  int actual_ok, bt type, int kind, int standard,
		  bool (*check) (gfc_expr *),
		  gfc_expr *(*simplify) (gfc_expr *),
		  void (*resolve) (gfc_expr *, gfc_expr *),
		  const char *a1, bt type1, int kind1, int optional1,
		  sym_intent intent1)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f1 = check;
  sf.f1 = simplify;
  rf.f1 = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, intent1,
	   (void *) 0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   1 arguments, specifying the intent of the argument.  */

static void
add_sym_1s (const char *name, gfc_isym_id id, enum klass cl, bt type, int kind,
	    int standard, bool (*check) (gfc_expr *),
	    gfc_expr *(*simplify) (gfc_expr *), void (*resolve) (gfc_code *),
	    const char *a1, bt type1, int kind1, int optional1,
	    sym_intent intent1)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f1 = check;
  sf.f1 = simplify;
  rf.s1 = resolve;

  add_sym (name, id, cl, ACTUAL_NO, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, intent1,
	   (void *) 0);
}

/* Add a symbol to the subroutine ilst where the subroutine takes one
   printf-style character argument and a variable number of arguments
   to follow.  */

static void
add_sym_1p (const char *name, gfc_isym_id id, enum klass cl, bt type, int kind,
	    int standard, bool (*check) (gfc_actual_arglist *),
	    gfc_expr *(*simplify) (gfc_expr*), void (*resolve) (gfc_code *),
	    const char *a1, bt type1, int kind1, int optional1, sym_intent intent1)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f1m = check;
  sf.f1 = simplify;
  rf.s1 = resolve;

  add_sym (name, id, cl, ACTUAL_NO, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, intent1,
	   (void *) 0);
}


/* Add a symbol from the MAX/MIN family of intrinsic functions to the
   function.  MAX et al take 2 or more arguments.  */

static void
add_sym_1m (const char *name, gfc_isym_id id, enum klass cl, int actual_ok, bt type,
	    int kind, int standard,
	    bool (*check) (gfc_actual_arglist *),
	    gfc_expr *(*simplify) (gfc_expr *),
	    void (*resolve) (gfc_expr *, gfc_actual_arglist *),
	    const char *a1, bt type1, int kind1, int optional1,
	    const char *a2, bt type2, int kind2, int optional2)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f1m = check;
  sf.f1 = simplify;
  rf.f1m = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, INTENT_IN,
	   a2, type2, kind2, optional2, INTENT_IN,
	   (void *) 0);
}


/* Add a symbol to the function list where the function takes
   2 arguments.  */

static void
add_sym_2 (const char *name, gfc_isym_id id, enum klass cl, int actual_ok, bt type,
	   int kind, int standard,
	   bool (*check) (gfc_expr *, gfc_expr *),
	   gfc_expr *(*simplify) (gfc_expr *, gfc_expr *),
	   void (*resolve) (gfc_expr *, gfc_expr *, gfc_expr *),
	   const char *a1, bt type1, int kind1, int optional1,
	   const char *a2, bt type2, int kind2, int optional2)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f2 = check;
  sf.f2 = simplify;
  rf.f2 = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, INTENT_IN,
	   a2, type2, kind2, optional2, INTENT_IN,
	   (void *) 0);
}


/* Add a symbol to the function list where the function takes
   2 arguments; same as add_sym_2 - but allows to specify the intent.  */

static void
add_sym_2_intent (const char *name, gfc_isym_id id, enum klass cl,
		  int actual_ok, bt type, int kind, int standard,
		  bool (*check) (gfc_expr *, gfc_expr *),
		  gfc_expr *(*simplify) (gfc_expr *, gfc_expr *),
		  void (*resolve) (gfc_expr *, gfc_expr *, gfc_expr *),
		  const char *a1, bt type1, int kind1, int optional1,
		  sym_intent intent1, const char *a2, bt type2, int kind2,
		  int optional2, sym_intent intent2)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f2 = check;
  sf.f2 = simplify;
  rf.f2 = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, intent1,
	   a2, type2, kind2, optional2, intent2,
	   (void *) 0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   2 arguments, specifying the intent of the arguments.  */

static void
add_sym_2s (const char *name, gfc_isym_id id, enum klass cl, bt type,
	    int kind, int standard,
	    bool (*check) (gfc_expr *, gfc_expr *),
	    gfc_expr *(*simplify) (gfc_expr *, gfc_expr *),
	    void (*resolve) (gfc_code *),
	    const char *a1, bt type1, int kind1, int optional1,
	    sym_intent intent1, const char *a2, bt type2, int kind2,
	    int optional2, sym_intent intent2)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f2 = check;
  sf.f2 = simplify;
  rf.s1 = resolve;

  add_sym (name, id, cl, ACTUAL_NO, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, intent1,
	   a2, type2, kind2, optional2, intent2,
	   (void *) 0);
}


/* Add a symbol to the function list where the function takes
   3 arguments.  */

static void
add_sym_3 (const char *name, gfc_isym_id id, enum klass cl, int actual_ok, bt type,
	   int kind, int standard,
	   bool (*check) (gfc_expr *, gfc_expr *, gfc_expr *),
	   gfc_expr *(*simplify) (gfc_expr *, gfc_expr *, gfc_expr *),
	   void (*resolve) (gfc_expr *, gfc_expr *, gfc_expr *, gfc_expr *),
	   const char *a1, bt type1, int kind1, int optional1,
	   const char *a2, bt type2, int kind2, int optional2,
	   const char *a3, bt type3, int kind3, int optional3)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f3 = check;
  sf.f3 = simplify;
  rf.f3 = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, INTENT_IN,
	   a2, type2, kind2, optional2, INTENT_IN,
	   a3, type3, kind3, optional3, INTENT_IN,
	   (void *) 0);
}


/* MINLOC and MAXLOC get special treatment because their
   argument might have to be reordered.  */

static void
add_sym_5ml (const char *name, gfc_isym_id id, enum klass cl, int actual_ok, bt type,
	     int kind, int standard,
	     bool (*check) (gfc_actual_arglist *),
	     gfc_expr *(*simplify) (gfc_expr *, gfc_expr *, gfc_expr *,
				    gfc_expr *, gfc_expr *),
	     void (*resolve) (gfc_expr *, gfc_expr *, gfc_expr *, gfc_expr *,
			      gfc_expr *, gfc_expr *),
	     const char *a1, bt type1, int kind1, int optional1,
	     const char *a2, bt type2, int kind2, int optional2,
	     const char *a3, bt type3, int kind3, int optional3,
	     const char *a4, bt type4, int kind4, int optional4,
	     const char *a5, bt type5, int kind5, int optional5)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f5ml = check;
  sf.f5 = simplify;
  rf.f5 = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, INTENT_IN,
	   a2, type2, kind2, optional2, INTENT_IN,
	   a3, type3, kind3, optional3, INTENT_IN,
	   a4, type4, kind4, optional4, INTENT_IN,
	   a5, type5, kind5, optional5, INTENT_IN,
	   (void *) 0);
}

/* Similar for FINDLOC.  */

static void
add_sym_6fl (const char *name, gfc_isym_id id, enum klass cl, int actual_ok,
	     bt type, int kind, int standard,
	     bool (*check) (gfc_actual_arglist *),
	     gfc_expr *(*simplify) (gfc_expr *, gfc_expr *, gfc_expr *,
				    gfc_expr *, gfc_expr *, gfc_expr *),
	     void (*resolve) (gfc_expr *, gfc_expr *, gfc_expr *, gfc_expr *,
			      gfc_expr *, gfc_expr *, gfc_expr *),
	     const char *a1, bt type1, int kind1, int optional1,
	     const char *a2, bt type2, int kind2, int optional2,
	     const char *a3, bt type3, int kind3, int optional3,
	     const char *a4, bt type4, int kind4, int optional4,
	     const char *a5, bt type5, int kind5, int optional5,
	     const char *a6, bt type6, int kind6, int optional6)

{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f6fl = check;
  sf.f6 = simplify;
  rf.f6 = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, INTENT_IN,
	   a2, type2, kind2, optional2, INTENT_IN,
	   a3, type3, kind3, optional3, INTENT_IN,
	   a4, type4, kind4, optional4, INTENT_IN,
	   a5, type5, kind5, optional5, INTENT_IN,
	   a6, type6, kind6, optional6, INTENT_IN,
	   (void *) 0);
}


/* MINVAL, MAXVAL, PRODUCT, and SUM also get special treatment because
   their argument also might have to be reordered.  */

static void
add_sym_3red (const char *name, gfc_isym_id id, enum klass cl, int actual_ok, bt type,
	      int kind, int standard,
	      bool (*check) (gfc_actual_arglist *),
	      gfc_expr *(*simplify) (gfc_expr *, gfc_expr *, gfc_expr *),
	      void (*resolve) (gfc_expr *, gfc_expr *, gfc_expr *, gfc_expr *),
	      const char *a1, bt type1, int kind1, int optional1,
	      const char *a2, bt type2, int kind2, int optional2,
	      const char *a3, bt type3, int kind3, int optional3)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f3red = check;
  sf.f3 = simplify;
  rf.f3 = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, INTENT_IN,
	   a2, type2, kind2, optional2, INTENT_IN,
	   a3, type3, kind3, optional3, INTENT_IN,
	   (void *) 0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   3 arguments, specifying the intent of the arguments.  */

static void
add_sym_3s (const char *name, gfc_isym_id id, enum klass cl, bt type,
	    int kind, int standard,
	    bool (*check) (gfc_expr *, gfc_expr *, gfc_expr *),
	    gfc_expr *(*simplify) (gfc_expr *, gfc_expr *, gfc_expr *),
	    void (*resolve) (gfc_code *),
	    const char *a1, bt type1, int kind1, int optional1,
	    sym_intent intent1, const char *a2, bt type2, int kind2,
	    int optional2, sym_intent intent2, const char *a3, bt type3,
	    int kind3, int optional3, sym_intent intent3)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f3 = check;
  sf.f3 = simplify;
  rf.s1 = resolve;

  add_sym (name, id, cl, ACTUAL_NO, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, intent1,
	   a2, type2, kind2, optional2, intent2,
	   a3, type3, kind3, optional3, intent3,
	   (void *) 0);
}


/* Add a symbol to the function list where the function takes
   4 arguments.  */

static void
add_sym_4 (const char *name, gfc_isym_id id, enum klass cl, int actual_ok, bt type,
	   int kind, int standard,
	   bool (*check) (gfc_expr *, gfc_expr *, gfc_expr *, gfc_expr *),
	   gfc_expr *(*simplify) (gfc_expr *, gfc_expr *, gfc_expr *,
				  gfc_expr *),
	   void (*resolve) (gfc_expr *, gfc_expr *, gfc_expr *, gfc_expr *,
			    gfc_expr *),
	   const char *a1, bt type1, int kind1, int optional1,
	   const char *a2, bt type2, int kind2, int optional2,
	   const char *a3, bt type3, int kind3, int optional3,
	   const char *a4, bt type4, int kind4, int optional4 )
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f4 = check;
  sf.f4 = simplify;
  rf.f4 = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, INTENT_IN,
	   a2, type2, kind2, optional2, INTENT_IN,
	   a3, type3, kind3, optional3, INTENT_IN,
	   a4, type4, kind4, optional4, INTENT_IN,
	   (void *) 0);
}

/* Add a symbol to the function list where the function takes 4
   arguments and resolution may need to change the number or
   arrangement of arguments. This is the case for INDEX, which needs
   its KIND argument removed.  */

static void
add_sym_4ind (const char *name, gfc_isym_id id, enum klass cl, int actual_ok,
	      bt type, int kind, int standard,
	      bool (*check) (gfc_expr *, gfc_expr *, gfc_expr *, gfc_expr *),
	      gfc_expr *(*simplify) (gfc_expr *, gfc_expr *, gfc_expr *,
				     gfc_expr *),
	      void (*resolve) (gfc_expr *, gfc_actual_arglist *),
	      const char *a1, bt type1, int kind1, int optional1,
	      const char *a2, bt type2, int kind2, int optional2,
	      const char *a3, bt type3, int kind3, int optional3,
	      const char *a4, bt type4, int kind4, int optional4 )
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f4 = check;
  sf.f4 = simplify;
  rf.f1m = resolve;

  add_sym (name, id, cl, actual_ok, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, INTENT_IN,
	   a2, type2, kind2, optional2, INTENT_IN,
	   a3, type3, kind3, optional3, INTENT_IN,
	   a4, type4, kind4, optional4, INTENT_IN,
	   (void *) 0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   4 arguments.  */

static void
add_sym_4s (const char *name, gfc_isym_id id, enum klass cl, bt type, int kind,
	    int standard,
	    bool (*check) (gfc_expr *, gfc_expr *, gfc_expr *, gfc_expr *),
	    gfc_expr *(*simplify) (gfc_expr *, gfc_expr *, gfc_expr *,
				   gfc_expr *),
	    void (*resolve) (gfc_code *),
	    const char *a1, bt type1, int kind1, int optional1,
	    sym_intent intent1, const char *a2, bt type2, int kind2,
	    int optional2, sym_intent intent2, const char *a3, bt type3,
	    int kind3, int optional3, sym_intent intent3, const char *a4,
	    bt type4, int kind4, int optional4, sym_intent intent4)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f4 = check;
  sf.f4 = simplify;
  rf.s1 = resolve;

  add_sym (name, id, cl, ACTUAL_NO, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, intent1,
	   a2, type2, kind2, optional2, intent2,
	   a3, type3, kind3, optional3, intent3,
	   a4, type4, kind4, optional4, intent4,
	   (void *) 0);
}


/* Add a symbol to the subroutine list where the subroutine takes
   5 arguments.  */

static void
add_sym_5s (const char *name, gfc_isym_id id, enum klass cl, bt type, int kind,
	    int standard,
	    bool (*check) (gfc_expr *, gfc_expr *, gfc_expr *, gfc_expr *,
			  gfc_expr *),
	    gfc_expr *(*simplify) (gfc_expr *, gfc_expr *, gfc_expr *,
				   gfc_expr *, gfc_expr *),
	    void (*resolve) (gfc_code *),
	    const char *a1, bt type1, int kind1, int optional1,
	    sym_intent intent1, const char *a2, bt type2, int kind2,
	    int optional2, sym_intent intent2, const char *a3, bt type3,
	    int kind3, int optional3, sym_intent intent3, const char *a4,
	    bt type4, int kind4, int optional4, sym_intent intent4,
	    const char *a5, bt type5, int kind5, int optional5,
	    sym_intent intent5)
{
  gfc_check_f cf;
  gfc_simplify_f sf;
  gfc_resolve_f rf;

  cf.f5 = check;
  sf.f5 = simplify;
  rf.s1 = resolve;

  add_sym (name, id, cl, ACTUAL_NO, type, kind, standard, cf, sf, rf,
	   a1, type1, kind1, optional1, intent1,
	   a2, type2, kind2, optional2, intent2,
	   a3, type3, kind3, optional3, intent3,
	   a4, type4, kind4, optional4, intent4,
	   a5, type5, kind5, optional5, intent5,
	   (void *) 0);
}


/* Locate an intrinsic symbol given a base pointer, number of elements
   in the table and a pointer to a name.  Returns the NULL pointer if
   a name is not found.  */

static gfc_intrinsic_sym *
find_sym (gfc_intrinsic_sym *start, int n, const char *name)
{
  /* name may be a user-supplied string, so we must first make sure
     that we're comparing against a pointer into the global string
     table.  */
  const char *p = gfc_get_string ("%s", name);

  while (n > 0)
    {
      if (p == start->name)
	return start;

      start++;
      n--;
    }

  return NULL;
}


gfc_isym_id
gfc_isym_id_by_intmod (intmod_id from_intmod, int intmod_sym_id)
{
  if (from_intmod == INTMOD_NONE)
    return (gfc_isym_id) intmod_sym_id;
  else if (from_intmod == INTMOD_ISO_C_BINDING)
    return (gfc_isym_id) c_interop_kinds_table[intmod_sym_id].value;
  else if (from_intmod == INTMOD_ISO_FORTRAN_ENV)
    switch (intmod_sym_id)
      {
#define NAMED_SUBROUTINE(a,b,c,d) \
      case a: \
	return (gfc_isym_id) c;
#define NAMED_FUNCTION(a,b,c,d) \
      case a: \
	return (gfc_isym_id) c;
#include "iso-fortran-env.def"
      default:
	gcc_unreachable ();
      }
  else
    gcc_unreachable ();
  return (gfc_isym_id) 0;
}


gfc_isym_id
gfc_isym_id_by_intmod_sym (gfc_symbol *sym)
{
  return gfc_isym_id_by_intmod (sym->from_intmod, sym->intmod_sym_id);
}


gfc_intrinsic_sym *
gfc_intrinsic_subroutine_by_id (gfc_isym_id id)
{
  gfc_intrinsic_sym *start = subroutines;
  int n = nsub;

  while (true)
    {
      gcc_assert (n > 0);
      if (id == start->id)
	return start;

      start++;
      n--;
    }
}


gfc_intrinsic_sym *
gfc_intrinsic_function_by_id (gfc_isym_id id)
{
  gfc_intrinsic_sym *start = functions;
  int n = nfunc;

  while (true)
    {
      gcc_assert (n > 0);
      if (id == start->id)
	return start;

      start++;
      n--;
    }
}


/* Given a name, find a function in the intrinsic function table.
   Returns NULL if not found.  */

gfc_intrinsic_sym *
gfc_find_function (const char *name)
{
  gfc_intrinsic_sym *sym;

  sym = find_sym (functions, nfunc, name);
  if (!sym || sym->from_module)
    sym = find_sym (conversion, nconv, name);

  return (!sym || sym->from_module) ? NULL : sym;
}


/* Given a name, find a function in the intrinsic subroutine table.
   Returns NULL if not found.  */

gfc_intrinsic_sym *
gfc_find_subroutine (const char *name)
{
  gfc_intrinsic_sym *sym;
  sym = find_sym (subroutines, nsub, name);
  return (!sym || sym->from_module) ? NULL : sym;
}


/* Given a string, figure out if it is the name of a generic intrinsic
   function or not.  */

int
gfc_generic_intrinsic (const char *name)
{
  gfc_intrinsic_sym *sym;

  sym = gfc_find_function (name);
  return (!sym || sym->from_module) ? 0 : sym->generic;
}


/* Given a string, figure out if it is the name of a specific
   intrinsic function or not.  */

int
gfc_specific_intrinsic (const char *name)
{
  gfc_intrinsic_sym *sym;

  sym = gfc_find_function (name);
  return (!sym || sym->from_module) ? 0 : sym->specific;
}


/* Given a string, figure out if it is the name of an intrinsic function
   or subroutine allowed as an actual argument or not.  */
int
gfc_intrinsic_actual_ok (const char *name, const bool subroutine_flag)
{
  gfc_intrinsic_sym *sym;

  /* Intrinsic subroutines are not allowed as actual arguments.  */
  if (subroutine_flag)
    return 0;
  else
    {
      sym = gfc_find_function (name);
      return (sym == NULL) ? 0 : sym->actual_ok;
    }
}


/* Given a symbol, find out if it is (and is to be treated as) an intrinsic.
   If its name refers to an intrinsic, but this intrinsic is not included in
   the selected standard, this returns FALSE and sets the symbol's external
   attribute.  */

bool
gfc_is_intrinsic (gfc_symbol* sym, int subroutine_flag, locus loc)
{
  gfc_intrinsic_sym* isym;
  const char* symstd;

  /* If INTRINSIC attribute is already known, return.  */
  if (sym->attr.intrinsic)
    return true;

  /* Check for attributes which prevent the symbol from being INTRINSIC.  */
  if (sym->attr.external || sym->attr.contained
      || sym->attr.if_source == IFSRC_IFBODY)
    return false;

  if (subroutine_flag)
    isym = gfc_find_subroutine (sym->name);
  else
    isym = gfc_find_function (sym->name);

  /* No such intrinsic available at all?  */
  if (!isym)
    return false;

  /* See if this intrinsic is allowed in the current standard.  */
  if (!gfc_check_intrinsic_standard (isym, &symstd, false, loc)
      && !sym->attr.artificial)
    {
      if (sym->attr.proc == PROC_UNKNOWN && warn_intrinsics_std)
	gfc_warning_now (OPT_Wintrinsics_std, "The intrinsic %qs at %L is not "
			 "included in the selected standard but %s and %qs will"
			 " be treated as if declared EXTERNAL.  Use an"
			 " appropriate %<-std=%>* option or define"
			 " %<-fall-intrinsics%> to allow this intrinsic.",
			 sym->name, &loc, symstd, sym->name);

      return false;
    }

  return true;
}


/* Collect a set of intrinsic functions into a generic collection.
   The first argument is the name of the generic function, which is
   also the name of a specific function.  The rest of the specifics
   currently in the table are placed into the list of specific
   functions associated with that generic.

   PR fortran/32778
   FIXME: Remove the argument STANDARD if no regressions are
          encountered. Change all callers (approx. 360).
*/

static void
make_generic (const char *name, gfc_isym_id id, int standard ATTRIBUTE_UNUSED)
{
  gfc_intrinsic_sym *g;

  if (sizing != SZ_NOTHING)
    return;

  g = gfc_find_function (name);
  if (g == NULL)
    gfc_internal_error ("make_generic(): Cannot find generic symbol %qs",
			name);

  gcc_assert (g->id == id);

  g->generic = 1;
  g->specific = 1;
  if ((g + 1)->name != NULL)
    g->specific_head = g + 1;
  g++;

  while (g->name != NULL)
    {
      g->next = g + 1;
      g->specific = 1;
      g++;
    }

  g--;
  g->next = NULL;
}


/* Create a duplicate intrinsic function entry for the current
   function, the only differences being the alternate name and
   a different standard if necessary. Note that we use argument
   lists more than once, but all argument lists are freed as a
   single block.  */

static void
make_alias (const char *name, int standard)
{
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
      next_sym->name = gfc_get_string ("%s", name);
      next_sym->standard = standard;
      next_sym++;
      break;

    default:
      break;
    }
}


/* Make the current subroutine noreturn.  */

static void
make_noreturn (void)
{
  if (sizing == SZ_NOTHING)
    next_sym[-1].noreturn = 1;
}


/* Mark current intrinsic as module intrinsic.  */
static void
make_from_module (void)
{
  if (sizing == SZ_NOTHING)
    next_sym[-1].from_module = 1;
}


/* Mark the current subroutine as having a variable number of
   arguments.  */

static void
make_vararg (void)
{
  if (sizing == SZ_NOTHING)
    next_sym[-1].vararg = 1;
}

/* Set the attr.value of the current procedure.  */

static void
set_attr_value (int n, ...)
{
  gfc_intrinsic_arg *arg;
  va_list argp;
  int i;

  if (sizing != SZ_NOTHING)
    return;

  va_start (argp, n);
  arg = next_sym[-1].formal;

  for (i = 0; i < n; i++)
    {
      gcc_assert (arg != NULL);
      arg->value = va_arg (argp, int);
      arg = arg->next;
    }
  va_end (argp);
}


/* Add intrinsic functions.  */

static void
add_functions (void)
{
  /* Argument names.  These are used as argument keywords and so need to
    match the documentation.  Please keep this list in sorted order.  */
  const char
    *a = "a", *a1 = "a1", *a2 = "a2", *ar = "array", *b = "b",
    *bck = "back", *bd = "boundary", *c = "c", *c_ptr_1 = "c_ptr_1",
    *c_ptr_2 = "c_ptr_2", *ca = "coarray", *com = "command",
    *dist = "distance", *dm = "dim", *f = "field", *failed="failed",
    *fs = "fsource", *han = "handler", *i = "i",
    *image = "image", *j = "j", *kind = "kind",
    *l = "l", *ln = "len", *level = "level", *m = "matrix", *ma = "matrix_a",
    *mb = "matrix_b", *md = "mode", *mo = "mold", *msk = "mask",
    *n = "n", *ncopies= "ncopies", *nm = "name", *num = "number",
    *ord = "order", *p = "p", *p1 = "path1", *p2 = "path2",
    *pad = "pad", *pid = "pid", *pos = "pos", *pt = "pointer",
    *r = "r", *s = "s", *set = "set", *sh = "shift", *shp = "shape",
    *sig = "sig", *src = "source", *ssg = "substring",
    *sta = "string_a", *stb = "string_b", *stg = "string",
    *sub = "sub", *sz = "size", *tg = "target", *team = "team", *tm = "time",
    *ts = "tsource", *ut = "unit", *v = "vector", *va = "vector_a",
    *vb = "vector_b", *vl = "values", *val = "value", *x = "x", *y = "y",
    *z = "z";

  int di, dr, dd, dl, dc, dz, ii;

  di = gfc_default_integer_kind;
  dr = gfc_default_real_kind;
  dd = gfc_default_double_kind;
  dl = gfc_default_logical_kind;
  dc = gfc_default_character_kind;
  dz = gfc_default_complex_kind;
  ii = gfc_index_integer_kind;

  add_sym_1 ("abs", GFC_ISYM_ABS, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_abs, gfc_simplify_abs, gfc_resolve_abs,
	     a, BT_REAL, dr, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("babs", GFC_STD_GNU);
      make_alias ("iiabs", GFC_STD_GNU);
      make_alias ("jiabs", GFC_STD_GNU);
      make_alias ("kiabs", GFC_STD_GNU);
    }

  add_sym_1 ("iabs", GFC_ISYM_ABS, CLASS_ELEMENTAL, ACTUAL_YES, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_abs, gfc_resolve_abs,
	     a, BT_INTEGER, di, REQUIRED);

  add_sym_1 ("dabs", GFC_ISYM_ABS, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_abs, gfc_resolve_abs,
	     a, BT_REAL, dd, REQUIRED);

  add_sym_1 ("cabs", GFC_ISYM_ABS, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     NULL, gfc_simplify_abs, gfc_resolve_abs,
	     a, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zabs", GFC_ISYM_ABS, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_abs, gfc_resolve_abs,
	     a, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdabs", GFC_STD_GNU);

  make_generic ("abs", GFC_ISYM_ABS, GFC_STD_F77);

  /* The checking function for ACCESS is called gfc_check_access_func
     because the name gfc_check_access is already used in module.c.  */
  add_sym_2 ("access", GFC_ISYM_ACCESS, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, gfc_check_access_func, NULL, gfc_resolve_access,
	     nm, BT_CHARACTER, dc, REQUIRED, md, BT_CHARACTER, dc, REQUIRED);

  make_generic ("access", GFC_ISYM_ACCESS, GFC_STD_GNU);

  add_sym_2 ("achar", GFC_ISYM_ACHAR, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_CHARACTER, dc, GFC_STD_F95,
	     gfc_check_achar, gfc_simplify_achar, gfc_resolve_achar,
	     i, BT_INTEGER, di, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("achar", GFC_ISYM_ACHAR, GFC_STD_F95);

  add_sym_1 ("acos", GFC_ISYM_ACOS, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc2008, gfc_simplify_acos, gfc_resolve_acos,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dacos", GFC_ISYM_ACOS, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_acos, gfc_resolve_acos,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("acos", GFC_ISYM_ACOS, GFC_STD_F77);

  add_sym_1 ("acosh", GFC_ISYM_ACOSH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr,
	     GFC_STD_F2008, gfc_check_fn_rc2008, gfc_simplify_acosh,
	     gfc_resolve_acosh, x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dacosh", GFC_ISYM_ACOSH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_acosh, gfc_resolve_acosh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("acosh", GFC_ISYM_ACOSH, GFC_STD_F2008);

  add_sym_1 ("adjustl", GFC_ISYM_ADJUSTL, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_CHARACTER, dc, GFC_STD_F95, NULL, gfc_simplify_adjustl,
	     gfc_resolve_adjustl, stg, BT_CHARACTER, 0, REQUIRED);

  make_generic ("adjustl", GFC_ISYM_ADJUSTL, GFC_STD_F95);

  add_sym_1 ("adjustr", GFC_ISYM_ADJUSTR, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_CHARACTER, dc, GFC_STD_F95, NULL, gfc_simplify_adjustr,
	     gfc_resolve_adjustr, stg, BT_CHARACTER, 0, REQUIRED);

  make_generic ("adjustr", GFC_ISYM_ADJUSTR, GFC_STD_F95);

  add_sym_1 ("aimag", GFC_ISYM_AIMAG, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_c, gfc_simplify_aimag, gfc_resolve_aimag,
	     z, BT_COMPLEX, dz, REQUIRED);

  make_alias ("imag", GFC_STD_GNU);
  make_alias ("imagpart", GFC_STD_GNU);

  add_sym_1 ("dimag", GFC_ISYM_AIMAG, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_aimag, gfc_resolve_aimag,
	     z, BT_COMPLEX, dd, REQUIRED);

  make_generic ("aimag", GFC_ISYM_AIMAG, GFC_STD_F77);

  add_sym_2 ("aint", GFC_ISYM_AINT, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_a_xkind, gfc_simplify_aint, gfc_resolve_aint,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  add_sym_1 ("dint", GFC_ISYM_AINT, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_dint, gfc_resolve_dint,
	     a, BT_REAL, dd, REQUIRED);

  make_generic ("aint", GFC_ISYM_AINT, GFC_STD_F77);

  add_sym_2 ("all", GFC_ISYM_ALL, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_LOGICAL, dl, GFC_STD_F95,
	     gfc_check_all_any, gfc_simplify_all, gfc_resolve_all,
	     msk, BT_LOGICAL, dl, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("all", GFC_ISYM_ALL, GFC_STD_F95);

  add_sym_1 ("allocated", GFC_ISYM_ALLOCATED, CLASS_INQUIRY, ACTUAL_NO, BT_LOGICAL, dl, GFC_STD_F95,
	     gfc_check_allocated, NULL, NULL,
	     ar, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("allocated", GFC_ISYM_ALLOCATED, GFC_STD_F95);

  add_sym_2 ("anint", GFC_ISYM_ANINT, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_a_xkind, gfc_simplify_anint, gfc_resolve_anint,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  add_sym_1 ("dnint", GFC_ISYM_ANINT, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     NULL, gfc_simplify_dnint, gfc_resolve_dnint,
	     a, BT_REAL, dd, REQUIRED);

  make_generic ("anint", GFC_ISYM_ANINT, GFC_STD_F77);

  add_sym_2 ("any", GFC_ISYM_ANY, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_LOGICAL, dl, GFC_STD_F95,
	     gfc_check_all_any, gfc_simplify_any, gfc_resolve_any,
	     msk, BT_LOGICAL, dl, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("any", GFC_ISYM_ANY, GFC_STD_F95);

  add_sym_1 ("asin", GFC_ISYM_ASIN, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc2008, gfc_simplify_asin, gfc_resolve_asin,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dasin", GFC_ISYM_ASIN, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_asin, gfc_resolve_asin,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("asin", GFC_ISYM_ASIN, GFC_STD_F77);

  add_sym_1 ("asinh", GFC_ISYM_ASINH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr,
	     GFC_STD_F2008, gfc_check_fn_rc2008, gfc_simplify_asinh,
	     gfc_resolve_asinh, x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dasinh", GFC_ISYM_ASINH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_asinh, gfc_resolve_asinh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("asinh", GFC_ISYM_ASINH, GFC_STD_F2008);

  add_sym_2 ("associated", GFC_ISYM_ASSOCIATED, CLASS_INQUIRY, ACTUAL_NO, BT_LOGICAL, dl,
	     GFC_STD_F95, gfc_check_associated, NULL, NULL,
	     pt, BT_UNKNOWN, 0, REQUIRED, tg, BT_UNKNOWN, 0, OPTIONAL);

  make_generic ("associated", GFC_ISYM_ASSOCIATED, GFC_STD_F95);

  add_sym_1 ("atan", GFC_ISYM_ATAN, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc2008, gfc_simplify_atan, gfc_resolve_atan,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("datan", GFC_ISYM_ATAN, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_atan, gfc_resolve_atan,
	     x, BT_REAL, dd, REQUIRED);

  /* Two-argument version of atan, equivalent to atan2.  */
  add_sym_2 ("atan", GFC_ISYM_ATAN2, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F2008,
	     gfc_check_atan_2, gfc_simplify_atan2, gfc_resolve_atan2,
	     y, BT_REAL, dr, REQUIRED, x, BT_REAL, dr, REQUIRED);

  make_generic ("atan", GFC_ISYM_ATAN, GFC_STD_F77);

  add_sym_1 ("atanh", GFC_ISYM_ATANH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr,
	     GFC_STD_F2008, gfc_check_fn_rc2008, gfc_simplify_atanh,
	     gfc_resolve_atanh, x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("datanh", GFC_ISYM_ATANH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_atanh, gfc_resolve_atanh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("atanh", GFC_ISYM_ATANH, GFC_STD_F2008);

  add_sym_2 ("atan2", GFC_ISYM_ATAN2, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_atan2, gfc_simplify_atan2, gfc_resolve_atan2,
	     y, BT_REAL, dr, REQUIRED, x, BT_REAL, dr, REQUIRED);

  add_sym_2 ("datan2", GFC_ISYM_ATAN2, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_datan2, gfc_simplify_atan2, gfc_resolve_atan2,
	     y, BT_REAL, dd, REQUIRED, x, BT_REAL, dd, REQUIRED);

  make_generic ("atan2", GFC_ISYM_ATAN2, GFC_STD_F77);

  /* Bessel and Neumann functions for G77 compatibility.  */
  add_sym_1 ("besj0", GFC_ISYM_J0, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_bessel_j0, gfc_resolve_g77_math1,
	     x, BT_REAL, dr, REQUIRED);

  make_alias ("bessel_j0", GFC_STD_F2008);

  add_sym_1 ("dbesj0", GFC_ISYM_J0, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_bessel_j0, gfc_resolve_g77_math1,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("bessel_j0", GFC_ISYM_J0, GFC_STD_F2008);

  add_sym_1 ("besj1", GFC_ISYM_J1, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_bessel_j1, gfc_resolve_g77_math1,
	     x, BT_REAL, dr, REQUIRED);

  make_alias ("bessel_j1", GFC_STD_F2008);

  add_sym_1 ("dbesj1", GFC_ISYM_J1, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_bessel_j1, gfc_resolve_g77_math1,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("bessel_j1", GFC_ISYM_J1, GFC_STD_F2008);

  add_sym_2 ("besjn", GFC_ISYM_JN, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_besn, gfc_simplify_bessel_jn, gfc_resolve_besn,
	     n, BT_INTEGER, di, REQUIRED, x, BT_REAL, dr, REQUIRED);

  make_alias ("bessel_jn", GFC_STD_F2008);

  add_sym_2 ("dbesjn", GFC_ISYM_JN, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_besn, gfc_simplify_bessel_jn, gfc_resolve_besn,
	     n, BT_INTEGER, di, REQUIRED, x, BT_REAL, dd, REQUIRED);

  add_sym_3 ("bessel_jn", GFC_ISYM_JN2, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F2008,
	     gfc_check_bessel_n2, gfc_simplify_bessel_jn2, gfc_resolve_bessel_n2,
	     "n1", BT_INTEGER, di, REQUIRED,"n2", BT_INTEGER, di, REQUIRED,
	     x, BT_REAL, dr, REQUIRED);
  set_attr_value (3, true, true, true);

  make_generic ("bessel_jn", GFC_ISYM_JN, GFC_STD_F2008);

  add_sym_1 ("besy0", GFC_ISYM_Y0, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_bessel_y0, gfc_resolve_g77_math1,
	     x, BT_REAL, dr, REQUIRED);

  make_alias ("bessel_y0", GFC_STD_F2008);

  add_sym_1 ("dbesy0", GFC_ISYM_Y0, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_bessel_y0, gfc_resolve_g77_math1,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("bessel_y0", GFC_ISYM_Y0, GFC_STD_F2008);

  add_sym_1 ("besy1", GFC_ISYM_Y1, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_bessel_y1, gfc_resolve_g77_math1,
	     x, BT_REAL, dr, REQUIRED);

  make_alias ("bessel_y1", GFC_STD_F2008);

  add_sym_1 ("dbesy1", GFC_ISYM_Y1, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_bessel_y1, gfc_resolve_g77_math1,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("bessel_y1", GFC_ISYM_Y1, GFC_STD_F2008);

  add_sym_2 ("besyn", GFC_ISYM_YN, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_besn, gfc_simplify_bessel_yn, gfc_resolve_besn,
	     n, BT_INTEGER, di, REQUIRED, x, BT_REAL, dr, REQUIRED);

  make_alias ("bessel_yn", GFC_STD_F2008);

  add_sym_2 ("dbesyn", GFC_ISYM_YN, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_besn, gfc_simplify_bessel_yn, gfc_resolve_besn,
	     n, BT_INTEGER, di, REQUIRED, x, BT_REAL, dd, REQUIRED);

  add_sym_3 ("bessel_yn", GFC_ISYM_YN2, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F2008,
	     gfc_check_bessel_n2, gfc_simplify_bessel_yn2, gfc_resolve_bessel_n2,
	     "n1", BT_INTEGER, di, REQUIRED,"n2", BT_INTEGER, di, REQUIRED,
	      x, BT_REAL, dr, REQUIRED);
  set_attr_value (3, true, true, true);

  make_generic ("bessel_yn", GFC_ISYM_YN, GFC_STD_F2008);

  add_sym_2 ("bge", GFC_ISYM_BGE, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_LOGICAL, dl, GFC_STD_F2008,
	     gfc_check_bge_bgt_ble_blt, gfc_simplify_bge, NULL,
	     i, BT_INTEGER, di, REQUIRED, j, BT_INTEGER, di, REQUIRED);

  make_generic ("bge", GFC_ISYM_BGE, GFC_STD_F2008);

  add_sym_2 ("bgt", GFC_ISYM_BGT, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_LOGICAL, dl, GFC_STD_F2008,
	     gfc_check_bge_bgt_ble_blt, gfc_simplify_bgt, NULL,
	     i, BT_INTEGER, di, REQUIRED, j, BT_INTEGER, di, REQUIRED);

  make_generic ("bgt", GFC_ISYM_BGT, GFC_STD_F2008);

  add_sym_1 ("bit_size", GFC_ISYM_BIT_SIZE, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_i, gfc_simplify_bit_size, NULL,
	     i, BT_INTEGER, di, REQUIRED);

  make_generic ("bit_size", GFC_ISYM_BIT_SIZE, GFC_STD_F95);

  add_sym_2 ("ble", GFC_ISYM_BLE, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_LOGICAL, dl, GFC_STD_F2008,
	     gfc_check_bge_bgt_ble_blt, gfc_simplify_ble, NULL,
	     i, BT_INTEGER, di, REQUIRED, j, BT_INTEGER, di, REQUIRED);

  make_generic ("ble", GFC_ISYM_BLE, GFC_STD_F2008);

  add_sym_2 ("blt", GFC_ISYM_BLT, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_LOGICAL, dl, GFC_STD_F2008,
	     gfc_check_bge_bgt_ble_blt, gfc_simplify_blt, NULL,
	     i, BT_INTEGER, di, REQUIRED, j, BT_INTEGER, di, REQUIRED);

  make_generic ("blt", GFC_ISYM_BLT, GFC_STD_F2008);

  add_sym_2 ("btest", GFC_ISYM_BTEST, CLASS_ELEMENTAL, ACTUAL_NO, BT_LOGICAL, dl, GFC_STD_F95,
	     gfc_check_bitfcn, gfc_simplify_btest, gfc_resolve_btest,
	     i, BT_INTEGER, di, REQUIRED, pos, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bbtest", GFC_STD_GNU);
      make_alias ("bitest", GFC_STD_GNU);
      make_alias ("bjtest", GFC_STD_GNU);
      make_alias ("bktest", GFC_STD_GNU);
    }

  make_generic ("btest", GFC_ISYM_BTEST, GFC_STD_F95);

  add_sym_2 ("ceiling", GFC_ISYM_CEILING, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_a_ikind, gfc_simplify_ceiling, gfc_resolve_ceiling,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("ceiling", GFC_ISYM_CEILING, GFC_STD_F95);

  add_sym_2 ("char", GFC_ISYM_CHAR, CLASS_ELEMENTAL, ACTUAL_NO, BT_CHARACTER, dc, GFC_STD_F77,
	     gfc_check_char, gfc_simplify_char, gfc_resolve_char,
	     i, BT_INTEGER, di, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("char", GFC_ISYM_CHAR, GFC_STD_F77);

  add_sym_1 ("chdir", GFC_ISYM_CHDIR, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_GNU, gfc_check_chdir, NULL, gfc_resolve_chdir,
	     nm, BT_CHARACTER, dc, REQUIRED);

  make_generic ("chdir", GFC_ISYM_CHDIR, GFC_STD_GNU);

  add_sym_2 ("chmod", GFC_ISYM_CHMOD, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, gfc_check_chmod, NULL, gfc_resolve_chmod,
	     nm, BT_CHARACTER, dc, REQUIRED, md, BT_CHARACTER, dc, REQUIRED);

  make_generic ("chmod", GFC_ISYM_CHMOD, GFC_STD_GNU);

  add_sym_3 ("cmplx", GFC_ISYM_CMPLX, CLASS_ELEMENTAL, ACTUAL_NO, BT_COMPLEX, dz, GFC_STD_F77,
	     gfc_check_cmplx, gfc_simplify_cmplx, gfc_resolve_cmplx,
	     x, BT_UNKNOWN, dr, REQUIRED, y, BT_UNKNOWN, dr, OPTIONAL,
	     kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("cmplx", GFC_ISYM_CMPLX, GFC_STD_F77);

  add_sym_0 ("command_argument_count", GFC_ISYM_COMMAND_ARGUMENT_COUNT, CLASS_INQUIRY,
	     ACTUAL_NO, BT_INTEGER, di, GFC_STD_F2003, NULL, NULL, NULL);

  make_generic ("command_argument_count", GFC_ISYM_COMMAND_ARGUMENT_COUNT,
		GFC_STD_F2003);

  add_sym_2 ("complex", GFC_ISYM_COMPLEX, CLASS_ELEMENTAL, ACTUAL_NO, BT_COMPLEX, dz, GFC_STD_GNU,
	     gfc_check_complex, gfc_simplify_complex, gfc_resolve_complex,
	     x, BT_UNKNOWN, dr, REQUIRED, y, BT_UNKNOWN, dr, REQUIRED);

  make_generic ("complex", GFC_ISYM_COMPLEX, GFC_STD_GNU);

  /* Making dcmplx a specific of cmplx causes cmplx to return a double
     complex instead of the default complex.  */

  add_sym_2 ("dcmplx", GFC_ISYM_CMPLX, CLASS_ELEMENTAL, ACTUAL_NO, BT_COMPLEX, dd, GFC_STD_GNU,
	     gfc_check_dcmplx, gfc_simplify_dcmplx, gfc_resolve_dcmplx,
	     x, BT_REAL, dd, REQUIRED, y, BT_REAL, dd, OPTIONAL);

  make_generic ("dcmplx", GFC_ISYM_CMPLX, GFC_STD_GNU);

  add_sym_1 ("conjg", GFC_ISYM_CONJG, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dz, GFC_STD_F77,
	     gfc_check_fn_c, gfc_simplify_conjg, gfc_resolve_conjg,
	     z, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("dconjg", GFC_ISYM_CONJG, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_conjg, gfc_resolve_conjg,
	     z, BT_COMPLEX, dd, REQUIRED);

  make_generic ("conjg", GFC_ISYM_CONJG, GFC_STD_F77);

  add_sym_1 ("cos", GFC_ISYM_COS, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_cos, gfc_resolve_cos,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dcos", GFC_ISYM_COS, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_cos, gfc_resolve_cos,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("ccos", GFC_ISYM_COS, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dz, GFC_STD_F77,
	     NULL, gfc_simplify_cos, gfc_resolve_cos,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zcos", GFC_ISYM_COS, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_cos, gfc_resolve_cos,
	     x, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdcos", GFC_STD_GNU);

  make_generic ("cos", GFC_ISYM_COS, GFC_STD_F77);

  add_sym_1 ("cosh", GFC_ISYM_COSH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc2008, gfc_simplify_cosh, gfc_resolve_cosh,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dcosh", GFC_ISYM_COSH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_cosh, gfc_resolve_cosh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("cosh", GFC_ISYM_COSH, GFC_STD_F77);

  add_sym_3 ("count", GFC_ISYM_COUNT, CLASS_TRANSFORMATIONAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_count, gfc_simplify_count, gfc_resolve_count,
	     msk, BT_LOGICAL, dl, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
	     kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("count", GFC_ISYM_COUNT, GFC_STD_F95);

  add_sym_3 ("cshift", GFC_ISYM_CSHIFT, CLASS_TRANSFORMATIONAL, ACTUAL_NO,
	     BT_REAL, dr, GFC_STD_F95,
	     gfc_check_cshift, gfc_simplify_cshift, gfc_resolve_cshift,
	     ar, BT_REAL, dr, REQUIRED,
	     sh, BT_INTEGER, di, REQUIRED,
	     dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("cshift", GFC_ISYM_CSHIFT, GFC_STD_F95);

  add_sym_1 ("ctime", GFC_ISYM_CTIME, CLASS_IMPURE, ACTUAL_NO, BT_CHARACTER,
	     0, GFC_STD_GNU, gfc_check_ctime, NULL, gfc_resolve_ctime,
	     tm, BT_INTEGER, di, REQUIRED);

  make_generic ("ctime", GFC_ISYM_CTIME, GFC_STD_GNU);

  add_sym_1 ("dble", GFC_ISYM_DBLE, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_dble, gfc_simplify_dble, gfc_resolve_dble,
	     a, BT_REAL, dr, REQUIRED);

  make_generic ("dble", GFC_ISYM_DBLE, GFC_STD_F77);

  add_sym_1 ("digits", GFC_ISYM_DIGITS, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_digits, gfc_simplify_digits, NULL,
	     x, BT_UNKNOWN, dr, REQUIRED);

  make_generic ("digits", GFC_ISYM_DIGITS, GFC_STD_F95);

  add_sym_2 ("dim", GFC_ISYM_DIM, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_a_p, gfc_simplify_dim, gfc_resolve_dim,
	     x, BT_REAL, dr, REQUIRED, y, BT_REAL, dr, REQUIRED);

  add_sym_2 ("idim", GFC_ISYM_DIM, CLASS_ELEMENTAL, ACTUAL_YES, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_dim, gfc_resolve_dim,
	     x, BT_INTEGER, di, REQUIRED, y, BT_INTEGER, di, REQUIRED);

  add_sym_2 ("ddim", GFC_ISYM_DIM, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_x_yd, gfc_simplify_dim, gfc_resolve_dim,
	     x, BT_REAL, dd, REQUIRED, y, BT_REAL, dd, REQUIRED);

  make_generic ("dim", GFC_ISYM_DIM, GFC_STD_F77);

  add_sym_2 ("dot_product", GFC_ISYM_DOT_PRODUCT, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr,
	     GFC_STD_F95, gfc_check_dot_product, gfc_simplify_dot_product, gfc_resolve_dot_product,
	     va, BT_REAL, dr, REQUIRED, vb, BT_REAL, dr, REQUIRED);

  make_generic ("dot_product", GFC_ISYM_DOT_PRODUCT, GFC_STD_F95);

  add_sym_2 ("dprod", GFC_ISYM_DPROD,CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_dprod, gfc_simplify_dprod, gfc_resolve_dprod,
	     x, BT_REAL, dr, REQUIRED, y, BT_REAL, dr, REQUIRED);

  make_generic ("dprod", GFC_ISYM_DPROD, GFC_STD_F77);

  add_sym_1 ("dreal", GFC_ISYM_REAL, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_REAL, dd, GFC_STD_GNU, NULL, gfc_simplify_dreal, NULL,
	     a, BT_COMPLEX, dd, REQUIRED);

  make_generic ("dreal", GFC_ISYM_REAL, GFC_STD_GNU);

  add_sym_3 ("dshiftl", GFC_ISYM_DSHIFTL, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_dshift, gfc_simplify_dshiftl, gfc_resolve_dshift,
	     i, BT_INTEGER, di, REQUIRED,
	     j, BT_INTEGER, di, REQUIRED,
	     sh, BT_INTEGER, di, REQUIRED);

  make_generic ("dshiftl", GFC_ISYM_DSHIFTL, GFC_STD_F2008);

  add_sym_3 ("dshiftr", GFC_ISYM_DSHIFTR, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_dshift, gfc_simplify_dshiftr, gfc_resolve_dshift,
	     i, BT_INTEGER, di, REQUIRED,
	     j, BT_INTEGER, di, REQUIRED,
	     sh, BT_INTEGER, di, REQUIRED);

  make_generic ("dshiftr", GFC_ISYM_DSHIFTR, GFC_STD_F2008);

  add_sym_4 ("eoshift", GFC_ISYM_EOSHIFT, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_eoshift, gfc_simplify_eoshift, gfc_resolve_eoshift,
	     ar, BT_REAL, dr, REQUIRED, sh, BT_INTEGER, ii, REQUIRED,
	     bd, BT_REAL, dr, OPTIONAL, dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("eoshift", GFC_ISYM_EOSHIFT, GFC_STD_F95);

  add_sym_1 ("epsilon", GFC_ISYM_EPSILON, CLASS_INQUIRY, ACTUAL_NO, BT_REAL, dr,
	     GFC_STD_F95, gfc_check_fn_r, gfc_simplify_epsilon, NULL,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("epsilon", GFC_ISYM_EPSILON, GFC_STD_F95);

  /* G77 compatibility for the ERF() and ERFC() functions.  */
  add_sym_1 ("erf", GFC_ISYM_ERF, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr,
	     GFC_STD_F2008, gfc_check_fn_r, gfc_simplify_erf,
	     gfc_resolve_g77_math1, x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("derf", GFC_ISYM_ERF, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd,
	     GFC_STD_GNU, gfc_check_fn_d, gfc_simplify_erf,
	     gfc_resolve_g77_math1, x, BT_REAL, dd, REQUIRED);

  make_generic ("erf", GFC_ISYM_ERF, GFC_STD_F2008);

  add_sym_1 ("erfc", GFC_ISYM_ERFC, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr,
	     GFC_STD_F2008, gfc_check_fn_r, gfc_simplify_erfc,
	     gfc_resolve_g77_math1, x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("derfc", GFC_ISYM_ERFC, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd,
	     GFC_STD_GNU, gfc_check_fn_d, gfc_simplify_erfc,
	     gfc_resolve_g77_math1, x, BT_REAL, dd, REQUIRED);

  make_generic ("erfc", GFC_ISYM_ERFC, GFC_STD_F2008);

  add_sym_1 ("erfc_scaled", GFC_ISYM_ERFC_SCALED, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_REAL, dr, GFC_STD_F2008, gfc_check_fn_r,
	     gfc_simplify_erfc_scaled, gfc_resolve_g77_math1, x, BT_REAL,
	     dr, REQUIRED);

  make_generic ("erfc_scaled", GFC_ISYM_ERFC_SCALED, GFC_STD_F2008);

  /* G77 compatibility */
  add_sym_1 ("dtime", GFC_ISYM_DTIME, CLASS_IMPURE, ACTUAL_NO, BT_REAL,
	     4, GFC_STD_GNU, gfc_check_dtime_etime, NULL, NULL,
	     x, BT_REAL, 4, REQUIRED);

  make_generic ("dtime", GFC_ISYM_DTIME, GFC_STD_GNU);

  add_sym_1 ("etime", GFC_ISYM_ETIME, CLASS_IMPURE, ACTUAL_NO, BT_REAL,
	     4, GFC_STD_GNU, gfc_check_dtime_etime, NULL, NULL,
	     x, BT_REAL, 4, REQUIRED);

  make_generic ("etime", GFC_ISYM_ETIME, GFC_STD_GNU);

  add_sym_1 ("exp", GFC_ISYM_EXP, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr,  GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_exp, gfc_resolve_exp,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dexp", GFC_ISYM_EXP, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_exp, gfc_resolve_exp,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("cexp", GFC_ISYM_EXP, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dz, GFC_STD_F77,
	     NULL, gfc_simplify_exp, gfc_resolve_exp,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zexp", GFC_ISYM_EXP, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dd,  GFC_STD_GNU,
	     NULL, gfc_simplify_exp, gfc_resolve_exp,
	     x, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdexp", GFC_STD_GNU);

  make_generic ("exp", GFC_ISYM_EXP, GFC_STD_F77);

  add_sym_1 ("exponent", GFC_ISYM_EXPONENT, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_F95, gfc_check_fn_r, gfc_simplify_exponent, gfc_resolve_exponent,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("exponent", GFC_ISYM_EXPONENT, GFC_STD_F95);

  add_sym_2 ("extends_type_of", GFC_ISYM_EXTENDS_TYPE_OF, CLASS_INQUIRY,
	     ACTUAL_NO, BT_LOGICAL, dl, GFC_STD_F2003,
	     gfc_check_same_type_as, gfc_simplify_extends_type_of,
	     gfc_resolve_extends_type_of,
	     a, BT_UNKNOWN, 0, REQUIRED,
	     mo, BT_UNKNOWN, 0, REQUIRED);

  add_sym_2 ("failed_images", GFC_ISYM_FAILED_IMAGES, CLASS_TRANSFORMATIONAL,
	     ACTUAL_NO, BT_INTEGER, dd, GFC_STD_F2018,
	     gfc_check_failed_or_stopped_images,
	     gfc_simplify_failed_or_stopped_images,
	     gfc_resolve_failed_images, team, BT_VOID, di, OPTIONAL,
	     kind, BT_INTEGER, di, OPTIONAL);

  add_sym_0 ("fdate",  GFC_ISYM_FDATE, CLASS_IMPURE, ACTUAL_NO, BT_CHARACTER,
	     dc, GFC_STD_GNU, NULL, NULL, gfc_resolve_fdate);

  make_generic ("fdate", GFC_ISYM_FDATE, GFC_STD_GNU);

  add_sym_2 ("floor", GFC_ISYM_FLOOR, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_a_ikind, gfc_simplify_floor, gfc_resolve_floor,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("floor", GFC_ISYM_FLOOR, GFC_STD_F95);

  /* G77 compatible fnum */
  add_sym_1 ("fnum", GFC_ISYM_FNUM, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, gfc_check_fnum, NULL, gfc_resolve_fnum,
	     ut, BT_INTEGER, di, REQUIRED);

  make_generic ("fnum", GFC_ISYM_FNUM, GFC_STD_GNU);

  add_sym_1 ("fraction", GFC_ISYM_FRACTION, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr,
	     GFC_STD_F95, gfc_check_fn_r, gfc_simplify_fraction, gfc_resolve_fraction,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("fraction", GFC_ISYM_FRACTION, GFC_STD_F95);

  add_sym_2_intent ("fstat", GFC_ISYM_FSTAT, CLASS_IMPURE, ACTUAL_NO,
		    BT_INTEGER, di, GFC_STD_GNU,
		    gfc_check_fstat, NULL, gfc_resolve_fstat,
		    ut, BT_INTEGER, di, REQUIRED, INTENT_IN,
		    vl, BT_INTEGER, di, REQUIRED, INTENT_OUT);

  make_generic ("fstat", GFC_ISYM_FSTAT, GFC_STD_GNU);

  add_sym_1 ("ftell", GFC_ISYM_FTELL, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     ii, GFC_STD_GNU, gfc_check_ftell, NULL, gfc_resolve_ftell,
	     ut, BT_INTEGER, di, REQUIRED);

  make_generic ("ftell", GFC_ISYM_FTELL, GFC_STD_GNU);

  add_sym_2_intent ("fgetc", GFC_ISYM_FGETC, CLASS_IMPURE, ACTUAL_NO,
		    BT_INTEGER, di, GFC_STD_GNU,
		    gfc_check_fgetputc, NULL, gfc_resolve_fgetc,
		    ut, BT_INTEGER, di, REQUIRED, INTENT_IN,
		    c, BT_CHARACTER, dc, REQUIRED, INTENT_OUT);

  make_generic ("fgetc", GFC_ISYM_FGETC, GFC_STD_GNU);

  add_sym_1_intent ("fget", GFC_ISYM_FGET, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, gfc_check_fgetput, NULL, gfc_resolve_fget,
	     c, BT_CHARACTER, dc, REQUIRED, INTENT_OUT);

  make_generic ("fget", GFC_ISYM_FGET, GFC_STD_GNU);

  add_sym_2 ("fputc", GFC_ISYM_FPUTC, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, gfc_check_fgetputc, NULL, gfc_resolve_fputc,
	     ut, BT_INTEGER, di, REQUIRED, c, BT_CHARACTER, dc, REQUIRED);

  make_generic ("fputc", GFC_ISYM_FPUTC, GFC_STD_GNU);

  add_sym_1 ("fput", GFC_ISYM_FPUT, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, gfc_check_fgetput, NULL, gfc_resolve_fput,
	     c, BT_CHARACTER, dc, REQUIRED);

  make_generic ("fput", GFC_ISYM_FPUT, GFC_STD_GNU);

  add_sym_1 ("gamma", GFC_ISYM_TGAMMA, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr,
	     GFC_STD_F2008, gfc_check_fn_r, gfc_simplify_gamma,
	     gfc_resolve_gamma, x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dgamma", GFC_ISYM_TGAMMA, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_gamma, gfc_resolve_gamma,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("gamma", GFC_ISYM_TGAMMA, GFC_STD_F2008);

  /* Unix IDs (g77 compatibility)  */
  add_sym_1 ("getcwd", GFC_ISYM_GETCWD, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di,  GFC_STD_GNU, NULL, NULL, gfc_resolve_getcwd,
	     c, BT_CHARACTER, dc, REQUIRED);

  make_generic ("getcwd", GFC_ISYM_GETCWD, GFC_STD_GNU);

  add_sym_0 ("getgid", GFC_ISYM_GETGID, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, NULL, NULL, gfc_resolve_getgid);

  make_generic ("getgid", GFC_ISYM_GETGID, GFC_STD_GNU);

  add_sym_0 ("getpid", GFC_ISYM_GETPID, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, NULL, NULL, gfc_resolve_getpid);

  make_generic ("getpid", GFC_ISYM_GETPID, GFC_STD_GNU);

  add_sym_1 ("get_team", GFC_ISYM_GET_TEAM, CLASS_TRANSFORMATIONAL,
	     ACTUAL_NO, BT_INTEGER, di, GFC_STD_F2018,
	     gfc_check_get_team, NULL, gfc_resolve_get_team,
	     level, BT_INTEGER, di, OPTIONAL);

  add_sym_0 ("getuid", GFC_ISYM_GETUID, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, NULL, NULL, gfc_resolve_getuid);

  make_generic ("getuid", GFC_ISYM_GETUID, GFC_STD_GNU);

  add_sym_1_intent ("hostnm", GFC_ISYM_HOSTNM, CLASS_IMPURE, ACTUAL_NO,
		    BT_INTEGER, di, GFC_STD_GNU,
		    gfc_check_hostnm, NULL, gfc_resolve_hostnm,
		    c, BT_CHARACTER, dc, REQUIRED, INTENT_OUT);

  make_generic ("hostnm", GFC_ISYM_HOSTNM, GFC_STD_GNU);

  add_sym_1 ("huge", GFC_ISYM_HUGE, CLASS_INQUIRY, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_huge, gfc_simplify_huge, NULL,
	     x, BT_UNKNOWN, dr, REQUIRED);

  make_generic ("huge", GFC_ISYM_HUGE, GFC_STD_F95);

  add_sym_2 ("hypot", GFC_ISYM_HYPOT, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_REAL, dr, GFC_STD_F2008,
	     gfc_check_hypot, gfc_simplify_hypot, gfc_resolve_hypot,
	     x, BT_REAL, dr, REQUIRED, y, BT_REAL, dr, REQUIRED);

  make_generic ("hypot", GFC_ISYM_HYPOT, GFC_STD_F2008);

  add_sym_2 ("iachar", GFC_ISYM_IACHAR, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ichar_iachar, gfc_simplify_iachar, gfc_resolve_iachar,
	     c, BT_CHARACTER, dc, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("iachar", GFC_ISYM_IACHAR, GFC_STD_F95);

  add_sym_2 ("iand", GFC_ISYM_IAND, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_F95,
	     gfc_check_iand_ieor_ior, gfc_simplify_iand, gfc_resolve_iand,
	     i, BT_INTEGER, di, REQUIRED, j, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("biand", GFC_STD_GNU);
      make_alias ("iiand", GFC_STD_GNU);
      make_alias ("jiand", GFC_STD_GNU);
      make_alias ("kiand", GFC_STD_GNU);
    }

  make_generic ("iand", GFC_ISYM_IAND, GFC_STD_F95);

  add_sym_2 ("and", GFC_ISYM_AND, CLASS_IMPURE, ACTUAL_NO, BT_LOGICAL,
	     dl, GFC_STD_GNU, gfc_check_and, gfc_simplify_and, gfc_resolve_and,
	     i, BT_UNKNOWN, 0, REQUIRED, j, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("and", GFC_ISYM_AND, GFC_STD_GNU);

  add_sym_3red ("iall", GFC_ISYM_IALL, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F2008,
		gfc_check_transf_bit_intrins, gfc_simplify_iall, gfc_resolve_iall,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("iall", GFC_ISYM_IALL, GFC_STD_F2008);

  add_sym_3red ("iany", GFC_ISYM_IANY, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F2008,
		gfc_check_transf_bit_intrins, gfc_simplify_iany, gfc_resolve_iany,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("iany", GFC_ISYM_IANY, GFC_STD_F2008);

  add_sym_0 ("iargc", GFC_ISYM_IARGC, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, NULL, NULL, NULL);

  make_generic ("iargc", GFC_ISYM_IARGC, GFC_STD_GNU);

  add_sym_2 ("ibclr", GFC_ISYM_IBCLR, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_bitfcn, gfc_simplify_ibclr, gfc_resolve_ibclr,
	     i, BT_INTEGER, di, REQUIRED, pos, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bbclr", GFC_STD_GNU);
      make_alias ("iibclr", GFC_STD_GNU);
      make_alias ("jibclr", GFC_STD_GNU);
      make_alias ("kibclr", GFC_STD_GNU);
    }

  make_generic ("ibclr", GFC_ISYM_IBCLR, GFC_STD_F95);

  add_sym_3 ("ibits", GFC_ISYM_IBITS, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ibits, gfc_simplify_ibits, gfc_resolve_ibits,
	     i, BT_INTEGER, di, REQUIRED, pos, BT_INTEGER, di, REQUIRED,
	     ln, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bbits", GFC_STD_GNU);
      make_alias ("iibits", GFC_STD_GNU);
      make_alias ("jibits", GFC_STD_GNU);
      make_alias ("kibits", GFC_STD_GNU);
    }

  make_generic ("ibits", GFC_ISYM_IBITS, GFC_STD_F95);

  add_sym_2 ("ibset", GFC_ISYM_IBSET, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_bitfcn, gfc_simplify_ibset, gfc_resolve_ibset,
	     i, BT_INTEGER, di, REQUIRED, pos, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bbset", GFC_STD_GNU);
      make_alias ("iibset", GFC_STD_GNU);
      make_alias ("jibset", GFC_STD_GNU);
      make_alias ("kibset", GFC_STD_GNU);
    }

  make_generic ("ibset", GFC_ISYM_IBSET, GFC_STD_F95);

  add_sym_2 ("ichar", GFC_ISYM_ICHAR, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_ichar_iachar, gfc_simplify_ichar, gfc_resolve_ichar,
	     c, BT_CHARACTER, dc, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("ichar", GFC_ISYM_ICHAR, GFC_STD_F77);

  add_sym_2 ("ieor", GFC_ISYM_IEOR, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_F95,
	     gfc_check_iand_ieor_ior, gfc_simplify_ieor, gfc_resolve_ieor,
	     i, BT_INTEGER, di, REQUIRED, j, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bieor", GFC_STD_GNU);
      make_alias ("iieor", GFC_STD_GNU);
      make_alias ("jieor", GFC_STD_GNU);
      make_alias ("kieor", GFC_STD_GNU);
    }

  make_generic ("ieor", GFC_ISYM_IEOR, GFC_STD_F95);

  add_sym_2 ("xor", GFC_ISYM_XOR, CLASS_IMPURE, ACTUAL_NO, BT_LOGICAL,
	     dl, GFC_STD_GNU, gfc_check_and, gfc_simplify_xor, gfc_resolve_xor,
	     i, BT_UNKNOWN, 0, REQUIRED, j, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("xor", GFC_ISYM_XOR, GFC_STD_GNU);

  add_sym_0 ("ierrno", GFC_ISYM_IERRNO, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, NULL, NULL, gfc_resolve_ierrno);

  make_generic ("ierrno", GFC_ISYM_IERRNO, GFC_STD_GNU);

  add_sym_2 ("image_index", GFC_ISYM_IMAGE_INDEX, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_image_index, gfc_simplify_image_index, gfc_resolve_image_index,
	     ca, BT_REAL, dr, REQUIRED, sub, BT_INTEGER, ii, REQUIRED);

  add_sym_2 ("image_status", GFC_ISYM_IMAGE_STATUS, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2018, gfc_check_image_status,
	     gfc_simplify_image_status, gfc_resolve_image_status, image,
	     BT_INTEGER, di, REQUIRED, team, BT_VOID, di, OPTIONAL);

  /* The resolution function for INDEX is called gfc_resolve_index_func
     because the name gfc_resolve_index is already used in resolve.c.  */
  add_sym_4ind ("index", GFC_ISYM_INDEX, CLASS_ELEMENTAL, ACTUAL_YES,
		BT_INTEGER, di, GFC_STD_F77,
		gfc_check_index, gfc_simplify_index, gfc_resolve_index_func,
		stg, BT_CHARACTER, dc, REQUIRED, ssg, BT_CHARACTER, dc, REQUIRED,
		bck, BT_LOGICAL, dl, OPTIONAL, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("index", GFC_ISYM_INDEX, GFC_STD_F77);

  add_sym_2 ("int", GFC_ISYM_INT, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_int, gfc_simplify_int, gfc_resolve_int,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  add_sym_1 ("ifix", GFC_ISYM_INT, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_ifix, NULL,
	     a, BT_REAL, dr, REQUIRED);

  add_sym_1 ("idint", GFC_ISYM_INT, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_idint, NULL,
	     a, BT_REAL, dd, REQUIRED);

  make_generic ("int", GFC_ISYM_INT, GFC_STD_F77);

  add_sym_1 ("int2", GFC_ISYM_INT2, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_intconv, gfc_simplify_int2, gfc_resolve_int2,
	     a, BT_REAL, dr, REQUIRED);

  make_alias ("short", GFC_STD_GNU);

  make_generic ("int2", GFC_ISYM_INT2, GFC_STD_GNU);

  add_sym_1 ("int8", GFC_ISYM_INT8, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_intconv, gfc_simplify_int8, gfc_resolve_int8,
	     a, BT_REAL, dr, REQUIRED);

  make_generic ("int8", GFC_ISYM_INT8, GFC_STD_GNU);

  add_sym_1 ("long", GFC_ISYM_LONG, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_intconv, gfc_simplify_long, gfc_resolve_long,
	     a, BT_REAL, dr, REQUIRED);

  make_generic ("long", GFC_ISYM_LONG, GFC_STD_GNU);

  add_sym_2 ("ior", GFC_ISYM_IOR, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_F95,
	     gfc_check_iand_ieor_ior, gfc_simplify_ior, gfc_resolve_ior,
	     i, BT_INTEGER, di, REQUIRED, j, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bior", GFC_STD_GNU);
      make_alias ("iior", GFC_STD_GNU);
      make_alias ("jior", GFC_STD_GNU);
      make_alias ("kior", GFC_STD_GNU);
    }

  make_generic ("ior", GFC_ISYM_IOR, GFC_STD_F95);

  add_sym_2 ("or", GFC_ISYM_OR, CLASS_IMPURE, ACTUAL_NO, BT_LOGICAL,
	     dl, GFC_STD_GNU, gfc_check_and, gfc_simplify_or, gfc_resolve_or,
	     i, BT_UNKNOWN, 0, REQUIRED, j, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("or", GFC_ISYM_OR, GFC_STD_GNU);

  add_sym_3red ("iparity", GFC_ISYM_IPARITY, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F2008,
		gfc_check_transf_bit_intrins, gfc_simplify_iparity, gfc_resolve_iparity,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("iparity", GFC_ISYM_IPARITY, GFC_STD_F2008);

  /* The following function is for G77 compatibility.  */
  add_sym_1 ("irand", GFC_ISYM_IRAND, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     4, GFC_STD_GNU, gfc_check_irand, NULL, NULL,
	     i, BT_INTEGER, 4, OPTIONAL);

  make_generic ("irand", GFC_ISYM_IRAND, GFC_STD_GNU);

  add_sym_1 ("isatty", GFC_ISYM_ISATTY, CLASS_IMPURE, ACTUAL_NO, BT_LOGICAL,
	     dl, GFC_STD_GNU, gfc_check_isatty, NULL, gfc_resolve_isatty,
	     ut, BT_INTEGER, di, REQUIRED);

  make_generic ("isatty", GFC_ISYM_ISATTY, GFC_STD_GNU);

  add_sym_1 ("is_contiguous", GFC_ISYM_IS_CONTIGUOUS, CLASS_INQUIRY, ACTUAL_NO,
	     BT_LOGICAL, dl, GFC_STD_F2008,
	     gfc_check_is_contiguous, gfc_simplify_is_contiguous,
	     gfc_resolve_is_contiguous,
	     ar, BT_REAL, dr, REQUIRED);

  make_generic ("is_contiguous", GFC_ISYM_IS_CONTIGUOUS, GFC_STD_F2008);

  add_sym_1 ("is_iostat_end", GFC_ISYM_IS_IOSTAT_END,
	     CLASS_ELEMENTAL, ACTUAL_NO, BT_LOGICAL, dl, GFC_STD_F2003,
	     gfc_check_i, gfc_simplify_is_iostat_end, NULL,
	     i, BT_INTEGER, 0, REQUIRED);

  make_generic ("is_iostat_end", GFC_ISYM_IS_IOSTAT_END, GFC_STD_F2003);

  add_sym_1 ("is_iostat_eor", GFC_ISYM_IS_IOSTAT_EOR,
	     CLASS_ELEMENTAL, ACTUAL_NO, BT_LOGICAL, dl, GFC_STD_F2003,
	     gfc_check_i, gfc_simplify_is_iostat_eor, NULL,
	     i, BT_INTEGER, 0, REQUIRED);

  make_generic ("is_iostat_eor", GFC_ISYM_IS_IOSTAT_EOR, GFC_STD_F2003);

  add_sym_1 ("isnan", GFC_ISYM_ISNAN, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_LOGICAL, dl, GFC_STD_GNU,
	     gfc_check_isnan, gfc_simplify_isnan, NULL,
	     x, BT_REAL, 0, REQUIRED);

  make_generic ("isnan", GFC_ISYM_ISNAN, GFC_STD_GNU);

  add_sym_2 ("rshift", GFC_ISYM_RSHIFT, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_ishft, gfc_simplify_rshift, gfc_resolve_rshift,
	     i, BT_INTEGER, di, REQUIRED, sh, BT_INTEGER, di, REQUIRED);

  make_generic ("rshift", GFC_ISYM_RSHIFT, GFC_STD_GNU);

  add_sym_2 ("lshift", GFC_ISYM_LSHIFT, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_GNU,
	     gfc_check_ishft, gfc_simplify_lshift, gfc_resolve_lshift,
	     i, BT_INTEGER, di, REQUIRED, sh, BT_INTEGER, di, REQUIRED);

  make_generic ("lshift", GFC_ISYM_LSHIFT, GFC_STD_GNU);

  add_sym_2 ("ishft", GFC_ISYM_ISHFT, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ishft, gfc_simplify_ishft, gfc_resolve_ishft,
	     i, BT_INTEGER, di, REQUIRED, sh, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bshft", GFC_STD_GNU);
      make_alias ("iishft", GFC_STD_GNU);
      make_alias ("jishft", GFC_STD_GNU);
      make_alias ("kishft", GFC_STD_GNU);
    }

  make_generic ("ishft", GFC_ISYM_ISHFT, GFC_STD_F95);

  add_sym_3 ("ishftc", GFC_ISYM_ISHFTC, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ishftc, gfc_simplify_ishftc, gfc_resolve_ishftc,
	     i, BT_INTEGER, di, REQUIRED, sh, BT_INTEGER, di, REQUIRED,
	     sz, BT_INTEGER, di, OPTIONAL);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bshftc", GFC_STD_GNU);
      make_alias ("iishftc", GFC_STD_GNU);
      make_alias ("jishftc", GFC_STD_GNU);
      make_alias ("kishftc", GFC_STD_GNU);
    }

  make_generic ("ishftc", GFC_ISYM_ISHFTC, GFC_STD_F95);

  add_sym_2 ("kill", GFC_ISYM_KILL, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, gfc_check_kill, NULL, NULL,
	     pid, BT_INTEGER, di, REQUIRED, sig, BT_INTEGER, di, REQUIRED);

  make_generic ("kill", GFC_ISYM_KILL, GFC_STD_GNU);

  add_sym_1 ("kind", GFC_ISYM_KIND, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_kind, gfc_simplify_kind, NULL,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("kind", GFC_ISYM_KIND, GFC_STD_F95);

  add_sym_3 ("lbound", GFC_ISYM_LBOUND, CLASS_INQUIRY, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_lbound, gfc_simplify_lbound, gfc_resolve_lbound,
	     ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, di, OPTIONAL,
	     kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("lbound", GFC_ISYM_LBOUND, GFC_STD_F95);

  add_sym_3 ("lcobound", GFC_ISYM_LCOBOUND, CLASS_INQUIRY, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_lcobound, gfc_simplify_lcobound, gfc_resolve_lcobound,
	     ca, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
	     kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("lcobound", GFC_ISYM_LCOBOUND, GFC_STD_F2008);

  add_sym_1 ("leadz", GFC_ISYM_LEADZ, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_i, gfc_simplify_leadz, NULL,
	     i, BT_INTEGER, di, REQUIRED);

  make_generic ("leadz", GFC_ISYM_LEADZ, GFC_STD_F2008);

  add_sym_2 ("len", GFC_ISYM_LEN, CLASS_INQUIRY, ACTUAL_YES,
	     BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_len_lentrim, gfc_simplify_len, gfc_resolve_len,
	     stg, BT_CHARACTER, dc, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("len", GFC_ISYM_LEN, GFC_STD_F77);

  add_sym_2 ("len_trim", GFC_ISYM_LEN_TRIM, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_len_lentrim, gfc_simplify_len_trim, gfc_resolve_len_trim,
	     stg, BT_CHARACTER, dc, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_alias ("lnblnk", GFC_STD_GNU);

  make_generic ("len_trim", GFC_ISYM_LEN_TRIM, GFC_STD_F95);

  add_sym_1 ("lgamma", GFC_ISYM_LGAMMA, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL,
	     dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_lgamma, gfc_resolve_lgamma,
	     x, BT_REAL, dr, REQUIRED);

  make_alias ("log_gamma", GFC_STD_F2008);

  add_sym_1 ("algama", GFC_ISYM_LGAMMA, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_lgamma, gfc_resolve_lgamma,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dlgama", GFC_ISYM_LGAMMA, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_lgamma, gfc_resolve_lgamma,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("log_gamma", GFC_ISYM_LGAMMA, GFC_STD_F2008);


  add_sym_2 ("lge", GFC_ISYM_LGE, CLASS_ELEMENTAL, ACTUAL_NO, BT_LOGICAL, dl,
	     GFC_STD_F77, gfc_check_lge_lgt_lle_llt, gfc_simplify_lge, NULL,
	     sta, BT_CHARACTER, dc, REQUIRED, stb, BT_CHARACTER, dc, REQUIRED);

  make_generic ("lge", GFC_ISYM_LGE, GFC_STD_F77);

  add_sym_2 ("lgt", GFC_ISYM_LGT, CLASS_ELEMENTAL, ACTUAL_NO, BT_LOGICAL, dl,
	     GFC_STD_F77, gfc_check_lge_lgt_lle_llt, gfc_simplify_lgt, NULL,
	     sta, BT_CHARACTER, dc, REQUIRED, stb, BT_CHARACTER, dc, REQUIRED);

  make_generic ("lgt", GFC_ISYM_LGT, GFC_STD_F77);

  add_sym_2 ("lle",GFC_ISYM_LLE,  CLASS_ELEMENTAL, ACTUAL_NO, BT_LOGICAL, dl,
	     GFC_STD_F77, gfc_check_lge_lgt_lle_llt, gfc_simplify_lle, NULL,
	     sta, BT_CHARACTER, dc, REQUIRED, stb, BT_CHARACTER, dc, REQUIRED);

  make_generic ("lle", GFC_ISYM_LLE, GFC_STD_F77);

  add_sym_2 ("llt", GFC_ISYM_LLT, CLASS_ELEMENTAL, ACTUAL_NO, BT_LOGICAL, dl,
	     GFC_STD_F77, gfc_check_lge_lgt_lle_llt, gfc_simplify_llt, NULL,
	     sta, BT_CHARACTER, dc, REQUIRED, stb, BT_CHARACTER, dc, REQUIRED);

  make_generic ("llt", GFC_ISYM_LLT, GFC_STD_F77);

  add_sym_2 ("link", GFC_ISYM_LINK, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_GNU, gfc_check_link, NULL, gfc_resolve_link,
	     p1, BT_CHARACTER, dc, REQUIRED, p2, BT_CHARACTER, dc, REQUIRED);

  make_generic ("link", GFC_ISYM_LINK, GFC_STD_GNU);

  add_sym_1 ("log", GFC_ISYM_LOG, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_log, gfc_resolve_log,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("alog", GFC_ISYM_LOG, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     NULL, gfc_simplify_log, gfc_resolve_log,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dlog", GFC_ISYM_LOG, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_log, gfc_resolve_log,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("clog", GFC_ISYM_LOG, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dz, GFC_STD_F77,
	     NULL, gfc_simplify_log, gfc_resolve_log,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zlog", GFC_ISYM_LOG, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dd,  GFC_STD_GNU,
	     NULL, gfc_simplify_log, gfc_resolve_log,
	     x, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdlog", GFC_STD_GNU);

  make_generic ("log", GFC_ISYM_LOG, GFC_STD_F77);

  add_sym_1 ("log10", GFC_ISYM_LOG10, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_r, gfc_simplify_log10, gfc_resolve_log10,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("alog10", GFC_ISYM_LOG10, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     NULL, gfc_simplify_log10, gfc_resolve_log10,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dlog10", GFC_ISYM_LOG10, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_log10, gfc_resolve_log10,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("log10", GFC_ISYM_LOG10, GFC_STD_F77);

  add_sym_2 ("logical", GFC_ISYM_LOGICAL, CLASS_ELEMENTAL, ACTUAL_NO, BT_LOGICAL, dl, GFC_STD_F95,
	     gfc_check_logical, gfc_simplify_logical, gfc_resolve_logical,
	     l, BT_LOGICAL, dl, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("logical", GFC_ISYM_LOGICAL, GFC_STD_F95);

  add_sym_2_intent ("lstat", GFC_ISYM_LSTAT, CLASS_IMPURE, ACTUAL_NO,
		    BT_INTEGER, di, GFC_STD_GNU,
		    gfc_check_stat, NULL, gfc_resolve_lstat,
		    nm, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
		    vl, BT_INTEGER, di, REQUIRED, INTENT_OUT);

  make_generic ("lstat", GFC_ISYM_LSTAT, GFC_STD_GNU);

  add_sym_1 ("malloc", GFC_ISYM_MALLOC, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER, ii,
	     GFC_STD_GNU, gfc_check_malloc, NULL, NULL,
	     sz, BT_INTEGER, di, REQUIRED);

  make_generic ("malloc", GFC_ISYM_MALLOC, GFC_STD_GNU);

  add_sym_2 ("maskl", GFC_ISYM_MASKL, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_mask, gfc_simplify_maskl, gfc_resolve_mask,
	     i, BT_INTEGER, di, REQUIRED,
	     kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("maskl", GFC_ISYM_MASKL, GFC_STD_F2008);

  add_sym_2 ("maskr", GFC_ISYM_MASKR, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_mask, gfc_simplify_maskr, gfc_resolve_mask,
	     i, BT_INTEGER, di, REQUIRED,
	     kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("maskr", GFC_ISYM_MASKR, GFC_STD_F2008);

  add_sym_2 ("matmul", GFC_ISYM_MATMUL, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_matmul, gfc_simplify_matmul, gfc_resolve_matmul,
	     ma, BT_REAL, dr, REQUIRED, mb, BT_REAL, dr, REQUIRED);

  make_generic ("matmul", GFC_ISYM_MATMUL, GFC_STD_F95);

  /* Note: amax0 is equivalent to real(max), max1 is equivalent to
     int(max).  The max function must take at least two arguments.  */

  add_sym_1m ("max", GFC_ISYM_MAX, CLASS_ELEMENTAL, ACTUAL_NO, BT_UNKNOWN, 0, GFC_STD_F77,
	     gfc_check_min_max, gfc_simplify_max, gfc_resolve_max,
	     a1, BT_UNKNOWN, dr, REQUIRED, a2, BT_UNKNOWN, dr, REQUIRED);

  add_sym_1m ("max0", GFC_ISYM_MAX, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_min_max_integer, gfc_simplify_max, NULL,
	     a1, BT_INTEGER, di, REQUIRED, a2, BT_INTEGER, di, REQUIRED);

  add_sym_1m ("amax0", GFC_ISYM_MAX, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_min_max_integer, gfc_simplify_max, NULL,
	     a1, BT_INTEGER, di, REQUIRED, a2, BT_INTEGER, di, REQUIRED);

  add_sym_1m ("amax1", GFC_ISYM_MAX, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_min_max_real, gfc_simplify_max, NULL,
	     a1, BT_REAL, dr, REQUIRED, a2, BT_REAL, dr, REQUIRED);

  add_sym_1m ("max1", GFC_ISYM_MAX, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_min_max_real, gfc_simplify_max, NULL,
	     a1, BT_REAL, dr, REQUIRED, a2, BT_REAL, dr, REQUIRED);

  add_sym_1m ("dmax1", GFC_ISYM_MAX, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_min_max_double, gfc_simplify_max, NULL,
	     a1, BT_REAL, dd, REQUIRED, a2, BT_REAL, dd, REQUIRED);

  make_generic ("max", GFC_ISYM_MAX, GFC_STD_F77);

  add_sym_1 ("maxexponent", GFC_ISYM_MAXEXPONENT, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_F95, gfc_check_fn_r, gfc_simplify_maxexponent, NULL,
	     x, BT_UNKNOWN, dr, REQUIRED);

  make_generic ("maxexponent", GFC_ISYM_MAXEXPONENT, GFC_STD_F95);

  add_sym_5ml ("maxloc", GFC_ISYM_MAXLOC, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	       gfc_check_minloc_maxloc, gfc_simplify_maxloc, gfc_resolve_maxloc,
	       ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
	       msk, BT_LOGICAL, dl, OPTIONAL, kind, BT_INTEGER, di, OPTIONAL,
	       bck, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("maxloc", GFC_ISYM_MAXLOC, GFC_STD_F95);

  add_sym_6fl ("findloc", GFC_ISYM_FINDLOC, CLASS_TRANSFORMATIONAL, ACTUAL_NO,
	       BT_INTEGER, di, GFC_STD_F2008,
	       gfc_check_findloc, gfc_simplify_findloc, gfc_resolve_findloc,
	       ar, BT_REAL, dr, REQUIRED, val, BT_REAL, dr, REQUIRED,
	       dm, BT_INTEGER, ii, OPTIONAL, msk, BT_LOGICAL, dl, OPTIONAL,
	       kind, BT_INTEGER, di, OPTIONAL, bck, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("findloc", GFC_ISYM_FINDLOC, GFC_STD_F2008);

  add_sym_3red ("maxval", GFC_ISYM_MAXVAL, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
		gfc_check_minval_maxval, gfc_simplify_maxval, gfc_resolve_maxval,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("maxval", GFC_ISYM_MAXVAL, GFC_STD_F95);

  add_sym_0 ("mclock", GFC_ISYM_MCLOCK, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_GNU, NULL, NULL, gfc_resolve_mclock);

  make_generic ("mclock", GFC_ISYM_MCLOCK, GFC_STD_GNU);

  add_sym_0 ("mclock8", GFC_ISYM_MCLOCK8, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, NULL, NULL, gfc_resolve_mclock8);

  make_generic ("mclock8", GFC_ISYM_MCLOCK8, GFC_STD_GNU);

  add_sym_3 ("merge", GFC_ISYM_MERGE, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_merge, gfc_simplify_merge, gfc_resolve_merge,
	     ts, BT_REAL, dr, REQUIRED, fs, BT_REAL, dr, REQUIRED,
	     msk, BT_LOGICAL, dl, REQUIRED);

  make_generic ("merge", GFC_ISYM_MERGE, GFC_STD_F95);

  add_sym_3 ("merge_bits", GFC_ISYM_MERGE_BITS, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_merge_bits, gfc_simplify_merge_bits,
	     gfc_resolve_merge_bits,
	     i, BT_INTEGER, di, REQUIRED,
	     j, BT_INTEGER, di, REQUIRED,
	     msk, BT_INTEGER, di, REQUIRED);

  make_generic ("merge_bits", GFC_ISYM_MERGE_BITS, GFC_STD_F2008);

  /* Note: amin0 is equivalent to real(min), min1 is equivalent to
     int(min).  */

  add_sym_1m ("min", GFC_ISYM_MIN, CLASS_ELEMENTAL, ACTUAL_NO, BT_UNKNOWN, 0, GFC_STD_F77,
	      gfc_check_min_max, gfc_simplify_min, gfc_resolve_min,
	      a1, BT_REAL, dr, REQUIRED, a2, BT_REAL, dr, REQUIRED);

  add_sym_1m ("min0", GFC_ISYM_MIN, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F77,
	      gfc_check_min_max_integer, gfc_simplify_min, NULL,
	      a1, BT_INTEGER, di, REQUIRED, a2, BT_INTEGER, di, REQUIRED);

  add_sym_1m ("amin0", GFC_ISYM_MIN, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F77,
	      gfc_check_min_max_integer, gfc_simplify_min, NULL,
	      a1, BT_INTEGER, di, REQUIRED, a2, BT_INTEGER, di, REQUIRED);

  add_sym_1m ("amin1", GFC_ISYM_MIN, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F77,
	      gfc_check_min_max_real, gfc_simplify_min, NULL,
	      a1, BT_REAL, dr, REQUIRED, a2, BT_REAL, dr, REQUIRED);

  add_sym_1m ("min1", GFC_ISYM_MIN, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F77,
	      gfc_check_min_max_real, gfc_simplify_min, NULL,
	      a1, BT_REAL, dr, REQUIRED, a2, BT_REAL, dr, REQUIRED);

  add_sym_1m ("dmin1", GFC_ISYM_MIN, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd, GFC_STD_F77,
	      gfc_check_min_max_double, gfc_simplify_min, NULL,
	      a1, BT_REAL, dd, REQUIRED, a2, BT_REAL, dd, REQUIRED);

  make_generic ("min", GFC_ISYM_MIN, GFC_STD_F77);

  add_sym_1 ("minexponent", GFC_ISYM_MINEXPONENT, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_F95, gfc_check_fn_r, gfc_simplify_minexponent, NULL,
	     x, BT_UNKNOWN, dr, REQUIRED);

  make_generic ("minexponent", GFC_ISYM_MINEXPONENT, GFC_STD_F95);

  add_sym_5ml ("minloc", GFC_ISYM_MINLOC, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	       gfc_check_minloc_maxloc, gfc_simplify_minloc, gfc_resolve_minloc,
	       ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
	       msk, BT_LOGICAL, dl, OPTIONAL, kind, BT_INTEGER, di, OPTIONAL,
	       bck, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("minloc", GFC_ISYM_MINLOC, GFC_STD_F95);

  add_sym_3red ("minval", GFC_ISYM_MINVAL, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
		gfc_check_minval_maxval, gfc_simplify_minval, gfc_resolve_minval,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("minval", GFC_ISYM_MINVAL, GFC_STD_F95);

  add_sym_2 ("mod", GFC_ISYM_MOD, CLASS_ELEMENTAL, ACTUAL_YES, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_a_p, gfc_simplify_mod, gfc_resolve_mod,
	     a, BT_INTEGER, di, REQUIRED, p, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bmod", GFC_STD_GNU);
      make_alias ("imod", GFC_STD_GNU);
      make_alias ("jmod", GFC_STD_GNU);
      make_alias ("kmod", GFC_STD_GNU);
    }

  add_sym_2 ("amod", GFC_ISYM_MOD, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     NULL, gfc_simplify_mod, gfc_resolve_mod,
	     a, BT_REAL, dr, REQUIRED, p, BT_REAL, dr, REQUIRED);

  add_sym_2 ("dmod", GFC_ISYM_MOD, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_x_yd, gfc_simplify_mod, gfc_resolve_mod,
	     a, BT_REAL, dd, REQUIRED, p, BT_REAL, dd, REQUIRED);

  make_generic ("mod", GFC_ISYM_MOD, GFC_STD_F77);

  add_sym_2 ("modulo", GFC_ISYM_MODULO, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, di, GFC_STD_F95,
	     gfc_check_a_p, gfc_simplify_modulo, gfc_resolve_modulo,
	     a, BT_REAL, di, REQUIRED, p, BT_REAL, di, REQUIRED);

  make_generic ("modulo", GFC_ISYM_MODULO, GFC_STD_F95);

  add_sym_2 ("nearest", GFC_ISYM_NEAREST, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_nearest, gfc_simplify_nearest, gfc_resolve_nearest,
	     x, BT_REAL, dr, REQUIRED, s, BT_REAL, dr, REQUIRED);

  make_generic ("nearest", GFC_ISYM_NEAREST, GFC_STD_F95);

  add_sym_1 ("new_line", GFC_ISYM_NEW_LINE, CLASS_INQUIRY, ACTUAL_NO, BT_CHARACTER, dc,
	     GFC_STD_F2003, gfc_check_new_line, gfc_simplify_new_line, NULL,
	     a, BT_CHARACTER, dc, REQUIRED);

  make_generic ("new_line", GFC_ISYM_NEW_LINE, GFC_STD_F2003);

  add_sym_2 ("nint", GFC_ISYM_NINT, CLASS_ELEMENTAL, ACTUAL_YES, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_a_ikind, gfc_simplify_nint, gfc_resolve_nint,
	     a, BT_REAL, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  add_sym_1 ("idnint", GFC_ISYM_NINT, CLASS_ELEMENTAL, ACTUAL_YES, BT_INTEGER, di, GFC_STD_F77,
	     gfc_check_idnint, gfc_simplify_idnint, gfc_resolve_idnint,
	     a, BT_REAL, dd, REQUIRED);

  make_generic ("nint", GFC_ISYM_NINT, GFC_STD_F77);

  add_sym_1 ("not", GFC_ISYM_NOT, CLASS_ELEMENTAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_i, gfc_simplify_not, gfc_resolve_not,
	     i, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bnot", GFC_STD_GNU);
      make_alias ("inot", GFC_STD_GNU);
      make_alias ("jnot", GFC_STD_GNU);
      make_alias ("knot", GFC_STD_GNU);
    }

  make_generic ("not", GFC_ISYM_NOT, GFC_STD_F95);

  add_sym_2 ("norm2", GFC_ISYM_NORM2, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr,
	     GFC_STD_F2008, gfc_check_norm2, gfc_simplify_norm2, gfc_resolve_norm2,
	     x, BT_REAL, dr, REQUIRED,
	     dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("norm2", GFC_ISYM_NORM2, GFC_STD_F2008);

  add_sym_1 ("null", GFC_ISYM_NULL, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_null, gfc_simplify_null, NULL,
	     mo, BT_INTEGER, di, OPTIONAL);

  make_generic ("null", GFC_ISYM_NULL, GFC_STD_F95);

  add_sym_2 ("num_images", GFC_ISYM_NUM_IMAGES, CLASS_TRANSFORMATIONAL,
	     ACTUAL_NO, BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_num_images, gfc_simplify_num_images, NULL,
	     dist, BT_INTEGER, di, OPTIONAL,
	     failed, BT_LOGICAL, dl, OPTIONAL);

  add_sym_3 ("pack", GFC_ISYM_PACK, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_pack, gfc_simplify_pack, gfc_resolve_pack,
	     ar, BT_REAL, dr, REQUIRED, msk, BT_LOGICAL, dl, REQUIRED,
	     v, BT_REAL, dr, OPTIONAL);

  make_generic ("pack", GFC_ISYM_PACK, GFC_STD_F95);


  add_sym_2 ("parity", GFC_ISYM_PARITY, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_LOGICAL, dl,
	     GFC_STD_F2008, gfc_check_parity, gfc_simplify_parity, gfc_resolve_parity,
	     msk, BT_LOGICAL, dl, REQUIRED,
	     dm, BT_INTEGER, ii, OPTIONAL);

  make_generic ("parity", GFC_ISYM_PARITY, GFC_STD_F2008);

  add_sym_1 ("popcnt", GFC_ISYM_POPCNT, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_i, gfc_simplify_popcnt, NULL,
	     i, BT_INTEGER, di, REQUIRED);

  make_generic ("popcnt", GFC_ISYM_POPCNT, GFC_STD_F2008);

  add_sym_1 ("poppar", GFC_ISYM_POPPAR, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_i, gfc_simplify_poppar, NULL,
	     i, BT_INTEGER, di, REQUIRED);

  make_generic ("poppar", GFC_ISYM_POPPAR, GFC_STD_F2008);

  add_sym_1 ("precision", GFC_ISYM_PRECISION, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_precision, gfc_simplify_precision, NULL,
	     x, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("precision", GFC_ISYM_PRECISION, GFC_STD_F95);

  add_sym_1_intent ("present", GFC_ISYM_PRESENT, CLASS_INQUIRY, ACTUAL_NO,
		    BT_LOGICAL, dl, GFC_STD_F95, gfc_check_present, NULL, NULL,
		    a, BT_REAL, dr, REQUIRED, INTENT_UNKNOWN);

  make_generic ("present", GFC_ISYM_PRESENT, GFC_STD_F95);

  add_sym_3red ("product", GFC_ISYM_PRODUCT, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
		gfc_check_product_sum, gfc_simplify_product, gfc_resolve_product,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("product", GFC_ISYM_PRODUCT, GFC_STD_F95);

  add_sym_1 ("radix", GFC_ISYM_RADIX, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_radix, gfc_simplify_radix, NULL,
	     x, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("radix", GFC_ISYM_RADIX, GFC_STD_F95);

  /* The following function is for G77 compatibility.  */
  add_sym_1 ("rand", GFC_ISYM_RAND, CLASS_IMPURE, ACTUAL_NO, BT_REAL,
	     4, GFC_STD_GNU, gfc_check_rand, NULL, NULL,
	     i, BT_INTEGER, 4, OPTIONAL);

  /* Compatibility with HP FORTRAN 77/iX Reference.  Note, rand() and ran()
     use slightly different shoddy multiplicative congruential PRNG.  */
  make_alias ("ran", GFC_STD_GNU);

  make_generic ("rand", GFC_ISYM_RAND, GFC_STD_GNU);

  add_sym_1 ("range", GFC_ISYM_RANGE, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_range, gfc_simplify_range, NULL,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("range", GFC_ISYM_RANGE, GFC_STD_F95);

  add_sym_1 ("rank", GFC_ISYM_RANK, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_F2018, gfc_check_rank, gfc_simplify_rank, gfc_resolve_rank,
	     a, BT_REAL, dr, REQUIRED);
  make_generic ("rank", GFC_ISYM_RANK, GFC_STD_F2018);

  add_sym_2 ("real", GFC_ISYM_REAL, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_real, gfc_simplify_real, gfc_resolve_real,
	     a, BT_UNKNOWN, dr, REQUIRED, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("real", GFC_ISYM_REAL, GFC_STD_F77);

  /* This provides compatibility with g77.  */
  add_sym_1 ("realpart", GFC_ISYM_REALPART, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_c, gfc_simplify_realpart, gfc_resolve_realpart,
	     a, BT_UNKNOWN, dr, REQUIRED);

  make_generic ("realpart", GFC_ISYM_REALPART, GFC_STD_F77);

  add_sym_1 ("float", GFC_ISYM_FLOAT, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_float, gfc_simplify_float, NULL,
	     a, BT_INTEGER, di, REQUIRED);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("floati", GFC_STD_GNU);
      make_alias ("floatj", GFC_STD_GNU);
      make_alias ("floatk", GFC_STD_GNU);
    }

  make_generic ("float", GFC_ISYM_FLOAT, GFC_STD_F77);

  add_sym_1 ("dfloat", GFC_ISYM_DFLOAT, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_float, gfc_simplify_dble, gfc_resolve_dble,
	     a, BT_REAL, dr, REQUIRED);

  make_generic ("dfloat", GFC_ISYM_DFLOAT, GFC_STD_F77);

  add_sym_1 ("sngl", GFC_ISYM_SNGL, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_sngl, gfc_simplify_sngl, NULL,
	     a, BT_REAL, dd, REQUIRED);

  make_generic ("sngl", GFC_ISYM_SNGL, GFC_STD_F77);

  add_sym_2 ("rename", GFC_ISYM_RENAME, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_GNU, gfc_check_rename, NULL, gfc_resolve_rename,
	     p1, BT_CHARACTER, dc, REQUIRED, p2, BT_CHARACTER, dc, REQUIRED);

  make_generic ("rename", GFC_ISYM_RENAME, GFC_STD_GNU);

  add_sym_2 ("repeat", GFC_ISYM_REPEAT, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_CHARACTER, dc, GFC_STD_F95,
	     gfc_check_repeat, gfc_simplify_repeat, gfc_resolve_repeat,
	     stg, BT_CHARACTER, dc, REQUIRED, ncopies, BT_INTEGER, di, REQUIRED);

  make_generic ("repeat", GFC_ISYM_REPEAT, GFC_STD_F95);

  add_sym_4 ("reshape", GFC_ISYM_RESHAPE, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_reshape, gfc_simplify_reshape, gfc_resolve_reshape,
	     src, BT_REAL, dr, REQUIRED, shp, BT_INTEGER, ii, REQUIRED,
	     pad, BT_REAL, dr, OPTIONAL, ord, BT_INTEGER, ii, OPTIONAL);

  make_generic ("reshape", GFC_ISYM_RESHAPE, GFC_STD_F95);

  add_sym_1 ("rrspacing", GFC_ISYM_RRSPACING, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr,
	     GFC_STD_F95, gfc_check_fn_r, gfc_simplify_rrspacing, gfc_resolve_rrspacing,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("rrspacing", GFC_ISYM_RRSPACING, GFC_STD_F95);

  add_sym_2 ("same_type_as", GFC_ISYM_SAME_TYPE_AS, CLASS_INQUIRY, ACTUAL_NO,
	     BT_LOGICAL, dl, GFC_STD_F2003,
	     gfc_check_same_type_as, gfc_simplify_same_type_as, NULL,
	     a, BT_UNKNOWN, 0, REQUIRED,
	     b, BT_UNKNOWN, 0, REQUIRED);

  add_sym_2 ("scale", GFC_ISYM_SCALE, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_scale, gfc_simplify_scale, gfc_resolve_scale,
	     x, BT_REAL, dr, REQUIRED, i, BT_INTEGER, di, REQUIRED);

  make_generic ("scale", GFC_ISYM_SCALE, GFC_STD_F95);

  add_sym_4 ("scan", GFC_ISYM_SCAN, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_scan, gfc_simplify_scan, gfc_resolve_scan,
	     stg, BT_CHARACTER, dc, REQUIRED, set, BT_CHARACTER, dc, REQUIRED,
	     bck, BT_LOGICAL, dl, OPTIONAL, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("scan", GFC_ISYM_SCAN, GFC_STD_F95);

  /* Added for G77 compatibility garbage.  */
  add_sym_0 ("second", GFC_ISYM_SECOND, CLASS_IMPURE, ACTUAL_NO, BT_REAL,
	     4, GFC_STD_GNU, NULL, NULL, NULL);

  make_generic ("second", GFC_ISYM_SECOND, GFC_STD_GNU);

  /* Added for G77 compatibility.  */
  add_sym_1 ("secnds", GFC_ISYM_SECNDS, CLASS_IMPURE, ACTUAL_NO, BT_REAL,
	     dr, GFC_STD_GNU, gfc_check_secnds, NULL, gfc_resolve_secnds,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("secnds", GFC_ISYM_SECNDS, GFC_STD_GNU);

  add_sym_1 ("selected_char_kind", GFC_ISYM_SC_KIND, CLASS_TRANSFORMATIONAL,
	     ACTUAL_NO, BT_INTEGER, di, GFC_STD_F2003,
	     gfc_check_selected_char_kind, gfc_simplify_selected_char_kind,
	     NULL, nm, BT_CHARACTER, dc, REQUIRED);

  make_generic ("selected_char_kind", GFC_ISYM_SC_KIND, GFC_STD_F2003);

  add_sym_1 ("selected_int_kind", GFC_ISYM_SI_KIND, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_F95, gfc_check_selected_int_kind,
	     gfc_simplify_selected_int_kind, NULL, r, BT_INTEGER, di, REQUIRED);

  make_generic ("selected_int_kind", GFC_ISYM_SI_KIND, GFC_STD_F95);

  add_sym_3 ("selected_real_kind", GFC_ISYM_SR_KIND, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_F95, gfc_check_selected_real_kind,
	     gfc_simplify_selected_real_kind, NULL,
	     p, BT_INTEGER, di, OPTIONAL, r, BT_INTEGER, di, OPTIONAL,
	     "radix", BT_INTEGER, di, OPTIONAL);

  make_generic ("selected_real_kind", GFC_ISYM_SR_KIND, GFC_STD_F95);

  add_sym_2 ("set_exponent", GFC_ISYM_SET_EXPONENT, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_set_exponent, gfc_simplify_set_exponent,
	     gfc_resolve_set_exponent,
	     x, BT_REAL, dr, REQUIRED, i, BT_INTEGER, di, REQUIRED);

  make_generic ("set_exponent", GFC_ISYM_SET_EXPONENT, GFC_STD_F95);

  add_sym_2 ("shape", GFC_ISYM_SHAPE, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_shape, gfc_simplify_shape, gfc_resolve_shape,
	     src, BT_REAL, dr, REQUIRED,
	     kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("shape", GFC_ISYM_SHAPE, GFC_STD_F95);

  add_sym_2 ("shifta", GFC_ISYM_SHIFTA, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_shift, gfc_simplify_shifta, gfc_resolve_shift,
	     i, BT_INTEGER, di, REQUIRED,
	     sh, BT_INTEGER, di, REQUIRED);

  make_generic ("shifta", GFC_ISYM_SHIFTA, GFC_STD_F2008);

  add_sym_2 ("shiftl", GFC_ISYM_SHIFTL, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_shift, gfc_simplify_shiftl, gfc_resolve_shift,
	     i, BT_INTEGER, di, REQUIRED,
	     sh, BT_INTEGER, di, REQUIRED);

  make_generic ("shiftl", GFC_ISYM_SHIFTL, GFC_STD_F2008);

  add_sym_2 ("shiftr", GFC_ISYM_SHIFTR, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_shift, gfc_simplify_shiftr, gfc_resolve_shift,
	     i, BT_INTEGER, di, REQUIRED,
	     sh, BT_INTEGER, di, REQUIRED);

  make_generic ("shiftr", GFC_ISYM_SHIFTR, GFC_STD_F2008);

  add_sym_2 ("sign", GFC_ISYM_SIGN, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_sign, gfc_simplify_sign, gfc_resolve_sign,
	     a, BT_REAL, dr, REQUIRED, b, BT_REAL, dr, REQUIRED);

  add_sym_2 ("isign", GFC_ISYM_SIGN, CLASS_ELEMENTAL, ACTUAL_YES, BT_INTEGER, di, GFC_STD_F77,
	     NULL, gfc_simplify_sign, gfc_resolve_sign,
	     a, BT_INTEGER, di, REQUIRED, b, BT_INTEGER, di, REQUIRED);

  add_sym_2 ("dsign", GFC_ISYM_SIGN, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_x_yd, gfc_simplify_sign, gfc_resolve_sign,
	     a, BT_REAL, dd, REQUIRED, b, BT_REAL, dd, REQUIRED);

  make_generic ("sign", GFC_ISYM_SIGN, GFC_STD_F77);

  add_sym_2 ("signal", GFC_ISYM_SIGNAL, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, gfc_check_signal, NULL, gfc_resolve_signal,
	     num, BT_INTEGER, di, REQUIRED, han, BT_VOID, 0, REQUIRED);

  make_generic ("signal", GFC_ISYM_SIGNAL, GFC_STD_GNU);

  add_sym_1 ("sin", GFC_ISYM_SIN, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_sin, gfc_resolve_sin,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dsin", GFC_ISYM_SIN, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_sin, gfc_resolve_sin,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("csin", GFC_ISYM_SIN, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dz, GFC_STD_F77,
	     NULL, gfc_simplify_sin, gfc_resolve_sin,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zsin", GFC_ISYM_SIN, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_sin, gfc_resolve_sin,
	     x, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdsin", GFC_STD_GNU);

  make_generic ("sin", GFC_ISYM_SIN, GFC_STD_F77);

  add_sym_1 ("sinh", GFC_ISYM_SINH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc2008, gfc_simplify_sinh, gfc_resolve_sinh,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dsinh", GFC_ISYM_SINH,CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_sinh, gfc_resolve_sinh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("sinh", GFC_ISYM_SINH, GFC_STD_F77);

  add_sym_3 ("size", GFC_ISYM_SIZE, CLASS_INQUIRY, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_size, gfc_simplify_size, gfc_resolve_size,
	     ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
	     kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("size", GFC_ISYM_SIZE, GFC_STD_F95);

  /* Obtain the stride for a given dimensions; to be used only internally.
     "make_from_module" makes it inaccessible for external users.  */
  add_sym_2 (GFC_PREFIX ("stride"), GFC_ISYM_STRIDE, CLASS_INQUIRY, ACTUAL_NO,
	     BT_INTEGER, gfc_index_integer_kind, GFC_STD_GNU,
	     NULL, NULL, gfc_resolve_stride,
	     ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL);
  make_from_module();

  add_sym_1 ("sizeof", GFC_ISYM_SIZEOF, CLASS_INQUIRY, ACTUAL_NO,
	     BT_INTEGER, ii, GFC_STD_GNU,
	     gfc_check_sizeof, gfc_simplify_sizeof, NULL,
	     x, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("sizeof", GFC_ISYM_SIZEOF, GFC_STD_GNU);

  /* The following functions are part of ISO_C_BINDING.  */
  add_sym_2 ("c_associated", GFC_ISYM_C_ASSOCIATED, CLASS_INQUIRY, ACTUAL_NO,
	     BT_LOGICAL, dl, GFC_STD_F2003, gfc_check_c_associated, NULL, NULL,
	     c_ptr_1, BT_VOID, 0, REQUIRED,
	     c_ptr_2, BT_VOID, 0, OPTIONAL);
  make_from_module();

  add_sym_1 ("c_loc", GFC_ISYM_C_LOC, CLASS_INQUIRY, ACTUAL_NO,
	     BT_VOID, 0, GFC_STD_F2003,
	     gfc_check_c_loc, NULL, gfc_resolve_c_loc,
	     x, BT_UNKNOWN, 0, REQUIRED);
  make_from_module();

  add_sym_1 ("c_funloc", GFC_ISYM_C_FUNLOC, CLASS_INQUIRY, ACTUAL_NO,
	     BT_VOID, 0, GFC_STD_F2003,
	     gfc_check_c_funloc, NULL, gfc_resolve_c_funloc,
	     x, BT_UNKNOWN, 0, REQUIRED);
  make_from_module();

  add_sym_1 ("c_sizeof", GFC_ISYM_C_SIZEOF, CLASS_INQUIRY, ACTUAL_NO,
	     BT_INTEGER, gfc_index_integer_kind, GFC_STD_F2008,
	     gfc_check_c_sizeof, gfc_simplify_sizeof, NULL,
	     x, BT_UNKNOWN, 0, REQUIRED);
  make_from_module();

  /* COMPILER_OPTIONS and COMPILER_VERSION are part of ISO_FORTRAN_ENV.  */
  add_sym_0 ("compiler_options", GFC_ISYM_COMPILER_OPTIONS, CLASS_INQUIRY,
	     ACTUAL_NO, BT_CHARACTER, dc, GFC_STD_F2008,
	     NULL, gfc_simplify_compiler_options, NULL);
  make_from_module();

  add_sym_0 ("compiler_version", GFC_ISYM_COMPILER_VERSION, CLASS_INQUIRY,
	     ACTUAL_NO, BT_CHARACTER, dc, GFC_STD_F2008,
	     NULL, gfc_simplify_compiler_version, NULL);
  make_from_module();

  add_sym_1 ("spacing", GFC_ISYM_SPACING, CLASS_ELEMENTAL, ACTUAL_NO, BT_REAL, dr,
	     GFC_STD_F95, gfc_check_fn_r, gfc_simplify_spacing, gfc_resolve_spacing,
	     x, BT_REAL, dr, REQUIRED);

  make_generic ("spacing", GFC_ISYM_SPACING, GFC_STD_F95);

  add_sym_3 ("spread", GFC_ISYM_SPREAD, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_spread, gfc_simplify_spread, gfc_resolve_spread,
	     src, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, REQUIRED,
	     ncopies, BT_INTEGER, di, REQUIRED);

  make_generic ("spread", GFC_ISYM_SPREAD, GFC_STD_F95);

  add_sym_1 ("sqrt", GFC_ISYM_SQRT, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc, gfc_simplify_sqrt, gfc_resolve_sqrt,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dsqrt", GFC_ISYM_SQRT, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_sqrt, gfc_resolve_sqrt,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("csqrt", GFC_ISYM_SQRT, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dz, GFC_STD_F77,
	     NULL, gfc_simplify_sqrt, gfc_resolve_sqrt,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zsqrt", GFC_ISYM_SQRT, CLASS_ELEMENTAL, ACTUAL_YES, BT_COMPLEX, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_sqrt, gfc_resolve_sqrt,
	     x, BT_COMPLEX, dd, REQUIRED);

  make_alias ("cdsqrt", GFC_STD_GNU);

  make_generic ("sqrt", GFC_ISYM_SQRT, GFC_STD_F77);

  add_sym_2_intent ("stat", GFC_ISYM_STAT, CLASS_IMPURE, ACTUAL_NO,
		    BT_INTEGER, di, GFC_STD_GNU,
		    gfc_check_stat, NULL, gfc_resolve_stat,
		    nm, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
		    vl, BT_INTEGER, di, REQUIRED, INTENT_OUT);

  make_generic ("stat", GFC_ISYM_STAT, GFC_STD_GNU);

  add_sym_2 ("stopped_images", GFC_ISYM_STOPPED_IMAGES, CLASS_TRANSFORMATIONAL,
	     ACTUAL_NO, BT_INTEGER, dd, GFC_STD_F2018,
	     gfc_check_failed_or_stopped_images,
	     gfc_simplify_failed_or_stopped_images,
	     gfc_resolve_stopped_images, team, BT_VOID, di, OPTIONAL,
	     kind, BT_INTEGER, di, OPTIONAL);

  add_sym_2 ("storage_size", GFC_ISYM_STORAGE_SIZE, CLASS_INQUIRY, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_storage_size, gfc_simplify_storage_size,
	     gfc_resolve_storage_size,
	     a, BT_UNKNOWN, 0, REQUIRED,
	     kind, BT_INTEGER, di, OPTIONAL);

  add_sym_3red ("sum", GFC_ISYM_SUM, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
		gfc_check_product_sum, gfc_simplify_sum, gfc_resolve_sum,
		ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
		msk, BT_LOGICAL, dl, OPTIONAL);

  make_generic ("sum", GFC_ISYM_SUM, GFC_STD_F95);

  add_sym_2 ("symlnk", GFC_ISYM_SYMLNK, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_GNU, gfc_check_symlnk, NULL, gfc_resolve_symlnk,
	     p1, BT_CHARACTER, dc, REQUIRED, p2, BT_CHARACTER, dc, REQUIRED);

  make_generic ("symlnk", GFC_ISYM_SYMLNK, GFC_STD_GNU);

  add_sym_1 ("system", GFC_ISYM_SYSTEM, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_GNU, NULL, NULL, NULL,
	     com, BT_CHARACTER, dc, REQUIRED);

  make_generic ("system", GFC_ISYM_SYSTEM, GFC_STD_GNU);

  add_sym_1 ("tan", GFC_ISYM_TAN, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc2008, gfc_simplify_tan, gfc_resolve_tan,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dtan", GFC_ISYM_TAN, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_tan, gfc_resolve_tan,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("tan", GFC_ISYM_TAN, GFC_STD_F77);

  add_sym_1 ("tanh", GFC_ISYM_TANH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dr, GFC_STD_F77,
	     gfc_check_fn_rc2008, gfc_simplify_tanh, gfc_resolve_tanh,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dtanh", GFC_ISYM_TANH, CLASS_ELEMENTAL, ACTUAL_YES, BT_REAL, dd, GFC_STD_F77,
	     gfc_check_fn_d, gfc_simplify_tanh, gfc_resolve_tanh,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("tanh", GFC_ISYM_TANH, GFC_STD_F77);

  add_sym_1 ("team_number", GFC_ISYM_TEAM_NUMBER, CLASS_TRANSFORMATIONAL,
	     ACTUAL_NO, BT_INTEGER, di, GFC_STD_F2018,
	     gfc_check_team_number, NULL, gfc_resolve_team_number,
	     team, BT_DERIVED, di, OPTIONAL);

  add_sym_3 ("this_image", GFC_ISYM_THIS_IMAGE, CLASS_INQUIRY, ACTUAL_NO, BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_this_image, gfc_simplify_this_image, gfc_resolve_this_image,
	     ca, BT_REAL, dr, OPTIONAL, dm, BT_INTEGER, ii, OPTIONAL,
	     dist, BT_INTEGER, di, OPTIONAL);

  add_sym_0 ("time", GFC_ISYM_TIME, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, NULL, NULL, gfc_resolve_time);

  make_generic ("time", GFC_ISYM_TIME, GFC_STD_GNU);

  add_sym_0 ("time8", GFC_ISYM_TIME8, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, NULL, NULL, gfc_resolve_time8);

  make_generic ("time8", GFC_ISYM_TIME8, GFC_STD_GNU);

  add_sym_1 ("tiny", GFC_ISYM_TINY, CLASS_INQUIRY, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_fn_r, gfc_simplify_tiny, NULL, x, BT_REAL, dr, REQUIRED);

  make_generic ("tiny", GFC_ISYM_TINY, GFC_STD_F95);

  add_sym_1 ("trailz", GFC_ISYM_TRAILZ, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F2008,
	     gfc_check_i, gfc_simplify_trailz, NULL,
	     i, BT_INTEGER, di, REQUIRED);

  make_generic ("trailz", GFC_ISYM_TRAILZ, GFC_STD_F2008);

  add_sym_3 ("transfer", GFC_ISYM_TRANSFER, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_transfer, gfc_simplify_transfer, gfc_resolve_transfer,
	     src, BT_REAL, dr, REQUIRED, mo, BT_REAL, dr, REQUIRED,
	     sz, BT_INTEGER, di, OPTIONAL);

  make_generic ("transfer", GFC_ISYM_TRANSFER, GFC_STD_F95);

  add_sym_1 ("transpose", GFC_ISYM_TRANSPOSE, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_transpose, gfc_simplify_transpose, gfc_resolve_transpose,
	     m, BT_REAL, dr, REQUIRED);

  make_generic ("transpose", GFC_ISYM_TRANSPOSE, GFC_STD_F95);

  add_sym_1 ("trim", GFC_ISYM_TRIM, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_CHARACTER, dc, GFC_STD_F95,
	     gfc_check_trim, gfc_simplify_trim, gfc_resolve_trim,
	     stg, BT_CHARACTER, dc, REQUIRED);

  make_generic ("trim", GFC_ISYM_TRIM, GFC_STD_F95);

  add_sym_1 ("ttynam", GFC_ISYM_TTYNAM, CLASS_IMPURE, ACTUAL_NO, BT_CHARACTER,
	     0, GFC_STD_GNU, gfc_check_ttynam, NULL, gfc_resolve_ttynam,
	     ut, BT_INTEGER, di, REQUIRED);

  make_generic ("ttynam", GFC_ISYM_TTYNAM, GFC_STD_GNU);

  add_sym_3 ("ubound", GFC_ISYM_UBOUND, CLASS_INQUIRY, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_ubound, gfc_simplify_ubound, gfc_resolve_ubound,
	     ar, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
	     kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("ubound", GFC_ISYM_UBOUND, GFC_STD_F95);

  add_sym_3 ("ucobound", GFC_ISYM_UCOBOUND, CLASS_INQUIRY, ACTUAL_NO,
	    BT_INTEGER, di, GFC_STD_F2008,
	    gfc_check_ucobound, gfc_simplify_ucobound, gfc_resolve_ucobound,
	    ca, BT_REAL, dr, REQUIRED, dm, BT_INTEGER, ii, OPTIONAL,
	    kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("ucobound", GFC_ISYM_UCOBOUND, GFC_STD_F2008);

  /* g77 compatibility for UMASK.  */
  add_sym_1 ("umask", GFC_ISYM_UMASK, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER, di,
	     GFC_STD_GNU, gfc_check_umask, NULL, gfc_resolve_umask,
	     msk, BT_INTEGER, di, REQUIRED);

  make_generic ("umask", GFC_ISYM_UMASK, GFC_STD_GNU);

  /* g77 compatibility for UNLINK.  */
  add_sym_1 ("unlink", GFC_ISYM_UNLINK, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER,
	     di, GFC_STD_GNU, gfc_check_unlink, NULL, gfc_resolve_unlink,
	     "path", BT_CHARACTER, dc, REQUIRED);

  make_generic ("unlink", GFC_ISYM_UNLINK, GFC_STD_GNU);

  add_sym_3 ("unpack", GFC_ISYM_UNPACK, CLASS_TRANSFORMATIONAL, ACTUAL_NO, BT_REAL, dr, GFC_STD_F95,
	     gfc_check_unpack, gfc_simplify_unpack, gfc_resolve_unpack,
	     v, BT_REAL, dr, REQUIRED, msk, BT_LOGICAL, dl, REQUIRED,
	     f, BT_REAL, dr, REQUIRED);

  make_generic ("unpack", GFC_ISYM_UNPACK, GFC_STD_F95);

  add_sym_4 ("verify", GFC_ISYM_VERIFY, CLASS_ELEMENTAL, ACTUAL_NO,
	     BT_INTEGER, di, GFC_STD_F95,
	     gfc_check_verify, gfc_simplify_verify, gfc_resolve_verify,
	     stg, BT_CHARACTER, dc, REQUIRED, set, BT_CHARACTER, dc, REQUIRED,
	     bck, BT_LOGICAL, dl, OPTIONAL, kind, BT_INTEGER, di, OPTIONAL);

  make_generic ("verify", GFC_ISYM_VERIFY, GFC_STD_F95);

  add_sym_1 ("loc", GFC_ISYM_LOC, CLASS_IMPURE, ACTUAL_NO, BT_INTEGER, ii,
	     GFC_STD_GNU, gfc_check_loc, NULL, gfc_resolve_loc,
	     x, BT_UNKNOWN, 0, REQUIRED);

  make_generic ("loc", GFC_ISYM_LOC, GFC_STD_GNU);


  /* The next of intrinsic subprogram are the degree trignometric functions.
     These were hidden behind the -fdec-math option, but are now simply
     included as extensions to the set of intrinsic subprograms.  */

  add_sym_1 ("acosd", GFC_ISYM_ACOSD, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_acosd, gfc_resolve_trigd,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dacosd", GFC_ISYM_ACOSD, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_acosd, gfc_resolve_trigd,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("acosd", GFC_ISYM_ACOSD, GFC_STD_GNU);

  add_sym_1 ("asind", GFC_ISYM_ASIND, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_asind, gfc_resolve_trigd,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dasind", GFC_ISYM_ASIND, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_asind, gfc_resolve_trigd,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("asind", GFC_ISYM_ASIND, GFC_STD_GNU);

  add_sym_1 ("atand", GFC_ISYM_ATAND, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_atand, gfc_resolve_trigd,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("datand", GFC_ISYM_ATAND, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_atand, gfc_resolve_trigd,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("atand", GFC_ISYM_ATAND, GFC_STD_GNU);

  add_sym_2 ("atan2d", GFC_ISYM_ATAN2D, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_atan2, gfc_simplify_atan2d, gfc_resolve_trigd2,
	     y, BT_REAL, dr, REQUIRED,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_2 ("datan2d", GFC_ISYM_ATAN2D, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_datan2, gfc_simplify_atan2d, gfc_resolve_trigd2,
	     y, BT_REAL, dd, REQUIRED,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("atan2d", GFC_ISYM_ATAN2D, GFC_STD_GNU);

  add_sym_1 ("cosd", GFC_ISYM_COSD, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_cosd, gfc_resolve_trigd,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dcosd", GFC_ISYM_COSD, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_cosd, gfc_resolve_trigd,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("cosd", GFC_ISYM_COSD, GFC_STD_GNU);

  add_sym_1 ("cotan", GFC_ISYM_COTAN, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_rc2008, gfc_simplify_cotan, gfc_resolve_trigd,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dcotan", GFC_ISYM_COTAN, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_cotan, gfc_resolve_trigd,
	     x, BT_REAL, dd, REQUIRED);

  add_sym_1 ("ccotan", GFC_ISYM_COTAN, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_COMPLEX, dz, GFC_STD_GNU,
	     NULL, gfc_simplify_cotan, gfc_resolve_trigd,
	     x, BT_COMPLEX, dz, REQUIRED);

  add_sym_1 ("zcotan", GFC_ISYM_COTAN, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_COMPLEX, dd, GFC_STD_GNU,
	     NULL, gfc_simplify_cotan, gfc_resolve_trigd,
	     x, BT_COMPLEX, dd, REQUIRED);

  make_generic ("cotan", GFC_ISYM_COTAN, GFC_STD_GNU);

  add_sym_1 ("cotand", GFC_ISYM_COTAND, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_cotand, gfc_resolve_trigd,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dcotand", GFC_ISYM_COTAND, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_cotand, gfc_resolve_trigd,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("cotand", GFC_ISYM_COTAND, GFC_STD_GNU);

  add_sym_1 ("sind", GFC_ISYM_SIND, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_sind, gfc_resolve_trigd,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dsind", GFC_ISYM_SIND, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_sind, gfc_resolve_trigd,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("sind", GFC_ISYM_SIND, GFC_STD_GNU);

  add_sym_1 ("tand", GFC_ISYM_TAND, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dr, GFC_STD_GNU,
	     gfc_check_fn_r, gfc_simplify_tand, gfc_resolve_trigd,
	     x, BT_REAL, dr, REQUIRED);

  add_sym_1 ("dtand", GFC_ISYM_TAND, CLASS_ELEMENTAL, ACTUAL_YES,
	     BT_REAL, dd, GFC_STD_GNU,
	     gfc_check_fn_d, gfc_simplify_tand, gfc_resolve_trigd,
	     x, BT_REAL, dd, REQUIRED);

  make_generic ("tand", GFC_ISYM_TAND, GFC_STD_GNU);

  /* The following function is internally used for coarray libray functions.
     "make_from_module" makes it inaccessible for external users.  */
  add_sym_1 (GFC_PREFIX ("caf_get"), GFC_ISYM_CAF_GET, CLASS_IMPURE, ACTUAL_NO,
	     BT_REAL, dr, GFC_STD_GNU, NULL, NULL, NULL,
	     x, BT_REAL, dr, REQUIRED);
  make_from_module();
}


/* Add intrinsic subroutines.  */

static void
add_subroutines (void)
{
  /* Argument names.  These are used as argument keywords and so need to
     match the documentation.  Please keep this list in sorted order.  */
  static const char
    *a = "a", *c_ = "c", *c = "count", *cm = "count_max", *com = "command",
    *cr = "count_rate", *dt = "date", *errmsg = "errmsg", *f = "from",
    *fp = "frompos", *gt = "get", *h = "harvest", *han = "handler",
    *length = "length", *ln = "len", *md = "mode", *msk = "mask",
    *name = "name", *num = "number", *of = "offset", *old = "old",
    *p1 = "path1", *p2 = "path2", *pid = "pid", *pos = "pos",
    *pt = "put", *ptr = "ptr", *res = "result",
    *result_image = "result_image", *sec = "seconds", *sig = "sig",
    *st = "status", *stat = "stat", *sz = "size", *t = "to",
    *tm = "time", *tp = "topos", *trim_name = "trim_name", *ut = "unit",
    *val = "value", *vl = "values", *whence = "whence", *zn = "zone";

  int di, dr, dc, dl, ii;

  di = gfc_default_integer_kind;
  dr = gfc_default_real_kind;
  dc = gfc_default_character_kind;
  dl = gfc_default_logical_kind;
  ii = gfc_index_integer_kind;

  add_sym_0s ("abort", GFC_ISYM_ABORT, GFC_STD_GNU, NULL);

  make_noreturn();

  add_sym_3s ("atomic_define", GFC_ISYM_ATOMIC_DEF, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2008,
	      gfc_check_atomic_def, NULL, gfc_resolve_atomic_def,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "value", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("atomic_ref", GFC_ISYM_ATOMIC_REF, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2008,
	      gfc_check_atomic_ref, NULL, gfc_resolve_atomic_ref,
	      "value", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_5s ("atomic_cas", GFC_ISYM_ATOMIC_CAS, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_atomic_cas, NULL, NULL,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_INOUT,
	      "old", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "compare", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      "new", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("atomic_add", GFC_ISYM_ATOMIC_ADD, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_atomic_op, NULL, NULL,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "value", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("atomic_and", GFC_ISYM_ATOMIC_AND, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_atomic_op, NULL, NULL,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "value", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("atomic_or", GFC_ISYM_ATOMIC_OR, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_atomic_op, NULL, NULL,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "value", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("atomic_xor", GFC_ISYM_ATOMIC_XOR, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_atomic_op, NULL, NULL,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "value", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_4s ("atomic_fetch_add", GFC_ISYM_ATOMIC_FETCH_ADD, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_atomic_fetch_op, NULL, NULL,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "value", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      "old", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_4s ("atomic_fetch_and", GFC_ISYM_ATOMIC_FETCH_AND, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_atomic_fetch_op, NULL, NULL,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "value", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      "old", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_4s ("atomic_fetch_or", GFC_ISYM_ATOMIC_FETCH_OR, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_atomic_fetch_op, NULL, NULL,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "value", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      "old", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_4s ("atomic_fetch_xor", GFC_ISYM_ATOMIC_FETCH_XOR, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_atomic_fetch_op, NULL, NULL,
	      "atom", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      "value", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      "old", BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_0s ("backtrace", GFC_ISYM_BACKTRACE, GFC_STD_GNU, NULL);

  add_sym_1s ("cpu_time", GFC_ISYM_CPU_TIME, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_F95, gfc_check_cpu_time, NULL, gfc_resolve_cpu_time,
	      tm, BT_REAL, dr, REQUIRED, INTENT_OUT);

  add_sym_3s ("event_query", GFC_ISYM_EVENT_QUERY, CLASS_ATOMIC,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_event_query, NULL, gfc_resolve_event_query,
	      "event", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      c, BT_INTEGER, di, OPTIONAL, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  /* More G77 compatibility garbage.  */
  add_sym_2s ("ctime", GFC_ISYM_CTIME, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_ctime_sub, NULL, gfc_resolve_ctime_sub,
	      tm, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      res, BT_CHARACTER, dc, REQUIRED, INTENT_OUT);

  add_sym_1s ("idate", GFC_ISYM_IDATE, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_itime_idate, NULL, gfc_resolve_idate,
	      vl, BT_INTEGER, 4, REQUIRED, INTENT_OUT);

  add_sym_1s ("itime", GFC_ISYM_ITIME, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_itime_idate, NULL, gfc_resolve_itime,
	      vl, BT_INTEGER, 4, REQUIRED, INTENT_OUT);

  add_sym_2s ("ltime", GFC_ISYM_LTIME, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_ltime_gmtime, NULL, gfc_resolve_ltime,
	      tm, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      vl, BT_INTEGER, di, REQUIRED, INTENT_OUT);

  add_sym_2s ("gmtime", GFC_ISYM_GMTIME, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_GNU, gfc_check_ltime_gmtime, NULL, gfc_resolve_gmtime,
	      tm, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      vl, BT_INTEGER, di, REQUIRED, INTENT_OUT);

  add_sym_1s ("second", GFC_ISYM_SECOND, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_GNU, gfc_check_second_sub, NULL, gfc_resolve_second_sub,
	      tm, BT_REAL, dr, REQUIRED, INTENT_OUT);

  add_sym_2s ("chdir", GFC_ISYM_CHDIR, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_chdir_sub, NULL, gfc_resolve_chdir_sub,
	      name, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("chmod", GFC_ISYM_CHMOD, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_chmod_sub, NULL, gfc_resolve_chmod_sub,
	      name, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      md, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_4s ("date_and_time", GFC_ISYM_DATE_AND_TIME, CLASS_IMPURE, BT_UNKNOWN,
	      0, GFC_STD_F95, gfc_check_date_and_time, NULL, NULL,
	      dt, BT_CHARACTER, dc, OPTIONAL, INTENT_OUT,
	      tm, BT_CHARACTER, dc, OPTIONAL, INTENT_OUT,
	      zn, BT_CHARACTER, dc, OPTIONAL, INTENT_OUT,
	      vl, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  /* More G77 compatibility garbage.  */
  add_sym_2s ("etime", GFC_ISYM_ETIME, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_dtime_etime_sub, NULL, gfc_resolve_etime_sub,
	      vl, BT_REAL, 4, REQUIRED, INTENT_OUT,
	      tm, BT_REAL, 4, REQUIRED, INTENT_OUT);

  add_sym_2s ("dtime", GFC_ISYM_DTIME, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_dtime_etime_sub, NULL, gfc_resolve_dtime_sub,
	      vl, BT_REAL, 4, REQUIRED, INTENT_OUT,
	      tm, BT_REAL, 4, REQUIRED, INTENT_OUT);

  add_sym_5s ("execute_command_line", GFC_ISYM_EXECUTE_COMMAND_LINE,
	      CLASS_IMPURE , BT_UNKNOWN, 0, GFC_STD_F2008,
	      NULL, NULL, gfc_resolve_execute_command_line,
	      "command", BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      "wait", BT_LOGICAL, dl, OPTIONAL, INTENT_IN,
	      "exitstat", BT_INTEGER, di, OPTIONAL, INTENT_INOUT,
	      "cmdstat", BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      "cmdmsg", BT_CHARACTER, dc, OPTIONAL, INTENT_INOUT);

  add_sym_1s ("fdate", GFC_ISYM_FDATE, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_fdate_sub, NULL, gfc_resolve_fdate_sub,
	      dt, BT_CHARACTER, dc, REQUIRED, INTENT_OUT);

  add_sym_1s ("gerror", GFC_ISYM_GERROR, CLASS_IMPURE, BT_UNKNOWN,
	      0, GFC_STD_GNU, gfc_check_gerror, NULL, gfc_resolve_gerror,
	      res, BT_CHARACTER, dc, REQUIRED, INTENT_OUT);

  add_sym_2s ("getcwd", GFC_ISYM_GETCWD, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_GNU, gfc_check_getcwd_sub, NULL, gfc_resolve_getcwd_sub,
	      c, BT_CHARACTER, dc, REQUIRED, INTENT_OUT,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_2s ("getenv", GFC_ISYM_GETENV, CLASS_IMPURE, BT_UNKNOWN,
	      0, GFC_STD_GNU, NULL, NULL, NULL,
	      name, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      val, BT_CHARACTER, dc, REQUIRED, INTENT_OUT);

  add_sym_2s ("getarg", GFC_ISYM_GETARG, CLASS_IMPURE, BT_UNKNOWN,
	      0, GFC_STD_GNU, gfc_check_getarg, NULL, gfc_resolve_getarg,
	      pos, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      val, BT_CHARACTER, dc, REQUIRED, INTENT_OUT);

  add_sym_1s ("getlog", GFC_ISYM_GETLOG, CLASS_IMPURE, BT_UNKNOWN,
	      0, GFC_STD_GNU, gfc_check_getlog, NULL, gfc_resolve_getlog,
	      c, BT_CHARACTER, dc, REQUIRED, INTENT_OUT);

  /* F2003 commandline routines.  */

  add_sym_3s ("get_command", GFC_ISYM_GET_COMMAND, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F2003,
	      NULL, NULL, gfc_resolve_get_command,
	      com, BT_CHARACTER, dc, OPTIONAL, INTENT_OUT,
	      length, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_4s ("get_command_argument", GFC_ISYM_GET_COMMAND_ARGUMENT,
	      CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_F2003, NULL, NULL,
	      gfc_resolve_get_command_argument,
	      num, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      val, BT_CHARACTER, dc, OPTIONAL, INTENT_OUT,
	      length, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  /* F2003 subroutine to get environment variables.  */

  add_sym_5s ("get_environment_variable", GFC_ISYM_GET_ENVIRONMENT_VARIABLE,
	      CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_F2003,
	      NULL, NULL, gfc_resolve_get_environment_variable,
	      name, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      val, BT_CHARACTER, dc, OPTIONAL, INTENT_OUT,
	      length, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      trim_name, BT_LOGICAL, dl, OPTIONAL, INTENT_IN);

  add_sym_2s ("move_alloc", GFC_ISYM_MOVE_ALLOC, CLASS_PURE, BT_UNKNOWN, 0,
	      GFC_STD_F2003,
	      gfc_check_move_alloc, NULL, NULL,
	      f, BT_UNKNOWN, 0, REQUIRED, INTENT_INOUT,
	      t, BT_UNKNOWN, 0, REQUIRED, INTENT_OUT);

  add_sym_5s ("mvbits", GFC_ISYM_MVBITS, CLASS_ELEMENTAL, BT_UNKNOWN, 0,
	      GFC_STD_F95, gfc_check_mvbits, NULL, gfc_resolve_mvbits,
	      f, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      fp, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      ln, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      t, BT_INTEGER, di, REQUIRED, INTENT_INOUT,
	      tp, BT_INTEGER, di, REQUIRED, INTENT_IN);

  if (flag_dec_intrinsic_ints)
    {
      make_alias ("bmvbits", GFC_STD_GNU);
      make_alias ("imvbits", GFC_STD_GNU);
      make_alias ("jmvbits", GFC_STD_GNU);
      make_alias ("kmvbits", GFC_STD_GNU);
    }

  add_sym_2s ("random_init", GFC_ISYM_RANDOM_INIT, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_random_init, NULL, gfc_resolve_random_init,
	      "repeatable",     BT_LOGICAL, dl, REQUIRED, INTENT_IN,
	      "image_distinct", BT_LOGICAL, dl, REQUIRED, INTENT_IN);

  add_sym_1s ("random_number", GFC_ISYM_RANDOM_NUMBER, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F95,
	      gfc_check_random_number, NULL, gfc_resolve_random_number,
	      h, BT_REAL, dr, REQUIRED, INTENT_OUT);

  add_sym_3s ("random_seed", GFC_ISYM_RANDOM_SEED, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F95,
	      gfc_check_random_seed, NULL, gfc_resolve_random_seed,
	      sz, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      pt, BT_INTEGER, di, OPTIONAL, INTENT_IN,
	      gt, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  /* The following subroutines are part of ISO_C_BINDING.  */

  add_sym_3s ("c_f_pointer", GFC_ISYM_C_F_POINTER, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_F2003, gfc_check_c_f_pointer, NULL, NULL,
	      "cptr", BT_VOID, 0, REQUIRED, INTENT_IN,
	      "fptr", BT_UNKNOWN, 0, REQUIRED, INTENT_OUT,
	      "shape", BT_INTEGER, di, OPTIONAL, INTENT_IN);
  make_from_module();

  add_sym_2s ("c_f_procpointer", GFC_ISYM_C_F_PROCPOINTER, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F2003, gfc_check_c_f_procpointer,
	      NULL, NULL,
	      "cptr", BT_VOID, 0, REQUIRED, INTENT_IN,
	      "fptr", BT_UNKNOWN, 0, REQUIRED, INTENT_OUT);
  make_from_module();

  /* Internal subroutine for emitting a runtime error.  */

  add_sym_1p ("fe_runtime_error", GFC_ISYM_FE_RUNTIME_ERROR, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_fe_runtime_error, NULL, gfc_resolve_fe_runtime_error,
	      "msg", BT_CHARACTER, dc, REQUIRED, INTENT_IN);

  make_noreturn ();
  make_vararg ();
  make_from_module ();

  /* Coarray collectives.  */
  add_sym_4s ("co_broadcast", GFC_ISYM_CO_BROADCAST, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_co_broadcast, NULL, NULL,
	      a, BT_REAL, dr, REQUIRED, INTENT_INOUT,
	      "source_image", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      errmsg, BT_CHARACTER, dc, OPTIONAL, INTENT_INOUT);

  add_sym_4s ("co_max", GFC_ISYM_CO_MAX, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_co_minmax, NULL, NULL,
	      a, BT_REAL, dr, REQUIRED, INTENT_INOUT,
	      result_image, BT_INTEGER, di, OPTIONAL, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      errmsg, BT_CHARACTER, dc, OPTIONAL, INTENT_INOUT);

  add_sym_4s ("co_min", GFC_ISYM_CO_MIN, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_co_minmax, NULL, NULL,
	      a, BT_REAL, dr, REQUIRED, INTENT_INOUT,
	      result_image, BT_INTEGER, di, OPTIONAL, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      errmsg, BT_CHARACTER, dc, OPTIONAL, INTENT_INOUT);

  add_sym_4s ("co_sum", GFC_ISYM_CO_SUM, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_co_sum, NULL, NULL,
	      a, BT_REAL, dr, REQUIRED, INTENT_INOUT,
	      result_image, BT_INTEGER, di, OPTIONAL, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      errmsg, BT_CHARACTER, dc, OPTIONAL, INTENT_INOUT);

  add_sym_5s ("co_reduce", GFC_ISYM_CO_REDUCE, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F2018,
	      gfc_check_co_reduce, NULL, NULL,
	      a, BT_REAL, dr, REQUIRED, INTENT_INOUT,
	      "operator", BT_INTEGER, di, REQUIRED, INTENT_IN,
	      result_image, BT_INTEGER, di, OPTIONAL, INTENT_IN,
	      stat, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      errmsg, BT_CHARACTER, dc, OPTIONAL, INTENT_INOUT);


  /* The following subroutine is internally used for coarray libray functions.
     "make_from_module" makes it inaccessible for external users.  */
  add_sym_2s (GFC_PREFIX ("caf_send"), GFC_ISYM_CAF_SEND, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_GNU, NULL, NULL, NULL,
	      "x", BT_REAL, dr, REQUIRED, INTENT_OUT,
	      "y", BT_REAL, dr, REQUIRED, INTENT_IN);
  make_from_module();


  /* More G77 compatibility garbage.  */
  add_sym_3s ("alarm", GFC_ISYM_ALARM, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_alarm_sub, NULL, gfc_resolve_alarm_sub,
	      sec, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      han, BT_UNKNOWN, 0, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_1s ("srand", GFC_ISYM_SRAND, CLASS_IMPURE, BT_UNKNOWN,
	      di, GFC_STD_GNU, gfc_check_srand, NULL, gfc_resolve_srand,
	      "seed", BT_INTEGER, 4, REQUIRED, INTENT_IN);

  add_sym_1s ("exit", GFC_ISYM_EXIT, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_exit, NULL, gfc_resolve_exit,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_IN);

  make_noreturn();

  add_sym_3s ("fgetc", GFC_ISYM_FGETC, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_fgetputc_sub, NULL, gfc_resolve_fgetc_sub,
	      ut, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      c_, BT_CHARACTER, dc, REQUIRED, INTENT_OUT,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_2s ("fget", GFC_ISYM_FGET, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_fgetput_sub, NULL, gfc_resolve_fget_sub,
	      c_, BT_CHARACTER, dc, REQUIRED, INTENT_OUT,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_1s ("flush", GFC_ISYM_FLUSH, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_flush, NULL, gfc_resolve_flush,
	      ut, BT_INTEGER, di, OPTIONAL, INTENT_IN);

  add_sym_3s ("fputc", GFC_ISYM_FPUTC, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_fgetputc_sub, NULL, gfc_resolve_fputc_sub,
	      ut, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      c_, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_2s ("fput", GFC_ISYM_FPUT, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_fgetput_sub, NULL, gfc_resolve_fput_sub,
	      c_, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_1s ("free", GFC_ISYM_FREE, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_free, NULL, NULL,
	      ptr, BT_INTEGER, ii, REQUIRED, INTENT_INOUT);

  add_sym_4s ("fseek", GFC_ISYM_FSEEK, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_fseek_sub, NULL, gfc_resolve_fseek_sub,
	      ut, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      of, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      whence, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_2s ("ftell", GFC_ISYM_FTELL, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_ftell_sub, NULL, gfc_resolve_ftell_sub,
	      ut, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      of, BT_INTEGER, ii, REQUIRED, INTENT_OUT);

  add_sym_2s ("hostnm", GFC_ISYM_HOSTNM, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_GNU, gfc_check_hostnm_sub, NULL, gfc_resolve_hostnm_sub,
	      c, BT_CHARACTER, dc, REQUIRED, INTENT_OUT,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("kill", GFC_ISYM_KILL, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_kill_sub, NULL, NULL,
	      pid, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      sig, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("link", GFC_ISYM_LINK, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_link_sub, NULL, gfc_resolve_link_sub,
	      p1, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      p2, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_1s ("perror", GFC_ISYM_PERROR, CLASS_IMPURE, BT_UNKNOWN,
	      0, GFC_STD_GNU, gfc_check_perror, NULL, gfc_resolve_perror,
	      "string", BT_CHARACTER, dc, REQUIRED, INTENT_IN);

  add_sym_3s ("rename", GFC_ISYM_RENAME, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_GNU, gfc_check_rename_sub, NULL, gfc_resolve_rename_sub,
	      p1, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      p2, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_1s ("sleep", GFC_ISYM_SLEEP, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_sleep_sub, NULL, gfc_resolve_sleep_sub,
	      sec, BT_INTEGER, di, REQUIRED, INTENT_IN);

  add_sym_3s ("fstat", GFC_ISYM_FSTAT, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_fstat_sub, NULL, gfc_resolve_fstat_sub,
	      ut, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      vl, BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("lstat", GFC_ISYM_LSTAT, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_stat_sub, NULL, gfc_resolve_lstat_sub,
	      name, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      vl, BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("stat", GFC_ISYM_STAT, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_stat_sub, NULL, gfc_resolve_stat_sub,
	      name, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      vl, BT_INTEGER, di, REQUIRED, INTENT_OUT,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("signal", GFC_ISYM_SIGNAL, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_GNU, gfc_check_signal_sub, NULL, gfc_resolve_signal_sub,
	      num, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      han, BT_UNKNOWN, 0, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("symlnk", GFC_ISYM_SYMLINK, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_GNU, gfc_check_symlnk_sub, NULL, gfc_resolve_symlnk_sub,
	      p1, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      p2, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_2s ("system", GFC_ISYM_SYSTEM, CLASS_IMPURE, BT_UNKNOWN,
	      0, GFC_STD_GNU, NULL, NULL, gfc_resolve_system_sub,
	      com, BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_3s ("system_clock", GFC_ISYM_SYSTEM_CLOCK, CLASS_IMPURE,
	      BT_UNKNOWN, 0, GFC_STD_F95,
	      gfc_check_system_clock, NULL, gfc_resolve_system_clock,
	      c, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      cr, BT_INTEGER, di, OPTIONAL, INTENT_OUT,
	      cm, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_2s ("ttynam", GFC_ISYM_TTYNAM, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_GNU, gfc_check_ttynam_sub, NULL, gfc_resolve_ttynam_sub,
	      ut, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      name, BT_CHARACTER, dc, REQUIRED, INTENT_OUT);

  add_sym_2s ("umask", GFC_ISYM_UMASK, CLASS_IMPURE, BT_UNKNOWN, 0, GFC_STD_GNU,
	      gfc_check_umask_sub, NULL, gfc_resolve_umask_sub,
	      msk, BT_INTEGER, di, REQUIRED, INTENT_IN,
	      old, BT_INTEGER, di, OPTIONAL, INTENT_OUT);

  add_sym_2s ("unlink", GFC_ISYM_UNLINK, CLASS_IMPURE, BT_UNKNOWN, 0,
	      GFC_STD_GNU, gfc_check_unlink_sub, NULL, gfc_resolve_unlink_sub,
	      "path", BT_CHARACTER, dc, REQUIRED, INTENT_IN,
	      st, BT_INTEGER, di, OPTIONAL, INTENT_OUT);
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
  sym->pure = 1;
  sym->conversion = 1;
  sym->ts = to;
  sym->id = GFC_ISYM_CONVERSION;

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

  /* DEC legacy feature allows character conversions similar to Hollerith
     conversions - the character data will transferred on a byte by byte
     basis.  */
  if (flag_dec_char_conversions)
    {
      /* Character-Integer conversions.  */
      for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
	add_conv (BT_CHARACTER, gfc_default_character_kind,
		  BT_INTEGER, gfc_integer_kinds[i].kind, GFC_STD_LEGACY);
      /* Character-Real conversions.  */
      for (i = 0; gfc_real_kinds[i].kind != 0; i++)
	add_conv (BT_CHARACTER, gfc_default_character_kind,
		  BT_REAL, gfc_real_kinds[i].kind, GFC_STD_LEGACY);
      /* Character-Complex conversions.  */
      for (i = 0; gfc_real_kinds[i].kind != 0; i++)
	add_conv (BT_CHARACTER, gfc_default_character_kind,
		  BT_COMPLEX, gfc_real_kinds[i].kind, GFC_STD_LEGACY);
      /* Character-Logical conversions.  */
      for (i = 0; gfc_logical_kinds[i].kind != 0; i++)
	add_conv (BT_CHARACTER, gfc_default_character_kind,
		  BT_LOGICAL, gfc_logical_kinds[i].kind, GFC_STD_LEGACY);
    }
}


static void
add_char_conversions (void)
{
  int n, i, j;

  /* Count possible conversions.  */
  for (i = 0; gfc_character_kinds[i].kind != 0; i++)
    for (j = 0; gfc_character_kinds[j].kind != 0; j++)
      if (i != j)
	ncharconv++;

  /* Allocate memory.  */
  char_conversions = XCNEWVEC (gfc_intrinsic_sym, ncharconv);

  /* Add the conversions themselves.  */
  n = 0;
  for (i = 0; gfc_character_kinds[i].kind != 0; i++)
    for (j = 0; gfc_character_kinds[j].kind != 0; j++)
      {
	gfc_typespec from, to;

	if (i == j)
	  continue;

	gfc_clear_ts (&from);
	from.type = BT_CHARACTER;
	from.kind = gfc_character_kinds[i].kind;

	gfc_clear_ts (&to);
	to.type = BT_CHARACTER;
	to.kind = gfc_character_kinds[j].kind;

	char_conversions[n].name = conv_name (&from, &to);
	char_conversions[n].lib_name = char_conversions[n].name;
	char_conversions[n].simplify.cc = gfc_convert_char_constant;
	char_conversions[n].standard = GFC_STD_F2003;
	char_conversions[n].elemental = 1;
	char_conversions[n].pure = 1;
	char_conversions[n].conversion = 0;
	char_conversions[n].ts = to;
	char_conversions[n].id = GFC_ISYM_CONVERSION;

	n++;
      }
}


/* Initialize the table of intrinsics.  */
void
gfc_intrinsic_init_1 (void)
{
  nargs = nfunc = nsub = nconv = 0;

  /* Create a namespace to hold the resolved intrinsic symbols.  */
  gfc_intrinsic_namespace = gfc_get_namespace (NULL, 0);

  sizing = SZ_FUNCS;
  add_functions ();
  sizing = SZ_SUBS;
  add_subroutines ();
  sizing = SZ_CONVS;
  add_conversions ();

  functions = XCNEWVAR (struct gfc_intrinsic_sym,
			sizeof (gfc_intrinsic_sym) * (nfunc + nsub)
			+ sizeof (gfc_intrinsic_arg) * nargs);

  next_sym = functions;
  subroutines = functions + nfunc;

  conversion = XCNEWVEC (gfc_intrinsic_sym, nconv);

  next_arg = ((gfc_intrinsic_arg *) (subroutines + nsub)) - 1;

  sizing = SZ_NOTHING;
  nconv = 0;

  add_functions ();
  add_subroutines ();
  add_conversions ();

  /* Character conversion intrinsics need to be treated separately.  */
  add_char_conversions ();
}


void
gfc_intrinsic_done_1 (void)
{
  free (functions);
  free (conversion);
  free (char_conversions);
  gfc_free_namespace (gfc_intrinsic_namespace);
}


/******** Subroutines to check intrinsic interfaces ***********/

/* Given a formal argument list, remove any NULL arguments that may
   have been left behind by a sort against some formal argument list.  */

static void
remove_nullargs (gfc_actual_arglist **ap)
{
  gfc_actual_arglist *head, *tail, *next;

  tail = NULL;

  for (head = *ap; head; head = next)
    {
      next = head->next;

      if (head->expr == NULL && !head->label)
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
   return false.  */

static bool
sort_actual (const char *name, gfc_actual_arglist **ap,
	     gfc_intrinsic_arg *formal, locus *where)
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
    return true;

  /* ALLOCATED has two mutually exclusive keywords, but only one
     can be present at time and neither is optional. */
  if (strcmp (name, "allocated") == 0)
    {
      if (!a)
	{
	  gfc_error ("ALLOCATED intrinsic at %L requires an array or scalar "
		     "allocatable entity", where);
	  return false;
	}

      if (a->name)
	{
	  if (strcmp (a->name, "scalar") == 0)
	    {
	      if (a->next)
		goto whoops;
	      if (a->expr->rank != 0)
		{
		  gfc_error ("Scalar entity required at %L", &a->expr->where);
		  return false;
		}
	      return true;
	    }
	  else if (strcmp (a->name, "array") == 0)
	    {
	      if (a->next)
		goto whoops;
	      if (a->expr->rank == 0)
		{
		  gfc_error ("Array entity required at %L", &a->expr->where);
		  return false;
		}
	      return true;
	    }
	  else
	    {
	      gfc_error ("Invalid keyword %qs in %qs intrinsic function at %L",
			 a->name, name, &a->expr->where);
	      return false;
	    }
	}
    }

  for (;;)
    {		/* Put the nonkeyword arguments in a 1:1 correspondence */
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

whoops:
  gfc_error ("Too many arguments in call to %qs at %L", name, where);
  return false;

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
	  if (a->name[0] == '%')
	    gfc_error ("The argument list functions %%VAL, %%LOC or %%REF "
		       "are not allowed in this context at %L", where);
	  else
	    gfc_error ("Cannot find keyword named %qs in call to %qs at %L",
		       a->name, name, where);
	  return false;
	}

      if (f->actual != NULL)
	{
	  gfc_error ("Argument %qs appears twice in call to %qs at %L",
		     f->name, name, where);
	  return false;
	}

      f->actual = a;
    }

optional:
  /* At this point, all unmatched formal args must be optional.  */
  for (f = formal; f; f = f->next)
    {
      if (f->actual == NULL && f->optional == 0)
	{
	  gfc_error ("Missing actual argument %qs in call to %qs at %L",
		     f->name, name, where);
	  return false;
	}
    }

do_sort:
  /* Using the formal argument list, string the actual argument list
     together in a way that corresponds with the formal list.  */
  actual = NULL;

  for (f = formal; f; f = f->next)
    {
      if (f->actual && f->actual->label != NULL && f->ts.type)
	{
	  gfc_error ("ALTERNATE RETURN not permitted at %L", where);
	  return false;
	}

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

  return true;
}


/* Compare an actual argument list with an intrinsic's formal argument
   list.  The lists are checked for agreement of type.  We don't check
   for arrayness here.  */

static bool
check_arglist (gfc_actual_arglist **ap, gfc_intrinsic_sym *sym,
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
      gfc_typespec ts;

      if (actual->expr == NULL)
	continue;

      ts = formal->ts;

      /* A kind of 0 means we don't check for kind.  */
      if (ts.kind == 0)
	ts.kind = actual->expr->ts.kind;

      if (!gfc_compare_types (&ts, &actual->expr->ts))
	{
	  if (error_flag)
	    gfc_error ("In call to %qs at %L, type mismatch in argument "
		       "%qs; pass %qs to %qs", gfc_current_intrinsic,
		       &actual->expr->where,
		       gfc_current_intrinsic_arg[i]->name,
		       gfc_typename (actual->expr),
		       gfc_dummy_typename (&formal->ts));
	  return false;
	}

      /* F2018, p. 328: An argument to an intrinsic procedure other than
	 ASSOCIATED, NULL, or PRESENT shall be a data object.  An EXPR_NULL
	 is not a data object.  */
      if (actual->expr->expr_type == EXPR_NULL
	  && (!(sym->id == GFC_ISYM_ASSOCIATED
		|| sym->id == GFC_ISYM_NULL
		|| sym->id == GFC_ISYM_PRESENT)))
	{
	  gfc_invalid_null_arg (actual->expr);
	  return false;
	}

      /* If the formal argument is INTENT([IN]OUT), check for definability.  */
      if (formal->intent == INTENT_INOUT || formal->intent == INTENT_OUT)
	{
	  const char* context = (error_flag
				 ? _("actual argument to INTENT = OUT/INOUT")
				 : NULL);

	  /* No pointer arguments for intrinsics.  */
	  if (!gfc_check_vardef_context (actual->expr, false, false, false, context))
	    return false;
	}
    }

  return true;
}


/* Given a pointer to an intrinsic symbol and an expression node that
   represent the function call to that subroutine, figure out the type
   of the result.  This may involve calling a resolution subroutine.  */

static void
resolve_intrinsic (gfc_intrinsic_sym *specific, gfc_expr *e)
{
  gfc_expr *a1, *a2, *a3, *a4, *a5, *a6;
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

  /* Special case hacks for MIN, MAX and INDEX.  */
  if (specific->resolve.f1m == gfc_resolve_max
      || specific->resolve.f1m == gfc_resolve_min
      || specific->resolve.f1m == gfc_resolve_index_func)
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

  a6 = arg->expr;
  arg = arg->next;

  if (arg == NULL)
    {
      (*specific->resolve.f6) (e, a1, a2, a3, a4, a5, a6);
      return;
    }

  gfc_internal_error ("resolve_intrinsic(): Too many args for intrinsic");
}


/* Given an intrinsic symbol node and an expression node, call the
   simplification function (if there is one), perhaps replacing the
   expression with something simpler.  We return false on an error
   of the simplification, true if the simplification worked, even
   if nothing has changed in the expression itself.  */

static bool
do_simplify (gfc_intrinsic_sym *specific, gfc_expr *e)
{
  gfc_expr *result, *a1, *a2, *a3, *a4, *a5, *a6;
  gfc_actual_arglist *arg;

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

  if (specific->simplify.cc == gfc_convert_constant
      || specific->simplify.cc == gfc_convert_char_constant)
    {
      result = specific->simplify.cc (a1, specific->ts.type, specific->ts.kind);
      goto finish;
    }

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
		    {
		      a6 = arg->expr;
		      arg = arg->next;

		      if (arg == NULL)
			result = (*specific->simplify.f6)
		       			(a1, a2, a3, a4, a5, a6);
		      else
			gfc_internal_error
			  ("do_simplify(): Too many args for intrinsic");
		    }
		}
	    }
	}
    }

finish:
  if (result == &gfc_bad_expr)
    return false;

  if (result == NULL)
    resolve_intrinsic (specific, e);	/* Must call at run-time */
  else
    {
      result->where = e->where;
      gfc_replace_expr (e, result);
    }

  return true;
}


/* Initialize the gfc_current_intrinsic_arg[] array for the benefit of
   error messages.  This subroutine returns false if a subroutine
   has more than MAX_INTRINSIC_ARGS, in which case the actual argument
   list cannot match any intrinsic.  */

static void
init_arglist (gfc_intrinsic_sym *isym)
{
  gfc_intrinsic_arg *formal;
  int i;

  gfc_current_intrinsic = isym->name;

  i = 0;
  for (formal = isym->formal; formal; formal = formal->next)
    {
      if (i >= MAX_INTRINSIC_ARGS)
	gfc_internal_error ("init_arglist(): too many arguments");
      gfc_current_intrinsic_arg[i++] = formal;
    }
}


/* Given a pointer to an intrinsic symbol and an expression consisting
   of a function call, see if the function call is consistent with the
   intrinsic's formal argument list.  Return true if the expression
   and intrinsic match, false otherwise.  */

static bool
check_specific (gfc_intrinsic_sym *specific, gfc_expr *expr, int error_flag)
{
  gfc_actual_arglist *arg, **ap;
  bool t;

  ap = &expr->value.function.actual;

  init_arglist (specific);

  /* Don't attempt to sort the argument list for min or max.  */
  if (specific->check.f1m == gfc_check_min_max
      || specific->check.f1m == gfc_check_min_max_integer
      || specific->check.f1m == gfc_check_min_max_real
      || specific->check.f1m == gfc_check_min_max_double)
    {
      if (!do_ts29113_check (specific, *ap))
	return false;
      return (*specific->check.f1m) (*ap);
    }

  if (!sort_actual (specific->name, ap, specific->formal, &expr->where))
    return false;

  if (!do_ts29113_check (specific, *ap))
    return false;

  if (specific->check.f5ml == gfc_check_minloc_maxloc)
    /* This is special because we might have to reorder the argument list.  */
    t = gfc_check_minloc_maxloc (*ap);
  else if (specific->check.f6fl == gfc_check_findloc)
    t = gfc_check_findloc (*ap);
  else if (specific->check.f3red == gfc_check_minval_maxval)
    /* This is also special because we also might have to reorder the
       argument list.  */
    t = gfc_check_minval_maxval (*ap);
  else if (specific->check.f3red == gfc_check_product_sum)
    /* Same here. The difference to the previous case is that we allow a
       general numeric type.  */
    t = gfc_check_product_sum (*ap);
  else if (specific->check.f3red == gfc_check_transf_bit_intrins)
    /* Same as for PRODUCT and SUM, but different checks.  */
    t = gfc_check_transf_bit_intrins (*ap);
  else
     {
       if (specific->check.f1 == NULL)
	 {
	   t = check_arglist (ap, specific, error_flag);
	   if (t)
	     expr->ts = specific->ts;
	 }
       else
	 t = do_check (specific, *ap);
     }

  /* Check conformance of elemental intrinsics.  */
  if (t && specific->elemental)
    {
      int n = 0;
      gfc_expr *first_expr;
      arg = expr->value.function.actual;

      /* There is no elemental intrinsic without arguments.  */
      gcc_assert(arg != NULL);
      first_expr = arg->expr;

      for ( ; arg && arg->expr; arg = arg->next, n++)
	if (!gfc_check_conformance (first_expr, arg->expr,
				    _("arguments '%s' and '%s' for "
				    "intrinsic '%s'"),
				    gfc_current_intrinsic_arg[0]->name,
				    gfc_current_intrinsic_arg[n]->name,
				    gfc_current_intrinsic))
	  return false;
    }

  if (!t)
    remove_nullargs (ap);

  return t;
}


/* Check whether an intrinsic belongs to whatever standard the user
   has chosen, taking also into account -fall-intrinsics.  Here, no
   warning/error is emitted; but if symstd is not NULL, it is pointed to a
   textual representation of the symbols standard status (like
   "new in Fortran 2008", "a GNU extension" or "obsolescent in Fortran 95") that
   can be used to construct a detailed warning/error message in case of
   a false.  */

bool
gfc_check_intrinsic_standard (const gfc_intrinsic_sym* isym,
			      const char** symstd, bool silent, locus where)
{
  const char* symstd_msg;

  /* For -fall-intrinsics, just succeed.  */
  if (flag_all_intrinsics)
    return true;

  /* Find the symbol's standard message for later usage.  */
  switch (isym->standard)
    {
    case GFC_STD_F77:
      symstd_msg = _("available since Fortran 77");
      break;

    case GFC_STD_F95_OBS:
      symstd_msg = _("obsolescent in Fortran 95");
      break;

    case GFC_STD_F95_DEL:
      symstd_msg = _("deleted in Fortran 95");
      break;

    case GFC_STD_F95:
      symstd_msg = _("new in Fortran 95");
      break;

    case GFC_STD_F2003:
      symstd_msg = _("new in Fortran 2003");
      break;

    case GFC_STD_F2008:
      symstd_msg = _("new in Fortran 2008");
      break;

    case GFC_STD_F2018:
      symstd_msg = _("new in Fortran 2018");
      break;

    case GFC_STD_GNU:
      symstd_msg = _("a GNU Fortran extension");
      break;

    case GFC_STD_LEGACY:
      symstd_msg = _("for backward compatibility");
      break;

    default:
      gfc_internal_error ("Invalid standard code on intrinsic %qs (%d)",
			  isym->name, isym->standard);
    }

  /* If warning about the standard, warn and succeed.  */
  if (gfc_option.warn_std & isym->standard)
    {
      /* Do only print a warning if not a GNU extension.  */
      if (!silent && isym->standard != GFC_STD_GNU)
	gfc_warning (0, "Intrinsic %qs (%s) used at %L",
		     isym->name, symstd_msg, &where);

      return true;
    }

  /* If allowing the symbol's standard, succeed, too.  */
  if (gfc_option.allow_std & isym->standard)
    return true;

  /* Otherwise, fail.  */
  if (symstd)
    *symstd = symstd_msg;
  return false;
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
gfc_intrinsic_func_interface (gfc_expr *expr, int error_flag)
{
  gfc_symbol *sym;
  gfc_intrinsic_sym *isym, *specific;
  gfc_actual_arglist *actual;
  int flag;

  if (expr->value.function.isym != NULL)
    return (!do_simplify(expr->value.function.isym, expr))
	   ? MATCH_ERROR : MATCH_YES;

  if (!error_flag)
    gfc_push_suppress_errors ();
  flag = 0;

  for (actual = expr->value.function.actual; actual; actual = actual->next)
    if (actual->expr != NULL)
      flag |= (actual->expr->ts.type != BT_INTEGER
	       && actual->expr->ts.type != BT_CHARACTER);

  sym = expr->symtree->n.sym;

  if (sym->intmod_sym_id)
    {
      gfc_isym_id id = gfc_isym_id_by_intmod_sym (sym);
      isym = specific = gfc_intrinsic_function_by_id (id);
    }
  else
    isym = specific = gfc_find_function (sym->name);

  if (isym == NULL)
    {
      if (!error_flag)
	gfc_pop_suppress_errors ();
      return MATCH_NO;
    }

  if ((isym->id == GFC_ISYM_REAL || isym->id == GFC_ISYM_DBLE
       || isym->id == GFC_ISYM_CMPLX || isym->id == GFC_ISYM_FLOAT
       || isym->id == GFC_ISYM_SNGL || isym->id == GFC_ISYM_DFLOAT)
      && gfc_init_expr_flag
      && !gfc_notify_std (GFC_STD_F2003, "Function %qs as initialization "
			  "expression at %L", sym->name, &expr->where))
    {
      if (!error_flag)
	gfc_pop_suppress_errors ();
      return MATCH_ERROR;
    }

  /* F95, 7.1.6.1: Only transformational functions REPEAT, RESHAPE,
     SELECTED_INT_KIND, SELECTED_REAL_KIND, TRANSFER, and TRIM are allowed in
     initialization expressions.  */

  if (gfc_init_expr_flag && isym->transformational)
    {
      gfc_isym_id id = isym->id;
      if (id != GFC_ISYM_REPEAT && id != GFC_ISYM_RESHAPE
	  && id != GFC_ISYM_SI_KIND && id != GFC_ISYM_SR_KIND
	  && id != GFC_ISYM_TRANSFER && id != GFC_ISYM_TRIM
	  && !gfc_notify_std (GFC_STD_F2003, "Transformational function %qs "
			      "at %L is invalid in an initialization "
			      "expression", sym->name, &expr->where))
	{
	  if (!error_flag)
	    gfc_pop_suppress_errors ();

	  return MATCH_ERROR;
	}
    }

  gfc_current_intrinsic_where = &expr->where;

  /* Bypass the generic list for min, max and ISO_C_Binding's c_loc.  */
  if (isym->check.f1m == gfc_check_min_max)
    {
      init_arglist (isym);

      if (isym->check.f1m(expr->value.function.actual))
	goto got_specific;

      if (!error_flag)
	gfc_pop_suppress_errors ();
      return MATCH_NO;
    }

  /* If the function is generic, check all of its specific
     incarnations.  If the generic name is also a specific, we check
     that name last, so that any error message will correspond to the
     specific.  */
  gfc_push_suppress_errors ();

  if (isym->generic)
    {
      for (specific = isym->specific_head; specific;
	   specific = specific->next)
	{
	  if (specific == isym)
	    continue;
	  if (check_specific (specific, expr, 0))
	    {
	      gfc_pop_suppress_errors ();
	      goto got_specific;
	    }
	}
    }

  gfc_pop_suppress_errors ();

  if (!check_specific (isym, expr, error_flag))
    {
      if (!error_flag)
	gfc_pop_suppress_errors ();
      return MATCH_NO;
    }

  specific = isym;

got_specific:
  expr->value.function.isym = specific;
  if (!error_flag)
    gfc_pop_suppress_errors ();

  if (!do_simplify (specific, expr))
    return MATCH_ERROR;

  /* F95, 7.1.6.1, Initialization expressions
     (4) An elemental intrinsic function reference of type integer or
         character where each argument is an initialization expression
         of type integer or character

     F2003, 7.1.7 Initialization expression
     (4)   A reference to an elemental standard intrinsic function,
           where each argument is an initialization expression  */

  if (gfc_init_expr_flag && isym->elemental && flag
      && !gfc_notify_std (GFC_STD_F2003, "Elemental function as "
			  "initialization expression with non-integer/non-"
			  "character arguments at %L", &expr->where))
    return MATCH_ERROR;

  if (sym->attr.flavor == FL_UNKNOWN)
    {
      sym->attr.function = 1;
      sym->attr.intrinsic = 1;
      sym->attr.flavor = FL_PROCEDURE;
    }
  if (sym->attr.flavor == FL_PROCEDURE)
    {
      sym->attr.function = 1;
      sym->attr.proc = PROC_INTRINSIC;
    }

  if (!sym->module)
    gfc_intrinsic_symbol (sym);

  /* Have another stab at simplification since elemental intrinsics with array
     actual arguments would be missed by the calls above to do_simplify.  */
  if (isym->elemental)
    gfc_simplify_expr (expr, 1);

  return MATCH_YES;
}


/* See if a CALL statement corresponds to an intrinsic subroutine.
   Returns MATCH_YES if the subroutine corresponds to an intrinsic,
   MATCH_NO if not, and MATCH_ERROR if there was an error (but did
   correspond).  */

match
gfc_intrinsic_sub_interface (gfc_code *c, int error_flag)
{
  gfc_intrinsic_sym *isym;
  const char *name;

  name = c->symtree->n.sym->name;

  if (c->symtree->n.sym->intmod_sym_id)
    {
      gfc_isym_id id;
      id = gfc_isym_id_by_intmod_sym (c->symtree->n.sym);
      isym = gfc_intrinsic_subroutine_by_id (id);
    }
  else
    isym = gfc_find_subroutine (name);
  if (isym == NULL)
    return MATCH_NO;

  if (!error_flag)
    gfc_push_suppress_errors ();

  init_arglist (isym);

  if (!isym->vararg && !sort_actual (name, &c->ext.actual, isym->formal, &c->loc))
    goto fail;

  if (!do_ts29113_check (isym, c->ext.actual))
    goto fail;

  if (isym->check.f1 != NULL)
    {
      if (!do_check (isym, c->ext.actual))
	goto fail;
    }
  else
    {
      if (!check_arglist (&c->ext.actual, isym, 1))
	goto fail;
    }

  /* The subroutine corresponds to an intrinsic.  Allow errors to be
     seen at this point.  */
  if (!error_flag)
    gfc_pop_suppress_errors ();

  c->resolved_isym = isym;
  if (isym->resolve.s1 != NULL)
    isym->resolve.s1 (c);
  else
    {
      c->resolved_sym = gfc_get_intrinsic_sub_symbol (isym->lib_name);
      c->resolved_sym->attr.elemental = isym->elemental;
    }

  if (gfc_do_concurrent_flag && !isym->pure)
    {
      gfc_error ("Subroutine call to intrinsic %qs in DO CONCURRENT "
		 "block at %L is not PURE", name, &c->loc);
      return MATCH_ERROR;
    }

  if (!isym->pure && gfc_pure (NULL))
    {
      gfc_error ("Subroutine call to intrinsic %qs at %L is not PURE", name,
		 &c->loc);
      return MATCH_ERROR;
    }

  if (!isym->pure)
    gfc_unset_implicit_pure (NULL);

  c->resolved_sym->attr.noreturn = isym->noreturn;

  return MATCH_YES;

fail:
  if (!error_flag)
    gfc_pop_suppress_errors ();
  return MATCH_NO;
}


/* Call gfc_convert_type() with warning enabled.  */

bool
gfc_convert_type (gfc_expr *expr, gfc_typespec *ts, int eflag)
{
  return gfc_convert_type_warn (expr, ts, eflag, 1);
}


/* Try to convert an expression (in place) from one type to another.
   'eflag' controls the behavior on error.

   The possible values are:

     1 Generate a gfc_error()
     2 Generate a gfc_internal_error().

   'wflag' controls the warning related to conversion.

   'array' indicates whether the conversion is in an array constructor.
   Non-standard conversion from character to numeric not allowed if true.
*/

bool
gfc_convert_type_warn (gfc_expr *expr, gfc_typespec *ts, int eflag, int wflag,
		       bool array)
{
  gfc_intrinsic_sym *sym;
  gfc_typespec from_ts;
  locus old_where;
  gfc_expr *new_expr;
  int rank;
  mpz_t *shape;
  bool is_char_constant = (expr->expr_type == EXPR_CONSTANT)
			  && (expr->ts.type == BT_CHARACTER);

  from_ts = expr->ts;		/* expr->ts gets clobbered */

  if (ts->type == BT_UNKNOWN)
    goto bad;

  expr->do_not_warn = ! wflag;

  /* NULL and zero size arrays get their type here, unless they already have a
     typespec.  */
  if ((expr->expr_type == EXPR_NULL
       || (expr->expr_type == EXPR_ARRAY && expr->value.constructor == NULL))
      && expr->ts.type == BT_UNKNOWN)
    {
      /* Sometimes the RHS acquire the type.  */
      expr->ts = *ts;
      return true;
    }

  if (expr->ts.type == BT_UNKNOWN)
    goto bad;

  /* In building an array constructor, gfortran can end up here when no
     conversion is required for an intrinsic type.  We need to let derived
     types drop through.  */
  if (from_ts.type != BT_DERIVED
      && (from_ts.type == ts->type && from_ts.kind == ts->kind))
    return true;

  if (expr->ts.type == BT_DERIVED && ts->type == BT_DERIVED
      && gfc_compare_types (&expr->ts, ts))
    return true;

  /* If array is true then conversion is in an array constructor where
     non-standard conversion is not allowed.  */
  if (array && from_ts.type == BT_CHARACTER
      && (gfc_numeric_ts (ts) || ts->type == BT_LOGICAL))
    goto bad;

  sym = find_conv (&expr->ts, ts);
  if (sym == NULL)
    goto bad;

  /* At this point, a conversion is necessary. A warning may be needed.  */
  if ((gfc_option.warn_std & sym->standard) != 0)
    {
      const char *type_name = is_char_constant ? gfc_typename (expr)
					       : gfc_typename (&from_ts);
      gfc_warning_now (0, "Extension: Conversion from %s to %s at %L",
		       type_name, gfc_dummy_typename (ts),
		       &expr->where);
    }
  else if (wflag)
    {
      if (flag_range_check && expr->expr_type == EXPR_CONSTANT
	  && from_ts.type == ts->type)
	{
	  /* Do nothing. Constants of the same type are range-checked
	     elsewhere. If a value too large for the target type is
	     assigned, an error is generated. Not checking here avoids
	     duplications of warnings/errors.
	     If range checking was disabled, but -Wconversion enabled,
	     a non range checked warning is generated below.  */
	}
      else if (flag_dec_char_conversions && from_ts.type == BT_CHARACTER
	       && (gfc_numeric_ts (ts) || ts->type == BT_LOGICAL))
	{
	  const char *type_name = is_char_constant ? gfc_typename (expr)
						   : gfc_typename (&from_ts);
	  gfc_warning_now (OPT_Wconversion, "Nonstandard conversion from %s "
			   "to %s at %L", type_name, gfc_typename (ts),
			   &expr->where);
	}
      else if (from_ts.type == ts->type
	       || (from_ts.type == BT_INTEGER && ts->type == BT_REAL)
	       || (from_ts.type == BT_INTEGER && ts->type == BT_COMPLEX)
	       || (from_ts.type == BT_REAL && ts->type == BT_COMPLEX))
	{
	  /* Larger kinds can hold values of smaller kinds without problems.
	     Hence, only warn if target kind is smaller than the source
	     kind - or if -Wconversion-extra is specified.  LOGICAL values
	     will always fit regardless of kind so ignore conversion.  */
	  if (expr->expr_type != EXPR_CONSTANT
	      && ts->type != BT_LOGICAL)
	    {
	      if (warn_conversion && from_ts.kind > ts->kind)
		gfc_warning_now (OPT_Wconversion, "Possible change of value in "
				 "conversion from %s to %s at %L",
				 gfc_typename (&from_ts), gfc_typename (ts),
				 &expr->where);
	      else
		gfc_warning_now (OPT_Wconversion_extra, "Conversion from %s to %s "
				 "at %L", gfc_typename (&from_ts),
				 gfc_typename (ts), &expr->where);
	    }
	}
      else if ((from_ts.type == BT_REAL && ts->type == BT_INTEGER)
	       || (from_ts.type == BT_COMPLEX && ts->type == BT_INTEGER)
	       || (from_ts.type == BT_COMPLEX && ts->type == BT_REAL))
	{
	  /* Conversion from REAL/COMPLEX to INTEGER or COMPLEX to REAL
	     usually comes with a loss of information, regardless of kinds.  */
	  if (expr->expr_type != EXPR_CONSTANT)
	    gfc_warning_now (OPT_Wconversion, "Possible change of value in "
			     "conversion from %s to %s at %L",
			     gfc_typename (&from_ts), gfc_typename (ts),
			     &expr->where);
	}
      else if (from_ts.type == BT_HOLLERITH || ts->type == BT_HOLLERITH)
	{
	  /* If HOLLERITH is involved, all bets are off.  */
	  gfc_warning_now (OPT_Wconversion, "Conversion from %s to %s at %L",
			   gfc_typename (&from_ts), gfc_dummy_typename (ts),
			   &expr->where);
	}
      else if (from_ts.type == BT_LOGICAL || ts->type == BT_LOGICAL)
	{
	  /* Do nothing. This block exists only to simplify the other
	     else-if expressions.
	       LOGICAL <> LOGICAL    no warning, independent of kind values
	       LOGICAL <> INTEGER    extension, warned elsewhere
	       LOGICAL <> REAL       invalid, error generated elsewhere
	       LOGICAL <> COMPLEX    invalid, error generated elsewhere  */
	}
      else
	gcc_unreachable ();
    }

  /* Insert a pre-resolved function call to the right function.  */
  old_where = expr->where;
  rank = expr->rank;
  shape = expr->shape;

  new_expr = gfc_get_expr ();
  *new_expr = *expr;

  new_expr = gfc_build_conversion (new_expr);
  new_expr->value.function.name = sym->lib_name;
  new_expr->value.function.isym = sym;
  new_expr->where = old_where;
  new_expr->ts = *ts;
  new_expr->rank = rank;
  new_expr->shape = gfc_copy_shape (shape, rank);

  gfc_get_ha_sym_tree (sym->name, &new_expr->symtree);
  new_expr->symtree->n.sym->result = new_expr->symtree->n.sym;
  new_expr->symtree->n.sym->ts.type = ts->type;
  new_expr->symtree->n.sym->ts.kind = ts->kind;
  new_expr->symtree->n.sym->attr.flavor = FL_PROCEDURE;
  new_expr->symtree->n.sym->attr.function = 1;
  new_expr->symtree->n.sym->attr.elemental = 1;
  new_expr->symtree->n.sym->attr.pure = 1;
  new_expr->symtree->n.sym->attr.referenced = 1;
  gfc_intrinsic_symbol(new_expr->symtree->n.sym);
  gfc_commit_symbol (new_expr->symtree->n.sym);

  *expr = *new_expr;

  free (new_expr);
  expr->ts = *ts;

  if (gfc_is_constant_expr (expr->value.function.actual->expr)
      && !do_simplify (sym, expr))
    {

      if (eflag == 2)
	goto bad;
      return false;		/* Error already generated in do_simplify() */
    }

  return true;

bad:
  const char *type_name = is_char_constant ? gfc_typename (expr)
					   : gfc_typename (&from_ts);
  if (eflag == 1)
    {
      gfc_error ("Cannot convert %s to %s at %L", type_name, gfc_typename (ts),
		 &expr->where);
      return false;
    }

  gfc_internal_error ("Cannot convert %qs to %qs at %L", type_name,
		      gfc_typename (ts), &expr->where);
  /* Not reached */
}


bool
gfc_convert_chartype (gfc_expr *expr, gfc_typespec *ts)
{
  gfc_intrinsic_sym *sym;
  locus old_where;
  gfc_expr *new_expr;
  int rank;
  mpz_t *shape;

  gcc_assert (expr->ts.type == BT_CHARACTER && ts->type == BT_CHARACTER);

  sym = find_char_conv (&expr->ts, ts);
  gcc_assert (sym);

  /* Insert a pre-resolved function call to the right function.  */
  old_where = expr->where;
  rank = expr->rank;
  shape = expr->shape;

  new_expr = gfc_get_expr ();
  *new_expr = *expr;

  new_expr = gfc_build_conversion (new_expr);
  new_expr->value.function.name = sym->lib_name;
  new_expr->value.function.isym = sym;
  new_expr->where = old_where;
  new_expr->ts = *ts;
  new_expr->rank = rank;
  new_expr->shape = gfc_copy_shape (shape, rank);

  gfc_get_ha_sym_tree (sym->name, &new_expr->symtree);
  new_expr->symtree->n.sym->ts.type = ts->type;
  new_expr->symtree->n.sym->ts.kind = ts->kind;
  new_expr->symtree->n.sym->attr.flavor = FL_PROCEDURE;
  new_expr->symtree->n.sym->attr.function = 1;
  new_expr->symtree->n.sym->attr.elemental = 1;
  new_expr->symtree->n.sym->attr.referenced = 1;
  gfc_intrinsic_symbol(new_expr->symtree->n.sym);
  gfc_commit_symbol (new_expr->symtree->n.sym);

  *expr = *new_expr;

  free (new_expr);
  expr->ts = *ts;

  if (gfc_is_constant_expr (expr->value.function.actual->expr)
      && !do_simplify (sym, expr))
    {
      /* Error already generated in do_simplify() */
      return false;
    }

  return true;
}


/* Check if the passed name is name of an intrinsic (taking into account the
   current -std=* and -fall-intrinsic settings).  If it is, see if we should
   warn about this as a user-procedure having the same name as an intrinsic
   (-Wintrinsic-shadow enabled) and do so if we should.  */

void
gfc_warn_intrinsic_shadow (const gfc_symbol* sym, bool in_module, bool func)
{
  gfc_intrinsic_sym* isym;

  /* If the warning is disabled, do nothing at all.  */
  if (!warn_intrinsic_shadow)
    return;

  /* Try to find an intrinsic of the same name.  */
  if (func)
    isym = gfc_find_function (sym->name);
  else
    isym = gfc_find_subroutine (sym->name);

  /* If no intrinsic was found with this name or it's not included in the
     selected standard, everything's fine.  */
  if (!isym || !gfc_check_intrinsic_standard (isym, NULL, true,
					      sym->declared_at))
    return;

  /* Emit the warning.  */
  if (in_module || sym->ns->proc_name)
    gfc_warning (OPT_Wintrinsic_shadow,
		 "%qs declared at %L may shadow the intrinsic of the same"
		 " name.  In order to call the intrinsic, explicit INTRINSIC"
		 " declarations may be required.",
		 sym->name, &sym->declared_at);
  else
    gfc_warning (OPT_Wintrinsic_shadow,
		 "%qs declared at %L is also the name of an intrinsic.  It can"
		 " only be called via an explicit interface or if declared"
		 " EXTERNAL.", sym->name, &sym->declared_at);
}
