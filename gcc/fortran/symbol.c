/* Maintain binary trees of symbols.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, 
   Inc.
   Contributed by Andy Vaught

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
#include "gfortran.h"
#include "parse.h"

/* Strings for all symbol attributes.  We use these for dumping the
   parse tree, in error messages, and also when reading and writing
   modules.  */

const mstring flavors[] =
{
  minit ("UNKNOWN-FL", FL_UNKNOWN), minit ("PROGRAM", FL_PROGRAM),
  minit ("BLOCK-DATA", FL_BLOCK_DATA), minit ("MODULE", FL_MODULE),
  minit ("VARIABLE", FL_VARIABLE), minit ("PARAMETER", FL_PARAMETER),
  minit ("LABEL", FL_LABEL), minit ("PROCEDURE", FL_PROCEDURE),
  minit ("DERIVED", FL_DERIVED), minit ("NAMELIST", FL_NAMELIST),
  minit (NULL, -1)
};

const mstring procedures[] =
{
    minit ("UNKNOWN-PROC", PROC_UNKNOWN),
    minit ("MODULE-PROC", PROC_MODULE),
    minit ("INTERNAL-PROC", PROC_INTERNAL),
    minit ("DUMMY-PROC", PROC_DUMMY),
    minit ("INTRINSIC-PROC", PROC_INTRINSIC),
    minit ("EXTERNAL-PROC", PROC_EXTERNAL),
    minit ("STATEMENT-PROC", PROC_ST_FUNCTION),
    minit (NULL, -1)
};

const mstring intents[] =
{
    minit ("UNKNOWN-INTENT", INTENT_UNKNOWN),
    minit ("IN", INTENT_IN),
    minit ("OUT", INTENT_OUT),
    minit ("INOUT", INTENT_INOUT),
    minit (NULL, -1)
};

const mstring access_types[] =
{
    minit ("UNKNOWN-ACCESS", ACCESS_UNKNOWN),
    minit ("PUBLIC", ACCESS_PUBLIC),
    minit ("PRIVATE", ACCESS_PRIVATE),
    minit (NULL, -1)
};

const mstring ifsrc_types[] =
{
    minit ("UNKNOWN", IFSRC_UNKNOWN),
    minit ("DECL", IFSRC_DECL),
    minit ("BODY", IFSRC_IFBODY),
    minit ("USAGE", IFSRC_USAGE)
};


/* This is to make sure the backend generates setup code in the correct
   order.  */

static int next_dummy_order = 1;


gfc_namespace *gfc_current_ns;

gfc_gsymbol *gfc_gsym_root = NULL;

static gfc_symbol *changed_syms = NULL;


/*********** IMPLICIT NONE and IMPLICIT statement handlers ***********/

/* The following static variable indicates whether a particular element has
   been explicitly set or not.  */

static int new_flag[GFC_LETTERS];


/* Handle a correctly parsed IMPLICIT NONE.  */

void
gfc_set_implicit_none (void)
{
  int i;

  for (i = 0; i < GFC_LETTERS; i++)
    {
      gfc_clear_ts (&gfc_current_ns->default_type[i]);
      gfc_current_ns->set_flag[i] = 1;
    }
}


/* Reset the implicit range flags.  */

void
gfc_clear_new_implicit (void)
{
  int i;

  for (i = 0; i < GFC_LETTERS; i++)
    new_flag[i] = 0;
}


/* Prepare for a new implicit range.  Sets flags in new_flag[].  */

try
gfc_add_new_implicit_range (int c1, int c2)
{
  int i;

  c1 -= 'a';
  c2 -= 'a';

  for (i = c1; i <= c2; i++)
    {
      if (new_flag[i])
	{
	  gfc_error ("Letter '%c' already set in IMPLICIT statement at %C",
		     i + 'A');
	  return FAILURE;
	}

      new_flag[i] = 1;
    }

  return SUCCESS;
}


/* Add a matched implicit range for gfc_set_implicit().  Check if merging
   the new implicit types back into the existing types will work.  */

try
gfc_merge_new_implicit (gfc_typespec * ts)
{
  int i;

  for (i = 0; i < GFC_LETTERS; i++)
    {
      if (new_flag[i])
	{

	  if (gfc_current_ns->set_flag[i])
	    {
	      gfc_error ("Letter %c already has an IMPLICIT type at %C",
			 i + 'A');
	      return FAILURE;
	    }
	  gfc_current_ns->default_type[i] = *ts;
	  gfc_current_ns->set_flag[i] = 1;
	}
    }
  return SUCCESS;
}


/* Given a symbol, return a pointer to the typespec for its default type.  */

gfc_typespec *
gfc_get_default_type (gfc_symbol * sym, gfc_namespace * ns)
{
  char letter;

  letter = sym->name[0];
  if (letter < 'a' || letter > 'z')
    gfc_internal_error ("gfc_get_default_type(): Bad symbol");

  if (ns == NULL)
    ns = gfc_current_ns;

  return &ns->default_type[letter - 'a'];
}


/* Given a pointer to a symbol, set its type according to the first
   letter of its name.  Fails if the letter in question has no default
   type.  */

try
gfc_set_default_type (gfc_symbol * sym, int error_flag, gfc_namespace * ns)
{
  gfc_typespec *ts;

  if (sym->ts.type != BT_UNKNOWN)
    gfc_internal_error ("gfc_set_default_type(): symbol already has a type");

  ts = gfc_get_default_type (sym, ns);

  if (ts->type == BT_UNKNOWN)
    {
      if (error_flag && !sym->attr.untyped)
	{
	  gfc_error ("Symbol '%s' at %L has no IMPLICIT type",
		     sym->name, &sym->declared_at);
	  sym->attr.untyped = 1; /* Ensure we only give an error once.  */
	}

      return FAILURE;
    }

  sym->ts = *ts;
  sym->attr.implicit_type = 1;

  return SUCCESS;
}


/******************** Symbol attribute stuff *********************/

/* This is a generic conflict-checker.  We do this to avoid having a
   single conflict in two places.  */

#define conf(a, b) if (attr->a && attr->b) { a1 = a; a2 = b; goto conflict; }
#define conf2(a) if (attr->a) { a2 = a; goto conflict; }

static try
check_conflict (symbol_attribute * attr, const char * name, locus * where)
{
  static const char *dummy = "DUMMY", *save = "SAVE", *pointer = "POINTER",
    *target = "TARGET", *external = "EXTERNAL", *intent = "INTENT",
    *intrinsic = "INTRINSIC", *allocatable = "ALLOCATABLE",
    *elemental = "ELEMENTAL", *private = "PRIVATE", *recursive = "RECURSIVE",
    *in_common = "COMMON", *result = "RESULT", *in_namelist = "NAMELIST",
    *public = "PUBLIC", *optional = "OPTIONAL", *entry = "ENTRY",
    *function = "FUNCTION", *subroutine = "SUBROUTINE",
    *dimension = "DIMENSION", *in_equivalence = "EQUIVALENCE",
    *use_assoc = "USE ASSOCIATED";

  const char *a1, *a2;

  if (where == NULL)
    where = &gfc_current_locus;

  if (attr->pointer && attr->intent != INTENT_UNKNOWN)
    {
      a1 = pointer;
      a2 = intent;
      goto conflict;
    }

  /* Check for attributes not allowed in a BLOCK DATA.  */
  if (gfc_current_state () == COMP_BLOCK_DATA)
    {
      a1 = NULL;

      if (attr->allocatable)
	a1 = allocatable;
      if (attr->external)
	a1 = external;
      if (attr->optional)
	a1 = optional;
      if (attr->access == ACCESS_PRIVATE)
	a1 = private;
      if (attr->access == ACCESS_PUBLIC)
	a1 = public;
      if (attr->intent != INTENT_UNKNOWN)
	a1 = intent;

      if (a1 != NULL)
	{
	  gfc_error
	    ("%s attribute not allowed in BLOCK DATA program unit at %L", a1,
	     where);
	  return FAILURE;
	}
    }

  conf (dummy, save);
  conf (pointer, target);
  conf (pointer, external);
  conf (pointer, intrinsic);
  conf (target, external);
  conf (target, intrinsic);
  conf (external, dimension);   /* See Fortran 95's R504.  */

  conf (external, intrinsic);
  conf (allocatable, pointer);
  conf (allocatable, dummy);	/* TODO: Allowed in Fortran 200x.  */
  conf (allocatable, function);	/* TODO: Allowed in Fortran 200x.  */
  conf (allocatable, result);	/* TODO: Allowed in Fortran 200x.  */
  conf (elemental, recursive);

  conf (in_common, dummy);
  conf (in_common, allocatable);
  conf (in_common, result);
  conf (in_common, save);
  conf (result, save);

  conf (dummy, result);

  conf (in_equivalence, use_assoc);
  conf (in_equivalence, dummy);
  conf (in_equivalence, target);
  conf (in_equivalence, pointer);
  conf (in_equivalence, function);
  conf (in_equivalence, result);
  conf (in_equivalence, entry);
  conf (in_equivalence, allocatable);

  conf (in_namelist, pointer);
  conf (in_namelist, allocatable);

  conf (entry, result);

  conf (function, subroutine);

  a1 = gfc_code2string (flavors, attr->flavor);

  if (attr->in_namelist
      && attr->flavor != FL_VARIABLE
      && attr->flavor != FL_UNKNOWN)
    {

      a2 = in_namelist;
      goto conflict;
    }

  switch (attr->flavor)
    {
    case FL_PROGRAM:
    case FL_BLOCK_DATA:
    case FL_MODULE:
    case FL_LABEL:
      conf2 (dummy);
      conf2 (save);
      conf2 (pointer);
      conf2 (target);
      conf2 (external);
      conf2 (intrinsic);
      conf2 (allocatable);
      conf2 (result);
      conf2 (in_namelist);
      conf2 (optional);
      conf2 (function);
      conf2 (subroutine);
      break;

    case FL_VARIABLE:
    case FL_NAMELIST:
      break;

    case FL_PROCEDURE:
      conf2 (intent);

      if (attr->subroutine)
	{
	  conf2(save);
	  conf2(pointer);
	  conf2(target);
	  conf2(allocatable);
	  conf2(result);
	  conf2(in_namelist);
	  conf2(function);
	}

      switch (attr->proc)
	{
	case PROC_ST_FUNCTION:
	  conf2 (in_common);
	  conf2 (dummy);
	  break;

	case PROC_MODULE:
	  conf2 (dummy);
	  break;

	case PROC_DUMMY:
	  conf2 (result);
	  conf2 (in_common);
	  conf2 (save);
	  break;

	default:
	  break;
	}

      break;

    case FL_DERIVED:
      conf2 (dummy);
      conf2 (save);
      conf2 (pointer);
      conf2 (target);
      conf2 (external);
      conf2 (intrinsic);
      conf2 (allocatable);
      conf2 (optional);
      conf2 (entry);
      conf2 (function);
      conf2 (subroutine);

      if (attr->intent != INTENT_UNKNOWN)
	{
	  a2 = intent;
	  goto conflict;
	}
      break;

    case FL_PARAMETER:
      conf2 (external);
      conf2 (intrinsic);
      conf2 (optional);
      conf2 (allocatable);
      conf2 (function);
      conf2 (subroutine);
      conf2 (entry);
      conf2 (pointer);
      conf2 (target);
      conf2 (dummy);
      conf2 (in_common);
      conf2 (save);
      break;

    default:
      break;
    }

  return SUCCESS;

conflict:
  if (name == NULL)
    gfc_error ("%s attribute conflicts with %s attribute at %L",
	       a1, a2, where);
  else
    gfc_error ("%s attribute conflicts with %s attribute in '%s' at %L",
	       a1, a2, name, where);

  return FAILURE;
}

#undef conf
#undef conf2


/* Mark a symbol as referenced.  */

void
gfc_set_sym_referenced (gfc_symbol * sym)
{
  if (sym->attr.referenced)
    return;

  sym->attr.referenced = 1;

  /* Remember which order dummy variables are accessed in.  */
  if (sym->attr.dummy)
    sym->dummy_order = next_dummy_order++;
}


/* Common subroutine called by attribute changing subroutines in order
   to prevent them from changing a symbol that has been
   use-associated.  Returns zero if it is OK to change the symbol,
   nonzero if not.  */

static int
check_used (symbol_attribute * attr, const char * name, locus * where)
{

  if (attr->use_assoc == 0)
    return 0;

  if (where == NULL)
    where = &gfc_current_locus;

  if (name == NULL)
    gfc_error ("Cannot change attributes of USE-associated symbol at %L",
	       where);
  else
    gfc_error ("Cannot change attributes of USE-associated symbol %s at %L",
	       name, where);

  return 1;
}


/* Used to prevent changing the attributes of a symbol after it has been
   used.  This check is only done for dummy variables as only these can be
   used in specification expressions.  Applying this to all symbols causes
   an error when we reach the body of a contained function.  */

static int
check_done (symbol_attribute * attr, locus * where)
{

  if (!(attr->dummy && attr->referenced))
    return 0;

  if (where == NULL)
    where = &gfc_current_locus;

  gfc_error ("Cannot change attributes of symbol at %L"
             " after it has been used", where);

  return 1;
}


/* Generate an error because of a duplicate attribute.  */

static void
duplicate_attr (const char *attr, locus * where)
{

  if (where == NULL)
    where = &gfc_current_locus;

  gfc_error ("Duplicate %s attribute specified at %L", attr, where);
}


try
gfc_add_allocatable (symbol_attribute * attr, locus * where)
{

  if (check_used (attr, NULL, where) || check_done (attr, where))
    return FAILURE;

  if (attr->allocatable)
    {
      duplicate_attr ("ALLOCATABLE", where);
      return FAILURE;
    }

  attr->allocatable = 1;
  return check_conflict (attr, NULL, where);
}


try
gfc_add_dimension (symbol_attribute * attr, const char *name, locus * where)
{

  if (check_used (attr, name, where) || check_done (attr, where))
    return FAILURE;

  if (attr->dimension)
    {
      duplicate_attr ("DIMENSION", where);
      return FAILURE;
    }

  attr->dimension = 1;
  return check_conflict (attr, name, where);
}


try
gfc_add_external (symbol_attribute * attr, locus * where)
{

  if (check_used (attr, NULL, where) || check_done (attr, where))
    return FAILURE;

  if (attr->external)
    {
      duplicate_attr ("EXTERNAL", where);
      return FAILURE;
    }

  attr->external = 1;

  return check_conflict (attr, NULL, where);
}


try
gfc_add_intrinsic (symbol_attribute * attr, locus * where)
{

  if (check_used (attr, NULL, where) || check_done (attr, where))
    return FAILURE;

  if (attr->intrinsic)
    {
      duplicate_attr ("INTRINSIC", where);
      return FAILURE;
    }

  attr->intrinsic = 1;

  return check_conflict (attr, NULL, where);
}


try
gfc_add_optional (symbol_attribute * attr, locus * where)
{

  if (check_used (attr, NULL, where) || check_done (attr, where))
    return FAILURE;

  if (attr->optional)
    {
      duplicate_attr ("OPTIONAL", where);
      return FAILURE;
    }

  attr->optional = 1;
  return check_conflict (attr, NULL, where);
}


try
gfc_add_pointer (symbol_attribute * attr, locus * where)
{

  if (check_used (attr, NULL, where) || check_done (attr, where))
    return FAILURE;

  attr->pointer = 1;
  return check_conflict (attr, NULL, where);
}


try
gfc_add_result (symbol_attribute * attr, const char *name, locus * where)
{

  if (check_used (attr, name, where) || check_done (attr, where))
    return FAILURE;

  attr->result = 1;
  return check_conflict (attr, name, where);
}


try
gfc_add_save (symbol_attribute * attr, const char *name, locus * where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  if (gfc_pure (NULL))
    {
      gfc_error
	("SAVE attribute at %L cannot be specified in a PURE procedure",
	 where);
      return FAILURE;
    }

  if (attr->save)
    {
      duplicate_attr ("SAVE", where);
      return FAILURE;
    }

  attr->save = 1;
  return check_conflict (attr, name, where);
}


try
gfc_add_target (symbol_attribute * attr, locus * where)
{

  if (check_used (attr, NULL, where) || check_done (attr, where))
    return FAILURE;

  if (attr->target)
    {
      duplicate_attr ("TARGET", where);
      return FAILURE;
    }

  attr->target = 1;
  return check_conflict (attr, NULL, where);
}


try
gfc_add_dummy (symbol_attribute * attr, const char *name, locus * where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  /* Duplicate dummy arguments are allowed due to ENTRY statements.  */
  attr->dummy = 1;
  return check_conflict (attr, name, where);
}


try
gfc_add_in_common (symbol_attribute * attr, const char *name, locus * where)
{

  if (check_used (attr, name, where) || check_done (attr, where))
    return FAILURE;

  /* Duplicate attribute already checked for.  */
  attr->in_common = 1;
  if (check_conflict (attr, name, where) == FAILURE)
    return FAILURE;

  if (attr->flavor == FL_VARIABLE)
    return SUCCESS;

  return gfc_add_flavor (attr, FL_VARIABLE, name, where);
}

try
gfc_add_in_equivalence (symbol_attribute * attr, const char *name, locus * where)
{

  /* Duplicate attribute already checked for.  */
  attr->in_equivalence = 1;
  if (check_conflict (attr, name, where) == FAILURE)
    return FAILURE;

  if (attr->flavor == FL_VARIABLE)
    return SUCCESS;

  return gfc_add_flavor (attr, FL_VARIABLE, name, where);
}


try
gfc_add_data (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  attr->data = 1;
  return check_conflict (attr, name, where);
}


try
gfc_add_in_namelist (symbol_attribute * attr, const char *name,
		     locus * where)
{

  attr->in_namelist = 1;
  return check_conflict (attr, name, where);
}


try
gfc_add_sequence (symbol_attribute * attr, const char *name, locus * where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  attr->sequence = 1;
  return check_conflict (attr, name, where);
}


try
gfc_add_elemental (symbol_attribute * attr, locus * where)
{

  if (check_used (attr, NULL, where) || check_done (attr, where))
    return FAILURE;

  attr->elemental = 1;
  return check_conflict (attr, NULL, where);
}


try
gfc_add_pure (symbol_attribute * attr, locus * where)
{

  if (check_used (attr, NULL, where) || check_done (attr, where))
    return FAILURE;

  attr->pure = 1;
  return check_conflict (attr, NULL, where);
}


try
gfc_add_recursive (symbol_attribute * attr, locus * where)
{

  if (check_used (attr, NULL, where) || check_done (attr, where))
    return FAILURE;

  attr->recursive = 1;
  return check_conflict (attr, NULL, where);
}


try
gfc_add_entry (symbol_attribute * attr, const char *name, locus * where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  if (attr->entry)
    {
      duplicate_attr ("ENTRY", where);
      return FAILURE;
    }

  attr->entry = 1;
  return check_conflict (attr, name, where);
}


try
gfc_add_function (symbol_attribute * attr, const char *name, locus * where)
{

  if (attr->flavor != FL_PROCEDURE
      && gfc_add_flavor (attr, FL_PROCEDURE, name, where) == FAILURE)
    return FAILURE;

  attr->function = 1;
  return check_conflict (attr, name, where);
}


try
gfc_add_subroutine (symbol_attribute * attr, const char *name, locus * where)
{

  if (attr->flavor != FL_PROCEDURE
      && gfc_add_flavor (attr, FL_PROCEDURE, name, where) == FAILURE)
    return FAILURE;

  attr->subroutine = 1;
  return check_conflict (attr, name, where);
}


try
gfc_add_generic (symbol_attribute * attr, const char *name, locus * where)
{

  if (attr->flavor != FL_PROCEDURE
      && gfc_add_flavor (attr, FL_PROCEDURE, name, where) == FAILURE)
    return FAILURE;

  attr->generic = 1;
  return check_conflict (attr, name, where);
}


/* Flavors are special because some flavors are not what Fortran
   considers attributes and can be reaffirmed multiple times.  */

try
gfc_add_flavor (symbol_attribute * attr, sym_flavor f, const char *name,
		locus * where)
{

  if ((f == FL_PROGRAM || f == FL_BLOCK_DATA || f == FL_MODULE
       || f == FL_PARAMETER || f == FL_LABEL || f == FL_DERIVED
       || f == FL_NAMELIST) && check_used (attr, name, where))
    return FAILURE;

  if (attr->flavor == f && f == FL_VARIABLE)
    return SUCCESS;

  if (attr->flavor != FL_UNKNOWN)
    {
      if (where == NULL)
	where = &gfc_current_locus;

      gfc_error ("%s attribute conflicts with %s attribute at %L",
		 gfc_code2string (flavors, attr->flavor),
		 gfc_code2string (flavors, f), where);

      return FAILURE;
    }

  attr->flavor = f;

  return check_conflict (attr, name, where);
}


try
gfc_add_procedure (symbol_attribute * attr, procedure_type t,
		   const char *name, locus * where)
{

  if (check_used (attr, name, where) || check_done (attr, where))
    return FAILURE;

  if (attr->flavor != FL_PROCEDURE
      && gfc_add_flavor (attr, FL_PROCEDURE, name, where) == FAILURE)
    return FAILURE;

  if (where == NULL)
    where = &gfc_current_locus;

  if (attr->proc != PROC_UNKNOWN)
    {
      gfc_error ("%s procedure at %L is already %s %s procedure",
		 gfc_code2string (procedures, t), where,
		 gfc_article (gfc_code2string (procedures, attr->proc)),
		 gfc_code2string (procedures, attr->proc));

      return FAILURE;
    }

  attr->proc = t;

  /* Statement functions are always scalar and functions.  */
  if (t == PROC_ST_FUNCTION
      && ((!attr->function && gfc_add_function (attr, name, where) == FAILURE)
	  || attr->dimension))
    return FAILURE;

  return check_conflict (attr, name, where);
}


try
gfc_add_intent (symbol_attribute * attr, sym_intent intent, locus * where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->intent == INTENT_UNKNOWN)
    {
      attr->intent = intent;
      return check_conflict (attr, NULL, where);
    }

  if (where == NULL)
    where = &gfc_current_locus;

  gfc_error ("INTENT (%s) conflicts with INTENT(%s) at %L",
	     gfc_intent_string (attr->intent),
	     gfc_intent_string (intent), where);

  return FAILURE;
}


/* No checks for use-association in public and private statements.  */

try
gfc_add_access (symbol_attribute * attr, gfc_access access,
		const char *name, locus * where)
{

  if (attr->access == ACCESS_UNKNOWN)
    {
      attr->access = access;
      return check_conflict (attr, name, where);
    }

  if (where == NULL)
    where = &gfc_current_locus;
  gfc_error ("ACCESS specification at %L was already specified", where);

  return FAILURE;
}


try
gfc_add_explicit_interface (gfc_symbol * sym, ifsrc source,
			    gfc_formal_arglist * formal, locus * where)
{

  if (check_used (&sym->attr, sym->name, where))
    return FAILURE;

  if (where == NULL)
    where = &gfc_current_locus;

  if (sym->attr.if_source != IFSRC_UNKNOWN
      && sym->attr.if_source != IFSRC_DECL)
    {
      gfc_error ("Symbol '%s' at %L already has an explicit interface",
		 sym->name, where);
      return FAILURE;
    }

  sym->formal = formal;
  sym->attr.if_source = source;

  return SUCCESS;
}


/* Add a type to a symbol.  */

try
gfc_add_type (gfc_symbol * sym, gfc_typespec * ts, locus * where)
{
  sym_flavor flavor;

/* TODO: This is legal if it is reaffirming an implicit type.
  if (check_done (&sym->attr, where))
    return FAILURE;*/

  if (where == NULL)
    where = &gfc_current_locus;

  if (sym->ts.type != BT_UNKNOWN)
    {
      gfc_error ("Symbol '%s' at %L already has basic type of %s", sym->name,
		 where, gfc_basic_typename (sym->ts.type));
      return FAILURE;
    }

  flavor = sym->attr.flavor;

  if (flavor == FL_PROGRAM || flavor == FL_BLOCK_DATA || flavor == FL_MODULE
      || flavor == FL_LABEL || (flavor == FL_PROCEDURE
				&& sym->attr.subroutine)
      || flavor == FL_DERIVED || flavor == FL_NAMELIST)
    {
      gfc_error ("Symbol '%s' at %L cannot have a type", sym->name, where);
      return FAILURE;
    }

  sym->ts = *ts;
  return SUCCESS;
}


/* Clears all attributes.  */

void
gfc_clear_attr (symbol_attribute * attr)
{
  memset (attr, 0, sizeof(symbol_attribute));
}


/* Check for missing attributes in the new symbol.  Currently does
   nothing, but it's not clear that it is unnecessary yet.  */

try
gfc_missing_attr (symbol_attribute * attr ATTRIBUTE_UNUSED,
		  locus * where ATTRIBUTE_UNUSED)
{

  return SUCCESS;
}


/* Copy an attribute to a symbol attribute, bit by bit.  Some
   attributes have a lot of side-effects but cannot be present given
   where we are called from, so we ignore some bits.  */

try
gfc_copy_attr (symbol_attribute * dest, symbol_attribute * src, locus * where)
{

  if (src->allocatable && gfc_add_allocatable (dest, where) == FAILURE)
    goto fail;

  if (src->dimension && gfc_add_dimension (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->optional && gfc_add_optional (dest, where) == FAILURE)
    goto fail;
  if (src->pointer && gfc_add_pointer (dest, where) == FAILURE)
    goto fail;
  if (src->save && gfc_add_save (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->target && gfc_add_target (dest, where) == FAILURE)
    goto fail;
  if (src->dummy && gfc_add_dummy (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->result && gfc_add_result (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->entry)
    dest->entry = 1;

  if (src->in_namelist && gfc_add_in_namelist (dest, NULL, where) == FAILURE)
    goto fail;

  if (src->in_common && gfc_add_in_common (dest, NULL, where) == FAILURE)
    goto fail;

  if (src->generic && gfc_add_generic (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->function && gfc_add_function (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->subroutine && gfc_add_subroutine (dest, NULL, where) == FAILURE)
    goto fail;

  if (src->sequence && gfc_add_sequence (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->elemental && gfc_add_elemental (dest, where) == FAILURE)
    goto fail;
  if (src->pure && gfc_add_pure (dest, where) == FAILURE)
    goto fail;
  if (src->recursive && gfc_add_recursive (dest, where) == FAILURE)
    goto fail;

  if (src->flavor != FL_UNKNOWN
      && gfc_add_flavor (dest, src->flavor, NULL, where) == FAILURE)
    goto fail;

  if (src->intent != INTENT_UNKNOWN
      && gfc_add_intent (dest, src->intent, where) == FAILURE)
    goto fail;

  if (src->access != ACCESS_UNKNOWN
      && gfc_add_access (dest, src->access, NULL, where) == FAILURE)
    goto fail;

  if (gfc_missing_attr (dest, where) == FAILURE)
    goto fail;

  /* The subroutines that set these bits also cause flavors to be set,
     and that has already happened in the original, so don't let it
     happen again.  */
  if (src->external)
    dest->external = 1;
  if (src->intrinsic)
    dest->intrinsic = 1;

  return SUCCESS;

fail:
  return FAILURE;
}


/************** Component name management ************/

/* Component names of a derived type form their own little namespaces
   that are separate from all other spaces.  The space is composed of
   a singly linked list of gfc_component structures whose head is
   located in the parent symbol.  */


/* Add a component name to a symbol.  The call fails if the name is
   already present.  On success, the component pointer is modified to
   point to the additional component structure.  */

try
gfc_add_component (gfc_symbol * sym, const char *name, gfc_component ** component)
{
  gfc_component *p, *tail;

  tail = NULL;

  for (p = sym->components; p; p = p->next)
    {
      if (strcmp (p->name, name) == 0)
	{
	  gfc_error ("Component '%s' at %C already declared at %L",
		     name, &p->loc);
	  return FAILURE;
	}

      tail = p;
    }

  /* Allocate a new component.  */
  p = gfc_get_component ();

  if (tail == NULL)
    sym->components = p;
  else
    tail->next = p;

  p->name = gfc_get_string (name);
  p->loc = gfc_current_locus;

  *component = p;
  return SUCCESS;
}


/* Recursive function to switch derived types of all symbol in a
   namespace.  */

static void
switch_types (gfc_symtree * st, gfc_symbol * from, gfc_symbol * to)
{
  gfc_symbol *sym;

  if (st == NULL)
    return;

  sym = st->n.sym;
  if (sym->ts.type == BT_DERIVED && sym->ts.derived == from)
    sym->ts.derived = to;

  switch_types (st->left, from, to);
  switch_types (st->right, from, to);
}


/* This subroutine is called when a derived type is used in order to
   make the final determination about which version to use.  The
   standard requires that a type be defined before it is 'used', but
   such types can appear in IMPLICIT statements before the actual
   definition.  'Using' in this context means declaring a variable to
   be that type or using the type constructor.

   If a type is used and the components haven't been defined, then we
   have to have a derived type in a parent unit.  We find the node in
   the other namespace and point the symtree node in this namespace to
   that node.  Further reference to this name point to the correct
   node.  If we can't find the node in a parent namespace, then we have
   an error.

   This subroutine takes a pointer to a symbol node and returns a
   pointer to the translated node or NULL for an error.  Usually there
   is no translation and we return the node we were passed.  */

gfc_symbol *
gfc_use_derived (gfc_symbol * sym)
{
  gfc_symbol *s, *p;
  gfc_typespec *t;
  gfc_symtree *st;
  int i;

  if (sym->components != NULL)
    return sym;               /* Already defined.  */

  if (sym->ns->parent == NULL)
    goto bad;

  if (gfc_find_symbol (sym->name, sym->ns->parent, 1, &s))
    {
      gfc_error ("Symbol '%s' at %C is ambiguous", sym->name);
      return NULL;
    }

  if (s == NULL || s->attr.flavor != FL_DERIVED)
    goto bad;

  /* Get rid of symbol sym, translating all references to s.  */
  for (i = 0; i < GFC_LETTERS; i++)
    {
      t = &sym->ns->default_type[i];
      if (t->derived == sym)
	t->derived = s;
    }

  st = gfc_find_symtree (sym->ns->sym_root, sym->name);
  st->n.sym = s;

  s->refs++;

  /* Unlink from list of modified symbols.  */
  if (changed_syms == sym)
    changed_syms = sym->tlink;
  else
    for (p = changed_syms; p; p = p->tlink)
      if (p->tlink == sym)
	{
	  p->tlink = sym->tlink;
	  break;
	}

  switch_types (sym->ns->sym_root, sym, s);

  /* TODO: Also have to replace sym -> s in other lists like
     namelists, common lists and interface lists.  */
  gfc_free_symbol (sym);

  return s;

bad:
  gfc_error ("Derived type '%s' at %C is being used before it is defined",
	     sym->name);
  return NULL;
}


/* Given a derived type node and a component name, try to locate the
   component structure.  Returns the NULL pointer if the component is
   not found or the components are private.  */

gfc_component *
gfc_find_component (gfc_symbol * sym, const char *name)
{
  gfc_component *p;

  if (name == NULL)
    return NULL;

  sym = gfc_use_derived (sym);

  if (sym == NULL)
    return NULL;

  for (p = sym->components; p; p = p->next)
    if (strcmp (p->name, name) == 0)
      break;

  if (p == NULL)
    gfc_error ("'%s' at %C is not a member of the '%s' structure",
	       name, sym->name);
  else
    {
      if (sym->attr.use_assoc && sym->component_access == ACCESS_PRIVATE)
	{
	  gfc_error ("Component '%s' at %C is a PRIVATE component of '%s'",
		     name, sym->name);
	  p = NULL;
	}
    }

  return p;
}


/* Given a symbol, free all of the component structures and everything
   they point to.  */

static void
free_components (gfc_component * p)
{
  gfc_component *q;

  for (; p; p = q)
    {
      q = p->next;

      gfc_free_array_spec (p->as);
      gfc_free_expr (p->initializer);

      gfc_free (p);
    }
}


/* Set component attributes from a standard symbol attribute
   structure.  */

void
gfc_set_component_attr (gfc_component * c, symbol_attribute * attr)
{

  c->dimension = attr->dimension;
  c->pointer = attr->pointer;
}


/* Get a standard symbol attribute structure given the component
   structure.  */

void
gfc_get_component_attr (symbol_attribute * attr, gfc_component * c)
{

  gfc_clear_attr (attr);
  attr->dimension = c->dimension;
  attr->pointer = c->pointer;
}


/******************** Statement label management ********************/

/* Free a single gfc_st_label structure, making sure the list is not
   messed up.  This function is called only when some parse error
   occurs.  */

void
gfc_free_st_label (gfc_st_label * l)
{

  if (l == NULL)
    return;

  if (l->prev)
    (l->prev->next = l->next);

  if (l->next)
    (l->next->prev = l->prev);

  if (l->format != NULL)
    gfc_free_expr (l->format);
  gfc_free (l);
}

/* Free a whole list of gfc_st_label structures.  */

static void
free_st_labels (gfc_st_label * l1)
{
  gfc_st_label *l2;

  for (; l1; l1 = l2)
    {
      l2 = l1->next;
      if (l1->format != NULL)
	gfc_free_expr (l1->format);
      gfc_free (l1);
    }
}


/* Given a label number, search for and return a pointer to the label
   structure, creating it if it does not exist.  */

gfc_st_label *
gfc_get_st_label (int labelno)
{
  gfc_st_label *lp;

  /* First see if the label is already in this namespace.  */
  for (lp = gfc_current_ns->st_labels; lp; lp = lp->next)
    if (lp->value == labelno)
      break;
  if (lp != NULL)
    return lp;

  lp = gfc_getmem (sizeof (gfc_st_label));

  lp->value = labelno;
  lp->defined = ST_LABEL_UNKNOWN;
  lp->referenced = ST_LABEL_UNKNOWN;

  lp->prev = NULL;
  lp->next = gfc_current_ns->st_labels;
  if (gfc_current_ns->st_labels)
    gfc_current_ns->st_labels->prev = lp;
  gfc_current_ns->st_labels = lp;

  return lp;
}


/* Called when a statement with a statement label is about to be
   accepted.  We add the label to the list of the current namespace,
   making sure it hasn't been defined previously and referenced
   correctly.  */

void
gfc_define_st_label (gfc_st_label * lp, gfc_sl_type type, locus * label_locus)
{
  int labelno;

  labelno = lp->value;

  if (lp->defined != ST_LABEL_UNKNOWN)
    gfc_error ("Duplicate statement label %d at %L and %L", labelno,
	       &lp->where, label_locus);
  else
    {
      lp->where = *label_locus;

      switch (type)
	{
	case ST_LABEL_FORMAT:
	  if (lp->referenced == ST_LABEL_TARGET)
	    gfc_error ("Label %d at %C already referenced as branch target",
		       labelno);
	  else
	    lp->defined = ST_LABEL_FORMAT;

	  break;

	case ST_LABEL_TARGET:
	  if (lp->referenced == ST_LABEL_FORMAT)
	    gfc_error ("Label %d at %C already referenced as a format label",
		       labelno);
	  else
	    lp->defined = ST_LABEL_TARGET;

	  break;

	default:
	  lp->defined = ST_LABEL_BAD_TARGET;
	  lp->referenced = ST_LABEL_BAD_TARGET;
	}
    }
}


/* Reference a label.  Given a label and its type, see if that
   reference is consistent with what is known about that label,
   updating the unknown state.  Returns FAILURE if something goes
   wrong.  */

try
gfc_reference_st_label (gfc_st_label * lp, gfc_sl_type type)
{
  gfc_sl_type label_type;
  int labelno;
  try rc;

  if (lp == NULL)
    return SUCCESS;

  labelno = lp->value;

  if (lp->defined != ST_LABEL_UNKNOWN)
    label_type = lp->defined;
  else
    {
      label_type = lp->referenced;
      lp->where = gfc_current_locus;
    }

  if (label_type == ST_LABEL_FORMAT && type == ST_LABEL_TARGET)
    {
      gfc_error ("Label %d at %C previously used as a FORMAT label", labelno);
      rc = FAILURE;
      goto done;
    }

  if ((label_type == ST_LABEL_TARGET || label_type == ST_LABEL_BAD_TARGET)
      && type == ST_LABEL_FORMAT)
    {
      gfc_error ("Label %d at %C previously used as branch target", labelno);
      rc = FAILURE;
      goto done;
    }

  lp->referenced = type;
  rc = SUCCESS;

done:
  return rc;
}


/************** Symbol table management subroutines ****************/

/* Basic details: Fortran 95 requires a potentially unlimited number
   of distinct namespaces when compiling a program unit.  This case
   occurs during a compilation of internal subprograms because all of
   the internal subprograms must be read before we can start
   generating code for the host.

   Given the tricky nature of the Fortran grammar, we must be able to
   undo changes made to a symbol table if the current interpretation
   of a statement is found to be incorrect.  Whenever a symbol is
   looked up, we make a copy of it and link to it.  All of these
   symbols are kept in a singly linked list so that we can commit or
   undo the changes at a later time.

   A symtree may point to a symbol node outside of its namespace.  In
   this case, that symbol has been used as a host associated variable
   at some previous time.  */

/* Allocate a new namespace structure.  Copies the implicit types from
   PARENT if PARENT_TYPES is set.  */

gfc_namespace *
gfc_get_namespace (gfc_namespace * parent, int parent_types)
{
  gfc_namespace *ns;
  gfc_typespec *ts;
  gfc_intrinsic_op in;
  int i;

  ns = gfc_getmem (sizeof (gfc_namespace));
  ns->sym_root = NULL;
  ns->uop_root = NULL;
  ns->default_access = ACCESS_UNKNOWN;
  ns->parent = parent;

  for (in = GFC_INTRINSIC_BEGIN; in != GFC_INTRINSIC_END; in++)
    ns->operator_access[in] = ACCESS_UNKNOWN;

  /* Initialize default implicit types.  */
  for (i = 'a'; i <= 'z'; i++)
    {
      ns->set_flag[i - 'a'] = 0;
      ts = &ns->default_type[i - 'a'];

      if (parent_types && ns->parent != NULL)
	{
	  /* Copy parent settings */
	  *ts = ns->parent->default_type[i - 'a'];
	  continue;
	}

      if (gfc_option.flag_implicit_none != 0)
	{
	  gfc_clear_ts (ts);
	  continue;
	}

      if ('i' <= i && i <= 'n')
	{
	  ts->type = BT_INTEGER;
	  ts->kind = gfc_default_integer_kind;
	}
      else
	{
	  ts->type = BT_REAL;
	  ts->kind = gfc_default_real_kind;
	}
    }

  ns->refs = 1;

  return ns;
}


/* Comparison function for symtree nodes.  */

static int
compare_symtree (void * _st1, void * _st2)
{
  gfc_symtree *st1, *st2;

  st1 = (gfc_symtree *) _st1;
  st2 = (gfc_symtree *) _st2;

  return strcmp (st1->name, st2->name);
}


/* Allocate a new symtree node and associate it with the new symbol.  */

gfc_symtree *
gfc_new_symtree (gfc_symtree ** root, const char *name)
{
  gfc_symtree *st;

  st = gfc_getmem (sizeof (gfc_symtree));
  st->name = gfc_get_string (name);

  gfc_insert_bbt (root, st, compare_symtree);
  return st;
}


/* Delete a symbol from the tree.  Does not free the symbol itself!  */

static void
delete_symtree (gfc_symtree ** root, const char *name)
{
  gfc_symtree st, *st0;

  st0 = gfc_find_symtree (*root, name);

  st.name = gfc_get_string (name);
  gfc_delete_bbt (root, &st, compare_symtree);

  gfc_free (st0);
}


/* Given a root symtree node and a name, try to find the symbol within
   the namespace.  Returns NULL if the symbol is not found.  */

gfc_symtree *
gfc_find_symtree (gfc_symtree * st, const char *name)
{
  int c;

  while (st != NULL)
    {
      c = strcmp (name, st->name);
      if (c == 0)
	return st;

      st = (c < 0) ? st->left : st->right;
    }

  return NULL;
}


/* Given a name find a user operator node, creating it if it doesn't
   exist.  These are much simpler than symbols because they can't be
   ambiguous with one another.  */

gfc_user_op *
gfc_get_uop (const char *name)
{
  gfc_user_op *uop;
  gfc_symtree *st;

  st = gfc_find_symtree (gfc_current_ns->uop_root, name);
  if (st != NULL)
    return st->n.uop;

  st = gfc_new_symtree (&gfc_current_ns->uop_root, name);

  uop = st->n.uop = gfc_getmem (sizeof (gfc_user_op));
  uop->name = gfc_get_string (name);
  uop->access = ACCESS_UNKNOWN;
  uop->ns = gfc_current_ns;

  return uop;
}


/* Given a name find the user operator node.  Returns NULL if it does
   not exist.  */

gfc_user_op *
gfc_find_uop (const char *name, gfc_namespace * ns)
{
  gfc_symtree *st;

  if (ns == NULL)
    ns = gfc_current_ns;

  st = gfc_find_symtree (ns->uop_root, name);
  return (st == NULL) ? NULL : st->n.uop;
}


/* Remove a gfc_symbol structure and everything it points to.  */

void
gfc_free_symbol (gfc_symbol * sym)
{

  if (sym == NULL)
    return;

  gfc_free_array_spec (sym->as);

  free_components (sym->components);

  gfc_free_expr (sym->value);

  gfc_free_namelist (sym->namelist);

  gfc_free_namespace (sym->formal_ns);

  gfc_free_interface (sym->generic);

  gfc_free_formal_arglist (sym->formal);

  gfc_free (sym);
}


/* Allocate and initialize a new symbol node.  */

gfc_symbol *
gfc_new_symbol (const char *name, gfc_namespace * ns)
{
  gfc_symbol *p;

  p = gfc_getmem (sizeof (gfc_symbol));

  gfc_clear_ts (&p->ts);
  gfc_clear_attr (&p->attr);
  p->ns = ns;

  p->declared_at = gfc_current_locus;

  if (strlen (name) > GFC_MAX_SYMBOL_LEN)
    gfc_internal_error ("new_symbol(): Symbol name too long");

  p->name = gfc_get_string (name);
  return p;
}


/* Generate an error if a symbol is ambiguous.  */

static void
ambiguous_symbol (const char *name, gfc_symtree * st)
{

  if (st->n.sym->module)
    gfc_error ("Name '%s' at %C is an ambiguous reference to '%s' "
	       "from module '%s'", name, st->n.sym->name, st->n.sym->module);
  else
    gfc_error ("Name '%s' at %C is an ambiguous reference to '%s' "
	       "from current program unit", name, st->n.sym->name);
}


/* Search for a symtree starting in the current namespace, resorting to
   any parent namespaces if requested by a nonzero parent_flag.
   Returns nonzero if the name is ambiguous.  */

int
gfc_find_sym_tree (const char *name, gfc_namespace * ns, int parent_flag,
		   gfc_symtree ** result)
{
  gfc_symtree *st;

  if (ns == NULL)
    ns = gfc_current_ns;

  do
    {
      st = gfc_find_symtree (ns->sym_root, name);
      if (st != NULL)
	{
	  *result = st;
	  if (st->ambiguous)
	    {
	      ambiguous_symbol (name, st);
	      return 1;
	    }

	  return 0;
	}

      if (!parent_flag)
	break;

      ns = ns->parent;
    }
  while (ns != NULL);

  *result = NULL;
  return 0;
}


/* Same, but returns the symbol instead.  */

int
gfc_find_symbol (const char *name, gfc_namespace * ns, int parent_flag,
		 gfc_symbol ** result)
{
  gfc_symtree *st;
  int i;

  i = gfc_find_sym_tree (name, ns, parent_flag, &st);

  if (st == NULL)
    *result = NULL;
  else
    *result = st->n.sym;

  return i;
}


/* Save symbol with the information necessary to back it out.  */

static void
save_symbol_data (gfc_symbol * sym)
{

  if (sym->new || sym->old_symbol != NULL)
    return;

  sym->old_symbol = gfc_getmem (sizeof (gfc_symbol));
  *(sym->old_symbol) = *sym;

  sym->tlink = changed_syms;
  changed_syms = sym;
}


/* Given a name, find a symbol, or create it if it does not exist yet
   in the current namespace.  If the symbol is found we make sure that
   it's OK.

   The integer return code indicates
     0   All OK
     1   The symbol name was ambiguous
     2   The name meant to be established was already host associated.

   So if the return value is nonzero, then an error was issued.  */

int
gfc_get_sym_tree (const char *name, gfc_namespace * ns, gfc_symtree ** result)
{
  gfc_symtree *st;
  gfc_symbol *p;

  /* This doesn't usually happen during resolution.  */
  if (ns == NULL)
    ns = gfc_current_ns;

  /* Try to find the symbol in ns.  */
  st = gfc_find_symtree (ns->sym_root, name);

  if (st == NULL)
    {
      /* If not there, create a new symbol.  */
      p = gfc_new_symbol (name, ns);

      /* Add to the list of tentative symbols.  */
      p->old_symbol = NULL;
      p->tlink = changed_syms;
      p->mark = 1;
      p->new = 1;
      changed_syms = p;

      st = gfc_new_symtree (&ns->sym_root, name);
      st->n.sym = p;
      p->refs++;

    }
  else
    {
      /* Make sure the existing symbol is OK.  */
      if (st->ambiguous)
	{
	  ambiguous_symbol (name, st);
	  return 1;
	}

      p = st->n.sym;

      if (p->ns != ns && (!p->attr.function || ns->proc_name != p))
	{
	  /* Symbol is from another namespace.  */
	  gfc_error ("Symbol '%s' at %C has already been host associated",
		     name);
	  return 2;
	}

      p->mark = 1;

      /* Copy in case this symbol is changed.  */
      save_symbol_data (p);
    }

  *result = st;
  return 0;
}


int
gfc_get_symbol (const char *name, gfc_namespace * ns, gfc_symbol ** result)
{
  gfc_symtree *st;
  int i;


  i = gfc_get_sym_tree (name, ns, &st);
  if (i != 0)
    return i;

  if (st)
    *result = st->n.sym;
  else
    *result = NULL;
  return i;
}


/* Subroutine that searches for a symbol, creating it if it doesn't
   exist, but tries to host-associate the symbol if possible.  */

int
gfc_get_ha_sym_tree (const char *name, gfc_symtree ** result)
{
  gfc_symtree *st;
  int i;

  i = gfc_find_sym_tree (name, gfc_current_ns, 0, &st);
  if (st != NULL)
    {
      save_symbol_data (st->n.sym);

      *result = st;
      return i;
    }

  if (gfc_current_ns->parent != NULL)
    {
      i = gfc_find_sym_tree (name, gfc_current_ns->parent, 1, &st);
      if (i)
	return i;

      if (st != NULL)
	{
	  *result = st;
	  return 0;
	}
    }

  return gfc_get_sym_tree (name, gfc_current_ns, result);
}


int
gfc_get_ha_symbol (const char *name, gfc_symbol ** result)
{
  int i;
  gfc_symtree *st;

  i = gfc_get_ha_sym_tree (name, &st);

  if (st)
    *result = st->n.sym;
  else
    *result = NULL;

  return i;
}

/* Return true if both symbols could refer to the same data object.  Does
   not take account of aliasing due to equivalence statements.  */

int
gfc_symbols_could_alias (gfc_symbol * lsym, gfc_symbol * rsym)
{
  /* Aliasing isn't possible if the symbols have different base types.  */
  if (gfc_compare_types (&lsym->ts, &rsym->ts) == 0)
    return 0;

  /* Pointers can point to other pointers, target objects and allocatable
     objects.  Two allocatable objects cannot share the same storage.  */
  if (lsym->attr.pointer
      && (rsym->attr.pointer || rsym->attr.allocatable || rsym->attr.target))
    return 1;
  if (lsym->attr.target && rsym->attr.pointer)
    return 1;
  if (lsym->attr.allocatable && rsym->attr.pointer)
    return 1;

  return 0;
}


/* Undoes all the changes made to symbols in the current statement.
   This subroutine is made simpler due to the fact that attributes are
   never removed once added.  */

void
gfc_undo_symbols (void)
{
  gfc_symbol *p, *q, *old;

  for (p = changed_syms; p; p = q)
    {
      q = p->tlink;

      if (p->new)
	{
	  /* Symbol was new.  */
	  delete_symtree (&p->ns->sym_root, p->name);

	  p->refs--;
	  if (p->refs < 0)
	    gfc_internal_error ("gfc_undo_symbols(): Negative refs");
	  if (p->refs == 0)
	    gfc_free_symbol (p);
	  continue;
	}

      /* Restore previous state of symbol.  Just copy simple stuff.  */
      p->mark = 0;
      old = p->old_symbol;

      p->ts.type = old->ts.type;
      p->ts.kind = old->ts.kind;

      p->attr = old->attr;

      if (p->value != old->value)
	{
	  gfc_free_expr (old->value);
	  p->value = NULL;
	}

      if (p->as != old->as)
	{
	  if (p->as)
	    gfc_free_array_spec (p->as);
	  p->as = old->as;
	}

      p->generic = old->generic;
      p->component_access = old->component_access;

      if (p->namelist != NULL && old->namelist == NULL)
	{
	  gfc_free_namelist (p->namelist);
	  p->namelist = NULL;
	}
      else
	{

	  if (p->namelist_tail != old->namelist_tail)
	    {
	      gfc_free_namelist (old->namelist_tail);
	      old->namelist_tail->next = NULL;
	    }
	}

      p->namelist_tail = old->namelist_tail;

      if (p->formal != old->formal)
	{
	  gfc_free_formal_arglist (p->formal);
	  p->formal = old->formal;
	}

      gfc_free (p->old_symbol);
      p->old_symbol = NULL;
      p->tlink = NULL;
    }

  changed_syms = NULL;
}


/* Makes the changes made in the current statement permanent-- gets
   rid of undo information.  */

void
gfc_commit_symbols (void)
{
  gfc_symbol *p, *q;

  for (p = changed_syms; p; p = q)
    {
      q = p->tlink;
      p->tlink = NULL;
      p->mark = 0;
      p->new = 0;

      if (p->old_symbol != NULL)
	{
	  gfc_free (p->old_symbol);
	  p->old_symbol = NULL;
	}
    }

  changed_syms = NULL;
}


/* Recursive function that deletes an entire tree and all the common
   head structures it points to.  */

static void
free_common_tree (gfc_symtree * common_tree)
{
  if (common_tree == NULL)
    return;

  free_common_tree (common_tree->left);
  free_common_tree (common_tree->right);

  gfc_free (common_tree);
}  


/* Recursive function that deletes an entire tree and all the user
   operator nodes that it contains.  */

static void
free_uop_tree (gfc_symtree * uop_tree)
{

  if (uop_tree == NULL)
    return;

  free_uop_tree (uop_tree->left);
  free_uop_tree (uop_tree->right);

  gfc_free_interface (uop_tree->n.uop->operator);

  gfc_free (uop_tree->n.uop);
  gfc_free (uop_tree);
}


/* Recursive function that deletes an entire tree and all the symbols
   that it contains.  */

static void
free_sym_tree (gfc_symtree * sym_tree)
{
  gfc_namespace *ns;
  gfc_symbol *sym;

  if (sym_tree == NULL)
    return;

  free_sym_tree (sym_tree->left);
  free_sym_tree (sym_tree->right);

  sym = sym_tree->n.sym;

  sym->refs--;
  if (sym->refs < 0)
    gfc_internal_error ("free_sym_tree(): Negative refs");

  if (sym->formal_ns != NULL && sym->refs == 1)
    {
      /* As formal_ns contains a reference to sym, delete formal_ns just
         before the deletion of sym.  */
      ns = sym->formal_ns;
      sym->formal_ns = NULL;
      gfc_free_namespace (ns);
    }
  else if (sym->refs == 0)
    {
      /* Go ahead and delete the symbol.  */
      gfc_free_symbol (sym);
    }

  gfc_free (sym_tree);
}


/* Free a namespace structure and everything below it.  Interface
   lists associated with intrinsic operators are not freed.  These are
   taken care of when a specific name is freed.  */

void
gfc_free_namespace (gfc_namespace * ns)
{
  gfc_charlen *cl, *cl2;
  gfc_namespace *p, *q;
  gfc_intrinsic_op i;

  if (ns == NULL)
    return;

  ns->refs--;
  if (ns->refs > 0)
    return;
  gcc_assert (ns->refs == 0);

  gfc_free_statements (ns->code);

  free_sym_tree (ns->sym_root);
  free_uop_tree (ns->uop_root);
  free_common_tree (ns->common_root);

  for (cl = ns->cl_list; cl; cl = cl2)
    {
      cl2 = cl->next;
      gfc_free_expr (cl->length);
      gfc_free (cl);
    }

  free_st_labels (ns->st_labels);

  gfc_free_equiv (ns->equiv);

  for (i = GFC_INTRINSIC_BEGIN; i != GFC_INTRINSIC_END; i++)
    gfc_free_interface (ns->operator[i]);

  gfc_free_data (ns->data);
  p = ns->contained;
  gfc_free (ns);

  /* Recursively free any contained namespaces.  */
  while (p != NULL)
    {
      q = p;
      p = p->sibling;

      gfc_free_namespace (q);
    }
}


void
gfc_symbol_init_2 (void)
{

  gfc_current_ns = gfc_get_namespace (NULL, 0);
}


void
gfc_symbol_done_2 (void)
{

  gfc_free_namespace (gfc_current_ns);
  gfc_current_ns = NULL;
}


/* Clear mark bits from symbol nodes associated with a symtree node.  */

static void
clear_sym_mark (gfc_symtree * st)
{

  st->n.sym->mark = 0;
}


/* Recursively traverse the symtree nodes.  */

void
gfc_traverse_symtree (gfc_symtree * st, void (*func) (gfc_symtree *))
{
  if (st != NULL)
    {
      (*func) (st);

      gfc_traverse_symtree (st->left, func);
      gfc_traverse_symtree (st->right, func);
    }
}


/* Recursive namespace traversal function.  */

static void
traverse_ns (gfc_symtree * st, void (*func) (gfc_symbol *))
{

  if (st == NULL)
    return;

  if (st->n.sym->mark == 0)
    (*func) (st->n.sym);
  st->n.sym->mark = 1;

  traverse_ns (st->left, func);
  traverse_ns (st->right, func);
}


/* Call a given function for all symbols in the namespace.  We take
   care that each gfc_symbol node is called exactly once.  */

void
gfc_traverse_ns (gfc_namespace * ns, void (*func) (gfc_symbol *))
{

  gfc_traverse_symtree (ns->sym_root, clear_sym_mark);

  traverse_ns (ns->sym_root, func);
}


/* Return TRUE if the symbol is an automatic variable.  */
static bool
gfc_is_var_automatic (gfc_symbol * sym)
{
  /* Pointer and allocatable variables are never automatic.  */
  if (sym->attr.pointer || sym->attr.allocatable)
    return false;
  /* Check for arrays with non-constant size.  */
  if (sym->attr.dimension && sym->as
      && !gfc_is_compile_time_shape (sym->as))
    return true;
  /* Check for non-constant length character vairables.  */
  if (sym->ts.type == BT_CHARACTER
      && sym->ts.cl
      && !gfc_is_constant_expr (sym->ts.cl->length))
    return true;
  return false;
}

/* Given a symbol, mark it as SAVEd if it is allowed.  */

static void
save_symbol (gfc_symbol * sym)
{

  if (sym->attr.use_assoc)
    return;

  if (sym->attr.in_common
      || sym->attr.dummy
      || sym->attr.flavor != FL_VARIABLE)
    return;
  /* Automatic objects are not saved.  */
  if (gfc_is_var_automatic (sym))
    return;
  gfc_add_save (&sym->attr, sym->name, &sym->declared_at);
}


/* Mark those symbols which can be SAVEd as such.  */

void
gfc_save_all (gfc_namespace * ns)
{

  gfc_traverse_ns (ns, save_symbol);
}


#ifdef GFC_DEBUG
/* Make sure that no changes to symbols are pending.  */

void
gfc_symbol_state(void) {

  if (changed_syms != NULL)
    gfc_internal_error("Symbol changes still pending!");
}
#endif


/************** Global symbol handling ************/


/* Search a tree for the global symbol.  */

gfc_gsymbol *
gfc_find_gsymbol (gfc_gsymbol *symbol, const char *name)
{
  gfc_gsymbol *s;

  if (symbol == NULL)
    return NULL;
  if (strcmp (symbol->name, name) == 0)
    return symbol;

  s = gfc_find_gsymbol (symbol->left, name);
  if (s != NULL)
    return s;

  s = gfc_find_gsymbol (symbol->right, name);
  if (s != NULL)
    return s;

  return NULL;
}


/* Compare two global symbols. Used for managing the BB tree.  */

static int
gsym_compare (void * _s1, void * _s2)
{
  gfc_gsymbol *s1, *s2;

  s1 = (gfc_gsymbol *)_s1;
  s2 = (gfc_gsymbol *)_s2;
  return strcmp(s1->name, s2->name);
}


/* Get a global symbol, creating it if it doesn't exist.  */

gfc_gsymbol *
gfc_get_gsymbol (const char *name)
{
  gfc_gsymbol *s;

  s = gfc_find_gsymbol (gfc_gsym_root, name);
  if (s != NULL)
    return s;

  s = gfc_getmem (sizeof (gfc_gsymbol));
  s->type = GSYM_UNKNOWN;
  strcpy (s->name, name);

  gfc_insert_bbt (&gfc_gsym_root, s, gsym_compare);

  return s;
}
