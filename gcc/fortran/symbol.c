/* Maintain binary trees of symbols.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
#include "flags.h"
#include "gfortran.h"
#include "parse.h"
#include "match.h"


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
    minit ("BODY", IFSRC_IFBODY)
};

const mstring save_status[] =
{
    minit ("UNKNOWN", SAVE_NONE),
    minit ("EXPLICIT-SAVE", SAVE_EXPLICIT),
    minit ("IMPLICIT-SAVE", SAVE_IMPLICIT),
};

/* This is to make sure the backend generates setup code in the correct
   order.  */

static int next_dummy_order = 1;


gfc_namespace *gfc_current_ns;
gfc_namespace *gfc_global_ns_list;

gfc_gsymbol *gfc_gsym_root = NULL;

static gfc_symbol *changed_syms = NULL;

gfc_dt_list *gfc_derived_types;


/* List of tentative typebound-procedures.  */

typedef struct tentative_tbp
{
  gfc_typebound_proc *proc;
  struct tentative_tbp *next;
}
tentative_tbp;

static tentative_tbp *tentative_tbp_list = NULL;


/*********** IMPLICIT NONE and IMPLICIT statement handlers ***********/

/* The following static variable indicates whether a particular element has
   been explicitly set or not.  */

static int new_flag[GFC_LETTERS];


/* Handle a correctly parsed IMPLICIT NONE.  */

void
gfc_set_implicit_none (void)
{
  int i;

  if (gfc_current_ns->seen_implicit_none)
    {
      gfc_error ("Duplicate IMPLICIT NONE statement at %C");
      return;
    }

  gfc_current_ns->seen_implicit_none = 1;

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

gfc_try
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

gfc_try
gfc_merge_new_implicit (gfc_typespec *ts)
{
  int i;

  if (gfc_current_ns->seen_implicit_none)
    {
      gfc_error ("Cannot specify IMPLICIT at %C after IMPLICIT NONE");
      return FAILURE;
    }

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
	  gfc_current_ns->implicit_loc[i] = gfc_current_locus;
	  gfc_current_ns->set_flag[i] = 1;
	}
    }
  return SUCCESS;
}


/* Given a symbol, return a pointer to the typespec for its default type.  */

gfc_typespec *
gfc_get_default_type (const char *name, gfc_namespace *ns)
{
  char letter;

  letter = name[0];

  if (gfc_option.flag_allow_leading_underscore && letter == '_')
    gfc_internal_error ("Option -fallow-leading-underscore is for use only by "
			"gfortran developers, and should not be used for "
			"implicitly typed variables");

  if (letter < 'a' || letter > 'z')
    gfc_internal_error ("gfc_get_default_type(): Bad symbol '%s'", name);

  if (ns == NULL)
    ns = gfc_current_ns;

  return &ns->default_type[letter - 'a'];
}


/* Given a pointer to a symbol, set its type according to the first
   letter of its name.  Fails if the letter in question has no default
   type.  */

gfc_try
gfc_set_default_type (gfc_symbol *sym, int error_flag, gfc_namespace *ns)
{
  gfc_typespec *ts;

  if (sym->ts.type != BT_UNKNOWN)
    gfc_internal_error ("gfc_set_default_type(): symbol already has a type");

  ts = gfc_get_default_type (sym->name, ns);

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

  if (ts->cl)
    {
      sym->ts.cl = gfc_get_charlen ();
      *sym->ts.cl = *ts->cl;
    }

  if (sym->attr.is_bind_c == 1)
    {
      /* BIND(C) variables should not be implicitly declared.  */
      gfc_warning_now ("Implicitly declared BIND(C) variable '%s' at %L may "
                       "not be C interoperable", sym->name, &sym->declared_at);
      sym->ts.f90_type = sym->ts.type;
    }

  if (sym->attr.dummy != 0)
    {
      if (sym->ns->proc_name != NULL
	  && (sym->ns->proc_name->attr.subroutine != 0
	      || sym->ns->proc_name->attr.function != 0)
	  && sym->ns->proc_name->attr.is_bind_c != 0)
        {
          /* Dummy args to a BIND(C) routine may not be interoperable if
             they are implicitly typed.  */
          gfc_warning_now ("Implicitly declared variable '%s' at %L may not "
                           "be C interoperable but it is a dummy argument to "
                           "the BIND(C) procedure '%s' at %L", sym->name,
                           &(sym->declared_at), sym->ns->proc_name->name,
                           &(sym->ns->proc_name->declared_at));
          sym->ts.f90_type = sym->ts.type;
        }
    }
  
  return SUCCESS;
}


/* This function is called from parse.c(parse_progunit) to check the
   type of the function is not implicitly typed in the host namespace
   and to implicitly type the function result, if necessary.  */

void
gfc_check_function_type (gfc_namespace *ns)
{
  gfc_symbol *proc = ns->proc_name;

  if (!proc->attr.contained || proc->result->attr.implicit_type)
    return;

  if (proc->result->ts.type == BT_UNKNOWN && proc->result->ts.interface == NULL)
    {
      if (gfc_set_default_type (proc->result, 0, gfc_current_ns)
		== SUCCESS)
	{
	  if (proc->result != proc)
	    {
	      proc->ts = proc->result->ts;
	      proc->as = gfc_copy_array_spec (proc->result->as);
	      proc->attr.dimension = proc->result->attr.dimension;
	      proc->attr.pointer = proc->result->attr.pointer;
	      proc->attr.allocatable = proc->result->attr.allocatable;
	    }
	}
      else if (!proc->result->attr.proc_pointer)
	{
	  gfc_error ("Function result '%s' at %L has no IMPLICIT type",
		     proc->result->name, &proc->result->declared_at);
	  proc->result->attr.untyped = 1;
	}
    }
}


/******************** Symbol attribute stuff *********************/

/* This is a generic conflict-checker.  We do this to avoid having a
   single conflict in two places.  */

#define conf(a, b) if (attr->a && attr->b) { a1 = a; a2 = b; goto conflict; }
#define conf2(a) if (attr->a) { a2 = a; goto conflict; }
#define conf_std(a, b, std) if (attr->a && attr->b)\
                              {\
                                a1 = a;\
                                a2 = b;\
                                standard = std;\
                                goto conflict_std;\
                              }

static gfc_try
check_conflict (symbol_attribute *attr, const char *name, locus *where)
{
  static const char *dummy = "DUMMY", *save = "SAVE", *pointer = "POINTER",
    *target = "TARGET", *external = "EXTERNAL", *intent = "INTENT",
    *intent_in = "INTENT(IN)", *intrinsic = "INTRINSIC",
    *intent_out = "INTENT(OUT)", *intent_inout = "INTENT(INOUT)",
    *allocatable = "ALLOCATABLE", *elemental = "ELEMENTAL",
    *privat = "PRIVATE", *recursive = "RECURSIVE",
    *in_common = "COMMON", *result = "RESULT", *in_namelist = "NAMELIST",
    *publik = "PUBLIC", *optional = "OPTIONAL", *entry = "ENTRY",
    *function = "FUNCTION", *subroutine = "SUBROUTINE",
    *dimension = "DIMENSION", *in_equivalence = "EQUIVALENCE",
    *use_assoc = "USE ASSOCIATED", *cray_pointer = "CRAY POINTER",
    *cray_pointee = "CRAY POINTEE", *data = "DATA", *value = "VALUE",
    *volatile_ = "VOLATILE", *is_protected = "PROTECTED",
    *is_bind_c = "BIND(C)", *procedure = "PROCEDURE";
  static const char *threadprivate = "THREADPRIVATE";

  const char *a1, *a2;
  int standard;

  if (where == NULL)
    where = &gfc_current_locus;

  if (attr->pointer && attr->intent != INTENT_UNKNOWN)
    {
      a1 = pointer;
      a2 = intent;
      standard = GFC_STD_F2003;
      goto conflict_std;
    }

  /* Check for attributes not allowed in a BLOCK DATA.  */
  if (gfc_current_state () == COMP_BLOCK_DATA)
    {
      a1 = NULL;

      if (attr->in_namelist)
	a1 = in_namelist;
      if (attr->allocatable)
	a1 = allocatable;
      if (attr->external)
	a1 = external;
      if (attr->optional)
	a1 = optional;
      if (attr->access == ACCESS_PRIVATE)
	a1 = privat;
      if (attr->access == ACCESS_PUBLIC)
	a1 = publik;
      if (attr->intent != INTENT_UNKNOWN)
	a1 = intent;

      if (a1 != NULL)
	{
	  gfc_error
	    ("%s attribute not allowed in BLOCK DATA program unit at %L",
	     a1, where);
	  return FAILURE;
	}
    }

  if (attr->save == SAVE_EXPLICIT)
    {
      conf (dummy, save);
      conf (in_common, save);
      conf (result, save);

      switch (attr->flavor)
	{
	  case FL_PROGRAM:
	  case FL_BLOCK_DATA:
	  case FL_MODULE:
	  case FL_LABEL:
	  case FL_DERIVED:
	  case FL_PARAMETER:
            a1 = gfc_code2string (flavors, attr->flavor);
            a2 = save;
	    goto conflict;

	  case FL_PROCEDURE:
	    /* Conflicts between SAVE and PROCEDURE will be checked at
	       resolution stage, see "resolve_fl_procedure".  */
	  case FL_VARIABLE:
	  case FL_NAMELIST:
	  default:
	    break;
	}
    }

  conf (dummy, entry);
  conf (dummy, intrinsic);
  conf (dummy, threadprivate);
  conf (pointer, target);
  conf (pointer, intrinsic);
  conf (pointer, elemental);
  conf (allocatable, elemental);

  conf (target, external);
  conf (target, intrinsic);

  if (!attr->if_source)
    conf (external, dimension);   /* See Fortran 95's R504.  */

  conf (external, intrinsic);
  conf (entry, intrinsic);

  if ((attr->if_source == IFSRC_DECL && !attr->procedure) || attr->contained)
    conf (external, subroutine);

  if (attr->proc_pointer && gfc_notify_std (GFC_STD_F2003,
			    "Fortran 2003: Procedure pointer at %C") == FAILURE)
    return FAILURE;

  conf (allocatable, pointer);
  conf_std (allocatable, dummy, GFC_STD_F2003);
  conf_std (allocatable, function, GFC_STD_F2003);
  conf_std (allocatable, result, GFC_STD_F2003);
  conf (elemental, recursive);

  conf (in_common, dummy);
  conf (in_common, allocatable);
  conf (in_common, result);

  conf (dummy, result);

  conf (in_equivalence, use_assoc);
  conf (in_equivalence, dummy);
  conf (in_equivalence, target);
  conf (in_equivalence, pointer);
  conf (in_equivalence, function);
  conf (in_equivalence, result);
  conf (in_equivalence, entry);
  conf (in_equivalence, allocatable);
  conf (in_equivalence, threadprivate);

  conf (in_namelist, pointer);
  conf (in_namelist, allocatable);

  conf (entry, result);

  conf (function, subroutine);

  if (!function && !subroutine)
    conf (is_bind_c, dummy);

  conf (is_bind_c, cray_pointer);
  conf (is_bind_c, cray_pointee);
  conf (is_bind_c, allocatable);
  conf (is_bind_c, elemental);

  /* Need to also get volatile attr, according to 5.1 of F2003 draft.
     Parameter conflict caught below.  Also, value cannot be specified
     for a dummy procedure.  */

  /* Cray pointer/pointee conflicts.  */
  conf (cray_pointer, cray_pointee);
  conf (cray_pointer, dimension);
  conf (cray_pointer, pointer);
  conf (cray_pointer, target);
  conf (cray_pointer, allocatable);
  conf (cray_pointer, external);
  conf (cray_pointer, intrinsic);
  conf (cray_pointer, in_namelist);
  conf (cray_pointer, function);
  conf (cray_pointer, subroutine);
  conf (cray_pointer, entry);

  conf (cray_pointee, allocatable);
  conf (cray_pointee, intent);
  conf (cray_pointee, optional);
  conf (cray_pointee, dummy);
  conf (cray_pointee, target);
  conf (cray_pointee, intrinsic);
  conf (cray_pointee, pointer);
  conf (cray_pointee, entry);
  conf (cray_pointee, in_common);
  conf (cray_pointee, in_equivalence);
  conf (cray_pointee, threadprivate);

  conf (data, dummy);
  conf (data, function);
  conf (data, result);
  conf (data, allocatable);
  conf (data, use_assoc);

  conf (value, pointer)
  conf (value, allocatable)
  conf (value, subroutine)
  conf (value, function)
  conf (value, volatile_)
  conf (value, dimension)
  conf (value, external)

  if (attr->value
      && (attr->intent == INTENT_OUT || attr->intent == INTENT_INOUT))
    {
      a1 = value;
      a2 = attr->intent == INTENT_OUT ? intent_out : intent_inout;
      goto conflict;
    }

  conf (is_protected, intrinsic)
  conf (is_protected, external)
  conf (is_protected, in_common)

  conf (volatile_, intrinsic)
  conf (volatile_, external)

  if (attr->volatile_ && attr->intent == INTENT_IN)
    {
      a1 = volatile_;
      a2 = intent_in;
      goto conflict;
    }

  conf (procedure, allocatable)
  conf (procedure, dimension)
  conf (procedure, intrinsic)
  conf (procedure, is_protected)
  conf (procedure, target)
  conf (procedure, value)
  conf (procedure, volatile_)
  conf (procedure, entry)

  a1 = gfc_code2string (flavors, attr->flavor);

  if (attr->in_namelist
      && attr->flavor != FL_VARIABLE
      && attr->flavor != FL_PROCEDURE
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
      conf2 (dimension);
      conf2 (dummy);
      conf2 (volatile_);
      conf2 (pointer);
      conf2 (is_protected);
      conf2 (target);
      conf2 (external);
      conf2 (intrinsic);
      conf2 (allocatable);
      conf2 (result);
      conf2 (in_namelist);
      conf2 (optional);
      conf2 (function);
      conf2 (subroutine);
      conf2 (threadprivate);

      if (attr->access == ACCESS_PUBLIC || attr->access == ACCESS_PRIVATE)
	{
	  a2 = attr->access == ACCESS_PUBLIC ? publik : privat;
	  gfc_error ("%s attribute applied to %s %s at %L", a2, a1,
	    name, where);
	  return FAILURE;
	}

      if (attr->is_bind_c)
	{
	  gfc_error_now ("BIND(C) applied to %s %s at %L", a1, name, where);
	  return FAILURE;
	}

      break;

    case FL_VARIABLE:
      break;

    case FL_NAMELIST:
      conf2 (result);
      break;

    case FL_PROCEDURE:
      /* Conflicts with INTENT, SAVE and RESULT will be checked
	 at resolution stage, see "resolve_fl_procedure".  */

      if (attr->subroutine)
	{
	  conf2 (target);
	  conf2 (allocatable);
	  conf2 (in_namelist);
	  conf2 (dimension);
	  conf2 (function);
	  conf2 (threadprivate);
	}

      if (!attr->proc_pointer)
	conf2 (in_common);

      switch (attr->proc)
	{
	case PROC_ST_FUNCTION:
	  conf2 (dummy);
	  break;

	case PROC_MODULE:
	  conf2 (dummy);
	  break;

	case PROC_DUMMY:
	  conf2 (result);
	  conf2 (threadprivate);
	  break;

	default:
	  break;
	}

      break;

    case FL_DERIVED:
      conf2 (dummy);
      conf2 (pointer);
      conf2 (target);
      conf2 (external);
      conf2 (intrinsic);
      conf2 (allocatable);
      conf2 (optional);
      conf2 (entry);
      conf2 (function);
      conf2 (subroutine);
      conf2 (threadprivate);
      conf2 (result);

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
      conf2 (is_protected);
      conf2 (target);
      conf2 (dummy);
      conf2 (in_common);
      conf2 (value);
      conf2 (volatile_);
      conf2 (threadprivate);
      conf2 (value);
      conf2 (is_bind_c);
      conf2 (result);
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

conflict_std:
  if (name == NULL)
    {
      return gfc_notify_std (standard, "Fortran 2003: %s attribute "
                             "with %s attribute at %L", a1, a2,
                             where);
    }
  else
    {
      return gfc_notify_std (standard, "Fortran 2003: %s attribute "
			     "with %s attribute in '%s' at %L",
                             a1, a2, name, where);
    }
}

#undef conf
#undef conf2
#undef conf_std


/* Mark a symbol as referenced.  */

void
gfc_set_sym_referenced (gfc_symbol *sym)
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
check_used (symbol_attribute *attr, const char *name, locus *where)
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


/* Generate an error because of a duplicate attribute.  */

static void
duplicate_attr (const char *attr, locus *where)
{

  if (where == NULL)
    where = &gfc_current_locus;

  gfc_error ("Duplicate %s attribute specified at %L", attr, where);
}


gfc_try
gfc_add_ext_attribute (symbol_attribute *attr, ext_attr_id_t ext_attr,
		       locus *where ATTRIBUTE_UNUSED)
{
  attr->ext_attr |= 1 << ext_attr;
  return SUCCESS;
}


/* Called from decl.c (attr_decl1) to check attributes, when declared
   separately.  */

gfc_try
gfc_add_attribute (symbol_attribute *attr, locus *where)
{
  if (check_used (attr, NULL, where))
    return FAILURE;

  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_allocatable (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->allocatable)
    {
      duplicate_attr ("ALLOCATABLE", where);
      return FAILURE;
    }

  if (attr->flavor == FL_PROCEDURE && attr->if_source == IFSRC_IFBODY
      && gfc_find_state (COMP_INTERFACE) == FAILURE)
    {
      gfc_error ("ALLOCATABLE specified outside of INTERFACE body at %L",
		 where);
      return FAILURE;
    }

  attr->allocatable = 1;
  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_dimension (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  if (attr->dimension)
    {
      duplicate_attr ("DIMENSION", where);
      return FAILURE;
    }

  if (attr->flavor == FL_PROCEDURE && attr->if_source == IFSRC_IFBODY
      && gfc_find_state (COMP_INTERFACE) == FAILURE)
    {
      gfc_error ("DIMENSION specified for '%s' outside its INTERFACE body "
		 "at %L", name, where);
      return FAILURE;
    }

  attr->dimension = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_external (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->external)
    {
      duplicate_attr ("EXTERNAL", where);
      return FAILURE;
    }

  if (attr->pointer && attr->if_source != IFSRC_IFBODY)
    {
      attr->pointer = 0;
      attr->proc_pointer = 1;
    }

  attr->external = 1;

  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_intrinsic (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->intrinsic)
    {
      duplicate_attr ("INTRINSIC", where);
      return FAILURE;
    }

  attr->intrinsic = 1;

  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_optional (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->optional)
    {
      duplicate_attr ("OPTIONAL", where);
      return FAILURE;
    }

  attr->optional = 1;
  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_pointer (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->pointer && !(attr->if_source == IFSRC_IFBODY
      && gfc_find_state (COMP_INTERFACE) == FAILURE))
    {
      duplicate_attr ("POINTER", where);
      return FAILURE;
    }

  if (attr->procedure || (attr->external && attr->if_source != IFSRC_IFBODY)
      || (attr->if_source == IFSRC_IFBODY
      && gfc_find_state (COMP_INTERFACE) == FAILURE))
    attr->proc_pointer = 1;
  else
    attr->pointer = 1;

  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_cray_pointer (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  attr->cray_pointer = 1;
  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_cray_pointee (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->cray_pointee)
    {
      gfc_error ("Cray Pointee at %L appears in multiple pointer()"
		 " statements", where);
      return FAILURE;
    }

  attr->cray_pointee = 1;
  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_protected (symbol_attribute *attr, const char *name, locus *where)
{
  if (check_used (attr, name, where))
    return FAILURE;

  if (attr->is_protected)
    {
	if (gfc_notify_std (GFC_STD_LEGACY, 
			    "Duplicate PROTECTED attribute specified at %L",
			    where) 
	    == FAILURE)
	  return FAILURE;
    }

  attr->is_protected = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_result (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  attr->result = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_save (symbol_attribute *attr, const char *name, locus *where)
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

  if (attr->save == SAVE_EXPLICIT)
    {
	if (gfc_notify_std (GFC_STD_LEGACY, 
			    "Duplicate SAVE attribute specified at %L",
			    where) 
	    == FAILURE)
	  return FAILURE;
    }

  attr->save = SAVE_EXPLICIT;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_value (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  if (attr->value)
    {
	if (gfc_notify_std (GFC_STD_LEGACY, 
			    "Duplicate VALUE attribute specified at %L",
			    where) 
	    == FAILURE)
	  return FAILURE;
    }

  attr->value = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_volatile (symbol_attribute *attr, const char *name, locus *where)
{
  /* No check_used needed as 11.2.1 of the F2003 standard allows
     that the local identifier made accessible by a use statement can be
     given a VOLATILE attribute.  */

  if (attr->volatile_ && attr->volatile_ns == gfc_current_ns)
    if (gfc_notify_std (GFC_STD_LEGACY, 
        		"Duplicate VOLATILE attribute specified at %L", where)
        == FAILURE)
      return FAILURE;

  attr->volatile_ = 1;
  attr->volatile_ns = gfc_current_ns;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_threadprivate (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  if (attr->threadprivate)
    {
      duplicate_attr ("THREADPRIVATE", where);
      return FAILURE;
    }

  attr->threadprivate = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_target (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->target)
    {
      duplicate_attr ("TARGET", where);
      return FAILURE;
    }

  attr->target = 1;
  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_dummy (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  /* Duplicate dummy arguments are allowed due to ENTRY statements.  */
  attr->dummy = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_in_common (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  /* Duplicate attribute already checked for.  */
  attr->in_common = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_in_equivalence (symbol_attribute *attr, const char *name, locus *where)
{

  /* Duplicate attribute already checked for.  */
  attr->in_equivalence = 1;
  if (check_conflict (attr, name, where) == FAILURE)
    return FAILURE;

  if (attr->flavor == FL_VARIABLE)
    return SUCCESS;

  return gfc_add_flavor (attr, FL_VARIABLE, name, where);
}


gfc_try
gfc_add_data (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  attr->data = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_in_namelist (symbol_attribute *attr, const char *name, locus *where)
{

  attr->in_namelist = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_sequence (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  attr->sequence = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_elemental (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->elemental)
    {
      duplicate_attr ("ELEMENTAL", where);
      return FAILURE;
    }

  attr->elemental = 1;
  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_pure (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->pure)
    {
      duplicate_attr ("PURE", where);
      return FAILURE;
    }

  attr->pure = 1;
  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_recursive (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->recursive)
    {
      duplicate_attr ("RECURSIVE", where);
      return FAILURE;
    }

  attr->recursive = 1;
  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_entry (symbol_attribute *attr, const char *name, locus *where)
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


gfc_try
gfc_add_function (symbol_attribute *attr, const char *name, locus *where)
{

  if (attr->flavor != FL_PROCEDURE
      && gfc_add_flavor (attr, FL_PROCEDURE, name, where) == FAILURE)
    return FAILURE;

  attr->function = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_subroutine (symbol_attribute *attr, const char *name, locus *where)
{

  if (attr->flavor != FL_PROCEDURE
      && gfc_add_flavor (attr, FL_PROCEDURE, name, where) == FAILURE)
    return FAILURE;

  attr->subroutine = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_generic (symbol_attribute *attr, const char *name, locus *where)
{

  if (attr->flavor != FL_PROCEDURE
      && gfc_add_flavor (attr, FL_PROCEDURE, name, where) == FAILURE)
    return FAILURE;

  attr->generic = 1;
  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_proc (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, NULL, where))
    return FAILURE;

  if (attr->flavor != FL_PROCEDURE
      && gfc_add_flavor (attr, FL_PROCEDURE, name, where) == FAILURE)
    return FAILURE;

  if (attr->procedure)
    {
      duplicate_attr ("PROCEDURE", where);
      return FAILURE;
    }

  attr->procedure = 1;

  return check_conflict (attr, NULL, where);
}


gfc_try
gfc_add_abstract (symbol_attribute* attr, locus* where)
{
  if (attr->abstract)
    {
      duplicate_attr ("ABSTRACT", where);
      return FAILURE;
    }

  attr->abstract = 1;
  return SUCCESS;
}


/* Flavors are special because some flavors are not what Fortran
   considers attributes and can be reaffirmed multiple times.  */

gfc_try
gfc_add_flavor (symbol_attribute *attr, sym_flavor f, const char *name,
		locus *where)
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

      if (name)
        gfc_error ("%s attribute of '%s' conflicts with %s attribute at %L",
		   gfc_code2string (flavors, attr->flavor), name,
		   gfc_code2string (flavors, f), where);
      else
        gfc_error ("%s attribute conflicts with %s attribute at %L",
		   gfc_code2string (flavors, attr->flavor),
		   gfc_code2string (flavors, f), where);

      return FAILURE;
    }

  attr->flavor = f;

  return check_conflict (attr, name, where);
}


gfc_try
gfc_add_procedure (symbol_attribute *attr, procedure_type t,
		   const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return FAILURE;

  if (attr->flavor != FL_PROCEDURE
      && gfc_add_flavor (attr, FL_PROCEDURE, name, where) == FAILURE)
    return FAILURE;

  if (where == NULL)
    where = &gfc_current_locus;

  if (attr->proc != PROC_UNKNOWN)
    {
      gfc_error ("%s procedure at %L is already declared as %s procedure",
		 gfc_code2string (procedures, t), where,
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


gfc_try
gfc_add_intent (symbol_attribute *attr, sym_intent intent, locus *where)
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

gfc_try
gfc_add_access (symbol_attribute *attr, gfc_access access,
		const char *name, locus *where)
{

  if (attr->access == ACCESS_UNKNOWN
	|| (attr->use_assoc && attr->access != ACCESS_PRIVATE))
    {
      attr->access = access;
      return check_conflict (attr, name, where);
    }

  if (where == NULL)
    where = &gfc_current_locus;
  gfc_error ("ACCESS specification at %L was already specified", where);

  return FAILURE;
}


/* Set the is_bind_c field for the given symbol_attribute.  */

gfc_try
gfc_add_is_bind_c (symbol_attribute *attr, const char *name, locus *where,
                   int is_proc_lang_bind_spec)
{

  if (is_proc_lang_bind_spec == 0 && attr->flavor == FL_PROCEDURE)
    gfc_error_now ("BIND(C) attribute at %L can only be used for "
		   "variables or common blocks", where);
  else if (attr->is_bind_c)
    gfc_error_now ("Duplicate BIND attribute specified at %L", where);
  else
    attr->is_bind_c = 1;
  
  if (where == NULL)
    where = &gfc_current_locus;
   
  if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: BIND(C) at %L", where)
      == FAILURE)
    return FAILURE;

  return check_conflict (attr, name, where);
}


/* Set the extension field for the given symbol_attribute.  */

gfc_try
gfc_add_extension (symbol_attribute *attr, locus *where)
{
  if (where == NULL)
    where = &gfc_current_locus;

  if (attr->extension)
    gfc_error_now ("Duplicate EXTENDS attribute specified at %L", where);
  else
    attr->extension = 1;

  if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: EXTENDS at %L", where)
	== FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_add_explicit_interface (gfc_symbol *sym, ifsrc source,
			    gfc_formal_arglist * formal, locus *where)
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

  if (source == IFSRC_IFBODY && (sym->attr.dimension || sym->attr.allocatable))
    {
      gfc_error ("'%s' at %L has attributes specified outside its INTERFACE "
		 "body", sym->name, where);
      return FAILURE;
    }

  sym->formal = formal;
  sym->attr.if_source = source;

  return SUCCESS;
}


/* Add a type to a symbol.  */

gfc_try
gfc_add_type (gfc_symbol *sym, gfc_typespec *ts, locus *where)
{
  sym_flavor flavor;
  bt type;

  if (where == NULL)
    where = &gfc_current_locus;

  if (sym->result)
    type = sym->result->ts.type;
  else
    type = sym->ts.type;

  if (sym->attr.result && type == BT_UNKNOWN && sym->ns->proc_name)
    type = sym->ns->proc_name->ts.type;

  if (type != BT_UNKNOWN && !(sym->attr.function && sym->attr.implicit_type))
    {
      gfc_error ("Symbol '%s' at %L already has basic type of %s", sym->name,
		 where, gfc_basic_typename (type));
      return FAILURE;
    }

  if (sym->attr.procedure && sym->ts.interface)
    {
      gfc_error ("Procedure '%s' at %L may not have basic type of %s",
		 sym->name, where, gfc_basic_typename (ts->type));
      return FAILURE;
    }

  flavor = sym->attr.flavor;

  if (flavor == FL_PROGRAM || flavor == FL_BLOCK_DATA || flavor == FL_MODULE
      || flavor == FL_LABEL
      || (flavor == FL_PROCEDURE && sym->attr.subroutine)
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
gfc_clear_attr (symbol_attribute *attr)
{
  memset (attr, 0, sizeof (symbol_attribute));
}


/* Check for missing attributes in the new symbol.  Currently does
   nothing, but it's not clear that it is unnecessary yet.  */

gfc_try
gfc_missing_attr (symbol_attribute *attr ATTRIBUTE_UNUSED,
		  locus *where ATTRIBUTE_UNUSED)
{

  return SUCCESS;
}


/* Copy an attribute to a symbol attribute, bit by bit.  Some
   attributes have a lot of side-effects but cannot be present given
   where we are called from, so we ignore some bits.  */

gfc_try
gfc_copy_attr (symbol_attribute *dest, symbol_attribute *src, locus *where)
{
  int is_proc_lang_bind_spec;
  
  /* In line with the other attributes, we only add bits but do not remove
     them; cf. also PR 41034.  */
  dest->ext_attr |= src->ext_attr;

  if (src->allocatable && gfc_add_allocatable (dest, where) == FAILURE)
    goto fail;

  if (src->dimension && gfc_add_dimension (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->optional && gfc_add_optional (dest, where) == FAILURE)
    goto fail;
  if (src->pointer && gfc_add_pointer (dest, where) == FAILURE)
    goto fail;
  if (src->is_protected && gfc_add_protected (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->save && gfc_add_save (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->value && gfc_add_value (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->volatile_ && gfc_add_volatile (dest, NULL, where) == FAILURE)
    goto fail;
  if (src->threadprivate
      && gfc_add_threadprivate (dest, NULL, where) == FAILURE)
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

  if (src->cray_pointer && gfc_add_cray_pointer (dest, where) == FAILURE)
    goto fail;
  if (src->cray_pointee && gfc_add_cray_pointee (dest, where) == FAILURE)
    goto fail;

  is_proc_lang_bind_spec = (src->flavor == FL_PROCEDURE ? 1 : 0);
  if (src->is_bind_c
      && gfc_add_is_bind_c (dest, NULL, where, is_proc_lang_bind_spec)
	 != SUCCESS)
    return FAILURE;

  if (src->is_c_interop)
    dest->is_c_interop = 1;
  if (src->is_iso_c)
    dest->is_iso_c = 1;
  
  if (src->external && gfc_add_external (dest, where) == FAILURE)
    goto fail;
  if (src->intrinsic && gfc_add_intrinsic (dest, where) == FAILURE)
    goto fail;
  if (src->proc_pointer)
    dest->proc_pointer = 1;

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

gfc_try
gfc_add_component (gfc_symbol *sym, const char *name,
		   gfc_component **component)
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

  if (sym->attr.extension
	&& gfc_find_component (sym->components->ts.derived, name, true, true))
    {
      gfc_error ("Component '%s' at %C already in the parent type "
		 "at %L", name, &sym->components->ts.derived->declared_at);
      return FAILURE;
    }

  /* Allocate a new component.  */
  p = gfc_get_component ();

  if (tail == NULL)
    sym->components = p;
  else
    tail->next = p;

  p->name = gfc_get_string (name);
  p->loc = gfc_current_locus;
  p->ts.type = BT_UNKNOWN;

  *component = p;
  return SUCCESS;
}


/* Recursive function to switch derived types of all symbol in a
   namespace.  */

static void
switch_types (gfc_symtree *st, gfc_symbol *from, gfc_symbol *to)
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
gfc_use_derived (gfc_symbol *sym)
{
  gfc_symbol *s;
  gfc_typespec *t;
  gfc_symtree *st;
  int i;

  if (sym->components != NULL || sym->attr.zero_comp)
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
  gfc_commit_symbol (sym);

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
   not found or the components are private.  If noaccess is set, no access
   checks are done.  */

gfc_component *
gfc_find_component (gfc_symbol *sym, const char *name,
		    bool noaccess, bool silent)
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

  if (p == NULL
	&& sym->attr.extension
	&& sym->components->ts.type == BT_DERIVED)
    {
      p = gfc_find_component (sym->components->ts.derived, name,
			      noaccess, silent);
      /* Do not overwrite the error.  */
      if (p == NULL)
	return p;
    }

  if (p == NULL && !silent)
    gfc_error ("'%s' at %C is not a member of the '%s' structure",
	       name, sym->name);

  else if (sym->attr.use_assoc && !noaccess)
    {
      if (p->attr.access == ACCESS_PRIVATE)
	{
	  if (!silent)
	    gfc_error ("Component '%s' at %C is a PRIVATE component of '%s'",
		       name, sym->name);
	  return NULL;
	}
	
      /* If there were components given and all components are private, error
	 out at this place.  */
      if (p->attr.access != ACCESS_PUBLIC && sym->component_access == ACCESS_PRIVATE)
	{
	  if (!silent)
	    gfc_error ("All components of '%s' are PRIVATE in structure"
		       " constructor at %C", sym->name);
	  return NULL;
	}
    }

  return p;
}


/* Given a symbol, free all of the component structures and everything
   they point to.  */

static void
free_components (gfc_component *p)
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


/******************** Statement label management ********************/

/* Comparison function for statement labels, used for managing the
   binary tree.  */

static int
compare_st_labels (void *a1, void *b1)
{
  int a = ((gfc_st_label *) a1)->value;
  int b = ((gfc_st_label *) b1)->value;

  return (b - a);
}


/* Free a single gfc_st_label structure, making sure the tree is not
   messed up.  This function is called only when some parse error
   occurs.  */

void
gfc_free_st_label (gfc_st_label *label)
{

  if (label == NULL)
    return;

  gfc_delete_bbt (&gfc_current_ns->st_labels, label, compare_st_labels);

  if (label->format != NULL)
    gfc_free_expr (label->format);

  gfc_free (label);
}


/* Free a whole tree of gfc_st_label structures.  */

static void
free_st_labels (gfc_st_label *label)
{

  if (label == NULL)
    return;

  free_st_labels (label->left);
  free_st_labels (label->right);
  
  if (label->format != NULL)
    gfc_free_expr (label->format);
  gfc_free (label);
}


/* Given a label number, search for and return a pointer to the label
   structure, creating it if it does not exist.  */

gfc_st_label *
gfc_get_st_label (int labelno)
{
  gfc_st_label *lp;

  /* First see if the label is already in this namespace.  */
  lp = gfc_current_ns->st_labels;
  while (lp)
    {
      if (lp->value == labelno)
	return lp;

      if (lp->value < labelno)
	lp = lp->left;
      else
	lp = lp->right;
    }

  lp = XCNEW (gfc_st_label);

  lp->value = labelno;
  lp->defined = ST_LABEL_UNKNOWN;
  lp->referenced = ST_LABEL_UNKNOWN;

  gfc_insert_bbt (&gfc_current_ns->st_labels, lp, compare_st_labels);

  return lp;
}


/* Called when a statement with a statement label is about to be
   accepted.  We add the label to the list of the current namespace,
   making sure it hasn't been defined previously and referenced
   correctly.  */

void
gfc_define_st_label (gfc_st_label *lp, gfc_sl_type type, locus *label_locus)
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

gfc_try
gfc_reference_st_label (gfc_st_label *lp, gfc_sl_type type)
{
  gfc_sl_type label_type;
  int labelno;
  gfc_try rc;

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


/*******A helper function for creating new expressions*************/


gfc_expr *
gfc_lval_expr_from_sym (gfc_symbol *sym)
{
  gfc_expr *lval;
  lval = gfc_get_expr ();
  lval->expr_type = EXPR_VARIABLE;
  lval->where = sym->declared_at;
  lval->ts = sym->ts;
  lval->symtree = gfc_find_symtree (sym->ns->sym_root, sym->name);

  /* It will always be a full array.  */
  lval->rank = sym->as ? sym->as->rank : 0;
  if (lval->rank)
    {
      lval->ref = gfc_get_ref ();
      lval->ref->type = REF_ARRAY;
      lval->ref->u.ar.type = AR_FULL;
      lval->ref->u.ar.dimen = lval->rank;
      lval->ref->u.ar.where = sym->declared_at;
      lval->ref->u.ar.as = sym->as;
    }

  return lval;
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
gfc_get_namespace (gfc_namespace *parent, int parent_types)
{
  gfc_namespace *ns;
  gfc_typespec *ts;
  int in;
  int i;

  ns = XCNEW (gfc_namespace);
  ns->sym_root = NULL;
  ns->uop_root = NULL;
  ns->tb_sym_root = NULL;
  ns->finalizers = NULL;
  ns->default_access = ACCESS_UNKNOWN;
  ns->parent = parent;

  for (in = GFC_INTRINSIC_BEGIN; in != GFC_INTRINSIC_END; in++)
    {
      ns->operator_access[in] = ACCESS_UNKNOWN;
      ns->tb_op[in] = NULL;
    }

  /* Initialize default implicit types.  */
  for (i = 'a'; i <= 'z'; i++)
    {
      ns->set_flag[i - 'a'] = 0;
      ts = &ns->default_type[i - 'a'];

      if (parent_types && ns->parent != NULL)
	{
	  /* Copy parent settings.  */
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
compare_symtree (void *_st1, void *_st2)
{
  gfc_symtree *st1, *st2;

  st1 = (gfc_symtree *) _st1;
  st2 = (gfc_symtree *) _st2;

  return strcmp (st1->name, st2->name);
}


/* Allocate a new symtree node and associate it with the new symbol.  */

gfc_symtree *
gfc_new_symtree (gfc_symtree **root, const char *name)
{
  gfc_symtree *st;

  st = XCNEW (gfc_symtree);
  st->name = gfc_get_string (name);

  gfc_insert_bbt (root, st, compare_symtree);
  return st;
}


/* Delete a symbol from the tree.  Does not free the symbol itself!  */

void
gfc_delete_symtree (gfc_symtree **root, const char *name)
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
gfc_find_symtree (gfc_symtree *st, const char *name)
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


/* Return a symtree node with a name that is guaranteed to be unique
   within the namespace and corresponds to an illegal fortran name.  */

gfc_symtree *
gfc_get_unique_symtree (gfc_namespace *ns)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  static int serial = 0;

  sprintf (name, "@%d", serial++);
  return gfc_new_symtree (&ns->sym_root, name);
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

  uop = st->n.uop = XCNEW (gfc_user_op);
  uop->name = gfc_get_string (name);
  uop->access = ACCESS_UNKNOWN;
  uop->ns = gfc_current_ns;

  return uop;
}


/* Given a name find the user operator node.  Returns NULL if it does
   not exist.  */

gfc_user_op *
gfc_find_uop (const char *name, gfc_namespace *ns)
{
  gfc_symtree *st;

  if (ns == NULL)
    ns = gfc_current_ns;

  st = gfc_find_symtree (ns->uop_root, name);
  return (st == NULL) ? NULL : st->n.uop;
}


/* Remove a gfc_symbol structure and everything it points to.  */

void
gfc_free_symbol (gfc_symbol *sym)
{

  if (sym == NULL)
    return;

  gfc_free_array_spec (sym->as);

  free_components (sym->components);

  gfc_free_expr (sym->value);

  gfc_free_namelist (sym->namelist);

  gfc_free_namespace (sym->formal_ns);

  if (!sym->attr.generic_copy)
    gfc_free_interface (sym->generic);

  gfc_free_formal_arglist (sym->formal);

  gfc_free_namespace (sym->f2k_derived);

  gfc_free (sym);
}


/* Allocate and initialize a new symbol node.  */

gfc_symbol *
gfc_new_symbol (const char *name, gfc_namespace *ns)
{
  gfc_symbol *p;

  p = XCNEW (gfc_symbol);

  gfc_clear_ts (&p->ts);
  gfc_clear_attr (&p->attr);
  p->ns = ns;

  p->declared_at = gfc_current_locus;

  if (strlen (name) > GFC_MAX_SYMBOL_LEN)
    gfc_internal_error ("new_symbol(): Symbol name too long");

  p->name = gfc_get_string (name);

  /* Make sure flags for symbol being C bound are clear initially.  */
  p->attr.is_bind_c = 0;
  p->attr.is_iso_c = 0;
  /* Make sure the binding label field has a Nul char to start.  */
  p->binding_label[0] = '\0';

  /* Clear the ptrs we may need.  */
  p->common_block = NULL;
  p->f2k_derived = NULL;
  
  return p;
}


/* Generate an error if a symbol is ambiguous.  */

static void
ambiguous_symbol (const char *name, gfc_symtree *st)
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
gfc_find_sym_tree (const char *name, gfc_namespace *ns, int parent_flag,
		   gfc_symtree **result)
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
	  /* Ambiguous generic interfaces are permitted, as long
	     as the specific interfaces are different.  */
	  if (st->ambiguous && !st->n.sym->attr.generic)
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
gfc_find_symbol (const char *name, gfc_namespace *ns, int parent_flag,
		 gfc_symbol **result)
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
save_symbol_data (gfc_symbol *sym)
{

  if (sym->gfc_new || sym->old_symbol != NULL)
    return;

  sym->old_symbol = XCNEW (gfc_symbol);
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
gfc_get_sym_tree (const char *name, gfc_namespace *ns, gfc_symtree **result,
		  bool allow_subroutine)
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
      p->gfc_new = 1;
      changed_syms = p;

      st = gfc_new_symtree (&ns->sym_root, name);
      st->n.sym = p;
      p->refs++;

    }
  else
    {
      /* Make sure the existing symbol is OK.  Ambiguous
	 generic interfaces are permitted, as long as the
	 specific interfaces are different.  */
      if (st->ambiguous && !st->n.sym->attr.generic)
	{
	  ambiguous_symbol (name, st);
	  return 1;
	}

      p = st->n.sym;
      if (p->ns != ns && (!p->attr.function || ns->proc_name != p)
	  && !(allow_subroutine && p->attr.subroutine)
	  && !(ns->proc_name && ns->proc_name->attr.if_source == IFSRC_IFBODY
	  && (ns->has_import_set || p->attr.imported)))
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
gfc_get_symbol (const char *name, gfc_namespace *ns, gfc_symbol **result)
{
  gfc_symtree *st;
  int i;

  i = gfc_get_sym_tree (name, ns, &st, false);
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
gfc_get_ha_sym_tree (const char *name, gfc_symtree **result)
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

  return gfc_get_sym_tree (name, gfc_current_ns, result, false);
}


int
gfc_get_ha_symbol (const char *name, gfc_symbol **result)
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
gfc_symbols_could_alias (gfc_symbol *lsym, gfc_symbol *rsym)
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
  tentative_tbp *tbp, *tbq;

  for (p = changed_syms; p; p = q)
    {
      q = p->tlink;

      if (p->gfc_new)
	{
	  /* Symbol was new.  */
	  if (p->attr.in_common && p->common_block->head)
	    {
	      /* If the symbol was added to any common block, it
		 needs to be removed to stop the resolver looking
		 for a (possibly) dead symbol.  */

	      if (p->common_block->head == p)
	        p->common_block->head = p->common_next;
	      else
		{
		  gfc_symbol *cparent, *csym;

		  cparent = p->common_block->head;
		  csym = cparent->common_next;

		  while (csym != p)
		    {
		      cparent = csym;
		      csym = csym->common_next;
		    }

		  gcc_assert(cparent->common_next == p);

		  cparent->common_next = csym->common_next;
		}
	    }

	  gfc_delete_symtree (&p->ns->sym_root, p->name);

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

  for (tbp = tentative_tbp_list; tbp; tbp = tbq)
    {
      tbq = tbp->next;
      /* Procedure is already marked `error' by default.  */
      gfc_free (tbp);
    }
  tentative_tbp_list = NULL;
}


/* Free sym->old_symbol. sym->old_symbol is mostly a shallow copy of sym; the
   components of old_symbol that might need deallocation are the "allocatables"
   that are restored in gfc_undo_symbols(), with two exceptions: namelist and
   namelist_tail.  In case these differ between old_symbol and sym, it's just
   because sym->namelist has gotten a few more items.  */

static void
free_old_symbol (gfc_symbol *sym)
{

  if (sym->old_symbol == NULL)
    return;

  if (sym->old_symbol->as != sym->as) 
    gfc_free_array_spec (sym->old_symbol->as);

  if (sym->old_symbol->value != sym->value) 
    gfc_free_expr (sym->old_symbol->value);

  if (sym->old_symbol->formal != sym->formal)
    gfc_free_formal_arglist (sym->old_symbol->formal);

  gfc_free (sym->old_symbol);
  sym->old_symbol = NULL;
}


/* Makes the changes made in the current statement permanent-- gets
   rid of undo information.  */

void
gfc_commit_symbols (void)
{
  gfc_symbol *p, *q;
  tentative_tbp *tbp, *tbq;

  for (p = changed_syms; p; p = q)
    {
      q = p->tlink;
      p->tlink = NULL;
      p->mark = 0;
      p->gfc_new = 0;
      free_old_symbol (p);
    }
  changed_syms = NULL;

  for (tbp = tentative_tbp_list; tbp; tbp = tbq)
    {
      tbq = tbp->next;
      tbp->proc->error = 0;
      gfc_free (tbp);
    }
  tentative_tbp_list = NULL;
}


/* Makes the changes made in one symbol permanent -- gets rid of undo
   information.  */

void
gfc_commit_symbol (gfc_symbol *sym)
{
  gfc_symbol *p;

  if (changed_syms == sym)
    changed_syms = sym->tlink;
  else
    {
      for (p = changed_syms; p; p = p->tlink)
        if (p->tlink == sym)
          {
            p->tlink = sym->tlink;
            break;
          }
    }

  sym->tlink = NULL;
  sym->mark = 0;
  sym->gfc_new = 0;

  free_old_symbol (sym);
}


/* Recursively free trees containing type-bound procedures.  */

static void
free_tb_tree (gfc_symtree *t)
{
  if (t == NULL)
    return;

  free_tb_tree (t->left);
  free_tb_tree (t->right);

  /* TODO: Free type-bound procedure structs themselves; probably needs some
     sort of ref-counting mechanism.  */

  gfc_free (t);
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
free_uop_tree (gfc_symtree *uop_tree)
{
  if (uop_tree == NULL)
    return;

  free_uop_tree (uop_tree->left);
  free_uop_tree (uop_tree->right);

  gfc_free_interface (uop_tree->n.uop->op);
  gfc_free (uop_tree->n.uop);
  gfc_free (uop_tree);
}


/* Recursive function that deletes an entire tree and all the symbols
   that it contains.  */

static void
free_sym_tree (gfc_symtree *sym_tree)
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


/* Free the derived type list.  */

void
gfc_free_dt_list (void)
{
  gfc_dt_list *dt, *n;

  for (dt = gfc_derived_types; dt; dt = n)
    {
      n = dt->next;
      gfc_free (dt);
    }

  gfc_derived_types = NULL;
}


/* Free the gfc_equiv_info's.  */

static void
gfc_free_equiv_infos (gfc_equiv_info *s)
{
  if (s == NULL)
    return;
  gfc_free_equiv_infos (s->next);
  gfc_free (s);
}


/* Free the gfc_equiv_lists.  */

static void
gfc_free_equiv_lists (gfc_equiv_list *l)
{
  if (l == NULL)
    return;
  gfc_free_equiv_lists (l->next);
  gfc_free_equiv_infos (l->equiv);
  gfc_free (l);
}


/* Free a finalizer procedure list.  */

void
gfc_free_finalizer (gfc_finalizer* el)
{
  if (el)
    {
      if (el->proc_sym)
	{
	  --el->proc_sym->refs;
	  if (!el->proc_sym->refs)
	    gfc_free_symbol (el->proc_sym);
	}

      gfc_free (el);
    }
}

static void
gfc_free_finalizer_list (gfc_finalizer* list)
{
  while (list)
    {
      gfc_finalizer* current = list;
      list = list->next;
      gfc_free_finalizer (current);
    }
}


/* Create a new gfc_charlen structure and add it to a namespace.  */

gfc_charlen*
gfc_new_charlen (gfc_namespace *ns)
{
  gfc_charlen *cl;
  cl = gfc_get_charlen ();
  cl->next = ns->cl_list;
  ns->cl_list = cl;
  return cl;
}


/* Free the charlen list from cl to end (end is not freed). 
   Free the whole list if end is NULL.  */

void gfc_free_charlen (gfc_charlen *cl, gfc_charlen *end)
{
  gfc_charlen *cl2;

  for (; cl != end; cl = cl2)
    {
      gcc_assert (cl);

      cl2 = cl->next;
      gfc_free_expr (cl->length);
      gfc_free (cl);
    }
}


/* Free a namespace structure and everything below it.  Interface
   lists associated with intrinsic operators are not freed.  These are
   taken care of when a specific name is freed.  */

void
gfc_free_namespace (gfc_namespace *ns)
{
  gfc_namespace *p, *q;
  int i;

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
  free_tb_tree (ns->tb_sym_root);
  free_tb_tree (ns->tb_uop_root);
  gfc_free_finalizer_list (ns->finalizers);
  gfc_free_charlen (ns->cl_list, NULL);
  free_st_labels (ns->st_labels);

  gfc_free_equiv (ns->equiv);
  gfc_free_equiv_lists (ns->equiv_lists);
  gfc_free_use_stmts (ns->use_stmts);

  for (i = GFC_INTRINSIC_BEGIN; i != GFC_INTRINSIC_END; i++)
    gfc_free_interface (ns->op[i]);

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
  gfc_free_dt_list ();
}


/* Clear mark bits from symbol nodes associated with a symtree node.  */

static void
clear_sym_mark (gfc_symtree *st)
{

  st->n.sym->mark = 0;
}


/* Recursively traverse the symtree nodes.  */

void
gfc_traverse_symtree (gfc_symtree *st, void (*func) (gfc_symtree *))
{
  if (!st)
    return;

  gfc_traverse_symtree (st->left, func);
  (*func) (st);
  gfc_traverse_symtree (st->right, func);
}


/* Recursive namespace traversal function.  */

static void
traverse_ns (gfc_symtree *st, void (*func) (gfc_symbol *))
{

  if (st == NULL)
    return;

  traverse_ns (st->left, func);

  if (st->n.sym->mark == 0)
    (*func) (st->n.sym);
  st->n.sym->mark = 1;

  traverse_ns (st->right, func);
}


/* Call a given function for all symbols in the namespace.  We take
   care that each gfc_symbol node is called exactly once.  */

void
gfc_traverse_ns (gfc_namespace *ns, void (*func) (gfc_symbol *))
{

  gfc_traverse_symtree (ns->sym_root, clear_sym_mark);

  traverse_ns (ns->sym_root, func);
}


/* Return TRUE when name is the name of an intrinsic type.  */

bool
gfc_is_intrinsic_typename (const char *name)
{
  if (strcmp (name, "integer") == 0
      || strcmp (name, "real") == 0
      || strcmp (name, "character") == 0
      || strcmp (name, "logical") == 0
      || strcmp (name, "complex") == 0
      || strcmp (name, "doubleprecision") == 0
      || strcmp (name, "doublecomplex") == 0)
    return true;
  else
    return false;
}


/* Return TRUE if the symbol is an automatic variable.  */

static bool
gfc_is_var_automatic (gfc_symbol *sym)
{
  /* Pointer and allocatable variables are never automatic.  */
  if (sym->attr.pointer || sym->attr.allocatable)
    return false;
  /* Check for arrays with non-constant size.  */
  if (sym->attr.dimension && sym->as
      && !gfc_is_compile_time_shape (sym->as))
    return true;
  /* Check for non-constant length character variables.  */
  if (sym->ts.type == BT_CHARACTER
      && sym->ts.cl
      && !gfc_is_constant_expr (sym->ts.cl->length))
    return true;
  return false;
}

/* Given a symbol, mark it as SAVEd if it is allowed.  */

static void
save_symbol (gfc_symbol *sym)
{

  if (sym->attr.use_assoc)
    return;

  if (sym->attr.in_common
      || sym->attr.dummy
      || sym->attr.result
      || sym->attr.flavor != FL_VARIABLE)
    return;
  /* Automatic objects are not saved.  */
  if (gfc_is_var_automatic (sym))
    return;
  gfc_add_save (&sym->attr, sym->name, &sym->declared_at);
}


/* Mark those symbols which can be SAVEd as such.  */

void
gfc_save_all (gfc_namespace *ns)
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
  int c;

  if (symbol == NULL)
    return NULL;

  while (symbol)
    {
      c = strcmp (name, symbol->name);
      if (!c)
	return symbol;

      symbol = (c < 0) ? symbol->left : symbol->right;
    }

  return NULL;
}


/* Compare two global symbols. Used for managing the BB tree.  */

static int
gsym_compare (void *_s1, void *_s2)
{
  gfc_gsymbol *s1, *s2;

  s1 = (gfc_gsymbol *) _s1;
  s2 = (gfc_gsymbol *) _s2;
  return strcmp (s1->name, s2->name);
}


/* Get a global symbol, creating it if it doesn't exist.  */

gfc_gsymbol *
gfc_get_gsymbol (const char *name)
{
  gfc_gsymbol *s;

  s = gfc_find_gsymbol (gfc_gsym_root, name);
  if (s != NULL)
    return s;

  s = XCNEW (gfc_gsymbol);
  s->type = GSYM_UNKNOWN;
  s->name = gfc_get_string (name);

  gfc_insert_bbt (&gfc_gsym_root, s, gsym_compare);

  return s;
}


static gfc_symbol *
get_iso_c_binding_dt (int sym_id)
{
  gfc_dt_list *dt_list;

  dt_list = gfc_derived_types;

  /* Loop through the derived types in the name list, searching for
     the desired symbol from iso_c_binding.  Search the parent namespaces
     if necessary and requested to (parent_flag).  */
  while (dt_list != NULL)
    {
      if (dt_list->derived->from_intmod != INTMOD_NONE
	  && dt_list->derived->intmod_sym_id == sym_id)
        return dt_list->derived;

      dt_list = dt_list->next;
    }

  return NULL;
}


/* Verifies that the given derived type symbol, derived_sym, is interoperable
   with C.  This is necessary for any derived type that is BIND(C) and for
   derived types that are parameters to functions that are BIND(C).  All
   fields of the derived type are required to be interoperable, and are tested
   for such.  If an error occurs, the errors are reported here, allowing for
   multiple errors to be handled for a single derived type.  */

gfc_try
verify_bind_c_derived_type (gfc_symbol *derived_sym)
{
  gfc_component *curr_comp = NULL;
  gfc_try is_c_interop = FAILURE;
  gfc_try retval = SUCCESS;
   
  if (derived_sym == NULL)
    gfc_internal_error ("verify_bind_c_derived_type(): Given symbol is "
                        "unexpectedly NULL");

  /* If we've already looked at this derived symbol, do not look at it again
     so we don't repeat warnings/errors.  */
  if (derived_sym->ts.is_c_interop)
    return SUCCESS;
  
  /* The derived type must have the BIND attribute to be interoperable
     J3/04-007, Section 15.2.3.  */
  if (derived_sym->attr.is_bind_c != 1)
    {
      derived_sym->ts.is_c_interop = 0;
      gfc_error_now ("Derived type '%s' declared at %L must have the BIND "
                     "attribute to be C interoperable", derived_sym->name,
                     &(derived_sym->declared_at));
      retval = FAILURE;
    }
  
  curr_comp = derived_sym->components;

  /* TODO: is this really an error?  */
  if (curr_comp == NULL)
    {
      gfc_error ("Derived type '%s' at %L is empty",
		 derived_sym->name, &(derived_sym->declared_at));
      return FAILURE;
    }

  /* Initialize the derived type as being C interoperable.
     If we find an error in the components, this will be set false.  */
  derived_sym->ts.is_c_interop = 1;
  
  /* Loop through the list of components to verify that the kind of
     each is a C interoperable type.  */
  do
    {
      /* The components cannot be pointers (fortran sense).  
         J3/04-007, Section 15.2.3, C1505.	*/
      if (curr_comp->attr.pointer != 0)
        {
          gfc_error ("Component '%s' at %L cannot have the "
                     "POINTER attribute because it is a member "
                     "of the BIND(C) derived type '%s' at %L",
                     curr_comp->name, &(curr_comp->loc),
                     derived_sym->name, &(derived_sym->declared_at));
          retval = FAILURE;
        }

      if (curr_comp->attr.proc_pointer != 0)
	{
	  gfc_error ("Procedure pointer component '%s' at %L cannot be a member"
		     " of the BIND(C) derived type '%s' at %L", curr_comp->name,
		     &curr_comp->loc, derived_sym->name,
		     &derived_sym->declared_at);
          retval = FAILURE;
        }

      /* The components cannot be allocatable.
         J3/04-007, Section 15.2.3, C1505.	*/
      if (curr_comp->attr.allocatable != 0)
        {
          gfc_error ("Component '%s' at %L cannot have the "
                     "ALLOCATABLE attribute because it is a member "
                     "of the BIND(C) derived type '%s' at %L",
                     curr_comp->name, &(curr_comp->loc),
                     derived_sym->name, &(derived_sym->declared_at));
          retval = FAILURE;
        }
      
      /* BIND(C) derived types must have interoperable components.  */
      if (curr_comp->ts.type == BT_DERIVED
	  && curr_comp->ts.derived->ts.is_iso_c != 1 
          && curr_comp->ts.derived != derived_sym)
        {
          /* This should be allowed; the draft says a derived-type can not
             have type parameters if it is has the BIND attribute.  Type
             parameters seem to be for making parameterized derived types.
             There's no need to verify the type if it is c_ptr/c_funptr.  */
          retval = verify_bind_c_derived_type (curr_comp->ts.derived);
	}
      else
	{
	  /* Grab the typespec for the given component and test the kind.  */ 
	  is_c_interop = verify_c_interop (&(curr_comp->ts));
	  
	  if (is_c_interop != SUCCESS)
	    {
	      /* Report warning and continue since not fatal.  The
		 draft does specify a constraint that requires all fields
		 to interoperate, but if the user says real(4), etc., it
		 may interoperate with *something* in C, but the compiler
		 most likely won't know exactly what.  Further, it may not
		 interoperate with the same data type(s) in C if the user
		 recompiles with different flags (e.g., -m32 and -m64 on
		 x86_64 and using integer(4) to claim interop with a
		 C_LONG).  */
	      if (derived_sym->attr.is_bind_c == 1)
		/* If the derived type is bind(c), all fields must be
		   interop.  */
		gfc_warning ("Component '%s' in derived type '%s' at %L "
                             "may not be C interoperable, even though "
                             "derived type '%s' is BIND(C)",
                             curr_comp->name, derived_sym->name,
                             &(curr_comp->loc), derived_sym->name);
	      else
		/* If derived type is param to bind(c) routine, or to one
		   of the iso_c_binding procs, it must be interoperable, so
		   all fields must interop too.	 */
		gfc_warning ("Component '%s' in derived type '%s' at %L "
                             "may not be C interoperable",
                             curr_comp->name, derived_sym->name,
                             &(curr_comp->loc));
	    }
	}
      
      curr_comp = curr_comp->next;
    } while (curr_comp != NULL); 


  /* Make sure we don't have conflicts with the attributes.  */
  if (derived_sym->attr.access == ACCESS_PRIVATE)
    {
      gfc_error ("Derived type '%s' at %L cannot be declared with both "
                 "PRIVATE and BIND(C) attributes", derived_sym->name,
                 &(derived_sym->declared_at));
      retval = FAILURE;
    }

  if (derived_sym->attr.sequence != 0)
    {
      gfc_error ("Derived type '%s' at %L cannot have the SEQUENCE "
                 "attribute because it is BIND(C)", derived_sym->name,
                 &(derived_sym->declared_at));
      retval = FAILURE;
    }

  /* Mark the derived type as not being C interoperable if we found an
     error.  If there were only warnings, proceed with the assumption
     it's interoperable.  */
  if (retval == FAILURE)
    derived_sym->ts.is_c_interop = 0;
  
  return retval;
}


/* Generate symbols for the named constants c_null_ptr and c_null_funptr.  */

static gfc_try
gen_special_c_interop_ptr (int ptr_id, const char *ptr_name,
                           const char *module_name)
{
  gfc_symtree *tmp_symtree;
  gfc_symbol *tmp_sym;

  tmp_symtree = gfc_find_symtree (gfc_current_ns->sym_root, ptr_name);
	 
  if (tmp_symtree != NULL)
    tmp_sym = tmp_symtree->n.sym;
  else
    {
      tmp_sym = NULL;
      gfc_internal_error ("gen_special_c_interop_ptr(): Unable to "
                          "create symbol for %s", ptr_name);
    }

  /* Set up the symbol's important fields.  Save attr required so we can
     initialize the ptr to NULL.  */
  tmp_sym->attr.save = SAVE_EXPLICIT;
  tmp_sym->ts.is_c_interop = 1;
  tmp_sym->attr.is_c_interop = 1;
  tmp_sym->ts.is_iso_c = 1;
  tmp_sym->ts.type = BT_DERIVED;

  /* The c_ptr and c_funptr derived types will provide the
     definition for c_null_ptr and c_null_funptr, respectively.  */
  if (ptr_id == ISOCBINDING_NULL_PTR)
    tmp_sym->ts.derived = get_iso_c_binding_dt (ISOCBINDING_PTR);
  else
    tmp_sym->ts.derived = get_iso_c_binding_dt (ISOCBINDING_FUNPTR);
  if (tmp_sym->ts.derived == NULL)
    {
      /* This can occur if the user forgot to declare c_ptr or
         c_funptr and they're trying to use one of the procedures
         that has arg(s) of the missing type.  In this case, a
         regular version of the thing should have been put in the
         current ns.  */
      generate_isocbinding_symbol (module_name, ptr_id == ISOCBINDING_NULL_PTR 
                                   ? ISOCBINDING_PTR : ISOCBINDING_FUNPTR,
                                   (const char *) (ptr_id == ISOCBINDING_NULL_PTR 
				   ? "_gfortran_iso_c_binding_c_ptr"
				   : "_gfortran_iso_c_binding_c_funptr"));

      tmp_sym->ts.derived =
        get_iso_c_binding_dt (ptr_id == ISOCBINDING_NULL_PTR
                              ? ISOCBINDING_PTR : ISOCBINDING_FUNPTR);
    }

  /* Module name is some mangled version of iso_c_binding.  */
  tmp_sym->module = gfc_get_string (module_name);
  
  /* Say it's from the iso_c_binding module.  */
  tmp_sym->attr.is_iso_c = 1;
  
  tmp_sym->attr.use_assoc = 1;
  tmp_sym->attr.is_bind_c = 1;
  /* Set the binding_label.  */
  sprintf (tmp_sym->binding_label, "%s_%s", module_name, tmp_sym->name);
  
  /* Set the c_address field of c_null_ptr and c_null_funptr to
     the value of NULL.	 */
  tmp_sym->value = gfc_get_expr ();
  tmp_sym->value->expr_type = EXPR_STRUCTURE;
  tmp_sym->value->ts.type = BT_DERIVED;
  tmp_sym->value->ts.derived = tmp_sym->ts.derived;
  /* Create a constructor with no expr, that way we can recognize if the user
     tries to call the structure constructor for one of the iso_c_binding
     derived types during resolution (resolve_structure_cons).  */
  tmp_sym->value->value.constructor = gfc_get_constructor ();
  /* Must declare c_null_ptr and c_null_funptr as having the
     PARAMETER attribute so they can be used in init expressions.  */
  tmp_sym->attr.flavor = FL_PARAMETER;

  return SUCCESS;
}


/* Add a formal argument, gfc_formal_arglist, to the
   end of the given list of arguments.	Set the reference to the
   provided symbol, param_sym, in the argument.  */

static void
add_formal_arg (gfc_formal_arglist **head,
                gfc_formal_arglist **tail,
                gfc_formal_arglist *formal_arg,
                gfc_symbol *param_sym)
{
  /* Put in list, either as first arg or at the tail (curr arg).  */
  if (*head == NULL)
    *head = *tail = formal_arg;
  else
    {
      (*tail)->next = formal_arg;
      (*tail) = formal_arg;
    }
   
  (*tail)->sym = param_sym;
  (*tail)->next = NULL;
   
  return;
}


/* Generates a symbol representing the CPTR argument to an
   iso_c_binding procedure.  Also, create a gfc_formal_arglist for the
   CPTR and add it to the provided argument list.  */

static void
gen_cptr_param (gfc_formal_arglist **head,
                gfc_formal_arglist **tail,
                const char *module_name,
                gfc_namespace *ns, const char *c_ptr_name,
                int iso_c_sym_id)
{
  gfc_symbol *param_sym = NULL;
  gfc_symbol *c_ptr_sym = NULL;
  gfc_symtree *param_symtree = NULL;
  gfc_formal_arglist *formal_arg = NULL;
  const char *c_ptr_in;
  const char *c_ptr_type = NULL;

  if (iso_c_sym_id == ISOCBINDING_F_PROCPOINTER)
    c_ptr_type = "_gfortran_iso_c_binding_c_funptr";
  else
    c_ptr_type = "_gfortran_iso_c_binding_c_ptr";

  if(c_ptr_name == NULL)
    c_ptr_in = "gfc_cptr__";
  else
    c_ptr_in = c_ptr_name;
  gfc_get_sym_tree (c_ptr_in, ns, &param_symtree, false);
  if (param_symtree != NULL)
    param_sym = param_symtree->n.sym;
  else
    gfc_internal_error ("gen_cptr_param(): Unable to "
			"create symbol for %s", c_ptr_in);

  /* Set up the appropriate fields for the new c_ptr param sym.  */
  param_sym->refs++;
  param_sym->attr.flavor = FL_DERIVED;
  param_sym->ts.type = BT_DERIVED;
  param_sym->attr.intent = INTENT_IN;
  param_sym->attr.dummy = 1;

  /* This will pass the ptr to the iso_c routines as a (void *).  */
  param_sym->attr.value = 1;
  param_sym->attr.use_assoc = 1;

  /* Get the symbol for c_ptr or c_funptr, no matter what it's name is 
     (user renamed).  */
  if (iso_c_sym_id == ISOCBINDING_F_PROCPOINTER)
    c_ptr_sym = get_iso_c_binding_dt (ISOCBINDING_FUNPTR);
  else
    c_ptr_sym = get_iso_c_binding_dt (ISOCBINDING_PTR);
  if (c_ptr_sym == NULL)
    {
      /* This can happen if the user did not define c_ptr but they are
         trying to use one of the iso_c_binding functions that need it.  */
      if (iso_c_sym_id == ISOCBINDING_F_PROCPOINTER)
	generate_isocbinding_symbol (module_name, ISOCBINDING_FUNPTR,
				     (const char *)c_ptr_type);
      else
	generate_isocbinding_symbol (module_name, ISOCBINDING_PTR,
				     (const char *)c_ptr_type);

      gfc_get_ha_symbol (c_ptr_type, &(c_ptr_sym));
    }

  param_sym->ts.derived = c_ptr_sym;
  param_sym->module = gfc_get_string (module_name);

  /* Make new formal arg.  */
  formal_arg = gfc_get_formal_arglist ();
  /* Add arg to list of formal args (the CPTR arg).  */
  add_formal_arg (head, tail, formal_arg, param_sym);
}


/* Generates a symbol representing the FPTR argument to an
   iso_c_binding procedure.  Also, create a gfc_formal_arglist for the
   FPTR and add it to the provided argument list.  */

static void
gen_fptr_param (gfc_formal_arglist **head,
                gfc_formal_arglist **tail,
                const char *module_name,
                gfc_namespace *ns, const char *f_ptr_name, int proc)
{
  gfc_symbol *param_sym = NULL;
  gfc_symtree *param_symtree = NULL;
  gfc_formal_arglist *formal_arg = NULL;
  const char *f_ptr_out = "gfc_fptr__";

  if (f_ptr_name != NULL)
    f_ptr_out = f_ptr_name;

  gfc_get_sym_tree (f_ptr_out, ns, &param_symtree, false);
  if (param_symtree != NULL)
    param_sym = param_symtree->n.sym;
  else
    gfc_internal_error ("generateFPtrParam(): Unable to "
			"create symbol for %s", f_ptr_out);

  /* Set up the necessary fields for the fptr output param sym.  */
  param_sym->refs++;
  if (proc)
    param_sym->attr.proc_pointer = 1;
  else
    param_sym->attr.pointer = 1;
  param_sym->attr.dummy = 1;
  param_sym->attr.use_assoc = 1;

  /* ISO C Binding type to allow any pointer type as actual param.  */
  param_sym->ts.type = BT_VOID;
  param_sym->module = gfc_get_string (module_name);
   
  /* Make the arg.  */
  formal_arg = gfc_get_formal_arglist ();
  /* Add arg to list of formal args.  */
  add_formal_arg (head, tail, formal_arg, param_sym);
}


/* Generates a symbol representing the optional SHAPE argument for the
   iso_c_binding c_f_pointer() procedure.  Also, create a
   gfc_formal_arglist for the SHAPE and add it to the provided
   argument list.  */

static void
gen_shape_param (gfc_formal_arglist **head,
                 gfc_formal_arglist **tail,
                 const char *module_name,
                 gfc_namespace *ns, const char *shape_param_name)
{
  gfc_symbol *param_sym = NULL;
  gfc_symtree *param_symtree = NULL;
  gfc_formal_arglist *formal_arg = NULL;
  const char *shape_param = "gfc_shape_array__";
  int i;

  if (shape_param_name != NULL)
    shape_param = shape_param_name;

  gfc_get_sym_tree (shape_param, ns, &param_symtree, false);
  if (param_symtree != NULL)
    param_sym = param_symtree->n.sym;
  else
    gfc_internal_error ("generateShapeParam(): Unable to "
			"create symbol for %s", shape_param);
   
  /* Set up the necessary fields for the shape input param sym.  */
  param_sym->refs++;
  param_sym->attr.dummy = 1;
  param_sym->attr.use_assoc = 1;

  /* Integer array, rank 1, describing the shape of the object.  Make it's
     type BT_VOID initially so we can accept any type/kind combination of
     integer.  During gfc_iso_c_sub_interface (resolve.c), we'll make it
     of BT_INTEGER type.  */
  param_sym->ts.type = BT_VOID;

  /* Initialize the kind to default integer.  However, it will be overridden
     during resolution to match the kind of the SHAPE parameter given as
     the actual argument (to allow for any valid integer kind).  */
  param_sym->ts.kind = gfc_default_integer_kind;   
  param_sym->as = gfc_get_array_spec ();

  /* Clear out the dimension info for the array.  */
  for (i = 0; i < GFC_MAX_DIMENSIONS; i++)
    {
      param_sym->as->lower[i] = NULL;
      param_sym->as->upper[i] = NULL;
    }
  param_sym->as->rank = 1;
  param_sym->as->lower[0] = gfc_int_expr (1);

  /* The extent is unknown until we get it.  The length give us
     the rank the incoming pointer.  */
  param_sym->as->type = AS_ASSUMED_SHAPE;

  /* The arg is also optional; it is required iff the second arg
     (fptr) is to an array, otherwise, it's ignored.  */
  param_sym->attr.optional = 1;
  param_sym->attr.intent = INTENT_IN;
  param_sym->attr.dimension = 1;
  param_sym->module = gfc_get_string (module_name);
   
  /* Make the arg.  */
  formal_arg = gfc_get_formal_arglist ();
  /* Add arg to list of formal args.  */
  add_formal_arg (head, tail, formal_arg, param_sym);
}


/* Add a procedure interface to the given symbol (i.e., store a
   reference to the list of formal arguments).  */

static void
add_proc_interface (gfc_symbol *sym, ifsrc source,
                    gfc_formal_arglist *formal)
{

  sym->formal = formal;
  sym->attr.if_source = source;
}


/* Copy the formal args from an existing symbol, src, into a new
   symbol, dest.  New formal args are created, and the description of
   each arg is set according to the existing ones.  This function is
   used when creating procedure declaration variables from a procedure
   declaration statement (see match_proc_decl()) to create the formal
   args based on the args of a given named interface.  */

void
gfc_copy_formal_args (gfc_symbol *dest, gfc_symbol *src)
{
  gfc_formal_arglist *head = NULL;
  gfc_formal_arglist *tail = NULL;
  gfc_formal_arglist *formal_arg = NULL;
  gfc_formal_arglist *curr_arg = NULL;
  gfc_formal_arglist *formal_prev = NULL;
  /* Save current namespace so we can change it for formal args.  */
  gfc_namespace *parent_ns = gfc_current_ns;

  /* Create a new namespace, which will be the formal ns (namespace
     of the formal args).  */
  gfc_current_ns = gfc_get_namespace (parent_ns, 0);
  gfc_current_ns->proc_name = dest;

  for (curr_arg = src->formal; curr_arg; curr_arg = curr_arg->next)
    {
      formal_arg = gfc_get_formal_arglist ();
      gfc_get_symbol (curr_arg->sym->name, gfc_current_ns, &(formal_arg->sym));

      /* May need to copy more info for the symbol.  */
      formal_arg->sym->attr = curr_arg->sym->attr;
      formal_arg->sym->ts = curr_arg->sym->ts;
      formal_arg->sym->as = gfc_copy_array_spec (curr_arg->sym->as);
      gfc_copy_formal_args (formal_arg->sym, curr_arg->sym);

      /* If this isn't the first arg, set up the next ptr.  For the
        last arg built, the formal_arg->next will never get set to
        anything other than NULL.  */
      if (formal_prev != NULL)
	formal_prev->next = formal_arg;
      else
	formal_arg->next = NULL;

      formal_prev = formal_arg;

      /* Add arg to list of formal args.  */
      add_formal_arg (&head, &tail, formal_arg, formal_arg->sym);
    }

  /* Add the interface to the symbol.  */
  add_proc_interface (dest, IFSRC_DECL, head);

  /* Store the formal namespace information.  */
  if (dest->formal != NULL)
    /* The current ns should be that for the dest proc.  */
    dest->formal_ns = gfc_current_ns;
  /* Restore the current namespace to what it was on entry.  */
  gfc_current_ns = parent_ns;
}


void
gfc_copy_formal_args_intr (gfc_symbol *dest, gfc_intrinsic_sym *src)
{
  gfc_formal_arglist *head = NULL;
  gfc_formal_arglist *tail = NULL;
  gfc_formal_arglist *formal_arg = NULL;
  gfc_intrinsic_arg *curr_arg = NULL;
  gfc_formal_arglist *formal_prev = NULL;
  /* Save current namespace so we can change it for formal args.  */
  gfc_namespace *parent_ns = gfc_current_ns;

  /* Create a new namespace, which will be the formal ns (namespace
     of the formal args).  */
  gfc_current_ns = gfc_get_namespace (parent_ns, 0);
  gfc_current_ns->proc_name = dest;

  for (curr_arg = src->formal; curr_arg; curr_arg = curr_arg->next)
    {
      formal_arg = gfc_get_formal_arglist ();
      gfc_get_symbol (curr_arg->name, gfc_current_ns, &(formal_arg->sym));

      /* May need to copy more info for the symbol.  */
      formal_arg->sym->ts = curr_arg->ts;
      formal_arg->sym->attr.optional = curr_arg->optional;
      formal_arg->sym->attr.intent = curr_arg->intent;
      formal_arg->sym->attr.flavor = FL_VARIABLE;
      formal_arg->sym->attr.dummy = 1;

      if (formal_arg->sym->ts.type == BT_CHARACTER)
	formal_arg->sym->ts.cl = gfc_new_charlen (gfc_current_ns);

      /* If this isn't the first arg, set up the next ptr.  For the
        last arg built, the formal_arg->next will never get set to
        anything other than NULL.  */
      if (formal_prev != NULL)
	formal_prev->next = formal_arg;
      else
	formal_arg->next = NULL;

      formal_prev = formal_arg;

      /* Add arg to list of formal args.  */
      add_formal_arg (&head, &tail, formal_arg, formal_arg->sym);
    }

  /* Add the interface to the symbol.  */
  add_proc_interface (dest, IFSRC_DECL, head);

  /* Store the formal namespace information.  */
  if (dest->formal != NULL)
    /* The current ns should be that for the dest proc.  */
    dest->formal_ns = gfc_current_ns;
  /* Restore the current namespace to what it was on entry.  */
  gfc_current_ns = parent_ns;
}


void
gfc_copy_formal_args_ppc (gfc_component *dest, gfc_symbol *src)
{
  gfc_formal_arglist *head = NULL;
  gfc_formal_arglist *tail = NULL;
  gfc_formal_arglist *formal_arg = NULL;
  gfc_formal_arglist *curr_arg = NULL;
  gfc_formal_arglist *formal_prev = NULL;
  /* Save current namespace so we can change it for formal args.  */
  gfc_namespace *parent_ns = gfc_current_ns;

  /* Create a new namespace, which will be the formal ns (namespace
     of the formal args).  */
  gfc_current_ns = gfc_get_namespace (parent_ns, 0);
  /* TODO: gfc_current_ns->proc_name = dest;*/

  for (curr_arg = src->formal; curr_arg; curr_arg = curr_arg->next)
    {
      formal_arg = gfc_get_formal_arglist ();
      gfc_get_symbol (curr_arg->sym->name, gfc_current_ns, &(formal_arg->sym));

      /* May need to copy more info for the symbol.  */
      formal_arg->sym->attr = curr_arg->sym->attr;
      formal_arg->sym->ts = curr_arg->sym->ts;
      formal_arg->sym->as = gfc_copy_array_spec (curr_arg->sym->as);
      gfc_copy_formal_args (formal_arg->sym, curr_arg->sym);

      /* If this isn't the first arg, set up the next ptr.  For the
        last arg built, the formal_arg->next will never get set to
        anything other than NULL.  */
      if (formal_prev != NULL)
	formal_prev->next = formal_arg;
      else
	formal_arg->next = NULL;

      formal_prev = formal_arg;

      /* Add arg to list of formal args.  */
      add_formal_arg (&head, &tail, formal_arg, formal_arg->sym);
    }

  /* Add the interface to the symbol.  */
  dest->formal = head;
  dest->attr.if_source = IFSRC_DECL;

  /* Store the formal namespace information.  */
  if (dest->formal != NULL)
    /* The current ns should be that for the dest proc.  */
    dest->formal_ns = gfc_current_ns;
  /* Restore the current namespace to what it was on entry.  */
  gfc_current_ns = parent_ns;
}


/* Builds the parameter list for the iso_c_binding procedure
   c_f_pointer or c_f_procpointer.  The old_sym typically refers to a
   generic version of either the c_f_pointer or c_f_procpointer
   functions.  The new_proc_sym represents a "resolved" version of the
   symbol.  The functions are resolved to match the types of their
   parameters; for example, c_f_pointer(cptr, fptr) would resolve to
   something similar to c_f_pointer_i4 if the type of data object fptr
   pointed to was a default integer.  The actual name of the resolved
   procedure symbol is further mangled with the module name, etc., but
   the idea holds true.  */

static void
build_formal_args (gfc_symbol *new_proc_sym,
                   gfc_symbol *old_sym, int add_optional_arg)
{
  gfc_formal_arglist *head = NULL, *tail = NULL;
  gfc_namespace *parent_ns = NULL;

  parent_ns = gfc_current_ns;
  /* Create a new namespace, which will be the formal ns (namespace
     of the formal args).  */
  gfc_current_ns = gfc_get_namespace(parent_ns, 0);
  gfc_current_ns->proc_name = new_proc_sym;

  /* Generate the params.  */
  if (old_sym->intmod_sym_id == ISOCBINDING_F_PROCPOINTER)
    {
      gen_cptr_param (&head, &tail, (const char *) new_proc_sym->module,
		      gfc_current_ns, "cptr", old_sym->intmod_sym_id);
      gen_fptr_param (&head, &tail, (const char *) new_proc_sym->module,
		      gfc_current_ns, "fptr", 1);
    }
  else if (old_sym->intmod_sym_id == ISOCBINDING_F_POINTER)
    {
      gen_cptr_param (&head, &tail, (const char *) new_proc_sym->module,
		      gfc_current_ns, "cptr", old_sym->intmod_sym_id);
      gen_fptr_param (&head, &tail, (const char *) new_proc_sym->module,
		      gfc_current_ns, "fptr", 0);
      /* If we're dealing with c_f_pointer, it has an optional third arg.  */
      gen_shape_param (&head, &tail,(const char *) new_proc_sym->module,
		       gfc_current_ns, "shape");

    }
  else if (old_sym->intmod_sym_id == ISOCBINDING_ASSOCIATED)
    {
      /* c_associated has one required arg and one optional; both
	 are c_ptrs.  */
      gen_cptr_param (&head, &tail, (const char *) new_proc_sym->module,
		      gfc_current_ns, "c_ptr_1", ISOCBINDING_ASSOCIATED);
      if (add_optional_arg)
	{
	  gen_cptr_param (&head, &tail, (const char *) new_proc_sym->module,
			  gfc_current_ns, "c_ptr_2", ISOCBINDING_ASSOCIATED);
	  /* The last param is optional so mark it as such.  */
	  tail->sym->attr.optional = 1;
	}
    }

  /* Add the interface (store formal args to new_proc_sym).  */
  add_proc_interface (new_proc_sym, IFSRC_DECL, head);

  /* Set up the formal_ns pointer to the one created for the
     new procedure so it'll get cleaned up during gfc_free_symbol().  */
  new_proc_sym->formal_ns = gfc_current_ns;

  gfc_current_ns = parent_ns;
}

static int
std_for_isocbinding_symbol (int id)
{
  switch (id)
    {
#define NAMED_INTCST(a,b,c,d) \
      case a:\
        return d;
#include "iso-c-binding.def"
#undef NAMED_INTCST
       default:
         return GFC_STD_F2003;
    }
}

/* Generate the given set of C interoperable kind objects, or all
   interoperable kinds.  This function will only be given kind objects
   for valid iso_c_binding defined types because this is verified when
   the 'use' statement is parsed.  If the user gives an 'only' clause,
   the specific kinds are looked up; if they don't exist, an error is
   reported.  If the user does not give an 'only' clause, all
   iso_c_binding symbols are generated.  If a list of specific kinds
   is given, it must have a NULL in the first empty spot to mark the
   end of the list.  */


void
generate_isocbinding_symbol (const char *mod_name, iso_c_binding_symbol s,
			     const char *local_name)
{
  const char *const name = (local_name && local_name[0]) ? local_name
					     : c_interop_kinds_table[s].name;
  gfc_symtree *tmp_symtree = NULL;
  gfc_symbol *tmp_sym = NULL;
  gfc_dt_list **dt_list_ptr = NULL;
  gfc_component *tmp_comp = NULL;
  char comp_name[(GFC_MAX_SYMBOL_LEN * 2) + 1];
  int index;

  if (gfc_notification_std (std_for_isocbinding_symbol (s)) == ERROR)
    return;
  tmp_symtree = gfc_find_symtree (gfc_current_ns->sym_root, name);

  /* Already exists in this scope so don't re-add it.
     TODO: we should probably check that it's really the same symbol.  */
  if (tmp_symtree != NULL)
    return;

  /* Create the sym tree in the current ns.  */
  gfc_get_sym_tree (name, gfc_current_ns, &tmp_symtree, false);
  if (tmp_symtree)
    tmp_sym = tmp_symtree->n.sym;
  else
    gfc_internal_error ("generate_isocbinding_symbol(): Unable to "
			"create symbol");

  /* Say what module this symbol belongs to.  */
  tmp_sym->module = gfc_get_string (mod_name);
  tmp_sym->from_intmod = INTMOD_ISO_C_BINDING;
  tmp_sym->intmod_sym_id = s;

  switch (s)
    {

#define NAMED_INTCST(a,b,c,d) case a : 
#define NAMED_REALCST(a,b,c) case a :
#define NAMED_CMPXCST(a,b,c) case a :
#define NAMED_LOGCST(a,b,c) case a :
#define NAMED_CHARKNDCST(a,b,c) case a :
#include "iso-c-binding.def"

	tmp_sym->value = gfc_int_expr (c_interop_kinds_table[s].value);

	/* Initialize an integer constant expression node.  */
	tmp_sym->attr.flavor = FL_PARAMETER;
	tmp_sym->ts.type = BT_INTEGER;
	tmp_sym->ts.kind = gfc_default_integer_kind;

	/* Mark this type as a C interoperable one.  */
	tmp_sym->ts.is_c_interop = 1;
	tmp_sym->ts.is_iso_c = 1;
	tmp_sym->value->ts.is_c_interop = 1;
	tmp_sym->value->ts.is_iso_c = 1;
	tmp_sym->attr.is_c_interop = 1;

	/* Tell what f90 type this c interop kind is valid.  */
	tmp_sym->ts.f90_type = c_interop_kinds_table[s].f90_type;

	/* Say it's from the iso_c_binding module.  */
	tmp_sym->attr.is_iso_c = 1;

	/* Make it use associated.  */
	tmp_sym->attr.use_assoc = 1;
	break;


#define NAMED_CHARCST(a,b,c) case a :
#include "iso-c-binding.def"

	/* Initialize an integer constant expression node for the
	   length of the character.  */
	tmp_sym->value = gfc_get_expr (); 
	tmp_sym->value->expr_type = EXPR_CONSTANT;
	tmp_sym->value->ts.type = BT_CHARACTER;
	tmp_sym->value->ts.kind = gfc_default_character_kind;
	tmp_sym->value->where = gfc_current_locus;
	tmp_sym->value->ts.is_c_interop = 1;
	tmp_sym->value->ts.is_iso_c = 1;
	tmp_sym->value->value.character.length = 1;
	tmp_sym->value->value.character.string = gfc_get_wide_string (2);
	tmp_sym->value->value.character.string[0]
	  = (gfc_char_t) c_interop_kinds_table[s].value;
	tmp_sym->value->value.character.string[1] = '\0';
	tmp_sym->ts.cl = gfc_get_charlen ();
	tmp_sym->ts.cl->length = gfc_int_expr (1);

	/* May not need this in both attr and ts, but do need in
	   attr for writing module file.  */
	tmp_sym->attr.is_c_interop = 1;

	tmp_sym->attr.flavor = FL_PARAMETER;
	tmp_sym->ts.type = BT_CHARACTER;

	/* Need to set it to the C_CHAR kind.  */
	tmp_sym->ts.kind = gfc_default_character_kind;

	/* Mark this type as a C interoperable one.  */
	tmp_sym->ts.is_c_interop = 1;
	tmp_sym->ts.is_iso_c = 1;

	/* Tell what f90 type this c interop kind is valid.  */
	tmp_sym->ts.f90_type = BT_CHARACTER;

	/* Say it's from the iso_c_binding module.  */
	tmp_sym->attr.is_iso_c = 1;

	/* Make it use associated.  */
	tmp_sym->attr.use_assoc = 1;
	break;

      case ISOCBINDING_PTR:
      case ISOCBINDING_FUNPTR:

	/* Initialize an integer constant expression node.  */
	tmp_sym->attr.flavor = FL_DERIVED;
	tmp_sym->ts.is_c_interop = 1;
	tmp_sym->attr.is_c_interop = 1;
	tmp_sym->attr.is_iso_c = 1;
	tmp_sym->ts.is_iso_c = 1;
	tmp_sym->ts.type = BT_DERIVED;

	/* A derived type must have the bind attribute to be
	   interoperable (J3/04-007, Section 15.2.3), even though
	   the binding label is not used.  */
	tmp_sym->attr.is_bind_c = 1;

	tmp_sym->attr.referenced = 1;

	tmp_sym->ts.derived = tmp_sym;

        /* Add the symbol created for the derived type to the current ns.  */
        dt_list_ptr = &(gfc_derived_types);
        while (*dt_list_ptr != NULL && (*dt_list_ptr)->next != NULL)
          dt_list_ptr = &((*dt_list_ptr)->next);

        /* There is already at least one derived type in the list, so append
           the one we're currently building for c_ptr or c_funptr.  */
        if (*dt_list_ptr != NULL)
          dt_list_ptr = &((*dt_list_ptr)->next);
        (*dt_list_ptr) = gfc_get_dt_list ();
        (*dt_list_ptr)->derived = tmp_sym;
        (*dt_list_ptr)->next = NULL;

        /* Set up the component of the derived type, which will be
           an integer with kind equal to c_ptr_size.  Mangle the name of
           the field for the c_address to prevent the curious user from
           trying to access it from Fortran.  */
        sprintf (comp_name, "__%s_%s", tmp_sym->name, "c_address");
        gfc_add_component (tmp_sym, comp_name, &tmp_comp);
        if (tmp_comp == NULL)
          gfc_internal_error ("generate_isocbinding_symbol(): Unable to "
			      "create component for c_address");

        tmp_comp->ts.type = BT_INTEGER;

        /* Set this because the module will need to read/write this field.  */
        tmp_comp->ts.f90_type = BT_INTEGER;

        /* The kinds for c_ptr and c_funptr are the same.  */
        index = get_c_kind ("c_ptr", c_interop_kinds_table);
        tmp_comp->ts.kind = c_interop_kinds_table[index].value;

        tmp_comp->attr.pointer = 0;
        tmp_comp->attr.dimension = 0;

        /* Mark the component as C interoperable.  */
        tmp_comp->ts.is_c_interop = 1;

        /* Make it use associated (iso_c_binding module).  */
        tmp_sym->attr.use_assoc = 1;
	break;

      case ISOCBINDING_NULL_PTR:
      case ISOCBINDING_NULL_FUNPTR:
        gen_special_c_interop_ptr (s, name, mod_name);
        break;

      case ISOCBINDING_F_POINTER:
      case ISOCBINDING_ASSOCIATED:
      case ISOCBINDING_LOC:
      case ISOCBINDING_FUNLOC:
      case ISOCBINDING_F_PROCPOINTER:

	tmp_sym->attr.proc = PROC_MODULE;

        /* Use the procedure's name as it is in the iso_c_binding module for
           setting the binding label in case the user renamed the symbol.  */
	sprintf (tmp_sym->binding_label, "%s_%s", mod_name,
                 c_interop_kinds_table[s].name);
	tmp_sym->attr.is_iso_c = 1;
	if (s == ISOCBINDING_F_POINTER || s == ISOCBINDING_F_PROCPOINTER)
	  tmp_sym->attr.subroutine = 1;
	else
	  {
            /* TODO!  This needs to be finished more for the expr of the
               function or something!
               This may not need to be here, because trying to do c_loc
               as an external.  */
	    if (s == ISOCBINDING_ASSOCIATED)
	      {
		tmp_sym->attr.function = 1;
		tmp_sym->ts.type = BT_LOGICAL;
		tmp_sym->ts.kind = gfc_default_logical_kind;
		tmp_sym->result = tmp_sym;
	      }
	    else
	      {
               /* Here, we're taking the simple approach.  We're defining
                  c_loc as an external identifier so the compiler will put
                  what we expect on the stack for the address we want the
                  C address of.  */
		tmp_sym->ts.type = BT_DERIVED;
                if (s == ISOCBINDING_LOC)
                  tmp_sym->ts.derived =
                    get_iso_c_binding_dt (ISOCBINDING_PTR);
                else
                  tmp_sym->ts.derived =
                    get_iso_c_binding_dt (ISOCBINDING_FUNPTR);

                if (tmp_sym->ts.derived == NULL)
                  {
                    /* Create the necessary derived type so we can continue
                       processing the file.  */
                    generate_isocbinding_symbol
		      (mod_name, s == ISOCBINDING_FUNLOC
				 ? ISOCBINDING_FUNPTR : ISOCBINDING_PTR,
		       (const char *)(s == ISOCBINDING_FUNLOC
                                ? "_gfortran_iso_c_binding_c_funptr"
				: "_gfortran_iso_c_binding_c_ptr"));
                    tmp_sym->ts.derived =
                      get_iso_c_binding_dt (s == ISOCBINDING_FUNLOC
                                            ? ISOCBINDING_FUNPTR
                                            : ISOCBINDING_PTR);
                  }

		/* The function result is itself (no result clause).  */
		tmp_sym->result = tmp_sym;
		tmp_sym->attr.external = 1;
		tmp_sym->attr.use_assoc = 0;
		tmp_sym->attr.pure = 1;
		tmp_sym->attr.if_source = IFSRC_UNKNOWN;
		tmp_sym->attr.proc = PROC_UNKNOWN;
	      }
	  }

	tmp_sym->attr.flavor = FL_PROCEDURE;
	tmp_sym->attr.contained = 0;
	
       /* Try using this builder routine, with the new and old symbols
          both being the generic iso_c proc sym being created.  This
          will create the formal args (and the new namespace for them).
          Don't build an arg list for c_loc because we're going to treat
          c_loc as an external procedure.  */
	if (s != ISOCBINDING_LOC && s != ISOCBINDING_FUNLOC)
          /* The 1 says to add any optional args, if applicable.  */
	  build_formal_args (tmp_sym, tmp_sym, 1);

        /* Set this after setting up the symbol, to prevent error messages.  */
	tmp_sym->attr.use_assoc = 1;

        /* This symbol will not be referenced directly.  It will be
           resolved to the implementation for the given f90 kind.  */
	tmp_sym->attr.referenced = 0;

	break;

      default:
	gcc_unreachable ();
    }
}


/* Creates a new symbol based off of an old iso_c symbol, with a new
   binding label.  This function can be used to create a new,
   resolved, version of a procedure symbol for c_f_pointer or
   c_f_procpointer that is based on the generic symbols.  A new
   parameter list is created for the new symbol using
   build_formal_args().  The add_optional_flag specifies whether the
   to add the optional SHAPE argument.  The new symbol is
   returned.  */

gfc_symbol *
get_iso_c_sym (gfc_symbol *old_sym, char *new_name,
               char *new_binding_label, int add_optional_arg)
{
  gfc_symtree *new_symtree = NULL;

  /* See if we have a symbol by that name already available, looking
     through any parent namespaces.  */
  gfc_find_sym_tree (new_name, gfc_current_ns, 1, &new_symtree);
  if (new_symtree != NULL)
    /* Return the existing symbol.  */
    return new_symtree->n.sym;

  /* Create the symtree/symbol, with attempted host association.  */
  gfc_get_ha_sym_tree (new_name, &new_symtree);
  if (new_symtree == NULL)
    gfc_internal_error ("get_iso_c_sym(): Unable to create "
			"symtree for '%s'", new_name);

  /* Now fill in the fields of the resolved symbol with the old sym.  */
  strcpy (new_symtree->n.sym->binding_label, new_binding_label);
  new_symtree->n.sym->attr = old_sym->attr;
  new_symtree->n.sym->ts = old_sym->ts;
  new_symtree->n.sym->module = gfc_get_string (old_sym->module);
  new_symtree->n.sym->from_intmod = old_sym->from_intmod;
  new_symtree->n.sym->intmod_sym_id = old_sym->intmod_sym_id;
  /* Build the formal arg list.  */
  build_formal_args (new_symtree->n.sym, old_sym, add_optional_arg);

  gfc_commit_symbol (new_symtree->n.sym);

  return new_symtree->n.sym;
}


/* Check that a symbol is already typed.  If strict is not set, an untyped
   symbol is acceptable for non-standard-conforming mode.  */

gfc_try
gfc_check_symbol_typed (gfc_symbol* sym, gfc_namespace* ns,
			bool strict, locus where)
{
  gcc_assert (sym);

  if (gfc_matching_prefix)
    return SUCCESS;

  /* Check for the type and try to give it an implicit one.  */
  if (sym->ts.type == BT_UNKNOWN
      && gfc_set_default_type (sym, 0, ns) == FAILURE)
    {
      if (strict)
	{
	  gfc_error ("Symbol '%s' is used before it is typed at %L",
		     sym->name, &where);
	  return FAILURE;
	}

      if (gfc_notify_std (GFC_STD_GNU,
			  "Extension: Symbol '%s' is used before"
			  " it is typed at %L", sym->name, &where) == FAILURE)
	return FAILURE;
    }

  /* Everything is ok.  */
  return SUCCESS;
}


/* Construct a typebound-procedure structure.  Those are stored in a tentative
   list and marked `error' until symbols are committed.  */

gfc_typebound_proc*
gfc_get_typebound_proc (void)
{
  gfc_typebound_proc *result;
  tentative_tbp *list_node;

  result = XCNEW (gfc_typebound_proc);
  result->error = 1;

  list_node = XCNEW (tentative_tbp);
  list_node->next = tentative_tbp_list;
  list_node->proc = result;
  tentative_tbp_list = list_node;

  return result;
}


/* Get the super-type of a given derived type.  */

gfc_symbol*
gfc_get_derived_super_type (gfc_symbol* derived)
{
  if (!derived->attr.extension)
    return NULL;

  gcc_assert (derived->components);
  gcc_assert (derived->components->ts.type == BT_DERIVED);
  gcc_assert (derived->components->ts.derived);

  return derived->components->ts.derived;
}


/* General worker function to find either a type-bound procedure or a
   type-bound user operator.  */

static gfc_symtree*
find_typebound_proc_uop (gfc_symbol* derived, gfc_try* t,
			 const char* name, bool noaccess, bool uop)
{
  gfc_symtree* res;
  gfc_symtree* root;

  /* Set correct symbol-root.  */
  gcc_assert (derived->f2k_derived);
  root = (uop ? derived->f2k_derived->tb_uop_root
	      : derived->f2k_derived->tb_sym_root);

  /* Set default to failure.  */
  if (t)
    *t = FAILURE;

  /* Try to find it in the current type's namespace.  */
  res = gfc_find_symtree (root, name);
  if (res && res->n.tb)
    {
      /* We found one.  */
      if (t)
	*t = SUCCESS;

      if (!noaccess && derived->attr.use_assoc
	  && res->n.tb->access == ACCESS_PRIVATE)
	{
	  gfc_error ("'%s' of '%s' is PRIVATE at %C", name, derived->name);
	  if (t)
	    *t = FAILURE;
	}

      return res;
    }

  /* Otherwise, recurse on parent type if derived is an extension.  */
  if (derived->attr.extension)
    {
      gfc_symbol* super_type;
      super_type = gfc_get_derived_super_type (derived);
      gcc_assert (super_type);

      return find_typebound_proc_uop (super_type, t, name, noaccess, uop);
    }

  /* Nothing found.  */
  return NULL;
}


/* Find a type-bound procedure or user operator by name for a derived-type
   (looking recursively through the super-types).  */

gfc_symtree*
gfc_find_typebound_proc (gfc_symbol* derived, gfc_try* t,
			 const char* name, bool noaccess)
{
  return find_typebound_proc_uop (derived, t, name, noaccess, false);
}

gfc_symtree*
gfc_find_typebound_user_op (gfc_symbol* derived, gfc_try* t,
			    const char* name, bool noaccess)
{
  return find_typebound_proc_uop (derived, t, name, noaccess, true);
}


/* Find a type-bound intrinsic operator looking recursively through the
   super-type hierarchy.  */

gfc_typebound_proc*
gfc_find_typebound_intrinsic_op (gfc_symbol* derived, gfc_try* t,
				 gfc_intrinsic_op op, bool noaccess)
{
  gfc_typebound_proc* res;

  /* Set default to failure.  */
  if (t)
    *t = FAILURE;

  /* Try to find it in the current type's namespace.  */
  if (derived->f2k_derived)
    res = derived->f2k_derived->tb_op[op];
  else  
    res = NULL;

  /* Check access.  */
  if (res)
    {
      /* We found one.  */
      if (t)
	*t = SUCCESS;

      if (!noaccess && derived->attr.use_assoc
	  && res->access == ACCESS_PRIVATE)
	{
	  gfc_error ("'%s' of '%s' is PRIVATE at %C",
		     gfc_op2string (op), derived->name);
	  if (t)
	    *t = FAILURE;
	}

      return res;
    }

  /* Otherwise, recurse on parent type if derived is an extension.  */
  if (derived->attr.extension)
    {
      gfc_symbol* super_type;
      super_type = gfc_get_derived_super_type (derived);
      gcc_assert (super_type);

      return gfc_find_typebound_intrinsic_op (super_type, t, op, noaccess);
    }

  /* Nothing found.  */
  return NULL;
}


/* Get a typebound-procedure symtree or create and insert it if not yet
   present.  This is like a very simplified version of gfc_get_sym_tree for
   tbp-symtrees rather than regular ones.  */

gfc_symtree*
gfc_get_tbp_symtree (gfc_symtree **root, const char *name)
{
  gfc_symtree *result;

  result = gfc_find_symtree (*root, name);
  if (!result)
    {
      result = gfc_new_symtree (root, name);
      gcc_assert (result);
      result->n.tb = NULL;
    }

  return result;
}
