/* Maintain binary trees of symbols.
   Copyright (C) 2000-2017 Free Software Foundation, Inc.
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
#include "coretypes.h"
#include "options.h"
#include "gfortran.h"
#include "parse.h"
#include "match.h"
#include "constructor.h"


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
  minit ("UNION", FL_UNION), minit ("STRUCTURE", FL_STRUCT),
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

/* Set the mstrings for DTIO procedure names.  */
const mstring dtio_procs[] =
{
    minit ("_dtio_formatted_read", DTIO_RF),
    minit ("_dtio_formatted_write", DTIO_WF),
    minit ("_dtio_unformatted_read", DTIO_RUF),
    minit ("_dtio_unformatted_write", DTIO_WUF),
};

/* This is to make sure the backend generates setup code in the correct
   order.  */

static int next_dummy_order = 1;


gfc_namespace *gfc_current_ns;
gfc_namespace *gfc_global_ns_list;

gfc_gsymbol *gfc_gsym_root = NULL;

gfc_dt_list *gfc_derived_types;

static gfc_undo_change_set default_undo_chgset_var = { vNULL, vNULL, NULL };
static gfc_undo_change_set *latest_undo_chgset = &default_undo_chgset_var;


/*********** IMPLICIT NONE and IMPLICIT statement handlers ***********/

/* The following static variable indicates whether a particular element has
   been explicitly set or not.  */

static int new_flag[GFC_LETTERS];


/* Handle a correctly parsed IMPLICIT NONE.  */

void
gfc_set_implicit_none (bool type, bool external, locus *loc)
{
  int i;

  if (external)
    gfc_current_ns->has_implicit_none_export = 1;

  if (type)
    {
      gfc_current_ns->seen_implicit_none = 1;
      for (i = 0; i < GFC_LETTERS; i++)
	{
	  if (gfc_current_ns->set_flag[i])
	    {
	      gfc_error_now ("IMPLICIT NONE (type) statement at %L following an "
			     "IMPLICIT statement", loc);
	      return;
	    }
	  gfc_clear_ts (&gfc_current_ns->default_type[i]);
	  gfc_current_ns->set_flag[i] = 1;
	}
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

bool
gfc_add_new_implicit_range (int c1, int c2)
{
  int i;

  c1 -= 'a';
  c2 -= 'a';

  for (i = c1; i <= c2; i++)
    {
      if (new_flag[i])
	{
	  gfc_error ("Letter %qc already set in IMPLICIT statement at %C",
		     i + 'A');
	  return false;
	}

      new_flag[i] = 1;
    }

  return true;
}


/* Add a matched implicit range for gfc_set_implicit().  Check if merging
   the new implicit types back into the existing types will work.  */

bool
gfc_merge_new_implicit (gfc_typespec *ts)
{
  int i;

  if (gfc_current_ns->seen_implicit_none)
    {
      gfc_error ("Cannot specify IMPLICIT at %C after IMPLICIT NONE");
      return false;
    }

  for (i = 0; i < GFC_LETTERS; i++)
    {
      if (new_flag[i])
	{
	  if (gfc_current_ns->set_flag[i])
	    {
	      gfc_error ("Letter %qc already has an IMPLICIT type at %C",
			 i + 'A');
	      return false;
	    }

	  gfc_current_ns->default_type[i] = *ts;
	  gfc_current_ns->implicit_loc[i] = gfc_current_locus;
	  gfc_current_ns->set_flag[i] = 1;
	}
    }
  return true;
}


/* Given a symbol, return a pointer to the typespec for its default type.  */

gfc_typespec *
gfc_get_default_type (const char *name, gfc_namespace *ns)
{
  char letter;

  letter = name[0];

  if (flag_allow_leading_underscore && letter == '_')
    gfc_fatal_error ("Option %<-fallow-leading-underscore%> is for use only by "
		     "gfortran developers, and should not be used for "
		     "implicitly typed variables");

  if (letter < 'a' || letter > 'z')
    gfc_internal_error ("gfc_get_default_type(): Bad symbol %qs", name);

  if (ns == NULL)
    ns = gfc_current_ns;

  return &ns->default_type[letter - 'a'];
}


/* Recursively append candidate SYM to CANDIDATES.  Store the number of
   candidates in CANDIDATES_LEN.  */

static void
lookup_symbol_fuzzy_find_candidates (gfc_symtree *sym,
				     char **&candidates,
				     size_t &candidates_len)
{
  gfc_symtree *p;

  if (sym == NULL)
    return;

  if (sym->n.sym->ts.type != BT_UNKNOWN && sym->n.sym->ts.type != BT_PROCEDURE)
    vec_push (candidates, candidates_len, sym->name);
  p = sym->left;
  if (p)
    lookup_symbol_fuzzy_find_candidates (p, candidates, candidates_len);

  p = sym->right;
  if (p)
    lookup_symbol_fuzzy_find_candidates (p, candidates, candidates_len);
}


/* Lookup symbol SYM_NAME fuzzily, taking names in SYMBOL into account.  */

static const char*
lookup_symbol_fuzzy (const char *sym_name, gfc_symbol *symbol)
{
  char **candidates = NULL;
  size_t candidates_len = 0;
  lookup_symbol_fuzzy_find_candidates (symbol->ns->sym_root, candidates,
				       candidates_len);
  return gfc_closest_fuzzy_match (sym_name, candidates);
}


/* Given a pointer to a symbol, set its type according to the first
   letter of its name.  Fails if the letter in question has no default
   type.  */

bool
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
	  const char *guessed = lookup_symbol_fuzzy (sym->name, sym);
	  if (guessed)
	    gfc_error ("Symbol %qs at %L has no IMPLICIT type"
		       "; did you mean %qs?",
		       sym->name, &sym->declared_at, guessed);
	  else
	    gfc_error ("Symbol %qs at %L has no IMPLICIT type",
		       sym->name, &sym->declared_at);
	  sym->attr.untyped = 1; /* Ensure we only give an error once.  */
	}

      return false;
    }

  sym->ts = *ts;
  sym->attr.implicit_type = 1;

  if (ts->type == BT_CHARACTER && ts->u.cl)
    sym->ts.u.cl = gfc_new_charlen (sym->ns, ts->u.cl);
  else if (ts->type == BT_CLASS
	   && !gfc_build_class_symbol (&sym->ts, &sym->attr, &sym->as))
    return false;

  if (sym->attr.is_bind_c == 1 && warn_c_binding_type)
    {
      /* BIND(C) variables should not be implicitly declared.  */
      gfc_warning_now (OPT_Wc_binding_type, "Implicitly declared BIND(C) "
		       "variable %qs at %L may not be C interoperable",
		       sym->name, &sym->declared_at);
      sym->ts.f90_type = sym->ts.type;
    }

  if (sym->attr.dummy != 0)
    {
      if (sym->ns->proc_name != NULL
	  && (sym->ns->proc_name->attr.subroutine != 0
	      || sym->ns->proc_name->attr.function != 0)
	  && sym->ns->proc_name->attr.is_bind_c != 0
	  && warn_c_binding_type)
        {
          /* Dummy args to a BIND(C) routine may not be interoperable if
             they are implicitly typed.  */
          gfc_warning_now (OPT_Wc_binding_type, "Implicitly declared variable "
			   "%qs at %L may not be C interoperable but it is a "
			   "dummy argument to the BIND(C) procedure %qs at %L",
			   sym->name, &(sym->declared_at),
			   sym->ns->proc_name->name,
                           &(sym->ns->proc_name->declared_at));
          sym->ts.f90_type = sym->ts.type;
        }
    }

  return true;
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
      if (gfc_set_default_type (proc->result, 0, gfc_current_ns))
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
	  gfc_error ("Function result %qs at %L has no IMPLICIT type",
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

static bool
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
    *is_bind_c = "BIND(C)", *procedure = "PROCEDURE",
    *proc_pointer = "PROCEDURE POINTER", *abstract = "ABSTRACT",
    *asynchronous = "ASYNCHRONOUS", *codimension = "CODIMENSION",
    *contiguous = "CONTIGUOUS", *generic = "GENERIC", *automatic = "AUTOMATIC",
    *pdt_len = "LEN", *pdt_kind = "KIND";
  static const char *threadprivate = "THREADPRIVATE";
  static const char *omp_declare_target = "OMP DECLARE TARGET";
  static const char *omp_declare_target_link = "OMP DECLARE TARGET LINK";
  static const char *oacc_declare_copyin = "OACC DECLARE COPYIN";
  static const char *oacc_declare_create = "OACC DECLARE CREATE";
  static const char *oacc_declare_deviceptr = "OACC DECLARE DEVICEPTR";
  static const char *oacc_declare_device_resident =
						"OACC DECLARE DEVICE_RESIDENT";

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

  if (attr->in_namelist && (attr->allocatable || attr->pointer))
    {
      a1 = in_namelist;
      a2 = attr->allocatable ? allocatable : pointer;
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
	  return false;
	}
    }

  if (attr->save == SAVE_EXPLICIT)
    {
      conf (dummy, save);
      conf (in_common, save);
      conf (result, save);
      conf (automatic, save);

      switch (attr->flavor)
	{
	  case FL_PROGRAM:
	  case FL_BLOCK_DATA:
	  case FL_MODULE:
	  case FL_LABEL:
	  case_fl_struct:
	  case FL_PARAMETER:
            a1 = gfc_code2string (flavors, attr->flavor);
            a2 = save;
	    goto conflict;
	  case FL_NAMELIST:
	    gfc_error ("Namelist group name at %L cannot have the "
		       "SAVE attribute", where);
	    return false;
	  case FL_PROCEDURE:
	    /* Conflicts between SAVE and PROCEDURE will be checked at
	       resolution stage, see "resolve_fl_procedure".  */
	  case FL_VARIABLE:
	  default:
	    break;
	}
    }

  /* The copying of procedure dummy arguments for module procedures in
     a submodule occur whilst the current state is COMP_CONTAINS. It
     is necessary, therefore, to let this through.  */
  if (attr->dummy
      && (attr->function || attr->subroutine)
      && gfc_current_state () == COMP_CONTAINS
      && !(gfc_new_block && gfc_new_block->abr_modproc_decl))
    gfc_error_now ("internal procedure %qs at %L conflicts with "
		   "DUMMY argument", name, where);

  conf (dummy, entry);
  conf (dummy, intrinsic);
  conf (dummy, threadprivate);
  conf (dummy, omp_declare_target);
  conf (dummy, omp_declare_target_link);
  conf (pointer, target);
  conf (pointer, intrinsic);
  conf (pointer, elemental);
  conf (pointer, codimension);
  conf (allocatable, elemental);

  conf (in_common, automatic);
  conf (in_equivalence, automatic);
  conf (result, automatic);
  conf (use_assoc, automatic);
  conf (dummy, automatic);

  conf (target, external);
  conf (target, intrinsic);

  if (!attr->if_source)
    conf (external, dimension);   /* See Fortran 95's R504.  */

  conf (external, intrinsic);
  conf (entry, intrinsic);

  if ((attr->if_source == IFSRC_DECL && !attr->procedure) || attr->contained)
    conf (external, subroutine);

  if (attr->proc_pointer && !gfc_notify_std (GFC_STD_F2003,
					     "Procedure pointer at %C"))
    return false;

  conf (allocatable, pointer);
  conf_std (allocatable, dummy, GFC_STD_F2003);
  conf_std (allocatable, function, GFC_STD_F2003);
  conf_std (allocatable, result, GFC_STD_F2003);
  conf (elemental, recursive);

  conf (in_common, dummy);
  conf (in_common, allocatable);
  conf (in_common, codimension);
  conf (in_common, result);

  conf (in_equivalence, use_assoc);
  conf (in_equivalence, codimension);
  conf (in_equivalence, dummy);
  conf (in_equivalence, target);
  conf (in_equivalence, pointer);
  conf (in_equivalence, function);
  conf (in_equivalence, result);
  conf (in_equivalence, entry);
  conf (in_equivalence, allocatable);
  conf (in_equivalence, threadprivate);
  conf (in_equivalence, omp_declare_target);
  conf (in_equivalence, omp_declare_target_link);
  conf (in_equivalence, oacc_declare_create);
  conf (in_equivalence, oacc_declare_copyin);
  conf (in_equivalence, oacc_declare_deviceptr);
  conf (in_equivalence, oacc_declare_device_resident);
  conf (in_equivalence, is_bind_c);

  conf (dummy, result);
  conf (entry, result);
  conf (generic, result);
  conf (generic, omp_declare_target);
  conf (generic, omp_declare_target_link);

  conf (function, subroutine);

  if (!function && !subroutine)
    conf (is_bind_c, dummy);

  conf (is_bind_c, cray_pointer);
  conf (is_bind_c, cray_pointee);
  conf (is_bind_c, codimension);
  conf (is_bind_c, allocatable);
  conf (is_bind_c, elemental);

  /* Need to also get volatile attr, according to 5.1 of F2003 draft.
     Parameter conflict caught below.  Also, value cannot be specified
     for a dummy procedure.  */

  /* Cray pointer/pointee conflicts.  */
  conf (cray_pointer, cray_pointee);
  conf (cray_pointer, dimension);
  conf (cray_pointer, codimension);
  conf (cray_pointer, contiguous);
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
  conf (cray_pointee, contiguous);
  conf (cray_pointee, codimension);
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
  conf (cray_pointee, omp_declare_target);
  conf (cray_pointee, omp_declare_target_link);
  conf (cray_pointee, oacc_declare_create);
  conf (cray_pointee, oacc_declare_copyin);
  conf (cray_pointee, oacc_declare_deviceptr);
  conf (cray_pointee, oacc_declare_device_resident);

  conf (data, dummy);
  conf (data, function);
  conf (data, result);
  conf (data, allocatable);

  conf (value, pointer)
  conf (value, allocatable)
  conf (value, subroutine)
  conf (value, function)
  conf (value, volatile_)
  conf (value, dimension)
  conf (value, codimension)
  conf (value, external)

  conf (codimension, result)

  if (attr->value
      && (attr->intent == INTENT_OUT || attr->intent == INTENT_INOUT))
    {
      a1 = value;
      a2 = attr->intent == INTENT_OUT ? intent_out : intent_inout;
      goto conflict;
    }

  conf (is_protected, intrinsic)
  conf (is_protected, in_common)

  conf (asynchronous, intrinsic)
  conf (asynchronous, external)

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
  conf (procedure, codimension)
  conf (procedure, intrinsic)
  conf (procedure, target)
  conf (procedure, value)
  conf (procedure, volatile_)
  conf (procedure, asynchronous)
  conf (procedure, entry)

  conf (proc_pointer, abstract)
  conf (proc_pointer, omp_declare_target)
  conf (proc_pointer, omp_declare_target_link)

  conf (entry, omp_declare_target)
  conf (entry, omp_declare_target_link)
  conf (entry, oacc_declare_create)
  conf (entry, oacc_declare_copyin)
  conf (entry, oacc_declare_deviceptr)
  conf (entry, oacc_declare_device_resident)

  conf (pdt_kind, allocatable)
  conf (pdt_kind, pointer)
  conf (pdt_kind, dimension)
  conf (pdt_kind, codimension)

  conf (pdt_len, allocatable)
  conf (pdt_len, pointer)
  conf (pdt_len, dimension)
  conf (pdt_len, codimension)

  if (attr->access == ACCESS_PRIVATE)
    {
      a1 = privat;
      conf2 (pdt_kind);
      conf2 (pdt_len);
    }

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
      conf2 (codimension);
      conf2 (dimension);
      conf2 (dummy);
      conf2 (volatile_);
      conf2 (asynchronous);
      conf2 (contiguous);
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
      conf2 (omp_declare_target);
      conf2 (omp_declare_target_link);
      conf2 (oacc_declare_create);
      conf2 (oacc_declare_copyin);
      conf2 (oacc_declare_deviceptr);
      conf2 (oacc_declare_device_resident);

      if (attr->access == ACCESS_PUBLIC || attr->access == ACCESS_PRIVATE)
	{
	  a2 = attr->access == ACCESS_PUBLIC ? publik : privat;
	  gfc_error ("%s attribute applied to %s %s at %L", a2, a1,
	    name, where);
	  return false;
	}

      if (attr->is_bind_c)
	{
	  gfc_error_now ("BIND(C) applied to %s %s at %L", a1, name, where);
	  return false;
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
	  a1 = subroutine;
	  conf2 (target);
	  conf2 (allocatable);
	  conf2 (volatile_);
	  conf2 (asynchronous);
	  conf2 (in_namelist);
	  conf2 (codimension);
	  conf2 (dimension);
	  conf2 (function);
	  if (!attr->proc_pointer)
	    conf2 (threadprivate);
	}

      if (!attr->proc_pointer)
	conf2 (in_common);

      conf2 (omp_declare_target_link);

      switch (attr->proc)
	{
	case PROC_ST_FUNCTION:
	  conf2 (dummy);
	  conf2 (target);
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

    case_fl_struct:
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
      conf2 (omp_declare_target);
      conf2 (omp_declare_target_link);
      conf2 (oacc_declare_create);
      conf2 (oacc_declare_copyin);
      conf2 (oacc_declare_deviceptr);
      conf2 (oacc_declare_device_resident);

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
      conf2 (contiguous);
      conf2 (pointer);
      conf2 (is_protected);
      conf2 (target);
      conf2 (dummy);
      conf2 (in_common);
      conf2 (value);
      conf2 (volatile_);
      conf2 (asynchronous);
      conf2 (threadprivate);
      conf2 (value);
      conf2 (codimension);
      conf2 (result);
      if (!attr->is_iso_c)
	conf2 (is_bind_c);
      break;

    default:
      break;
    }

  return true;

conflict:
  if (name == NULL)
    gfc_error ("%s attribute conflicts with %s attribute at %L",
	       a1, a2, where);
  else
    gfc_error ("%s attribute conflicts with %s attribute in %qs at %L",
	       a1, a2, name, where);

  return false;

conflict_std:
  if (name == NULL)
    {
      return gfc_notify_std (standard, "%s attribute conflicts "
                             "with %s attribute at %L", a1, a2,
                             where);
    }
  else
    {
      return gfc_notify_std (standard, "%s attribute conflicts "
			     "with %s attribute in %qs at %L",
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


bool
gfc_add_ext_attribute (symbol_attribute *attr, ext_attr_id_t ext_attr,
		       locus *where ATTRIBUTE_UNUSED)
{
  attr->ext_attr |= 1 << ext_attr;
  return true;
}


/* Called from decl.c (attr_decl1) to check attributes, when declared
   separately.  */

bool
gfc_add_attribute (symbol_attribute *attr, locus *where)
{
  if (check_used (attr, NULL, where))
    return false;

  return check_conflict (attr, NULL, where);
}


bool
gfc_add_allocatable (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->allocatable)
    {
      duplicate_attr ("ALLOCATABLE", where);
      return false;
    }

  if (attr->flavor == FL_PROCEDURE && attr->if_source == IFSRC_IFBODY
      && !gfc_find_state (COMP_INTERFACE))
    {
      gfc_error ("ALLOCATABLE specified outside of INTERFACE body at %L",
		 where);
      return false;
    }

  attr->allocatable = 1;
  return check_conflict (attr, NULL, where);
}


bool
gfc_add_automatic (symbol_attribute *attr, const char *name, locus *where)
{
  if (check_used (attr, name, where))
    return false;

  if (attr->automatic && !gfc_notify_std (GFC_STD_LEGACY,
	"Duplicate AUTOMATIC attribute specified at %L", where))
    return false;

  attr->automatic = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_codimension (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  if (attr->codimension)
    {
      duplicate_attr ("CODIMENSION", where);
      return false;
    }

  if (attr->flavor == FL_PROCEDURE && attr->if_source == IFSRC_IFBODY
      && !gfc_find_state (COMP_INTERFACE))
    {
      gfc_error ("CODIMENSION specified for %qs outside its INTERFACE body "
		 "at %L", name, where);
      return false;
    }

  attr->codimension = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_dimension (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  if (attr->dimension)
    {
      duplicate_attr ("DIMENSION", where);
      return false;
    }

  if (attr->flavor == FL_PROCEDURE && attr->if_source == IFSRC_IFBODY
      && !gfc_find_state (COMP_INTERFACE))
    {
      gfc_error ("DIMENSION specified for %qs outside its INTERFACE body "
		 "at %L", name, where);
      return false;
    }

  attr->dimension = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_contiguous (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  attr->contiguous = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_external (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->external)
    {
      duplicate_attr ("EXTERNAL", where);
      return false;
    }

  if (attr->pointer && attr->if_source != IFSRC_IFBODY)
    {
      attr->pointer = 0;
      attr->proc_pointer = 1;
    }

  attr->external = 1;

  return check_conflict (attr, NULL, where);
}


bool
gfc_add_intrinsic (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->intrinsic)
    {
      duplicate_attr ("INTRINSIC", where);
      return false;
    }

  attr->intrinsic = 1;

  return check_conflict (attr, NULL, where);
}


bool
gfc_add_optional (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->optional)
    {
      duplicate_attr ("OPTIONAL", where);
      return false;
    }

  attr->optional = 1;
  return check_conflict (attr, NULL, where);
}

bool
gfc_add_kind (symbol_attribute *attr, locus *where)
{
  if (attr->pdt_kind)
    {
      duplicate_attr ("KIND", where);
      return false;
    }

  attr->pdt_kind = 1;
  return check_conflict (attr, NULL, where);
}

bool
gfc_add_len (symbol_attribute *attr, locus *where)
{
  if (attr->pdt_len)
    {
      duplicate_attr ("LEN", where);
      return false;
    }

  attr->pdt_len = 1;
  return check_conflict (attr, NULL, where);
}


bool
gfc_add_pointer (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->pointer && !(attr->if_source == IFSRC_IFBODY
      && !gfc_find_state (COMP_INTERFACE)))
    {
      duplicate_attr ("POINTER", where);
      return false;
    }

  if (attr->procedure || (attr->external && attr->if_source != IFSRC_IFBODY)
      || (attr->if_source == IFSRC_IFBODY
      && !gfc_find_state (COMP_INTERFACE)))
    attr->proc_pointer = 1;
  else
    attr->pointer = 1;

  return check_conflict (attr, NULL, where);
}


bool
gfc_add_cray_pointer (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  attr->cray_pointer = 1;
  return check_conflict (attr, NULL, where);
}


bool
gfc_add_cray_pointee (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->cray_pointee)
    {
      gfc_error ("Cray Pointee at %L appears in multiple pointer()"
		 " statements", where);
      return false;
    }

  attr->cray_pointee = 1;
  return check_conflict (attr, NULL, where);
}


bool
gfc_add_protected (symbol_attribute *attr, const char *name, locus *where)
{
  if (check_used (attr, name, where))
    return false;

  if (attr->is_protected)
    {
	if (!gfc_notify_std (GFC_STD_LEGACY,
			     "Duplicate PROTECTED attribute specified at %L",
			     where))
	  return false;
    }

  attr->is_protected = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_result (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  attr->result = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_save (symbol_attribute *attr, save_state s, const char *name,
	      locus *where)
{

  if (check_used (attr, name, where))
    return false;

  if (s == SAVE_EXPLICIT && gfc_pure (NULL))
    {
      gfc_error
	("SAVE attribute at %L cannot be specified in a PURE procedure",
	 where);
      return false;
    }

  if (s == SAVE_EXPLICIT)
    gfc_unset_implicit_pure (NULL);

  if (s == SAVE_EXPLICIT && attr->save == SAVE_EXPLICIT)
    {
	if (!gfc_notify_std (GFC_STD_LEGACY,
			     "Duplicate SAVE attribute specified at %L",
			     where))
	  return false;
    }

  attr->save = s;
  return check_conflict (attr, name, where);
}


bool
gfc_add_value (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  if (attr->value)
    {
	if (!gfc_notify_std (GFC_STD_LEGACY,
			     "Duplicate VALUE attribute specified at %L",
			     where))
	  return false;
    }

  attr->value = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_volatile (symbol_attribute *attr, const char *name, locus *where)
{
  /* No check_used needed as 11.2.1 of the F2003 standard allows
     that the local identifier made accessible by a use statement can be
     given a VOLATILE attribute - unless it is a coarray (F2008, C560).  */

  if (attr->volatile_ && attr->volatile_ns == gfc_current_ns)
    if (!gfc_notify_std (GFC_STD_LEGACY,
			 "Duplicate VOLATILE attribute specified at %L",
			 where))
      return false;

  attr->volatile_ = 1;
  attr->volatile_ns = gfc_current_ns;
  return check_conflict (attr, name, where);
}


bool
gfc_add_asynchronous (symbol_attribute *attr, const char *name, locus *where)
{
  /* No check_used needed as 11.2.1 of the F2003 standard allows
     that the local identifier made accessible by a use statement can be
     given a ASYNCHRONOUS attribute.  */

  if (attr->asynchronous && attr->asynchronous_ns == gfc_current_ns)
    if (!gfc_notify_std (GFC_STD_LEGACY,
			 "Duplicate ASYNCHRONOUS attribute specified at %L",
			 where))
      return false;

  attr->asynchronous = 1;
  attr->asynchronous_ns = gfc_current_ns;
  return check_conflict (attr, name, where);
}


bool
gfc_add_threadprivate (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  if (attr->threadprivate)
    {
      duplicate_attr ("THREADPRIVATE", where);
      return false;
    }

  attr->threadprivate = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_omp_declare_target (symbol_attribute *attr, const char *name,
			    locus *where)
{

  if (check_used (attr, name, where))
    return false;

  if (attr->omp_declare_target)
    return true;

  attr->omp_declare_target = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_omp_declare_target_link (symbol_attribute *attr, const char *name,
				 locus *where)
{

  if (check_used (attr, name, where))
    return false;

  if (attr->omp_declare_target_link)
    return true;

  attr->omp_declare_target_link = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_oacc_declare_create (symbol_attribute *attr, const char *name,
			     locus *where)
{
  if (check_used (attr, name, where))
    return false;

  if (attr->oacc_declare_create)
    return true;

  attr->oacc_declare_create = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_oacc_declare_copyin (symbol_attribute *attr, const char *name,
			     locus *where)
{
  if (check_used (attr, name, where))
    return false;

  if (attr->oacc_declare_copyin)
    return true;

  attr->oacc_declare_copyin = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_oacc_declare_deviceptr (symbol_attribute *attr, const char *name,
				locus *where)
{
  if (check_used (attr, name, where))
    return false;

  if (attr->oacc_declare_deviceptr)
    return true;

  attr->oacc_declare_deviceptr = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_oacc_declare_device_resident (symbol_attribute *attr, const char *name,
				      locus *where)
{
  if (check_used (attr, name, where))
    return false;

  if (attr->oacc_declare_device_resident)
    return true;

  attr->oacc_declare_device_resident = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_target (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->target)
    {
      duplicate_attr ("TARGET", where);
      return false;
    }

  attr->target = 1;
  return check_conflict (attr, NULL, where);
}


bool
gfc_add_dummy (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  /* Duplicate dummy arguments are allowed due to ENTRY statements.  */
  attr->dummy = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_in_common (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  /* Duplicate attribute already checked for.  */
  attr->in_common = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_in_equivalence (symbol_attribute *attr, const char *name, locus *where)
{

  /* Duplicate attribute already checked for.  */
  attr->in_equivalence = 1;
  if (!check_conflict (attr, name, where))
    return false;

  if (attr->flavor == FL_VARIABLE)
    return true;

  return gfc_add_flavor (attr, FL_VARIABLE, name, where);
}


bool
gfc_add_data (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  attr->data = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_in_namelist (symbol_attribute *attr, const char *name, locus *where)
{

  attr->in_namelist = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_sequence (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  attr->sequence = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_elemental (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->elemental)
    {
      duplicate_attr ("ELEMENTAL", where);
      return false;
    }

  attr->elemental = 1;
  return check_conflict (attr, NULL, where);
}


bool
gfc_add_pure (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->pure)
    {
      duplicate_attr ("PURE", where);
      return false;
    }

  attr->pure = 1;
  return check_conflict (attr, NULL, where);
}


bool
gfc_add_recursive (symbol_attribute *attr, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->recursive)
    {
      duplicate_attr ("RECURSIVE", where);
      return false;
    }

  attr->recursive = 1;
  return check_conflict (attr, NULL, where);
}


bool
gfc_add_entry (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  if (attr->entry)
    {
      duplicate_attr ("ENTRY", where);
      return false;
    }

  attr->entry = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_function (symbol_attribute *attr, const char *name, locus *where)
{

  if (attr->flavor != FL_PROCEDURE
      && !gfc_add_flavor (attr, FL_PROCEDURE, name, where))
    return false;

  attr->function = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_subroutine (symbol_attribute *attr, const char *name, locus *where)
{

  if (attr->flavor != FL_PROCEDURE
      && !gfc_add_flavor (attr, FL_PROCEDURE, name, where))
    return false;

  attr->subroutine = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_generic (symbol_attribute *attr, const char *name, locus *where)
{

  if (attr->flavor != FL_PROCEDURE
      && !gfc_add_flavor (attr, FL_PROCEDURE, name, where))
    return false;

  attr->generic = 1;
  return check_conflict (attr, name, where);
}


bool
gfc_add_proc (symbol_attribute *attr, const char *name, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

  if (attr->flavor != FL_PROCEDURE
      && !gfc_add_flavor (attr, FL_PROCEDURE, name, where))
    return false;

  if (attr->procedure)
    {
      duplicate_attr ("PROCEDURE", where);
      return false;
    }

  attr->procedure = 1;

  return check_conflict (attr, NULL, where);
}


bool
gfc_add_abstract (symbol_attribute* attr, locus* where)
{
  if (attr->abstract)
    {
      duplicate_attr ("ABSTRACT", where);
      return false;
    }

  attr->abstract = 1;

  return check_conflict (attr, NULL, where);
}


/* Flavors are special because some flavors are not what Fortran
   considers attributes and can be reaffirmed multiple times.  */

bool
gfc_add_flavor (symbol_attribute *attr, sym_flavor f, const char *name,
		locus *where)
{

  if ((f == FL_PROGRAM || f == FL_BLOCK_DATA || f == FL_MODULE
       || f == FL_PARAMETER || f == FL_LABEL || gfc_fl_struct(f)
       || f == FL_NAMELIST) && check_used (attr, name, where))
    return false;

  if (attr->flavor == f && f == FL_VARIABLE)
    return true;

  /* Copying a procedure dummy argument for a module procedure in a
     submodule results in the flavor being copied and would result in
     an error without this.  */
  if (gfc_new_block && gfc_new_block->abr_modproc_decl
      && attr->flavor == f && f == FL_PROCEDURE)
    return true;

  if (attr->flavor != FL_UNKNOWN)
    {
      if (where == NULL)
	where = &gfc_current_locus;

      if (name)
        gfc_error ("%s attribute of %qs conflicts with %s attribute at %L",
		   gfc_code2string (flavors, attr->flavor), name,
		   gfc_code2string (flavors, f), where);
      else
        gfc_error ("%s attribute conflicts with %s attribute at %L",
		   gfc_code2string (flavors, attr->flavor),
		   gfc_code2string (flavors, f), where);

      return false;
    }

  attr->flavor = f;

  return check_conflict (attr, name, where);
}


bool
gfc_add_procedure (symbol_attribute *attr, procedure_type t,
		   const char *name, locus *where)
{

  if (check_used (attr, name, where))
    return false;

  if (attr->flavor != FL_PROCEDURE
      && !gfc_add_flavor (attr, FL_PROCEDURE, name, where))
    return false;

  if (where == NULL)
    where = &gfc_current_locus;

  if (attr->proc != PROC_UNKNOWN && !attr->module_procedure)
    {
      if (attr->proc == PROC_ST_FUNCTION && t == PROC_INTERNAL
	  && !gfc_notification_std (GFC_STD_F2008))
	gfc_error ("%s procedure at %L is already declared as %s "
		   "procedure. \nF2008: A pointer function assignment "
		   "is ambiguous if it is the first executable statement "
		   "after the specification block. Please add any other "
		   "kind of executable statement before it. FIXME",
		 gfc_code2string (procedures, t), where,
		 gfc_code2string (procedures, attr->proc));
      else
	gfc_error ("%s procedure at %L is already declared as %s "
		   "procedure", gfc_code2string (procedures, t), where,
		   gfc_code2string (procedures, attr->proc));

      return false;
    }

  attr->proc = t;

  /* Statement functions are always scalar and functions.  */
  if (t == PROC_ST_FUNCTION
      && ((!attr->function && !gfc_add_function (attr, name, where))
	  || attr->dimension))
    return false;

  return check_conflict (attr, name, where);
}


bool
gfc_add_intent (symbol_attribute *attr, sym_intent intent, locus *where)
{

  if (check_used (attr, NULL, where))
    return false;

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

  return false;
}


/* No checks for use-association in public and private statements.  */

bool
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

  return false;
}


/* Set the is_bind_c field for the given symbol_attribute.  */

bool
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

  if (!gfc_notify_std (GFC_STD_F2003, "BIND(C) at %L", where))
    return false;

  return check_conflict (attr, name, where);
}


/* Set the extension field for the given symbol_attribute.  */

bool
gfc_add_extension (symbol_attribute *attr, locus *where)
{
  if (where == NULL)
    where = &gfc_current_locus;

  if (attr->extension)
    gfc_error_now ("Duplicate EXTENDS attribute specified at %L", where);
  else
    attr->extension = 1;

  if (!gfc_notify_std (GFC_STD_F2003, "EXTENDS at %L", where))
    return false;

  return true;
}


bool
gfc_add_explicit_interface (gfc_symbol *sym, ifsrc source,
			    gfc_formal_arglist * formal, locus *where)
{
  if (check_used (&sym->attr, sym->name, where))
    return false;

  /* Skip the following checks in the case of a module_procedures in a
     submodule since they will manifestly fail.  */
  if (sym->attr.module_procedure == 1
      && source == IFSRC_DECL)
    goto finish;

  if (where == NULL)
    where = &gfc_current_locus;

  if (sym->attr.if_source != IFSRC_UNKNOWN
      && sym->attr.if_source != IFSRC_DECL)
    {
      gfc_error ("Symbol %qs at %L already has an explicit interface",
		 sym->name, where);
      return false;
    }

  if (source == IFSRC_IFBODY && (sym->attr.dimension || sym->attr.allocatable))
    {
      gfc_error ("%qs at %L has attributes specified outside its INTERFACE "
		 "body", sym->name, where);
      return false;
    }

finish:
  sym->formal = formal;
  sym->attr.if_source = source;

  return true;
}


/* Add a type to a symbol.  */

bool
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

  if (type != BT_UNKNOWN && !(sym->attr.function && sym->attr.implicit_type)
      && !(gfc_state_stack->previous && gfc_state_stack->previous->previous
	   && gfc_state_stack->previous->previous->state == COMP_SUBMODULE)
      && !sym->attr.module_procedure)
    {
      if (sym->attr.use_assoc)
	gfc_error ("Symbol %qs at %L conflicts with symbol from module %qs, "
		   "use-associated at %L", sym->name, where, sym->module,
		   &sym->declared_at);
      else
	gfc_error ("Symbol %qs at %L already has basic type of %s", sym->name,
		 where, gfc_basic_typename (type));
      return false;
    }

  if (sym->attr.procedure && sym->ts.interface)
    {
      gfc_error ("Procedure %qs at %L may not have basic type of %s",
		 sym->name, where, gfc_basic_typename (ts->type));
      return false;
    }

  flavor = sym->attr.flavor;

  if (flavor == FL_PROGRAM || flavor == FL_BLOCK_DATA || flavor == FL_MODULE
      || flavor == FL_LABEL
      || (flavor == FL_PROCEDURE && sym->attr.subroutine)
      || flavor == FL_DERIVED || flavor == FL_NAMELIST)
    {
      gfc_error ("Symbol %qs at %L cannot have a type", sym->name, where);
      return false;
    }

  sym->ts = *ts;
  return true;
}


/* Clears all attributes.  */

void
gfc_clear_attr (symbol_attribute *attr)
{
  memset (attr, 0, sizeof (symbol_attribute));
}


/* Check for missing attributes in the new symbol.  Currently does
   nothing, but it's not clear that it is unnecessary yet.  */

bool
gfc_missing_attr (symbol_attribute *attr ATTRIBUTE_UNUSED,
		  locus *where ATTRIBUTE_UNUSED)
{

  return true;
}


/* Copy an attribute to a symbol attribute, bit by bit.  Some
   attributes have a lot of side-effects but cannot be present given
   where we are called from, so we ignore some bits.  */

bool
gfc_copy_attr (symbol_attribute *dest, symbol_attribute *src, locus *where)
{
  int is_proc_lang_bind_spec;

  /* In line with the other attributes, we only add bits but do not remove
     them; cf. also PR 41034.  */
  dest->ext_attr |= src->ext_attr;

  if (src->allocatable && !gfc_add_allocatable (dest, where))
    goto fail;

  if (src->automatic && !gfc_add_automatic (dest, NULL, where))
    goto fail;
  if (src->dimension && !gfc_add_dimension (dest, NULL, where))
    goto fail;
  if (src->codimension && !gfc_add_codimension (dest, NULL, where))
    goto fail;
  if (src->contiguous && !gfc_add_contiguous (dest, NULL, where))
    goto fail;
  if (src->optional && !gfc_add_optional (dest, where))
    goto fail;
  if (src->pointer && !gfc_add_pointer (dest, where))
    goto fail;
  if (src->is_protected && !gfc_add_protected (dest, NULL, where))
    goto fail;
  if (src->save && !gfc_add_save (dest, src->save, NULL, where))
    goto fail;
  if (src->value && !gfc_add_value (dest, NULL, where))
    goto fail;
  if (src->volatile_ && !gfc_add_volatile (dest, NULL, where))
    goto fail;
  if (src->asynchronous && !gfc_add_asynchronous (dest, NULL, where))
    goto fail;
  if (src->threadprivate
      && !gfc_add_threadprivate (dest, NULL, where))
    goto fail;
  if (src->omp_declare_target
      && !gfc_add_omp_declare_target (dest, NULL, where))
    goto fail;
  if (src->omp_declare_target_link
      && !gfc_add_omp_declare_target_link (dest, NULL, where))
    goto fail;
  if (src->oacc_declare_create
      && !gfc_add_oacc_declare_create (dest, NULL, where))
    goto fail;
  if (src->oacc_declare_copyin
      && !gfc_add_oacc_declare_copyin (dest, NULL, where))
    goto fail;
  if (src->oacc_declare_deviceptr
      && !gfc_add_oacc_declare_deviceptr (dest, NULL, where))
    goto fail;
  if (src->oacc_declare_device_resident
      && !gfc_add_oacc_declare_device_resident (dest, NULL, where))
    goto fail;
  if (src->target && !gfc_add_target (dest, where))
    goto fail;
  if (src->dummy && !gfc_add_dummy (dest, NULL, where))
    goto fail;
  if (src->result && !gfc_add_result (dest, NULL, where))
    goto fail;
  if (src->entry)
    dest->entry = 1;

  if (src->in_namelist && !gfc_add_in_namelist (dest, NULL, where))
    goto fail;

  if (src->in_common && !gfc_add_in_common (dest, NULL, where))
    goto fail;

  if (src->generic && !gfc_add_generic (dest, NULL, where))
    goto fail;
  if (src->function && !gfc_add_function (dest, NULL, where))
    goto fail;
  if (src->subroutine && !gfc_add_subroutine (dest, NULL, where))
    goto fail;

  if (src->sequence && !gfc_add_sequence (dest, NULL, where))
    goto fail;
  if (src->elemental && !gfc_add_elemental (dest, where))
    goto fail;
  if (src->pure && !gfc_add_pure (dest, where))
    goto fail;
  if (src->recursive && !gfc_add_recursive (dest, where))
    goto fail;

  if (src->flavor != FL_UNKNOWN
      && !gfc_add_flavor (dest, src->flavor, NULL, where))
    goto fail;

  if (src->intent != INTENT_UNKNOWN
      && !gfc_add_intent (dest, src->intent, where))
    goto fail;

  if (src->access != ACCESS_UNKNOWN
      && !gfc_add_access (dest, src->access, NULL, where))
    goto fail;

  if (!gfc_missing_attr (dest, where))
    goto fail;

  if (src->cray_pointer && !gfc_add_cray_pointer (dest, where))
    goto fail;
  if (src->cray_pointee && !gfc_add_cray_pointee (dest, where))
    goto fail;

  is_proc_lang_bind_spec = (src->flavor == FL_PROCEDURE ? 1 : 0);
  if (src->is_bind_c
      && !gfc_add_is_bind_c (dest, NULL, where, is_proc_lang_bind_spec))
    return false;

  if (src->is_c_interop)
    dest->is_c_interop = 1;
  if (src->is_iso_c)
    dest->is_iso_c = 1;

  if (src->external && !gfc_add_external (dest, where))
    goto fail;
  if (src->intrinsic && !gfc_add_intrinsic (dest, where))
    goto fail;
  if (src->proc_pointer)
    dest->proc_pointer = 1;

  return true;

fail:
  return false;
}


/* A function to generate a dummy argument symbol using that from the
   interface declaration. Can be used for the result symbol as well if
   the flag is set.  */

int
gfc_copy_dummy_sym (gfc_symbol **dsym, gfc_symbol *sym, int result)
{
  int rc;

  rc = gfc_get_symbol (sym->name, NULL, dsym);
  if (rc)
    return rc;

  if (!gfc_add_type (*dsym, &(sym->ts), &gfc_current_locus))
    return 1;

  if (!gfc_copy_attr (&(*dsym)->attr, &(sym->attr),
      &gfc_current_locus))
    return 1;

  if ((*dsym)->attr.dimension)
    (*dsym)->as = gfc_copy_array_spec (sym->as);

  (*dsym)->attr.class_ok = sym->attr.class_ok;

  if ((*dsym) != NULL && !result
      && (!gfc_add_dummy(&(*dsym)->attr, (*dsym)->name, NULL)
	  || !gfc_missing_attr (&(*dsym)->attr, NULL)))
    return 1;
  else if ((*dsym) != NULL && result
      && (!gfc_add_result(&(*dsym)->attr, (*dsym)->name, NULL)
	  || !gfc_missing_attr (&(*dsym)->attr, NULL)))
    return 1;

  return 0;
}


/************** Component name management ************/

/* Component names of a derived type form their own little namespaces
   that are separate from all other spaces.  The space is composed of
   a singly linked list of gfc_component structures whose head is
   located in the parent symbol.  */


/* Add a component name to a symbol.  The call fails if the name is
   already present.  On success, the component pointer is modified to
   point to the additional component structure.  */

bool
gfc_add_component (gfc_symbol *sym, const char *name,
		   gfc_component **component)
{
  gfc_component *p, *tail;

  /* Check for existing components with the same name, but not for union
     components or containers. Unions and maps are anonymous so they have
     unique internal names which will never conflict.
     Don't use gfc_find_component here because it calls gfc_use_derived,
     but the derived type may not be fully defined yet. */
  tail = NULL;

  for (p = sym->components; p; p = p->next)
    {
      if (strcmp (p->name, name) == 0)
	{
	  gfc_error ("Component %qs at %C already declared at %L",
		     name, &p->loc);
	  return false;
	}

      tail = p;
    }

  if (sym->attr.extension
	&& gfc_find_component (sym->components->ts.u.derived,
                               name, true, true, NULL))
    {
      gfc_error ("Component %qs at %C already in the parent type "
		 "at %L", name, &sym->components->ts.u.derived->declared_at);
      return false;
    }

  /* Allocate a new component.  */
  p = gfc_get_component ();

  if (tail == NULL)
    sym->components = p;
  else
    tail->next = p;

  p->name = gfc_get_string ("%s", name);
  p->loc = gfc_current_locus;
  p->ts.type = BT_UNKNOWN;

  *component = p;
  return true;
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
  if (sym->ts.type == BT_DERIVED && sym->ts.u.derived == from)
    sym->ts.u.derived = to;

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

  if (!sym)
    return NULL;

  if (sym->attr.unlimited_polymorphic)
    return sym;

  if (sym->attr.generic)
    sym = gfc_find_dt_in_generic (sym);

  if (sym->components != NULL || sym->attr.zero_comp)
    return sym;               /* Already defined.  */

  if (sym->ns->parent == NULL)
    goto bad;

  if (gfc_find_symbol (sym->name, sym->ns->parent, 1, &s))
    {
      gfc_error ("Symbol %qs at %C is ambiguous", sym->name);
      return NULL;
    }

  if (s == NULL || !gfc_fl_struct (s->attr.flavor))
    goto bad;

  /* Get rid of symbol sym, translating all references to s.  */
  for (i = 0; i < GFC_LETTERS; i++)
    {
      t = &sym->ns->default_type[i];
      if (t->u.derived == sym)
	t->u.derived = s;
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
  gfc_error ("Derived type %qs at %C is being used before it is defined",
	     sym->name);
  return NULL;
}


/* Find the component with the given name in the union type symbol.
   If ref is not NULL it will be set to the chain of components through which
   the component can actually be accessed. This is necessary for unions because
   intermediate structures may be maps, nested structures, or other unions,
   all of which may (or must) be 'anonymous' to user code.  */

static gfc_component *
find_union_component (gfc_symbol *un, const char *name,
                      bool noaccess, gfc_ref **ref)
{
  gfc_component *m, *check;
  gfc_ref *sref, *tmp;

  for (m = un->components; m; m = m->next)
    {
      check = gfc_find_component (m->ts.u.derived, name, noaccess, true, &tmp);
      if (check == NULL)
        continue;

      /* Found component somewhere in m; chain the refs together.  */
      if (ref)
        {
          /* Map ref. */
          sref = gfc_get_ref ();
          sref->type = REF_COMPONENT;
          sref->u.c.component = m;
          sref->u.c.sym = m->ts.u.derived;
          sref->next = tmp;

          *ref = sref;
        }
      /* Other checks (such as access) were done in the recursive calls.  */
      return check;
    }
  return NULL;
}


/* Recursively append candidate COMPONENT structures to CANDIDATES.  Store
   the number of total candidates in CANDIDATES_LEN.  */

static void
lookup_component_fuzzy_find_candidates (gfc_component *component,
					char **&candidates,
					size_t &candidates_len)
{
  for (gfc_component *p = component; p; p = p->next)
    vec_push (candidates, candidates_len, p->name);
}


/* Lookup component MEMBER fuzzily, taking names in COMPONENT into account.  */

static const char*
lookup_component_fuzzy (const char *member, gfc_component *component)
{
  char **candidates = NULL;
  size_t candidates_len = 0;
  lookup_component_fuzzy_find_candidates (component, candidates,
					  candidates_len);
  return gfc_closest_fuzzy_match (member, candidates);
}


/* Given a derived type node and a component name, try to locate the
   component structure.  Returns the NULL pointer if the component is
   not found or the components are private.  If noaccess is set, no access
   checks are done.  If silent is set, an error will not be generated if
   the component cannot be found or accessed.

   If ref is not NULL, *ref is set to represent the chain of components
   required to get to the ultimate component.

   If the component is simply a direct subcomponent, or is inherited from a
   parent derived type in the given derived type, this is a single ref with its
   component set to the returned component.

   Otherwise, *ref is constructed as a chain of subcomponents. This occurs
   when the component is found through an implicit chain of nested union and
   map components. Unions and maps are "anonymous" substructures in FORTRAN
   which cannot be explicitly referenced, but the reference chain must be
   considered as in C for backend translation to correctly compute layouts.
   (For example, x.a may refer to x->(UNION)->(MAP)->(UNION)->(MAP)->a).  */

gfc_component *
gfc_find_component (gfc_symbol *sym, const char *name,
		    bool noaccess, bool silent, gfc_ref **ref)
{
  gfc_component *p, *check;
  gfc_ref *sref = NULL, *tmp = NULL;

  if (name == NULL || sym == NULL)
    return NULL;

  if (sym->attr.flavor == FL_DERIVED)
    sym = gfc_use_derived (sym);
  else
    gcc_assert (gfc_fl_struct (sym->attr.flavor));

  if (sym == NULL)
    return NULL;

  /* Handle UNIONs specially - mutually recursive with gfc_find_component. */
  if (sym->attr.flavor == FL_UNION)
    return find_union_component (sym, name, noaccess, ref);

  if (ref) *ref = NULL;
  for (p = sym->components; p; p = p->next)
    {
      /* Nest search into union's maps. */
      if (p->ts.type == BT_UNION)
        {
          check = find_union_component (p->ts.u.derived, name, noaccess, &tmp);
          if (check != NULL)
            {
              /* Union ref. */
              if (ref)
                {
                  sref = gfc_get_ref ();
                  sref->type = REF_COMPONENT;
                  sref->u.c.component = p;
                  sref->u.c.sym = p->ts.u.derived;
                  sref->next = tmp;
                  *ref = sref;
                }
              return check;
            }
        }
      else if (strcmp (p->name, name) == 0)
        break;

      continue;
    }

  if (p && sym->attr.use_assoc && !noaccess)
    {
      bool is_parent_comp = sym->attr.extension && (p == sym->components);
      if (p->attr.access == ACCESS_PRIVATE ||
	  (p->attr.access != ACCESS_PUBLIC
	   && sym->component_access == ACCESS_PRIVATE
	   && !is_parent_comp))
	{
	  if (!silent)
	    gfc_error ("Component %qs at %C is a PRIVATE component of %qs",
		       name, sym->name);
	  return NULL;
	}
    }

  if (p == NULL
	&& sym->attr.extension
	&& sym->components->ts.type == BT_DERIVED)
    {
      p = gfc_find_component (sym->components->ts.u.derived, name,
			      noaccess, silent, ref);
      /* Do not overwrite the error.  */
      if (p == NULL)
	return p;
    }

  if (p == NULL && !silent)
    {
      const char *guessed = lookup_component_fuzzy (name, sym->components);
      if (guessed)
	gfc_error ("%qs at %C is not a member of the %qs structure"
		   "; did you mean %qs?",
		   name, sym->name, guessed);
      else
	gfc_error ("%qs at %C is not a member of the %qs structure",
		   name, sym->name);
    }

  /* Component was found; build the ultimate component reference. */
  if (p != NULL && ref)
    {
      tmp = gfc_get_ref ();
      tmp->type = REF_COMPONENT;
      tmp->u.c.component = p;
      tmp->u.c.sym = sym;
      /* Link the final component ref to the end of the chain of subrefs. */
      if (sref)
        {
          *ref = sref;
          for (; sref->next; sref = sref->next)
            ;
          sref->next = tmp;
        }
      else
        *ref = tmp;
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
      if (p->kind_expr)
	gfc_free_expr (p->kind_expr);
      if (p->param_list)
	gfc_free_actual_arglist (p->param_list);
      free (p->tb);

      free (p);
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

  gfc_delete_bbt (&label->ns->st_labels, label, compare_st_labels);

  if (label->format != NULL)
    gfc_free_expr (label->format);

  free (label);
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
  free (label);
}


/* Given a label number, search for and return a pointer to the label
   structure, creating it if it does not exist.  */

gfc_st_label *
gfc_get_st_label (int labelno)
{
  gfc_st_label *lp;
  gfc_namespace *ns;

  if (gfc_current_state () == COMP_DERIVED)
    ns = gfc_current_block ()->f2k_derived;
  else
    {
      /* Find the namespace of the scoping unit:
	 If we're in a BLOCK construct, jump to the parent namespace.  */
      ns = gfc_current_ns;
      while (ns->proc_name && ns->proc_name->attr.flavor == FL_LABEL)
	ns = ns->parent;
    }

  /* First see if the label is already in this namespace.  */
  lp = ns->st_labels;
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
  lp->ns = ns;

  gfc_insert_bbt (&ns->st_labels, lp, compare_st_labels);

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
	  if (lp->referenced == ST_LABEL_TARGET
	      || lp->referenced == ST_LABEL_DO_TARGET)
	    gfc_error ("Label %d at %C already referenced as branch target",
		       labelno);
	  else
	    lp->defined = ST_LABEL_FORMAT;

	  break;

	case ST_LABEL_TARGET:
	case ST_LABEL_DO_TARGET:
	  if (lp->referenced == ST_LABEL_FORMAT)
	    gfc_error ("Label %d at %C already referenced as a format label",
		       labelno);
	  else
	    lp->defined = type;

	  if (lp->referenced == ST_LABEL_DO_TARGET && type != ST_LABEL_DO_TARGET
      	      && !gfc_notify_std (GFC_STD_F95_OBS, "DO termination statement "
				  "which is not END DO or CONTINUE with "
				  "label %d at %C", labelno))
	    return;
	  break;

	default:
	  lp->defined = ST_LABEL_BAD_TARGET;
	  lp->referenced = ST_LABEL_BAD_TARGET;
	}
    }
}


/* Reference a label.  Given a label and its type, see if that
   reference is consistent with what is known about that label,
   updating the unknown state.  Returns false if something goes
   wrong.  */

bool
gfc_reference_st_label (gfc_st_label *lp, gfc_sl_type type)
{
  gfc_sl_type label_type;
  int labelno;
  bool rc;

  if (lp == NULL)
    return true;

  labelno = lp->value;

  if (lp->defined != ST_LABEL_UNKNOWN)
    label_type = lp->defined;
  else
    {
      label_type = lp->referenced;
      lp->where = gfc_current_locus;
    }

  if (label_type == ST_LABEL_FORMAT
      && (type == ST_LABEL_TARGET || type == ST_LABEL_DO_TARGET))
    {
      gfc_error ("Label %d at %C previously used as a FORMAT label", labelno);
      rc = false;
      goto done;
    }

  if ((label_type == ST_LABEL_TARGET || label_type == ST_LABEL_DO_TARGET
       || label_type == ST_LABEL_BAD_TARGET)
      && type == ST_LABEL_FORMAT)
    {
      gfc_error ("Label %d at %C previously used as branch target", labelno);
      rc = false;
      goto done;
    }

  if (lp->referenced == ST_LABEL_DO_TARGET && type == ST_LABEL_DO_TARGET
      && !gfc_notify_std (GFC_STD_F95_OBS, "Shared DO termination label %d "
			  "at %C", labelno))
    return false;

  if (lp->referenced != ST_LABEL_DO_TARGET)
    lp->referenced = type;
  rc = true;

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
   symbols are kept in a vector so that we can commit or
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

      if (flag_implicit_none != 0)
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

  if (parent_types && ns->parent != NULL)
    ns->has_implicit_none_export = ns->parent->has_implicit_none_export;

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
  st->name = gfc_get_string ("%s", name);

  gfc_insert_bbt (root, st, compare_symtree);
  return st;
}


/* Delete a symbol from the tree.  Does not free the symbol itself!  */

void
gfc_delete_symtree (gfc_symtree **root, const char *name)
{
  gfc_symtree st, *st0;
  const char *p;

  /* Submodules are marked as mod.submod.  When freeing a submodule
     symbol, the symtree only has "submod", so adjust that here.  */

  p = strrchr(name, '.');
  if (p)
    p++;
  else
    p = name;

  st0 = gfc_find_symtree (*root, p);

  st.name = gfc_get_string ("%s", p);
  gfc_delete_bbt (root, &st, compare_symtree);

  free (st0);
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
  gfc_namespace *ns = gfc_current_ns;

  if (ns->omp_udr_ns)
    ns = ns->parent;
  st = gfc_find_symtree (ns->uop_root, name);
  if (st != NULL)
    return st->n.uop;

  st = gfc_new_symtree (&ns->uop_root, name);

  uop = st->n.uop = XCNEW (gfc_user_op);
  uop->name = gfc_get_string ("%s", name);
  uop->access = ACCESS_UNKNOWN;
  uop->ns = ns;

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


/* Update a symbol's common_block field, and take care of the associated
   memory management.  */

static void
set_symbol_common_block (gfc_symbol *sym, gfc_common_head *common_block)
{
  if (sym->common_block == common_block)
    return;

  if (sym->common_block && sym->common_block->name[0] != '\0')
    {
      sym->common_block->refs--;
      if (sym->common_block->refs == 0)
	free (sym->common_block);
    }
  sym->common_block = common_block;
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

  if (sym->ns != sym->formal_ns)
    gfc_free_namespace (sym->formal_ns);

  if (!sym->attr.generic_copy)
    gfc_free_interface (sym->generic);

  gfc_free_formal_arglist (sym->formal);

  gfc_free_namespace (sym->f2k_derived);

  set_symbol_common_block (sym, NULL);

  if (sym->param_list)
    gfc_free_actual_arglist (sym->param_list);

  free (sym);
}


/* Decrease the reference counter and free memory when we reach zero.  */

void
gfc_release_symbol (gfc_symbol *sym)
{
  if (sym == NULL)
    return;

  if (sym->formal_ns != NULL && sym->refs == 2 && sym->formal_ns != sym->ns
      && (!sym->attr.entry || !sym->module))
    {
      /* As formal_ns contains a reference to sym, delete formal_ns just
	 before the deletion of sym.  */
      gfc_namespace *ns = sym->formal_ns;
      sym->formal_ns = NULL;
      gfc_free_namespace (ns);
    }

  sym->refs--;
  if (sym->refs > 0)
    return;

  gcc_assert (sym->refs == 0);
  gfc_free_symbol (sym);
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

  p->name = gfc_get_string ("%s", name);

  /* Make sure flags for symbol being C bound are clear initially.  */
  p->attr.is_bind_c = 0;
  p->attr.is_iso_c = 0;

  /* Clear the ptrs we may need.  */
  p->common_block = NULL;
  p->f2k_derived = NULL;
  p->assoc = NULL;
  p->fn_result_spec = 0;

  return p;
}


/* Generate an error if a symbol is ambiguous.  */

static void
ambiguous_symbol (const char *name, gfc_symtree *st)
{

  if (st->n.sym->module)
    gfc_error ("Name %qs at %C is an ambiguous reference to %qs "
	       "from module %qs", name, st->n.sym->name, st->n.sym->module);
  else
    gfc_error ("Name %qs at %C is an ambiguous reference to %qs "
	       "from current program unit", name, st->n.sym->name);
}


/* If we're in a SELECT TYPE block, check if the variable 'st' matches any
   selector on the stack. If yes, replace it by the corresponding temporary.  */

static void
select_type_insert_tmp (gfc_symtree **st)
{
  gfc_select_type_stack *stack = select_type_stack;
  for (; stack; stack = stack->prev)
    if ((*st)->n.sym == stack->selector && stack->tmp)
      {
        *st = stack->tmp;
        select_type_insert_tmp (st);
        return;
      }
}


/* Look for a symtree in the current procedure -- that is, go up to
   parent namespaces but only if inside a BLOCK.  Returns NULL if not found.  */

gfc_symtree*
gfc_find_symtree_in_proc (const char* name, gfc_namespace* ns)
{
  while (ns)
    {
      gfc_symtree* st = gfc_find_symtree (ns->sym_root, name);
      if (st)
	return st;

      if (!ns->construct_entities)
	break;
      ns = ns->parent;
    }

  return NULL;
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
	  select_type_insert_tmp (&st);

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

      /* Don't escape an interface block.  */
      if (ns && !ns->has_import_set
          && ns->proc_name && ns->proc_name->attr.if_source == IFSRC_IFBODY)
	break;

      ns = ns->parent;
    }
  while (ns != NULL);

  if (gfc_current_state() == COMP_DERIVED
      && gfc_current_block ()->attr.pdt_template)
    {
      gfc_symbol *der = gfc_current_block ();
      for (; der; der = gfc_get_derived_super_type (der))
	{
	  if (der->f2k_derived && der->f2k_derived->sym_root)
	    {
	      st = gfc_find_symtree (der->f2k_derived->sym_root, name);
	      if (st)
		break;
	    }
	}
      *result = st;
      return 0;
    }

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


/* Tells whether there is only one set of changes in the stack.  */

static bool
single_undo_checkpoint_p (void)
{
  if (latest_undo_chgset == &default_undo_chgset_var)
    {
      gcc_assert (latest_undo_chgset->previous == NULL);
      return true;
    }
  else
    {
      gcc_assert (latest_undo_chgset->previous != NULL);
      return false;
    }
}

/* Save symbol with the information necessary to back it out.  */

void
gfc_save_symbol_data (gfc_symbol *sym)
{
  gfc_symbol *s;
  unsigned i;

  if (!single_undo_checkpoint_p ())
    {
      /* If there is more than one change set, look for the symbol in the
         current one.  If it is found there, we can reuse it.  */
      FOR_EACH_VEC_ELT (latest_undo_chgset->syms, i, s)
	if (s == sym)
	  {
	    gcc_assert (sym->gfc_new || sym->old_symbol != NULL);
	    return;
	  }
    }
  else if (sym->gfc_new || sym->old_symbol != NULL)
    return;

  s = XCNEW (gfc_symbol);
  *s = *sym;
  sym->old_symbol = s;
  sym->gfc_new = 0;

  latest_undo_chgset->syms.safe_push (sym);
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

  if (st == NULL && ns->omp_udr_ns)
    {
      ns = ns->parent;
      st = gfc_find_symtree (ns->sym_root, name);
    }

  if (st == NULL)
    {
      /* If not there, create a new symbol.  */
      p = gfc_new_symbol (name, ns);

      /* Add to the list of tentative symbols.  */
      p->old_symbol = NULL;
      p->mark = 1;
      p->gfc_new = 1;
      latest_undo_chgset->syms.safe_push (p);

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
	  gfc_error ("Symbol %qs at %C has already been host associated",
		     name);
	  return 2;
	}

      p->mark = 1;

      /* Copy in case this symbol is changed.  */
      gfc_save_symbol_data (p);
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
      gfc_save_symbol_data (st->n.sym);
      *result = st;
      return i;
    }

  i = gfc_find_sym_tree (name, gfc_current_ns, 1, &st);
  if (i)
    return i;

  if (st != NULL)
    {
      *result = st;
      return 0;
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


/* Search for the symtree belonging to a gfc_common_head; we cannot use
   head->name as the common_root symtree's name might be mangled.  */

static gfc_symtree *
find_common_symtree (gfc_symtree *st, gfc_common_head *head)
{

  gfc_symtree *result;

  if (st == NULL)
    return NULL;

  if (st->n.common == head)
    return st;

  result = find_common_symtree (st->left, head);
  if (!result)
    result = find_common_symtree (st->right, head);

  return result;
}


/* Clear the given storage, and make it the current change set for registering
   changed symbols.  Its contents are freed after a call to
   gfc_restore_last_undo_checkpoint or gfc_drop_last_undo_checkpoint, but
   it is up to the caller to free the storage itself.  It is usually a local
   variable, so there is nothing to do anyway.  */

void
gfc_new_undo_checkpoint (gfc_undo_change_set &chg_syms)
{
  chg_syms.syms = vNULL;
  chg_syms.tbps = vNULL;
  chg_syms.previous = latest_undo_chgset;
  latest_undo_chgset = &chg_syms;
}


/* Restore previous state of symbol.  Just copy simple stuff.  */

static void
restore_old_symbol (gfc_symbol *p)
{
  gfc_symbol *old;

  p->mark = 0;
  old = p->old_symbol;

  p->ts.type = old->ts.type;
  p->ts.kind = old->ts.kind;

  p->attr = old->attr;

  if (p->value != old->value)
    {
      gcc_checking_assert (old->value == NULL);
      gfc_free_expr (p->value);
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
	  gfc_free_namelist (old->namelist_tail->next);
	  old->namelist_tail->next = NULL;
	}
    }

  p->namelist_tail = old->namelist_tail;

  if (p->formal != old->formal)
    {
      gfc_free_formal_arglist (p->formal);
      p->formal = old->formal;
    }

  set_symbol_common_block (p, old->common_block);
  p->common_head = old->common_head;

  p->old_symbol = old->old_symbol;
  free (old);
}


/* Frees the internal data of a gfc_undo_change_set structure.  Doesn't free
   the structure itself.  */

static void
free_undo_change_set_data (gfc_undo_change_set &cs)
{
  cs.syms.release ();
  cs.tbps.release ();
}


/* Given a change set pointer, free its target's contents and update it with
   the address of the previous change set.  Note that only the contents are
   freed, not the target itself (the contents' container).  It is not a problem
   as the latter will be a local variable usually.  */

static void
pop_undo_change_set (gfc_undo_change_set *&cs)
{
  free_undo_change_set_data (*cs);
  cs = cs->previous;
}


static void free_old_symbol (gfc_symbol *sym);


/* Merges the current change set into the previous one.  The changes themselves
   are left untouched; only one checkpoint is forgotten.  */

void
gfc_drop_last_undo_checkpoint (void)
{
  gfc_symbol *s, *t;
  unsigned i, j;

  FOR_EACH_VEC_ELT (latest_undo_chgset->syms, i, s)
    {
      /* No need to loop in this case.  */
      if (s->old_symbol == NULL)
        continue;

      /* Remove the duplicate symbols.  */
      FOR_EACH_VEC_ELT (latest_undo_chgset->previous->syms, j, t)
	if (t == s)
	  {
	    latest_undo_chgset->previous->syms.unordered_remove (j);

	    /* S->OLD_SYMBOL is the backup symbol for S as it was at the
	       last checkpoint.  We drop that checkpoint, so S->OLD_SYMBOL
	       shall contain from now on the backup symbol for S as it was
	       at the checkpoint before.  */
	    if (s->old_symbol->gfc_new)
	      {
		gcc_assert (s->old_symbol->old_symbol == NULL);
		s->gfc_new = s->old_symbol->gfc_new;
		free_old_symbol (s);
	      }
	    else
	      restore_old_symbol (s->old_symbol);
	    break;
	  }
    }

  latest_undo_chgset->previous->syms.safe_splice (latest_undo_chgset->syms);
  latest_undo_chgset->previous->tbps.safe_splice (latest_undo_chgset->tbps);

  pop_undo_change_set (latest_undo_chgset);
}


/* Undoes all the changes made to symbols since the previous checkpoint.
   This subroutine is made simpler due to the fact that attributes are
   never removed once added.  */

void
gfc_restore_last_undo_checkpoint (void)
{
  gfc_symbol *p;
  unsigned i;

  FOR_EACH_VEC_ELT (latest_undo_chgset->syms, i, p)
    {
      /* Symbol in a common block was new. Or was old and just put in common */
      if (p->common_block
	  && (p->gfc_new || !p->old_symbol->common_block))
	{
	  /* If the symbol was added to any common block, it
	     needs to be removed to stop the resolver looking
	     for a (possibly) dead symbol.  */
	  if (p->common_block->head == p && !p->common_next)
	    {
	      gfc_symtree st, *st0;
	      st0 = find_common_symtree (p->ns->common_root,
					 p->common_block);
	      if (st0)
		{
		  st.name = st0->name;
		  gfc_delete_bbt (&p->ns->common_root, &st, compare_symtree);
		  free (st0);
		}
	    }

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
	  p->common_next = NULL;
	}
      if (p->gfc_new)
	{
	  /* The derived type is saved in the symtree with the first
	     letter capitalized; the all lower-case version to the
	     derived type contains its associated generic function.  */
	  if (gfc_fl_struct (p->attr.flavor))
	    gfc_delete_symtree (&p->ns->sym_root,gfc_dt_upper_string (p->name));
          else
	    gfc_delete_symtree (&p->ns->sym_root, p->name);

	  gfc_release_symbol (p);
	}
      else
	restore_old_symbol (p);
    }

  latest_undo_chgset->syms.truncate (0);
  latest_undo_chgset->tbps.truncate (0);

  if (!single_undo_checkpoint_p ())
    pop_undo_change_set (latest_undo_chgset);
}


/* Makes sure that there is only one set of changes; in other words we haven't
   forgotten to pair a call to gfc_new_checkpoint with a call to either
   gfc_drop_last_undo_checkpoint or gfc_restore_last_undo_checkpoint.  */

static void
enforce_single_undo_checkpoint (void)
{
  gcc_checking_assert (single_undo_checkpoint_p ());
}


/* Undoes all the changes made to symbols in the current statement.  */

void
gfc_undo_symbols (void)
{
  enforce_single_undo_checkpoint ();
  gfc_restore_last_undo_checkpoint ();
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

  free (sym->old_symbol);
  sym->old_symbol = NULL;
}


/* Makes the changes made in the current statement permanent-- gets
   rid of undo information.  */

void
gfc_commit_symbols (void)
{
  gfc_symbol *p;
  gfc_typebound_proc *tbp;
  unsigned i;

  enforce_single_undo_checkpoint ();

  FOR_EACH_VEC_ELT (latest_undo_chgset->syms, i, p)
    {
      p->mark = 0;
      p->gfc_new = 0;
      free_old_symbol (p);
    }
  latest_undo_chgset->syms.truncate (0);

  FOR_EACH_VEC_ELT (latest_undo_chgset->tbps, i, tbp)
    tbp->error = 0;
  latest_undo_chgset->tbps.truncate (0);
}


/* Makes the changes made in one symbol permanent -- gets rid of undo
   information.  */

void
gfc_commit_symbol (gfc_symbol *sym)
{
  gfc_symbol *p;
  unsigned i;

  enforce_single_undo_checkpoint ();

  FOR_EACH_VEC_ELT (latest_undo_chgset->syms, i, p)
    if (p == sym)
      {
	latest_undo_chgset->syms.unordered_remove (i);
	break;
      }

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

  free (t);
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

  free (common_tree);
}


/* Recursive function that deletes an entire tree and all the common
   head structures it points to.  */

static void
free_omp_udr_tree (gfc_symtree * omp_udr_tree)
{
  if (omp_udr_tree == NULL)
    return;

  free_omp_udr_tree (omp_udr_tree->left);
  free_omp_udr_tree (omp_udr_tree->right);

  gfc_free_omp_udr (omp_udr_tree->n.omp_udr);
  free (omp_udr_tree);
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
  free (uop_tree->n.uop);
  free (uop_tree);
}


/* Recursive function that deletes an entire tree and all the symbols
   that it contains.  */

static void
free_sym_tree (gfc_symtree *sym_tree)
{
  if (sym_tree == NULL)
    return;

  free_sym_tree (sym_tree->left);
  free_sym_tree (sym_tree->right);

  gfc_release_symbol (sym_tree->n.sym);
  free (sym_tree);
}


/* Free the derived type list.  */

void
gfc_free_dt_list (void)
{
  gfc_dt_list *dt, *n;

  for (dt = gfc_derived_types; dt; dt = n)
    {
      n = dt->next;
      free (dt);
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
  free (s);
}


/* Free the gfc_equiv_lists.  */

static void
gfc_free_equiv_lists (gfc_equiv_list *l)
{
  if (l == NULL)
    return;
  gfc_free_equiv_lists (l->next);
  gfc_free_equiv_infos (l->equiv);
  free (l);
}


/* Free a finalizer procedure list.  */

void
gfc_free_finalizer (gfc_finalizer* el)
{
  if (el)
    {
      gfc_release_symbol (el->proc_sym);
      free (el);
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


/* Create a new gfc_charlen structure and add it to a namespace.
   If 'old_cl' is given, the newly created charlen will be a copy of it.  */

gfc_charlen*
gfc_new_charlen (gfc_namespace *ns, gfc_charlen *old_cl)
{
  gfc_charlen *cl;

  cl = gfc_get_charlen ();

  /* Copy old_cl.  */
  if (old_cl)
    {
      cl->length = gfc_copy_expr (old_cl->length);
      cl->length_from_typespec = old_cl->length_from_typespec;
      cl->backend_decl = old_cl->backend_decl;
      cl->passed_length = old_cl->passed_length;
      cl->resolved = old_cl->resolved;
    }

  /* Put into namespace.  */
  cl->next = ns->cl_list;
  ns->cl_list = cl;

  return cl;
}


/* Free the charlen list from cl to end (end is not freed).
   Free the whole list if end is NULL.  */

void
gfc_free_charlen (gfc_charlen *cl, gfc_charlen *end)
{
  gfc_charlen *cl2;

  for (; cl != end; cl = cl2)
    {
      gcc_assert (cl);

      cl2 = cl->next;
      gfc_free_expr (cl->length);
      free (cl);
    }
}


/* Free entry list structs.  */

static void
free_entry_list (gfc_entry_list *el)
{
  gfc_entry_list *next;

  if (el == NULL)
    return;

  next = el->next;
  free (el);
  free_entry_list (next);
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
  free_omp_udr_tree (ns->omp_udr_root);
  free_tb_tree (ns->tb_sym_root);
  free_tb_tree (ns->tb_uop_root);
  gfc_free_finalizer_list (ns->finalizers);
  gfc_free_omp_declare_simd_list (ns->omp_declare_simd);
  gfc_free_charlen (ns->cl_list, NULL);
  free_st_labels (ns->st_labels);

  free_entry_list (ns->entries);
  gfc_free_equiv (ns->equiv);
  gfc_free_equiv_lists (ns->equiv_lists);
  gfc_free_use_stmts (ns->use_stmts);

  for (i = GFC_INTRINSIC_BEGIN; i != GFC_INTRINSIC_END; i++)
    gfc_free_interface (ns->op[i]);

  gfc_free_data (ns->data);
  p = ns->contained;
  free (ns);

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

  enforce_single_undo_checkpoint ();
  free_undo_change_set_data (*latest_undo_chgset);
}


/* Count how many nodes a symtree has.  */

static unsigned
count_st_nodes (const gfc_symtree *st)
{
  unsigned nodes;
  if (!st)
    return 0;

  nodes = count_st_nodes (st->left);
  nodes++;
  nodes += count_st_nodes (st->right);

  return nodes;
}


/* Convert symtree tree into symtree vector.  */

static unsigned
fill_st_vector (gfc_symtree *st, gfc_symtree **st_vec, unsigned node_cntr)
{
  if (!st)
    return node_cntr;

  node_cntr = fill_st_vector (st->left, st_vec, node_cntr);
  st_vec[node_cntr++] = st;
  node_cntr = fill_st_vector (st->right, st_vec, node_cntr);

  return node_cntr;
}


/* Traverse namespace.  As the functions might modify the symtree, we store the
   symtree as a vector and operate on this vector.  Note: We assume that
   sym_func or st_func never deletes nodes from the symtree - only adding is
   allowed. Additionally, newly added nodes are not traversed.  */

static void
do_traverse_symtree (gfc_symtree *st, void (*st_func) (gfc_symtree *),
		     void (*sym_func) (gfc_symbol *))
{
  gfc_symtree **st_vec;
  unsigned nodes, i, node_cntr;

  gcc_assert ((st_func && !sym_func) || (!st_func && sym_func));
  nodes = count_st_nodes (st);
  st_vec = XALLOCAVEC (gfc_symtree *, nodes);
  node_cntr = 0;
  fill_st_vector (st, st_vec, node_cntr);

  if (sym_func)
    {
      /* Clear marks.  */
      for (i = 0; i < nodes; i++)
	st_vec[i]->n.sym->mark = 0;
      for (i = 0; i < nodes; i++)
	if (!st_vec[i]->n.sym->mark)
	  {
	    (*sym_func) (st_vec[i]->n.sym);
	    st_vec[i]->n.sym->mark = 1;
	  }
     }
   else
      for (i = 0; i < nodes; i++)
	(*st_func) (st_vec[i]);
}


/* Recursively traverse the symtree nodes.  */

void
gfc_traverse_symtree (gfc_symtree *st, void (*st_func) (gfc_symtree *))
{
  do_traverse_symtree (st, st_func, NULL);
}


/* Call a given function for all symbols in the namespace.  We take
   care that each gfc_symbol node is called exactly once.  */

void
gfc_traverse_ns (gfc_namespace *ns, void (*sym_func) (gfc_symbol *))
{
  do_traverse_symtree (ns->sym_root, NULL, sym_func);
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
      && sym->ts.u.cl
      && !gfc_is_constant_expr (sym->ts.u.cl->length))
    return true;
  /* Variables with explicit AUTOMATIC attribute.  */
  if (sym->attr.automatic)
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
  gfc_add_save (&sym->attr, SAVE_EXPLICIT, sym->name, &sym->declared_at);
}


/* Mark those symbols which can be SAVEd as such.  */

void
gfc_save_all (gfc_namespace *ns)
{
  gfc_traverse_ns (ns, save_symbol);
}


/* Make sure that no changes to symbols are pending.  */

void
gfc_enforce_clean_symbol_state(void)
{
  enforce_single_undo_checkpoint ();
  gcc_assert (latest_undo_chgset->syms.is_empty ());
}


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
  s->name = gfc_get_string ("%s", name);

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

bool
verify_bind_c_derived_type (gfc_symbol *derived_sym)
{
  gfc_component *curr_comp = NULL;
  bool is_c_interop = false;
  bool retval = true;

  if (derived_sym == NULL)
    gfc_internal_error ("verify_bind_c_derived_type(): Given symbol is "
                        "unexpectedly NULL");

  /* If we've already looked at this derived symbol, do not look at it again
     so we don't repeat warnings/errors.  */
  if (derived_sym->ts.is_c_interop)
    return true;

  /* The derived type must have the BIND attribute to be interoperable
     J3/04-007, Section 15.2.3.  */
  if (derived_sym->attr.is_bind_c != 1)
    {
      derived_sym->ts.is_c_interop = 0;
      gfc_error_now ("Derived type %qs declared at %L must have the BIND "
                     "attribute to be C interoperable", derived_sym->name,
                     &(derived_sym->declared_at));
      retval = false;
    }

  curr_comp = derived_sym->components;

  /* Fortran 2003 allows an empty derived type.  C99 appears to disallow an
     empty struct.  Section 15.2 in Fortran 2003 states:  "The following
     subclauses define the conditions under which a Fortran entity is
     interoperable.  If a Fortran entity is interoperable, an equivalent
     entity may be defined by means of C and the Fortran entity is said
     to be interoperable with the C entity.  There does not have to be such
     an interoperating C entity."
  */
  if (curr_comp == NULL)
    {
      gfc_warning (0, "Derived type %qs with BIND(C) attribute at %L is empty, "
		   "and may be inaccessible by the C companion processor",
		   derived_sym->name, &(derived_sym->declared_at));
      derived_sym->ts.is_c_interop = 1;
      derived_sym->attr.is_bind_c = 1;
      return true;
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
          gfc_error ("Component %qs at %L cannot have the "
                     "POINTER attribute because it is a member "
                     "of the BIND(C) derived type %qs at %L",
                     curr_comp->name, &(curr_comp->loc),
                     derived_sym->name, &(derived_sym->declared_at));
          retval = false;
        }

      if (curr_comp->attr.proc_pointer != 0)
	{
	  gfc_error ("Procedure pointer component %qs at %L cannot be a member"
		     " of the BIND(C) derived type %qs at %L", curr_comp->name,
		     &curr_comp->loc, derived_sym->name,
		     &derived_sym->declared_at);
          retval = false;
        }

      /* The components cannot be allocatable.
         J3/04-007, Section 15.2.3, C1505.	*/
      if (curr_comp->attr.allocatable != 0)
        {
          gfc_error ("Component %qs at %L cannot have the "
                     "ALLOCATABLE attribute because it is a member "
                     "of the BIND(C) derived type %qs at %L",
                     curr_comp->name, &(curr_comp->loc),
                     derived_sym->name, &(derived_sym->declared_at));
          retval = false;
        }

      /* BIND(C) derived types must have interoperable components.  */
      if (curr_comp->ts.type == BT_DERIVED
	  && curr_comp->ts.u.derived->ts.is_iso_c != 1
          && curr_comp->ts.u.derived != derived_sym)
        {
          /* This should be allowed; the draft says a derived-type can not
             have type parameters if it is has the BIND attribute.  Type
             parameters seem to be for making parameterized derived types.
             There's no need to verify the type if it is c_ptr/c_funptr.  */
          retval = verify_bind_c_derived_type (curr_comp->ts.u.derived);
	}
      else
	{
	  /* Grab the typespec for the given component and test the kind.  */
	  is_c_interop = gfc_verify_c_interop (&(curr_comp->ts));

	  if (!is_c_interop)
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
	      if (derived_sym->attr.is_bind_c == 1 && warn_c_binding_type)
		/* If the derived type is bind(c), all fields must be
		   interop.  */
		gfc_warning (OPT_Wc_binding_type,
			     "Component %qs in derived type %qs at %L "
                             "may not be C interoperable, even though "
                             "derived type %qs is BIND(C)",
                             curr_comp->name, derived_sym->name,
                             &(curr_comp->loc), derived_sym->name);
	      else if (warn_c_binding_type)
		/* If derived type is param to bind(c) routine, or to one
		   of the iso_c_binding procs, it must be interoperable, so
		   all fields must interop too.	 */
		gfc_warning (OPT_Wc_binding_type,
			     "Component %qs in derived type %qs at %L "
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
      gfc_error ("Derived type %qs at %L cannot be declared with both "
                 "PRIVATE and BIND(C) attributes", derived_sym->name,
                 &(derived_sym->declared_at));
      retval = false;
    }

  if (derived_sym->attr.sequence != 0)
    {
      gfc_error ("Derived type %qs at %L cannot have the SEQUENCE "
                 "attribute because it is BIND(C)", derived_sym->name,
                 &(derived_sym->declared_at));
      retval = false;
    }

  /* Mark the derived type as not being C interoperable if we found an
     error.  If there were only warnings, proceed with the assumption
     it's interoperable.  */
  if (!retval)
    derived_sym->ts.is_c_interop = 0;

  return retval;
}


/* Generate symbols for the named constants c_null_ptr and c_null_funptr.  */

static bool
gen_special_c_interop_ptr (gfc_symbol *tmp_sym, gfc_symtree *dt_symtree)
{
  gfc_constructor *c;

  gcc_assert (tmp_sym && dt_symtree && dt_symtree->n.sym);
  dt_symtree->n.sym->attr.referenced = 1;

  tmp_sym->attr.is_c_interop = 1;
  tmp_sym->attr.is_bind_c = 1;
  tmp_sym->ts.is_c_interop = 1;
  tmp_sym->ts.is_iso_c = 1;
  tmp_sym->ts.type = BT_DERIVED;
  tmp_sym->ts.f90_type = BT_VOID;
  tmp_sym->attr.flavor = FL_PARAMETER;
  tmp_sym->ts.u.derived = dt_symtree->n.sym;

  /* Set the c_address field of c_null_ptr and c_null_funptr to
     the value of NULL.	 */
  tmp_sym->value = gfc_get_expr ();
  tmp_sym->value->expr_type = EXPR_STRUCTURE;
  tmp_sym->value->ts.type = BT_DERIVED;
  tmp_sym->value->ts.f90_type = BT_VOID;
  tmp_sym->value->ts.u.derived = tmp_sym->ts.u.derived;
  gfc_constructor_append_expr (&tmp_sym->value->value.constructor, NULL, NULL);
  c = gfc_constructor_first (tmp_sym->value->value.constructor);
  c->expr = gfc_get_int_expr (gfc_index_integer_kind, NULL, 0);
  c->expr->ts.is_iso_c = 1;

  return true;
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


/* Add a procedure interface to the given symbol (i.e., store a
   reference to the list of formal arguments).  */

static void
add_proc_interface (gfc_symbol *sym, ifsrc source, gfc_formal_arglist *formal)
{

  sym->formal = formal;
  sym->attr.if_source = source;
}


/* Copy the formal args from an existing symbol, src, into a new
   symbol, dest.  New formal args are created, and the description of
   each arg is set according to the existing ones.  This function is
   used when creating procedure declaration variables from a procedure
   declaration statement (see match_proc_decl()) to create the formal
   args based on the args of a given named interface.

   When an actual argument list is provided, skip the absent arguments.
   To be used together with gfc_se->ignore_optional.  */

void
gfc_copy_formal_args_intr (gfc_symbol *dest, gfc_intrinsic_sym *src,
			   gfc_actual_arglist *actual)
{
  gfc_formal_arglist *head = NULL;
  gfc_formal_arglist *tail = NULL;
  gfc_formal_arglist *formal_arg = NULL;
  gfc_intrinsic_arg *curr_arg = NULL;
  gfc_formal_arglist *formal_prev = NULL;
  gfc_actual_arglist *act_arg = actual;
  /* Save current namespace so we can change it for formal args.  */
  gfc_namespace *parent_ns = gfc_current_ns;

  /* Create a new namespace, which will be the formal ns (namespace
     of the formal args).  */
  gfc_current_ns = gfc_get_namespace (parent_ns, 0);
  gfc_current_ns->proc_name = dest;

  for (curr_arg = src->formal; curr_arg; curr_arg = curr_arg->next)
    {
      /* Skip absent arguments.  */
      if (actual)
	{
	  gcc_assert (act_arg != NULL);
	  if (act_arg->expr == NULL)
	    {
	      act_arg = act_arg->next;
	      continue;
	    }
	  act_arg = act_arg->next;
	}
      formal_arg = gfc_get_formal_arglist ();
      gfc_get_symbol (curr_arg->name, gfc_current_ns, &(formal_arg->sym));

      /* May need to copy more info for the symbol.  */
      formal_arg->sym->ts = curr_arg->ts;
      formal_arg->sym->attr.optional = curr_arg->optional;
      formal_arg->sym->attr.value = curr_arg->value;
      formal_arg->sym->attr.intent = curr_arg->intent;
      formal_arg->sym->attr.flavor = FL_VARIABLE;
      formal_arg->sym->attr.dummy = 1;

      if (formal_arg->sym->ts.type == BT_CHARACTER)
	formal_arg->sym->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);

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

      /* Validate changes.  */
      gfc_commit_symbol (formal_arg->sym);
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

#define NAMED_FUNCTION(a,b,c,d) \
      case a:\
        return d;
#define NAMED_SUBROUTINE(a,b,c,d) \
      case a:\
        return d;
#include "iso-c-binding.def"
#undef NAMED_FUNCTION
#undef NAMED_SUBROUTINE

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
   end of the list. For C_null_(fun)ptr, dt_symtree has to be set and
   point to the symtree for c_(fun)ptr.  */

gfc_symtree *
generate_isocbinding_symbol (const char *mod_name, iso_c_binding_symbol s,
			     const char *local_name, gfc_symtree *dt_symtree,
			     bool hidden)
{
  const char *const name = (local_name && local_name[0])
			   ? local_name : c_interop_kinds_table[s].name;
  gfc_symtree *tmp_symtree;
  gfc_symbol *tmp_sym = NULL;
  int index;

  if (gfc_notification_std (std_for_isocbinding_symbol (s)) == ERROR)
    return NULL;

  tmp_symtree = gfc_find_symtree (gfc_current_ns->sym_root, name);
  if (hidden
      && (!tmp_symtree || !tmp_symtree->n.sym
	  || tmp_symtree->n.sym->from_intmod != INTMOD_ISO_C_BINDING
	  || tmp_symtree->n.sym->intmod_sym_id != s))
    tmp_symtree = NULL;

  /* Already exists in this scope so don't re-add it.  */
  if (tmp_symtree != NULL && (tmp_sym = tmp_symtree->n.sym) != NULL
      && (!tmp_sym->attr.generic
	  || (tmp_sym = gfc_find_dt_in_generic (tmp_sym)) != NULL)
      && tmp_sym->from_intmod == INTMOD_ISO_C_BINDING)
    {
      if (tmp_sym->attr.flavor == FL_DERIVED
	  && !get_iso_c_binding_dt (tmp_sym->intmod_sym_id))
	{
	  gfc_dt_list *dt_list;
	  dt_list = gfc_get_dt_list ();
	  dt_list->derived = tmp_sym;
	  dt_list->next = gfc_derived_types;
  	  gfc_derived_types = dt_list;
        }

      return tmp_symtree;
    }

  /* Create the sym tree in the current ns.  */
  if (hidden)
    {
      tmp_symtree = gfc_get_unique_symtree (gfc_current_ns);
      tmp_sym = gfc_new_symbol (name, gfc_current_ns);

      /* Add to the list of tentative symbols.  */
      latest_undo_chgset->syms.safe_push (tmp_sym);
      tmp_sym->old_symbol = NULL;
      tmp_sym->mark = 1;
      tmp_sym->gfc_new = 1;

      tmp_symtree->n.sym = tmp_sym;
      tmp_sym->refs++;
    }
  else
    {
      gfc_get_sym_tree (name, gfc_current_ns, &tmp_symtree, false);
      gcc_assert (tmp_symtree);
      tmp_sym = tmp_symtree->n.sym;
    }

  /* Say what module this symbol belongs to.  */
  tmp_sym->module = gfc_get_string ("%s", mod_name);
  tmp_sym->from_intmod = INTMOD_ISO_C_BINDING;
  tmp_sym->intmod_sym_id = s;
  tmp_sym->attr.is_iso_c = 1;
  tmp_sym->attr.use_assoc = 1;

  gcc_assert (dt_symtree == NULL || s == ISOCBINDING_NULL_FUNPTR
	      || s == ISOCBINDING_NULL_PTR);

  switch (s)
    {

#define NAMED_INTCST(a,b,c,d) case a :
#define NAMED_REALCST(a,b,c,d) case a :
#define NAMED_CMPXCST(a,b,c,d) case a :
#define NAMED_LOGCST(a,b,c) case a :
#define NAMED_CHARKNDCST(a,b,c) case a :
#include "iso-c-binding.def"

	tmp_sym->value = gfc_get_int_expr (gfc_default_integer_kind, NULL,
				 	   c_interop_kinds_table[s].value);

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

	break;


#define NAMED_CHARCST(a,b,c) case a :
#include "iso-c-binding.def"

	/* Initialize an integer constant expression node for the
	   length of the character.  */
	tmp_sym->value = gfc_get_character_expr (gfc_default_character_kind,
						 &gfc_current_locus, NULL, 1);
	tmp_sym->value->ts.is_c_interop = 1;
	tmp_sym->value->ts.is_iso_c = 1;
	tmp_sym->value->value.character.length = 1;
	tmp_sym->value->value.character.string[0]
	  = (gfc_char_t) c_interop_kinds_table[s].value;
	tmp_sym->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
	tmp_sym->ts.u.cl->length = gfc_get_int_expr (gfc_default_integer_kind,
						     NULL, 1);

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

	break;

      case ISOCBINDING_PTR:
      case ISOCBINDING_FUNPTR:
	{
	  gfc_symbol *dt_sym;
	  gfc_dt_list **dt_list_ptr = NULL;
	  gfc_component *tmp_comp = NULL;

	  /* Generate real derived type.  */
	  if (hidden)
	    dt_sym = tmp_sym;
	  else
	    {
	      const char *hidden_name;
	      gfc_interface *intr, *head;

	      hidden_name = gfc_dt_upper_string (tmp_sym->name);
	      tmp_symtree = gfc_find_symtree (gfc_current_ns->sym_root,
					      hidden_name);
	      gcc_assert (tmp_symtree == NULL);
	      gfc_get_sym_tree (hidden_name, gfc_current_ns, &tmp_symtree, false);
	      dt_sym = tmp_symtree->n.sym;
	      dt_sym->name = gfc_get_string (s == ISOCBINDING_PTR
					     ? "c_ptr" : "c_funptr");

	      /* Generate an artificial generic function.  */
	      head = tmp_sym->generic;
	      intr = gfc_get_interface ();
	      intr->sym = dt_sym;
	      intr->where = gfc_current_locus;
	      intr->next = head;
	      tmp_sym->generic = intr;

	      if (!tmp_sym->attr.generic
		  && !gfc_add_generic (&tmp_sym->attr, tmp_sym->name, NULL))
		return NULL;

	      if (!tmp_sym->attr.function
		  && !gfc_add_function (&tmp_sym->attr, tmp_sym->name, NULL))
		return NULL;
	    }

	  /* Say what module this symbol belongs to.  */
	  dt_sym->module = gfc_get_string ("%s", mod_name);
	  dt_sym->from_intmod = INTMOD_ISO_C_BINDING;
	  dt_sym->intmod_sym_id = s;
          dt_sym->attr.use_assoc = 1;

	  /* Initialize an integer constant expression node.  */
	  dt_sym->attr.flavor = FL_DERIVED;
	  dt_sym->ts.is_c_interop = 1;
	  dt_sym->attr.is_c_interop = 1;
	  dt_sym->attr.private_comp = 1;
	  dt_sym->component_access = ACCESS_PRIVATE;
	  dt_sym->ts.is_iso_c = 1;
	  dt_sym->ts.type = BT_DERIVED;
	  dt_sym->ts.f90_type = BT_VOID;

	  /* A derived type must have the bind attribute to be
	     interoperable (J3/04-007, Section 15.2.3), even though
	     the binding label is not used.  */
	  dt_sym->attr.is_bind_c = 1;

	  dt_sym->attr.referenced = 1;
	  dt_sym->ts.u.derived = dt_sym;

	  /* Add the symbol created for the derived type to the current ns.  */
	  dt_list_ptr = &(gfc_derived_types);
	  while (*dt_list_ptr != NULL && (*dt_list_ptr)->next != NULL)
	    dt_list_ptr = &((*dt_list_ptr)->next);

	  /* There is already at least one derived type in the list, so append
	     the one we're currently building for c_ptr or c_funptr.  */
	  if (*dt_list_ptr != NULL)
	    dt_list_ptr = &((*dt_list_ptr)->next);
	  (*dt_list_ptr) = gfc_get_dt_list ();
	  (*dt_list_ptr)->derived = dt_sym;
	  (*dt_list_ptr)->next = NULL;

	  gfc_add_component (dt_sym, "c_address", &tmp_comp);
	  if (tmp_comp == NULL)
	    gcc_unreachable ();

	  tmp_comp->ts.type = BT_INTEGER;

	  /* Set this because the module will need to read/write this field.  */
	  tmp_comp->ts.f90_type = BT_INTEGER;

	  /* The kinds for c_ptr and c_funptr are the same.  */
	  index = get_c_kind ("c_ptr", c_interop_kinds_table);
	  tmp_comp->ts.kind = c_interop_kinds_table[index].value;
	  tmp_comp->attr.access = ACCESS_PRIVATE;

	  /* Mark the component as C interoperable.  */
	  tmp_comp->ts.is_c_interop = 1;
	}

	break;

      case ISOCBINDING_NULL_PTR:
      case ISOCBINDING_NULL_FUNPTR:
        gen_special_c_interop_ptr (tmp_sym, dt_symtree);
        break;

      default:
	gcc_unreachable ();
    }
  gfc_commit_symbol (tmp_sym);
  return tmp_symtree;
}


/* Check that a symbol is already typed.  If strict is not set, an untyped
   symbol is acceptable for non-standard-conforming mode.  */

bool
gfc_check_symbol_typed (gfc_symbol* sym, gfc_namespace* ns,
			bool strict, locus where)
{
  gcc_assert (sym);

  if (gfc_matching_prefix)
    return true;

  /* Check for the type and try to give it an implicit one.  */
  if (sym->ts.type == BT_UNKNOWN
      && !gfc_set_default_type (sym, 0, ns))
    {
      if (strict)
	{
	  gfc_error ("Symbol %qs is used before it is typed at %L",
		     sym->name, &where);
	  return false;
	}

      if (!gfc_notify_std (GFC_STD_GNU, "Symbol %qs is used before"
			   " it is typed at %L", sym->name, &where))
	return false;
    }

  /* Everything is ok.  */
  return true;
}


/* Construct a typebound-procedure structure.  Those are stored in a tentative
   list and marked `error' until symbols are committed.  */

gfc_typebound_proc*
gfc_get_typebound_proc (gfc_typebound_proc *tb0)
{
  gfc_typebound_proc *result;

  result = XCNEW (gfc_typebound_proc);
  if (tb0)
    *result = *tb0;
  result->error = 1;

  latest_undo_chgset->tbps.safe_push (result);

  return result;
}


/* Get the super-type of a given derived type.  */

gfc_symbol*
gfc_get_derived_super_type (gfc_symbol* derived)
{
  gcc_assert (derived);

  if (derived->attr.generic)
    derived = gfc_find_dt_in_generic (derived);

  if (!derived->attr.extension)
    return NULL;

  gcc_assert (derived->components);
  gcc_assert (derived->components->ts.type == BT_DERIVED);
  gcc_assert (derived->components->ts.u.derived);

  if (derived->components->ts.u.derived->attr.generic)
    return gfc_find_dt_in_generic (derived->components->ts.u.derived);

  return derived->components->ts.u.derived;
}


/* Get the ultimate super-type of a given derived type.  */

gfc_symbol*
gfc_get_ultimate_derived_super_type (gfc_symbol* derived)
{
  if (!derived->attr.extension)
    return NULL;

  derived = gfc_get_derived_super_type (derived);

  if (derived->attr.extension)
    return gfc_get_ultimate_derived_super_type (derived);
  else
    return derived;
}


/* Check if a derived type t2 is an extension of (or equal to) a type t1.  */

bool
gfc_type_is_extension_of (gfc_symbol *t1, gfc_symbol *t2)
{
  while (!gfc_compare_derived_types (t1, t2) && t2->attr.extension)
    t2 = gfc_get_derived_super_type (t2);
  return gfc_compare_derived_types (t1, t2);
}


/* Check if two typespecs are type compatible (F03:5.1.1.2):
   If ts1 is nonpolymorphic, ts2 must be the same type.
   If ts1 is polymorphic (CLASS), ts2 must be an extension of ts1.  */

bool
gfc_type_compatible (gfc_typespec *ts1, gfc_typespec *ts2)
{
  bool is_class1 = (ts1->type == BT_CLASS);
  bool is_class2 = (ts2->type == BT_CLASS);
  bool is_derived1 = (ts1->type == BT_DERIVED);
  bool is_derived2 = (ts2->type == BT_DERIVED);
  bool is_union1 = (ts1->type == BT_UNION);
  bool is_union2 = (ts2->type == BT_UNION);

  if (is_class1
      && ts1->u.derived->components
      && ((ts1->u.derived->attr.is_class
	   && ts1->u.derived->components->ts.u.derived->attr
							.unlimited_polymorphic)
	  || ts1->u.derived->attr.unlimited_polymorphic))
    return 1;

  if (!is_derived1 && !is_derived2 && !is_class1 && !is_class2
      && !is_union1 && !is_union2)
    return (ts1->type == ts2->type);

  if ((is_derived1 && is_derived2) || (is_union1 && is_union2))
    return gfc_compare_derived_types (ts1->u.derived, ts2->u.derived);

  if (is_derived1 && is_class2)
    return gfc_compare_derived_types (ts1->u.derived,
				      ts2->u.derived->attr.is_class ?
				      ts2->u.derived->components->ts.u.derived
				      : ts2->u.derived);
  if (is_class1 && is_derived2)
    return gfc_type_is_extension_of (ts1->u.derived->attr.is_class ?
				       ts1->u.derived->components->ts.u.derived
				     : ts1->u.derived,
				     ts2->u.derived);
  else if (is_class1 && is_class2)
    return gfc_type_is_extension_of (ts1->u.derived->attr.is_class ?
				       ts1->u.derived->components->ts.u.derived
				     : ts1->u.derived,
				     ts2->u.derived->attr.is_class ?
				       ts2->u.derived->components->ts.u.derived
				     : ts2->u.derived);
  else
    return 0;
}


/* Find the parent-namespace of the current function.  If we're inside
   BLOCK constructs, it may not be the current one.  */

gfc_namespace*
gfc_find_proc_namespace (gfc_namespace* ns)
{
  while (ns->construct_entities)
    {
      ns = ns->parent;
      gcc_assert (ns);
    }

  return ns;
}


/* Check if an associate-variable should be translated as an `implicit' pointer
   internally (if it is associated to a variable and not an array with
   descriptor).  */

bool
gfc_is_associate_pointer (gfc_symbol* sym)
{
  if (!sym->assoc)
    return false;

  if (sym->ts.type == BT_CLASS)
    return true;

  if (sym->ts.type == BT_CHARACTER
      && sym->ts.deferred
      && sym->assoc->target
      && sym->assoc->target->expr_type == EXPR_FUNCTION)
    return true;

  if (!sym->assoc->variable)
    return false;

  if (sym->attr.dimension && sym->as->type != AS_EXPLICIT)
    return false;

  return true;
}


gfc_symbol *
gfc_find_dt_in_generic (gfc_symbol *sym)
{
  gfc_interface *intr = NULL;

  if (!sym || gfc_fl_struct (sym->attr.flavor))
    return sym;

  if (sym->attr.generic)
    for (intr = sym->generic; intr; intr = intr->next)
      if (gfc_fl_struct (intr->sym->attr.flavor))
        break;
  return intr ? intr->sym : NULL;
}


/* Get the dummy arguments from a procedure symbol. If it has been declared
   via a PROCEDURE statement with a named interface, ts.interface will be set
   and the arguments need to be taken from there.  */

gfc_formal_arglist *
gfc_sym_get_dummy_args (gfc_symbol *sym)
{
  gfc_formal_arglist *dummies;

  dummies = sym->formal;
  if (dummies == NULL && sym->ts.interface != NULL)
    dummies = sym->ts.interface->formal;

  return dummies;
}
