/* Perform type resolution on the various structures.
   Copyright (C) 2001-2017 Free Software Foundation, Inc.
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
#include "bitmap.h"
#include "gfortran.h"
#include "arith.h"  /* For gfc_compare_expr().  */
#include "dependency.h"
#include "data.h"
#include "target-memory.h" /* for gfc_simplify_transfer */
#include "constructor.h"

/* Types used in equivalence statements.  */

enum seq_type
{
  SEQ_NONDEFAULT, SEQ_NUMERIC, SEQ_CHARACTER, SEQ_MIXED
};

/* Stack to keep track of the nesting of blocks as we move through the
   code.  See resolve_branch() and gfc_resolve_code().  */

typedef struct code_stack
{
  struct gfc_code *head, *current;
  struct code_stack *prev;

  /* This bitmap keeps track of the targets valid for a branch from
     inside this block except for END {IF|SELECT}s of enclosing
     blocks.  */
  bitmap reachable_labels;
}
code_stack;

static code_stack *cs_base = NULL;


/* Nonzero if we're inside a FORALL or DO CONCURRENT block.  */

static int forall_flag;
int gfc_do_concurrent_flag;

/* True when we are resolving an expression that is an actual argument to
   a procedure.  */
static bool actual_arg = false;
/* True when we are resolving an expression that is the first actual argument
   to a procedure.  */
static bool first_actual_arg = false;


/* Nonzero if we're inside a OpenMP WORKSHARE or PARALLEL WORKSHARE block.  */

static int omp_workshare_flag;

/* True if we are processing a formal arglist. The corresponding function
   resets the flag each time that it is read.  */
static bool formal_arg_flag = false;

/* True if we are resolving a specification expression.  */
static bool specification_expr = false;

/* The id of the last entry seen.  */
static int current_entry_id;

/* We use bitmaps to determine if a branch target is valid.  */
static bitmap_obstack labels_obstack;

/* True when simplifying a EXPR_VARIABLE argument to an inquiry function.  */
static bool inquiry_argument = false;


bool
gfc_is_formal_arg (void)
{
  return formal_arg_flag;
}

/* Is the symbol host associated?  */
static bool
is_sym_host_assoc (gfc_symbol *sym, gfc_namespace *ns)
{
  for (ns = ns->parent; ns; ns = ns->parent)
    {
      if (sym->ns == ns)
	return true;
    }

  return false;
}

/* Ensure a typespec used is valid; for instance, TYPE(t) is invalid if t is
   an ABSTRACT derived-type.  If where is not NULL, an error message with that
   locus is printed, optionally using name.  */

static bool
resolve_typespec_used (gfc_typespec* ts, locus* where, const char* name)
{
  if (ts->type == BT_DERIVED && ts->u.derived->attr.abstract)
    {
      if (where)
	{
	  if (name)
	    gfc_error ("%qs at %L is of the ABSTRACT type %qs",
		       name, where, ts->u.derived->name);
	  else
	    gfc_error ("ABSTRACT type %qs used at %L",
		       ts->u.derived->name, where);
	}

      return false;
    }

  return true;
}


static bool
check_proc_interface (gfc_symbol *ifc, locus *where)
{
  /* Several checks for F08:C1216.  */
  if (ifc->attr.procedure)
    {
      gfc_error ("Interface %qs at %L is declared "
		 "in a later PROCEDURE statement", ifc->name, where);
      return false;
    }
  if (ifc->generic)
    {
      /* For generic interfaces, check if there is
	 a specific procedure with the same name.  */
      gfc_interface *gen = ifc->generic;
      while (gen && strcmp (gen->sym->name, ifc->name) != 0)
	gen = gen->next;
      if (!gen)
	{
	  gfc_error ("Interface %qs at %L may not be generic",
		     ifc->name, where);
	  return false;
	}
    }
  if (ifc->attr.proc == PROC_ST_FUNCTION)
    {
      gfc_error ("Interface %qs at %L may not be a statement function",
		 ifc->name, where);
      return false;
    }
  if (gfc_is_intrinsic (ifc, 0, ifc->declared_at)
      || gfc_is_intrinsic (ifc, 1, ifc->declared_at))
    ifc->attr.intrinsic = 1;
  if (ifc->attr.intrinsic && !gfc_intrinsic_actual_ok (ifc->name, 0))
    {
      gfc_error ("Intrinsic procedure %qs not allowed in "
		 "PROCEDURE statement at %L", ifc->name, where);
      return false;
    }
  if (!ifc->attr.if_source && !ifc->attr.intrinsic && ifc->name[0] != '\0')
    {
      gfc_error ("Interface %qs at %L must be explicit", ifc->name, where);
      return false;
    }
  return true;
}


static void resolve_symbol (gfc_symbol *sym);


/* Resolve the interface for a PROCEDURE declaration or procedure pointer.  */

static bool
resolve_procedure_interface (gfc_symbol *sym)
{
  gfc_symbol *ifc = sym->ts.interface;

  if (!ifc)
    return true;

  if (ifc == sym)
    {
      gfc_error ("PROCEDURE %qs at %L may not be used as its own interface",
		 sym->name, &sym->declared_at);
      return false;
    }
  if (!check_proc_interface (ifc, &sym->declared_at))
    return false;

  if (ifc->attr.if_source || ifc->attr.intrinsic)
    {
      /* Resolve interface and copy attributes.  */
      resolve_symbol (ifc);
      if (ifc->attr.intrinsic)
	gfc_resolve_intrinsic (ifc, &ifc->declared_at);

      if (ifc->result)
	{
	  sym->ts = ifc->result->ts;
	  sym->attr.allocatable = ifc->result->attr.allocatable;
	  sym->attr.pointer = ifc->result->attr.pointer;
	  sym->attr.dimension = ifc->result->attr.dimension;
	  sym->attr.class_ok = ifc->result->attr.class_ok;
	  sym->as = gfc_copy_array_spec (ifc->result->as);
	  sym->result = sym;
	}
      else
	{
	  sym->ts = ifc->ts;
	  sym->attr.allocatable = ifc->attr.allocatable;
	  sym->attr.pointer = ifc->attr.pointer;
	  sym->attr.dimension = ifc->attr.dimension;
	  sym->attr.class_ok = ifc->attr.class_ok;
	  sym->as = gfc_copy_array_spec (ifc->as);
	}
      sym->ts.interface = ifc;
      sym->attr.function = ifc->attr.function;
      sym->attr.subroutine = ifc->attr.subroutine;

      sym->attr.pure = ifc->attr.pure;
      sym->attr.elemental = ifc->attr.elemental;
      sym->attr.contiguous = ifc->attr.contiguous;
      sym->attr.recursive = ifc->attr.recursive;
      sym->attr.always_explicit = ifc->attr.always_explicit;
      sym->attr.ext_attr |= ifc->attr.ext_attr;
      sym->attr.is_bind_c = ifc->attr.is_bind_c;
      /* Copy char length.  */
      if (ifc->ts.type == BT_CHARACTER && ifc->ts.u.cl)
	{
	  sym->ts.u.cl = gfc_new_charlen (sym->ns, ifc->ts.u.cl);
	  if (sym->ts.u.cl->length && !sym->ts.u.cl->resolved
	      && !gfc_resolve_expr (sym->ts.u.cl->length))
	    return false;
	}
    }

  return true;
}


/* Resolve types of formal argument lists.  These have to be done early so that
   the formal argument lists of module procedures can be copied to the
   containing module before the individual procedures are resolved
   individually.  We also resolve argument lists of procedures in interface
   blocks because they are self-contained scoping units.

   Since a dummy argument cannot be a non-dummy procedure, the only
   resort left for untyped names are the IMPLICIT types.  */

static void
resolve_formal_arglist (gfc_symbol *proc)
{
  gfc_formal_arglist *f;
  gfc_symbol *sym;
  bool saved_specification_expr;
  int i;

  if (proc->result != NULL)
    sym = proc->result;
  else
    sym = proc;

  if (gfc_elemental (proc)
      || sym->attr.pointer || sym->attr.allocatable
      || (sym->as && sym->as->rank != 0))
    {
      proc->attr.always_explicit = 1;
      sym->attr.always_explicit = 1;
    }

  formal_arg_flag = true;

  for (f = proc->formal; f; f = f->next)
    {
      gfc_array_spec *as;

      sym = f->sym;

      if (sym == NULL)
	{
	  /* Alternate return placeholder.  */
	  if (gfc_elemental (proc))
	    gfc_error ("Alternate return specifier in elemental subroutine "
		       "%qs at %L is not allowed", proc->name,
		       &proc->declared_at);
	  if (proc->attr.function)
	    gfc_error ("Alternate return specifier in function "
		       "%qs at %L is not allowed", proc->name,
		       &proc->declared_at);
	  continue;
	}
      else if (sym->attr.procedure && sym->attr.if_source != IFSRC_DECL
	       && !resolve_procedure_interface (sym))
	return;

      if (strcmp (proc->name, sym->name) == 0)
        {
          gfc_error ("Self-referential argument "
                     "%qs at %L is not allowed", sym->name,
                     &proc->declared_at);
          return;
        }

      if (sym->attr.if_source != IFSRC_UNKNOWN)
	resolve_formal_arglist (sym);

      if (sym->attr.subroutine || sym->attr.external)
	{
	  if (sym->attr.flavor == FL_UNKNOWN)
	    gfc_add_flavor (&sym->attr, FL_PROCEDURE, sym->name, &sym->declared_at);
	}
      else
	{
	  if (sym->ts.type == BT_UNKNOWN && !proc->attr.intrinsic
	      && (!sym->attr.function || sym->result == sym))
	    gfc_set_default_type (sym, 1, sym->ns);
	}

      as = sym->ts.type == BT_CLASS && sym->attr.class_ok
	   ? CLASS_DATA (sym)->as : sym->as;

      saved_specification_expr = specification_expr;
      specification_expr = true;
      gfc_resolve_array_spec (as, 0);
      specification_expr = saved_specification_expr;

      /* We can't tell if an array with dimension (:) is assumed or deferred
	 shape until we know if it has the pointer or allocatable attributes.
      */
      if (as && as->rank > 0 && as->type == AS_DEFERRED
	  && ((sym->ts.type != BT_CLASS
	       && !(sym->attr.pointer || sym->attr.allocatable))
              || (sym->ts.type == BT_CLASS
		  && !(CLASS_DATA (sym)->attr.class_pointer
		       || CLASS_DATA (sym)->attr.allocatable)))
	  && sym->attr.flavor != FL_PROCEDURE)
	{
	  as->type = AS_ASSUMED_SHAPE;
	  for (i = 0; i < as->rank; i++)
	    as->lower[i] = gfc_get_int_expr (gfc_default_integer_kind, NULL, 1);
	}

      if ((as && as->rank > 0 && as->type == AS_ASSUMED_SHAPE)
	  || (as && as->type == AS_ASSUMED_RANK)
	  || sym->attr.pointer || sym->attr.allocatable || sym->attr.target
	  || (sym->ts.type == BT_CLASS && sym->attr.class_ok
	      && (CLASS_DATA (sym)->attr.class_pointer
		  || CLASS_DATA (sym)->attr.allocatable
		  || CLASS_DATA (sym)->attr.target))
	  || sym->attr.optional)
	{
	  proc->attr.always_explicit = 1;
	  if (proc->result)
	    proc->result->attr.always_explicit = 1;
	}

      /* If the flavor is unknown at this point, it has to be a variable.
	 A procedure specification would have already set the type.  */

      if (sym->attr.flavor == FL_UNKNOWN)
	gfc_add_flavor (&sym->attr, FL_VARIABLE, sym->name, &sym->declared_at);

      if (gfc_pure (proc))
	{
	  if (sym->attr.flavor == FL_PROCEDURE)
	    {
	      /* F08:C1279.  */
	      if (!gfc_pure (sym))
		{
		  gfc_error ("Dummy procedure %qs of PURE procedure at %L must "
			    "also be PURE", sym->name, &sym->declared_at);
		  continue;
		}
	    }
	  else if (!sym->attr.pointer)
	    {
	      if (proc->attr.function && sym->attr.intent != INTENT_IN)
		{
		  if (sym->attr.value)
		    gfc_notify_std (GFC_STD_F2008, "Argument %qs"
				    " of pure function %qs at %L with VALUE "
				    "attribute but without INTENT(IN)",
				    sym->name, proc->name, &sym->declared_at);
		  else
		    gfc_error ("Argument %qs of pure function %qs at %L must "
			       "be INTENT(IN) or VALUE", sym->name, proc->name,
			       &sym->declared_at);
		}

	      if (proc->attr.subroutine && sym->attr.intent == INTENT_UNKNOWN)
		{
		  if (sym->attr.value)
		    gfc_notify_std (GFC_STD_F2008, "Argument %qs"
				    " of pure subroutine %qs at %L with VALUE "
				    "attribute but without INTENT", sym->name,
				    proc->name, &sym->declared_at);
		  else
		    gfc_error ("Argument %qs of pure subroutine %qs at %L "
			       "must have its INTENT specified or have the "
			       "VALUE attribute", sym->name, proc->name,
			       &sym->declared_at);
		}
	    }

	  /* F08:C1278a.  */
	  if (sym->ts.type == BT_CLASS && sym->attr.intent == INTENT_OUT)
	    {
	      gfc_error ("INTENT(OUT) argument %qs of pure procedure %qs at %L"
			 " may not be polymorphic", sym->name, proc->name,
			 &sym->declared_at);
	      continue;
	    }
	}

      if (proc->attr.implicit_pure)
	{
	  if (sym->attr.flavor == FL_PROCEDURE)
	    {
	      if (!gfc_pure (sym))
		proc->attr.implicit_pure = 0;
	    }
	  else if (!sym->attr.pointer)
	    {
	      if (proc->attr.function && sym->attr.intent != INTENT_IN
		  && !sym->value)
		proc->attr.implicit_pure = 0;

	      if (proc->attr.subroutine && sym->attr.intent == INTENT_UNKNOWN
		  && !sym->value)
		proc->attr.implicit_pure = 0;
	    }
	}

      if (gfc_elemental (proc))
	{
	  /* F08:C1289.  */
	  if (sym->attr.codimension
	      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
		  && CLASS_DATA (sym)->attr.codimension))
	    {
	      gfc_error ("Coarray dummy argument %qs at %L to elemental "
			 "procedure", sym->name, &sym->declared_at);
	      continue;
	    }

	  if (sym->as || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
			  && CLASS_DATA (sym)->as))
	    {
	      gfc_error ("Argument %qs of elemental procedure at %L must "
			 "be scalar", sym->name, &sym->declared_at);
	      continue;
	    }

	  if (sym->attr.allocatable
	      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
		  && CLASS_DATA (sym)->attr.allocatable))
	    {
	      gfc_error ("Argument %qs of elemental procedure at %L cannot "
			 "have the ALLOCATABLE attribute", sym->name,
			 &sym->declared_at);
	      continue;
	    }

	  if (sym->attr.pointer
	      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
		  && CLASS_DATA (sym)->attr.class_pointer))
	    {
	      gfc_error ("Argument %qs of elemental procedure at %L cannot "
			 "have the POINTER attribute", sym->name,
			 &sym->declared_at);
	      continue;
	    }

	  if (sym->attr.flavor == FL_PROCEDURE)
	    {
	      gfc_error ("Dummy procedure %qs not allowed in elemental "
			 "procedure %qs at %L", sym->name, proc->name,
			 &sym->declared_at);
	      continue;
	    }

	  /* Fortran 2008 Corrigendum 1, C1290a.  */
	  if (sym->attr.intent == INTENT_UNKNOWN && !sym->attr.value)
	    {
	      gfc_error ("Argument %qs of elemental procedure %qs at %L must "
			 "have its INTENT specified or have the VALUE "
			 "attribute", sym->name, proc->name,
			 &sym->declared_at);
	      continue;
	    }
	}

      /* Each dummy shall be specified to be scalar.  */
      if (proc->attr.proc == PROC_ST_FUNCTION)
	{
	  if (sym->as != NULL)
	    {
	      gfc_error ("Argument %qs of statement function at %L must "
			 "be scalar", sym->name, &sym->declared_at);
	      continue;
	    }

	  if (sym->ts.type == BT_CHARACTER)
	    {
	      gfc_charlen *cl = sym->ts.u.cl;
	      if (!cl || !cl->length || cl->length->expr_type != EXPR_CONSTANT)
		{
		  gfc_error ("Character-valued argument %qs of statement "
			     "function at %L must have constant length",
			     sym->name, &sym->declared_at);
		  continue;
		}
	    }
	}
    }
  formal_arg_flag = false;
}


/* Work function called when searching for symbols that have argument lists
   associated with them.  */

static void
find_arglists (gfc_symbol *sym)
{
  if (sym->attr.if_source == IFSRC_UNKNOWN || sym->ns != gfc_current_ns
      || gfc_fl_struct (sym->attr.flavor) || sym->attr.intrinsic)
    return;

  resolve_formal_arglist (sym);
}


/* Given a namespace, resolve all formal argument lists within the namespace.
 */

static void
resolve_formal_arglists (gfc_namespace *ns)
{
  if (ns == NULL)
    return;

  gfc_traverse_ns (ns, find_arglists);
}


static void
resolve_contained_fntype (gfc_symbol *sym, gfc_namespace *ns)
{
  bool t;

  if (sym && sym->attr.flavor == FL_PROCEDURE
      && sym->ns->parent
      && sym->ns->parent->proc_name
      && sym->ns->parent->proc_name->attr.flavor == FL_PROCEDURE
      && !strcmp (sym->name, sym->ns->parent->proc_name->name))
    gfc_error ("Contained procedure %qs at %L has the same name as its "
	       "encompassing procedure", sym->name, &sym->declared_at);

  /* If this namespace is not a function or an entry master function,
     ignore it.  */
  if (! sym || !(sym->attr.function || sym->attr.flavor == FL_VARIABLE)
      || sym->attr.entry_master)
    return;

  /* Try to find out of what the return type is.  */
  if (sym->result->ts.type == BT_UNKNOWN && sym->result->ts.interface == NULL)
    {
      t = gfc_set_default_type (sym->result, 0, ns);

      if (!t && !sym->result->attr.untyped)
	{
	  if (sym->result == sym)
	    gfc_error ("Contained function %qs at %L has no IMPLICIT type",
		       sym->name, &sym->declared_at);
	  else if (!sym->result->attr.proc_pointer)
	    gfc_error ("Result %qs of contained function %qs at %L has "
		       "no IMPLICIT type", sym->result->name, sym->name,
		       &sym->result->declared_at);
	  sym->result->attr.untyped = 1;
	}
    }

  /* Fortran 95 Draft Standard, page 51, Section 5.1.1.5, on the Character
     type, lists the only ways a character length value of * can be used:
     dummy arguments of procedures, named constants, and function results
     in external functions.  Internal function results and results of module
     procedures are not on this list, ergo, not permitted.  */

  if (sym->result->ts.type == BT_CHARACTER)
    {
      gfc_charlen *cl = sym->result->ts.u.cl;
      if ((!cl || !cl->length) && !sym->result->ts.deferred)
	{
	  /* See if this is a module-procedure and adapt error message
	     accordingly.  */
	  bool module_proc;
	  gcc_assert (ns->parent && ns->parent->proc_name);
	  module_proc = (ns->parent->proc_name->attr.flavor == FL_MODULE);

	  gfc_error (module_proc
		     ? G_("Character-valued module procedure %qs at %L"
			  " must not be assumed length")
		     : G_("Character-valued internal function %qs at %L"
			  " must not be assumed length"),
		     sym->name, &sym->declared_at);
	}
    }
}


/* Add NEW_ARGS to the formal argument list of PROC, taking care not to
   introduce duplicates.  */

static void
merge_argument_lists (gfc_symbol *proc, gfc_formal_arglist *new_args)
{
  gfc_formal_arglist *f, *new_arglist;
  gfc_symbol *new_sym;

  for (; new_args != NULL; new_args = new_args->next)
    {
      new_sym = new_args->sym;
      /* See if this arg is already in the formal argument list.  */
      for (f = proc->formal; f; f = f->next)
	{
	  if (new_sym == f->sym)
	    break;
	}

      if (f)
	continue;

      /* Add a new argument.  Argument order is not important.  */
      new_arglist = gfc_get_formal_arglist ();
      new_arglist->sym = new_sym;
      new_arglist->next = proc->formal;
      proc->formal  = new_arglist;
    }
}


/* Flag the arguments that are not present in all entries.  */

static void
check_argument_lists (gfc_symbol *proc, gfc_formal_arglist *new_args)
{
  gfc_formal_arglist *f, *head;
  head = new_args;

  for (f = proc->formal; f; f = f->next)
    {
      if (f->sym == NULL)
	continue;

      for (new_args = head; new_args; new_args = new_args->next)
	{
	  if (new_args->sym == f->sym)
	    break;
	}

      if (new_args)
	continue;

      f->sym->attr.not_always_present = 1;
    }
}


/* Resolve alternate entry points.  If a symbol has multiple entry points we
   create a new master symbol for the main routine, and turn the existing
   symbol into an entry point.  */

static void
resolve_entries (gfc_namespace *ns)
{
  gfc_namespace *old_ns;
  gfc_code *c;
  gfc_symbol *proc;
  gfc_entry_list *el;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  static int master_count = 0;

  if (ns->proc_name == NULL)
    return;

  /* No need to do anything if this procedure doesn't have alternate entry
     points.  */
  if (!ns->entries)
    return;

  /* We may already have resolved alternate entry points.  */
  if (ns->proc_name->attr.entry_master)
    return;

  /* If this isn't a procedure something has gone horribly wrong.  */
  gcc_assert (ns->proc_name->attr.flavor == FL_PROCEDURE);

  /* Remember the current namespace.  */
  old_ns = gfc_current_ns;

  gfc_current_ns = ns;

  /* Add the main entry point to the list of entry points.  */
  el = gfc_get_entry_list ();
  el->sym = ns->proc_name;
  el->id = 0;
  el->next = ns->entries;
  ns->entries = el;
  ns->proc_name->attr.entry = 1;

  /* If it is a module function, it needs to be in the right namespace
     so that gfc_get_fake_result_decl can gather up the results. The
     need for this arose in get_proc_name, where these beasts were
     left in their own namespace, to keep prior references linked to
     the entry declaration.*/
  if (ns->proc_name->attr.function
      && ns->parent && ns->parent->proc_name->attr.flavor == FL_MODULE)
    el->sym->ns = ns;

  /* Do the same for entries where the master is not a module
     procedure.  These are retained in the module namespace because
     of the module procedure declaration.  */
  for (el = el->next; el; el = el->next)
    if (el->sym->ns->proc_name->attr.flavor == FL_MODULE
	  && el->sym->attr.mod_proc)
      el->sym->ns = ns;
  el = ns->entries;

  /* Add an entry statement for it.  */
  c = gfc_get_code (EXEC_ENTRY);
  c->ext.entry = el;
  c->next = ns->code;
  ns->code = c;

  /* Create a new symbol for the master function.  */
  /* Give the internal function a unique name (within this file).
     Also include the function name so the user has some hope of figuring
     out what is going on.  */
  snprintf (name, GFC_MAX_SYMBOL_LEN, "master.%d.%s",
	    master_count++, ns->proc_name->name);
  gfc_get_ha_symbol (name, &proc);
  gcc_assert (proc != NULL);

  gfc_add_procedure (&proc->attr, PROC_INTERNAL, proc->name, NULL);
  if (ns->proc_name->attr.subroutine)
    gfc_add_subroutine (&proc->attr, proc->name, NULL);
  else
    {
      gfc_symbol *sym;
      gfc_typespec *ts, *fts;
      gfc_array_spec *as, *fas;
      gfc_add_function (&proc->attr, proc->name, NULL);
      proc->result = proc;
      fas = ns->entries->sym->as;
      fas = fas ? fas : ns->entries->sym->result->as;
      fts = &ns->entries->sym->result->ts;
      if (fts->type == BT_UNKNOWN)
	fts = gfc_get_default_type (ns->entries->sym->result->name, NULL);
      for (el = ns->entries->next; el; el = el->next)
	{
	  ts = &el->sym->result->ts;
	  as = el->sym->as;
	  as = as ? as : el->sym->result->as;
	  if (ts->type == BT_UNKNOWN)
	    ts = gfc_get_default_type (el->sym->result->name, NULL);

	  if (! gfc_compare_types (ts, fts)
	      || (el->sym->result->attr.dimension
		  != ns->entries->sym->result->attr.dimension)
	      || (el->sym->result->attr.pointer
		  != ns->entries->sym->result->attr.pointer))
	    break;
	  else if (as && fas && ns->entries->sym->result != el->sym->result
		      && gfc_compare_array_spec (as, fas) == 0)
	    gfc_error ("Function %s at %L has entries with mismatched "
		       "array specifications", ns->entries->sym->name,
		       &ns->entries->sym->declared_at);
	  /* The characteristics need to match and thus both need to have
	     the same string length, i.e. both len=*, or both len=4.
	     Having both len=<variable> is also possible, but difficult to
	     check at compile time.  */
	  else if (ts->type == BT_CHARACTER && ts->u.cl && fts->u.cl
		   && (((ts->u.cl->length && !fts->u.cl->length)
			||(!ts->u.cl->length && fts->u.cl->length))
		       || (ts->u.cl->length
			   && ts->u.cl->length->expr_type
			      != fts->u.cl->length->expr_type)
		       || (ts->u.cl->length
			   && ts->u.cl->length->expr_type == EXPR_CONSTANT
		           && mpz_cmp (ts->u.cl->length->value.integer,
				       fts->u.cl->length->value.integer) != 0)))
	    gfc_notify_std (GFC_STD_GNU, "Function %s at %L with "
			    "entries returning variables of different "
			    "string lengths", ns->entries->sym->name,
			    &ns->entries->sym->declared_at);
	}

      if (el == NULL)
	{
	  sym = ns->entries->sym->result;
	  /* All result types the same.  */
	  proc->ts = *fts;
	  if (sym->attr.dimension)
	    gfc_set_array_spec (proc, gfc_copy_array_spec (sym->as), NULL);
	  if (sym->attr.pointer)
	    gfc_add_pointer (&proc->attr, NULL);
	}
      else
	{
	  /* Otherwise the result will be passed through a union by
	     reference.  */
	  proc->attr.mixed_entry_master = 1;
	  for (el = ns->entries; el; el = el->next)
	    {
	      sym = el->sym->result;
	      if (sym->attr.dimension)
		{
		  if (el == ns->entries)
		    gfc_error ("FUNCTION result %s can't be an array in "
			       "FUNCTION %s at %L", sym->name,
			       ns->entries->sym->name, &sym->declared_at);
		  else
		    gfc_error ("ENTRY result %s can't be an array in "
			       "FUNCTION %s at %L", sym->name,
			       ns->entries->sym->name, &sym->declared_at);
		}
	      else if (sym->attr.pointer)
		{
		  if (el == ns->entries)
		    gfc_error ("FUNCTION result %s can't be a POINTER in "
			       "FUNCTION %s at %L", sym->name,
			       ns->entries->sym->name, &sym->declared_at);
		  else
		    gfc_error ("ENTRY result %s can't be a POINTER in "
			       "FUNCTION %s at %L", sym->name,
			       ns->entries->sym->name, &sym->declared_at);
		}
	      else
		{
		  ts = &sym->ts;
		  if (ts->type == BT_UNKNOWN)
		    ts = gfc_get_default_type (sym->name, NULL);
		  switch (ts->type)
		    {
		    case BT_INTEGER:
		      if (ts->kind == gfc_default_integer_kind)
			sym = NULL;
		      break;
		    case BT_REAL:
		      if (ts->kind == gfc_default_real_kind
			  || ts->kind == gfc_default_double_kind)
			sym = NULL;
		      break;
		    case BT_COMPLEX:
		      if (ts->kind == gfc_default_complex_kind)
			sym = NULL;
		      break;
		    case BT_LOGICAL:
		      if (ts->kind == gfc_default_logical_kind)
			sym = NULL;
		      break;
		    case BT_UNKNOWN:
		      /* We will issue error elsewhere.  */
		      sym = NULL;
		      break;
		    default:
		      break;
		    }
		  if (sym)
		    {
		      if (el == ns->entries)
			gfc_error ("FUNCTION result %s can't be of type %s "
				   "in FUNCTION %s at %L", sym->name,
				   gfc_typename (ts), ns->entries->sym->name,
				   &sym->declared_at);
		      else
			gfc_error ("ENTRY result %s can't be of type %s "
				   "in FUNCTION %s at %L", sym->name,
				   gfc_typename (ts), ns->entries->sym->name,
				   &sym->declared_at);
		    }
		}
	    }
	}
    }
  proc->attr.access = ACCESS_PRIVATE;
  proc->attr.entry_master = 1;

  /* Merge all the entry point arguments.  */
  for (el = ns->entries; el; el = el->next)
    merge_argument_lists (proc, el->sym->formal);

  /* Check the master formal arguments for any that are not
     present in all entry points.  */
  for (el = ns->entries; el; el = el->next)
    check_argument_lists (proc, el->sym->formal);

  /* Use the master function for the function body.  */
  ns->proc_name = proc;

  /* Finalize the new symbols.  */
  gfc_commit_symbols ();

  /* Restore the original namespace.  */
  gfc_current_ns = old_ns;
}


/* Resolve common variables.  */
static void
resolve_common_vars (gfc_common_head *common_block, bool named_common)
{
  gfc_symbol *csym = common_block->head;

  for (; csym; csym = csym->common_next)
    {
      /* gfc_add_in_common may have been called before, but the reported errors
	 have been ignored to continue parsing.
	 We do the checks again here.  */
      if (!csym->attr.use_assoc)
	gfc_add_in_common (&csym->attr, csym->name, &common_block->where);

      if (csym->value || csym->attr.data)
	{
	  if (!csym->ns->is_block_data)
	    gfc_notify_std (GFC_STD_GNU, "Variable %qs at %L is in COMMON "
			    "but only in BLOCK DATA initialization is "
			    "allowed", csym->name, &csym->declared_at);
	  else if (!named_common)
	    gfc_notify_std (GFC_STD_GNU, "Initialized variable %qs at %L is "
			    "in a blank COMMON but initialization is only "
			    "allowed in named common blocks", csym->name,
			    &csym->declared_at);
	}

      if (UNLIMITED_POLY (csym))
	gfc_error_now ("%qs in cannot appear in COMMON at %L "
		       "[F2008:C5100]", csym->name, &csym->declared_at);

      if (csym->ts.type != BT_DERIVED)
	continue;

      if (!(csym->ts.u.derived->attr.sequence
	    || csym->ts.u.derived->attr.is_bind_c))
	gfc_error_now ("Derived type variable %qs in COMMON at %L "
		       "has neither the SEQUENCE nor the BIND(C) "
		       "attribute", csym->name, &csym->declared_at);
      if (csym->ts.u.derived->attr.alloc_comp)
	gfc_error_now ("Derived type variable %qs in COMMON at %L "
		       "has an ultimate component that is "
		       "allocatable", csym->name, &csym->declared_at);
      if (gfc_has_default_initializer (csym->ts.u.derived))
	gfc_error_now ("Derived type variable %qs in COMMON at %L "
		       "may not have default initializer", csym->name,
		       &csym->declared_at);

      if (csym->attr.flavor == FL_UNKNOWN && !csym->attr.proc_pointer)
	gfc_add_flavor (&csym->attr, FL_VARIABLE, csym->name, &csym->declared_at);
    }
}

/* Resolve common blocks.  */
static void
resolve_common_blocks (gfc_symtree *common_root)
{
  gfc_symbol *sym;
  gfc_gsymbol * gsym;

  if (common_root == NULL)
    return;

  if (common_root->left)
    resolve_common_blocks (common_root->left);
  if (common_root->right)
    resolve_common_blocks (common_root->right);

  resolve_common_vars (common_root->n.common, true);

  /* The common name is a global name - in Fortran 2003 also if it has a
     C binding name, since Fortran 2008 only the C binding name is a global
     identifier.  */
  if (!common_root->n.common->binding_label
      || gfc_notification_std (GFC_STD_F2008))
    {
      gsym = gfc_find_gsymbol (gfc_gsym_root,
			       common_root->n.common->name);

      if (gsym && gfc_notification_std (GFC_STD_F2008)
	  && gsym->type == GSYM_COMMON
	  && ((common_root->n.common->binding_label
	       && (!gsym->binding_label
		   || strcmp (common_root->n.common->binding_label,
			      gsym->binding_label) != 0))
	      || (!common_root->n.common->binding_label
		  && gsym->binding_label)))
	{
	  gfc_error ("In Fortran 2003 COMMON %qs block at %L is a global "
		     "identifier and must thus have the same binding name "
		     "as the same-named COMMON block at %L: %s vs %s",
		     common_root->n.common->name, &common_root->n.common->where,
		     &gsym->where,
		     common_root->n.common->binding_label
		     ? common_root->n.common->binding_label : "(blank)",
		     gsym->binding_label ? gsym->binding_label : "(blank)");
	  return;
	}

      if (gsym && gsym->type != GSYM_COMMON
	  && !common_root->n.common->binding_label)
	{
	  gfc_error ("COMMON block %qs at %L uses the same global identifier "
		     "as entity at %L",
		     common_root->n.common->name, &common_root->n.common->where,
		     &gsym->where);
	  return;
	}
      if (gsym && gsym->type != GSYM_COMMON)
	{
	  gfc_error ("Fortran 2008: COMMON block %qs with binding label at "
		     "%L sharing the identifier with global non-COMMON-block "
		     "entity at %L", common_root->n.common->name,
		     &common_root->n.common->where, &gsym->where);
	  return;
	}
      if (!gsym)
	{
	  gsym = gfc_get_gsymbol (common_root->n.common->name);
	  gsym->type = GSYM_COMMON;
	  gsym->where = common_root->n.common->where;
	  gsym->defined = 1;
	}
      gsym->used = 1;
    }

  if (common_root->n.common->binding_label)
    {
      gsym = gfc_find_gsymbol (gfc_gsym_root,
			       common_root->n.common->binding_label);
      if (gsym && gsym->type != GSYM_COMMON)
	{
	  gfc_error ("COMMON block at %L with binding label %s uses the same "
		     "global identifier as entity at %L",
		     &common_root->n.common->where,
		     common_root->n.common->binding_label, &gsym->where);
	  return;
	}
      if (!gsym)
	{
	  gsym = gfc_get_gsymbol (common_root->n.common->binding_label);
	  gsym->type = GSYM_COMMON;
	  gsym->where = common_root->n.common->where;
	  gsym->defined = 1;
	}
      gsym->used = 1;
    }

  gfc_find_symbol (common_root->name, gfc_current_ns, 0, &sym);
  if (sym == NULL)
    return;

  if (sym->attr.flavor == FL_PARAMETER)
    gfc_error ("COMMON block %qs at %L is used as PARAMETER at %L",
	       sym->name, &common_root->n.common->where, &sym->declared_at);

  if (sym->attr.external)
    gfc_error ("COMMON block %qs at %L can not have the EXTERNAL attribute",
	       sym->name, &common_root->n.common->where);

  if (sym->attr.intrinsic)
    gfc_error ("COMMON block %qs at %L is also an intrinsic procedure",
	       sym->name, &common_root->n.common->where);
  else if (sym->attr.result
	   || gfc_is_function_return_value (sym, gfc_current_ns))
    gfc_notify_std (GFC_STD_F2003, "COMMON block %qs at %L "
		    "that is also a function result", sym->name,
		    &common_root->n.common->where);
  else if (sym->attr.flavor == FL_PROCEDURE && sym->attr.proc != PROC_INTERNAL
	   && sym->attr.proc != PROC_ST_FUNCTION)
    gfc_notify_std (GFC_STD_F2003, "COMMON block %qs at %L "
		    "that is also a global procedure", sym->name,
		    &common_root->n.common->where);
}


/* Resolve contained function types.  Because contained functions can call one
   another, they have to be worked out before any of the contained procedures
   can be resolved.

   The good news is that if a function doesn't already have a type, the only
   way it can get one is through an IMPLICIT type or a RESULT variable, because
   by definition contained functions are contained namespace they're contained
   in, not in a sibling or parent namespace.  */

static void
resolve_contained_functions (gfc_namespace *ns)
{
  gfc_namespace *child;
  gfc_entry_list *el;

  resolve_formal_arglists (ns);

  for (child = ns->contained; child; child = child->sibling)
    {
      /* Resolve alternate entry points first.  */
      resolve_entries (child);

      /* Then check function return types.  */
      resolve_contained_fntype (child->proc_name, child);
      for (el = child->entries; el; el = el->next)
	resolve_contained_fntype (el->sym, child);
    }
}


static bool resolve_fl_derived0 (gfc_symbol *sym);
static bool resolve_fl_struct (gfc_symbol *sym);


/* Resolve all of the elements of a structure constructor and make sure that
   the types are correct. The 'init' flag indicates that the given
   constructor is an initializer.  */

static bool
resolve_structure_cons (gfc_expr *expr, int init)
{
  gfc_constructor *cons;
  gfc_component *comp;
  bool t;
  symbol_attribute a;

  t = true;

  if (expr->ts.type == BT_DERIVED || expr->ts.type == BT_UNION)
    {
      if (expr->ts.u.derived->attr.flavor == FL_DERIVED)
        resolve_fl_derived0 (expr->ts.u.derived);
      else
        resolve_fl_struct (expr->ts.u.derived);
    }

  cons = gfc_constructor_first (expr->value.constructor);

  /* A constructor may have references if it is the result of substituting a
     parameter variable.  In this case we just pull out the component we
     want.  */
  if (expr->ref)
    comp = expr->ref->u.c.sym->components;
  else
    comp = expr->ts.u.derived->components;

  for (; comp && cons; comp = comp->next, cons = gfc_constructor_next (cons))
    {
      int rank;

      if (!cons->expr)
	continue;

      /* Unions use an EXPR_NULL contrived expression to tell the translation
         phase to generate an initializer of the appropriate length.
         Ignore it here.  */
      if (cons->expr->ts.type == BT_UNION && cons->expr->expr_type == EXPR_NULL)
        continue;

      if (!gfc_resolve_expr (cons->expr))
	{
	  t = false;
	  continue;
	}

      rank = comp->as ? comp->as->rank : 0;
      if (comp->ts.type == BT_CLASS && CLASS_DATA (comp)->as)
 	rank = CLASS_DATA (comp)->as->rank;

      if (cons->expr->expr_type != EXPR_NULL && rank != cons->expr->rank
	  && (comp->attr.allocatable || cons->expr->rank))
	{
	  gfc_error ("The rank of the element in the structure "
		     "constructor at %L does not match that of the "
		     "component (%d/%d)", &cons->expr->where,
		     cons->expr->rank, rank);
	  t = false;
	}

      /* If we don't have the right type, try to convert it.  */

      if (!comp->attr.proc_pointer &&
	  !gfc_compare_types (&cons->expr->ts, &comp->ts))
	{
	  if (strcmp (comp->name, "_extends") == 0)
	    {
	      /* Can afford to be brutal with the _extends initializer.
		 The derived type can get lost because it is PRIVATE
		 but it is not usage constrained by the standard.  */
	      cons->expr->ts = comp->ts;
	    }
	  else if (comp->attr.pointer && cons->expr->ts.type != BT_UNKNOWN)
	    {
	      gfc_error ("The element in the structure constructor at %L, "
			 "for pointer component %qs, is %s but should be %s",
			 &cons->expr->where, comp->name,
			 gfc_basic_typename (cons->expr->ts.type),
			 gfc_basic_typename (comp->ts.type));
	      t = false;
	    }
	  else
	    {
	      bool t2 = gfc_convert_type (cons->expr, &comp->ts, 1);
	      if (t)
		t = t2;
	    }
	}

      /* For strings, the length of the constructor should be the same as
	 the one of the structure, ensure this if the lengths are known at
 	 compile time and when we are dealing with PARAMETER or structure
	 constructors.  */
      if (cons->expr->ts.type == BT_CHARACTER && comp->ts.u.cl
	  && comp->ts.u.cl->length
	  && comp->ts.u.cl->length->expr_type == EXPR_CONSTANT
	  && cons->expr->ts.u.cl && cons->expr->ts.u.cl->length
	  && cons->expr->ts.u.cl->length->expr_type == EXPR_CONSTANT
	  && cons->expr->rank != 0
	  && mpz_cmp (cons->expr->ts.u.cl->length->value.integer,
		      comp->ts.u.cl->length->value.integer) != 0)
	{
	  if (cons->expr->expr_type == EXPR_VARIABLE
	      && cons->expr->symtree->n.sym->attr.flavor == FL_PARAMETER)
	    {
	      /* Wrap the parameter in an array constructor (EXPR_ARRAY)
		 to make use of the gfc_resolve_character_array_constructor
		 machinery.  The expression is later simplified away to
		 an array of string literals.  */
	      gfc_expr *para = cons->expr;
	      cons->expr = gfc_get_expr ();
	      cons->expr->ts = para->ts;
	      cons->expr->where = para->where;
	      cons->expr->expr_type = EXPR_ARRAY;
	      cons->expr->rank = para->rank;
	      cons->expr->shape = gfc_copy_shape (para->shape, para->rank);
	      gfc_constructor_append_expr (&cons->expr->value.constructor,
					   para, &cons->expr->where);
	    }

	  if (cons->expr->expr_type == EXPR_ARRAY)
	    {
	      /* Rely on the cleanup of the namespace to deal correctly with
		 the old charlen.  (There was a block here that attempted to
		 remove the charlen but broke the chain in so doing.)  */
	      cons->expr->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
	      cons->expr->ts.u.cl->length_from_typespec = true;
	      cons->expr->ts.u.cl->length = gfc_copy_expr (comp->ts.u.cl->length);
	      gfc_resolve_character_array_constructor (cons->expr);
	    }
	}

      if (cons->expr->expr_type == EXPR_NULL
	  && !(comp->attr.pointer || comp->attr.allocatable
	       || comp->attr.proc_pointer || comp->ts.f90_type == BT_VOID
	       || (comp->ts.type == BT_CLASS
		   && (CLASS_DATA (comp)->attr.class_pointer
		       || CLASS_DATA (comp)->attr.allocatable))))
	{
	  t = false;
	  gfc_error ("The NULL in the structure constructor at %L is "
		     "being applied to component %qs, which is neither "
		     "a POINTER nor ALLOCATABLE", &cons->expr->where,
		     comp->name);
	}

      if (comp->attr.proc_pointer && comp->ts.interface)
	{
	  /* Check procedure pointer interface.  */
	  gfc_symbol *s2 = NULL;
	  gfc_component *c2;
	  const char *name;
	  char err[200];

	  c2 = gfc_get_proc_ptr_comp (cons->expr);
	  if (c2)
	    {
	      s2 = c2->ts.interface;
	      name = c2->name;
	    }
	  else if (cons->expr->expr_type == EXPR_FUNCTION)
	    {
	      s2 = cons->expr->symtree->n.sym->result;
	      name = cons->expr->symtree->n.sym->result->name;
	    }
	  else if (cons->expr->expr_type != EXPR_NULL)
	    {
	      s2 = cons->expr->symtree->n.sym;
	      name = cons->expr->symtree->n.sym->name;
	    }

	  if (s2 && !gfc_compare_interfaces (comp->ts.interface, s2, name, 0, 1,
					     err, sizeof (err), NULL, NULL))
	    {
	      gfc_error_opt (OPT_Wargument_mismatch,
			     "Interface mismatch for procedure-pointer "
			     "component %qs in structure constructor at %L:"
			     " %s", comp->name, &cons->expr->where, err);
	      return false;
	    }
	}

      if (!comp->attr.pointer || comp->attr.proc_pointer
	  || cons->expr->expr_type == EXPR_NULL)
	continue;

      a = gfc_expr_attr (cons->expr);

      if (!a.pointer && !a.target)
	{
	  t = false;
	  gfc_error ("The element in the structure constructor at %L, "
		     "for pointer component %qs should be a POINTER or "
		     "a TARGET", &cons->expr->where, comp->name);
	}

      if (init)
	{
	  /* F08:C461. Additional checks for pointer initialization.  */
	  if (a.allocatable)
	    {
	      t = false;
	      gfc_error ("Pointer initialization target at %L "
			 "must not be ALLOCATABLE", &cons->expr->where);
	    }
	  if (!a.save)
	    {
	      t = false;
	      gfc_error ("Pointer initialization target at %L "
			 "must have the SAVE attribute", &cons->expr->where);
	    }
	}

      /* F2003, C1272 (3).  */
      bool impure = cons->expr->expr_type == EXPR_VARIABLE
		    && (gfc_impure_variable (cons->expr->symtree->n.sym)
			|| gfc_is_coindexed (cons->expr));
      if (impure && gfc_pure (NULL))
	{
	  t = false;
	  gfc_error ("Invalid expression in the structure constructor for "
		     "pointer component %qs at %L in PURE procedure",
		     comp->name, &cons->expr->where);
	}

      if (impure)
	gfc_unset_implicit_pure (NULL);
    }

  return t;
}


/****************** Expression name resolution ******************/

/* Returns 0 if a symbol was not declared with a type or
   attribute declaration statement, nonzero otherwise.  */

static int
was_declared (gfc_symbol *sym)
{
  symbol_attribute a;

  a = sym->attr;

  if (!a.implicit_type && sym->ts.type != BT_UNKNOWN)
    return 1;

  if (a.allocatable || a.dimension || a.dummy || a.external || a.intrinsic
      || a.optional || a.pointer || a.save || a.target || a.volatile_
      || a.value || a.access != ACCESS_UNKNOWN || a.intent != INTENT_UNKNOWN
      || a.asynchronous || a.codimension)
    return 1;

  return 0;
}


/* Determine if a symbol is generic or not.  */

static int
generic_sym (gfc_symbol *sym)
{
  gfc_symbol *s;

  if (sym->attr.generic ||
      (sym->attr.intrinsic && gfc_generic_intrinsic (sym->name)))
    return 1;

  if (was_declared (sym) || sym->ns->parent == NULL)
    return 0;

  gfc_find_symbol (sym->name, sym->ns->parent, 1, &s);

  if (s != NULL)
    {
      if (s == sym)
	return 0;
      else
	return generic_sym (s);
    }

  return 0;
}


/* Determine if a symbol is specific or not.  */

static int
specific_sym (gfc_symbol *sym)
{
  gfc_symbol *s;

  if (sym->attr.if_source == IFSRC_IFBODY
      || sym->attr.proc == PROC_MODULE
      || sym->attr.proc == PROC_INTERNAL
      || sym->attr.proc == PROC_ST_FUNCTION
      || (sym->attr.intrinsic && gfc_specific_intrinsic (sym->name))
      || sym->attr.external)
    return 1;

  if (was_declared (sym) || sym->ns->parent == NULL)
    return 0;

  gfc_find_symbol (sym->name, sym->ns->parent, 1, &s);

  return (s == NULL) ? 0 : specific_sym (s);
}


/* Figure out if the procedure is specific, generic or unknown.  */

enum proc_type
{ PTYPE_GENERIC = 1, PTYPE_SPECIFIC, PTYPE_UNKNOWN };

static proc_type
procedure_kind (gfc_symbol *sym)
{
  if (generic_sym (sym))
    return PTYPE_GENERIC;

  if (specific_sym (sym))
    return PTYPE_SPECIFIC;

  return PTYPE_UNKNOWN;
}

/* Check references to assumed size arrays.  The flag need_full_assumed_size
   is nonzero when matching actual arguments.  */

static int need_full_assumed_size = 0;

static bool
check_assumed_size_reference (gfc_symbol *sym, gfc_expr *e)
{
  if (need_full_assumed_size || !(sym->as && sym->as->type == AS_ASSUMED_SIZE))
      return false;

  /* FIXME: The comparison "e->ref->u.ar.type == AR_FULL" is wrong.
     What should it be?  */
  if (e->ref && (e->ref->u.ar.end[e->ref->u.ar.as->rank - 1] == NULL)
	  && (e->ref->u.ar.as->type == AS_ASSUMED_SIZE)
	       && (e->ref->u.ar.type == AR_FULL))
    {
      gfc_error ("The upper bound in the last dimension must "
		 "appear in the reference to the assumed size "
		 "array %qs at %L", sym->name, &e->where);
      return true;
    }
  return false;
}


/* Look for bad assumed size array references in argument expressions
  of elemental and array valued intrinsic procedures.  Since this is
  called from procedure resolution functions, it only recurses at
  operators.  */

static bool
resolve_assumed_size_actual (gfc_expr *e)
{
  if (e == NULL)
   return false;

  switch (e->expr_type)
    {
    case EXPR_VARIABLE:
      if (e->symtree && check_assumed_size_reference (e->symtree->n.sym, e))
	return true;
      break;

    case EXPR_OP:
      if (resolve_assumed_size_actual (e->value.op.op1)
	  || resolve_assumed_size_actual (e->value.op.op2))
	return true;
      break;

    default:
      break;
    }
  return false;
}


/* Check a generic procedure, passed as an actual argument, to see if
   there is a matching specific name.  If none, it is an error, and if
   more than one, the reference is ambiguous.  */
static int
count_specific_procs (gfc_expr *e)
{
  int n;
  gfc_interface *p;
  gfc_symbol *sym;

  n = 0;
  sym = e->symtree->n.sym;

  for (p = sym->generic; p; p = p->next)
    if (strcmp (sym->name, p->sym->name) == 0)
      {
	e->symtree = gfc_find_symtree (p->sym->ns->sym_root,
				       sym->name);
	n++;
      }

  if (n > 1)
    gfc_error ("%qs at %L is ambiguous", e->symtree->n.sym->name,
	       &e->where);

  if (n == 0)
    gfc_error ("GENERIC procedure %qs is not allowed as an actual "
	       "argument at %L", sym->name, &e->where);

  return n;
}


/* See if a call to sym could possibly be a not allowed RECURSION because of
   a missing RECURSIVE declaration.  This means that either sym is the current
   context itself, or sym is the parent of a contained procedure calling its
   non-RECURSIVE containing procedure.
   This also works if sym is an ENTRY.  */

static bool
is_illegal_recursion (gfc_symbol* sym, gfc_namespace* context)
{
  gfc_symbol* proc_sym;
  gfc_symbol* context_proc;
  gfc_namespace* real_context;

  if (sym->attr.flavor == FL_PROGRAM
      || gfc_fl_struct (sym->attr.flavor))
    return false;

  gcc_assert (sym->attr.flavor == FL_PROCEDURE);

  /* If we've got an ENTRY, find real procedure.  */
  if (sym->attr.entry && sym->ns->entries)
    proc_sym = sym->ns->entries->sym;
  else
    proc_sym = sym;

  /* If sym is RECURSIVE, all is well of course.  */
  if (proc_sym->attr.recursive || flag_recursive)
    return false;

  /* Find the context procedure's "real" symbol if it has entries.
     We look for a procedure symbol, so recurse on the parents if we don't
     find one (like in case of a BLOCK construct).  */
  for (real_context = context; ; real_context = real_context->parent)
    {
      /* We should find something, eventually!  */
      gcc_assert (real_context);

      context_proc = (real_context->entries ? real_context->entries->sym
					    : real_context->proc_name);

      /* In some special cases, there may not be a proc_name, like for this
	 invalid code:
	 real(bad_kind()) function foo () ...
	 when checking the call to bad_kind ().
	 In these cases, we simply return here and assume that the
	 call is ok.  */
      if (!context_proc)
	return false;

      if (context_proc->attr.flavor != FL_LABEL)
	break;
    }

  /* A call from sym's body to itself is recursion, of course.  */
  if (context_proc == proc_sym)
    return true;

  /* The same is true if context is a contained procedure and sym the
     containing one.  */
  if (context_proc->attr.contained)
    {
      gfc_symbol* parent_proc;

      gcc_assert (context->parent);
      parent_proc = (context->parent->entries ? context->parent->entries->sym
					      : context->parent->proc_name);

      if (parent_proc == proc_sym)
	return true;
    }

  return false;
}


/* Resolve an intrinsic procedure: Set its function/subroutine attribute,
   its typespec and formal argument list.  */

bool
gfc_resolve_intrinsic (gfc_symbol *sym, locus *loc)
{
  gfc_intrinsic_sym* isym = NULL;
  const char* symstd;

  if (sym->formal)
    return true;

  /* Already resolved.  */
  if (sym->from_intmod && sym->ts.type != BT_UNKNOWN)
    return true;

  /* We already know this one is an intrinsic, so we don't call
     gfc_is_intrinsic for full checking but rather use gfc_find_function and
     gfc_find_subroutine directly to check whether it is a function or
     subroutine.  */

  if (sym->intmod_sym_id && sym->attr.subroutine)
    {
      gfc_isym_id id = gfc_isym_id_by_intmod_sym (sym);
      isym = gfc_intrinsic_subroutine_by_id (id);
    }
  else if (sym->intmod_sym_id)
    {
      gfc_isym_id id = gfc_isym_id_by_intmod_sym (sym);
      isym = gfc_intrinsic_function_by_id (id);
    }
  else if (!sym->attr.subroutine)
    isym = gfc_find_function (sym->name);

  if (isym && !sym->attr.subroutine)
    {
      if (sym->ts.type != BT_UNKNOWN && warn_surprising
	  && !sym->attr.implicit_type)
	gfc_warning (OPT_Wsurprising,
		     "Type specified for intrinsic function %qs at %L is"
		      " ignored", sym->name, &sym->declared_at);

      if (!sym->attr.function &&
	  !gfc_add_function(&sym->attr, sym->name, loc))
	return false;

      sym->ts = isym->ts;
    }
  else if (isym || (isym = gfc_find_subroutine (sym->name)))
    {
      if (sym->ts.type != BT_UNKNOWN && !sym->attr.implicit_type)
	{
	  gfc_error ("Intrinsic subroutine %qs at %L shall not have a type"
		      " specifier", sym->name, &sym->declared_at);
	  return false;
	}

      if (!sym->attr.subroutine &&
	  !gfc_add_subroutine(&sym->attr, sym->name, loc))
	return false;
    }
  else
    {
      gfc_error ("%qs declared INTRINSIC at %L does not exist", sym->name,
		 &sym->declared_at);
      return false;
    }

  gfc_copy_formal_args_intr (sym, isym, NULL);

  sym->attr.pure = isym->pure;
  sym->attr.elemental = isym->elemental;

  /* Check it is actually available in the standard settings.  */
  if (!gfc_check_intrinsic_standard (isym, &symstd, false, sym->declared_at))
    {
      gfc_error ("The intrinsic %qs declared INTRINSIC at %L is not "
		 "available in the current standard settings but %s. Use "
		 "an appropriate %<-std=*%> option or enable "
		 "%<-fall-intrinsics%> in order to use it.",
		 sym->name, &sym->declared_at, symstd);
      return false;
    }

  return true;
}


/* Resolve a procedure expression, like passing it to a called procedure or as
   RHS for a procedure pointer assignment.  */

static bool
resolve_procedure_expression (gfc_expr* expr)
{
  gfc_symbol* sym;

  if (expr->expr_type != EXPR_VARIABLE)
    return true;
  gcc_assert (expr->symtree);

  sym = expr->symtree->n.sym;

  if (sym->attr.intrinsic)
    gfc_resolve_intrinsic (sym, &expr->where);

  if (sym->attr.flavor != FL_PROCEDURE
      || (sym->attr.function && sym->result == sym))
    return true;

  /* A non-RECURSIVE procedure that is used as procedure expression within its
     own body is in danger of being called recursively.  */
  if (is_illegal_recursion (sym, gfc_current_ns))
    gfc_warning (0, "Non-RECURSIVE procedure %qs at %L is possibly calling"
		 " itself recursively.  Declare it RECURSIVE or use"
		 " %<-frecursive%>", sym->name, &expr->where);

  return true;
}


/* Resolve an actual argument list.  Most of the time, this is just
   resolving the expressions in the list.
   The exception is that we sometimes have to decide whether arguments
   that look like procedure arguments are really simple variable
   references.  */

static bool
resolve_actual_arglist (gfc_actual_arglist *arg, procedure_type ptype,
			bool no_formal_args)
{
  gfc_symbol *sym;
  gfc_symtree *parent_st;
  gfc_expr *e;
  gfc_component *comp;
  int save_need_full_assumed_size;
  bool return_value = false;
  bool actual_arg_sav = actual_arg, first_actual_arg_sav = first_actual_arg;

  actual_arg = true;
  first_actual_arg = true;

  for (; arg; arg = arg->next)
    {
      e = arg->expr;
      if (e == NULL)
	{
	  /* Check the label is a valid branching target.  */
	  if (arg->label)
	    {
	      if (arg->label->defined == ST_LABEL_UNKNOWN)
		{
		  gfc_error ("Label %d referenced at %L is never defined",
			     arg->label->value, &arg->label->where);
		  goto cleanup;
		}
	    }
	  first_actual_arg = false;
	  continue;
	}

      if (e->expr_type == EXPR_VARIABLE
	    && e->symtree->n.sym->attr.generic
	    && no_formal_args
	    && count_specific_procs (e) != 1)
	goto cleanup;

      if (e->ts.type != BT_PROCEDURE)
	{
	  save_need_full_assumed_size = need_full_assumed_size;
	  if (e->expr_type != EXPR_VARIABLE)
	    need_full_assumed_size = 0;
	  if (!gfc_resolve_expr (e))
	    goto cleanup;
	  need_full_assumed_size = save_need_full_assumed_size;
	  goto argument_list;
	}

      /* See if the expression node should really be a variable reference.  */

      sym = e->symtree->n.sym;

      if (sym->attr.flavor == FL_PROCEDURE
	  || sym->attr.intrinsic
	  || sym->attr.external)
	{
	  int actual_ok;

	  /* If a procedure is not already determined to be something else
	     check if it is intrinsic.  */
	  if (gfc_is_intrinsic (sym, sym->attr.subroutine, e->where))
	    sym->attr.intrinsic = 1;

	  if (sym->attr.proc == PROC_ST_FUNCTION)
	    {
	      gfc_error ("Statement function %qs at %L is not allowed as an "
			 "actual argument", sym->name, &e->where);
	    }

	  actual_ok = gfc_intrinsic_actual_ok (sym->name,
					       sym->attr.subroutine);
	  if (sym->attr.intrinsic && actual_ok == 0)
	    {
	      gfc_error ("Intrinsic %qs at %L is not allowed as an "
			 "actual argument", sym->name, &e->where);
	    }

	  if (sym->attr.contained && !sym->attr.use_assoc
	      && sym->ns->proc_name->attr.flavor != FL_MODULE)
	    {
	      if (!gfc_notify_std (GFC_STD_F2008, "Internal procedure %qs is"
				   " used as actual argument at %L",
				   sym->name, &e->where))
		goto cleanup;
	    }

	  if (sym->attr.elemental && !sym->attr.intrinsic)
	    {
	      gfc_error ("ELEMENTAL non-INTRINSIC procedure %qs is not "
			 "allowed as an actual argument at %L", sym->name,
			 &e->where);
	    }

	  /* Check if a generic interface has a specific procedure
	    with the same name before emitting an error.  */
	  if (sym->attr.generic && count_specific_procs (e) != 1)
	    goto cleanup;

	  /* Just in case a specific was found for the expression.  */
	  sym = e->symtree->n.sym;

	  /* If the symbol is the function that names the current (or
	     parent) scope, then we really have a variable reference.  */

	  if (gfc_is_function_return_value (sym, sym->ns))
	    goto got_variable;

	  /* If all else fails, see if we have a specific intrinsic.  */
	  if (sym->ts.type == BT_UNKNOWN && sym->attr.intrinsic)
	    {
	      gfc_intrinsic_sym *isym;

	      isym = gfc_find_function (sym->name);
	      if (isym == NULL || !isym->specific)
		{
		  gfc_error ("Unable to find a specific INTRINSIC procedure "
			     "for the reference %qs at %L", sym->name,
			     &e->where);
		  goto cleanup;
		}
	      sym->ts = isym->ts;
	      sym->attr.intrinsic = 1;
	      sym->attr.function = 1;
	    }

	  if (!gfc_resolve_expr (e))
	    goto cleanup;
	  goto argument_list;
	}

      /* See if the name is a module procedure in a parent unit.  */

      if (was_declared (sym) || sym->ns->parent == NULL)
	goto got_variable;

      if (gfc_find_sym_tree (sym->name, sym->ns->parent, 1, &parent_st))
	{
	  gfc_error ("Symbol %qs at %L is ambiguous", sym->name, &e->where);
	  goto cleanup;
	}

      if (parent_st == NULL)
	goto got_variable;

      sym = parent_st->n.sym;
      e->symtree = parent_st;		/* Point to the right thing.  */

      if (sym->attr.flavor == FL_PROCEDURE
	  || sym->attr.intrinsic
	  || sym->attr.external)
	{
	  if (!gfc_resolve_expr (e))
	    goto cleanup;
	  goto argument_list;
	}

    got_variable:
      e->expr_type = EXPR_VARIABLE;
      e->ts = sym->ts;
      if ((sym->as != NULL && sym->ts.type != BT_CLASS)
	  || (sym->ts.type == BT_CLASS && sym->attr.class_ok
	      && CLASS_DATA (sym)->as))
	{
	  e->rank = sym->ts.type == BT_CLASS
		    ? CLASS_DATA (sym)->as->rank : sym->as->rank;
	  e->ref = gfc_get_ref ();
	  e->ref->type = REF_ARRAY;
	  e->ref->u.ar.type = AR_FULL;
	  e->ref->u.ar.as = sym->ts.type == BT_CLASS
			    ? CLASS_DATA (sym)->as : sym->as;
	}

      /* Expressions are assigned a default ts.type of BT_PROCEDURE in
	 primary.c (match_actual_arg). If above code determines that it
	 is a  variable instead, it needs to be resolved as it was not
	 done at the beginning of this function.  */
      save_need_full_assumed_size = need_full_assumed_size;
      if (e->expr_type != EXPR_VARIABLE)
	need_full_assumed_size = 0;
      if (!gfc_resolve_expr (e))
	goto cleanup;
      need_full_assumed_size = save_need_full_assumed_size;

    argument_list:
      /* Check argument list functions %VAL, %LOC and %REF.  There is
	 nothing to do for %REF.  */
      if (arg->name && arg->name[0] == '%')
	{
	  if (strncmp ("%VAL", arg->name, 4) == 0)
	    {
	      if (e->ts.type == BT_CHARACTER || e->ts.type == BT_DERIVED)
		{
		  gfc_error ("By-value argument at %L is not of numeric "
			     "type", &e->where);
		  goto cleanup;
		}

	      if (e->rank)
		{
		  gfc_error ("By-value argument at %L cannot be an array or "
			     "an array section", &e->where);
		  goto cleanup;
		}

	      /* Intrinsics are still PROC_UNKNOWN here.  However,
		 since same file external procedures are not resolvable
		 in gfortran, it is a good deal easier to leave them to
		 intrinsic.c.  */
	      if (ptype != PROC_UNKNOWN
		  && ptype != PROC_DUMMY
		  && ptype != PROC_EXTERNAL
		  && ptype != PROC_MODULE)
		{
		  gfc_error ("By-value argument at %L is not allowed "
			     "in this context", &e->where);
		  goto cleanup;
		}
	    }

	  /* Statement functions have already been excluded above.  */
	  else if (strncmp ("%LOC", arg->name, 4) == 0
		   && e->ts.type == BT_PROCEDURE)
	    {
	      if (e->symtree->n.sym->attr.proc == PROC_INTERNAL)
		{
		  gfc_error ("Passing internal procedure at %L by location "
			     "not allowed", &e->where);
		  goto cleanup;
		}
	    }
	}

      comp = gfc_get_proc_ptr_comp(e);
      if (e->expr_type == EXPR_VARIABLE
	  && comp && comp->attr.elemental)
	{
	    gfc_error ("ELEMENTAL procedure pointer component %qs is not "
		       "allowed as an actual argument at %L", comp->name,
		       &e->where);
	}

      /* Fortran 2008, C1237.  */
      if (e->expr_type == EXPR_VARIABLE && gfc_is_coindexed (e)
	  && gfc_has_ultimate_pointer (e))
	{
	  gfc_error ("Coindexed actual argument at %L with ultimate pointer "
		     "component", &e->where);
	  goto cleanup;
	}

      first_actual_arg = false;
    }

  return_value = true;

cleanup:
  actual_arg = actual_arg_sav;
  first_actual_arg = first_actual_arg_sav;

  return return_value;
}


/* Do the checks of the actual argument list that are specific to elemental
   procedures.  If called with c == NULL, we have a function, otherwise if
   expr == NULL, we have a subroutine.  */

static bool
resolve_elemental_actual (gfc_expr *expr, gfc_code *c)
{
  gfc_actual_arglist *arg0;
  gfc_actual_arglist *arg;
  gfc_symbol *esym = NULL;
  gfc_intrinsic_sym *isym = NULL;
  gfc_expr *e = NULL;
  gfc_intrinsic_arg *iformal = NULL;
  gfc_formal_arglist *eformal = NULL;
  bool formal_optional = false;
  bool set_by_optional = false;
  int i;
  int rank = 0;

  /* Is this an elemental procedure?  */
  if (expr && expr->value.function.actual != NULL)
    {
      if (expr->value.function.esym != NULL
	  && expr->value.function.esym->attr.elemental)
	{
	  arg0 = expr->value.function.actual;
	  esym = expr->value.function.esym;
	}
      else if (expr->value.function.isym != NULL
	       && expr->value.function.isym->elemental)
	{
	  arg0 = expr->value.function.actual;
	  isym = expr->value.function.isym;
	}
      else
	return true;
    }
  else if (c && c->ext.actual != NULL)
    {
      arg0 = c->ext.actual;

      if (c->resolved_sym)
	esym = c->resolved_sym;
      else
	esym = c->symtree->n.sym;
      gcc_assert (esym);

      if (!esym->attr.elemental)
	return true;
    }
  else
    return true;

  /* The rank of an elemental is the rank of its array argument(s).  */
  for (arg = arg0; arg; arg = arg->next)
    {
      if (arg->expr != NULL && arg->expr->rank != 0)
	{
	  rank = arg->expr->rank;
	  if (arg->expr->expr_type == EXPR_VARIABLE
	      && arg->expr->symtree->n.sym->attr.optional)
	    set_by_optional = true;

	  /* Function specific; set the result rank and shape.  */
	  if (expr)
	    {
	      expr->rank = rank;
	      if (!expr->shape && arg->expr->shape)
		{
		  expr->shape = gfc_get_shape (rank);
		  for (i = 0; i < rank; i++)
		    mpz_init_set (expr->shape[i], arg->expr->shape[i]);
		}
	    }
	  break;
	}
    }

  /* If it is an array, it shall not be supplied as an actual argument
     to an elemental procedure unless an array of the same rank is supplied
     as an actual argument corresponding to a nonoptional dummy argument of
     that elemental procedure(12.4.1.5).  */
  formal_optional = false;
  if (isym)
    iformal = isym->formal;
  else
    eformal = esym->formal;

  for (arg = arg0; arg; arg = arg->next)
    {
      if (eformal)
	{
	  if (eformal->sym && eformal->sym->attr.optional)
	    formal_optional = true;
	  eformal = eformal->next;
	}
      else if (isym && iformal)
	{
	  if (iformal->optional)
	    formal_optional = true;
	  iformal = iformal->next;
	}
      else if (isym)
	formal_optional = true;

      if (pedantic && arg->expr != NULL
	  && arg->expr->expr_type == EXPR_VARIABLE
	  && arg->expr->symtree->n.sym->attr.optional
	  && formal_optional
	  && arg->expr->rank
	  && (set_by_optional || arg->expr->rank != rank)
	  && !(isym && isym->id == GFC_ISYM_CONVERSION))
	{
	  gfc_warning (OPT_Wpedantic,
		       "%qs at %L is an array and OPTIONAL; IF IT IS "
		       "MISSING, it cannot be the actual argument of an "
		       "ELEMENTAL procedure unless there is a non-optional "
		       "argument with the same rank (12.4.1.5)",
		       arg->expr->symtree->n.sym->name, &arg->expr->where);
	}
    }

  for (arg = arg0; arg; arg = arg->next)
    {
      if (arg->expr == NULL || arg->expr->rank == 0)
	continue;

      /* Being elemental, the last upper bound of an assumed size array
	 argument must be present.  */
      if (resolve_assumed_size_actual (arg->expr))
	return false;

      /* Elemental procedure's array actual arguments must conform.  */
      if (e != NULL)
	{
	  if (!gfc_check_conformance (arg->expr, e, "elemental procedure"))
	    return false;
	}
      else
	e = arg->expr;
    }

  /* INTENT(OUT) is only allowed for subroutines; if any actual argument
     is an array, the intent inout/out variable needs to be also an array.  */
  if (rank > 0 && esym && expr == NULL)
    for (eformal = esym->formal, arg = arg0; arg && eformal;
	 arg = arg->next, eformal = eformal->next)
      if ((eformal->sym->attr.intent == INTENT_OUT
	   || eformal->sym->attr.intent == INTENT_INOUT)
	  && arg->expr && arg->expr->rank == 0)
	{
	  gfc_error ("Actual argument at %L for INTENT(%s) dummy %qs of "
		     "ELEMENTAL subroutine %qs is a scalar, but another "
		     "actual argument is an array", &arg->expr->where,
		     (eformal->sym->attr.intent == INTENT_OUT) ? "OUT"
		     : "INOUT", eformal->sym->name, esym->name);
	  return false;
	}
  return true;
}


/* This function does the checking of references to global procedures
   as defined in sections 18.1 and 14.1, respectively, of the Fortran
   77 and 95 standards.  It checks for a gsymbol for the name, making
   one if it does not already exist.  If it already exists, then the
   reference being resolved must correspond to the type of gsymbol.
   Otherwise, the new symbol is equipped with the attributes of the
   reference.  The corresponding code that is called in creating
   global entities is parse.c.

   In addition, for all but -std=legacy, the gsymbols are used to
   check the interfaces of external procedures from the same file.
   The namespace of the gsymbol is resolved and then, once this is
   done the interface is checked.  */


static bool
not_in_recursive (gfc_symbol *sym, gfc_namespace *gsym_ns)
{
  if (!gsym_ns->proc_name->attr.recursive)
    return true;

  if (sym->ns == gsym_ns)
    return false;

  if (sym->ns->parent && sym->ns->parent == gsym_ns)
    return false;

  return true;
}

static bool
not_entry_self_reference  (gfc_symbol *sym, gfc_namespace *gsym_ns)
{
  if (gsym_ns->entries)
    {
      gfc_entry_list *entry = gsym_ns->entries;

      for (; entry; entry = entry->next)
	{
	  if (strcmp (sym->name, entry->sym->name) == 0)
	    {
	      if (strcmp (gsym_ns->proc_name->name,
			  sym->ns->proc_name->name) == 0)
		return false;

	      if (sym->ns->parent
		  && strcmp (gsym_ns->proc_name->name,
			     sym->ns->parent->proc_name->name) == 0)
		return false;
	    }
	}
    }
  return true;
}


/* Check for the requirement of an explicit interface. F08:12.4.2.2.  */

bool
gfc_explicit_interface_required (gfc_symbol *sym, char *errmsg, int err_len)
{
  gfc_formal_arglist *arg = gfc_sym_get_dummy_args (sym);

  for ( ; arg; arg = arg->next)
    {
      if (!arg->sym)
	continue;

      if (arg->sym->attr.allocatable)  /* (2a)  */
	{
	  strncpy (errmsg, _("allocatable argument"), err_len);
	  return true;
	}
      else if (arg->sym->attr.asynchronous)
	{
	  strncpy (errmsg, _("asynchronous argument"), err_len);
	  return true;
	}
      else if (arg->sym->attr.optional)
	{
	  strncpy (errmsg, _("optional argument"), err_len);
	  return true;
	}
      else if (arg->sym->attr.pointer)
	{
	  strncpy (errmsg, _("pointer argument"), err_len);
	  return true;
	}
      else if (arg->sym->attr.target)
	{
	  strncpy (errmsg, _("target argument"), err_len);
	  return true;
	}
      else if (arg->sym->attr.value)
	{
	  strncpy (errmsg, _("value argument"), err_len);
	  return true;
	}
      else if (arg->sym->attr.volatile_)
	{
	  strncpy (errmsg, _("volatile argument"), err_len);
	  return true;
	}
      else if (arg->sym->as && arg->sym->as->type == AS_ASSUMED_SHAPE)  /* (2b)  */
	{
	  strncpy (errmsg, _("assumed-shape argument"), err_len);
	  return true;
	}
      else if (arg->sym->as && arg->sym->as->type == AS_ASSUMED_RANK)  /* TS 29113, 6.2.  */
	{
	  strncpy (errmsg, _("assumed-rank argument"), err_len);
	  return true;
	}
      else if (arg->sym->attr.codimension)  /* (2c)  */
	{
	  strncpy (errmsg, _("coarray argument"), err_len);
	  return true;
	}
      else if (false)  /* (2d) TODO: parametrized derived type  */
	{
	  strncpy (errmsg, _("parametrized derived type argument"), err_len);
	  return true;
	}
      else if (arg->sym->ts.type == BT_CLASS)  /* (2e)  */
	{
	  strncpy (errmsg, _("polymorphic argument"), err_len);
	  return true;
	}
      else if (arg->sym->attr.ext_attr & (1 << EXT_ATTR_NO_ARG_CHECK))
	{
	  strncpy (errmsg, _("NO_ARG_CHECK attribute"), err_len);
	  return true;
	}
      else if (arg->sym->ts.type == BT_ASSUMED)
	{
	  /* As assumed-type is unlimited polymorphic (cf. above).
	     See also TS 29113, Note 6.1.  */
	  strncpy (errmsg, _("assumed-type argument"), err_len);
	  return true;
	}
    }

  if (sym->attr.function)
    {
      gfc_symbol *res = sym->result ? sym->result : sym;

      if (res->attr.dimension)  /* (3a)  */
	{
	  strncpy (errmsg, _("array result"), err_len);
	  return true;
	}
      else if (res->attr.pointer || res->attr.allocatable)  /* (3b)  */
	{
	  strncpy (errmsg, _("pointer or allocatable result"), err_len);
	  return true;
	}
      else if (res->ts.type == BT_CHARACTER && res->ts.u.cl
	       && res->ts.u.cl->length
	       && res->ts.u.cl->length->expr_type != EXPR_CONSTANT)  /* (3c)  */
	{
	  strncpy (errmsg, _("result with non-constant character length"), err_len);
	  return true;
	}
    }

  if (sym->attr.elemental && !sym->attr.intrinsic)  /* (4)  */
    {
      strncpy (errmsg, _("elemental procedure"), err_len);
      return true;
    }
  else if (sym->attr.is_bind_c)  /* (5)  */
    {
      strncpy (errmsg, _("bind(c) procedure"), err_len);
      return true;
    }

  return false;
}


static void
resolve_global_procedure (gfc_symbol *sym, locus *where,
			  gfc_actual_arglist **actual, int sub)
{
  gfc_gsymbol * gsym;
  gfc_namespace *ns;
  enum gfc_symbol_type type;
  char reason[200];

  type = sub ? GSYM_SUBROUTINE : GSYM_FUNCTION;

  gsym = gfc_get_gsymbol (sym->binding_label ? sym->binding_label : sym->name);

  if ((gsym->type != GSYM_UNKNOWN && gsym->type != type))
    gfc_global_used (gsym, where);

  if ((sym->attr.if_source == IFSRC_UNKNOWN
       || sym->attr.if_source == IFSRC_IFBODY)
      && gsym->type != GSYM_UNKNOWN
      && !gsym->binding_label
      && gsym->ns
      && gsym->ns->resolved != -1
      && gsym->ns->proc_name
      && not_in_recursive (sym, gsym->ns)
      && not_entry_self_reference (sym, gsym->ns))
    {
      gfc_symbol *def_sym;

      /* Resolve the gsymbol namespace if needed.  */
      if (!gsym->ns->resolved)
	{
	  gfc_dt_list *old_dt_list;

	  /* Stash away derived types so that the backend_decls do not
	     get mixed up.  */
	  old_dt_list = gfc_derived_types;
	  gfc_derived_types = NULL;

	  gfc_resolve (gsym->ns);

	  /* Store the new derived types with the global namespace.  */
	  if (gfc_derived_types)
	    gsym->ns->derived_types = gfc_derived_types;

	  /* Restore the derived types of this namespace.  */
	  gfc_derived_types = old_dt_list;
	}

      /* Make sure that translation for the gsymbol occurs before
	 the procedure currently being resolved.  */
      ns = gfc_global_ns_list;
      for (; ns && ns != gsym->ns; ns = ns->sibling)
	{
	  if (ns->sibling == gsym->ns)
	    {
	      ns->sibling = gsym->ns->sibling;
	      gsym->ns->sibling = gfc_global_ns_list;
	      gfc_global_ns_list = gsym->ns;
	      break;
	    }
	}

      def_sym = gsym->ns->proc_name;

      /* This can happen if a binding name has been specified.  */
      if (gsym->binding_label && gsym->sym_name != def_sym->name)
	gfc_find_symbol (gsym->sym_name, gsym->ns, 0, &def_sym);

      if (def_sym->attr.entry_master)
	{
	  gfc_entry_list *entry;
	  for (entry = gsym->ns->entries; entry; entry = entry->next)
	    if (strcmp (entry->sym->name, sym->name) == 0)
	      {
		def_sym = entry->sym;
		break;
	      }
	}

      if (sym->attr.function && !gfc_compare_types (&sym->ts, &def_sym->ts))
	{
	  gfc_error ("Return type mismatch of function %qs at %L (%s/%s)",
		     sym->name, &sym->declared_at, gfc_typename (&sym->ts),
		     gfc_typename (&def_sym->ts));
	  goto done;
	}

      if (sym->attr.if_source == IFSRC_UNKNOWN
	  && gfc_explicit_interface_required (def_sym, reason, sizeof(reason)))
	{
	  gfc_error ("Explicit interface required for %qs at %L: %s",
		     sym->name, &sym->declared_at, reason);
	  goto done;
	}

      if (!pedantic && (gfc_option.allow_std & GFC_STD_GNU))
	/* Turn erros into warnings with -std=gnu and -std=legacy.  */
	gfc_errors_to_warnings (true);

      if (!gfc_compare_interfaces (sym, def_sym, sym->name, 0, 1,
				   reason, sizeof(reason), NULL, NULL))
	{
	  gfc_error_opt (OPT_Wargument_mismatch,
			 "Interface mismatch in global procedure %qs at %L:"
			 " %s", sym->name, &sym->declared_at, reason);
	  goto done;
	}

      if (!pedantic
	  || ((gfc_option.warn_std & GFC_STD_LEGACY)
	      && !(gfc_option.warn_std & GFC_STD_GNU)))
	gfc_errors_to_warnings (true);

      if (sym->attr.if_source != IFSRC_IFBODY)
	gfc_procedure_use (def_sym, actual, where);
    }

done:
  gfc_errors_to_warnings (false);

  if (gsym->type == GSYM_UNKNOWN)
    {
      gsym->type = type;
      gsym->where = *where;
    }

  gsym->used = 1;
}


/************* Function resolution *************/

/* Resolve a function call known to be generic.
   Section 14.1.2.4.1.  */

static match
resolve_generic_f0 (gfc_expr *expr, gfc_symbol *sym)
{
  gfc_symbol *s;

  if (sym->attr.generic)
    {
      s = gfc_search_interface (sym->generic, 0, &expr->value.function.actual);
      if (s != NULL)
	{
	  expr->value.function.name = s->name;
	  expr->value.function.esym = s;

	  if (s->ts.type != BT_UNKNOWN)
	    expr->ts = s->ts;
	  else if (s->result != NULL && s->result->ts.type != BT_UNKNOWN)
	    expr->ts = s->result->ts;

	  if (s->as != NULL)
	    expr->rank = s->as->rank;
	  else if (s->result != NULL && s->result->as != NULL)
	    expr->rank = s->result->as->rank;

	  gfc_set_sym_referenced (expr->value.function.esym);

	  return MATCH_YES;
	}

      /* TODO: Need to search for elemental references in generic
	 interface.  */
    }

  if (sym->attr.intrinsic)
    return gfc_intrinsic_func_interface (expr, 0);

  return MATCH_NO;
}


static bool
resolve_generic_f (gfc_expr *expr)
{
  gfc_symbol *sym;
  match m;
  gfc_interface *intr = NULL;

  sym = expr->symtree->n.sym;

  for (;;)
    {
      m = resolve_generic_f0 (expr, sym);
      if (m == MATCH_YES)
	return true;
      else if (m == MATCH_ERROR)
	return false;

generic:
      if (!intr)
	for (intr = sym->generic; intr; intr = intr->next)
	  if (gfc_fl_struct (intr->sym->attr.flavor))
	    break;

      if (sym->ns->parent == NULL)
	break;
      gfc_find_symbol (sym->name, sym->ns->parent, 1, &sym);

      if (sym == NULL)
	break;
      if (!generic_sym (sym))
	goto generic;
    }

  /* Last ditch attempt.  See if the reference is to an intrinsic
     that possesses a matching interface.  14.1.2.4  */
  if (sym  && !intr && !gfc_is_intrinsic (sym, 0, expr->where))
    {
      if (gfc_init_expr_flag)
	gfc_error ("Function %qs in initialization expression at %L "
		   "must be an intrinsic function",
		   expr->symtree->n.sym->name, &expr->where);
      else
	gfc_error ("There is no specific function for the generic %qs "
		   "at %L", expr->symtree->n.sym->name, &expr->where);
      return false;
    }

  if (intr)
    {
      if (!gfc_convert_to_structure_constructor (expr, intr->sym, NULL,
						 NULL, false))
	return false;
      return resolve_structure_cons (expr, 0);
    }

  m = gfc_intrinsic_func_interface (expr, 0);
  if (m == MATCH_YES)
    return true;

  if (m == MATCH_NO)
    gfc_error ("Generic function %qs at %L is not consistent with a "
	       "specific intrinsic interface", expr->symtree->n.sym->name,
	       &expr->where);

  return false;
}


/* Resolve a function call known to be specific.  */

static match
resolve_specific_f0 (gfc_symbol *sym, gfc_expr *expr)
{
  match m;

  if (sym->attr.external || sym->attr.if_source == IFSRC_IFBODY)
    {
      if (sym->attr.dummy)
	{
	  sym->attr.proc = PROC_DUMMY;
	  goto found;
	}

      sym->attr.proc = PROC_EXTERNAL;
      goto found;
    }

  if (sym->attr.proc == PROC_MODULE
      || sym->attr.proc == PROC_ST_FUNCTION
      || sym->attr.proc == PROC_INTERNAL)
    goto found;

  if (sym->attr.intrinsic)
    {
      m = gfc_intrinsic_func_interface (expr, 1);
      if (m == MATCH_YES)
	return MATCH_YES;
      if (m == MATCH_NO)
	gfc_error ("Function %qs at %L is INTRINSIC but is not compatible "
		   "with an intrinsic", sym->name, &expr->where);

      return MATCH_ERROR;
    }

  return MATCH_NO;

found:
  gfc_procedure_use (sym, &expr->value.function.actual, &expr->where);

  if (sym->result)
    expr->ts = sym->result->ts;
  else
    expr->ts = sym->ts;
  expr->value.function.name = sym->name;
  expr->value.function.esym = sym;
  /* Prevent crash when sym->ts.u.derived->components is not set due to previous
     error(s).  */
  if (sym->ts.type == BT_CLASS && !CLASS_DATA (sym))
    return MATCH_ERROR;
  if (sym->ts.type == BT_CLASS && CLASS_DATA (sym)->as)
    expr->rank = CLASS_DATA (sym)->as->rank;
  else if (sym->as != NULL)
    expr->rank = sym->as->rank;

  return MATCH_YES;
}


static bool
resolve_specific_f (gfc_expr *expr)
{
  gfc_symbol *sym;
  match m;

  sym = expr->symtree->n.sym;

  for (;;)
    {
      m = resolve_specific_f0 (sym, expr);
      if (m == MATCH_YES)
	return true;
      if (m == MATCH_ERROR)
	return false;

      if (sym->ns->parent == NULL)
	break;

      gfc_find_symbol (sym->name, sym->ns->parent, 1, &sym);

      if (sym == NULL)
	break;
    }

  gfc_error ("Unable to resolve the specific function %qs at %L",
	     expr->symtree->n.sym->name, &expr->where);

  return true;
}


/* Resolve a procedure call not known to be generic nor specific.  */

static bool
resolve_unknown_f (gfc_expr *expr)
{
  gfc_symbol *sym;
  gfc_typespec *ts;

  sym = expr->symtree->n.sym;

  if (sym->attr.dummy)
    {
      sym->attr.proc = PROC_DUMMY;
      expr->value.function.name = sym->name;
      goto set_type;
    }

  /* See if we have an intrinsic function reference.  */

  if (gfc_is_intrinsic (sym, 0, expr->where))
    {
      if (gfc_intrinsic_func_interface (expr, 1) == MATCH_YES)
	return true;
      return false;
    }

  /* The reference is to an external name.  */

  sym->attr.proc = PROC_EXTERNAL;
  expr->value.function.name = sym->name;
  expr->value.function.esym = expr->symtree->n.sym;

  if (sym->as != NULL)
    expr->rank = sym->as->rank;

  /* Type of the expression is either the type of the symbol or the
     default type of the symbol.  */

set_type:
  gfc_procedure_use (sym, &expr->value.function.actual, &expr->where);

  if (sym->ts.type != BT_UNKNOWN)
    expr->ts = sym->ts;
  else
    {
      ts = gfc_get_default_type (sym->name, sym->ns);

      if (ts->type == BT_UNKNOWN)
	{
	  gfc_error ("Function %qs at %L has no IMPLICIT type",
		     sym->name, &expr->where);
	  return false;
	}
      else
	expr->ts = *ts;
    }

  return true;
}


/* Return true, if the symbol is an external procedure.  */
static bool
is_external_proc (gfc_symbol *sym)
{
  if (!sym->attr.dummy && !sym->attr.contained
	&& !gfc_is_intrinsic (sym, sym->attr.subroutine, sym->declared_at)
	&& sym->attr.proc != PROC_ST_FUNCTION
	&& !sym->attr.proc_pointer
	&& !sym->attr.use_assoc
	&& sym->name)
    return true;

  return false;
}


/* Figure out if a function reference is pure or not.  Also set the name
   of the function for a potential error message.  Return nonzero if the
   function is PURE, zero if not.  */
static int
pure_stmt_function (gfc_expr *, gfc_symbol *);

static int
pure_function (gfc_expr *e, const char **name)
{
  int pure;
  gfc_component *comp;

  *name = NULL;

  if (e->symtree != NULL
        && e->symtree->n.sym != NULL
        && e->symtree->n.sym->attr.proc == PROC_ST_FUNCTION)
    return pure_stmt_function (e, e->symtree->n.sym);

  comp = gfc_get_proc_ptr_comp (e);
  if (comp)
    {
      pure = gfc_pure (comp->ts.interface);
      *name = comp->name;
    }
  else if (e->value.function.esym)
    {
      pure = gfc_pure (e->value.function.esym);
      *name = e->value.function.esym->name;
    }
  else if (e->value.function.isym)
    {
      pure = e->value.function.isym->pure
	     || e->value.function.isym->elemental;
      *name = e->value.function.isym->name;
    }
  else
    {
      /* Implicit functions are not pure.  */
      pure = 0;
      *name = e->value.function.name;
    }

  return pure;
}


static bool
impure_stmt_fcn (gfc_expr *e, gfc_symbol *sym,
		 int *f ATTRIBUTE_UNUSED)
{
  const char *name;

  /* Don't bother recursing into other statement functions
     since they will be checked individually for purity.  */
  if (e->expr_type != EXPR_FUNCTION
	|| !e->symtree
	|| e->symtree->n.sym == sym
	|| e->symtree->n.sym->attr.proc == PROC_ST_FUNCTION)
    return false;

  return pure_function (e, &name) ? false : true;
}


static int
pure_stmt_function (gfc_expr *e, gfc_symbol *sym)
{
  return gfc_traverse_expr (e, sym, impure_stmt_fcn, 0) ? 0 : 1;
}


/* Check if an impure function is allowed in the current context. */

static bool check_pure_function (gfc_expr *e)
{
  const char *name = NULL;
  if (!pure_function (e, &name) && name)
    {
      if (forall_flag)
	{
	  gfc_error ("Reference to impure function %qs at %L inside a "
		     "FORALL %s", name, &e->where,
		     forall_flag == 2 ? "mask" : "block");
	  return false;
	}
      else if (gfc_do_concurrent_flag)
	{
	  gfc_error ("Reference to impure function %qs at %L inside a "
		     "DO CONCURRENT %s", name, &e->where,
		     gfc_do_concurrent_flag == 2 ? "mask" : "block");
	  return false;
	}
      else if (gfc_pure (NULL))
	{
	  gfc_error ("Reference to impure function %qs at %L "
		     "within a PURE procedure", name, &e->where);
	  return false;
	}
      gfc_unset_implicit_pure (NULL);
    }
  return true;
}


/* Update current procedure's array_outer_dependency flag, considering
   a call to procedure SYM.  */

static void
update_current_proc_array_outer_dependency (gfc_symbol *sym)
{
  /* Check to see if this is a sibling function that has not yet
     been resolved.  */
  gfc_namespace *sibling = gfc_current_ns->sibling;
  for (; sibling; sibling = sibling->sibling)
    {
      if (sibling->proc_name == sym)
	{
	  gfc_resolve (sibling);
	  break;
	}
    }

  /* If SYM has references to outer arrays, so has the procedure calling
     SYM.  If SYM is a procedure pointer, we can assume the worst.  */
  if (sym->attr.array_outer_dependency
      || sym->attr.proc_pointer)
    gfc_current_ns->proc_name->attr.array_outer_dependency = 1;
}


/* Resolve a function call, which means resolving the arguments, then figuring
   out which entity the name refers to.  */

static bool
resolve_function (gfc_expr *expr)
{
  gfc_actual_arglist *arg;
  gfc_symbol *sym;
  bool t;
  int temp;
  procedure_type p = PROC_INTRINSIC;
  bool no_formal_args;

  sym = NULL;
  if (expr->symtree)
    sym = expr->symtree->n.sym;

  /* If this is a procedure pointer component, it has already been resolved.  */
  if (gfc_is_proc_ptr_comp (expr))
    return true;

  /* Avoid re-resolving the arguments of caf_get, which can lead to inserting
     another caf_get.  */
  if (sym && sym->attr.intrinsic
      && (sym->intmod_sym_id == GFC_ISYM_CAF_GET
	  || sym->intmod_sym_id == GFC_ISYM_CAF_SEND))
    return true;

  if (sym && sym->attr.intrinsic
      && !gfc_resolve_intrinsic (sym, &expr->where))
    return false;

  if (sym && (sym->attr.flavor == FL_VARIABLE || sym->attr.subroutine))
    {
      gfc_error ("%qs at %L is not a function", sym->name, &expr->where);
      return false;
    }

  /* If this ia a deferred TBP with an abstract interface (which may
     of course be referenced), expr->value.function.esym will be set.  */
  if (sym && sym->attr.abstract && !expr->value.function.esym)
    {
      gfc_error ("ABSTRACT INTERFACE %qs must not be referenced at %L",
		 sym->name, &expr->where);
      return false;
    }

  /* Switch off assumed size checking and do this again for certain kinds
     of procedure, once the procedure itself is resolved.  */
  need_full_assumed_size++;

  if (expr->symtree && expr->symtree->n.sym)
    p = expr->symtree->n.sym->attr.proc;

  if (expr->value.function.isym && expr->value.function.isym->inquiry)
    inquiry_argument = true;
  no_formal_args = sym && is_external_proc (sym)
  		       && gfc_sym_get_dummy_args (sym) == NULL;

  if (!resolve_actual_arglist (expr->value.function.actual,
			       p, no_formal_args))
    {
      inquiry_argument = false;
      return false;
    }

  inquiry_argument = false;

  /* Resume assumed_size checking.  */
  need_full_assumed_size--;

  /* If the procedure is external, check for usage.  */
  if (sym && is_external_proc (sym))
    resolve_global_procedure (sym, &expr->where,
			      &expr->value.function.actual, 0);

  if (sym && sym->ts.type == BT_CHARACTER
      && sym->ts.u.cl
      && sym->ts.u.cl->length == NULL
      && !sym->attr.dummy
      && !sym->ts.deferred
      && expr->value.function.esym == NULL
      && !sym->attr.contained)
    {
      /* Internal procedures are taken care of in resolve_contained_fntype.  */
      gfc_error ("Function %qs is declared CHARACTER(*) and cannot "
		 "be used at %L since it is not a dummy argument",
		 sym->name, &expr->where);
      return false;
    }

  /* See if function is already resolved.  */

  if (expr->value.function.name != NULL
      || expr->value.function.isym != NULL)
    {
      if (expr->ts.type == BT_UNKNOWN)
	expr->ts = sym->ts;
      t = true;
    }
  else
    {
      /* Apply the rules of section 14.1.2.  */

      switch (procedure_kind (sym))
	{
	case PTYPE_GENERIC:
	  t = resolve_generic_f (expr);
	  break;

	case PTYPE_SPECIFIC:
	  t = resolve_specific_f (expr);
	  break;

	case PTYPE_UNKNOWN:
	  t = resolve_unknown_f (expr);
	  break;

	default:
	  gfc_internal_error ("resolve_function(): bad function type");
	}
    }

  /* If the expression is still a function (it might have simplified),
     then we check to see if we are calling an elemental function.  */

  if (expr->expr_type != EXPR_FUNCTION)
    return t;

  temp = need_full_assumed_size;
  need_full_assumed_size = 0;

  if (!resolve_elemental_actual (expr, NULL))
    return false;

  if (omp_workshare_flag
      && expr->value.function.esym
      && ! gfc_elemental (expr->value.function.esym))
    {
      gfc_error ("User defined non-ELEMENTAL function %qs at %L not allowed "
		 "in WORKSHARE construct", expr->value.function.esym->name,
		 &expr->where);
      t = false;
    }

#define GENERIC_ID expr->value.function.isym->id
  else if (expr->value.function.actual != NULL
	   && expr->value.function.isym != NULL
	   && GENERIC_ID != GFC_ISYM_LBOUND
	   && GENERIC_ID != GFC_ISYM_LCOBOUND
	   && GENERIC_ID != GFC_ISYM_UCOBOUND
	   && GENERIC_ID != GFC_ISYM_LEN
	   && GENERIC_ID != GFC_ISYM_LOC
	   && GENERIC_ID != GFC_ISYM_C_LOC
	   && GENERIC_ID != GFC_ISYM_PRESENT)
    {
      /* Array intrinsics must also have the last upper bound of an
	 assumed size array argument.  UBOUND and SIZE have to be
	 excluded from the check if the second argument is anything
	 than a constant.  */

      for (arg = expr->value.function.actual; arg; arg = arg->next)
	{
	  if ((GENERIC_ID == GFC_ISYM_UBOUND || GENERIC_ID == GFC_ISYM_SIZE)
	      && arg == expr->value.function.actual
	      && arg->next != NULL && arg->next->expr)
	    {
	      if (arg->next->expr->expr_type != EXPR_CONSTANT)
		break;

	      if (arg->next->name && strncmp (arg->next->name, "kind", 4) == 0)
		break;

	      if ((int)mpz_get_si (arg->next->expr->value.integer)
			< arg->expr->rank)
		break;
	    }

	  if (arg->expr != NULL
	      && arg->expr->rank > 0
	      && resolve_assumed_size_actual (arg->expr))
	    return false;
	}
    }
#undef GENERIC_ID

  need_full_assumed_size = temp;

  if (!check_pure_function(expr))
    t = false;

  /* Functions without the RECURSIVE attribution are not allowed to
   * call themselves.  */
  if (expr->value.function.esym && !expr->value.function.esym->attr.recursive)
    {
      gfc_symbol *esym;
      esym = expr->value.function.esym;

      if (is_illegal_recursion (esym, gfc_current_ns))
      {
	if (esym->attr.entry && esym->ns->entries)
	  gfc_error ("ENTRY %qs at %L cannot be called recursively, as"
		     " function %qs is not RECURSIVE",
		     esym->name, &expr->where, esym->ns->entries->sym->name);
	else
	  gfc_error ("Function %qs at %L cannot be called recursively, as it"
		     " is not RECURSIVE", esym->name, &expr->where);

	t = false;
      }
    }

  /* Character lengths of use associated functions may contains references to
     symbols not referenced from the current program unit otherwise.  Make sure
     those symbols are marked as referenced.  */

  if (expr->ts.type == BT_CHARACTER && expr->value.function.esym
      && expr->value.function.esym->attr.use_assoc)
    {
      gfc_expr_set_symbols_referenced (expr->ts.u.cl->length);
    }

  /* Make sure that the expression has a typespec that works.  */
  if (expr->ts.type == BT_UNKNOWN)
    {
      if (expr->symtree->n.sym->result
	    && expr->symtree->n.sym->result->ts.type != BT_UNKNOWN
	    && !expr->symtree->n.sym->result->attr.proc_pointer)
	expr->ts = expr->symtree->n.sym->result->ts;
    }

  if (!expr->ref && !expr->value.function.isym)
    {
      if (expr->value.function.esym)
	update_current_proc_array_outer_dependency (expr->value.function.esym);
      else
	update_current_proc_array_outer_dependency (sym);
    }
  else if (expr->ref)
    /* typebound procedure: Assume the worst.  */
    gfc_current_ns->proc_name->attr.array_outer_dependency = 1;

  return t;
}


/************* Subroutine resolution *************/

static bool
pure_subroutine (gfc_symbol *sym, const char *name, locus *loc)
{
  if (gfc_pure (sym))
    return true;

  if (forall_flag)
    {
      gfc_error ("Subroutine call to %qs in FORALL block at %L is not PURE",
		 name, loc);
      return false;
    }
  else if (gfc_do_concurrent_flag)
    {
      gfc_error ("Subroutine call to %qs in DO CONCURRENT block at %L is not "
		 "PURE", name, loc);
      return false;
    }
  else if (gfc_pure (NULL))
    {
      gfc_error ("Subroutine call to %qs at %L is not PURE", name, loc);
      return false;
    }

  gfc_unset_implicit_pure (NULL);
  return true;
}


static match
resolve_generic_s0 (gfc_code *c, gfc_symbol *sym)
{
  gfc_symbol *s;

  if (sym->attr.generic)
    {
      s = gfc_search_interface (sym->generic, 1, &c->ext.actual);
      if (s != NULL)
	{
	  c->resolved_sym = s;
	  if (!pure_subroutine (s, s->name, &c->loc))
	    return MATCH_ERROR;
	  return MATCH_YES;
	}

      /* TODO: Need to search for elemental references in generic interface.  */
    }

  if (sym->attr.intrinsic)
    return gfc_intrinsic_sub_interface (c, 0);

  return MATCH_NO;
}


static bool
resolve_generic_s (gfc_code *c)
{
  gfc_symbol *sym;
  match m;

  sym = c->symtree->n.sym;

  for (;;)
    {
      m = resolve_generic_s0 (c, sym);
      if (m == MATCH_YES)
	return true;
      else if (m == MATCH_ERROR)
	return false;

generic:
      if (sym->ns->parent == NULL)
	break;
      gfc_find_symbol (sym->name, sym->ns->parent, 1, &sym);

      if (sym == NULL)
	break;
      if (!generic_sym (sym))
	goto generic;
    }

  /* Last ditch attempt.  See if the reference is to an intrinsic
     that possesses a matching interface.  14.1.2.4  */
  sym = c->symtree->n.sym;

  if (!gfc_is_intrinsic (sym, 1, c->loc))
    {
      gfc_error ("There is no specific subroutine for the generic %qs at %L",
		 sym->name, &c->loc);
      return false;
    }

  m = gfc_intrinsic_sub_interface (c, 0);
  if (m == MATCH_YES)
    return true;
  if (m == MATCH_NO)
    gfc_error ("Generic subroutine %qs at %L is not consistent with an "
	       "intrinsic subroutine interface", sym->name, &c->loc);

  return false;
}


/* Resolve a subroutine call known to be specific.  */

static match
resolve_specific_s0 (gfc_code *c, gfc_symbol *sym)
{
  match m;

  if (sym->attr.external || sym->attr.if_source == IFSRC_IFBODY)
    {
      if (sym->attr.dummy)
	{
	  sym->attr.proc = PROC_DUMMY;
	  goto found;
	}

      sym->attr.proc = PROC_EXTERNAL;
      goto found;
    }

  if (sym->attr.proc == PROC_MODULE || sym->attr.proc == PROC_INTERNAL)
    goto found;

  if (sym->attr.intrinsic)
    {
      m = gfc_intrinsic_sub_interface (c, 1);
      if (m == MATCH_YES)
	return MATCH_YES;
      if (m == MATCH_NO)
	gfc_error ("Subroutine %qs at %L is INTRINSIC but is not compatible "
		   "with an intrinsic", sym->name, &c->loc);

      return MATCH_ERROR;
    }

  return MATCH_NO;

found:
  gfc_procedure_use (sym, &c->ext.actual, &c->loc);

  c->resolved_sym = sym;
  if (!pure_subroutine (sym, sym->name, &c->loc))
    return MATCH_ERROR;

  return MATCH_YES;
}


static bool
resolve_specific_s (gfc_code *c)
{
  gfc_symbol *sym;
  match m;

  sym = c->symtree->n.sym;

  for (;;)
    {
      m = resolve_specific_s0 (c, sym);
      if (m == MATCH_YES)
	return true;
      if (m == MATCH_ERROR)
	return false;

      if (sym->ns->parent == NULL)
	break;

      gfc_find_symbol (sym->name, sym->ns->parent, 1, &sym);

      if (sym == NULL)
	break;
    }

  sym = c->symtree->n.sym;
  gfc_error ("Unable to resolve the specific subroutine %qs at %L",
	     sym->name, &c->loc);

  return false;
}


/* Resolve a subroutine call not known to be generic nor specific.  */

static bool
resolve_unknown_s (gfc_code *c)
{
  gfc_symbol *sym;

  sym = c->symtree->n.sym;

  if (sym->attr.dummy)
    {
      sym->attr.proc = PROC_DUMMY;
      goto found;
    }

  /* See if we have an intrinsic function reference.  */

  if (gfc_is_intrinsic (sym, 1, c->loc))
    {
      if (gfc_intrinsic_sub_interface (c, 1) == MATCH_YES)
	return true;
      return false;
    }

  /* The reference is to an external name.  */

found:
  gfc_procedure_use (sym, &c->ext.actual, &c->loc);

  c->resolved_sym = sym;

  return pure_subroutine (sym, sym->name, &c->loc);
}


/* Resolve a subroutine call.  Although it was tempting to use the same code
   for functions, subroutines and functions are stored differently and this
   makes things awkward.  */

static bool
resolve_call (gfc_code *c)
{
  bool t;
  procedure_type ptype = PROC_INTRINSIC;
  gfc_symbol *csym, *sym;
  bool no_formal_args;

  csym = c->symtree ? c->symtree->n.sym : NULL;

  if (csym && csym->ts.type != BT_UNKNOWN)
    {
      gfc_error ("%qs at %L has a type, which is not consistent with "
		 "the CALL at %L", csym->name, &csym->declared_at, &c->loc);
      return false;
    }

  if (csym && gfc_current_ns->parent && csym->ns != gfc_current_ns)
    {
      gfc_symtree *st;
      gfc_find_sym_tree (c->symtree->name, gfc_current_ns, 1, &st);
      sym = st ? st->n.sym : NULL;
      if (sym && csym != sym
	      && sym->ns == gfc_current_ns
	      && sym->attr.flavor == FL_PROCEDURE
	      && sym->attr.contained)
	{
	  sym->refs++;
	  if (csym->attr.generic)
	    c->symtree->n.sym = sym;
	  else
	    c->symtree = st;
	  csym = c->symtree->n.sym;
	}
    }

  /* If this ia a deferred TBP, c->expr1 will be set.  */
  if (!c->expr1 && csym)
    {
      if (csym->attr.abstract)
	{
	  gfc_error ("ABSTRACT INTERFACE %qs must not be referenced at %L",
		    csym->name, &c->loc);
	  return false;
	}

      /* Subroutines without the RECURSIVE attribution are not allowed to
	 call themselves.  */
      if (is_illegal_recursion (csym, gfc_current_ns))
	{
	  if (csym->attr.entry && csym->ns->entries)
	    gfc_error ("ENTRY %qs at %L cannot be called recursively, "
		       "as subroutine %qs is not RECURSIVE",
		       csym->name, &c->loc, csym->ns->entries->sym->name);
	  else
	    gfc_error ("SUBROUTINE %qs at %L cannot be called recursively, "
		       "as it is not RECURSIVE", csym->name, &c->loc);

	  t = false;
	}
    }

  /* Switch off assumed size checking and do this again for certain kinds
     of procedure, once the procedure itself is resolved.  */
  need_full_assumed_size++;

  if (csym)
    ptype = csym->attr.proc;

  no_formal_args = csym && is_external_proc (csym)
			&& gfc_sym_get_dummy_args (csym) == NULL;
  if (!resolve_actual_arglist (c->ext.actual, ptype, no_formal_args))
    return false;

  /* Resume assumed_size checking.  */
  need_full_assumed_size--;

  /* If external, check for usage.  */
  if (csym && is_external_proc (csym))
    resolve_global_procedure (csym, &c->loc, &c->ext.actual, 1);

  t = true;
  if (c->resolved_sym == NULL)
    {
      c->resolved_isym = NULL;
      switch (procedure_kind (csym))
	{
	case PTYPE_GENERIC:
	  t = resolve_generic_s (c);
	  break;

	case PTYPE_SPECIFIC:
	  t = resolve_specific_s (c);
	  break;

	case PTYPE_UNKNOWN:
	  t = resolve_unknown_s (c);
	  break;

	default:
	  gfc_internal_error ("resolve_subroutine(): bad function type");
	}
    }

  /* Some checks of elemental subroutine actual arguments.  */
  if (!resolve_elemental_actual (NULL, c))
    return false;

  if (!c->expr1)
    update_current_proc_array_outer_dependency (csym);
  else
    /* Typebound procedure: Assume the worst.  */
    gfc_current_ns->proc_name->attr.array_outer_dependency = 1;

  return t;
}


/* Compare the shapes of two arrays that have non-NULL shapes.  If both
   op1->shape and op2->shape are non-NULL return true if their shapes
   match.  If both op1->shape and op2->shape are non-NULL return false
   if their shapes do not match.  If either op1->shape or op2->shape is
   NULL, return true.  */

static bool
compare_shapes (gfc_expr *op1, gfc_expr *op2)
{
  bool t;
  int i;

  t = true;

  if (op1->shape != NULL && op2->shape != NULL)
    {
      for (i = 0; i < op1->rank; i++)
	{
	  if (mpz_cmp (op1->shape[i], op2->shape[i]) != 0)
	   {
	     gfc_error ("Shapes for operands at %L and %L are not conformable",
			&op1->where, &op2->where);
	     t = false;
	     break;
	   }
	}
    }

  return t;
}

/* Convert a logical operator to the corresponding bitwise intrinsic call.
   For example A .AND. B becomes IAND(A, B).  */
static gfc_expr *
logical_to_bitwise (gfc_expr *e)
{
  gfc_expr *tmp, *op1, *op2;
  gfc_isym_id isym;
  gfc_actual_arglist *args = NULL;

  gcc_assert (e->expr_type == EXPR_OP);

  isym = GFC_ISYM_NONE;
  op1 = e->value.op.op1;
  op2 = e->value.op.op2;

  switch (e->value.op.op)
    {
    case INTRINSIC_NOT:
      isym = GFC_ISYM_NOT;
      break;
    case INTRINSIC_AND:
      isym = GFC_ISYM_IAND;
      break;
    case INTRINSIC_OR:
      isym = GFC_ISYM_IOR;
      break;
    case INTRINSIC_NEQV:
      isym = GFC_ISYM_IEOR;
      break;
    case INTRINSIC_EQV:
      /* "Bitwise eqv" is just the complement of NEQV === IEOR.
	 Change the old expression to NEQV, which will get replaced by IEOR,
	 and wrap it in NOT.  */
      tmp = gfc_copy_expr (e);
      tmp->value.op.op = INTRINSIC_NEQV;
      tmp = logical_to_bitwise (tmp);
      isym = GFC_ISYM_NOT;
      op1 = tmp;
      op2 = NULL;
      break;
    default:
      gfc_internal_error ("logical_to_bitwise(): Bad intrinsic");
    }

  /* Inherit the original operation's operands as arguments.  */
  args = gfc_get_actual_arglist ();
  args->expr = op1;
  if (op2)
    {
      args->next = gfc_get_actual_arglist ();
      args->next->expr = op2;
    }

  /* Convert the expression to a function call.  */
  e->expr_type = EXPR_FUNCTION;
  e->value.function.actual = args;
  e->value.function.isym = gfc_intrinsic_function_by_id (isym);
  e->value.function.name = e->value.function.isym->name;
  e->value.function.esym = NULL;

  /* Make up a pre-resolved function call symtree if we need to.  */
  if (!e->symtree || !e->symtree->n.sym)
    {
      gfc_symbol *sym;
      gfc_get_ha_sym_tree (e->value.function.isym->name, &e->symtree);
      sym = e->symtree->n.sym;
      sym->result = sym;
      sym->attr.flavor = FL_PROCEDURE;
      sym->attr.function = 1;
      sym->attr.elemental = 1;
      sym->attr.pure = 1;
      sym->attr.referenced = 1;
      gfc_intrinsic_symbol (sym);
      gfc_commit_symbol (sym);
    }

  args->name = e->value.function.isym->formal->name;
  if (e->value.function.isym->formal->next)
    args->next->name = e->value.function.isym->formal->next->name;

  return e;
}

/* Resolve an operator expression node.  This can involve replacing the
   operation with a user defined function call.  */

static bool
resolve_operator (gfc_expr *e)
{
  gfc_expr *op1, *op2;
  char msg[200];
  bool dual_locus_error;
  bool t;

  /* Resolve all subnodes-- give them types.  */

  switch (e->value.op.op)
    {
    default:
      if (!gfc_resolve_expr (e->value.op.op2))
	return false;

    /* Fall through.  */

    case INTRINSIC_NOT:
    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:
    case INTRINSIC_PARENTHESES:
      if (!gfc_resolve_expr (e->value.op.op1))
	return false;
      break;
    }

  /* Typecheck the new node.  */

  op1 = e->value.op.op1;
  op2 = e->value.op.op2;
  dual_locus_error = false;

  if ((op1 && op1->expr_type == EXPR_NULL)
      || (op2 && op2->expr_type == EXPR_NULL))
    {
      sprintf (msg, _("Invalid context for NULL() pointer at %%L"));
      goto bad_op;
    }

  switch (e->value.op.op)
    {
    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:
      if (op1->ts.type == BT_INTEGER
	  || op1->ts.type == BT_REAL
	  || op1->ts.type == BT_COMPLEX)
	{
	  e->ts = op1->ts;
	  break;
	}

      sprintf (msg, _("Operand of unary numeric operator %%<%s%%> at %%L is %s"),
	       gfc_op2string (e->value.op.op), gfc_typename (&e->ts));
      goto bad_op;

    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
    case INTRINSIC_POWER:
      if (gfc_numeric_ts (&op1->ts) && gfc_numeric_ts (&op2->ts))
	{
	  gfc_type_convert_binary (e, 1);
	  break;
	}

      sprintf (msg,
	       _("Operands of binary numeric operator %%<%s%%> at %%L are %s/%s"),
	       gfc_op2string (e->value.op.op), gfc_typename (&op1->ts),
	       gfc_typename (&op2->ts));
      goto bad_op;

    case INTRINSIC_CONCAT:
      if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER
	  && op1->ts.kind == op2->ts.kind)
	{
	  e->ts.type = BT_CHARACTER;
	  e->ts.kind = op1->ts.kind;
	  break;
	}

      sprintf (msg,
	       _("Operands of string concatenation operator at %%L are %s/%s"),
	       gfc_typename (&op1->ts), gfc_typename (&op2->ts));
      goto bad_op;

    case INTRINSIC_AND:
    case INTRINSIC_OR:
    case INTRINSIC_EQV:
    case INTRINSIC_NEQV:
      if (op1->ts.type == BT_LOGICAL && op2->ts.type == BT_LOGICAL)
	{
	  e->ts.type = BT_LOGICAL;
	  e->ts.kind = gfc_kind_max (op1, op2);
	  if (op1->ts.kind < e->ts.kind)
	    gfc_convert_type (op1, &e->ts, 2);
	  else if (op2->ts.kind < e->ts.kind)
	    gfc_convert_type (op2, &e->ts, 2);
	  break;
	}

      /* Logical ops on integers become bitwise ops with -fdec.  */
      else if (flag_dec
	       && (op1->ts.type == BT_INTEGER || op2->ts.type == BT_INTEGER))
	{
	  e->ts.type = BT_INTEGER;
	  e->ts.kind = gfc_kind_max (op1, op2);
	  if (op1->ts.type != e->ts.type || op1->ts.kind != e->ts.kind)
	    gfc_convert_type (op1, &e->ts, 1);
	  if (op2->ts.type != e->ts.type || op2->ts.kind != e->ts.kind)
	    gfc_convert_type (op2, &e->ts, 1);
	  e = logical_to_bitwise (e);
	  return resolve_function (e);
	}

      sprintf (msg, _("Operands of logical operator %%<%s%%> at %%L are %s/%s"),
	       gfc_op2string (e->value.op.op), gfc_typename (&op1->ts),
	       gfc_typename (&op2->ts));

      goto bad_op;

    case INTRINSIC_NOT:
      /* Logical ops on integers become bitwise ops with -fdec.  */
      if (flag_dec && op1->ts.type == BT_INTEGER)
	{
	  e->ts.type = BT_INTEGER;
	  e->ts.kind = op1->ts.kind;
	  e = logical_to_bitwise (e);
	  return resolve_function (e);
	}

      if (op1->ts.type == BT_LOGICAL)
	{
	  e->ts.type = BT_LOGICAL;
	  e->ts.kind = op1->ts.kind;
	  break;
	}

      sprintf (msg, _("Operand of .not. operator at %%L is %s"),
	       gfc_typename (&op1->ts));
      goto bad_op;

    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:
      if (op1->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX)
	{
	  strcpy (msg, _("COMPLEX quantities cannot be compared at %L"));
	  goto bad_op;
	}

      /* Fall through.  */

    case INTRINSIC_EQ:
    case INTRINSIC_EQ_OS:
    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
      if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER
	  && op1->ts.kind == op2->ts.kind)
	{
	  e->ts.type = BT_LOGICAL;
	  e->ts.kind = gfc_default_logical_kind;
	  break;
	}

      if (gfc_numeric_ts (&op1->ts) && gfc_numeric_ts (&op2->ts))
	{
	  gfc_type_convert_binary (e, 1);

	  e->ts.type = BT_LOGICAL;
	  e->ts.kind = gfc_default_logical_kind;

	  if (warn_compare_reals)
	    {
	      gfc_intrinsic_op op = e->value.op.op;

	      /* Type conversion has made sure that the types of op1 and op2
		 agree, so it is only necessary to check the first one.   */
	      if ((op1->ts.type == BT_REAL || op1->ts.type == BT_COMPLEX)
		  && (op == INTRINSIC_EQ || op == INTRINSIC_EQ_OS
		      || op == INTRINSIC_NE || op == INTRINSIC_NE_OS))
		{
		  const char *msg;

		  if (op == INTRINSIC_EQ || op == INTRINSIC_EQ_OS)
		    msg = "Equality comparison for %s at %L";
		  else
		    msg = "Inequality comparison for %s at %L";

		  gfc_warning (OPT_Wcompare_reals, msg,
			       gfc_typename (&op1->ts), &op1->where);
		}
	    }

	  break;
	}

      if (op1->ts.type == BT_LOGICAL && op2->ts.type == BT_LOGICAL)
	sprintf (msg,
		 _("Logicals at %%L must be compared with %s instead of %s"),
		 (e->value.op.op == INTRINSIC_EQ
		  || e->value.op.op == INTRINSIC_EQ_OS)
		 ? ".eqv." : ".neqv.", gfc_op2string (e->value.op.op));
      else
	sprintf (msg,
		 _("Operands of comparison operator %%<%s%%> at %%L are %s/%s"),
		 gfc_op2string (e->value.op.op), gfc_typename (&op1->ts),
		 gfc_typename (&op2->ts));

      goto bad_op;

    case INTRINSIC_USER:
      if (e->value.op.uop->op == NULL)
	sprintf (msg, _("Unknown operator %%<%s%%> at %%L"),
		 e->value.op.uop->name);
      else if (op2 == NULL)
	sprintf (msg, _("Operand of user operator %%<%s%%> at %%L is %s"),
		 e->value.op.uop->name, gfc_typename (&op1->ts));
      else
	{
	  sprintf (msg, _("Operands of user operator %%<%s%%> at %%L are %s/%s"),
		   e->value.op.uop->name, gfc_typename (&op1->ts),
		   gfc_typename (&op2->ts));
	  e->value.op.uop->op->sym->attr.referenced = 1;
	}

      goto bad_op;

    case INTRINSIC_PARENTHESES:
      e->ts = op1->ts;
      if (e->ts.type == BT_CHARACTER)
	e->ts.u.cl = op1->ts.u.cl;
      break;

    default:
      gfc_internal_error ("resolve_operator(): Bad intrinsic");
    }

  /* Deal with arrayness of an operand through an operator.  */

  t = true;

  switch (e->value.op.op)
    {
    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
    case INTRINSIC_POWER:
    case INTRINSIC_CONCAT:
    case INTRINSIC_AND:
    case INTRINSIC_OR:
    case INTRINSIC_EQV:
    case INTRINSIC_NEQV:
    case INTRINSIC_EQ:
    case INTRINSIC_EQ_OS:
    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:

      if (op1->rank == 0 && op2->rank == 0)
	e->rank = 0;

      if (op1->rank == 0 && op2->rank != 0)
	{
	  e->rank = op2->rank;

	  if (e->shape == NULL)
	    e->shape = gfc_copy_shape (op2->shape, op2->rank);
	}

      if (op1->rank != 0 && op2->rank == 0)
	{
	  e->rank = op1->rank;

	  if (e->shape == NULL)
	    e->shape = gfc_copy_shape (op1->shape, op1->rank);
	}

      if (op1->rank != 0 && op2->rank != 0)
	{
	  if (op1->rank == op2->rank)
	    {
	      e->rank = op1->rank;
	      if (e->shape == NULL)
		{
		  t = compare_shapes (op1, op2);
		  if (!t)
		    e->shape = NULL;
		  else
		    e->shape = gfc_copy_shape (op1->shape, op1->rank);
		}
	    }
	  else
	    {
	      /* Allow higher level expressions to work.  */
	      e->rank = 0;

	      /* Try user-defined operators, and otherwise throw an error.  */
	      dual_locus_error = true;
	      sprintf (msg,
		       _("Inconsistent ranks for operator at %%L and %%L"));
	      goto bad_op;
	    }
	}

      break;

    case INTRINSIC_PARENTHESES:
    case INTRINSIC_NOT:
    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:
      /* Simply copy arrayness attribute */
      e->rank = op1->rank;

      if (e->shape == NULL)
	e->shape = gfc_copy_shape (op1->shape, op1->rank);

      break;

    default:
      break;
    }

  /* Attempt to simplify the expression.  */
  if (t)
    {
      t = gfc_simplify_expr (e, 0);
      /* Some calls do not succeed in simplification and return false
	 even though there is no error; e.g. variable references to
	 PARAMETER arrays.  */
      if (!gfc_is_constant_expr (e))
	t = true;
    }
  return t;

bad_op:

  {
    match m = gfc_extend_expr (e);
    if (m == MATCH_YES)
      return true;
    if (m == MATCH_ERROR)
      return false;
  }

  if (dual_locus_error)
    gfc_error (msg, &op1->where, &op2->where);
  else
    gfc_error (msg, &e->where);

  return false;
}


/************** Array resolution subroutines **************/

enum compare_result
{ CMP_LT, CMP_EQ, CMP_GT, CMP_UNKNOWN };

/* Compare two integer expressions.  */

static compare_result
compare_bound (gfc_expr *a, gfc_expr *b)
{
  int i;

  if (a == NULL || a->expr_type != EXPR_CONSTANT
      || b == NULL || b->expr_type != EXPR_CONSTANT)
    return CMP_UNKNOWN;

  /* If either of the types isn't INTEGER, we must have
     raised an error earlier.  */

  if (a->ts.type != BT_INTEGER || b->ts.type != BT_INTEGER)
    return CMP_UNKNOWN;

  i = mpz_cmp (a->value.integer, b->value.integer);

  if (i < 0)
    return CMP_LT;
  if (i > 0)
    return CMP_GT;
  return CMP_EQ;
}


/* Compare an integer expression with an integer.  */

static compare_result
compare_bound_int (gfc_expr *a, int b)
{
  int i;

  if (a == NULL || a->expr_type != EXPR_CONSTANT)
    return CMP_UNKNOWN;

  if (a->ts.type != BT_INTEGER)
    gfc_internal_error ("compare_bound_int(): Bad expression");

  i = mpz_cmp_si (a->value.integer, b);

  if (i < 0)
    return CMP_LT;
  if (i > 0)
    return CMP_GT;
  return CMP_EQ;
}


/* Compare an integer expression with a mpz_t.  */

static compare_result
compare_bound_mpz_t (gfc_expr *a, mpz_t b)
{
  int i;

  if (a == NULL || a->expr_type != EXPR_CONSTANT)
    return CMP_UNKNOWN;

  if (a->ts.type != BT_INTEGER)
    gfc_internal_error ("compare_bound_int(): Bad expression");

  i = mpz_cmp (a->value.integer, b);

  if (i < 0)
    return CMP_LT;
  if (i > 0)
    return CMP_GT;
  return CMP_EQ;
}


/* Compute the last value of a sequence given by a triplet.
   Return 0 if it wasn't able to compute the last value, or if the
   sequence if empty, and 1 otherwise.  */

static int
compute_last_value_for_triplet (gfc_expr *start, gfc_expr *end,
				gfc_expr *stride, mpz_t last)
{
  mpz_t rem;

  if (start == NULL || start->expr_type != EXPR_CONSTANT
      || end == NULL || end->expr_type != EXPR_CONSTANT
      || (stride != NULL && stride->expr_type != EXPR_CONSTANT))
    return 0;

  if (start->ts.type != BT_INTEGER || end->ts.type != BT_INTEGER
      || (stride != NULL && stride->ts.type != BT_INTEGER))
    return 0;

  if (stride == NULL || compare_bound_int (stride, 1) == CMP_EQ)
    {
      if (compare_bound (start, end) == CMP_GT)
	return 0;
      mpz_set (last, end->value.integer);
      return 1;
    }

  if (compare_bound_int (stride, 0) == CMP_GT)
    {
      /* Stride is positive */
      if (mpz_cmp (start->value.integer, end->value.integer) > 0)
	return 0;
    }
  else
    {
      /* Stride is negative */
      if (mpz_cmp (start->value.integer, end->value.integer) < 0)
	return 0;
    }

  mpz_init (rem);
  mpz_sub (rem, end->value.integer, start->value.integer);
  mpz_tdiv_r (rem, rem, stride->value.integer);
  mpz_sub (last, end->value.integer, rem);
  mpz_clear (rem);

  return 1;
}


/* Compare a single dimension of an array reference to the array
   specification.  */

static bool
check_dimension (int i, gfc_array_ref *ar, gfc_array_spec *as)
{
  mpz_t last_value;

  if (ar->dimen_type[i] == DIMEN_STAR)
    {
      gcc_assert (ar->stride[i] == NULL);
      /* This implies [*] as [*:] and [*:3] are not possible.  */
      if (ar->start[i] == NULL)
	{
	  gcc_assert (ar->end[i] == NULL);
	  return true;
	}
    }

/* Given start, end and stride values, calculate the minimum and
   maximum referenced indexes.  */

  switch (ar->dimen_type[i])
    {
    case DIMEN_VECTOR:
    case DIMEN_THIS_IMAGE:
      break;

    case DIMEN_STAR:
    case DIMEN_ELEMENT:
      if (compare_bound (ar->start[i], as->lower[i]) == CMP_LT)
	{
	  if (i < as->rank)
	    gfc_warning (0, "Array reference at %L is out of bounds "
			 "(%ld < %ld) in dimension %d", &ar->c_where[i],
			 mpz_get_si (ar->start[i]->value.integer),
			 mpz_get_si (as->lower[i]->value.integer), i+1);
	  else
	    gfc_warning (0, "Array reference at %L is out of bounds "
			 "(%ld < %ld) in codimension %d", &ar->c_where[i],
			 mpz_get_si (ar->start[i]->value.integer),
			 mpz_get_si (as->lower[i]->value.integer),
			 i + 1 - as->rank);
	  return true;
	}
      if (compare_bound (ar->start[i], as->upper[i]) == CMP_GT)
	{
	  if (i < as->rank)
	    gfc_warning (0, "Array reference at %L is out of bounds "
			 "(%ld > %ld) in dimension %d", &ar->c_where[i],
			 mpz_get_si (ar->start[i]->value.integer),
			 mpz_get_si (as->upper[i]->value.integer), i+1);
	  else
	    gfc_warning (0, "Array reference at %L is out of bounds "
			 "(%ld > %ld) in codimension %d", &ar->c_where[i],
			 mpz_get_si (ar->start[i]->value.integer),
			 mpz_get_si (as->upper[i]->value.integer),
			 i + 1 - as->rank);
	  return true;
	}

      break;

    case DIMEN_RANGE:
      {
#define AR_START (ar->start[i] ? ar->start[i] : as->lower[i])
#define AR_END (ar->end[i] ? ar->end[i] : as->upper[i])

	compare_result comp_start_end = compare_bound (AR_START, AR_END);

	/* Check for zero stride, which is not allowed.  */
	if (compare_bound_int (ar->stride[i], 0) == CMP_EQ)
	  {
	    gfc_error ("Illegal stride of zero at %L", &ar->c_where[i]);
	    return false;
	  }

	/* if start == len || (stride > 0 && start < len)
			   || (stride < 0 && start > len),
	   then the array section contains at least one element.  In this
	   case, there is an out-of-bounds access if
	   (start < lower || start > upper).  */
	if (compare_bound (AR_START, AR_END) == CMP_EQ
	    || ((compare_bound_int (ar->stride[i], 0) == CMP_GT
		 || ar->stride[i] == NULL) && comp_start_end == CMP_LT)
	    || (compare_bound_int (ar->stride[i], 0) == CMP_LT
	        && comp_start_end == CMP_GT))
	  {
	    if (compare_bound (AR_START, as->lower[i]) == CMP_LT)
	      {
		gfc_warning (0, "Lower array reference at %L is out of bounds "
		       "(%ld < %ld) in dimension %d", &ar->c_where[i],
		       mpz_get_si (AR_START->value.integer),
		       mpz_get_si (as->lower[i]->value.integer), i+1);
		return true;
	      }
	    if (compare_bound (AR_START, as->upper[i]) == CMP_GT)
	      {
		gfc_warning (0, "Lower array reference at %L is out of bounds "
		       "(%ld > %ld) in dimension %d", &ar->c_where[i],
		       mpz_get_si (AR_START->value.integer),
		       mpz_get_si (as->upper[i]->value.integer), i+1);
		return true;
	      }
	  }

	/* If we can compute the highest index of the array section,
	   then it also has to be between lower and upper.  */
	mpz_init (last_value);
	if (compute_last_value_for_triplet (AR_START, AR_END, ar->stride[i],
					    last_value))
	  {
	    if (compare_bound_mpz_t (as->lower[i], last_value) == CMP_GT)
	      {
		gfc_warning (0, "Upper array reference at %L is out of bounds "
		       "(%ld < %ld) in dimension %d", &ar->c_where[i],
		       mpz_get_si (last_value),
		       mpz_get_si (as->lower[i]->value.integer), i+1);
	        mpz_clear (last_value);
		return true;
	      }
	    if (compare_bound_mpz_t (as->upper[i], last_value) == CMP_LT)
	      {
		gfc_warning (0, "Upper array reference at %L is out of bounds "
		       "(%ld > %ld) in dimension %d", &ar->c_where[i],
		       mpz_get_si (last_value),
		       mpz_get_si (as->upper[i]->value.integer), i+1);
	        mpz_clear (last_value);
		return true;
	      }
	  }
	mpz_clear (last_value);

#undef AR_START
#undef AR_END
      }
      break;

    default:
      gfc_internal_error ("check_dimension(): Bad array reference");
    }

  return true;
}


/* Compare an array reference with an array specification.  */

static bool
compare_spec_to_ref (gfc_array_ref *ar)
{
  gfc_array_spec *as;
  int i;

  as = ar->as;
  i = as->rank - 1;
  /* TODO: Full array sections are only allowed as actual parameters.  */
  if (as->type == AS_ASSUMED_SIZE
      && (/*ar->type == AR_FULL
	  ||*/ (ar->type == AR_SECTION
	      && ar->dimen_type[i] == DIMEN_RANGE && ar->end[i] == NULL)))
    {
      gfc_error ("Rightmost upper bound of assumed size array section "
		 "not specified at %L", &ar->where);
      return false;
    }

  if (ar->type == AR_FULL)
    return true;

  if (as->rank != ar->dimen)
    {
      gfc_error ("Rank mismatch in array reference at %L (%d/%d)",
		 &ar->where, ar->dimen, as->rank);
      return false;
    }

  /* ar->codimen == 0 is a local array.  */
  if (as->corank != ar->codimen && ar->codimen != 0)
    {
      gfc_error ("Coindex rank mismatch in array reference at %L (%d/%d)",
		 &ar->where, ar->codimen, as->corank);
      return false;
    }

  for (i = 0; i < as->rank; i++)
    if (!check_dimension (i, ar, as))
      return false;

  /* Local access has no coarray spec.  */
  if (ar->codimen != 0)
    for (i = as->rank; i < as->rank + as->corank; i++)
      {
	if (ar->dimen_type[i] != DIMEN_ELEMENT && !ar->in_allocate
	    && ar->dimen_type[i] != DIMEN_THIS_IMAGE)
	  {
	    gfc_error ("Coindex of codimension %d must be a scalar at %L",
		       i + 1 - as->rank, &ar->where);
	    return false;
	  }
	if (!check_dimension (i, ar, as))
	  return false;
      }

  return true;
}


/* Resolve one part of an array index.  */

static bool
gfc_resolve_index_1 (gfc_expr *index, int check_scalar,
		     int force_index_integer_kind)
{
  gfc_typespec ts;

  if (index == NULL)
    return true;

  if (!gfc_resolve_expr (index))
    return false;

  if (check_scalar && index->rank != 0)
    {
      gfc_error ("Array index at %L must be scalar", &index->where);
      return false;
    }

  if (index->ts.type != BT_INTEGER && index->ts.type != BT_REAL)
    {
      gfc_error ("Array index at %L must be of INTEGER type, found %s",
		 &index->where, gfc_basic_typename (index->ts.type));
      return false;
    }

  if (index->ts.type == BT_REAL)
    if (!gfc_notify_std (GFC_STD_LEGACY, "REAL array index at %L",
			 &index->where))
      return false;

  if ((index->ts.kind != gfc_index_integer_kind
       && force_index_integer_kind)
      || index->ts.type != BT_INTEGER)
    {
      gfc_clear_ts (&ts);
      ts.type = BT_INTEGER;
      ts.kind = gfc_index_integer_kind;

      gfc_convert_type_warn (index, &ts, 2, 0);
    }

  return true;
}

/* Resolve one part of an array index.  */

bool
gfc_resolve_index (gfc_expr *index, int check_scalar)
{
  return gfc_resolve_index_1 (index, check_scalar, 1);
}

/* Resolve a dim argument to an intrinsic function.  */

bool
gfc_resolve_dim_arg (gfc_expr *dim)
{
  if (dim == NULL)
    return true;

  if (!gfc_resolve_expr (dim))
    return false;

  if (dim->rank != 0)
    {
      gfc_error ("Argument dim at %L must be scalar", &dim->where);
      return false;

    }

  if (dim->ts.type != BT_INTEGER)
    {
      gfc_error ("Argument dim at %L must be of INTEGER type", &dim->where);
      return false;
    }

  if (dim->ts.kind != gfc_index_integer_kind)
    {
      gfc_typespec ts;

      gfc_clear_ts (&ts);
      ts.type = BT_INTEGER;
      ts.kind = gfc_index_integer_kind;

      gfc_convert_type_warn (dim, &ts, 2, 0);
    }

  return true;
}

/* Given an expression that contains array references, update those array
   references to point to the right array specifications.  While this is
   filled in during matching, this information is difficult to save and load
   in a module, so we take care of it here.

   The idea here is that the original array reference comes from the
   base symbol.  We traverse the list of reference structures, setting
   the stored reference to references.  Component references can
   provide an additional array specification.  */

static void
find_array_spec (gfc_expr *e)
{
  gfc_array_spec *as;
  gfc_component *c;
  gfc_ref *ref;

  if (e->symtree->n.sym->ts.type == BT_CLASS)
    as = CLASS_DATA (e->symtree->n.sym)->as;
  else
    as = e->symtree->n.sym->as;

  for (ref = e->ref; ref; ref = ref->next)
    switch (ref->type)
      {
      case REF_ARRAY:
	if (as == NULL)
	  gfc_internal_error ("find_array_spec(): Missing spec");

	ref->u.ar.as = as;
	as = NULL;
	break;

      case REF_COMPONENT:
	c = ref->u.c.component;
	if (c->attr.dimension)
	  {
	    if (as != NULL)
	      gfc_internal_error ("find_array_spec(): unused as(1)");
	    as = c->as;
	  }

	break;

      case REF_SUBSTRING:
	break;
      }

  if (as != NULL)
    gfc_internal_error ("find_array_spec(): unused as(2)");
}


/* Resolve an array reference.  */

static bool
resolve_array_ref (gfc_array_ref *ar)
{
  int i, check_scalar;
  gfc_expr *e;

  for (i = 0; i < ar->dimen + ar->codimen; i++)
    {
      check_scalar = ar->dimen_type[i] == DIMEN_RANGE;

      /* Do not force gfc_index_integer_kind for the start.  We can
         do fine with any integer kind.  This avoids temporary arrays
	 created for indexing with a vector.  */
      if (!gfc_resolve_index_1 (ar->start[i], check_scalar, 0))
	return false;
      if (!gfc_resolve_index (ar->end[i], check_scalar))
	return false;
      if (!gfc_resolve_index (ar->stride[i], check_scalar))
	return false;

      e = ar->start[i];

      if (ar->dimen_type[i] == DIMEN_UNKNOWN)
	switch (e->rank)
	  {
	  case 0:
	    ar->dimen_type[i] = DIMEN_ELEMENT;
	    break;

	  case 1:
	    ar->dimen_type[i] = DIMEN_VECTOR;
	    if (e->expr_type == EXPR_VARIABLE
		&& e->symtree->n.sym->ts.type == BT_DERIVED)
	      ar->start[i] = gfc_get_parentheses (e);
	    break;

	  default:
	    gfc_error ("Array index at %L is an array of rank %d",
		       &ar->c_where[i], e->rank);
	    return false;
	  }

      /* Fill in the upper bound, which may be lower than the
	 specified one for something like a(2:10:5), which is
	 identical to a(2:7:5).  Only relevant for strides not equal
	 to one.  Don't try a division by zero.  */
      if (ar->dimen_type[i] == DIMEN_RANGE
	  && ar->stride[i] != NULL && ar->stride[i]->expr_type == EXPR_CONSTANT
	  && mpz_cmp_si (ar->stride[i]->value.integer, 1L) != 0
	  && mpz_cmp_si (ar->stride[i]->value.integer, 0L) != 0)
	{
	  mpz_t size, end;

	  if (gfc_ref_dimen_size (ar, i, &size, &end))
	    {
	      if (ar->end[i] == NULL)
		{
		  ar->end[i] =
		    gfc_get_constant_expr (BT_INTEGER, gfc_index_integer_kind,
					   &ar->where);
		  mpz_set (ar->end[i]->value.integer, end);
		}
	      else if (ar->end[i]->ts.type == BT_INTEGER
		       && ar->end[i]->expr_type == EXPR_CONSTANT)
		{
		  mpz_set (ar->end[i]->value.integer, end);
		}
	      else
		gcc_unreachable ();

	      mpz_clear (size);
	      mpz_clear (end);
	    }
	}
    }

  if (ar->type == AR_FULL)
    {
      if (ar->as->rank == 0)
	ar->type = AR_ELEMENT;

      /* Make sure array is the same as array(:,:), this way
	 we don't need to special case all the time.  */
      ar->dimen = ar->as->rank;
      for (i = 0; i < ar->dimen; i++)
	{
	  ar->dimen_type[i] = DIMEN_RANGE;

	  gcc_assert (ar->start[i] == NULL);
	  gcc_assert (ar->end[i] == NULL);
	  gcc_assert (ar->stride[i] == NULL);
	}
    }

  /* If the reference type is unknown, figure out what kind it is.  */

  if (ar->type == AR_UNKNOWN)
    {
      ar->type = AR_ELEMENT;
      for (i = 0; i < ar->dimen; i++)
	if (ar->dimen_type[i] == DIMEN_RANGE
	    || ar->dimen_type[i] == DIMEN_VECTOR)
	  {
	    ar->type = AR_SECTION;
	    break;
	  }
    }

  if (!ar->as->cray_pointee && !compare_spec_to_ref (ar))
    return false;

  if (ar->as->corank && ar->codimen == 0)
    {
      int n;
      ar->codimen = ar->as->corank;
      for (n = ar->dimen; n < ar->dimen + ar->codimen; n++)
	ar->dimen_type[n] = DIMEN_THIS_IMAGE;
    }

  return true;
}


static bool
resolve_substring (gfc_ref *ref)
{
  int k = gfc_validate_kind (BT_INTEGER, gfc_charlen_int_kind, false);

  if (ref->u.ss.start != NULL)
    {
      if (!gfc_resolve_expr (ref->u.ss.start))
	return false;

      if (ref->u.ss.start->ts.type != BT_INTEGER)
	{
	  gfc_error ("Substring start index at %L must be of type INTEGER",
		     &ref->u.ss.start->where);
	  return false;
	}

      if (ref->u.ss.start->rank != 0)
	{
	  gfc_error ("Substring start index at %L must be scalar",
		     &ref->u.ss.start->where);
	  return false;
	}

      if (compare_bound_int (ref->u.ss.start, 1) == CMP_LT
	  && (compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_EQ
	      || compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_GT))
	{
	  gfc_error ("Substring start index at %L is less than one",
		     &ref->u.ss.start->where);
	  return false;
	}
    }

  if (ref->u.ss.end != NULL)
    {
      if (!gfc_resolve_expr (ref->u.ss.end))
	return false;

      if (ref->u.ss.end->ts.type != BT_INTEGER)
	{
	  gfc_error ("Substring end index at %L must be of type INTEGER",
		     &ref->u.ss.end->where);
	  return false;
	}

      if (ref->u.ss.end->rank != 0)
	{
	  gfc_error ("Substring end index at %L must be scalar",
		     &ref->u.ss.end->where);
	  return false;
	}

      if (ref->u.ss.length != NULL
	  && compare_bound (ref->u.ss.end, ref->u.ss.length->length) == CMP_GT
	  && (compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_EQ
	      || compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_GT))
	{
	  gfc_error ("Substring end index at %L exceeds the string length",
		     &ref->u.ss.start->where);
	  return false;
	}

      if (compare_bound_mpz_t (ref->u.ss.end,
			       gfc_integer_kinds[k].huge) == CMP_GT
	  && (compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_EQ
	      || compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_GT))
	{
	  gfc_error ("Substring end index at %L is too large",
		     &ref->u.ss.end->where);
	  return false;
	}
    }

  return true;
}


/* This function supplies missing substring charlens.  */

void
gfc_resolve_substring_charlen (gfc_expr *e)
{
  gfc_ref *char_ref;
  gfc_expr *start, *end;
  gfc_typespec *ts = NULL;

  for (char_ref = e->ref; char_ref; char_ref = char_ref->next)
    {
      if (char_ref->type == REF_SUBSTRING)
      	break;
      if (char_ref->type == REF_COMPONENT)
	ts = &char_ref->u.c.component->ts;
    }

  if (!char_ref)
    return;

  gcc_assert (char_ref->next == NULL);

  if (e->ts.u.cl)
    {
      if (e->ts.u.cl->length)
	gfc_free_expr (e->ts.u.cl->length);
      else if (e->expr_type == EXPR_VARIABLE && e->symtree->n.sym->attr.dummy)
	return;
    }

  e->ts.type = BT_CHARACTER;
  e->ts.kind = gfc_default_character_kind;

  if (!e->ts.u.cl)
    e->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);

  if (char_ref->u.ss.start)
    start = gfc_copy_expr (char_ref->u.ss.start);
  else
    start = gfc_get_int_expr (gfc_default_integer_kind, NULL, 1);

  if (char_ref->u.ss.end)
    end = gfc_copy_expr (char_ref->u.ss.end);
  else if (e->expr_type == EXPR_VARIABLE)
    {
      if (!ts)
	ts = &e->symtree->n.sym->ts;
      end = gfc_copy_expr (ts->u.cl->length);
    }
  else
    end = NULL;

  if (!start || !end)
    {
      gfc_free_expr (start);
      gfc_free_expr (end);
      return;
    }

  /* Length = (end - start + 1).  */
  e->ts.u.cl->length = gfc_subtract (end, start);
  e->ts.u.cl->length = gfc_add (e->ts.u.cl->length,
				gfc_get_int_expr (gfc_default_integer_kind,
						  NULL, 1));

  /* F2008, 6.4.1:  Both the starting point and the ending point shall
     be within the range 1, 2, ..., n unless the starting point exceeds
     the ending point, in which case the substring has length zero.  */

  if (mpz_cmp_si (e->ts.u.cl->length->value.integer, 0) < 0)
    mpz_set_si (e->ts.u.cl->length->value.integer, 0);

  e->ts.u.cl->length->ts.type = BT_INTEGER;
  e->ts.u.cl->length->ts.kind = gfc_charlen_int_kind;

  /* Make sure that the length is simplified.  */
  gfc_simplify_expr (e->ts.u.cl->length, 1);
  gfc_resolve_expr (e->ts.u.cl->length);
}


/* Resolve subtype references.  */

static bool
resolve_ref (gfc_expr *expr)
{
  int current_part_dimension, n_components, seen_part_dimension;
  gfc_ref *ref;

  for (ref = expr->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.as == NULL)
      {
	find_array_spec (expr);
	break;
      }

  for (ref = expr->ref; ref; ref = ref->next)
    switch (ref->type)
      {
      case REF_ARRAY:
	if (!resolve_array_ref (&ref->u.ar))
	  return false;
	break;

      case REF_COMPONENT:
	break;

      case REF_SUBSTRING:
	if (!resolve_substring (ref))
	  return false;
	break;
      }

  /* Check constraints on part references.  */

  current_part_dimension = 0;
  seen_part_dimension = 0;
  n_components = 0;

  for (ref = expr->ref; ref; ref = ref->next)
    {
      switch (ref->type)
	{
	case REF_ARRAY:
	  switch (ref->u.ar.type)
	    {
	    case AR_FULL:
	      /* Coarray scalar.  */
	      if (ref->u.ar.as->rank == 0)
		{
		  current_part_dimension = 0;
		  break;
		}
	      /* Fall through.  */
	    case AR_SECTION:
	      current_part_dimension = 1;
	      break;

	    case AR_ELEMENT:
	      current_part_dimension = 0;
	      break;

	    case AR_UNKNOWN:
	      gfc_internal_error ("resolve_ref(): Bad array reference");
	    }

	  break;

	case REF_COMPONENT:
	  if (current_part_dimension || seen_part_dimension)
	    {
	      /* F03:C614.  */
	      if (ref->u.c.component->attr.pointer
		  || ref->u.c.component->attr.proc_pointer
		  || (ref->u.c.component->ts.type == BT_CLASS
			&& CLASS_DATA (ref->u.c.component)->attr.pointer))
		{
		  gfc_error ("Component to the right of a part reference "
			     "with nonzero rank must not have the POINTER "
			     "attribute at %L", &expr->where);
		  return false;
		}
	      else if (ref->u.c.component->attr.allocatable
			|| (ref->u.c.component->ts.type == BT_CLASS
			    && CLASS_DATA (ref->u.c.component)->attr.allocatable))

		{
		  gfc_error ("Component to the right of a part reference "
			     "with nonzero rank must not have the ALLOCATABLE "
			     "attribute at %L", &expr->where);
		  return false;
		}
	    }

	  n_components++;
	  break;

	case REF_SUBSTRING:
	  break;
	}

      if (((ref->type == REF_COMPONENT && n_components > 1)
	   || ref->next == NULL)
	  && current_part_dimension
	  && seen_part_dimension)
	{
	  gfc_error ("Two or more part references with nonzero rank must "
		     "not be specified at %L", &expr->where);
	  return false;
	}

      if (ref->type == REF_COMPONENT)
	{
	  if (current_part_dimension)
	    seen_part_dimension = 1;

	  /* reset to make sure */
	  current_part_dimension = 0;
	}
    }

  return true;
}


/* Given an expression, determine its shape.  This is easier than it sounds.
   Leaves the shape array NULL if it is not possible to determine the shape.  */

static void
expression_shape (gfc_expr *e)
{
  mpz_t array[GFC_MAX_DIMENSIONS];
  int i;

  if (e->rank <= 0 || e->shape != NULL)
    return;

  for (i = 0; i < e->rank; i++)
    if (!gfc_array_dimen_size (e, i, &array[i]))
      goto fail;

  e->shape = gfc_get_shape (e->rank);

  memcpy (e->shape, array, e->rank * sizeof (mpz_t));

  return;

fail:
  for (i--; i >= 0; i--)
    mpz_clear (array[i]);
}


/* Given a variable expression node, compute the rank of the expression by
   examining the base symbol and any reference structures it may have.  */

void
expression_rank (gfc_expr *e)
{
  gfc_ref *ref;
  int i, rank;

  /* Just to make sure, because EXPR_COMPCALL's also have an e->ref and that
     could lead to serious confusion...  */
  gcc_assert (e->expr_type != EXPR_COMPCALL);

  if (e->ref == NULL)
    {
      if (e->expr_type == EXPR_ARRAY)
	goto done;
      /* Constructors can have a rank different from one via RESHAPE().  */

      if (e->symtree == NULL)
	{
	  e->rank = 0;
	  goto done;
	}

      e->rank = (e->symtree->n.sym->as == NULL)
		? 0 : e->symtree->n.sym->as->rank;
      goto done;
    }

  rank = 0;

  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT && ref->u.c.component->attr.proc_pointer
	  && ref->u.c.component->attr.function && !ref->next)
	rank = ref->u.c.component->as ? ref->u.c.component->as->rank : 0;

      if (ref->type != REF_ARRAY)
	continue;

      if (ref->u.ar.type == AR_FULL)
	{
	  rank = ref->u.ar.as->rank;
	  break;
	}

      if (ref->u.ar.type == AR_SECTION)
	{
	  /* Figure out the rank of the section.  */
	  if (rank != 0)
	    gfc_internal_error ("expression_rank(): Two array specs");

	  for (i = 0; i < ref->u.ar.dimen; i++)
	    if (ref->u.ar.dimen_type[i] == DIMEN_RANGE
		|| ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
	      rank++;

	  break;
	}
    }

  e->rank = rank;

done:
  expression_shape (e);
}


static void
add_caf_get_intrinsic (gfc_expr *e)
{
  gfc_expr *wrapper, *tmp_expr;
  gfc_ref *ref;
  int n;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.codimen > 0)
      break;
  if (ref == NULL)
    return;

  for (n = ref->u.ar.dimen; n < ref->u.ar.dimen + ref->u.ar.codimen; n++)
    if (ref->u.ar.dimen_type[n] != DIMEN_ELEMENT)
      return;

  tmp_expr = XCNEW (gfc_expr);
  *tmp_expr = *e;
  wrapper = gfc_build_intrinsic_call (gfc_current_ns, GFC_ISYM_CAF_GET,
				      "caf_get", tmp_expr->where, 1, tmp_expr);
  wrapper->ts = e->ts;
  wrapper->rank = e->rank;
  if (e->rank)
    wrapper->shape = gfc_copy_shape (e->shape, e->rank);
  *e = *wrapper;
  free (wrapper);
}


static void
remove_caf_get_intrinsic (gfc_expr *e)
{
  gcc_assert (e->expr_type == EXPR_FUNCTION && e->value.function.isym
	      && e->value.function.isym->id == GFC_ISYM_CAF_GET);
  gfc_expr *e2 = e->value.function.actual->expr;
  e->value.function.actual->expr = NULL;
  gfc_free_actual_arglist (e->value.function.actual);
  gfc_free_shape (&e->shape, e->rank);
  *e = *e2;
  free (e2);
}


/* Resolve a variable expression.  */

static bool
resolve_variable (gfc_expr *e)
{
  gfc_symbol *sym;
  bool t;

  t = true;

  if (e->symtree == NULL)
    return false;
  sym = e->symtree->n.sym;

  /* Use same check as for TYPE(*) below; this check has to be before TYPE(*)
     as ts.type is set to BT_ASSUMED in resolve_symbol.  */
  if (sym->attr.ext_attr & (1 << EXT_ATTR_NO_ARG_CHECK))
    {
      if (!actual_arg || inquiry_argument)
	{
	  gfc_error ("Variable %s at %L with NO_ARG_CHECK attribute may only "
		     "be used as actual argument", sym->name, &e->where);
	  return false;
	}
    }
  /* TS 29113, 407b.  */
  else if (e->ts.type == BT_ASSUMED)
    {
      if (!actual_arg)
	{
	  gfc_error ("Assumed-type variable %s at %L may only be used "
		     "as actual argument", sym->name, &e->where);
	  return false;
	}
      else if (inquiry_argument && !first_actual_arg)
	{
	  /* FIXME: It doesn't work reliably as inquiry_argument is not set
	     for all inquiry functions in resolve_function; the reason is
	     that the function-name resolution happens too late in that
	     function.  */
	  gfc_error ("Assumed-type variable %s at %L as actual argument to "
		     "an inquiry function shall be the first argument",
		     sym->name, &e->where);
	  return false;
	}
    }
  /* TS 29113, C535b.  */
  else if ((sym->ts.type == BT_CLASS && sym->attr.class_ok
	    && CLASS_DATA (sym)->as
	    && CLASS_DATA (sym)->as->type == AS_ASSUMED_RANK)
	   || (sym->ts.type != BT_CLASS && sym->as
	       && sym->as->type == AS_ASSUMED_RANK))
    {
      if (!actual_arg)
	{
	  gfc_error ("Assumed-rank variable %s at %L may only be used as "
		     "actual argument", sym->name, &e->where);
	  return false;
	}
      else if (inquiry_argument && !first_actual_arg)
	{
	  /* FIXME: It doesn't work reliably as inquiry_argument is not set
	     for all inquiry functions in resolve_function; the reason is
	     that the function-name resolution happens too late in that
	     function.  */
	  gfc_error ("Assumed-rank variable %s at %L as actual argument "
		     "to an inquiry function shall be the first argument",
		     sym->name, &e->where);
	  return false;
	}
    }

  if ((sym->attr.ext_attr & (1 << EXT_ATTR_NO_ARG_CHECK)) && e->ref
      && !(e->ref->type == REF_ARRAY && e->ref->u.ar.type == AR_FULL
	   && e->ref->next == NULL))
    {
      gfc_error ("Variable %s at %L with NO_ARG_CHECK attribute shall not have "
		 "a subobject reference", sym->name, &e->ref->u.ar.where);
      return false;
    }
  /* TS 29113, 407b.  */
  else if (e->ts.type == BT_ASSUMED && e->ref
	   && !(e->ref->type == REF_ARRAY && e->ref->u.ar.type == AR_FULL
		&& e->ref->next == NULL))
    {
      gfc_error ("Assumed-type variable %s at %L shall not have a subobject "
		 "reference", sym->name, &e->ref->u.ar.where);
      return false;
    }

  /* TS 29113, C535b.  */
  if (((sym->ts.type == BT_CLASS && sym->attr.class_ok
	&& CLASS_DATA (sym)->as
	&& CLASS_DATA (sym)->as->type == AS_ASSUMED_RANK)
       || (sym->ts.type != BT_CLASS && sym->as
	   && sym->as->type == AS_ASSUMED_RANK))
      && e->ref
      && !(e->ref->type == REF_ARRAY && e->ref->u.ar.type == AR_FULL
	   && e->ref->next == NULL))
    {
      gfc_error ("Assumed-rank variable %s at %L shall not have a subobject "
		 "reference", sym->name, &e->ref->u.ar.where);
      return false;
    }

  /* For variables that are used in an associate (target => object) where
     the object's basetype is array valued while the target is scalar,
     the ts' type of the component refs is still array valued, which
     can't be translated that way.  */
  if (sym->assoc && e->rank == 0 && e->ref && sym->ts.type == BT_CLASS
      && sym->assoc->target->ts.type == BT_CLASS
      && CLASS_DATA (sym->assoc->target)->as)
    {
      gfc_ref *ref = e->ref;
      while (ref)
	{
	  switch (ref->type)
	    {
	    case REF_COMPONENT:
	      ref->u.c.sym = sym->ts.u.derived;
	      /* Stop the loop.  */
	      ref = NULL;
	      break;
	    default:
	      ref = ref->next;
	      break;
	    }
	}
    }

  /* If this is an associate-name, it may be parsed with an array reference
     in error even though the target is scalar.  Fail directly in this case.
     TODO Understand why class scalar expressions must be excluded.  */
  if (sym->assoc && !(sym->ts.type == BT_CLASS && e->rank == 0))
    {
      if (sym->ts.type == BT_CLASS)
	gfc_fix_class_refs (e);
      if (!sym->attr.dimension && e->ref && e->ref->type == REF_ARRAY)
	return false;
    }

  if (sym->ts.type == BT_DERIVED && sym->ts.u.derived->attr.generic)
    sym->ts.u.derived = gfc_find_dt_in_generic (sym->ts.u.derived);

  /* On the other hand, the parser may not have known this is an array;
     in this case, we have to add a FULL reference.  */
  if (sym->assoc && sym->attr.dimension && !e->ref)
    {
      e->ref = gfc_get_ref ();
      e->ref->type = REF_ARRAY;
      e->ref->u.ar.type = AR_FULL;
      e->ref->u.ar.dimen = 0;
    }

  /* Like above, but for class types, where the checking whether an array
     ref is present is more complicated.  Furthermore make sure not to add
     the full array ref to _vptr or _len refs.  */
  if (sym->assoc && sym->ts.type == BT_CLASS
      && CLASS_DATA (sym)->attr.dimension
      && (e->ts.type != BT_DERIVED || !e->ts.u.derived->attr.vtype))
    {
      gfc_ref *ref, *newref;

      newref = gfc_get_ref ();
      newref->type = REF_ARRAY;
      newref->u.ar.type = AR_FULL;
      newref->u.ar.dimen = 0;
      /* Because this is an associate var and the first ref either is a ref to
	 the _data component or not, no traversal of the ref chain is
	 needed.  The array ref needs to be inserted after the _data ref,
	 or when that is not present, which may happend for polymorphic
	 types, then at the first position.  */
      ref = e->ref;
      if (!ref)
	e->ref = newref;
      else if (ref->type == REF_COMPONENT
	       && strcmp ("_data", ref->u.c.component->name) == 0)
	{
	  if (!ref->next || ref->next->type != REF_ARRAY)
	    {
	      newref->next = ref->next;
	      ref->next = newref;
	    }
	  else
	    /* Array ref present already.  */
	    gfc_free_ref_list (newref);
	}
      else if (ref->type == REF_ARRAY)
	/* Array ref present already.  */
	gfc_free_ref_list (newref);
      else
	{
	  newref->next = ref;
	  e->ref = newref;
	}
    }

  if (e->ref && !resolve_ref (e))
    return false;

  if (sym->attr.flavor == FL_PROCEDURE
      && (!sym->attr.function
	  || (sym->attr.function && sym->result
	      && sym->result->attr.proc_pointer
	      && !sym->result->attr.function)))
    {
      e->ts.type = BT_PROCEDURE;
      goto resolve_procedure;
    }

  if (sym->ts.type != BT_UNKNOWN)
    gfc_variable_attr (e, &e->ts);
  else if (sym->attr.flavor == FL_PROCEDURE
	   && sym->attr.function && sym->result
	   && sym->result->ts.type != BT_UNKNOWN
	   && sym->result->attr.proc_pointer)
    e->ts = sym->result->ts;
  else
    {
      /* Must be a simple variable reference.  */
      if (!gfc_set_default_type (sym, 1, sym->ns))
	return false;
      e->ts = sym->ts;
    }

  if (check_assumed_size_reference (sym, e))
    return false;

  /* Deal with forward references to entries during gfc_resolve_code, to
     satisfy, at least partially, 12.5.2.5.  */
  if (gfc_current_ns->entries
      && current_entry_id == sym->entry_id
      && cs_base
      && cs_base->current
      && cs_base->current->op != EXEC_ENTRY)
    {
      gfc_entry_list *entry;
      gfc_formal_arglist *formal;
      int n;
      bool seen, saved_specification_expr;

      /* If the symbol is a dummy...  */
      if (sym->attr.dummy && sym->ns == gfc_current_ns)
	{
	  entry = gfc_current_ns->entries;
	  seen = false;

	  /* ...test if the symbol is a parameter of previous entries.  */
	  for (; entry && entry->id <= current_entry_id; entry = entry->next)
	    for (formal = entry->sym->formal; formal; formal = formal->next)
	      {
		if (formal->sym && sym->name == formal->sym->name)
		  {
		    seen = true;
		    break;
		  }
	      }

	  /*  If it has not been seen as a dummy, this is an error.  */
	  if (!seen)
	    {
	      if (specification_expr)
		gfc_error ("Variable %qs, used in a specification expression"
			   ", is referenced at %L before the ENTRY statement "
			   "in which it is a parameter",
			   sym->name, &cs_base->current->loc);
	      else
		gfc_error ("Variable %qs is used at %L before the ENTRY "
			   "statement in which it is a parameter",
			   sym->name, &cs_base->current->loc);
	      t = false;
	    }
	}

      /* Now do the same check on the specification expressions.  */
      saved_specification_expr = specification_expr;
      specification_expr = true;
      if (sym->ts.type == BT_CHARACTER
	  && !gfc_resolve_expr (sym->ts.u.cl->length))
	t = false;

      if (sym->as)
	for (n = 0; n < sym->as->rank; n++)
	  {
	     if (!gfc_resolve_expr (sym->as->lower[n]))
	       t = false;
	     if (!gfc_resolve_expr (sym->as->upper[n]))
	       t = false;
	  }
      specification_expr = saved_specification_expr;

      if (t)
	/* Update the symbol's entry level.  */
	sym->entry_id = current_entry_id + 1;
    }

  /* If a symbol has been host_associated mark it.  This is used latter,
     to identify if aliasing is possible via host association.  */
  if (sym->attr.flavor == FL_VARIABLE
	&& gfc_current_ns->parent
	&& (gfc_current_ns->parent == sym->ns
	      || (gfc_current_ns->parent->parent
		    && gfc_current_ns->parent->parent == sym->ns)))
    sym->attr.host_assoc = 1;

  if (gfc_current_ns->proc_name
      && sym->attr.dimension
      && (sym->ns != gfc_current_ns
	  || sym->attr.use_assoc
	  || sym->attr.in_common))
    gfc_current_ns->proc_name->attr.array_outer_dependency = 1;

resolve_procedure:
  if (t && !resolve_procedure_expression (e))
    t = false;

  /* F2008, C617 and C1229.  */
  if (!inquiry_argument && (e->ts.type == BT_CLASS || e->ts.type == BT_DERIVED)
      && gfc_is_coindexed (e))
    {
      gfc_ref *ref, *ref2 = NULL;

      for (ref = e->ref; ref; ref = ref->next)
	{
	  if (ref->type == REF_COMPONENT)
	    ref2 = ref;
	  if (ref->type == REF_ARRAY && ref->u.ar.codimen > 0)
	    break;
	}

      for ( ; ref; ref = ref->next)
	if (ref->type == REF_COMPONENT)
	  break;

      /* Expression itself is not coindexed object.  */
      if (ref && e->ts.type == BT_CLASS)
	{
	  gfc_error ("Polymorphic subobject of coindexed object at %L",
		     &e->where);
	  t = false;
	}

      /* Expression itself is coindexed object.  */
      if (ref == NULL)
	{
	  gfc_component *c;
	  c = ref2 ? ref2->u.c.component : e->symtree->n.sym->components;
	  for ( ; c; c = c->next)
	    if (c->attr.allocatable && c->ts.type == BT_CLASS)
	      {
		gfc_error ("Coindexed object with polymorphic allocatable "
			 "subcomponent at %L", &e->where);
		t = false;
		break;
	      }
	}
    }

  if (t)
    expression_rank (e);

  if (t && flag_coarray == GFC_FCOARRAY_LIB && gfc_is_coindexed (e))
    add_caf_get_intrinsic (e);

  return t;
}


/* Checks to see that the correct symbol has been host associated.
   The only situation where this arises is that in which a twice
   contained function is parsed after the host association is made.
   Therefore, on detecting this, change the symbol in the expression
   and convert the array reference into an actual arglist if the old
   symbol is a variable.  */
static bool
check_host_association (gfc_expr *e)
{
  gfc_symbol *sym, *old_sym;
  gfc_symtree *st;
  int n;
  gfc_ref *ref;
  gfc_actual_arglist *arg, *tail = NULL;
  bool retval = e->expr_type == EXPR_FUNCTION;

  /*  If the expression is the result of substitution in
      interface.c(gfc_extend_expr) because there is no way in
      which the host association can be wrong.  */
  if (e->symtree == NULL
	|| e->symtree->n.sym == NULL
	|| e->user_operator)
    return retval;

  old_sym = e->symtree->n.sym;

  if (gfc_current_ns->parent
	&& old_sym->ns != gfc_current_ns)
    {
      /* Use the 'USE' name so that renamed module symbols are
	 correctly handled.  */
      gfc_find_symbol (e->symtree->name, gfc_current_ns, 1, &sym);

      if (sym && old_sym != sym
	      && sym->ts.type == old_sym->ts.type
	      && sym->attr.flavor == FL_PROCEDURE
	      && sym->attr.contained)
	{
	  /* Clear the shape, since it might not be valid.  */
	  gfc_free_shape (&e->shape, e->rank);

	  /* Give the expression the right symtree!  */
	  gfc_find_sym_tree (e->symtree->name, NULL, 1, &st);
	  gcc_assert (st != NULL);

	  if (old_sym->attr.flavor == FL_PROCEDURE
		|| e->expr_type == EXPR_FUNCTION)
  	    {
	      /* Original was function so point to the new symbol, since
		 the actual argument list is already attached to the
		 expression.  */
	      e->value.function.esym = NULL;
	      e->symtree = st;
	    }
	  else
	    {
	      /* Original was variable so convert array references into
		 an actual arglist. This does not need any checking now
		 since resolve_function will take care of it.  */
	      e->value.function.actual = NULL;
	      e->expr_type = EXPR_FUNCTION;
	      e->symtree = st;

	      /* Ambiguity will not arise if the array reference is not
		 the last reference.  */
	      for (ref = e->ref; ref; ref = ref->next)
		if (ref->type == REF_ARRAY && ref->next == NULL)
		  break;

	      gcc_assert (ref->type == REF_ARRAY);

	      /* Grab the start expressions from the array ref and
		 copy them into actual arguments.  */
	      for (n = 0; n < ref->u.ar.dimen; n++)
		{
		  arg = gfc_get_actual_arglist ();
		  arg->expr = gfc_copy_expr (ref->u.ar.start[n]);
		  if (e->value.function.actual == NULL)
		    tail = e->value.function.actual = arg;
	          else
		    {
		      tail->next = arg;
		      tail = arg;
		    }
		}

	      /* Dump the reference list and set the rank.  */
	      gfc_free_ref_list (e->ref);
	      e->ref = NULL;
	      e->rank = sym->as ? sym->as->rank : 0;
	    }

	  gfc_resolve_expr (e);
	  sym->refs++;
	}
    }
  /* This might have changed!  */
  return e->expr_type == EXPR_FUNCTION;
}


static void
gfc_resolve_character_operator (gfc_expr *e)
{
  gfc_expr *op1 = e->value.op.op1;
  gfc_expr *op2 = e->value.op.op2;
  gfc_expr *e1 = NULL;
  gfc_expr *e2 = NULL;

  gcc_assert (e->value.op.op == INTRINSIC_CONCAT);

  if (op1->ts.u.cl && op1->ts.u.cl->length)
    e1 = gfc_copy_expr (op1->ts.u.cl->length);
  else if (op1->expr_type == EXPR_CONSTANT)
    e1 = gfc_get_int_expr (gfc_default_integer_kind, NULL,
			   op1->value.character.length);

  if (op2->ts.u.cl && op2->ts.u.cl->length)
    e2 = gfc_copy_expr (op2->ts.u.cl->length);
  else if (op2->expr_type == EXPR_CONSTANT)
    e2 = gfc_get_int_expr (gfc_default_integer_kind, NULL,
			   op2->value.character.length);

  e->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);

  if (!e1 || !e2)
    {
      gfc_free_expr (e1);
      gfc_free_expr (e2);

      return;
    }

  e->ts.u.cl->length = gfc_add (e1, e2);
  e->ts.u.cl->length->ts.type = BT_INTEGER;
  e->ts.u.cl->length->ts.kind = gfc_charlen_int_kind;
  gfc_simplify_expr (e->ts.u.cl->length, 0);
  gfc_resolve_expr (e->ts.u.cl->length);

  return;
}


/*  Ensure that an character expression has a charlen and, if possible, a
    length expression.  */

static void
fixup_charlen (gfc_expr *e)
{
  /* The cases fall through so that changes in expression type and the need
     for multiple fixes are picked up.  In all circumstances, a charlen should
     be available for the middle end to hang a backend_decl on.  */
  switch (e->expr_type)
    {
    case EXPR_OP:
      gfc_resolve_character_operator (e);
      /* FALLTHRU */

    case EXPR_ARRAY:
      if (e->expr_type == EXPR_ARRAY)
	gfc_resolve_character_array_constructor (e);
      /* FALLTHRU */

    case EXPR_SUBSTRING:
      if (!e->ts.u.cl && e->ref)
	gfc_resolve_substring_charlen (e);
      /* FALLTHRU */

    default:
      if (!e->ts.u.cl)
	e->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);

      break;
    }
}


/* Update an actual argument to include the passed-object for type-bound
   procedures at the right position.  */

static gfc_actual_arglist*
update_arglist_pass (gfc_actual_arglist* lst, gfc_expr* po, unsigned argpos,
		     const char *name)
{
  gcc_assert (argpos > 0);

  if (argpos == 1)
    {
      gfc_actual_arglist* result;

      result = gfc_get_actual_arglist ();
      result->expr = po;
      result->next = lst;
      if (name)
        result->name = name;

      return result;
    }

  if (lst)
    lst->next = update_arglist_pass (lst->next, po, argpos - 1, name);
  else
    lst = update_arglist_pass (NULL, po, argpos - 1, name);
  return lst;
}


/* Extract the passed-object from an EXPR_COMPCALL (a copy of it).  */

static gfc_expr*
extract_compcall_passed_object (gfc_expr* e)
{
  gfc_expr* po;

  gcc_assert (e->expr_type == EXPR_COMPCALL);

  if (e->value.compcall.base_object)
    po = gfc_copy_expr (e->value.compcall.base_object);
  else
    {
      po = gfc_get_expr ();
      po->expr_type = EXPR_VARIABLE;
      po->symtree = e->symtree;
      po->ref = gfc_copy_ref (e->ref);
      po->where = e->where;
    }

  if (!gfc_resolve_expr (po))
    return NULL;

  return po;
}


/* Update the arglist of an EXPR_COMPCALL expression to include the
   passed-object.  */

static bool
update_compcall_arglist (gfc_expr* e)
{
  gfc_expr* po;
  gfc_typebound_proc* tbp;

  tbp = e->value.compcall.tbp;

  if (tbp->error)
    return false;

  po = extract_compcall_passed_object (e);
  if (!po)
    return false;

  if (tbp->nopass || e->value.compcall.ignore_pass)
    {
      gfc_free_expr (po);
      return true;
    }

  gcc_assert (tbp->pass_arg_num > 0);
  e->value.compcall.actual = update_arglist_pass (e->value.compcall.actual, po,
						  tbp->pass_arg_num,
						  tbp->pass_arg);

  return true;
}


/* Extract the passed object from a PPC call (a copy of it).  */

static gfc_expr*
extract_ppc_passed_object (gfc_expr *e)
{
  gfc_expr *po;
  gfc_ref **ref;

  po = gfc_get_expr ();
  po->expr_type = EXPR_VARIABLE;
  po->symtree = e->symtree;
  po->ref = gfc_copy_ref (e->ref);
  po->where = e->where;

  /* Remove PPC reference.  */
  ref = &po->ref;
  while ((*ref)->next)
    ref = &(*ref)->next;
  gfc_free_ref_list (*ref);
  *ref = NULL;

  if (!gfc_resolve_expr (po))
    return NULL;

  return po;
}


/* Update the actual arglist of a procedure pointer component to include the
   passed-object.  */

static bool
update_ppc_arglist (gfc_expr* e)
{
  gfc_expr* po;
  gfc_component *ppc;
  gfc_typebound_proc* tb;

  ppc = gfc_get_proc_ptr_comp (e);
  if (!ppc)
    return false;

  tb = ppc->tb;

  if (tb->error)
    return false;
  else if (tb->nopass)
    return true;

  po = extract_ppc_passed_object (e);
  if (!po)
    return false;

  /* F08:R739.  */
  if (po->rank != 0)
    {
      gfc_error ("Passed-object at %L must be scalar", &e->where);
      return false;
    }

  /* F08:C611.  */
  if (po->ts.type == BT_DERIVED && po->ts.u.derived->attr.abstract)
    {
      gfc_error ("Base object for procedure-pointer component call at %L is of"
		 " ABSTRACT type %qs", &e->where, po->ts.u.derived->name);
      return false;
    }

  gcc_assert (tb->pass_arg_num > 0);
  e->value.compcall.actual = update_arglist_pass (e->value.compcall.actual, po,
						  tb->pass_arg_num,
						  tb->pass_arg);

  return true;
}


/* Check that the object a TBP is called on is valid, i.e. it must not be
   of ABSTRACT type (as in subobject%abstract_parent%tbp()).  */

static bool
check_typebound_baseobject (gfc_expr* e)
{
  gfc_expr* base;
  bool return_value = false;

  base = extract_compcall_passed_object (e);
  if (!base)
    return false;

  gcc_assert (base->ts.type == BT_DERIVED || base->ts.type == BT_CLASS);

  if (base->ts.type == BT_CLASS && !gfc_expr_attr (base).class_ok)
    return false;

  /* F08:C611.  */
  if (base->ts.type == BT_DERIVED && base->ts.u.derived->attr.abstract)
    {
      gfc_error ("Base object for type-bound procedure call at %L is of"
		 " ABSTRACT type %qs", &e->where, base->ts.u.derived->name);
      goto cleanup;
    }

  /* F08:C1230. If the procedure called is NOPASS,
     the base object must be scalar.  */
  if (e->value.compcall.tbp->nopass && base->rank != 0)
    {
      gfc_error ("Base object for NOPASS type-bound procedure call at %L must"
		 " be scalar", &e->where);
      goto cleanup;
    }

  return_value = true;

cleanup:
  gfc_free_expr (base);
  return return_value;
}


/* Resolve a call to a type-bound procedure, either function or subroutine,
   statically from the data in an EXPR_COMPCALL expression.  The adapted
   arglist and the target-procedure symtree are returned.  */

static bool
resolve_typebound_static (gfc_expr* e, gfc_symtree** target,
			  gfc_actual_arglist** actual)
{
  gcc_assert (e->expr_type == EXPR_COMPCALL);
  gcc_assert (!e->value.compcall.tbp->is_generic);

  /* Update the actual arglist for PASS.  */
  if (!update_compcall_arglist (e))
    return false;

  *actual = e->value.compcall.actual;
  *target = e->value.compcall.tbp->u.specific;

  gfc_free_ref_list (e->ref);
  e->ref = NULL;
  e->value.compcall.actual = NULL;

  /* If we find a deferred typebound procedure, check for derived types
     that an overriding typebound procedure has not been missed.  */
  if (e->value.compcall.name
      && !e->value.compcall.tbp->non_overridable
      && e->value.compcall.base_object
      && e->value.compcall.base_object->ts.type == BT_DERIVED)
    {
      gfc_symtree *st;
      gfc_symbol *derived;

      /* Use the derived type of the base_object.  */
      derived = e->value.compcall.base_object->ts.u.derived;
      st = NULL;

      /* If necessary, go through the inheritance chain.  */
      while (!st && derived)
	{
	  /* Look for the typebound procedure 'name'.  */
	  if (derived->f2k_derived && derived->f2k_derived->tb_sym_root)
	    st = gfc_find_symtree (derived->f2k_derived->tb_sym_root,
				   e->value.compcall.name);
	  if (!st)
	    derived = gfc_get_derived_super_type (derived);
	}

      /* Now find the specific name in the derived type namespace.  */
      if (st && st->n.tb && st->n.tb->u.specific)
	gfc_find_sym_tree (st->n.tb->u.specific->name,
			   derived->ns, 1, &st);
      if (st)
	*target = st;
    }
  return true;
}


/* Get the ultimate declared type from an expression.  In addition,
   return the last class/derived type reference and the copy of the
   reference list.  If check_types is set true, derived types are
   identified as well as class references.  */
static gfc_symbol*
get_declared_from_expr (gfc_ref **class_ref, gfc_ref **new_ref,
			gfc_expr *e, bool check_types)
{
  gfc_symbol *declared;
  gfc_ref *ref;

  declared = NULL;
  if (class_ref)
    *class_ref = NULL;
  if (new_ref)
    *new_ref = gfc_copy_ref (e->ref);

  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type != REF_COMPONENT)
	continue;

      if ((ref->u.c.component->ts.type == BT_CLASS
	     || (check_types && gfc_bt_struct (ref->u.c.component->ts.type)))
	  && ref->u.c.component->attr.flavor != FL_PROCEDURE)
	{
	  declared = ref->u.c.component->ts.u.derived;
	  if (class_ref)
	    *class_ref = ref;
	}
    }

  if (declared == NULL)
    declared = e->symtree->n.sym->ts.u.derived;

  return declared;
}


/* Given an EXPR_COMPCALL calling a GENERIC typebound procedure, figure out
   which of the specific bindings (if any) matches the arglist and transform
   the expression into a call of that binding.  */

static bool
resolve_typebound_generic_call (gfc_expr* e, const char **name)
{
  gfc_typebound_proc* genproc;
  const char* genname;
  gfc_symtree *st;
  gfc_symbol *derived;

  gcc_assert (e->expr_type == EXPR_COMPCALL);
  genname = e->value.compcall.name;
  genproc = e->value.compcall.tbp;

  if (!genproc->is_generic)
    return true;

  /* Try the bindings on this type and in the inheritance hierarchy.  */
  for (; genproc; genproc = genproc->overridden)
    {
      gfc_tbp_generic* g;

      gcc_assert (genproc->is_generic);
      for (g = genproc->u.generic; g; g = g->next)
	{
	  gfc_symbol* target;
	  gfc_actual_arglist* args;
	  bool matches;

	  gcc_assert (g->specific);

	  if (g->specific->error)
	    continue;

	  target = g->specific->u.specific->n.sym;

	  /* Get the right arglist by handling PASS/NOPASS.  */
	  args = gfc_copy_actual_arglist (e->value.compcall.actual);
	  if (!g->specific->nopass)
	    {
	      gfc_expr* po;
	      po = extract_compcall_passed_object (e);
	      if (!po)
		{
		  gfc_free_actual_arglist (args);
		  return false;
		}

	      gcc_assert (g->specific->pass_arg_num > 0);
	      gcc_assert (!g->specific->error);
	      args = update_arglist_pass (args, po, g->specific->pass_arg_num,
					  g->specific->pass_arg);
	    }
	  resolve_actual_arglist (args, target->attr.proc,
				  is_external_proc (target)
				  && gfc_sym_get_dummy_args (target) == NULL);

	  /* Check if this arglist matches the formal.  */
	  matches = gfc_arglist_matches_symbol (&args, target);

	  /* Clean up and break out of the loop if we've found it.  */
	  gfc_free_actual_arglist (args);
	  if (matches)
	    {
	      e->value.compcall.tbp = g->specific;
	      genname = g->specific_st->name;
	      /* Pass along the name for CLASS methods, where the vtab
		 procedure pointer component has to be referenced.  */
	      if (name)
		*name = genname;
	      goto success;
	    }
	}
    }

  /* Nothing matching found!  */
  gfc_error ("Found no matching specific binding for the call to the GENERIC"
	     " %qs at %L", genname, &e->where);
  return false;

success:
  /* Make sure that we have the right specific instance for the name.  */
  derived = get_declared_from_expr (NULL, NULL, e, true);

  st = gfc_find_typebound_proc (derived, NULL, genname, true, &e->where);
  if (st)
    e->value.compcall.tbp = st->n.tb;

  return true;
}


/* Resolve a call to a type-bound subroutine.  */

static bool
resolve_typebound_call (gfc_code* c, const char **name, bool *overridable)
{
  gfc_actual_arglist* newactual;
  gfc_symtree* target;

  /* Check that's really a SUBROUTINE.  */
  if (!c->expr1->value.compcall.tbp->subroutine)
    {
      gfc_error ("%qs at %L should be a SUBROUTINE",
		 c->expr1->value.compcall.name, &c->loc);
      return false;
    }

  if (!check_typebound_baseobject (c->expr1))
    return false;

  /* Pass along the name for CLASS methods, where the vtab
     procedure pointer component has to be referenced.  */
  if (name)
    *name = c->expr1->value.compcall.name;

  if (!resolve_typebound_generic_call (c->expr1, name))
    return false;

  /* Pass along the NON_OVERRIDABLE attribute of the specific TBP. */
  if (overridable)
    *overridable = !c->expr1->value.compcall.tbp->non_overridable;

  /* Transform into an ordinary EXEC_CALL for now.  */

  if (!resolve_typebound_static (c->expr1, &target, &newactual))
    return false;

  c->ext.actual = newactual;
  c->symtree = target;
  c->op = (c->expr1->value.compcall.assign ? EXEC_ASSIGN_CALL : EXEC_CALL);

  gcc_assert (!c->expr1->ref && !c->expr1->value.compcall.actual);

  gfc_free_expr (c->expr1);
  c->expr1 = gfc_get_expr ();
  c->expr1->expr_type = EXPR_FUNCTION;
  c->expr1->symtree = target;
  c->expr1->where = c->loc;

  return resolve_call (c);
}


/* Resolve a component-call expression.  */
static bool
resolve_compcall (gfc_expr* e, const char **name)
{
  gfc_actual_arglist* newactual;
  gfc_symtree* target;

  /* Check that's really a FUNCTION.  */
  if (!e->value.compcall.tbp->function)
    {
      gfc_error ("%qs at %L should be a FUNCTION",
		 e->value.compcall.name, &e->where);
      return false;
    }

  /* These must not be assign-calls!  */
  gcc_assert (!e->value.compcall.assign);

  if (!check_typebound_baseobject (e))
    return false;

  /* Pass along the name for CLASS methods, where the vtab
     procedure pointer component has to be referenced.  */
  if (name)
    *name = e->value.compcall.name;

  if (!resolve_typebound_generic_call (e, name))
    return false;
  gcc_assert (!e->value.compcall.tbp->is_generic);

  /* Take the rank from the function's symbol.  */
  if (e->value.compcall.tbp->u.specific->n.sym->as)
    e->rank = e->value.compcall.tbp->u.specific->n.sym->as->rank;

  /* For now, we simply transform it into an EXPR_FUNCTION call with the same
     arglist to the TBP's binding target.  */

  if (!resolve_typebound_static (e, &target, &newactual))
    return false;

  e->value.function.actual = newactual;
  e->value.function.name = NULL;
  e->value.function.esym = target->n.sym;
  e->value.function.isym = NULL;
  e->symtree = target;
  e->ts = target->n.sym->ts;
  e->expr_type = EXPR_FUNCTION;

  /* Resolution is not necessary if this is a class subroutine; this
     function only has to identify the specific proc. Resolution of
     the call will be done next in resolve_typebound_call.  */
  return gfc_resolve_expr (e);
}


static bool resolve_fl_derived (gfc_symbol *sym);


/* Resolve a typebound function, or 'method'. First separate all
   the non-CLASS references by calling resolve_compcall directly.  */

static bool
resolve_typebound_function (gfc_expr* e)
{
  gfc_symbol *declared;
  gfc_component *c;
  gfc_ref *new_ref;
  gfc_ref *class_ref;
  gfc_symtree *st;
  const char *name;
  gfc_typespec ts;
  gfc_expr *expr;
  bool overridable;

  st = e->symtree;

  /* Deal with typebound operators for CLASS objects.  */
  expr = e->value.compcall.base_object;
  overridable = !e->value.compcall.tbp->non_overridable;
  if (expr && expr->ts.type == BT_CLASS && e->value.compcall.name)
    {
      /* If the base_object is not a variable, the corresponding actual
	 argument expression must be stored in e->base_expression so
	 that the corresponding tree temporary can be used as the base
	 object in gfc_conv_procedure_call.  */
      if (expr->expr_type != EXPR_VARIABLE)
	{
	  gfc_actual_arglist *args;

	  for (args= e->value.function.actual; args; args = args->next)
	    {
	      if (expr == args->expr)
		expr = args->expr;
	    }
	}

      /* Since the typebound operators are generic, we have to ensure
	 that any delays in resolution are corrected and that the vtab
	 is present.  */
      ts = expr->ts;
      declared = ts.u.derived;
      c = gfc_find_component (declared, "_vptr", true, true, NULL);
      if (c->ts.u.derived == NULL)
	c->ts.u.derived = gfc_find_derived_vtab (declared);

      if (!resolve_compcall (e, &name))
	return false;

      /* Use the generic name if it is there.  */
      name = name ? name : e->value.function.esym->name;
      e->symtree = expr->symtree;
      e->ref = gfc_copy_ref (expr->ref);
      get_declared_from_expr (&class_ref, NULL, e, false);

      /* Trim away the extraneous references that emerge from nested
	 use of interface.c (extend_expr).  */
      if (class_ref && class_ref->next)
	{
	  gfc_free_ref_list (class_ref->next);
	  class_ref->next = NULL;
	}
      else if (e->ref && !class_ref && expr->ts.type != BT_CLASS)
	{
	  gfc_free_ref_list (e->ref);
	  e->ref = NULL;
	}

      gfc_add_vptr_component (e);
      gfc_add_component_ref (e, name);
      e->value.function.esym = NULL;
      if (expr->expr_type != EXPR_VARIABLE)
	e->base_expr = expr;
      return true;
    }

  if (st == NULL)
    return resolve_compcall (e, NULL);

  if (!resolve_ref (e))
    return false;

  /* Get the CLASS declared type.  */
  declared = get_declared_from_expr (&class_ref, &new_ref, e, true);

  if (!resolve_fl_derived (declared))
    return false;

  /* Weed out cases of the ultimate component being a derived type.  */
  if ((class_ref && gfc_bt_struct (class_ref->u.c.component->ts.type))
	 || (!class_ref && st->n.sym->ts.type != BT_CLASS))
    {
      gfc_free_ref_list (new_ref);
      return resolve_compcall (e, NULL);
    }

  c = gfc_find_component (declared, "_data", true, true, NULL);
  declared = c->ts.u.derived;

  /* Treat the call as if it is a typebound procedure, in order to roll
     out the correct name for the specific function.  */
  if (!resolve_compcall (e, &name))
    {
      gfc_free_ref_list (new_ref);
      return false;
    }
  ts = e->ts;

  if (overridable)
    {
      /* Convert the expression to a procedure pointer component call.  */
      e->value.function.esym = NULL;
      e->symtree = st;

      if (new_ref)
	e->ref = new_ref;

      /* '_vptr' points to the vtab, which contains the procedure pointers.  */
      gfc_add_vptr_component (e);
      gfc_add_component_ref (e, name);

      /* Recover the typespec for the expression.  This is really only
	necessary for generic procedures, where the additional call
	to gfc_add_component_ref seems to throw the collection of the
	correct typespec.  */
      e->ts = ts;
    }
  else if (new_ref)
    gfc_free_ref_list (new_ref);

  return true;
}

/* Resolve a typebound subroutine, or 'method'. First separate all
   the non-CLASS references by calling resolve_typebound_call
   directly.  */

static bool
resolve_typebound_subroutine (gfc_code *code)
{
  gfc_symbol *declared;
  gfc_component *c;
  gfc_ref *new_ref;
  gfc_ref *class_ref;
  gfc_symtree *st;
  const char *name;
  gfc_typespec ts;
  gfc_expr *expr;
  bool overridable;

  st = code->expr1->symtree;

  /* Deal with typebound operators for CLASS objects.  */
  expr = code->expr1->value.compcall.base_object;
  overridable = !code->expr1->value.compcall.tbp->non_overridable;
  if (expr && expr->ts.type == BT_CLASS && code->expr1->value.compcall.name)
    {
      /* If the base_object is not a variable, the corresponding actual
	 argument expression must be stored in e->base_expression so
	 that the corresponding tree temporary can be used as the base
	 object in gfc_conv_procedure_call.  */
      if (expr->expr_type != EXPR_VARIABLE)
	{
	  gfc_actual_arglist *args;

	  args= code->expr1->value.function.actual;
	  for (; args; args = args->next)
	    if (expr == args->expr)
	      expr = args->expr;
	}

      /* Since the typebound operators are generic, we have to ensure
	 that any delays in resolution are corrected and that the vtab
	 is present.  */
      declared = expr->ts.u.derived;
      c = gfc_find_component (declared, "_vptr", true, true, NULL);
      if (c->ts.u.derived == NULL)
	c->ts.u.derived = gfc_find_derived_vtab (declared);

      if (!resolve_typebound_call (code, &name, NULL))
	return false;

      /* Use the generic name if it is there.  */
      name = name ? name : code->expr1->value.function.esym->name;
      code->expr1->symtree = expr->symtree;
      code->expr1->ref = gfc_copy_ref (expr->ref);

      /* Trim away the extraneous references that emerge from nested
	 use of interface.c (extend_expr).  */
      get_declared_from_expr (&class_ref, NULL, code->expr1, false);
      if (class_ref && class_ref->next)
	{
	  gfc_free_ref_list (class_ref->next);
	  class_ref->next = NULL;
	}
      else if (code->expr1->ref && !class_ref)
	{
	  gfc_free_ref_list (code->expr1->ref);
	  code->expr1->ref = NULL;
	}

      /* Now use the procedure in the vtable.  */
      gfc_add_vptr_component (code->expr1);
      gfc_add_component_ref (code->expr1, name);
      code->expr1->value.function.esym = NULL;
      if (expr->expr_type != EXPR_VARIABLE)
	code->expr1->base_expr = expr;
      return true;
    }

  if (st == NULL)
    return resolve_typebound_call (code, NULL, NULL);

  if (!resolve_ref (code->expr1))
    return false;

  /* Get the CLASS declared type.  */
  get_declared_from_expr (&class_ref, &new_ref, code->expr1, true);

  /* Weed out cases of the ultimate component being a derived type.  */
  if ((class_ref && gfc_bt_struct (class_ref->u.c.component->ts.type))
	 || (!class_ref && st->n.sym->ts.type != BT_CLASS))
    {
      gfc_free_ref_list (new_ref);
      return resolve_typebound_call (code, NULL, NULL);
    }

  if (!resolve_typebound_call (code, &name, &overridable))
    {
      gfc_free_ref_list (new_ref);
      return false;
    }
  ts = code->expr1->ts;

  if (overridable)
    {
      /* Convert the expression to a procedure pointer component call.  */
      code->expr1->value.function.esym = NULL;
      code->expr1->symtree = st;

      if (new_ref)
	code->expr1->ref = new_ref;

      /* '_vptr' points to the vtab, which contains the procedure pointers.  */
      gfc_add_vptr_component (code->expr1);
      gfc_add_component_ref (code->expr1, name);

      /* Recover the typespec for the expression.  This is really only
	necessary for generic procedures, where the additional call
	to gfc_add_component_ref seems to throw the collection of the
	correct typespec.  */
      code->expr1->ts = ts;
    }
  else if (new_ref)
    gfc_free_ref_list (new_ref);

  return true;
}


/* Resolve a CALL to a Procedure Pointer Component (Subroutine).  */

static bool
resolve_ppc_call (gfc_code* c)
{
  gfc_component *comp;

  comp = gfc_get_proc_ptr_comp (c->expr1);
  gcc_assert (comp != NULL);

  c->resolved_sym = c->expr1->symtree->n.sym;
  c->expr1->expr_type = EXPR_VARIABLE;

  if (!comp->attr.subroutine)
    gfc_add_subroutine (&comp->attr, comp->name, &c->expr1->where);

  if (!resolve_ref (c->expr1))
    return false;

  if (!update_ppc_arglist (c->expr1))
    return false;

  c->ext.actual = c->expr1->value.compcall.actual;

  if (!resolve_actual_arglist (c->ext.actual, comp->attr.proc,
			       !(comp->ts.interface
				 && comp->ts.interface->formal)))
    return false;

  if (!pure_subroutine (comp->ts.interface, comp->name, &c->expr1->where))
    return false;

  gfc_ppc_use (comp, &c->expr1->value.compcall.actual, &c->expr1->where);

  return true;
}


/* Resolve a Function Call to a Procedure Pointer Component (Function).  */

static bool
resolve_expr_ppc (gfc_expr* e)
{
  gfc_component *comp;

  comp = gfc_get_proc_ptr_comp (e);
  gcc_assert (comp != NULL);

  /* Convert to EXPR_FUNCTION.  */
  e->expr_type = EXPR_FUNCTION;
  e->value.function.isym = NULL;
  e->value.function.actual = e->value.compcall.actual;
  e->ts = comp->ts;
  if (comp->as != NULL)
    e->rank = comp->as->rank;

  if (!comp->attr.function)
    gfc_add_function (&comp->attr, comp->name, &e->where);

  if (!resolve_ref (e))
    return false;

  if (!resolve_actual_arglist (e->value.function.actual, comp->attr.proc,
			       !(comp->ts.interface
				 && comp->ts.interface->formal)))
    return false;

  if (!update_ppc_arglist (e))
    return false;

  if (!check_pure_function(e))
    return false;

  gfc_ppc_use (comp, &e->value.compcall.actual, &e->where);

  return true;
}


static bool
gfc_is_expandable_expr (gfc_expr *e)
{
  gfc_constructor *con;

  if (e->expr_type == EXPR_ARRAY)
    {
      /* Traverse the constructor looking for variables that are flavor
	 parameter.  Parameters must be expanded since they are fully used at
	 compile time.  */
      con = gfc_constructor_first (e->value.constructor);
      for (; con; con = gfc_constructor_next (con))
	{
	  if (con->expr->expr_type == EXPR_VARIABLE
	      && con->expr->symtree
	      && (con->expr->symtree->n.sym->attr.flavor == FL_PARAMETER
	      || con->expr->symtree->n.sym->attr.flavor == FL_VARIABLE))
	    return true;
	  if (con->expr->expr_type == EXPR_ARRAY
	      && gfc_is_expandable_expr (con->expr))
	    return true;
	}
    }

  return false;
}


/* Sometimes variables in specification expressions of the result
   of module procedures in submodules wind up not being the 'real'
   dummy.  Find this, if possible, in the namespace of the first
   formal argument.  */

static void
fixup_unique_dummy (gfc_expr *e)
{
  gfc_symtree *st = NULL;
  gfc_symbol *s = NULL;

  if (e->symtree->n.sym->ns->proc_name
      && e->symtree->n.sym->ns->proc_name->formal)
    s = e->symtree->n.sym->ns->proc_name->formal->sym;

  if (s != NULL)
    st = gfc_find_symtree (s->ns->sym_root, e->symtree->n.sym->name);

  if (st != NULL
      && st->n.sym != NULL
      && st->n.sym->attr.dummy)
    e->symtree = st;
}

/* Resolve an expression.  That is, make sure that types of operands agree
   with their operators, intrinsic operators are converted to function calls
   for overloaded types and unresolved function references are resolved.  */

bool
gfc_resolve_expr (gfc_expr *e)
{
  bool t;
  bool inquiry_save, actual_arg_save, first_actual_arg_save;

  if (e == NULL)
    return true;

  /* inquiry_argument only applies to variables.  */
  inquiry_save = inquiry_argument;
  actual_arg_save = actual_arg;
  first_actual_arg_save = first_actual_arg;

  if (e->expr_type != EXPR_VARIABLE)
    {
      inquiry_argument = false;
      actual_arg = false;
      first_actual_arg = false;
    }
  else if (e->symtree != NULL
	   && *e->symtree->name == '@'
	   && e->symtree->n.sym->attr.dummy)
    {
      /* Deal with submodule specification expressions that are not
	 found to be referenced in module.c(read_cleanup).  */
      fixup_unique_dummy (e);
    }

  switch (e->expr_type)
    {
    case EXPR_OP:
      t = resolve_operator (e);
      break;

    case EXPR_FUNCTION:
    case EXPR_VARIABLE:

      if (check_host_association (e))
	t = resolve_function (e);
      else
	t = resolve_variable (e);

      if (e->ts.type == BT_CHARACTER && e->ts.u.cl == NULL && e->ref
	  && e->ref->type != REF_SUBSTRING)
	gfc_resolve_substring_charlen (e);

      break;

    case EXPR_COMPCALL:
      t = resolve_typebound_function (e);
      break;

    case EXPR_SUBSTRING:
      t = resolve_ref (e);
      break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
      t = true;
      break;

    case EXPR_PPC:
      t = resolve_expr_ppc (e);
      break;

    case EXPR_ARRAY:
      t = false;
      if (!resolve_ref (e))
	break;

      t = gfc_resolve_array_constructor (e);
      /* Also try to expand a constructor.  */
      if (t)
	{
	  expression_rank (e);
	  if (gfc_is_constant_expr (e) || gfc_is_expandable_expr (e))
	    gfc_expand_constructor (e, false);
	}

      /* This provides the opportunity for the length of constructors with
	 character valued function elements to propagate the string length
	 to the expression.  */
      if (t && e->ts.type == BT_CHARACTER)
        {
	  /* For efficiency, we call gfc_expand_constructor for BT_CHARACTER
	     here rather then add a duplicate test for it above.  */
	  gfc_expand_constructor (e, false);
	  t = gfc_resolve_character_array_constructor (e);
	}

      break;

    case EXPR_STRUCTURE:
      t = resolve_ref (e);
      if (!t)
	break;

      t = resolve_structure_cons (e, 0);
      if (!t)
	break;

      t = gfc_simplify_expr (e, 0);
      break;

    default:
      gfc_internal_error ("gfc_resolve_expr(): Bad expression type");
    }

  if (e->ts.type == BT_CHARACTER && t && !e->ts.u.cl)
    fixup_charlen (e);

  inquiry_argument = inquiry_save;
  actual_arg = actual_arg_save;
  first_actual_arg = first_actual_arg_save;

  return t;
}


/* Resolve an expression from an iterator.  They must be scalar and have
   INTEGER or (optionally) REAL type.  */

static bool
gfc_resolve_iterator_expr (gfc_expr *expr, bool real_ok,
			   const char *name_msgid)
{
  if (!gfc_resolve_expr (expr))
    return false;

  if (expr->rank != 0)
    {
      gfc_error ("%s at %L must be a scalar", _(name_msgid), &expr->where);
      return false;
    }

  if (expr->ts.type != BT_INTEGER)
    {
      if (expr->ts.type == BT_REAL)
	{
	  if (real_ok)
	    return gfc_notify_std (GFC_STD_F95_DEL,
				   "%s at %L must be integer",
				   _(name_msgid), &expr->where);
	  else
	    {
	      gfc_error ("%s at %L must be INTEGER", _(name_msgid),
			 &expr->where);
	      return false;
	    }
	}
      else
	{
	  gfc_error ("%s at %L must be INTEGER", _(name_msgid), &expr->where);
	  return false;
	}
    }
  return true;
}


/* Resolve the expressions in an iterator structure.  If REAL_OK is
   false allow only INTEGER type iterators, otherwise allow REAL types.
   Set own_scope to true for ac-implied-do and data-implied-do as those
   have a separate scope such that, e.g., a INTENT(IN) doesn't apply.  */

bool
gfc_resolve_iterator (gfc_iterator *iter, bool real_ok, bool own_scope)
{
  if (!gfc_resolve_iterator_expr (iter->var, real_ok, "Loop variable"))
    return false;

  if (!gfc_check_vardef_context (iter->var, false, false, own_scope,
				 _("iterator variable")))
    return false;

  if (!gfc_resolve_iterator_expr (iter->start, real_ok,
				  "Start expression in DO loop"))
    return false;

  if (!gfc_resolve_iterator_expr (iter->end, real_ok,
				  "End expression in DO loop"))
    return false;

  if (!gfc_resolve_iterator_expr (iter->step, real_ok,
				  "Step expression in DO loop"))
    return false;

  if (iter->step->expr_type == EXPR_CONSTANT)
    {
      if ((iter->step->ts.type == BT_INTEGER
	   && mpz_cmp_ui (iter->step->value.integer, 0) == 0)
	  || (iter->step->ts.type == BT_REAL
	      && mpfr_sgn (iter->step->value.real) == 0))
	{
	  gfc_error ("Step expression in DO loop at %L cannot be zero",
		     &iter->step->where);
	  return false;
	}
    }

  /* Convert start, end, and step to the same type as var.  */
  if (iter->start->ts.kind != iter->var->ts.kind
      || iter->start->ts.type != iter->var->ts.type)
    gfc_convert_type (iter->start, &iter->var->ts, 1);

  if (iter->end->ts.kind != iter->var->ts.kind
      || iter->end->ts.type != iter->var->ts.type)
    gfc_convert_type (iter->end, &iter->var->ts, 1);

  if (iter->step->ts.kind != iter->var->ts.kind
      || iter->step->ts.type != iter->var->ts.type)
    gfc_convert_type (iter->step, &iter->var->ts, 1);

  if (iter->start->expr_type == EXPR_CONSTANT
      && iter->end->expr_type == EXPR_CONSTANT
      && iter->step->expr_type == EXPR_CONSTANT)
    {
      int sgn, cmp;
      if (iter->start->ts.type == BT_INTEGER)
	{
	  sgn = mpz_cmp_ui (iter->step->value.integer, 0);
	  cmp = mpz_cmp (iter->end->value.integer, iter->start->value.integer);
	}
      else
	{
	  sgn = mpfr_sgn (iter->step->value.real);
	  cmp = mpfr_cmp (iter->end->value.real, iter->start->value.real);
	}
      if (warn_zerotrip && ((sgn > 0 && cmp < 0) || (sgn < 0 && cmp > 0)))
	gfc_warning (OPT_Wzerotrip,
		     "DO loop at %L will be executed zero times",
		     &iter->step->where);
    }

  if (iter->end->expr_type == EXPR_CONSTANT
      && iter->end->ts.type == BT_INTEGER
      && iter->step->expr_type == EXPR_CONSTANT
      && iter->step->ts.type == BT_INTEGER
      && (mpz_cmp_si (iter->step->value.integer, -1L) == 0
	  || mpz_cmp_si (iter->step->value.integer, 1L) == 0))
    {
      bool is_step_positive = mpz_cmp_ui (iter->step->value.integer, 1) == 0;
      int k = gfc_validate_kind (BT_INTEGER, iter->end->ts.kind, false);

      if (is_step_positive
	  && mpz_cmp (iter->end->value.integer, gfc_integer_kinds[k].huge) == 0)
	gfc_warning (OPT_Wundefined_do_loop,
		     "DO loop at %L is undefined as it overflows",
		     &iter->step->where);
      else if (!is_step_positive
	       && mpz_cmp (iter->end->value.integer,
			   gfc_integer_kinds[k].min_int) == 0)
	gfc_warning (OPT_Wundefined_do_loop,
		     "DO loop at %L is undefined as it underflows",
		     &iter->step->where);
    }

  return true;
}


/* Traversal function for find_forall_index.  f == 2 signals that
   that variable itself is not to be checked - only the references.  */

static bool
forall_index (gfc_expr *expr, gfc_symbol *sym, int *f)
{
  if (expr->expr_type != EXPR_VARIABLE)
    return false;

  /* A scalar assignment  */
  if (!expr->ref || *f == 1)
    {
      if (expr->symtree->n.sym == sym)
	return true;
      else
	return false;
    }

  if (*f == 2)
    *f = 1;
  return false;
}


/* Check whether the FORALL index appears in the expression or not.
   Returns true if SYM is found in EXPR.  */

bool
find_forall_index (gfc_expr *expr, gfc_symbol *sym, int f)
{
  if (gfc_traverse_expr (expr, sym, forall_index, f))
    return true;
  else
    return false;
}


/* Resolve a list of FORALL iterators.  The FORALL index-name is constrained
   to be a scalar INTEGER variable.  The subscripts and stride are scalar
   INTEGERs, and if stride is a constant it must be nonzero.
   Furthermore "A subscript or stride in a forall-triplet-spec shall
   not contain a reference to any index-name in the
   forall-triplet-spec-list in which it appears." (7.5.4.1)  */

static void
resolve_forall_iterators (gfc_forall_iterator *it)
{
  gfc_forall_iterator *iter, *iter2;

  for (iter = it; iter; iter = iter->next)
    {
      if (gfc_resolve_expr (iter->var)
	  && (iter->var->ts.type != BT_INTEGER || iter->var->rank != 0))
	gfc_error ("FORALL index-name at %L must be a scalar INTEGER",
		   &iter->var->where);

      if (gfc_resolve_expr (iter->start)
	  && (iter->start->ts.type != BT_INTEGER || iter->start->rank != 0))
	gfc_error ("FORALL start expression at %L must be a scalar INTEGER",
		   &iter->start->where);
      if (iter->var->ts.kind != iter->start->ts.kind)
	gfc_convert_type (iter->start, &iter->var->ts, 1);

      if (gfc_resolve_expr (iter->end)
	  && (iter->end->ts.type != BT_INTEGER || iter->end->rank != 0))
	gfc_error ("FORALL end expression at %L must be a scalar INTEGER",
		   &iter->end->where);
      if (iter->var->ts.kind != iter->end->ts.kind)
	gfc_convert_type (iter->end, &iter->var->ts, 1);

      if (gfc_resolve_expr (iter->stride))
	{
	  if (iter->stride->ts.type != BT_INTEGER || iter->stride->rank != 0)
	    gfc_error ("FORALL stride expression at %L must be a scalar %s",
		       &iter->stride->where, "INTEGER");

	  if (iter->stride->expr_type == EXPR_CONSTANT
	      && mpz_cmp_ui (iter->stride->value.integer, 0) == 0)
	    gfc_error ("FORALL stride expression at %L cannot be zero",
		       &iter->stride->where);
	}
      if (iter->var->ts.kind != iter->stride->ts.kind)
	gfc_convert_type (iter->stride, &iter->var->ts, 1);
    }

  for (iter = it; iter; iter = iter->next)
    for (iter2 = iter; iter2; iter2 = iter2->next)
      {
	if (find_forall_index (iter2->start, iter->var->symtree->n.sym, 0)
	    || find_forall_index (iter2->end, iter->var->symtree->n.sym, 0)
	    || find_forall_index (iter2->stride, iter->var->symtree->n.sym, 0))
	  gfc_error ("FORALL index %qs may not appear in triplet "
		     "specification at %L", iter->var->symtree->name,
		     &iter2->start->where);
      }
}


/* Given a pointer to a symbol that is a derived type, see if it's
   inaccessible, i.e. if it's defined in another module and the components are
   PRIVATE.  The search is recursive if necessary.  Returns zero if no
   inaccessible components are found, nonzero otherwise.  */

static int
derived_inaccessible (gfc_symbol *sym)
{
  gfc_component *c;

  if (sym->attr.use_assoc && sym->attr.private_comp)
    return 1;

  for (c = sym->components; c; c = c->next)
    {
	/* Prevent an infinite loop through this function.  */
	if (c->ts.type == BT_DERIVED && c->attr.pointer
	    && sym == c->ts.u.derived)
	  continue;

	if (c->ts.type == BT_DERIVED && derived_inaccessible (c->ts.u.derived))
	  return 1;
    }

  return 0;
}


/* Resolve the argument of a deallocate expression.  The expression must be
   a pointer or a full array.  */

static bool
resolve_deallocate_expr (gfc_expr *e)
{
  symbol_attribute attr;
  int allocatable, pointer;
  gfc_ref *ref;
  gfc_symbol *sym;
  gfc_component *c;
  bool unlimited;

  if (!gfc_resolve_expr (e))
    return false;

  if (e->expr_type != EXPR_VARIABLE)
    goto bad;

  sym = e->symtree->n.sym;
  unlimited = UNLIMITED_POLY(sym);

  if (sym->ts.type == BT_CLASS)
    {
      allocatable = CLASS_DATA (sym)->attr.allocatable;
      pointer = CLASS_DATA (sym)->attr.class_pointer;
    }
  else
    {
      allocatable = sym->attr.allocatable;
      pointer = sym->attr.pointer;
    }
  for (ref = e->ref; ref; ref = ref->next)
    {
      switch (ref->type)
	{
	case REF_ARRAY:
	  if (ref->u.ar.type != AR_FULL
	      && !(ref->u.ar.type == AR_ELEMENT && ref->u.ar.as->rank == 0
	           && ref->u.ar.codimen && gfc_ref_this_image (ref)))
	    allocatable = 0;
	  break;

	case REF_COMPONENT:
	  c = ref->u.c.component;
	  if (c->ts.type == BT_CLASS)
	    {
	      allocatable = CLASS_DATA (c)->attr.allocatable;
	      pointer = CLASS_DATA (c)->attr.class_pointer;
	    }
	  else
	    {
	      allocatable = c->attr.allocatable;
	      pointer = c->attr.pointer;
	    }
	  break;

	case REF_SUBSTRING:
	  allocatable = 0;
	  break;
	}
    }

  attr = gfc_expr_attr (e);

  if (allocatable == 0 && attr.pointer == 0 && !unlimited)
    {
    bad:
      gfc_error ("Allocate-object at %L must be ALLOCATABLE or a POINTER",
		 &e->where);
      return false;
    }

  /* F2008, C644.  */
  if (gfc_is_coindexed (e))
    {
      gfc_error ("Coindexed allocatable object at %L", &e->where);
      return false;
    }

  if (pointer
      && !gfc_check_vardef_context (e, true, true, false,
				    _("DEALLOCATE object")))
    return false;
  if (!gfc_check_vardef_context (e, false, true, false,
				 _("DEALLOCATE object")))
    return false;

  return true;
}


/* Returns true if the expression e contains a reference to the symbol sym.  */
static bool
sym_in_expr (gfc_expr *e, gfc_symbol *sym, int *f ATTRIBUTE_UNUSED)
{
  if (e->expr_type == EXPR_VARIABLE && e->symtree->n.sym == sym)
    return true;

  return false;
}

bool
gfc_find_sym_in_expr (gfc_symbol *sym, gfc_expr *e)
{
  return gfc_traverse_expr (e, sym, sym_in_expr, 0);
}


/* Given the expression node e for an allocatable/pointer of derived type to be
   allocated, get the expression node to be initialized afterwards (needed for
   derived types with default initializers, and derived types with allocatable
   components that need nullification.)  */

gfc_expr *
gfc_expr_to_initialize (gfc_expr *e)
{
  gfc_expr *result;
  gfc_ref *ref;
  int i;

  result = gfc_copy_expr (e);

  /* Change the last array reference from AR_ELEMENT to AR_FULL.  */
  for (ref = result->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->next == NULL)
      {
	ref->u.ar.type = AR_FULL;

	for (i = 0; i < ref->u.ar.dimen; i++)
	  ref->u.ar.start[i] = ref->u.ar.end[i] = ref->u.ar.stride[i] = NULL;

	break;
      }

  gfc_free_shape (&result->shape, result->rank);

  /* Recalculate rank, shape, etc.  */
  gfc_resolve_expr (result);
  return result;
}


/* If the last ref of an expression is an array ref, return a copy of the
   expression with that one removed.  Otherwise, a copy of the original
   expression.  This is used for allocate-expressions and pointer assignment
   LHS, where there may be an array specification that needs to be stripped
   off when using gfc_check_vardef_context.  */

static gfc_expr*
remove_last_array_ref (gfc_expr* e)
{
  gfc_expr* e2;
  gfc_ref** r;

  e2 = gfc_copy_expr (e);
  for (r = &e2->ref; *r; r = &(*r)->next)
    if ((*r)->type == REF_ARRAY && !(*r)->next)
      {
	gfc_free_ref_list (*r);
	*r = NULL;
	break;
      }

  return e2;
}


/* Used in resolve_allocate_expr to check that a allocation-object and
   a source-expr are conformable.  This does not catch all possible
   cases; in particular a runtime checking is needed.  */

static bool
conformable_arrays (gfc_expr *e1, gfc_expr *e2)
{
  gfc_ref *tail;
  for (tail = e2->ref; tail && tail->next; tail = tail->next);

  /* First compare rank.  */
  if ((tail && e1->rank != tail->u.ar.as->rank)
      || (!tail && e1->rank != e2->rank))
    {
      gfc_error ("Source-expr at %L must be scalar or have the "
		 "same rank as the allocate-object at %L",
		 &e1->where, &e2->where);
      return false;
    }

  if (e1->shape)
    {
      int i;
      mpz_t s;

      mpz_init (s);

      for (i = 0; i < e1->rank; i++)
	{
	  if (tail->u.ar.start[i] == NULL)
	    break;

	  if (tail->u.ar.end[i])
	    {
	      mpz_set (s, tail->u.ar.end[i]->value.integer);
	      mpz_sub (s, s, tail->u.ar.start[i]->value.integer);
	      mpz_add_ui (s, s, 1);
	    }
	  else
	    {
	      mpz_set (s, tail->u.ar.start[i]->value.integer);
	    }

	  if (mpz_cmp (e1->shape[i], s) != 0)
	    {
	      gfc_error ("Source-expr at %L and allocate-object at %L must "
			 "have the same shape", &e1->where, &e2->where);
	      mpz_clear (s);
   	      return false;
	    }
	}

      mpz_clear (s);
    }

  return true;
}


/* Resolve the expression in an ALLOCATE statement, doing the additional
   checks to see whether the expression is OK or not.  The expression must
   have a trailing array reference that gives the size of the array.  */

static bool
resolve_allocate_expr (gfc_expr *e, gfc_code *code, bool *array_alloc_wo_spec)
{
  int i, pointer, allocatable, dimension, is_abstract;
  int codimension;
  bool coindexed;
  bool unlimited;
  symbol_attribute attr;
  gfc_ref *ref, *ref2;
  gfc_expr *e2;
  gfc_array_ref *ar;
  gfc_symbol *sym = NULL;
  gfc_alloc *a;
  gfc_component *c;
  bool t;

  /* Mark the utmost array component as being in allocate to allow DIMEN_STAR
     checking of coarrays.  */
  for (ref = e->ref; ref; ref = ref->next)
    if (ref->next == NULL)
      break;

  if (ref && ref->type == REF_ARRAY)
    ref->u.ar.in_allocate = true;

  if (!gfc_resolve_expr (e))
    goto failure;

  /* Make sure the expression is allocatable or a pointer.  If it is
     pointer, the next-to-last reference must be a pointer.  */

  ref2 = NULL;
  if (e->symtree)
    sym = e->symtree->n.sym;

  /* Check whether ultimate component is abstract and CLASS.  */
  is_abstract = 0;

  /* Is the allocate-object unlimited polymorphic?  */
  unlimited = UNLIMITED_POLY(e);

  if (e->expr_type != EXPR_VARIABLE)
    {
      allocatable = 0;
      attr = gfc_expr_attr (e);
      pointer = attr.pointer;
      dimension = attr.dimension;
      codimension = attr.codimension;
    }
  else
    {
      if (sym->ts.type == BT_CLASS && CLASS_DATA (sym))
	{
	  allocatable = CLASS_DATA (sym)->attr.allocatable;
	  pointer = CLASS_DATA (sym)->attr.class_pointer;
	  dimension = CLASS_DATA (sym)->attr.dimension;
	  codimension = CLASS_DATA (sym)->attr.codimension;
	  is_abstract = CLASS_DATA (sym)->attr.abstract;
	}
      else
	{
	  allocatable = sym->attr.allocatable;
	  pointer = sym->attr.pointer;
	  dimension = sym->attr.dimension;
	  codimension = sym->attr.codimension;
	}

      coindexed = false;

      for (ref = e->ref; ref; ref2 = ref, ref = ref->next)
	{
	  switch (ref->type)
	    {
 	      case REF_ARRAY:
                if (ref->u.ar.codimen > 0)
		  {
		    int n;
		    for (n = ref->u.ar.dimen;
			 n < ref->u.ar.dimen + ref->u.ar.codimen; n++)
		      if (ref->u.ar.dimen_type[n] != DIMEN_THIS_IMAGE)
			{
			  coindexed = true;
			  break;
			}
		   }

		if (ref->next != NULL)
		  pointer = 0;
		break;

	      case REF_COMPONENT:
		/* F2008, C644.  */
		if (coindexed)
		  {
		    gfc_error ("Coindexed allocatable object at %L",
			       &e->where);
		    goto failure;
		  }

		c = ref->u.c.component;
		if (c->ts.type == BT_CLASS)
		  {
		    allocatable = CLASS_DATA (c)->attr.allocatable;
		    pointer = CLASS_DATA (c)->attr.class_pointer;
		    dimension = CLASS_DATA (c)->attr.dimension;
		    codimension = CLASS_DATA (c)->attr.codimension;
		    is_abstract = CLASS_DATA (c)->attr.abstract;
		  }
		else
		  {
		    allocatable = c->attr.allocatable;
		    pointer = c->attr.pointer;
		    dimension = c->attr.dimension;
		    codimension = c->attr.codimension;
		    is_abstract = c->attr.abstract;
		  }
		break;

	      case REF_SUBSTRING:
		allocatable = 0;
		pointer = 0;
		break;
	    }
	}
    }

  /* Check for F08:C628.  */
  if (allocatable == 0 && pointer == 0 && !unlimited)
    {
      gfc_error ("Allocate-object at %L must be ALLOCATABLE or a POINTER",
		 &e->where);
      goto failure;
    }

  /* Some checks for the SOURCE tag.  */
  if (code->expr3)
    {
      /* Check F03:C631.  */
      if (!gfc_type_compatible (&e->ts, &code->expr3->ts))
	{
	  gfc_error ("Type of entity at %L is type incompatible with "
		     "source-expr at %L", &e->where, &code->expr3->where);
	  goto failure;
	}

      /* Check F03:C632 and restriction following Note 6.18.  */
      if (code->expr3->rank > 0 && !conformable_arrays (code->expr3, e))
	goto failure;

      /* Check F03:C633.  */
      if (code->expr3->ts.kind != e->ts.kind && !unlimited)
	{
	  gfc_error ("The allocate-object at %L and the source-expr at %L "
		     "shall have the same kind type parameter",
		     &e->where, &code->expr3->where);
	  goto failure;
	}

      /* Check F2008, C642.  */
      if (code->expr3->ts.type == BT_DERIVED
	  && ((codimension && gfc_expr_attr (code->expr3).lock_comp)
	      || (code->expr3->ts.u.derived->from_intmod
		     == INTMOD_ISO_FORTRAN_ENV
		  && code->expr3->ts.u.derived->intmod_sym_id
		     == ISOFORTRAN_LOCK_TYPE)))
	{
	  gfc_error ("The source-expr at %L shall neither be of type "
		     "LOCK_TYPE nor have a LOCK_TYPE component if "
		      "allocate-object at %L is a coarray",
		      &code->expr3->where, &e->where);
	  goto failure;
	}

      /* Check TS18508, C702/C703.  */
      if (code->expr3->ts.type == BT_DERIVED
	  && ((codimension && gfc_expr_attr (code->expr3).event_comp)
	      || (code->expr3->ts.u.derived->from_intmod
		     == INTMOD_ISO_FORTRAN_ENV
		  && code->expr3->ts.u.derived->intmod_sym_id
		     == ISOFORTRAN_EVENT_TYPE)))
	{
	  gfc_error ("The source-expr at %L shall neither be of type "
		     "EVENT_TYPE nor have a EVENT_TYPE component if "
		      "allocate-object at %L is a coarray",
		      &code->expr3->where, &e->where);
	  goto failure;
	}
    }

  /* Check F08:C629.  */
  if (is_abstract && code->ext.alloc.ts.type == BT_UNKNOWN
      && !code->expr3)
    {
      gcc_assert (e->ts.type == BT_CLASS);
      gfc_error ("Allocating %s of ABSTRACT base type at %L requires a "
		 "type-spec or source-expr", sym->name, &e->where);
      goto failure;
    }

  /* Check F08:C632.  */
  if (code->ext.alloc.ts.type == BT_CHARACTER && !e->ts.deferred
      && !UNLIMITED_POLY (e))
    {
      int cmp = gfc_dep_compare_expr (e->ts.u.cl->length,
				      code->ext.alloc.ts.u.cl->length);
      if (cmp == 1 || cmp == -1 || cmp == -3)
	{
	  gfc_error ("Allocating %s at %L with type-spec requires the same "
		     "character-length parameter as in the declaration",
		     sym->name, &e->where);
	  goto failure;
	}
    }

  /* In the variable definition context checks, gfc_expr_attr is used
     on the expression.  This is fooled by the array specification
     present in e, thus we have to eliminate that one temporarily.  */
  e2 = remove_last_array_ref (e);
  t = true;
  if (t && pointer)
    t = gfc_check_vardef_context (e2, true, true, false,
				  _("ALLOCATE object"));
  if (t)
    t = gfc_check_vardef_context (e2, false, true, false,
				  _("ALLOCATE object"));
  gfc_free_expr (e2);
  if (!t)
    goto failure;

  if (e->ts.type == BT_CLASS && CLASS_DATA (e)->attr.dimension
	&& !code->expr3 && code->ext.alloc.ts.type == BT_DERIVED)
    {
      /* For class arrays, the initialization with SOURCE is done
	 using _copy and trans_call. It is convenient to exploit that
	 when the allocated type is different from the declared type but
	 no SOURCE exists by setting expr3.  */
      code->expr3 = gfc_default_initializer (&code->ext.alloc.ts);
    }
  else if (flag_coarray != GFC_FCOARRAY_LIB && e->ts.type == BT_DERIVED
	   && e->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
	   && e->ts.u.derived->intmod_sym_id == ISOFORTRAN_EVENT_TYPE)
    {
      /* We have to zero initialize the integer variable.  */
      code->expr3 = gfc_get_int_expr (gfc_default_integer_kind, &e->where, 0);
    }

  if (e->ts.type == BT_CLASS && !unlimited && !UNLIMITED_POLY (code->expr3))
    {
      /* Make sure the vtab symbol is present when
	 the module variables are generated.  */
      gfc_typespec ts = e->ts;
      if (code->expr3)
	ts = code->expr3->ts;
      else if (code->ext.alloc.ts.type == BT_DERIVED)
	ts = code->ext.alloc.ts;

      /* Finding the vtab also publishes the type's symbol.  Therefore this
	 statement is necessary.  */
      gfc_find_derived_vtab (ts.u.derived);
    }
  else if (unlimited && !UNLIMITED_POLY (code->expr3))
    {
      /* Again, make sure the vtab symbol is present when
	 the module variables are generated.  */
      gfc_typespec *ts = NULL;
      if (code->expr3)
	ts = &code->expr3->ts;
      else
	ts = &code->ext.alloc.ts;

      gcc_assert (ts);

      /* Finding the vtab also publishes the type's symbol.  Therefore this
	 statement is necessary.  */
      gfc_find_vtab (ts);
    }

  if (dimension == 0 && codimension == 0)
    goto success;

  /* Make sure the last reference node is an array specification.  */

  if (!ref2 || ref2->type != REF_ARRAY || ref2->u.ar.type == AR_FULL
      || (dimension && ref2->u.ar.dimen == 0))
    {
      /* F08:C633.  */
      if (code->expr3)
	{
	  if (!gfc_notify_std (GFC_STD_F2008, "Array specification required "
			       "in ALLOCATE statement at %L", &e->where))
	    goto failure;
	  if (code->expr3->rank != 0)
	    *array_alloc_wo_spec = true;
	  else
	    {
	      gfc_error ("Array specification or array-valued SOURCE= "
			 "expression required in ALLOCATE statement at %L",
			 &e->where);
	      goto failure;
	    }
	}
      else
	{
	  gfc_error ("Array specification required in ALLOCATE statement "
		     "at %L", &e->where);
	  goto failure;
	}
    }

  /* Make sure that the array section reference makes sense in the
     context of an ALLOCATE specification.  */

  ar = &ref2->u.ar;

  if (codimension)
    for (i = ar->dimen; i < ar->dimen + ar->codimen; i++)
      if (ar->dimen_type[i] == DIMEN_THIS_IMAGE)
	{
	  gfc_error ("Coarray specification required in ALLOCATE statement "
		     "at %L", &e->where);
	  goto failure;
	}

  for (i = 0; i < ar->dimen; i++)
    {
      if (ar->type == AR_ELEMENT || ar->type == AR_FULL)
	goto check_symbols;

      switch (ar->dimen_type[i])
	{
	case DIMEN_ELEMENT:
	  break;

	case DIMEN_RANGE:
	  if (ar->start[i] != NULL
	      && ar->end[i] != NULL
	      && ar->stride[i] == NULL)
	    break;

	  /* Fall through.  */

	case DIMEN_UNKNOWN:
	case DIMEN_VECTOR:
	case DIMEN_STAR:
	case DIMEN_THIS_IMAGE:
	  gfc_error ("Bad array specification in ALLOCATE statement at %L",
		     &e->where);
	  goto failure;
	}

check_symbols:
      for (a = code->ext.alloc.list; a; a = a->next)
	{
	  sym = a->expr->symtree->n.sym;

	  /* TODO - check derived type components.  */
	  if (gfc_bt_struct (sym->ts.type) || sym->ts.type == BT_CLASS)
	    continue;

	  if ((ar->start[i] != NULL
	       && gfc_find_sym_in_expr (sym, ar->start[i]))
	      || (ar->end[i] != NULL
		  && gfc_find_sym_in_expr (sym, ar->end[i])))
	    {
	      gfc_error ("%qs must not appear in the array specification at "
			 "%L in the same ALLOCATE statement where it is "
			 "itself allocated", sym->name, &ar->where);
	      goto failure;
	    }
	}
    }

  for (i = ar->dimen; i < ar->codimen + ar->dimen; i++)
    {
      if (ar->dimen_type[i] == DIMEN_ELEMENT
	  || ar->dimen_type[i] == DIMEN_RANGE)
	{
	  if (i == (ar->dimen + ar->codimen - 1))
	    {
	      gfc_error ("Expected '*' in coindex specification in ALLOCATE "
			 "statement at %L", &e->where);
	      goto failure;
	    }
	  continue;
	}

      if (ar->dimen_type[i] == DIMEN_STAR && i == (ar->dimen + ar->codimen - 1)
	  && ar->stride[i] == NULL)
	break;

      gfc_error ("Bad coarray specification in ALLOCATE statement at %L",
		 &e->where);
      goto failure;
    }

success:
  return true;

failure:
  return false;
}


static void
resolve_allocate_deallocate (gfc_code *code, const char *fcn)
{
  gfc_expr *stat, *errmsg, *pe, *qe;
  gfc_alloc *a, *p, *q;

  stat = code->expr1;
  errmsg = code->expr2;

  /* Check the stat variable.  */
  if (stat)
    {
      gfc_check_vardef_context (stat, false, false, false,
				_("STAT variable"));

      if ((stat->ts.type != BT_INTEGER
	   && !(stat->ref && (stat->ref->type == REF_ARRAY
			      || stat->ref->type == REF_COMPONENT)))
	  || stat->rank > 0)
	gfc_error ("Stat-variable at %L must be a scalar INTEGER "
		   "variable", &stat->where);

      for (p = code->ext.alloc.list; p; p = p->next)
	if (p->expr->symtree->n.sym->name == stat->symtree->n.sym->name)
	  {
	    gfc_ref *ref1, *ref2;
	    bool found = true;

	    for (ref1 = p->expr->ref, ref2 = stat->ref; ref1 && ref2;
		 ref1 = ref1->next, ref2 = ref2->next)
	      {
		if (ref1->type != REF_COMPONENT || ref2->type != REF_COMPONENT)
		  continue;
		if (ref1->u.c.component->name != ref2->u.c.component->name)
		  {
		    found = false;
		    break;
		  }
	      }

	    if (found)
	      {
		gfc_error ("Stat-variable at %L shall not be %sd within "
			   "the same %s statement", &stat->where, fcn, fcn);
		break;
	      }
	  }
    }

  /* Check the errmsg variable.  */
  if (errmsg)
    {
      if (!stat)
	gfc_warning (0, "ERRMSG at %L is useless without a STAT tag",
		     &errmsg->where);

      gfc_check_vardef_context (errmsg, false, false, false,
				_("ERRMSG variable"));

      if ((errmsg->ts.type != BT_CHARACTER
	   && !(errmsg->ref
		&& (errmsg->ref->type == REF_ARRAY
		    || errmsg->ref->type == REF_COMPONENT)))
	  || errmsg->rank > 0 )
	gfc_error ("Errmsg-variable at %L must be a scalar CHARACTER "
		   "variable", &errmsg->where);

      for (p = code->ext.alloc.list; p; p = p->next)
	if (p->expr->symtree->n.sym->name == errmsg->symtree->n.sym->name)
	  {
	    gfc_ref *ref1, *ref2;
	    bool found = true;

	    for (ref1 = p->expr->ref, ref2 = errmsg->ref; ref1 && ref2;
		 ref1 = ref1->next, ref2 = ref2->next)
	      {
		if (ref1->type != REF_COMPONENT || ref2->type != REF_COMPONENT)
		  continue;
		if (ref1->u.c.component->name != ref2->u.c.component->name)
		  {
		    found = false;
		    break;
		  }
	      }

	    if (found)
	      {
		gfc_error ("Errmsg-variable at %L shall not be %sd within "
			   "the same %s statement", &errmsg->where, fcn, fcn);
		break;
	      }
	  }
    }

  /* Check that an allocate-object appears only once in the statement.  */

  for (p = code->ext.alloc.list; p; p = p->next)
    {
      pe = p->expr;
      for (q = p->next; q; q = q->next)
	{
	  qe = q->expr;
	  if (pe->symtree->n.sym->name == qe->symtree->n.sym->name)
	    {
	      /* This is a potential collision.  */
	      gfc_ref *pr = pe->ref;
	      gfc_ref *qr = qe->ref;

	      /* Follow the references  until
		 a) They start to differ, in which case there is no error;
		 you can deallocate a%b and a%c in a single statement
		 b) Both of them stop, which is an error
		 c) One of them stops, which is also an error.  */
	      while (1)
		{
		  if (pr == NULL && qr == NULL)
		    {
		      gfc_error ("Allocate-object at %L also appears at %L",
				 &pe->where, &qe->where);
		      break;
		    }
		  else if (pr != NULL && qr == NULL)
		    {
		      gfc_error ("Allocate-object at %L is subobject of"
				 " object at %L", &pe->where, &qe->where);
		      break;
		    }
		  else if (pr == NULL && qr != NULL)
		    {
		      gfc_error ("Allocate-object at %L is subobject of"
				 " object at %L", &qe->where, &pe->where);
		      break;
		    }
		  /* Here, pr != NULL && qr != NULL  */
		  gcc_assert(pr->type == qr->type);
		  if (pr->type == REF_ARRAY)
		    {
		      /* Handle cases like allocate(v(3)%x(3), v(2)%x(3)),
			 which are legal.  */
		      gcc_assert (qr->type == REF_ARRAY);

		      if (pr->next && qr->next)
			{
			  int i;
			  gfc_array_ref *par = &(pr->u.ar);
			  gfc_array_ref *qar = &(qr->u.ar);

			  for (i=0; i<par->dimen; i++)
			    {
			      if ((par->start[i] != NULL
				   || qar->start[i] != NULL)
				  && gfc_dep_compare_expr (par->start[i],
							   qar->start[i]) != 0)
				goto break_label;
			    }
			}
		    }
		  else
		    {
		      if (pr->u.c.component->name != qr->u.c.component->name)
			break;
		    }

		  pr = pr->next;
		  qr = qr->next;
		}
	    break_label:
	      ;
	    }
	}
    }

  if (strcmp (fcn, "ALLOCATE") == 0)
    {
      bool arr_alloc_wo_spec = false;

      /* Resolving the expr3 in the loop over all objects to allocate would
	 execute loop invariant code for each loop item.  Therefore do it just
	 once here.  */
      if (code->expr3 && code->expr3->mold
	  && code->expr3->ts.type == BT_DERIVED)
	{
	  /* Default initialization via MOLD (non-polymorphic).  */
	  gfc_expr *rhs = gfc_default_initializer (&code->expr3->ts);
	  if (rhs != NULL)
	    {
	      gfc_resolve_expr (rhs);
	      gfc_free_expr (code->expr3);
	      code->expr3 = rhs;
	    }
	}
      for (a = code->ext.alloc.list; a; a = a->next)
	resolve_allocate_expr (a->expr, code, &arr_alloc_wo_spec);

      if (arr_alloc_wo_spec && code->expr3)
	{
	  /* Mark the allocate to have to take the array specification
	     from the expr3.  */
	  code->ext.alloc.arr_spec_from_expr3 = 1;
	}
    }
  else
    {
      for (a = code->ext.alloc.list; a; a = a->next)
	resolve_deallocate_expr (a->expr);
    }
}


/************ SELECT CASE resolution subroutines ************/

/* Callback function for our mergesort variant.  Determines interval
   overlaps for CASEs. Return <0 if op1 < op2, 0 for overlap, >0 for
   op1 > op2.  Assumes we're not dealing with the default case.
   We have op1 = (:L), (K:L) or (K:) and op2 = (:N), (M:N) or (M:).
   There are nine situations to check.  */

static int
compare_cases (const gfc_case *op1, const gfc_case *op2)
{
  int retval;

  if (op1->low == NULL) /* op1 = (:L)  */
    {
      /* op2 = (:N), so overlap.  */
      retval = 0;
      /* op2 = (M:) or (M:N),  L < M  */
      if (op2->low != NULL
	  && gfc_compare_expr (op1->high, op2->low, INTRINSIC_LT) < 0)
	retval = -1;
    }
  else if (op1->high == NULL) /* op1 = (K:)  */
    {
      /* op2 = (M:), so overlap.  */
      retval = 0;
      /* op2 = (:N) or (M:N), K > N  */
      if (op2->high != NULL
	  && gfc_compare_expr (op1->low, op2->high, INTRINSIC_GT) > 0)
	retval = 1;
    }
  else /* op1 = (K:L)  */
    {
      if (op2->low == NULL)       /* op2 = (:N), K > N  */
	retval = (gfc_compare_expr (op1->low, op2->high, INTRINSIC_GT) > 0)
		 ? 1 : 0;
      else if (op2->high == NULL) /* op2 = (M:), L < M  */
	retval = (gfc_compare_expr (op1->high, op2->low, INTRINSIC_LT) < 0)
		 ? -1 : 0;
      else			/* op2 = (M:N)  */
	{
	  retval =  0;
	  /* L < M  */
	  if (gfc_compare_expr (op1->high, op2->low, INTRINSIC_LT) < 0)
	    retval =  -1;
	  /* K > N  */
	  else if (gfc_compare_expr (op1->low, op2->high, INTRINSIC_GT) > 0)
	    retval =  1;
	}
    }

  return retval;
}


/* Merge-sort a double linked case list, detecting overlap in the
   process.  LIST is the head of the double linked case list before it
   is sorted.  Returns the head of the sorted list if we don't see any
   overlap, or NULL otherwise.  */

static gfc_case *
check_case_overlap (gfc_case *list)
{
  gfc_case *p, *q, *e, *tail;
  int insize, nmerges, psize, qsize, cmp, overlap_seen;

  /* If the passed list was empty, return immediately.  */
  if (!list)
    return NULL;

  overlap_seen = 0;
  insize = 1;

  /* Loop unconditionally.  The only exit from this loop is a return
     statement, when we've finished sorting the case list.  */
  for (;;)
    {
      p = list;
      list = NULL;
      tail = NULL;

      /* Count the number of merges we do in this pass.  */
      nmerges = 0;

      /* Loop while there exists a merge to be done.  */
      while (p)
	{
	  int i;

	  /* Count this merge.  */
	  nmerges++;

	  /* Cut the list in two pieces by stepping INSIZE places
	     forward in the list, starting from P.  */
	  psize = 0;
	  q = p;
	  for (i = 0; i < insize; i++)
	    {
	      psize++;
	      q = q->right;
	      if (!q)
		break;
	    }
	  qsize = insize;

	  /* Now we have two lists.  Merge them!  */
	  while (psize > 0 || (qsize > 0 && q != NULL))
	    {
	      /* See from which the next case to merge comes from.  */
	      if (psize == 0)
		{
		  /* P is empty so the next case must come from Q.  */
		  e = q;
		  q = q->right;
		  qsize--;
		}
	      else if (qsize == 0 || q == NULL)
		{
		  /* Q is empty.  */
		  e = p;
		  p = p->right;
		  psize--;
		}
	      else
		{
		  cmp = compare_cases (p, q);
		  if (cmp < 0)
		    {
		      /* The whole case range for P is less than the
			 one for Q.  */
		      e = p;
		      p = p->right;
		      psize--;
		    }
		  else if (cmp > 0)
		    {
		      /* The whole case range for Q is greater than
			 the case range for P.  */
		      e = q;
		      q = q->right;
		      qsize--;
		    }
		  else
		    {
		      /* The cases overlap, or they are the same
			 element in the list.  Either way, we must
			 issue an error and get the next case from P.  */
		      /* FIXME: Sort P and Q by line number.  */
		      gfc_error ("CASE label at %L overlaps with CASE "
				 "label at %L", &p->where, &q->where);
		      overlap_seen = 1;
		      e = p;
		      p = p->right;
		      psize--;
		    }
		}

		/* Add the next element to the merged list.  */
	      if (tail)
		tail->right = e;
	      else
		list = e;
	      e->left = tail;
	      tail = e;
	    }

	  /* P has now stepped INSIZE places along, and so has Q.  So
	     they're the same.  */
	  p = q;
	}
      tail->right = NULL;

      /* If we have done only one merge or none at all, we've
	 finished sorting the cases.  */
      if (nmerges <= 1)
	{
	  if (!overlap_seen)
	    return list;
	  else
	    return NULL;
	}

      /* Otherwise repeat, merging lists twice the size.  */
      insize *= 2;
    }
}


/* Check to see if an expression is suitable for use in a CASE statement.
   Makes sure that all case expressions are scalar constants of the same
   type.  Return false if anything is wrong.  */

static bool
validate_case_label_expr (gfc_expr *e, gfc_expr *case_expr)
{
  if (e == NULL) return true;

  if (e->ts.type != case_expr->ts.type)
    {
      gfc_error ("Expression in CASE statement at %L must be of type %s",
		 &e->where, gfc_basic_typename (case_expr->ts.type));
      return false;
    }

  /* C805 (R808) For a given case-construct, each case-value shall be of
     the same type as case-expr.  For character type, length differences
     are allowed, but the kind type parameters shall be the same.  */

  if (case_expr->ts.type == BT_CHARACTER && e->ts.kind != case_expr->ts.kind)
    {
      gfc_error ("Expression in CASE statement at %L must be of kind %d",
		 &e->where, case_expr->ts.kind);
      return false;
    }

  /* Convert the case value kind to that of case expression kind,
     if needed */

  if (e->ts.kind != case_expr->ts.kind)
    gfc_convert_type_warn (e, &case_expr->ts, 2, 0);

  if (e->rank != 0)
    {
      gfc_error ("Expression in CASE statement at %L must be scalar",
		 &e->where);
      return false;
    }

  return true;
}


/* Given a completely parsed select statement, we:

     - Validate all expressions and code within the SELECT.
     - Make sure that the selection expression is not of the wrong type.
     - Make sure that no case ranges overlap.
     - Eliminate unreachable cases and unreachable code resulting from
       removing case labels.

   The standard does allow unreachable cases, e.g. CASE (5:3).  But
   they are a hassle for code generation, and to prevent that, we just
   cut them out here.  This is not necessary for overlapping cases
   because they are illegal and we never even try to generate code.

   We have the additional caveat that a SELECT construct could have
   been a computed GOTO in the source code. Fortunately we can fairly
   easily work around that here: The case_expr for a "real" SELECT CASE
   is in code->expr1, but for a computed GOTO it is in code->expr2. All
   we have to do is make sure that the case_expr is a scalar integer
   expression.  */

static void
resolve_select (gfc_code *code, bool select_type)
{
  gfc_code *body;
  gfc_expr *case_expr;
  gfc_case *cp, *default_case, *tail, *head;
  int seen_unreachable;
  int seen_logical;
  int ncases;
  bt type;
  bool t;

  if (code->expr1 == NULL)
    {
      /* This was actually a computed GOTO statement.  */
      case_expr = code->expr2;
      if (case_expr->ts.type != BT_INTEGER|| case_expr->rank != 0)
	gfc_error ("Selection expression in computed GOTO statement "
		   "at %L must be a scalar integer expression",
		   &case_expr->where);

      /* Further checking is not necessary because this SELECT was built
	 by the compiler, so it should always be OK.  Just move the
	 case_expr from expr2 to expr so that we can handle computed
	 GOTOs as normal SELECTs from here on.  */
      code->expr1 = code->expr2;
      code->expr2 = NULL;
      return;
    }

  case_expr = code->expr1;
  type = case_expr->ts.type;

  /* F08:C830.  */
  if (type != BT_LOGICAL && type != BT_INTEGER && type != BT_CHARACTER)
    {
      gfc_error ("Argument of SELECT statement at %L cannot be %s",
		 &case_expr->where, gfc_typename (&case_expr->ts));

      /* Punt. Going on here just produce more garbage error messages.  */
      return;
    }

  /* F08:R842.  */
  if (!select_type && case_expr->rank != 0)
    {
      gfc_error ("Argument of SELECT statement at %L must be a scalar "
		 "expression", &case_expr->where);

      /* Punt.  */
      return;
    }

  /* Raise a warning if an INTEGER case value exceeds the range of
     the case-expr. Later, all expressions will be promoted to the
     largest kind of all case-labels.  */

  if (type == BT_INTEGER)
    for (body = code->block; body; body = body->block)
      for (cp = body->ext.block.case_list; cp; cp = cp->next)
	{
	  if (cp->low
	      && gfc_check_integer_range (cp->low->value.integer,
					  case_expr->ts.kind) != ARITH_OK)
	    gfc_warning (0, "Expression in CASE statement at %L is "
			 "not in the range of %s", &cp->low->where,
			 gfc_typename (&case_expr->ts));

	  if (cp->high
	      && cp->low != cp->high
	      && gfc_check_integer_range (cp->high->value.integer,
					  case_expr->ts.kind) != ARITH_OK)
	    gfc_warning (0, "Expression in CASE statement at %L is "
			 "not in the range of %s", &cp->high->where,
			 gfc_typename (&case_expr->ts));
	}

  /* PR 19168 has a long discussion concerning a mismatch of the kinds
     of the SELECT CASE expression and its CASE values.  Walk the lists
     of case values, and if we find a mismatch, promote case_expr to
     the appropriate kind.  */

  if (type == BT_LOGICAL || type == BT_INTEGER)
    {
      for (body = code->block; body; body = body->block)
	{
	  /* Walk the case label list.  */
	  for (cp = body->ext.block.case_list; cp; cp = cp->next)
	    {
	      /* Intercept the DEFAULT case.  It does not have a kind.  */
	      if (cp->low == NULL && cp->high == NULL)
		continue;

	      /* Unreachable case ranges are discarded, so ignore.  */
	      if (cp->low != NULL && cp->high != NULL
		  && cp->low != cp->high
		  && gfc_compare_expr (cp->low, cp->high, INTRINSIC_GT) > 0)
		continue;

	      if (cp->low != NULL
		  && case_expr->ts.kind != gfc_kind_max(case_expr, cp->low))
		gfc_convert_type_warn (case_expr, &cp->low->ts, 2, 0);

	      if (cp->high != NULL
		  && case_expr->ts.kind != gfc_kind_max(case_expr, cp->high))
		gfc_convert_type_warn (case_expr, &cp->high->ts, 2, 0);
	    }
	 }
    }

  /* Assume there is no DEFAULT case.  */
  default_case = NULL;
  head = tail = NULL;
  ncases = 0;
  seen_logical = 0;

  for (body = code->block; body; body = body->block)
    {
      /* Assume the CASE list is OK, and all CASE labels can be matched.  */
      t = true;
      seen_unreachable = 0;

      /* Walk the case label list, making sure that all case labels
	 are legal.  */
      for (cp = body->ext.block.case_list; cp; cp = cp->next)
	{
	  /* Count the number of cases in the whole construct.  */
	  ncases++;

	  /* Intercept the DEFAULT case.  */
	  if (cp->low == NULL && cp->high == NULL)
	    {
	      if (default_case != NULL)
		{
		  gfc_error ("The DEFAULT CASE at %L cannot be followed "
			     "by a second DEFAULT CASE at %L",
			     &default_case->where, &cp->where);
		  t = false;
		  break;
		}
	      else
		{
		  default_case = cp;
		  continue;
		}
	    }

	  /* Deal with single value cases and case ranges.  Errors are
	     issued from the validation function.  */
	  if (!validate_case_label_expr (cp->low, case_expr)
	      || !validate_case_label_expr (cp->high, case_expr))
	    {
	      t = false;
	      break;
	    }

	  if (type == BT_LOGICAL
	      && ((cp->low == NULL || cp->high == NULL)
		  || cp->low != cp->high))
	    {
	      gfc_error ("Logical range in CASE statement at %L is not "
			 "allowed", &cp->low->where);
	      t = false;
	      break;
	    }

	  if (type == BT_LOGICAL && cp->low->expr_type == EXPR_CONSTANT)
	    {
	      int value;
	      value = cp->low->value.logical == 0 ? 2 : 1;
	      if (value & seen_logical)
		{
		  gfc_error ("Constant logical value in CASE statement "
			     "is repeated at %L",
			     &cp->low->where);
		  t = false;
		  break;
		}
	      seen_logical |= value;
	    }

	  if (cp->low != NULL && cp->high != NULL
	      && cp->low != cp->high
	      && gfc_compare_expr (cp->low, cp->high, INTRINSIC_GT) > 0)
	    {
	      if (warn_surprising)
		gfc_warning (OPT_Wsurprising,
			     "Range specification at %L can never be matched",
			     &cp->where);

	      cp->unreachable = 1;
	      seen_unreachable = 1;
	    }
	  else
	    {
	      /* If the case range can be matched, it can also overlap with
		 other cases.  To make sure it does not, we put it in a
		 double linked list here.  We sort that with a merge sort
		 later on to detect any overlapping cases.  */
	      if (!head)
		{
		  head = tail = cp;
		  head->right = head->left = NULL;
		}
	      else
		{
		  tail->right = cp;
		  tail->right->left = tail;
		  tail = tail->right;
		  tail->right = NULL;
		}
	    }
	}

      /* It there was a failure in the previous case label, give up
	 for this case label list.  Continue with the next block.  */
      if (!t)
	continue;

      /* See if any case labels that are unreachable have been seen.
	 If so, we eliminate them.  This is a bit of a kludge because
	 the case lists for a single case statement (label) is a
	 single forward linked lists.  */
      if (seen_unreachable)
      {
	/* Advance until the first case in the list is reachable.  */
	while (body->ext.block.case_list != NULL
	       && body->ext.block.case_list->unreachable)
	  {
	    gfc_case *n = body->ext.block.case_list;
	    body->ext.block.case_list = body->ext.block.case_list->next;
	    n->next = NULL;
	    gfc_free_case_list (n);
	  }

	/* Strip all other unreachable cases.  */
	if (body->ext.block.case_list)
	  {
	    for (cp = body->ext.block.case_list; cp && cp->next; cp = cp->next)
	      {
		if (cp->next->unreachable)
		  {
		    gfc_case *n = cp->next;
		    cp->next = cp->next->next;
		    n->next = NULL;
		    gfc_free_case_list (n);
		  }
	      }
	  }
      }
    }

  /* See if there were overlapping cases.  If the check returns NULL,
     there was overlap.  In that case we don't do anything.  If head
     is non-NULL, we prepend the DEFAULT case.  The sorted list can
     then used during code generation for SELECT CASE constructs with
     a case expression of a CHARACTER type.  */
  if (head)
    {
      head = check_case_overlap (head);

      /* Prepend the default_case if it is there.  */
      if (head != NULL && default_case)
	{
	  default_case->left = NULL;
	  default_case->right = head;
	  head->left = default_case;
	}
    }

  /* Eliminate dead blocks that may be the result if we've seen
     unreachable case labels for a block.  */
  for (body = code; body && body->block; body = body->block)
    {
      if (body->block->ext.block.case_list == NULL)
	{
	  /* Cut the unreachable block from the code chain.  */
	  gfc_code *c = body->block;
	  body->block = c->block;

	  /* Kill the dead block, but not the blocks below it.  */
	  c->block = NULL;
	  gfc_free_statements (c);
	}
    }

  /* More than two cases is legal but insane for logical selects.
     Issue a warning for it.  */
  if (warn_surprising && type == BT_LOGICAL && ncases > 2)
    gfc_warning (OPT_Wsurprising,
		 "Logical SELECT CASE block at %L has more that two cases",
		 &code->loc);
}


/* Check if a derived type is extensible.  */

bool
gfc_type_is_extensible (gfc_symbol *sym)
{
  return !(sym->attr.is_bind_c || sym->attr.sequence
	   || (sym->attr.is_class
	       && sym->components->ts.u.derived->attr.unlimited_polymorphic));
}


static void
resolve_types (gfc_namespace *ns);

/* Resolve an associate-name:  Resolve target and ensure the type-spec is
   correct as well as possibly the array-spec.  */

static void
resolve_assoc_var (gfc_symbol* sym, bool resolve_target)
{
  gfc_expr* target;

  gcc_assert (sym->assoc);
  gcc_assert (sym->attr.flavor == FL_VARIABLE);

  /* If this is for SELECT TYPE, the target may not yet be set.  In that
     case, return.  Resolution will be called later manually again when
     this is done.  */
  target = sym->assoc->target;
  if (!target)
    return;
  gcc_assert (!sym->assoc->dangling);

  if (resolve_target && !gfc_resolve_expr (target))
    return;

  /* For variable targets, we get some attributes from the target.  */
  if (target->expr_type == EXPR_VARIABLE)
    {
      gfc_symbol* tsym;

      gcc_assert (target->symtree);
      tsym = target->symtree->n.sym;

      sym->attr.asynchronous = tsym->attr.asynchronous;
      sym->attr.volatile_ = tsym->attr.volatile_;

      sym->attr.target = tsym->attr.target
			 || gfc_expr_attr (target).pointer;
      if (is_subref_array (target))
	sym->attr.subref_array_pointer = 1;
    }

  if (target->expr_type == EXPR_NULL)
    {
      gfc_error ("Selector at %L cannot be NULL()", &target->where);
      return;
    }
  else if (target->ts.type == BT_UNKNOWN)
    {
      gfc_error ("Selector at %L has no type", &target->where);
      return;
    }

  /* Get type if this was not already set.  Note that it can be
     some other type than the target in case this is a SELECT TYPE
     selector!  So we must not update when the type is already there.  */
  if (sym->ts.type == BT_UNKNOWN)
    sym->ts = target->ts;

  gcc_assert (sym->ts.type != BT_UNKNOWN);

  /* See if this is a valid association-to-variable.  */
  sym->assoc->variable = (target->expr_type == EXPR_VARIABLE
			  && !gfc_has_vector_subscript (target));

  /* Finally resolve if this is an array or not.  */
  if (sym->attr.dimension && target->rank == 0)
    {
      /* primary.c makes the assumption that a reference to an associate
	 name followed by a left parenthesis is an array reference.  */
      if (sym->ts.type != BT_CHARACTER)
	gfc_error ("Associate-name %qs at %L is used as array",
		   sym->name, &sym->declared_at);
      sym->attr.dimension = 0;
      return;
    }


  /* We cannot deal with class selectors that need temporaries.  */
  if (target->ts.type == BT_CLASS
	&& gfc_ref_needs_temporary_p (target->ref))
    {
      gfc_error ("CLASS selector at %L needs a temporary which is not "
		 "yet implemented", &target->where);
      return;
    }

  if (target->ts.type == BT_CLASS)
    gfc_fix_class_refs (target);

  if (target->rank != 0)
    {
      gfc_array_spec *as;
      /* The rank may be incorrectly guessed at parsing, therefore make sure
	 it is corrected now.  */
      if (sym->ts.type != BT_CLASS && (!sym->as || sym->assoc->rankguessed))
	{
	  if (!sym->as)
	    sym->as = gfc_get_array_spec ();
	  as = sym->as;
	  as->rank = target->rank;
	  as->type = AS_DEFERRED;
	  as->corank = gfc_get_corank (target);
	  sym->attr.dimension = 1;
	  if (as->corank != 0)
	    sym->attr.codimension = 1;
	}
    }
  else
    {
      /* target's rank is 0, but the type of the sym is still array valued,
	 which has to be corrected.  */
      if (sym->ts.type == BT_CLASS && CLASS_DATA (sym)->as)
	{
	  gfc_array_spec *as;
	  symbol_attribute attr;
	  /* The associated variable's type is still the array type
	     correct this now.  */
	  gfc_typespec *ts = &target->ts;
	  gfc_ref *ref;
	  gfc_component *c;
	  for (ref = target->ref; ref != NULL; ref = ref->next)
	    {
	      switch (ref->type)
		{
		case REF_COMPONENT:
		  ts = &ref->u.c.component->ts;
		  break;
		case REF_ARRAY:
		  if (ts->type == BT_CLASS)
		    ts = &ts->u.derived->components->ts;
		  break;
		default:
		  break;
		}
	    }
	  /* Create a scalar instance of the current class type.  Because the
	     rank of a class array goes into its name, the type has to be
	     rebuild.  The alternative of (re-)setting just the attributes
	     and as in the current type, destroys the type also in other
	     places.  */
	  as = NULL;
	  sym->ts = *ts;
	  sym->ts.type = BT_CLASS;
	  attr = CLASS_DATA (sym)->attr;
	  attr.class_ok = 0;
	  attr.associate_var = 1;
	  attr.dimension = attr.codimension = 0;
	  attr.class_pointer = 1;
	  if (!gfc_build_class_symbol (&sym->ts, &attr, &as))
	    gcc_unreachable ();
	  /* Make sure the _vptr is set.  */
	  c = gfc_find_component (sym->ts.u.derived, "_vptr", true, true, NULL);
	  if (c->ts.u.derived == NULL)
	    c->ts.u.derived = gfc_find_derived_vtab (sym->ts.u.derived);
	  CLASS_DATA (sym)->attr.pointer = 1;
	  CLASS_DATA (sym)->attr.class_pointer = 1;
	  gfc_set_sym_referenced (sym->ts.u.derived);
	  gfc_commit_symbol (sym->ts.u.derived);
	  /* _vptr now has the _vtab in it, change it to the _vtype.  */
	  if (c->ts.u.derived->attr.vtab)
	    c->ts.u.derived = c->ts.u.derived->ts.u.derived;
	  c->ts.u.derived->ns->types_resolved = 0;
	  resolve_types (c->ts.u.derived->ns);
	}
    }

  /* Mark this as an associate variable.  */
  sym->attr.associate_var = 1;

  /* Fix up the type-spec for CHARACTER types.  */
  if (sym->ts.type == BT_CHARACTER && !sym->attr.select_type_temporary)
    {
      if (!sym->ts.u.cl)
	sym->ts.u.cl = target->ts.u.cl;

      if (!sym->ts.u.cl->length)
	sym->ts.u.cl->length
	  = gfc_get_int_expr (gfc_default_integer_kind,
			      NULL, target->value.character.length);
    }

  /* If the target is a good class object, so is the associate variable.  */
  if (sym->ts.type == BT_CLASS && gfc_expr_attr (target).class_ok)
    sym->attr.class_ok = 1;
}


/* Ensure that SELECT TYPE expressions have the correct rank and a full
   array reference, where necessary.  The symbols are artificial and so
   the dimension attribute and arrayspec can also be set.  In addition,
   sometimes the expr1 arrives as BT_DERIVED, when the symbol is BT_CLASS.
   This is corrected here as well.*/

static void
fixup_array_ref (gfc_expr **expr1, gfc_expr *expr2,
		 int rank, gfc_ref *ref)
{
  gfc_ref *nref = (*expr1)->ref;
  gfc_symbol *sym1 = (*expr1)->symtree->n.sym;
  gfc_symbol *sym2 = expr2 ? expr2->symtree->n.sym : NULL;
  (*expr1)->rank = rank;
  if (sym1->ts.type == BT_CLASS)
    {
      if ((*expr1)->ts.type != BT_CLASS)
	(*expr1)->ts = sym1->ts;

      CLASS_DATA (sym1)->attr.dimension = 1;
      if (CLASS_DATA (sym1)->as == NULL && sym2)
	CLASS_DATA (sym1)->as
		= gfc_copy_array_spec (CLASS_DATA (sym2)->as);
    }
  else
    {
      sym1->attr.dimension = 1;
      if (sym1->as == NULL && sym2)
	sym1->as = gfc_copy_array_spec (sym2->as);
    }

  for (; nref; nref = nref->next)
    if (nref->next == NULL)
      break;

  if (ref && nref && nref->type != REF_ARRAY)
    nref->next = gfc_copy_ref (ref);
  else if (ref && !nref)
    (*expr1)->ref = gfc_copy_ref (ref);
}


static gfc_expr *
build_loc_call (gfc_expr *sym_expr)
{
  gfc_expr *loc_call;
  loc_call = gfc_get_expr ();
  loc_call->expr_type = EXPR_FUNCTION;
  gfc_get_sym_tree ("loc", gfc_current_ns, &loc_call->symtree, false);
  loc_call->symtree->n.sym->attr.flavor = FL_PROCEDURE;
  loc_call->symtree->n.sym->attr.intrinsic = 1;
  loc_call->symtree->n.sym->result = loc_call->symtree->n.sym;
  gfc_commit_symbol (loc_call->symtree->n.sym);
  loc_call->ts.type = BT_INTEGER;
  loc_call->ts.kind = gfc_index_integer_kind;
  loc_call->value.function.isym = gfc_intrinsic_function_by_id (GFC_ISYM_LOC);
  loc_call->value.function.actual = gfc_get_actual_arglist ();
  loc_call->value.function.actual->expr = sym_expr;
  loc_call->where = sym_expr->where;
  return loc_call;
}

/* Resolve a SELECT TYPE statement.  */

static void
resolve_select_type (gfc_code *code, gfc_namespace *old_ns)
{
  gfc_symbol *selector_type;
  gfc_code *body, *new_st, *if_st, *tail;
  gfc_code *class_is = NULL, *default_case = NULL;
  gfc_case *c;
  gfc_symtree *st;
  char name[GFC_MAX_SYMBOL_LEN];
  gfc_namespace *ns;
  int error = 0;
  int charlen = 0;
  int rank = 0;
  gfc_ref* ref = NULL;
  gfc_expr *selector_expr = NULL;

  ns = code->ext.block.ns;
  gfc_resolve (ns);

  /* Check for F03:C813.  */
  if (code->expr1->ts.type != BT_CLASS
      && !(code->expr2 && code->expr2->ts.type == BT_CLASS))
    {
      gfc_error ("Selector shall be polymorphic in SELECT TYPE statement "
		 "at %L", &code->loc);
      return;
    }

  if (!code->expr1->symtree->n.sym->attr.class_ok)
    return;

  if (code->expr2)
    {
      if (code->expr1->symtree->n.sym->attr.untyped)
	code->expr1->symtree->n.sym->ts = code->expr2->ts;
      selector_type = CLASS_DATA (code->expr2)->ts.u.derived;

      /* F2008: C803 The selector expression must not be coindexed.  */
      if (gfc_is_coindexed (code->expr2))
	{
	  gfc_error ("Selector at %L must not be coindexed",
		     &code->expr2->where);
	  return;
	}

    }
  else
    {
      selector_type = CLASS_DATA (code->expr1)->ts.u.derived;

      if (gfc_is_coindexed (code->expr1))
	{
	  gfc_error ("Selector at %L must not be coindexed",
		     &code->expr1->where);
	  return;
	}
    }

  /* Loop over TYPE IS / CLASS IS cases.  */
  for (body = code->block; body; body = body->block)
    {
      c = body->ext.block.case_list;

      if (!error)
	{
	  /* Check for repeated cases.  */
	  for (tail = code->block; tail; tail = tail->block)
	    {
	      gfc_case *d = tail->ext.block.case_list;
	      if (tail == body)
		break;

	      if (c->ts.type == d->ts.type
		  && ((c->ts.type == BT_DERIVED
		       && c->ts.u.derived && d->ts.u.derived
		       && !strcmp (c->ts.u.derived->name,
				   d->ts.u.derived->name))
		      || c->ts.type == BT_UNKNOWN
		      || (!(c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
			  && c->ts.kind == d->ts.kind)))
		{
		  gfc_error ("TYPE IS at %L overlaps with TYPE IS at %L",
			     &c->where, &d->where);
		  return;
		}
	    }
	}

      /* Check F03:C815.  */
      if ((c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
	  && !selector_type->attr.unlimited_polymorphic
	  && !gfc_type_is_extensible (c->ts.u.derived))
	{
	  gfc_error ("Derived type %qs at %L must be extensible",
		     c->ts.u.derived->name, &c->where);
	  error++;
	  continue;
	}

      /* Check F03:C816.  */
      if (c->ts.type != BT_UNKNOWN && !selector_type->attr.unlimited_polymorphic
	  && ((c->ts.type != BT_DERIVED && c->ts.type != BT_CLASS)
	      || !gfc_type_is_extension_of (selector_type, c->ts.u.derived)))
	{
	  if (c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
	    gfc_error ("Derived type %qs at %L must be an extension of %qs",
		       c->ts.u.derived->name, &c->where, selector_type->name);
	  else
	    gfc_error ("Unexpected intrinsic type %qs at %L",
		       gfc_basic_typename (c->ts.type), &c->where);
	  error++;
	  continue;
	}

      /* Check F03:C814.  */
      if (c->ts.type == BT_CHARACTER
	  && (c->ts.u.cl->length != NULL || c->ts.deferred))
	{
	  gfc_error ("The type-spec at %L shall specify that each length "
		     "type parameter is assumed", &c->where);
	  error++;
	  continue;
	}

      /* Intercept the DEFAULT case.  */
      if (c->ts.type == BT_UNKNOWN)
	{
	  /* Check F03:C818.  */
	  if (default_case)
	    {
	      gfc_error ("The DEFAULT CASE at %L cannot be followed "
			 "by a second DEFAULT CASE at %L",
			 &default_case->ext.block.case_list->where, &c->where);
	      error++;
	      continue;
	    }

	  default_case = body;
	}
    }

  if (error > 0)
    return;

  /* Transform SELECT TYPE statement to BLOCK and associate selector to
     target if present.  If there are any EXIT statements referring to the
     SELECT TYPE construct, this is no problem because the gfc_code
     reference stays the same and EXIT is equally possible from the BLOCK
     it is changed to.  */
  code->op = EXEC_BLOCK;
  if (code->expr2)
    {
      gfc_association_list* assoc;

      assoc = gfc_get_association_list ();
      assoc->st = code->expr1->symtree;
      assoc->target = gfc_copy_expr (code->expr2);
      assoc->target->where = code->expr2->where;
      /* assoc->variable will be set by resolve_assoc_var.  */

      code->ext.block.assoc = assoc;
      code->expr1->symtree->n.sym->assoc = assoc;

      resolve_assoc_var (code->expr1->symtree->n.sym, false);
    }
  else
    code->ext.block.assoc = NULL;

  /* Ensure that the selector rank and arrayspec are available to
     correct expressions in which they might be missing.  */
  if (code->expr2 && code->expr2->rank)
    {
      rank = code->expr2->rank;
      for (ref = code->expr2->ref; ref; ref = ref->next)
	if (ref->next == NULL)
	  break;
      if (ref && ref->type == REF_ARRAY)
	ref = gfc_copy_ref (ref);

      /* Fixup expr1 if necessary.  */
      if (rank)
	fixup_array_ref (&code->expr1, code->expr2, rank, ref);
    }
  else if (code->expr1->rank)
    {
      rank = code->expr1->rank;
      for (ref = code->expr1->ref; ref; ref = ref->next)
	if (ref->next == NULL)
	  break;
      if (ref && ref->type == REF_ARRAY)
	ref = gfc_copy_ref (ref);
    }

  /* Add EXEC_SELECT to switch on type.  */
  new_st = gfc_get_code (code->op);
  new_st->expr1 = code->expr1;
  new_st->expr2 = code->expr2;
  new_st->block = code->block;
  code->expr1 = code->expr2 =  NULL;
  code->block = NULL;
  if (!ns->code)
    ns->code = new_st;
  else
    ns->code->next = new_st;
  code = new_st;
  code->op = EXEC_SELECT_TYPE;

  /* Use the intrinsic LOC function to generate an integer expression
     for the vtable of the selector.  Note that the rank of the selector
     expression has to be set to zero.  */
  gfc_add_vptr_component (code->expr1);
  code->expr1->rank = 0;
  code->expr1 = build_loc_call (code->expr1);
  selector_expr = code->expr1->value.function.actual->expr;

  /* Loop over TYPE IS / CLASS IS cases.  */
  for (body = code->block; body; body = body->block)
    {
      gfc_symbol *vtab;
      gfc_expr *e;
      c = body->ext.block.case_list;

      /* Generate an index integer expression for address of the
	 TYPE/CLASS vtable and store it in c->low.  The hash expression
	 is stored in c->high and is used to resolve intrinsic cases.  */
      if (c->ts.type != BT_UNKNOWN)
	{
	  if (c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
	    {
	      vtab = gfc_find_derived_vtab (c->ts.u.derived);
	      gcc_assert (vtab);
	      c->high = gfc_get_int_expr (gfc_default_integer_kind, NULL,
					  c->ts.u.derived->hash_value);
	    }
	  else
	    {
	      vtab = gfc_find_vtab (&c->ts);
	      gcc_assert (vtab && CLASS_DATA (vtab)->initializer);
	      e = CLASS_DATA (vtab)->initializer;
	      c->high = gfc_copy_expr (e);
	    }

	  e = gfc_lval_expr_from_sym (vtab);
	  c->low = build_loc_call (e);
	}
      else
	continue;

      /* Associate temporary to selector.  This should only be done
	 when this case is actually true, so build a new ASSOCIATE
	 that does precisely this here (instead of using the
	 'global' one).  */

      if (c->ts.type == BT_CLASS)
	sprintf (name, "__tmp_class_%s", c->ts.u.derived->name);
      else if (c->ts.type == BT_DERIVED)
	sprintf (name, "__tmp_type_%s", c->ts.u.derived->name);
      else if (c->ts.type == BT_CHARACTER)
	{
	  if (c->ts.u.cl && c->ts.u.cl->length
	      && c->ts.u.cl->length->expr_type == EXPR_CONSTANT)
	    charlen = mpz_get_si (c->ts.u.cl->length->value.integer);
	  sprintf (name, "__tmp_%s_%d_%d", gfc_basic_typename (c->ts.type),
	           charlen, c->ts.kind);
	}
      else
	sprintf (name, "__tmp_%s_%d", gfc_basic_typename (c->ts.type),
	         c->ts.kind);

      st = gfc_find_symtree (ns->sym_root, name);
      gcc_assert (st->n.sym->assoc);
      st->n.sym->assoc->target = gfc_get_variable_expr (selector_expr->symtree);
      st->n.sym->assoc->target->where = selector_expr->where;
      if (c->ts.type != BT_CLASS && c->ts.type != BT_UNKNOWN)
	{
	  gfc_add_data_component (st->n.sym->assoc->target);
	  /* Fixup the target expression if necessary.  */
	  if (rank)
	    fixup_array_ref (&st->n.sym->assoc->target, NULL, rank, ref);
	}

      new_st = gfc_get_code (EXEC_BLOCK);
      new_st->ext.block.ns = gfc_build_block_ns (ns);
      new_st->ext.block.ns->code = body->next;
      body->next = new_st;

      /* Chain in the new list only if it is marked as dangling.  Otherwise
	 there is a CASE label overlap and this is already used.  Just ignore,
	 the error is diagnosed elsewhere.  */
      if (st->n.sym->assoc->dangling)
	{
	  new_st->ext.block.assoc = st->n.sym->assoc;
	  st->n.sym->assoc->dangling = 0;
	}

      resolve_assoc_var (st->n.sym, false);
    }

  /* Take out CLASS IS cases for separate treatment.  */
  body = code;
  while (body && body->block)
    {
      if (body->block->ext.block.case_list->ts.type == BT_CLASS)
	{
	  /* Add to class_is list.  */
	  if (class_is == NULL)
	    {
	      class_is = body->block;
	      tail = class_is;
	    }
	  else
	    {
	      for (tail = class_is; tail->block; tail = tail->block) ;
	      tail->block = body->block;
	      tail = tail->block;
	    }
	  /* Remove from EXEC_SELECT list.  */
	  body->block = body->block->block;
	  tail->block = NULL;
	}
      else
	body = body->block;
    }

  if (class_is)
    {
      gfc_symbol *vtab;

      if (!default_case)
	{
	  /* Add a default case to hold the CLASS IS cases.  */
	  for (tail = code; tail->block; tail = tail->block) ;
	  tail->block = gfc_get_code (EXEC_SELECT_TYPE);
	  tail = tail->block;
	  tail->ext.block.case_list = gfc_get_case ();
	  tail->ext.block.case_list->ts.type = BT_UNKNOWN;
	  tail->next = NULL;
	  default_case = tail;
	}

      /* More than one CLASS IS block?  */
      if (class_is->block)
	{
	  gfc_code **c1,*c2;
	  bool swapped;
	  /* Sort CLASS IS blocks by extension level.  */
	  do
	    {
	      swapped = false;
	      for (c1 = &class_is; (*c1) && (*c1)->block; c1 = &((*c1)->block))
		{
		  c2 = (*c1)->block;
		  /* F03:C817 (check for doubles).  */
		  if ((*c1)->ext.block.case_list->ts.u.derived->hash_value
		      == c2->ext.block.case_list->ts.u.derived->hash_value)
		    {
		      gfc_error ("Double CLASS IS block in SELECT TYPE "
				 "statement at %L",
				 &c2->ext.block.case_list->where);
		      return;
		    }
		  if ((*c1)->ext.block.case_list->ts.u.derived->attr.extension
		      < c2->ext.block.case_list->ts.u.derived->attr.extension)
		    {
		      /* Swap.  */
		      (*c1)->block = c2->block;
		      c2->block = *c1;
		      *c1 = c2;
		      swapped = true;
		    }
		}
	    }
	  while (swapped);
	}

      /* Generate IF chain.  */
      if_st = gfc_get_code (EXEC_IF);
      new_st = if_st;
      for (body = class_is; body; body = body->block)
	{
	  new_st->block = gfc_get_code (EXEC_IF);
	  new_st = new_st->block;
	  /* Set up IF condition: Call _gfortran_is_extension_of.  */
	  new_st->expr1 = gfc_get_expr ();
	  new_st->expr1->expr_type = EXPR_FUNCTION;
	  new_st->expr1->ts.type = BT_LOGICAL;
	  new_st->expr1->ts.kind = 4;
	  new_st->expr1->value.function.name = gfc_get_string (PREFIX ("is_extension_of"));
	  new_st->expr1->value.function.isym = XCNEW (gfc_intrinsic_sym);
	  new_st->expr1->value.function.isym->id = GFC_ISYM_EXTENDS_TYPE_OF;
	  /* Set up arguments.  */
	  new_st->expr1->value.function.actual = gfc_get_actual_arglist ();
	  new_st->expr1->value.function.actual->expr = gfc_get_variable_expr (selector_expr->symtree);
	  new_st->expr1->value.function.actual->expr->where = code->loc;
	  new_st->expr1->where = code->loc;
	  gfc_add_vptr_component (new_st->expr1->value.function.actual->expr);
	  vtab = gfc_find_derived_vtab (body->ext.block.case_list->ts.u.derived);
	  st = gfc_find_symtree (vtab->ns->sym_root, vtab->name);
	  new_st->expr1->value.function.actual->next = gfc_get_actual_arglist ();
	  new_st->expr1->value.function.actual->next->expr = gfc_get_variable_expr (st);
	  new_st->expr1->value.function.actual->next->expr->where = code->loc;
	  new_st->next = body->next;
	}
	if (default_case->next)
	  {
	    new_st->block = gfc_get_code (EXEC_IF);
	    new_st = new_st->block;
	    new_st->next = default_case->next;
	  }

	/* Replace CLASS DEFAULT code by the IF chain.  */
	default_case->next = if_st;
    }

  /* Resolve the internal code.  This can not be done earlier because
     it requires that the sym->assoc of selectors is set already.  */
  gfc_current_ns = ns;
  gfc_resolve_blocks (code->block, gfc_current_ns);
  gfc_current_ns = old_ns;

  if (ref)
    free (ref);
}


/* Resolve a transfer statement. This is making sure that:
   -- a derived type being transferred has only non-pointer components
   -- a derived type being transferred doesn't have private components, unless
      it's being transferred from the module where the type was defined
   -- we're not trying to transfer a whole assumed size array.  */

static void
resolve_transfer (gfc_code *code)
{
  gfc_typespec *ts;
  gfc_symbol *sym, *derived;
  gfc_ref *ref;
  gfc_expr *exp;
  bool write = false;
  bool formatted = false;
  gfc_dt *dt = code->ext.dt;
  gfc_symbol *dtio_sub = NULL;

  exp = code->expr1;

  while (exp != NULL && exp->expr_type == EXPR_OP
	 && exp->value.op.op == INTRINSIC_PARENTHESES)
    exp = exp->value.op.op1;

  if (exp && exp->expr_type == EXPR_NULL
      && code->ext.dt)
    {
      gfc_error ("Invalid context for NULL () intrinsic at %L",
		 &exp->where);
      return;
    }

  if (exp == NULL || (exp->expr_type != EXPR_VARIABLE
		      && exp->expr_type != EXPR_FUNCTION
		      && exp->expr_type != EXPR_STRUCTURE))
    return;

  /* If we are reading, the variable will be changed.  Note that
     code->ext.dt may be NULL if the TRANSFER is related to
     an INQUIRE statement -- but in this case, we are not reading, either.  */
  if (dt && dt->dt_io_kind->value.iokind == M_READ
      && !gfc_check_vardef_context (exp, false, false, false,
				    _("item in READ")))
    return;

  ts = exp->expr_type == EXPR_STRUCTURE ? &exp->ts : &exp->symtree->n.sym->ts;

  /* Go to actual component transferred.  */
  for (ref = exp->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT)
      ts = &ref->u.c.component->ts;

  if (dt && dt->dt_io_kind->value.iokind != M_INQUIRE
      && (ts->type == BT_DERIVED || ts->type == BT_CLASS))
    {
      if (ts->type == BT_DERIVED)
	derived = ts->u.derived;
      else
	derived = ts->u.derived->components->ts.u.derived;

      if (dt->format_expr)
	{
	  char *fmt;
	  fmt = gfc_widechar_to_char (dt->format_expr->value.character.string,
				      -1);
	  if (strtok (fmt, "DT") != NULL)
	    formatted = true;
	}
      else if (dt->format_label == &format_asterisk)
	{
	  /* List directed io must call the formatted DTIO procedure.  */
	  formatted = true;
	}

      write = dt->dt_io_kind->value.iokind == M_WRITE
	      || dt->dt_io_kind->value.iokind == M_PRINT;
      dtio_sub = gfc_find_specific_dtio_proc (derived, write, formatted);

      if (dtio_sub != NULL && exp->expr_type == EXPR_VARIABLE)
	{
	  dt->udtio = exp;
	  sym = exp->symtree->n.sym->ns->proc_name;
	  /* Check to see if this is a nested DTIO call, with the
	     dummy as the io-list object.  */
	  if (sym && sym == dtio_sub && sym->formal
	      && sym->formal->sym == exp->symtree->n.sym
	      && exp->ref == NULL)
	    {
	      if (!sym->attr.recursive)
		{
		  gfc_error ("DTIO %s procedure at %L must be recursive",
			     sym->name, &sym->declared_at);
		  return;
		}
	    }
	}
    }

  if (ts->type == BT_CLASS && dtio_sub == NULL)
    {
      gfc_error ("Data transfer element at %L cannot be polymorphic unless "
                "it is processed by a defined input/output procedure",
                &code->loc);
      return;
    }

  if (ts->type == BT_DERIVED)
    {
      /* Check that transferred derived type doesn't contain POINTER
	 components unless it is processed by a defined input/output
	 procedure".  */
      if (ts->u.derived->attr.pointer_comp && dtio_sub == NULL)
	{
	  gfc_error ("Data transfer element at %L cannot have POINTER "
		     "components unless it is processed by a defined "
		     "input/output procedure", &code->loc);
	  return;
	}

      /* F08:C935.  */
      if (ts->u.derived->attr.proc_pointer_comp)
	{
	  gfc_error ("Data transfer element at %L cannot have "
		     "procedure pointer components", &code->loc);
	  return;
	}

      if (ts->u.derived->attr.alloc_comp && dtio_sub == NULL)
	{
	  gfc_error ("Data transfer element at %L cannot have ALLOCATABLE "
		     "components unless it is processed by a defined "
		     "input/output procedure", &code->loc);
	  return;
	}

      /* C_PTR and C_FUNPTR have private components which means they can not
         be printed.  However, if -std=gnu and not -pedantic, allow
         the component to be printed to help debugging.  */
      if (ts->u.derived->ts.f90_type == BT_VOID)
	{
	  if (!gfc_notify_std (GFC_STD_GNU, "Data transfer element at %L "
			       "cannot have PRIVATE components", &code->loc))
	    return;
	}
      else if (derived_inaccessible (ts->u.derived) && dtio_sub == NULL)
	{
	  gfc_error ("Data transfer element at %L cannot have "
		     "PRIVATE components unless it is processed by "
		     "a defined input/output procedure", &code->loc);
	  return;
	}
    }

  if (exp->expr_type == EXPR_STRUCTURE)
    return;

  sym = exp->symtree->n.sym;

  if (sym->as != NULL && sym->as->type == AS_ASSUMED_SIZE && exp->ref
      && exp->ref->type == REF_ARRAY && exp->ref->u.ar.type == AR_FULL)
    {
      gfc_error ("Data transfer element at %L cannot be a full reference to "
		 "an assumed-size array", &code->loc);
      return;
    }
}


/*********** Toplevel code resolution subroutines ***********/

/* Find the set of labels that are reachable from this block.  We also
   record the last statement in each block.  */

static void
find_reachable_labels (gfc_code *block)
{
  gfc_code *c;

  if (!block)
    return;

  cs_base->reachable_labels = bitmap_obstack_alloc (&labels_obstack);

  /* Collect labels in this block.  We don't keep those corresponding
     to END {IF|SELECT}, these are checked in resolve_branch by going
     up through the code_stack.  */
  for (c = block; c; c = c->next)
    {
      if (c->here && c->op != EXEC_END_NESTED_BLOCK)
	bitmap_set_bit (cs_base->reachable_labels, c->here->value);
    }

  /* Merge with labels from parent block.  */
  if (cs_base->prev)
    {
      gcc_assert (cs_base->prev->reachable_labels);
      bitmap_ior_into (cs_base->reachable_labels,
		       cs_base->prev->reachable_labels);
    }
}


static void
resolve_lock_unlock_event (gfc_code *code)
{
  if (code->expr1->expr_type == EXPR_FUNCTION
      && code->expr1->value.function.isym
      && code->expr1->value.function.isym->id == GFC_ISYM_CAF_GET)
    remove_caf_get_intrinsic (code->expr1);

  if ((code->op == EXEC_LOCK || code->op == EXEC_UNLOCK)
      && (code->expr1->ts.type != BT_DERIVED
	  || code->expr1->expr_type != EXPR_VARIABLE
	  || code->expr1->ts.u.derived->from_intmod != INTMOD_ISO_FORTRAN_ENV
	  || code->expr1->ts.u.derived->intmod_sym_id != ISOFORTRAN_LOCK_TYPE
	  || code->expr1->rank != 0
	  || (!gfc_is_coarray (code->expr1) &&
	      !gfc_is_coindexed (code->expr1))))
    gfc_error ("Lock variable at %L must be a scalar of type LOCK_TYPE",
	       &code->expr1->where);
  else if ((code->op == EXEC_EVENT_POST || code->op == EXEC_EVENT_WAIT)
	   && (code->expr1->ts.type != BT_DERIVED
	       || code->expr1->expr_type != EXPR_VARIABLE
	       || code->expr1->ts.u.derived->from_intmod
		  != INTMOD_ISO_FORTRAN_ENV
	       || code->expr1->ts.u.derived->intmod_sym_id
		  != ISOFORTRAN_EVENT_TYPE
	       || code->expr1->rank != 0))
    gfc_error ("Event variable at %L must be a scalar of type EVENT_TYPE",
	       &code->expr1->where);
  else if (code->op == EXEC_EVENT_POST && !gfc_is_coarray (code->expr1)
	   && !gfc_is_coindexed (code->expr1))
    gfc_error ("Event variable argument at %L must be a coarray or coindexed",
	       &code->expr1->where);
  else if (code->op == EXEC_EVENT_WAIT && !gfc_is_coarray (code->expr1))
    gfc_error ("Event variable argument at %L must be a coarray but not "
	       "coindexed", &code->expr1->where);

  /* Check STAT.  */
  if (code->expr2
      && (code->expr2->ts.type != BT_INTEGER || code->expr2->rank != 0
	  || code->expr2->expr_type != EXPR_VARIABLE))
    gfc_error ("STAT= argument at %L must be a scalar INTEGER variable",
	       &code->expr2->where);

  if (code->expr2
      && !gfc_check_vardef_context (code->expr2, false, false, false,
				    _("STAT variable")))
    return;

  /* Check ERRMSG.  */
  if (code->expr3
      && (code->expr3->ts.type != BT_CHARACTER || code->expr3->rank != 0
	  || code->expr3->expr_type != EXPR_VARIABLE))
    gfc_error ("ERRMSG= argument at %L must be a scalar CHARACTER variable",
	       &code->expr3->where);

  if (code->expr3
      && !gfc_check_vardef_context (code->expr3, false, false, false,
				    _("ERRMSG variable")))
    return;

  /* Check for LOCK the ACQUIRED_LOCK.  */
  if (code->op != EXEC_EVENT_WAIT && code->expr4
      && (code->expr4->ts.type != BT_LOGICAL || code->expr4->rank != 0
	  || code->expr4->expr_type != EXPR_VARIABLE))
    gfc_error ("ACQUIRED_LOCK= argument at %L must be a scalar LOGICAL "
	       "variable", &code->expr4->where);

  if (code->op != EXEC_EVENT_WAIT && code->expr4
      && !gfc_check_vardef_context (code->expr4, false, false, false,
				    _("ACQUIRED_LOCK variable")))
    return;

  /* Check for EVENT WAIT the UNTIL_COUNT.  */
  if (code->op == EXEC_EVENT_WAIT && code->expr4)
    {
      if (!gfc_resolve_expr (code->expr4) || code->expr4->ts.type != BT_INTEGER
	  || code->expr4->rank != 0)
	gfc_error ("UNTIL_COUNT= argument at %L must be a scalar INTEGER "
		   "expression", &code->expr4->where);
    }
}


static void
resolve_critical (gfc_code *code)
{
  gfc_symtree *symtree;
  gfc_symbol *lock_type;
  char name[GFC_MAX_SYMBOL_LEN];
  static int serial = 0;

  if (flag_coarray != GFC_FCOARRAY_LIB)
    return;

  symtree = gfc_find_symtree (gfc_current_ns->sym_root,
			      GFC_PREFIX ("lock_type"));
  if (symtree)
    lock_type = symtree->n.sym;
  else
    {
      if (gfc_get_sym_tree (GFC_PREFIX ("lock_type"), gfc_current_ns, &symtree,
			    false) != 0)
	gcc_unreachable ();
      lock_type = symtree->n.sym;
      lock_type->attr.flavor = FL_DERIVED;
      lock_type->attr.zero_comp = 1;
      lock_type->from_intmod = INTMOD_ISO_FORTRAN_ENV;
      lock_type->intmod_sym_id = ISOFORTRAN_LOCK_TYPE;
    }

  sprintf(name, GFC_PREFIX ("lock_var") "%d",serial++);
  if (gfc_get_sym_tree (name, gfc_current_ns, &symtree, false) != 0)
    gcc_unreachable ();

  code->resolved_sym = symtree->n.sym;
  symtree->n.sym->attr.flavor = FL_VARIABLE;
  symtree->n.sym->attr.referenced = 1;
  symtree->n.sym->attr.artificial = 1;
  symtree->n.sym->attr.codimension = 1;
  symtree->n.sym->ts.type = BT_DERIVED;
  symtree->n.sym->ts.u.derived = lock_type;
  symtree->n.sym->as = gfc_get_array_spec ();
  symtree->n.sym->as->corank = 1;
  symtree->n.sym->as->type = AS_EXPLICIT;
  symtree->n.sym->as->cotype = AS_EXPLICIT;
  symtree->n.sym->as->lower[0] = gfc_get_int_expr (gfc_default_integer_kind,
						   NULL, 1);
  gfc_commit_symbols();
}


static void
resolve_sync (gfc_code *code)
{
  /* Check imageset. The * case matches expr1 == NULL.  */
  if (code->expr1)
    {
      if (code->expr1->ts.type != BT_INTEGER || code->expr1->rank > 1)
	gfc_error ("Imageset argument at %L must be a scalar or rank-1 "
		   "INTEGER expression", &code->expr1->where);
      if (code->expr1->expr_type == EXPR_CONSTANT && code->expr1->rank == 0
	  && mpz_cmp_si (code->expr1->value.integer, 1) < 0)
	gfc_error ("Imageset argument at %L must between 1 and num_images()",
		   &code->expr1->where);
      else if (code->expr1->expr_type == EXPR_ARRAY
	       && gfc_simplify_expr (code->expr1, 0))
	{
	   gfc_constructor *cons;
	   cons = gfc_constructor_first (code->expr1->value.constructor);
	   for (; cons; cons = gfc_constructor_next (cons))
	     if (cons->expr->expr_type == EXPR_CONSTANT
		 &&  mpz_cmp_si (cons->expr->value.integer, 1) < 0)
	       gfc_error ("Imageset argument at %L must between 1 and "
			  "num_images()", &cons->expr->where);
	}
    }

  /* Check STAT.  */
  if (code->expr2
      && (code->expr2->ts.type != BT_INTEGER || code->expr2->rank != 0
	  || code->expr2->expr_type != EXPR_VARIABLE))
    gfc_error ("STAT= argument at %L must be a scalar INTEGER variable",
	       &code->expr2->where);

  /* Check ERRMSG.  */
  if (code->expr3
      && (code->expr3->ts.type != BT_CHARACTER || code->expr3->rank != 0
	  || code->expr3->expr_type != EXPR_VARIABLE))
    gfc_error ("ERRMSG= argument at %L must be a scalar CHARACTER variable",
	       &code->expr3->where);
}


/* Given a branch to a label, see if the branch is conforming.
   The code node describes where the branch is located.  */

static void
resolve_branch (gfc_st_label *label, gfc_code *code)
{
  code_stack *stack;

  if (label == NULL)
    return;

  /* Step one: is this a valid branching target?  */

  if (label->defined == ST_LABEL_UNKNOWN)
    {
      gfc_error ("Label %d referenced at %L is never defined", label->value,
		 &code->loc);
      return;
    }

  if (label->defined != ST_LABEL_TARGET && label->defined != ST_LABEL_DO_TARGET)
    {
      gfc_error ("Statement at %L is not a valid branch target statement "
		 "for the branch statement at %L", &label->where, &code->loc);
      return;
    }

  /* Step two: make sure this branch is not a branch to itself ;-)  */

  if (code->here == label)
    {
      gfc_warning (0,
		   "Branch at %L may result in an infinite loop", &code->loc);
      return;
    }

  /* Step three:  See if the label is in the same block as the
     branching statement.  The hard work has been done by setting up
     the bitmap reachable_labels.  */

  if (bitmap_bit_p (cs_base->reachable_labels, label->value))
    {
      /* Check now whether there is a CRITICAL construct; if so, check
	 whether the label is still visible outside of the CRITICAL block,
	 which is invalid.  */
      for (stack = cs_base; stack; stack = stack->prev)
	{
	  if (stack->current->op == EXEC_CRITICAL
	      && bitmap_bit_p (stack->reachable_labels, label->value))
	    gfc_error ("GOTO statement at %L leaves CRITICAL construct for "
		      "label at %L", &code->loc, &label->where);
	  else if (stack->current->op == EXEC_DO_CONCURRENT
		   && bitmap_bit_p (stack->reachable_labels, label->value))
	    gfc_error ("GOTO statement at %L leaves DO CONCURRENT construct "
		      "for label at %L", &code->loc, &label->where);
	}

      return;
    }

  /* Step four:  If we haven't found the label in the bitmap, it may
    still be the label of the END of the enclosing block, in which
    case we find it by going up the code_stack.  */

  for (stack = cs_base; stack; stack = stack->prev)
    {
      if (stack->current->next && stack->current->next->here == label)
	break;
      if (stack->current->op == EXEC_CRITICAL)
	{
	  /* Note: A label at END CRITICAL does not leave the CRITICAL
	     construct as END CRITICAL is still part of it.  */
	  gfc_error ("GOTO statement at %L leaves CRITICAL construct for label"
		      " at %L", &code->loc, &label->where);
	  return;
	}
      else if (stack->current->op == EXEC_DO_CONCURRENT)
	{
	  gfc_error ("GOTO statement at %L leaves DO CONCURRENT construct for "
		     "label at %L", &code->loc, &label->where);
	  return;
	}
    }

  if (stack)
    {
      gcc_assert (stack->current->next->op == EXEC_END_NESTED_BLOCK);
      return;
    }

  /* The label is not in an enclosing block, so illegal.  This was
     allowed in Fortran 66, so we allow it as extension.  No
     further checks are necessary in this case.  */
  gfc_notify_std (GFC_STD_LEGACY, "Label at %L is not in the same block "
		  "as the GOTO statement at %L", &label->where,
		  &code->loc);
  return;
}


/* Check whether EXPR1 has the same shape as EXPR2.  */

static bool
resolve_where_shape (gfc_expr *expr1, gfc_expr *expr2)
{
  mpz_t shape[GFC_MAX_DIMENSIONS];
  mpz_t shape2[GFC_MAX_DIMENSIONS];
  bool result = false;
  int i;

  /* Compare the rank.  */
  if (expr1->rank != expr2->rank)
    return result;

  /* Compare the size of each dimension.  */
  for (i=0; i<expr1->rank; i++)
    {
      if (!gfc_array_dimen_size (expr1, i, &shape[i]))
	goto ignore;

      if (!gfc_array_dimen_size (expr2, i, &shape2[i]))
	goto ignore;

      if (mpz_cmp (shape[i], shape2[i]))
	goto over;
    }

  /* When either of the two expression is an assumed size array, we
     ignore the comparison of dimension sizes.  */
ignore:
  result = true;

over:
  gfc_clear_shape (shape, i);
  gfc_clear_shape (shape2, i);
  return result;
}


/* Check whether a WHERE assignment target or a WHERE mask expression
   has the same shape as the outmost WHERE mask expression.  */

static void
resolve_where (gfc_code *code, gfc_expr *mask)
{
  gfc_code *cblock;
  gfc_code *cnext;
  gfc_expr *e = NULL;

  cblock = code->block;

  /* Store the first WHERE mask-expr of the WHERE statement or construct.
     In case of nested WHERE, only the outmost one is stored.  */
  if (mask == NULL) /* outmost WHERE */
    e = cblock->expr1;
  else /* inner WHERE */
    e = mask;

  while (cblock)
    {
      if (cblock->expr1)
	{
	  /* Check if the mask-expr has a consistent shape with the
	     outmost WHERE mask-expr.  */
	  if (!resolve_where_shape (cblock->expr1, e))
	    gfc_error ("WHERE mask at %L has inconsistent shape",
		       &cblock->expr1->where);
	 }

      /* the assignment statement of a WHERE statement, or the first
	 statement in where-body-construct of a WHERE construct */
      cnext = cblock->next;
      while (cnext)
	{
	  switch (cnext->op)
	    {
	    /* WHERE assignment statement */
	    case EXEC_ASSIGN:

	      /* Check shape consistent for WHERE assignment target.  */
	      if (e && !resolve_where_shape (cnext->expr1, e))
	       gfc_error ("WHERE assignment target at %L has "
			  "inconsistent shape", &cnext->expr1->where);
	      break;


	    case EXEC_ASSIGN_CALL:
	      resolve_call (cnext);
	      if (!cnext->resolved_sym->attr.elemental)
		gfc_error("Non-ELEMENTAL user-defined assignment in WHERE at %L",
			  &cnext->ext.actual->expr->where);
	      break;

	    /* WHERE or WHERE construct is part of a where-body-construct */
	    case EXEC_WHERE:
	      resolve_where (cnext, e);
	      break;

	    default:
	      gfc_error ("Unsupported statement inside WHERE at %L",
			 &cnext->loc);
	    }
	 /* the next statement within the same where-body-construct */
	 cnext = cnext->next;
       }
    /* the next masked-elsewhere-stmt, elsewhere-stmt, or end-where-stmt */
    cblock = cblock->block;
  }
}


/* Resolve assignment in FORALL construct.
   NVAR is the number of FORALL index variables, and VAR_EXPR records the
   FORALL index variables.  */

static void
gfc_resolve_assign_in_forall (gfc_code *code, int nvar, gfc_expr **var_expr)
{
  int n;

  for (n = 0; n < nvar; n++)
    {
      gfc_symbol *forall_index;

      forall_index = var_expr[n]->symtree->n.sym;

      /* Check whether the assignment target is one of the FORALL index
	 variable.  */
      if ((code->expr1->expr_type == EXPR_VARIABLE)
	  && (code->expr1->symtree->n.sym == forall_index))
	gfc_error ("Assignment to a FORALL index variable at %L",
		   &code->expr1->where);
      else
	{
	  /* If one of the FORALL index variables doesn't appear in the
	     assignment variable, then there could be a many-to-one
	     assignment.  Emit a warning rather than an error because the
	     mask could be resolving this problem.  */
	  if (!find_forall_index (code->expr1, forall_index, 0))
	    gfc_warning (0, "The FORALL with index %qs is not used on the "
			 "left side of the assignment at %L and so might "
			 "cause multiple assignment to this object",
			 var_expr[n]->symtree->name, &code->expr1->where);
	}
    }
}


/* Resolve WHERE statement in FORALL construct.  */

static void
gfc_resolve_where_code_in_forall (gfc_code *code, int nvar,
				  gfc_expr **var_expr)
{
  gfc_code *cblock;
  gfc_code *cnext;

  cblock = code->block;
  while (cblock)
    {
      /* the assignment statement of a WHERE statement, or the first
	 statement in where-body-construct of a WHERE construct */
      cnext = cblock->next;
      while (cnext)
	{
	  switch (cnext->op)
	    {
	    /* WHERE assignment statement */
	    case EXEC_ASSIGN:
	      gfc_resolve_assign_in_forall (cnext, nvar, var_expr);
	      break;

	    /* WHERE operator assignment statement */
	    case EXEC_ASSIGN_CALL:
	      resolve_call (cnext);
	      if (!cnext->resolved_sym->attr.elemental)
		gfc_error("Non-ELEMENTAL user-defined assignment in WHERE at %L",
			  &cnext->ext.actual->expr->where);
	      break;

	    /* WHERE or WHERE construct is part of a where-body-construct */
	    case EXEC_WHERE:
	      gfc_resolve_where_code_in_forall (cnext, nvar, var_expr);
	      break;

	    default:
	      gfc_error ("Unsupported statement inside WHERE at %L",
			 &cnext->loc);
	    }
	  /* the next statement within the same where-body-construct */
	  cnext = cnext->next;
	}
      /* the next masked-elsewhere-stmt, elsewhere-stmt, or end-where-stmt */
      cblock = cblock->block;
    }
}


/* Traverse the FORALL body to check whether the following errors exist:
   1. For assignment, check if a many-to-one assignment happens.
   2. For WHERE statement, check the WHERE body to see if there is any
      many-to-one assignment.  */

static void
gfc_resolve_forall_body (gfc_code *code, int nvar, gfc_expr **var_expr)
{
  gfc_code *c;

  c = code->block->next;
  while (c)
    {
      switch (c->op)
	{
	case EXEC_ASSIGN:
	case EXEC_POINTER_ASSIGN:
	  gfc_resolve_assign_in_forall (c, nvar, var_expr);
	  break;

	case EXEC_ASSIGN_CALL:
	  resolve_call (c);
	  break;

	/* Because the gfc_resolve_blocks() will handle the nested FORALL,
	   there is no need to handle it here.  */
	case EXEC_FORALL:
	  break;
	case EXEC_WHERE:
	  gfc_resolve_where_code_in_forall(c, nvar, var_expr);
	  break;
	default:
	  break;
	}
      /* The next statement in the FORALL body.  */
      c = c->next;
    }
}


/* Counts the number of iterators needed inside a forall construct, including
   nested forall constructs. This is used to allocate the needed memory
   in gfc_resolve_forall.  */

static int
gfc_count_forall_iterators (gfc_code *code)
{
  int max_iters, sub_iters, current_iters;
  gfc_forall_iterator *fa;

  gcc_assert(code->op == EXEC_FORALL);
  max_iters = 0;
  current_iters = 0;

  for (fa = code->ext.forall_iterator; fa; fa = fa->next)
    current_iters ++;

  code = code->block->next;

  while (code)
    {
      if (code->op == EXEC_FORALL)
        {
          sub_iters = gfc_count_forall_iterators (code);
          if (sub_iters > max_iters)
            max_iters = sub_iters;
        }
      code = code->next;
    }

  return current_iters + max_iters;
}


/* Given a FORALL construct, first resolve the FORALL iterator, then call
   gfc_resolve_forall_body to resolve the FORALL body.  */

static void
gfc_resolve_forall (gfc_code *code, gfc_namespace *ns, int forall_save)
{
  static gfc_expr **var_expr;
  static int total_var = 0;
  static int nvar = 0;
  int i, old_nvar, tmp;
  gfc_forall_iterator *fa;

  old_nvar = nvar;

  /* Start to resolve a FORALL construct   */
  if (forall_save == 0)
    {
      /* Count the total number of FORALL indices in the nested FORALL
         construct in order to allocate the VAR_EXPR with proper size.  */
      total_var = gfc_count_forall_iterators (code);

      /* Allocate VAR_EXPR with NUMBER_OF_FORALL_INDEX elements.  */
      var_expr = XCNEWVEC (gfc_expr *, total_var);
    }

  /* The information about FORALL iterator, including FORALL indices start, end
     and stride.  An outer FORALL indice cannot appear in start, end or stride.  */
  for (fa = code->ext.forall_iterator; fa; fa = fa->next)
    {
      /* Fortran 20008: C738 (R753).  */
      if (fa->var->ref && fa->var->ref->type == REF_ARRAY)
	{
	  gfc_error ("FORALL index-name at %L must be a scalar variable "
		     "of type integer", &fa->var->where);
	  continue;
	}

      /* Check if any outer FORALL index name is the same as the current
	 one.  */
      for (i = 0; i < nvar; i++)
	{
	  if (fa->var->symtree->n.sym == var_expr[i]->symtree->n.sym)
	    gfc_error ("An outer FORALL construct already has an index "
			"with this name %L", &fa->var->where);
	}

      /* Record the current FORALL index.  */
      var_expr[nvar] = gfc_copy_expr (fa->var);

      nvar++;

      /* No memory leak.  */
      gcc_assert (nvar <= total_var);
    }

  /* Resolve the FORALL body.  */
  gfc_resolve_forall_body (code, nvar, var_expr);

  /* May call gfc_resolve_forall to resolve the inner FORALL loop.  */
  gfc_resolve_blocks (code->block, ns);

  tmp = nvar;
  nvar = old_nvar;
  /* Free only the VAR_EXPRs allocated in this frame.  */
  for (i = nvar; i < tmp; i++)
     gfc_free_expr (var_expr[i]);

  if (nvar == 0)
    {
      /* We are in the outermost FORALL construct.  */
      gcc_assert (forall_save == 0);

      /* VAR_EXPR is not needed any more.  */
      free (var_expr);
      total_var = 0;
    }
}


/* Resolve a BLOCK construct statement.  */

static void
resolve_block_construct (gfc_code* code)
{
  /* Resolve the BLOCK's namespace.  */
  gfc_resolve (code->ext.block.ns);

  /* For an ASSOCIATE block, the associations (and their targets) are already
     resolved during resolve_symbol.  */
}


/* Resolve lists of blocks found in IF, SELECT CASE, WHERE, FORALL, GOTO and
   DO code nodes.  */

void
gfc_resolve_blocks (gfc_code *b, gfc_namespace *ns)
{
  bool t;

  for (; b; b = b->block)
    {
      t = gfc_resolve_expr (b->expr1);
      if (!gfc_resolve_expr (b->expr2))
	t = false;

      switch (b->op)
	{
	case EXEC_IF:
	  if (t && b->expr1 != NULL
	      && (b->expr1->ts.type != BT_LOGICAL || b->expr1->rank != 0))
	    gfc_error ("IF clause at %L requires a scalar LOGICAL expression",
		       &b->expr1->where);
	  break;

	case EXEC_WHERE:
	  if (t
	      && b->expr1 != NULL
	      && (b->expr1->ts.type != BT_LOGICAL || b->expr1->rank == 0))
	    gfc_error ("WHERE/ELSEWHERE clause at %L requires a LOGICAL array",
		       &b->expr1->where);
	  break;

	case EXEC_GOTO:
	  resolve_branch (b->label1, b);
	  break;

	case EXEC_BLOCK:
	  resolve_block_construct (b);
	  break;

	case EXEC_SELECT:
	case EXEC_SELECT_TYPE:
	case EXEC_FORALL:
	case EXEC_DO:
	case EXEC_DO_WHILE:
	case EXEC_DO_CONCURRENT:
	case EXEC_CRITICAL:
	case EXEC_READ:
	case EXEC_WRITE:
	case EXEC_IOLENGTH:
	case EXEC_WAIT:
	  break;

	case EXEC_OMP_ATOMIC:
	case EXEC_OACC_ATOMIC:
	  {
	    gfc_omp_atomic_op aop
	      = (gfc_omp_atomic_op) (b->ext.omp_atomic & GFC_OMP_ATOMIC_MASK);

	    /* Verify this before calling gfc_resolve_code, which might
	       change it.  */
	    gcc_assert (b->next && b->next->op == EXEC_ASSIGN);
	    gcc_assert (((aop != GFC_OMP_ATOMIC_CAPTURE)
			 && b->next->next == NULL)
			|| ((aop == GFC_OMP_ATOMIC_CAPTURE)
			    && b->next->next != NULL
			    && b->next->next->op == EXEC_ASSIGN
			    && b->next->next->next == NULL));
	  }
	  break;

	case EXEC_OACC_PARALLEL_LOOP:
	case EXEC_OACC_PARALLEL:
	case EXEC_OACC_KERNELS_LOOP:
	case EXEC_OACC_KERNELS:
	case EXEC_OACC_DATA:
	case EXEC_OACC_HOST_DATA:
	case EXEC_OACC_LOOP:
	case EXEC_OACC_UPDATE:
	case EXEC_OACC_WAIT:
	case EXEC_OACC_CACHE:
	case EXEC_OACC_ENTER_DATA:
	case EXEC_OACC_EXIT_DATA:
	case EXEC_OACC_ROUTINE:
	case EXEC_OMP_CRITICAL:
	case EXEC_OMP_DISTRIBUTE:
	case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
	case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
	case EXEC_OMP_DISTRIBUTE_SIMD:
	case EXEC_OMP_DO:
	case EXEC_OMP_DO_SIMD:
	case EXEC_OMP_MASTER:
	case EXEC_OMP_ORDERED:
	case EXEC_OMP_PARALLEL:
	case EXEC_OMP_PARALLEL_DO:
	case EXEC_OMP_PARALLEL_DO_SIMD:
	case EXEC_OMP_PARALLEL_SECTIONS:
	case EXEC_OMP_PARALLEL_WORKSHARE:
	case EXEC_OMP_SECTIONS:
	case EXEC_OMP_SIMD:
	case EXEC_OMP_SINGLE:
	case EXEC_OMP_TARGET:
	case EXEC_OMP_TARGET_DATA:
	case EXEC_OMP_TARGET_ENTER_DATA:
	case EXEC_OMP_TARGET_EXIT_DATA:
	case EXEC_OMP_TARGET_PARALLEL:
	case EXEC_OMP_TARGET_PARALLEL_DO:
	case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
	case EXEC_OMP_TARGET_SIMD:
	case EXEC_OMP_TARGET_TEAMS:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
	case EXEC_OMP_TARGET_UPDATE:
	case EXEC_OMP_TASK:
	case EXEC_OMP_TASKGROUP:
	case EXEC_OMP_TASKLOOP:
	case EXEC_OMP_TASKLOOP_SIMD:
	case EXEC_OMP_TASKWAIT:
	case EXEC_OMP_TASKYIELD:
	case EXEC_OMP_TEAMS:
	case EXEC_OMP_TEAMS_DISTRIBUTE:
	case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
	case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
	case EXEC_OMP_WORKSHARE:
	  break;

	default:
	  gfc_internal_error ("gfc_resolve_blocks(): Bad block type");
	}

      gfc_resolve_code (b->next, ns);
    }
}


/* Does everything to resolve an ordinary assignment.  Returns true
   if this is an interface assignment.  */
static bool
resolve_ordinary_assign (gfc_code *code, gfc_namespace *ns)
{
  bool rval = false;
  gfc_expr *lhs;
  gfc_expr *rhs;
  int llen = 0;
  int rlen = 0;
  int n;
  gfc_ref *ref;
  symbol_attribute attr;

  if (gfc_extend_assign (code, ns))
    {
      gfc_expr** rhsptr;

      if (code->op == EXEC_ASSIGN_CALL)
	{
	  lhs = code->ext.actual->expr;
	  rhsptr = &code->ext.actual->next->expr;
	}
      else
	{
	  gfc_actual_arglist* args;
	  gfc_typebound_proc* tbp;

	  gcc_assert (code->op == EXEC_COMPCALL);

	  args = code->expr1->value.compcall.actual;
	  lhs = args->expr;
	  rhsptr = &args->next->expr;

	  tbp = code->expr1->value.compcall.tbp;
	  gcc_assert (!tbp->is_generic);
	}

      /* Make a temporary rhs when there is a default initializer
	 and rhs is the same symbol as the lhs.  */
      if ((*rhsptr)->expr_type == EXPR_VARIABLE
	    && (*rhsptr)->symtree->n.sym->ts.type == BT_DERIVED
	    && gfc_has_default_initializer ((*rhsptr)->symtree->n.sym->ts.u.derived)
	    && (lhs->symtree->n.sym == (*rhsptr)->symtree->n.sym))
	*rhsptr = gfc_get_parentheses (*rhsptr);

      return true;
    }

  lhs = code->expr1;
  rhs = code->expr2;

  if (rhs->is_boz
      && !gfc_notify_std (GFC_STD_GNU, "BOZ literal at %L outside "
			  "a DATA statement and outside INT/REAL/DBLE/CMPLX",
			  &code->loc))
    return false;

  /* Handle the case of a BOZ literal on the RHS.  */
  if (rhs->is_boz && lhs->ts.type != BT_INTEGER)
    {
      int rc;
      if (warn_surprising)
	gfc_warning (OPT_Wsurprising,
		     "BOZ literal at %L is bitwise transferred "
		     "non-integer symbol %qs", &code->loc,
		     lhs->symtree->n.sym->name);

      if (!gfc_convert_boz (rhs, &lhs->ts))
	return false;
      if ((rc = gfc_range_check (rhs)) != ARITH_OK)
	{
	  if (rc == ARITH_UNDERFLOW)
	    gfc_error ("Arithmetic underflow of bit-wise transferred BOZ at %L"
		       ". This check can be disabled with the option "
		       "%<-fno-range-check%>", &rhs->where);
	  else if (rc == ARITH_OVERFLOW)
	    gfc_error ("Arithmetic overflow of bit-wise transferred BOZ at %L"
		       ". This check can be disabled with the option "
		       "%<-fno-range-check%>", &rhs->where);
	  else if (rc == ARITH_NAN)
	    gfc_error ("Arithmetic NaN of bit-wise transferred BOZ at %L"
		       ". This check can be disabled with the option "
		       "%<-fno-range-check%>", &rhs->where);
	  return false;
	}
    }

  if (lhs->ts.type == BT_CHARACTER
	&& warn_character_truncation)
    {
      if (lhs->ts.u.cl != NULL
	    && lhs->ts.u.cl->length != NULL
	    && lhs->ts.u.cl->length->expr_type == EXPR_CONSTANT)
	llen = mpz_get_si (lhs->ts.u.cl->length->value.integer);

      if (rhs->expr_type == EXPR_CONSTANT)
 	rlen = rhs->value.character.length;

      else if (rhs->ts.u.cl != NULL
		 && rhs->ts.u.cl->length != NULL
		 && rhs->ts.u.cl->length->expr_type == EXPR_CONSTANT)
	rlen = mpz_get_si (rhs->ts.u.cl->length->value.integer);

      if (rlen && llen && rlen > llen)
	gfc_warning_now (OPT_Wcharacter_truncation,
			 "CHARACTER expression will be truncated "
			 "in assignment (%d/%d) at %L",
			 llen, rlen, &code->loc);
    }

  /* Ensure that a vector index expression for the lvalue is evaluated
     to a temporary if the lvalue symbol is referenced in it.  */
  if (lhs->rank)
    {
      for (ref = lhs->ref; ref; ref= ref->next)
	if (ref->type == REF_ARRAY)
	  {
	    for (n = 0; n < ref->u.ar.dimen; n++)
	      if (ref->u.ar.dimen_type[n] == DIMEN_VECTOR
		  && gfc_find_sym_in_expr (lhs->symtree->n.sym,
					   ref->u.ar.start[n]))
		ref->u.ar.start[n]
			= gfc_get_parentheses (ref->u.ar.start[n]);
	  }
    }

  if (gfc_pure (NULL))
    {
      if (lhs->ts.type == BT_DERIVED
	    && lhs->expr_type == EXPR_VARIABLE
	    && lhs->ts.u.derived->attr.pointer_comp
	    && rhs->expr_type == EXPR_VARIABLE
	    && (gfc_impure_variable (rhs->symtree->n.sym)
		|| gfc_is_coindexed (rhs)))
	{
	  /* F2008, C1283.  */
	  if (gfc_is_coindexed (rhs))
	    gfc_error ("Coindexed expression at %L is assigned to "
			"a derived type variable with a POINTER "
			"component in a PURE procedure",
			&rhs->where);
	  else
	    gfc_error ("The impure variable at %L is assigned to "
			"a derived type variable with a POINTER "
			"component in a PURE procedure (12.6)",
			&rhs->where);
	  return rval;
	}

      /* Fortran 2008, C1283.  */
      if (gfc_is_coindexed (lhs))
	{
	  gfc_error ("Assignment to coindexed variable at %L in a PURE "
		     "procedure", &rhs->where);
	  return rval;
	}
    }

  if (gfc_implicit_pure (NULL))
    {
      if (lhs->expr_type == EXPR_VARIABLE
	    && lhs->symtree->n.sym != gfc_current_ns->proc_name
	    && lhs->symtree->n.sym->ns != gfc_current_ns)
	gfc_unset_implicit_pure (NULL);

      if (lhs->ts.type == BT_DERIVED
	    && lhs->expr_type == EXPR_VARIABLE
	    && lhs->ts.u.derived->attr.pointer_comp
	    && rhs->expr_type == EXPR_VARIABLE
	    && (gfc_impure_variable (rhs->symtree->n.sym)
		|| gfc_is_coindexed (rhs)))
	gfc_unset_implicit_pure (NULL);

      /* Fortran 2008, C1283.  */
      if (gfc_is_coindexed (lhs))
	gfc_unset_implicit_pure (NULL);
    }

  /* F2008, 7.2.1.2.  */
  attr = gfc_expr_attr (lhs);
  if (lhs->ts.type == BT_CLASS && attr.allocatable)
    {
      if (attr.codimension)
	{
	  gfc_error ("Assignment to polymorphic coarray at %L is not "
		     "permitted", &lhs->where);
	  return false;
	}
      if (!gfc_notify_std (GFC_STD_F2008, "Assignment to an allocatable "
			   "polymorphic variable at %L", &lhs->where))
	return false;
      if (!flag_realloc_lhs)
	{
	  gfc_error ("Assignment to an allocatable polymorphic variable at %L "
		     "requires %<-frealloc-lhs%>", &lhs->where);
	  return false;
	}
    }
  else if (lhs->ts.type == BT_CLASS)
    {
      gfc_error ("Nonallocatable variable must not be polymorphic in intrinsic "
		 "assignment at %L - check that there is a matching specific "
		 "subroutine for '=' operator", &lhs->where);
      return false;
    }

  bool lhs_coindexed = gfc_is_coindexed (lhs);

  /* F2008, Section 7.2.1.2.  */
  if (lhs_coindexed && gfc_has_ultimate_allocatable (lhs))
    {
      gfc_error ("Coindexed variable must not have an allocatable ultimate "
		 "component in assignment at %L", &lhs->where);
      return false;
    }

  /* Assign the 'data' of a class object to a derived type.  */
  if (lhs->ts.type == BT_DERIVED
      && rhs->ts.type == BT_CLASS)
    gfc_add_data_component (rhs);

  bool caf_convert_to_send = flag_coarray == GFC_FCOARRAY_LIB
      && (lhs_coindexed
	  || (code->expr2->expr_type == EXPR_FUNCTION
	      && code->expr2->value.function.isym
	      && code->expr2->value.function.isym->id == GFC_ISYM_CAF_GET
	      && (code->expr1->rank == 0 || code->expr2->rank != 0)
	      && !gfc_expr_attr (rhs).allocatable
	      && !gfc_has_vector_subscript (rhs)));

  gfc_check_assign (lhs, rhs, 1, !caf_convert_to_send);

  /* Insert a GFC_ISYM_CAF_SEND intrinsic, when the LHS is a coindexed variable.
     Additionally, insert this code when the RHS is a CAF as we then use the
     GFC_ISYM_CAF_SEND intrinsic just to avoid a temporary; but do not do so if
     the LHS is (re)allocatable or has a vector subscript.  If the LHS is a
     noncoindexed array and the RHS is a coindexed scalar, use the normal code
     path.  */
  if (caf_convert_to_send)
    {
      if (code->expr2->expr_type == EXPR_FUNCTION
	  && code->expr2->value.function.isym
	  && code->expr2->value.function.isym->id == GFC_ISYM_CAF_GET)
	remove_caf_get_intrinsic (code->expr2);
      code->op = EXEC_CALL;
      gfc_get_sym_tree (GFC_PREFIX ("caf_send"), ns, &code->symtree, true);
      code->resolved_sym = code->symtree->n.sym;
      code->resolved_sym->attr.flavor = FL_PROCEDURE;
      code->resolved_sym->attr.intrinsic = 1;
      code->resolved_sym->attr.subroutine = 1;
      code->resolved_isym = gfc_intrinsic_subroutine_by_id (GFC_ISYM_CAF_SEND);
      gfc_commit_symbol (code->resolved_sym);
      code->ext.actual = gfc_get_actual_arglist ();
      code->ext.actual->expr = lhs;
      code->ext.actual->next = gfc_get_actual_arglist ();
      code->ext.actual->next->expr = rhs;
      code->expr1 = NULL;
      code->expr2 = NULL;
    }

  return false;
}


/* Add a component reference onto an expression.  */

static void
add_comp_ref (gfc_expr *e, gfc_component *c)
{
  gfc_ref **ref;
  ref = &(e->ref);
  while (*ref)
    ref = &((*ref)->next);
  *ref = gfc_get_ref ();
  (*ref)->type = REF_COMPONENT;
  (*ref)->u.c.sym = e->ts.u.derived;
  (*ref)->u.c.component = c;
  e->ts = c->ts;

  /* Add a full array ref, as necessary.  */
  if (c->as)
    {
      gfc_add_full_array_ref (e, c->as);
      e->rank = c->as->rank;
    }
}


/* Build an assignment.  Keep the argument 'op' for future use, so that
   pointer assignments can be made.  */

static gfc_code *
build_assignment (gfc_exec_op op, gfc_expr *expr1, gfc_expr *expr2,
		  gfc_component *comp1, gfc_component *comp2, locus loc)
{
  gfc_code *this_code;

  this_code = gfc_get_code (op);
  this_code->next = NULL;
  this_code->expr1 = gfc_copy_expr (expr1);
  this_code->expr2 = gfc_copy_expr (expr2);
  this_code->loc = loc;
  if (comp1 && comp2)
    {
      add_comp_ref (this_code->expr1, comp1);
      add_comp_ref (this_code->expr2, comp2);
    }

  return this_code;
}


/* Makes a temporary variable expression based on the characteristics of
   a given variable expression.  */

static gfc_expr*
get_temp_from_expr (gfc_expr *e, gfc_namespace *ns)
{
  static int serial = 0;
  char name[GFC_MAX_SYMBOL_LEN];
  gfc_symtree *tmp;
  gfc_array_spec *as;
  gfc_array_ref *aref;
  gfc_ref *ref;

  sprintf (name, GFC_PREFIX("DA%d"), serial++);
  gfc_get_sym_tree (name, ns, &tmp, false);
  gfc_add_type (tmp->n.sym, &e->ts, NULL);

  as = NULL;
  ref = NULL;
  aref = NULL;

  /* Obtain the arrayspec for the temporary.  */
   if (e->rank && e->expr_type != EXPR_ARRAY
       && e->expr_type != EXPR_FUNCTION
       && e->expr_type != EXPR_OP)
    {
      aref = gfc_find_array_ref (e);
      if (e->expr_type == EXPR_VARIABLE
	  && e->symtree->n.sym->as == aref->as)
	as = aref->as;
      else
	{
	  for (ref = e->ref; ref; ref = ref->next)
	    if (ref->type == REF_COMPONENT
		&& ref->u.c.component->as == aref->as)
	      {
		as = aref->as;
		break;
	      }
	}
    }

  /* Add the attributes and the arrayspec to the temporary.  */
  tmp->n.sym->attr = gfc_expr_attr (e);
  tmp->n.sym->attr.function = 0;
  tmp->n.sym->attr.result = 0;
  tmp->n.sym->attr.flavor = FL_VARIABLE;

  if (as)
    {
      tmp->n.sym->as = gfc_copy_array_spec (as);
      if (!ref)
	ref = e->ref;
      if (as->type == AS_DEFERRED)
	tmp->n.sym->attr.allocatable = 1;
    }
  else if (e->rank && (e->expr_type == EXPR_ARRAY
		       || e->expr_type == EXPR_FUNCTION
		       || e->expr_type == EXPR_OP))
    {
      tmp->n.sym->as = gfc_get_array_spec ();
      tmp->n.sym->as->type = AS_DEFERRED;
      tmp->n.sym->as->rank = e->rank;
      tmp->n.sym->attr.allocatable = 1;
      tmp->n.sym->attr.dimension = 1;
    }
  else
    tmp->n.sym->attr.dimension = 0;

  gfc_set_sym_referenced (tmp->n.sym);
  gfc_commit_symbol (tmp->n.sym);
  e = gfc_lval_expr_from_sym (tmp->n.sym);

  /* Should the lhs be a section, use its array ref for the
     temporary expression.  */
  if (aref && aref->type != AR_FULL)
    {
      gfc_free_ref_list (e->ref);
      e->ref = gfc_copy_ref (ref);
    }
  return e;
}


/* Add one line of code to the code chain, making sure that 'head' and
   'tail' are appropriately updated.  */

static void
add_code_to_chain (gfc_code **this_code, gfc_code **head, gfc_code **tail)
{
  gcc_assert (this_code);
  if (*head == NULL)
    *head = *tail = *this_code;
  else
    *tail = gfc_append_code (*tail, *this_code);
  *this_code = NULL;
}


/* Counts the potential number of part array references that would
   result from resolution of typebound defined assignments.  */

static int
nonscalar_typebound_assign (gfc_symbol *derived, int depth)
{
  gfc_component *c;
  int c_depth = 0, t_depth;

  for (c= derived->components; c; c = c->next)
    {
      if ((!gfc_bt_struct (c->ts.type)
	    || c->attr.pointer
	    || c->attr.allocatable
	    || c->attr.proc_pointer_comp
	    || c->attr.class_pointer
	    || c->attr.proc_pointer)
	  && !c->attr.defined_assign_comp)
	continue;

      if (c->as && c_depth == 0)
	c_depth = 1;

      if (c->ts.u.derived->attr.defined_assign_comp)
	t_depth = nonscalar_typebound_assign (c->ts.u.derived,
					      c->as ? 1 : 0);
      else
	t_depth = 0;

      c_depth = t_depth > c_depth ? t_depth : c_depth;
    }
  return depth + c_depth;
}


/* Implement 7.2.1.3 of the F08 standard:
   "An intrinsic assignment where the variable is of derived type is
   performed as if each component of the variable were assigned from the
   corresponding component of expr using pointer assignment (7.2.2) for
   each pointer component, defined assignment for each nonpointer
   nonallocatable component of a type that has a type-bound defined
   assignment consistent with the component, intrinsic assignment for
   each other nonpointer nonallocatable component, ..."

   The pointer assignments are taken care of by the intrinsic
   assignment of the structure itself.  This function recursively adds
   defined assignments where required.  The recursion is accomplished
   by calling gfc_resolve_code.

   When the lhs in a defined assignment has intent INOUT, we need a
   temporary for the lhs.  In pseudo-code:

   ! Only call function lhs once.
      if (lhs is not a constant or an variable)
	  temp_x = expr2
          expr2 => temp_x
   ! Do the intrinsic assignment
      expr1 = expr2
   ! Now do the defined assignments
      do over components with typebound defined assignment [%cmp]
	#if one component's assignment procedure is INOUT
	  t1 = expr1
	  #if expr2 non-variable
	    temp_x = expr2
	    expr2 => temp_x
	  # endif
	  expr1 = expr2
	  # for each cmp
	    t1%cmp {defined=} expr2%cmp
	    expr1%cmp = t1%cmp
	#else
	  expr1 = expr2

	# for each cmp
	  expr1%cmp {defined=} expr2%cmp
	#endif
   */

/* The temporary assignments have to be put on top of the additional
   code to avoid the result being changed by the intrinsic assignment.
   */
static int component_assignment_level = 0;
static gfc_code *tmp_head = NULL, *tmp_tail = NULL;

static void
generate_component_assignments (gfc_code **code, gfc_namespace *ns)
{
  gfc_component *comp1, *comp2;
  gfc_code *this_code = NULL, *head = NULL, *tail = NULL;
  gfc_expr *t1;
  int error_count, depth;

  gfc_get_errors (NULL, &error_count);

  /* Filter out continuing processing after an error.  */
  if (error_count
      || (*code)->expr1->ts.type != BT_DERIVED
      || (*code)->expr2->ts.type != BT_DERIVED)
    return;

  /* TODO: Handle more than one part array reference in assignments.  */
  depth = nonscalar_typebound_assign ((*code)->expr1->ts.u.derived,
				      (*code)->expr1->rank ? 1 : 0);
  if (depth > 1)
    {
      gfc_warning (0, "TODO: type-bound defined assignment(s) at %L not "
		   "done because multiple part array references would "
		   "occur in intermediate expressions.", &(*code)->loc);
      return;
    }

  component_assignment_level++;

  /* Create a temporary so that functions get called only once.  */
  if ((*code)->expr2->expr_type != EXPR_VARIABLE
      && (*code)->expr2->expr_type != EXPR_CONSTANT)
    {
      gfc_expr *tmp_expr;

      /* Assign the rhs to the temporary.  */
      tmp_expr = get_temp_from_expr ((*code)->expr1, ns);
      this_code = build_assignment (EXEC_ASSIGN,
				    tmp_expr, (*code)->expr2,
				    NULL, NULL, (*code)->loc);
      /* Add the code and substitute the rhs expression.  */
      add_code_to_chain (&this_code, &tmp_head, &tmp_tail);
      gfc_free_expr ((*code)->expr2);
      (*code)->expr2 = tmp_expr;
    }

  /* Do the intrinsic assignment.  This is not needed if the lhs is one
     of the temporaries generated here, since the intrinsic assignment
     to the final result already does this.  */
  if ((*code)->expr1->symtree->n.sym->name[2] != '@')
    {
      this_code = build_assignment (EXEC_ASSIGN,
				    (*code)->expr1, (*code)->expr2,
				    NULL, NULL, (*code)->loc);
      add_code_to_chain (&this_code, &head, &tail);
    }

  comp1 = (*code)->expr1->ts.u.derived->components;
  comp2 = (*code)->expr2->ts.u.derived->components;

  t1 = NULL;
  for (; comp1; comp1 = comp1->next, comp2 = comp2->next)
    {
      bool inout = false;

      /* The intrinsic assignment does the right thing for pointers
	 of all kinds and allocatable components.  */
      if (!gfc_bt_struct (comp1->ts.type)
	  || comp1->attr.pointer
	  || comp1->attr.allocatable
	  || comp1->attr.proc_pointer_comp
	  || comp1->attr.class_pointer
	  || comp1->attr.proc_pointer)
	continue;

      /* Make an assigment for this component.  */
      this_code = build_assignment (EXEC_ASSIGN,
				    (*code)->expr1, (*code)->expr2,
				    comp1, comp2, (*code)->loc);

      /* Convert the assignment if there is a defined assignment for
	 this type.  Otherwise, using the call from gfc_resolve_code,
	 recurse into its components.  */
      gfc_resolve_code (this_code, ns);

      if (this_code->op == EXEC_ASSIGN_CALL)
	{
	  gfc_formal_arglist *dummy_args;
	  gfc_symbol *rsym;
	  /* Check that there is a typebound defined assignment.  If not,
	     then this must be a module defined assignment.  We cannot
	     use the defined_assign_comp attribute here because it must
	     be this derived type that has the defined assignment and not
	     a parent type.  */
	  if (!(comp1->ts.u.derived->f2k_derived
		&& comp1->ts.u.derived->f2k_derived
					->tb_op[INTRINSIC_ASSIGN]))
	    {
	      gfc_free_statements (this_code);
	      this_code = NULL;
	      continue;
	    }

	  /* If the first argument of the subroutine has intent INOUT
	     a temporary must be generated and used instead.  */
	  rsym = this_code->resolved_sym;
	  dummy_args = gfc_sym_get_dummy_args (rsym);
	  if (dummy_args
	      && dummy_args->sym->attr.intent == INTENT_INOUT)
	    {
	      gfc_code *temp_code;
	      inout = true;

	      /* Build the temporary required for the assignment and put
		 it at the head of the generated code.  */
	      if (!t1)
		{
		  t1 = get_temp_from_expr ((*code)->expr1, ns);
		  temp_code = build_assignment (EXEC_ASSIGN,
						t1, (*code)->expr1,
				NULL, NULL, (*code)->loc);

		  /* For allocatable LHS, check whether it is allocated.  Note
		     that allocatable components with defined assignment are
		     not yet support.  See PR 57696.  */
		  if ((*code)->expr1->symtree->n.sym->attr.allocatable)
		    {
		      gfc_code *block;
		      gfc_expr *e =
			gfc_lval_expr_from_sym ((*code)->expr1->symtree->n.sym);
		      block = gfc_get_code (EXEC_IF);
		      block->block = gfc_get_code (EXEC_IF);
		      block->block->expr1
			  = gfc_build_intrinsic_call (ns,
				    GFC_ISYM_ALLOCATED, "allocated",
				    (*code)->loc, 1, e);
		      block->block->next = temp_code;
		      temp_code = block;
		    }
		  add_code_to_chain (&temp_code, &tmp_head, &tmp_tail);
		}

	      /* Replace the first actual arg with the component of the
		 temporary.  */
	      gfc_free_expr (this_code->ext.actual->expr);
	      this_code->ext.actual->expr = gfc_copy_expr (t1);
	      add_comp_ref (this_code->ext.actual->expr, comp1);

	      /* If the LHS variable is allocatable and wasn't allocated and
                 the temporary is allocatable, pointer assign the address of
                 the freshly allocated LHS to the temporary.  */
	      if ((*code)->expr1->symtree->n.sym->attr.allocatable
		  && gfc_expr_attr ((*code)->expr1).allocatable)
		{
		  gfc_code *block;
		  gfc_expr *cond;

		  cond = gfc_get_expr ();
		  cond->ts.type = BT_LOGICAL;
		  cond->ts.kind = gfc_default_logical_kind;
		  cond->expr_type = EXPR_OP;
		  cond->where = (*code)->loc;
		  cond->value.op.op = INTRINSIC_NOT;
		  cond->value.op.op1 = gfc_build_intrinsic_call (ns,
					  GFC_ISYM_ALLOCATED, "allocated",
					  (*code)->loc, 1, gfc_copy_expr (t1));
		  block = gfc_get_code (EXEC_IF);
		  block->block = gfc_get_code (EXEC_IF);
		  block->block->expr1 = cond;
		  block->block->next = build_assignment (EXEC_POINTER_ASSIGN,
					t1, (*code)->expr1,
					NULL, NULL, (*code)->loc);
		  add_code_to_chain (&block, &head, &tail);
		}
	    }
	}
      else if (this_code->op == EXEC_ASSIGN && !this_code->next)
	{
	  /* Don't add intrinsic assignments since they are already
	     effected by the intrinsic assignment of the structure.  */
	  gfc_free_statements (this_code);
	  this_code = NULL;
	  continue;
	}

      add_code_to_chain (&this_code, &head, &tail);

      if (t1 && inout)
	{
	  /* Transfer the value to the final result.  */
	  this_code = build_assignment (EXEC_ASSIGN,
					(*code)->expr1, t1,
					comp1, comp2, (*code)->loc);
	  add_code_to_chain (&this_code, &head, &tail);
	}
    }

  /* Put the temporary assignments at the top of the generated code.  */
  if (tmp_head && component_assignment_level == 1)
    {
      gfc_append_code (tmp_head, head);
      head = tmp_head;
      tmp_head = tmp_tail = NULL;
    }

  // If we did a pointer assignment - thus, we need to ensure that the LHS is
  // not accidentally deallocated. Hence, nullify t1.
  if (t1 && (*code)->expr1->symtree->n.sym->attr.allocatable
      && gfc_expr_attr ((*code)->expr1).allocatable)
    {
      gfc_code *block;
      gfc_expr *cond;
      gfc_expr *e;

      e = gfc_lval_expr_from_sym ((*code)->expr1->symtree->n.sym);
      cond = gfc_build_intrinsic_call (ns, GFC_ISYM_ASSOCIATED, "associated",
				       (*code)->loc, 2, gfc_copy_expr (t1), e);
      block = gfc_get_code (EXEC_IF);
      block->block = gfc_get_code (EXEC_IF);
      block->block->expr1 = cond;
      block->block->next = build_assignment (EXEC_POINTER_ASSIGN,
					t1, gfc_get_null_expr (&(*code)->loc),
					NULL, NULL, (*code)->loc);
      gfc_append_code (tail, block);
      tail = block;
    }

  /* Now attach the remaining code chain to the input code.  Step on
     to the end of the new code since resolution is complete.  */
  gcc_assert ((*code)->op == EXEC_ASSIGN);
  tail->next = (*code)->next;
  /* Overwrite 'code' because this would place the intrinsic assignment
     before the temporary for the lhs is created.  */
  gfc_free_expr ((*code)->expr1);
  gfc_free_expr ((*code)->expr2);
  **code = *head;
  if (head != tail)
    free (head);
  *code = tail;

  component_assignment_level--;
}


/* F2008: Pointer function assignments are of the form:
	ptr_fcn (args) = expr
   This function breaks these assignments into two statements:
	temporary_pointer => ptr_fcn(args)
	temporary_pointer = expr  */

static bool
resolve_ptr_fcn_assign (gfc_code **code, gfc_namespace *ns)
{
  gfc_expr *tmp_ptr_expr;
  gfc_code *this_code;
  gfc_component *comp;
  gfc_symbol *s;

  if ((*code)->expr1->expr_type != EXPR_FUNCTION)
    return false;

  /* Even if standard does not support this feature, continue to build
     the two statements to avoid upsetting frontend_passes.c.  */
  gfc_notify_std (GFC_STD_F2008, "Pointer procedure assignment at "
		  "%L", &(*code)->loc);

  comp = gfc_get_proc_ptr_comp ((*code)->expr1);

  if (comp)
    s = comp->ts.interface;
  else
    s = (*code)->expr1->symtree->n.sym;

  if (s == NULL || !s->result->attr.pointer)
    {
      gfc_error ("The function result on the lhs of the assignment at "
		 "%L must have the pointer attribute.",
		 &(*code)->expr1->where);
      (*code)->op = EXEC_NOP;
      return false;
    }

  tmp_ptr_expr = get_temp_from_expr ((*code)->expr2, ns);

  /* get_temp_from_expression is set up for ordinary assignments. To that
     end, where array bounds are not known, arrays are made allocatable.
     Change the temporary to a pointer here.  */
  tmp_ptr_expr->symtree->n.sym->attr.pointer = 1;
  tmp_ptr_expr->symtree->n.sym->attr.allocatable = 0;
  tmp_ptr_expr->where = (*code)->loc;

  this_code = build_assignment (EXEC_ASSIGN,
				tmp_ptr_expr, (*code)->expr2,
				NULL, NULL, (*code)->loc);
  this_code->next = (*code)->next;
  (*code)->next = this_code;
  (*code)->op = EXEC_POINTER_ASSIGN;
  (*code)->expr2 = (*code)->expr1;
  (*code)->expr1 = tmp_ptr_expr;

  return true;
}


/* Deferred character length assignments from an operator expression
   require a temporary because the character length of the lhs can
   change in the course of the assignment.  */

static bool
deferred_op_assign (gfc_code **code, gfc_namespace *ns)
{
  gfc_expr *tmp_expr;
  gfc_code *this_code;

  if (!((*code)->expr1->ts.type == BT_CHARACTER
	 && (*code)->expr1->ts.deferred && (*code)->expr1->rank
	 && (*code)->expr2->expr_type == EXPR_OP))
    return false;

  if (!gfc_check_dependency ((*code)->expr1, (*code)->expr2, 1))
    return false;

  tmp_expr = get_temp_from_expr ((*code)->expr1, ns);
  tmp_expr->where = (*code)->loc;

  /* A new charlen is required to ensure that the variable string
     length is different to that of the original lhs.  */
  tmp_expr->ts.u.cl = gfc_get_charlen();
  tmp_expr->symtree->n.sym->ts.u.cl = tmp_expr->ts.u.cl;
  tmp_expr->ts.u.cl->next = (*code)->expr2->ts.u.cl->next;
  (*code)->expr2->ts.u.cl->next = tmp_expr->ts.u.cl;

  tmp_expr->symtree->n.sym->ts.deferred = 1;

  this_code = build_assignment (EXEC_ASSIGN,
				(*code)->expr1,
				gfc_copy_expr (tmp_expr),
				NULL, NULL, (*code)->loc);

  (*code)->expr1 = tmp_expr;

  this_code->next = (*code)->next;
  (*code)->next = this_code;

  return true;
}


/* Given a block of code, recursively resolve everything pointed to by this
   code block.  */

void
gfc_resolve_code (gfc_code *code, gfc_namespace *ns)
{
  int omp_workshare_save;
  int forall_save, do_concurrent_save;
  code_stack frame;
  bool t;

  frame.prev = cs_base;
  frame.head = code;
  cs_base = &frame;

  find_reachable_labels (code);

  for (; code; code = code->next)
    {
      frame.current = code;
      forall_save = forall_flag;
      do_concurrent_save = gfc_do_concurrent_flag;

      if (code->op == EXEC_FORALL)
	{
	  forall_flag = 1;
	  gfc_resolve_forall (code, ns, forall_save);
	  forall_flag = 2;
	}
      else if (code->block)
	{
	  omp_workshare_save = -1;
	  switch (code->op)
	    {
	    case EXEC_OACC_PARALLEL_LOOP:
	    case EXEC_OACC_PARALLEL:
	    case EXEC_OACC_KERNELS_LOOP:
	    case EXEC_OACC_KERNELS:
	    case EXEC_OACC_DATA:
	    case EXEC_OACC_HOST_DATA:
	    case EXEC_OACC_LOOP:
	      gfc_resolve_oacc_blocks (code, ns);
	      break;
	    case EXEC_OMP_PARALLEL_WORKSHARE:
	      omp_workshare_save = omp_workshare_flag;
	      omp_workshare_flag = 1;
	      gfc_resolve_omp_parallel_blocks (code, ns);
	      break;
	    case EXEC_OMP_PARALLEL:
	    case EXEC_OMP_PARALLEL_DO:
	    case EXEC_OMP_PARALLEL_DO_SIMD:
	    case EXEC_OMP_PARALLEL_SECTIONS:
	    case EXEC_OMP_TARGET_PARALLEL:
	    case EXEC_OMP_TARGET_PARALLEL_DO:
	    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
	    case EXEC_OMP_TARGET_TEAMS:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
	    case EXEC_OMP_TASK:
	    case EXEC_OMP_TEAMS:
	    case EXEC_OMP_TEAMS_DISTRIBUTE:
	    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
	    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
	      omp_workshare_save = omp_workshare_flag;
	      omp_workshare_flag = 0;
	      gfc_resolve_omp_parallel_blocks (code, ns);
	      break;
	    case EXEC_OMP_DISTRIBUTE:
	    case EXEC_OMP_DISTRIBUTE_SIMD:
	    case EXEC_OMP_DO:
	    case EXEC_OMP_DO_SIMD:
	    case EXEC_OMP_SIMD:
	    case EXEC_OMP_TARGET_SIMD:
	    case EXEC_OMP_TASKLOOP:
	    case EXEC_OMP_TASKLOOP_SIMD:
	      gfc_resolve_omp_do_blocks (code, ns);
	      break;
	    case EXEC_SELECT_TYPE:
	      /* Blocks are handled in resolve_select_type because we have
		 to transform the SELECT TYPE into ASSOCIATE first.  */
	      break;
            case EXEC_DO_CONCURRENT:
	      gfc_do_concurrent_flag = 1;
	      gfc_resolve_blocks (code->block, ns);
	      gfc_do_concurrent_flag = 2;
	      break;
	    case EXEC_OMP_WORKSHARE:
	      omp_workshare_save = omp_workshare_flag;
	      omp_workshare_flag = 1;
	      /* FALL THROUGH */
	    default:
	      gfc_resolve_blocks (code->block, ns);
	      break;
	    }

	  if (omp_workshare_save != -1)
	    omp_workshare_flag = omp_workshare_save;
	}
start:
      t = true;
      if (code->op != EXEC_COMPCALL && code->op != EXEC_CALL_PPC)
	t = gfc_resolve_expr (code->expr1);
      forall_flag = forall_save;
      gfc_do_concurrent_flag = do_concurrent_save;

      if (!gfc_resolve_expr (code->expr2))
	t = false;

      if (code->op == EXEC_ALLOCATE
	  && !gfc_resolve_expr (code->expr3))
	t = false;

      switch (code->op)
	{
	case EXEC_NOP:
	case EXEC_END_BLOCK:
	case EXEC_END_NESTED_BLOCK:
	case EXEC_CYCLE:
	case EXEC_PAUSE:
	case EXEC_STOP:
	case EXEC_ERROR_STOP:
	case EXEC_EXIT:
	case EXEC_CONTINUE:
	case EXEC_DT_END:
	case EXEC_ASSIGN_CALL:
	  break;

	case EXEC_CRITICAL:
	  resolve_critical (code);
	  break;

	case EXEC_SYNC_ALL:
	case EXEC_SYNC_IMAGES:
	case EXEC_SYNC_MEMORY:
	  resolve_sync (code);
	  break;

	case EXEC_LOCK:
	case EXEC_UNLOCK:
	case EXEC_EVENT_POST:
	case EXEC_EVENT_WAIT:
	  resolve_lock_unlock_event (code);
	  break;

	case EXEC_FAIL_IMAGE:
	  break;

	case EXEC_ENTRY:
	  /* Keep track of which entry we are up to.  */
	  current_entry_id = code->ext.entry->id;
	  break;

	case EXEC_WHERE:
	  resolve_where (code, NULL);
	  break;

	case EXEC_GOTO:
	  if (code->expr1 != NULL)
	    {
	      if (code->expr1->ts.type != BT_INTEGER)
		gfc_error ("ASSIGNED GOTO statement at %L requires an "
			   "INTEGER variable", &code->expr1->where);
	      else if (code->expr1->symtree->n.sym->attr.assign != 1)
		gfc_error ("Variable %qs has not been assigned a target "
			   "label at %L", code->expr1->symtree->n.sym->name,
			   &code->expr1->where);
	    }
	  else
	    resolve_branch (code->label1, code);
	  break;

	case EXEC_RETURN:
	  if (code->expr1 != NULL
		&& (code->expr1->ts.type != BT_INTEGER || code->expr1->rank))
	    gfc_error ("Alternate RETURN statement at %L requires a SCALAR-"
		       "INTEGER return specifier", &code->expr1->where);
	  break;

	case EXEC_INIT_ASSIGN:
	case EXEC_END_PROCEDURE:
	  break;

	case EXEC_ASSIGN:
	  if (!t)
	    break;

	  /* Remove a GFC_ISYM_CAF_GET inserted for a coindexed variable on
	     the LHS.  */
	  if (code->expr1->expr_type == EXPR_FUNCTION
	      && code->expr1->value.function.isym
	      && code->expr1->value.function.isym->id == GFC_ISYM_CAF_GET)
	    remove_caf_get_intrinsic (code->expr1);

	  /* If this is a pointer function in an lvalue variable context,
	     the new code will have to be resolved afresh. This is also the
	     case with an error, where the code is transformed into NOP to
	     prevent ICEs downstream.  */
	  if (resolve_ptr_fcn_assign (&code, ns)
	      || code->op == EXEC_NOP)
	    goto start;

	  if (!gfc_check_vardef_context (code->expr1, false, false, false,
					 _("assignment")))
	    break;

	  if (resolve_ordinary_assign (code, ns))
	    {
	      if (code->op == EXEC_COMPCALL)
		goto compcall;
	      else
		goto call;
	    }

	  /* Check for dependencies in deferred character length array
	     assignments and generate a temporary, if necessary.  */
	  if (code->op == EXEC_ASSIGN && deferred_op_assign (&code, ns))
	    break;

	  /* F03 7.4.1.3 for non-allocatable, non-pointer components.  */
	  if (code->op != EXEC_CALL && code->expr1->ts.type == BT_DERIVED
	      && code->expr1->ts.u.derived
	      && code->expr1->ts.u.derived->attr.defined_assign_comp)
	    generate_component_assignments (&code, ns);

	  break;

	case EXEC_LABEL_ASSIGN:
	  if (code->label1->defined == ST_LABEL_UNKNOWN)
	    gfc_error ("Label %d referenced at %L is never defined",
		       code->label1->value, &code->label1->where);
	  if (t
	      && (code->expr1->expr_type != EXPR_VARIABLE
		  || code->expr1->symtree->n.sym->ts.type != BT_INTEGER
		  || code->expr1->symtree->n.sym->ts.kind
		     != gfc_default_integer_kind
		  || code->expr1->symtree->n.sym->as != NULL))
	    gfc_error ("ASSIGN statement at %L requires a scalar "
		       "default INTEGER variable", &code->expr1->where);
	  break;

	case EXEC_POINTER_ASSIGN:
	  {
	    gfc_expr* e;

	    if (!t)
	      break;

	    /* This is both a variable definition and pointer assignment
	       context, so check both of them.  For rank remapping, a final
	       array ref may be present on the LHS and fool gfc_expr_attr
	       used in gfc_check_vardef_context.  Remove it.  */
	    e = remove_last_array_ref (code->expr1);
	    t = gfc_check_vardef_context (e, true, false, false,
					  _("pointer assignment"));
	    if (t)
	      t = gfc_check_vardef_context (e, false, false, false,
					    _("pointer assignment"));
	    gfc_free_expr (e);
	    if (!t)
	      break;

	    gfc_check_pointer_assign (code->expr1, code->expr2);

	    /* Assigning a class object always is a regular assign.  */
	    if (code->expr2->ts.type == BT_CLASS
		&& code->expr1->ts.type == BT_CLASS
		&& !CLASS_DATA (code->expr2)->attr.dimension
		&& !(gfc_expr_attr (code->expr1).proc_pointer
		     && code->expr2->expr_type == EXPR_VARIABLE
		     && code->expr2->symtree->n.sym->attr.flavor
			== FL_PROCEDURE))
	      code->op = EXEC_ASSIGN;
	    break;
	  }

	case EXEC_ARITHMETIC_IF:
	  {
	    gfc_expr *e = code->expr1;

	    gfc_resolve_expr (e);
	    if (e->expr_type == EXPR_NULL)
	      gfc_error ("Invalid NULL at %L", &e->where);

	    if (t && (e->rank > 0
		      || !(e->ts.type == BT_REAL || e->ts.type == BT_INTEGER)))
	      gfc_error ("Arithmetic IF statement at %L requires a scalar "
			 "REAL or INTEGER expression", &e->where);

	    resolve_branch (code->label1, code);
	    resolve_branch (code->label2, code);
	    resolve_branch (code->label3, code);
	  }
	  break;

	case EXEC_IF:
	  if (t && code->expr1 != NULL
	      && (code->expr1->ts.type != BT_LOGICAL
		  || code->expr1->rank != 0))
	    gfc_error ("IF clause at %L requires a scalar LOGICAL expression",
		       &code->expr1->where);
	  break;

	case EXEC_CALL:
	call:
	  resolve_call (code);
	  break;

	case EXEC_COMPCALL:
	compcall:
	  resolve_typebound_subroutine (code);
	  break;

	case EXEC_CALL_PPC:
	  resolve_ppc_call (code);
	  break;

	case EXEC_SELECT:
	  /* Select is complicated. Also, a SELECT construct could be
	     a transformed computed GOTO.  */
	  resolve_select (code, false);
	  break;

	case EXEC_SELECT_TYPE:
	  resolve_select_type (code, ns);
	  break;

	case EXEC_BLOCK:
	  resolve_block_construct (code);
	  break;

	case EXEC_DO:
	  if (code->ext.iterator != NULL)
	    {
	      gfc_iterator *iter = code->ext.iterator;
	      if (gfc_resolve_iterator (iter, true, false))
		gfc_resolve_do_iterator (code, iter->var->symtree->n.sym);
	    }
	  break;

	case EXEC_DO_WHILE:
	  if (code->expr1 == NULL)
	    gfc_internal_error ("gfc_resolve_code(): No expression on "
				"DO WHILE");
	  if (t
	      && (code->expr1->rank != 0
		  || code->expr1->ts.type != BT_LOGICAL))
	    gfc_error ("Exit condition of DO WHILE loop at %L must be "
		       "a scalar LOGICAL expression", &code->expr1->where);
	  break;

	case EXEC_ALLOCATE:
	  if (t)
	    resolve_allocate_deallocate (code, "ALLOCATE");

	  break;

	case EXEC_DEALLOCATE:
	  if (t)
	    resolve_allocate_deallocate (code, "DEALLOCATE");

	  break;

	case EXEC_OPEN:
	  if (!gfc_resolve_open (code->ext.open))
	    break;

	  resolve_branch (code->ext.open->err, code);
	  break;

	case EXEC_CLOSE:
	  if (!gfc_resolve_close (code->ext.close))
	    break;

	  resolve_branch (code->ext.close->err, code);
	  break;

	case EXEC_BACKSPACE:
	case EXEC_ENDFILE:
	case EXEC_REWIND:
	case EXEC_FLUSH:
	  if (!gfc_resolve_filepos (code->ext.filepos))
	    break;

	  resolve_branch (code->ext.filepos->err, code);
	  break;

	case EXEC_INQUIRE:
	  if (!gfc_resolve_inquire (code->ext.inquire))
	      break;

	  resolve_branch (code->ext.inquire->err, code);
	  break;

	case EXEC_IOLENGTH:
	  gcc_assert (code->ext.inquire != NULL);
	  if (!gfc_resolve_inquire (code->ext.inquire))
	    break;

	  resolve_branch (code->ext.inquire->err, code);
	  break;

	case EXEC_WAIT:
	  if (!gfc_resolve_wait (code->ext.wait))
	    break;

	  resolve_branch (code->ext.wait->err, code);
	  resolve_branch (code->ext.wait->end, code);
	  resolve_branch (code->ext.wait->eor, code);
	  break;

	case EXEC_READ:
	case EXEC_WRITE:
	  if (!gfc_resolve_dt (code->ext.dt, &code->loc))
	    break;

	  resolve_branch (code->ext.dt->err, code);
	  resolve_branch (code->ext.dt->end, code);
	  resolve_branch (code->ext.dt->eor, code);
	  break;

	case EXEC_TRANSFER:
	  resolve_transfer (code);
	  break;

	case EXEC_DO_CONCURRENT:
	case EXEC_FORALL:
	  resolve_forall_iterators (code->ext.forall_iterator);

	  if (code->expr1 != NULL
	      && (code->expr1->ts.type != BT_LOGICAL || code->expr1->rank))
	    gfc_error ("FORALL mask clause at %L requires a scalar LOGICAL "
		       "expression", &code->expr1->where);
	  break;

	case EXEC_OACC_PARALLEL_LOOP:
	case EXEC_OACC_PARALLEL:
	case EXEC_OACC_KERNELS_LOOP:
	case EXEC_OACC_KERNELS:
	case EXEC_OACC_DATA:
	case EXEC_OACC_HOST_DATA:
	case EXEC_OACC_LOOP:
	case EXEC_OACC_UPDATE:
	case EXEC_OACC_WAIT:
	case EXEC_OACC_CACHE:
	case EXEC_OACC_ENTER_DATA:
	case EXEC_OACC_EXIT_DATA:
	case EXEC_OACC_ATOMIC:
	case EXEC_OACC_DECLARE:
	  gfc_resolve_oacc_directive (code, ns);
	  break;

	case EXEC_OMP_ATOMIC:
	case EXEC_OMP_BARRIER:
	case EXEC_OMP_CANCEL:
	case EXEC_OMP_CANCELLATION_POINT:
	case EXEC_OMP_CRITICAL:
	case EXEC_OMP_FLUSH:
	case EXEC_OMP_DISTRIBUTE:
	case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
	case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
	case EXEC_OMP_DISTRIBUTE_SIMD:
	case EXEC_OMP_DO:
	case EXEC_OMP_DO_SIMD:
	case EXEC_OMP_MASTER:
	case EXEC_OMP_ORDERED:
	case EXEC_OMP_SECTIONS:
	case EXEC_OMP_SIMD:
	case EXEC_OMP_SINGLE:
	case EXEC_OMP_TARGET:
	case EXEC_OMP_TARGET_DATA:
	case EXEC_OMP_TARGET_ENTER_DATA:
	case EXEC_OMP_TARGET_EXIT_DATA:
	case EXEC_OMP_TARGET_PARALLEL:
	case EXEC_OMP_TARGET_PARALLEL_DO:
	case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
	case EXEC_OMP_TARGET_SIMD:
	case EXEC_OMP_TARGET_TEAMS:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
	case EXEC_OMP_TARGET_UPDATE:
	case EXEC_OMP_TASK:
	case EXEC_OMP_TASKGROUP:
	case EXEC_OMP_TASKLOOP:
	case EXEC_OMP_TASKLOOP_SIMD:
	case EXEC_OMP_TASKWAIT:
	case EXEC_OMP_TASKYIELD:
	case EXEC_OMP_TEAMS:
	case EXEC_OMP_TEAMS_DISTRIBUTE:
	case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
	case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
	case EXEC_OMP_WORKSHARE:
	  gfc_resolve_omp_directive (code, ns);
	  break;

	case EXEC_OMP_PARALLEL:
	case EXEC_OMP_PARALLEL_DO:
	case EXEC_OMP_PARALLEL_DO_SIMD:
	case EXEC_OMP_PARALLEL_SECTIONS:
	case EXEC_OMP_PARALLEL_WORKSHARE:
	  omp_workshare_save = omp_workshare_flag;
	  omp_workshare_flag = 0;
	  gfc_resolve_omp_directive (code, ns);
	  omp_workshare_flag = omp_workshare_save;
	  break;

	default:
	  gfc_internal_error ("gfc_resolve_code(): Bad statement code");
	}
    }

  cs_base = frame.prev;
}


/* Resolve initial values and make sure they are compatible with
   the variable.  */

static void
resolve_values (gfc_symbol *sym)
{
  bool t;

  if (sym->value == NULL)
    return;

  if (sym->value->expr_type == EXPR_STRUCTURE)
    t= resolve_structure_cons (sym->value, 1);
  else
    t = gfc_resolve_expr (sym->value);

  if (!t)
    return;

  gfc_check_assign_symbol (sym, NULL, sym->value);
}


/* Verify any BIND(C) derived types in the namespace so we can report errors
   for them once, rather than for each variable declared of that type.  */

static void
resolve_bind_c_derived_types (gfc_symbol *derived_sym)
{
  if (derived_sym != NULL && derived_sym->attr.flavor == FL_DERIVED
      && derived_sym->attr.is_bind_c == 1)
    verify_bind_c_derived_type (derived_sym);

  return;
}


/* Check the interfaces of DTIO procedures associated with derived
   type 'sym'.  These procedures can either have typebound bindings or
   can appear in DTIO generic interfaces.  */

static void
gfc_verify_DTIO_procedures (gfc_symbol *sym)
{
  if (!sym || sym->attr.flavor != FL_DERIVED)
    return;

  gfc_check_dtio_interfaces (sym);

  return;
}

/* Verify that any binding labels used in a given namespace do not collide
   with the names or binding labels of any global symbols.  Multiple INTERFACE
   for the same procedure are permitted.  */

static void
gfc_verify_binding_labels (gfc_symbol *sym)
{
  gfc_gsymbol *gsym;
  const char *module;

  if (!sym || !sym->attr.is_bind_c || sym->attr.is_iso_c
      || sym->attr.flavor == FL_DERIVED || !sym->binding_label)
    return;

  gsym = gfc_find_gsymbol (gfc_gsym_root, sym->binding_label);

  if (sym->module)
    module = sym->module;
  else if (sym->ns && sym->ns->proc_name
	   && sym->ns->proc_name->attr.flavor == FL_MODULE)
    module = sym->ns->proc_name->name;
  else if (sym->ns && sym->ns->parent
	   && sym->ns && sym->ns->parent->proc_name
	   && sym->ns->parent->proc_name->attr.flavor == FL_MODULE)
    module = sym->ns->parent->proc_name->name;
  else
    module = NULL;

  if (!gsym
      || (!gsym->defined
	  && (gsym->type == GSYM_FUNCTION || gsym->type == GSYM_SUBROUTINE)))
    {
      if (!gsym)
	gsym = gfc_get_gsymbol (sym->binding_label);
      gsym->where = sym->declared_at;
      gsym->sym_name = sym->name;
      gsym->binding_label = sym->binding_label;
      gsym->ns = sym->ns;
      gsym->mod_name = module;
      if (sym->attr.function)
        gsym->type = GSYM_FUNCTION;
      else if (sym->attr.subroutine)
	gsym->type = GSYM_SUBROUTINE;
      /* Mark as variable/procedure as defined, unless its an INTERFACE.  */
      gsym->defined = sym->attr.if_source != IFSRC_IFBODY;
      return;
    }

  if (sym->attr.flavor == FL_VARIABLE && gsym->type != GSYM_UNKNOWN)
    {
      gfc_error ("Variable %s with binding label %s at %L uses the same global "
		 "identifier as entity at %L", sym->name,
		 sym->binding_label, &sym->declared_at, &gsym->where);
      /* Clear the binding label to prevent checking multiple times.  */
      sym->binding_label = NULL;

    }
  else if (sym->attr.flavor == FL_VARIABLE && module
	   && (strcmp (module, gsym->mod_name) != 0
	       || strcmp (sym->name, gsym->sym_name) != 0))
    {
      /* This can only happen if the variable is defined in a module - if it
	 isn't the same module, reject it.  */
      gfc_error ("Variable %s from module %s with binding label %s at %L uses "
		   "the same global identifier as entity at %L from module %s",
		 sym->name, module, sym->binding_label,
		 &sym->declared_at, &gsym->where, gsym->mod_name);
      sym->binding_label = NULL;
    }
  else if ((sym->attr.function || sym->attr.subroutine)
	   && ((gsym->type != GSYM_SUBROUTINE && gsym->type != GSYM_FUNCTION)
	       || (gsym->defined && sym->attr.if_source != IFSRC_IFBODY))
	   && sym != gsym->ns->proc_name
	   && (module != gsym->mod_name
	       || strcmp (gsym->sym_name, sym->name) != 0
	       || (module && strcmp (module, gsym->mod_name) != 0)))
    {
      /* Print an error if the procedure is defined multiple times; we have to
	 exclude references to the same procedure via module association or
	 multiple checks for the same procedure.  */
      gfc_error ("Procedure %s with binding label %s at %L uses the same "
		 "global identifier as entity at %L", sym->name,
		 sym->binding_label, &sym->declared_at, &gsym->where);
      sym->binding_label = NULL;
    }
}


/* Resolve an index expression.  */

static bool
resolve_index_expr (gfc_expr *e)
{
  if (!gfc_resolve_expr (e))
    return false;

  if (!gfc_simplify_expr (e, 0))
    return false;

  if (!gfc_specification_expr (e))
    return false;

  return true;
}


/* Resolve a charlen structure.  */

static bool
resolve_charlen (gfc_charlen *cl)
{
  int i, k;
  bool saved_specification_expr;

  if (cl->resolved)
    return true;

  cl->resolved = 1;
  saved_specification_expr = specification_expr;
  specification_expr = true;

  if (cl->length_from_typespec)
    {
      if (!gfc_resolve_expr (cl->length))
	{
	  specification_expr = saved_specification_expr;
	  return false;
	}

      if (!gfc_simplify_expr (cl->length, 0))
	{
	  specification_expr = saved_specification_expr;
	  return false;
	}
    }
  else
    {

      if (!resolve_index_expr (cl->length))
	{
	  specification_expr = saved_specification_expr;
	  return false;
	}
    }

  /* F2008, 4.4.3.2:  If the character length parameter value evaluates to
     a negative value, the length of character entities declared is zero.  */
  if (cl->length && !gfc_extract_int (cl->length, &i) && i < 0)
    gfc_replace_expr (cl->length,
		      gfc_get_int_expr (gfc_default_integer_kind, NULL, 0));

  /* Check that the character length is not too large.  */
  k = gfc_validate_kind (BT_INTEGER, gfc_charlen_int_kind, false);
  if (cl->length && cl->length->expr_type == EXPR_CONSTANT
      && cl->length->ts.type == BT_INTEGER
      && mpz_cmp (cl->length->value.integer, gfc_integer_kinds[k].huge) > 0)
    {
      gfc_error ("String length at %L is too large", &cl->length->where);
      specification_expr = saved_specification_expr;
      return false;
    }

  specification_expr = saved_specification_expr;
  return true;
}


/* Test for non-constant shape arrays.  */

static bool
is_non_constant_shape_array (gfc_symbol *sym)
{
  gfc_expr *e;
  int i;
  bool not_constant;

  not_constant = false;
  if (sym->as != NULL)
    {
      /* Unfortunately, !gfc_is_compile_time_shape hits a legal case that
	 has not been simplified; parameter array references.  Do the
	 simplification now.  */
      for (i = 0; i < sym->as->rank + sym->as->corank; i++)
	{
	  e = sym->as->lower[i];
	  if (e && (!resolve_index_expr(e)
		    || !gfc_is_constant_expr (e)))
	    not_constant = true;
	  e = sym->as->upper[i];
	  if (e && (!resolve_index_expr(e)
		    || !gfc_is_constant_expr (e)))
	    not_constant = true;
	}
    }
  return not_constant;
}

/* Given a symbol and an initialization expression, add code to initialize
   the symbol to the function entry.  */
static void
build_init_assign (gfc_symbol *sym, gfc_expr *init)
{
  gfc_expr *lval;
  gfc_code *init_st;
  gfc_namespace *ns = sym->ns;

  /* Search for the function namespace if this is a contained
     function without an explicit result.  */
  if (sym->attr.function && sym == sym->result
      && sym->name != sym->ns->proc_name->name)
    {
      ns = ns->contained;
      for (;ns; ns = ns->sibling)
	if (strcmp (ns->proc_name->name, sym->name) == 0)
	  break;
    }

  if (ns == NULL)
    {
      gfc_free_expr (init);
      return;
    }

  /* Build an l-value expression for the result.  */
  lval = gfc_lval_expr_from_sym (sym);

  /* Add the code at scope entry.  */
  init_st = gfc_get_code (EXEC_INIT_ASSIGN);
  init_st->next = ns->code;
  ns->code = init_st;

  /* Assign the default initializer to the l-value.  */
  init_st->loc = sym->declared_at;
  init_st->expr1 = lval;
  init_st->expr2 = init;
}


/* Whether or not we can generate a default initializer for a symbol.  */

static bool
can_generate_init (gfc_symbol *sym)
{
  symbol_attribute *a;
  if (!sym)
    return false;
  a = &sym->attr;

  /* These symbols should never have a default initialization.  */
  return !(
       a->allocatable
    || a->external
    || a->pointer
    || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
        && (CLASS_DATA (sym)->attr.class_pointer
            || CLASS_DATA (sym)->attr.proc_pointer))
    || a->in_equivalence
    || a->in_common
    || a->data
    || sym->module
    || a->cray_pointee
    || a->cray_pointer
    || sym->assoc
    || (!a->referenced && !a->result)
    || (a->dummy && a->intent != INTENT_OUT)
    || (a->function && sym != sym->result)
  );
}


/* Assign the default initializer to a derived type variable or result.  */

static void
apply_default_init (gfc_symbol *sym)
{
  gfc_expr *init = NULL;

  if (sym->attr.flavor != FL_VARIABLE && !sym->attr.function)
    return;

  if (sym->ts.type == BT_DERIVED && sym->ts.u.derived)
    init = gfc_generate_initializer (&sym->ts, can_generate_init (sym));

  if (init == NULL && sym->ts.type != BT_CLASS)
    return;

  build_init_assign (sym, init);
  sym->attr.referenced = 1;
}


/* Build an initializer for a local. Returns null if the symbol should not have
   a default initialization.  */

static gfc_expr *
build_default_init_expr (gfc_symbol *sym)
{
  /* These symbols should never have a default initialization.  */
  if (sym->attr.allocatable
      || sym->attr.external
      || sym->attr.dummy
      || sym->attr.pointer
      || sym->attr.in_equivalence
      || sym->attr.in_common
      || sym->attr.data
      || sym->module
      || sym->attr.cray_pointee
      || sym->attr.cray_pointer
      || sym->assoc)
    return NULL;

  /* Get the appropriate init expression.  */
  return gfc_build_default_init_expr (&sym->ts, &sym->declared_at);
}

/* Add an initialization expression to a local variable.  */
static void
apply_default_init_local (gfc_symbol *sym)
{
  gfc_expr *init = NULL;

  /* The symbol should be a variable or a function return value.  */
  if ((sym->attr.flavor != FL_VARIABLE && !sym->attr.function)
      || (sym->attr.function && sym->result != sym))
    return;

  /* Try to build the initializer expression.  If we can't initialize
     this symbol, then init will be NULL.  */
  init = build_default_init_expr (sym);
  if (init == NULL)
    return;

  /* For saved variables, we don't want to add an initializer at function
     entry, so we just add a static initializer. Note that automatic variables
     are stack allocated even with -fno-automatic; we have also to exclude
     result variable, which are also nonstatic.  */
  if (!sym->attr.automatic
      && (sym->attr.save || sym->ns->save_all
	  || (flag_max_stack_var_size == 0 && !sym->attr.result
	      && (sym->ns->proc_name && !sym->ns->proc_name->attr.recursive)
	      && (!sym->attr.dimension || !is_non_constant_shape_array (sym)))))
    {
      /* Don't clobber an existing initializer!  */
      gcc_assert (sym->value == NULL);
      sym->value = init;
      return;
    }

  build_init_assign (sym, init);
}


/* Resolution of common features of flavors variable and procedure.  */

static bool
resolve_fl_var_and_proc (gfc_symbol *sym, int mp_flag)
{
  gfc_array_spec *as;

  if (sym->ts.type == BT_CLASS && sym->attr.class_ok)
    as = CLASS_DATA (sym)->as;
  else
    as = sym->as;

  /* Constraints on deferred shape variable.  */
  if (as == NULL || as->type != AS_DEFERRED)
    {
      bool pointer, allocatable, dimension;

      if (sym->ts.type == BT_CLASS && sym->attr.class_ok)
	{
	  pointer = CLASS_DATA (sym)->attr.class_pointer;
	  allocatable = CLASS_DATA (sym)->attr.allocatable;
	  dimension = CLASS_DATA (sym)->attr.dimension;
	}
      else
	{
	  pointer = sym->attr.pointer && !sym->attr.select_type_temporary;
	  allocatable = sym->attr.allocatable;
	  dimension = sym->attr.dimension;
	}

      if (allocatable)
	{
	  if (dimension && as->type != AS_ASSUMED_RANK)
	    {
	      gfc_error ("Allocatable array %qs at %L must have a deferred "
			 "shape or assumed rank", sym->name, &sym->declared_at);
	      return false;
	    }
	  else if (!gfc_notify_std (GFC_STD_F2003, "Scalar object "
				    "%qs at %L may not be ALLOCATABLE",
				    sym->name, &sym->declared_at))
	    return false;
	}

      if (pointer && dimension && as->type != AS_ASSUMED_RANK)
	{
	  gfc_error ("Array pointer %qs at %L must have a deferred shape or "
		     "assumed rank", sym->name, &sym->declared_at);
	  return false;
	}
    }
  else
    {
      if (!mp_flag && !sym->attr.allocatable && !sym->attr.pointer
	  && sym->ts.type != BT_CLASS && !sym->assoc)
	{
	  gfc_error ("Array %qs at %L cannot have a deferred shape",
		     sym->name, &sym->declared_at);
	  return false;
	 }
    }

  /* Constraints on polymorphic variables.  */
  if (sym->ts.type == BT_CLASS && !(sym->result && sym->result != sym))
    {
      /* F03:C502.  */
      if (sym->attr.class_ok
	  && !sym->attr.select_type_temporary
	  && !UNLIMITED_POLY (sym)
	  && !gfc_type_is_extensible (CLASS_DATA (sym)->ts.u.derived))
	{
	  gfc_error ("Type %qs of CLASS variable %qs at %L is not extensible",
		     CLASS_DATA (sym)->ts.u.derived->name, sym->name,
		     &sym->declared_at);
	  return false;
	}

      /* F03:C509.  */
      /* Assume that use associated symbols were checked in the module ns.
	 Class-variables that are associate-names are also something special
	 and excepted from the test.  */
      if (!sym->attr.class_ok && !sym->attr.use_assoc && !sym->assoc)
	{
	  gfc_error ("CLASS variable %qs at %L must be dummy, allocatable "
		     "or pointer", sym->name, &sym->declared_at);
	  return false;
	}
    }

  return true;
}


/* Additional checks for symbols with flavor variable and derived
   type.  To be called from resolve_fl_variable.  */

static bool
resolve_fl_variable_derived (gfc_symbol *sym, int no_init_flag)
{
  gcc_assert (sym->ts.type == BT_DERIVED || sym->ts.type == BT_CLASS);

  /* Check to see if a derived type is blocked from being host
     associated by the presence of another class I symbol in the same
     namespace.  14.6.1.3 of the standard and the discussion on
     comp.lang.fortran.  */
  if (sym->ns != sym->ts.u.derived->ns
      && sym->ns->proc_name->attr.if_source != IFSRC_IFBODY)
    {
      gfc_symbol *s;
      gfc_find_symbol (sym->ts.u.derived->name, sym->ns, 0, &s);
      if (s && s->attr.generic)
	s = gfc_find_dt_in_generic (s);
      if (s && !gfc_fl_struct (s->attr.flavor))
	{
	  gfc_error ("The type %qs cannot be host associated at %L "
		     "because it is blocked by an incompatible object "
		     "of the same name declared at %L",
		     sym->ts.u.derived->name, &sym->declared_at,
		     &s->declared_at);
	  return false;
	}
    }

  /* 4th constraint in section 11.3: "If an object of a type for which
     component-initialization is specified (R429) appears in the
     specification-part of a module and does not have the ALLOCATABLE
     or POINTER attribute, the object shall have the SAVE attribute."

     The check for initializers is performed with
     gfc_has_default_initializer because gfc_default_initializer generates
     a hidden default for allocatable components.  */
  if (!(sym->value || no_init_flag) && sym->ns->proc_name
      && sym->ns->proc_name->attr.flavor == FL_MODULE
      && !(sym->ns->save_all && !sym->attr.automatic) && !sym->attr.save
      && !sym->attr.pointer && !sym->attr.allocatable
      && gfc_has_default_initializer (sym->ts.u.derived)
      && !gfc_notify_std (GFC_STD_F2008, "Implied SAVE for module variable "
			  "%qs at %L, needed due to the default "
			  "initialization", sym->name, &sym->declared_at))
    return false;

  /* Assign default initializer.  */
  if (!(sym->value || sym->attr.pointer || sym->attr.allocatable)
      && (!no_init_flag || sym->attr.intent == INTENT_OUT))
    sym->value = gfc_generate_initializer (&sym->ts, can_generate_init (sym));

  return true;
}


/* F2008, C402 (R401):  A colon shall not be used as a type-param-value
   except in the declaration of an entity or component that has the POINTER
   or ALLOCATABLE attribute.  */

static bool
deferred_requirements (gfc_symbol *sym)
{
  if (sym->ts.deferred
      && !(sym->attr.pointer
	   || sym->attr.allocatable
	   || sym->attr.associate_var
	   || sym->attr.omp_udr_artificial_var))
    {
      gfc_error ("Entity %qs at %L has a deferred type parameter and "
		 "requires either the POINTER or ALLOCATABLE attribute",
		 sym->name, &sym->declared_at);
      return false;
    }
  return true;
}


/* Resolve symbols with flavor variable.  */

static bool
resolve_fl_variable (gfc_symbol *sym, int mp_flag)
{
  int no_init_flag, automatic_flag;
  gfc_expr *e;
  const char *auto_save_msg;
  bool saved_specification_expr;

  auto_save_msg = "Automatic object %qs at %L cannot have the "
		  "SAVE attribute";

  if (!resolve_fl_var_and_proc (sym, mp_flag))
    return false;

  /* Set this flag to check that variables are parameters of all entries.
     This check is effected by the call to gfc_resolve_expr through
     is_non_constant_shape_array.  */
  saved_specification_expr = specification_expr;
  specification_expr = true;

  if (sym->ns->proc_name
      && (sym->ns->proc_name->attr.flavor == FL_MODULE
	  || sym->ns->proc_name->attr.is_main_program)
      && !sym->attr.use_assoc
      && !sym->attr.allocatable
      && !sym->attr.pointer
      && is_non_constant_shape_array (sym))
    {
      /* F08:C541. The shape of an array defined in a main program or module
       * needs to be constant.  */
      gfc_error ("The module or main program array %qs at %L must "
		 "have constant shape", sym->name, &sym->declared_at);
      specification_expr = saved_specification_expr;
      return false;
    }

  /* Constraints on deferred type parameter.  */
  if (!deferred_requirements (sym))
    return false;

  if (sym->ts.type == BT_CHARACTER && !sym->attr.associate_var)
    {
      /* Make sure that character string variables with assumed length are
	 dummy arguments.  */
      e = sym->ts.u.cl->length;
      if (e == NULL && !sym->attr.dummy && !sym->attr.result
	  && !sym->ts.deferred && !sym->attr.select_type_temporary
	  && !sym->attr.omp_udr_artificial_var)
	{
	  gfc_error ("Entity with assumed character length at %L must be a "
		     "dummy argument or a PARAMETER", &sym->declared_at);
	  specification_expr = saved_specification_expr;
	  return false;
	}

      if (e && sym->attr.save == SAVE_EXPLICIT && !gfc_is_constant_expr (e))
	{
	  gfc_error (auto_save_msg, sym->name, &sym->declared_at);
	  specification_expr = saved_specification_expr;
	  return false;
	}

      if (!gfc_is_constant_expr (e)
	  && !(e->expr_type == EXPR_VARIABLE
	       && e->symtree->n.sym->attr.flavor == FL_PARAMETER))
	{
	  if (!sym->attr.use_assoc && sym->ns->proc_name
	      && (sym->ns->proc_name->attr.flavor == FL_MODULE
		  || sym->ns->proc_name->attr.is_main_program))
	    {
	      gfc_error ("%qs at %L must have constant character length "
			"in this context", sym->name, &sym->declared_at);
	      specification_expr = saved_specification_expr;
	      return false;
	    }
	  if (sym->attr.in_common)
	    {
	      gfc_error ("COMMON variable %qs at %L must have constant "
			 "character length", sym->name, &sym->declared_at);
	      specification_expr = saved_specification_expr;
	      return false;
	    }
	}
    }

  if (sym->value == NULL && sym->attr.referenced)
    apply_default_init_local (sym); /* Try to apply a default initialization.  */

  /* Determine if the symbol may not have an initializer.  */
  no_init_flag = automatic_flag = 0;
  if (sym->attr.allocatable || sym->attr.external || sym->attr.dummy
      || sym->attr.intrinsic || sym->attr.result)
    no_init_flag = 1;
  else if ((sym->attr.dimension || sym->attr.codimension) && !sym->attr.pointer
	   && is_non_constant_shape_array (sym))
    {
      no_init_flag = automatic_flag = 1;

      /* Also, they must not have the SAVE attribute.
	 SAVE_IMPLICIT is checked below.  */
      if (sym->as && sym->attr.codimension)
	{
	  int corank = sym->as->corank;
	  sym->as->corank = 0;
	  no_init_flag = automatic_flag = is_non_constant_shape_array (sym);
	  sym->as->corank = corank;
	}
      if (automatic_flag && sym->attr.save == SAVE_EXPLICIT)
	{
	  gfc_error (auto_save_msg, sym->name, &sym->declared_at);
	  specification_expr = saved_specification_expr;
	  return false;
	}
    }

  /* Ensure that any initializer is simplified.  */
  if (sym->value)
    gfc_simplify_expr (sym->value, 1);

  /* Reject illegal initializers.  */
  if (!sym->mark && sym->value)
    {
      if (sym->attr.allocatable || (sym->ts.type == BT_CLASS
				    && CLASS_DATA (sym)->attr.allocatable))
	gfc_error ("Allocatable %qs at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.external)
	gfc_error ("External %qs at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.dummy
	&& !(sym->ts.type == BT_DERIVED && sym->attr.intent == INTENT_OUT))
	gfc_error ("Dummy %qs at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.intrinsic)
	gfc_error ("Intrinsic %qs at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.result)
	gfc_error ("Function result %qs at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (automatic_flag)
	gfc_error ("Automatic array %qs at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else
	goto no_init_error;
      specification_expr = saved_specification_expr;
      return false;
    }

no_init_error:
  if (sym->ts.type == BT_DERIVED || sym->ts.type == BT_CLASS)
    {
      bool res = resolve_fl_variable_derived (sym, no_init_flag);
      specification_expr = saved_specification_expr;
      return res;
    }

  specification_expr = saved_specification_expr;
  return true;
}


/* Compare the dummy characteristics of a module procedure interface
   declaration with the corresponding declaration in a submodule.  */
static gfc_formal_arglist *new_formal;
static char errmsg[200];

static void
compare_fsyms (gfc_symbol *sym)
{
  gfc_symbol *fsym;

  if (sym == NULL || new_formal == NULL)
    return;

  fsym = new_formal->sym;

  if (sym == fsym)
    return;

  if (strcmp (sym->name, fsym->name) == 0)
    {
      if (!gfc_check_dummy_characteristics (fsym, sym, true, errmsg, 200))
	gfc_error ("%s at %L", errmsg, &fsym->declared_at);
    }
}


/* Resolve a procedure.  */

static bool
resolve_fl_procedure (gfc_symbol *sym, int mp_flag)
{
  gfc_formal_arglist *arg;

  if (sym->attr.function
      && !resolve_fl_var_and_proc (sym, mp_flag))
    return false;

  if (sym->ts.type == BT_CHARACTER)
    {
      gfc_charlen *cl = sym->ts.u.cl;

      if (cl && cl->length && gfc_is_constant_expr (cl->length)
	     && !resolve_charlen (cl))
	return false;

      if ((!cl || !cl->length || cl->length->expr_type != EXPR_CONSTANT)
	  && sym->attr.proc == PROC_ST_FUNCTION)
	{
	  gfc_error ("Character-valued statement function %qs at %L must "
		     "have constant length", sym->name, &sym->declared_at);
	  return false;
	}
    }

  /* Ensure that derived type for are not of a private type.  Internal
     module procedures are excluded by 2.2.3.3 - i.e., they are not
     externally accessible and can access all the objects accessible in
     the host.  */
  if (!(sym->ns->parent
	&& sym->ns->parent->proc_name->attr.flavor == FL_MODULE)
      && gfc_check_symbol_access (sym))
    {
      gfc_interface *iface;

      for (arg = gfc_sym_get_dummy_args (sym); arg; arg = arg->next)
	{
	  if (arg->sym
	      && arg->sym->ts.type == BT_DERIVED
	      && !arg->sym->ts.u.derived->attr.use_assoc
	      && !gfc_check_symbol_access (arg->sym->ts.u.derived)
	      && !gfc_notify_std (GFC_STD_F2003, "%qs is of a PRIVATE type "
				  "and cannot be a dummy argument"
				  " of %qs, which is PUBLIC at %L",
				  arg->sym->name, sym->name,
				  &sym->declared_at))
	    {
	      /* Stop this message from recurring.  */
	      arg->sym->ts.u.derived->attr.access = ACCESS_PUBLIC;
	      return false;
	    }
	}

      /* PUBLIC interfaces may expose PRIVATE procedures that take types
	 PRIVATE to the containing module.  */
      for (iface = sym->generic; iface; iface = iface->next)
	{
	  for (arg = gfc_sym_get_dummy_args (iface->sym); arg; arg = arg->next)
	    {
	      if (arg->sym
		  && arg->sym->ts.type == BT_DERIVED
		  && !arg->sym->ts.u.derived->attr.use_assoc
		  && !gfc_check_symbol_access (arg->sym->ts.u.derived)
		  && !gfc_notify_std (GFC_STD_F2003, "Procedure %qs in "
				      "PUBLIC interface %qs at %L "
				      "takes dummy arguments of %qs which "
				      "is PRIVATE", iface->sym->name,
				      sym->name, &iface->sym->declared_at,
				      gfc_typename(&arg->sym->ts)))
		{
		  /* Stop this message from recurring.  */
		  arg->sym->ts.u.derived->attr.access = ACCESS_PUBLIC;
		  return false;
		}
	     }
	}
    }

  if (sym->attr.function && sym->value && sym->attr.proc != PROC_ST_FUNCTION
      && !sym->attr.proc_pointer)
    {
      gfc_error ("Function %qs at %L cannot have an initializer",
		 sym->name, &sym->declared_at);
      return false;
    }

  /* An external symbol may not have an initializer because it is taken to be
     a procedure. Exception: Procedure Pointers.  */
  if (sym->attr.external && sym->value && !sym->attr.proc_pointer)
    {
      gfc_error ("External object %qs at %L may not have an initializer",
		 sym->name, &sym->declared_at);
      return false;
    }

  /* An elemental function is required to return a scalar 12.7.1  */
  if (sym->attr.elemental && sym->attr.function && sym->as)
    {
      gfc_error ("ELEMENTAL function %qs at %L must have a scalar "
		 "result", sym->name, &sym->declared_at);
      /* Reset so that the error only occurs once.  */
      sym->attr.elemental = 0;
      return false;
    }

  if (sym->attr.proc == PROC_ST_FUNCTION
      && (sym->attr.allocatable || sym->attr.pointer))
    {
      gfc_error ("Statement function %qs at %L may not have pointer or "
		 "allocatable attribute", sym->name, &sym->declared_at);
      return false;
    }

  /* 5.1.1.5 of the Standard: A function name declared with an asterisk
     char-len-param shall not be array-valued, pointer-valued, recursive
     or pure.  ....snip... A character value of * may only be used in the
     following ways: (i) Dummy arg of procedure - dummy associates with
     actual length; (ii) To declare a named constant; or (iii) External
     function - but length must be declared in calling scoping unit.  */
  if (sym->attr.function
      && sym->ts.type == BT_CHARACTER && !sym->ts.deferred
      && sym->ts.u.cl && sym->ts.u.cl->length == NULL)
    {
      if ((sym->as && sym->as->rank) || (sym->attr.pointer)
	  || (sym->attr.recursive) || (sym->attr.pure))
	{
	  if (sym->as && sym->as->rank)
	    gfc_error ("CHARACTER(*) function %qs at %L cannot be "
		       "array-valued", sym->name, &sym->declared_at);

	  if (sym->attr.pointer)
	    gfc_error ("CHARACTER(*) function %qs at %L cannot be "
		       "pointer-valued", sym->name, &sym->declared_at);

	  if (sym->attr.pure)
	    gfc_error ("CHARACTER(*) function %qs at %L cannot be "
		       "pure", sym->name, &sym->declared_at);

	  if (sym->attr.recursive)
	    gfc_error ("CHARACTER(*) function %qs at %L cannot be "
		       "recursive", sym->name, &sym->declared_at);

	  return false;
	}

      /* Appendix B.2 of the standard.  Contained functions give an
	 error anyway.  Deferred character length is an F2003 feature.
	 Don't warn on intrinsic conversion functions, which start
	 with two underscores.  */
      if (!sym->attr.contained && !sym->ts.deferred
	  && (sym->name[0] != '_' || sym->name[1] != '_'))
	gfc_notify_std (GFC_STD_F95_OBS,
			"CHARACTER(*) function %qs at %L",
			sym->name, &sym->declared_at);
    }

  /* F2008, C1218.  */
  if (sym->attr.elemental)
    {
      if (sym->attr.proc_pointer)
	{
	  gfc_error ("Procedure pointer %qs at %L shall not be elemental",
		     sym->name, &sym->declared_at);
	  return false;
	}
      if (sym->attr.dummy)
	{
	  gfc_error ("Dummy procedure %qs at %L shall not be elemental",
		     sym->name, &sym->declared_at);
	  return false;
	}
    }

  if (sym->attr.is_bind_c && sym->attr.is_c_interop != 1)
    {
      gfc_formal_arglist *curr_arg;
      int has_non_interop_arg = 0;

      if (!verify_bind_c_sym (sym, &(sym->ts), sym->attr.in_common,
			      sym->common_block))
        {
          /* Clear these to prevent looking at them again if there was an
             error.  */
          sym->attr.is_bind_c = 0;
          sym->attr.is_c_interop = 0;
          sym->ts.is_c_interop = 0;
        }
      else
        {
          /* So far, no errors have been found.  */
          sym->attr.is_c_interop = 1;
          sym->ts.is_c_interop = 1;
        }

      curr_arg = gfc_sym_get_dummy_args (sym);
      while (curr_arg != NULL)
        {
          /* Skip implicitly typed dummy args here.  */
	  if (curr_arg->sym->attr.implicit_type == 0)
	    if (!gfc_verify_c_interop_param (curr_arg->sym))
	      /* If something is found to fail, record the fact so we
		 can mark the symbol for the procedure as not being
		 BIND(C) to try and prevent multiple errors being
		 reported.  */
	      has_non_interop_arg = 1;

          curr_arg = curr_arg->next;
        }

      /* See if any of the arguments were not interoperable and if so, clear
	 the procedure symbol to prevent duplicate error messages.  */
      if (has_non_interop_arg != 0)
	{
	  sym->attr.is_c_interop = 0;
	  sym->ts.is_c_interop = 0;
	  sym->attr.is_bind_c = 0;
	}
    }

  if (!sym->attr.proc_pointer)
    {
      if (sym->attr.save == SAVE_EXPLICIT)
	{
	  gfc_error ("PROCEDURE attribute conflicts with SAVE attribute "
		     "in %qs at %L", sym->name, &sym->declared_at);
	  return false;
	}
      if (sym->attr.intent)
	{
	  gfc_error ("PROCEDURE attribute conflicts with INTENT attribute "
		     "in %qs at %L", sym->name, &sym->declared_at);
	  return false;
	}
      if (sym->attr.subroutine && sym->attr.result)
	{
	  gfc_error ("PROCEDURE attribute conflicts with RESULT attribute "
		     "in %qs at %L", sym->name, &sym->declared_at);
	  return false;
	}
      if (sym->attr.external && sym->attr.function && !sym->attr.module_procedure
	  && ((sym->attr.if_source == IFSRC_DECL && !sym->attr.procedure)
	      || sym->attr.contained))
	{
	  gfc_error ("EXTERNAL attribute conflicts with FUNCTION attribute "
		     "in %qs at %L", sym->name, &sym->declared_at);
	  return false;
	}
      if (strcmp ("ppr@", sym->name) == 0)
	{
	  gfc_error ("Procedure pointer result %qs at %L "
		     "is missing the pointer attribute",
		     sym->ns->proc_name->name, &sym->declared_at);
	  return false;
	}
    }

  /* Assume that a procedure whose body is not known has references
     to external arrays.  */
  if (sym->attr.if_source != IFSRC_DECL)
    sym->attr.array_outer_dependency = 1;

  /* Compare the characteristics of a module procedure with the
     interface declaration. Ideally this would be done with
     gfc_compare_interfaces but, at present, the formal interface
     cannot be copied to the ts.interface.  */
  if (sym->attr.module_procedure
      && sym->attr.if_source == IFSRC_DECL)
    {
      gfc_symbol *iface;
      char name[2*GFC_MAX_SYMBOL_LEN + 1];
      char *module_name;
      char *submodule_name;
      strcpy (name, sym->ns->proc_name->name);
      module_name = strtok (name, ".");
      submodule_name = strtok (NULL, ".");

      iface = sym->tlink;
      sym->tlink = NULL;

      /* Make sure that the result uses the correct charlen for deferred
	 length results.  */
      if (iface && sym->result
	  && iface->ts.type == BT_CHARACTER
	  && iface->ts.deferred)
	sym->result->ts.u.cl = iface->ts.u.cl;

      if (iface == NULL)
	goto check_formal;

      /* Check the procedure characteristics.  */
      if (sym->attr.elemental != iface->attr.elemental)
	{
	  gfc_error ("Mismatch in ELEMENTAL attribute between MODULE "
		     "PROCEDURE at %L and its interface in %s",
		     &sym->declared_at, module_name);
	  return false;
	}

      if (sym->attr.pure != iface->attr.pure)
	{
	  gfc_error ("Mismatch in PURE attribute between MODULE "
		     "PROCEDURE at %L and its interface in %s",
		     &sym->declared_at, module_name);
	  return false;
	}

      if (sym->attr.recursive != iface->attr.recursive)
	{
	  gfc_error ("Mismatch in RECURSIVE attribute between MODULE "
		     "PROCEDURE at %L and its interface in %s",
		     &sym->declared_at, module_name);
	  return false;
	}

      /* Check the result characteristics.  */
      if (!gfc_check_result_characteristics (sym, iface, errmsg, 200))
	{
	  gfc_error ("%s between the MODULE PROCEDURE declaration "
		     "in MODULE %qs and the declaration at %L in "
		     "(SUB)MODULE %qs",
		     errmsg, module_name, &sym->declared_at,
		     submodule_name ? submodule_name : module_name);
	  return false;
	}

check_formal:
      /* Check the characteristics of the formal arguments.  */
      if (sym->formal && sym->formal_ns)
	{
	  for (arg = sym->formal; arg && arg->sym; arg = arg->next)
	    {
	      new_formal = arg;
	      gfc_traverse_ns (sym->formal_ns, compare_fsyms);
	    }
	}
    }
  return true;
}


/* Resolve a list of finalizer procedures.  That is, after they have hopefully
   been defined and we now know their defined arguments, check that they fulfill
   the requirements of the standard for procedures used as finalizers.  */

static bool
gfc_resolve_finalizers (gfc_symbol* derived, bool *finalizable)
{
  gfc_finalizer* list;
  gfc_finalizer** prev_link; /* For removing wrong entries from the list.  */
  bool result = true;
  bool seen_scalar = false;
  gfc_symbol *vtab;
  gfc_component *c;
  gfc_symbol *parent = gfc_get_derived_super_type (derived);

  if (parent)
    gfc_resolve_finalizers (parent, finalizable);

  /* Return early when not finalizable. Additionally, ensure that derived-type
     components have a their finalizables resolved.  */
  if (!derived->f2k_derived || !derived->f2k_derived->finalizers)
    {
      bool has_final = false;
      for (c = derived->components; c; c = c->next)
	if (c->ts.type == BT_DERIVED
	    && !c->attr.pointer && !c->attr.proc_pointer && !c->attr.allocatable)
	  {
	    bool has_final2 = false;
	    if (!gfc_resolve_finalizers (c->ts.u.derived, &has_final))
	      return false;  /* Error.  */
	    has_final = has_final || has_final2;
	  }
      if (!has_final)
	{
	  if (finalizable)
	    *finalizable = false;
	  return true;
	}
    }

  /* Walk over the list of finalizer-procedures, check them, and if any one
     does not fit in with the standard's definition, print an error and remove
     it from the list.  */
  prev_link = &derived->f2k_derived->finalizers;
  for (list = derived->f2k_derived->finalizers; list; list = *prev_link)
    {
      gfc_formal_arglist *dummy_args;
      gfc_symbol* arg;
      gfc_finalizer* i;
      int my_rank;

      /* Skip this finalizer if we already resolved it.  */
      if (list->proc_tree)
	{
	  if (list->proc_tree->n.sym->formal->sym->as == NULL
	      || list->proc_tree->n.sym->formal->sym->as->rank == 0)
	    seen_scalar = true;
	  prev_link = &(list->next);
	  continue;
	}

      /* Check this exists and is a SUBROUTINE.  */
      if (!list->proc_sym->attr.subroutine)
	{
	  gfc_error ("FINAL procedure %qs at %L is not a SUBROUTINE",
		     list->proc_sym->name, &list->where);
	  goto error;
	}

      /* We should have exactly one argument.  */
      dummy_args = gfc_sym_get_dummy_args (list->proc_sym);
      if (!dummy_args || dummy_args->next)
	{
	  gfc_error ("FINAL procedure at %L must have exactly one argument",
		     &list->where);
	  goto error;
	}
      arg = dummy_args->sym;

      /* This argument must be of our type.  */
      if (arg->ts.type != BT_DERIVED || arg->ts.u.derived != derived)
	{
	  gfc_error ("Argument of FINAL procedure at %L must be of type %qs",
		     &arg->declared_at, derived->name);
	  goto error;
	}

      /* It must neither be a pointer nor allocatable nor optional.  */
      if (arg->attr.pointer)
	{
	  gfc_error ("Argument of FINAL procedure at %L must not be a POINTER",
		     &arg->declared_at);
	  goto error;
	}
      if (arg->attr.allocatable)
	{
	  gfc_error ("Argument of FINAL procedure at %L must not be"
		     " ALLOCATABLE", &arg->declared_at);
	  goto error;
	}
      if (arg->attr.optional)
	{
	  gfc_error ("Argument of FINAL procedure at %L must not be OPTIONAL",
		     &arg->declared_at);
	  goto error;
	}

      /* It must not be INTENT(OUT).  */
      if (arg->attr.intent == INTENT_OUT)
	{
	  gfc_error ("Argument of FINAL procedure at %L must not be"
		     " INTENT(OUT)", &arg->declared_at);
	  goto error;
	}

      /* Warn if the procedure is non-scalar and not assumed shape.  */
      if (warn_surprising && arg->as && arg->as->rank != 0
	  && arg->as->type != AS_ASSUMED_SHAPE)
	gfc_warning (OPT_Wsurprising,
		     "Non-scalar FINAL procedure at %L should have assumed"
		     " shape argument", &arg->declared_at);

      /* Check that it does not match in kind and rank with a FINAL procedure
	 defined earlier.  To really loop over the *earlier* declarations,
	 we need to walk the tail of the list as new ones were pushed at the
	 front.  */
      /* TODO: Handle kind parameters once they are implemented.  */
      my_rank = (arg->as ? arg->as->rank : 0);
      for (i = list->next; i; i = i->next)
	{
	  gfc_formal_arglist *dummy_args;

	  /* Argument list might be empty; that is an error signalled earlier,
	     but we nevertheless continued resolving.  */
	  dummy_args = gfc_sym_get_dummy_args (i->proc_sym);
	  if (dummy_args)
	    {
	      gfc_symbol* i_arg = dummy_args->sym;
	      const int i_rank = (i_arg->as ? i_arg->as->rank : 0);
	      if (i_rank == my_rank)
		{
		  gfc_error ("FINAL procedure %qs declared at %L has the same"
			     " rank (%d) as %qs",
			     list->proc_sym->name, &list->where, my_rank,
			     i->proc_sym->name);
		  goto error;
		}
	    }
	}

	/* Is this the/a scalar finalizer procedure?  */
	if (my_rank == 0)
	  seen_scalar = true;

	/* Find the symtree for this procedure.  */
	gcc_assert (!list->proc_tree);
	list->proc_tree = gfc_find_sym_in_symtree (list->proc_sym);

	prev_link = &list->next;
	continue;

	/* Remove wrong nodes immediately from the list so we don't risk any
	   troubles in the future when they might fail later expectations.  */
error:
	i = list;
	*prev_link = list->next;
	gfc_free_finalizer (i);
	result = false;
    }

  if (result == false)
    return false;

  /* Warn if we haven't seen a scalar finalizer procedure (but we know there
     were nodes in the list, must have been for arrays.  It is surely a good
     idea to have a scalar version there if there's something to finalize.  */
  if (warn_surprising && derived->f2k_derived->finalizers && !seen_scalar)
    gfc_warning (OPT_Wsurprising,
		 "Only array FINAL procedures declared for derived type %qs"
		 " defined at %L, suggest also scalar one",
		 derived->name, &derived->declared_at);

  vtab = gfc_find_derived_vtab (derived);
  c = vtab->ts.u.derived->components->next->next->next->next->next;
  gfc_set_sym_referenced (c->initializer->symtree->n.sym);

  if (finalizable)
    *finalizable = true;

  return true;
}


/* Check if two GENERIC targets are ambiguous and emit an error is they are.  */

static bool
check_generic_tbp_ambiguity (gfc_tbp_generic* t1, gfc_tbp_generic* t2,
			     const char* generic_name, locus where)
{
  gfc_symbol *sym1, *sym2;
  const char *pass1, *pass2;
  gfc_formal_arglist *dummy_args;

  gcc_assert (t1->specific && t2->specific);
  gcc_assert (!t1->specific->is_generic);
  gcc_assert (!t2->specific->is_generic);
  gcc_assert (t1->is_operator == t2->is_operator);

  sym1 = t1->specific->u.specific->n.sym;
  sym2 = t2->specific->u.specific->n.sym;

  if (sym1 == sym2)
    return true;

  /* Both must be SUBROUTINEs or both must be FUNCTIONs.  */
  if (sym1->attr.subroutine != sym2->attr.subroutine
      || sym1->attr.function != sym2->attr.function)
    {
      gfc_error ("%qs and %qs can't be mixed FUNCTION/SUBROUTINE for"
		 " GENERIC %qs at %L",
		 sym1->name, sym2->name, generic_name, &where);
      return false;
    }

  /* Determine PASS arguments.  */
  if (t1->specific->nopass)
    pass1 = NULL;
  else if (t1->specific->pass_arg)
    pass1 = t1->specific->pass_arg;
  else
    {
      dummy_args = gfc_sym_get_dummy_args (t1->specific->u.specific->n.sym);
      if (dummy_args)
	pass1 = dummy_args->sym->name;
      else
	pass1 = NULL;
    }
  if (t2->specific->nopass)
    pass2 = NULL;
  else if (t2->specific->pass_arg)
    pass2 = t2->specific->pass_arg;
  else
    {
      dummy_args = gfc_sym_get_dummy_args (t2->specific->u.specific->n.sym);
      if (dummy_args)
	pass2 = dummy_args->sym->name;
      else
	pass2 = NULL;
    }

  /* Compare the interfaces.  */
  if (gfc_compare_interfaces (sym1, sym2, sym2->name, !t1->is_operator, 0,
			      NULL, 0, pass1, pass2))
    {
      gfc_error ("%qs and %qs for GENERIC %qs at %L are ambiguous",
		 sym1->name, sym2->name, generic_name, &where);
      return false;
    }

  return true;
}


/* Worker function for resolving a generic procedure binding; this is used to
   resolve GENERIC as well as user and intrinsic OPERATOR typebound procedures.

   The difference between those cases is finding possible inherited bindings
   that are overridden, as one has to look for them in tb_sym_root,
   tb_uop_root or tb_op, respectively.  Thus the caller must already find
   the super-type and set p->overridden correctly.  */

static bool
resolve_tb_generic_targets (gfc_symbol* super_type,
			    gfc_typebound_proc* p, const char* name)
{
  gfc_tbp_generic* target;
  gfc_symtree* first_target;
  gfc_symtree* inherited;

  gcc_assert (p && p->is_generic);

  /* Try to find the specific bindings for the symtrees in our target-list.  */
  gcc_assert (p->u.generic);
  for (target = p->u.generic; target; target = target->next)
    if (!target->specific)
      {
	gfc_typebound_proc* overridden_tbp;
	gfc_tbp_generic* g;
	const char* target_name;

	target_name = target->specific_st->name;

	/* Defined for this type directly.  */
	if (target->specific_st->n.tb && !target->specific_st->n.tb->error)
	  {
	    target->specific = target->specific_st->n.tb;
	    goto specific_found;
	  }

	/* Look for an inherited specific binding.  */
	if (super_type)
	  {
	    inherited = gfc_find_typebound_proc (super_type, NULL, target_name,
						 true, NULL);

	    if (inherited)
	      {
		gcc_assert (inherited->n.tb);
		target->specific = inherited->n.tb;
		goto specific_found;
	      }
	  }

	gfc_error ("Undefined specific binding %qs as target of GENERIC %qs"
		   " at %L", target_name, name, &p->where);
	return false;

	/* Once we've found the specific binding, check it is not ambiguous with
	   other specifics already found or inherited for the same GENERIC.  */
specific_found:
	gcc_assert (target->specific);

	/* This must really be a specific binding!  */
	if (target->specific->is_generic)
	  {
	    gfc_error ("GENERIC %qs at %L must target a specific binding,"
		       " %qs is GENERIC, too", name, &p->where, target_name);
	    return false;
	  }

	/* Check those already resolved on this type directly.  */
	for (g = p->u.generic; g; g = g->next)
	  if (g != target && g->specific
	      && !check_generic_tbp_ambiguity (target, g, name, p->where))
	    return false;

	/* Check for ambiguity with inherited specific targets.  */
	for (overridden_tbp = p->overridden; overridden_tbp;
	     overridden_tbp = overridden_tbp->overridden)
	  if (overridden_tbp->is_generic)
	    {
	      for (g = overridden_tbp->u.generic; g; g = g->next)
		{
		  gcc_assert (g->specific);
		  if (!check_generic_tbp_ambiguity (target, g, name, p->where))
		    return false;
		}
	    }
      }

  /* If we attempt to "overwrite" a specific binding, this is an error.  */
  if (p->overridden && !p->overridden->is_generic)
    {
      gfc_error ("GENERIC %qs at %L can't overwrite specific binding with"
		 " the same name", name, &p->where);
      return false;
    }

  /* Take the SUBROUTINE/FUNCTION attributes of the first specific target, as
     all must have the same attributes here.  */
  first_target = p->u.generic->specific->u.specific;
  gcc_assert (first_target);
  p->subroutine = first_target->n.sym->attr.subroutine;
  p->function = first_target->n.sym->attr.function;

  return true;
}


/* Resolve a GENERIC procedure binding for a derived type.  */

static bool
resolve_typebound_generic (gfc_symbol* derived, gfc_symtree* st)
{
  gfc_symbol* super_type;

  /* Find the overridden binding if any.  */
  st->n.tb->overridden = NULL;
  super_type = gfc_get_derived_super_type (derived);
  if (super_type)
    {
      gfc_symtree* overridden;
      overridden = gfc_find_typebound_proc (super_type, NULL, st->name,
					    true, NULL);

      if (overridden && overridden->n.tb)
	st->n.tb->overridden = overridden->n.tb;
    }

  /* Resolve using worker function.  */
  return resolve_tb_generic_targets (super_type, st->n.tb, st->name);
}


/* Retrieve the target-procedure of an operator binding and do some checks in
   common for intrinsic and user-defined type-bound operators.  */

static gfc_symbol*
get_checked_tb_operator_target (gfc_tbp_generic* target, locus where)
{
  gfc_symbol* target_proc;

  gcc_assert (target->specific && !target->specific->is_generic);
  target_proc = target->specific->u.specific->n.sym;
  gcc_assert (target_proc);

  /* F08:C468. All operator bindings must have a passed-object dummy argument.  */
  if (target->specific->nopass)
    {
      gfc_error ("Type-bound operator at %L can't be NOPASS", &where);
      return NULL;
    }

  return target_proc;
}


/* Resolve a type-bound intrinsic operator.  */

static bool
resolve_typebound_intrinsic_op (gfc_symbol* derived, gfc_intrinsic_op op,
				gfc_typebound_proc* p)
{
  gfc_symbol* super_type;
  gfc_tbp_generic* target;

  /* If there's already an error here, do nothing (but don't fail again).  */
  if (p->error)
    return true;

  /* Operators should always be GENERIC bindings.  */
  gcc_assert (p->is_generic);

  /* Look for an overridden binding.  */
  super_type = gfc_get_derived_super_type (derived);
  if (super_type && super_type->f2k_derived)
    p->overridden = gfc_find_typebound_intrinsic_op (super_type, NULL,
						     op, true, NULL);
  else
    p->overridden = NULL;

  /* Resolve general GENERIC properties using worker function.  */
  if (!resolve_tb_generic_targets (super_type, p, gfc_op2string(op)))
    goto error;

  /* Check the targets to be procedures of correct interface.  */
  for (target = p->u.generic; target; target = target->next)
    {
      gfc_symbol* target_proc;

      target_proc = get_checked_tb_operator_target (target, p->where);
      if (!target_proc)
	goto error;

      if (!gfc_check_operator_interface (target_proc, op, p->where))
	goto error;

      /* Add target to non-typebound operator list.  */
      if (!target->specific->deferred && !derived->attr.use_assoc
	  && p->access != ACCESS_PRIVATE && derived->ns == gfc_current_ns)
	{
	  gfc_interface *head, *intr;

	  /* Preempt 'gfc_check_new_interface' for submodules, where the
	     mechanism for handling module procedures winds up resolving
	     operator interfaces twice and would otherwise cause an error.  */
	  for (intr = derived->ns->op[op]; intr; intr = intr->next)
	    if (intr->sym == target_proc
		&& target_proc->attr.used_in_submodule)
	      return true;

	  if (!gfc_check_new_interface (derived->ns->op[op],
					target_proc, p->where))
	    return false;
	  head = derived->ns->op[op];
	  intr = gfc_get_interface ();
	  intr->sym = target_proc;
	  intr->where = p->where;
	  intr->next = head;
	  derived->ns->op[op] = intr;
	}
    }

  return true;

error:
  p->error = 1;
  return false;
}


/* Resolve a type-bound user operator (tree-walker callback).  */

static gfc_symbol* resolve_bindings_derived;
static bool resolve_bindings_result;

static bool check_uop_procedure (gfc_symbol* sym, locus where);

static void
resolve_typebound_user_op (gfc_symtree* stree)
{
  gfc_symbol* super_type;
  gfc_tbp_generic* target;

  gcc_assert (stree && stree->n.tb);

  if (stree->n.tb->error)
    return;

  /* Operators should always be GENERIC bindings.  */
  gcc_assert (stree->n.tb->is_generic);

  /* Find overridden procedure, if any.  */
  super_type = gfc_get_derived_super_type (resolve_bindings_derived);
  if (super_type && super_type->f2k_derived)
    {
      gfc_symtree* overridden;
      overridden = gfc_find_typebound_user_op (super_type, NULL,
					       stree->name, true, NULL);

      if (overridden && overridden->n.tb)
	stree->n.tb->overridden = overridden->n.tb;
    }
  else
    stree->n.tb->overridden = NULL;

  /* Resolve basically using worker function.  */
  if (!resolve_tb_generic_targets (super_type, stree->n.tb, stree->name))
    goto error;

  /* Check the targets to be functions of correct interface.  */
  for (target = stree->n.tb->u.generic; target; target = target->next)
    {
      gfc_symbol* target_proc;

      target_proc = get_checked_tb_operator_target (target, stree->n.tb->where);
      if (!target_proc)
	goto error;

      if (!check_uop_procedure (target_proc, stree->n.tb->where))
	goto error;
    }

  return;

error:
  resolve_bindings_result = false;
  stree->n.tb->error = 1;
}


/* Resolve the type-bound procedures for a derived type.  */

static void
resolve_typebound_procedure (gfc_symtree* stree)
{
  gfc_symbol* proc;
  locus where;
  gfc_symbol* me_arg;
  gfc_symbol* super_type;
  gfc_component* comp;

  gcc_assert (stree);

  /* Undefined specific symbol from GENERIC target definition.  */
  if (!stree->n.tb)
    return;

  if (stree->n.tb->error)
    return;

  /* If this is a GENERIC binding, use that routine.  */
  if (stree->n.tb->is_generic)
    {
      if (!resolve_typebound_generic (resolve_bindings_derived, stree))
	goto error;
      return;
    }

  /* Get the target-procedure to check it.  */
  gcc_assert (!stree->n.tb->is_generic);
  gcc_assert (stree->n.tb->u.specific);
  proc = stree->n.tb->u.specific->n.sym;
  where = stree->n.tb->where;

  /* Default access should already be resolved from the parser.  */
  gcc_assert (stree->n.tb->access != ACCESS_UNKNOWN);

  if (stree->n.tb->deferred)
    {
      if (!check_proc_interface (proc, &where))
	goto error;
    }
  else
    {
      /* Check for F08:C465.  */
      if ((!proc->attr.subroutine && !proc->attr.function)
	  || (proc->attr.proc != PROC_MODULE
	      && proc->attr.if_source != IFSRC_IFBODY)
	  || proc->attr.abstract)
	{
	  gfc_error ("%qs must be a module procedure or an external procedure with"
		    " an explicit interface at %L", proc->name, &where);
	  goto error;
	}
    }

  stree->n.tb->subroutine = proc->attr.subroutine;
  stree->n.tb->function = proc->attr.function;

  /* Find the super-type of the current derived type.  We could do this once and
     store in a global if speed is needed, but as long as not I believe this is
     more readable and clearer.  */
  super_type = gfc_get_derived_super_type (resolve_bindings_derived);

  /* If PASS, resolve and check arguments if not already resolved / loaded
     from a .mod file.  */
  if (!stree->n.tb->nopass && stree->n.tb->pass_arg_num == 0)
    {
      gfc_formal_arglist *dummy_args;

      dummy_args = gfc_sym_get_dummy_args (proc);
      if (stree->n.tb->pass_arg)
	{
	  gfc_formal_arglist *i;

	  /* If an explicit passing argument name is given, walk the arg-list
	     and look for it.  */

	  me_arg = NULL;
	  stree->n.tb->pass_arg_num = 1;
	  for (i = dummy_args; i; i = i->next)
	    {
	      if (!strcmp (i->sym->name, stree->n.tb->pass_arg))
		{
		  me_arg = i->sym;
		  break;
		}
	      ++stree->n.tb->pass_arg_num;
	    }

	  if (!me_arg)
	    {
	      gfc_error ("Procedure %qs with PASS(%s) at %L has no"
			 " argument %qs",
			 proc->name, stree->n.tb->pass_arg, &where,
			 stree->n.tb->pass_arg);
	      goto error;
	    }
	}
      else
	{
	  /* Otherwise, take the first one; there should in fact be at least
	     one.  */
	  stree->n.tb->pass_arg_num = 1;
	  if (!dummy_args)
	    {
	      gfc_error ("Procedure %qs with PASS at %L must have at"
			 " least one argument", proc->name, &where);
	      goto error;
	    }
	  me_arg = dummy_args->sym;
	}

      /* Now check that the argument-type matches and the passed-object
	 dummy argument is generally fine.  */

      gcc_assert (me_arg);

      if (me_arg->ts.type != BT_CLASS)
	{
	  gfc_error ("Non-polymorphic passed-object dummy argument of %qs"
		     " at %L", proc->name, &where);
	  goto error;
	}

      if (CLASS_DATA (me_arg)->ts.u.derived
	  != resolve_bindings_derived)
	{
	  gfc_error ("Argument %qs of %qs with PASS(%s) at %L must be of"
		     " the derived-type %qs", me_arg->name, proc->name,
		     me_arg->name, &where, resolve_bindings_derived->name);
	  goto error;
	}

      gcc_assert (me_arg->ts.type == BT_CLASS);
      if (CLASS_DATA (me_arg)->as && CLASS_DATA (me_arg)->as->rank != 0)
	{
	  gfc_error ("Passed-object dummy argument of %qs at %L must be"
		     " scalar", proc->name, &where);
	  goto error;
	}
      if (CLASS_DATA (me_arg)->attr.allocatable)
	{
	  gfc_error ("Passed-object dummy argument of %qs at %L must not"
		     " be ALLOCATABLE", proc->name, &where);
	  goto error;
	}
      if (CLASS_DATA (me_arg)->attr.class_pointer)
	{
	  gfc_error ("Passed-object dummy argument of %qs at %L must not"
		     " be POINTER", proc->name, &where);
	  goto error;
	}
    }

  /* If we are extending some type, check that we don't override a procedure
     flagged NON_OVERRIDABLE.  */
  stree->n.tb->overridden = NULL;
  if (super_type)
    {
      gfc_symtree* overridden;
      overridden = gfc_find_typebound_proc (super_type, NULL,
					    stree->name, true, NULL);

      if (overridden)
	{
	  if (overridden->n.tb)
	    stree->n.tb->overridden = overridden->n.tb;

	  if (!gfc_check_typebound_override (stree, overridden))
	    goto error;
	}
    }

  /* See if there's a name collision with a component directly in this type.  */
  for (comp = resolve_bindings_derived->components; comp; comp = comp->next)
    if (!strcmp (comp->name, stree->name))
      {
	gfc_error ("Procedure %qs at %L has the same name as a component of"
		   " %qs",
		   stree->name, &where, resolve_bindings_derived->name);
	goto error;
      }

  /* Try to find a name collision with an inherited component.  */
  if (super_type && gfc_find_component (super_type, stree->name, true, true,
                                        NULL))
    {
      gfc_error ("Procedure %qs at %L has the same name as an inherited"
		 " component of %qs",
		 stree->name, &where, resolve_bindings_derived->name);
      goto error;
    }

  stree->n.tb->error = 0;
  return;

error:
  resolve_bindings_result = false;
  stree->n.tb->error = 1;
}


static bool
resolve_typebound_procedures (gfc_symbol* derived)
{
  int op;
  gfc_symbol* super_type;

  if (!derived->f2k_derived || !derived->f2k_derived->tb_sym_root)
    return true;

  super_type = gfc_get_derived_super_type (derived);
  if (super_type)
    resolve_symbol (super_type);

  resolve_bindings_derived = derived;
  resolve_bindings_result = true;

  if (derived->f2k_derived->tb_sym_root)
    gfc_traverse_symtree (derived->f2k_derived->tb_sym_root,
			  &resolve_typebound_procedure);

  if (derived->f2k_derived->tb_uop_root)
    gfc_traverse_symtree (derived->f2k_derived->tb_uop_root,
			  &resolve_typebound_user_op);

  for (op = 0; op != GFC_INTRINSIC_OPS; ++op)
    {
      gfc_typebound_proc* p = derived->f2k_derived->tb_op[op];
      if (p && !resolve_typebound_intrinsic_op (derived,
						(gfc_intrinsic_op)op, p))
	resolve_bindings_result = false;
    }

  return resolve_bindings_result;
}


/* Add a derived type to the dt_list.  The dt_list is used in trans-types.c
   to give all identical derived types the same backend_decl.  */
static void
add_dt_to_dt_list (gfc_symbol *derived)
{
  gfc_dt_list *dt_list;

  for (dt_list = gfc_derived_types; dt_list; dt_list = dt_list->next)
    if (derived == dt_list->derived)
      return;

  dt_list = gfc_get_dt_list ();
  dt_list->next = gfc_derived_types;
  dt_list->derived = derived;
  gfc_derived_types = dt_list;
}


/* Ensure that a derived-type is really not abstract, meaning that every
   inherited DEFERRED binding is overridden by a non-DEFERRED one.  */

static bool
ensure_not_abstract_walker (gfc_symbol* sub, gfc_symtree* st)
{
  if (!st)
    return true;

  if (!ensure_not_abstract_walker (sub, st->left))
    return false;
  if (!ensure_not_abstract_walker (sub, st->right))
    return false;

  if (st->n.tb && st->n.tb->deferred)
    {
      gfc_symtree* overriding;
      overriding = gfc_find_typebound_proc (sub, NULL, st->name, true, NULL);
      if (!overriding)
	return false;
      gcc_assert (overriding->n.tb);
      if (overriding->n.tb->deferred)
	{
	  gfc_error ("Derived-type %qs declared at %L must be ABSTRACT because"
		     " %qs is DEFERRED and not overridden",
		     sub->name, &sub->declared_at, st->name);
	  return false;
	}
    }

  return true;
}

static bool
ensure_not_abstract (gfc_symbol* sub, gfc_symbol* ancestor)
{
  /* The algorithm used here is to recursively travel up the ancestry of sub
     and for each ancestor-type, check all bindings.  If any of them is
     DEFERRED, look it up starting from sub and see if the found (overriding)
     binding is not DEFERRED.
     This is not the most efficient way to do this, but it should be ok and is
     clearer than something sophisticated.  */

  gcc_assert (ancestor && !sub->attr.abstract);

  if (!ancestor->attr.abstract)
    return true;

  /* Walk bindings of this ancestor.  */
  if (ancestor->f2k_derived)
    {
      bool t;
      t = ensure_not_abstract_walker (sub, ancestor->f2k_derived->tb_sym_root);
      if (!t)
	return false;
    }

  /* Find next ancestor type and recurse on it.  */
  ancestor = gfc_get_derived_super_type (ancestor);
  if (ancestor)
    return ensure_not_abstract (sub, ancestor);

  return true;
}


/* This check for typebound defined assignments is done recursively
   since the order in which derived types are resolved is not always in
   order of the declarations.  */

static void
check_defined_assignments (gfc_symbol *derived)
{
  gfc_component *c;

  for (c = derived->components; c; c = c->next)
    {
      if (!gfc_bt_struct (c->ts.type)
	  || c->attr.pointer
	  || c->attr.allocatable
	  || c->attr.proc_pointer_comp
	  || c->attr.class_pointer
	  || c->attr.proc_pointer)
	continue;

      if (c->ts.u.derived->attr.defined_assign_comp
	  || (c->ts.u.derived->f2k_derived
	     && c->ts.u.derived->f2k_derived->tb_op[INTRINSIC_ASSIGN]))
	{
	  derived->attr.defined_assign_comp = 1;
	  return;
	}

      check_defined_assignments (c->ts.u.derived);
      if (c->ts.u.derived->attr.defined_assign_comp)
	{
	  derived->attr.defined_assign_comp = 1;
	  return;
	}
    }
}


/* Resolve a single component of a derived type or structure.  */

static bool
resolve_component (gfc_component *c, gfc_symbol *sym)
{
  gfc_symbol *super_type;

  if (c->attr.artificial)
    return true;

  if (sym->attr.vtype && sym->attr.use_assoc)
    return true;

  /* F2008, C442.  */
  if ((!sym->attr.is_class || c != sym->components)
      && c->attr.codimension
      && (!c->attr.allocatable || (c->as && c->as->type != AS_DEFERRED)))
    {
      gfc_error ("Coarray component %qs at %L must be allocatable with "
                 "deferred shape", c->name, &c->loc);
      return false;
    }

  /* F2008, C443.  */
  if (c->attr.codimension && c->ts.type == BT_DERIVED
      && c->ts.u.derived->ts.is_iso_c)
    {
      gfc_error ("Component %qs at %L of TYPE(C_PTR) or TYPE(C_FUNPTR) "
                 "shall not be a coarray", c->name, &c->loc);
      return false;
    }

  /* F2008, C444.  */
  if (gfc_bt_struct (c->ts.type) && c->ts.u.derived->attr.coarray_comp
      && (c->attr.codimension || c->attr.pointer || c->attr.dimension
          || c->attr.allocatable))
    {
      gfc_error ("Component %qs at %L with coarray component "
                 "shall be a nonpointer, nonallocatable scalar",
                 c->name, &c->loc);
      return false;
    }

  /* F2008, C448.  */
  if (c->attr.contiguous && (!c->attr.dimension || !c->attr.pointer))
    {
      gfc_error ("Component %qs at %L has the CONTIGUOUS attribute but "
                 "is not an array pointer", c->name, &c->loc);
      return false;
    }

  if (c->attr.proc_pointer && c->ts.interface)
    {
      gfc_symbol *ifc = c->ts.interface;

      if (!sym->attr.vtype && !check_proc_interface (ifc, &c->loc))
        {
          c->tb->error = 1;
          return false;
        }

      if (ifc->attr.if_source || ifc->attr.intrinsic)
        {
          /* Resolve interface and copy attributes.  */
          if (ifc->formal && !ifc->formal_ns)
            resolve_symbol (ifc);
          if (ifc->attr.intrinsic)
            gfc_resolve_intrinsic (ifc, &ifc->declared_at);

          if (ifc->result)
            {
              c->ts = ifc->result->ts;
              c->attr.allocatable = ifc->result->attr.allocatable;
              c->attr.pointer = ifc->result->attr.pointer;
              c->attr.dimension = ifc->result->attr.dimension;
              c->as = gfc_copy_array_spec (ifc->result->as);
              c->attr.class_ok = ifc->result->attr.class_ok;
            }
          else
            {
              c->ts = ifc->ts;
              c->attr.allocatable = ifc->attr.allocatable;
              c->attr.pointer = ifc->attr.pointer;
              c->attr.dimension = ifc->attr.dimension;
              c->as = gfc_copy_array_spec (ifc->as);
              c->attr.class_ok = ifc->attr.class_ok;
            }
          c->ts.interface = ifc;
          c->attr.function = ifc->attr.function;
          c->attr.subroutine = ifc->attr.subroutine;

          c->attr.pure = ifc->attr.pure;
          c->attr.elemental = ifc->attr.elemental;
          c->attr.recursive = ifc->attr.recursive;
          c->attr.always_explicit = ifc->attr.always_explicit;
          c->attr.ext_attr |= ifc->attr.ext_attr;
          /* Copy char length.  */
          if (ifc->ts.type == BT_CHARACTER && ifc->ts.u.cl)
            {
              gfc_charlen *cl = gfc_new_charlen (sym->ns, ifc->ts.u.cl);
              if (cl->length && !cl->resolved
                  && !gfc_resolve_expr (cl->length))
                {
                  c->tb->error = 1;
                  return false;
                }
              c->ts.u.cl = cl;
            }
        }
    }
  else if (c->attr.proc_pointer && c->ts.type == BT_UNKNOWN)
    {
      /* Since PPCs are not implicitly typed, a PPC without an explicit
         interface must be a subroutine.  */
      gfc_add_subroutine (&c->attr, c->name, &c->loc);
    }

  /* Procedure pointer components: Check PASS arg.  */
  if (c->attr.proc_pointer && !c->tb->nopass && c->tb->pass_arg_num == 0
      && !sym->attr.vtype)
    {
      gfc_symbol* me_arg;

      if (c->tb->pass_arg)
        {
          gfc_formal_arglist* i;

          /* If an explicit passing argument name is given, walk the arg-list
            and look for it.  */

          me_arg = NULL;
          c->tb->pass_arg_num = 1;
          for (i = c->ts.interface->formal; i; i = i->next)
            {
              if (!strcmp (i->sym->name, c->tb->pass_arg))
                {
                  me_arg = i->sym;
                  break;
                }
              c->tb->pass_arg_num++;
            }

          if (!me_arg)
            {
              gfc_error ("Procedure pointer component %qs with PASS(%s) "
                         "at %L has no argument %qs", c->name,
                         c->tb->pass_arg, &c->loc, c->tb->pass_arg);
              c->tb->error = 1;
              return false;
            }
        }
      else
        {
          /* Otherwise, take the first one; there should in fact be at least
            one.  */
          c->tb->pass_arg_num = 1;
          if (!c->ts.interface->formal)
            {
              gfc_error ("Procedure pointer component %qs with PASS at %L "
                         "must have at least one argument",
                         c->name, &c->loc);
              c->tb->error = 1;
              return false;
            }
          me_arg = c->ts.interface->formal->sym;
        }

      /* Now check that the argument-type matches.  */
      gcc_assert (me_arg);
      if ((me_arg->ts.type != BT_DERIVED && me_arg->ts.type != BT_CLASS)
          || (me_arg->ts.type == BT_DERIVED && me_arg->ts.u.derived != sym)
          || (me_arg->ts.type == BT_CLASS
              && CLASS_DATA (me_arg)->ts.u.derived != sym))
        {
          gfc_error ("Argument %qs of %qs with PASS(%s) at %L must be of"
                     " the derived type %qs", me_arg->name, c->name,
                     me_arg->name, &c->loc, sym->name);
          c->tb->error = 1;
          return false;
        }

      /* Check for C453.  */
      if (me_arg->attr.dimension)
        {
          gfc_error ("Argument %qs of %qs with PASS(%s) at %L "
                     "must be scalar", me_arg->name, c->name, me_arg->name,
                     &c->loc);
          c->tb->error = 1;
          return false;
        }

      if (me_arg->attr.pointer)
        {
          gfc_error ("Argument %qs of %qs with PASS(%s) at %L "
                     "may not have the POINTER attribute", me_arg->name,
                     c->name, me_arg->name, &c->loc);
          c->tb->error = 1;
          return false;
        }

      if (me_arg->attr.allocatable)
        {
          gfc_error ("Argument %qs of %qs with PASS(%s) at %L "
                     "may not be ALLOCATABLE", me_arg->name, c->name,
                     me_arg->name, &c->loc);
          c->tb->error = 1;
          return false;
        }

      if (gfc_type_is_extensible (sym) && me_arg->ts.type != BT_CLASS)
        {
          gfc_error ("Non-polymorphic passed-object dummy argument of %qs"
                     " at %L", c->name, &c->loc);
          return false;
        }

    }

  /* Check type-spec if this is not the parent-type component.  */
  if (((sym->attr.is_class
        && (!sym->components->ts.u.derived->attr.extension
            || c != sym->components->ts.u.derived->components))
       || (!sym->attr.is_class
           && (!sym->attr.extension || c != sym->components)))
      && !sym->attr.vtype
      && !resolve_typespec_used (&c->ts, &c->loc, c->name))
    return false;

  super_type = gfc_get_derived_super_type (sym);

  /* If this type is an extension, set the accessibility of the parent
     component.  */
  if (super_type
      && ((sym->attr.is_class
           && c == sym->components->ts.u.derived->components)
          || (!sym->attr.is_class && c == sym->components))
      && strcmp (super_type->name, c->name) == 0)
    c->attr.access = super_type->attr.access;

  /* If this type is an extension, see if this component has the same name
     as an inherited type-bound procedure.  */
  if (super_type && !sym->attr.is_class
      && gfc_find_typebound_proc (super_type, NULL, c->name, true, NULL))
    {
      gfc_error ("Component %qs of %qs at %L has the same name as an"
                 " inherited type-bound procedure",
                 c->name, sym->name, &c->loc);
      return false;
    }

  if (c->ts.type == BT_CHARACTER && !c->attr.proc_pointer
        && !c->ts.deferred)
    {
     if (c->ts.u.cl->length == NULL
         || (!resolve_charlen(c->ts.u.cl))
         || !gfc_is_constant_expr (c->ts.u.cl->length))
       {
         gfc_error ("Character length of component %qs needs to "
                    "be a constant specification expression at %L",
                    c->name,
                    c->ts.u.cl->length ? &c->ts.u.cl->length->where : &c->loc);
         return false;
       }
    }

  if (c->ts.type == BT_CHARACTER && c->ts.deferred
      && !c->attr.pointer && !c->attr.allocatable)
    {
      gfc_error ("Character component %qs of %qs at %L with deferred "
                 "length must be a POINTER or ALLOCATABLE",
                 c->name, sym->name, &c->loc);
      return false;
    }

  /* Add the hidden deferred length field.  */
  if (c->ts.type == BT_CHARACTER && c->ts.deferred && !c->attr.function
      && !sym->attr.is_class)
    {
      char name[GFC_MAX_SYMBOL_LEN+9];
      gfc_component *strlen;
      sprintf (name, "_%s_length", c->name);
      strlen = gfc_find_component (sym, name, true, true, NULL);
      if (strlen == NULL)
        {
          if (!gfc_add_component (sym, name, &strlen))
            return false;
          strlen->ts.type = BT_INTEGER;
          strlen->ts.kind = gfc_charlen_int_kind;
          strlen->attr.access = ACCESS_PRIVATE;
          strlen->attr.artificial = 1;
        }
    }

  if (c->ts.type == BT_DERIVED
      && sym->component_access != ACCESS_PRIVATE
      && gfc_check_symbol_access (sym)
      && !is_sym_host_assoc (c->ts.u.derived, sym->ns)
      && !c->ts.u.derived->attr.use_assoc
      && !gfc_check_symbol_access (c->ts.u.derived)
      && !gfc_notify_std (GFC_STD_F2003, "the component %qs is a "
                          "PRIVATE type and cannot be a component of "
                          "%qs, which is PUBLIC at %L", c->name,
                          sym->name, &sym->declared_at))
    return false;

  if ((sym->attr.sequence || sym->attr.is_bind_c) && c->ts.type == BT_CLASS)
    {
      gfc_error ("Polymorphic component %s at %L in SEQUENCE or BIND(C) "
                 "type %s", c->name, &c->loc, sym->name);
      return false;
    }

  if (sym->attr.sequence)
    {
      if (c->ts.type == BT_DERIVED && c->ts.u.derived->attr.sequence == 0)
        {
          gfc_error ("Component %s of SEQUENCE type declared at %L does "
                     "not have the SEQUENCE attribute",
                     c->ts.u.derived->name, &sym->declared_at);
          return false;
        }
    }

  if (c->ts.type == BT_DERIVED && c->ts.u.derived->attr.generic)
    c->ts.u.derived = gfc_find_dt_in_generic (c->ts.u.derived);
  else if (c->ts.type == BT_CLASS && c->attr.class_ok
           && CLASS_DATA (c)->ts.u.derived->attr.generic)
    CLASS_DATA (c)->ts.u.derived
                    = gfc_find_dt_in_generic (CLASS_DATA (c)->ts.u.derived);

  if (!sym->attr.is_class && c->ts.type == BT_DERIVED && !sym->attr.vtype
      && c->attr.pointer && c->ts.u.derived->components == NULL
      && !c->ts.u.derived->attr.zero_comp)
    {
      gfc_error ("The pointer component %qs of %qs at %L is a type "
                 "that has not been declared", c->name, sym->name,
                 &c->loc);
      return false;
    }

  if (c->ts.type == BT_CLASS && c->attr.class_ok
      && CLASS_DATA (c)->attr.class_pointer
      && CLASS_DATA (c)->ts.u.derived->components == NULL
      && !CLASS_DATA (c)->ts.u.derived->attr.zero_comp
      && !UNLIMITED_POLY (c))
    {
      gfc_error ("The pointer component %qs of %qs at %L is a type "
                 "that has not been declared", c->name, sym->name,
                 &c->loc);
      return false;
    }

  /* If an allocatable component derived type is of the same type as
     the enclosing derived type, we need a vtable generating so that
     the __deallocate procedure is created.  */
  if ((c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
       && c->ts.u.derived == sym && c->attr.allocatable == 1)
    gfc_find_vtab (&c->ts);

  /* Ensure that all the derived type components are put on the
     derived type list; even in formal namespaces, where derived type
     pointer components might not have been declared.  */
  if (c->ts.type == BT_DERIVED
        && c->ts.u.derived
        && c->ts.u.derived->components
        && c->attr.pointer
        && sym != c->ts.u.derived)
    add_dt_to_dt_list (c->ts.u.derived);

  if (!gfc_resolve_array_spec (c->as,
                               !(c->attr.pointer || c->attr.proc_pointer
                                 || c->attr.allocatable)))
    return false;

  if (c->initializer && !sym->attr.vtype
      && !gfc_check_assign_symbol (sym, c, c->initializer))
    return false;

  return true;
}


/* Be nice about the locus for a structure expression - show the locus of the
   first non-null sub-expression if we can.  */

static locus *
cons_where (gfc_expr *struct_expr)
{
  gfc_constructor *cons;

  gcc_assert (struct_expr && struct_expr->expr_type == EXPR_STRUCTURE);

  cons = gfc_constructor_first (struct_expr->value.constructor);
  for (; cons; cons = gfc_constructor_next (cons))
    {
      if (cons->expr && cons->expr->expr_type != EXPR_NULL)
        return &cons->expr->where;
    }

  return &struct_expr->where;
}

/* Resolve the components of a structure type. Much less work than derived
   types.  */

static bool
resolve_fl_struct (gfc_symbol *sym)
{
  gfc_component *c;
  gfc_expr *init = NULL;
  bool success;

  /* Make sure UNIONs do not have overlapping initializers.  */
  if (sym->attr.flavor == FL_UNION)
    {
      for (c = sym->components; c; c = c->next)
        {
          if (init && c->initializer)
            {
              gfc_error ("Conflicting initializers in union at %L and %L",
                         cons_where (init), cons_where (c->initializer));
              gfc_free_expr (c->initializer);
              c->initializer = NULL;
            }
          if (init == NULL)
            init = c->initializer;
        }
    }

  success = true;
  for (c = sym->components; c; c = c->next)
    if (!resolve_component (c, sym))
      success = false;

  if (!success)
    return false;

  if (sym->components)
    add_dt_to_dt_list (sym);

  return true;
}


/* Resolve the components of a derived type. This does not have to wait until
   resolution stage, but can be done as soon as the dt declaration has been
   parsed.  */

static bool
resolve_fl_derived0 (gfc_symbol *sym)
{
  gfc_symbol* super_type;
  gfc_component *c;
  bool success;

  if (sym->attr.unlimited_polymorphic)
    return true;

  super_type = gfc_get_derived_super_type (sym);

  /* F2008, C432.  */
  if (super_type && sym->attr.coarray_comp && !super_type->attr.coarray_comp)
    {
      gfc_error ("As extending type %qs at %L has a coarray component, "
		 "parent type %qs shall also have one", sym->name,
		 &sym->declared_at, super_type->name);
      return false;
    }

  /* Ensure the extended type gets resolved before we do.  */
  if (super_type && !resolve_fl_derived0 (super_type))
    return false;

  /* An ABSTRACT type must be extensible.  */
  if (sym->attr.abstract && !gfc_type_is_extensible (sym))
    {
      gfc_error ("Non-extensible derived-type %qs at %L must not be ABSTRACT",
		 sym->name, &sym->declared_at);
      return false;
    }

  c = (sym->attr.is_class) ? sym->components->ts.u.derived->components
			   : sym->components;

  success = true;
  for ( ; c != NULL; c = c->next)
    if (!resolve_component (c, sym))
      success = false;

  if (!success)
    return false;

  check_defined_assignments (sym);

  if (!sym->attr.defined_assign_comp && super_type)
    sym->attr.defined_assign_comp
			= super_type->attr.defined_assign_comp;

  /* If this is a non-ABSTRACT type extending an ABSTRACT one, ensure that
     all DEFERRED bindings are overridden.  */
  if (super_type && super_type->attr.abstract && !sym->attr.abstract
      && !sym->attr.is_class
      && !ensure_not_abstract (sym, super_type))
    return false;

  /* Add derived type to the derived type list.  */
  add_dt_to_dt_list (sym);

  return true;
}


/* The following procedure does the full resolution of a derived type,
   including resolution of all type-bound procedures (if present). In contrast
   to 'resolve_fl_derived0' this can only be done after the module has been
   parsed completely.  */

static bool
resolve_fl_derived (gfc_symbol *sym)
{
  gfc_symbol *gen_dt = NULL;

  if (sym->attr.unlimited_polymorphic)
    return true;

  if (!sym->attr.is_class)
    gfc_find_symbol (sym->name, sym->ns, 0, &gen_dt);
  if (gen_dt && gen_dt->generic && gen_dt->generic->next
      && (!gen_dt->generic->sym->attr.use_assoc
	  || gen_dt->generic->sym->module != gen_dt->generic->next->sym->module)
      && !gfc_notify_std (GFC_STD_F2003, "Generic name %qs of function "
			  "%qs at %L being the same name as derived "
			  "type at %L", sym->name,
			  gen_dt->generic->sym == sym
			  ? gen_dt->generic->next->sym->name
			  : gen_dt->generic->sym->name,
			  gen_dt->generic->sym == sym
			  ? &gen_dt->generic->next->sym->declared_at
			  : &gen_dt->generic->sym->declared_at,
			  &sym->declared_at))
    return false;

  /* Resolve the finalizer procedures.  */
  if (!gfc_resolve_finalizers (sym, NULL))
    return false;

  if (sym->attr.is_class && sym->ts.u.derived == NULL)
    {
      /* Fix up incomplete CLASS symbols.  */
      gfc_component *data = gfc_find_component (sym, "_data", true, true, NULL);
      gfc_component *vptr = gfc_find_component (sym, "_vptr", true, true, NULL);

      /* Nothing more to do for unlimited polymorphic entities.  */
      if (data->ts.u.derived->attr.unlimited_polymorphic)
	return true;
      else if (vptr->ts.u.derived == NULL)
	{
	  gfc_symbol *vtab = gfc_find_derived_vtab (data->ts.u.derived);
	  gcc_assert (vtab);
	  vptr->ts.u.derived = vtab->ts.u.derived;
	  if (!resolve_fl_derived0 (vptr->ts.u.derived))
	    return false;
	}
    }

  if (!resolve_fl_derived0 (sym))
    return false;

  /* Resolve the type-bound procedures.  */
  if (!resolve_typebound_procedures (sym))
    return false;

  /* Generate module vtables subject to their accessibility and their not
     being vtables or pdt templates. If this is not done class declarations
     in external procedures wind up with their own version and so SELECT TYPE
     fails because the vptrs do not have the same address.  */
  if (gfc_option.allow_std & GFC_STD_F2003
      && sym->ns->proc_name
      && sym->ns->proc_name->attr.flavor == FL_MODULE
      && sym->attr.access != ACCESS_PRIVATE
      && !(sym->attr.use_assoc || sym->attr.vtype))
    {
      gfc_symbol *vtab = gfc_find_derived_vtab (sym);
      gfc_set_sym_referenced (vtab);
    }

  return true;
}


static bool
resolve_fl_namelist (gfc_symbol *sym)
{
  gfc_namelist *nl;
  gfc_symbol *nlsym;

  for (nl = sym->namelist; nl; nl = nl->next)
    {
      /* Check again, the check in match only works if NAMELIST comes
	 after the decl.  */
      if (nl->sym->as && nl->sym->as->type == AS_ASSUMED_SIZE)
     	{
	  gfc_error ("Assumed size array %qs in namelist %qs at %L is not "
		     "allowed", nl->sym->name, sym->name, &sym->declared_at);
	  return false;
	}

      if (nl->sym->as && nl->sym->as->type == AS_ASSUMED_SHAPE
	  && !gfc_notify_std (GFC_STD_F2003, "NAMELIST array object %qs "
			      "with assumed shape in namelist %qs at %L",
			      nl->sym->name, sym->name, &sym->declared_at))
	return false;

      if (is_non_constant_shape_array (nl->sym)
	  && !gfc_notify_std (GFC_STD_F2003, "NAMELIST array object %qs "
			      "with nonconstant shape in namelist %qs at %L",
			      nl->sym->name, sym->name, &sym->declared_at))
	return false;

      if (nl->sym->ts.type == BT_CHARACTER
	  && (nl->sym->ts.u.cl->length == NULL
	      || !gfc_is_constant_expr (nl->sym->ts.u.cl->length))
	  && !gfc_notify_std (GFC_STD_F2003, "NAMELIST object %qs with "
			      "nonconstant character length in "
			      "namelist %qs at %L", nl->sym->name,
			      sym->name, &sym->declared_at))
	return false;

    }

  /* Reject PRIVATE objects in a PUBLIC namelist.  */
  if (gfc_check_symbol_access (sym))
    {
      for (nl = sym->namelist; nl; nl = nl->next)
	{
	  if (!nl->sym->attr.use_assoc
	      && !is_sym_host_assoc (nl->sym, sym->ns)
	      && !gfc_check_symbol_access (nl->sym))
	    {
	      gfc_error ("NAMELIST object %qs was declared PRIVATE and "
			 "cannot be member of PUBLIC namelist %qs at %L",
			 nl->sym->name, sym->name, &sym->declared_at);
	      return false;
	    }

	  if (nl->sym->ts.type == BT_DERIVED
	     && (nl->sym->ts.u.derived->attr.alloc_comp
		 || nl->sym->ts.u.derived->attr.pointer_comp))
	   {
	     if (!gfc_notify_std (GFC_STD_F2003, "NAMELIST object %qs in "
				  "namelist %qs at %L with ALLOCATABLE "
				  "or POINTER components", nl->sym->name,
				  sym->name, &sym->declared_at))
	       return false;
	     return true;
	   }

	  /* Types with private components that came here by USE-association.  */
	  if (nl->sym->ts.type == BT_DERIVED
	      && derived_inaccessible (nl->sym->ts.u.derived))
	    {
	      gfc_error ("NAMELIST object %qs has use-associated PRIVATE "
			 "components and cannot be member of namelist %qs at %L",
			 nl->sym->name, sym->name, &sym->declared_at);
	      return false;
	    }

	  /* Types with private components that are defined in the same module.  */
	  if (nl->sym->ts.type == BT_DERIVED
	      && !is_sym_host_assoc (nl->sym->ts.u.derived, sym->ns)
	      && nl->sym->ts.u.derived->attr.private_comp)
	    {
	      gfc_error ("NAMELIST object %qs has PRIVATE components and "
			 "cannot be a member of PUBLIC namelist %qs at %L",
			 nl->sym->name, sym->name, &sym->declared_at);
	      return false;
	    }
	}
    }


  /* 14.1.2 A module or internal procedure represent local entities
     of the same type as a namelist member and so are not allowed.  */
  for (nl = sym->namelist; nl; nl = nl->next)
    {
      if (nl->sym->ts.kind != 0 && nl->sym->attr.flavor == FL_VARIABLE)
	continue;

      if (nl->sym->attr.function && nl->sym == nl->sym->result)
	if ((nl->sym == sym->ns->proc_name)
	       ||
	    (sym->ns->parent && nl->sym == sym->ns->parent->proc_name))
	  continue;

      nlsym = NULL;
      if (nl->sym->name)
	gfc_find_symbol (nl->sym->name, sym->ns, 1, &nlsym);
      if (nlsym && nlsym->attr.flavor == FL_PROCEDURE)
	{
	  gfc_error ("PROCEDURE attribute conflicts with NAMELIST "
		     "attribute in %qs at %L", nlsym->name,
		     &sym->declared_at);
	  return false;
	}
    }

  return true;
}


static bool
resolve_fl_parameter (gfc_symbol *sym)
{
  /* A parameter array's shape needs to be constant.  */
  if (sym->as != NULL
      && (sym->as->type == AS_DEFERRED
          || is_non_constant_shape_array (sym)))
    {
      gfc_error ("Parameter array %qs at %L cannot be automatic "
		 "or of deferred shape", sym->name, &sym->declared_at);
      return false;
    }

  /* Constraints on deferred type parameter.  */
  if (!deferred_requirements (sym))
    return false;

  /* Make sure a parameter that has been implicitly typed still
     matches the implicit type, since PARAMETER statements can precede
     IMPLICIT statements.  */
  if (sym->attr.implicit_type
      && !gfc_compare_types (&sym->ts, gfc_get_default_type (sym->name,
							     sym->ns)))
    {
      gfc_error ("Implicitly typed PARAMETER %qs at %L doesn't match a "
		 "later IMPLICIT type", sym->name, &sym->declared_at);
      return false;
    }

  /* Make sure the types of derived parameters are consistent.  This
     type checking is deferred until resolution because the type may
     refer to a derived type from the host.  */
  if (sym->ts.type == BT_DERIVED
      && !gfc_compare_types (&sym->ts, &sym->value->ts))
    {
      gfc_error ("Incompatible derived type in PARAMETER at %L",
		 &sym->value->where);
      return false;
    }

  /* F03:C509,C514.  */
  if (sym->ts.type == BT_CLASS)
    {
      gfc_error ("CLASS variable %qs at %L cannot have the PARAMETER attribute",
		 sym->name, &sym->declared_at);
      return false;
    }

  return true;
}


/* Do anything necessary to resolve a symbol.  Right now, we just
   assume that an otherwise unknown symbol is a variable.  This sort
   of thing commonly happens for symbols in module.  */

static void
resolve_symbol (gfc_symbol *sym)
{
  int check_constant, mp_flag;
  gfc_symtree *symtree;
  gfc_symtree *this_symtree;
  gfc_namespace *ns;
  gfc_component *c;
  symbol_attribute class_attr;
  gfc_array_spec *as;
  bool saved_specification_expr;

  if (sym->resolved)
    return;
  sym->resolved = 1;

  /* No symbol will ever have union type; only components can be unions.
     Union type declaration symbols have type BT_UNKNOWN but flavor FL_UNION
     (just like derived type declaration symbols have flavor FL_DERIVED). */
  gcc_assert (sym->ts.type != BT_UNION);

  /* Coarrayed polymorphic objects with allocatable or pointer components are
     yet unsupported for -fcoarray=lib.  */
  if (flag_coarray == GFC_FCOARRAY_LIB && sym->ts.type == BT_CLASS
      && sym->ts.u.derived && CLASS_DATA (sym)
      && CLASS_DATA (sym)->attr.codimension
      && (CLASS_DATA (sym)->ts.u.derived->attr.alloc_comp
	  || CLASS_DATA (sym)->ts.u.derived->attr.pointer_comp))
    {
      gfc_error ("Sorry, allocatable/pointer components in polymorphic (CLASS) "
		 "type coarrays at %L are unsupported", &sym->declared_at);
      return;
    }

  if (sym->attr.artificial)
    return;

  if (sym->attr.unlimited_polymorphic)
    return;

  if (sym->attr.flavor == FL_UNKNOWN
      || (sym->attr.flavor == FL_PROCEDURE && !sym->attr.intrinsic
	  && !sym->attr.generic && !sym->attr.external
	  && sym->attr.if_source == IFSRC_UNKNOWN
	  && sym->ts.type == BT_UNKNOWN))
    {

    /* If we find that a flavorless symbol is an interface in one of the
       parent namespaces, find its symtree in this namespace, free the
       symbol and set the symtree to point to the interface symbol.  */
      for (ns = gfc_current_ns->parent; ns; ns = ns->parent)
	{
	  symtree = gfc_find_symtree (ns->sym_root, sym->name);
	  if (symtree && (symtree->n.sym->generic ||
			  (symtree->n.sym->attr.flavor == FL_PROCEDURE
			   && sym->ns->construct_entities)))
	    {
	      this_symtree = gfc_find_symtree (gfc_current_ns->sym_root,
					       sym->name);
	      if (this_symtree->n.sym == sym)
		{
		  symtree->n.sym->refs++;
		  gfc_release_symbol (sym);
		  this_symtree->n.sym = symtree->n.sym;
		  return;
		}
	    }
	}

      /* Otherwise give it a flavor according to such attributes as
	 it has.  */
      if (sym->attr.flavor == FL_UNKNOWN && sym->attr.external == 0
	  && sym->attr.intrinsic == 0)
	sym->attr.flavor = FL_VARIABLE;
      else if (sym->attr.flavor == FL_UNKNOWN)
	{
	  sym->attr.flavor = FL_PROCEDURE;
	  if (sym->attr.dimension)
	    sym->attr.function = 1;
	}
    }

  if (sym->attr.external && sym->ts.type != BT_UNKNOWN && !sym->attr.function)
    gfc_add_function (&sym->attr, sym->name, &sym->declared_at);

  if (sym->attr.procedure && sym->attr.if_source != IFSRC_DECL
      && !resolve_procedure_interface (sym))
    return;

  if (sym->attr.is_protected && !sym->attr.proc_pointer
      && (sym->attr.procedure || sym->attr.external))
    {
      if (sym->attr.external)
	gfc_error ("PROTECTED attribute conflicts with EXTERNAL attribute "
	           "at %L", &sym->declared_at);
      else
	gfc_error ("PROCEDURE attribute conflicts with PROTECTED attribute "
	           "at %L", &sym->declared_at);

      return;
    }

  if (sym->attr.flavor == FL_DERIVED && !resolve_fl_derived (sym))
    return;

  else if ((sym->attr.flavor == FL_STRUCT || sym->attr.flavor == FL_UNION)
           && !resolve_fl_struct (sym))
    return;

  /* Symbols that are module procedures with results (functions) have
     the types and array specification copied for type checking in
     procedures that call them, as well as for saving to a module
     file.  These symbols can't stand the scrutiny that their results
     can.  */
  mp_flag = (sym->result != NULL && sym->result != sym);

  /* Make sure that the intrinsic is consistent with its internal
     representation. This needs to be done before assigning a default
     type to avoid spurious warnings.  */
  if (sym->attr.flavor != FL_MODULE && sym->attr.intrinsic
      && !gfc_resolve_intrinsic (sym, &sym->declared_at))
    return;

  /* Resolve associate names.  */
  if (sym->assoc)
    resolve_assoc_var (sym, true);

  /* Assign default type to symbols that need one and don't have one.  */
  if (sym->ts.type == BT_UNKNOWN)
    {
      if (sym->attr.flavor == FL_VARIABLE || sym->attr.flavor == FL_PARAMETER)
	{
	  gfc_set_default_type (sym, 1, NULL);
	}

      if (sym->attr.flavor == FL_PROCEDURE && sym->attr.external
	  && !sym->attr.function && !sym->attr.subroutine
	  && gfc_get_default_type (sym->name, sym->ns)->type == BT_UNKNOWN)
	gfc_add_subroutine (&sym->attr, sym->name, &sym->declared_at);

      if (sym->attr.flavor == FL_PROCEDURE && sym->attr.function)
	{
	  /* The specific case of an external procedure should emit an error
	     in the case that there is no implicit type.  */
	  if (!mp_flag)
	    {
	      if (!sym->attr.mixed_entry_master)
		gfc_set_default_type (sym, sym->attr.external, NULL);
	    }
	  else
	    {
	      /* Result may be in another namespace.  */
	      resolve_symbol (sym->result);

	      if (!sym->result->attr.proc_pointer)
		{
		  sym->ts = sym->result->ts;
		  sym->as = gfc_copy_array_spec (sym->result->as);
		  sym->attr.dimension = sym->result->attr.dimension;
		  sym->attr.pointer = sym->result->attr.pointer;
		  sym->attr.allocatable = sym->result->attr.allocatable;
		  sym->attr.contiguous = sym->result->attr.contiguous;
		}
	    }
	}
    }
  else if (mp_flag && sym->attr.flavor == FL_PROCEDURE && sym->attr.function)
    {
      bool saved_specification_expr = specification_expr;
      specification_expr = true;
      gfc_resolve_array_spec (sym->result->as, false);
      specification_expr = saved_specification_expr;
    }

  if (sym->ts.type == BT_CLASS && sym->attr.class_ok)
    {
      as = CLASS_DATA (sym)->as;
      class_attr = CLASS_DATA (sym)->attr;
      class_attr.pointer = class_attr.class_pointer;
    }
  else
    {
      class_attr = sym->attr;
      as = sym->as;
    }

  /* F2008, C530.  */
  if (sym->attr.contiguous
      && (!class_attr.dimension
	  || (as->type != AS_ASSUMED_SHAPE && as->type != AS_ASSUMED_RANK
	      && !class_attr.pointer)))
    {
      gfc_error ("%qs at %L has the CONTIGUOUS attribute but is not an "
		 "array pointer or an assumed-shape or assumed-rank array",
		 sym->name, &sym->declared_at);
      return;
    }

  /* Assumed size arrays and assumed shape arrays must be dummy
     arguments.  Array-spec's of implied-shape should have been resolved to
     AS_EXPLICIT already.  */

  if (as)
    {
      /* If AS_IMPLIED_SHAPE makes it to here, it must be a bad
	 specification expression.  */
      if (as->type == AS_IMPLIED_SHAPE)
	{
	  int i;
	  for (i=0; i<as->rank; i++)
	    {
	      if (as->lower[i] != NULL && as->upper[i] == NULL)
		{
		  gfc_error ("Bad specification for assumed size array at %L",
			     &as->lower[i]->where);
		  return;
		}
	    }
	  gcc_unreachable();
	}

      if (((as->type == AS_ASSUMED_SIZE && !as->cp_was_assumed)
	   || as->type == AS_ASSUMED_SHAPE)
	  && !sym->attr.dummy && !sym->attr.select_type_temporary)
	{
	  if (as->type == AS_ASSUMED_SIZE)
	    gfc_error ("Assumed size array at %L must be a dummy argument",
		       &sym->declared_at);
	  else
	    gfc_error ("Assumed shape array at %L must be a dummy argument",
		       &sym->declared_at);
	  return;
	}
      /* TS 29113, C535a.  */
      if (as->type == AS_ASSUMED_RANK && !sym->attr.dummy
	  && !sym->attr.select_type_temporary)
	{
	  gfc_error ("Assumed-rank array at %L must be a dummy argument",
		     &sym->declared_at);
	  return;
	}
      if (as->type == AS_ASSUMED_RANK
	  && (sym->attr.codimension || sym->attr.value))
	{
	  gfc_error ("Assumed-rank array at %L may not have the VALUE or "
		     "CODIMENSION attribute", &sym->declared_at);
	  return;
	}
    }

  /* Make sure symbols with known intent or optional are really dummy
     variable.  Because of ENTRY statement, this has to be deferred
     until resolution time.  */

  if (!sym->attr.dummy
      && (sym->attr.optional || sym->attr.intent != INTENT_UNKNOWN))
    {
      gfc_error ("Symbol at %L is not a DUMMY variable", &sym->declared_at);
      return;
    }

  if (sym->attr.value && !sym->attr.dummy)
    {
      gfc_error ("%qs at %L cannot have the VALUE attribute because "
		 "it is not a dummy argument", sym->name, &sym->declared_at);
      return;
    }

  if (sym->attr.value && sym->ts.type == BT_CHARACTER)
    {
      gfc_charlen *cl = sym->ts.u.cl;
      if (!cl || !cl->length || cl->length->expr_type != EXPR_CONSTANT)
	{
	  gfc_error ("Character dummy variable %qs at %L with VALUE "
		     "attribute must have constant length",
		     sym->name, &sym->declared_at);
	  return;
	}

      if (sym->ts.is_c_interop
	  && mpz_cmp_si (cl->length->value.integer, 1) != 0)
	{
	  gfc_error ("C interoperable character dummy variable %qs at %L "
		     "with VALUE attribute must have length one",
		     sym->name, &sym->declared_at);
	  return;
	}
    }

  if (sym->ts.type == BT_DERIVED && !sym->attr.is_iso_c
      && sym->ts.u.derived->attr.generic)
    {
      sym->ts.u.derived = gfc_find_dt_in_generic (sym->ts.u.derived);
      if (!sym->ts.u.derived)
	{
	  gfc_error ("The derived type %qs at %L is of type %qs, "
		     "which has not been defined", sym->name,
		     &sym->declared_at, sym->ts.u.derived->name);
	  sym->ts.type = BT_UNKNOWN;
	  return;
	}
    }

    /* Use the same constraints as TYPE(*), except for the type check
       and that only scalars and assumed-size arrays are permitted.  */
    if (sym->attr.ext_attr & (1 << EXT_ATTR_NO_ARG_CHECK))
      {
	if (!sym->attr.dummy)
	  {
	    gfc_error ("Variable %s at %L with NO_ARG_CHECK attribute shall be "
		       "a dummy argument", sym->name, &sym->declared_at);
	    return;
	  }

	if (sym->ts.type != BT_ASSUMED && sym->ts.type != BT_INTEGER
	    && sym->ts.type != BT_REAL && sym->ts.type != BT_LOGICAL
	    && sym->ts.type != BT_COMPLEX)
	  {
	    gfc_error ("Variable %s at %L with NO_ARG_CHECK attribute shall be "
		       "of type TYPE(*) or of an numeric intrinsic type",
		       sym->name, &sym->declared_at);
	    return;
	  }

      if (sym->attr.allocatable || sym->attr.codimension
	  || sym->attr.pointer || sym->attr.value)
	{
	  gfc_error ("Variable %s at %L with NO_ARG_CHECK attribute may not "
		     "have the ALLOCATABLE, CODIMENSION, POINTER or VALUE "
		     "attribute", sym->name, &sym->declared_at);
	  return;
	}

      if (sym->attr.intent == INTENT_OUT)
	{
	  gfc_error ("Variable %s at %L with NO_ARG_CHECK attribute may not "
		     "have the INTENT(OUT) attribute",
		     sym->name, &sym->declared_at);
	  return;
	}
      if (sym->attr.dimension && sym->as->type != AS_ASSUMED_SIZE)
	{
	  gfc_error ("Variable %s at %L with NO_ARG_CHECK attribute shall "
		     "either be a scalar or an assumed-size array",
		     sym->name, &sym->declared_at);
	  return;
	}

      /* Set the type to TYPE(*) and add a dimension(*) to ensure
	 NO_ARG_CHECK is correctly handled in trans*.c, e.g. with
	 packing.  */
      sym->ts.type = BT_ASSUMED;
      sym->as = gfc_get_array_spec ();
      sym->as->type = AS_ASSUMED_SIZE;
      sym->as->rank = 1;
      sym->as->lower[0] = gfc_get_int_expr (gfc_default_integer_kind, NULL, 1);
    }
  else if (sym->ts.type == BT_ASSUMED)
    {
      /* TS 29113, C407a.  */
      if (!sym->attr.dummy)
	{
	  gfc_error ("Assumed type of variable %s at %L is only permitted "
		     "for dummy variables", sym->name, &sym->declared_at);
	  return;
	}
      if (sym->attr.allocatable || sym->attr.codimension
	  || sym->attr.pointer || sym->attr.value)
    	{
	  gfc_error ("Assumed-type variable %s at %L may not have the "
		     "ALLOCATABLE, CODIMENSION, POINTER or VALUE attribute",
		     sym->name, &sym->declared_at);
	  return;
	}
      if (sym->attr.intent == INTENT_OUT)
    	{
	  gfc_error ("Assumed-type variable %s at %L may not have the "
		     "INTENT(OUT) attribute",
		     sym->name, &sym->declared_at);
	  return;
	}
      if (sym->attr.dimension && sym->as->type == AS_EXPLICIT)
	{
	  gfc_error ("Assumed-type variable %s at %L shall not be an "
		     "explicit-shape array", sym->name, &sym->declared_at);
	  return;
	}
    }

  /* If the symbol is marked as bind(c), verify it's type and kind.  Do not
     do this for something that was implicitly typed because that is handled
     in gfc_set_default_type.  Handle dummy arguments and procedure
     definitions separately.  Also, anything that is use associated is not
     handled here but instead is handled in the module it is declared in.
     Finally, derived type definitions are allowed to be BIND(C) since that
     only implies that they're interoperable, and they are checked fully for
     interoperability when a variable is declared of that type.  */
  if (sym->attr.is_bind_c && sym->attr.implicit_type == 0 &&
      sym->attr.use_assoc == 0 && sym->attr.dummy == 0 &&
      sym->attr.flavor != FL_PROCEDURE && sym->attr.flavor != FL_DERIVED)
    {
      bool t = true;

      /* First, make sure the variable is declared at the
	 module-level scope (J3/04-007, Section 15.3).	*/
      if (sym->ns->proc_name->attr.flavor != FL_MODULE &&
          sym->attr.in_common == 0)
	{
	  gfc_error ("Variable %qs at %L cannot be BIND(C) because it "
		     "is neither a COMMON block nor declared at the "
		     "module level scope", sym->name, &(sym->declared_at));
	  t = false;
	}
      else if (sym->common_head != NULL)
        {
          t = verify_com_block_vars_c_interop (sym->common_head);
        }
      else
	{
	  /* If type() declaration, we need to verify that the components
	     of the given type are all C interoperable, etc.  */
	  if (sym->ts.type == BT_DERIVED &&
              sym->ts.u.derived->attr.is_c_interop != 1)
            {
              /* Make sure the user marked the derived type as BIND(C).  If
                 not, call the verify routine.  This could print an error
                 for the derived type more than once if multiple variables
                 of that type are declared.  */
              if (sym->ts.u.derived->attr.is_bind_c != 1)
                verify_bind_c_derived_type (sym->ts.u.derived);
              t = false;
            }

	  /* Verify the variable itself as C interoperable if it
             is BIND(C).  It is not possible for this to succeed if
             the verify_bind_c_derived_type failed, so don't have to handle
             any error returned by verify_bind_c_derived_type.  */
          t = verify_bind_c_sym (sym, &(sym->ts), sym->attr.in_common,
                                 sym->common_block);
	}

      if (!t)
        {
          /* clear the is_bind_c flag to prevent reporting errors more than
             once if something failed.  */
          sym->attr.is_bind_c = 0;
          return;
        }
    }

  /* If a derived type symbol has reached this point, without its
     type being declared, we have an error.  Notice that most
     conditions that produce undefined derived types have already
     been dealt with.  However, the likes of:
     implicit type(t) (t) ..... call foo (t) will get us here if
     the type is not declared in the scope of the implicit
     statement. Change the type to BT_UNKNOWN, both because it is so
     and to prevent an ICE.  */
  if (sym->ts.type == BT_DERIVED && !sym->attr.is_iso_c
      && sym->ts.u.derived->components == NULL
      && !sym->ts.u.derived->attr.zero_comp)
    {
      gfc_error ("The derived type %qs at %L is of type %qs, "
		 "which has not been defined", sym->name,
		  &sym->declared_at, sym->ts.u.derived->name);
      sym->ts.type = BT_UNKNOWN;
      return;
    }

  /* Make sure that the derived type has been resolved and that the
     derived type is visible in the symbol's namespace, if it is a
     module function and is not PRIVATE.  */
  if (sym->ts.type == BT_DERIVED
	&& sym->ts.u.derived->attr.use_assoc
	&& sym->ns->proc_name
	&& sym->ns->proc_name->attr.flavor == FL_MODULE
        && !resolve_fl_derived (sym->ts.u.derived))
    return;

  /* Unless the derived-type declaration is use associated, Fortran 95
     does not allow public entries of private derived types.
     See 4.4.1 (F95) and 4.5.1.1 (F2003); and related interpretation
     161 in 95-006r3.  */
  if (sym->ts.type == BT_DERIVED
      && sym->ns->proc_name && sym->ns->proc_name->attr.flavor == FL_MODULE
      && !sym->ts.u.derived->attr.use_assoc
      && gfc_check_symbol_access (sym)
      && !gfc_check_symbol_access (sym->ts.u.derived)
      && !gfc_notify_std (GFC_STD_F2003, "PUBLIC %s %qs at %L of PRIVATE "
			  "derived type %qs",
			  (sym->attr.flavor == FL_PARAMETER)
			  ? "parameter" : "variable",
			  sym->name, &sym->declared_at,
			  sym->ts.u.derived->name))
    return;

  /* F2008, C1302.  */
  if (sym->ts.type == BT_DERIVED
      && ((sym->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
	   && sym->ts.u.derived->intmod_sym_id == ISOFORTRAN_LOCK_TYPE)
	  || sym->ts.u.derived->attr.lock_comp)
      && !sym->attr.codimension && !sym->ts.u.derived->attr.coarray_comp)
    {
      gfc_error ("Variable %s at %L of type LOCK_TYPE or with subcomponent of "
		 "type LOCK_TYPE must be a coarray", sym->name,
		 &sym->declared_at);
      return;
    }

  /* TS18508, C702/C703.  */
  if (sym->ts.type == BT_DERIVED
      && ((sym->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
	   && sym->ts.u.derived->intmod_sym_id == ISOFORTRAN_EVENT_TYPE)
	  || sym->ts.u.derived->attr.event_comp)
      && !sym->attr.codimension && !sym->ts.u.derived->attr.coarray_comp)
    {
      gfc_error ("Variable %s at %L of type EVENT_TYPE or with subcomponent of "
		 "type LOCK_TYPE must be a coarray", sym->name,
		 &sym->declared_at);
      return;
    }

  /* An assumed-size array with INTENT(OUT) shall not be of a type for which
     default initialization is defined (5.1.2.4.4).  */
  if (sym->ts.type == BT_DERIVED
      && sym->attr.dummy
      && sym->attr.intent == INTENT_OUT
      && sym->as
      && sym->as->type == AS_ASSUMED_SIZE)
    {
      for (c = sym->ts.u.derived->components; c; c = c->next)
	{
	  if (c->initializer)
	    {
	      gfc_error ("The INTENT(OUT) dummy argument %qs at %L is "
			 "ASSUMED SIZE and so cannot have a default initializer",
			 sym->name, &sym->declared_at);
	      return;
	    }
	}
    }

  /* F2008, C542.  */
  if (sym->ts.type == BT_DERIVED && sym->attr.dummy
      && sym->attr.intent == INTENT_OUT && sym->attr.lock_comp)
    {
      gfc_error ("Dummy argument %qs at %L of LOCK_TYPE shall not be "
		 "INTENT(OUT)", sym->name, &sym->declared_at);
      return;
    }

  /* TS18508.  */
  if (sym->ts.type == BT_DERIVED && sym->attr.dummy
      && sym->attr.intent == INTENT_OUT && sym->attr.event_comp)
    {
      gfc_error ("Dummy argument %qs at %L of EVENT_TYPE shall not be "
		 "INTENT(OUT)", sym->name, &sym->declared_at);
      return;
    }

  /* F2008, C525.  */
  if ((((sym->ts.type == BT_DERIVED && sym->ts.u.derived->attr.coarray_comp)
	 || (sym->ts.type == BT_CLASS && sym->attr.class_ok
	     && CLASS_DATA (sym)->attr.coarray_comp))
       || class_attr.codimension)
      && (sym->attr.result || sym->result == sym))
    {
      gfc_error ("Function result %qs at %L shall not be a coarray or have "
	         "a coarray component", sym->name, &sym->declared_at);
      return;
    }

  /* F2008, C524.  */
  if (sym->attr.codimension && sym->ts.type == BT_DERIVED
      && sym->ts.u.derived->ts.is_iso_c)
    {
      gfc_error ("Variable %qs at %L of TYPE(C_PTR) or TYPE(C_FUNPTR) "
		 "shall not be a coarray", sym->name, &sym->declared_at);
      return;
    }

  /* F2008, C525.  */
  if (((sym->ts.type == BT_DERIVED && sym->ts.u.derived->attr.coarray_comp)
	|| (sym->ts.type == BT_CLASS && sym->attr.class_ok
	    && CLASS_DATA (sym)->attr.coarray_comp))
      && (class_attr.codimension || class_attr.pointer || class_attr.dimension
	  || class_attr.allocatable))
    {
      gfc_error ("Variable %qs at %L with coarray component shall be a "
		 "nonpointer, nonallocatable scalar, which is not a coarray",
		 sym->name, &sym->declared_at);
      return;
    }

  /* F2008, C526.  The function-result case was handled above.  */
  if (class_attr.codimension
      && !(class_attr.allocatable || sym->attr.dummy || sym->attr.save
	   || sym->attr.select_type_temporary
	   || sym->attr.associate_var
	   || (sym->ns->save_all && !sym->attr.automatic)
	   || sym->ns->proc_name->attr.flavor == FL_MODULE
	   || sym->ns->proc_name->attr.is_main_program
	   || sym->attr.function || sym->attr.result || sym->attr.use_assoc))
    {
      gfc_error ("Variable %qs at %L is a coarray and is not ALLOCATABLE, SAVE "
		 "nor a dummy argument", sym->name, &sym->declared_at);
      return;
    }
  /* F2008, C528.  */
  else if (class_attr.codimension && !sym->attr.select_type_temporary
	   && !class_attr.allocatable && as && as->cotype == AS_DEFERRED)
    {
      gfc_error ("Coarray variable %qs at %L shall not have codimensions with "
		 "deferred shape", sym->name, &sym->declared_at);
      return;
    }
  else if (class_attr.codimension && class_attr.allocatable && as
	   && (as->cotype != AS_DEFERRED || as->type != AS_DEFERRED))
    {
      gfc_error ("Allocatable coarray variable %qs at %L must have "
		 "deferred shape", sym->name, &sym->declared_at);
      return;
    }

  /* F2008, C541.  */
  if ((((sym->ts.type == BT_DERIVED && sym->ts.u.derived->attr.coarray_comp)
	|| (sym->ts.type == BT_CLASS && sym->attr.class_ok
	    && CLASS_DATA (sym)->attr.coarray_comp))
       || (class_attr.codimension && class_attr.allocatable))
      && sym->attr.dummy && sym->attr.intent == INTENT_OUT)
    {
      gfc_error ("Variable %qs at %L is INTENT(OUT) and can thus not be an "
		 "allocatable coarray or have coarray components",
		 sym->name, &sym->declared_at);
      return;
    }

  if (class_attr.codimension && sym->attr.dummy
      && sym->ns->proc_name && sym->ns->proc_name->attr.is_bind_c)
    {
      gfc_error ("Coarray dummy variable %qs at %L not allowed in BIND(C) "
		 "procedure %qs", sym->name, &sym->declared_at,
		 sym->ns->proc_name->name);
      return;
    }

  if (sym->ts.type == BT_LOGICAL
      && ((sym->attr.function && sym->attr.is_bind_c && sym->result == sym)
	  || ((sym->attr.dummy || sym->attr.result) && sym->ns->proc_name
	      && sym->ns->proc_name->attr.is_bind_c)))
    {
      int i;
      for (i = 0; gfc_logical_kinds[i].kind; i++)
        if (gfc_logical_kinds[i].kind == sym->ts.kind)
          break;
      if (!gfc_logical_kinds[i].c_bool && sym->attr.dummy
	  && !gfc_notify_std (GFC_STD_GNU, "LOGICAL dummy argument %qs at "
			      "%L with non-C_Bool kind in BIND(C) procedure "
			      "%qs", sym->name, &sym->declared_at,
			      sym->ns->proc_name->name))
	return;
      else if (!gfc_logical_kinds[i].c_bool
	       && !gfc_notify_std (GFC_STD_GNU, "LOGICAL result variable "
				   "%qs at %L with non-C_Bool kind in "
				   "BIND(C) procedure %qs", sym->name,
				   &sym->declared_at,
				   sym->attr.function ? sym->name
				   : sym->ns->proc_name->name))
	return;
    }

  switch (sym->attr.flavor)
    {
    case FL_VARIABLE:
      if (!resolve_fl_variable (sym, mp_flag))
	return;
      break;

    case FL_PROCEDURE:
      if (sym->formal && !sym->formal_ns)
	{
	  /* Check that none of the arguments are a namelist.  */
	  gfc_formal_arglist *formal = sym->formal;

	  for (; formal; formal = formal->next)
	    if (formal->sym && formal->sym->attr.flavor == FL_NAMELIST)
	      {
		gfc_error ("Namelist %qs can not be an argument to "
			   "subroutine or function at %L",
			   formal->sym->name, &sym->declared_at);
		return;
	      }
	}

      if (!resolve_fl_procedure (sym, mp_flag))
	return;
      break;

    case FL_NAMELIST:
      if (!resolve_fl_namelist (sym))
	return;
      break;

    case FL_PARAMETER:
      if (!resolve_fl_parameter (sym))
	return;
      break;

    default:
      break;
    }

  /* Resolve array specifier. Check as well some constraints
     on COMMON blocks.  */

  check_constant = sym->attr.in_common && !sym->attr.pointer;

  /* Set the formal_arg_flag so that check_conflict will not throw
     an error for host associated variables in the specification
     expression for an array_valued function.  */
  if (sym->attr.function && sym->as)
    formal_arg_flag = true;

  saved_specification_expr = specification_expr;
  specification_expr = true;
  gfc_resolve_array_spec (sym->as, check_constant);
  specification_expr = saved_specification_expr;

  formal_arg_flag = false;

  /* Resolve formal namespaces.  */
  if (sym->formal_ns && sym->formal_ns != gfc_current_ns
      && !sym->attr.contained && !sym->attr.intrinsic)
    gfc_resolve (sym->formal_ns);

  /* Make sure the formal namespace is present.  */
  if (sym->formal && !sym->formal_ns)
    {
      gfc_formal_arglist *formal = sym->formal;
      while (formal && !formal->sym)
	formal = formal->next;

      if (formal)
	{
	  sym->formal_ns = formal->sym->ns;
          if (sym->ns != formal->sym->ns)
	    sym->formal_ns->refs++;
	}
    }

  /* Check threadprivate restrictions.  */
  if (sym->attr.threadprivate && !sym->attr.save
      && !(sym->ns->save_all && !sym->attr.automatic)
      && (!sym->attr.in_common
	  && sym->module == NULL
	  && (sym->ns->proc_name == NULL
	      || sym->ns->proc_name->attr.flavor != FL_MODULE)))
    gfc_error ("Threadprivate at %L isn't SAVEd", &sym->declared_at);

  /* Check omp declare target restrictions.  */
  if (sym->attr.omp_declare_target
      && sym->attr.flavor == FL_VARIABLE
      && !sym->attr.save
      && !(sym->ns->save_all && !sym->attr.automatic)
      && (!sym->attr.in_common
	  && sym->module == NULL
	  && (sym->ns->proc_name == NULL
	      || sym->ns->proc_name->attr.flavor != FL_MODULE)))
    gfc_error ("!$OMP DECLARE TARGET variable %qs at %L isn't SAVEd",
	       sym->name, &sym->declared_at);

  /* If we have come this far we can apply default-initializers, as
     described in 14.7.5, to those variables that have not already
     been assigned one.  */
  if (sym->ts.type == BT_DERIVED
      && !sym->value
      && !sym->attr.allocatable
      && !sym->attr.alloc_comp)
    {
      symbol_attribute *a = &sym->attr;

      if ((!a->save && !a->dummy && !a->pointer
	   && !a->in_common && !a->use_assoc
	   && a->referenced
	   && !((a->function || a->result)
		&& (!a->dimension
		    || sym->ts.u.derived->attr.alloc_comp
		    || sym->ts.u.derived->attr.pointer_comp))
	   && !(a->function && sym != sym->result))
	  || (a->dummy && a->intent == INTENT_OUT && !a->pointer))
	apply_default_init (sym);
      else if (a->function && sym->result && a->access != ACCESS_PRIVATE
	       && (sym->ts.u.derived->attr.alloc_comp
		   || sym->ts.u.derived->attr.pointer_comp))
	/* Mark the result symbol to be referenced, when it has allocatable
	   components.  */
	sym->result->attr.referenced = 1;
    }

  if (sym->ts.type == BT_CLASS && sym->ns == gfc_current_ns
      && sym->attr.dummy && sym->attr.intent == INTENT_OUT
      && !CLASS_DATA (sym)->attr.class_pointer
      && !CLASS_DATA (sym)->attr.allocatable)
    apply_default_init (sym);

  /* If this symbol has a type-spec, check it.  */
  if (sym->attr.flavor == FL_VARIABLE || sym->attr.flavor == FL_PARAMETER
      || (sym->attr.flavor == FL_PROCEDURE && sym->attr.function))
    if (!resolve_typespec_used (&sym->ts, &sym->declared_at, sym->name))
      return;
}


/************* Resolve DATA statements *************/

static struct
{
  gfc_data_value *vnode;
  mpz_t left;
}
values;


/* Advance the values structure to point to the next value in the data list.  */

static bool
next_data_value (void)
{
  while (mpz_cmp_ui (values.left, 0) == 0)
    {

      if (values.vnode->next == NULL)
	return false;

      values.vnode = values.vnode->next;
      mpz_set (values.left, values.vnode->repeat);
    }

  return true;
}


static bool
check_data_variable (gfc_data_variable *var, locus *where)
{
  gfc_expr *e;
  mpz_t size;
  mpz_t offset;
  bool t;
  ar_type mark = AR_UNKNOWN;
  int i;
  mpz_t section_index[GFC_MAX_DIMENSIONS];
  gfc_ref *ref;
  gfc_array_ref *ar;
  gfc_symbol *sym;
  int has_pointer;

  if (!gfc_resolve_expr (var->expr))
    return false;

  ar = NULL;
  mpz_init_set_si (offset, 0);
  e = var->expr;

  if (e->expr_type == EXPR_FUNCTION && e->value.function.isym
      && e->value.function.isym->id == GFC_ISYM_CAF_GET)
    e = e->value.function.actual->expr;

  if (e->expr_type != EXPR_VARIABLE)
    gfc_internal_error ("check_data_variable(): Bad expression");

  sym = e->symtree->n.sym;

  if (sym->ns->is_block_data && !sym->attr.in_common)
    {
      gfc_error ("BLOCK DATA element %qs at %L must be in COMMON",
		 sym->name, &sym->declared_at);
    }

  if (e->ref == NULL && sym->as)
    {
      gfc_error ("DATA array %qs at %L must be specified in a previous"
		 " declaration", sym->name, where);
      return false;
    }

  has_pointer = sym->attr.pointer;

  if (gfc_is_coindexed (e))
    {
      gfc_error ("DATA element %qs at %L cannot have a coindex", sym->name,
		 where);
      return false;
    }

  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT && ref->u.c.component->attr.pointer)
	has_pointer = 1;

      if (has_pointer
	    && ref->type == REF_ARRAY
	    && ref->u.ar.type != AR_FULL)
	  {
	    gfc_error ("DATA element %qs at %L is a pointer and so must "
			"be a full array", sym->name, where);
	    return false;
	  }
    }

  if (e->rank == 0 || has_pointer)
    {
      mpz_init_set_ui (size, 1);
      ref = NULL;
    }
  else
    {
      ref = e->ref;

      /* Find the array section reference.  */
      for (ref = e->ref; ref; ref = ref->next)
	{
	  if (ref->type != REF_ARRAY)
	    continue;
	  if (ref->u.ar.type == AR_ELEMENT)
	    continue;
	  break;
	}
      gcc_assert (ref);

      /* Set marks according to the reference pattern.  */
      switch (ref->u.ar.type)
	{
	case AR_FULL:
	  mark = AR_FULL;
	  break;

	case AR_SECTION:
	  ar = &ref->u.ar;
	  /* Get the start position of array section.  */
	  gfc_get_section_index (ar, section_index, &offset);
	  mark = AR_SECTION;
	  break;

	default:
	  gcc_unreachable ();
	}

      if (!gfc_array_size (e, &size))
	{
	  gfc_error ("Nonconstant array section at %L in DATA statement",
		     &e->where);
	  mpz_clear (offset);
	  return false;
	}
    }

  t = true;

  while (mpz_cmp_ui (size, 0) > 0)
    {
      if (!next_data_value ())
	{
	  gfc_error ("DATA statement at %L has more variables than values",
		     where);
	  t = false;
	  break;
	}

      t = gfc_check_assign (var->expr, values.vnode->expr, 0);
      if (!t)
	break;

      /* If we have more than one element left in the repeat count,
	 and we have more than one element left in the target variable,
	 then create a range assignment.  */
      /* FIXME: Only done for full arrays for now, since array sections
	 seem tricky.  */
      if (mark == AR_FULL && ref && ref->next == NULL
	  && mpz_cmp_ui (values.left, 1) > 0 && mpz_cmp_ui (size, 1) > 0)
	{
	  mpz_t range;

	  if (mpz_cmp (size, values.left) >= 0)
	    {
	      mpz_init_set (range, values.left);
	      mpz_sub (size, size, values.left);
	      mpz_set_ui (values.left, 0);
	    }
	  else
	    {
	      mpz_init_set (range, size);
	      mpz_sub (values.left, values.left, size);
	      mpz_set_ui (size, 0);
	    }

	  t = gfc_assign_data_value (var->expr, values.vnode->expr,
				     offset, &range);

	  mpz_add (offset, offset, range);
	  mpz_clear (range);

	  if (!t)
	    break;
	}

      /* Assign initial value to symbol.  */
      else
	{
	  mpz_sub_ui (values.left, values.left, 1);
	  mpz_sub_ui (size, size, 1);

	  t = gfc_assign_data_value (var->expr, values.vnode->expr,
				     offset, NULL);
	  if (!t)
	    break;

	  if (mark == AR_FULL)
	    mpz_add_ui (offset, offset, 1);

	  /* Modify the array section indexes and recalculate the offset
	     for next element.  */
	  else if (mark == AR_SECTION)
	    gfc_advance_section (section_index, ar, &offset);
	}
    }

  if (mark == AR_SECTION)
    {
      for (i = 0; i < ar->dimen; i++)
	mpz_clear (section_index[i]);
    }

  mpz_clear (size);
  mpz_clear (offset);

  return t;
}


static bool traverse_data_var (gfc_data_variable *, locus *);

/* Iterate over a list of elements in a DATA statement.  */

static bool
traverse_data_list (gfc_data_variable *var, locus *where)
{
  mpz_t trip;
  iterator_stack frame;
  gfc_expr *e, *start, *end, *step;
  bool retval = true;

  mpz_init (frame.value);
  mpz_init (trip);

  start = gfc_copy_expr (var->iter.start);
  end = gfc_copy_expr (var->iter.end);
  step = gfc_copy_expr (var->iter.step);

  if (!gfc_simplify_expr (start, 1)
      || start->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("start of implied-do loop at %L could not be "
		 "simplified to a constant value", &start->where);
      retval = false;
      goto cleanup;
    }
  if (!gfc_simplify_expr (end, 1)
      || end->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("end of implied-do loop at %L could not be "
		 "simplified to a constant value", &start->where);
      retval = false;
      goto cleanup;
    }
  if (!gfc_simplify_expr (step, 1)
      || step->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("step of implied-do loop at %L could not be "
		 "simplified to a constant value", &start->where);
      retval = false;
      goto cleanup;
    }

  mpz_set (trip, end->value.integer);
  mpz_sub (trip, trip, start->value.integer);
  mpz_add (trip, trip, step->value.integer);

  mpz_div (trip, trip, step->value.integer);

  mpz_set (frame.value, start->value.integer);

  frame.prev = iter_stack;
  frame.variable = var->iter.var->symtree;
  iter_stack = &frame;

  while (mpz_cmp_ui (trip, 0) > 0)
    {
      if (!traverse_data_var (var->list, where))
	{
	  retval = false;
	  goto cleanup;
	}

      e = gfc_copy_expr (var->expr);
      if (!gfc_simplify_expr (e, 1))
	{
	  gfc_free_expr (e);
	  retval = false;
	  goto cleanup;
	}

      mpz_add (frame.value, frame.value, step->value.integer);

      mpz_sub_ui (trip, trip, 1);
    }

cleanup:
  mpz_clear (frame.value);
  mpz_clear (trip);

  gfc_free_expr (start);
  gfc_free_expr (end);
  gfc_free_expr (step);

  iter_stack = frame.prev;
  return retval;
}


/* Type resolve variables in the variable list of a DATA statement.  */

static bool
traverse_data_var (gfc_data_variable *var, locus *where)
{
  bool t;

  for (; var; var = var->next)
    {
      if (var->expr == NULL)
	t = traverse_data_list (var, where);
      else
	t = check_data_variable (var, where);

      if (!t)
	return false;
    }

  return true;
}


/* Resolve the expressions and iterators associated with a data statement.
   This is separate from the assignment checking because data lists should
   only be resolved once.  */

static bool
resolve_data_variables (gfc_data_variable *d)
{
  for (; d; d = d->next)
    {
      if (d->list == NULL)
	{
	  if (!gfc_resolve_expr (d->expr))
	    return false;
	}
      else
	{
	  if (!gfc_resolve_iterator (&d->iter, false, true))
	    return false;

	  if (!resolve_data_variables (d->list))
	    return false;
	}
    }

  return true;
}


/* Resolve a single DATA statement.  We implement this by storing a pointer to
   the value list into static variables, and then recursively traversing the
   variables list, expanding iterators and such.  */

static void
resolve_data (gfc_data *d)
{

  if (!resolve_data_variables (d->var))
    return;

  values.vnode = d->value;
  if (d->value == NULL)
    mpz_set_ui (values.left, 0);
  else
    mpz_set (values.left, d->value->repeat);

  if (!traverse_data_var (d->var, &d->where))
    return;

  /* At this point, we better not have any values left.  */

  if (next_data_value ())
    gfc_error ("DATA statement at %L has more values than variables",
	       &d->where);
}


/* 12.6 Constraint: In a pure subprogram any variable which is in common or
   accessed by host or use association, is a dummy argument to a pure function,
   is a dummy argument with INTENT (IN) to a pure subroutine, or an object that
   is storage associated with any such variable, shall not be used in the
   following contexts: (clients of this function).  */

/* Determines if a variable is not 'pure', i.e., not assignable within a pure
   procedure.  Returns zero if assignment is OK, nonzero if there is a
   problem.  */
int
gfc_impure_variable (gfc_symbol *sym)
{
  gfc_symbol *proc;
  gfc_namespace *ns;

  if (sym->attr.use_assoc || sym->attr.in_common)
    return 1;

  /* Check if the symbol's ns is inside the pure procedure.  */
  for (ns = gfc_current_ns; ns; ns = ns->parent)
    {
      if (ns == sym->ns)
	break;
      if (ns->proc_name->attr.flavor == FL_PROCEDURE && !sym->attr.function)
	return 1;
    }

  proc = sym->ns->proc_name;
  if (sym->attr.dummy
      && ((proc->attr.subroutine && sym->attr.intent == INTENT_IN)
	  || proc->attr.function))
    return 1;

  /* TODO: Sort out what can be storage associated, if anything, and include
     it here.  In principle equivalences should be scanned but it does not
     seem to be possible to storage associate an impure variable this way.  */
  return 0;
}


/* Test whether a symbol is pure or not.  For a NULL pointer, checks if the
   current namespace is inside a pure procedure.  */

int
gfc_pure (gfc_symbol *sym)
{
  symbol_attribute attr;
  gfc_namespace *ns;

  if (sym == NULL)
    {
      /* Check if the current namespace or one of its parents
	belongs to a pure procedure.  */
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	{
	  sym = ns->proc_name;
	  if (sym == NULL)
	    return 0;
	  attr = sym->attr;
	  if (attr.flavor == FL_PROCEDURE && attr.pure)
	    return 1;
	}
      return 0;
    }

  attr = sym->attr;

  return attr.flavor == FL_PROCEDURE && attr.pure;
}


/* Test whether a symbol is implicitly pure or not.  For a NULL pointer,
   checks if the current namespace is implicitly pure.  Note that this
   function returns false for a PURE procedure.  */

int
gfc_implicit_pure (gfc_symbol *sym)
{
  gfc_namespace *ns;

  if (sym == NULL)
    {
      /* Check if the current procedure is implicit_pure.  Walk up
	 the procedure list until we find a procedure.  */
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	{
	  sym = ns->proc_name;
	  if (sym == NULL)
	    return 0;

	  if (sym->attr.flavor == FL_PROCEDURE)
	    break;
	}
    }

  return sym->attr.flavor == FL_PROCEDURE && sym->attr.implicit_pure
    && !sym->attr.pure;
}


void
gfc_unset_implicit_pure (gfc_symbol *sym)
{
  gfc_namespace *ns;

  if (sym == NULL)
    {
      /* Check if the current procedure is implicit_pure.  Walk up
	 the procedure list until we find a procedure.  */
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	{
	  sym = ns->proc_name;
	  if (sym == NULL)
	    return;

	  if (sym->attr.flavor == FL_PROCEDURE)
	    break;
	}
    }

  if (sym->attr.flavor == FL_PROCEDURE)
    sym->attr.implicit_pure = 0;
  else
    sym->attr.pure = 0;
}


/* Test whether the current procedure is elemental or not.  */

int
gfc_elemental (gfc_symbol *sym)
{
  symbol_attribute attr;

  if (sym == NULL)
    sym = gfc_current_ns->proc_name;
  if (sym == NULL)
    return 0;
  attr = sym->attr;

  return attr.flavor == FL_PROCEDURE && attr.elemental;
}


/* Warn about unused labels.  */

static void
warn_unused_fortran_label (gfc_st_label *label)
{
  if (label == NULL)
    return;

  warn_unused_fortran_label (label->left);

  if (label->defined == ST_LABEL_UNKNOWN)
    return;

  switch (label->referenced)
    {
    case ST_LABEL_UNKNOWN:
      gfc_warning (OPT_Wunused_label, "Label %d at %L defined but not used",
		   label->value, &label->where);
      break;

    case ST_LABEL_BAD_TARGET:
      gfc_warning (OPT_Wunused_label,
		   "Label %d at %L defined but cannot be used",
		   label->value, &label->where);
      break;

    default:
      break;
    }

  warn_unused_fortran_label (label->right);
}


/* Returns the sequence type of a symbol or sequence.  */

static seq_type
sequence_type (gfc_typespec ts)
{
  seq_type result;
  gfc_component *c;

  switch (ts.type)
  {
    case BT_DERIVED:

      if (ts.u.derived->components == NULL)
	return SEQ_NONDEFAULT;

      result = sequence_type (ts.u.derived->components->ts);
      for (c = ts.u.derived->components->next; c; c = c->next)
	if (sequence_type (c->ts) != result)
	  return SEQ_MIXED;

      return result;

    case BT_CHARACTER:
      if (ts.kind != gfc_default_character_kind)
	  return SEQ_NONDEFAULT;

      return SEQ_CHARACTER;

    case BT_INTEGER:
      if (ts.kind != gfc_default_integer_kind)
	  return SEQ_NONDEFAULT;

      return SEQ_NUMERIC;

    case BT_REAL:
      if (!(ts.kind == gfc_default_real_kind
	    || ts.kind == gfc_default_double_kind))
	  return SEQ_NONDEFAULT;

      return SEQ_NUMERIC;

    case BT_COMPLEX:
      if (ts.kind != gfc_default_complex_kind)
	  return SEQ_NONDEFAULT;

      return SEQ_NUMERIC;

    case BT_LOGICAL:
      if (ts.kind != gfc_default_logical_kind)
	  return SEQ_NONDEFAULT;

      return SEQ_NUMERIC;

    default:
      return SEQ_NONDEFAULT;
  }
}


/* Resolve derived type EQUIVALENCE object.  */

static bool
resolve_equivalence_derived (gfc_symbol *derived, gfc_symbol *sym, gfc_expr *e)
{
  gfc_component *c = derived->components;

  if (!derived)
    return true;

  /* Shall not be an object of nonsequence derived type.  */
  if (!derived->attr.sequence)
    {
      gfc_error ("Derived type variable %qs at %L must have SEQUENCE "
		 "attribute to be an EQUIVALENCE object", sym->name,
		 &e->where);
      return false;
    }

  /* Shall not have allocatable components.  */
  if (derived->attr.alloc_comp)
    {
      gfc_error ("Derived type variable %qs at %L cannot have ALLOCATABLE "
		 "components to be an EQUIVALENCE object",sym->name,
		 &e->where);
      return false;
    }

  if (sym->attr.in_common && gfc_has_default_initializer (sym->ts.u.derived))
    {
      gfc_error ("Derived type variable %qs at %L with default "
		 "initialization cannot be in EQUIVALENCE with a variable "
		 "in COMMON", sym->name, &e->where);
      return false;
    }

  for (; c ; c = c->next)
    {
      if (gfc_bt_struct (c->ts.type)
	  && (!resolve_equivalence_derived(c->ts.u.derived, sym, e)))
	return false;

      /* Shall not be an object of sequence derived type containing a pointer
	 in the structure.  */
      if (c->attr.pointer)
	{
	  gfc_error ("Derived type variable %qs at %L with pointer "
		     "component(s) cannot be an EQUIVALENCE object",
		     sym->name, &e->where);
	  return false;
	}
    }
  return true;
}


/* Resolve equivalence object.
   An EQUIVALENCE object shall not be a dummy argument, a pointer, a target,
   an allocatable array, an object of nonsequence derived type, an object of
   sequence derived type containing a pointer at any level of component
   selection, an automatic object, a function name, an entry name, a result
   name, a named constant, a structure component, or a subobject of any of
   the preceding objects.  A substring shall not have length zero.  A
   derived type shall not have components with default initialization nor
   shall two objects of an equivalence group be initialized.
   Either all or none of the objects shall have an protected attribute.
   The simple constraints are done in symbol.c(check_conflict) and the rest
   are implemented here.  */

static void
resolve_equivalence (gfc_equiv *eq)
{
  gfc_symbol *sym;
  gfc_symbol *first_sym;
  gfc_expr *e;
  gfc_ref *r;
  locus *last_where = NULL;
  seq_type eq_type, last_eq_type;
  gfc_typespec *last_ts;
  int object, cnt_protected;
  const char *msg;

  last_ts = &eq->expr->symtree->n.sym->ts;

  first_sym = eq->expr->symtree->n.sym;

  cnt_protected = 0;

  for (object = 1; eq; eq = eq->eq, object++)
    {
      e = eq->expr;

      e->ts = e->symtree->n.sym->ts;
      /* match_varspec might not know yet if it is seeing
	 array reference or substring reference, as it doesn't
	 know the types.  */
      if (e->ref && e->ref->type == REF_ARRAY)
	{
	  gfc_ref *ref = e->ref;
	  sym = e->symtree->n.sym;

	  if (sym->attr.dimension)
	    {
	      ref->u.ar.as = sym->as;
	      ref = ref->next;
	    }

	  /* For substrings, convert REF_ARRAY into REF_SUBSTRING.  */
	  if (e->ts.type == BT_CHARACTER
	      && ref
	      && ref->type == REF_ARRAY
	      && ref->u.ar.dimen == 1
	      && ref->u.ar.dimen_type[0] == DIMEN_RANGE
	      && ref->u.ar.stride[0] == NULL)
	    {
	      gfc_expr *start = ref->u.ar.start[0];
	      gfc_expr *end = ref->u.ar.end[0];
	      void *mem = NULL;

	      /* Optimize away the (:) reference.  */
	      if (start == NULL && end == NULL)
		{
		  if (e->ref == ref)
		    e->ref = ref->next;
		  else
		    e->ref->next = ref->next;
		  mem = ref;
		}
	      else
		{
		  ref->type = REF_SUBSTRING;
		  if (start == NULL)
		    start = gfc_get_int_expr (gfc_default_integer_kind,
					      NULL, 1);
		  ref->u.ss.start = start;
		  if (end == NULL && e->ts.u.cl)
		    end = gfc_copy_expr (e->ts.u.cl->length);
		  ref->u.ss.end = end;
		  ref->u.ss.length = e->ts.u.cl;
		  e->ts.u.cl = NULL;
		}
	      ref = ref->next;
	      free (mem);
	    }

	  /* Any further ref is an error.  */
	  if (ref)
	    {
	      gcc_assert (ref->type == REF_ARRAY);
	      gfc_error ("Syntax error in EQUIVALENCE statement at %L",
			 &ref->u.ar.where);
	      continue;
	    }
	}

      if (!gfc_resolve_expr (e))
	continue;

      sym = e->symtree->n.sym;

      if (sym->attr.is_protected)
	cnt_protected++;
      if (cnt_protected > 0 && cnt_protected != object)
       	{
	      gfc_error ("Either all or none of the objects in the "
			 "EQUIVALENCE set at %L shall have the "
			 "PROTECTED attribute",
			 &e->where);
	      break;
	}

      /* Shall not equivalence common block variables in a PURE procedure.  */
      if (sym->ns->proc_name
	  && sym->ns->proc_name->attr.pure
	  && sym->attr.in_common)
	{
	  /* Need to check for symbols that may have entered the pure
	     procedure via a USE statement.  */
	  bool saw_sym = false;
	  if (sym->ns->use_stmts)
	    {
	      gfc_use_rename *r;
	      for (r = sym->ns->use_stmts->rename; r; r = r->next)
		if (strcmp(r->use_name, sym->name) == 0) saw_sym = true;
	    }
	  else
	    saw_sym = true;

	  if (saw_sym)
	    gfc_error ("COMMON block member %qs at %L cannot be an "
		       "EQUIVALENCE object in the pure procedure %qs",
		       sym->name, &e->where, sym->ns->proc_name->name);
	  break;
	}

      /* Shall not be a named constant.  */
      if (e->expr_type == EXPR_CONSTANT)
	{
	  gfc_error ("Named constant %qs at %L cannot be an EQUIVALENCE "
		     "object", sym->name, &e->where);
	  continue;
	}

      if (e->ts.type == BT_DERIVED
	  && !resolve_equivalence_derived (e->ts.u.derived, sym, e))
	continue;

      /* Check that the types correspond correctly:
	 Note 5.28:
	 A numeric sequence structure may be equivalenced to another sequence
	 structure, an object of default integer type, default real type, double
	 precision real type, default logical type such that components of the
	 structure ultimately only become associated to objects of the same
	 kind. A character sequence structure may be equivalenced to an object
	 of default character kind or another character sequence structure.
	 Other objects may be equivalenced only to objects of the same type and
	 kind parameters.  */

      /* Identical types are unconditionally OK.  */
      if (object == 1 || gfc_compare_types (last_ts, &sym->ts))
	goto identical_types;

      last_eq_type = sequence_type (*last_ts);
      eq_type = sequence_type (sym->ts);

      /* Since the pair of objects is not of the same type, mixed or
	 non-default sequences can be rejected.  */

      msg = "Sequence %s with mixed components in EQUIVALENCE "
	    "statement at %L with different type objects";
      if ((object ==2
	   && last_eq_type == SEQ_MIXED
	   && !gfc_notify_std (GFC_STD_GNU, msg, first_sym->name, last_where))
	  || (eq_type == SEQ_MIXED
	      && !gfc_notify_std (GFC_STD_GNU, msg, sym->name, &e->where)))
	continue;

      msg = "Non-default type object or sequence %s in EQUIVALENCE "
	    "statement at %L with objects of different type";
      if ((object ==2
	   && last_eq_type == SEQ_NONDEFAULT
	   && !gfc_notify_std (GFC_STD_GNU, msg, first_sym->name, last_where))
	  || (eq_type == SEQ_NONDEFAULT
	      && !gfc_notify_std (GFC_STD_GNU, msg, sym->name, &e->where)))
	continue;

      msg ="Non-CHARACTER object %qs in default CHARACTER "
	   "EQUIVALENCE statement at %L";
      if (last_eq_type == SEQ_CHARACTER
	  && eq_type != SEQ_CHARACTER
	  && !gfc_notify_std (GFC_STD_GNU, msg, sym->name, &e->where))
		continue;

      msg ="Non-NUMERIC object %qs in default NUMERIC "
	   "EQUIVALENCE statement at %L";
      if (last_eq_type == SEQ_NUMERIC
	  && eq_type != SEQ_NUMERIC
	  && !gfc_notify_std (GFC_STD_GNU, msg, sym->name, &e->where))
		continue;

  identical_types:
      last_ts =&sym->ts;
      last_where = &e->where;

      if (!e->ref)
	continue;

      /* Shall not be an automatic array.  */
      if (e->ref->type == REF_ARRAY
	  && !gfc_resolve_array_spec (e->ref->u.ar.as, 1))
	{
	  gfc_error ("Array %qs at %L with non-constant bounds cannot be "
		     "an EQUIVALENCE object", sym->name, &e->where);
	  continue;
	}

      r = e->ref;
      while (r)
	{
	  /* Shall not be a structure component.  */
	  if (r->type == REF_COMPONENT)
	    {
	      gfc_error ("Structure component %qs at %L cannot be an "
			 "EQUIVALENCE object",
			 r->u.c.component->name, &e->where);
	      break;
	    }

	  /* A substring shall not have length zero.  */
	  if (r->type == REF_SUBSTRING)
	    {
	      if (compare_bound (r->u.ss.start, r->u.ss.end) == CMP_GT)
		{
		  gfc_error ("Substring at %L has length zero",
			     &r->u.ss.start->where);
		  break;
		}
	    }
	  r = r->next;
	}
    }
}


/* Function called by resolve_fntype to flag other symbol used in the
   length type parameter specification of function resuls.  */

static bool
flag_fn_result_spec (gfc_expr *expr,
                     gfc_symbol *sym ATTRIBUTE_UNUSED,
                     int *f ATTRIBUTE_UNUSED)
{
  gfc_namespace *ns;
  gfc_symbol *s;

  if (expr->expr_type == EXPR_VARIABLE)
    {
      s = expr->symtree->n.sym;
      for (ns = s->ns; ns; ns = ns->parent)
	if (!ns->parent)
	  break;

      if (!s->fn_result_spec
	  && s->attr.flavor == FL_PARAMETER)
	{
	  /* Function contained in a module.... */
	  if (ns->proc_name && ns->proc_name->attr.flavor == FL_MODULE)
	    {
	      gfc_symtree *st;
	      s->fn_result_spec = 1;
	      /* Make sure that this symbol is translated as a module
		 variable.  */
	      st = gfc_get_unique_symtree (ns);
	      st->n.sym = s;
	      s->refs++;
	    }
	  /* ... which is use associated and called.  */
	  else if (s->attr.use_assoc || s->attr.used_in_submodule
			||
		  /* External function matched with an interface.  */
		  (s->ns->proc_name
		   && ((s->ns == ns
			 && s->ns->proc_name->attr.if_source == IFSRC_DECL)
		       || s->ns->proc_name->attr.if_source == IFSRC_IFBODY)
		   && s->ns->proc_name->attr.function))
	    s->fn_result_spec = 1;
	}
    }
  return false;
}


/* Resolve function and ENTRY types, issue diagnostics if needed.  */

static void
resolve_fntype (gfc_namespace *ns)
{
  gfc_entry_list *el;
  gfc_symbol *sym;

  if (ns->proc_name == NULL || !ns->proc_name->attr.function)
    return;

  /* If there are any entries, ns->proc_name is the entry master
     synthetic symbol and ns->entries->sym actual FUNCTION symbol.  */
  if (ns->entries)
    sym = ns->entries->sym;
  else
    sym = ns->proc_name;
  if (sym->result == sym
      && sym->ts.type == BT_UNKNOWN
      && !gfc_set_default_type (sym, 0, NULL)
      && !sym->attr.untyped)
    {
      gfc_error ("Function %qs at %L has no IMPLICIT type",
		 sym->name, &sym->declared_at);
      sym->attr.untyped = 1;
    }

  if (sym->ts.type == BT_DERIVED && !sym->ts.u.derived->attr.use_assoc
      && !sym->attr.contained
      && !gfc_check_symbol_access (sym->ts.u.derived)
      && gfc_check_symbol_access (sym))
    {
      gfc_notify_std (GFC_STD_F2003, "PUBLIC function %qs at "
		      "%L of PRIVATE type %qs", sym->name,
		      &sym->declared_at, sym->ts.u.derived->name);
    }

    if (ns->entries)
    for (el = ns->entries->next; el; el = el->next)
      {
	if (el->sym->result == el->sym
	    && el->sym->ts.type == BT_UNKNOWN
	    && !gfc_set_default_type (el->sym, 0, NULL)
	    && !el->sym->attr.untyped)
	  {
	    gfc_error ("ENTRY %qs at %L has no IMPLICIT type",
		       el->sym->name, &el->sym->declared_at);
	    el->sym->attr.untyped = 1;
	  }
      }

  if (sym->ts.type == BT_CHARACTER)
    gfc_traverse_expr (sym->ts.u.cl->length, NULL, flag_fn_result_spec, 0);
}


/* 12.3.2.1.1 Defined operators.  */

static bool
check_uop_procedure (gfc_symbol *sym, locus where)
{
  gfc_formal_arglist *formal;

  if (!sym->attr.function)
    {
      gfc_error ("User operator procedure %qs at %L must be a FUNCTION",
		 sym->name, &where);
      return false;
    }

  if (sym->ts.type == BT_CHARACTER
      && !((sym->ts.u.cl && sym->ts.u.cl->length) || sym->ts.deferred)
      && !(sym->result && ((sym->result->ts.u.cl
	   && sym->result->ts.u.cl->length) || sym->result->ts.deferred)))
    {
      gfc_error ("User operator procedure %qs at %L cannot be assumed "
		 "character length", sym->name, &where);
      return false;
    }

  formal = gfc_sym_get_dummy_args (sym);
  if (!formal || !formal->sym)
    {
      gfc_error ("User operator procedure %qs at %L must have at least "
		 "one argument", sym->name, &where);
      return false;
    }

  if (formal->sym->attr.intent != INTENT_IN)
    {
      gfc_error ("First argument of operator interface at %L must be "
		 "INTENT(IN)", &where);
      return false;
    }

  if (formal->sym->attr.optional)
    {
      gfc_error ("First argument of operator interface at %L cannot be "
		 "optional", &where);
      return false;
    }

  formal = formal->next;
  if (!formal || !formal->sym)
    return true;

  if (formal->sym->attr.intent != INTENT_IN)
    {
      gfc_error ("Second argument of operator interface at %L must be "
		 "INTENT(IN)", &where);
      return false;
    }

  if (formal->sym->attr.optional)
    {
      gfc_error ("Second argument of operator interface at %L cannot be "
		 "optional", &where);
      return false;
    }

  if (formal->next)
    {
      gfc_error ("Operator interface at %L must have, at most, two "
		 "arguments", &where);
      return false;
    }

  return true;
}

static void
gfc_resolve_uops (gfc_symtree *symtree)
{
  gfc_interface *itr;

  if (symtree == NULL)
    return;

  gfc_resolve_uops (symtree->left);
  gfc_resolve_uops (symtree->right);

  for (itr = symtree->n.uop->op; itr; itr = itr->next)
    check_uop_procedure (itr->sym, itr->sym->declared_at);
}


/* Examine all of the expressions associated with a program unit,
   assign types to all intermediate expressions, make sure that all
   assignments are to compatible types and figure out which names
   refer to which functions or subroutines.  It doesn't check code
   block, which is handled by gfc_resolve_code.  */

static void
resolve_types (gfc_namespace *ns)
{
  gfc_namespace *n;
  gfc_charlen *cl;
  gfc_data *d;
  gfc_equiv *eq;
  gfc_namespace* old_ns = gfc_current_ns;

  if (ns->types_resolved)
    return;

  /* Check that all IMPLICIT types are ok.  */
  if (!ns->seen_implicit_none)
    {
      unsigned letter;
      for (letter = 0; letter != GFC_LETTERS; ++letter)
	if (ns->set_flag[letter]
	    && !resolve_typespec_used (&ns->default_type[letter],
				       &ns->implicit_loc[letter], NULL))
	  return;
    }

  gfc_current_ns = ns;

  resolve_entries (ns);

  resolve_common_vars (&ns->blank_common, false);
  resolve_common_blocks (ns->common_root);

  resolve_contained_functions (ns);

  if (ns->proc_name && ns->proc_name->attr.flavor == FL_PROCEDURE
      && ns->proc_name->attr.if_source == IFSRC_IFBODY)
    resolve_formal_arglist (ns->proc_name);

  gfc_traverse_ns (ns, resolve_bind_c_derived_types);

  for (cl = ns->cl_list; cl; cl = cl->next)
    resolve_charlen (cl);

  gfc_traverse_ns (ns, resolve_symbol);

  resolve_fntype (ns);

  for (n = ns->contained; n; n = n->sibling)
    {
      if (gfc_pure (ns->proc_name) && !gfc_pure (n->proc_name))
	gfc_error ("Contained procedure %qs at %L of a PURE procedure must "
		   "also be PURE", n->proc_name->name,
		   &n->proc_name->declared_at);

      resolve_types (n);
    }

  forall_flag = 0;
  gfc_do_concurrent_flag = 0;
  gfc_check_interfaces (ns);

  gfc_traverse_ns (ns, resolve_values);

  if (ns->save_all)
    gfc_save_all (ns);

  iter_stack = NULL;
  for (d = ns->data; d; d = d->next)
    resolve_data (d);

  iter_stack = NULL;
  gfc_traverse_ns (ns, gfc_formalize_init_value);

  gfc_traverse_ns (ns, gfc_verify_binding_labels);

  for (eq = ns->equiv; eq; eq = eq->next)
    resolve_equivalence (eq);

  /* Warn about unused labels.  */
  if (warn_unused_label)
    warn_unused_fortran_label (ns->st_labels);

  gfc_resolve_uops (ns->uop_root);

  gfc_traverse_ns (ns, gfc_verify_DTIO_procedures);

  gfc_resolve_omp_declare_simd (ns);

  gfc_resolve_omp_udrs (ns->omp_udr_root);

  ns->types_resolved = 1;

  gfc_current_ns = old_ns;
}


/* Call gfc_resolve_code recursively.  */

static void
resolve_codes (gfc_namespace *ns)
{
  gfc_namespace *n;
  bitmap_obstack old_obstack;

  if (ns->resolved == 1)
    return;

  for (n = ns->contained; n; n = n->sibling)
    resolve_codes (n);

  gfc_current_ns = ns;

  /* Don't clear 'cs_base' if this is the namespace of a BLOCK construct.  */
  if (!(ns->proc_name && ns->proc_name->attr.flavor == FL_LABEL))
    cs_base = NULL;

  /* Set to an out of range value.  */
  current_entry_id = -1;

  old_obstack = labels_obstack;
  bitmap_obstack_initialize (&labels_obstack);

  gfc_resolve_oacc_declare (ns);
  gfc_resolve_code (ns->code, ns);

  bitmap_obstack_release (&labels_obstack);
  labels_obstack = old_obstack;
}


/* This function is called after a complete program unit has been compiled.
   Its purpose is to examine all of the expressions associated with a program
   unit, assign types to all intermediate expressions, make sure that all
   assignments are to compatible types and figure out which names refer to
   which functions or subroutines.  */

void
gfc_resolve (gfc_namespace *ns)
{
  gfc_namespace *old_ns;
  code_stack *old_cs_base;
  struct gfc_omp_saved_state old_omp_state;

  if (ns->resolved)
    return;

  ns->resolved = -1;
  old_ns = gfc_current_ns;
  old_cs_base = cs_base;

  /* As gfc_resolve can be called during resolution of an OpenMP construct
     body, we should clear any state associated to it, so that say NS's
     DO loops are not interpreted as OpenMP loops.  */
  if (!ns->construct_entities)
    gfc_omp_save_and_clear_state (&old_omp_state);

  resolve_types (ns);
  component_assignment_level = 0;
  resolve_codes (ns);

  gfc_current_ns = old_ns;
  cs_base = old_cs_base;
  ns->resolved = 1;

  gfc_run_passes (ns);

  if (!ns->construct_entities)
    gfc_omp_restore_state (&old_omp_state);
}
