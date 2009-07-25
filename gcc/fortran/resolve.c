/* Perform type resolution on the various structures.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
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
#include "obstack.h"
#include "bitmap.h"
#include "arith.h"  /* For gfc_compare_expr().  */
#include "dependency.h"
#include "data.h"
#include "target-memory.h" /* for gfc_simplify_transfer */

/* Types used in equivalence statements.  */

typedef enum seq_type
{
  SEQ_NONDEFAULT, SEQ_NUMERIC, SEQ_CHARACTER, SEQ_MIXED
}
seq_type;

/* Stack to keep track of the nesting of blocks as we move through the
   code.  See resolve_branch() and resolve_code().  */

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


/* Nonzero if we're inside a FORALL block.  */

static int forall_flag;

/* Nonzero if we're inside a OpenMP WORKSHARE or PARALLEL WORKSHARE block.  */

static int omp_workshare_flag;

/* Nonzero if we are processing a formal arglist. The corresponding function
   resets the flag each time that it is read.  */
static int formal_arg_flag = 0;

/* True if we are resolving a specification expression.  */
static int specification_expr = 0;

/* The id of the last entry seen.  */
static int current_entry_id;

/* We use bitmaps to determine if a branch target is valid.  */
static bitmap_obstack labels_obstack;

int
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

static gfc_try
resolve_typespec_used (gfc_typespec* ts, locus* where, const char* name)
{
  if (ts->type == BT_DERIVED && ts->derived->attr.abstract)
    {
      if (where)
	{
	  if (name)
	    gfc_error ("'%s' at %L is of the ABSTRACT type '%s'",
		       name, where, ts->derived->name);
	  else
	    gfc_error ("ABSTRACT type '%s' used at %L",
		       ts->derived->name, where);
	}

      return FAILURE;
    }

  return SUCCESS;
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
  int i;

  if (proc->result != NULL)
    sym = proc->result;
  else
    sym = proc;

  if (gfc_elemental (proc)
      || sym->attr.pointer || sym->attr.allocatable
      || (sym->as && sym->as->rank > 0))
    {
      proc->attr.always_explicit = 1;
      sym->attr.always_explicit = 1;
    }

  formal_arg_flag = 1;

  for (f = proc->formal; f; f = f->next)
    {
      sym = f->sym;

      if (sym == NULL)
	{
	  /* Alternate return placeholder.  */
	  if (gfc_elemental (proc))
	    gfc_error ("Alternate return specifier in elemental subroutine "
		       "'%s' at %L is not allowed", proc->name,
		       &proc->declared_at);
	  if (proc->attr.function)
	    gfc_error ("Alternate return specifier in function "
		       "'%s' at %L is not allowed", proc->name,
		       &proc->declared_at);
	  continue;
	}

      if (sym->attr.if_source != IFSRC_UNKNOWN)
	resolve_formal_arglist (sym);

      if (sym->attr.subroutine || sym->attr.external || sym->attr.intrinsic)
	{
	  if (gfc_pure (proc) && !gfc_pure (sym))
	    {
	      gfc_error ("Dummy procedure '%s' of PURE procedure at %L must "
			 "also be PURE", sym->name, &sym->declared_at);
	      continue;
	    }

	  if (gfc_elemental (proc))
	    {
	      gfc_error ("Dummy procedure at %L not allowed in ELEMENTAL "
			 "procedure", &sym->declared_at);
	      continue;
	    }

	  if (sym->attr.function
		&& sym->ts.type == BT_UNKNOWN
		&& sym->attr.intrinsic)
	    {
	      gfc_intrinsic_sym *isym;
	      isym = gfc_find_function (sym->name);
	      if (isym == NULL || !isym->specific)
		{
		  gfc_error ("Unable to find a specific INTRINSIC procedure "
			     "for the reference '%s' at %L", sym->name,
			     &sym->declared_at);
		}
	      sym->ts = isym->ts;
	    }

	  continue;
	}

      if (sym->ts.type == BT_UNKNOWN)
	{
	  if (!sym->attr.function || sym->result == sym)
	    gfc_set_default_type (sym, 1, sym->ns);
	}

      gfc_resolve_array_spec (sym->as, 0);

      /* We can't tell if an array with dimension (:) is assumed or deferred
	 shape until we know if it has the pointer or allocatable attributes.
      */
      if (sym->as && sym->as->rank > 0 && sym->as->type == AS_DEFERRED
	  && !(sym->attr.pointer || sym->attr.allocatable))
	{
	  sym->as->type = AS_ASSUMED_SHAPE;
	  for (i = 0; i < sym->as->rank; i++)
	    sym->as->lower[i] = gfc_int_expr (1);
	}

      if ((sym->as && sym->as->rank > 0 && sym->as->type == AS_ASSUMED_SHAPE)
	  || sym->attr.pointer || sym->attr.allocatable || sym->attr.target
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

      if (gfc_pure (proc) && !sym->attr.pointer
	  && sym->attr.flavor != FL_PROCEDURE)
	{
	  if (proc->attr.function && sym->attr.intent != INTENT_IN)
	    gfc_error ("Argument '%s' of pure function '%s' at %L must be "
		       "INTENT(IN)", sym->name, proc->name,
		       &sym->declared_at);

	  if (proc->attr.subroutine && sym->attr.intent == INTENT_UNKNOWN)
	    gfc_error ("Argument '%s' of pure subroutine '%s' at %L must "
		       "have its INTENT specified", sym->name, proc->name,
		       &sym->declared_at);
	}

      if (gfc_elemental (proc))
	{
	  if (sym->as != NULL)
	    {
	      gfc_error ("Argument '%s' of elemental procedure at %L must "
			 "be scalar", sym->name, &sym->declared_at);
	      continue;
	    }

	  if (sym->attr.pointer)
	    {
	      gfc_error ("Argument '%s' of elemental procedure at %L cannot "
			 "have the POINTER attribute", sym->name,
			 &sym->declared_at);
	      continue;
	    }

	  if (sym->attr.flavor == FL_PROCEDURE)
	    {
	      gfc_error ("Dummy procedure '%s' not allowed in elemental "
			 "procedure '%s' at %L", sym->name, proc->name,
			 &sym->declared_at);
	      continue;
	    }
	}

      /* Each dummy shall be specified to be scalar.  */
      if (proc->attr.proc == PROC_ST_FUNCTION)
	{
	  if (sym->as != NULL)
	    {
	      gfc_error ("Argument '%s' of statement function at %L must "
			 "be scalar", sym->name, &sym->declared_at);
	      continue;
	    }

	  if (sym->ts.type == BT_CHARACTER)
	    {
	      gfc_charlen *cl = sym->ts.cl;
	      if (!cl || !cl->length || cl->length->expr_type != EXPR_CONSTANT)
		{
		  gfc_error ("Character-valued argument '%s' of statement "
			     "function at %L must have constant length",
			     sym->name, &sym->declared_at);
		  continue;
		}
	    }
	}
    }
  formal_arg_flag = 0;
}


/* Work function called when searching for symbols that have argument lists
   associated with them.  */

static void
find_arglists (gfc_symbol *sym)
{
  if (sym->attr.if_source == IFSRC_UNKNOWN || sym->ns != gfc_current_ns)
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
  gfc_try t;

  /* If this namespace is not a function or an entry master function,
     ignore it.  */
  if (! sym || !(sym->attr.function || sym->attr.flavor == FL_VARIABLE)
      || sym->attr.entry_master)
    return;

  /* Try to find out of what the return type is.  */
  if (sym->result->ts.type == BT_UNKNOWN && sym->result->ts.interface == NULL)
    {
      t = gfc_set_default_type (sym->result, 0, ns);

      if (t == FAILURE && !sym->result->attr.untyped)
	{
	  if (sym->result == sym)
	    gfc_error ("Contained function '%s' at %L has no IMPLICIT type",
		       sym->name, &sym->declared_at);
	  else if (!sym->result->attr.proc_pointer)
	    gfc_error ("Result '%s' of contained function '%s' at %L has "
		       "no IMPLICIT type", sym->result->name, sym->name,
		       &sym->result->declared_at);
	  sym->result->attr.untyped = 1;
	}
    }

  /* Fortran 95 Draft Standard, page 51, Section 5.1.1.5, on the Character 
     type, lists the only ways a character length value of * can be used:
     dummy arguments of procedures, named constants, and function results
     in external functions.  Internal function results are not on that list;
     ergo, not permitted.  */

  if (sym->result->ts.type == BT_CHARACTER)
    {
      gfc_charlen *cl = sym->result->ts.cl;
      if (!cl || !cl->length)
	gfc_error ("Character-valued internal function '%s' at %L must "
		   "not be assumed length", sym->name, &sym->declared_at);
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
  c = gfc_get_code ();
  c->op = EXEC_ENTRY;
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
	  else if (ts->type == BT_CHARACTER && ts->cl && fts->cl
		   && (((ts->cl->length && !fts->cl->length)
			||(!ts->cl->length && fts->cl->length))
		       || (ts->cl->length
			   && ts->cl->length->expr_type
			      != fts->cl->length->expr_type)
		       || (ts->cl->length
			   && ts->cl->length->expr_type == EXPR_CONSTANT
		           && mpz_cmp (ts->cl->length->value.integer,
				       fts->cl->length->value.integer) != 0)))
	    gfc_notify_std (GFC_STD_GNU, "Extension: Function %s at %L with "
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


static bool
has_default_initializer (gfc_symbol *der)
{
  gfc_component *c;

  gcc_assert (der->attr.flavor == FL_DERIVED);
  for (c = der->components; c; c = c->next)
    if ((c->ts.type != BT_DERIVED && c->initializer)
	|| (c->ts.type == BT_DERIVED
	    && (!c->attr.pointer && has_default_initializer (c->ts.derived))))
      break;

  return c != NULL;
}

/* Resolve common variables.  */
static void
resolve_common_vars (gfc_symbol *sym, bool named_common)
{
  gfc_symbol *csym = sym;

  for (; csym; csym = csym->common_next)
    {
      if (csym->value || csym->attr.data)
	{
	  if (!csym->ns->is_block_data)
	    gfc_notify_std (GFC_STD_GNU, "Variable '%s' at %L is in COMMON "
			    "but only in BLOCK DATA initialization is "
			    "allowed", csym->name, &csym->declared_at);
	  else if (!named_common)
	    gfc_notify_std (GFC_STD_GNU, "Initialized variable '%s' at %L is "
			    "in a blank COMMON but initialization is only "
			    "allowed in named common blocks", csym->name,
			    &csym->declared_at);
	}

      if (csym->ts.type != BT_DERIVED)
	continue;

      if (!(csym->ts.derived->attr.sequence
	    || csym->ts.derived->attr.is_bind_c))
	gfc_error_now ("Derived type variable '%s' in COMMON at %L "
		       "has neither the SEQUENCE nor the BIND(C) "
		       "attribute", csym->name, &csym->declared_at);
      if (csym->ts.derived->attr.alloc_comp)
	gfc_error_now ("Derived type variable '%s' in COMMON at %L "
		       "has an ultimate component that is "
		       "allocatable", csym->name, &csym->declared_at);
      if (has_default_initializer (csym->ts.derived))
	gfc_error_now ("Derived type variable '%s' in COMMON at %L "
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

  if (common_root == NULL)
    return;

  if (common_root->left)
    resolve_common_blocks (common_root->left);
  if (common_root->right)
    resolve_common_blocks (common_root->right);

  resolve_common_vars (common_root->n.common->head, true);

  gfc_find_symbol (common_root->name, gfc_current_ns, 0, &sym);
  if (sym == NULL)
    return;

  if (sym->attr.flavor == FL_PARAMETER)
    gfc_error ("COMMON block '%s' at %L is used as PARAMETER at %L",
	       sym->name, &common_root->n.common->where, &sym->declared_at);

  if (sym->attr.intrinsic)
    gfc_error ("COMMON block '%s' at %L is also an intrinsic procedure",
	       sym->name, &common_root->n.common->where);
  else if (sym->attr.result
	   ||(sym->attr.function && gfc_current_ns->proc_name == sym))
    gfc_notify_std (GFC_STD_F2003, "Fortran 2003: COMMON block '%s' at %L "
		    "that is also a function result", sym->name,
		    &common_root->n.common->where);
  else if (sym->attr.flavor == FL_PROCEDURE && sym->attr.proc != PROC_INTERNAL
	   && sym->attr.proc != PROC_ST_FUNCTION)
    gfc_notify_std (GFC_STD_F2003, "Fortran 2003: COMMON block '%s' at %L "
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


/* Resolve all of the elements of a structure constructor and make sure that
   the types are correct.  */

static gfc_try
resolve_structure_cons (gfc_expr *expr)
{
  gfc_constructor *cons;
  gfc_component *comp;
  gfc_try t;
  symbol_attribute a;

  t = SUCCESS;
  cons = expr->value.constructor;
  /* A constructor may have references if it is the result of substituting a
     parameter variable.  In this case we just pull out the component we
     want.  */
  if (expr->ref)
    comp = expr->ref->u.c.sym->components;
  else
    comp = expr->ts.derived->components;

  /* See if the user is trying to invoke a structure constructor for one of
     the iso_c_binding derived types.  */
  if (expr->ts.derived && expr->ts.derived->ts.is_iso_c && cons
      && cons->expr != NULL)
    {
      gfc_error ("Components of structure constructor '%s' at %L are PRIVATE",
		 expr->ts.derived->name, &(expr->where));
      return FAILURE;
    }

  for (; comp; comp = comp->next, cons = cons->next)
    {
      int rank;

      if (!cons->expr)
	continue;

      if (gfc_resolve_expr (cons->expr) == FAILURE)
	{
	  t = FAILURE;
	  continue;
	}

      rank = comp->as ? comp->as->rank : 0;
      if (cons->expr->expr_type != EXPR_NULL && rank != cons->expr->rank
	  && (comp->attr.allocatable || cons->expr->rank))
	{
	  gfc_error ("The rank of the element in the derived type "
		     "constructor at %L does not match that of the "
		     "component (%d/%d)", &cons->expr->where,
		     cons->expr->rank, rank);
	  t = FAILURE;
	}

      /* If we don't have the right type, try to convert it.  */

      if (!gfc_compare_types (&cons->expr->ts, &comp->ts))
	{
	  t = FAILURE;
	  if (comp->attr.pointer && cons->expr->ts.type != BT_UNKNOWN)
	    gfc_error ("The element in the derived type constructor at %L, "
		       "for pointer component '%s', is %s but should be %s",
		       &cons->expr->where, comp->name,
		       gfc_basic_typename (cons->expr->ts.type),
		       gfc_basic_typename (comp->ts.type));
	  else
	    t = gfc_convert_type (cons->expr, &comp->ts, 1);
	}

      if (cons->expr->expr_type == EXPR_NULL
	  && !(comp->attr.pointer || comp->attr.allocatable
	       || comp->attr.proc_pointer))
	{
	  t = FAILURE;
	  gfc_error ("The NULL in the derived type constructor at %L is "
		     "being applied to component '%s', which is neither "
		     "a POINTER nor ALLOCATABLE", &cons->expr->where,
		     comp->name);
	}

      if (!comp->attr.pointer || cons->expr->expr_type == EXPR_NULL)
	continue;

      a = gfc_expr_attr (cons->expr);

      if (!a.pointer && !a.target)
	{
	  t = FAILURE;
	  gfc_error ("The element in the derived type constructor at %L, "
		     "for pointer component '%s' should be a POINTER or "
		     "a TARGET", &cons->expr->where, comp->name);
	}
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
      || a.value || a.access != ACCESS_UNKNOWN || a.intent != INTENT_UNKNOWN)
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

typedef enum
{ PTYPE_GENERIC = 1, PTYPE_SPECIFIC, PTYPE_UNKNOWN }
proc_type;

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
  if ((e->ref->u.ar.end[e->ref->u.ar.as->rank - 1] == NULL)
	  && (e->ref->u.ar.as->type == AS_ASSUMED_SIZE)
	       && (e->ref->u.ar.type == AR_FULL))
    {
      gfc_error ("The upper bound in the last dimension must "
		 "appear in the reference to the assumed size "
		 "array '%s' at %L", sym->name, &e->where);
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
    gfc_error ("'%s' at %L is ambiguous", e->symtree->n.sym->name,
	       &e->where);

  if (n == 0)
    gfc_error ("GENERIC procedure '%s' is not allowed as an actual "
	       "argument at %L", sym->name, &e->where);

  return n;
}


/* See if a call to sym could possibly be a not allowed RECURSION because of
   a missing RECURIVE declaration.  This means that either sym is the current
   context itself, or sym is the parent of a contained procedure calling its
   non-RECURSIVE containing procedure.
   This also works if sym is an ENTRY.  */

static bool
is_illegal_recursion (gfc_symbol* sym, gfc_namespace* context)
{
  gfc_symbol* proc_sym;
  gfc_symbol* context_proc;

  gcc_assert (sym->attr.flavor == FL_PROCEDURE);

  /* If we've got an ENTRY, find real procedure.  */
  if (sym->attr.entry && sym->ns->entries)
    proc_sym = sym->ns->entries->sym;
  else
    proc_sym = sym;

  /* If sym is RECURSIVE, all is well of course.  */
  if (proc_sym->attr.recursive || gfc_option.flag_recursive)
    return false;

  /* Find the context procdure's "real" symbol if it has entries.  */
  context_proc = (context->entries ? context->entries->sym
				   : context->proc_name);
  if (!context_proc)
    return true;

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

static gfc_try
resolve_intrinsic (gfc_symbol *sym, locus *loc)
{
  gfc_intrinsic_sym *isym = gfc_find_function (sym->name);
  if (isym)
    {
      if (!sym->attr.function &&
	  gfc_add_function (&sym->attr, sym->name, loc) == FAILURE)
	return FAILURE;
      sym->ts = isym->ts;
    }
  else
    {
      isym = gfc_find_subroutine (sym->name);
      gcc_assert (isym);
      if (!sym->attr.subroutine &&
	  gfc_add_subroutine (&sym->attr, sym->name, loc) == FAILURE)
	return FAILURE;
    }
  if (!sym->formal)
    gfc_copy_formal_args_intr (sym, isym);
  return SUCCESS;
}


/* Resolve a procedure expression, like passing it to a called procedure or as
   RHS for a procedure pointer assignment.  */

static gfc_try
resolve_procedure_expression (gfc_expr* expr)
{
  gfc_symbol* sym;

  if (expr->expr_type != EXPR_VARIABLE)
    return SUCCESS;
  gcc_assert (expr->symtree);

  sym = expr->symtree->n.sym;

  if (sym->attr.intrinsic)
    resolve_intrinsic (sym, &expr->where);

  if (sym->attr.flavor != FL_PROCEDURE
      || (sym->attr.function && sym->result == sym))
    return SUCCESS;

  /* A non-RECURSIVE procedure that is used as procedure expression within its
     own body is in danger of being called recursively.  */
  if (is_illegal_recursion (sym, gfc_current_ns))
    gfc_warning ("Non-RECURSIVE procedure '%s' at %L is possibly calling"
		 " itself recursively.  Declare it RECURSIVE or use"
		 " -frecursive", sym->name, &expr->where);
  
  return SUCCESS;
}


/* Resolve an actual argument list.  Most of the time, this is just
   resolving the expressions in the list.
   The exception is that we sometimes have to decide whether arguments
   that look like procedure arguments are really simple variable
   references.  */

static gfc_try
resolve_actual_arglist (gfc_actual_arglist *arg, procedure_type ptype,
			bool no_formal_args)
{
  gfc_symbol *sym;
  gfc_symtree *parent_st;
  gfc_expr *e;
  int save_need_full_assumed_size;
  gfc_component *comp;
	
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
		  return FAILURE;
		}
	    }
	  continue;
	}

      if (gfc_is_proc_ptr_comp (e, &comp))
	{
	  e->ts = comp->ts;
	  if (e->value.compcall.actual == NULL)
	    e->expr_type = EXPR_VARIABLE;
	  else
	    {
	      if (comp->as != NULL)
		e->rank = comp->as->rank;
	      e->expr_type = EXPR_FUNCTION;
	    }
	  goto argument_list;
	}

      if (e->expr_type == EXPR_VARIABLE
	    && e->symtree->n.sym->attr.generic
	    && no_formal_args
	    && count_specific_procs (e) != 1)
	return FAILURE;

      if (e->ts.type != BT_PROCEDURE)
	{
	  save_need_full_assumed_size = need_full_assumed_size;
	  if (e->expr_type != EXPR_VARIABLE)
	    need_full_assumed_size = 0;
	  if (gfc_resolve_expr (e) != SUCCESS)
	    return FAILURE;
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
	  if (!sym->attr.intrinsic
	      && !(sym->attr.external || sym->attr.use_assoc
		   || sym->attr.if_source == IFSRC_IFBODY)
	      && gfc_is_intrinsic (sym, sym->attr.subroutine, e->where))
	    sym->attr.intrinsic = 1;

	  if (sym->attr.proc == PROC_ST_FUNCTION)
	    {
	      gfc_error ("Statement function '%s' at %L is not allowed as an "
			 "actual argument", sym->name, &e->where);
	    }

	  actual_ok = gfc_intrinsic_actual_ok (sym->name,
					       sym->attr.subroutine);
	  if (sym->attr.intrinsic && actual_ok == 0)
	    {
	      gfc_error ("Intrinsic '%s' at %L is not allowed as an "
			 "actual argument", sym->name, &e->where);
	    }

	  if (sym->attr.contained && !sym->attr.use_assoc
	      && sym->ns->proc_name->attr.flavor != FL_MODULE)
	    {
	      gfc_error ("Internal procedure '%s' is not allowed as an "
			 "actual argument at %L", sym->name, &e->where);
	    }

	  if (sym->attr.elemental && !sym->attr.intrinsic)
	    {
	      gfc_error ("ELEMENTAL non-INTRINSIC procedure '%s' is not "
			 "allowed as an actual argument at %L", sym->name,
			 &e->where);
	    }

	  /* Check if a generic interface has a specific procedure
	    with the same name before emitting an error.  */
	  if (sym->attr.generic && count_specific_procs (e) != 1)
	    return FAILURE;
	  
	  /* Just in case a specific was found for the expression.  */
	  sym = e->symtree->n.sym;

	  /* If the symbol is the function that names the current (or
	     parent) scope, then we really have a variable reference.  */

	  if (sym->attr.function && sym->result == sym
	      && (sym->ns->proc_name == sym
		  || (sym->ns->parent != NULL
		      && sym->ns->parent->proc_name == sym)))
	    goto got_variable;

	  /* If all else fails, see if we have a specific intrinsic.  */
	  if (sym->ts.type == BT_UNKNOWN && sym->attr.intrinsic)
	    {
	      gfc_intrinsic_sym *isym;

	      isym = gfc_find_function (sym->name);
	      if (isym == NULL || !isym->specific)
		{
		  gfc_error ("Unable to find a specific INTRINSIC procedure "
			     "for the reference '%s' at %L", sym->name,
			     &e->where);
		  return FAILURE;
		}
	      sym->ts = isym->ts;
	      sym->attr.intrinsic = 1;
	      sym->attr.function = 1;
	    }

	  if (gfc_resolve_expr (e) == FAILURE)
	    return FAILURE;
	  goto argument_list;
	}

      /* See if the name is a module procedure in a parent unit.  */

      if (was_declared (sym) || sym->ns->parent == NULL)
	goto got_variable;

      if (gfc_find_sym_tree (sym->name, sym->ns->parent, 1, &parent_st))
	{
	  gfc_error ("Symbol '%s' at %L is ambiguous", sym->name, &e->where);
	  return FAILURE;
	}

      if (parent_st == NULL)
	goto got_variable;

      sym = parent_st->n.sym;
      e->symtree = parent_st;		/* Point to the right thing.  */

      if (sym->attr.flavor == FL_PROCEDURE
	  || sym->attr.intrinsic
	  || sym->attr.external)
	{
	  if (gfc_resolve_expr (e) == FAILURE)
	    return FAILURE;
	  goto argument_list;
	}

    got_variable:
      e->expr_type = EXPR_VARIABLE;
      e->ts = sym->ts;
      if (sym->as != NULL)
	{
	  e->rank = sym->as->rank;
	  e->ref = gfc_get_ref ();
	  e->ref->type = REF_ARRAY;
	  e->ref->u.ar.type = AR_FULL;
	  e->ref->u.ar.as = sym->as;
	}

      /* Expressions are assigned a default ts.type of BT_PROCEDURE in
	 primary.c (match_actual_arg). If above code determines that it
	 is a  variable instead, it needs to be resolved as it was not
	 done at the beginning of this function.  */
      save_need_full_assumed_size = need_full_assumed_size;
      if (e->expr_type != EXPR_VARIABLE)
	need_full_assumed_size = 0;
      if (gfc_resolve_expr (e) != SUCCESS)
	return FAILURE;
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
		  return FAILURE;
		}

	      if (e->rank)
		{
		  gfc_error ("By-value argument at %L cannot be an array or "
			     "an array section", &e->where);
		return FAILURE;
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
		  return FAILURE;
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
		  return FAILURE;
		}
	    }
	}
    }

  return SUCCESS;
}


/* Do the checks of the actual argument list that are specific to elemental
   procedures.  If called with c == NULL, we have a function, otherwise if
   expr == NULL, we have a subroutine.  */

static gfc_try
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
	return SUCCESS;
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
	return SUCCESS;
    }
  else
    return SUCCESS;

  /* The rank of an elemental is the rank of its array argument(s).  */
  for (arg = arg0; arg; arg = arg->next)
    {
      if (arg->expr != NULL && arg->expr->rank > 0)
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
	  gfc_warning ("'%s' at %L is an array and OPTIONAL; IF IT IS "
		       "MISSING, it cannot be the actual argument of an "
		       "ELEMENTAL procedure unless there is a non-optional "
		       "argument with the same rank (12.4.1.5)",
		       arg->expr->symtree->n.sym->name, &arg->expr->where);
	  return FAILURE;
	}
    }

  for (arg = arg0; arg; arg = arg->next)
    {
      if (arg->expr == NULL || arg->expr->rank == 0)
	continue;

      /* Being elemental, the last upper bound of an assumed size array
	 argument must be present.  */
      if (resolve_assumed_size_actual (arg->expr))
	return FAILURE;

      /* Elemental procedure's array actual arguments must conform.  */
      if (e != NULL)
	{
	  if (gfc_check_conformance (arg->expr, e,
				     "elemental procedure") == FAILURE)
	    return FAILURE;
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
	  gfc_error ("Actual argument at %L for INTENT(%s) dummy '%s' of "
		     "ELEMENTAL subroutine '%s' is a scalar, but another "
		     "actual argument is an array", &arg->expr->where,
		     (eformal->sym->attr.intent == INTENT_OUT) ? "OUT"
		     : "INOUT", eformal->sym->name, esym->name);
	  return FAILURE;
	}
  return SUCCESS;
}


/* Go through each actual argument in ACTUAL and see if it can be
   implemented as an inlined, non-copying intrinsic.  FNSYM is the
   function being called, or NULL if not known.  */

static void
find_noncopying_intrinsics (gfc_symbol *fnsym, gfc_actual_arglist *actual)
{
  gfc_actual_arglist *ap;
  gfc_expr *expr;

  for (ap = actual; ap; ap = ap->next)
    if (ap->expr
	&& (expr = gfc_get_noncopying_intrinsic_argument (ap->expr))
	&& !gfc_check_fncall_dependency (expr, INTENT_IN, fnsym, actual,
					 NOT_ELEMENTAL))
      ap->expr->inline_noncopying_intrinsic = 1;
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

static void
resolve_global_procedure (gfc_symbol *sym, locus *where,
			  gfc_actual_arglist **actual, int sub)
{
  gfc_gsymbol * gsym;
  gfc_namespace *ns;
  enum gfc_symbol_type type;

  type = sub ? GSYM_SUBROUTINE : GSYM_FUNCTION;

  gsym = gfc_get_gsymbol (sym->name);

  if ((gsym->type != GSYM_UNKNOWN && gsym->type != type))
    gfc_global_used (gsym, where);

  if (gfc_option.flag_whole_file
	&& gsym->type != GSYM_UNKNOWN
	&& gsym->ns
	&& gsym->ns->proc_name)
    {
      /* Make sure that translation for the gsymbol occurs before
	 the procedure currently being resolved.  */
      ns = gsym->ns->resolved ? NULL : gfc_global_ns_list;
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

      if (!gsym->ns->resolved)
	gfc_resolve (gsym->ns);

      gfc_procedure_use (gsym->ns->proc_name, actual, where);
    }

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


static gfc_try
resolve_generic_f (gfc_expr *expr)
{
  gfc_symbol *sym;
  match m;

  sym = expr->symtree->n.sym;

  for (;;)
    {
      m = resolve_generic_f0 (expr, sym);
      if (m == MATCH_YES)
	return SUCCESS;
      else if (m == MATCH_ERROR)
	return FAILURE;

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
  if (sym && !gfc_is_intrinsic (sym, 0, expr->where))
    {
      gfc_error ("There is no specific function for the generic '%s' at %L",
		 expr->symtree->n.sym->name, &expr->where);
      return FAILURE;
    }

  m = gfc_intrinsic_func_interface (expr, 0);
  if (m == MATCH_YES)
    return SUCCESS;
  if (m == MATCH_NO)
    gfc_error ("Generic function '%s' at %L is not consistent with a "
	       "specific intrinsic interface", expr->symtree->n.sym->name,
	       &expr->where);

  return FAILURE;
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
	gfc_error ("Function '%s' at %L is INTRINSIC but is not compatible "
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
  if (sym->as != NULL)
    expr->rank = sym->as->rank;

  return MATCH_YES;
}


static gfc_try
resolve_specific_f (gfc_expr *expr)
{
  gfc_symbol *sym;
  match m;

  sym = expr->symtree->n.sym;

  for (;;)
    {
      m = resolve_specific_f0 (sym, expr);
      if (m == MATCH_YES)
	return SUCCESS;
      if (m == MATCH_ERROR)
	return FAILURE;

      if (sym->ns->parent == NULL)
	break;

      gfc_find_symbol (sym->name, sym->ns->parent, 1, &sym);

      if (sym == NULL)
	break;
    }

  gfc_error ("Unable to resolve the specific function '%s' at %L",
	     expr->symtree->n.sym->name, &expr->where);

  return SUCCESS;
}


/* Resolve a procedure call not known to be generic nor specific.  */

static gfc_try
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
	return SUCCESS;
      return FAILURE;
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
	  gfc_error ("Function '%s' at %L has no IMPLICIT type",
		     sym->name, &expr->where);
	  return FAILURE;
	}
      else
	expr->ts = *ts;
    }

  return SUCCESS;
}


/* Return true, if the symbol is an external procedure.  */
static bool
is_external_proc (gfc_symbol *sym)
{
  if (!sym->attr.dummy && !sym->attr.contained
	&& !(sym->attr.intrinsic
	      || gfc_is_intrinsic (sym, sym->attr.subroutine, sym->declared_at))
	&& sym->attr.proc != PROC_ST_FUNCTION
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

  *name = NULL;

  if (e->symtree != NULL
        && e->symtree->n.sym != NULL
        && e->symtree->n.sym->attr.proc == PROC_ST_FUNCTION)
    return pure_stmt_function (e, e->symtree->n.sym);

  if (e->value.function.esym)
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


static gfc_try
is_scalar_expr_ptr (gfc_expr *expr)
{
  gfc_try retval = SUCCESS;
  gfc_ref *ref;
  int start;
  int end;

  /* See if we have a gfc_ref, which means we have a substring, array
     reference, or a component.  */
  if (expr->ref != NULL)
    {
      ref = expr->ref;
      while (ref->next != NULL)
        ref = ref->next;

      switch (ref->type)
        {
        case REF_SUBSTRING:
          if (ref->u.ss.length != NULL 
              && ref->u.ss.length->length != NULL
              && ref->u.ss.start
              && ref->u.ss.start->expr_type == EXPR_CONSTANT 
              && ref->u.ss.end
              && ref->u.ss.end->expr_type == EXPR_CONSTANT)
            {
              start = (int) mpz_get_si (ref->u.ss.start->value.integer);
              end = (int) mpz_get_si (ref->u.ss.end->value.integer);
              if (end - start + 1 != 1)
                retval = FAILURE;
            }
          else
            retval = FAILURE;
          break;
        case REF_ARRAY:
          if (ref->u.ar.type == AR_ELEMENT)
            retval = SUCCESS;
          else if (ref->u.ar.type == AR_FULL)
            {
              /* The user can give a full array if the array is of size 1.  */
              if (ref->u.ar.as != NULL
                  && ref->u.ar.as->rank == 1
                  && ref->u.ar.as->type == AS_EXPLICIT
                  && ref->u.ar.as->lower[0] != NULL
                  && ref->u.ar.as->lower[0]->expr_type == EXPR_CONSTANT
                  && ref->u.ar.as->upper[0] != NULL
                  && ref->u.ar.as->upper[0]->expr_type == EXPR_CONSTANT)
                {
		  /* If we have a character string, we need to check if
		     its length is one.	 */
		  if (expr->ts.type == BT_CHARACTER)
		    {
		      if (expr->ts.cl == NULL
			  || expr->ts.cl->length == NULL
			  || mpz_cmp_si (expr->ts.cl->length->value.integer, 1)
			  != 0)
                        retval = FAILURE;
		    }
		  else
		    {
		      /* We have constant lower and upper bounds.  If the
			 difference between is 1, it can be considered a
			 scalar.  */
		      start = (int) mpz_get_si
				(ref->u.ar.as->lower[0]->value.integer);
		      end = (int) mpz_get_si
				(ref->u.ar.as->upper[0]->value.integer);
		      if (end - start + 1 != 1)
			retval = FAILURE;
		   }
                }
              else
                retval = FAILURE;
            }
          else
            retval = FAILURE;
          break;
        default:
          retval = SUCCESS;
          break;
        }
    }
  else if (expr->ts.type == BT_CHARACTER && expr->rank == 0)
    {
      /* Character string.  Make sure it's of length 1.  */
      if (expr->ts.cl == NULL
          || expr->ts.cl->length == NULL
          || mpz_cmp_si (expr->ts.cl->length->value.integer, 1) != 0)
        retval = FAILURE;
    }
  else if (expr->rank != 0)
    retval = FAILURE;

  return retval;
}


/* Match one of the iso_c_binding functions (c_associated or c_loc)
   and, in the case of c_associated, set the binding label based on
   the arguments.  */

static gfc_try
gfc_iso_c_func_interface (gfc_symbol *sym, gfc_actual_arglist *args,
                          gfc_symbol **new_sym)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  char binding_label[GFC_MAX_BINDING_LABEL_LEN + 1];
  int optional_arg = 0, is_pointer = 0;
  gfc_try retval = SUCCESS;
  gfc_symbol *args_sym;
  gfc_typespec *arg_ts;

  if (args->expr->expr_type == EXPR_CONSTANT
      || args->expr->expr_type == EXPR_OP
      || args->expr->expr_type == EXPR_NULL)
    {
      gfc_error ("Argument to '%s' at %L is not a variable",
		 sym->name, &(args->expr->where));
      return FAILURE;
    }

  args_sym = args->expr->symtree->n.sym;

  /* The typespec for the actual arg should be that stored in the expr
     and not necessarily that of the expr symbol (args_sym), because
     the actual expression could be a part-ref of the expr symbol.  */
  arg_ts = &(args->expr->ts);

  is_pointer = gfc_is_data_pointer (args->expr);
    
  if (sym->intmod_sym_id == ISOCBINDING_ASSOCIATED)
    {
      /* If the user gave two args then they are providing something for
	 the optional arg (the second cptr).  Therefore, set the name and
	 binding label to the c_associated for two cptrs.  Otherwise,
	 set c_associated to expect one cptr.  */
      if (args->next)
	{
	  /* two args.  */
	  sprintf (name, "%s_2", sym->name);
	  sprintf (binding_label, "%s_2", sym->binding_label);
	  optional_arg = 1;
	}
      else
	{
	  /* one arg.  */
	  sprintf (name, "%s_1", sym->name);
	  sprintf (binding_label, "%s_1", sym->binding_label);
	  optional_arg = 0;
	}

      /* Get a new symbol for the version of c_associated that
	 will get called.  */
      *new_sym = get_iso_c_sym (sym, name, binding_label, optional_arg);
    }
  else if (sym->intmod_sym_id == ISOCBINDING_LOC
	   || sym->intmod_sym_id == ISOCBINDING_FUNLOC)
    {
      sprintf (name, "%s", sym->name);
      sprintf (binding_label, "%s", sym->binding_label);

      /* Error check the call.  */
      if (args->next != NULL)
        {
          gfc_error_now ("More actual than formal arguments in '%s' "
                         "call at %L", name, &(args->expr->where));
          retval = FAILURE;
        }
      else if (sym->intmod_sym_id == ISOCBINDING_LOC)
        {
          /* Make sure we have either the target or pointer attribute.  */
	  if (!args_sym->attr.target && !is_pointer)
            {
              gfc_error_now ("Parameter '%s' to '%s' at %L must be either "
                             "a TARGET or an associated pointer",
                             args_sym->name,
                             sym->name, &(args->expr->where));
              retval = FAILURE;
            }

          /* See if we have interoperable type and type param.  */
          if (verify_c_interop (arg_ts) == SUCCESS
              || gfc_check_any_c_kind (arg_ts) == SUCCESS)
            {
              if (args_sym->attr.target == 1)
                {
                  /* Case 1a, section 15.1.2.5, J3/04-007: variable that
                     has the target attribute and is interoperable.  */
                  /* Case 1b, section 15.1.2.5, J3/04-007: allocated
                     allocatable variable that has the TARGET attribute and
                     is not an array of zero size.  */
                  if (args_sym->attr.allocatable == 1)
                    {
                      if (args_sym->attr.dimension != 0 
                          && (args_sym->as && args_sym->as->rank == 0))
                        {
                          gfc_error_now ("Allocatable variable '%s' used as a "
                                         "parameter to '%s' at %L must not be "
                                         "an array of zero size",
                                         args_sym->name, sym->name,
                                         &(args->expr->where));
                          retval = FAILURE;
                        }
                    }
                  else
		    {
		      /* A non-allocatable target variable with C
			 interoperable type and type parameters must be
			 interoperable.	 */
		      if (args_sym && args_sym->attr.dimension)
			{
			  if (args_sym->as->type == AS_ASSUMED_SHAPE)
			    {
			      gfc_error ("Assumed-shape array '%s' at %L "
					 "cannot be an argument to the "
					 "procedure '%s' because "
					 "it is not C interoperable",
					 args_sym->name,
					 &(args->expr->where), sym->name);
			      retval = FAILURE;
			    }
			  else if (args_sym->as->type == AS_DEFERRED)
			    {
			      gfc_error ("Deferred-shape array '%s' at %L "
					 "cannot be an argument to the "
					 "procedure '%s' because "
					 "it is not C interoperable",
					 args_sym->name,
					 &(args->expr->where), sym->name);
			      retval = FAILURE;
			    }
			}
                              
                      /* Make sure it's not a character string.  Arrays of
                         any type should be ok if the variable is of a C
                         interoperable type.  */
		      if (arg_ts->type == BT_CHARACTER)
			if (arg_ts->cl != NULL
			    && (arg_ts->cl->length == NULL
				|| arg_ts->cl->length->expr_type
				   != EXPR_CONSTANT
				|| mpz_cmp_si
				    (arg_ts->cl->length->value.integer, 1)
				   != 0)
			    && is_scalar_expr_ptr (args->expr) != SUCCESS)
			  {
			    gfc_error_now ("CHARACTER argument '%s' to '%s' "
					   "at %L must have a length of 1",
					   args_sym->name, sym->name,
					   &(args->expr->where));
			    retval = FAILURE;
			  }
                    }
                }
              else if (is_pointer
		       && is_scalar_expr_ptr (args->expr) != SUCCESS)
                {
                  /* Case 1c, section 15.1.2.5, J3/04-007: an associated
                     scalar pointer.  */
                  gfc_error_now ("Argument '%s' to '%s' at %L must be an "
                                 "associated scalar POINTER", args_sym->name,
                                 sym->name, &(args->expr->where));
                  retval = FAILURE;
                }
            }
          else
            {
              /* The parameter is not required to be C interoperable.  If it
                 is not C interoperable, it must be a nonpolymorphic scalar
                 with no length type parameters.  It still must have either
                 the pointer or target attribute, and it can be
                 allocatable (but must be allocated when c_loc is called).  */
              if (args->expr->rank != 0 
                  && is_scalar_expr_ptr (args->expr) != SUCCESS)
                {
                  gfc_error_now ("Parameter '%s' to '%s' at %L must be a "
                                 "scalar", args_sym->name, sym->name,
                                 &(args->expr->where));
                  retval = FAILURE;
                }
              else if (arg_ts->type == BT_CHARACTER 
                       && is_scalar_expr_ptr (args->expr) != SUCCESS)
                {
                  gfc_error_now ("CHARACTER argument '%s' to '%s' at "
                                 "%L must have a length of 1",
                                 args_sym->name, sym->name,
                                 &(args->expr->where));
                  retval = FAILURE;
                }
            }
        }
      else if (sym->intmod_sym_id == ISOCBINDING_FUNLOC)
        {
          if (args_sym->attr.flavor != FL_PROCEDURE)
            {
              /* TODO: Update this error message to allow for procedure
                 pointers once they are implemented.  */
              gfc_error_now ("Parameter '%s' to '%s' at %L must be a "
                             "procedure",
                             args_sym->name, sym->name,
                             &(args->expr->where));
              retval = FAILURE;
            }
	  else if (args_sym->attr.is_bind_c != 1)
	    {
	      gfc_error_now ("Parameter '%s' to '%s' at %L must be "
			     "BIND(C)",
			     args_sym->name, sym->name,
			     &(args->expr->where));
	      retval = FAILURE;
	    }
        }
      
      /* for c_loc/c_funloc, the new symbol is the same as the old one */
      *new_sym = sym;
    }
  else
    {
      gfc_internal_error ("gfc_iso_c_func_interface(): Unhandled "
			  "iso_c_binding function: '%s'!\n", sym->name);
    }

  return retval;
}


/* Resolve a function call, which means resolving the arguments, then figuring
   out which entity the name refers to.  */
/* TODO: Check procedure arguments so that an INTENT(IN) isn't passed
   to INTENT(OUT) or INTENT(INOUT).  */

static gfc_try
resolve_function (gfc_expr *expr)
{
  gfc_actual_arglist *arg;
  gfc_symbol *sym;
  const char *name;
  gfc_try t;
  int temp;
  procedure_type p = PROC_INTRINSIC;
  bool no_formal_args;

  sym = NULL;
  if (expr->symtree)
    sym = expr->symtree->n.sym;

  if (sym && sym->attr.intrinsic
      && resolve_intrinsic (sym, &expr->where) == FAILURE)
    return FAILURE;

  if (sym && (sym->attr.flavor == FL_VARIABLE || sym->attr.subroutine))
    {
      gfc_error ("'%s' at %L is not a function", sym->name, &expr->where);
      return FAILURE;
    }

  if (sym && sym->attr.abstract)
    {
      gfc_error ("ABSTRACT INTERFACE '%s' must not be referenced at %L",
		 sym->name, &expr->where);
      return FAILURE;
    }

  /* Switch off assumed size checking and do this again for certain kinds
     of procedure, once the procedure itself is resolved.  */
  need_full_assumed_size++;

  if (expr->symtree && expr->symtree->n.sym)
    p = expr->symtree->n.sym->attr.proc;

  no_formal_args = sym && is_external_proc (sym) && sym->formal == NULL;
  if (resolve_actual_arglist (expr->value.function.actual,
			      p, no_formal_args) == FAILURE)
      return FAILURE;

  /* Need to setup the call to the correct c_associated, depending on
     the number of cptrs to user gives to compare.  */
  if (sym && sym->attr.is_iso_c == 1)
    {
      if (gfc_iso_c_func_interface (sym, expr->value.function.actual, &sym)
          == FAILURE)
        return FAILURE;
      
      /* Get the symtree for the new symbol (resolved func).
         the old one will be freed later, when it's no longer used.  */
      gfc_find_sym_tree (sym->name, sym->ns, 1, &(expr->symtree));
    }
  
  /* Resume assumed_size checking.  */
  need_full_assumed_size--;

  /* If the procedure is external, check for usage.  */
  if (sym && is_external_proc (sym))
    resolve_global_procedure (sym, &expr->where,
			      &expr->value.function.actual, 0);

  if (sym && sym->ts.type == BT_CHARACTER
      && sym->ts.cl
      && sym->ts.cl->length == NULL
      && !sym->attr.dummy
      && expr->value.function.esym == NULL
      && !sym->attr.contained)
    {
      /* Internal procedures are taken care of in resolve_contained_fntype.  */
      gfc_error ("Function '%s' is declared CHARACTER(*) and cannot "
		 "be used at %L since it is not a dummy argument",
		 sym->name, &expr->where);
      return FAILURE;
    }

  /* See if function is already resolved.  */

  if (expr->value.function.name != NULL)
    {
      if (expr->ts.type == BT_UNKNOWN)
	expr->ts = sym->ts;
      t = SUCCESS;
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

  if (resolve_elemental_actual (expr, NULL) == FAILURE)
    return FAILURE;

  if (omp_workshare_flag
      && expr->value.function.esym
      && ! gfc_elemental (expr->value.function.esym))
    {
      gfc_error ("User defined non-ELEMENTAL function '%s' at %L not allowed "
		 "in WORKSHARE construct", expr->value.function.esym->name,
		 &expr->where);
      t = FAILURE;
    }

#define GENERIC_ID expr->value.function.isym->id
  else if (expr->value.function.actual != NULL
	   && expr->value.function.isym != NULL
	   && GENERIC_ID != GFC_ISYM_LBOUND
	   && GENERIC_ID != GFC_ISYM_LEN
	   && GENERIC_ID != GFC_ISYM_LOC
	   && GENERIC_ID != GFC_ISYM_PRESENT)
    {
      /* Array intrinsics must also have the last upper bound of an
	 assumed size array argument.  UBOUND and SIZE have to be
	 excluded from the check if the second argument is anything
	 than a constant.  */

      for (arg = expr->value.function.actual; arg; arg = arg->next)
	{
	  if ((GENERIC_ID == GFC_ISYM_UBOUND || GENERIC_ID == GFC_ISYM_SIZE)
	      && arg->next != NULL && arg->next->expr)
	    {
	      if (arg->next->expr->expr_type != EXPR_CONSTANT)
		break;

	      if (arg->next->name && strncmp(arg->next->name, "kind", 4) == 0)
		break;

	      if ((int)mpz_get_si (arg->next->expr->value.integer)
			< arg->expr->rank)
		break;
	    }

	  if (arg->expr != NULL
	      && arg->expr->rank > 0
	      && resolve_assumed_size_actual (arg->expr))
	    return FAILURE;
	}
    }
#undef GENERIC_ID

  need_full_assumed_size = temp;
  name = NULL;

  if (!pure_function (expr, &name) && name)
    {
      if (forall_flag)
	{
	  gfc_error ("reference to non-PURE function '%s' at %L inside a "
		     "FORALL %s", name, &expr->where,
		     forall_flag == 2 ? "mask" : "block");
	  t = FAILURE;
	}
      else if (gfc_pure (NULL))
	{
	  gfc_error ("Function reference to '%s' at %L is to a non-PURE "
		     "procedure within a PURE procedure", name, &expr->where);
	  t = FAILURE;
	}
    }

  /* Functions without the RECURSIVE attribution are not allowed to
   * call themselves.  */
  if (expr->value.function.esym && !expr->value.function.esym->attr.recursive)
    {
      gfc_symbol *esym;
      esym = expr->value.function.esym;

      if (is_illegal_recursion (esym, gfc_current_ns))
      {
	if (esym->attr.entry && esym->ns->entries)
	  gfc_error ("ENTRY '%s' at %L cannot be called recursively, as"
		     " function '%s' is not RECURSIVE",
		     esym->name, &expr->where, esym->ns->entries->sym->name);
	else
	  gfc_error ("Function '%s' at %L cannot be called recursively, as it"
		     " is not RECURSIVE", esym->name, &expr->where);

	t = FAILURE;
      }
    }

  /* Character lengths of use associated functions may contains references to
     symbols not referenced from the current program unit otherwise.  Make sure
     those symbols are marked as referenced.  */

  if (expr->ts.type == BT_CHARACTER && expr->value.function.esym
      && expr->value.function.esym->attr.use_assoc)
    {
      gfc_expr_set_symbols_referenced (expr->ts.cl->length);
    }

  if (t == SUCCESS
	&& !((expr->value.function.esym
		&& expr->value.function.esym->attr.elemental)
			||
	     (expr->value.function.isym
		&& expr->value.function.isym->elemental)))
    find_noncopying_intrinsics (expr->value.function.esym,
				expr->value.function.actual);

  /* Make sure that the expression has a typespec that works.  */
  if (expr->ts.type == BT_UNKNOWN)
    {
      if (expr->symtree->n.sym->result
	    && expr->symtree->n.sym->result->ts.type != BT_UNKNOWN
	    && !expr->symtree->n.sym->result->attr.proc_pointer)
	expr->ts = expr->symtree->n.sym->result->ts;
    }

  return t;
}


/************* Subroutine resolution *************/

static void
pure_subroutine (gfc_code *c, gfc_symbol *sym)
{
  if (gfc_pure (sym))
    return;

  if (forall_flag)
    gfc_error ("Subroutine call to '%s' in FORALL block at %L is not PURE",
	       sym->name, &c->loc);
  else if (gfc_pure (NULL))
    gfc_error ("Subroutine call to '%s' at %L is not PURE", sym->name,
	       &c->loc);
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
	  pure_subroutine (c, s);
	  return MATCH_YES;
	}

      /* TODO: Need to search for elemental references in generic interface.  */
    }

  if (sym->attr.intrinsic)
    return gfc_intrinsic_sub_interface (c, 0);

  return MATCH_NO;
}


static gfc_try
resolve_generic_s (gfc_code *c)
{
  gfc_symbol *sym;
  match m;

  sym = c->symtree->n.sym;

  for (;;)
    {
      m = resolve_generic_s0 (c, sym);
      if (m == MATCH_YES)
	return SUCCESS;
      else if (m == MATCH_ERROR)
	return FAILURE;

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
      gfc_error ("There is no specific subroutine for the generic '%s' at %L",
		 sym->name, &c->loc);
      return FAILURE;
    }

  m = gfc_intrinsic_sub_interface (c, 0);
  if (m == MATCH_YES)
    return SUCCESS;
  if (m == MATCH_NO)
    gfc_error ("Generic subroutine '%s' at %L is not consistent with an "
	       "intrinsic subroutine interface", sym->name, &c->loc);

  return FAILURE;
}


/* Set the name and binding label of the subroutine symbol in the call
   expression represented by 'c' to include the type and kind of the
   second parameter.  This function is for resolving the appropriate
   version of c_f_pointer() and c_f_procpointer().  For example, a
   call to c_f_pointer() for a default integer pointer could have a
   name of c_f_pointer_i4.  If no second arg exists, which is an error
   for these two functions, it defaults to the generic symbol's name
   and binding label.  */

static void
set_name_and_label (gfc_code *c, gfc_symbol *sym,
                    char *name, char *binding_label)
{
  gfc_expr *arg = NULL;
  char type;
  int kind;

  /* The second arg of c_f_pointer and c_f_procpointer determines
     the type and kind for the procedure name.  */
  arg = c->ext.actual->next->expr;

  if (arg != NULL)
    {
      /* Set up the name to have the given symbol's name,
         plus the type and kind.  */
      /* a derived type is marked with the type letter 'u' */
      if (arg->ts.type == BT_DERIVED)
        {
          type = 'd';
          kind = 0; /* set the kind as 0 for now */
        }
      else
        {
          type = gfc_type_letter (arg->ts.type);
          kind = arg->ts.kind;
        }

      if (arg->ts.type == BT_CHARACTER)
	/* Kind info for character strings not needed.	*/
	kind = 0;

      sprintf (name, "%s_%c%d", sym->name, type, kind);
      /* Set up the binding label as the given symbol's label plus
         the type and kind.  */
      sprintf (binding_label, "%s_%c%d", sym->binding_label, type, kind);
    }
  else
    {
      /* If the second arg is missing, set the name and label as
         was, cause it should at least be found, and the missing
         arg error will be caught by compare_parameters().  */
      sprintf (name, "%s", sym->name);
      sprintf (binding_label, "%s", sym->binding_label);
    }
   
  return;
}


/* Resolve a generic version of the iso_c_binding procedure given
   (sym) to the specific one based on the type and kind of the
   argument(s).  Currently, this function resolves c_f_pointer() and
   c_f_procpointer based on the type and kind of the second argument
   (FPTR).  Other iso_c_binding procedures aren't specially handled.
   Upon successfully exiting, c->resolved_sym will hold the resolved
   symbol.  Returns MATCH_ERROR if an error occurred; MATCH_YES
   otherwise.  */

match
gfc_iso_c_sub_interface (gfc_code *c, gfc_symbol *sym)
{
  gfc_symbol *new_sym;
  /* this is fine, since we know the names won't use the max */
  char name[GFC_MAX_SYMBOL_LEN + 1];
  char binding_label[GFC_MAX_BINDING_LABEL_LEN + 1];
  /* default to success; will override if find error */
  match m = MATCH_YES;

  /* Make sure the actual arguments are in the necessary order (based on the 
     formal args) before resolving.  */
  gfc_procedure_use (sym, &c->ext.actual, &(c->loc));

  if ((sym->intmod_sym_id == ISOCBINDING_F_POINTER) ||
      (sym->intmod_sym_id == ISOCBINDING_F_PROCPOINTER))
    {
      set_name_and_label (c, sym, name, binding_label);
      
      if (sym->intmod_sym_id == ISOCBINDING_F_POINTER)
	{
	  if (c->ext.actual != NULL && c->ext.actual->next != NULL)
	    {
	      /* Make sure we got a third arg if the second arg has non-zero
		 rank.	We must also check that the type and rank are
		 correct since we short-circuit this check in
		 gfc_procedure_use() (called above to sort actual args).  */
	      if (c->ext.actual->next->expr->rank != 0)
		{
		  if(c->ext.actual->next->next == NULL 
		     || c->ext.actual->next->next->expr == NULL)
		    {
		      m = MATCH_ERROR;
		      gfc_error ("Missing SHAPE parameter for call to %s "
				 "at %L", sym->name, &(c->loc));
		    }
		  else if (c->ext.actual->next->next->expr->ts.type
			   != BT_INTEGER
			   || c->ext.actual->next->next->expr->rank != 1)
		    {
		      m = MATCH_ERROR;
		      gfc_error ("SHAPE parameter for call to %s at %L must "
				 "be a rank 1 INTEGER array", sym->name,
				 &(c->loc));
		    }
		}
	    }
	}
      
      if (m != MATCH_ERROR)
	{
	  /* the 1 means to add the optional arg to formal list */
	  new_sym = get_iso_c_sym (sym, name, binding_label, 1);
	 
	  /* for error reporting, say it's declared where the original was */
	  new_sym->declared_at = sym->declared_at;
	}
    }
  else
    {
      /* no differences for c_loc or c_funloc */
      new_sym = sym;
    }

  /* set the resolved symbol */
  if (m != MATCH_ERROR)
    c->resolved_sym = new_sym;
  else
    c->resolved_sym = sym;
  
  return m;
}


/* Resolve a subroutine call known to be specific.  */

static match
resolve_specific_s0 (gfc_code *c, gfc_symbol *sym)
{
  match m;

  if(sym->attr.is_iso_c)
    {
      m = gfc_iso_c_sub_interface (c,sym);
      return m;
    }
  
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
	gfc_error ("Subroutine '%s' at %L is INTRINSIC but is not compatible "
		   "with an intrinsic", sym->name, &c->loc);

      return MATCH_ERROR;
    }

  return MATCH_NO;

found:
  gfc_procedure_use (sym, &c->ext.actual, &c->loc);

  c->resolved_sym = sym;
  pure_subroutine (c, sym);

  return MATCH_YES;
}


static gfc_try
resolve_specific_s (gfc_code *c)
{
  gfc_symbol *sym;
  match m;

  sym = c->symtree->n.sym;

  for (;;)
    {
      m = resolve_specific_s0 (c, sym);
      if (m == MATCH_YES)
	return SUCCESS;
      if (m == MATCH_ERROR)
	return FAILURE;

      if (sym->ns->parent == NULL)
	break;

      gfc_find_symbol (sym->name, sym->ns->parent, 1, &sym);

      if (sym == NULL)
	break;
    }

  sym = c->symtree->n.sym;
  gfc_error ("Unable to resolve the specific subroutine '%s' at %L",
	     sym->name, &c->loc);

  return FAILURE;
}


/* Resolve a subroutine call not known to be generic nor specific.  */

static gfc_try
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
	return SUCCESS;
      return FAILURE;
    }

  /* The reference is to an external name.  */

found:
  gfc_procedure_use (sym, &c->ext.actual, &c->loc);

  c->resolved_sym = sym;

  pure_subroutine (c, sym);

  return SUCCESS;
}


/* Resolve a subroutine call.  Although it was tempting to use the same code
   for functions, subroutines and functions are stored differently and this
   makes things awkward.  */

static gfc_try
resolve_call (gfc_code *c)
{
  gfc_try t;
  procedure_type ptype = PROC_INTRINSIC;
  gfc_symbol *csym, *sym;
  bool no_formal_args;

  csym = c->symtree ? c->symtree->n.sym : NULL;

  if (csym && csym->ts.type != BT_UNKNOWN)
    {
      gfc_error ("'%s' at %L has a type, which is not consistent with "
		 "the CALL at %L", csym->name, &csym->declared_at, &c->loc);
      return FAILURE;
    }

  if (csym && gfc_current_ns->parent && csym->ns != gfc_current_ns)
    {
      gfc_symtree *st;
      gfc_find_sym_tree (csym->name, gfc_current_ns, 1, &st);
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

  /* Subroutines without the RECURSIVE attribution are not allowed to
   * call themselves.  */
  if (csym && is_illegal_recursion (csym, gfc_current_ns))
    {
      if (csym->attr.entry && csym->ns->entries)
	gfc_error ("ENTRY '%s' at %L cannot be called recursively, as"
		   " subroutine '%s' is not RECURSIVE",
		   csym->name, &c->loc, csym->ns->entries->sym->name);
      else
	gfc_error ("SUBROUTINE '%s' at %L cannot be called recursively, as it"
		   " is not RECURSIVE", csym->name, &c->loc);

      t = FAILURE;
    }

  /* Switch off assumed size checking and do this again for certain kinds
     of procedure, once the procedure itself is resolved.  */
  need_full_assumed_size++;

  if (csym)
    ptype = csym->attr.proc;

  no_formal_args = csym && is_external_proc (csym) && csym->formal == NULL;
  if (resolve_actual_arglist (c->ext.actual, ptype,
			      no_formal_args) == FAILURE)
    return FAILURE;

  /* Resume assumed_size checking.  */
  need_full_assumed_size--;

  /* If external, check for usage.  */
  if (csym && is_external_proc (csym))
    resolve_global_procedure (csym, &c->loc, &c->ext.actual, 1);

  t = SUCCESS;
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
  if (resolve_elemental_actual (NULL, c) == FAILURE)
    return FAILURE;

  if (t == SUCCESS && !(c->resolved_sym && c->resolved_sym->attr.elemental))
    find_noncopying_intrinsics (c->resolved_sym, c->ext.actual);
  return t;
}


/* Compare the shapes of two arrays that have non-NULL shapes.  If both
   op1->shape and op2->shape are non-NULL return SUCCESS if their shapes
   match.  If both op1->shape and op2->shape are non-NULL return FAILURE
   if their shapes do not match.  If either op1->shape or op2->shape is
   NULL, return SUCCESS.  */

static gfc_try
compare_shapes (gfc_expr *op1, gfc_expr *op2)
{
  gfc_try t;
  int i;

  t = SUCCESS;

  if (op1->shape != NULL && op2->shape != NULL)
    {
      for (i = 0; i < op1->rank; i++)
	{
	  if (mpz_cmp (op1->shape[i], op2->shape[i]) != 0)
	   {
	     gfc_error ("Shapes for operands at %L and %L are not conformable",
			 &op1->where, &op2->where);
	     t = FAILURE;
	     break;
	   }
	}
    }

  return t;
}


/* Resolve an operator expression node.  This can involve replacing the
   operation with a user defined function call.  */

static gfc_try
resolve_operator (gfc_expr *e)
{
  gfc_expr *op1, *op2;
  char msg[200];
  bool dual_locus_error;
  gfc_try t;

  /* Resolve all subnodes-- give them types.  */

  switch (e->value.op.op)
    {
    default:
      if (gfc_resolve_expr (e->value.op.op2) == FAILURE)
	return FAILURE;

    /* Fall through...  */

    case INTRINSIC_NOT:
    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:
    case INTRINSIC_PARENTHESES:
      if (gfc_resolve_expr (e->value.op.op1) == FAILURE)
	return FAILURE;
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

      sprintf (msg, _("Operand of unary numeric operator '%s' at %%L is %s"),
	       gfc_op2string (e->value.op.op), gfc_typename (&e->ts));
      goto bad_op;

    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
    case INTRINSIC_POWER:
      if (gfc_numeric_ts (&op1->ts) && gfc_numeric_ts (&op2->ts))
	{
	  gfc_type_convert_binary (e);
	  break;
	}

      sprintf (msg,
	       _("Operands of binary numeric operator '%s' at %%L are %s/%s"),
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

      sprintf (msg, _("Operands of logical operator '%s' at %%L are %s/%s"),
	       gfc_op2string (e->value.op.op), gfc_typename (&op1->ts),
	       gfc_typename (&op2->ts));

      goto bad_op;

    case INTRINSIC_NOT:
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

      /* Fall through...  */

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
	  gfc_type_convert_binary (e);

	  e->ts.type = BT_LOGICAL;
	  e->ts.kind = gfc_default_logical_kind;
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
		 _("Operands of comparison operator '%s' at %%L are %s/%s"),
		 gfc_op2string (e->value.op.op), gfc_typename (&op1->ts),
		 gfc_typename (&op2->ts));

      goto bad_op;

    case INTRINSIC_USER:
      if (e->value.op.uop->op == NULL)
	sprintf (msg, _("Unknown operator '%s' at %%L"), e->value.op.uop->name);
      else if (op2 == NULL)
	sprintf (msg, _("Operand of user operator '%s' at %%L is %s"),
		 e->value.op.uop->name, gfc_typename (&op1->ts));
      else
	sprintf (msg, _("Operands of user operator '%s' at %%L are %s/%s"),
		 e->value.op.uop->name, gfc_typename (&op1->ts),
		 gfc_typename (&op2->ts));

      goto bad_op;

    case INTRINSIC_PARENTHESES:
      e->ts = op1->ts;
      if (e->ts.type == BT_CHARACTER)
	e->ts.cl = op1->ts.cl;
      break;

    default:
      gfc_internal_error ("resolve_operator(): Bad intrinsic");
    }

  /* Deal with arrayness of an operand through an operator.  */

  t = SUCCESS;

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
		  t = compare_shapes(op1, op2);
		  if (t == FAILURE)
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
  if (t == SUCCESS)
    {
      t = gfc_simplify_expr (e, 0);
      /* Some calls do not succeed in simplification and return FAILURE
	 even though there is no error; e.g. variable references to
	 PARAMETER arrays.  */
      if (!gfc_is_constant_expr (e))
	t = SUCCESS;
    }
  return t;

bad_op:

  if (gfc_extend_expr (e) == SUCCESS)
    return SUCCESS;

  if (dual_locus_error)
    gfc_error (msg, &op1->where, &op2->where);
  else
    gfc_error (msg, &e->where);

  return FAILURE;
}


/************** Array resolution subroutines **************/

typedef enum
{ CMP_LT, CMP_EQ, CMP_GT, CMP_UNKNOWN }
comparison;

/* Compare two integer expressions.  */

static comparison
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

static comparison
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

static comparison
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

  if (stride == NULL || compare_bound_int(stride, 1) == CMP_EQ)
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

static gfc_try
check_dimension (int i, gfc_array_ref *ar, gfc_array_spec *as)
{
  mpz_t last_value;

/* Given start, end and stride values, calculate the minimum and
   maximum referenced indexes.  */

  switch (ar->dimen_type[i])
    {
    case DIMEN_VECTOR:
      break;

    case DIMEN_ELEMENT:
      if (compare_bound (ar->start[i], as->lower[i]) == CMP_LT)
	{
	  gfc_warning ("Array reference at %L is out of bounds "
		       "(%ld < %ld) in dimension %d", &ar->c_where[i],
		       mpz_get_si (ar->start[i]->value.integer),
		       mpz_get_si (as->lower[i]->value.integer), i+1);
	  return SUCCESS;
	}
      if (compare_bound (ar->start[i], as->upper[i]) == CMP_GT)
	{
	  gfc_warning ("Array reference at %L is out of bounds "
		       "(%ld > %ld) in dimension %d", &ar->c_where[i],
		       mpz_get_si (ar->start[i]->value.integer),
		       mpz_get_si (as->upper[i]->value.integer), i+1);
	  return SUCCESS;
	}

      break;

    case DIMEN_RANGE:
      {
#define AR_START (ar->start[i] ? ar->start[i] : as->lower[i])
#define AR_END (ar->end[i] ? ar->end[i] : as->upper[i])

	comparison comp_start_end = compare_bound (AR_START, AR_END);

	/* Check for zero stride, which is not allowed.  */
	if (compare_bound_int (ar->stride[i], 0) == CMP_EQ)
	  {
	    gfc_error ("Illegal stride of zero at %L", &ar->c_where[i]);
	    return FAILURE;
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
		gfc_warning ("Lower array reference at %L is out of bounds "
		       "(%ld < %ld) in dimension %d", &ar->c_where[i],
		       mpz_get_si (AR_START->value.integer),
		       mpz_get_si (as->lower[i]->value.integer), i+1);
		return SUCCESS;
	      }
	    if (compare_bound (AR_START, as->upper[i]) == CMP_GT)
	      {
		gfc_warning ("Lower array reference at %L is out of bounds "
		       "(%ld > %ld) in dimension %d", &ar->c_where[i],
		       mpz_get_si (AR_START->value.integer),
		       mpz_get_si (as->upper[i]->value.integer), i+1);
		return SUCCESS;
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
		gfc_warning ("Upper array reference at %L is out of bounds "
		       "(%ld < %ld) in dimension %d", &ar->c_where[i],
		       mpz_get_si (last_value),
		       mpz_get_si (as->lower[i]->value.integer), i+1);
	        mpz_clear (last_value);
		return SUCCESS;
	      }
	    if (compare_bound_mpz_t (as->upper[i], last_value) == CMP_LT)
	      {
		gfc_warning ("Upper array reference at %L is out of bounds "
		       "(%ld > %ld) in dimension %d", &ar->c_where[i],
		       mpz_get_si (last_value),
		       mpz_get_si (as->upper[i]->value.integer), i+1);
	        mpz_clear (last_value);
		return SUCCESS;
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

  return SUCCESS;
}


/* Compare an array reference with an array specification.  */

static gfc_try
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
      return FAILURE;
    }

  if (ar->type == AR_FULL)
    return SUCCESS;

  if (as->rank != ar->dimen)
    {
      gfc_error ("Rank mismatch in array reference at %L (%d/%d)",
		 &ar->where, ar->dimen, as->rank);
      return FAILURE;
    }

  for (i = 0; i < as->rank; i++)
    if (check_dimension (i, ar, as) == FAILURE)
      return FAILURE;

  return SUCCESS;
}


/* Resolve one part of an array index.  */

gfc_try
gfc_resolve_index (gfc_expr *index, int check_scalar)
{
  gfc_typespec ts;

  if (index == NULL)
    return SUCCESS;

  if (gfc_resolve_expr (index) == FAILURE)
    return FAILURE;

  if (check_scalar && index->rank != 0)
    {
      gfc_error ("Array index at %L must be scalar", &index->where);
      return FAILURE;
    }

  if (index->ts.type != BT_INTEGER && index->ts.type != BT_REAL)
    {
      gfc_error ("Array index at %L must be of INTEGER type, found %s",
		 &index->where, gfc_basic_typename (index->ts.type));
      return FAILURE;
    }

  if (index->ts.type == BT_REAL)
    if (gfc_notify_std (GFC_STD_LEGACY, "Extension: REAL array index at %L",
			&index->where) == FAILURE)
      return FAILURE;

  if (index->ts.kind != gfc_index_integer_kind
      || index->ts.type != BT_INTEGER)
    {
      gfc_clear_ts (&ts);
      ts.type = BT_INTEGER;
      ts.kind = gfc_index_integer_kind;

      gfc_convert_type_warn (index, &ts, 2, 0);
    }

  return SUCCESS;
}

/* Resolve a dim argument to an intrinsic function.  */

gfc_try
gfc_resolve_dim_arg (gfc_expr *dim)
{
  if (dim == NULL)
    return SUCCESS;

  if (gfc_resolve_expr (dim) == FAILURE)
    return FAILURE;

  if (dim->rank != 0)
    {
      gfc_error ("Argument dim at %L must be scalar", &dim->where);
      return FAILURE;

    }

  if (dim->ts.type != BT_INTEGER)
    {
      gfc_error ("Argument dim at %L must be of INTEGER type", &dim->where);
      return FAILURE;
    }

  if (dim->ts.kind != gfc_index_integer_kind)
    {
      gfc_typespec ts;

      ts.type = BT_INTEGER;
      ts.kind = gfc_index_integer_kind;

      gfc_convert_type_warn (dim, &ts, 2, 0);
    }

  return SUCCESS;
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
  gfc_symbol *derived;
  gfc_ref *ref;

  as = e->symtree->n.sym->as;
  derived = NULL;

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
	if (derived == NULL)
	  derived = e->symtree->n.sym->ts.derived;

	c = derived->components;

	for (; c; c = c->next)
	  if (c == ref->u.c.component)
	    {
	      /* Track the sequence of component references.  */
	      if (c->ts.type == BT_DERIVED)
		derived = c->ts.derived;
	      break;
	    }

	if (c == NULL)
	  gfc_internal_error ("find_array_spec(): Component not found");

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

static gfc_try
resolve_array_ref (gfc_array_ref *ar)
{
  int i, check_scalar;
  gfc_expr *e;

  for (i = 0; i < ar->dimen; i++)
    {
      check_scalar = ar->dimen_type[i] == DIMEN_RANGE;

      if (gfc_resolve_index (ar->start[i], check_scalar) == FAILURE)
	return FAILURE;
      if (gfc_resolve_index (ar->end[i], check_scalar) == FAILURE)
	return FAILURE;
      if (gfc_resolve_index (ar->stride[i], check_scalar) == FAILURE)
	return FAILURE;

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
	    return FAILURE;
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

  if (!ar->as->cray_pointee && compare_spec_to_ref (ar) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


static gfc_try
resolve_substring (gfc_ref *ref)
{
  int k = gfc_validate_kind (BT_INTEGER, gfc_charlen_int_kind, false);

  if (ref->u.ss.start != NULL)
    {
      if (gfc_resolve_expr (ref->u.ss.start) == FAILURE)
	return FAILURE;

      if (ref->u.ss.start->ts.type != BT_INTEGER)
	{
	  gfc_error ("Substring start index at %L must be of type INTEGER",
		     &ref->u.ss.start->where);
	  return FAILURE;
	}

      if (ref->u.ss.start->rank != 0)
	{
	  gfc_error ("Substring start index at %L must be scalar",
		     &ref->u.ss.start->where);
	  return FAILURE;
	}

      if (compare_bound_int (ref->u.ss.start, 1) == CMP_LT
	  && (compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_EQ
	      || compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_GT))
	{
	  gfc_error ("Substring start index at %L is less than one",
		     &ref->u.ss.start->where);
	  return FAILURE;
	}
    }

  if (ref->u.ss.end != NULL)
    {
      if (gfc_resolve_expr (ref->u.ss.end) == FAILURE)
	return FAILURE;

      if (ref->u.ss.end->ts.type != BT_INTEGER)
	{
	  gfc_error ("Substring end index at %L must be of type INTEGER",
		     &ref->u.ss.end->where);
	  return FAILURE;
	}

      if (ref->u.ss.end->rank != 0)
	{
	  gfc_error ("Substring end index at %L must be scalar",
		     &ref->u.ss.end->where);
	  return FAILURE;
	}

      if (ref->u.ss.length != NULL
	  && compare_bound (ref->u.ss.end, ref->u.ss.length->length) == CMP_GT
	  && (compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_EQ
	      || compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_GT))
	{
	  gfc_error ("Substring end index at %L exceeds the string length",
		     &ref->u.ss.start->where);
	  return FAILURE;
	}

      if (compare_bound_mpz_t (ref->u.ss.end,
			       gfc_integer_kinds[k].huge) == CMP_GT
	  && (compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_EQ
	      || compare_bound (ref->u.ss.end, ref->u.ss.start) == CMP_GT))
	{
	  gfc_error ("Substring end index at %L is too large",
		     &ref->u.ss.end->where);
	  return FAILURE;
	}
    }

  return SUCCESS;
}


/* This function supplies missing substring charlens.  */

void
gfc_resolve_substring_charlen (gfc_expr *e)
{
  gfc_ref *char_ref;
  gfc_expr *start, *end;

  for (char_ref = e->ref; char_ref; char_ref = char_ref->next)
    if (char_ref->type == REF_SUBSTRING)
      break;

  if (!char_ref)
    return;

  gcc_assert (char_ref->next == NULL);

  if (e->ts.cl)
    {
      if (e->ts.cl->length)
	gfc_free_expr (e->ts.cl->length);
      else if (e->expr_type == EXPR_VARIABLE
		 && e->symtree->n.sym->attr.dummy)
	return;
    }

  e->ts.type = BT_CHARACTER;
  e->ts.kind = gfc_default_character_kind;

  if (!e->ts.cl)
    e->ts.cl = gfc_new_charlen (gfc_current_ns);

  if (char_ref->u.ss.start)
    start = gfc_copy_expr (char_ref->u.ss.start);
  else
    start = gfc_int_expr (1);

  if (char_ref->u.ss.end)
    end = gfc_copy_expr (char_ref->u.ss.end);
  else if (e->expr_type == EXPR_VARIABLE)
    end = gfc_copy_expr (e->symtree->n.sym->ts.cl->length);
  else
    end = NULL;

  if (!start || !end)
    return;

  /* Length = (end - start +1).  */
  e->ts.cl->length = gfc_subtract (end, start);
  e->ts.cl->length = gfc_add (e->ts.cl->length, gfc_int_expr (1));

  e->ts.cl->length->ts.type = BT_INTEGER;
  e->ts.cl->length->ts.kind = gfc_charlen_int_kind;

  /* Make sure that the length is simplified.  */
  gfc_simplify_expr (e->ts.cl->length, 1);
  gfc_resolve_expr (e->ts.cl->length);
}


/* Resolve subtype references.  */

static gfc_try
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
	if (resolve_array_ref (&ref->u.ar) == FAILURE)
	  return FAILURE;
	break;

      case REF_COMPONENT:
	break;

      case REF_SUBSTRING:
	resolve_substring (ref);
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
	      if (ref->u.c.component->attr.pointer)
		{
		  gfc_error ("Component to the right of a part reference "
			     "with nonzero rank must not have the POINTER "
			     "attribute at %L", &expr->where);
		  return FAILURE;
		}
	      else if (ref->u.c.component->attr.allocatable)
		{
		  gfc_error ("Component to the right of a part reference "
			     "with nonzero rank must not have the ALLOCATABLE "
			     "attribute at %L", &expr->where);
		  return FAILURE;
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
	  return FAILURE;
	}

      if (ref->type == REF_COMPONENT)
	{
	  if (current_part_dimension)
	    seen_part_dimension = 1;

	  /* reset to make sure */
	  current_part_dimension = 0;
	}
    }

  return SUCCESS;
}


/* Given an expression, determine its shape.  This is easier than it sounds.
   Leaves the shape array NULL if it is not possible to determine the shape.  */

static void
expression_shape (gfc_expr *e)
{
  mpz_t array[GFC_MAX_DIMENSIONS];
  int i;

  if (e->rank == 0 || e->shape != NULL)
    return;

  for (i = 0; i < e->rank; i++)
    if (gfc_array_dimen_size (e, i, &array[i]) == FAILURE)
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

static void
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


/* Resolve a variable expression.  */

static gfc_try
resolve_variable (gfc_expr *e)
{
  gfc_symbol *sym;
  gfc_try t;

  t = SUCCESS;

  if (e->symtree == NULL)
    return FAILURE;

  if (e->ref && resolve_ref (e) == FAILURE)
    return FAILURE;

  sym = e->symtree->n.sym;
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
  else
    {
      /* Must be a simple variable reference.  */
      if (gfc_set_default_type (sym, 1, sym->ns) == FAILURE)
	return FAILURE;
      e->ts = sym->ts;
    }

  if (check_assumed_size_reference (sym, e))
    return FAILURE;

  /* Deal with forward references to entries during resolve_code, to
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
      bool seen;

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
		  seen = true;
	      }

	  /*  If it has not been seen as a dummy, this is an error.  */
	  if (!seen)
	    {
	      if (specification_expr)
		gfc_error ("Variable '%s', used in a specification expression"
			   ", is referenced at %L before the ENTRY statement "
			   "in which it is a parameter",
			   sym->name, &cs_base->current->loc);
	      else
		gfc_error ("Variable '%s' is used at %L before the ENTRY "
			   "statement in which it is a parameter",
			   sym->name, &cs_base->current->loc);
	      t = FAILURE;
	    }
	}

      /* Now do the same check on the specification expressions.  */
      specification_expr = 1;
      if (sym->ts.type == BT_CHARACTER
	  && gfc_resolve_expr (sym->ts.cl->length) == FAILURE)
	t = FAILURE;

      if (sym->as)
	for (n = 0; n < sym->as->rank; n++)
	  {
	     specification_expr = 1;
	     if (gfc_resolve_expr (sym->as->lower[n]) == FAILURE)
	       t = FAILURE;
	     specification_expr = 1;
	     if (gfc_resolve_expr (sym->as->upper[n]) == FAILURE)
	       t = FAILURE;
	  }
      specification_expr = 0;

      if (t == SUCCESS)
	/* Update the symbol's entry level.  */
	sym->entry_id = current_entry_id + 1;
    }

resolve_procedure:
  if (t == SUCCESS && resolve_procedure_expression (e) == FAILURE)
    t = FAILURE;

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
	  if (e->shape != NULL)
	    {
	      for (n = 0; n < e->rank; n++)
		mpz_clear (e->shape[n]);

	      gfc_free (e->shape);
	    }

	  /* Give the expression the right symtree!  */
	  gfc_find_sym_tree (e->symtree->name, NULL, 1, &st);
	  gcc_assert (st != NULL);

	  if (old_sym->attr.flavor == FL_PROCEDURE
		|| e->expr_type == EXPR_FUNCTION)
  	    {
	      /* Original was function so point to the new symbol, since
		 the actual argument list is already attached to the
		 expression. */
	      e->value.function.esym = NULL;
	      e->symtree = st;
	    }
	  else
	    {
	      /* Original was variable so convert array references into
		 an actual arglist. This does not need any checking now
		 since gfc_resolve_function will take care of it.  */
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

  if (op1->ts.cl && op1->ts.cl->length)
    e1 = gfc_copy_expr (op1->ts.cl->length);
  else if (op1->expr_type == EXPR_CONSTANT)
    e1 = gfc_int_expr (op1->value.character.length);

  if (op2->ts.cl && op2->ts.cl->length)
    e2 = gfc_copy_expr (op2->ts.cl->length);
  else if (op2->expr_type == EXPR_CONSTANT)
    e2 = gfc_int_expr (op2->value.character.length);

  e->ts.cl = gfc_new_charlen (gfc_current_ns);

  if (!e1 || !e2)
    return;

  e->ts.cl->length = gfc_add (e1, e2);
  e->ts.cl->length->ts.type = BT_INTEGER;
  e->ts.cl->length->ts.kind = gfc_charlen_int_kind;
  gfc_simplify_expr (e->ts.cl->length, 0);
  gfc_resolve_expr (e->ts.cl->length);

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

    case EXPR_ARRAY:
      if (e->expr_type == EXPR_ARRAY)
	gfc_resolve_character_array_constructor (e);

    case EXPR_SUBSTRING:
      if (!e->ts.cl && e->ref)
	gfc_resolve_substring_charlen (e);

    default:
      if (!e->ts.cl)
	e->ts.cl = gfc_new_charlen (gfc_current_ns);

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

  po = gfc_get_expr ();
  po->expr_type = EXPR_VARIABLE;
  po->symtree = e->symtree;
  po->ref = gfc_copy_ref (e->ref);

  if (gfc_resolve_expr (po) == FAILURE)
    return NULL;

  return po;
}


/* Update the arglist of an EXPR_COMPCALL expression to include the
   passed-object.  */

static gfc_try
update_compcall_arglist (gfc_expr* e)
{
  gfc_expr* po;
  gfc_typebound_proc* tbp;

  tbp = e->value.compcall.tbp;

  if (tbp->error)
    return FAILURE;

  po = extract_compcall_passed_object (e);
  if (!po)
    return FAILURE;

  if (po->rank > 0)
    {
      gfc_error ("Passed-object at %L must be scalar", &e->where);
      return FAILURE;
    }

  if (tbp->nopass)
    {
      gfc_free_expr (po);
      return SUCCESS;
    }

  gcc_assert (tbp->pass_arg_num > 0);
  e->value.compcall.actual = update_arglist_pass (e->value.compcall.actual, po,
						  tbp->pass_arg_num,
						  tbp->pass_arg);

  return SUCCESS;
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

  /* Remove PPC reference.  */
  ref = &po->ref;
  while ((*ref)->next)
    (*ref) = (*ref)->next;
  gfc_free_ref_list (*ref);
  *ref = NULL;

  if (gfc_resolve_expr (po) == FAILURE)
    return NULL;

  return po;
}


/* Update the actual arglist of a procedure pointer component to include the
   passed-object.  */

static gfc_try
update_ppc_arglist (gfc_expr* e)
{
  gfc_expr* po;
  gfc_component *ppc;
  gfc_typebound_proc* tb;

  if (!gfc_is_proc_ptr_comp (e, &ppc))
    return FAILURE;

  tb = ppc->tb;

  if (tb->error)
    return FAILURE;
  else if (tb->nopass)
    return SUCCESS;

  po = extract_ppc_passed_object (e);
  if (!po)
    return FAILURE;

  if (po->rank > 0)
    {
      gfc_error ("Passed-object at %L must be scalar", &e->where);
      return FAILURE;
    }

  gcc_assert (tb->pass_arg_num > 0);
  e->value.compcall.actual = update_arglist_pass (e->value.compcall.actual, po,
						  tb->pass_arg_num,
						  tb->pass_arg);

  return SUCCESS;
}


/* Check that the object a TBP is called on is valid, i.e. it must not be
   of ABSTRACT type (as in subobject%abstract_parent%tbp()).  */

static gfc_try
check_typebound_baseobject (gfc_expr* e)
{
  gfc_expr* base;

  base = extract_compcall_passed_object (e);
  if (!base)
    return FAILURE;

  gcc_assert (base->ts.type == BT_DERIVED);
  if (base->ts.derived->attr.abstract)
    {
      gfc_error ("Base object for type-bound procedure call at %L is of"
		 " ABSTRACT type '%s'", &e->where, base->ts.derived->name);
      return FAILURE;
    }

  return SUCCESS;
}


/* Resolve a call to a type-bound procedure, either function or subroutine,
   statically from the data in an EXPR_COMPCALL expression.  The adapted
   arglist and the target-procedure symtree are returned.  */

static gfc_try
resolve_typebound_static (gfc_expr* e, gfc_symtree** target,
			  gfc_actual_arglist** actual)
{
  gcc_assert (e->expr_type == EXPR_COMPCALL);
  gcc_assert (!e->value.compcall.tbp->is_generic);

  /* Update the actual arglist for PASS.  */
  if (update_compcall_arglist (e) == FAILURE)
    return FAILURE;

  *actual = e->value.compcall.actual;
  *target = e->value.compcall.tbp->u.specific;

  gfc_free_ref_list (e->ref);
  e->ref = NULL;
  e->value.compcall.actual = NULL;

  return SUCCESS;
}


/* Given an EXPR_COMPCALL calling a GENERIC typebound procedure, figure out
   which of the specific bindings (if any) matches the arglist and transform
   the expression into a call of that binding.  */

static gfc_try
resolve_typebound_generic_call (gfc_expr* e)
{
  gfc_typebound_proc* genproc;
  const char* genname;

  gcc_assert (e->expr_type == EXPR_COMPCALL);
  genname = e->value.compcall.name;
  genproc = e->value.compcall.tbp;

  if (!genproc->is_generic)
    return SUCCESS;

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
		return FAILURE;

	      gcc_assert (g->specific->pass_arg_num > 0);
	      gcc_assert (!g->specific->error);
	      args = update_arglist_pass (args, po, g->specific->pass_arg_num,
					  g->specific->pass_arg);
	    }
	  resolve_actual_arglist (args, target->attr.proc,
				  is_external_proc (target) && !target->formal);

	  /* Check if this arglist matches the formal.  */
	  matches = gfc_arglist_matches_symbol (&args, target);

	  /* Clean up and break out of the loop if we've found it.  */
	  gfc_free_actual_arglist (args);
	  if (matches)
	    {
	      e->value.compcall.tbp = g->specific;
	      goto success;
	    }
	}
    }

  /* Nothing matching found!  */
  gfc_error ("Found no matching specific binding for the call to the GENERIC"
	     " '%s' at %L", genname, &e->where);
  return FAILURE;

success:
  return SUCCESS;
}


/* Resolve a call to a type-bound subroutine.  */

static gfc_try
resolve_typebound_call (gfc_code* c)
{
  gfc_actual_arglist* newactual;
  gfc_symtree* target;

  /* Check that's really a SUBROUTINE.  */
  if (!c->expr1->value.compcall.tbp->subroutine)
    {
      gfc_error ("'%s' at %L should be a SUBROUTINE",
		 c->expr1->value.compcall.name, &c->loc);
      return FAILURE;
    }

  if (check_typebound_baseobject (c->expr1) == FAILURE)
    return FAILURE;

  if (resolve_typebound_generic_call (c->expr1) == FAILURE)
    return FAILURE;

  /* Transform into an ordinary EXEC_CALL for now.  */

  if (resolve_typebound_static (c->expr1, &target, &newactual) == FAILURE)
    return FAILURE;

  c->ext.actual = newactual;
  c->symtree = target;
  c->op = EXEC_CALL;

  gcc_assert (!c->expr1->ref && !c->expr1->value.compcall.actual);
  gfc_free_expr (c->expr1);
  c->expr1 = NULL;

  return resolve_call (c);
}


/* Resolve a component-call expression.  */

static gfc_try
resolve_compcall (gfc_expr* e)
{
  gfc_actual_arglist* newactual;
  gfc_symtree* target;

  /* Check that's really a FUNCTION.  */
  if (!e->value.compcall.tbp->function)
    {
      gfc_error ("'%s' at %L should be a FUNCTION",
		 e->value.compcall.name, &e->where);
      return FAILURE;
    }

  if (check_typebound_baseobject (e) == FAILURE)
    return FAILURE;

  if (resolve_typebound_generic_call (e) == FAILURE)
    return FAILURE;
  gcc_assert (!e->value.compcall.tbp->is_generic);

  /* Take the rank from the function's symbol.  */
  if (e->value.compcall.tbp->u.specific->n.sym->as)
    e->rank = e->value.compcall.tbp->u.specific->n.sym->as->rank;

  /* For now, we simply transform it into an EXPR_FUNCTION call with the same
     arglist to the TBP's binding target.  */

  if (resolve_typebound_static (e, &target, &newactual) == FAILURE)
    return FAILURE;

  e->value.function.actual = newactual;
  e->value.function.name = e->value.compcall.name;
  e->value.function.esym = target->n.sym;
  e->value.function.isym = NULL;
  e->symtree = target;
  e->ts = target->n.sym->ts;
  e->expr_type = EXPR_FUNCTION;

  return gfc_resolve_expr (e);
}


/* Resolve a CALL to a Procedure Pointer Component (Subroutine).  */

static gfc_try
resolve_ppc_call (gfc_code* c)
{
  gfc_component *comp;
  gcc_assert (gfc_is_proc_ptr_comp (c->expr1, &comp));

  c->resolved_sym = c->expr1->symtree->n.sym;
  c->expr1->expr_type = EXPR_VARIABLE;

  if (!comp->attr.subroutine)
    gfc_add_subroutine (&comp->attr, comp->name, &c->expr1->where);

  if (resolve_ref (c->expr1) == FAILURE)
    return FAILURE;

  if (update_ppc_arglist (c->expr1) == FAILURE)
    return FAILURE;

  c->ext.actual = c->expr1->value.compcall.actual;

  if (resolve_actual_arglist (c->ext.actual, comp->attr.proc,
			      comp->formal == NULL) == FAILURE)
    return FAILURE;

  gfc_ppc_use (comp, &c->expr1->value.compcall.actual, &c->expr1->where);

  return SUCCESS;
}


/* Resolve a Function Call to a Procedure Pointer Component (Function).  */

static gfc_try
resolve_expr_ppc (gfc_expr* e)
{
  gfc_component *comp;
  gcc_assert (gfc_is_proc_ptr_comp (e, &comp));

  /* Convert to EXPR_FUNCTION.  */
  e->expr_type = EXPR_FUNCTION;
  e->value.function.isym = NULL;
  e->value.function.actual = e->value.compcall.actual;
  e->ts = comp->ts;
  if (comp->as != NULL)
    e->rank = comp->as->rank;

  if (!comp->attr.function)
    gfc_add_function (&comp->attr, comp->name, &e->where);

  if (resolve_ref (e) == FAILURE)
    return FAILURE;

  if (resolve_actual_arglist (e->value.function.actual, comp->attr.proc,
			      comp->formal == NULL) == FAILURE)
    return FAILURE;

  if (update_ppc_arglist (e) == FAILURE)
    return FAILURE;

  gfc_ppc_use (comp, &e->value.compcall.actual, &e->where);

  return SUCCESS;
}


/* Resolve an expression.  That is, make sure that types of operands agree
   with their operators, intrinsic operators are converted to function calls
   for overloaded types and unresolved function references are resolved.  */

gfc_try
gfc_resolve_expr (gfc_expr *e)
{
  gfc_try t;

  if (e == NULL)
    return SUCCESS;

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
	{
	  t = resolve_variable (e);
	  if (t == SUCCESS)
	    expression_rank (e);
	}

      if (e->ts.type == BT_CHARACTER && e->ts.cl == NULL && e->ref
	  && e->ref->type != REF_SUBSTRING)
	gfc_resolve_substring_charlen (e);

      break;

    case EXPR_COMPCALL:
      t = resolve_compcall (e);
      break;

    case EXPR_SUBSTRING:
      t = resolve_ref (e);
      break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
      t = SUCCESS;
      break;

    case EXPR_PPC:
      t = resolve_expr_ppc (e);
      break;

    case EXPR_ARRAY:
      t = FAILURE;
      if (resolve_ref (e) == FAILURE)
	break;

      t = gfc_resolve_array_constructor (e);
      /* Also try to expand a constructor.  */
      if (t == SUCCESS)
	{
	  expression_rank (e);
	  gfc_expand_constructor (e);
	}

      /* This provides the opportunity for the length of constructors with
	 character valued function elements to propagate the string length
	 to the expression.  */
      if (t == SUCCESS && e->ts.type == BT_CHARACTER)
	t = gfc_resolve_character_array_constructor (e);

      break;

    case EXPR_STRUCTURE:
      t = resolve_ref (e);
      if (t == FAILURE)
	break;

      t = resolve_structure_cons (e);
      if (t == FAILURE)
	break;

      t = gfc_simplify_expr (e, 0);
      break;

    default:
      gfc_internal_error ("gfc_resolve_expr(): Bad expression type");
    }

  if (e->ts.type == BT_CHARACTER && t == SUCCESS && !e->ts.cl)
    fixup_charlen (e);

  return t;
}


/* Resolve an expression from an iterator.  They must be scalar and have
   INTEGER or (optionally) REAL type.  */

static gfc_try
gfc_resolve_iterator_expr (gfc_expr *expr, bool real_ok,
			   const char *name_msgid)
{
  if (gfc_resolve_expr (expr) == FAILURE)
    return FAILURE;

  if (expr->rank != 0)
    {
      gfc_error ("%s at %L must be a scalar", _(name_msgid), &expr->where);
      return FAILURE;
    }

  if (expr->ts.type != BT_INTEGER)
    {
      if (expr->ts.type == BT_REAL)
	{
	  if (real_ok)
	    return gfc_notify_std (GFC_STD_F95_DEL,
				   "Deleted feature: %s at %L must be integer",
				   _(name_msgid), &expr->where);
	  else
	    {
	      gfc_error ("%s at %L must be INTEGER", _(name_msgid),
			 &expr->where);
	      return FAILURE;
	    }
	}
      else
	{
	  gfc_error ("%s at %L must be INTEGER", _(name_msgid), &expr->where);
	  return FAILURE;
	}
    }
  return SUCCESS;
}


/* Resolve the expressions in an iterator structure.  If REAL_OK is
   false allow only INTEGER type iterators, otherwise allow REAL types.  */

gfc_try
gfc_resolve_iterator (gfc_iterator *iter, bool real_ok)
{
  if (gfc_resolve_iterator_expr (iter->var, real_ok, "Loop variable")
      == FAILURE)
    return FAILURE;

  if (gfc_pure (NULL) && gfc_impure_variable (iter->var->symtree->n.sym))
    {
      gfc_error ("Cannot assign to loop variable in PURE procedure at %L",
		 &iter->var->where);
      return FAILURE;
    }

  if (gfc_resolve_iterator_expr (iter->start, real_ok,
				 "Start expression in DO loop") == FAILURE)
    return FAILURE;

  if (gfc_resolve_iterator_expr (iter->end, real_ok,
				 "End expression in DO loop") == FAILURE)
    return FAILURE;

  if (gfc_resolve_iterator_expr (iter->step, real_ok,
				 "Step expression in DO loop") == FAILURE)
    return FAILURE;

  if (iter->step->expr_type == EXPR_CONSTANT)
    {
      if ((iter->step->ts.type == BT_INTEGER
	   && mpz_cmp_ui (iter->step->value.integer, 0) == 0)
	  || (iter->step->ts.type == BT_REAL
	      && mpfr_sgn (iter->step->value.real) == 0))
	{
	  gfc_error ("Step expression in DO loop at %L cannot be zero",
		     &iter->step->where);
	  return FAILURE;
	}
    }

  /* Convert start, end, and step to the same type as var.  */
  if (iter->start->ts.kind != iter->var->ts.kind
      || iter->start->ts.type != iter->var->ts.type)
    gfc_convert_type (iter->start, &iter->var->ts, 2);

  if (iter->end->ts.kind != iter->var->ts.kind
      || iter->end->ts.type != iter->var->ts.type)
    gfc_convert_type (iter->end, &iter->var->ts, 2);

  if (iter->step->ts.kind != iter->var->ts.kind
      || iter->step->ts.type != iter->var->ts.type)
    gfc_convert_type (iter->step, &iter->var->ts, 2);

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
      if ((sgn > 0 && cmp < 0) || (sgn < 0 && cmp > 0))
	gfc_warning ("DO loop at %L will be executed zero times",
		     &iter->step->where);
    }

  return SUCCESS;
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
   Returns SUCCESS if SYM is found in EXPR.  */

gfc_try
find_forall_index (gfc_expr *expr, gfc_symbol *sym, int f)
{
  if (gfc_traverse_expr (expr, sym, forall_index, f))
    return SUCCESS;
  else
    return FAILURE;
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
      if (gfc_resolve_expr (iter->var) == SUCCESS
	  && (iter->var->ts.type != BT_INTEGER || iter->var->rank != 0))
	gfc_error ("FORALL index-name at %L must be a scalar INTEGER",
		   &iter->var->where);

      if (gfc_resolve_expr (iter->start) == SUCCESS
	  && (iter->start->ts.type != BT_INTEGER || iter->start->rank != 0))
	gfc_error ("FORALL start expression at %L must be a scalar INTEGER",
		   &iter->start->where);
      if (iter->var->ts.kind != iter->start->ts.kind)
	gfc_convert_type (iter->start, &iter->var->ts, 2);

      if (gfc_resolve_expr (iter->end) == SUCCESS
	  && (iter->end->ts.type != BT_INTEGER || iter->end->rank != 0))
	gfc_error ("FORALL end expression at %L must be a scalar INTEGER",
		   &iter->end->where);
      if (iter->var->ts.kind != iter->end->ts.kind)
	gfc_convert_type (iter->end, &iter->var->ts, 2);

      if (gfc_resolve_expr (iter->stride) == SUCCESS)
	{
	  if (iter->stride->ts.type != BT_INTEGER || iter->stride->rank != 0)
	    gfc_error ("FORALL stride expression at %L must be a scalar %s",
		       &iter->stride->where, "INTEGER");

	  if (iter->stride->expr_type == EXPR_CONSTANT
	      && mpz_cmp_ui(iter->stride->value.integer, 0) == 0)
	    gfc_error ("FORALL stride expression at %L cannot be zero",
		       &iter->stride->where);
	}
      if (iter->var->ts.kind != iter->stride->ts.kind)
	gfc_convert_type (iter->stride, &iter->var->ts, 2);
    }

  for (iter = it; iter; iter = iter->next)
    for (iter2 = iter; iter2; iter2 = iter2->next)
      {
	if (find_forall_index (iter2->start,
			       iter->var->symtree->n.sym, 0) == SUCCESS
	    || find_forall_index (iter2->end,
				  iter->var->symtree->n.sym, 0) == SUCCESS
	    || find_forall_index (iter2->stride,
				  iter->var->symtree->n.sym, 0) == SUCCESS)
	  gfc_error ("FORALL index '%s' may not appear in triplet "
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
	if (c->ts.type == BT_DERIVED && derived_inaccessible (c->ts.derived))
	  return 1;
    }

  return 0;
}


/* Resolve the argument of a deallocate expression.  The expression must be
   a pointer or a full array.  */

static gfc_try
resolve_deallocate_expr (gfc_expr *e)
{
  symbol_attribute attr;
  int allocatable, pointer, check_intent_in;
  gfc_ref *ref;

  /* Check INTENT(IN), unless the object is a sub-component of a pointer.  */
  check_intent_in = 1;

  if (gfc_resolve_expr (e) == FAILURE)
    return FAILURE;

  if (e->expr_type != EXPR_VARIABLE)
    goto bad;

  allocatable = e->symtree->n.sym->attr.allocatable;
  pointer = e->symtree->n.sym->attr.pointer;
  for (ref = e->ref; ref; ref = ref->next)
    {
      if (pointer)
	check_intent_in = 0;

      switch (ref->type)
	{
	case REF_ARRAY:
	  if (ref->u.ar.type != AR_FULL)
	    allocatable = 0;
	  break;

	case REF_COMPONENT:
	  allocatable = (ref->u.c.component->as != NULL
			 && ref->u.c.component->as->type == AS_DEFERRED);
	  pointer = ref->u.c.component->attr.pointer;
	  break;

	case REF_SUBSTRING:
	  allocatable = 0;
	  break;
	}
    }

  attr = gfc_expr_attr (e);

  if (allocatable == 0 && attr.pointer == 0)
    {
    bad:
      gfc_error ("Allocate-object at %L must be ALLOCATABLE or a POINTER",
		 &e->where);
    }

  if (check_intent_in
      && e->symtree->n.sym->attr.intent == INTENT_IN)
    {
      gfc_error ("Cannot deallocate INTENT(IN) variable '%s' at %L",
		 e->symtree->n.sym->name, &e->where);
      return FAILURE;
    }

  return SUCCESS;
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

static gfc_expr *
expr_to_initialize (gfc_expr *e)
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

	result->rank = ref->u.ar.dimen;
	break;
      }

  return result;
}


/* Resolve the expression in an ALLOCATE statement, doing the additional
   checks to see whether the expression is OK or not.  The expression must
   have a trailing array reference that gives the size of the array.  */

static gfc_try
resolve_allocate_expr (gfc_expr *e, gfc_code *code)
{
  int i, pointer, allocatable, dimension, check_intent_in;
  symbol_attribute attr;
  gfc_ref *ref, *ref2;
  gfc_array_ref *ar;
  gfc_code *init_st;
  gfc_expr *init_e;
  gfc_symbol *sym;
  gfc_alloc *a;

  /* Check INTENT(IN), unless the object is a sub-component of a pointer.  */
  check_intent_in = 1;

  if (gfc_resolve_expr (e) == FAILURE)
    return FAILURE;

  /* Make sure the expression is allocatable or a pointer.  If it is
     pointer, the next-to-last reference must be a pointer.  */

  ref2 = NULL;

  if (e->expr_type != EXPR_VARIABLE)
    {
      allocatable = 0;
      attr = gfc_expr_attr (e);
      pointer = attr.pointer;
      dimension = attr.dimension;
    }
  else
    {
      allocatable = e->symtree->n.sym->attr.allocatable;
      pointer = e->symtree->n.sym->attr.pointer;
      dimension = e->symtree->n.sym->attr.dimension;

      for (ref = e->ref; ref; ref2 = ref, ref = ref->next)
	{
	  if (pointer)
	    check_intent_in = 0;

	  switch (ref->type)
	    {
 	      case REF_ARRAY:
		if (ref->next != NULL)
		  pointer = 0;
		break;

	      case REF_COMPONENT:
		allocatable = (ref->u.c.component->as != NULL
			       && ref->u.c.component->as->type == AS_DEFERRED);

		pointer = ref->u.c.component->attr.pointer;
		dimension = ref->u.c.component->attr.dimension;
		break;

	      case REF_SUBSTRING:
		allocatable = 0;
		pointer = 0;
		break;
	    }
	}
    }

  if (allocatable == 0 && pointer == 0)
    {
      gfc_error ("Allocate-object at %L must be ALLOCATABLE or a POINTER",
		 &e->where);
      return FAILURE;
    }

  if (check_intent_in
      && e->symtree->n.sym->attr.intent == INTENT_IN)
    {
      gfc_error ("Cannot allocate INTENT(IN) variable '%s' at %L",
		 e->symtree->n.sym->name, &e->where);
      return FAILURE;
    }

  /* Add default initializer for those derived types that need them.  */
  if (e->ts.type == BT_DERIVED && (init_e = gfc_default_initializer (&e->ts)))
    {
      init_st = gfc_get_code ();
      init_st->loc = code->loc;
      init_st->op = EXEC_INIT_ASSIGN;
      init_st->expr1 = expr_to_initialize (e);
      init_st->expr2 = init_e;
      init_st->next = code->next;
      code->next = init_st;
    }

  if (pointer && dimension == 0)
    return SUCCESS;

  /* Make sure the next-to-last reference node is an array specification.  */

  if (ref2 == NULL || ref2->type != REF_ARRAY || ref2->u.ar.type == AR_FULL)
    {
      gfc_error ("Array specification required in ALLOCATE statement "
		 "at %L", &e->where);
      return FAILURE;
    }

  /* Make sure that the array section reference makes sense in the
    context of an ALLOCATE specification.  */

  ar = &ref2->u.ar;

  for (i = 0; i < ar->dimen; i++)
    {
      if (ref2->u.ar.type == AR_ELEMENT)
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

	  /* Fall Through...  */

	case DIMEN_UNKNOWN:
	case DIMEN_VECTOR:
	  gfc_error ("Bad array specification in ALLOCATE statement at %L",
		     &e->where);
	  return FAILURE;
	}

check_symbols:

      for (a = code->ext.alloc_list; a; a = a->next)
	{
	  sym = a->expr->symtree->n.sym;

	  /* TODO - check derived type components.  */
	  if (sym->ts.type == BT_DERIVED)
	    continue;

	  if ((ar->start[i] != NULL
	       && gfc_find_sym_in_expr (sym, ar->start[i]))
	      || (ar->end[i] != NULL
		  && gfc_find_sym_in_expr (sym, ar->end[i])))
	    {
	      gfc_error ("'%s' must not appear in the array specification at "
			 "%L in the same ALLOCATE statement where it is "
			 "itself allocated", sym->name, &ar->where);
	      return FAILURE;
	    }
	}
    }

  return SUCCESS;
}

static void
resolve_allocate_deallocate (gfc_code *code, const char *fcn)
{
  gfc_expr *stat, *errmsg, *pe, *qe;
  gfc_alloc *a, *p, *q;

  stat = code->expr1 ? code->expr1 : NULL;

  errmsg = code->expr2 ? code->expr2 : NULL;

  /* Check the stat variable.  */
  if (stat)
    {
      if (stat->symtree->n.sym->attr.intent == INTENT_IN)
	gfc_error ("Stat-variable '%s' at %L cannot be INTENT(IN)",
		   stat->symtree->n.sym->name, &stat->where);

      if (gfc_pure (NULL) && gfc_impure_variable (stat->symtree->n.sym))
	gfc_error ("Illegal stat-variable at %L for a PURE procedure",
		   &stat->where);

      if (stat->ts.type != BT_INTEGER
	  && !(stat->ref && (stat->ref->type == REF_ARRAY
	       || stat->ref->type == REF_COMPONENT)))
	gfc_error ("Stat-variable at %L must be a scalar INTEGER "
		   "variable", &stat->where);

      for (p = code->ext.alloc_list; p; p = p->next)
	if (p->expr->symtree->n.sym->name == stat->symtree->n.sym->name)
	  gfc_error ("Stat-variable at %L shall not be %sd within "
		     "the same %s statement", &stat->where, fcn, fcn);
    }

  /* Check the errmsg variable.  */
  if (errmsg)
    {
      if (!stat)
	gfc_warning ("ERRMSG at %L is useless without a STAT tag",
		     &errmsg->where);

      if (errmsg->symtree->n.sym->attr.intent == INTENT_IN)
	gfc_error ("Errmsg-variable '%s' at %L cannot be INTENT(IN)",
		   errmsg->symtree->n.sym->name, &errmsg->where);

      if (gfc_pure (NULL) && gfc_impure_variable (errmsg->symtree->n.sym))
	gfc_error ("Illegal errmsg-variable at %L for a PURE procedure",
		   &errmsg->where);

      if (errmsg->ts.type != BT_CHARACTER
	  && !(errmsg->ref
	       && (errmsg->ref->type == REF_ARRAY
	  	   || errmsg->ref->type == REF_COMPONENT)))
	gfc_error ("Errmsg-variable at %L must be a scalar CHARACTER "
		   "variable", &errmsg->where);

      for (p = code->ext.alloc_list; p; p = p->next)
	if (p->expr->symtree->n.sym->name == errmsg->symtree->n.sym->name)
	  gfc_error ("Errmsg-variable at %L shall not be %sd within "
		     "the same %s statement", &errmsg->where, fcn, fcn);
    }

  /* Check that an allocate-object appears only once in the statement.  
     FIXME: Checking derived types is disabled.  */
  for (p = code->ext.alloc_list; p; p = p->next)
    {
      pe = p->expr;
      if ((pe->ref && pe->ref->type != REF_COMPONENT)
	   && (pe->symtree->n.sym->ts.type != BT_DERIVED))
	{
	  for (q = p->next; q; q = q->next)
	    {
	      qe = q->expr;
	      if ((qe->ref && qe->ref->type != REF_COMPONENT)
		  && (qe->symtree->n.sym->ts.type != BT_DERIVED)
		  && (pe->symtree->n.sym->name == qe->symtree->n.sym->name))
		gfc_error ("Allocate-object at %L also appears at %L",
			   &pe->where, &qe->where);
	    }
	}
    }

  if (strcmp (fcn, "ALLOCATE") == 0)
    {
      for (a = code->ext.alloc_list; a; a = a->next)
	resolve_allocate_expr (a->expr, code);
    }
  else
    {
      for (a = code->ext.alloc_list; a; a = a->next)
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
   type.  Return FAILURE if anything is wrong.  */

static gfc_try
validate_case_label_expr (gfc_expr *e, gfc_expr *case_expr)
{
  if (e == NULL) return SUCCESS;

  if (e->ts.type != case_expr->ts.type)
    {
      gfc_error ("Expression in CASE statement at %L must be of type %s",
		 &e->where, gfc_basic_typename (case_expr->ts.type));
      return FAILURE;
    }

  /* C805 (R808) For a given case-construct, each case-value shall be of
     the same type as case-expr.  For character type, length differences
     are allowed, but the kind type parameters shall be the same.  */

  if (case_expr->ts.type == BT_CHARACTER && e->ts.kind != case_expr->ts.kind)
    {
      gfc_error ("Expression in CASE statement at %L must be of kind %d",
		 &e->where, case_expr->ts.kind);
      return FAILURE;
    }

  /* Convert the case value kind to that of case expression kind, if needed.
     FIXME:  Should a warning be issued?  */
  if (e->ts.kind != case_expr->ts.kind)
    gfc_convert_type_warn (e, &case_expr->ts, 2, 0);

  if (e->rank != 0)
    {
      gfc_error ("Expression in CASE statement at %L must be scalar",
		 &e->where);
      return FAILURE;
    }

  return SUCCESS;
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
resolve_select (gfc_code *code)
{
  gfc_code *body;
  gfc_expr *case_expr;
  gfc_case *cp, *default_case, *tail, *head;
  int seen_unreachable;
  int seen_logical;
  int ncases;
  bt type;
  gfc_try t;

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
  if (type != BT_LOGICAL && type != BT_INTEGER && type != BT_CHARACTER)
    {
      gfc_error ("Argument of SELECT statement at %L cannot be %s",
		 &case_expr->where, gfc_typename (&case_expr->ts));

      /* Punt. Going on here just produce more garbage error messages.  */
      return;
    }

  if (case_expr->rank != 0)
    {
      gfc_error ("Argument of SELECT statement at %L must be a scalar "
		 "expression", &case_expr->where);

      /* Punt.  */
      return;
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
	  for (cp = body->ext.case_list; cp; cp = cp->next)
	    {
	      /* Intercept the DEFAULT case.  It does not have a kind.  */
	      if (cp->low == NULL && cp->high == NULL)
		continue;

	      /* Unreachable case ranges are discarded, so ignore.  */
	      if (cp->low != NULL && cp->high != NULL
		  && cp->low != cp->high
		  && gfc_compare_expr (cp->low, cp->high, INTRINSIC_GT) > 0)
		continue;

	      /* FIXME: Should a warning be issued?  */
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
      t = SUCCESS;
      seen_unreachable = 0;

      /* Walk the case label list, making sure that all case labels
	 are legal.  */
      for (cp = body->ext.case_list; cp; cp = cp->next)
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
		  t = FAILURE;
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
	  if(validate_case_label_expr (cp->low, case_expr) != SUCCESS
	     || validate_case_label_expr (cp->high, case_expr) != SUCCESS)
	    {
	      t = FAILURE;
	      break;
	    }

	  if (type == BT_LOGICAL
	      && ((cp->low == NULL || cp->high == NULL)
		  || cp->low != cp->high))
	    {
	      gfc_error ("Logical range in CASE statement at %L is not "
			 "allowed", &cp->low->where);
	      t = FAILURE;
	      break;
	    }

	  if (type == BT_LOGICAL && cp->low->expr_type == EXPR_CONSTANT)
	    {
	      int value;
	      value = cp->low->value.logical == 0 ? 2 : 1;
	      if (value & seen_logical)
		{
		  gfc_error ("constant logical value in CASE statement "
			     "is repeated at %L",
			     &cp->low->where);
		  t = FAILURE;
		  break;
		}
	      seen_logical |= value;
	    }

	  if (cp->low != NULL && cp->high != NULL
	      && cp->low != cp->high
	      && gfc_compare_expr (cp->low, cp->high, INTRINSIC_GT) > 0)
	    {
	      if (gfc_option.warn_surprising)
		gfc_warning ("Range specification at %L can never "
			     "be matched", &cp->where);

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
      if (t == FAILURE)
	continue;

      /* See if any case labels that are unreachable have been seen.
	 If so, we eliminate them.  This is a bit of a kludge because
	 the case lists for a single case statement (label) is a
	 single forward linked lists.  */
      if (seen_unreachable)
      {
	/* Advance until the first case in the list is reachable.  */
	while (body->ext.case_list != NULL
	       && body->ext.case_list->unreachable)
	  {
	    gfc_case *n = body->ext.case_list;
	    body->ext.case_list = body->ext.case_list->next;
	    n->next = NULL;
	    gfc_free_case_list (n);
	  }

	/* Strip all other unreachable cases.  */
	if (body->ext.case_list)
	  {
	    for (cp = body->ext.case_list; cp->next; cp = cp->next)
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
      if (body->block->ext.case_list == NULL)
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
  if (gfc_option.warn_surprising && type == BT_LOGICAL
      && ncases > 2)
    gfc_warning ("Logical SELECT CASE block at %L has more that two cases",
		 &code->loc);
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
  gfc_symbol *sym;
  gfc_ref *ref;
  gfc_expr *exp;

  exp = code->expr1;

  if (exp->expr_type != EXPR_VARIABLE && exp->expr_type != EXPR_FUNCTION)
    return;

  sym = exp->symtree->n.sym;
  ts = &sym->ts;

  /* Go to actual component transferred.  */
  for (ref = code->expr1->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT)
      ts = &ref->u.c.component->ts;

  if (ts->type == BT_DERIVED)
    {
      /* Check that transferred derived type doesn't contain POINTER
	 components.  */
      if (ts->derived->attr.pointer_comp)
	{
	  gfc_error ("Data transfer element at %L cannot have "
		     "POINTER components", &code->loc);
	  return;
	}

      if (ts->derived->attr.alloc_comp)
	{
	  gfc_error ("Data transfer element at %L cannot have "
		     "ALLOCATABLE components", &code->loc);
	  return;
	}

      if (derived_inaccessible (ts->derived))
	{
	  gfc_error ("Data transfer element at %L cannot have "
		     "PRIVATE components",&code->loc);
	  return;
	}
    }

  if (sym->as != NULL && sym->as->type == AS_ASSUMED_SIZE
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
      if (c->here && c->op != EXEC_END_BLOCK)
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
		 &label->where);
      return;
    }

  if (label->defined != ST_LABEL_TARGET)
    {
      gfc_error ("Statement at %L is not a valid branch target statement "
		 "for the branch statement at %L", &label->where, &code->loc);
      return;
    }

  /* Step two: make sure this branch is not a branch to itself ;-)  */

  if (code->here == label)
    {
      gfc_warning ("Branch at %L may result in an infinite loop", &code->loc);
      return;
    }

  /* Step three:  See if the label is in the same block as the
     branching statement.  The hard work has been done by setting up
     the bitmap reachable_labels.  */

  if (bitmap_bit_p (cs_base->reachable_labels, label->value))
    return;

  /* Step four:  If we haven't found the label in the bitmap, it may
    still be the label of the END of the enclosing block, in which
    case we find it by going up the code_stack.  */

  for (stack = cs_base; stack; stack = stack->prev)
    if (stack->current->next && stack->current->next->here == label)
      break;

  if (stack)
    {
      gcc_assert (stack->current->next->op == EXEC_END_BLOCK);
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

static gfc_try
resolve_where_shape (gfc_expr *expr1, gfc_expr *expr2)
{
  mpz_t shape[GFC_MAX_DIMENSIONS];
  mpz_t shape2[GFC_MAX_DIMENSIONS];
  gfc_try result = FAILURE;
  int i;

  /* Compare the rank.  */
  if (expr1->rank != expr2->rank)
    return result;

  /* Compare the size of each dimension.  */
  for (i=0; i<expr1->rank; i++)
    {
      if (gfc_array_dimen_size (expr1, i, &shape[i]) == FAILURE)
	goto ignore;

      if (gfc_array_dimen_size (expr2, i, &shape2[i]) == FAILURE)
	goto ignore;

      if (mpz_cmp (shape[i], shape2[i]))
	goto over;
    }

  /* When either of the two expression is an assumed size array, we
     ignore the comparison of dimension sizes.  */
ignore:
  result = SUCCESS;

over:
  for (i--; i >= 0; i--)
    {
      mpz_clear (shape[i]);
      mpz_clear (shape2[i]);
    }
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
	  if (resolve_where_shape (cblock->expr1, e) == FAILURE)
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
	      if (e && resolve_where_shape (cnext->expr1, e) == FAILURE)
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
	  if (find_forall_index (code->expr1, forall_index, 0) == FAILURE)
	    gfc_warning ("The FORALL with index '%s' is not used on the "
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
  int old_nvar, tmp;
  gfc_forall_iterator *fa;
  int i;

  old_nvar = nvar;

  /* Start to resolve a FORALL construct   */
  if (forall_save == 0)
    {
      /* Count the total number of FORALL index in the nested FORALL
         construct in order to allocate the VAR_EXPR with proper size.  */
      total_var = gfc_count_forall_iterators (code);

      /* Allocate VAR_EXPR with NUMBER_OF_FORALL_INDEX elements.  */
      var_expr = (gfc_expr **) gfc_getmem (total_var * sizeof (gfc_expr *));
    }

  /* The information about FORALL iterator, including FORALL index start, end
     and stride. The FORALL index can not appear in start, end or stride.  */
  for (fa = code->ext.forall_iterator; fa; fa = fa->next)
    {
      /* Check if any outer FORALL index name is the same as the current
	 one.  */
      for (i = 0; i < nvar; i++)
	{
	  if (fa->var->symtree->n.sym == var_expr[i]->symtree->n.sym)
	    {
	      gfc_error ("An outer FORALL construct already has an index "
			 "with this name %L", &fa->var->where);
	    }
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
      gfc_free (var_expr);
      total_var = 0;
    }
}


/* Resolve lists of blocks found in IF, SELECT CASE, WHERE, FORALL ,GOTO and
   DO code nodes.  */

static void resolve_code (gfc_code *, gfc_namespace *);

void
gfc_resolve_blocks (gfc_code *b, gfc_namespace *ns)
{
  gfc_try t;

  for (; b; b = b->block)
    {
      t = gfc_resolve_expr (b->expr1);
      if (gfc_resolve_expr (b->expr2) == FAILURE)
	t = FAILURE;

      switch (b->op)
	{
	case EXEC_IF:
	  if (t == SUCCESS && b->expr1 != NULL
	      && (b->expr1->ts.type != BT_LOGICAL || b->expr1->rank != 0))
	    gfc_error ("IF clause at %L requires a scalar LOGICAL expression",
		       &b->expr1->where);
	  break;

	case EXEC_WHERE:
	  if (t == SUCCESS
	      && b->expr1 != NULL
	      && (b->expr1->ts.type != BT_LOGICAL || b->expr1->rank == 0))
	    gfc_error ("WHERE/ELSEWHERE clause at %L requires a LOGICAL array",
		       &b->expr1->where);
	  break;

	case EXEC_GOTO:
	  resolve_branch (b->label1, b);
	  break;

	case EXEC_SELECT:
	case EXEC_FORALL:
	case EXEC_DO:
	case EXEC_DO_WHILE:
	case EXEC_READ:
	case EXEC_WRITE:
	case EXEC_IOLENGTH:
	case EXEC_WAIT:
	  break;

	case EXEC_OMP_ATOMIC:
	case EXEC_OMP_CRITICAL:
	case EXEC_OMP_DO:
	case EXEC_OMP_MASTER:
	case EXEC_OMP_ORDERED:
	case EXEC_OMP_PARALLEL:
	case EXEC_OMP_PARALLEL_DO:
	case EXEC_OMP_PARALLEL_SECTIONS:
	case EXEC_OMP_PARALLEL_WORKSHARE:
	case EXEC_OMP_SECTIONS:
	case EXEC_OMP_SINGLE:
	case EXEC_OMP_TASK:
	case EXEC_OMP_TASKWAIT:
	case EXEC_OMP_WORKSHARE:
	  break;

	default:
	  gfc_internal_error ("resolve_block(): Bad block type");
	}

      resolve_code (b->next, ns);
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

  if (gfc_extend_assign (code, ns) == SUCCESS)
    {
      lhs = code->ext.actual->expr;
      rhs = code->ext.actual->next->expr;
      if (gfc_pure (NULL) && !gfc_pure (code->symtree->n.sym))
	{
	  gfc_error ("Subroutine '%s' called instead of assignment at "
		     "%L must be PURE", code->symtree->n.sym->name,
		     &code->loc);
	  return rval;
	}

      /* Make a temporary rhs when there is a default initializer
	 and rhs is the same symbol as the lhs.  */
      if (rhs->expr_type == EXPR_VARIABLE
	    && rhs->symtree->n.sym->ts.type == BT_DERIVED
	    && has_default_initializer (rhs->symtree->n.sym->ts.derived)
	    && (lhs->symtree->n.sym == rhs->symtree->n.sym))
        code->ext.actual->next->expr = gfc_get_parentheses (rhs);

      return true;
    }

  lhs = code->expr1;
  rhs = code->expr2;

  if (rhs->is_boz
      && gfc_notify_std (GFC_STD_GNU, "Extension: BOZ literal at %L outside "
                         "a DATA statement and outside INT/REAL/DBLE/CMPLX",
                         &code->loc) == FAILURE)
    return false;

  /* Handle the case of a BOZ literal on the RHS.  */
  if (rhs->is_boz && lhs->ts.type != BT_INTEGER)
    {
      int rc;
      if (gfc_option.warn_surprising)
	gfc_warning ("BOZ literal at %L is bitwise transferred "
		     "non-integer symbol '%s'", &code->loc,
		     lhs->symtree->n.sym->name);

      if (!gfc_convert_boz (rhs, &lhs->ts))
	return false;
      if ((rc = gfc_range_check (rhs)) != ARITH_OK)
	{
	  if (rc == ARITH_UNDERFLOW)
	    gfc_error ("Arithmetic underflow of bit-wise transferred BOZ at %L"
		       ". This check can be disabled with the option "
		       "-fno-range-check", &rhs->where);
	  else if (rc == ARITH_OVERFLOW)
	    gfc_error ("Arithmetic overflow of bit-wise transferred BOZ at %L"
		       ". This check can be disabled with the option "
		       "-fno-range-check", &rhs->where);
	  else if (rc == ARITH_NAN)
	    gfc_error ("Arithmetic NaN of bit-wise transferred BOZ at %L"
		       ". This check can be disabled with the option "
		       "-fno-range-check", &rhs->where);
	  return false;
	}
    }


  if (lhs->ts.type == BT_CHARACTER
	&& gfc_option.warn_character_truncation)
    {
      if (lhs->ts.cl != NULL
	    && lhs->ts.cl->length != NULL
	    && lhs->ts.cl->length->expr_type == EXPR_CONSTANT)
	llen = mpz_get_si (lhs->ts.cl->length->value.integer);

      if (rhs->expr_type == EXPR_CONSTANT)
 	rlen = rhs->value.character.length;

      else if (rhs->ts.cl != NULL
	         && rhs->ts.cl->length != NULL
		 && rhs->ts.cl->length->expr_type == EXPR_CONSTANT)
	rlen = mpz_get_si (rhs->ts.cl->length->value.integer);

      if (rlen && llen && rlen > llen)
	gfc_warning_now ("CHARACTER expression will be truncated "
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
      if (gfc_impure_variable (lhs->symtree->n.sym))
	{
	  gfc_error ("Cannot assign to variable '%s' in PURE "
		     "procedure at %L",
		      lhs->symtree->n.sym->name,
		      &lhs->where);
	  return rval;
	}

      if (lhs->ts.type == BT_DERIVED
	    && lhs->expr_type == EXPR_VARIABLE
	    && lhs->ts.derived->attr.pointer_comp
	    && gfc_impure_variable (rhs->symtree->n.sym))
	{
	  gfc_error ("The impure variable at %L is assigned to "
		     "a derived type variable with a POINTER "
		     "component in a PURE procedure (12.6)",
		     &rhs->where);
	  return rval;
	}
    }

  gfc_check_assign (lhs, rhs, 1);
  return false;
}

/* Given a block of code, recursively resolve everything pointed to by this
   code block.  */

static void
resolve_code (gfc_code *code, gfc_namespace *ns)
{
  int omp_workshare_save;
  int forall_save;
  code_stack frame;
  gfc_try t;

  frame.prev = cs_base;
  frame.head = code;
  cs_base = &frame;

  find_reachable_labels (code);

  for (; code; code = code->next)
    {
      frame.current = code;
      forall_save = forall_flag;

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
	    case EXEC_OMP_PARALLEL_WORKSHARE:
	      omp_workshare_save = omp_workshare_flag;
	      omp_workshare_flag = 1;
	      gfc_resolve_omp_parallel_blocks (code, ns);
	      break;
	    case EXEC_OMP_PARALLEL:
	    case EXEC_OMP_PARALLEL_DO:
	    case EXEC_OMP_PARALLEL_SECTIONS:
	    case EXEC_OMP_TASK:
	      omp_workshare_save = omp_workshare_flag;
	      omp_workshare_flag = 0;
	      gfc_resolve_omp_parallel_blocks (code, ns);
	      break;
	    case EXEC_OMP_DO:
	      gfc_resolve_omp_do_blocks (code, ns);
	      break;
	    case EXEC_OMP_WORKSHARE:
	      omp_workshare_save = omp_workshare_flag;
	      omp_workshare_flag = 1;
	      /* FALLTHROUGH */
	    default:
	      gfc_resolve_blocks (code->block, ns);
	      break;
	    }

	  if (omp_workshare_save != -1)
	    omp_workshare_flag = omp_workshare_save;
	}

      t = SUCCESS;
      if (code->op != EXEC_COMPCALL && code->op != EXEC_CALL_PPC)
	t = gfc_resolve_expr (code->expr1);
      forall_flag = forall_save;

      if (gfc_resolve_expr (code->expr2) == FAILURE)
	t = FAILURE;

      switch (code->op)
	{
	case EXEC_NOP:
	case EXEC_END_BLOCK:
	case EXEC_CYCLE:
	case EXEC_PAUSE:
	case EXEC_STOP:
	case EXEC_EXIT:
	case EXEC_CONTINUE:
	case EXEC_DT_END:
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
		gfc_error ("Variable '%s' has not been assigned a target "
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
	  if (t == FAILURE)
	    break;

	  if (resolve_ordinary_assign (code, ns))
	    goto call;

	  break;

	case EXEC_LABEL_ASSIGN:
	  if (code->label1->defined == ST_LABEL_UNKNOWN)
	    gfc_error ("Label %d referenced at %L is never defined",
		       code->label1->value, &code->label1->where);
	  if (t == SUCCESS
	      && (code->expr1->expr_type != EXPR_VARIABLE
		  || code->expr1->symtree->n.sym->ts.type != BT_INTEGER
		  || code->expr1->symtree->n.sym->ts.kind
		     != gfc_default_integer_kind
		  || code->expr1->symtree->n.sym->as != NULL))
	    gfc_error ("ASSIGN statement at %L requires a scalar "
		       "default INTEGER variable", &code->expr1->where);
	  break;

	case EXEC_POINTER_ASSIGN:
	  if (t == FAILURE)
	    break;

	  gfc_check_pointer_assign (code->expr1, code->expr2);
	  break;

	case EXEC_ARITHMETIC_IF:
	  if (t == SUCCESS
	      && code->expr1->ts.type != BT_INTEGER
	      && code->expr1->ts.type != BT_REAL)
	    gfc_error ("Arithmetic IF statement at %L requires a numeric "
		       "expression", &code->expr1->where);

	  resolve_branch (code->label1, code);
	  resolve_branch (code->label2, code);
	  resolve_branch (code->label3, code);
	  break;

	case EXEC_IF:
	  if (t == SUCCESS && code->expr1 != NULL
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
	  resolve_typebound_call (code);
	  break;

	case EXEC_CALL_PPC:
          resolve_ppc_call (code);
	  break;

	case EXEC_SELECT:
	  /* Select is complicated. Also, a SELECT construct could be
	     a transformed computed GOTO.  */
	  resolve_select (code);
	  break;

	case EXEC_DO:
	  if (code->ext.iterator != NULL)
	    {
	      gfc_iterator *iter = code->ext.iterator;
	      if (gfc_resolve_iterator (iter, true) != FAILURE)
		gfc_resolve_do_iterator (code, iter->var->symtree->n.sym);
	    }
	  break;

	case EXEC_DO_WHILE:
	  if (code->expr1 == NULL)
	    gfc_internal_error ("resolve_code(): No expression on DO WHILE");
	  if (t == SUCCESS
	      && (code->expr1->rank != 0
		  || code->expr1->ts.type != BT_LOGICAL))
	    gfc_error ("Exit condition of DO WHILE loop at %L must be "
		       "a scalar LOGICAL expression", &code->expr1->where);
	  break;

	case EXEC_ALLOCATE:
	  if (t == SUCCESS)
	    resolve_allocate_deallocate (code, "ALLOCATE");

	  break;

	case EXEC_DEALLOCATE:
	  if (t == SUCCESS)
	    resolve_allocate_deallocate (code, "DEALLOCATE");

	  break;

	case EXEC_OPEN:
	  if (gfc_resolve_open (code->ext.open) == FAILURE)
	    break;

	  resolve_branch (code->ext.open->err, code);
	  break;

	case EXEC_CLOSE:
	  if (gfc_resolve_close (code->ext.close) == FAILURE)
	    break;

	  resolve_branch (code->ext.close->err, code);
	  break;

	case EXEC_BACKSPACE:
	case EXEC_ENDFILE:
	case EXEC_REWIND:
	case EXEC_FLUSH:
	  if (gfc_resolve_filepos (code->ext.filepos) == FAILURE)
	    break;

	  resolve_branch (code->ext.filepos->err, code);
	  break;

	case EXEC_INQUIRE:
	  if (gfc_resolve_inquire (code->ext.inquire) == FAILURE)
	      break;

	  resolve_branch (code->ext.inquire->err, code);
	  break;

	case EXEC_IOLENGTH:
	  gcc_assert (code->ext.inquire != NULL);
	  if (gfc_resolve_inquire (code->ext.inquire) == FAILURE)
	    break;

	  resolve_branch (code->ext.inquire->err, code);
	  break;

	case EXEC_WAIT:
	  if (gfc_resolve_wait (code->ext.wait) == FAILURE)
	    break;

	  resolve_branch (code->ext.wait->err, code);
	  resolve_branch (code->ext.wait->end, code);
	  resolve_branch (code->ext.wait->eor, code);
	  break;

	case EXEC_READ:
	case EXEC_WRITE:
	  if (gfc_resolve_dt (code->ext.dt, &code->loc) == FAILURE)
	    break;

	  resolve_branch (code->ext.dt->err, code);
	  resolve_branch (code->ext.dt->end, code);
	  resolve_branch (code->ext.dt->eor, code);
	  break;

	case EXEC_TRANSFER:
	  resolve_transfer (code);
	  break;

	case EXEC_FORALL:
	  resolve_forall_iterators (code->ext.forall_iterator);

	  if (code->expr1 != NULL && code->expr1->ts.type != BT_LOGICAL)
	    gfc_error ("FORALL mask clause at %L requires a LOGICAL "
		       "expression", &code->expr1->where);
	  break;

	case EXEC_OMP_ATOMIC:
	case EXEC_OMP_BARRIER:
	case EXEC_OMP_CRITICAL:
	case EXEC_OMP_FLUSH:
	case EXEC_OMP_DO:
	case EXEC_OMP_MASTER:
	case EXEC_OMP_ORDERED:
	case EXEC_OMP_SECTIONS:
	case EXEC_OMP_SINGLE:
	case EXEC_OMP_TASKWAIT:
	case EXEC_OMP_WORKSHARE:
	  gfc_resolve_omp_directive (code, ns);
	  break;

	case EXEC_OMP_PARALLEL:
	case EXEC_OMP_PARALLEL_DO:
	case EXEC_OMP_PARALLEL_SECTIONS:
	case EXEC_OMP_PARALLEL_WORKSHARE:
	case EXEC_OMP_TASK:
	  omp_workshare_save = omp_workshare_flag;
	  omp_workshare_flag = 0;
	  gfc_resolve_omp_directive (code, ns);
	  omp_workshare_flag = omp_workshare_save;
	  break;

	default:
	  gfc_internal_error ("resolve_code(): Bad statement code");
	}
    }

  cs_base = frame.prev;
}


/* Resolve initial values and make sure they are compatible with
   the variable.  */

static void
resolve_values (gfc_symbol *sym)
{
  if (sym->value == NULL)
    return;

  if (gfc_resolve_expr (sym->value) == FAILURE)
    return;

  gfc_check_assign_symbol (sym, sym->value);
}


/* Verify the binding labels for common blocks that are BIND(C).  The label
   for a BIND(C) common block must be identical in all scoping units in which
   the common block is declared.  Further, the binding label can not collide
   with any other global entity in the program.  */

static void
resolve_bind_c_comms (gfc_symtree *comm_block_tree)
{
  if (comm_block_tree->n.common->is_bind_c == 1)
    {
      gfc_gsymbol *binding_label_gsym;
      gfc_gsymbol *comm_name_gsym;

      /* See if a global symbol exists by the common block's name.  It may
         be NULL if the common block is use-associated.  */
      comm_name_gsym = gfc_find_gsymbol (gfc_gsym_root,
                                         comm_block_tree->n.common->name);
      if (comm_name_gsym != NULL && comm_name_gsym->type != GSYM_COMMON)
        gfc_error ("Binding label '%s' for common block '%s' at %L collides "
                   "with the global entity '%s' at %L",
                   comm_block_tree->n.common->binding_label,
                   comm_block_tree->n.common->name,
                   &(comm_block_tree->n.common->where),
                   comm_name_gsym->name, &(comm_name_gsym->where));
      else if (comm_name_gsym != NULL
	       && strcmp (comm_name_gsym->name,
			  comm_block_tree->n.common->name) == 0)
        {
          /* TODO: Need to make sure the fields of gfc_gsymbol are initialized
             as expected.  */
          if (comm_name_gsym->binding_label == NULL)
            /* No binding label for common block stored yet; save this one.  */
            comm_name_gsym->binding_label =
              comm_block_tree->n.common->binding_label;
          else
            if (strcmp (comm_name_gsym->binding_label,
                        comm_block_tree->n.common->binding_label) != 0)
              {
                /* Common block names match but binding labels do not.  */
                gfc_error ("Binding label '%s' for common block '%s' at %L "
                           "does not match the binding label '%s' for common "
                           "block '%s' at %L",
                           comm_block_tree->n.common->binding_label,
                           comm_block_tree->n.common->name,
                           &(comm_block_tree->n.common->where),
                           comm_name_gsym->binding_label,
                           comm_name_gsym->name,
                           &(comm_name_gsym->where));
                return;
              }
        }

      /* There is no binding label (NAME="") so we have nothing further to
         check and nothing to add as a global symbol for the label.  */
      if (comm_block_tree->n.common->binding_label[0] == '\0' )
        return;
      
      binding_label_gsym =
        gfc_find_gsymbol (gfc_gsym_root,
                          comm_block_tree->n.common->binding_label);
      if (binding_label_gsym == NULL)
        {
          /* Need to make a global symbol for the binding label to prevent
             it from colliding with another.  */
          binding_label_gsym =
            gfc_get_gsymbol (comm_block_tree->n.common->binding_label);
          binding_label_gsym->sym_name = comm_block_tree->n.common->name;
          binding_label_gsym->type = GSYM_COMMON;
        }
      else
        {
          /* If comm_name_gsym is NULL, the name common block is use
             associated and the name could be colliding.  */
          if (binding_label_gsym->type != GSYM_COMMON)
            gfc_error ("Binding label '%s' for common block '%s' at %L "
                       "collides with the global entity '%s' at %L",
                       comm_block_tree->n.common->binding_label,
                       comm_block_tree->n.common->name,
                       &(comm_block_tree->n.common->where),
                       binding_label_gsym->name,
                       &(binding_label_gsym->where));
          else if (comm_name_gsym != NULL
		   && (strcmp (binding_label_gsym->name,
			       comm_name_gsym->binding_label) != 0)
		   && (strcmp (binding_label_gsym->sym_name,
			       comm_name_gsym->name) != 0))
            gfc_error ("Binding label '%s' for common block '%s' at %L "
                       "collides with global entity '%s' at %L",
                       binding_label_gsym->name, binding_label_gsym->sym_name,
                       &(comm_block_tree->n.common->where),
                       comm_name_gsym->name, &(comm_name_gsym->where));
        }
    }
  
  return;
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


/* Verify that any binding labels used in a given namespace do not collide 
   with the names or binding labels of any global symbols.  */

static void
gfc_verify_binding_labels (gfc_symbol *sym)
{
  int has_error = 0;
  
  if (sym != NULL && sym->attr.is_bind_c && sym->attr.is_iso_c == 0 
      && sym->attr.flavor != FL_DERIVED && sym->binding_label[0] != '\0')
    {
      gfc_gsymbol *bind_c_sym;

      bind_c_sym = gfc_find_gsymbol (gfc_gsym_root, sym->binding_label);
      if (bind_c_sym != NULL 
          && strcmp (bind_c_sym->name, sym->binding_label) == 0)
        {
          if (sym->attr.if_source == IFSRC_DECL 
              && (bind_c_sym->type != GSYM_SUBROUTINE 
                  && bind_c_sym->type != GSYM_FUNCTION) 
              && ((sym->attr.contained == 1 
                   && strcmp (bind_c_sym->sym_name, sym->name) != 0) 
                  || (sym->attr.use_assoc == 1 
                      && (strcmp (bind_c_sym->mod_name, sym->module) != 0))))
            {
              /* Make sure global procedures don't collide with anything.  */
              gfc_error ("Binding label '%s' at %L collides with the global "
                         "entity '%s' at %L", sym->binding_label,
                         &(sym->declared_at), bind_c_sym->name,
                         &(bind_c_sym->where));
              has_error = 1;
            }
          else if (sym->attr.contained == 0 
                   && (sym->attr.if_source == IFSRC_IFBODY 
                       && sym->attr.flavor == FL_PROCEDURE) 
                   && (bind_c_sym->sym_name != NULL 
                       && strcmp (bind_c_sym->sym_name, sym->name) != 0))
            {
              /* Make sure procedures in interface bodies don't collide.  */
              gfc_error ("Binding label '%s' in interface body at %L collides "
                         "with the global entity '%s' at %L",
                         sym->binding_label,
                         &(sym->declared_at), bind_c_sym->name,
                         &(bind_c_sym->where));
              has_error = 1;
            }
          else if (sym->attr.contained == 0 
                   && sym->attr.if_source == IFSRC_UNKNOWN)
	    if ((sym->attr.use_assoc && bind_c_sym->mod_name
		 && strcmp (bind_c_sym->mod_name, sym->module) != 0) 
		|| sym->attr.use_assoc == 0)
              {
                gfc_error ("Binding label '%s' at %L collides with global "
                           "entity '%s' at %L", sym->binding_label,
                           &(sym->declared_at), bind_c_sym->name,
                           &(bind_c_sym->where));
                has_error = 1;
              }

          if (has_error != 0)
            /* Clear the binding label to prevent checking multiple times.  */
            sym->binding_label[0] = '\0';
        }
      else if (bind_c_sym == NULL)
	{
	  bind_c_sym = gfc_get_gsymbol (sym->binding_label);
	  bind_c_sym->where = sym->declared_at;
	  bind_c_sym->sym_name = sym->name;

          if (sym->attr.use_assoc == 1)
            bind_c_sym->mod_name = sym->module;
          else
            if (sym->ns->proc_name != NULL)
              bind_c_sym->mod_name = sym->ns->proc_name->name;

          if (sym->attr.contained == 0)
            {
              if (sym->attr.subroutine)
                bind_c_sym->type = GSYM_SUBROUTINE;
              else if (sym->attr.function)
                bind_c_sym->type = GSYM_FUNCTION;
            }
        }
    }
  return;
}


/* Resolve an index expression.  */

static gfc_try
resolve_index_expr (gfc_expr *e)
{
  if (gfc_resolve_expr (e) == FAILURE)
    return FAILURE;

  if (gfc_simplify_expr (e, 0) == FAILURE)
    return FAILURE;

  if (gfc_specification_expr (e) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

/* Resolve a charlen structure.  */

static gfc_try
resolve_charlen (gfc_charlen *cl)
{
  int i, k;

  if (cl->resolved)
    return SUCCESS;

  cl->resolved = 1;

  specification_expr = 1;

  if (resolve_index_expr (cl->length) == FAILURE)
    {
      specification_expr = 0;
      return FAILURE;
    }

  /* "If the character length parameter value evaluates to a negative
     value, the length of character entities declared is zero."  */
  if (cl->length && !gfc_extract_int (cl->length, &i) && i < 0)
    {
      gfc_warning_now ("CHARACTER variable has zero length at %L",
		       &cl->length->where);
      gfc_replace_expr (cl->length, gfc_int_expr (0));
    }

  /* Check that the character length is not too large.  */
  k = gfc_validate_kind (BT_INTEGER, gfc_charlen_int_kind, false);
  if (cl->length && cl->length->expr_type == EXPR_CONSTANT
      && cl->length->ts.type == BT_INTEGER
      && mpz_cmp (cl->length->value.integer, gfc_integer_kinds[k].huge) > 0)
    {
      gfc_error ("String length at %L is too large", &cl->length->where);
      return FAILURE;
    }

  return SUCCESS;
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
      for (i = 0; i < sym->as->rank; i++)
	{
	  e = sym->as->lower[i];
	  if (e && (resolve_index_expr (e) == FAILURE
		    || !gfc_is_constant_expr (e)))
	    not_constant = true;

	  e = sym->as->upper[i];
	  if (e && (resolve_index_expr (e) == FAILURE
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
  init_st = gfc_get_code ();
  init_st->next = ns->code;
  ns->code = init_st;

  /* Assign the default initializer to the l-value.  */
  init_st->loc = sym->declared_at;
  init_st->op = EXEC_INIT_ASSIGN;
  init_st->expr1 = lval;
  init_st->expr2 = init;
}

/* Assign the default initializer to a derived type variable or result.  */

static void
apply_default_init (gfc_symbol *sym)
{
  gfc_expr *init = NULL;

  if (sym->attr.flavor != FL_VARIABLE && !sym->attr.function)
    return;

  if (sym->ts.type == BT_DERIVED && sym->ts.derived)
    init = gfc_default_initializer (&sym->ts);

  if (init == NULL)
    return;

  build_init_assign (sym, init);
}

/* Build an initializer for a local integer, real, complex, logical, or
   character variable, based on the command line flags finit-local-zero,
   finit-integer=, finit-real=, finit-logical=, and finit-runtime.  Returns 
   null if the symbol should not have a default initialization.  */
static gfc_expr *
build_default_init_expr (gfc_symbol *sym)
{
  int char_len;
  gfc_expr *init_expr;
  int i;

  /* These symbols should never have a default initialization.  */
  if ((sym->attr.dimension && !gfc_is_compile_time_shape (sym->as))
      || sym->attr.external
      || sym->attr.dummy
      || sym->attr.pointer
      || sym->attr.in_equivalence
      || sym->attr.in_common
      || sym->attr.data
      || sym->module
      || sym->attr.cray_pointee
      || sym->attr.cray_pointer)
    return NULL;

  /* Now we'll try to build an initializer expression.  */
  init_expr = gfc_get_expr ();
  init_expr->expr_type = EXPR_CONSTANT;
  init_expr->ts.type = sym->ts.type;
  init_expr->ts.kind = sym->ts.kind;
  init_expr->where = sym->declared_at;
  
  /* We will only initialize integers, reals, complex, logicals, and
     characters, and only if the corresponding command-line flags
     were set.  Otherwise, we free init_expr and return null.  */
  switch (sym->ts.type)
    {    
    case BT_INTEGER:
      if (gfc_option.flag_init_integer != GFC_INIT_INTEGER_OFF)
	mpz_init_set_si (init_expr->value.integer, 
			 gfc_option.flag_init_integer_value);
      else
	{
	  gfc_free_expr (init_expr);
	  init_expr = NULL;
	}
      break;

    case BT_REAL:
      mpfr_init (init_expr->value.real);
      switch (gfc_option.flag_init_real)
	{
	case GFC_INIT_REAL_SNAN:
	  init_expr->is_snan = 1;
	  /* Fall through.  */
	case GFC_INIT_REAL_NAN:
	  mpfr_set_nan (init_expr->value.real);
	  break;

	case GFC_INIT_REAL_INF:
	  mpfr_set_inf (init_expr->value.real, 1);
	  break;

	case GFC_INIT_REAL_NEG_INF:
	  mpfr_set_inf (init_expr->value.real, -1);
	  break;

	case GFC_INIT_REAL_ZERO:
	  mpfr_set_ui (init_expr->value.real, 0.0, GFC_RND_MODE);
	  break;

	default:
	  gfc_free_expr (init_expr);
	  init_expr = NULL;
	  break;
	}
      break;
	  
    case BT_COMPLEX:
#ifdef HAVE_mpc
      mpc_init2 (init_expr->value.complex, mpfr_get_default_prec());
#else
      mpfr_init (init_expr->value.complex.r);
      mpfr_init (init_expr->value.complex.i);
#endif
      switch (gfc_option.flag_init_real)
	{
	case GFC_INIT_REAL_SNAN:
	  init_expr->is_snan = 1;
	  /* Fall through.  */
	case GFC_INIT_REAL_NAN:
	  mpfr_set_nan (mpc_realref (init_expr->value.complex));
	  mpfr_set_nan (mpc_imagref (init_expr->value.complex));
	  break;

	case GFC_INIT_REAL_INF:
	  mpfr_set_inf (mpc_realref (init_expr->value.complex), 1);
	  mpfr_set_inf (mpc_imagref (init_expr->value.complex), 1);
	  break;

	case GFC_INIT_REAL_NEG_INF:
	  mpfr_set_inf (mpc_realref (init_expr->value.complex), -1);
	  mpfr_set_inf (mpc_imagref (init_expr->value.complex), -1);
	  break;

	case GFC_INIT_REAL_ZERO:
#ifdef HAVE_mpc
	  mpc_set_ui (init_expr->value.complex, 0, GFC_MPC_RND_MODE);
#else
	  mpfr_set_ui (init_expr->value.complex.r, 0.0, GFC_RND_MODE);
	  mpfr_set_ui (init_expr->value.complex.i, 0.0, GFC_RND_MODE);
#endif
	  break;

	default:
	  gfc_free_expr (init_expr);
	  init_expr = NULL;
	  break;
	}
      break;
	  
    case BT_LOGICAL:
      if (gfc_option.flag_init_logical == GFC_INIT_LOGICAL_FALSE)
	init_expr->value.logical = 0;
      else if (gfc_option.flag_init_logical == GFC_INIT_LOGICAL_TRUE)
	init_expr->value.logical = 1;
      else
	{
	  gfc_free_expr (init_expr);
	  init_expr = NULL;
	}
      break;
	  
    case BT_CHARACTER:
      /* For characters, the length must be constant in order to 
	 create a default initializer.  */
      if (gfc_option.flag_init_character == GFC_INIT_CHARACTER_ON
	  && sym->ts.cl->length
	  && sym->ts.cl->length->expr_type == EXPR_CONSTANT)
	{
	  char_len = mpz_get_si (sym->ts.cl->length->value.integer);
	  init_expr->value.character.length = char_len;
	  init_expr->value.character.string = gfc_get_wide_string (char_len+1);
	  for (i = 0; i < char_len; i++)
	    init_expr->value.character.string[i]
	      = (unsigned char) gfc_option.flag_init_character_value;
	}
      else
	{
	  gfc_free_expr (init_expr);
	  init_expr = NULL;
	}
      break;
	  
    default:
     gfc_free_expr (init_expr);
     init_expr = NULL;
    }
  return init_expr;
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

  /* For saved variables, we don't want to add an initializer at 
     function entry, so we just add a static initializer.  */
  if (sym->attr.save || sym->ns->save_all)
    {
      /* Don't clobber an existing initializer!  */
      gcc_assert (sym->value == NULL);
      sym->value = init;
      return;
    }

  build_init_assign (sym, init);
}

/* Resolution of common features of flavors variable and procedure.  */

static gfc_try
resolve_fl_var_and_proc (gfc_symbol *sym, int mp_flag)
{
  /* Constraints on deferred shape variable.  */
  if (sym->as == NULL || sym->as->type != AS_DEFERRED)
    {
      if (sym->attr.allocatable)
	{
	  if (sym->attr.dimension)
	    gfc_error ("Allocatable array '%s' at %L must have "
		       "a deferred shape", sym->name, &sym->declared_at);
	  else
	    gfc_error ("Scalar object '%s' at %L may not be ALLOCATABLE",
		       sym->name, &sym->declared_at);
	    return FAILURE;
	}

      if (sym->attr.pointer && sym->attr.dimension)
	{
	  gfc_error ("Array pointer '%s' at %L must have a deferred shape",
		     sym->name, &sym->declared_at);
	  return FAILURE;
	}

    }
  else
    {
      if (!mp_flag && !sym->attr.allocatable
	  && !sym->attr.pointer && !sym->attr.dummy)
	{
	  gfc_error ("Array '%s' at %L cannot have a deferred shape",
		     sym->name, &sym->declared_at);
	  return FAILURE;
	 }
    }
  return SUCCESS;
}


/* Additional checks for symbols with flavor variable and derived
   type.  To be called from resolve_fl_variable.  */

static gfc_try
resolve_fl_variable_derived (gfc_symbol *sym, int no_init_flag)
{
  gcc_assert (sym->ts.type == BT_DERIVED);

  /* Check to see if a derived type is blocked from being host
     associated by the presence of another class I symbol in the same
     namespace.  14.6.1.3 of the standard and the discussion on
     comp.lang.fortran.  */
  if (sym->ns != sym->ts.derived->ns
      && sym->ns->proc_name->attr.if_source != IFSRC_IFBODY)
    {
      gfc_symbol *s;
      gfc_find_symbol (sym->ts.derived->name, sym->ns, 0, &s);
      if (s && s->attr.flavor != FL_DERIVED)
	{
	  gfc_error ("The type '%s' cannot be host associated at %L "
		     "because it is blocked by an incompatible object "
		     "of the same name declared at %L",
		     sym->ts.derived->name, &sym->declared_at,
		     &s->declared_at);
	  return FAILURE;
	}
    }

  /* 4th constraint in section 11.3: "If an object of a type for which
     component-initialization is specified (R429) appears in the
     specification-part of a module and does not have the ALLOCATABLE
     or POINTER attribute, the object shall have the SAVE attribute."

     The check for initializers is performed with
     has_default_initializer because gfc_default_initializer generates
     a hidden default for allocatable components.  */
  if (!(sym->value || no_init_flag) && sym->ns->proc_name
      && sym->ns->proc_name->attr.flavor == FL_MODULE
      && !sym->ns->save_all && !sym->attr.save
      && !sym->attr.pointer && !sym->attr.allocatable
      && has_default_initializer (sym->ts.derived))
    {
      gfc_error("Object '%s' at %L must have the SAVE attribute for "
		"default initialization of a component",
		sym->name, &sym->declared_at);
      return FAILURE;
    }

  /* Assign default initializer.  */
  if (!(sym->value || sym->attr.pointer || sym->attr.allocatable)
      && (!no_init_flag || sym->attr.intent == INTENT_OUT))
    {
      sym->value = gfc_default_initializer (&sym->ts);
    }

  return SUCCESS;
}


/* Resolve symbols with flavor variable.  */

static gfc_try
resolve_fl_variable (gfc_symbol *sym, int mp_flag)
{
  int no_init_flag, automatic_flag;
  gfc_expr *e;
  const char *auto_save_msg;

  auto_save_msg = "Automatic object '%s' at %L cannot have the "
		  "SAVE attribute";

  if (resolve_fl_var_and_proc (sym, mp_flag) == FAILURE)
    return FAILURE;

  /* Set this flag to check that variables are parameters of all entries.
     This check is effected by the call to gfc_resolve_expr through
     is_non_constant_shape_array.  */
  specification_expr = 1;

  if (sym->ns->proc_name
      && (sym->ns->proc_name->attr.flavor == FL_MODULE
	  || sym->ns->proc_name->attr.is_main_program)
      && !sym->attr.use_assoc
      && !sym->attr.allocatable
      && !sym->attr.pointer
      && is_non_constant_shape_array (sym))
    {
      /* The shape of a main program or module array needs to be
	 constant.  */
      gfc_error ("The module or main program array '%s' at %L must "
		 "have constant shape", sym->name, &sym->declared_at);
      specification_expr = 0;
      return FAILURE;
    }

  if (sym->ts.type == BT_CHARACTER)
    {
      /* Make sure that character string variables with assumed length are
	 dummy arguments.  */
      e = sym->ts.cl->length;
      if (e == NULL && !sym->attr.dummy && !sym->attr.result)
	{
	  gfc_error ("Entity with assumed character length at %L must be a "
		     "dummy argument or a PARAMETER", &sym->declared_at);
	  return FAILURE;
	}

      if (e && sym->attr.save && !gfc_is_constant_expr (e))
	{
	  gfc_error (auto_save_msg, sym->name, &sym->declared_at);
	  return FAILURE;
	}

      if (!gfc_is_constant_expr (e)
	  && !(e->expr_type == EXPR_VARIABLE
	       && e->symtree->n.sym->attr.flavor == FL_PARAMETER)
	  && sym->ns->proc_name
	  && (sym->ns->proc_name->attr.flavor == FL_MODULE
	      || sym->ns->proc_name->attr.is_main_program)
	  && !sym->attr.use_assoc)
	{
	  gfc_error ("'%s' at %L must have constant character length "
		     "in this context", sym->name, &sym->declared_at);
	  return FAILURE;
	}
    }

  if (sym->value == NULL && sym->attr.referenced)
    apply_default_init_local (sym); /* Try to apply a default initialization.  */

  /* Determine if the symbol may not have an initializer.  */
  no_init_flag = automatic_flag = 0;
  if (sym->attr.allocatable || sym->attr.external || sym->attr.dummy
      || sym->attr.intrinsic || sym->attr.result)
    no_init_flag = 1;
  else if (sym->attr.dimension && !sym->attr.pointer
	   && is_non_constant_shape_array (sym))
    {
      no_init_flag = automatic_flag = 1;

      /* Also, they must not have the SAVE attribute.
	 SAVE_IMPLICIT is checked below.  */
      if (sym->attr.save == SAVE_EXPLICIT)
	{
	  gfc_error (auto_save_msg, sym->name, &sym->declared_at);
	  return FAILURE;
	}
    }

  /* Ensure that any initializer is simplified.  */
  if (sym->value)
    gfc_simplify_expr (sym->value, 1);

  /* Reject illegal initializers.  */
  if (!sym->mark && sym->value)
    {
      if (sym->attr.allocatable)
	gfc_error ("Allocatable '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.external)
	gfc_error ("External '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.dummy
	&& !(sym->ts.type == BT_DERIVED && sym->attr.intent == INTENT_OUT))
	gfc_error ("Dummy '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.intrinsic)
	gfc_error ("Intrinsic '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.result)
	gfc_error ("Function result '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (automatic_flag)
	gfc_error ("Automatic array '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else
	goto no_init_error;
      return FAILURE;
    }

no_init_error:
  if (sym->ts.type == BT_DERIVED)
    return resolve_fl_variable_derived (sym, no_init_flag);

  return SUCCESS;
}


/* Resolve a procedure.  */

static gfc_try
resolve_fl_procedure (gfc_symbol *sym, int mp_flag)
{
  gfc_formal_arglist *arg;

  if (sym->attr.ambiguous_interfaces && !sym->attr.referenced)
    gfc_warning ("Although not referenced, '%s' at %L has ambiguous "
		 "interfaces", sym->name, &sym->declared_at);

  if (sym->attr.function
      && resolve_fl_var_and_proc (sym, mp_flag) == FAILURE)
    return FAILURE;

  if (sym->ts.type == BT_CHARACTER)
    {
      gfc_charlen *cl = sym->ts.cl;

      if (cl && cl->length && gfc_is_constant_expr (cl->length)
	     && resolve_charlen (cl) == FAILURE)
	return FAILURE;

      if (!cl || !cl->length || cl->length->expr_type != EXPR_CONSTANT)
	{
	  if (sym->attr.proc == PROC_ST_FUNCTION)
	    {
	      gfc_error ("Character-valued statement function '%s' at %L must "
			 "have constant length", sym->name, &sym->declared_at);
	      return FAILURE;
	    }

	  if (sym->attr.external && sym->formal == NULL
	      && cl && cl->length && cl->length->expr_type != EXPR_CONSTANT)
	    {
	      gfc_error ("Automatic character length function '%s' at %L must "
			 "have an explicit interface", sym->name,
			 &sym->declared_at);
	      return FAILURE;
	    }
	}
    }

  /* Ensure that derived type for are not of a private type.  Internal
     module procedures are excluded by 2.2.3.3 - i.e., they are not
     externally accessible and can access all the objects accessible in
     the host.  */
  if (!(sym->ns->parent
	&& sym->ns->parent->proc_name->attr.flavor == FL_MODULE)
      && gfc_check_access(sym->attr.access, sym->ns->default_access))
    {
      gfc_interface *iface;

      for (arg = sym->formal; arg; arg = arg->next)
	{
	  if (arg->sym
	      && arg->sym->ts.type == BT_DERIVED
	      && !arg->sym->ts.derived->attr.use_assoc
	      && !gfc_check_access (arg->sym->ts.derived->attr.access,
				    arg->sym->ts.derived->ns->default_access)
	      && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' is of a "
				 "PRIVATE type and cannot be a dummy argument"
				 " of '%s', which is PUBLIC at %L",
				 arg->sym->name, sym->name, &sym->declared_at)
		 == FAILURE)
	    {
	      /* Stop this message from recurring.  */
	      arg->sym->ts.derived->attr.access = ACCESS_PUBLIC;
	      return FAILURE;
	    }
	}

      /* PUBLIC interfaces may expose PRIVATE procedures that take types
	 PRIVATE to the containing module.  */
      for (iface = sym->generic; iface; iface = iface->next)
	{
	  for (arg = iface->sym->formal; arg; arg = arg->next)
	    {
	      if (arg->sym
		  && arg->sym->ts.type == BT_DERIVED
		  && !arg->sym->ts.derived->attr.use_assoc
		  && !gfc_check_access (arg->sym->ts.derived->attr.access,
					arg->sym->ts.derived->ns->default_access)
		  && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: Procedure "
				     "'%s' in PUBLIC interface '%s' at %L "
				     "takes dummy arguments of '%s' which is "
				     "PRIVATE", iface->sym->name, sym->name,
				     &iface->sym->declared_at,
				     gfc_typename (&arg->sym->ts)) == FAILURE)
		{
		  /* Stop this message from recurring.  */
		  arg->sym->ts.derived->attr.access = ACCESS_PUBLIC;
		  return FAILURE;
		}
	     }
	}

      /* PUBLIC interfaces may expose PRIVATE procedures that take types
	 PRIVATE to the containing module.  */
      for (iface = sym->generic; iface; iface = iface->next)
	{
	  for (arg = iface->sym->formal; arg; arg = arg->next)
	    {
	      if (arg->sym
		  && arg->sym->ts.type == BT_DERIVED
		  && !arg->sym->ts.derived->attr.use_assoc
		  && !gfc_check_access (arg->sym->ts.derived->attr.access,
					arg->sym->ts.derived->ns->default_access)
		  && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: Procedure "
				     "'%s' in PUBLIC interface '%s' at %L "
				     "takes dummy arguments of '%s' which is "
				     "PRIVATE", iface->sym->name, sym->name,
				     &iface->sym->declared_at,
				     gfc_typename (&arg->sym->ts)) == FAILURE)
		{
		  /* Stop this message from recurring.  */
		  arg->sym->ts.derived->attr.access = ACCESS_PUBLIC;
		  return FAILURE;
		}
	     }
	}
    }

  if (sym->attr.function && sym->value && sym->attr.proc != PROC_ST_FUNCTION
      && !sym->attr.proc_pointer)
    {
      gfc_error ("Function '%s' at %L cannot have an initializer",
		 sym->name, &sym->declared_at);
      return FAILURE;
    }

  /* An external symbol may not have an initializer because it is taken to be
     a procedure. Exception: Procedure Pointers.  */
  if (sym->attr.external && sym->value && !sym->attr.proc_pointer)
    {
      gfc_error ("External object '%s' at %L may not have an initializer",
		 sym->name, &sym->declared_at);
      return FAILURE;
    }

  /* An elemental function is required to return a scalar 12.7.1  */
  if (sym->attr.elemental && sym->attr.function && sym->as)
    {
      gfc_error ("ELEMENTAL function '%s' at %L must have a scalar "
		 "result", sym->name, &sym->declared_at);
      /* Reset so that the error only occurs once.  */
      sym->attr.elemental = 0;
      return FAILURE;
    }

  /* 5.1.1.5 of the Standard: A function name declared with an asterisk
     char-len-param shall not be array-valued, pointer-valued, recursive
     or pure.  ....snip... A character value of * may only be used in the
     following ways: (i) Dummy arg of procedure - dummy associates with
     actual length; (ii) To declare a named constant; or (iii) External
     function - but length must be declared in calling scoping unit.  */
  if (sym->attr.function
      && sym->ts.type == BT_CHARACTER
      && sym->ts.cl && sym->ts.cl->length == NULL)
    {
      if ((sym->as && sym->as->rank) || (sym->attr.pointer)
	  || (sym->attr.recursive) || (sym->attr.pure))
	{
	  if (sym->as && sym->as->rank)
	    gfc_error ("CHARACTER(*) function '%s' at %L cannot be "
		       "array-valued", sym->name, &sym->declared_at);

	  if (sym->attr.pointer)
	    gfc_error ("CHARACTER(*) function '%s' at %L cannot be "
		       "pointer-valued", sym->name, &sym->declared_at);

	  if (sym->attr.pure)
	    gfc_error ("CHARACTER(*) function '%s' at %L cannot be "
		       "pure", sym->name, &sym->declared_at);

	  if (sym->attr.recursive)
	    gfc_error ("CHARACTER(*) function '%s' at %L cannot be "
		       "recursive", sym->name, &sym->declared_at);

	  return FAILURE;
	}

      /* Appendix B.2 of the standard.  Contained functions give an
	 error anyway.  Fixed-form is likely to be F77/legacy.  */
      if (!sym->attr.contained && gfc_current_form != FORM_FIXED)
	gfc_notify_std (GFC_STD_F95_OBS, "CHARACTER(*) function "
			"'%s' at %L is obsolescent in fortran 95",
			sym->name, &sym->declared_at);
    }

  if (sym->attr.is_bind_c && sym->attr.is_c_interop != 1)
    {
      gfc_formal_arglist *curr_arg;
      int has_non_interop_arg = 0;

      if (verify_bind_c_sym (sym, &(sym->ts), sym->attr.in_common,
                             sym->common_block) == FAILURE)
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
      
      curr_arg = sym->formal;
      while (curr_arg != NULL)
        {
          /* Skip implicitly typed dummy args here.  */
	  if (curr_arg->sym->attr.implicit_type == 0)
	    if (verify_c_interop_param (curr_arg->sym) == FAILURE)
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
		     "in '%s' at %L", sym->name, &sym->declared_at);
	  return FAILURE;
	}
      if (sym->attr.intent)
	{
	  gfc_error ("PROCEDURE attribute conflicts with INTENT attribute "
		     "in '%s' at %L", sym->name, &sym->declared_at);
	  return FAILURE;
	}
      if (sym->attr.subroutine && sym->attr.result)
	{
	  gfc_error ("PROCEDURE attribute conflicts with RESULT attribute "
		     "in '%s' at %L", sym->name, &sym->declared_at);
	  return FAILURE;
	}
      if (sym->attr.external && sym->attr.function
	  && ((sym->attr.if_source == IFSRC_DECL && !sym->attr.procedure)
	      || sym->attr.contained))
	{
	  gfc_error ("EXTERNAL attribute conflicts with FUNCTION attribute "
		     "in '%s' at %L", sym->name, &sym->declared_at);
	  return FAILURE;
	}
      if (strcmp ("ppr@", sym->name) == 0)
	{
	  gfc_error ("Procedure pointer result '%s' at %L "
		     "is missing the pointer attribute",
		     sym->ns->proc_name->name, &sym->declared_at);
	  return FAILURE;
	}
    }

  return SUCCESS;
}


/* Resolve a list of finalizer procedures.  That is, after they have hopefully
   been defined and we now know their defined arguments, check that they fulfill
   the requirements of the standard for procedures used as finalizers.  */

static gfc_try
gfc_resolve_finalizers (gfc_symbol* derived)
{
  gfc_finalizer* list;
  gfc_finalizer** prev_link; /* For removing wrong entries from the list.  */
  gfc_try result = SUCCESS;
  bool seen_scalar = false;

  if (!derived->f2k_derived || !derived->f2k_derived->finalizers)
    return SUCCESS;

  /* Walk over the list of finalizer-procedures, check them, and if any one
     does not fit in with the standard's definition, print an error and remove
     it from the list.  */
  prev_link = &derived->f2k_derived->finalizers;
  for (list = derived->f2k_derived->finalizers; list; list = *prev_link)
    {
      gfc_symbol* arg;
      gfc_finalizer* i;
      int my_rank;

      /* Skip this finalizer if we already resolved it.  */
      if (list->proc_tree)
	{
	  prev_link = &(list->next);
	  continue;
	}

      /* Check this exists and is a SUBROUTINE.  */
      if (!list->proc_sym->attr.subroutine)
	{
	  gfc_error ("FINAL procedure '%s' at %L is not a SUBROUTINE",
		     list->proc_sym->name, &list->where);
	  goto error;
	}

      /* We should have exactly one argument.  */
      if (!list->proc_sym->formal || list->proc_sym->formal->next)
	{
	  gfc_error ("FINAL procedure at %L must have exactly one argument",
		     &list->where);
	  goto error;
	}
      arg = list->proc_sym->formal->sym;

      /* This argument must be of our type.  */
      if (arg->ts.type != BT_DERIVED || arg->ts.derived != derived)
	{
	  gfc_error ("Argument of FINAL procedure at %L must be of type '%s'",
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
      if (gfc_option.warn_surprising && arg->as && arg->as->rank > 0
	  && arg->as->type != AS_ASSUMED_SHAPE)
	gfc_warning ("Non-scalar FINAL procedure at %L should have assumed"
		     " shape argument", &arg->declared_at);

      /* Check that it does not match in kind and rank with a FINAL procedure
	 defined earlier.  To really loop over the *earlier* declarations,
	 we need to walk the tail of the list as new ones were pushed at the
	 front.  */
      /* TODO: Handle kind parameters once they are implemented.  */
      my_rank = (arg->as ? arg->as->rank : 0);
      for (i = list->next; i; i = i->next)
	{
	  /* Argument list might be empty; that is an error signalled earlier,
	     but we nevertheless continued resolving.  */
	  if (i->proc_sym->formal)
	    {
	      gfc_symbol* i_arg = i->proc_sym->formal->sym;
	      const int i_rank = (i_arg->as ? i_arg->as->rank : 0);
	      if (i_rank == my_rank)
		{
		  gfc_error ("FINAL procedure '%s' declared at %L has the same"
			     " rank (%d) as '%s'",
			     list->proc_sym->name, &list->where, my_rank, 
			     i->proc_sym->name);
		  goto error;
		}
	    }
	}

	/* Is this the/a scalar finalizer procedure?  */
	if (!arg->as || arg->as->rank == 0)
	  seen_scalar = true;

	/* Find the symtree for this procedure.  */
	gcc_assert (!list->proc_tree);
	list->proc_tree = gfc_find_sym_in_symtree (list->proc_sym);

	prev_link = &list->next;
	continue;

	/* Remove wrong nodes immediately from the list so we don't risk any
	   troubles in the future when they might fail later expectations.  */
error:
	result = FAILURE;
	i = list;
	*prev_link = list->next;
	gfc_free_finalizer (i);
    }

  /* Warn if we haven't seen a scalar finalizer procedure (but we know there
     were nodes in the list, must have been for arrays.  It is surely a good
     idea to have a scalar version there if there's something to finalize.  */
  if (gfc_option.warn_surprising && result == SUCCESS && !seen_scalar)
    gfc_warning ("Only array FINAL procedures declared for derived type '%s'"
		 " defined at %L, suggest also scalar one",
		 derived->name, &derived->declared_at);

  /* TODO:  Remove this error when finalization is finished.  */
  gfc_error ("Finalization at %L is not yet implemented",
	     &derived->declared_at);

  return result;
}


/* Check that it is ok for the typebound procedure proc to override the
   procedure old.  */

static gfc_try
check_typebound_override (gfc_symtree* proc, gfc_symtree* old)
{
  locus where;
  const gfc_symbol* proc_target;
  const gfc_symbol* old_target;
  unsigned proc_pass_arg, old_pass_arg, argpos;
  gfc_formal_arglist* proc_formal;
  gfc_formal_arglist* old_formal;

  /* This procedure should only be called for non-GENERIC proc.  */
  gcc_assert (!proc->n.tb->is_generic);

  /* If the overwritten procedure is GENERIC, this is an error.  */
  if (old->n.tb->is_generic)
    {
      gfc_error ("Can't overwrite GENERIC '%s' at %L",
		 old->name, &proc->n.tb->where);
      return FAILURE;
    }

  where = proc->n.tb->where;
  proc_target = proc->n.tb->u.specific->n.sym;
  old_target = old->n.tb->u.specific->n.sym;

  /* Check that overridden binding is not NON_OVERRIDABLE.  */
  if (old->n.tb->non_overridable)
    {
      gfc_error ("'%s' at %L overrides a procedure binding declared"
		 " NON_OVERRIDABLE", proc->name, &where);
      return FAILURE;
    }

  /* It's an error to override a non-DEFERRED procedure with a DEFERRED one.  */
  if (!old->n.tb->deferred && proc->n.tb->deferred)
    {
      gfc_error ("'%s' at %L must not be DEFERRED as it overrides a"
		 " non-DEFERRED binding", proc->name, &where);
      return FAILURE;
    }

  /* If the overridden binding is PURE, the overriding must be, too.  */
  if (old_target->attr.pure && !proc_target->attr.pure)
    {
      gfc_error ("'%s' at %L overrides a PURE procedure and must also be PURE",
		 proc->name, &where);
      return FAILURE;
    }

  /* If the overridden binding is ELEMENTAL, the overriding must be, too.  If it
     is not, the overriding must not be either.  */
  if (old_target->attr.elemental && !proc_target->attr.elemental)
    {
      gfc_error ("'%s' at %L overrides an ELEMENTAL procedure and must also be"
		 " ELEMENTAL", proc->name, &where);
      return FAILURE;
    }
  if (!old_target->attr.elemental && proc_target->attr.elemental)
    {
      gfc_error ("'%s' at %L overrides a non-ELEMENTAL procedure and must not"
		 " be ELEMENTAL, either", proc->name, &where);
      return FAILURE;
    }

  /* If the overridden binding is a SUBROUTINE, the overriding must also be a
     SUBROUTINE.  */
  if (old_target->attr.subroutine && !proc_target->attr.subroutine)
    {
      gfc_error ("'%s' at %L overrides a SUBROUTINE and must also be a"
		 " SUBROUTINE", proc->name, &where);
      return FAILURE;
    }

  /* If the overridden binding is a FUNCTION, the overriding must also be a
     FUNCTION and have the same characteristics.  */
  if (old_target->attr.function)
    {
      if (!proc_target->attr.function)
	{
	  gfc_error ("'%s' at %L overrides a FUNCTION and must also be a"
		     " FUNCTION", proc->name, &where);
	  return FAILURE;
	}

      /* FIXME:  Do more comprehensive checking (including, for instance, the
	 rank and array-shape).  */
      gcc_assert (proc_target->result && old_target->result);
      if (!gfc_compare_types (&proc_target->result->ts,
			      &old_target->result->ts))
	{
	  gfc_error ("'%s' at %L and the overridden FUNCTION should have"
		     " matching result types", proc->name, &where);
	  return FAILURE;
	}
    }

  /* If the overridden binding is PUBLIC, the overriding one must not be
     PRIVATE.  */
  if (old->n.tb->access == ACCESS_PUBLIC
      && proc->n.tb->access == ACCESS_PRIVATE)
    {
      gfc_error ("'%s' at %L overrides a PUBLIC procedure and must not be"
		 " PRIVATE", proc->name, &where);
      return FAILURE;
    }

  /* Compare the formal argument lists of both procedures.  This is also abused
     to find the position of the passed-object dummy arguments of both
     bindings as at least the overridden one might not yet be resolved and we
     need those positions in the check below.  */
  proc_pass_arg = old_pass_arg = 0;
  if (!proc->n.tb->nopass && !proc->n.tb->pass_arg)
    proc_pass_arg = 1;
  if (!old->n.tb->nopass && !old->n.tb->pass_arg)
    old_pass_arg = 1;
  argpos = 1;
  for (proc_formal = proc_target->formal, old_formal = old_target->formal;
       proc_formal && old_formal;
       proc_formal = proc_formal->next, old_formal = old_formal->next)
    {
      if (proc->n.tb->pass_arg
	  && !strcmp (proc->n.tb->pass_arg, proc_formal->sym->name))
	proc_pass_arg = argpos;
      if (old->n.tb->pass_arg
	  && !strcmp (old->n.tb->pass_arg, old_formal->sym->name))
	old_pass_arg = argpos;

      /* Check that the names correspond.  */
      if (strcmp (proc_formal->sym->name, old_formal->sym->name))
	{
	  gfc_error ("Dummy argument '%s' of '%s' at %L should be named '%s' as"
		     " to match the corresponding argument of the overridden"
		     " procedure", proc_formal->sym->name, proc->name, &where,
		     old_formal->sym->name);
	  return FAILURE;
	}

      /* Check that the types correspond if neither is the passed-object
	 argument.  */
      /* FIXME:  Do more comprehensive testing here.  */
      if (proc_pass_arg != argpos && old_pass_arg != argpos
	  && !gfc_compare_types (&proc_formal->sym->ts, &old_formal->sym->ts))
	{
	  gfc_error ("Types mismatch for dummy argument '%s' of '%s' %L in"
		     " in respect to the overridden procedure",
		     proc_formal->sym->name, proc->name, &where);
	  return FAILURE;
	}

      ++argpos;
    }
  if (proc_formal || old_formal)
    {
      gfc_error ("'%s' at %L must have the same number of formal arguments as"
		 " the overridden procedure", proc->name, &where);
      return FAILURE;
    }

  /* If the overridden binding is NOPASS, the overriding one must also be
     NOPASS.  */
  if (old->n.tb->nopass && !proc->n.tb->nopass)
    {
      gfc_error ("'%s' at %L overrides a NOPASS binding and must also be"
		 " NOPASS", proc->name, &where);
      return FAILURE;
    }

  /* If the overridden binding is PASS(x), the overriding one must also be
     PASS and the passed-object dummy arguments must correspond.  */
  if (!old->n.tb->nopass)
    {
      if (proc->n.tb->nopass)
	{
	  gfc_error ("'%s' at %L overrides a binding with PASS and must also be"
		     " PASS", proc->name, &where);
	  return FAILURE;
	}

      if (proc_pass_arg != old_pass_arg)
	{
	  gfc_error ("Passed-object dummy argument of '%s' at %L must be at"
		     " the same position as the passed-object dummy argument of"
		     " the overridden procedure", proc->name, &where);
	  return FAILURE;
	}
    }

  return SUCCESS;
}


/* Check if two GENERIC targets are ambiguous and emit an error is they are.  */

static gfc_try
check_generic_tbp_ambiguity (gfc_tbp_generic* t1, gfc_tbp_generic* t2,
			     const char* generic_name, locus where)
{
  gfc_symbol* sym1;
  gfc_symbol* sym2;

  gcc_assert (t1->specific && t2->specific);
  gcc_assert (!t1->specific->is_generic);
  gcc_assert (!t2->specific->is_generic);

  sym1 = t1->specific->u.specific->n.sym;
  sym2 = t2->specific->u.specific->n.sym;

  /* Both must be SUBROUTINEs or both must be FUNCTIONs.  */
  if (sym1->attr.subroutine != sym2->attr.subroutine
      || sym1->attr.function != sym2->attr.function)
    {
      gfc_error ("'%s' and '%s' can't be mixed FUNCTION/SUBROUTINE for"
		 " GENERIC '%s' at %L",
		 sym1->name, sym2->name, generic_name, &where);
      return FAILURE;
    }

  /* Compare the interfaces.  */
  if (gfc_compare_interfaces (sym1, sym2, 1, 0, NULL, 0))
    {
      gfc_error ("'%s' and '%s' for GENERIC '%s' at %L are ambiguous",
		 sym1->name, sym2->name, generic_name, &where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Resolve a GENERIC procedure binding for a derived type.  */

static gfc_try
resolve_typebound_generic (gfc_symbol* derived, gfc_symtree* st)
{
  gfc_tbp_generic* target;
  gfc_symtree* first_target;
  gfc_symbol* super_type;
  gfc_symtree* inherited;
  locus where;

  gcc_assert (st->n.tb);
  gcc_assert (st->n.tb->is_generic);

  where = st->n.tb->where;
  super_type = gfc_get_derived_super_type (derived);

  /* Find the overridden binding if any.  */
  st->n.tb->overridden = NULL;
  if (super_type)
    {
      gfc_symtree* overridden;
      overridden = gfc_find_typebound_proc (super_type, NULL, st->name, true);

      if (overridden && overridden->n.tb)
	st->n.tb->overridden = overridden->n.tb;
    }

  /* Try to find the specific bindings for the symtrees in our target-list.  */
  gcc_assert (st->n.tb->u.generic);
  for (target = st->n.tb->u.generic; target; target = target->next)
    if (!target->specific)
      {
	gfc_typebound_proc* overridden_tbp;
	gfc_tbp_generic* g;
	const char* target_name;

	target_name = target->specific_st->name;

	/* Defined for this type directly.  */
	if (target->specific_st->n.tb)
	  {
	    target->specific = target->specific_st->n.tb;
	    goto specific_found;
	  }

	/* Look for an inherited specific binding.  */
	if (super_type)
	  {
	    inherited = gfc_find_typebound_proc (super_type, NULL,
						 target_name, true);

	    if (inherited)
	      {
		gcc_assert (inherited->n.tb);
		target->specific = inherited->n.tb;
		goto specific_found;
	      }
	  }

	gfc_error ("Undefined specific binding '%s' as target of GENERIC '%s'"
		   " at %L", target_name, st->name, &where);
	return FAILURE;

	/* Once we've found the specific binding, check it is not ambiguous with
	   other specifics already found or inherited for the same GENERIC.  */
specific_found:
	gcc_assert (target->specific);

	/* This must really be a specific binding!  */
	if (target->specific->is_generic)
	  {
	    gfc_error ("GENERIC '%s' at %L must target a specific binding,"
		       " '%s' is GENERIC, too", st->name, &where, target_name);
	    return FAILURE;
	  }

	/* Check those already resolved on this type directly.  */
	for (g = st->n.tb->u.generic; g; g = g->next)
	  if (g != target && g->specific
	      && check_generic_tbp_ambiguity (target, g, st->name, where)
		  == FAILURE)
	    return FAILURE;

	/* Check for ambiguity with inherited specific targets.  */
	for (overridden_tbp = st->n.tb->overridden; overridden_tbp;
	     overridden_tbp = overridden_tbp->overridden)
	  if (overridden_tbp->is_generic)
	    {
	      for (g = overridden_tbp->u.generic; g; g = g->next)
		{
		  gcc_assert (g->specific);
		  if (check_generic_tbp_ambiguity (target, g,
						   st->name, where) == FAILURE)
		    return FAILURE;
		}
	    }
      }

  /* If we attempt to "overwrite" a specific binding, this is an error.  */
  if (st->n.tb->overridden && !st->n.tb->overridden->is_generic)
    {
      gfc_error ("GENERIC '%s' at %L can't overwrite specific binding with"
		 " the same name", st->name, &where);
      return FAILURE;
    }

  /* Take the SUBROUTINE/FUNCTION attributes of the first specific target, as
     all must have the same attributes here.  */
  first_target = st->n.tb->u.generic->specific->u.specific;
  gcc_assert (first_target);
  st->n.tb->subroutine = first_target->n.sym->attr.subroutine;
  st->n.tb->function = first_target->n.sym->attr.function;

  return SUCCESS;
}


/* Resolve the type-bound procedures for a derived type.  */

static gfc_symbol* resolve_bindings_derived;
static gfc_try resolve_bindings_result;

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
      if (resolve_typebound_generic (resolve_bindings_derived, stree)
	    == FAILURE)
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

  /* It should be a module procedure or an external procedure with explicit
     interface.  For DEFERRED bindings, abstract interfaces are ok as well.  */
  if ((!proc->attr.subroutine && !proc->attr.function)
      || (proc->attr.proc != PROC_MODULE
	  && proc->attr.if_source != IFSRC_IFBODY)
      || (proc->attr.abstract && !stree->n.tb->deferred))
    {
      gfc_error ("'%s' must be a module procedure or an external procedure with"
		 " an explicit interface at %L", proc->name, &where);
      goto error;
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
      if (stree->n.tb->pass_arg)
	{
	  gfc_formal_arglist* i;

	  /* If an explicit passing argument name is given, walk the arg-list
	     and look for it.  */

	  me_arg = NULL;
	  stree->n.tb->pass_arg_num = 1;
	  for (i = proc->formal; i; i = i->next)
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
	      gfc_error ("Procedure '%s' with PASS(%s) at %L has no"
			 " argument '%s'",
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
	  if (!proc->formal)
	    {
	      gfc_error ("Procedure '%s' with PASS at %L must have at"
			 " least one argument", proc->name, &where);
	      goto error;
	    }
	  me_arg = proc->formal->sym;
	}

      /* Now check that the argument-type matches.  */
      gcc_assert (me_arg);
      if (me_arg->ts.type != BT_DERIVED
	  || me_arg->ts.derived != resolve_bindings_derived)
	{
	  gfc_error ("Argument '%s' of '%s' with PASS(%s) at %L must be of"
		     " the derived-type '%s'", me_arg->name, proc->name,
		     me_arg->name, &where, resolve_bindings_derived->name);
	  goto error;
	}

      gfc_warning ("Polymorphic entities are not yet implemented,"
		   " non-polymorphic passed-object dummy argument of '%s'"
		   " at %L accepted", proc->name, &where);
    }

  /* If we are extending some type, check that we don't override a procedure
     flagged NON_OVERRIDABLE.  */
  stree->n.tb->overridden = NULL;
  if (super_type)
    {
      gfc_symtree* overridden;
      overridden = gfc_find_typebound_proc (super_type, NULL,
					    stree->name, true);

      if (overridden && overridden->n.tb)
	stree->n.tb->overridden = overridden->n.tb;

      if (overridden && check_typebound_override (stree, overridden) == FAILURE)
	goto error;
    }

  /* See if there's a name collision with a component directly in this type.  */
  for (comp = resolve_bindings_derived->components; comp; comp = comp->next)
    if (!strcmp (comp->name, stree->name))
      {
	gfc_error ("Procedure '%s' at %L has the same name as a component of"
		   " '%s'",
		   stree->name, &where, resolve_bindings_derived->name);
	goto error;
      }

  /* Try to find a name collision with an inherited component.  */
  if (super_type && gfc_find_component (super_type, stree->name, true, true))
    {
      gfc_error ("Procedure '%s' at %L has the same name as an inherited"
		 " component of '%s'",
		 stree->name, &where, resolve_bindings_derived->name);
      goto error;
    }

  stree->n.tb->error = 0;
  return;

error:
  resolve_bindings_result = FAILURE;
  stree->n.tb->error = 1;
}

static gfc_try
resolve_typebound_procedures (gfc_symbol* derived)
{
  if (!derived->f2k_derived || !derived->f2k_derived->tb_sym_root)
    return SUCCESS;

  resolve_bindings_derived = derived;
  resolve_bindings_result = SUCCESS;
  gfc_traverse_symtree (derived->f2k_derived->tb_sym_root,
			&resolve_typebound_procedure);

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
      break;

  if (dt_list == NULL)
    {
      dt_list = gfc_get_dt_list ();
      dt_list->next = gfc_derived_types;
      dt_list->derived = derived;
      gfc_derived_types = dt_list;
    }
}


/* Ensure that a derived-type is really not abstract, meaning that every
   inherited DEFERRED binding is overridden by a non-DEFERRED one.  */

static gfc_try
ensure_not_abstract_walker (gfc_symbol* sub, gfc_symtree* st)
{
  if (!st)
    return SUCCESS;

  if (ensure_not_abstract_walker (sub, st->left) == FAILURE)
    return FAILURE;
  if (ensure_not_abstract_walker (sub, st->right) == FAILURE)
    return FAILURE;

  if (st->n.tb && st->n.tb->deferred)
    {
      gfc_symtree* overriding;
      overriding = gfc_find_typebound_proc (sub, NULL, st->name, true);
      gcc_assert (overriding && overriding->n.tb);
      if (overriding->n.tb->deferred)
	{
	  gfc_error ("Derived-type '%s' declared at %L must be ABSTRACT because"
		     " '%s' is DEFERRED and not overridden",
		     sub->name, &sub->declared_at, st->name);
	  return FAILURE;
	}
    }

  return SUCCESS;
}

static gfc_try
ensure_not_abstract (gfc_symbol* sub, gfc_symbol* ancestor)
{
  /* The algorithm used here is to recursively travel up the ancestry of sub
     and for each ancestor-type, check all bindings.  If any of them is
     DEFERRED, look it up starting from sub and see if the found (overriding)
     binding is not DEFERRED.
     This is not the most efficient way to do this, but it should be ok and is
     clearer than something sophisticated.  */

  gcc_assert (ancestor && ancestor->attr.abstract && !sub->attr.abstract);

  /* Walk bindings of this ancestor.  */
  if (ancestor->f2k_derived)
    {
      gfc_try t;
      t = ensure_not_abstract_walker (sub, ancestor->f2k_derived->tb_sym_root);
      if (t == FAILURE)
	return FAILURE;
    }

  /* Find next ancestor type and recurse on it.  */
  ancestor = gfc_get_derived_super_type (ancestor);
  if (ancestor)
    return ensure_not_abstract (sub, ancestor);

  return SUCCESS;
}


static void resolve_symbol (gfc_symbol *sym);


/* Resolve the components of a derived type.  */

static gfc_try
resolve_fl_derived (gfc_symbol *sym)
{
  gfc_symbol* super_type;
  gfc_component *c;
  int i;

  super_type = gfc_get_derived_super_type (sym);

  /* Ensure the extended type gets resolved before we do.  */
  if (super_type && resolve_fl_derived (super_type) == FAILURE)
    return FAILURE;

  /* An ABSTRACT type must be extensible.  */
  if (sym->attr.abstract && (sym->attr.is_bind_c || sym->attr.sequence))
    {
      gfc_error ("Non-extensible derived-type '%s' at %L must not be ABSTRACT",
		 sym->name, &sym->declared_at);
      return FAILURE;
    }

  for (c = sym->components; c != NULL; c = c->next)
    {
      if (c->attr.proc_pointer && c->ts.interface)
	{
	  if (c->ts.interface->attr.procedure)
	    gfc_error ("Interface '%s', used by procedure pointer component "
		       "'%s' at %L, is declared in a later PROCEDURE statement",
		       c->ts.interface->name, c->name, &c->loc);

	  /* Get the attributes from the interface (now resolved).  */
	  if (c->ts.interface->attr.if_source
	      || c->ts.interface->attr.intrinsic)
	    {
	      gfc_symbol *ifc = c->ts.interface;

	      if (ifc->formal && !ifc->formal_ns)
		resolve_symbol (ifc);

	      if (ifc->attr.intrinsic)
		resolve_intrinsic (ifc, &ifc->declared_at);

	      if (ifc->result)
		{
		  c->ts = ifc->result->ts;
		  c->attr.allocatable = ifc->result->attr.allocatable;
		  c->attr.pointer = ifc->result->attr.pointer;
		  c->attr.dimension = ifc->result->attr.dimension;
		  c->as = gfc_copy_array_spec (ifc->result->as);
		}
	      else
		{   
		  c->ts = ifc->ts;
		  c->attr.allocatable = ifc->attr.allocatable;
		  c->attr.pointer = ifc->attr.pointer;
		  c->attr.dimension = ifc->attr.dimension;
		  c->as = gfc_copy_array_spec (ifc->as);
		}
	      c->ts.interface = ifc;
	      c->attr.function = ifc->attr.function;
	      c->attr.subroutine = ifc->attr.subroutine;
	      gfc_copy_formal_args_ppc (c, ifc);

	      c->attr.pure = ifc->attr.pure;
	      c->attr.elemental = ifc->attr.elemental;
	      c->attr.recursive = ifc->attr.recursive;
	      c->attr.always_explicit = ifc->attr.always_explicit;
	      /* Replace symbols in array spec.  */
	      if (c->as)
		{
		  int i;
		  for (i = 0; i < c->as->rank; i++)
		    {
		      gfc_expr_replace_comp (c->as->lower[i], c);
		      gfc_expr_replace_comp (c->as->upper[i], c);
		    }
	        }
	      /* Copy char length.  */
	      if (ifc->ts.cl)
		{
		  c->ts.cl = gfc_new_charlen (sym->ns);
	          c->ts.cl->resolved = ifc->ts.cl->resolved;
		  c->ts.cl->length = gfc_copy_expr (ifc->ts.cl->length);
		  /* TODO: gfc_expr_replace_symbols (c->ts.cl->length, c);*/
		}
	    }
	  else if (c->ts.interface->name[0] != '\0')
	    {
	      gfc_error ("Interface '%s' of procedure pointer component "
			 "'%s' at %L must be explicit", c->ts.interface->name,
			 c->name, &c->loc);
	      return FAILURE;
 	    }
	}
      else if (c->attr.proc_pointer && c->ts.type == BT_UNKNOWN)
	{
	  c->ts = *gfc_get_default_type (c->name, NULL);
	  c->attr.implicit_type = 1;
	}

      /* Procedure pointer components: Check PASS arg.  */
      if (c->attr.proc_pointer && !c->tb->nopass && c->tb->pass_arg_num == 0)
	{
	  gfc_symbol* me_arg;

	  if (c->tb->pass_arg)
	    {
	      gfc_formal_arglist* i;

	      /* If an explicit passing argument name is given, walk the arg-list
		and look for it.  */

	      me_arg = NULL;
	      c->tb->pass_arg_num = 1;
	      for (i = c->formal; i; i = i->next)
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
		  gfc_error ("Procedure pointer component '%s' with PASS(%s) "
			     "at %L has no argument '%s'", c->name,
			     c->tb->pass_arg, &c->loc, c->tb->pass_arg);
		  c->tb->error = 1;
		  return FAILURE;
		}
	    }
	  else
	    {
	      /* Otherwise, take the first one; there should in fact be at least
		one.  */
	      c->tb->pass_arg_num = 1;
	      if (!c->formal)
		{
		  gfc_error ("Procedure pointer component '%s' with PASS at %L "
			     "must have at least one argument",
			     c->name, &c->loc);
		  c->tb->error = 1;
		  return FAILURE;
		}
	      me_arg = c->formal->sym;
	    }

	  /* Now check that the argument-type matches.  */
	  gcc_assert (me_arg);
	  if (me_arg->ts.type != BT_DERIVED
	      || me_arg->ts.derived != sym)
	    {
	      gfc_error ("Argument '%s' of '%s' with PASS(%s) at %L must be of"
			 " the derived type '%s'", me_arg->name, c->name,
			 me_arg->name, &c->loc, sym->name);
	      c->tb->error = 1;
	      return FAILURE;
	    }

	  /* Check for C453.  */
	  if (me_arg->attr.dimension)
	    {
	      gfc_error ("Argument '%s' of '%s' with PASS(%s) at %L "
			 "must be scalar", me_arg->name, c->name, me_arg->name,
			 &c->loc);
	      c->tb->error = 1;
	      return FAILURE;
	    }

	  if (me_arg->attr.pointer)
	    {
	      gfc_error ("Argument '%s' of '%s' with PASS(%s) at %L "
			 "may not have the POINTER attribute", me_arg->name,
			 c->name, me_arg->name, &c->loc);
	      c->tb->error = 1;
	      return FAILURE;
	    }

	  if (me_arg->attr.allocatable)
	    {
	      gfc_error ("Argument '%s' of '%s' with PASS(%s) at %L "
			 "may not be ALLOCATABLE", me_arg->name, c->name,
			 me_arg->name, &c->loc);
	      c->tb->error = 1;
	      return FAILURE;
	    }

	  /* TODO: Make this an error once CLASS is implemented.  */
	  if (!sym->attr.sequence)
	    gfc_warning ("Polymorphic entities are not yet implemented,"
			 " non-polymorphic passed-object dummy argument of '%s'"
			 " at %L accepted", c->name, &c->loc);

	}

      /* Check type-spec if this is not the parent-type component.  */
      if ((!sym->attr.extension || c != sym->components)
	  && resolve_typespec_used (&c->ts, &c->loc, c->name) == FAILURE)
	return FAILURE;

      /* If this type is an extension, see if this component has the same name
	 as an inherited type-bound procedure.  */
      if (super_type
	  && gfc_find_typebound_proc (super_type, NULL, c->name, true))
	{
	  gfc_error ("Component '%s' of '%s' at %L has the same name as an"
		     " inherited type-bound procedure",
		     c->name, sym->name, &c->loc);
	  return FAILURE;
	}

      if (c->ts.type == BT_CHARACTER)
	{
	 if (c->ts.cl->length == NULL
	     || (resolve_charlen (c->ts.cl) == FAILURE)
	     || !gfc_is_constant_expr (c->ts.cl->length))
	   {
	     gfc_error ("Character length of component '%s' needs to "
			"be a constant specification expression at %L",
			c->name,
			c->ts.cl->length ? &c->ts.cl->length->where : &c->loc);
	     return FAILURE;
	   }
	}

      if (c->ts.type == BT_DERIVED
	  && sym->component_access != ACCESS_PRIVATE
	  && gfc_check_access (sym->attr.access, sym->ns->default_access)
	  && !is_sym_host_assoc (c->ts.derived, sym->ns)
	  && !c->ts.derived->attr.use_assoc
	  && !gfc_check_access (c->ts.derived->attr.access,
				c->ts.derived->ns->default_access)
	  && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: the component '%s' "
			     "is a PRIVATE type and cannot be a component of "
			     "'%s', which is PUBLIC at %L", c->name,
			     sym->name, &sym->declared_at) == FAILURE)
	return FAILURE;

      if (sym->attr.sequence)
	{
	  if (c->ts.type == BT_DERIVED && c->ts.derived->attr.sequence == 0)
	    {
	      gfc_error ("Component %s of SEQUENCE type declared at %L does "
			 "not have the SEQUENCE attribute",
			 c->ts.derived->name, &sym->declared_at);
	      return FAILURE;
	    }
	}

      if (c->ts.type == BT_DERIVED && c->attr.pointer
	  && c->ts.derived->components == NULL
	  && !c->ts.derived->attr.zero_comp)
	{
	  gfc_error ("The pointer component '%s' of '%s' at %L is a type "
		     "that has not been declared", c->name, sym->name,
		     &c->loc);
	  return FAILURE;
	}

      /* Ensure that all the derived type components are put on the
	 derived type list; even in formal namespaces, where derived type
	 pointer components might not have been declared.  */
      if (c->ts.type == BT_DERIVED
	    && c->ts.derived
	    && c->ts.derived->components
	    && c->attr.pointer
	    && sym != c->ts.derived)
	add_dt_to_dt_list (c->ts.derived);

      if (c->attr.pointer || c->attr.proc_pointer || c->attr.allocatable
	  || c->as == NULL)
	continue;

      for (i = 0; i < c->as->rank; i++)
	{
	  if (c->as->lower[i] == NULL
	      || (resolve_index_expr (c->as->lower[i]) == FAILURE)
	      || !gfc_is_constant_expr (c->as->lower[i])
	      || c->as->upper[i] == NULL
	      || (resolve_index_expr (c->as->upper[i]) == FAILURE)
	      || !gfc_is_constant_expr (c->as->upper[i]))
	    {
	      gfc_error ("Component '%s' of '%s' at %L must have "
			 "constant array bounds",
			 c->name, sym->name, &c->loc);
	      return FAILURE;
	    }
	}
    }

  /* Resolve the type-bound procedures.  */
  if (resolve_typebound_procedures (sym) == FAILURE)
    return FAILURE;

  /* Resolve the finalizer procedures.  */
  if (gfc_resolve_finalizers (sym) == FAILURE)
    return FAILURE;

  /* If this is a non-ABSTRACT type extending an ABSTRACT one, ensure that
     all DEFERRED bindings are overridden.  */
  if (super_type && super_type->attr.abstract && !sym->attr.abstract
      && ensure_not_abstract (sym, super_type) == FAILURE)
    return FAILURE;

  /* Add derived type to the derived type list.  */
  add_dt_to_dt_list (sym);

  return SUCCESS;
}


static gfc_try
resolve_fl_namelist (gfc_symbol *sym)
{
  gfc_namelist *nl;
  gfc_symbol *nlsym;

  /* Reject PRIVATE objects in a PUBLIC namelist.  */
  if (gfc_check_access(sym->attr.access, sym->ns->default_access))
    {
      for (nl = sym->namelist; nl; nl = nl->next)
	{
	  if (!nl->sym->attr.use_assoc
	      && !is_sym_host_assoc (nl->sym, sym->ns)
	      && !gfc_check_access(nl->sym->attr.access,
				nl->sym->ns->default_access))
	    {
	      gfc_error ("NAMELIST object '%s' was declared PRIVATE and "
			 "cannot be member of PUBLIC namelist '%s' at %L",
			 nl->sym->name, sym->name, &sym->declared_at);
	      return FAILURE;
	    }

	  /* Types with private components that came here by USE-association.  */
	  if (nl->sym->ts.type == BT_DERIVED
	      && derived_inaccessible (nl->sym->ts.derived))
	    {
	      gfc_error ("NAMELIST object '%s' has use-associated PRIVATE "
			 "components and cannot be member of namelist '%s' at %L",
			 nl->sym->name, sym->name, &sym->declared_at);
	      return FAILURE;
	    }

	  /* Types with private components that are defined in the same module.  */
	  if (nl->sym->ts.type == BT_DERIVED
	      && !is_sym_host_assoc (nl->sym->ts.derived, sym->ns)
	      && !gfc_check_access (nl->sym->ts.derived->attr.private_comp
					? ACCESS_PRIVATE : ACCESS_UNKNOWN,
					nl->sym->ns->default_access))
	    {
	      gfc_error ("NAMELIST object '%s' has PRIVATE components and "
			 "cannot be a member of PUBLIC namelist '%s' at %L",
			 nl->sym->name, sym->name, &sym->declared_at);
	      return FAILURE;
	    }
	}
    }

  for (nl = sym->namelist; nl; nl = nl->next)
    {
      /* Reject namelist arrays of assumed shape.  */
      if (nl->sym->as && nl->sym->as->type == AS_ASSUMED_SHAPE
	  && gfc_notify_std (GFC_STD_F2003, "NAMELIST array object '%s' "
			     "must not have assumed shape in namelist "
			     "'%s' at %L", nl->sym->name, sym->name,
			     &sym->declared_at) == FAILURE)
	    return FAILURE;

      /* Reject namelist arrays that are not constant shape.  */
      if (is_non_constant_shape_array (nl->sym))
	{
	  gfc_error ("NAMELIST array object '%s' must have constant "
		     "shape in namelist '%s' at %L", nl->sym->name,
		     sym->name, &sym->declared_at);
	  return FAILURE;
	}

      /* Namelist objects cannot have allocatable or pointer components.  */
      if (nl->sym->ts.type != BT_DERIVED)
	continue;

      if (nl->sym->ts.derived->attr.alloc_comp)
	{
	  gfc_error ("NAMELIST object '%s' in namelist '%s' at %L cannot "
		     "have ALLOCATABLE components",
		     nl->sym->name, sym->name, &sym->declared_at);
	  return FAILURE;
	}

      if (nl->sym->ts.derived->attr.pointer_comp)
	{
	  gfc_error ("NAMELIST object '%s' in namelist '%s' at %L cannot "
		     "have POINTER components", 
		     nl->sym->name, sym->name, &sym->declared_at);
	  return FAILURE;
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
      if (nl->sym && nl->sym->name)
	gfc_find_symbol (nl->sym->name, sym->ns, 1, &nlsym);
      if (nlsym && nlsym->attr.flavor == FL_PROCEDURE)
	{
	  gfc_error ("PROCEDURE attribute conflicts with NAMELIST "
		     "attribute in '%s' at %L", nlsym->name,
		     &sym->declared_at);
	  return FAILURE;
	}
    }

  return SUCCESS;
}


static gfc_try
resolve_fl_parameter (gfc_symbol *sym)
{
  /* A parameter array's shape needs to be constant.  */
  if (sym->as != NULL 
      && (sym->as->type == AS_DEFERRED
          || is_non_constant_shape_array (sym)))
    {
      gfc_error ("Parameter array '%s' at %L cannot be automatic "
		 "or of deferred shape", sym->name, &sym->declared_at);
      return FAILURE;
    }

  /* Make sure a parameter that has been implicitly typed still
     matches the implicit type, since PARAMETER statements can precede
     IMPLICIT statements.  */
  if (sym->attr.implicit_type
      && !gfc_compare_types (&sym->ts, gfc_get_default_type (sym->name,
							     sym->ns)))
    {
      gfc_error ("Implicitly typed PARAMETER '%s' at %L doesn't match a "
		 "later IMPLICIT type", sym->name, &sym->declared_at);
      return FAILURE;
    }

  /* Make sure the types of derived parameters are consistent.  This
     type checking is deferred until resolution because the type may
     refer to a derived type from the host.  */
  if (sym->ts.type == BT_DERIVED
      && !gfc_compare_types (&sym->ts, &sym->value->ts))
    {
      gfc_error ("Incompatible derived type in PARAMETER at %L",
		 &sym->value->where);
      return FAILURE;
    }
  return SUCCESS;
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

  if (sym->attr.flavor == FL_UNKNOWN)
    {

    /* If we find that a flavorless symbol is an interface in one of the
       parent namespaces, find its symtree in this namespace, free the
       symbol and set the symtree to point to the interface symbol.  */
      for (ns = gfc_current_ns->parent; ns; ns = ns->parent)
	{
	  symtree = gfc_find_symtree (ns->sym_root, sym->name);
	  if (symtree && symtree->n.sym->generic)
	    {
	      this_symtree = gfc_find_symtree (gfc_current_ns->sym_root,
					       sym->name);
	      sym->refs--;
	      if (!sym->refs)
		gfc_free_symbol (sym);
	      symtree->n.sym->refs++;
	      this_symtree->n.sym = symtree->n.sym;
	      return;
	    }
	}

      /* Otherwise give it a flavor according to such attributes as
	 it has.  */
      if (sym->attr.external == 0 && sym->attr.intrinsic == 0)
	sym->attr.flavor = FL_VARIABLE;
      else
	{
	  sym->attr.flavor = FL_PROCEDURE;
	  if (sym->attr.dimension)
	    sym->attr.function = 1;
	}
    }

  if (sym->attr.external && sym->ts.type != BT_UNKNOWN && !sym->attr.function)
    gfc_add_function (&sym->attr, sym->name, &sym->declared_at);

  if (sym->attr.procedure && sym->ts.interface
      && sym->attr.if_source != IFSRC_DECL)
    {
      if (sym->ts.interface == sym)
	{
	  gfc_error ("PROCEDURE '%s' at %L may not be used as its own "
		     "interface", sym->name, &sym->declared_at);
	  return;
	}
      if (sym->ts.interface->attr.procedure)
	{
	  gfc_error ("Interface '%s', used by procedure '%s' at %L, is declared"
		     " in a later PROCEDURE statement", sym->ts.interface->name,
		     sym->name,&sym->declared_at);
	  return;
	}

      /* Get the attributes from the interface (now resolved).  */
      if (sym->ts.interface->attr.if_source
	  || sym->ts.interface->attr.intrinsic)
	{
	  gfc_symbol *ifc = sym->ts.interface;
	  resolve_symbol (ifc);

	  if (ifc->attr.intrinsic)
	    resolve_intrinsic (ifc, &ifc->declared_at);

	  if (ifc->result)
	    sym->ts = ifc->result->ts;
	  else   
	    sym->ts = ifc->ts;
	  sym->ts.interface = ifc;
	  sym->attr.function = ifc->attr.function;
	  sym->attr.subroutine = ifc->attr.subroutine;
	  gfc_copy_formal_args (sym, ifc);

	  sym->attr.allocatable = ifc->attr.allocatable;
	  sym->attr.pointer = ifc->attr.pointer;
	  sym->attr.pure = ifc->attr.pure;
	  sym->attr.elemental = ifc->attr.elemental;
	  sym->attr.dimension = ifc->attr.dimension;
	  sym->attr.recursive = ifc->attr.recursive;
	  sym->attr.always_explicit = ifc->attr.always_explicit;
	  /* Copy array spec.  */
	  sym->as = gfc_copy_array_spec (ifc->as);
	  if (sym->as)
	    {
	      int i;
	      for (i = 0; i < sym->as->rank; i++)
		{
		  gfc_expr_replace_symbols (sym->as->lower[i], sym);
		  gfc_expr_replace_symbols (sym->as->upper[i], sym);
		}
	    }
	  /* Copy char length.  */
	  if (ifc->ts.cl)
	    {
	      sym->ts.cl = gfc_new_charlen (sym->ns);
	      sym->ts.cl->resolved = ifc->ts.cl->resolved;
	      sym->ts.cl->length = gfc_copy_expr (ifc->ts.cl->length);
	      gfc_expr_replace_symbols (sym->ts.cl->length, sym);
	    }
	}
      else if (sym->ts.interface->name[0] != '\0')
	{
	  gfc_error ("Interface '%s' of procedure '%s' at %L must be explicit",
		    sym->ts.interface->name, sym->name, &sym->declared_at);
	  return;
	}
    }

  if (sym->attr.flavor == FL_DERIVED && resolve_fl_derived (sym) == FAILURE)
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
  if (sym->attr.flavor != FL_MODULE && sym->attr.intrinsic)
    {
      gfc_intrinsic_sym* isym;
      const char* symstd;

      /* We already know this one is an intrinsic, so we don't call
	 gfc_is_intrinsic for full checking but rather use gfc_find_function and
	 gfc_find_subroutine directly to check whether it is a function or
	 subroutine.  */

      if ((isym = gfc_find_function (sym->name)))
	{
	  if (sym->ts.type != BT_UNKNOWN && gfc_option.warn_surprising
	      && !sym->attr.implicit_type)
	    gfc_warning ("Type specified for intrinsic function '%s' at %L is"
			 " ignored", sym->name, &sym->declared_at);
	}
      else if ((isym = gfc_find_subroutine (sym->name)))
	{
	  if (sym->ts.type != BT_UNKNOWN && !sym->attr.implicit_type)
	    {
	      gfc_error ("Intrinsic subroutine '%s' at %L shall not have a type"
			 " specifier", sym->name, &sym->declared_at);
	      return;
	    }
	}
      else
	{
	  gfc_error ("'%s' declared INTRINSIC at %L does not exist",
		     sym->name, &sym->declared_at);
	  return;
	}

      /* Check it is actually available in the standard settings.  */
      if (gfc_check_intrinsic_standard (isym, &symstd, false, sym->declared_at)
	    == FAILURE)
	{
	  gfc_error ("The intrinsic '%s' declared INTRINSIC at %L is not"
		     " available in the current standard settings but %s.  Use"
                     " an appropriate -std=* option or enable -fall-intrinsics"
                     " in order to use it.",
                     sym->name, &sym->declared_at, symstd);
	  return;
	}
     }

  /* Assign default type to symbols that need one and don't have one.  */
  if (sym->ts.type == BT_UNKNOWN)
    {
      if (sym->attr.flavor == FL_VARIABLE || sym->attr.flavor == FL_PARAMETER)
	gfc_set_default_type (sym, 1, NULL);

      if (sym->attr.flavor == FL_PROCEDURE && sym->attr.external
	  && !sym->attr.function && !sym->attr.subroutine
	  && gfc_get_default_type (sym->name, sym->ns)->type == BT_UNKNOWN)
	gfc_add_subroutine (&sym->attr, sym->name, &sym->declared_at);

      if (sym->attr.flavor == FL_PROCEDURE && sym->attr.function)
	{
	  /* The specific case of an external procedure should emit an error
	     in the case that there is no implicit type.  */
	  if (!mp_flag)
	    gfc_set_default_type (sym, sym->attr.external, NULL);
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
		}
	    }
	}
    }

  /* Assumed size arrays and assumed shape arrays must be dummy
     arguments.  */

  if (sym->as != NULL
      && (sym->as->type == AS_ASSUMED_SIZE
	  || sym->as->type == AS_ASSUMED_SHAPE)
      && sym->attr.dummy == 0)
    {
      if (sym->as->type == AS_ASSUMED_SIZE)
	gfc_error ("Assumed size array at %L must be a dummy argument",
		   &sym->declared_at);
      else
	gfc_error ("Assumed shape array at %L must be a dummy argument",
		   &sym->declared_at);
      return;
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
      gfc_error ("'%s' at %L cannot have the VALUE attribute because "
		 "it is not a dummy argument", sym->name, &sym->declared_at);
      return;
    }

  if (sym->attr.value && sym->ts.type == BT_CHARACTER)
    {
      gfc_charlen *cl = sym->ts.cl;
      if (!cl || !cl->length || cl->length->expr_type != EXPR_CONSTANT)
	{
	  gfc_error ("Character dummy variable '%s' at %L with VALUE "
		     "attribute must have constant length",
		     sym->name, &sym->declared_at);
	  return;
	}

      if (sym->ts.is_c_interop
	  && mpz_cmp_si (cl->length->value.integer, 1) != 0)
	{
	  gfc_error ("C interoperable character dummy variable '%s' at %L "
		     "with VALUE attribute must have length one",
		     sym->name, &sym->declared_at);
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
      gfc_try t = SUCCESS;
      
      /* First, make sure the variable is declared at the
	 module-level scope (J3/04-007, Section 15.3).	*/
      if (sym->ns->proc_name->attr.flavor != FL_MODULE &&
          sym->attr.in_common == 0)
	{
	  gfc_error ("Variable '%s' at %L cannot be BIND(C) because it "
		     "is neither a COMMON block nor declared at the "
		     "module level scope", sym->name, &(sym->declared_at));
	  t = FAILURE;
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
              sym->ts.derived->attr.is_c_interop != 1)
            {
              /* Make sure the user marked the derived type as BIND(C).  If
                 not, call the verify routine.  This could print an error
                 for the derived type more than once if multiple variables
                 of that type are declared.  */
              if (sym->ts.derived->attr.is_bind_c != 1)
                verify_bind_c_derived_type (sym->ts.derived);
              t = FAILURE;
            }
	  
	  /* Verify the variable itself as C interoperable if it
             is BIND(C).  It is not possible for this to succeed if
             the verify_bind_c_derived_type failed, so don't have to handle
             any error returned by verify_bind_c_derived_type.  */
          t = verify_bind_c_sym (sym, &(sym->ts), sym->attr.in_common,
                                 sym->common_block);
	}

      if (t == FAILURE)
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
  if (sym->ts.type == BT_DERIVED && sym->ts.derived->components == NULL
      && !sym->ts.derived->attr.zero_comp)
    {
      gfc_error ("The derived type '%s' at %L is of type '%s', "
		 "which has not been defined", sym->name,
		  &sym->declared_at, sym->ts.derived->name);
      sym->ts.type = BT_UNKNOWN;
      return;
    }

  /* Make sure that the derived type has been resolved and that the
     derived type is visible in the symbol's namespace, if it is a
     module function and is not PRIVATE.  */
  if (sym->ts.type == BT_DERIVED
	&& sym->ts.derived->attr.use_assoc
	&& sym->ns->proc_name
	&& sym->ns->proc_name->attr.flavor == FL_MODULE)
    {
      gfc_symbol *ds;

      if (resolve_fl_derived (sym->ts.derived) == FAILURE)
	return;

      gfc_find_symbol (sym->ts.derived->name, sym->ns, 1, &ds);
      if (!ds && sym->attr.function
	    && gfc_check_access (sym->attr.access, sym->ns->default_access))
	{
	  symtree = gfc_new_symtree (&sym->ns->sym_root,
				     sym->ts.derived->name);
	  symtree->n.sym = sym->ts.derived;
	  sym->ts.derived->refs++;
	}
    }

  /* Unless the derived-type declaration is use associated, Fortran 95
     does not allow public entries of private derived types.
     See 4.4.1 (F95) and 4.5.1.1 (F2003); and related interpretation
     161 in 95-006r3.  */
  if (sym->ts.type == BT_DERIVED
      && sym->ns->proc_name && sym->ns->proc_name->attr.flavor == FL_MODULE
      && !sym->ts.derived->attr.use_assoc
      && gfc_check_access (sym->attr.access, sym->ns->default_access)
      && !gfc_check_access (sym->ts.derived->attr.access,
			    sym->ts.derived->ns->default_access)
      && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: PUBLIC %s '%s' at %L "
		         "of PRIVATE derived type '%s'",
			 (sym->attr.flavor == FL_PARAMETER) ? "parameter"
			 : "variable", sym->name, &sym->declared_at,
			 sym->ts.derived->name) == FAILURE)
    return;

  /* An assumed-size array with INTENT(OUT) shall not be of a type for which
     default initialization is defined (5.1.2.4.4).  */
  if (sym->ts.type == BT_DERIVED
      && sym->attr.dummy
      && sym->attr.intent == INTENT_OUT
      && sym->as
      && sym->as->type == AS_ASSUMED_SIZE)
    {
      for (c = sym->ts.derived->components; c; c = c->next)
	{
	  if (c->initializer)
	    {
	      gfc_error ("The INTENT(OUT) dummy argument '%s' at %L is "
			 "ASSUMED SIZE and so cannot have a default initializer",
			 sym->name, &sym->declared_at);
	      return;
	    }
	}
    }

  switch (sym->attr.flavor)
    {
    case FL_VARIABLE:
      if (resolve_fl_variable (sym, mp_flag) == FAILURE)
	return;
      break;

    case FL_PROCEDURE:
      if (resolve_fl_procedure (sym, mp_flag) == FAILURE)
	return;
      break;

    case FL_NAMELIST:
      if (resolve_fl_namelist (sym) == FAILURE)
	return;
      break;

    case FL_PARAMETER:
      if (resolve_fl_parameter (sym) == FAILURE)
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
    formal_arg_flag = 1;

  gfc_resolve_array_spec (sym->as, check_constant);

  formal_arg_flag = 0;

  /* Resolve formal namespaces.  */
  if (sym->formal_ns && sym->formal_ns != gfc_current_ns
      && !sym->attr.contained)
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
	  sym->formal_ns->refs++;
	}
    }

  /* Check threadprivate restrictions.  */
  if (sym->attr.threadprivate && !sym->attr.save && !sym->ns->save_all
      && (!sym->attr.in_common
	  && sym->module == NULL
	  && (sym->ns->proc_name == NULL
	      || sym->ns->proc_name->attr.flavor != FL_MODULE)))
    gfc_error ("Threadprivate at %L isn't SAVEd", &sym->declared_at);

  /* If we have come this far we can apply default-initializers, as
     described in 14.7.5, to those variables that have not already
     been assigned one.  */
  if (sym->ts.type == BT_DERIVED
      && sym->attr.referenced
      && sym->ns == gfc_current_ns
      && !sym->value
      && !sym->attr.allocatable
      && !sym->attr.alloc_comp)
    {
      symbol_attribute *a = &sym->attr;

      if ((!a->save && !a->dummy && !a->pointer
	   && !a->in_common && !a->use_assoc
	   && !(a->function && sym != sym->result))
	  || (a->dummy && a->intent == INTENT_OUT))
	apply_default_init (sym);
    }

  /* If this symbol has a type-spec, check it.  */
  if (sym->attr.flavor == FL_VARIABLE || sym->attr.flavor == FL_PARAMETER
      || (sym->attr.flavor == FL_PROCEDURE && sym->attr.function))
    if (resolve_typespec_used (&sym->ts, &sym->declared_at, sym->name)
	  == FAILURE)
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

static gfc_try
next_data_value (void)
{
  while (mpz_cmp_ui (values.left, 0) == 0)
    {
      if (!gfc_is_constant_expr (values.vnode->expr))
	gfc_error ("non-constant DATA value at %L",
		   &values.vnode->expr->where);

      if (values.vnode->next == NULL)
	return FAILURE;

      values.vnode = values.vnode->next;
      mpz_set (values.left, values.vnode->repeat);
    }

  return SUCCESS;
}


static gfc_try
check_data_variable (gfc_data_variable *var, locus *where)
{
  gfc_expr *e;
  mpz_t size;
  mpz_t offset;
  gfc_try t;
  ar_type mark = AR_UNKNOWN;
  int i;
  mpz_t section_index[GFC_MAX_DIMENSIONS];
  gfc_ref *ref;
  gfc_array_ref *ar;
  gfc_symbol *sym;
  int has_pointer;

  if (gfc_resolve_expr (var->expr) == FAILURE)
    return FAILURE;

  ar = NULL;
  mpz_init_set_si (offset, 0);
  e = var->expr;

  if (e->expr_type != EXPR_VARIABLE)
    gfc_internal_error ("check_data_variable(): Bad expression");

  sym = e->symtree->n.sym;

  if (sym->ns->is_block_data && !sym->attr.in_common)
    {
      gfc_error ("BLOCK DATA element '%s' at %L must be in COMMON",
		 sym->name, &sym->declared_at);
    }

  if (e->ref == NULL && sym->as)
    {
      gfc_error ("DATA array '%s' at %L must be specified in a previous"
		 " declaration", sym->name, where);
      return FAILURE;
    }

  has_pointer = sym->attr.pointer;

  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT && ref->u.c.component->attr.pointer)
	has_pointer = 1;

      if (has_pointer
	    && ref->type == REF_ARRAY
	    && ref->u.ar.type != AR_FULL)
	  {
	    gfc_error ("DATA element '%s' at %L is a pointer and so must "
			"be a full array", sym->name, where);
	    return FAILURE;
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

      if (gfc_array_size (e, &size) == FAILURE)
	{
	  gfc_error ("Nonconstant array section at %L in DATA statement",
		     &e->where);
	  mpz_clear (offset);
	  return FAILURE;
	}
    }

  t = SUCCESS;

  while (mpz_cmp_ui (size, 0) > 0)
    {
      if (next_data_value () == FAILURE)
	{
	  gfc_error ("DATA statement at %L has more variables than values",
		     where);
	  t = FAILURE;
	  break;
	}

      t = gfc_check_assign (var->expr, values.vnode->expr, 0);
      if (t == FAILURE)
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

	  gfc_assign_data_value_range (var->expr, values.vnode->expr,
				       offset, range);

	  mpz_add (offset, offset, range);
	  mpz_clear (range);
	}

      /* Assign initial value to symbol.  */
      else
	{
	  mpz_sub_ui (values.left, values.left, 1);
	  mpz_sub_ui (size, size, 1);

	  t = gfc_assign_data_value (var->expr, values.vnode->expr, offset);
	  if (t == FAILURE)
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


static gfc_try traverse_data_var (gfc_data_variable *, locus *);

/* Iterate over a list of elements in a DATA statement.  */

static gfc_try
traverse_data_list (gfc_data_variable *var, locus *where)
{
  mpz_t trip;
  iterator_stack frame;
  gfc_expr *e, *start, *end, *step;
  gfc_try retval = SUCCESS;

  mpz_init (frame.value);

  start = gfc_copy_expr (var->iter.start);
  end = gfc_copy_expr (var->iter.end);
  step = gfc_copy_expr (var->iter.step);

  if (gfc_simplify_expr (start, 1) == FAILURE
      || start->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("iterator start at %L does not simplify", &start->where);
      retval = FAILURE;
      goto cleanup;
    }
  if (gfc_simplify_expr (end, 1) == FAILURE
      || end->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("iterator end at %L does not simplify", &end->where);
      retval = FAILURE;
      goto cleanup;
    }
  if (gfc_simplify_expr (step, 1) == FAILURE
      || step->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("iterator step at %L does not simplify", &step->where);
      retval = FAILURE;
      goto cleanup;
    }

  mpz_init_set (trip, end->value.integer);
  mpz_sub (trip, trip, start->value.integer);
  mpz_add (trip, trip, step->value.integer);

  mpz_div (trip, trip, step->value.integer);

  mpz_set (frame.value, start->value.integer);

  frame.prev = iter_stack;
  frame.variable = var->iter.var->symtree;
  iter_stack = &frame;

  while (mpz_cmp_ui (trip, 0) > 0)
    {
      if (traverse_data_var (var->list, where) == FAILURE)
	{
	  mpz_clear (trip);
	  retval = FAILURE;
	  goto cleanup;
	}

      e = gfc_copy_expr (var->expr);
      if (gfc_simplify_expr (e, 1) == FAILURE)
	{
	  gfc_free_expr (e);
	  mpz_clear (trip);
	  retval = FAILURE;
	  goto cleanup;
	}

      mpz_add (frame.value, frame.value, step->value.integer);

      mpz_sub_ui (trip, trip, 1);
    }

  mpz_clear (trip);
cleanup:
  mpz_clear (frame.value);

  gfc_free_expr (start);
  gfc_free_expr (end);
  gfc_free_expr (step);

  iter_stack = frame.prev;
  return retval;
}


/* Type resolve variables in the variable list of a DATA statement.  */

static gfc_try
traverse_data_var (gfc_data_variable *var, locus *where)
{
  gfc_try t;

  for (; var; var = var->next)
    {
      if (var->expr == NULL)
	t = traverse_data_list (var, where);
      else
	t = check_data_variable (var, where);

      if (t == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


/* Resolve the expressions and iterators associated with a data statement.
   This is separate from the assignment checking because data lists should
   only be resolved once.  */

static gfc_try
resolve_data_variables (gfc_data_variable *d)
{
  for (; d; d = d->next)
    {
      if (d->list == NULL)
	{
	  if (gfc_resolve_expr (d->expr) == FAILURE)
	    return FAILURE;
	}
      else
	{
	  if (gfc_resolve_iterator (&d->iter, false) == FAILURE)
	    return FAILURE;

	  if (resolve_data_variables (d->list) == FAILURE)
	    return FAILURE;
	}
    }

  return SUCCESS;
}


/* Resolve a single DATA statement.  We implement this by storing a pointer to
   the value list into static variables, and then recursively traversing the
   variables list, expanding iterators and such.  */

static void
resolve_data (gfc_data *d)
{

  if (resolve_data_variables (d->var) == FAILURE)
    return;

  values.vnode = d->value;
  if (d->value == NULL)
    mpz_set_ui (values.left, 0);
  else
    mpz_set (values.left, d->value->repeat);

  if (traverse_data_var (d->var, &d->where) == FAILURE)
    return;

  /* At this point, we better not have any values left.  */

  if (next_data_value () == SUCCESS)
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

  if (sym->attr.use_assoc || sym->attr.in_common)
    return 1;

  if (sym->ns != gfc_current_ns)
    return !sym->attr.function;

  proc = sym->ns->proc_name;
  if (sym->attr.dummy && gfc_pure (proc)
	&& ((proc->attr.subroutine && sym->attr.intent == INTENT_IN)
		||
	     proc->attr.function))
    return 1;

  /* TODO: Sort out what can be storage associated, if anything, and include
     it here.  In principle equivalences should be scanned but it does not
     seem to be possible to storage associate an impure variable this way.  */
  return 0;
}


/* Test whether a symbol is pure or not.  For a NULL pointer, checks the
   symbol of the current procedure.  */

int
gfc_pure (gfc_symbol *sym)
{
  symbol_attribute attr;

  if (sym == NULL)
    sym = gfc_current_ns->proc_name;
  if (sym == NULL)
    return 0;

  attr = sym->attr;

  return attr.flavor == FL_PROCEDURE && (attr.pure || attr.elemental);
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
      gfc_warning ("Label %d at %L defined but not used", label->value,
		   &label->where);
      break;

    case ST_LABEL_BAD_TARGET:
      gfc_warning ("Label %d at %L defined but cannot be used",
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

      if (ts.derived->components == NULL)
	return SEQ_NONDEFAULT;

      result = sequence_type (ts.derived->components->ts);
      for (c = ts.derived->components->next; c; c = c->next)
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

static gfc_try
resolve_equivalence_derived (gfc_symbol *derived, gfc_symbol *sym, gfc_expr *e)
{
  gfc_symbol *d;
  gfc_component *c = derived->components;

  if (!derived)
    return SUCCESS;

  /* Shall not be an object of nonsequence derived type.  */
  if (!derived->attr.sequence)
    {
      gfc_error ("Derived type variable '%s' at %L must have SEQUENCE "
		 "attribute to be an EQUIVALENCE object", sym->name,
		 &e->where);
      return FAILURE;
    }

  /* Shall not have allocatable components.  */
  if (derived->attr.alloc_comp)
    {
      gfc_error ("Derived type variable '%s' at %L cannot have ALLOCATABLE "
		 "components to be an EQUIVALENCE object",sym->name,
		 &e->where);
      return FAILURE;
    }

  if (sym->attr.in_common && has_default_initializer (sym->ts.derived))
    {
      gfc_error ("Derived type variable '%s' at %L with default "
		 "initialization cannot be in EQUIVALENCE with a variable "
		 "in COMMON", sym->name, &e->where);
      return FAILURE;
    }

  for (; c ; c = c->next)
    {
      d = c->ts.derived;
      if (d
	  && (resolve_equivalence_derived (c->ts.derived, sym, e) == FAILURE))
	return FAILURE;

      /* Shall not be an object of sequence derived type containing a pointer
	 in the structure.  */
      if (c->attr.pointer)
	{
	  gfc_error ("Derived type variable '%s' at %L with pointer "
		     "component(s) cannot be an EQUIVALENCE object",
		     sym->name, &e->where);
	  return FAILURE;
	}
    }
  return SUCCESS;
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
  gfc_symbol *derived;
  gfc_symbol *first_sym;
  gfc_expr *e;
  gfc_ref *r;
  locus *last_where = NULL;
  seq_type eq_type, last_eq_type;
  gfc_typespec *last_ts;
  int object, cnt_protected;
  const char *value_name;
  const char *msg;

  value_name = NULL;
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
		    start = gfc_int_expr (1);
		  ref->u.ss.start = start;
		  if (end == NULL && e->ts.cl)
		    end = gfc_copy_expr (e->ts.cl->length);
		  ref->u.ss.end = end;
		  ref->u.ss.length = e->ts.cl;
		  e->ts.cl = NULL;
		}
	      ref = ref->next;
	      gfc_free (mem);
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

      if (gfc_resolve_expr (e) == FAILURE)
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
	  gfc_error ("Common block member '%s' at %L cannot be an EQUIVALENCE "
		     "object in the pure procedure '%s'",
		     sym->name, &e->where, sym->ns->proc_name->name);
	  break;
	}

      /* Shall not be a named constant.  */
      if (e->expr_type == EXPR_CONSTANT)
	{
	  gfc_error ("Named constant '%s' at %L cannot be an EQUIVALENCE "
		     "object", sym->name, &e->where);
	  continue;
	}

      derived = e->ts.derived;
      if (derived && resolve_equivalence_derived (derived, sym, e) == FAILURE)
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
	   && gfc_notify_std (GFC_STD_GNU, msg, first_sym->name, last_where)
	      == FAILURE)
	  || (eq_type == SEQ_MIXED
	      && gfc_notify_std (GFC_STD_GNU, msg, sym->name,
				 &e->where) == FAILURE))
	continue;

      msg = "Non-default type object or sequence %s in EQUIVALENCE "
	    "statement at %L with objects of different type";
      if ((object ==2
	   && last_eq_type == SEQ_NONDEFAULT
	   && gfc_notify_std (GFC_STD_GNU, msg, first_sym->name,
			      last_where) == FAILURE)
	  || (eq_type == SEQ_NONDEFAULT
	      && gfc_notify_std (GFC_STD_GNU, msg, sym->name,
				 &e->where) == FAILURE))
	continue;

      msg ="Non-CHARACTER object '%s' in default CHARACTER "
	   "EQUIVALENCE statement at %L";
      if (last_eq_type == SEQ_CHARACTER
	  && eq_type != SEQ_CHARACTER
	  && gfc_notify_std (GFC_STD_GNU, msg, sym->name,
			     &e->where) == FAILURE)
		continue;

      msg ="Non-NUMERIC object '%s' in default NUMERIC "
	   "EQUIVALENCE statement at %L";
      if (last_eq_type == SEQ_NUMERIC
	  && eq_type != SEQ_NUMERIC
	  && gfc_notify_std (GFC_STD_GNU, msg, sym->name,
			     &e->where) == FAILURE)
		continue;

  identical_types:
      last_ts =&sym->ts;
      last_where = &e->where;

      if (!e->ref)
	continue;

      /* Shall not be an automatic array.  */
      if (e->ref->type == REF_ARRAY
	  && gfc_resolve_array_spec (e->ref->u.ar.as, 1) == FAILURE)
	{
	  gfc_error ("Array '%s' at %L with non-constant bounds cannot be "
		     "an EQUIVALENCE object", sym->name, &e->where);
	  continue;
	}

      r = e->ref;
      while (r)
	{
	  /* Shall not be a structure component.  */
	  if (r->type == REF_COMPONENT)
	    {
	      gfc_error ("Structure component '%s' at %L cannot be an "
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
      && gfc_set_default_type (sym, 0, NULL) == FAILURE
      && !sym->attr.untyped)
    {
      gfc_error ("Function '%s' at %L has no IMPLICIT type",
		 sym->name, &sym->declared_at);
      sym->attr.untyped = 1;
    }

  if (sym->ts.type == BT_DERIVED && !sym->ts.derived->attr.use_assoc
      && !sym->attr.contained
      && !gfc_check_access (sym->ts.derived->attr.access,
			    sym->ts.derived->ns->default_access)
      && gfc_check_access (sym->attr.access, sym->ns->default_access))
    {
      gfc_notify_std (GFC_STD_F2003, "Fortran 2003: PUBLIC function '%s' at "
		      "%L of PRIVATE type '%s'", sym->name,
		      &sym->declared_at, sym->ts.derived->name);
    }

    if (ns->entries)
    for (el = ns->entries->next; el; el = el->next)
      {
	if (el->sym->result == el->sym
	    && el->sym->ts.type == BT_UNKNOWN
	    && gfc_set_default_type (el->sym, 0, NULL) == FAILURE
	    && !el->sym->attr.untyped)
	  {
	    gfc_error ("ENTRY '%s' at %L has no IMPLICIT type",
		       el->sym->name, &el->sym->declared_at);
	    el->sym->attr.untyped = 1;
	  }
      }
}

/* 12.3.2.1.1 Defined operators.  */

static void
gfc_resolve_uops (gfc_symtree *symtree)
{
  gfc_interface *itr;
  gfc_symbol *sym;
  gfc_formal_arglist *formal;

  if (symtree == NULL)
    return;

  gfc_resolve_uops (symtree->left);
  gfc_resolve_uops (symtree->right);

  for (itr = symtree->n.uop->op; itr; itr = itr->next)
    {
      sym = itr->sym;
      if (!sym->attr.function)
	gfc_error ("User operator procedure '%s' at %L must be a FUNCTION",
		   sym->name, &sym->declared_at);

      if (sym->ts.type == BT_CHARACTER
	  && !(sym->ts.cl && sym->ts.cl->length)
	  && !(sym->result && sym->result->ts.cl
	       && sym->result->ts.cl->length))
	gfc_error ("User operator procedure '%s' at %L cannot be assumed "
		   "character length", sym->name, &sym->declared_at);

      formal = sym->formal;
      if (!formal || !formal->sym)
	{
	  gfc_error ("User operator procedure '%s' at %L must have at least "
		     "one argument", sym->name, &sym->declared_at);
	  continue;
	}

      if (formal->sym->attr.intent != INTENT_IN)
	gfc_error ("First argument of operator interface at %L must be "
		   "INTENT(IN)", &sym->declared_at);

      if (formal->sym->attr.optional)
	gfc_error ("First argument of operator interface at %L cannot be "
		   "optional", &sym->declared_at);

      formal = formal->next;
      if (!formal || !formal->sym)
	continue;

      if (formal->sym->attr.intent != INTENT_IN)
	gfc_error ("Second argument of operator interface at %L must be "
		   "INTENT(IN)", &sym->declared_at);

      if (formal->sym->attr.optional)
	gfc_error ("Second argument of operator interface at %L cannot be "
		   "optional", &sym->declared_at);

      if (formal->next)
	gfc_error ("Operator interface at %L must have, at most, two "
		   "arguments", &sym->declared_at);
    }
}


/* Examine all of the expressions associated with a program unit,
   assign types to all intermediate expressions, make sure that all
   assignments are to compatible types and figure out which names
   refer to which functions or subroutines.  It doesn't check code
   block, which is handled by resolve_code.  */

static void
resolve_types (gfc_namespace *ns)
{
  gfc_namespace *n;
  gfc_charlen *cl;
  gfc_data *d;
  gfc_equiv *eq;
  gfc_namespace* old_ns = gfc_current_ns;

  /* Check that all IMPLICIT types are ok.  */
  if (!ns->seen_implicit_none)
    {
      unsigned letter;
      for (letter = 0; letter != GFC_LETTERS; ++letter)
	if (ns->set_flag[letter]
	    && resolve_typespec_used (&ns->default_type[letter],
				      &ns->implicit_loc[letter],
				      NULL) == FAILURE)
	  return;
    }

  gfc_current_ns = ns;

  resolve_entries (ns);

  resolve_common_vars (ns->blank_common.head, false);
  resolve_common_blocks (ns->common_root);

  resolve_contained_functions (ns);

  gfc_traverse_ns (ns, resolve_bind_c_derived_types);

  for (cl = ns->cl_list; cl; cl = cl->next)
    resolve_charlen (cl);

  gfc_traverse_ns (ns, resolve_symbol);

  resolve_fntype (ns);

  for (n = ns->contained; n; n = n->sibling)
    {
      if (gfc_pure (ns->proc_name) && !gfc_pure (n->proc_name))
	gfc_error ("Contained procedure '%s' at %L of a PURE procedure must "
		   "also be PURE", n->proc_name->name,
		   &n->proc_name->declared_at);

      resolve_types (n);
    }

  forall_flag = 0;
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

  if (ns->common_root != NULL)
    gfc_traverse_symtree (ns->common_root, resolve_bind_c_comms);

  for (eq = ns->equiv; eq; eq = eq->next)
    resolve_equivalence (eq);

  /* Warn about unused labels.  */
  if (warn_unused_label)
    warn_unused_fortran_label (ns->st_labels);

  gfc_resolve_uops (ns->uop_root);

  gfc_current_ns = old_ns;
}


/* Call resolve_code recursively.  */

static void
resolve_codes (gfc_namespace *ns)
{
  gfc_namespace *n;
  bitmap_obstack old_obstack;

  for (n = ns->contained; n; n = n->sibling)
    resolve_codes (n);

  gfc_current_ns = ns;
  cs_base = NULL;
  /* Set to an out of range value.  */
  current_entry_id = -1;

  old_obstack = labels_obstack;
  bitmap_obstack_initialize (&labels_obstack);

  resolve_code (ns->code, ns);

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

  if (ns->resolved)
    return;

  old_ns = gfc_current_ns;

  resolve_types (ns);
  resolve_codes (ns);

  gfc_current_ns = old_ns;
  ns->resolved = 1;
}
