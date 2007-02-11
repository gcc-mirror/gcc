/* Perform type resolution on the various stuctures.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, 
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
Software Foundation, 51 Franklin Street, Fifth Floor,Boston, MA
02110-1301, USA.  */


#include "config.h"
#include "system.h"
#include "flags.h"
#include "gfortran.h"
#include "arith.h"  /* For gfc_compare_expr().  */

/* Types used in equivalence statements.  */

typedef enum seq_type
{
  SEQ_NONDEFAULT, SEQ_NUMERIC, SEQ_CHARACTER, SEQ_MIXED
}
seq_type;

/* Stack to push the current if we descend into a block during
   resolution.  See resolve_branch() and resolve_code().  */

typedef struct code_stack
{
  struct gfc_code *head, *current;
  struct code_stack *prev;
}
code_stack;

static code_stack *cs_base = NULL;


/* Nonzero if we're inside a FORALL block */

static int forall_flag;

/* Nonzero if we are processing a formal arglist. The corresponding function
   resets the flag each time that it is read.  */
static int formal_arg_flag = 0;

/* True if we are resolving a specification expression.  */
static int specification_expr = 0;

/* The id of the last entry seen.  */
static int current_entry_id;

int
gfc_is_formal_arg (void)
{
  return formal_arg_flag;
}

/* Resolve types of formal argument lists.  These have to be done early so that
   the formal argument lists of module procedures can be copied to the
   containing module before the individual procedures are resolved
   individually.  We also resolve argument lists of procedures in interface
   blocks because they are self-contained scoping units.

   Since a dummy argument cannot be a non-dummy procedure, the only
   resort left for untyped names are the IMPLICIT types.  */

static void
resolve_formal_arglist (gfc_symbol * proc)
{
  gfc_formal_arglist *f;
  gfc_symbol *sym;
  int i;

  /* TODO: Procedures whose return character length parameter is not constant
     or assumed must also have explicit interfaces.  */
  if (proc->result != NULL)
    sym = proc->result;
  else
    sym = proc;

  if (gfc_elemental (proc)
      || sym->attr.pointer || sym->attr.allocatable
      || (sym->as && sym->as->rank > 0))
    proc->attr.always_explicit = 1;

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
	      gfc_error
		("Dummy procedure '%s' of PURE procedure at %L must also "
		 "be PURE", sym->name, &sym->declared_at);
	      continue;
	    }

	  if (gfc_elemental (proc))
	    {
	      gfc_error
		("Dummy procedure at %L not allowed in ELEMENTAL procedure",
		 &sym->declared_at);
	      continue;
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
        proc->attr.always_explicit = 1;

      /* If the flavor is unknown at this point, it has to be a variable.
         A procedure specification would have already set the type.  */

      if (sym->attr.flavor == FL_UNKNOWN)
	gfc_add_flavor (&sym->attr, FL_VARIABLE, sym->name, &sym->declared_at);

      if (gfc_pure (proc))
	{
	  if (proc->attr.function && !sym->attr.pointer
              && sym->attr.flavor != FL_PROCEDURE
	      && sym->attr.intent != INTENT_IN)

	    gfc_error ("Argument '%s' of pure function '%s' at %L must be "
		       "INTENT(IN)", sym->name, proc->name,
		       &sym->declared_at);

	  if (proc->attr.subroutine && !sym->attr.pointer
	      && sym->attr.intent == INTENT_UNKNOWN)

	    gfc_error
	      ("Argument '%s' of pure subroutine '%s' at %L must have "
	       "its INTENT specified", sym->name, proc->name,
	       &sym->declared_at);
	}


      if (gfc_elemental (proc))
	{
	  if (sym->as != NULL)
	    {
	      gfc_error
		("Argument '%s' of elemental procedure at %L must be scalar",
		 sym->name, &sym->declared_at);
	      continue;
	    }

	  if (sym->attr.pointer)
	    {
	      gfc_error
		("Argument '%s' of elemental procedure at %L cannot have "
		 "the POINTER attribute", sym->name, &sym->declared_at);
	      continue;
	    }
	}

      /* Each dummy shall be specified to be scalar.  */
      if (proc->attr.proc == PROC_ST_FUNCTION)
        {
          if (sym->as != NULL)
            {
              gfc_error
                ("Argument '%s' of statement function at %L must be scalar",
                 sym->name, &sym->declared_at);
              continue;
            }

          if (sym->ts.type == BT_CHARACTER)
            {
              gfc_charlen *cl = sym->ts.cl;
              if (!cl || !cl->length || cl->length->expr_type != EXPR_CONSTANT)
                {
                  gfc_error
                    ("Character-valued argument '%s' of statement function at "
                     "%L must have constant length",
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
find_arglists (gfc_symbol * sym)
{

  if (sym->attr.if_source == IFSRC_UNKNOWN || sym->ns != gfc_current_ns)
    return;

  resolve_formal_arglist (sym);
}


/* Given a namespace, resolve all formal argument lists within the namespace.
 */

static void
resolve_formal_arglists (gfc_namespace * ns)
{

  if (ns == NULL)
    return;

  gfc_traverse_ns (ns, find_arglists);
}


static void
resolve_contained_fntype (gfc_symbol * sym, gfc_namespace * ns)
{
  try t;
  
  /* If this namespace is not a function, ignore it.  */
  if (! sym
      || !(sym->attr.function
	   || sym->attr.flavor == FL_VARIABLE))
    return;

  /* Try to find out of what the return type is.  */
  if (sym->result != NULL)
    sym = sym->result;

  if (sym->ts.type == BT_UNKNOWN)
    {
      t = gfc_set_default_type (sym, 0, ns);

      if (t == FAILURE && !sym->attr.untyped)
	{
	  gfc_error ("Contained function '%s' at %L has no IMPLICIT type",
		     sym->name, &sym->declared_at); /* FIXME */
	  sym->attr.untyped = 1;
	}
    }

  if (sym->ts.type == BT_CHARACTER)
    {
      gfc_charlen *cl = sym->ts.cl;
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
      /* See if ths arg is already in the formal argument list.  */
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


/* Resolve alternate entry points.  If a symbol has multiple entry points we
   create a new master symbol for the main routine, and turn the existing
   symbol into an entry point.  */

static void
resolve_entries (gfc_namespace * ns)
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
	&& ns->parent
	&& ns->parent->proc_name->attr.flavor == FL_MODULE)
    el->sym->ns = ns;

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
	fts = gfc_get_default_type (ns->entries->sym->result, NULL);
      for (el = ns->entries->next; el; el = el->next)
	{
	  ts = &el->sym->result->ts;
	  as = el->sym->as;
	  as = as ? as : el->sym->result->as;
	  if (ts->type == BT_UNKNOWN)
	    ts = gfc_get_default_type (el->sym->result, NULL);

	  if (! gfc_compare_types (ts, fts)
	      || (el->sym->result->attr.dimension
		  != ns->entries->sym->result->attr.dimension)
	      || (el->sym->result->attr.pointer
		  != ns->entries->sym->result->attr.pointer))
	    break;

	  else if (as && fas && gfc_compare_array_spec (as, fas) == 0)
	    gfc_error ("Procedure %s at %L has entries with mismatched "
		       "array specifications", ns->entries->sym->name,
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
		  gfc_error
		  ("FUNCTION result %s can't be an array in FUNCTION %s at %L",
		   sym->name, ns->entries->sym->name, &sym->declared_at);
	        else
		  gfc_error
		    ("ENTRY result %s can't be an array in FUNCTION %s at %L",
		     sym->name, ns->entries->sym->name, &sym->declared_at);
	      }
	      else if (sym->attr.pointer)
	      {
		if (el == ns->entries)
		  gfc_error
		  ("FUNCTION result %s can't be a POINTER in FUNCTION %s at %L",
		   sym->name, ns->entries->sym->name, &sym->declared_at);
	        else
		  gfc_error
		    ("ENTRY result %s can't be a POINTER in FUNCTION %s at %L",
		     sym->name, ns->entries->sym->name, &sym->declared_at);
	      }
	      else
		{
		  ts = &sym->ts;
		  if (ts->type == BT_UNKNOWN)
		    ts = gfc_get_default_type (sym, NULL);
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
		      gfc_error
			("FUNCTION result %s can't be of type %s in FUNCTION %s at %L",
			 sym->name, gfc_typename (ts), ns->entries->sym->name,
			 &sym->declared_at);
		    else
		      gfc_error
			("ENTRY result %s can't be of type %s in FUNCTION %s at %L",
			 sym->name, gfc_typename (ts), ns->entries->sym->name,
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

  /* Use the master function for the function body.  */
  ns->proc_name = proc;

  /* Finalize the new symbols.  */
  gfc_commit_symbols ();

  /* Restore the original namespace.  */
  gfc_current_ns = old_ns;
}


/* Resolve contained function types.  Because contained functions can call one
   another, they have to be worked out before any of the contained procedures
   can be resolved.

   The good news is that if a function doesn't already have a type, the only
   way it can get one is through an IMPLICIT type or a RESULT variable, because
   by definition contained functions are contained namespace they're contained
   in, not in a sibling or parent namespace.  */

static void
resolve_contained_functions (gfc_namespace * ns)
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

static try
resolve_structure_cons (gfc_expr * expr)
{
  gfc_constructor *cons;
  gfc_component *comp;
  try t;
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

  for (; comp; comp = comp->next, cons = cons->next)
    {
      if (! cons->expr)
	continue;

      if (gfc_resolve_expr (cons->expr) == FAILURE)
	{
	  t = FAILURE;
	  continue;
	}

      /* If we don't have the right type, try to convert it.  */

      if (!gfc_compare_types (&cons->expr->ts, &comp->ts))
	{
	  t = FAILURE;
	  if (comp->pointer && cons->expr->ts.type != BT_UNKNOWN)
	    gfc_error ("The element in the derived type constructor at %L, "
		       "for pointer component '%s', is %s but should be %s",
		       &cons->expr->where, comp->name,
		       gfc_basic_typename (cons->expr->ts.type),
		       gfc_basic_typename (comp->ts.type));
	  else
	    t = gfc_convert_type (cons->expr, &comp->ts, 1);
	}

      if (!comp->pointer || cons->expr->expr_type == EXPR_NULL)
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
was_declared (gfc_symbol * sym)
{
  symbol_attribute a;

  a = sym->attr;

  if (!a.implicit_type && sym->ts.type != BT_UNKNOWN)
    return 1;

  if (a.allocatable || a.dimension || a.dummy || a.external || a.intrinsic
      || a.optional || a.pointer || a.save || a.target
      || a.access != ACCESS_UNKNOWN || a.intent != INTENT_UNKNOWN)
    return 1;

  return 0;
}


/* Determine if a symbol is generic or not.  */

static int
generic_sym (gfc_symbol * sym)
{
  gfc_symbol *s;

  if (sym->attr.generic ||
      (sym->attr.intrinsic && gfc_generic_intrinsic (sym->name)))
    return 1;

  if (was_declared (sym) || sym->ns->parent == NULL)
    return 0;

  gfc_find_symbol (sym->name, sym->ns->parent, 1, &s);

  return (s == NULL) ? 0 : generic_sym (s);
}


/* Determine if a symbol is specific or not.  */

static int
specific_sym (gfc_symbol * sym)
{
  gfc_symbol *s;

  if (sym->attr.if_source == IFSRC_IFBODY
      || sym->attr.proc == PROC_MODULE
      || sym->attr.proc == PROC_INTERNAL
      || sym->attr.proc == PROC_ST_FUNCTION
      || (sym->attr.intrinsic &&
	  gfc_specific_intrinsic (sym->name))
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
procedure_kind (gfc_symbol * sym)
{

  if (generic_sym (sym))
    return PTYPE_GENERIC;

  if (specific_sym (sym))
    return PTYPE_SPECIFIC;

  return PTYPE_UNKNOWN;
}

/* Check references to assumed size arrays.  The flag need_full_assumed_size
   is non-zero when matching actual arguments.  */

static int need_full_assumed_size = 0;

static bool
check_assumed_size_reference (gfc_symbol * sym, gfc_expr * e)
{
  gfc_ref * ref;
  int dim;
  int last = 1;

  if (need_full_assumed_size
	|| !(sym->as && sym->as->type == AS_ASSUMED_SIZE))
      return false;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY)
      for (dim = 0; dim < ref->u.ar.as->rank; dim++)
	last = (ref->u.ar.end[dim] == NULL) && (ref->u.ar.type == DIMEN_ELEMENT);

  if (last)
    {
      gfc_error ("The upper bound in the last dimension must "
		 "appear in the reference to the assumed size "
		 "array '%s' at %L.", sym->name, &e->where);
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
      if (e->symtree
	    && check_assumed_size_reference (e->symtree->n.sym, e))
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


/* Resolve an actual argument list.  Most of the time, this is just
   resolving the expressions in the list.
   The exception is that we sometimes have to decide whether arguments
   that look like procedure arguments are really simple variable
   references.  */

static try
resolve_actual_arglist (gfc_actual_arglist * arg)
{
  gfc_symbol *sym;
  gfc_symtree *parent_st;
  gfc_expr *e;

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

      if (e->ts.type != BT_PROCEDURE)
	{
	  if (gfc_resolve_expr (e) != SUCCESS)
	    return FAILURE;
	  continue;
	}

      /* See if the expression node should really be a variable
	 reference.  */

      sym = e->symtree->n.sym;

      if (sym->attr.flavor == FL_PROCEDURE
	  || sym->attr.intrinsic
	  || sym->attr.external)
	{

	  /* If a procedure is not already determined to be something else
	     check if it is intrinsic.  */
	  if (!sym->attr.intrinsic
		&& !(sym->attr.external || sym->attr.use_assoc
		       || sym->attr.if_source == IFSRC_IFBODY)
		&& gfc_intrinsic_name (sym->name, sym->attr.subroutine))
	    sym->attr.intrinsic = 1;

	  if (sym->attr.proc == PROC_ST_FUNCTION)
	    {
	      gfc_error ("Statement function '%s' at %L is not allowed as an "
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

	  if (sym->attr.generic)
	    {
	      gfc_error ("GENERIC non-INTRINSIC procedure '%s' is not "
		         "allowed as an actual argument at %L", sym->name,
			 &e->where);
	    }

	  /* If the symbol is the function that names the current (or
	     parent) scope, then we really have a variable reference.  */

	  if (sym->attr.function && sym->result == sym
	      && (sym->ns->proc_name == sym
		  || (sym->ns->parent != NULL
		      && sym->ns->parent->proc_name == sym)))
	    goto got_variable;

	  continue;
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
	  continue;
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
    }

  return SUCCESS;
}


/* Do the checks of the actual argument list that are specific to elemental
   procedures.  If called with c == NULL, we have a function, otherwise if
   expr == NULL, we have a subroutine.  */
static try
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
  else if (c && c->ext.actual != NULL
	     && c->symtree->n.sym->attr.elemental)
    {
      arg0 = c->ext.actual;
      esym = c->symtree->n.sym;
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
	    && !(isym && isym->generic_id == GFC_ISYM_CONVERSION)) 
	{
	  gfc_warning ("'%s' at %L is an array and OPTIONAL; IF IT IS "
		       "MISSING, it cannot be the actual argument of an "
		       "ELEMENTAL procedure unless there is a non-optional"
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

      if (expr)
	continue;

      /* Elemental subroutine array actual arguments must conform.  */
      if (e != NULL)
	{
	  if (gfc_check_conformance ("elemental subroutine", arg->expr, e)
		== FAILURE)
	    return FAILURE;
	}
      else
	e = arg->expr;
    }

  return SUCCESS;
}


/* This function does the checking of references to global procedures
   as defined in sections 18.1 and 14.1, respectively, of the Fortran
   77 and 95 standards.  It checks for a gsymbol for the name, making
   one if it does not already exist.  If it already exists, then the
   reference being resolved must correspond to the type of gsymbol.
   Otherwise, the new symbol is equipped with the attributes of the 
   reference.  The corresponding code that is called in creating
   global entities is parse.c.  */

static void
resolve_global_procedure (gfc_symbol *sym, locus *where, int sub)
{
  gfc_gsymbol * gsym;
  uint type;

  type = sub ? GSYM_SUBROUTINE : GSYM_FUNCTION;

  gsym = gfc_get_gsymbol (sym->name);

  if ((gsym->type != GSYM_UNKNOWN && gsym->type != type))
    global_used (gsym, where);

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
resolve_generic_f0 (gfc_expr * expr, gfc_symbol * sym)
{
  gfc_symbol *s;

  if (sym->attr.generic)
    {
      s =
	gfc_search_interface (sym->generic, 0, &expr->value.function.actual);
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

	  return MATCH_YES;
	}

      /* TODO: Need to search for elemental references in generic interface */
    }

  if (sym->attr.intrinsic)
    return gfc_intrinsic_func_interface (expr, 0);

  return MATCH_NO;
}


static try
resolve_generic_f (gfc_expr * expr)
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

  /* Last ditch attempt.  */

  if (!gfc_generic_intrinsic (expr->symtree->n.sym->name))
    {
      gfc_error ("There is no specific function for the generic '%s' at %L",
		 expr->symtree->n.sym->name, &expr->where);
      return FAILURE;
    }

  m = gfc_intrinsic_func_interface (expr, 0);
  if (m == MATCH_YES)
    return SUCCESS;
  if (m == MATCH_NO)
    gfc_error
      ("Generic function '%s' at %L is not consistent with a specific "
       "intrinsic interface", expr->symtree->n.sym->name, &expr->where);

  return FAILURE;
}


/* Resolve a function call known to be specific.  */

static match
resolve_specific_f0 (gfc_symbol * sym, gfc_expr * expr)
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
	gfc_error
	  ("Function '%s' at %L is INTRINSIC but is not compatible with "
	   "an intrinsic", sym->name, &expr->where);

      return MATCH_ERROR;
    }

  return MATCH_NO;

found:
  gfc_procedure_use (sym, &expr->value.function.actual, &expr->where);

  expr->ts = sym->ts;
  expr->value.function.name = sym->name;
  expr->value.function.esym = sym;
  if (sym->as != NULL)
    expr->rank = sym->as->rank;

  return MATCH_YES;
}


static try
resolve_specific_f (gfc_expr * expr)
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

static try
resolve_unknown_f (gfc_expr * expr)
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

  if (gfc_intrinsic_name (sym->name, 0))
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
      ts = gfc_get_default_type (sym, sym->ns);

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


/* Figure out if a function reference is pure or not.  Also set the name
   of the function for a potential error message.  Return nonzero if the
   function is PURE, zero if not.  */

static int
pure_function (gfc_expr * e, const char **name)
{
  int pure;

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


/* Resolve a function call, which means resolving the arguments, then figuring
   out which entity the name refers to.  */
/* TODO: Check procedure arguments so that an INTENT(IN) isn't passed
   to INTENT(OUT) or INTENT(INOUT).  */

static try
resolve_function (gfc_expr * expr)
{
  gfc_actual_arglist *arg;
  gfc_symbol * sym;
  const char *name;
  try t;
  int temp;

  sym = NULL;
  if (expr->symtree)
    sym = expr->symtree->n.sym;

  /* If the procedure is not internal, a statement function or a module
     procedure,it must be external and should be checked for usage.  */
  if (sym && !sym->attr.dummy && !sym->attr.contained
	&& sym->attr.proc != PROC_ST_FUNCTION
	&& !sym->attr.use_assoc)
    resolve_global_procedure (sym, &expr->where, 0);

  /* Switch off assumed size checking and do this again for certain kinds
     of procedure, once the procedure itself is resolved.  */
  need_full_assumed_size++;

  if (resolve_actual_arglist (expr->value.function.actual) == FAILURE)
    return FAILURE;

  /* Resume assumed_size checking. */
  need_full_assumed_size--;

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

  else if (expr->value.function.actual != NULL
	     && expr->value.function.isym != NULL
	     && expr->value.function.isym->generic_id != GFC_ISYM_LBOUND
	     && expr->value.function.isym->generic_id != GFC_ISYM_LOC
	     && expr->value.function.isym->generic_id != GFC_ISYM_PRESENT)
    {
      /* Array instrinsics must also have the last upper bound of an
	 asumed size array argument.  UBOUND and SIZE have to be
	 excluded from the check if the second argument is anything
	 than a constant.  */
      int inquiry;
      inquiry = expr->value.function.isym->generic_id == GFC_ISYM_UBOUND
		  || expr->value.function.isym->generic_id == GFC_ISYM_SIZE;
	    
      for (arg = expr->value.function.actual; arg; arg = arg->next)
	{
	  if (inquiry && arg->next != NULL && arg->next->expr
		&& arg->next->expr->expr_type != EXPR_CONSTANT)
	    break;
	  
	  if (arg->expr != NULL
		&& arg->expr->rank > 0
		&& resolve_assumed_size_actual (arg->expr))
	    return FAILURE;
	}
    }

  need_full_assumed_size = temp;

  if (!pure_function (expr, &name) && name)
    {
      if (forall_flag)
	{
	  gfc_error
	    ("reference to non-PURE function '%s' at %L inside a "
	     "FORALL %s", name, &expr->where, forall_flag == 2 ?
	     "mask" : "block");
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
      gfc_symbol *esym, *proc;
      esym = expr->value.function.esym;
      proc = gfc_current_ns->proc_name;
      if (esym == proc)
      {
        gfc_error ("Function '%s' at %L cannot call itself, as it is not "
                   "RECURSIVE", name, &expr->where);
        t = FAILURE;
      }

      if (esym->attr.entry && esym->ns->entries && proc->ns->entries
          && esym->ns->entries->sym == proc->ns->entries->sym)
	{
	  gfc_error ("Call to ENTRY '%s' at %L is recursive, but function "
		     "'%s' is not declared as RECURSIVE",
		     esym->name, &expr->where, esym->ns->entries->sym->name);
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

  return t;
}


/************* Subroutine resolution *************/

static void
pure_subroutine (gfc_code * c, gfc_symbol * sym)
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
resolve_generic_s0 (gfc_code * c, gfc_symbol * sym)
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


static try
resolve_generic_s (gfc_code * c)
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

  /* Last ditch attempt.  */
  sym = c->symtree->n.sym;
  if (!gfc_generic_intrinsic (sym->name))
    {
      gfc_error
	("There is no specific subroutine for the generic '%s' at %L",
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


/* Resolve a subroutine call known to be specific.  */

static match
resolve_specific_s0 (gfc_code * c, gfc_symbol * sym)
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


static try
resolve_specific_s (gfc_code * c)
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

static try
resolve_unknown_s (gfc_code * c)
{
  gfc_symbol *sym;

  sym = c->symtree->n.sym;

  if (sym->attr.dummy)
    {
      sym->attr.proc = PROC_DUMMY;
      goto found;
    }

  /* See if we have an intrinsic function reference.  */

  if (gfc_intrinsic_name (sym->name, 1))
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

static try
resolve_call (gfc_code * c)
{
  try t;

  if (c->symtree && c->symtree->n.sym
	&& c->symtree->n.sym->ts.type != BT_UNKNOWN)
    {
      gfc_error ("'%s' at %L has a type, which is not consistent with "
		 "the CALL at %L", c->symtree->n.sym->name,
		 &c->symtree->n.sym->declared_at, &c->loc);
      return FAILURE;
    }

  /* If the procedure is not internal or module, it must be external and
     should be checked for usage.  */
  if (c->symtree && c->symtree->n.sym
	&& !c->symtree->n.sym->attr.dummy
	&& !c->symtree->n.sym->attr.contained
	&& !c->symtree->n.sym->attr.use_assoc)
    resolve_global_procedure (c->symtree->n.sym, &c->loc, 1);

  /* Subroutines without the RECURSIVE attribution are not allowed to
   * call themselves.  */
  if (c->symtree && c->symtree->n.sym && !c->symtree->n.sym->attr.recursive)
    {
      gfc_symbol *csym, *proc;
      csym = c->symtree->n.sym;
      proc = gfc_current_ns->proc_name;
      if (csym == proc)
      {
        gfc_error ("SUBROUTINE '%s' at %L cannot call itself, as it is not "
                   "RECURSIVE", csym->name, &c->loc);
        t = FAILURE;
      }

      if (csym->attr.entry && csym->ns->entries && proc->ns->entries
          && csym->ns->entries->sym == proc->ns->entries->sym)
      {
        gfc_error ("Call to ENTRY '%s' at %L is recursive, but subroutine "
                   "'%s' is not declared as RECURSIVE",
                   csym->name, &c->loc, csym->ns->entries->sym->name);
        t = FAILURE;
      }
    }

  /* Switch off assumed size checking and do this again for certain kinds
     of procedure, once the procedure itself is resolved.  */
  need_full_assumed_size++;

  if (resolve_actual_arglist (c->ext.actual) == FAILURE)
    return FAILURE;

  /* Resume assumed_size checking. */
  need_full_assumed_size--;

  if (c->resolved_sym != NULL)
    return SUCCESS;

  switch (procedure_kind (c->symtree->n.sym))
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

  /* Some checks of elemental subroutine actual arguments.  */
  if (resolve_elemental_actual (NULL, c) == FAILURE)
    return FAILURE;

  return t;
}

/* Compare the shapes of two arrays that have non-NULL shapes.  If both
   op1->shape and op2->shape are non-NULL return SUCCESS if their shapes
   match.  If both op1->shape and op2->shape are non-NULL return FAILURE
   if their shapes do not match.  If either op1->shape or op2->shape is
   NULL, return SUCCESS.  */

static try
compare_shapes (gfc_expr * op1, gfc_expr * op2)
{
  try t;
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

static try
resolve_operator (gfc_expr * e)
{
  gfc_expr *op1, *op2;
  char msg[200];
  try t;

  /* Resolve all subnodes-- give them types.  */

  switch (e->value.op.operator)
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

  switch (e->value.op.operator)
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
	       gfc_op2string (e->value.op.operator), gfc_typename (&e->ts));
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
	       gfc_op2string (e->value.op.operator), gfc_typename (&op1->ts),
	       gfc_typename (&op2->ts));
      goto bad_op;

    case INTRINSIC_CONCAT:
      if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER)
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
	       gfc_op2string (e->value.op.operator), gfc_typename (&op1->ts),
	       gfc_typename (&op2->ts));

      goto bad_op;

    case INTRINSIC_NOT:
      if (op1->ts.type == BT_LOGICAL)
	{
	  e->ts.type = BT_LOGICAL;
	  e->ts.kind = op1->ts.kind;
	  break;
	}

      sprintf (msg, _("Operand of .NOT. operator at %%L is %s"),
	       gfc_typename (&op1->ts));
      goto bad_op;

    case INTRINSIC_GT:
    case INTRINSIC_GE:
    case INTRINSIC_LT:
    case INTRINSIC_LE:
      if (op1->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX)
	{
	  strcpy (msg, _("COMPLEX quantities cannot be compared at %L"));
	  goto bad_op;
	}

      /* Fall through...  */

    case INTRINSIC_EQ:
    case INTRINSIC_NE:
      if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER)
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
		 e->value.op.operator == INTRINSIC_EQ ? ".EQV." : ".NEQV.",
		 gfc_op2string (e->value.op.operator));
      else
	sprintf (msg,
	         _("Operands of comparison operator '%s' at %%L are %s/%s"),
		 gfc_op2string (e->value.op.operator), gfc_typename (&op1->ts),
		 gfc_typename (&op2->ts));

      goto bad_op;

    case INTRINSIC_USER:
      if (op2 == NULL)
	sprintf (msg, _("Operand of user operator '%s' at %%L is %s"),
		 e->value.op.uop->name, gfc_typename (&op1->ts));
      else
	sprintf (msg, _("Operands of user operator '%s' at %%L are %s/%s"),
		 e->value.op.uop->name, gfc_typename (&op1->ts),
		 gfc_typename (&op2->ts));

      goto bad_op;

    case INTRINSIC_PARENTHESES:
      break;

    default:
      gfc_internal_error ("resolve_operator(): Bad intrinsic");
    }

  /* Deal with arrayness of an operand through an operator.  */

  t = SUCCESS;

  switch (e->value.op.operator)
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
    case INTRINSIC_NE:
    case INTRINSIC_GT:
    case INTRINSIC_GE:
    case INTRINSIC_LT:
    case INTRINSIC_LE:

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
	      gfc_error ("Inconsistent ranks for operator at %L and %L",
			 &op1->where, &op2->where);
	      t = FAILURE;

              /* Allow higher level expressions to work.  */
	      e->rank = 0;
	    }
	}

      break;

    case INTRINSIC_NOT:
    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:
    case INTRINSIC_PARENTHESES:
      e->rank = op1->rank;

      if (e->shape == NULL)
	e->shape = gfc_copy_shape (op1->shape, op1->rank);

      /* Simply copy arrayness attribute */
      break;

    default:
      break;
    }

  /* Attempt to simplify the expression.  */
  if (t == SUCCESS)
    t = gfc_simplify_expr (e, 0);
  return t;

bad_op:

  if (gfc_extend_expr (e) == SUCCESS)
    return SUCCESS;

  gfc_error (msg, &e->where);

  return FAILURE;
}


/************** Array resolution subroutines **************/


typedef enum
{ CMP_LT, CMP_EQ, CMP_GT, CMP_UNKNOWN }
comparison;

/* Compare two integer expressions.  */

static comparison
compare_bound (gfc_expr * a, gfc_expr * b)
{
  int i;

  if (a == NULL || a->expr_type != EXPR_CONSTANT
      || b == NULL || b->expr_type != EXPR_CONSTANT)
    return CMP_UNKNOWN;

  if (a->ts.type != BT_INTEGER || b->ts.type != BT_INTEGER)
    gfc_internal_error ("compare_bound(): Bad expression");

  i = mpz_cmp (a->value.integer, b->value.integer);

  if (i < 0)
    return CMP_LT;
  if (i > 0)
    return CMP_GT;
  return CMP_EQ;
}


/* Compare an integer expression with an integer.  */

static comparison
compare_bound_int (gfc_expr * a, int b)
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
compare_bound_mpz_t (gfc_expr * a, mpz_t b)
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
compute_last_value_for_triplet (gfc_expr * start, gfc_expr * end,
				gfc_expr * stride, mpz_t last)
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

static try
check_dimension (int i, gfc_array_ref * ar, gfc_array_spec * as)
{
  mpz_t last_value;

/* Given start, end and stride values, calculate the minimum and
   maximum referenced indexes.  */

  switch (ar->type)
    {
    case AR_FULL:
      break;

    case AR_ELEMENT:
      if (compare_bound (ar->start[i], as->lower[i]) == CMP_LT)
	goto bound;
      if (compare_bound (ar->start[i], as->upper[i]) == CMP_GT)
	goto bound;

      break;

    case AR_SECTION:
      if (compare_bound_int (ar->stride[i], 0) == CMP_EQ)
	{
	  gfc_error ("Illegal stride of zero at %L", &ar->c_where[i]);
	  return FAILURE;
	}

#define AR_START (ar->start[i] ? ar->start[i] : as->lower[i])
#define AR_END (ar->end[i] ? ar->end[i] : as->upper[i])

      if (compare_bound (AR_START, AR_END) == CMP_EQ
	  && (compare_bound (AR_START, as->lower[i]) == CMP_LT
	      || compare_bound (AR_START, as->upper[i]) == CMP_GT))
	goto bound;

      if (((compare_bound_int (ar->stride[i], 0) == CMP_GT
	    || ar->stride[i] == NULL)
	   && compare_bound (AR_START, AR_END) != CMP_GT)
	  || (compare_bound_int (ar->stride[i], 0) == CMP_LT
	      && compare_bound (AR_START, AR_END) != CMP_LT))
	{
	  if (compare_bound (AR_START, as->lower[i]) == CMP_LT)
	    goto bound;
	  if (compare_bound (AR_START, as->upper[i]) == CMP_GT)
	    goto bound;
	}

      mpz_init (last_value);
      if (compute_last_value_for_triplet (AR_START, AR_END, ar->stride[i],
					  last_value))
	{
	  if (compare_bound_mpz_t (as->lower[i], last_value) == CMP_GT
	      || compare_bound_mpz_t (as->upper[i], last_value) == CMP_LT)
	    {
	      mpz_clear (last_value);
	      goto bound;
	    }
	}
      mpz_clear (last_value);

#undef AR_START
#undef AR_END

      break;

    default:
      gfc_internal_error ("check_dimension(): Bad array reference");
    }

  return SUCCESS;

bound:
  gfc_warning ("Array reference at %L is out of bounds", &ar->c_where[i]);
  return SUCCESS;
}


/* Compare an array reference with an array specification.  */

static try
compare_spec_to_ref (gfc_array_ref * ar)
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
      gfc_error ("Rightmost upper bound of assumed size array section"
                 " not specified at %L", &ar->where);
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

try
gfc_resolve_index (gfc_expr * index, int check_scalar)
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
      gfc_error ("Array index at %L must be of INTEGER type",
		 &index->where);
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

try
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
find_array_spec (gfc_expr * e)
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

	if (c->dimension)
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

static try
resolve_array_ref (gfc_array_ref * ar)
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


static try
resolve_substring (gfc_ref * ref)
{

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
    }

  return SUCCESS;
}


/* Resolve subtype references.  */

static try
resolve_ref (gfc_expr * expr)
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
	  if ((current_part_dimension || seen_part_dimension)
	      && ref->u.c.component->pointer)
	    {
	      gfc_error
		("Component to the right of a part reference with nonzero "
		 "rank must not have the POINTER attribute at %L",
		 &expr->where);
	      return FAILURE;
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
expression_shape (gfc_expr * e)
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
expression_rank (gfc_expr * e)
{
  gfc_ref *ref;
  int i, rank;

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

static try
resolve_variable (gfc_expr * e)
{
  gfc_symbol *sym;
  try t;

  t = SUCCESS;

  if (e->symtree == NULL)
    return FAILURE;

  if (e->ref && resolve_ref (e) == FAILURE)
    return FAILURE;

  sym = e->symtree->n.sym;
  if (sym->attr.flavor == FL_PROCEDURE && !sym->attr.function)
    {
      e->ts.type = BT_PROCEDURE;
      return SUCCESS;
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
      if (sym->attr.dummy)
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
		gfc_error ("Variable '%s',used in a specification expression, "
			   "is referenced at %L before the ENTRY statement "
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

  return t;
}


/* Resolve an expression.  That is, make sure that types of operands agree
   with their operators, intrinsic operators are converted to function calls
   for overloaded types and unresolved function references are resolved.  */

try
gfc_resolve_expr (gfc_expr * e)
{
  try t;

  if (e == NULL)
    return SUCCESS;

  switch (e->expr_type)
    {
    case EXPR_OP:
      t = resolve_operator (e);
      break;

    case EXPR_FUNCTION:
      t = resolve_function (e);
      break;

    case EXPR_VARIABLE:
      t = resolve_variable (e);
      if (t == SUCCESS)
	expression_rank (e);
      break;

    case EXPR_SUBSTRING:
      t = resolve_ref (e);
      break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
      t = SUCCESS;
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

      /* This provides the opportunity for the length of constructors with character
	valued function elements to propogate the string length to the expression.  */
      if (e->ts.type == BT_CHARACTER)
        gfc_resolve_character_array_constructor (e);

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

  return t;
}


/* Resolve an expression from an iterator.  They must be scalar and have
   INTEGER or (optionally) REAL type.  */

static try
gfc_resolve_iterator_expr (gfc_expr * expr, bool real_ok,
			   const char * name_msgid)
{
  if (gfc_resolve_expr (expr) == FAILURE)
    return FAILURE;

  if (expr->rank != 0)
    {
      gfc_error ("%s at %L must be a scalar", _(name_msgid), &expr->where);
      return FAILURE;
    }

  if (!(expr->ts.type == BT_INTEGER
	|| (expr->ts.type == BT_REAL && real_ok)))
    {
      if (real_ok)
	gfc_error ("%s at %L must be INTEGER or REAL", _(name_msgid),
		   &expr->where);
      else
	gfc_error ("%s at %L must be INTEGER", _(name_msgid), &expr->where);
      return FAILURE;
    }
  return SUCCESS;
}


/* Resolve the expressions in an iterator structure.  If REAL_OK is
   false allow only INTEGER type iterators, otherwise allow REAL types.  */

try
gfc_resolve_iterator (gfc_iterator * iter, bool real_ok)
{

  if (iter->var->ts.type == BT_REAL)
    gfc_notify_std (GFC_STD_F95_DEL,
		    "Obsolete: REAL DO loop iterator at %L",
		    &iter->var->where);

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

  return SUCCESS;
}


/* Resolve a list of FORALL iterators.  The FORALL index-name is constrained
   to be a scalar INTEGER variable.  The subscripts and stride are scalar
   INTEGERs, and if stride is a constant it must be nonzero.  */

static void
resolve_forall_iterators (gfc_forall_iterator * iter)
{

  while (iter)
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

      iter = iter->next;
    }
}


/* Given a pointer to a symbol that is a derived type, see if any components
   have the POINTER attribute.  The search is recursive if necessary.
   Returns zero if no pointer components are found, nonzero otherwise.  */

static int
derived_pointer (gfc_symbol * sym)
{
  gfc_component *c;

  for (c = sym->components; c; c = c->next)
    {
      if (c->pointer)
	return 1;

      if (c->ts.type == BT_DERIVED && derived_pointer (c->ts.derived))
	return 1;
    }

  return 0;
}


/* Given a pointer to a symbol that is a derived type, see if it's
   inaccessible, i.e. if it's defined in another module and the components are
   PRIVATE.  The search is recursive if necessary.  Returns zero if no
   inaccessible components are found, nonzero otherwise.  */

static int
derived_inaccessible (gfc_symbol *sym)
{
  gfc_component *c;

  if (sym->attr.use_assoc && sym->component_access == ACCESS_PRIVATE)
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

static try
resolve_deallocate_expr (gfc_expr * e)
{
  symbol_attribute attr;
  int allocatable;
  gfc_ref *ref;

  if (gfc_resolve_expr (e) == FAILURE)
    return FAILURE;

  attr = gfc_expr_attr (e);
  if (attr.pointer)
    return SUCCESS;

  if (e->expr_type != EXPR_VARIABLE)
    goto bad;

  allocatable = e->symtree->n.sym->attr.allocatable;
  for (ref = e->ref; ref; ref = ref->next)
    switch (ref->type)
      {
      case REF_ARRAY:
	if (ref->u.ar.type != AR_FULL)
	  allocatable = 0;
	break;

      case REF_COMPONENT:
	allocatable = (ref->u.c.component->as != NULL
		       && ref->u.c.component->as->type == AS_DEFERRED);
	break;

      case REF_SUBSTRING:
	allocatable = 0;
	break;
      }

  if (allocatable == 0)
    {
    bad:
      gfc_error ("Expression in DEALLOCATE statement at %L must be "
		 "ALLOCATABLE or a POINTER", &e->where);
    }

  return SUCCESS;
}


/* Given the expression node e for an allocatable/pointer of derived type to be
   allocated, get the expression node to be initialized afterwards (needed for
   derived types with default initializers).  */

static gfc_expr *
expr_to_initialize (gfc_expr * e)
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

static try
resolve_allocate_expr (gfc_expr * e, gfc_code * code)
{
  int i, pointer, allocatable, dimension;
  symbol_attribute attr;
  gfc_ref *ref, *ref2;
  gfc_array_ref *ar;
  gfc_code *init_st;
  gfc_expr *init_e;

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
	switch (ref->type)
	  {
	  case REF_ARRAY:
	    if (ref->next != NULL)
	      pointer = 0;
	    break;

	  case REF_COMPONENT:
	    allocatable = (ref->u.c.component->as != NULL
			   && ref->u.c.component->as->type == AS_DEFERRED);

	    pointer = ref->u.c.component->pointer;
	    dimension = ref->u.c.component->dimension;
	    break;

	  case REF_SUBSTRING:
	    allocatable = 0;
	    pointer = 0;
	    break;
	  }
    }

  if (allocatable == 0 && pointer == 0)
    {
      gfc_error ("Expression in ALLOCATE statement at %L must be "
		 "ALLOCATABLE or a POINTER", &e->where);
      return FAILURE;
    }

  /* Add default initializer for those derived types that need them.  */
  if (e->ts.type == BT_DERIVED && (init_e = gfc_default_initializer (&e->ts)))
    {
        init_st = gfc_get_code ();
        init_st->loc = code->loc;
        init_st->op = EXEC_INIT_ASSIGN;
        init_st->expr = expr_to_initialize (e);
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

  if (ref2->u.ar.type == AR_ELEMENT)
    return SUCCESS;

  /* Make sure that the array section reference makes sense in the
    context of an ALLOCATE specification.  */

  ar = &ref2->u.ar;

  for (i = 0; i < ar->dimen; i++)
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

  return SUCCESS;
}


/************ SELECT CASE resolution subroutines ************/

/* Callback function for our mergesort variant.  Determines interval
   overlaps for CASEs. Return <0 if op1 < op2, 0 for overlap, >0 for
   op1 > op2.  Assumes we're not dealing with the default case.  
   We have op1 = (:L), (K:L) or (K:) and op2 = (:N), (M:N) or (M:).
   There are nine situations to check.  */

static int
compare_cases (const gfc_case * op1, const gfc_case * op2)
{
  int retval;

  if (op1->low == NULL) /* op1 = (:L)  */
    {
      /* op2 = (:N), so overlap.  */
      retval = 0;
      /* op2 = (M:) or (M:N),  L < M  */
      if (op2->low != NULL
	  && gfc_compare_expr (op1->high, op2->low) < 0)
	retval = -1;
    }
  else if (op1->high == NULL) /* op1 = (K:)  */
    {
      /* op2 = (M:), so overlap.  */
      retval = 0;
      /* op2 = (:N) or (M:N), K > N  */
      if (op2->high != NULL
	  && gfc_compare_expr (op1->low, op2->high) > 0)
	retval = 1;
    }
  else /* op1 = (K:L)  */
    {
      if (op2->low == NULL)       /* op2 = (:N), K > N  */
	retval = (gfc_compare_expr (op1->low, op2->high) > 0) ? 1 : 0;
      else if (op2->high == NULL) /* op2 = (M:), L < M  */
	retval = (gfc_compare_expr (op1->high, op2->low) < 0) ? -1 : 0;
      else                        /* op2 = (M:N)  */
        {
	  retval =  0;
          /* L < M  */
	  if (gfc_compare_expr (op1->high, op2->low) < 0)
	    retval =  -1;
          /* K > N  */
	  else if (gfc_compare_expr (op1->low, op2->high) > 0)
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
check_case_overlap (gfc_case * list)
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

static try
validate_case_label_expr (gfc_expr * e, gfc_expr * case_expr)
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
      gfc_error("Expression in CASE statement at %L must be kind %d",
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
resolve_select (gfc_code * code)
{
  gfc_code *body;
  gfc_expr *case_expr;
  gfc_case *cp, *default_case, *tail, *head;
  int seen_unreachable;
  int seen_logical;
  int ncases;
  bt type;
  try t;

  if (code->expr == NULL)
    {
      /* This was actually a computed GOTO statement.  */
      case_expr = code->expr2;
      if (case_expr->ts.type != BT_INTEGER
	  || case_expr->rank != 0)
	gfc_error ("Selection expression in computed GOTO statement "
		   "at %L must be a scalar integer expression",
		   &case_expr->where);

      /* Further checking is not necessary because this SELECT was built
	 by the compiler, so it should always be OK.  Just move the
	 case_expr from expr2 to expr so that we can handle computed
	 GOTOs as normal SELECTs from here on.  */
      code->expr = code->expr2;
      code->expr2 = NULL;
      return;
    }

  case_expr = code->expr;

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
		  && gfc_compare_expr (cp->low, cp->high) > 0)
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
	      gfc_error
		("Logical range in CASE statement at %L is not allowed",
		 &cp->low->where);
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
	      && gfc_compare_expr (cp->low, cp->high) > 0)
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
resolve_transfer (gfc_code * code)
{
  gfc_typespec *ts;
  gfc_symbol *sym;
  gfc_ref *ref;
  gfc_expr *exp;

  exp = code->expr;

  if (exp->expr_type != EXPR_VARIABLE)
    return;

  sym = exp->symtree->n.sym;
  ts = &sym->ts;

  /* Go to actual component transferred.  */
  for (ref = code->expr->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT)
      ts = &ref->u.c.component->ts;

  if (ts->type == BT_DERIVED)
    {
      /* Check that transferred derived type doesn't contain POINTER
	 components.  */
      if (derived_pointer (ts->derived))
	{
	  gfc_error ("Data transfer element at %L cannot have "
		     "POINTER components", &code->loc);
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

/* Given a branch to a label and a namespace, if the branch is conforming.
   The code node described where the branch is located.  */

static void
resolve_branch (gfc_st_label * label, gfc_code * code)
{
  gfc_code *block, *found;
  code_stack *stack;
  gfc_st_label *lp;

  if (label == NULL)
    return;
  lp = label;

  /* Step one: is this a valid branching target?  */

  if (lp->defined == ST_LABEL_UNKNOWN)
    {
      gfc_error ("Label %d referenced at %L is never defined", lp->value,
		 &lp->where);
      return;
    }

  if (lp->defined != ST_LABEL_TARGET)
    {
      gfc_error ("Statement at %L is not a valid branch target statement "
		 "for the branch statement at %L", &lp->where, &code->loc);
      return;
    }

  /* Step two: make sure this branch is not a branch to itself ;-)  */

  if (code->here == label)
    {
      gfc_warning ("Branch at %L causes an infinite loop", &code->loc);
      return;
    }

  /* Step three: Try to find the label in the parse tree. To do this,
     we traverse the tree block-by-block: first the block that
     contains this GOTO, then the block that it is nested in, etc.  We
     can ignore other blocks because branching into another block is
     not allowed.  */

  found = NULL;

  for (stack = cs_base; stack; stack = stack->prev)
    {
      for (block = stack->head; block; block = block->next)
	{
	  if (block->here == label)
	    {
	      found = block;
	      break;
	    }
	}

      if (found)
	break;
    }

  if (found == NULL)
    {
      /* The label is not in an enclosing block, so illegal.  This was
	 allowed in Fortran 66, so we allow it as extension.  We also
	 forego further checks if we run into this.  */
      gfc_notify_std (GFC_STD_LEGACY,
		      "Label at %L is not in the same block as the "
		      "GOTO statement at %L", &lp->where, &code->loc);
      return;
    }

  /* Step four: Make sure that the branching target is legal if
     the statement is an END {SELECT,DO,IF}.  */

  if (found->op == EXEC_NOP)
    {
      for (stack = cs_base; stack; stack = stack->prev)
	if (stack->current->next == found)
	  break;

      if (stack == NULL)
	gfc_notify_std (GFC_STD_F95_DEL,
			"Obsolete: GOTO at %L jumps to END of construct at %L",
			&code->loc, &found->loc);
    }
}


/* Check whether EXPR1 has the same shape as EXPR2.  */

static try
resolve_where_shape (gfc_expr *expr1, gfc_expr *expr2)
{
  mpz_t shape[GFC_MAX_DIMENSIONS];
  mpz_t shape2[GFC_MAX_DIMENSIONS];
  try result = FAILURE;
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
  for (i--; i>=0; i--)
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
    e = cblock->expr;
  else /* inner WHERE */
    e = mask;

  while (cblock)
    {
      if (cblock->expr)
        {
          /* Check if the mask-expr has a consistent shape with the
             outmost WHERE mask-expr.  */
          if (resolve_where_shape (cblock->expr, e) == FAILURE)
            gfc_error ("WHERE mask at %L has inconsistent shape",
                       &cblock->expr->where);
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
              if (e && resolve_where_shape (cnext->expr, e) == FAILURE)
               gfc_error ("WHERE assignment target at %L has "
                          "inconsistent shape", &cnext->expr->where);
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


/* Check whether the FORALL index appears in the expression or not.  */

static try
gfc_find_forall_index (gfc_expr *expr, gfc_symbol *symbol)
{
  gfc_array_ref ar;
  gfc_ref *tmp;
  gfc_actual_arglist *args;
  int i;

  switch (expr->expr_type)
    {
    case EXPR_VARIABLE:
      gcc_assert (expr->symtree->n.sym);

      /* A scalar assignment  */
      if (!expr->ref)
        {
          if (expr->symtree->n.sym == symbol)
            return SUCCESS;
          else
            return FAILURE;
        }

      /* the expr is array ref, substring or struct component.  */
      tmp = expr->ref;
      while (tmp != NULL)
        {
          switch (tmp->type)
            {
            case  REF_ARRAY:
              /* Check if the symbol appears in the array subscript.  */
              ar = tmp->u.ar;
              for (i = 0; i < GFC_MAX_DIMENSIONS; i++)
                {
                  if (ar.start[i])
                    if (gfc_find_forall_index (ar.start[i], symbol) == SUCCESS)
                      return SUCCESS;

                  if (ar.end[i])
                    if (gfc_find_forall_index (ar.end[i], symbol) == SUCCESS)
                      return SUCCESS;

                  if (ar.stride[i])
                    if (gfc_find_forall_index (ar.stride[i], symbol) == SUCCESS)
                      return SUCCESS;
                }  /* end for  */
              break;

            case REF_SUBSTRING:
              if (expr->symtree->n.sym == symbol)
                return SUCCESS;
              tmp = expr->ref;
              /* Check if the symbol appears in the substring section.  */
              if (gfc_find_forall_index (tmp->u.ss.start, symbol) == SUCCESS)
                return SUCCESS;
              if (gfc_find_forall_index (tmp->u.ss.end, symbol) == SUCCESS)
                return SUCCESS;
              break;

            case REF_COMPONENT:
              break;

            default:
              gfc_error("expresion reference type error at %L", &expr->where);
            }
          tmp = tmp->next;
        }
      break;

    /* If the expression is a function call, then check if the symbol
       appears in the actual arglist of the function.  */
    case EXPR_FUNCTION:
      for (args = expr->value.function.actual; args; args = args->next)
        {
          if (gfc_find_forall_index(args->expr,symbol) == SUCCESS)
            return SUCCESS;
        }
      break;

    /* It seems not to happen.  */
    case EXPR_SUBSTRING:
      if (expr->ref)
        {
          tmp = expr->ref;
          gcc_assert (expr->ref->type == REF_SUBSTRING);
          if (gfc_find_forall_index (tmp->u.ss.start, symbol) == SUCCESS)
            return SUCCESS;
          if (gfc_find_forall_index (tmp->u.ss.end, symbol) == SUCCESS)
            return SUCCESS;
        }
      break;

    /* It seems not to happen.  */
    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
      gfc_error ("Unsupported statement while finding forall index in "
                 "expression");
      break;

    case EXPR_OP:
      /* Find the FORALL index in the first operand.  */
      if (expr->value.op.op1)
	{
	  if (gfc_find_forall_index (expr->value.op.op1, symbol) == SUCCESS)
	    return SUCCESS;
	}

      /* Find the FORALL index in the second operand.  */
      if (expr->value.op.op2)
	{
	  if (gfc_find_forall_index (expr->value.op.op2, symbol) == SUCCESS)
	    return SUCCESS;
	}
      break;

    default:
      break;
    }

  return FAILURE;
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
      if ((code->expr->expr_type == EXPR_VARIABLE)
          && (code->expr->symtree->n.sym == forall_index))
        gfc_error ("Assignment to a FORALL index variable at %L",
                   &code->expr->where);
      else
        {
          /* If one of the FORALL index variables doesn't appear in the
             assignment target, then there will be a many-to-one
             assignment.  */
          if (gfc_find_forall_index (code->expr, forall_index) == FAILURE)
            gfc_error ("The FORALL with index '%s' cause more than one "
                       "assignment to this object at %L",
                       var_expr[n]->symtree->name, &code->expr->where);
        }
    }
}


/* Resolve WHERE statement in FORALL construct.  */

static void
gfc_resolve_where_code_in_forall (gfc_code *code, int nvar, gfc_expr **var_expr){
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

        /* Because the resolve_blocks() will handle the nested FORALL,
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


/* Given a FORALL construct, first resolve the FORALL iterator, then call
   gfc_resolve_forall_body to resolve the FORALL body.  */

static void resolve_blocks (gfc_code *, gfc_namespace *);

static void
gfc_resolve_forall (gfc_code *code, gfc_namespace *ns, int forall_save)
{
  static gfc_expr **var_expr;
  static int total_var = 0;
  static int nvar = 0;
  gfc_forall_iterator *fa;
  gfc_symbol *forall_index;
  gfc_code *next;
  int i;

  /* Start to resolve a FORALL construct   */
  if (forall_save == 0)
    {
      /* Count the total number of FORALL index in the nested FORALL
         construct in order to allocate the VAR_EXPR with proper size.  */
      next = code;
      while ((next != NULL) && (next->op == EXEC_FORALL))
        {
          for (fa = next->ext.forall_iterator; fa; fa = fa->next)
            total_var ++;
          next = next->block->next;
        }

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

      forall_index = fa->var->symtree->n.sym;

      /* Check if the FORALL index appears in start, end or stride.  */
      if (gfc_find_forall_index (fa->start, forall_index) == SUCCESS)
        gfc_error ("A FORALL index must not appear in a limit or stride "
                   "expression in the same FORALL at %L", &fa->start->where);
      if (gfc_find_forall_index (fa->end, forall_index) == SUCCESS)
        gfc_error ("A FORALL index must not appear in a limit or stride "
                   "expression in the same FORALL at %L", &fa->end->where);
      if (gfc_find_forall_index (fa->stride, forall_index) == SUCCESS)
        gfc_error ("A FORALL index must not appear in a limit or stride "
                   "expression in the same FORALL at %L", &fa->stride->where);
      nvar++;
    }

  /* Resolve the FORALL body.  */
  gfc_resolve_forall_body (code, nvar, var_expr);

  /* May call gfc_resolve_forall to resolve the inner FORALL loop.  */
  resolve_blocks (code->block, ns);

  /* Free VAR_EXPR after the whole FORALL construct resolved.  */
  for (i = 0; i < total_var; i++)
    gfc_free_expr (var_expr[i]);

  /* Reset the counters.  */
  total_var = 0;
  nvar = 0;
}


/* Resolve lists of blocks found in IF, SELECT CASE, WHERE, FORALL ,GOTO and
   DO code nodes.  */

static void resolve_code (gfc_code *, gfc_namespace *);

static void
resolve_blocks (gfc_code * b, gfc_namespace * ns)
{
  try t;

  for (; b; b = b->block)
    {
      t = gfc_resolve_expr (b->expr);
      if (gfc_resolve_expr (b->expr2) == FAILURE)
	t = FAILURE;

      switch (b->op)
	{
	case EXEC_IF:
	  if (t == SUCCESS && b->expr != NULL
	      && (b->expr->ts.type != BT_LOGICAL || b->expr->rank != 0))
	    gfc_error
	      ("ELSE IF clause at %L requires a scalar LOGICAL expression",
	       &b->expr->where);
	  break;

	case EXEC_WHERE:
	  if (t == SUCCESS
	      && b->expr != NULL
	      && (b->expr->ts.type != BT_LOGICAL
		  || b->expr->rank == 0))
	    gfc_error
	      ("WHERE/ELSEWHERE clause at %L requires a LOGICAL array",
	       &b->expr->where);
	  break;

        case EXEC_GOTO:
          resolve_branch (b->label, b);
          break;

	case EXEC_SELECT:
	case EXEC_FORALL:
	case EXEC_DO:
	case EXEC_DO_WHILE:
	case EXEC_READ:
	case EXEC_WRITE:
	case EXEC_IOLENGTH:
	  break;

	default:
	  gfc_internal_error ("resolve_block(): Bad block type");
	}

      resolve_code (b->next, ns);
    }
}


/* Given a block of code, recursively resolve everything pointed to by this
   code block.  */

static void
resolve_code (gfc_code * code, gfc_namespace * ns)
{
  int forall_save;
  code_stack frame;
  gfc_alloc *a;
  try t;

  frame.prev = cs_base;
  frame.head = code;
  cs_base = &frame;

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
      else
        resolve_blocks (code->block, ns);

      t = gfc_resolve_expr (code->expr);
      forall_flag = forall_save;

      if (gfc_resolve_expr (code->expr2) == FAILURE)
	t = FAILURE;

      switch (code->op)
	{
	case EXEC_NOP:
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
          if (code->expr != NULL)
	    {
	      if (code->expr->ts.type != BT_INTEGER)
		gfc_error ("ASSIGNED GOTO statement at %L requires an INTEGER "
                       "variable", &code->expr->where);
	      else if (code->expr->symtree->n.sym->attr.assign != 1)
		gfc_error ("Variable '%s' has not been assigned a target label "
			"at %L", code->expr->symtree->n.sym->name,
			&code->expr->where);
	    }
	  else
            resolve_branch (code->label, code);
	  break;

	case EXEC_RETURN:
	  if (code->expr != NULL
		&& (code->expr->ts.type != BT_INTEGER || code->expr->rank))
	    gfc_error ("Alternate RETURN statement at %L requires a SCALAR-"
		       "INTEGER return specifier", &code->expr->where);
	  break;

	case EXEC_INIT_ASSIGN:
	  break;

	case EXEC_ASSIGN:
	  if (t == FAILURE)
	    break;

	  if (gfc_extend_assign (code, ns) == SUCCESS)
	    {
	      if (gfc_pure (NULL) && !gfc_pure (code->symtree->n.sym))
		{
		  gfc_error ("Subroutine '%s' called instead of assignment at "
			     "%L must be PURE", code->symtree->n.sym->name,
			     &code->loc);
		  break;
		}
	      goto call;
	    }

	  if (gfc_pure (NULL))
	    {
	      if (gfc_impure_variable (code->expr->symtree->n.sym))
		{
		  gfc_error
		    ("Cannot assign to variable '%s' in PURE procedure at %L",
		     code->expr->symtree->n.sym->name, &code->expr->where);
		  break;
		}

	      if (code->expr2->ts.type == BT_DERIVED
		  && derived_pointer (code->expr2->ts.derived))
		{
		  gfc_error
		    ("Right side of assignment at %L is a derived type "
		     "containing a POINTER in a PURE procedure",
		     &code->expr2->where);
		  break;
		}
	    }

	  gfc_check_assign (code->expr, code->expr2, 1);
	  break;

	case EXEC_LABEL_ASSIGN:
          if (code->label->defined == ST_LABEL_UNKNOWN)
            gfc_error ("Label %d referenced at %L is never defined",
                       code->label->value, &code->label->where);
          if (t == SUCCESS
	      && (code->expr->expr_type != EXPR_VARIABLE
		  || code->expr->symtree->n.sym->ts.type != BT_INTEGER
		  || code->expr->symtree->n.sym->ts.kind 
		        != gfc_default_integer_kind
		  || code->expr->symtree->n.sym->as != NULL))
	    gfc_error ("ASSIGN statement at %L requires a scalar "
		       "default INTEGER variable", &code->expr->where);
	  break;

	case EXEC_POINTER_ASSIGN:
	  if (t == FAILURE)
	    break;

	  gfc_check_pointer_assign (code->expr, code->expr2);
	  break;

	case EXEC_ARITHMETIC_IF:
	  if (t == SUCCESS
	      && code->expr->ts.type != BT_INTEGER
	      && code->expr->ts.type != BT_REAL)
	    gfc_error ("Arithmetic IF statement at %L requires a numeric "
		       "expression", &code->expr->where);

	  resolve_branch (code->label, code);
	  resolve_branch (code->label2, code);
	  resolve_branch (code->label3, code);
	  break;

	case EXEC_IF:
	  if (t == SUCCESS && code->expr != NULL
	      && (code->expr->ts.type != BT_LOGICAL
		  || code->expr->rank != 0))
	    gfc_error ("IF clause at %L requires a scalar LOGICAL expression",
		       &code->expr->where);
	  break;

	case EXEC_CALL:
	call:
	  resolve_call (code);
	  break;

	case EXEC_SELECT:
	  /* Select is complicated. Also, a SELECT construct could be
	     a transformed computed GOTO.  */
	  resolve_select (code);
	  break;

	case EXEC_DO:
	  if (code->ext.iterator != NULL)
	    gfc_resolve_iterator (code->ext.iterator, true);
	  break;

	case EXEC_DO_WHILE:
	  if (code->expr == NULL)
	    gfc_internal_error ("resolve_code(): No expression on DO WHILE");
	  if (t == SUCCESS
	      && (code->expr->rank != 0
		  || code->expr->ts.type != BT_LOGICAL))
	    gfc_error ("Exit condition of DO WHILE loop at %L must be "
		       "a scalar LOGICAL expression", &code->expr->where);
	  break;

	case EXEC_ALLOCATE:
	  if (t == SUCCESS && code->expr != NULL
	      && code->expr->ts.type != BT_INTEGER)
	    gfc_error ("STAT tag in ALLOCATE statement at %L must be "
		       "of type INTEGER", &code->expr->where);

	  for (a = code->ext.alloc_list; a; a = a->next)
	    resolve_allocate_expr (a->expr, code);

	  break;

	case EXEC_DEALLOCATE:
	  if (t == SUCCESS && code->expr != NULL
	      && code->expr->ts.type != BT_INTEGER)
	    gfc_error
	      ("STAT tag in DEALLOCATE statement at %L must be of type "
	       "INTEGER", &code->expr->where);

	  for (a = code->ext.alloc_list; a; a = a->next)
	    resolve_deallocate_expr (a->expr);

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

	case EXEC_READ:
	case EXEC_WRITE:
	  if (gfc_resolve_dt (code->ext.dt) == FAILURE)
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

	  if (code->expr != NULL && code->expr->ts.type != BT_LOGICAL)
	    gfc_error
	      ("FORALL mask clause at %L requires a LOGICAL expression",
	       &code->expr->where);
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
resolve_values (gfc_symbol * sym)
{

  if (sym->value == NULL)
    return;

  if (gfc_resolve_expr (sym->value) == FAILURE)
    return;

  gfc_check_assign_symbol (sym, sym->value);
}


/* Resolve an index expression.  */

static try
resolve_index_expr (gfc_expr * e)
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

static try
resolve_charlen (gfc_charlen *cl)
{
  if (cl->resolved)
    return SUCCESS;

  cl->resolved = 1;

  specification_expr = 1;

  if (resolve_index_expr (cl->length) == FAILURE)
    {
      specification_expr = 0;
      return FAILURE;
    }

  return SUCCESS;
}


/* Test for non-constant shape arrays. */

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


/* Assign the default initializer to a derived type variable or result.  */

static void
apply_default_init (gfc_symbol *sym)
{
  gfc_expr *lval;
  gfc_expr *init = NULL;
  gfc_code *init_st;
  gfc_namespace *ns = sym->ns;

  if (sym->attr.flavor != FL_VARIABLE && !sym->attr.function)
    return;

  if (sym->ts.type == BT_DERIVED && sym->ts.derived)
    init = gfc_default_initializer (&sym->ts);

  if (init == NULL)
    return;

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

  /* Add the code at scope entry.  */
  init_st = gfc_get_code ();
  init_st->next = ns->code;
  ns->code = init_st;

  /* Assign the default initializer to the l-value.  */
  init_st->loc = sym->declared_at;
  init_st->op = EXEC_INIT_ASSIGN;
  init_st->expr = lval;
  init_st->expr2 = init;
}


/* Resolution of common features of flavors variable and procedure. */

static try
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

/* Resolve symbols with flavor variable.  */

static try
resolve_fl_variable (gfc_symbol *sym, int mp_flag)
{
  int flag;
  int i;
  gfc_expr *e;
  gfc_expr *constructor_expr;
  const char * auto_save_msg;

  auto_save_msg = "automatic object '%s' at %L cannot have the "
		  "SAVE attribute";

  if (resolve_fl_var_and_proc (sym, mp_flag) == FAILURE)
    return FAILURE;

  /* Set this flag to check that variables are parameters of all entries.
     This check is effected by the call to gfc_resolve_expr through
     is_non_constant_shape_array.  */
  specification_expr = 1;

  if (!sym->attr.use_assoc
	&& !sym->attr.allocatable
	&& !sym->attr.pointer
	&& is_non_constant_shape_array (sym))
    {
	/* The shape of a main program or module array needs to be constant.  */
	if (sym->ns->proc_name
	      && (sym->ns->proc_name->attr.flavor == FL_MODULE
		    || sym->ns->proc_name->attr.is_main_program))
	  {
	    gfc_error ("The module or main program array '%s' at %L must "
		       "have constant shape", sym->name, &sym->declared_at);
	    specification_expr = 0;
	    return FAILURE;
	  }
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

  /* Can the symbol have an initializer?  */
  flag = 0;
  if (sym->attr.allocatable || sym->attr.external || sym->attr.dummy
	|| sym->attr.intrinsic || sym->attr.result)
    flag = 1;
  else if (sym->attr.dimension && !sym->attr.pointer)
    {
      /* Don't allow initialization of automatic arrays.  */
      for (i = 0; i < sym->as->rank; i++)
	{
	  if (sym->as->lower[i] == NULL
		|| sym->as->lower[i]->expr_type != EXPR_CONSTANT
		|| sym->as->upper[i] == NULL
		|| sym->as->upper[i]->expr_type != EXPR_CONSTANT)
	    {
	      flag = 1;
	      break;
	    }
	}

      /* Also, they must not have the SAVE attribute.  */
      if (flag && sym->attr.save)
	{
	  gfc_error (auto_save_msg, sym->name, &sym->declared_at);
	  return FAILURE;
	}
  }

  /* Reject illegal initializers.  */
  if (sym->value && flag)
    {
      if (sym->attr.allocatable)
	gfc_error ("Allocatable '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.external)
	gfc_error ("External '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.dummy)
	gfc_error ("Dummy '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.intrinsic)
	gfc_error ("Intrinsic '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else if (sym->attr.result)
	gfc_error ("Function result '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      else
	gfc_error ("Automatic array '%s' at %L cannot have an initializer",
		   sym->name, &sym->declared_at);
      return FAILURE;
    }

  /* Check to see if a derived type is blocked from being host associated
     by the presence of another class I symbol in the same namespace.
     14.6.1.3 of the standard and the discussion on comp.lang.fortran.  */
  if (sym->ts.type == BT_DERIVED && sym->ns != sym->ts.derived->ns)
    {
      gfc_symbol *s;
      gfc_find_symbol (sym->ts.derived->name, sym->ns, 0, &s);
      if (s && (s->attr.flavor != FL_DERIVED
		  || !gfc_compare_derived_types (s, sym->ts.derived)))
	{
	  gfc_error ("The type %s cannot be host associated at %L because "
		     "it is blocked by an incompatible object of the same "
		     "name at %L", sym->ts.derived->name, &sym->declared_at,
		     &s->declared_at);
	  return FAILURE;
	}
    }

  /* 4th constraint in section 11.3:  "If an object of a type for which
     component-initialization is specified (R429) appears in the
     specification-part of a module and does not have the ALLOCATABLE
     or POINTER attribute, the object shall have the SAVE attribute."  */

  constructor_expr = NULL;
  if (sym->ts.type == BT_DERIVED && !(sym->value || flag))
	constructor_expr = gfc_default_initializer (&sym->ts);

  if (sym->ns->proc_name
	&& sym->ns->proc_name->attr.flavor == FL_MODULE
	&& constructor_expr
	&& !sym->ns->save_all && !sym->attr.save
	&& !sym->attr.pointer && !sym->attr.allocatable)
    {
      gfc_error("Object '%s' at %L must have the SAVE attribute %s",
 	 	sym->name, &sym->declared_at,
		"for default initialization of a component");
      return FAILURE;
    }

  /* Assign default initializer.  */
  if (sym->ts.type == BT_DERIVED && !sym->value && !sym->attr.pointer
      && !sym->attr.allocatable && (!flag || sym->attr.intent == INTENT_OUT))
    sym->value = gfc_default_initializer (&sym->ts);

  return SUCCESS;
}


/* Resolve a procedure.  */

static try
resolve_fl_procedure (gfc_symbol *sym, int mp_flag)
{
  gfc_formal_arglist *arg;

  if (sym->attr.function
	&& resolve_fl_var_and_proc (sym, mp_flag) == FAILURE)
    return FAILURE;

  if (sym->attr.proc == PROC_ST_FUNCTION)
    {
      if (sym->ts.type == BT_CHARACTER)
        {
          gfc_charlen *cl = sym->ts.cl;
          if (!cl || !cl->length || cl->length->expr_type != EXPR_CONSTANT)
            {
              gfc_error ("Character-valued statement function '%s' at %L must "
                         "have constant length", sym->name, &sym->declared_at);
              return FAILURE;
            }
        }
    }

  /* Ensure that derived type for are not of a private type.  Internal
     module procedures are excluded by 2.2.3.3 - ie. they are not
     externally accessible and can access all the objects accesible in
     the host. */
  if (!(sym->ns->parent
	    && sym->ns->parent->proc_name->attr.flavor == FL_MODULE)
	&& gfc_check_access(sym->attr.access, sym->ns->default_access))
    {
      for (arg = sym->formal; arg; arg = arg->next)
	{
	  if (arg->sym
		&& arg->sym->ts.type == BT_DERIVED
		&& !arg->sym->ts.derived->attr.use_assoc
		&& !gfc_check_access(arg->sym->ts.derived->attr.access,
			arg->sym->ts.derived->ns->default_access))
	    {
	      gfc_error_now ("'%s' is of a PRIVATE type and cannot be "
			     "a dummy argument of '%s', which is "
			     "PUBLIC at %L", arg->sym->name, sym->name,
			     &sym->declared_at);
	      /* Stop this message from recurring.  */
	      arg->sym->ts.derived->attr.access = ACCESS_PUBLIC;
	      return FAILURE;
	    }
	}
    }

  /* An external symbol may not have an intializer because it is taken to be
     a procedure.  */
  if (sym->attr.external && sym->value)
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
  return SUCCESS;
}


/* Resolve the components of a derived type.  */

static try
resolve_fl_derived (gfc_symbol *sym)
{
  gfc_component *c;
  gfc_dt_list * dt_list;
  int i;

  for (c = sym->components; c != NULL; c = c->next)
    {
      if (c->ts.type == BT_CHARACTER)
	{
	 if (c->ts.cl->length == NULL
	     || (resolve_charlen (c->ts.cl) == FAILURE)
	     || !gfc_is_constant_expr (c->ts.cl->length))
	   {
	     gfc_error ("Character length of component '%s' needs to "
			"be a constant specification expression at %L.",
			c->name,
			c->ts.cl->length ? &c->ts.cl->length->where : &c->loc);
	     return FAILURE;
	   }
	}

      if (c->ts.type == BT_DERIVED
	    && sym->component_access != ACCESS_PRIVATE
	    && gfc_check_access(sym->attr.access, sym->ns->default_access)
	    && !c->ts.derived->attr.use_assoc
	    && !gfc_check_access(c->ts.derived->attr.access,
				 c->ts.derived->ns->default_access))
	{
	  gfc_error ("The component '%s' is a PRIVATE type and cannot be "
		     "a component of '%s', which is PUBLIC at %L",
		      c->name, sym->name, &sym->declared_at);
	  return FAILURE;
	}

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

      if (c->ts.type == BT_DERIVED && c->pointer
	    && c->ts.derived->components == NULL)
	{
	  gfc_error ("The pointer component '%s' of '%s' at %L is a type "
		     "that has not been declared", c->name, sym->name,
		     &c->loc);
	  return FAILURE;
	}

      if (c->pointer || c->as == NULL)
	continue;

      for (i = 0; i < c->as->rank; i++)
	{
	  if (c->as->lower[i] == NULL
		|| !gfc_is_constant_expr (c->as->lower[i])
		|| (resolve_index_expr (c->as->lower[i]) == FAILURE)
		|| c->as->upper[i] == NULL
		|| (resolve_index_expr (c->as->upper[i]) == FAILURE)
		|| !gfc_is_constant_expr (c->as->upper[i]))
	    {
	      gfc_error ("Component '%s' of '%s' at %L must have "
			 "constant array bounds.",
			 c->name, sym->name, &c->loc);
	      return FAILURE;
	    }
	}
    }
    
  /* Add derived type to the derived type list.  */
  for (dt_list = sym->ns->derived_types; dt_list; dt_list = dt_list->next)
    if (sym == dt_list->derived)
      break;

  if (dt_list == NULL)
    {
      dt_list = gfc_get_dt_list ();
      dt_list->next = sym->ns->derived_types;
      dt_list->derived = sym;
      sym->ns->derived_types = dt_list;
    }

  return SUCCESS;
}


static try
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
		&& !(sym->ns->parent == nl->sym->ns)
		       && !gfc_check_access(nl->sym->attr.access,
					    nl->sym->ns->default_access))
	    {
	      gfc_error ("PRIVATE symbol '%s' cannot be member of "
			 "PUBLIC namelist at %L", nl->sym->name,
			 &sym->declared_at);
	      return FAILURE;
	    }
	}
    }

    /* Reject namelist arrays that are not constant shape.  */
    for (nl = sym->namelist; nl; nl = nl->next)
      {
	if (is_non_constant_shape_array (nl->sym))
	  {
	    gfc_error ("The array '%s' must have constant shape to be "
		       "a NAMELIST object at %L", nl->sym->name,
		       &sym->declared_at);
	    return FAILURE;
	  }
    }

  /* 14.1.2 A module or internal procedure represent local entities
     of the same type as a namelist member and so are not allowed.
     Note that this is sometimes caught by check_conflict so the
     same message has been used.  */
  for (nl = sym->namelist; nl; nl = nl->next)
    {
      if (nl->sym->ts.kind != 0 && nl->sym->attr.flavor == FL_VARIABLE)
	continue;
      nlsym = NULL;
      if (sym->ns->parent && nl->sym && nl->sym->name)
	gfc_find_symbol (nl->sym->name, sym->ns->parent, 0, &nlsym);
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


static try
resolve_fl_parameter (gfc_symbol *sym)
{
  /* A parameter array's shape needs to be constant.  */
  if (sym->as != NULL && !gfc_is_compile_time_shape (sym->as))
    {
      gfc_error ("Parameter array '%s' at %L cannot be automatic "
		 "or assumed shape", sym->name, &sym->declared_at);
      return FAILURE;
    }

  /* Make sure a parameter that has been implicitly typed still
     matches the implicit type, since PARAMETER statements can precede
     IMPLICIT statements.  */
  if (sym->attr.implicit_type
	&& !gfc_compare_types (&sym->ts,
			       gfc_get_default_type (sym, sym->ns)))
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
resolve_symbol (gfc_symbol * sym)
{
  /* Zero if we are checking a formal namespace.  */
  static int formal_ns_flag = 1;
  int formal_ns_save, check_constant, mp_flag;
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

  if (sym->attr.flavor == FL_DERIVED && resolve_fl_derived (sym) == FAILURE)
    return;

  /* Symbols that are module procedures with results (functions) have
     the types and array specification copied for type checking in
     procedures that call them, as well as for saving to a module
     file.  These symbols can't stand the scrutiny that their results
     can.  */
  mp_flag = (sym->result != NULL && sym->result != sym);

  /* Assign default type to symbols that need one and don't have one.  */
  if (sym->ts.type == BT_UNKNOWN)
    {
      if (sym->attr.flavor == FL_VARIABLE || sym->attr.flavor == FL_PARAMETER)
	gfc_set_default_type (sym, 1, NULL);

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

	      sym->ts = sym->result->ts;
	      sym->as = gfc_copy_array_spec (sym->result->as);
	      sym->attr.dimension = sym->result->attr.dimension;
	      sym->attr.pointer = sym->result->attr.pointer;
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
      && (sym->attr.optional
	  || sym->attr.intent != INTENT_UNKNOWN))
    {
      gfc_error ("Symbol at %L is not a DUMMY variable", &sym->declared_at);
      return;
    }

  /* If a derived type symbol has reached this point, without its
     type being declared, we have an error.  Notice that most
     conditions that produce undefined derived types have already
     been dealt with.  However, the likes of:
     implicit type(t) (t) ..... call foo (t) will get us here if
     the type is not declared in the scope of the implicit
     statement. Change the type to BT_UNKNOWN, both because it is so
     and to prevent an ICE.  */
  if (sym->ts.type == BT_DERIVED
	&& sym->ts.derived->components == NULL)
    {
      gfc_error ("The derived type '%s' at %L is of type '%s', "
		 "which has not been defined.", sym->name,
		  &sym->declared_at, sym->ts.derived->name);
      sym->ts.type = BT_UNKNOWN;
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

  /* Make sure that intrinsic exist */
  if (sym->attr.intrinsic
      && ! gfc_intrinsic_name(sym->name, 0)
      && ! gfc_intrinsic_name(sym->name, 1))
    gfc_error("Intrinsic at %L does not exist", &sym->declared_at);

  /* Resolve array specifier. Check as well some constraints
     on COMMON blocks.  */

  check_constant = sym->attr.in_common && !sym->attr.pointer;
  gfc_resolve_array_spec (sym->as, check_constant);

  /* Resolve formal namespaces.  */

  if (formal_ns_flag && sym != NULL && sym->formal_ns != NULL)
    {
      formal_ns_save = formal_ns_flag;
      formal_ns_flag = 0;
      gfc_resolve (sym->formal_ns);
      formal_ns_flag = formal_ns_save;
    }

  /* If we have come this far we can apply default-initializers, as
     described in 14.7.5, to those variables that have not already
     been assigned one.  */
  if (sym->ts.type == BT_DERIVED
	&& sym->attr.referenced
	&& sym->ns == gfc_current_ns
	&& !sym->value
	&& !sym->attr.allocatable)
    {
      symbol_attribute *a = &sym->attr;

      if ((!a->save && !a->dummy && !a->pointer
		&& !a->in_common && !a->use_assoc
		&& !(a->function && sym != sym->result))
	     ||
	  (a->dummy && a->intent == INTENT_OUT))
	apply_default_init (sym);
    }
}



/************* Resolve DATA statements *************/

static struct
{
  gfc_data_value *vnode;
  unsigned int left;
}
values;


/* Advance the values structure to point to the next value in the data list.  */

static try
next_data_value (void)
{
  while (values.left == 0)
    {
      if (values.vnode->next == NULL)
	return FAILURE;

      values.vnode = values.vnode->next;
      values.left = values.vnode->repeat;
    }

  return SUCCESS;
}


static try
check_data_variable (gfc_data_variable * var, locus * where)
{
  gfc_expr *e;
  mpz_t size;
  mpz_t offset;
  try t;
  ar_type mark = AR_UNKNOWN;
  int i;
  mpz_t section_index[GFC_MAX_DIMENSIONS];
  gfc_ref *ref;
  gfc_array_ref *ar;

  if (gfc_resolve_expr (var->expr) == FAILURE)
    return FAILURE;

  ar = NULL;
  mpz_init_set_si (offset, 0);
  e = var->expr;

  if (e->expr_type != EXPR_VARIABLE)
    gfc_internal_error ("check_data_variable(): Bad expression");

  if (e->symtree->n.sym->ns->is_block_data
	&& !e->symtree->n.sym->attr.in_common)
    {
      gfc_error ("BLOCK DATA element '%s' at %L must be in COMMON",
	         e->symtree->n.sym->name, &e->symtree->n.sym->declared_at);
    }

  if (e->rank == 0)
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
      /* ??? Only done for full arrays for now, since array sections
	 seem tricky.  */
      if (mark == AR_FULL && ref && ref->next == NULL
	  && values.left > 1 && mpz_cmp_ui (size, 1) > 0)
	{
	  mpz_t range;

	  if (mpz_cmp_ui (size, values.left) >= 0)
	    {
	      mpz_init_set_ui (range, values.left);
	      mpz_sub_ui (size, size, values.left);
	      values.left = 0;
	    }
	  else
	    {
	      mpz_init_set (range, size);
	      values.left -= mpz_get_ui (size);
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
	  values.left -= 1;
	  mpz_sub_ui (size, size, 1);

	  gfc_assign_data_value (var->expr, values.vnode->expr, offset);

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


static try traverse_data_var (gfc_data_variable *, locus *);

/* Iterate over a list of elements in a DATA statement.  */

static try
traverse_data_list (gfc_data_variable * var, locus * where)
{
  mpz_t trip;
  iterator_stack frame;
  gfc_expr *e;

  mpz_init (frame.value);

  mpz_init_set (trip, var->iter.end->value.integer);
  mpz_sub (trip, trip, var->iter.start->value.integer);
  mpz_add (trip, trip, var->iter.step->value.integer);

  mpz_div (trip, trip, var->iter.step->value.integer);

  mpz_set (frame.value, var->iter.start->value.integer);

  frame.prev = iter_stack;
  frame.variable = var->iter.var->symtree;
  iter_stack = &frame;

  while (mpz_cmp_ui (trip, 0) > 0)
    {
      if (traverse_data_var (var->list, where) == FAILURE)
	{
	  mpz_clear (trip);
	  return FAILURE;
	}

      e = gfc_copy_expr (var->expr);
      if (gfc_simplify_expr (e, 1) == FAILURE)
        {
          gfc_free_expr (e);
          return FAILURE;
        }

      mpz_add (frame.value, frame.value, var->iter.step->value.integer);

      mpz_sub_ui (trip, trip, 1);
    }

  mpz_clear (trip);
  mpz_clear (frame.value);

  iter_stack = frame.prev;
  return SUCCESS;
}


/* Type resolve variables in the variable list of a DATA statement.  */

static try
traverse_data_var (gfc_data_variable * var, locus * where)
{
  try t;

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

static try
resolve_data_variables (gfc_data_variable * d)
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

	  if (d->iter.start->expr_type != EXPR_CONSTANT
	      || d->iter.end->expr_type != EXPR_CONSTANT
	      || d->iter.step->expr_type != EXPR_CONSTANT)
	    gfc_internal_error ("resolve_data_variables(): Bad iterator");

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
resolve_data (gfc_data * d)
{
  if (resolve_data_variables (d->var) == FAILURE)
    return;

  values.vnode = d->value;
  values.left = (d->value == NULL) ? 0 : d->value->repeat;

  if (traverse_data_var (d->var, &d->where) == FAILURE)
    return;

  /* At this point, we better not have any values left.  */

  if (next_data_value () == SUCCESS)
    gfc_error ("DATA statement at %L has more values than variables",
	       &d->where);
}


/* Determines if a variable is not 'pure', ie not assignable within a pure
   procedure.  Returns zero if assignment is OK, nonzero if there is a problem.
 */

int
gfc_impure_variable (gfc_symbol * sym)
{
  if (sym->attr.use_assoc || sym->attr.in_common)
    return 1;

  if (sym->ns != gfc_current_ns)
    return !sym->attr.function;

  /* TODO: Check storage association through EQUIVALENCE statements */

  return 0;
}


/* Test whether a symbol is pure or not.  For a NULL pointer, checks the
   symbol of the current procedure.  */

int
gfc_pure (gfc_symbol * sym)
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
gfc_elemental (gfc_symbol * sym)
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
warn_unused_fortran_label (gfc_namespace * ns)
{
  gfc_st_label *l;

  l = ns->st_labels;
  if (l == NULL)
    return;

  while (l->next)
    l = l->next;

  for (; l; l = l->prev)
    {
      if (l->defined == ST_LABEL_UNKNOWN)
	continue;

      switch (l->referenced)
	{
	case ST_LABEL_UNKNOWN:
	  gfc_warning ("Label %d at %L defined but not used", l->value,
		       &l->where);
	  break;

	case ST_LABEL_BAD_TARGET:
	  gfc_warning ("Label %d at %L defined but cannot be used", l->value,
		       &l->where);
	  break;

	default:
	  break;
	}
    }
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

static try
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
                 "attribute to be an EQUIVALENCE object", sym->name, &e->where);
      return FAILURE;
    }

  for (; c ; c = c->next)
    {
      d = c->ts.derived;
      if (d && (resolve_equivalence_derived (c->ts.derived, sym, e) == FAILURE))
        return FAILURE;
        
      /* Shall not be an object of sequence derived type containing a pointer
         in the structure.  */
      if (c->pointer)
        {
          gfc_error ("Derived type variable '%s' at %L with pointer component(s) "
                     "cannot be an EQUIVALENCE object", sym->name, &e->where);
          return FAILURE;
        }

      if (c->initializer)
        {
          gfc_error ("Derived type variable '%s' at %L with default initializer "
                     "cannot be an EQUIVALENCE object", sym->name, &e->where);
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
  int object;
  const char *value_name;
  const char *msg;

  value_name = NULL;
  last_ts = &eq->expr->symtree->n.sym->ts;

  first_sym = eq->expr->symtree->n.sym;

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

      /* An equivalence statement cannot have more than one initialized
	 object.  */
      if (sym->value)
	{
	  if (value_name != NULL)
	    {
	      gfc_error ("Initialized objects '%s' and '%s' cannot both "
			 "be in the EQUIVALENCE statement at %L",
			 value_name, sym->name, &e->where);
	      continue;
	    }
	  else
	    value_name = sym->name;
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
	       && gfc_notify_std (GFC_STD_GNU, msg, first_sym->name,
				  last_where) == FAILURE)
	   ||  (eq_type == SEQ_MIXED
	       && gfc_notify_std (GFC_STD_GNU, msg,sym->name,
				  &e->where) == FAILURE))
	continue;

      msg = "Non-default type object or sequence %s in EQUIVALENCE "
	    "statement at %L with objects of different type";
      if ((object ==2
	       && last_eq_type == SEQ_NONDEFAULT
	       && gfc_notify_std (GFC_STD_GNU, msg, first_sym->name,
				  last_where) == FAILURE)
	   ||  (eq_type == SEQ_NONDEFAULT
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


/* Resolve function and ENTRY types, issue diagnostics if needed. */

static void
resolve_fntype (gfc_namespace * ns)
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
      && !gfc_check_access (sym->ts.derived->attr.access,
                            sym->ts.derived->ns->default_access)
      && gfc_check_access (sym->attr.access, sym->ns->default_access))
    {
      gfc_error ("PUBLIC function '%s' at %L cannot be of PRIVATE type '%s'",
                 sym->name, &sym->declared_at, sym->ts.derived->name);
    }

  /* Make sure that the type of a module derived type function is in the
     module namespace, by copying it from the namespace's derived type
     list, if necessary.  */
  if (sym->ts.type == BT_DERIVED
	&& sym->ns->proc_name->attr.flavor == FL_MODULE
	&& sym->ts.derived->ns
	&& sym->ns != sym->ts.derived->ns)
    {
      gfc_dt_list *dt = sym->ns->derived_types;

      for (; dt; dt = dt->next)
        if (gfc_compare_derived_types (sym->ts.derived, dt->derived))
	  sym->ts.derived = dt->derived;
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
gfc_resolve_uops(gfc_symtree *symtree)
{
  gfc_interface *itr;
  gfc_symbol *sym;
  gfc_formal_arglist *formal;

  if (symtree == NULL) 
    return; 
 
  gfc_resolve_uops (symtree->left);
  gfc_resolve_uops (symtree->right);

  for (itr = symtree->n.uop->operator; itr; itr = itr->next)
    {
      sym = itr->sym;
      if (!sym->attr.function)
	gfc_error("User operator procedure '%s' at %L must be a FUNCTION",
		  sym->name, &sym->declared_at);

      if (sym->ts.type == BT_CHARACTER
	    && !(sym->ts.cl && sym->ts.cl->length)
	    && !(sym->result && sym->result->ts.cl && sym->result->ts.cl->length))
	gfc_error("User operator procedure '%s' at %L cannot be assumed character "
		  "length", sym->name, &sym->declared_at);

      formal = sym->formal;
      if (!formal || !formal->sym)
	{
	  gfc_error("User operator procedure '%s' at %L must have at least "
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
resolve_types (gfc_namespace * ns)
{
  gfc_namespace *n;
  gfc_charlen *cl;
  gfc_data *d;
  gfc_equiv *eq;

  gfc_current_ns = ns;

  resolve_entries (ns);

  resolve_contained_functions (ns);

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

  for (cl = ns->cl_list; cl; cl = cl->next)
    resolve_charlen (cl);

  gfc_traverse_ns (ns, resolve_values);

  if (ns->save_all)
    gfc_save_all (ns);

  iter_stack = NULL;
  for (d = ns->data; d; d = d->next)
    resolve_data (d);

  iter_stack = NULL;
  gfc_traverse_ns (ns, gfc_formalize_init_value);

  for (eq = ns->equiv; eq; eq = eq->next)
    resolve_equivalence (eq);

  /* Warn about unused labels.  */
  if (gfc_option.warn_unused_labels)
    warn_unused_fortran_label (ns);

  gfc_resolve_uops (ns->uop_root);
}


/* Call resolve_code recursively.  */

static void
resolve_codes (gfc_namespace * ns)
{
  gfc_namespace *n;

  for (n = ns->contained; n; n = n->sibling)
    resolve_codes (n);

  gfc_current_ns = ns;
  cs_base = NULL;
  /* Set to an out of range value.  */
  current_entry_id = -1;
  resolve_code (ns->code, ns);
}


/* This function is called after a complete program unit has been compiled.
   Its purpose is to examine all of the expressions associated with a program
   unit, assign types to all intermediate expressions, make sure that all
   assignments are to compatible types and figure out which names refer to
   which functions or subroutines.  */

void
gfc_resolve (gfc_namespace * ns)
{
  gfc_namespace *old_ns;

  old_ns = gfc_current_ns;

  resolve_types (ns);
  resolve_codes (ns);

  gfc_current_ns = old_ns;
}
