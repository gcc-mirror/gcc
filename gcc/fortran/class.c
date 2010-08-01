/* Implementation of Fortran 2003 Polymorphism.
   Copyright (C) 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Paul Richard Thomas & Janus Weil

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


/* class.c -- This file contains the front end functions needed to service
              the implementation of Fortran 2003 polymorphism and other
              object-oriented features.  */


/* Outline of the internal representation:

   Each CLASS variable is encapsulated by a class container, which is a
   structure with two fields:
    * $data: A pointer to the actual data of the variable. This field has the
             declared type of the class variable and its attributes
             (pointer/allocatable/dimension/...).
    * $vptr: A pointer to the vtable entry (see below) of the dynamic type.
    
   For each derived type we set up a "vtable" entry, i.e. a structure with the
   following fields:
    * $hash: A hash value serving as a unique identifier for this type.
    * $size: The size in bytes of the derived type.
    * $extends: A pointer to the vtable entry of the parent derived type.
   In addition to these fields, each vtable entry contains additional procedure
   pointer components, which contain pointers to the procedures which are bound
   to the type's "methods" (type-bound procedures).  */


#include "config.h"
#include "system.h"
#include "gfortran.h"
#include "constructor.h"


/* Insert a reference to the component of the given name.
   Only to be used with CLASS containers.  */

void
gfc_add_component_ref (gfc_expr *e, const char *name)
{
  gfc_ref **tail = &(e->ref);
  gfc_ref *next = NULL;
  gfc_symbol *derived = e->symtree->n.sym->ts.u.derived;
  while (*tail != NULL)
    {
      if ((*tail)->type == REF_COMPONENT)
	derived = (*tail)->u.c.component->ts.u.derived;
      if ((*tail)->type == REF_ARRAY && (*tail)->next == NULL)
	break;
      tail = &((*tail)->next);
    }
  if (*tail != NULL && strcmp (name, "$data") == 0)
    next = *tail;
  (*tail) = gfc_get_ref();
  (*tail)->next = next;
  (*tail)->type = REF_COMPONENT;
  (*tail)->u.c.sym = derived;
  (*tail)->u.c.component = gfc_find_component (derived, name, true, true);
  gcc_assert((*tail)->u.c.component);
  if (!next)
    e->ts = (*tail)->u.c.component->ts;
}


/* Build a NULL initializer for CLASS pointers,
   initializing the $data and $vptr components to zero.  */

gfc_expr *
gfc_class_null_initializer (gfc_typespec *ts)
{
  gfc_expr *init;
  gfc_component *comp;
  
  init = gfc_get_structure_constructor_expr (ts->type, ts->kind,
					     &ts->u.derived->declared_at);
  init->ts = *ts;
  
  for (comp = ts->u.derived->components; comp; comp = comp->next)
    {
      gfc_constructor *ctor = gfc_constructor_get();
      ctor->expr = gfc_get_expr ();
      ctor->expr->expr_type = EXPR_NULL;
      ctor->expr->ts = comp->ts;
      gfc_constructor_append (&init->value.constructor, ctor);
    }

  return init;
}


/* Build a polymorphic CLASS entity, using the symbol that comes from
   build_sym. A CLASS entity is represented by an encapsulating type,
   which contains the declared type as '$data' component, plus a pointer
   component '$vptr' which determines the dynamic type.  */

gfc_try
gfc_build_class_symbol (gfc_typespec *ts, symbol_attribute *attr,
			gfc_array_spec **as, bool delayed_vtab)
{
  char name[GFC_MAX_SYMBOL_LEN + 5];
  gfc_symbol *fclass;
  gfc_symbol *vtab;
  gfc_component *c;

  /* Determine the name of the encapsulating type.  */
  if ((*as) && (*as)->rank && attr->allocatable)
    sprintf (name, "class$%s_%d_a", ts->u.derived->name, (*as)->rank);
  else if ((*as) && (*as)->rank)
    sprintf (name, "class$%s_%d", ts->u.derived->name, (*as)->rank);
  else if (attr->pointer)
    sprintf (name, "class$%s_p", ts->u.derived->name);
  else if (attr->allocatable)
    sprintf (name, "class$%s_a", ts->u.derived->name);
  else
    sprintf (name, "class$%s", ts->u.derived->name);

  gfc_find_symbol (name, ts->u.derived->ns, 0, &fclass);
  if (fclass == NULL)
    {
      gfc_symtree *st;
      /* If not there, create a new symbol.  */
      fclass = gfc_new_symbol (name, ts->u.derived->ns);
      st = gfc_new_symtree (&ts->u.derived->ns->sym_root, name);
      st->n.sym = fclass;
      gfc_set_sym_referenced (fclass);
      fclass->refs++;
      fclass->ts.type = BT_UNKNOWN;
      fclass->attr.abstract = ts->u.derived->attr.abstract;
      if (ts->u.derived->f2k_derived)
	fclass->f2k_derived = gfc_get_namespace (NULL, 0);
      if (gfc_add_flavor (&fclass->attr, FL_DERIVED,
	  NULL, &gfc_current_locus) == FAILURE)
	return FAILURE;

      /* Add component '$data'.  */
      if (gfc_add_component (fclass, "$data", &c) == FAILURE)
	return FAILURE;
      c->ts = *ts;
      c->ts.type = BT_DERIVED;
      c->attr.access = ACCESS_PRIVATE;
      c->ts.u.derived = ts->u.derived;
      c->attr.class_pointer = attr->pointer;
      c->attr.pointer = attr->pointer || attr->dummy;
      c->attr.allocatable = attr->allocatable;
      c->attr.dimension = attr->dimension;
      c->attr.codimension = attr->codimension;
      c->attr.abstract = ts->u.derived->attr.abstract;
      c->as = (*as);
      c->initializer = NULL;

      /* Add component '$vptr'.  */
      if (gfc_add_component (fclass, "$vptr", &c) == FAILURE)
	return FAILURE;
      c->ts.type = BT_DERIVED;
      if (delayed_vtab)
	c->ts.u.derived = NULL;
      else
	{
	  vtab = gfc_find_derived_vtab (ts->u.derived);
	  gcc_assert (vtab);
	  c->ts.u.derived = vtab->ts.u.derived;
	}
      c->attr.access = ACCESS_PRIVATE;
      c->attr.pointer = 1;
    }

  /* Since the extension field is 8 bit wide, we can only have
     up to 255 extension levels.  */
  if (ts->u.derived->attr.extension == 255)
    {
      gfc_error ("Maximum extension level reached with type '%s' at %L",
		 ts->u.derived->name, &ts->u.derived->declared_at);
      return FAILURE;
    }
    
  fclass->attr.extension = ts->u.derived->attr.extension + 1;
  fclass->attr.is_class = 1;
  ts->u.derived = fclass;
  attr->allocatable = attr->pointer = attr->dimension = 0;
  (*as) = NULL;  /* XXX */
  return SUCCESS;
}


/* Add a procedure pointer component to the vtype
   to represent a specific type-bound procedure.  */

static void
add_proc_comp (gfc_symbol *vtype, const char *name, gfc_typebound_proc *tb)
{
  gfc_component *c;
  c = gfc_find_component (vtype, name, true, true);

  if (c == NULL)
    {
      /* Add procedure component.  */
      if (gfc_add_component (vtype, name, &c) == FAILURE)
	return;
      if (tb->u.specific)
	c->ts.interface = tb->u.specific->n.sym;

      if (!c->tb)
	c->tb = XCNEW (gfc_typebound_proc);
      *c->tb = *tb;
      c->tb->ppc = 1;
      c->attr.procedure = 1;
      c->attr.proc_pointer = 1;
      c->attr.flavor = FL_PROCEDURE;
      c->attr.access = ACCESS_PRIVATE;
      c->attr.external = 1;
      c->attr.untyped = 1;
      c->attr.if_source = IFSRC_IFBODY;

      /* A static initializer cannot be used here because the specific
	function is not a constant; internal compiler error: in
	output_constant, at varasm.c:4623  */
      c->initializer = NULL;
    }
  else if (c->attr.proc_pointer && c->tb)
    {
      *c->tb = *tb;
      c->tb->ppc = 1;
      c->ts.interface = tb->u.specific->n.sym;	  
    }
}


/* Add all specific type-bound procedures in the symtree 'st' to a vtype.  */

static void
add_procs_to_declared_vtab1 (gfc_symtree *st, gfc_symbol *vtype)
{
  if (!st)
    return;

  if (st->left)
    add_procs_to_declared_vtab1 (st->left, vtype);

  if (st->right)
    add_procs_to_declared_vtab1 (st->right, vtype);

  if (!st->n.tb)
    return;

  if (!st->n.tb->is_generic && st->n.tb->u.specific)
    add_proc_comp (vtype, st->name, st->n.tb);
}


/* Copy procedure pointers components from the parent type.  */

static void
copy_vtab_proc_comps (gfc_symbol *declared, gfc_symbol *vtype)
{
  gfc_component *cmp;
  gfc_symbol *vtab;

  vtab = gfc_find_derived_vtab (declared);

  for (cmp = vtab->ts.u.derived->components; cmp; cmp = cmp->next)
    {
      if (gfc_find_component (vtype, cmp->name, true, true))
	continue;

      add_proc_comp (vtype, cmp->name, cmp->tb);
    }
}


/* Add procedure pointers for all type-bound procedures to a vtab.  */

static void
add_procs_to_declared_vtab (gfc_symbol *derived, gfc_symbol *vtype)
{
  gfc_symbol* super_type;

  super_type = gfc_get_derived_super_type (derived);

  if (super_type && (super_type != derived))
    {
      /* Make sure that the PPCs appear in the same order as in the parent.  */
      copy_vtab_proc_comps (super_type, vtype);
      /* Only needed to get the PPC interfaces right.  */
      add_procs_to_declared_vtab (super_type, vtype);
    }

  if (derived->f2k_derived && derived->f2k_derived->tb_sym_root)
    add_procs_to_declared_vtab1 (derived->f2k_derived->tb_sym_root, vtype);

  if (derived->f2k_derived && derived->f2k_derived->tb_uop_root)
    add_procs_to_declared_vtab1 (derived->f2k_derived->tb_uop_root, vtype);
}


/* Find the symbol for a derived type's vtab.
   A vtab has the following fields:
    * $hash	a hash value used to identify the derived type
    * $size	the size in bytes of the derived type
    * $extends	a pointer to the vtable of the parent derived type
   After these follow procedure pointer components for the
   specific type-bound procedures.  */

gfc_symbol *
gfc_find_derived_vtab (gfc_symbol *derived)
{
  gfc_namespace *ns;
  gfc_symbol *vtab = NULL, *vtype = NULL, *found_sym = NULL;
  char name[2 * GFC_MAX_SYMBOL_LEN + 8];

  ns = gfc_current_ns;

  for (; ns; ns = ns->parent)
    if (!ns->parent)
      break;

  if (ns)
    {
      sprintf (name, "vtab$%s", derived->name);
      gfc_find_symbol (name, ns, 0, &vtab);

      if (vtab == NULL)
	{
	  gfc_get_symbol (name, ns, &vtab);
	  vtab->ts.type = BT_DERIVED;
	  vtab->attr.flavor = FL_VARIABLE;
	  vtab->attr.target = 1;
	  vtab->attr.save = SAVE_EXPLICIT;
	  vtab->attr.vtab = 1;
	  vtab->attr.access = ACCESS_PUBLIC;
	  vtab->refs++;
	  gfc_set_sym_referenced (vtab);
	  sprintf (name, "vtype$%s", derived->name);
	  
	  gfc_find_symbol (name, ns, 0, &vtype);
	  if (vtype == NULL)
	    {
	      gfc_component *c;
	      gfc_symbol *parent = NULL, *parent_vtab = NULL;

	      gfc_get_symbol (name, ns, &vtype);
	      if (gfc_add_flavor (&vtype->attr, FL_DERIVED,
				  NULL, &gfc_current_locus) == FAILURE)
		goto cleanup;
	      vtype->attr.access = ACCESS_PUBLIC;
	      vtype->refs++;
	      gfc_set_sym_referenced (vtype);

	      /* Add component '$hash'.  */
	      if (gfc_add_component (vtype, "$hash", &c) == FAILURE)
		goto cleanup;
	      c->ts.type = BT_INTEGER;
	      c->ts.kind = 4;
	      c->attr.access = ACCESS_PRIVATE;
	      c->initializer = gfc_get_int_expr (gfc_default_integer_kind,
						 NULL, derived->hash_value);

	      /* Add component '$size'.  */
	      if (gfc_add_component (vtype, "$size", &c) == FAILURE)
		goto cleanup;
	      c->ts.type = BT_INTEGER;
	      c->ts.kind = 4;
	      c->attr.access = ACCESS_PRIVATE;
	      /* Remember the derived type in ts.u.derived,
		 so that the correct initializer can be set later on
		 (in gfc_conv_structure).  */
	      c->ts.u.derived = derived;
	      c->initializer = gfc_get_int_expr (gfc_default_integer_kind,
						 NULL, 0);

	      /* Add component $extends.  */
	      if (gfc_add_component (vtype, "$extends", &c) == FAILURE)
		goto cleanup;
	      c->attr.pointer = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      parent = gfc_get_derived_super_type (derived);
	      if (parent)
		{
		  parent_vtab = gfc_find_derived_vtab (parent);
		  c->ts.type = BT_DERIVED;
		  c->ts.u.derived = parent_vtab->ts.u.derived;
		  c->initializer = gfc_get_expr ();
		  c->initializer->expr_type = EXPR_VARIABLE;
		  gfc_find_sym_tree (parent_vtab->name, parent_vtab->ns,
				     0, &c->initializer->symtree);
		}
	      else
		{
		  c->ts.type = BT_DERIVED;
		  c->ts.u.derived = vtype;
		  c->initializer = gfc_get_null_expr (NULL);
		}

	      add_procs_to_declared_vtab (derived, vtype);
	      vtype->attr.vtype = 1;
	    }

	  vtab->ts.u.derived = vtype;
	  vtab->value = gfc_default_initializer (&vtab->ts);
	}
    }

  found_sym = vtab;

cleanup:
  /* It is unexpected to have some symbols added at resolution or code
     generation time. We commit the changes in order to keep a clean state.  */
  if (found_sym)
    gfc_commit_symbols ();
  else
    gfc_undo_symbols ();

  return found_sym;
}


/* General worker function to find either a type-bound procedure or a
   type-bound user operator.  */

static gfc_symtree*
find_typebound_proc_uop (gfc_symbol* derived, gfc_try* t,
			 const char* name, bool noaccess, bool uop,
			 locus* where)
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
  if (res && res->n.tb && !res->n.tb->error)
    {
      /* We found one.  */
      if (t)
	*t = SUCCESS;

      if (!noaccess && derived->attr.use_assoc
	  && res->n.tb->access == ACCESS_PRIVATE)
	{
	  if (where)
	    gfc_error ("'%s' of '%s' is PRIVATE at %L",
		       name, derived->name, where);
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

      return find_typebound_proc_uop (super_type, t, name,
				      noaccess, uop, where);
    }

  /* Nothing found.  */
  return NULL;
}


/* Find a type-bound procedure or user operator by name for a derived-type
   (looking recursively through the super-types).  */

gfc_symtree*
gfc_find_typebound_proc (gfc_symbol* derived, gfc_try* t,
			 const char* name, bool noaccess, locus* where)
{
  return find_typebound_proc_uop (derived, t, name, noaccess, false, where);
}

gfc_symtree*
gfc_find_typebound_user_op (gfc_symbol* derived, gfc_try* t,
			    const char* name, bool noaccess, locus* where)
{
  return find_typebound_proc_uop (derived, t, name, noaccess, true, where);
}


/* Find a type-bound intrinsic operator looking recursively through the
   super-type hierarchy.  */

gfc_typebound_proc*
gfc_find_typebound_intrinsic_op (gfc_symbol* derived, gfc_try* t,
				 gfc_intrinsic_op op, bool noaccess,
				 locus* where)
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
  if (res && !res->error)
    {
      /* We found one.  */
      if (t)
	*t = SUCCESS;

      if (!noaccess && derived->attr.use_assoc
	  && res->access == ACCESS_PRIVATE)
	{
	  if (where)
	    gfc_error ("'%s' of '%s' is PRIVATE at %L",
		       gfc_op2string (op), derived->name, where);
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

      return gfc_find_typebound_intrinsic_op (super_type, t, op,
					      noaccess, where);
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
