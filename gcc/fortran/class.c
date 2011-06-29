/* Implementation of Fortran 2003 Polymorphism.
   Copyright (C) 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Paul Richard Thomas <pault@gcc.gnu.org>
   and Janus Weil <janus@gcc.gnu.org>

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
    * _data: A pointer to the actual data of the variable. This field has the
             declared type of the class variable and its attributes
             (pointer/allocatable/dimension/...).
    * _vptr: A pointer to the vtable entry (see below) of the dynamic type.
    
   For each derived type we set up a "vtable" entry, i.e. a structure with the
   following fields:
    * _hash:     A hash value serving as a unique identifier for this type.
    * _size:     The size in bytes of the derived type.
    * _extends:  A pointer to the vtable entry of the parent derived type.
    * _def_init: A pointer to a default initialized variable of this type.
    * _copy:     A procedure pointer to a copying procedure.
   After these follow procedure pointer components for the specific
   type-bound procedures.  */


#include "config.h"
#include "system.h"
#include "gfortran.h"
#include "constructor.h"


/* Insert a reference to the component of the given name.
   Only to be used with CLASS containers and vtables.  */

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
  if (*tail != NULL && strcmp (name, "_data") == 0)
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
   initializing the _data component to NULL and
   the _vptr component to the declared type.  */

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
      if (strcmp (comp->name, "_vptr") == 0)
	ctor->expr = gfc_lval_expr_from_sym (gfc_find_derived_vtab (ts->u.derived));
      else
	ctor->expr = gfc_get_null_expr (NULL);
      gfc_constructor_append (&init->value.constructor, ctor);
    }

  return init;
}


/* Create a unique string identifier for a derived type, composed of its name
   and module name. This is used to construct unique names for the class
   containers and vtab symbols.  */

static void
get_unique_type_string (char *string, gfc_symbol *derived)
{
  char dt_name[GFC_MAX_SYMBOL_LEN+1];
  sprintf (dt_name, "%s", derived->name);
  dt_name[0] = TOUPPER (dt_name[0]);
  if (derived->module)
    sprintf (string, "%s_%s", derived->module, dt_name);
  else if (derived->ns->proc_name)
    sprintf (string, "%s_%s", derived->ns->proc_name->name, dt_name);
  else
    sprintf (string, "_%s", dt_name);
}


/* A relative of 'get_unique_type_string' which makes sure the generated
   string will not be too long (replacing it by a hash string if needed).  */

static void
get_unique_hashed_string (char *string, gfc_symbol *derived)
{
  char tmp[2*GFC_MAX_SYMBOL_LEN+2];
  get_unique_type_string (&tmp[0], derived);
  /* If string is too long, use hash value in hex representation (allow for
     extra decoration, cf. gfc_build_class_symbol & gfc_find_derived_vtab).  */
  if (strlen (tmp) > GFC_MAX_SYMBOL_LEN - 11)
    {
      int h = gfc_hash_value (derived);
      sprintf (string, "%X", h);
    }
  else
    strcpy (string, tmp);
}


/* Assign a hash value for a derived type. The algorithm is that of SDBM.  */

unsigned int
gfc_hash_value (gfc_symbol *sym)
{
  unsigned int hash = 0;
  char c[2*(GFC_MAX_SYMBOL_LEN+1)];
  int i, len;
  
  get_unique_type_string (&c[0], sym);
  len = strlen (c);
  
  for (i = 0; i < len; i++)
    hash = (hash << 6) + (hash << 16) - hash + c[i];

  /* Return the hash but take the modulus for the sake of module read,
     even though this slightly increases the chance of collision.  */
  return (hash % 100000000);
}


/* Build a polymorphic CLASS entity, using the symbol that comes from
   build_sym. A CLASS entity is represented by an encapsulating type,
   which contains the declared type as '_data' component, plus a pointer
   component '_vptr' which determines the dynamic type.  */

gfc_try
gfc_build_class_symbol (gfc_typespec *ts, symbol_attribute *attr,
			gfc_array_spec **as, bool delayed_vtab)
{
  char name[GFC_MAX_SYMBOL_LEN+1], tname[GFC_MAX_SYMBOL_LEN+1];
  gfc_symbol *fclass;
  gfc_symbol *vtab;
  gfc_component *c;
  
  if (attr->class_ok)
    /* Class container has already been built.  */
    return SUCCESS;

  attr->class_ok = attr->dummy || attr->pointer  || attr->allocatable;
  
  if (!attr->class_ok)
    /* We can not build the class container yet.  */
    return SUCCESS;

  if (*as)
    {
      gfc_fatal_error ("Polymorphic array at %C not yet supported");
      return FAILURE;
    }

  /* Determine the name of the encapsulating type.  */
  get_unique_hashed_string (tname, ts->u.derived);
  if ((*as) && (*as)->rank && attr->allocatable)
    sprintf (name, "__class_%s_%d_a", tname, (*as)->rank);
  else if ((*as) && (*as)->rank)
    sprintf (name, "__class_%s_%d", tname, (*as)->rank);
  else if (attr->pointer)
    sprintf (name, "__class_%s_p", tname);
  else if (attr->allocatable)
    sprintf (name, "__class_%s_a", tname);
  else
    sprintf (name, "__class_%s", tname);

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

      /* Add component '_data'.  */
      if (gfc_add_component (fclass, "_data", &c) == FAILURE)
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

      /* Add component '_vptr'.  */
      if (gfc_add_component (fclass, "_vptr", &c) == FAILURE)
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
    }
  else if (c->attr.proc_pointer && c->tb)
    {
      *c->tb = *tb;
      c->tb->ppc = 1;
    }

  if (tb->u.specific)
    {
      c->ts.interface = tb->u.specific->n.sym;
      if (!tb->deferred)
	c->initializer = gfc_get_variable_expr (tb->u.specific);
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

  if (st->n.tb && !st->n.tb->error 
      && !st->n.tb->is_generic && st->n.tb->u.specific)
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
      /* Only needed to get the PPC initializers right.  */
      add_procs_to_declared_vtab (super_type, vtype);
    }

  if (derived->f2k_derived && derived->f2k_derived->tb_sym_root)
    add_procs_to_declared_vtab1 (derived->f2k_derived->tb_sym_root, vtype);

  if (derived->f2k_derived && derived->f2k_derived->tb_uop_root)
    add_procs_to_declared_vtab1 (derived->f2k_derived->tb_uop_root, vtype);
}


/* Find (or generate) the symbol for a derived type's vtab.  */

gfc_symbol *
gfc_find_derived_vtab (gfc_symbol *derived)
{
  gfc_namespace *ns;
  gfc_symbol *vtab = NULL, *vtype = NULL, *found_sym = NULL, *def_init = NULL;
  gfc_symbol *copy = NULL, *src = NULL, *dst = NULL;
  
  /* Find the top-level namespace (MODULE or PROGRAM).  */
  for (ns = gfc_current_ns; ns; ns = ns->parent)
    if (!ns->parent)
      break;

  /* If the type is a class container, use the underlying derived type.  */
  if (derived->attr.is_class)
    derived = gfc_get_derived_super_type (derived);
    
  if (ns)
    {
      char name[GFC_MAX_SYMBOL_LEN+1], tname[GFC_MAX_SYMBOL_LEN+1];
      
      get_unique_hashed_string (tname, derived);
      sprintf (name, "__vtab_%s", tname);

      /* Look for the vtab symbol in various namespaces.  */
      gfc_find_symbol (name, gfc_current_ns, 0, &vtab);
      if (vtab == NULL)
	gfc_find_symbol (name, ns, 0, &vtab);
      if (vtab == NULL)
	gfc_find_symbol (name, derived->ns, 0, &vtab);

      if (vtab == NULL)
	{
	  gfc_get_symbol (name, ns, &vtab);
	  vtab->ts.type = BT_DERIVED;
	  if (gfc_add_flavor (&vtab->attr, FL_VARIABLE, NULL,
	                      &gfc_current_locus) == FAILURE)
	    goto cleanup;
	  vtab->attr.target = 1;
	  vtab->attr.save = SAVE_IMPLICIT;
	  vtab->attr.vtab = 1;
	  vtab->attr.access = ACCESS_PUBLIC;
	  gfc_set_sym_referenced (vtab);
	  sprintf (name, "__vtype_%s", tname);
	  
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
	      vtype->attr.vtype = 1;
	      gfc_set_sym_referenced (vtype);

	      /* Add component '_hash'.  */
	      if (gfc_add_component (vtype, "_hash", &c) == FAILURE)
		goto cleanup;
	      c->ts.type = BT_INTEGER;
	      c->ts.kind = 4;
	      c->attr.access = ACCESS_PRIVATE;
	      c->initializer = gfc_get_int_expr (gfc_default_integer_kind,
						 NULL, derived->hash_value);

	      /* Add component '_size'.  */
	      if (gfc_add_component (vtype, "_size", &c) == FAILURE)
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

	      /* Add component _extends.  */
	      if (gfc_add_component (vtype, "_extends", &c) == FAILURE)
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

	      if (derived->components == NULL && !derived->attr.zero_comp)
		{
		  /* At this point an error must have occurred.
		     Prevent further errors on the vtype components.  */
		  found_sym = vtab;
		  goto have_vtype;
		}

	      /* Add component _def_init.  */
	      if (gfc_add_component (vtype, "_def_init", &c) == FAILURE)
		goto cleanup;
	      c->attr.pointer = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      c->ts.type = BT_DERIVED;
	      c->ts.u.derived = derived;
	      if (derived->attr.abstract)
		c->initializer = gfc_get_null_expr (NULL);
	      else
		{
		  /* Construct default initialization variable.  */
		  sprintf (name, "__def_init_%s", tname);
		  gfc_get_symbol (name, ns, &def_init);
		  def_init->attr.target = 1;
		  def_init->attr.save = SAVE_IMPLICIT;
		  def_init->attr.access = ACCESS_PUBLIC;
		  def_init->attr.flavor = FL_VARIABLE;
		  gfc_set_sym_referenced (def_init);
		  def_init->ts.type = BT_DERIVED;
		  def_init->ts.u.derived = derived;
		  def_init->value = gfc_default_initializer (&def_init->ts);

		  c->initializer = gfc_lval_expr_from_sym (def_init);
		}

	      /* Add component _copy.  */
	      if (gfc_add_component (vtype, "_copy", &c) == FAILURE)
		goto cleanup;
	      c->attr.proc_pointer = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      c->tb = XCNEW (gfc_typebound_proc);
	      c->tb->ppc = 1;
	      if (derived->attr.abstract)
		c->initializer = gfc_get_null_expr (NULL);
	      else
		{
		  /* Set up namespace.  */
		  gfc_namespace *sub_ns = gfc_get_namespace (ns, 0);
		  sub_ns->sibling = ns->contained;
		  ns->contained = sub_ns;
		  sub_ns->resolved = 1;
		  /* Set up procedure symbol.  */
		  sprintf (name, "__copy_%s", tname);
		  gfc_get_symbol (name, sub_ns, &copy);
		  sub_ns->proc_name = copy;
		  copy->attr.flavor = FL_PROCEDURE;
		  copy->attr.if_source = IFSRC_DECL;
		  if (ns->proc_name->attr.flavor == FL_MODULE)
		    copy->module = ns->proc_name->name;
		  gfc_set_sym_referenced (copy);
		  /* Set up formal arguments.  */
		  gfc_get_symbol ("src", sub_ns, &src);
		  src->ts.type = BT_DERIVED;
		  src->ts.u.derived = derived;
		  src->attr.flavor = FL_VARIABLE;
		  src->attr.dummy = 1;
		  gfc_set_sym_referenced (src);
		  copy->formal = gfc_get_formal_arglist ();
		  copy->formal->sym = src;
		  gfc_get_symbol ("dst", sub_ns, &dst);
		  dst->ts.type = BT_DERIVED;
		  dst->ts.u.derived = derived;
		  dst->attr.flavor = FL_VARIABLE;
		  dst->attr.dummy = 1;
		  gfc_set_sym_referenced (dst);
		  copy->formal->next = gfc_get_formal_arglist ();
		  copy->formal->next->sym = dst;
		  /* Set up code.  */
		  sub_ns->code = gfc_get_code ();
		  sub_ns->code->op = EXEC_INIT_ASSIGN;
		  sub_ns->code->expr1 = gfc_lval_expr_from_sym (dst);
		  sub_ns->code->expr2 = gfc_lval_expr_from_sym (src);
		  /* Set initializer.  */
		  c->initializer = gfc_lval_expr_from_sym (copy);
		  c->ts.interface = copy;
		}

	      /* Add procedure pointers for type-bound procedures.  */
	      add_procs_to_declared_vtab (derived, vtype);
	    }

have_vtype:
	  vtab->ts.u.derived = vtype;
	  vtab->value = gfc_default_initializer (&vtab->ts);
	}
    }

  found_sym = vtab;

cleanup:
  /* It is unexpected to have some symbols added at resolution or code
     generation time. We commit the changes in order to keep a clean state.  */
  if (found_sym)
    {
      gfc_commit_symbol (vtab);
      if (vtype)
	gfc_commit_symbol (vtype);
      if (def_init)
	gfc_commit_symbol (def_init);
      if (copy)
	gfc_commit_symbol (copy);
      if (src)
	gfc_commit_symbol (src);
      if (dst)
	gfc_commit_symbol (dst);
    }
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
