/* Implementation of Fortran 2003 Polymorphism.
   Copyright (C) 2009-2016 Free Software Foundation, Inc.
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

    Only for unlimited polymorphic classes:
    * _len:  An integer(4) to store the string length when the unlimited
             polymorphic pointer is used to point to a char array.  The '_len'
             component will be zero when no character array is stored in
             '_data'.

   For each derived type we set up a "vtable" entry, i.e. a structure with the
   following fields:
    * _hash:     A hash value serving as a unique identifier for this type.
    * _size:     The size in bytes of the derived type.
    * _extends:  A pointer to the vtable entry of the parent derived type.
    * _def_init: A pointer to a default initialized variable of this type.
    * _copy:     A procedure pointer to a copying procedure.
    * _final:    A procedure pointer to a wrapper function, which frees
		 allocatable components and calls FINAL subroutines.

   After these follow procedure pointer components for the specific
   type-bound procedures.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gfortran.h"
#include "constructor.h"
#include "target-memory.h"

/* Inserts a derived type component reference in a data reference chain.
    TS: base type of the ref chain so far, in which we will pick the component
    REF: the address of the GFC_REF pointer to update
    NAME: name of the component to insert
   Note that component insertion makes sense only if we are at the end of
   the chain (*REF == NULL) or if we are adding a missing "_data" component
   to access the actual contents of a class object.  */

static void
insert_component_ref (gfc_typespec *ts, gfc_ref **ref, const char * const name)
{
  gfc_symbol *type_sym;
  gfc_ref *new_ref;

  gcc_assert (ts->type == BT_DERIVED || ts->type == BT_CLASS);
  type_sym = ts->u.derived;

  gfc_find_component (type_sym, name, true, true, &new_ref);
  gcc_assert (new_ref->u.c.component);
  while (new_ref->next)
    new_ref = new_ref->next;
  new_ref->next = *ref;

  if (new_ref->next)
    {
      gfc_ref *next = NULL;

      /* We need to update the base type in the trailing reference chain to
	 that of the new component.  */

      gcc_assert (strcmp (name, "_data") == 0);

      if (new_ref->next->type == REF_COMPONENT)
	next = new_ref->next;
      else if (new_ref->next->type == REF_ARRAY
	       && new_ref->next->next
	       && new_ref->next->next->type == REF_COMPONENT)
	next = new_ref->next->next;

      if (next != NULL)
	{
	  gcc_assert (new_ref->u.c.component->ts.type == BT_CLASS
		      || new_ref->u.c.component->ts.type == BT_DERIVED);
	  next->u.c.sym = new_ref->u.c.component->ts.u.derived;
	}
    }

  *ref = new_ref;
}


/* Tells whether we need to add a "_data" reference to access REF subobject
   from an object of type TS.  If FIRST_REF_IN_CHAIN is set, then the base
   object accessed by REF is a variable; in other words it is a full object,
   not a subobject.  */

static bool
class_data_ref_missing (gfc_typespec *ts, gfc_ref *ref, bool first_ref_in_chain)
{
  /* Only class containers may need the "_data" reference.  */
  if (ts->type != BT_CLASS)
    return false;

  /* Accessing a class container with an array reference is certainly wrong.  */
  if (ref->type != REF_COMPONENT)
    return true;

  /* Accessing the class container's fields is fine.  */
  if (ref->u.c.component->name[0] == '_')
    return false;

  /* At this point we have a class container with a non class container's field
     component reference.  We don't want to add the "_data" component if we are
     at the first reference and the symbol's type is an extended derived type.
     In that case, conv_parent_component_references will do the right thing so
     it is not absolutely necessary.  Omitting it prevents a regression (see
     class_41.f03) in the interface mapping mechanism.  When evaluating string
     lengths depending on dummy arguments, we create a fake symbol with a type
     equal to that of the dummy type.  However, because of type extension,
     the backend type (corresponding to the actual argument) can have a
     different (extended) type.  Adding the "_data" component explicitly, using
     the base type, confuses the gfc_conv_component_ref code which deals with
     the extended type.  */
  if (first_ref_in_chain && ts->u.derived->attr.extension)
    return false;

  /* We have a class container with a non class container's field component
     reference that doesn't fall into the above.  */
  return true;
}


/* Browse through a data reference chain and add the missing "_data" references
   when a subobject of a class object is accessed without it.
   Note that it doesn't add the "_data" reference when the class container
   is the last element in the reference chain.  */

void
gfc_fix_class_refs (gfc_expr *e)
{
  gfc_typespec *ts;
  gfc_ref **ref;

  if ((e->expr_type != EXPR_VARIABLE
       && e->expr_type != EXPR_FUNCTION)
      || (e->expr_type == EXPR_FUNCTION
	  && e->value.function.isym != NULL))
    return;

  if (e->expr_type == EXPR_VARIABLE)
    ts = &e->symtree->n.sym->ts;
  else
    {
      gfc_symbol *func;

      gcc_assert (e->expr_type == EXPR_FUNCTION);
      if (e->value.function.esym != NULL)
	func = e->value.function.esym;
      else
	func = e->symtree->n.sym;

      if (func->result != NULL)
	ts = &func->result->ts;
      else
	ts = &func->ts;
    }

  for (ref = &e->ref; *ref != NULL; ref = &(*ref)->next)
    {
      if (class_data_ref_missing (ts, *ref, ref == &e->ref))
	insert_component_ref (ts, ref, "_data");

      if ((*ref)->type == REF_COMPONENT)
	ts = &(*ref)->u.c.component->ts;
    }
}


/* Insert a reference to the component of the given name.
   Only to be used with CLASS containers and vtables.  */

void
gfc_add_component_ref (gfc_expr *e, const char *name)
{
  gfc_component *c;
  gfc_ref **tail = &(e->ref);
  gfc_ref *ref, *next = NULL;
  gfc_symbol *derived = e->symtree->n.sym->ts.u.derived;
  while (*tail != NULL)
    {
      if ((*tail)->type == REF_COMPONENT)
	{
	  if (strcmp ((*tail)->u.c.component->name, "_data") == 0
		&& (*tail)->next
		&& (*tail)->next->type == REF_ARRAY
		&& (*tail)->next->next == NULL)
	    return;
	  derived = (*tail)->u.c.component->ts.u.derived;
	}
      if ((*tail)->type == REF_ARRAY && (*tail)->next == NULL)
	break;
      tail = &((*tail)->next);
    }
  if (derived->components->next->ts.type == BT_DERIVED &&
      derived->components->next->ts.u.derived == NULL)
    {
      /* Fix up missing vtype.  */
      gfc_symbol *vtab = gfc_find_derived_vtab (derived->components->ts.u.derived);
      gcc_assert (vtab);
      derived->components->next->ts.u.derived = vtab->ts.u.derived;
    }
  if (*tail != NULL && strcmp (name, "_data") == 0)
    next = *tail;
  else
    /* Avoid losing memory.  */
    gfc_free_ref_list (*tail);
  c = gfc_find_component (derived, name, true, true, tail);

  if (c) {
    for (ref = *tail; ref->next; ref = ref->next)
      ;
    ref->next = next;
    if (!next)
      e->ts = c->ts;
  }
}


/* This is used to add both the _data component reference and an array
   reference to class expressions.  Used in translation of intrinsic
   array inquiry functions.  */

void
gfc_add_class_array_ref (gfc_expr *e)
{
  int rank = CLASS_DATA (e)->as->rank;
  gfc_array_spec *as = CLASS_DATA (e)->as;
  gfc_ref *ref = NULL;
  gfc_add_data_component (e);
  e->rank = rank;
  for (ref = e->ref; ref; ref = ref->next)
    if (!ref->next)
      break;
  if (ref->type != REF_ARRAY)
    {
      ref->next = gfc_get_ref ();
      ref = ref->next;
      ref->type = REF_ARRAY;
      ref->u.ar.type = AR_FULL;
      ref->u.ar.as = as;
    }
}


/* Unfortunately, class array expressions can appear in various conditions;
   with and without both _data component and an arrayspec.  This function
   deals with that variability.  The previous reference to 'ref' is to a
   class array.  */

static bool
class_array_ref_detected (gfc_ref *ref, bool *full_array)
{
  bool no_data = false;
  bool with_data = false;

  /* An array reference with no _data component.  */
  if (ref && ref->type == REF_ARRAY
	&& !ref->next
	&& ref->u.ar.type != AR_ELEMENT)
    {
      if (full_array)
        *full_array = ref->u.ar.type == AR_FULL;
      no_data = true;
    }

  /* Cover cases where _data appears, with or without an array ref.  */
  if (ref && ref->type == REF_COMPONENT
	&& strcmp (ref->u.c.component->name, "_data") == 0)
    {
      if (!ref->next)
	{
	  with_data = true;
	  if (full_array)
	    *full_array = true;
	}
      else if (ref->next && ref->next->type == REF_ARRAY
	    && !ref->next->next
	    && ref->type == REF_COMPONENT
	    && ref->next->type == REF_ARRAY
	    && ref->next->u.ar.type != AR_ELEMENT)
	{
	  with_data = true;
	  if (full_array)
	    *full_array = ref->next->u.ar.type == AR_FULL;
	}
    }

  return no_data || with_data;
}


/* Returns true if the expression contains a reference to a class
   array.  Notice that class array elements return false.  */

bool
gfc_is_class_array_ref (gfc_expr *e, bool *full_array)
{
  gfc_ref *ref;

  if (!e->rank)
    return false;

  if (full_array)
    *full_array= false;

  /* Is this a class array object? ie. Is the symbol of type class?  */
  if (e->symtree
	&& e->symtree->n.sym->ts.type == BT_CLASS
	&& CLASS_DATA (e->symtree->n.sym)
	&& CLASS_DATA (e->symtree->n.sym)->attr.dimension
	&& class_array_ref_detected (e->ref, full_array))
    return true;

  /* Or is this a class array component reference?  */
  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT
	    && ref->u.c.component->ts.type == BT_CLASS
	    && CLASS_DATA (ref->u.c.component)->attr.dimension
	    && class_array_ref_detected (ref->next, full_array))
	return true;
    }

  return false;
}


/* Returns true if the expression is a reference to a class
   scalar.  This function is necessary because such expressions
   can be dressed with a reference to the _data component and so
   have a type other than BT_CLASS.  */

bool
gfc_is_class_scalar_expr (gfc_expr *e)
{
  gfc_ref *ref;

  if (e->rank)
    return false;

  /* Is this a class object?  */
  if (e->symtree
	&& e->symtree->n.sym->ts.type == BT_CLASS
	&& CLASS_DATA (e->symtree->n.sym)
	&& !CLASS_DATA (e->symtree->n.sym)->attr.dimension
	&& (e->ref == NULL
	    || (strcmp (e->ref->u.c.component->name, "_data") == 0
		&& e->ref->next == NULL)))
    return true;

  /* Or is the final reference BT_CLASS or _data?  */
  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT
	    && ref->u.c.component->ts.type == BT_CLASS
	    && CLASS_DATA (ref->u.c.component)
	    && !CLASS_DATA (ref->u.c.component)->attr.dimension
	    && (ref->next == NULL
		|| (strcmp (ref->next->u.c.component->name, "_data") == 0
		    && ref->next->next == NULL)))
	return true;
    }

  return false;
}


/* Tells whether the expression E is a reference to a (scalar) class container.
   Scalar because array class containers usually have an array reference after
   them, and gfc_fix_class_refs will add the missing "_data" component reference
   in that case.  */

bool
gfc_is_class_container_ref (gfc_expr *e)
{
  gfc_ref *ref;
  bool result;

  if (e->expr_type != EXPR_VARIABLE)
    return e->ts.type == BT_CLASS;

  if (e->symtree->n.sym->ts.type == BT_CLASS)
    result = true;
  else
    result = false;

  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type != REF_COMPONENT)
	result = false;
      else if (ref->u.c.component->ts.type == BT_CLASS)
	result = true;
      else
	result = false;
    }

  return result;
}


/* Build an initializer for CLASS pointers,
   initializing the _data component to the init_expr (or NULL) and the _vptr
   component to the corresponding type (or the declared type, given by ts).  */

gfc_expr *
gfc_class_initializer (gfc_typespec *ts, gfc_expr *init_expr)
{
  gfc_expr *init;
  gfc_component *comp;
  gfc_symbol *vtab = NULL;

  if (init_expr && init_expr->expr_type != EXPR_NULL)
    vtab = gfc_find_vtab (&init_expr->ts);
  else
    vtab = gfc_find_vtab (ts);

  init = gfc_get_structure_constructor_expr (ts->type, ts->kind,
					     &ts->u.derived->declared_at);
  init->ts = *ts;

  for (comp = ts->u.derived->components; comp; comp = comp->next)
    {
      gfc_constructor *ctor = gfc_constructor_get();
      if (strcmp (comp->name, "_vptr") == 0 && vtab)
	ctor->expr = gfc_lval_expr_from_sym (vtab);
      else if (init_expr && init_expr->expr_type != EXPR_NULL)
	  ctor->expr = gfc_copy_expr (init_expr);
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
  if (derived->attr.unlimited_polymorphic)
    strcpy (dt_name, "STAR");
  else
    strcpy (dt_name, gfc_dt_upper_string (derived->name));
  if (derived->attr.unlimited_polymorphic)
    sprintf (string, "_%s", dt_name);
  else if (derived->module)
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
     extra decoration, cf. gfc_build_class_symbol & gfc_find_derived_vtab).
     We need space to for 15 characters "__class_" + symbol name + "_%d_%da",
     where %d is the (co)rank which can be up to n = 15.  */
  if (strlen (tmp) > GFC_MAX_SYMBOL_LEN - 15)
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


/* Assign a hash value for an intrinsic type. The algorithm is that of SDBM.  */

unsigned int
gfc_intrinsic_hash_value (gfc_typespec *ts)
{
  unsigned int hash = 0;
  const char *c = gfc_typename (ts);
  int i, len;

  len = strlen (c);

  for (i = 0; i < len; i++)
    hash = (hash << 6) + (hash << 16) - hash + c[i];

  /* Return the hash but take the modulus for the sake of module read,
     even though this slightly increases the chance of collision.  */
  return (hash % 100000000);
}


/* Get the _len component from a class/derived object storing a string.
   For unlimited polymorphic entities a ref to the _data component is available
   while a ref to the _len component is needed.  This routine traverese the
   ref-chain and strips the last ref to a _data from it replacing it with a
   ref to the _len component.  */

gfc_expr *
gfc_get_len_component (gfc_expr *e)
{
  gfc_expr *ptr;
  gfc_ref *ref, **last;

  ptr = gfc_copy_expr (e);

  /* We need to remove the last _data component ref from ptr.  */
  last = &(ptr->ref);
  ref = ptr->ref;
  while (ref)
    {
      if (!ref->next
	  && ref->type == REF_COMPONENT
	  && strcmp ("_data", ref->u.c.component->name)== 0)
	{
	  gfc_free_ref_list (ref);
	  *last = NULL;
	  break;
	}
      last = &(ref->next);
      ref = ref->next;
    }
  /* And replace if with a ref to the _len component.  */
  gfc_add_len_component (ptr);
  return ptr;
}


/* Build a polymorphic CLASS entity, using the symbol that comes from
   build_sym. A CLASS entity is represented by an encapsulating type,
   which contains the declared type as '_data' component, plus a pointer
   component '_vptr' which determines the dynamic type.  When this CLASS
   entity is unlimited polymorphic, then also add a component '_len' to
   store the length of string when that is stored in it.  */

bool
gfc_build_class_symbol (gfc_typespec *ts, symbol_attribute *attr,
			gfc_array_spec **as)
{
  char name[GFC_MAX_SYMBOL_LEN+1], tname[GFC_MAX_SYMBOL_LEN+1];
  gfc_symbol *fclass;
  gfc_symbol *vtab;
  gfc_component *c;
  gfc_namespace *ns;
  int rank;

  gcc_assert (as);

  if (*as && (*as)->type == AS_ASSUMED_SIZE)
    {
      gfc_error ("Assumed size polymorphic objects or components, such "
		 "as that at %C, have not yet been implemented");
      return false;
    }

  if (attr->class_ok)
    /* Class container has already been built.  */
    return true;

  attr->class_ok = attr->dummy || attr->pointer || attr->allocatable
		   || attr->select_type_temporary || attr->associate_var;

  if (!attr->class_ok)
    /* We can not build the class container yet.  */
    return true;

  /* Determine the name of the encapsulating type.  */
  rank = !(*as) || (*as)->rank == -1 ? GFC_MAX_DIMENSIONS : (*as)->rank;
  get_unique_hashed_string (tname, ts->u.derived);
  if ((*as) && attr->allocatable)
    sprintf (name, "__class_%s_%d_%da", tname, rank, (*as)->corank);
  else if ((*as) && attr->pointer)
    sprintf (name, "__class_%s_%d_%dp", tname, rank, (*as)->corank);
  else if ((*as))
    sprintf (name, "__class_%s_%d_%dt", tname, rank, (*as)->corank);
  else if (attr->pointer)
    sprintf (name, "__class_%s_p", tname);
  else if (attr->allocatable)
    sprintf (name, "__class_%s_a", tname);
  else
    sprintf (name, "__class_%s_t", tname);

  if (ts->u.derived->attr.unlimited_polymorphic)
    {
      /* Find the top-level namespace.  */
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	if (!ns->parent)
	  break;
    }
  else
    ns = ts->u.derived->ns;

  gfc_find_symbol (name, ns, 0, &fclass);
  if (fclass == NULL)
    {
      gfc_symtree *st;
      /* If not there, create a new symbol.  */
      fclass = gfc_new_symbol (name, ns);
      st = gfc_new_symtree (&ns->sym_root, name);
      st->n.sym = fclass;
      gfc_set_sym_referenced (fclass);
      fclass->refs++;
      fclass->ts.type = BT_UNKNOWN;
      if (!ts->u.derived->attr.unlimited_polymorphic)
	fclass->attr.abstract = ts->u.derived->attr.abstract;
      fclass->f2k_derived = gfc_get_namespace (NULL, 0);
      if (!gfc_add_flavor (&fclass->attr, FL_DERIVED, NULL,
			   &gfc_current_locus))
	return false;

      /* Add component '_data'.  */
      if (!gfc_add_component (fclass, "_data", &c))
	return false;
      c->ts = *ts;
      c->ts.type = BT_DERIVED;
      c->attr.access = ACCESS_PRIVATE;
      c->ts.u.derived = ts->u.derived;
      c->attr.class_pointer = attr->pointer;
      c->attr.pointer = attr->pointer || (attr->dummy && !attr->allocatable)
			|| attr->select_type_temporary;
      c->attr.allocatable = attr->allocatable;
      c->attr.dimension = attr->dimension;
      c->attr.codimension = attr->codimension;
      c->attr.abstract = fclass->attr.abstract;
      c->as = (*as);
      c->initializer = NULL;

      /* Add component '_vptr'.  */
      if (!gfc_add_component (fclass, "_vptr", &c))
	return false;
      c->ts.type = BT_DERIVED;
      c->attr.access = ACCESS_PRIVATE;
      c->attr.pointer = 1;

      if (ts->u.derived->attr.unlimited_polymorphic)
	{
	  vtab = gfc_find_derived_vtab (ts->u.derived);
	  gcc_assert (vtab);
	  c->ts.u.derived = vtab->ts.u.derived;

	  /* Add component '_len'.  Only unlimited polymorphic pointers may
             have a string assigned to them, i.e., only those need the _len
             component.  */
	  if (!gfc_add_component (fclass, "_len", &c))
	    return false;
	  c->ts.type = BT_INTEGER;
	  c->ts.kind = gfc_charlen_int_kind;
	  c->attr.access = ACCESS_PRIVATE;
	  c->attr.artificial = 1;
	}
      else
	/* Build vtab later.  */
	c->ts.u.derived = NULL;
    }

  if (!ts->u.derived->attr.unlimited_polymorphic)
    {
      /* Since the extension field is 8 bit wide, we can only have
	 up to 255 extension levels.  */
      if (ts->u.derived->attr.extension == 255)
	{
	  gfc_error ("Maximum extension level reached with type %qs at %L",
		     ts->u.derived->name, &ts->u.derived->declared_at);
	return false;
	}

      fclass->attr.extension = ts->u.derived->attr.extension + 1;
      fclass->attr.alloc_comp = ts->u.derived->attr.alloc_comp;
      fclass->attr.coarray_comp = ts->u.derived->attr.coarray_comp;
    }

  fclass->attr.is_class = 1;
  ts->u.derived = fclass;
  attr->allocatable = attr->pointer = attr->dimension = attr->codimension = 0;
  (*as) = NULL;
  return true;
}


/* Add a procedure pointer component to the vtype
   to represent a specific type-bound procedure.  */

static void
add_proc_comp (gfc_symbol *vtype, const char *name, gfc_typebound_proc *tb)
{
  gfc_component *c;

  if (tb->non_overridable)
    return;

  c = gfc_find_component (vtype, name, true, true, NULL);

  if (c == NULL)
    {
      /* Add procedure component.  */
      if (!gfc_add_component (vtype, name, &c))
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
      gfc_symbol *ifc = tb->u.specific->n.sym;
      c->ts.interface = ifc;
      if (!tb->deferred)
	c->initializer = gfc_get_variable_expr (tb->u.specific);
      c->attr.pure = ifc->attr.pure;
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
      if (gfc_find_component (vtype, cmp->name, true, true, NULL))
	continue;

      add_proc_comp (vtype, cmp->name, cmp->tb);
    }
}


/* Returns true if any of its nonpointer nonallocatable components or
   their nonpointer nonallocatable subcomponents has a finalization
   subroutine.  */

static bool
has_finalizer_component (gfc_symbol *derived)
{
   gfc_component *c;

  for (c = derived->components; c; c = c->next)
    {
      if (c->ts.type == BT_DERIVED && c->ts.u.derived->f2k_derived
	  && c->ts.u.derived->f2k_derived->finalizers)
	return true;

      /* Stop infinite recursion through this function by inhibiting
	 calls when the derived type and that of the component are
	 the same.  */
      if (c->ts.type == BT_DERIVED
	  && !gfc_compare_derived_types (derived, c->ts.u.derived)
	  && !c->attr.pointer && !c->attr.allocatable
	  && has_finalizer_component (c->ts.u.derived))
	return true;
    }
  return false;
}


static bool
comp_is_finalizable (gfc_component *comp)
{
  if (comp->attr.proc_pointer)
    return false;
  else if (comp->attr.allocatable && comp->ts.type != BT_CLASS)
    return true;
  else if (comp->ts.type == BT_DERIVED && !comp->attr.pointer
	   && (comp->ts.u.derived->attr.alloc_comp
	       || has_finalizer_component (comp->ts.u.derived)
	       || (comp->ts.u.derived->f2k_derived
		   && comp->ts.u.derived->f2k_derived->finalizers)))
    return true;
  else if (comp->ts.type == BT_CLASS && CLASS_DATA (comp)
	    && CLASS_DATA (comp)->attr.allocatable)
    return true;
  else
    return false;
}


/* Call DEALLOCATE for the passed component if it is allocatable, if it is
   neither allocatable nor a pointer but has a finalizer, call it. If it
   is a nonpointer component with allocatable components or has finalizers, walk
   them. Either of them is required; other nonallocatables and pointers aren't
   handled gracefully.
   Note: If the component is allocatable, the DEALLOCATE handling takes care
   of calling the appropriate finalizers, coarray deregistering, and
   deallocation of allocatable subcomponents.  */

static void
finalize_component (gfc_expr *expr, gfc_symbol *derived, gfc_component *comp,
		    gfc_symbol *stat, gfc_symbol *fini_coarray, gfc_code **code,
		    gfc_namespace *sub_ns)
{
  gfc_expr *e;
  gfc_ref *ref;

  if (!comp_is_finalizable (comp))
    return;

  e = gfc_copy_expr (expr);
  if (!e->ref)
    e->ref = ref = gfc_get_ref ();
  else
    {
      for (ref = e->ref; ref->next; ref = ref->next)
	;
      ref->next = gfc_get_ref ();
      ref = ref->next;
    }
  ref->type = REF_COMPONENT;
  ref->u.c.sym = derived;
  ref->u.c.component = comp;
  e->ts = comp->ts;

  if (comp->attr.dimension || comp->attr.codimension
      || (comp->ts.type == BT_CLASS && CLASS_DATA (comp)
	  && (CLASS_DATA (comp)->attr.dimension
	      || CLASS_DATA (comp)->attr.codimension)))
    {
      ref->next = gfc_get_ref ();
      ref->next->type = REF_ARRAY;
      ref->next->u.ar.dimen = 0;
      ref->next->u.ar.as = comp->ts.type == BT_CLASS ? CLASS_DATA (comp)->as
							: comp->as;
      e->rank = ref->next->u.ar.as->rank;
      ref->next->u.ar.type = e->rank ? AR_FULL : AR_ELEMENT;
    }

  /* Call DEALLOCATE (comp, stat=ignore).  */
  if (comp->attr.allocatable
      || (comp->ts.type == BT_CLASS && CLASS_DATA (comp)
	  && CLASS_DATA (comp)->attr.allocatable))
    {
      gfc_code *dealloc, *block = NULL;

      /* Add IF (fini_coarray).  */
      if (comp->attr.codimension
	  || (comp->ts.type == BT_CLASS && CLASS_DATA (comp)
	      && CLASS_DATA (comp)->attr.codimension))
	{
	  block = gfc_get_code (EXEC_IF);
	  if (*code)
	    {
	      (*code)->next = block;
	      (*code) = (*code)->next;
	    }
	  else
	      (*code) = block;

	  block->block = gfc_get_code (EXEC_IF);
	  block = block->block;
	  block->expr1 = gfc_lval_expr_from_sym (fini_coarray);
	}

      dealloc = gfc_get_code (EXEC_DEALLOCATE);

      dealloc->ext.alloc.list = gfc_get_alloc ();
      dealloc->ext.alloc.list->expr = e;
      dealloc->expr1 = gfc_lval_expr_from_sym (stat);

      gfc_code *cond = gfc_get_code (EXEC_IF);
      cond->block = gfc_get_code (EXEC_IF);
      cond->block->expr1 = gfc_get_expr ();
      cond->block->expr1->expr_type = EXPR_FUNCTION;
      gfc_get_sym_tree ("associated", sub_ns, &cond->block->expr1->symtree, false);
      cond->block->expr1->symtree->n.sym->attr.flavor = FL_PROCEDURE;
      cond->block->expr1->symtree->n.sym->attr.intrinsic = 1;
      cond->block->expr1->symtree->n.sym->result = cond->block->expr1->symtree->n.sym;
      gfc_commit_symbol (cond->block->expr1->symtree->n.sym);
      cond->block->expr1->ts.type = BT_LOGICAL;
      cond->block->expr1->ts.kind = gfc_default_logical_kind;
      cond->block->expr1->value.function.isym = gfc_intrinsic_function_by_id (GFC_ISYM_ASSOCIATED);
      cond->block->expr1->value.function.actual = gfc_get_actual_arglist ();
      cond->block->expr1->value.function.actual->expr = gfc_copy_expr (expr);
      cond->block->expr1->value.function.actual->next = gfc_get_actual_arglist ();
      cond->block->next = dealloc;

      if (block)
	block->next = cond;
      else if (*code)
	{
	  (*code)->next = cond;
	  (*code) = (*code)->next;
	}
      else
	(*code) = cond;
    }
  else if (comp->ts.type == BT_DERIVED
	    && comp->ts.u.derived->f2k_derived
	    && comp->ts.u.derived->f2k_derived->finalizers)
    {
      /* Call FINAL_WRAPPER (comp);  */
      gfc_code *final_wrap;
      gfc_symbol *vtab;
      gfc_component *c;

      vtab = gfc_find_derived_vtab (comp->ts.u.derived);
      for (c = vtab->ts.u.derived->components; c; c = c->next)
	if (strcmp (c->name, "_final") == 0)
	  break;

      gcc_assert (c);
      final_wrap = gfc_get_code (EXEC_CALL);
      final_wrap->symtree = c->initializer->symtree;
      final_wrap->resolved_sym = c->initializer->symtree->n.sym;
      final_wrap->ext.actual = gfc_get_actual_arglist ();
      final_wrap->ext.actual->expr = e;

      if (*code)
	{
	  (*code)->next = final_wrap;
	  (*code) = (*code)->next;
	}
      else
	(*code) = final_wrap;
    }
  else
    {
      gfc_component *c;

      for (c = comp->ts.u.derived->components; c; c = c->next)
	finalize_component (e, comp->ts.u.derived, c, stat, fini_coarray, code,
			    sub_ns);
      gfc_free_expr (e);
    }
}


/* Generate code equivalent to
   CALL C_F_POINTER (TRANSFER (TRANSFER (C_LOC (array, cptr), c_intptr)
		     + offset, c_ptr), ptr).  */

static gfc_code *
finalization_scalarizer (gfc_symbol *array, gfc_symbol *ptr,
			 gfc_expr *offset, gfc_namespace *sub_ns)
{
  gfc_code *block;
  gfc_expr *expr, *expr2;

  /* C_F_POINTER().  */
  block = gfc_get_code (EXEC_CALL);
  gfc_get_sym_tree ("c_f_pointer", sub_ns, &block->symtree, true);
  block->resolved_sym = block->symtree->n.sym;
  block->resolved_sym->attr.flavor = FL_PROCEDURE;
  block->resolved_sym->attr.intrinsic = 1;
  block->resolved_sym->attr.subroutine = 1;
  block->resolved_sym->from_intmod = INTMOD_ISO_C_BINDING;
  block->resolved_sym->intmod_sym_id = ISOCBINDING_F_POINTER;
  block->resolved_isym = gfc_intrinsic_subroutine_by_id (GFC_ISYM_C_F_POINTER);
  gfc_commit_symbol (block->resolved_sym);

  /* C_F_POINTER's first argument: TRANSFER ( <addr>, c_intptr_t).  */
  block->ext.actual = gfc_get_actual_arglist ();
  block->ext.actual->next = gfc_get_actual_arglist ();
  block->ext.actual->next->expr = gfc_get_int_expr (gfc_index_integer_kind,
						    NULL, 0);
  block->ext.actual->next->next = gfc_get_actual_arglist (); /* SIZE.  */

  /* The <addr> part: TRANSFER (C_LOC (array), c_intptr_t).  */

  /* TRANSFER's first argument: C_LOC (array).  */
  expr = gfc_get_expr ();
  expr->expr_type = EXPR_FUNCTION;
  gfc_get_sym_tree ("c_loc", sub_ns, &expr->symtree, false);
  expr->symtree->n.sym->attr.flavor = FL_PROCEDURE;
  expr->symtree->n.sym->intmod_sym_id = ISOCBINDING_LOC;
  expr->symtree->n.sym->attr.intrinsic = 1;
  expr->symtree->n.sym->from_intmod = INTMOD_ISO_C_BINDING;
  expr->value.function.isym = gfc_intrinsic_function_by_id (GFC_ISYM_C_LOC);
  expr->value.function.actual = gfc_get_actual_arglist ();
  expr->value.function.actual->expr
	    = gfc_lval_expr_from_sym (array);
  expr->symtree->n.sym->result = expr->symtree->n.sym;
  gfc_commit_symbol (expr->symtree->n.sym);
  expr->ts.type = BT_INTEGER;
  expr->ts.kind = gfc_index_integer_kind;

  /* TRANSFER.  */
  expr2 = gfc_build_intrinsic_call (sub_ns, GFC_ISYM_TRANSFER, "transfer",
				    gfc_current_locus, 3, expr,
				    gfc_get_int_expr (gfc_index_integer_kind,
						      NULL, 0), NULL);
  expr2->ts.type = BT_INTEGER;
  expr2->ts.kind = gfc_index_integer_kind;

  /* <array addr> + <offset>.  */
  block->ext.actual->expr = gfc_get_expr ();
  block->ext.actual->expr->expr_type = EXPR_OP;
  block->ext.actual->expr->value.op.op = INTRINSIC_PLUS;
  block->ext.actual->expr->value.op.op1 = expr2;
  block->ext.actual->expr->value.op.op2 = offset;
  block->ext.actual->expr->ts = expr->ts;

  /* C_F_POINTER's 2nd arg: ptr -- and its absent shape=.  */
  block->ext.actual->next = gfc_get_actual_arglist ();
  block->ext.actual->next->expr = gfc_lval_expr_from_sym (ptr);
  block->ext.actual->next->next = gfc_get_actual_arglist ();

  return block;
}


/* Calculates the offset to the (idx+1)th element of an array, taking the
   stride into account. It generates the code:
     offset = 0
     do idx2 = 1, rank
       offset = offset + mod (idx, sizes(idx2)) / sizes(idx2-1) * strides(idx2)
     end do
     offset = offset * byte_stride.  */

static gfc_code*
finalization_get_offset (gfc_symbol *idx, gfc_symbol *idx2, gfc_symbol *offset,
			 gfc_symbol *strides, gfc_symbol *sizes,
			 gfc_symbol *byte_stride, gfc_expr *rank,
			 gfc_code *block, gfc_namespace *sub_ns)
{
  gfc_iterator *iter;
  gfc_expr *expr, *expr2;

  /* offset = 0.  */
  block->next = gfc_get_code (EXEC_ASSIGN);
  block = block->next;
  block->expr1 = gfc_lval_expr_from_sym (offset);
  block->expr2 = gfc_get_int_expr (gfc_index_integer_kind, NULL, 0);

  /* Create loop.  */
  iter = gfc_get_iterator ();
  iter->var = gfc_lval_expr_from_sym (idx2);
  iter->start = gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
  iter->end = gfc_copy_expr (rank);
  iter->step = gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
  block->next = gfc_get_code (EXEC_DO);
  block = block->next;
  block->ext.iterator = iter;
  block->block = gfc_get_code (EXEC_DO);

  /* Loop body: offset = offset + mod (idx, sizes(idx2)) / sizes(idx2-1)
				  * strides(idx2).  */

  /* mod (idx, sizes(idx2)).  */
  expr = gfc_lval_expr_from_sym (sizes);
  expr->ref = gfc_get_ref ();
  expr->ref->type = REF_ARRAY;
  expr->ref->u.ar.as = sizes->as;
  expr->ref->u.ar.type = AR_ELEMENT;
  expr->ref->u.ar.dimen = 1;
  expr->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
  expr->ref->u.ar.start[0] = gfc_lval_expr_from_sym (idx2);

  expr = gfc_build_intrinsic_call (sub_ns, GFC_ISYM_MOD, "mod",
				   gfc_current_locus, 2,
				   gfc_lval_expr_from_sym (idx), expr);
  expr->ts = idx->ts;

  /* (...) / sizes(idx2-1).  */
  expr2 = gfc_get_expr ();
  expr2->expr_type = EXPR_OP;
  expr2->value.op.op = INTRINSIC_DIVIDE;
  expr2->value.op.op1 = expr;
  expr2->value.op.op2 = gfc_lval_expr_from_sym (sizes);
  expr2->value.op.op2->ref = gfc_get_ref ();
  expr2->value.op.op2->ref->type = REF_ARRAY;
  expr2->value.op.op2->ref->u.ar.as = sizes->as;
  expr2->value.op.op2->ref->u.ar.type = AR_ELEMENT;
  expr2->value.op.op2->ref->u.ar.dimen = 1;
  expr2->value.op.op2->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
  expr2->value.op.op2->ref->u.ar.start[0] = gfc_get_expr ();
  expr2->value.op.op2->ref->u.ar.start[0]->expr_type = EXPR_OP;
  expr2->value.op.op2->ref->u.ar.start[0]->value.op.op = INTRINSIC_MINUS;
  expr2->value.op.op2->ref->u.ar.start[0]->value.op.op1
	= gfc_lval_expr_from_sym (idx2);
  expr2->value.op.op2->ref->u.ar.start[0]->value.op.op2
	= gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
  expr2->value.op.op2->ref->u.ar.start[0]->ts
	= expr2->value.op.op2->ref->u.ar.start[0]->value.op.op1->ts;
  expr2->ts = idx->ts;

  /* ... * strides(idx2).  */
  expr = gfc_get_expr ();
  expr->expr_type = EXPR_OP;
  expr->value.op.op = INTRINSIC_TIMES;
  expr->value.op.op1 = expr2;
  expr->value.op.op2 = gfc_lval_expr_from_sym (strides);
  expr->value.op.op2->ref = gfc_get_ref ();
  expr->value.op.op2->ref->type = REF_ARRAY;
  expr->value.op.op2->ref->u.ar.type = AR_ELEMENT;
  expr->value.op.op2->ref->u.ar.dimen = 1;
  expr->value.op.op2->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
  expr->value.op.op2->ref->u.ar.start[0] = gfc_lval_expr_from_sym (idx2);
  expr->value.op.op2->ref->u.ar.as = strides->as;
  expr->ts = idx->ts;

  /* offset = offset + ...  */
  block->block->next = gfc_get_code (EXEC_ASSIGN);
  block->block->next->expr1 = gfc_lval_expr_from_sym (offset);
  block->block->next->expr2 = gfc_get_expr ();
  block->block->next->expr2->expr_type = EXPR_OP;
  block->block->next->expr2->value.op.op = INTRINSIC_PLUS;
  block->block->next->expr2->value.op.op1 = gfc_lval_expr_from_sym (offset);
  block->block->next->expr2->value.op.op2 = expr;
  block->block->next->expr2->ts = idx->ts;

  /* After the loop:  offset = offset * byte_stride.  */
  block->next = gfc_get_code (EXEC_ASSIGN);
  block = block->next;
  block->expr1 = gfc_lval_expr_from_sym (offset);
  block->expr2 = gfc_get_expr ();
  block->expr2->expr_type = EXPR_OP;
  block->expr2->value.op.op = INTRINSIC_TIMES;
  block->expr2->value.op.op1 = gfc_lval_expr_from_sym (offset);
  block->expr2->value.op.op2 = gfc_lval_expr_from_sym (byte_stride);
  block->expr2->ts = block->expr2->value.op.op1->ts;
  return block;
}


/* Insert code of the following form:

   block
     integer(c_intptr_t) :: i

     if ((byte_stride == STORAGE_SIZE (array)/NUMERIC_STORAGE_SIZE
	  && (is_contiguous || !final_rank3->attr.contiguous
	      || final_rank3->as->type != AS_ASSUMED_SHAPE))
         || 0 == STORAGE_SIZE (array)) then
       call final_rank3 (array)
     else
       block
         integer(c_intptr_t) :: offset, j
         type(t) :: tmp(shape (array))

         do i = 0, size (array)-1
	   offset = obtain_offset(i, strides, sizes, byte_stride)
	   addr = transfer (c_loc (array), addr) + offset
	   call c_f_pointer (transfer (addr, cptr), ptr)

	   addr = transfer (c_loc (tmp), addr)
			    + i * STORAGE_SIZE (array)/NUMERIC_STORAGE_SIZE
	   call c_f_pointer (transfer (addr, cptr), ptr2)
	   ptr2 = ptr
         end do
         call final_rank3 (tmp)
       end block
     end if
   block  */

static void
finalizer_insert_packed_call (gfc_code *block, gfc_finalizer *fini,
			      gfc_symbol *array, gfc_symbol *byte_stride,
			      gfc_symbol *idx, gfc_symbol *ptr,
			      gfc_symbol *nelem,
			      gfc_symbol *strides, gfc_symbol *sizes,
			      gfc_symbol *idx2, gfc_symbol *offset,
			      gfc_symbol *is_contiguous, gfc_expr *rank,
			      gfc_namespace *sub_ns)
{
  gfc_symbol *tmp_array, *ptr2;
  gfc_expr *size_expr, *offset2, *expr;
  gfc_namespace *ns;
  gfc_iterator *iter;
  gfc_code *block2;
  int i;

  block->next = gfc_get_code (EXEC_IF);
  block = block->next;

  block->block = gfc_get_code (EXEC_IF);
  block = block->block;

  /* size_expr = STORAGE_SIZE (...) / NUMERIC_STORAGE_SIZE.  */
  size_expr = gfc_get_expr ();
  size_expr->where = gfc_current_locus;
  size_expr->expr_type = EXPR_OP;
  size_expr->value.op.op = INTRINSIC_DIVIDE;

  /* STORAGE_SIZE (array,kind=c_intptr_t).  */
  size_expr->value.op.op1
	= gfc_build_intrinsic_call (sub_ns, GFC_ISYM_STORAGE_SIZE,
				    "storage_size", gfc_current_locus, 2,
				    gfc_lval_expr_from_sym (array),
				    gfc_get_int_expr (gfc_index_integer_kind,
						      NULL, 0));

  /* NUMERIC_STORAGE_SIZE.  */
  size_expr->value.op.op2 = gfc_get_int_expr (gfc_index_integer_kind, NULL,
					      gfc_character_storage_size);
  size_expr->value.op.op1->ts = size_expr->value.op.op2->ts;
  size_expr->ts = size_expr->value.op.op1->ts;

  /* IF condition: (stride == size_expr
		    && ((fini's as->ASSUMED_SIZE && !fini's attr.contiguous)
			|| is_contiguous)
		   || 0 == size_expr.  */
  block->expr1 = gfc_get_expr ();
  block->expr1->ts.type = BT_LOGICAL;
  block->expr1->ts.kind = gfc_default_logical_kind;
  block->expr1->expr_type = EXPR_OP;
  block->expr1->where = gfc_current_locus;

  block->expr1->value.op.op = INTRINSIC_OR;

  /* byte_stride == size_expr */
  expr = gfc_get_expr ();
  expr->ts.type = BT_LOGICAL;
  expr->ts.kind = gfc_default_logical_kind;
  expr->expr_type = EXPR_OP;
  expr->where = gfc_current_locus;
  expr->value.op.op = INTRINSIC_EQ;
  expr->value.op.op1
	= gfc_lval_expr_from_sym (byte_stride);
  expr->value.op.op2 = size_expr;

  /* If strides aren't allowed (not assumed shape or CONTIGUOUS),
     add is_contiguous check.  */

  if (fini->proc_tree->n.sym->formal->sym->as->type != AS_ASSUMED_SHAPE
      || fini->proc_tree->n.sym->formal->sym->attr.contiguous)
    {
      gfc_expr *expr2;
      expr2 = gfc_get_expr ();
      expr2->ts.type = BT_LOGICAL;
      expr2->ts.kind = gfc_default_logical_kind;
      expr2->expr_type = EXPR_OP;
      expr2->where = gfc_current_locus;
      expr2->value.op.op = INTRINSIC_AND;
      expr2->value.op.op1 = expr;
      expr2->value.op.op2 = gfc_lval_expr_from_sym (is_contiguous);
      expr = expr2;
    }

  block->expr1->value.op.op1 = expr;

  /* 0 == size_expr */
  block->expr1->value.op.op2 = gfc_get_expr ();
  block->expr1->value.op.op2->ts.type = BT_LOGICAL;
  block->expr1->value.op.op2->ts.kind = gfc_default_logical_kind;
  block->expr1->value.op.op2->expr_type = EXPR_OP;
  block->expr1->value.op.op2->where = gfc_current_locus;
  block->expr1->value.op.op2->value.op.op = INTRINSIC_EQ;
  block->expr1->value.op.op2->value.op.op1 =
			gfc_get_int_expr (gfc_index_integer_kind, NULL, 0);
  block->expr1->value.op.op2->value.op.op2 = gfc_copy_expr (size_expr);

  /* IF body: call final subroutine.  */
  block->next = gfc_get_code (EXEC_CALL);
  block->next->symtree = fini->proc_tree;
  block->next->resolved_sym = fini->proc_tree->n.sym;
  block->next->ext.actual = gfc_get_actual_arglist ();
  block->next->ext.actual->expr = gfc_lval_expr_from_sym (array);

  /* ELSE.  */

  block->block = gfc_get_code (EXEC_IF);
  block = block->block;

  /* BLOCK ... END BLOCK.  */
  block->next = gfc_get_code (EXEC_BLOCK);
  block = block->next;

  ns = gfc_build_block_ns (sub_ns);
  block->ext.block.ns = ns;
  block->ext.block.assoc = NULL;

  gfc_get_symbol ("ptr2", ns, &ptr2);
  ptr2->ts.type = BT_DERIVED;
  ptr2->ts.u.derived = array->ts.u.derived;
  ptr2->attr.flavor = FL_VARIABLE;
  ptr2->attr.pointer = 1;
  ptr2->attr.artificial = 1;
  gfc_set_sym_referenced (ptr2);
  gfc_commit_symbol (ptr2);

  gfc_get_symbol ("tmp_array", ns, &tmp_array);
  tmp_array->ts.type = BT_DERIVED;
  tmp_array->ts.u.derived = array->ts.u.derived;
  tmp_array->attr.flavor = FL_VARIABLE;
  tmp_array->attr.dimension = 1;
  tmp_array->attr.artificial = 1;
  tmp_array->as = gfc_get_array_spec();
  tmp_array->attr.intent = INTENT_INOUT;
  tmp_array->as->type = AS_EXPLICIT;
  tmp_array->as->rank = fini->proc_tree->n.sym->formal->sym->as->rank;

  for (i = 0; i < tmp_array->as->rank; i++)
    {
      gfc_expr *shape_expr;
      tmp_array->as->lower[i] = gfc_get_int_expr (gfc_default_integer_kind,
						  NULL, 1);
      /* SIZE (array, dim=i+1, kind=gfc_index_integer_kind).  */
      shape_expr
	= gfc_build_intrinsic_call (sub_ns, GFC_ISYM_SIZE, "size",
				    gfc_current_locus, 3,
				    gfc_lval_expr_from_sym (array),
				    gfc_get_int_expr (gfc_default_integer_kind,
						      NULL, i+1),
				    gfc_get_int_expr (gfc_default_integer_kind,
						      NULL,
						      gfc_index_integer_kind));
      shape_expr->ts.kind = gfc_index_integer_kind;
      tmp_array->as->upper[i] = shape_expr;
    }
  gfc_set_sym_referenced (tmp_array);
  gfc_commit_symbol (tmp_array);

  /* Create loop.  */
  iter = gfc_get_iterator ();
  iter->var = gfc_lval_expr_from_sym (idx);
  iter->start = gfc_get_int_expr (gfc_index_integer_kind, NULL, 0);
  iter->end = gfc_lval_expr_from_sym (nelem);
  iter->step = gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);

  block = gfc_get_code (EXEC_DO);
  ns->code = block;
  block->ext.iterator = iter;
  block->block = gfc_get_code (EXEC_DO);

  /* Offset calculation for the new array: idx * size of type (in bytes).  */
  offset2 = gfc_get_expr ();
  offset2->expr_type = EXPR_OP;
  offset2->value.op.op = INTRINSIC_TIMES;
  offset2->value.op.op1 = gfc_lval_expr_from_sym (idx);
  offset2->value.op.op2 = gfc_copy_expr (size_expr);
  offset2->ts = byte_stride->ts;

  /* Offset calculation of "array".  */
  block2 = finalization_get_offset (idx, idx2, offset, strides, sizes,
				    byte_stride, rank, block->block, sub_ns);

  /* Create code for
     CALL C_F_POINTER (TRANSFER (TRANSFER (C_LOC (array, cptr), c_intptr)
		       + idx * stride, c_ptr), ptr).  */
  block2->next = finalization_scalarizer (array, ptr,
					  gfc_lval_expr_from_sym (offset),
					  sub_ns);
  block2 = block2->next;
  block2->next = finalization_scalarizer (tmp_array, ptr2, offset2, sub_ns);
  block2 = block2->next;

  /* ptr2 = ptr.  */
  block2->next = gfc_get_code (EXEC_ASSIGN);
  block2 = block2->next;
  block2->expr1 = gfc_lval_expr_from_sym (ptr2);
  block2->expr2 = gfc_lval_expr_from_sym (ptr);

  /* Call now the user's final subroutine.  */
  block->next  = gfc_get_code (EXEC_CALL);
  block = block->next;
  block->symtree = fini->proc_tree;
  block->resolved_sym = fini->proc_tree->n.sym;
  block->ext.actual = gfc_get_actual_arglist ();
  block->ext.actual->expr = gfc_lval_expr_from_sym (tmp_array);

  if (fini->proc_tree->n.sym->formal->sym->attr.intent == INTENT_IN)
    return;

  /* Copy back.  */

  /* Loop.  */
  iter = gfc_get_iterator ();
  iter->var = gfc_lval_expr_from_sym (idx);
  iter->start = gfc_get_int_expr (gfc_index_integer_kind, NULL, 0);
  iter->end = gfc_lval_expr_from_sym (nelem);
  iter->step = gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);

  block->next = gfc_get_code (EXEC_DO);
  block = block->next;
  block->ext.iterator = iter;
  block->block = gfc_get_code (EXEC_DO);

  /* Offset calculation of "array".  */
  block2 = finalization_get_offset (idx, idx2, offset, strides, sizes,
				    byte_stride, rank, block->block, sub_ns);

  /* Create code for
     CALL C_F_POINTER (TRANSFER (TRANSFER (C_LOC (array, cptr), c_intptr)
		       + offset, c_ptr), ptr).  */
  block2->next = finalization_scalarizer (array, ptr,
					  gfc_lval_expr_from_sym (offset),
					  sub_ns);
  block2 = block2->next;
  block2->next = finalization_scalarizer (tmp_array, ptr2,
					  gfc_copy_expr (offset2), sub_ns);
  block2 = block2->next;

  /* ptr = ptr2.  */
  block2->next = gfc_get_code (EXEC_ASSIGN);
  block2->next->expr1 = gfc_lval_expr_from_sym (ptr);
  block2->next->expr2 = gfc_lval_expr_from_sym (ptr2);
}


/* Generate the finalization/polymorphic freeing wrapper subroutine for the
   derived type "derived". The function first calls the approriate FINAL
   subroutine, then it DEALLOCATEs (finalizes/frees) the allocatable
   components (but not the inherited ones). Last, it calls the wrapper
   subroutine of the parent. The generated wrapper procedure takes as argument
   an assumed-rank array.
   If neither allocatable components nor FINAL subroutines exists, the vtab
   will contain a NULL pointer.
   The generated function has the form
     _final(assumed-rank array, stride, skip_corarray)
   where the array has to be contiguous (except of the lowest dimension). The
   stride (in bytes) is used to allow different sizes for ancestor types by
   skipping over the additionally added components in the scalarizer. If
   "fini_coarray" is false, coarray components are not finalized to allow for
   the correct semantic with intrinsic assignment.  */

static void
generate_finalization_wrapper (gfc_symbol *derived, gfc_namespace *ns,
			       const char *tname, gfc_component *vtab_final)
{
  gfc_symbol *final, *array, *fini_coarray, *byte_stride, *sizes, *strides;
  gfc_symbol *ptr = NULL, *idx, *idx2, *is_contiguous, *offset, *nelem;
  gfc_component *comp;
  gfc_namespace *sub_ns;
  gfc_code *last_code, *block;
  char name[GFC_MAX_SYMBOL_LEN+1];
  bool finalizable_comp = false;
  bool expr_null_wrapper = false;
  gfc_expr *ancestor_wrapper = NULL, *rank;
  gfc_iterator *iter;

  if (derived->attr.unlimited_polymorphic)
    {
      vtab_final->initializer = gfc_get_null_expr (NULL);
      return;
    }

  /* Search for the ancestor's finalizers.  */
  if (derived->attr.extension && derived->components
      && (!derived->components->ts.u.derived->attr.abstract
	  || has_finalizer_component (derived)))
    {
      gfc_symbol *vtab;
      gfc_component *comp;

      vtab = gfc_find_derived_vtab (derived->components->ts.u.derived);
      for (comp = vtab->ts.u.derived->components; comp; comp = comp->next)
	if (comp->name[0] == '_' && comp->name[1] == 'f')
	  {
	    ancestor_wrapper = comp->initializer;
	    break;
	  }
    }

  /* No wrapper of the ancestor and no own FINAL subroutines and allocatable
     components: Return a NULL() expression; we defer this a bit to have have
     an interface declaration.  */
  if ((!ancestor_wrapper || ancestor_wrapper->expr_type == EXPR_NULL)
      && !derived->attr.alloc_comp
      && (!derived->f2k_derived || !derived->f2k_derived->finalizers)
      && !has_finalizer_component (derived))
    expr_null_wrapper = true;
  else
    /* Check whether there are new allocatable components.  */
    for (comp = derived->components; comp; comp = comp->next)
      {
	if (comp == derived->components && derived->attr.extension
	    && ancestor_wrapper && ancestor_wrapper->expr_type != EXPR_NULL)
	continue;

	finalizable_comp |= comp_is_finalizable (comp);
      }

  /* If there is no new finalizer and no new allocatable, return with
     an expr to the ancestor's one.  */
  if (!expr_null_wrapper && !finalizable_comp
      && (!derived->f2k_derived || !derived->f2k_derived->finalizers))
    {
      gcc_assert (ancestor_wrapper && ancestor_wrapper->ref == NULL
	          && ancestor_wrapper->expr_type == EXPR_VARIABLE);
      vtab_final->initializer = gfc_copy_expr (ancestor_wrapper);
      vtab_final->ts.interface = vtab_final->initializer->symtree->n.sym;
      return;
    }

  /* We now create a wrapper, which does the following:
     1. Call the suitable finalization subroutine for this type
     2. Loop over all noninherited allocatable components and noninherited
	components with allocatable components and DEALLOCATE those; this will
	take care of finalizers, coarray deregistering and allocatable
	nested components.
     3. Call the ancestor's finalizer.  */

  /* Declare the wrapper function; it takes an assumed-rank array
     and a VALUE logical as arguments.  */

  /* Set up the namespace.  */
  sub_ns = gfc_get_namespace (ns, 0);
  sub_ns->sibling = ns->contained;
  if (!expr_null_wrapper)
    ns->contained = sub_ns;
  sub_ns->resolved = 1;

  /* Set up the procedure symbol.  */
  sprintf (name, "__final_%s", tname);
  gfc_get_symbol (name, sub_ns, &final);
  sub_ns->proc_name = final;
  final->attr.flavor = FL_PROCEDURE;
  final->attr.function = 1;
  final->attr.pure = 0;
  final->result = final;
  final->ts.type = BT_INTEGER;
  final->ts.kind = 4;
  final->attr.artificial = 1;
  final->attr.always_explicit = 1;
  final->attr.if_source = expr_null_wrapper ? IFSRC_IFBODY : IFSRC_DECL;
  if (ns->proc_name->attr.flavor == FL_MODULE)
    final->module = ns->proc_name->name;
  gfc_set_sym_referenced (final);
  gfc_commit_symbol (final);

  /* Set up formal argument.  */
  gfc_get_symbol ("array", sub_ns, &array);
  array->ts.type = BT_DERIVED;
  array->ts.u.derived = derived;
  array->attr.flavor = FL_VARIABLE;
  array->attr.dummy = 1;
  array->attr.contiguous = 1;
  array->attr.dimension = 1;
  array->attr.artificial = 1;
  array->as = gfc_get_array_spec();
  array->as->type = AS_ASSUMED_RANK;
  array->as->rank = -1;
  array->attr.intent = INTENT_INOUT;
  gfc_set_sym_referenced (array);
  final->formal = gfc_get_formal_arglist ();
  final->formal->sym = array;
  gfc_commit_symbol (array);

  /* Set up formal argument.  */
  gfc_get_symbol ("byte_stride", sub_ns, &byte_stride);
  byte_stride->ts.type = BT_INTEGER;
  byte_stride->ts.kind = gfc_index_integer_kind;
  byte_stride->attr.flavor = FL_VARIABLE;
  byte_stride->attr.dummy = 1;
  byte_stride->attr.value = 1;
  byte_stride->attr.artificial = 1;
  gfc_set_sym_referenced (byte_stride);
  final->formal->next = gfc_get_formal_arglist ();
  final->formal->next->sym = byte_stride;
  gfc_commit_symbol (byte_stride);

  /* Set up formal argument.  */
  gfc_get_symbol ("fini_coarray", sub_ns, &fini_coarray);
  fini_coarray->ts.type = BT_LOGICAL;
  fini_coarray->ts.kind = 1;
  fini_coarray->attr.flavor = FL_VARIABLE;
  fini_coarray->attr.dummy = 1;
  fini_coarray->attr.value = 1;
  fini_coarray->attr.artificial = 1;
  gfc_set_sym_referenced (fini_coarray);
  final->formal->next->next = gfc_get_formal_arglist ();
  final->formal->next->next->sym = fini_coarray;
  gfc_commit_symbol (fini_coarray);

  /* Return with a NULL() expression but with an interface which has
     the formal arguments.  */
  if (expr_null_wrapper)
    {
      vtab_final->initializer = gfc_get_null_expr (NULL);
      vtab_final->ts.interface = final;
      return;
    }

  /* Local variables.  */

  gfc_get_symbol ("idx", sub_ns, &idx);
  idx->ts.type = BT_INTEGER;
  idx->ts.kind = gfc_index_integer_kind;
  idx->attr.flavor = FL_VARIABLE;
  idx->attr.artificial = 1;
  gfc_set_sym_referenced (idx);
  gfc_commit_symbol (idx);

  gfc_get_symbol ("idx2", sub_ns, &idx2);
  idx2->ts.type = BT_INTEGER;
  idx2->ts.kind = gfc_index_integer_kind;
  idx2->attr.flavor = FL_VARIABLE;
  idx2->attr.artificial = 1;
  gfc_set_sym_referenced (idx2);
  gfc_commit_symbol (idx2);

  gfc_get_symbol ("offset", sub_ns, &offset);
  offset->ts.type = BT_INTEGER;
  offset->ts.kind = gfc_index_integer_kind;
  offset->attr.flavor = FL_VARIABLE;
  offset->attr.artificial = 1;
  gfc_set_sym_referenced (offset);
  gfc_commit_symbol (offset);

  /* Create RANK expression.  */
  rank = gfc_build_intrinsic_call (sub_ns, GFC_ISYM_RANK, "rank",
				   gfc_current_locus, 1,
				   gfc_lval_expr_from_sym (array));
  if (rank->ts.kind != idx->ts.kind)
    gfc_convert_type_warn (rank, &idx->ts, 2, 0);

  /* Create is_contiguous variable.  */
  gfc_get_symbol ("is_contiguous", sub_ns, &is_contiguous);
  is_contiguous->ts.type = BT_LOGICAL;
  is_contiguous->ts.kind = gfc_default_logical_kind;
  is_contiguous->attr.flavor = FL_VARIABLE;
  is_contiguous->attr.artificial = 1;
  gfc_set_sym_referenced (is_contiguous);
  gfc_commit_symbol (is_contiguous);

  /* Create "sizes(0..rank)" variable, which contains the multiplied
     up extent of the dimensions, i.e. sizes(0) = 1, sizes(1) = extent(dim=1),
     sizes(2) = sizes(1) * extent(dim=2) etc.  */
  gfc_get_symbol ("sizes", sub_ns, &sizes);
  sizes->ts.type = BT_INTEGER;
  sizes->ts.kind = gfc_index_integer_kind;
  sizes->attr.flavor = FL_VARIABLE;
  sizes->attr.dimension = 1;
  sizes->attr.artificial = 1;
  sizes->as = gfc_get_array_spec();
  sizes->attr.intent = INTENT_INOUT;
  sizes->as->type = AS_EXPLICIT;
  sizes->as->rank = 1;
  sizes->as->lower[0] = gfc_get_int_expr (gfc_index_integer_kind, NULL, 0);
  sizes->as->upper[0] = gfc_copy_expr (rank);
  gfc_set_sym_referenced (sizes);
  gfc_commit_symbol (sizes);

  /* Create "strides(1..rank)" variable, which contains the strides per
     dimension.  */
  gfc_get_symbol ("strides", sub_ns, &strides);
  strides->ts.type = BT_INTEGER;
  strides->ts.kind = gfc_index_integer_kind;
  strides->attr.flavor = FL_VARIABLE;
  strides->attr.dimension = 1;
  strides->attr.artificial = 1;
  strides->as = gfc_get_array_spec();
  strides->attr.intent = INTENT_INOUT;
  strides->as->type = AS_EXPLICIT;
  strides->as->rank = 1;
  strides->as->lower[0] = gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
  strides->as->upper[0] = gfc_copy_expr (rank);
  gfc_set_sym_referenced (strides);
  gfc_commit_symbol (strides);


  /* Set return value to 0.  */
  last_code = gfc_get_code (EXEC_ASSIGN);
  last_code->expr1 = gfc_lval_expr_from_sym (final);
  last_code->expr2 = gfc_get_int_expr (4, NULL, 0);
  sub_ns->code = last_code;

  /* Set:  is_contiguous = .true.  */
  last_code->next = gfc_get_code (EXEC_ASSIGN);
  last_code = last_code->next;
  last_code->expr1 = gfc_lval_expr_from_sym (is_contiguous);
  last_code->expr2 = gfc_get_logical_expr (gfc_default_logical_kind,
					   &gfc_current_locus, true);

  /* Set:  sizes(0) = 1.  */
  last_code->next = gfc_get_code (EXEC_ASSIGN);
  last_code = last_code->next;
  last_code->expr1 = gfc_lval_expr_from_sym (sizes);
  last_code->expr1->ref = gfc_get_ref ();
  last_code->expr1->ref->type = REF_ARRAY;
  last_code->expr1->ref->u.ar.type = AR_ELEMENT;
  last_code->expr1->ref->u.ar.dimen = 1;
  last_code->expr1->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
  last_code->expr1->ref->u.ar.start[0]
		= gfc_get_int_expr (gfc_index_integer_kind, NULL, 0);
  last_code->expr1->ref->u.ar.as = sizes->as;
  last_code->expr2 = gfc_get_int_expr (gfc_default_integer_kind, NULL, 1);

  /* Create:
     DO idx = 1, rank
       strides(idx) = _F._stride (array, dim=idx)
       sizes(idx) = sizes(i-1) * size(array, dim=idx, kind=index_kind)
       if (strides (idx) /= sizes(i-1)) is_contiguous = .false.
     END DO.  */

  /* Create loop.  */
  iter = gfc_get_iterator ();
  iter->var = gfc_lval_expr_from_sym (idx);
  iter->start = gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
  iter->end = gfc_copy_expr (rank);
  iter->step = gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
  last_code->next = gfc_get_code (EXEC_DO);
  last_code = last_code->next;
  last_code->ext.iterator = iter;
  last_code->block = gfc_get_code (EXEC_DO);

  /* strides(idx) = _F._stride(array,dim=idx).  */
  last_code->block->next = gfc_get_code (EXEC_ASSIGN);
  block = last_code->block->next;

  block->expr1 = gfc_lval_expr_from_sym (strides);
  block->expr1->ref = gfc_get_ref ();
  block->expr1->ref->type = REF_ARRAY;
  block->expr1->ref->u.ar.type = AR_ELEMENT;
  block->expr1->ref->u.ar.dimen = 1;
  block->expr1->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
  block->expr1->ref->u.ar.start[0] = gfc_lval_expr_from_sym (idx);
  block->expr1->ref->u.ar.as = strides->as;

  block->expr2 = gfc_build_intrinsic_call (sub_ns, GFC_ISYM_STRIDE, "stride",
					   gfc_current_locus, 2,
					   gfc_lval_expr_from_sym (array),
					   gfc_lval_expr_from_sym (idx));

  /* sizes(idx) = sizes(idx-1) * size(array,dim=idx, kind=index_kind).  */
  block->next = gfc_get_code (EXEC_ASSIGN);
  block = block->next;

  /* sizes(idx) = ...  */
  block->expr1 = gfc_lval_expr_from_sym (sizes);
  block->expr1->ref = gfc_get_ref ();
  block->expr1->ref->type = REF_ARRAY;
  block->expr1->ref->u.ar.type = AR_ELEMENT;
  block->expr1->ref->u.ar.dimen = 1;
  block->expr1->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
  block->expr1->ref->u.ar.start[0] = gfc_lval_expr_from_sym (idx);
  block->expr1->ref->u.ar.as = sizes->as;

  block->expr2 = gfc_get_expr ();
  block->expr2->expr_type = EXPR_OP;
  block->expr2->value.op.op = INTRINSIC_TIMES;

  /* sizes(idx-1).  */
  block->expr2->value.op.op1 = gfc_lval_expr_from_sym (sizes);
  block->expr2->value.op.op1->ref = gfc_get_ref ();
  block->expr2->value.op.op1->ref->type = REF_ARRAY;
  block->expr2->value.op.op1->ref->u.ar.as = sizes->as;
  block->expr2->value.op.op1->ref->u.ar.type = AR_ELEMENT;
  block->expr2->value.op.op1->ref->u.ar.dimen = 1;
  block->expr2->value.op.op1->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
  block->expr2->value.op.op1->ref->u.ar.start[0] = gfc_get_expr ();
  block->expr2->value.op.op1->ref->u.ar.start[0]->expr_type = EXPR_OP;
  block->expr2->value.op.op1->ref->u.ar.start[0]->value.op.op = INTRINSIC_MINUS;
  block->expr2->value.op.op1->ref->u.ar.start[0]->value.op.op1
	= gfc_lval_expr_from_sym (idx);
  block->expr2->value.op.op1->ref->u.ar.start[0]->value.op.op2
	= gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
  block->expr2->value.op.op1->ref->u.ar.start[0]->ts
	= block->expr2->value.op.op1->ref->u.ar.start[0]->value.op.op1->ts;

  /* size(array, dim=idx, kind=index_kind).  */
  block->expr2->value.op.op2
	= gfc_build_intrinsic_call (sub_ns, GFC_ISYM_SIZE, "size",
				    gfc_current_locus, 3,
				    gfc_lval_expr_from_sym (array),
				    gfc_lval_expr_from_sym (idx),
				    gfc_get_int_expr (gfc_index_integer_kind,
						      NULL,
						      gfc_index_integer_kind));
  block->expr2->value.op.op2->ts.kind = gfc_index_integer_kind;
  block->expr2->ts = idx->ts;

  /* if (strides (idx) /= sizes(idx-1)) is_contiguous = .false.  */
  block->next = gfc_get_code (EXEC_IF);
  block = block->next;

  block->block = gfc_get_code (EXEC_IF);
  block = block->block;

  /* if condition: strides(idx) /= sizes(idx-1).  */
  block->expr1 = gfc_get_expr ();
  block->expr1->ts.type = BT_LOGICAL;
  block->expr1->ts.kind = gfc_default_logical_kind;
  block->expr1->expr_type = EXPR_OP;
  block->expr1->where = gfc_current_locus;
  block->expr1->value.op.op = INTRINSIC_NE;

  block->expr1->value.op.op1 = gfc_lval_expr_from_sym (strides);
  block->expr1->value.op.op1->ref = gfc_get_ref ();
  block->expr1->value.op.op1->ref->type = REF_ARRAY;
  block->expr1->value.op.op1->ref->u.ar.type = AR_ELEMENT;
  block->expr1->value.op.op1->ref->u.ar.dimen = 1;
  block->expr1->value.op.op1->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
  block->expr1->value.op.op1->ref->u.ar.start[0] = gfc_lval_expr_from_sym (idx);
  block->expr1->value.op.op1->ref->u.ar.as = strides->as;

  block->expr1->value.op.op2 = gfc_lval_expr_from_sym (sizes);
  block->expr1->value.op.op2->ref = gfc_get_ref ();
  block->expr1->value.op.op2->ref->type = REF_ARRAY;
  block->expr1->value.op.op2->ref->u.ar.as = sizes->as;
  block->expr1->value.op.op2->ref->u.ar.type = AR_ELEMENT;
  block->expr1->value.op.op2->ref->u.ar.dimen = 1;
  block->expr1->value.op.op2->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
  block->expr1->value.op.op2->ref->u.ar.start[0] = gfc_get_expr ();
  block->expr1->value.op.op2->ref->u.ar.start[0]->expr_type = EXPR_OP;
  block->expr1->value.op.op2->ref->u.ar.start[0]->value.op.op = INTRINSIC_MINUS;
  block->expr1->value.op.op2->ref->u.ar.start[0]->value.op.op1
	= gfc_lval_expr_from_sym (idx);
  block->expr1->value.op.op2->ref->u.ar.start[0]->value.op.op2
	= gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
  block->expr1->value.op.op2->ref->u.ar.start[0]->ts
	= block->expr1->value.op.op2->ref->u.ar.start[0]->value.op.op1->ts;

  /* if body: is_contiguous = .false.  */
  block->next = gfc_get_code (EXEC_ASSIGN);
  block = block->next;
  block->expr1 = gfc_lval_expr_from_sym (is_contiguous);
  block->expr2 = gfc_get_logical_expr (gfc_default_logical_kind,
				       &gfc_current_locus, false);

  /* Obtain the size (number of elements) of "array" MINUS ONE,
     which is used in the scalarization.  */
  gfc_get_symbol ("nelem", sub_ns, &nelem);
  nelem->ts.type = BT_INTEGER;
  nelem->ts.kind = gfc_index_integer_kind;
  nelem->attr.flavor = FL_VARIABLE;
  nelem->attr.artificial = 1;
  gfc_set_sym_referenced (nelem);
  gfc_commit_symbol (nelem);

  /* nelem = sizes (rank) - 1.  */
  last_code->next = gfc_get_code (EXEC_ASSIGN);
  last_code = last_code->next;

  last_code->expr1 = gfc_lval_expr_from_sym (nelem);

  last_code->expr2 = gfc_get_expr ();
  last_code->expr2->expr_type = EXPR_OP;
  last_code->expr2->value.op.op = INTRINSIC_MINUS;
  last_code->expr2->value.op.op2
	= gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
  last_code->expr2->ts = last_code->expr2->value.op.op2->ts;

  last_code->expr2->value.op.op1 = gfc_lval_expr_from_sym (sizes);
  last_code->expr2->value.op.op1->ref = gfc_get_ref ();
  last_code->expr2->value.op.op1->ref->type = REF_ARRAY;
  last_code->expr2->value.op.op1->ref->u.ar.type = AR_ELEMENT;
  last_code->expr2->value.op.op1->ref->u.ar.dimen = 1;
  last_code->expr2->value.op.op1->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
  last_code->expr2->value.op.op1->ref->u.ar.start[0] = gfc_copy_expr (rank);
  last_code->expr2->value.op.op1->ref->u.ar.as = sizes->as;

  /* Call final subroutines. We now generate code like:
     use iso_c_binding
     integer, pointer :: ptr
     type(c_ptr) :: cptr
     integer(c_intptr_t) :: i, addr

     select case (rank (array))
       case (3)
         ! If needed, the array is packed
	 call final_rank3 (array)
       case default:
	 do i = 0, size (array)-1
	   addr = transfer (c_loc (array), addr) + i * stride
	   call c_f_pointer (transfer (addr, cptr), ptr)
	   call elemental_final (ptr)
	 end do
     end select */

  if (derived->f2k_derived && derived->f2k_derived->finalizers)
    {
      gfc_finalizer *fini, *fini_elem = NULL;

      gfc_get_symbol ("ptr1", sub_ns, &ptr);
      ptr->ts.type = BT_DERIVED;
      ptr->ts.u.derived = derived;
      ptr->attr.flavor = FL_VARIABLE;
      ptr->attr.pointer = 1;
      ptr->attr.artificial = 1;
      gfc_set_sym_referenced (ptr);
      gfc_commit_symbol (ptr);

      /* SELECT CASE (RANK (array)).  */
      last_code->next = gfc_get_code (EXEC_SELECT);
      last_code = last_code->next;
      last_code->expr1 = gfc_copy_expr (rank);
      block = NULL;

      for (fini = derived->f2k_derived->finalizers; fini; fini = fini->next)
	{
	  gcc_assert (fini->proc_tree);   /* Should have been set in gfc_resolve_finalizers.  */
	  if (fini->proc_tree->n.sym->attr.elemental)
	    {
	      fini_elem = fini;
	      continue;
	    }

	  /* CASE (fini_rank).  */
	  if (block)
	    {
	      block->block = gfc_get_code (EXEC_SELECT);
	      block = block->block;
	    }
	  else
	    {
	      block = gfc_get_code (EXEC_SELECT);
	      last_code->block = block;
	    }
	  block->ext.block.case_list = gfc_get_case ();
	  block->ext.block.case_list->where = gfc_current_locus;
	  if (fini->proc_tree->n.sym->formal->sym->attr.dimension)
	    block->ext.block.case_list->low
	     = gfc_get_int_expr (gfc_default_integer_kind, NULL,
				 fini->proc_tree->n.sym->formal->sym->as->rank);
	  else
	    block->ext.block.case_list->low
		= gfc_get_int_expr (gfc_default_integer_kind, NULL, 0);
	  block->ext.block.case_list->high
		= gfc_copy_expr (block->ext.block.case_list->low);

	  /* CALL fini_rank (array) - possibly with packing.  */
          if (fini->proc_tree->n.sym->formal->sym->attr.dimension)
	    finalizer_insert_packed_call (block, fini, array, byte_stride,
					  idx, ptr, nelem, strides,
					  sizes, idx2, offset, is_contiguous,
					  rank, sub_ns);
	  else
	    {
	      block->next = gfc_get_code (EXEC_CALL);
	      block->next->symtree = fini->proc_tree;
	      block->next->resolved_sym = fini->proc_tree->n.sym;
	      block->next->ext.actual = gfc_get_actual_arglist ();
	      block->next->ext.actual->expr = gfc_lval_expr_from_sym (array);
	    }
	}

      /* Elemental call - scalarized.  */
      if (fini_elem)
	{
	  /* CASE DEFAULT.  */
	  if (block)
	    {
	      block->block = gfc_get_code (EXEC_SELECT);
	      block = block->block;
	    }
	  else
	    {
	      block = gfc_get_code (EXEC_SELECT);
	      last_code->block = block;
	    }
	  block->ext.block.case_list = gfc_get_case ();

	  /* Create loop.  */
	  iter = gfc_get_iterator ();
	  iter->var = gfc_lval_expr_from_sym (idx);
	  iter->start = gfc_get_int_expr (gfc_index_integer_kind, NULL, 0);
	  iter->end = gfc_lval_expr_from_sym (nelem);
	  iter->step = gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
	  block->next = gfc_get_code (EXEC_DO);
	  block = block->next;
	  block->ext.iterator = iter;
	  block->block = gfc_get_code (EXEC_DO);

	  /* Offset calculation.  */
	  block = finalization_get_offset (idx, idx2, offset, strides, sizes,
					   byte_stride, rank, block->block,
					   sub_ns);

	  /* Create code for
	     CALL C_F_POINTER (TRANSFER (TRANSFER (C_LOC (array, cptr), c_intptr)
			       + offset, c_ptr), ptr).  */
	  block->next
		= finalization_scalarizer (array, ptr,
					   gfc_lval_expr_from_sym (offset),
					   sub_ns);
	  block = block->next;

	  /* CALL final_elemental (array).  */
	  block->next = gfc_get_code (EXEC_CALL);
	  block = block->next;
	  block->symtree = fini_elem->proc_tree;
	  block->resolved_sym = fini_elem->proc_sym;
	  block->ext.actual = gfc_get_actual_arglist ();
	  block->ext.actual->expr = gfc_lval_expr_from_sym (ptr);
	}
    }

  /* Finalize and deallocate allocatable components. The same manual
     scalarization is used as above.  */

  if (finalizable_comp)
    {
      gfc_symbol *stat;
      gfc_code *block = NULL;

      if (!ptr)
	{
	  gfc_get_symbol ("ptr2", sub_ns, &ptr);
	  ptr->ts.type = BT_DERIVED;
	  ptr->ts.u.derived = derived;
	  ptr->attr.flavor = FL_VARIABLE;
	  ptr->attr.pointer = 1;
	  ptr->attr.artificial = 1;
	  gfc_set_sym_referenced (ptr);
	  gfc_commit_symbol (ptr);
	}

      gfc_get_symbol ("ignore", sub_ns, &stat);
      stat->attr.flavor = FL_VARIABLE;
      stat->attr.artificial = 1;
      stat->ts.type = BT_INTEGER;
      stat->ts.kind = gfc_default_integer_kind;
      gfc_set_sym_referenced (stat);
      gfc_commit_symbol (stat);

      /* Create loop.  */
      iter = gfc_get_iterator ();
      iter->var = gfc_lval_expr_from_sym (idx);
      iter->start = gfc_get_int_expr (gfc_index_integer_kind, NULL, 0);
      iter->end = gfc_lval_expr_from_sym (nelem);
      iter->step = gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);
      last_code->next = gfc_get_code (EXEC_DO);
      last_code = last_code->next;
      last_code->ext.iterator = iter;
      last_code->block = gfc_get_code (EXEC_DO);

      /* Offset calculation.  */
      block = finalization_get_offset (idx, idx2, offset, strides, sizes,
				       byte_stride, rank, last_code->block,
				       sub_ns);

      /* Create code for
	 CALL C_F_POINTER (TRANSFER (TRANSFER (C_LOC (array, cptr), c_intptr)
			   + idx * stride, c_ptr), ptr).  */
      block->next = finalization_scalarizer (array, ptr,
					     gfc_lval_expr_from_sym(offset),
					     sub_ns);
      block = block->next;

      for (comp = derived->components; comp; comp = comp->next)
	{
	  if (comp == derived->components && derived->attr.extension
	      && ancestor_wrapper && ancestor_wrapper->expr_type != EXPR_NULL)
	    continue;

	  finalize_component (gfc_lval_expr_from_sym (ptr), derived, comp,
			      stat, fini_coarray, &block, sub_ns);
	  if (!last_code->block->next)
	    last_code->block->next = block;
	}

    }

  /* Call the finalizer of the ancestor.  */
  if (ancestor_wrapper && ancestor_wrapper->expr_type != EXPR_NULL)
    {
      last_code->next = gfc_get_code (EXEC_CALL);
      last_code = last_code->next;
      last_code->symtree = ancestor_wrapper->symtree;
      last_code->resolved_sym = ancestor_wrapper->symtree->n.sym;

      last_code->ext.actual = gfc_get_actual_arglist ();
      last_code->ext.actual->expr = gfc_lval_expr_from_sym (array);
      last_code->ext.actual->next = gfc_get_actual_arglist ();
      last_code->ext.actual->next->expr = gfc_lval_expr_from_sym (byte_stride);
      last_code->ext.actual->next->next = gfc_get_actual_arglist ();
      last_code->ext.actual->next->next->expr
			= gfc_lval_expr_from_sym (fini_coarray);
    }

  gfc_free_expr (rank);
  vtab_final->initializer = gfc_lval_expr_from_sym (final);
  vtab_final->ts.interface = final;
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


/* Find or generate the symbol for a derived type's vtab.  */

gfc_symbol *
gfc_find_derived_vtab (gfc_symbol *derived)
{
  gfc_namespace *ns;
  gfc_symbol *vtab = NULL, *vtype = NULL, *found_sym = NULL, *def_init = NULL;
  gfc_symbol *copy = NULL, *src = NULL, *dst = NULL;

  /* Find the top-level namespace.  */
  for (ns = gfc_current_ns; ns; ns = ns->parent)
    if (!ns->parent)
      break;

  /* If the type is a class container, use the underlying derived type.  */
  if (!derived->attr.unlimited_polymorphic && derived->attr.is_class)
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
	  if (!gfc_add_flavor (&vtab->attr, FL_VARIABLE, NULL,
			       &gfc_current_locus))
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
	      if (!gfc_add_flavor (&vtype->attr, FL_DERIVED, NULL,
				   &gfc_current_locus))
		goto cleanup;
	      vtype->attr.access = ACCESS_PUBLIC;
	      vtype->attr.vtype = 1;
	      gfc_set_sym_referenced (vtype);

	      /* Add component '_hash'.  */
	      if (!gfc_add_component (vtype, "_hash", &c))
		goto cleanup;
	      c->ts.type = BT_INTEGER;
	      c->ts.kind = 4;
	      c->attr.access = ACCESS_PRIVATE;
	      c->initializer = gfc_get_int_expr (gfc_default_integer_kind,
						 NULL, derived->hash_value);

	      /* Add component '_size'.  */
	      if (!gfc_add_component (vtype, "_size", &c))
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
	      if (!gfc_add_component (vtype, "_extends", &c))
		goto cleanup;
	      c->attr.pointer = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      if (!derived->attr.unlimited_polymorphic)
		parent = gfc_get_derived_super_type (derived);
	      else
		parent = NULL;

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

	      if (!derived->attr.unlimited_polymorphic
		  && derived->components == NULL
		  && !derived->attr.zero_comp)
		{
		  /* At this point an error must have occurred.
		     Prevent further errors on the vtype components.  */
		  found_sym = vtab;
		  goto have_vtype;
		}

	      /* Add component _def_init.  */
	      if (!gfc_add_component (vtype, "_def_init", &c))
		goto cleanup;
	      c->attr.pointer = 1;
	      c->attr.artificial = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      c->ts.type = BT_DERIVED;
	      c->ts.u.derived = derived;
	      if (derived->attr.unlimited_polymorphic
		  || derived->attr.abstract)
		c->initializer = gfc_get_null_expr (NULL);
	      else
		{
		  /* Construct default initialization variable.  */
		  sprintf (name, "__def_init_%s", tname);
		  gfc_get_symbol (name, ns, &def_init);
		  def_init->attr.target = 1;
		  def_init->attr.artificial = 1;
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
	      if (!gfc_add_component (vtype, "_copy", &c))
		goto cleanup;
	      c->attr.proc_pointer = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      c->tb = XCNEW (gfc_typebound_proc);
	      c->tb->ppc = 1;
	      if (derived->attr.unlimited_polymorphic
		  || derived->attr.abstract)
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
		  copy->attr.subroutine = 1;
		  copy->attr.pure = 1;
		  copy->attr.artificial = 1;
		  copy->attr.if_source = IFSRC_DECL;
		  /* This is elemental so that arrays are automatically
		     treated correctly by the scalarizer.  */
		  copy->attr.elemental = 1;
		  if (ns->proc_name->attr.flavor == FL_MODULE)
		    copy->module = ns->proc_name->name;
		  gfc_set_sym_referenced (copy);
		  /* Set up formal arguments.  */
		  gfc_get_symbol ("src", sub_ns, &src);
		  src->ts.type = BT_DERIVED;
		  src->ts.u.derived = derived;
		  src->attr.flavor = FL_VARIABLE;
		  src->attr.dummy = 1;
		  src->attr.artificial = 1;
     		  src->attr.intent = INTENT_IN;
		  gfc_set_sym_referenced (src);
		  copy->formal = gfc_get_formal_arglist ();
		  copy->formal->sym = src;
		  gfc_get_symbol ("dst", sub_ns, &dst);
		  dst->ts.type = BT_DERIVED;
		  dst->ts.u.derived = derived;
		  dst->attr.flavor = FL_VARIABLE;
		  dst->attr.dummy = 1;
		  dst->attr.artificial = 1;
		  dst->attr.intent = INTENT_INOUT;
		  gfc_set_sym_referenced (dst);
		  copy->formal->next = gfc_get_formal_arglist ();
		  copy->formal->next->sym = dst;
		  /* Set up code.  */
		  sub_ns->code = gfc_get_code (EXEC_INIT_ASSIGN);
		  sub_ns->code->expr1 = gfc_lval_expr_from_sym (dst);
		  sub_ns->code->expr2 = gfc_lval_expr_from_sym (src);
		  /* Set initializer.  */
		  c->initializer = gfc_lval_expr_from_sym (copy);
		  c->ts.interface = copy;
		}

	      /* Add component _final, which contains a procedure pointer to
		 a wrapper which handles both the freeing of allocatable
		 components and the calls to finalization subroutines.
		 Note: The actual wrapper function can only be generated
		 at resolution time.  */
	      if (!gfc_add_component (vtype, "_final", &c))
		goto cleanup;
	      c->attr.proc_pointer = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      c->tb = XCNEW (gfc_typebound_proc);
	      c->tb->ppc = 1;
	      generate_finalization_wrapper (derived, ns, tname, c);

	      /* Add procedure pointers for type-bound procedures.  */
	      if (!derived->attr.unlimited_polymorphic)
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


/* Check if a derived type is finalizable. That is the case if it
   (1) has a FINAL subroutine or
   (2) has a nonpointer nonallocatable component of finalizable type.
   If it is finalizable, return an expression containing the
   finalization wrapper.  */

bool
gfc_is_finalizable (gfc_symbol *derived, gfc_expr **final_expr)
{
  gfc_symbol *vtab;
  gfc_component *c;

  /* (1) Check for FINAL subroutines.  */
  if (derived->f2k_derived && derived->f2k_derived->finalizers)
    goto yes;

  /* (2) Check for components of finalizable type.  */
  for (c = derived->components; c; c = c->next)
    if (c->ts.type == BT_DERIVED
	&& !c->attr.pointer && !c->attr.proc_pointer && !c->attr.allocatable
	&& gfc_is_finalizable (c->ts.u.derived, NULL))
      goto yes;

  return false;

yes:
  /* Make sure vtab is generated.  */
  vtab = gfc_find_derived_vtab (derived);
  if (final_expr)
    {
      /* Return finalizer expression.  */
      gfc_component *final;
      final = vtab->ts.u.derived->components->next->next->next->next->next;
      gcc_assert (strcmp (final->name, "_final") == 0);
      gcc_assert (final->initializer
		  && final->initializer->expr_type != EXPR_NULL);
      *final_expr = final->initializer;
    }
  return true;
}


/* Find (or generate) the symbol for an intrinsic type's vtab.  This is
   needed to support unlimited polymorphism.  */

static gfc_symbol *
find_intrinsic_vtab (gfc_typespec *ts)
{
  gfc_namespace *ns;
  gfc_symbol *vtab = NULL, *vtype = NULL, *found_sym = NULL;
  gfc_symbol *copy = NULL, *src = NULL, *dst = NULL;
  int charlen = 0;

  if (ts->type == BT_CHARACTER && !ts->deferred && ts->u.cl && ts->u.cl->length
      && ts->u.cl->length->expr_type == EXPR_CONSTANT)
    charlen = mpz_get_si (ts->u.cl->length->value.integer);

  /* Find the top-level namespace.  */
  for (ns = gfc_current_ns; ns; ns = ns->parent)
    if (!ns->parent)
      break;

  if (ns)
    {
      char name[GFC_MAX_SYMBOL_LEN+1], tname[GFC_MAX_SYMBOL_LEN+1];

      if (ts->type == BT_CHARACTER)
	sprintf (tname, "%s_%d_%d", gfc_basic_typename (ts->type),
		 charlen, ts->kind);
      else
	sprintf (tname, "%s_%d_", gfc_basic_typename (ts->type), ts->kind);

      sprintf (name, "__vtab_%s", tname);

      /* Look for the vtab symbol in the top-level namespace only.  */
      gfc_find_symbol (name, ns, 0, &vtab);

      if (vtab == NULL)
	{
	  gfc_get_symbol (name, ns, &vtab);
	  vtab->ts.type = BT_DERIVED;
	  if (!gfc_add_flavor (&vtab->attr, FL_VARIABLE, NULL,
			       &gfc_current_locus))
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
	      int hash;
	      gfc_namespace *sub_ns;
	      gfc_namespace *contained;
	      gfc_expr *e;

	      gfc_get_symbol (name, ns, &vtype);
	      if (!gfc_add_flavor (&vtype->attr, FL_DERIVED, NULL,
				   &gfc_current_locus))
		goto cleanup;
	      vtype->attr.access = ACCESS_PUBLIC;
	      vtype->attr.vtype = 1;
	      gfc_set_sym_referenced (vtype);

	      /* Add component '_hash'.  */
	      if (!gfc_add_component (vtype, "_hash", &c))
		goto cleanup;
	      c->ts.type = BT_INTEGER;
	      c->ts.kind = 4;
	      c->attr.access = ACCESS_PRIVATE;
	      hash = gfc_intrinsic_hash_value (ts);
	      c->initializer = gfc_get_int_expr (gfc_default_integer_kind,
						 NULL, hash);

	      /* Add component '_size'.  */
	      if (!gfc_add_component (vtype, "_size", &c))
		goto cleanup;
	      c->ts.type = BT_INTEGER;
	      c->ts.kind = 4;
	      c->attr.access = ACCESS_PRIVATE;

	      /* Build a minimal expression to make use of
		 target-memory.c/gfc_element_size for 'size'.  Special handling
		 for character arrays, that are not constant sized: to support
		 len (str) * kind, only the kind information is stored in the
		 vtab.  */
	      e = gfc_get_expr ();
	      e->ts = *ts;
	      e->expr_type = EXPR_VARIABLE;
	      c->initializer = gfc_get_int_expr (gfc_default_integer_kind,
						 NULL,
						 ts->type == BT_CHARACTER
						 && charlen == 0 ?
						   ts->kind :
						   (int)gfc_element_size (e));
	      gfc_free_expr (e);

	      /* Add component _extends.  */
	      if (!gfc_add_component (vtype, "_extends", &c))
		goto cleanup;
	      c->attr.pointer = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      c->ts.type = BT_VOID;
	      c->initializer = gfc_get_null_expr (NULL);

	      /* Add component _def_init.  */
	      if (!gfc_add_component (vtype, "_def_init", &c))
		goto cleanup;
	      c->attr.pointer = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      c->ts.type = BT_VOID;
	      c->initializer = gfc_get_null_expr (NULL);

	      /* Add component _copy.  */
	      if (!gfc_add_component (vtype, "_copy", &c))
		goto cleanup;
	      c->attr.proc_pointer = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      c->tb = XCNEW (gfc_typebound_proc);
	      c->tb->ppc = 1;

	      if (ts->type != BT_CHARACTER)
		sprintf (name, "__copy_%s", tname);
	      else
		{
		  /* __copy is always the same for characters.
		     Check to see if copy function already exists.  */
		  sprintf (name, "__copy_character_%d", ts->kind);
		  contained = ns->contained;
		  for (; contained; contained = contained->sibling)
		    if (contained->proc_name
			&& strcmp (name, contained->proc_name->name) == 0)
		      {
			copy = contained->proc_name;
			goto got_char_copy;
		      }
		}

	      /* Set up namespace.  */
	      sub_ns = gfc_get_namespace (ns, 0);
	      sub_ns->sibling = ns->contained;
	      ns->contained = sub_ns;
	      sub_ns->resolved = 1;
	      /* Set up procedure symbol.  */
	      gfc_get_symbol (name, sub_ns, &copy);
	      sub_ns->proc_name = copy;
	      copy->attr.flavor = FL_PROCEDURE;
	      copy->attr.subroutine = 1;
	      copy->attr.pure = 1;
	      copy->attr.if_source = IFSRC_DECL;
	      /* This is elemental so that arrays are automatically
		 treated correctly by the scalarizer.  */
	      copy->attr.elemental = 1;
	      if (ns->proc_name->attr.flavor == FL_MODULE)
		copy->module = ns->proc_name->name;
		  gfc_set_sym_referenced (copy);
	      /* Set up formal arguments.  */
	      gfc_get_symbol ("src", sub_ns, &src);
	      src->ts.type = ts->type;
	      src->ts.kind = ts->kind;
	      src->attr.flavor = FL_VARIABLE;
	      src->attr.dummy = 1;
	      src->attr.intent = INTENT_IN;
	      gfc_set_sym_referenced (src);
	      copy->formal = gfc_get_formal_arglist ();
	      copy->formal->sym = src;
	      gfc_get_symbol ("dst", sub_ns, &dst);
	      dst->ts.type = ts->type;
	      dst->ts.kind = ts->kind;
	      dst->attr.flavor = FL_VARIABLE;
	      dst->attr.dummy = 1;
	      dst->attr.intent = INTENT_INOUT;
	      gfc_set_sym_referenced (dst);
	      copy->formal->next = gfc_get_formal_arglist ();
	      copy->formal->next->sym = dst;
	      /* Set up code.  */
	      sub_ns->code = gfc_get_code (EXEC_INIT_ASSIGN);
	      sub_ns->code->expr1 = gfc_lval_expr_from_sym (dst);
	      sub_ns->code->expr2 = gfc_lval_expr_from_sym (src);
	    got_char_copy:
	      /* Set initializer.  */
	      c->initializer = gfc_lval_expr_from_sym (copy);
	      c->ts.interface = copy;

	      /* Add component _final.  */
	      if (!gfc_add_component (vtype, "_final", &c))
		goto cleanup;
	      c->attr.proc_pointer = 1;
	      c->attr.access = ACCESS_PRIVATE;
	      c->tb = XCNEW (gfc_typebound_proc);
	      c->tb->ppc = 1;
	      c->initializer = gfc_get_null_expr (NULL);
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
    {
      gfc_commit_symbol (vtab);
      if (vtype)
	gfc_commit_symbol (vtype);
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


/*  Find (or generate) a vtab for an arbitrary type (derived or intrinsic).  */

gfc_symbol *
gfc_find_vtab (gfc_typespec *ts)
{
  switch (ts->type)
    {
    case BT_UNKNOWN:
      return NULL;
    case BT_DERIVED:
      return gfc_find_derived_vtab (ts->u.derived);
    case BT_CLASS:
      return gfc_find_derived_vtab (ts->u.derived->components->ts.u.derived);
    default:
      return find_intrinsic_vtab (ts);
    }
}


/* General worker function to find either a type-bound procedure or a
   type-bound user operator.  */

static gfc_symtree*
find_typebound_proc_uop (gfc_symbol* derived, bool* t,
			 const char* name, bool noaccess, bool uop,
			 locus* where)
{
  gfc_symtree* res;
  gfc_symtree* root;

  /* Set default to failure.  */
  if (t)
    *t = false;

  if (derived->f2k_derived)
    /* Set correct symbol-root.  */
    root = (uop ? derived->f2k_derived->tb_uop_root
		: derived->f2k_derived->tb_sym_root);
  else
    return NULL;

  /* Try to find it in the current type's namespace.  */
  res = gfc_find_symtree (root, name);
  if (res && res->n.tb && !res->n.tb->error)
    {
      /* We found one.  */
      if (t)
	*t = true;

      if (!noaccess && derived->attr.use_assoc
	  && res->n.tb->access == ACCESS_PRIVATE)
	{
	  if (where)
	    gfc_error ("%qs of %qs is PRIVATE at %L",
		       name, derived->name, where);
	  if (t)
	    *t = false;
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
gfc_find_typebound_proc (gfc_symbol* derived, bool* t,
			 const char* name, bool noaccess, locus* where)
{
  return find_typebound_proc_uop (derived, t, name, noaccess, false, where);
}

gfc_symtree*
gfc_find_typebound_user_op (gfc_symbol* derived, bool* t,
			    const char* name, bool noaccess, locus* where)
{
  return find_typebound_proc_uop (derived, t, name, noaccess, true, where);
}


/* Find a type-bound intrinsic operator looking recursively through the
   super-type hierarchy.  */

gfc_typebound_proc*
gfc_find_typebound_intrinsic_op (gfc_symbol* derived, bool* t,
				 gfc_intrinsic_op op, bool noaccess,
				 locus* where)
{
  gfc_typebound_proc* res;

  /* Set default to failure.  */
  if (t)
    *t = false;

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
	*t = true;

      if (!noaccess && derived->attr.use_assoc
	  && res->access == ACCESS_PRIVATE)
	{
	  if (where)
	    gfc_error ("%qs of %qs is PRIVATE at %L",
		       gfc_op2string (op), derived->name, where);
	  if (t)
	    *t = false;
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
