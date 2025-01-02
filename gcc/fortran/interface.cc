/* Deal with interfaces.
   Copyright (C) 2000-2025 Free Software Foundation, Inc.
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


/* Deal with interfaces.  An explicit interface is represented as a
   singly linked list of formal argument structures attached to the
   relevant symbols.  For an implicit interface, the arguments don't
   point to symbols.  Explicit interfaces point to namespaces that
   contain the symbols within that interface.

   Implicit interfaces are linked together in a singly linked list
   along the next_if member of symbol nodes.  Since a particular
   symbol can only have a single explicit interface, the symbol cannot
   be part of multiple lists and a single next-member suffices.

   This is not the case for general classes, though.  An operator
   definition is independent of just about all other uses and has it's
   own head pointer.

   Nameless interfaces:
     Nameless interfaces create symbols with explicit interfaces within
     the current namespace.  They are otherwise unlinked.

   Generic interfaces:
     The generic name points to a linked list of symbols.  Each symbol
     has an explicit interface.  Each explicit interface has its own
     namespace containing the arguments.  Module procedures are symbols in
     which the interface is added later when the module procedure is parsed.

   User operators:
     User-defined operators are stored in a their own set of symtrees
     separate from regular symbols.  The symtrees point to gfc_user_op
     structures which in turn head up a list of relevant interfaces.

   Extended intrinsics and assignment:
     The head of these interface lists are stored in the containing namespace.

   Implicit interfaces:
     An implicit interface is represented as a singly linked list of
     formal argument list structures that don't point to any symbol
     nodes -- they just contain types.


   When a subprogram is defined, the program unit's name points to an
   interface as usual, but the link to the namespace is NULL and the
   formal argument list points to symbols within the same namespace as
   the program unit name.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "gfortran.h"
#include "match.h"
#include "arith.h"

/* The current_interface structure holds information about the
   interface currently being parsed.  This structure is saved and
   restored during recursive interfaces.  */

gfc_interface_info current_interface;


/* Free the leading members of the gfc_interface linked list given in INTR
   up to the END element (exclusive: the END element is not freed).
   If END is not nullptr, it is assumed that END is in the linked list starting
   with INTR.  */

static void
free_interface_elements_until (gfc_interface *intr, gfc_interface *end)
{
  gfc_interface *next;

  for (; intr != end; intr = next)
    {
      next = intr->next;
      free (intr);
    }
}


/* Free a singly linked list of gfc_interface structures.  */

void
gfc_free_interface (gfc_interface *intr)
{
  free_interface_elements_until (intr, nullptr);
}


/* Update the interface pointer given by IFC_PTR to make it point to TAIL.
   It is expected that TAIL (if non-null) is in the list pointed to by
   IFC_PTR, hence the tail of it.  The members of the list before TAIL are
   freed before the pointer reassignment.  */

void
gfc_drop_interface_elements_before (gfc_interface **ifc_ptr,
				    gfc_interface *tail)
{
  if (ifc_ptr == nullptr)
    return;

  free_interface_elements_until (*ifc_ptr, tail);
  *ifc_ptr = tail;
}


/* Change the operators unary plus and minus into binary plus and
   minus respectively, leaving the rest unchanged.  */

static gfc_intrinsic_op
fold_unary_intrinsic (gfc_intrinsic_op op)
{
  switch (op)
    {
    case INTRINSIC_UPLUS:
      op = INTRINSIC_PLUS;
      break;
    case INTRINSIC_UMINUS:
      op = INTRINSIC_MINUS;
      break;
    default:
      break;
    }

  return op;
}


/* Return the operator depending on the DTIO moded string.  Note that
   these are not operators in the normal sense and so have been placed
   beyond GFC_INTRINSIC_END in gfortran.h:enum gfc_intrinsic_op.  */

static gfc_intrinsic_op
dtio_op (char* mode)
{
  if (strcmp (mode, "formatted") == 0)
    return INTRINSIC_FORMATTED;
  if (strcmp (mode, "unformatted") == 0)
    return INTRINSIC_UNFORMATTED;
  return INTRINSIC_NONE;
}


/* Match a generic specification.  Depending on which type of
   interface is found, the 'name' or 'op' pointers may be set.
   This subroutine doesn't return MATCH_NO.  */

match
gfc_match_generic_spec (interface_type *type,
			char *name,
			gfc_intrinsic_op *op)
{
  char buffer[GFC_MAX_SYMBOL_LEN + 1];
  match m;
  gfc_intrinsic_op i;

  if (gfc_match (" assignment ( = )") == MATCH_YES)
    {
      *type = INTERFACE_INTRINSIC_OP;
      *op = INTRINSIC_ASSIGN;
      return MATCH_YES;
    }

  if (gfc_match (" operator ( %o )", &i) == MATCH_YES)
    {				/* Operator i/f */
      *type = INTERFACE_INTRINSIC_OP;
      *op = fold_unary_intrinsic (i);
      return MATCH_YES;
    }

  *op = INTRINSIC_NONE;
  if (gfc_match (" operator ( ") == MATCH_YES)
    {
      m = gfc_match_defined_op_name (buffer, 1);
      if (m == MATCH_NO)
	goto syntax;
      if (m != MATCH_YES)
	return MATCH_ERROR;

      m = gfc_match_char (')');
      if (m == MATCH_NO)
	goto syntax;
      if (m != MATCH_YES)
	return MATCH_ERROR;

      strcpy (name, buffer);
      *type = INTERFACE_USER_OP;
      return MATCH_YES;
    }

  if (gfc_match (" read ( %n )", buffer) == MATCH_YES)
    {
      *op = dtio_op (buffer);
      if (*op == INTRINSIC_FORMATTED)
	{
	  strcpy (name, gfc_code2string (dtio_procs, DTIO_RF));
	  *type = INTERFACE_DTIO;
	}
      if (*op == INTRINSIC_UNFORMATTED)
	{
	  strcpy (name, gfc_code2string (dtio_procs, DTIO_RUF));
	  *type = INTERFACE_DTIO;
	}
      if (*op != INTRINSIC_NONE)
	return MATCH_YES;
    }

  if (gfc_match (" write ( %n )", buffer) == MATCH_YES)
    {
      *op = dtio_op (buffer);
      if (*op == INTRINSIC_FORMATTED)
	{
	  strcpy (name, gfc_code2string (dtio_procs, DTIO_WF));
	  *type = INTERFACE_DTIO;
	}
      if (*op == INTRINSIC_UNFORMATTED)
	{
	  strcpy (name, gfc_code2string (dtio_procs, DTIO_WUF));
	  *type = INTERFACE_DTIO;
	}
      if (*op != INTRINSIC_NONE)
	return MATCH_YES;
    }

  if (gfc_match_name (buffer) == MATCH_YES)
    {
      strcpy (name, buffer);
      *type = INTERFACE_GENERIC;
      return MATCH_YES;
    }

  *type = INTERFACE_NAMELESS;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in generic specification at %C");
  return MATCH_ERROR;
}


/* Match one of the five F95 forms of an interface statement.  The
   matcher for the abstract interface follows.  */

match
gfc_match_interface (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  interface_type type;
  gfc_symbol *sym;
  gfc_intrinsic_op op;
  match m;

  m = gfc_match_space ();

  if (gfc_match_generic_spec (&type, name, &op) == MATCH_ERROR)
    return MATCH_ERROR;

  /* If we're not looking at the end of the statement now, or if this
     is not a nameless interface but we did not see a space, punt.  */
  if (gfc_match_eos () != MATCH_YES
      || (type != INTERFACE_NAMELESS && m != MATCH_YES))
    {
      gfc_error ("Syntax error: Trailing garbage in INTERFACE statement "
		 "at %C");
      return MATCH_ERROR;
    }

  current_interface.type = type;

  switch (type)
    {
    case INTERFACE_DTIO:
    case INTERFACE_GENERIC:
      if (gfc_get_symbol (name, NULL, &sym))
	return MATCH_ERROR;

      if (!sym->attr.generic
	  && !gfc_add_generic (&sym->attr, sym->name, NULL))
	return MATCH_ERROR;

      if (sym->attr.dummy)
	{
	  gfc_error ("Dummy procedure %qs at %C cannot have a "
		     "generic interface", sym->name);
	  return MATCH_ERROR;
	}

      current_interface.sym = gfc_new_block = sym;
      break;

    case INTERFACE_USER_OP:
      current_interface.uop = gfc_get_uop (name);
      break;

    case INTERFACE_INTRINSIC_OP:
      current_interface.op = op;
      break;

    case INTERFACE_NAMELESS:
    case INTERFACE_ABSTRACT:
      break;
    }

  return MATCH_YES;
}



/* Match a F2003 abstract interface.  */

match
gfc_match_abstract_interface (void)
{
  match m;

  if (!gfc_notify_std (GFC_STD_F2003, "ABSTRACT INTERFACE at %C"))
    return MATCH_ERROR;

  m = gfc_match_eos ();

  if (m != MATCH_YES)
    {
      gfc_error ("Syntax error in ABSTRACT INTERFACE statement at %C");
      return MATCH_ERROR;
    }

  current_interface.type = INTERFACE_ABSTRACT;

  return m;
}


/* Match the different sort of generic-specs that can be present after
   the END INTERFACE itself.  */

match
gfc_match_end_interface (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  interface_type type;
  gfc_intrinsic_op op;
  match m;

  m = gfc_match_space ();

  if (gfc_match_generic_spec (&type, name, &op) == MATCH_ERROR)
    return MATCH_ERROR;

  /* If we're not looking at the end of the statement now, or if this
     is not a nameless interface but we did not see a space, punt.  */
  if (gfc_match_eos () != MATCH_YES
      || (type != INTERFACE_NAMELESS && m != MATCH_YES))
    {
      gfc_error ("Syntax error: Trailing garbage in END INTERFACE "
		 "statement at %C");
      return MATCH_ERROR;
    }

  m = MATCH_YES;

  switch (current_interface.type)
    {
    case INTERFACE_NAMELESS:
    case INTERFACE_ABSTRACT:
      if (type != INTERFACE_NAMELESS)
	{
	  gfc_error ("Expected a nameless interface at %C");
	  m = MATCH_ERROR;
	}

      break;

    case INTERFACE_INTRINSIC_OP:
      if (type != current_interface.type || op != current_interface.op)
	{

	  if (current_interface.op == INTRINSIC_ASSIGN)
	    {
	      m = MATCH_ERROR;
	      gfc_error ("Expected %<END INTERFACE ASSIGNMENT (=)%> at %C");
	    }
	  else
	    {
	      const char *s1, *s2;
	      s1 = gfc_op2string (current_interface.op);
	      s2 = gfc_op2string (op);

	      /* The following if-statements are used to enforce C1202
		 from F2003.  */
	      if ((strcmp(s1, "==") == 0 && strcmp (s2, ".eq.") == 0)
		  || (strcmp(s1, ".eq.") == 0 && strcmp (s2, "==") == 0))
		break;
	      if ((strcmp(s1, "/=") == 0 && strcmp (s2, ".ne.") == 0)
		  || (strcmp(s1, ".ne.") == 0 && strcmp (s2, "/=") == 0))
		break;
	      if ((strcmp(s1, "<=") == 0 && strcmp (s2, ".le.") == 0)
		  || (strcmp(s1, ".le.") == 0 && strcmp (s2, "<=") == 0))
		break;
	      if ((strcmp(s1, "<") == 0 && strcmp (s2, ".lt.") == 0)
		  || (strcmp(s1, ".lt.") == 0 && strcmp (s2, "<") == 0))
		break;
	      if ((strcmp(s1, ">=") == 0 && strcmp (s2, ".ge.") == 0)
		  || (strcmp(s1, ".ge.") == 0 && strcmp (s2, ">=") == 0))
		break;
	      if ((strcmp(s1, ">") == 0 && strcmp (s2, ".gt.") == 0)
		  || (strcmp(s1, ".gt.") == 0 && strcmp (s2, ">") == 0))
		break;

	      m = MATCH_ERROR;
	      if (strcmp(s2, "none") == 0)
		gfc_error ("Expecting %<END INTERFACE OPERATOR (%s)%> "
			   "at %C", s1);
	      else
		gfc_error ("Expecting %<END INTERFACE OPERATOR (%s)%> at %C, "
			   "but got %qs", s1, s2);
	    }

	}

      break;

    case INTERFACE_USER_OP:
      /* Comparing the symbol node names is OK because only use-associated
	 symbols can be renamed.  */
      if (type != current_interface.type
	  || strcmp (current_interface.uop->name, name) != 0)
	{
	  gfc_error ("Expecting %<END INTERFACE OPERATOR (.%s.)%> at %C",
		     current_interface.uop->name);
	  m = MATCH_ERROR;
	}

      break;

    case INTERFACE_DTIO:
    case INTERFACE_GENERIC:
      if (type != current_interface.type
	  || strcmp (current_interface.sym->name, name) != 0)
	{
	  gfc_error ("Expecting %<END INTERFACE %s%> at %C",
		     current_interface.sym->name);
	  m = MATCH_ERROR;
	}

      break;
    }

  return m;
}


/* Return whether the component was defined anonymously.  */

static bool
is_anonymous_component (gfc_component *cmp)
{
  /* Only UNION and MAP components are anonymous.  In the case of a MAP,
     the derived type symbol is FL_STRUCT and the component name looks like mM*.
     This is the only case in which the second character of a component name is
     uppercase.  */
  return cmp->ts.type == BT_UNION
    || (cmp->ts.type == BT_DERIVED
        && cmp->ts.u.derived->attr.flavor == FL_STRUCT
        && cmp->name[0] && cmp->name[1] && ISUPPER (cmp->name[1]));
}


/* Return whether the derived type was defined anonymously.  */

static bool
is_anonymous_dt (gfc_symbol *derived)
{
  /* UNION and MAP types are always anonymous. Otherwise, only nested STRUCTURE
     types can be anonymous.  For anonymous MAP/STRUCTURE, we have FL_STRUCT
     and the type name looks like XX*.  This is the only case in which the
     second character of a type name is uppercase.  */
  return derived->attr.flavor == FL_UNION
    || (derived->attr.flavor == FL_STRUCT
        && derived->name[0] && derived->name[1] && ISUPPER (derived->name[1]));
}


/* Compare components according to 4.4.2 of the Fortran standard.  */

static bool
compare_components (gfc_component *cmp1, gfc_component *cmp2,
    gfc_symbol *derived1, gfc_symbol *derived2)
{
  /* Compare names, but not for anonymous components such as UNION or MAP.  */
  if (!is_anonymous_component (cmp1) && !is_anonymous_component (cmp2)
      && strcmp (cmp1->name, cmp2->name) != 0)
    return false;

  if (cmp1->attr.access != cmp2->attr.access)
    return false;

  if (cmp1->attr.pointer != cmp2->attr.pointer)
    return false;

  if (cmp1->attr.dimension != cmp2->attr.dimension)
    return false;

  if (cmp1->attr.codimension != cmp2->attr.codimension)
    return false;

  if (cmp1->attr.allocatable != cmp2->attr.allocatable)
    return false;

  if (cmp1->attr.dimension && gfc_compare_array_spec (cmp1->as, cmp2->as) == 0)
    return false;

  if (cmp1->attr.codimension
      && gfc_compare_array_spec (cmp1->as, cmp2->as) == 0)
    return false;

  if (cmp1->ts.type == BT_CHARACTER && cmp2->ts.type == BT_CHARACTER)
    {
      gfc_charlen *l1 = cmp1->ts.u.cl;
      gfc_charlen *l2 = cmp2->ts.u.cl;
      if (l1 && l2 && l1->length && l2->length
          && l1->length->expr_type == EXPR_CONSTANT
          && l2->length->expr_type == EXPR_CONSTANT
          && gfc_dep_compare_expr (l1->length, l2->length) != 0)
        return false;
    }

  /* Make sure that link lists do not put this function into an
     endless recursive loop!  */
  if (!(cmp1->ts.type == BT_DERIVED && derived1 == cmp1->ts.u.derived)
      && !(cmp2->ts.type == BT_DERIVED && derived2 == cmp2->ts.u.derived)
      && !gfc_compare_types (&cmp1->ts, &cmp2->ts))
    return false;

  else if ( (cmp1->ts.type == BT_DERIVED && derived1 == cmp1->ts.u.derived)
        && !(cmp2->ts.type == BT_DERIVED && derived2 == cmp2->ts.u.derived))
    return false;

  else if (!(cmp1->ts.type == BT_DERIVED && derived1 == cmp1->ts.u.derived)
        &&  (cmp2->ts.type == BT_DERIVED && derived2 == cmp2->ts.u.derived))
    return false;

  return true;
}


/* Compare two union types by comparing the components of their maps.
   Because unions and maps are anonymous their types get special internal
   names; therefore the usual derived type comparison will fail on them.

   Returns nonzero if equal, as with gfc_compare_derived_types. Also as with
   gfc_compare_derived_types, 'equal' is closer to meaning 'duplicate
   definitions' than 'equivalent structure'. */

static bool
compare_union_types (gfc_symbol *un1, gfc_symbol *un2)
{
  gfc_component *map1, *map2, *cmp1, *cmp2;
  gfc_symbol *map1_t, *map2_t;

  if (un1->attr.flavor != FL_UNION || un2->attr.flavor != FL_UNION)
    return false;

  if (un1->attr.zero_comp != un2->attr.zero_comp)
    return false;

  if (un1->attr.zero_comp)
    return true;

  map1 = un1->components;
  map2 = un2->components;

  /* In terms of 'equality' here we are worried about types which are
     declared the same in two places, not types that represent equivalent
     structures. (This is common because of FORTRAN's weird scoping rules.)
     Though two unions with their maps in different orders could be equivalent,
     we will say they are not equal for the purposes of this test; therefore
     we compare the maps sequentially. */
  for (;;)
    {
      map1_t = map1->ts.u.derived;
      map2_t = map2->ts.u.derived;

      cmp1 = map1_t->components;
      cmp2 = map2_t->components;

      /* Protect against null components.  */
      if (map1_t->attr.zero_comp != map2_t->attr.zero_comp)
	return false;

      if (map1_t->attr.zero_comp)
	return true;

      for (;;)
	{
	  /* No two fields will ever point to the same map type unless they are
	     the same component, because one map field is created with its type
	     declaration. Therefore don't worry about recursion here. */
	  /* TODO: worry about recursion into parent types of the unions? */
	  if (!compare_components (cmp1, cmp2, map1_t, map2_t))
	    return false;

	  cmp1 = cmp1->next;
	  cmp2 = cmp2->next;

	  if (cmp1 == NULL && cmp2 == NULL)
	    break;
	  if (cmp1 == NULL || cmp2 == NULL)
	    return false;
	}

      map1 = map1->next;
      map2 = map2->next;

      if (map1 == NULL && map2 == NULL)
	break;
      if (map1 == NULL || map2 == NULL)
	return false;
    }

  return true;
}



/* Compare two derived types using the criteria in 4.4.2 of the standard,
   recursing through gfc_compare_types for the components.  */

bool
gfc_compare_derived_types (gfc_symbol *derived1, gfc_symbol *derived2)
{
  gfc_component *cmp1, *cmp2;

  if (derived1 == derived2)
    return true;

  if (!derived1 || !derived2)
    gfc_internal_error ("gfc_compare_derived_types: invalid derived type");

  if (derived1->attr.unlimited_polymorphic
      && derived2->attr.unlimited_polymorphic)
    return true;

  if (derived1->attr.unlimited_polymorphic
      != derived2->attr.unlimited_polymorphic)
    return false;

  /* Compare UNION types specially.  */
  if (derived1->attr.flavor == FL_UNION || derived2->attr.flavor == FL_UNION)
    return compare_union_types (derived1, derived2);

  /* Special case for comparing derived types across namespaces.  If the
     true names and module names are the same and the module name is
     nonnull, then they are equal.  */
  if (strcmp (derived1->name, derived2->name) == 0
      && derived1->module != NULL && derived2->module != NULL
      && strcmp (derived1->module, derived2->module) == 0)
    return true;

  /* Compare type via the rules of the standard.  Both types must have the
     SEQUENCE or BIND(C) attribute to be equal.  We also compare types
     recursively if they are class descriptors types or virtual tables types.
     STRUCTUREs are special because they can be anonymous; therefore two
     structures with different names may be equal.  */

  /* Compare names, but not for anonymous types such as UNION or MAP.  */
  if (!is_anonymous_dt (derived1) && !is_anonymous_dt (derived2)
      && strcmp (derived1->name, derived2->name) != 0)
    return false;

  if (derived1->component_access == ACCESS_PRIVATE
      || derived2->component_access == ACCESS_PRIVATE)
    return false;

  if (!(derived1->attr.sequence && derived2->attr.sequence)
      && !(derived1->attr.is_bind_c && derived2->attr.is_bind_c)
      && !(derived1->attr.is_class && derived2->attr.is_class)
      && !(derived1->attr.vtype && derived2->attr.vtype)
      && !(derived1->attr.pdt_type && derived2->attr.pdt_type))
    return false;

  /* Protect against null components.  */
  if (derived1->attr.zero_comp != derived2->attr.zero_comp)
    return false;

  if (derived1->attr.zero_comp)
    return true;

  cmp1 = derived1->components;
  cmp2 = derived2->components;

  /* Since subtypes of SEQUENCE types must be SEQUENCE types as well, a
     simple test can speed things up.  Otherwise, lots of things have to
     match.  */
  for (;;)
    {
      if (!compare_components (cmp1, cmp2, derived1, derived2))
        return false;

      cmp1 = cmp1->next;
      cmp2 = cmp2->next;

      if (cmp1 == NULL && cmp2 == NULL)
	break;
      if (cmp1 == NULL || cmp2 == NULL)
	return false;
    }

  return true;
}


/* Compare two typespecs, recursively if necessary.  */

bool
gfc_compare_types (gfc_typespec *ts1, gfc_typespec *ts2)
{
  /* See if one of the typespecs is a BT_VOID, which is what is being used
     to allow the funcs like c_f_pointer to accept any pointer type.
     TODO: Possibly should narrow this to just the one typespec coming in
     that is for the formal arg, but oh well.  */
  if (ts1->type == BT_VOID || ts2->type == BT_VOID)
    return true;

  /* Special case for our C interop types.  FIXME: There should be a
     better way of doing this.  When ISO C binding is cleared up,
     this can probably be removed.  See PR 57048.  */

  if ((ts1->type == BT_INTEGER
       && ts2->type == BT_DERIVED
       && ts1->f90_type == BT_VOID
       && ts2->u.derived->from_intmod == INTMOD_ISO_C_BINDING
       && ts1->u.derived
       && strcmp (ts1->u.derived->name, ts2->u.derived->name) == 0)
      || (ts2->type == BT_INTEGER
	  && ts1->type == BT_DERIVED
	  && ts2->f90_type == BT_VOID
	  && ts1->u.derived->from_intmod == INTMOD_ISO_C_BINDING
	  && ts2->u.derived
	  && strcmp (ts1->u.derived->name, ts2->u.derived->name) == 0))
    return true;

  /* The _data component is not always present, therefore check for its
     presence before assuming, that its derived->attr is available.
     When the _data component is not present, then nevertheless the
     unlimited_polymorphic flag may be set in the derived type's attr.  */
  if (ts1->type == BT_CLASS && ts1->u.derived->components
      && ((ts1->u.derived->attr.is_class
	   && ts1->u.derived->components->ts.u.derived->attr
						  .unlimited_polymorphic)
	  || ts1->u.derived->attr.unlimited_polymorphic))
    return true;

  /* F2003: C717  */
  if (ts2->type == BT_CLASS && ts1->type == BT_DERIVED
      && ts2->u.derived->components
      && ((ts2->u.derived->attr.is_class
	   && ts2->u.derived->components->ts.u.derived->attr
						  .unlimited_polymorphic)
	  || ts2->u.derived->attr.unlimited_polymorphic)
      && (ts1->u.derived->attr.sequence || ts1->u.derived->attr.is_bind_c))
    return true;

  if (ts1->type != ts2->type
      && ((ts1->type != BT_DERIVED && ts1->type != BT_CLASS)
	  || (ts2->type != BT_DERIVED && ts2->type != BT_CLASS)))
    return false;

  if (ts1->type == BT_UNION)
    return compare_union_types (ts1->u.derived, ts2->u.derived);

  if (ts1->type != BT_DERIVED && ts1->type != BT_CLASS)
    return (ts1->kind == ts2->kind);

  /* Compare derived types.  */
  return gfc_type_compatible (ts1, ts2);
}


static bool
compare_type (gfc_symbol *s1, gfc_symbol *s2)
{
  if (s2->attr.ext_attr & (1 << EXT_ATTR_NO_ARG_CHECK))
    return true;

  return gfc_compare_types (&s1->ts, &s2->ts) || s2->ts.type == BT_ASSUMED;
}


static bool
compare_type_characteristics (gfc_symbol *s1, gfc_symbol *s2)
{
  /* TYPE and CLASS of the same declared type are type compatible,
     but have different characteristics.  */
  if ((s1->ts.type == BT_CLASS && s2->ts.type == BT_DERIVED)
      || (s1->ts.type == BT_DERIVED && s2->ts.type == BT_CLASS))
    return false;

  return compare_type (s1, s2);
}


static bool
compare_rank (gfc_symbol *s1, gfc_symbol *s2)
{
  gfc_array_spec *as1, *as2;
  int r1, r2;

  if (s2->attr.ext_attr & (1 << EXT_ATTR_NO_ARG_CHECK))
    return true;

  as1 = (s1->ts.type == BT_CLASS
	 && !s1->ts.u.derived->attr.unlimited_polymorphic)
	? CLASS_DATA (s1)->as : s1->as;
  as2 = (s2->ts.type == BT_CLASS
	 && !s2->ts.u.derived->attr.unlimited_polymorphic)
	? CLASS_DATA (s2)->as : s2->as;

  r1 = as1 ? as1->rank : 0;
  r2 = as2 ? as2->rank : 0;

  if (r1 != r2 && (!as2 || as2->type != AS_ASSUMED_RANK))
    return false;  /* Ranks differ.  */

  return true;
}


/* Given two symbols that are formal arguments, compare their ranks
   and types.  Returns true if they have the same rank and type,
   false otherwise.  */

static bool
compare_type_rank (gfc_symbol *s1, gfc_symbol *s2)
{
  return compare_type (s1, s2) && compare_rank (s1, s2);
}


/* Given two symbols that are formal arguments, compare their types
   and rank and their formal interfaces if they are both dummy
   procedures.  Returns true if the same, false if different.  */

static bool
compare_type_rank_if (gfc_symbol *s1, gfc_symbol *s2)
{
  if (s1 == NULL || s2 == NULL)
    return (s1 == s2);

  if (s1 == s2)
    return true;

  if (s1->attr.flavor != FL_PROCEDURE && s2->attr.flavor != FL_PROCEDURE)
    return compare_type_rank (s1, s2);

  if (s1->attr.flavor != FL_PROCEDURE || s2->attr.flavor != FL_PROCEDURE)
    return false;

  /* At this point, both symbols are procedures.  It can happen that
     external procedures are compared, where one is identified by usage
     to be a function or subroutine but the other is not.  Check TKR
     nonetheless for these cases.  */
  if (s1->attr.function == 0 && s1->attr.subroutine == 0)
    return s1->attr.external ? compare_type_rank (s1, s2) : false;

  if (s2->attr.function == 0 && s2->attr.subroutine == 0)
    return s2->attr.external ? compare_type_rank (s1, s2) : false;

  /* Now the type of procedure has been identified.  */
  if (s1->attr.function != s2->attr.function
      || s1->attr.subroutine != s2->attr.subroutine)
    return false;

  if (s1->attr.function && !compare_type_rank (s1, s2))
    return false;

  /* Originally, gfortran recursed here to check the interfaces of passed
     procedures.  This is explicitly not required by the standard.  */
  return true;
}


/* Given a formal argument list and a keyword name, search the list
   for that keyword.  Returns the correct symbol node if found, NULL
   if not found.  */

static gfc_symbol *
find_keyword_arg (const char *name, gfc_formal_arglist *f)
{
  for (; f; f = f->next)
    if (strcmp (f->sym->name, name) == 0)
      return f->sym;

  return NULL;
}


/******** Interface checking subroutines **********/


/* Given an operator interface and the operator, make sure that all
   interfaces for that operator are legal.  */

bool
gfc_check_operator_interface (gfc_symbol *sym, gfc_intrinsic_op op,
			      locus opwhere)
{
  gfc_formal_arglist *formal;
  sym_intent i1, i2;
  bt t1, t2;
  int args, r1, r2, k1, k2;

  gcc_assert (sym);

  args = 0;
  t1 = t2 = BT_UNKNOWN;
  i1 = i2 = INTENT_UNKNOWN;
  r1 = r2 = -1;
  k1 = k2 = -1;

  for (formal = gfc_sym_get_dummy_args (sym); formal; formal = formal->next)
    {
      gfc_symbol *fsym = formal->sym;
      if (fsym == NULL)
	{
	  gfc_error ("Alternate return cannot appear in operator "
		     "interface at %L", &sym->declared_at);
	  return false;
	}
      if (args == 0)
	{
	  t1 = fsym->ts.type;
	  i1 = fsym->attr.intent;
	  r1 = (fsym->as != NULL) ? fsym->as->rank : 0;
	  k1 = fsym->ts.kind;
	}
      if (args == 1)
	{
	  t2 = fsym->ts.type;
	  i2 = fsym->attr.intent;
	  r2 = (fsym->as != NULL) ? fsym->as->rank : 0;
	  k2 = fsym->ts.kind;
	}
      args++;
    }

  /* Only +, - and .not. can be unary operators.
     .not. cannot be a binary operator.  */
  if (args == 0 || args > 2 || (args == 1 && op != INTRINSIC_PLUS
				&& op != INTRINSIC_MINUS
				&& op != INTRINSIC_NOT)
      || (args == 2 && op == INTRINSIC_NOT))
    {
      if (op == INTRINSIC_ASSIGN)
	gfc_error ("Assignment operator interface at %L must have "
		   "two arguments", &sym->declared_at);
      else
	gfc_error ("Operator interface at %L has the wrong number of arguments",
		   &sym->declared_at);
      return false;
    }

  /* Check that intrinsics are mapped to functions, except
     INTRINSIC_ASSIGN which should map to a subroutine.  */
  if (op == INTRINSIC_ASSIGN)
    {
      gfc_formal_arglist *dummy_args;

      if (!sym->attr.subroutine)
	{
	  gfc_error ("Assignment operator interface at %L must be "
		     "a SUBROUTINE", &sym->declared_at);
	  return false;
	}

      /* Allowed are (per F2003, 12.3.2.1.2 Defined assignments):
	 - First argument an array with different rank than second,
	 - First argument is a scalar and second an array,
	 - Types and kinds do not conform, or
	 - First argument is of derived type.  */
      dummy_args = gfc_sym_get_dummy_args (sym);
      if (dummy_args->sym->ts.type != BT_DERIVED
	  && dummy_args->sym->ts.type != BT_CLASS
	  && (r2 == 0 || r1 == r2)
	  && (dummy_args->sym->ts.type == dummy_args->next->sym->ts.type
	      || (gfc_numeric_ts (&dummy_args->sym->ts)
		  && gfc_numeric_ts (&dummy_args->next->sym->ts))))
	{
	  gfc_error ("Assignment operator interface at %L must not redefine "
		     "an INTRINSIC type assignment", &sym->declared_at);
	  return false;
	}
    }
  else
    {
      if (!sym->attr.function)
	{
	  gfc_error ("Intrinsic operator interface at %L must be a FUNCTION",
		     &sym->declared_at);
	  return false;
	}
    }

  /* Check intents on operator interfaces.  */
  if (op == INTRINSIC_ASSIGN)
    {
      if (i1 != INTENT_OUT && i1 != INTENT_INOUT)
	{
	  gfc_error ("First argument of defined assignment at %L must be "
		     "INTENT(OUT) or INTENT(INOUT)", &sym->declared_at);
	  return false;
	}

      if (i2 != INTENT_IN)
	{
	  gfc_error ("Second argument of defined assignment at %L must be "
		     "INTENT(IN)", &sym->declared_at);
	  return false;
	}
    }
  else
    {
      if (i1 != INTENT_IN)
	{
	  gfc_error ("First argument of operator interface at %L must be "
		     "INTENT(IN)", &sym->declared_at);
	  return false;
	}

      if (args == 2 && i2 != INTENT_IN)
	{
	  gfc_error ("Second argument of operator interface at %L must be "
		     "INTENT(IN)", &sym->declared_at);
	  return false;
	}
    }

  /* From now on, all we have to do is check that the operator definition
     doesn't conflict with an intrinsic operator. The rules for this
     game are defined in 7.1.2 and 7.1.3 of both F95 and F2003 standards,
     as well as 12.3.2.1.1 of Fortran 2003:

     "If the operator is an intrinsic-operator (R310), the number of
     function arguments shall be consistent with the intrinsic uses of
     that operator, and the types, kind type parameters, or ranks of the
     dummy arguments shall differ from those required for the intrinsic
     operation (7.1.2)."  */

#define IS_NUMERIC_TYPE(t) \
  ((t) == BT_INTEGER || (t) == BT_REAL || (t) == BT_COMPLEX)

  /* Unary ops are easy, do them first.  */
  if (op == INTRINSIC_NOT)
    {
      if (t1 == BT_LOGICAL)
	goto bad_repl;
      else
	return true;
    }

  if (args == 1 && (op == INTRINSIC_PLUS || op == INTRINSIC_MINUS))
    {
      if (IS_NUMERIC_TYPE (t1))
	goto bad_repl;
      else
	return true;
    }

  /* Character intrinsic operators have same character kind, thus
     operator definitions with operands of different character kinds
     are always safe.  */
  if (t1 == BT_CHARACTER && t2 == BT_CHARACTER && k1 != k2)
    return true;

  /* Intrinsic operators always perform on arguments of same rank,
     so different ranks is also always safe.  (rank == 0) is an exception
     to that, because all intrinsic operators are elemental.  */
  if (r1 != r2 && r1 != 0 && r2 != 0)
    return true;

  switch (op)
  {
    case INTRINSIC_EQ:
    case INTRINSIC_EQ_OS:
    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
      if (t1 == BT_CHARACTER && t2 == BT_CHARACTER)
	goto bad_repl;
      /* Fall through.  */

    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
    case INTRINSIC_POWER:
      if (IS_NUMERIC_TYPE (t1) && IS_NUMERIC_TYPE (t2))
	goto bad_repl;
      break;

    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:
      if (t1 == BT_CHARACTER && t2 == BT_CHARACTER)
	goto bad_repl;
      if ((t1 == BT_INTEGER || t1 == BT_REAL)
	  && (t2 == BT_INTEGER || t2 == BT_REAL))
	goto bad_repl;
      break;

    case INTRINSIC_CONCAT:
      if (t1 == BT_CHARACTER && t2 == BT_CHARACTER)
	goto bad_repl;
      break;

    case INTRINSIC_AND:
    case INTRINSIC_OR:
    case INTRINSIC_EQV:
    case INTRINSIC_NEQV:
      if (t1 == BT_LOGICAL && t2 == BT_LOGICAL)
	goto bad_repl;
      break;

    default:
      break;
  }

  return true;

#undef IS_NUMERIC_TYPE

bad_repl:
  gfc_error ("Operator interface at %L conflicts with intrinsic interface",
	     &opwhere);
  return false;
}


/* Given a pair of formal argument lists, we see if the two lists can
   be distinguished by counting the number of nonoptional arguments of
   a given type/rank in f1 and seeing if there are less then that
   number of those arguments in f2 (including optional arguments).
   Since this test is asymmetric, it has to be called twice to make it
   symmetric. Returns nonzero if the argument lists are incompatible
   by this test. This subroutine implements rule 1 of section F03:16.2.3.
   'p1' and 'p2' are the PASS arguments of both procedures (if applicable).  */

static bool
count_types_test (gfc_formal_arglist *f1, gfc_formal_arglist *f2,
		  const char *p1, const char *p2)
{
  int ac1, ac2, i, j, k, n1;
  gfc_formal_arglist *f;

  typedef struct
  {
    int flag;
    gfc_symbol *sym;
  }
  arginfo;

  arginfo *arg;

  n1 = 0;

  for (f = f1; f; f = f->next)
    n1++;

  /* Build an array of integers that gives the same integer to
     arguments of the same type/rank.  */
  arg = XCNEWVEC (arginfo, n1);

  f = f1;
  for (i = 0; i < n1; i++, f = f->next)
    {
      arg[i].flag = -1;
      arg[i].sym = f->sym;
    }

  k = 0;

  for (i = 0; i < n1; i++)
    {
      if (arg[i].flag != -1)
	continue;

      if (arg[i].sym && (arg[i].sym->attr.optional
			 || (p1 && strcmp (arg[i].sym->name, p1) == 0)))
	continue;		/* Skip OPTIONAL and PASS arguments.  */

      arg[i].flag = k;

      /* Find other non-optional, non-pass arguments of the same type/rank.  */
      for (j = i + 1; j < n1; j++)
	if ((arg[j].sym == NULL
	     || !(arg[j].sym->attr.optional
		  || (p1 && strcmp (arg[j].sym->name, p1) == 0)))
	    && (compare_type_rank_if (arg[i].sym, arg[j].sym)
	        || compare_type_rank_if (arg[j].sym, arg[i].sym)))
	  arg[j].flag = k;

      k++;
    }

  /* Now loop over each distinct type found in f1.  */
  k = 0;
  bool rc = false;

  for (i = 0; i < n1; i++)
    {
      if (arg[i].flag != k)
	continue;

      ac1 = 1;
      for (j = i + 1; j < n1; j++)
	if (arg[j].flag == k)
	  ac1++;

      /* Count the number of non-pass arguments in f2 with that type,
	 including those that are optional.  */
      ac2 = 0;

      for (f = f2; f; f = f->next)
	if ((!p2 || strcmp (f->sym->name, p2) != 0)
	    && (compare_type_rank_if (arg[i].sym, f->sym)
		|| compare_type_rank_if (f->sym, arg[i].sym)))
	  ac2++;

      if (ac1 > ac2)
	{
	  rc = true;
	  break;
	}

      k++;
    }

  free (arg);

  return rc;
}


/* Returns true if two dummy arguments are distinguishable due to their POINTER
   and ALLOCATABLE attributes according to F2018 section 15.4.3.4.5 (3).
   The function is asymmetric wrt to the arguments s1 and s2 and should always
   be called twice (with flipped arguments in the second call).  */

static bool
compare_ptr_alloc(gfc_symbol *s1, gfc_symbol *s2)
{
  /* Is s1 allocatable?  */
  const bool a1 = s1->ts.type == BT_CLASS ?
		  CLASS_DATA(s1)->attr.allocatable : s1->attr.allocatable;
  /* Is s2 a pointer?  */
  const bool p2 = s2->ts.type == BT_CLASS ?
		  CLASS_DATA(s2)->attr.class_pointer : s2->attr.pointer;
  return a1 && p2 && (s2->attr.intent != INTENT_IN);
}


/* Perform the correspondence test in rule (3) of F08:C1215.
   Returns zero if no argument is found that satisfies this rule,
   nonzero otherwise. 'p1' and 'p2' are the PASS arguments of both procedures
   (if applicable).

   This test is also not symmetric in f1 and f2 and must be called
   twice.  This test finds problems caused by sorting the actual
   argument list with keywords.  For example:

   INTERFACE FOO
     SUBROUTINE F1(A, B)
       INTEGER :: A ; REAL :: B
     END SUBROUTINE F1

     SUBROUTINE F2(B, A)
       INTEGER :: A ; REAL :: B
     END SUBROUTINE F1
   END INTERFACE FOO

   At this point, 'CALL FOO(A=1, B=1.0)' is ambiguous.  */

static bool
generic_correspondence (gfc_formal_arglist *f1, gfc_formal_arglist *f2,
			const char *p1, const char *p2)
{
  gfc_formal_arglist *f2_save, *g;
  gfc_symbol *sym;

  f2_save = f2;

  while (f1)
    {
      if (!f1->sym || f1->sym->attr.optional)
	goto next;

      if (p1 && strcmp (f1->sym->name, p1) == 0)
	f1 = f1->next;
      if (f2 && p2 && strcmp (f2->sym->name, p2) == 0)
	f2 = f2->next;

      if (f2 != NULL && (compare_type_rank (f1->sym, f2->sym)
			 || compare_type_rank (f2->sym, f1->sym))
	  && !((gfc_option.allow_std & GFC_STD_F2008)
	       && (compare_ptr_alloc(f1->sym, f2->sym)
		   || compare_ptr_alloc(f2->sym, f1->sym))))
	goto next;

      /* Now search for a disambiguating keyword argument starting at
	 the current non-match.  */
      for (g = f1; g; g = g->next)
	{
	  if (g->sym->attr.optional || (p1 && strcmp (g->sym->name, p1) == 0))
	    continue;

	  sym = find_keyword_arg (g->sym->name, f2_save);
	  if (sym == NULL || !compare_type_rank (g->sym, sym)
	      || ((gfc_option.allow_std & GFC_STD_F2008)
		  && (compare_ptr_alloc(sym, g->sym)
		      || compare_ptr_alloc(g->sym, sym))))
	    return true;
	}

    next:
      if (f1 != NULL)
	f1 = f1->next;
      if (f2 != NULL)
	f2 = f2->next;
    }

  return false;
}


static int
symbol_rank (gfc_symbol *sym)
{
  gfc_array_spec *as = NULL;

  if (sym->ts.type == BT_CLASS && CLASS_DATA (sym))
    as = CLASS_DATA (sym)->as;
  else
    as = sym->as;

  return as ? as->rank : 0;
}


/* Check if the characteristics of two dummy arguments match,
   cf. F08:12.3.2.  */

bool
gfc_check_dummy_characteristics (gfc_symbol *s1, gfc_symbol *s2,
				 bool type_must_agree, char *errmsg,
				 int err_len)
{
  if (s1 == NULL || s2 == NULL)
    return s1 == s2 ? true : false;

  if (s1->attr.proc == PROC_ST_FUNCTION || s2->attr.proc == PROC_ST_FUNCTION)
    {
      strncpy (errmsg, "Statement function", err_len);
      return false;
    }

  /* Check type and rank.  */
  if (type_must_agree)
    {
      if (!compare_type_characteristics (s1, s2)
	  || !compare_type_characteristics (s2, s1))
	{
	  snprintf (errmsg, err_len, "Type mismatch in argument '%s' (%s/%s)",
		    s1->name, gfc_dummy_typename (&s1->ts),
		    gfc_dummy_typename (&s2->ts));
	  return false;
	}
      if (!compare_rank (s1, s2))
	{
	  snprintf (errmsg, err_len, "Rank mismatch in argument '%s' (%i/%i)",
		    s1->name, symbol_rank (s1), symbol_rank (s2));
	  return false;
	}
    }

  /* Check INTENT.  */
  if (s1->attr.intent != s2->attr.intent && !s1->attr.artificial
      && !s2->attr.artificial)
    {
      snprintf (errmsg, err_len, "INTENT mismatch in argument '%s'",
		s1->name);
      return false;
    }

  /* Check OPTIONAL attribute.  */
  if (s1->attr.optional != s2->attr.optional)
    {
      snprintf (errmsg, err_len, "OPTIONAL mismatch in argument '%s'",
		s1->name);
      return false;
    }

  /* Check ALLOCATABLE attribute.  */
  if (s1->attr.allocatable != s2->attr.allocatable)
    {
      snprintf (errmsg, err_len, "ALLOCATABLE mismatch in argument '%s'",
		s1->name);
      return false;
    }

  /* Check POINTER attribute.  */
  if (s1->attr.pointer != s2->attr.pointer)
    {
      snprintf (errmsg, err_len, "POINTER mismatch in argument '%s'",
		s1->name);
      return false;
    }

  /* Check TARGET attribute.  */
  if (s1->attr.target != s2->attr.target)
    {
      snprintf (errmsg, err_len, "TARGET mismatch in argument '%s'",
		s1->name);
      return false;
    }

  /* Check ASYNCHRONOUS attribute.  */
  if (s1->attr.asynchronous != s2->attr.asynchronous)
    {
      snprintf (errmsg, err_len, "ASYNCHRONOUS mismatch in argument '%s'",
		s1->name);
      return false;
    }

  /* Check CONTIGUOUS attribute.  */
  if (s1->attr.contiguous != s2->attr.contiguous)
    {
      snprintf (errmsg, err_len, "CONTIGUOUS mismatch in argument '%s'",
		s1->name);
      return false;
    }

  /* Check VALUE attribute.  */
  if (s1->attr.value != s2->attr.value)
    {
      snprintf (errmsg, err_len, "VALUE mismatch in argument '%s'",
		s1->name);
      return false;
    }

  /* Check VOLATILE attribute.  */
  if (s1->attr.volatile_ != s2->attr.volatile_)
    {
      snprintf (errmsg, err_len, "VOLATILE mismatch in argument '%s'",
		s1->name);
      return false;
    }

  /* Check interface of dummy procedures.  */
  if (s1->attr.flavor == FL_PROCEDURE)
    {
      char err[200];
      if (!gfc_compare_interfaces (s1, s2, s2->name, 0, 1, err, sizeof(err),
				   NULL, NULL))
	{
	  snprintf (errmsg, err_len, "Interface mismatch in dummy procedure "
		    "'%s': %s", s1->name, err);
	  return false;
	}
    }

  /* Check string length.  */
  if (s1->ts.type == BT_CHARACTER
      && s1->ts.u.cl && s1->ts.u.cl->length
      && s2->ts.u.cl && s2->ts.u.cl->length)
    {
      int compval = gfc_dep_compare_expr (s1->ts.u.cl->length,
					  s2->ts.u.cl->length);
      switch (compval)
      {
	case -1:
	case  1:
	case -3:
	  snprintf (errmsg, err_len, "Character length mismatch "
		    "in argument '%s'", s1->name);
	  return false;

	case -2:
	  /* FIXME: Implement a warning for this case.
	  gfc_warning (0, "Possible character length mismatch in argument %qs",
		       s1->name);*/
	  break;

	case 0:
	  break;

	default:
	  gfc_internal_error ("check_dummy_characteristics: Unexpected result "
			      "%i of gfc_dep_compare_expr", compval);
	  break;
      }
    }

  /* Check array shape.  */
  if (s1->as && s2->as)
    {
      int i, compval;
      gfc_expr *shape1, *shape2;

      /* Sometimes the ambiguity between deferred shape and assumed shape
	 does not get resolved in module procedures, where the only explicit
	 declaration of the dummy is in the interface.  */
      if (s1->ns->proc_name && s1->ns->proc_name->attr.module_procedure
	  && s1->as->type == AS_ASSUMED_SHAPE
	  && s2->as->type == AS_DEFERRED)
	{
	  s2->as->type = AS_ASSUMED_SHAPE;
	  for (i = 0; i < s2->as->rank; i++)
	    if (s1->as->lower[i] != NULL)
	      s2->as->lower[i] = gfc_copy_expr (s1->as->lower[i]);
	}

      if (s1->as->type != s2->as->type)
	{
	  snprintf (errmsg, err_len, "Shape mismatch in argument '%s'",
		    s1->name);
	  return false;
	}

      if (s1->as->corank != s2->as->corank)
	{
	  snprintf (errmsg, err_len, "Corank mismatch in argument '%s' (%i/%i)",
		    s1->name, s1->as->corank, s2->as->corank);
	  return false;
	}

      if (s1->as->type == AS_EXPLICIT)
	for (i = 0; i < s1->as->rank + MAX (0, s1->as->corank-1); i++)
	  {
	    shape1 = gfc_subtract (gfc_copy_expr (s1->as->upper[i]),
				  gfc_copy_expr (s1->as->lower[i]));
	    shape2 = gfc_subtract (gfc_copy_expr (s2->as->upper[i]),
				  gfc_copy_expr (s2->as->lower[i]));
	    compval = gfc_dep_compare_expr (shape1, shape2);
	    gfc_free_expr (shape1);
	    gfc_free_expr (shape2);
	    switch (compval)
	    {
	      case -1:
	      case  1:
	      case -3:
		if (i < s1->as->rank)
		  snprintf (errmsg, err_len, "Shape mismatch in dimension %i of"
			    " argument '%s'", i + 1, s1->name);
		else
		  snprintf (errmsg, err_len, "Shape mismatch in codimension %i "
			    "of argument '%s'", i - s1->as->rank + 1, s1->name);
		return false;

	      case -2:
		/* FIXME: Implement a warning for this case.
		gfc_warning (0, "Possible shape mismatch in argument %qs",
			    s1->name);*/
		break;

	      case 0:
		break;

	      default:
		gfc_internal_error ("check_dummy_characteristics: Unexpected "
				    "result %i of gfc_dep_compare_expr",
				    compval);
		break;
	    }
	  }
    }

  return true;
}


/* Check if the characteristics of two function results match,
   cf. F08:12.3.3.  */

bool
gfc_check_result_characteristics (gfc_symbol *s1, gfc_symbol *s2,
				  char *errmsg, int err_len)
{
  gfc_symbol *r1, *r2;

  if (s1->ts.interface && s1->ts.interface->result)
    r1 = s1->ts.interface->result;
  else
    r1 = s1->result ? s1->result : s1;

  if (s2->ts.interface && s2->ts.interface->result)
    r2 = s2->ts.interface->result;
  else
    r2 = s2->result ? s2->result : s2;

  if (r1->ts.type == BT_UNKNOWN)
    return true;

  /* Check type and rank.  */
  if (!compare_type_characteristics (r1, r2))
    {
      snprintf (errmsg, err_len, "Type mismatch in function result (%s/%s)",
		gfc_typename (&r1->ts), gfc_typename (&r2->ts));
      return false;
    }
  if (!compare_rank (r1, r2))
    {
      snprintf (errmsg, err_len, "Rank mismatch in function result (%i/%i)",
		symbol_rank (r1), symbol_rank (r2));
      return false;
    }

  /* Check ALLOCATABLE attribute.  */
  if (r1->attr.allocatable != r2->attr.allocatable)
    {
      snprintf (errmsg, err_len, "ALLOCATABLE attribute mismatch in "
		"function result");
      return false;
    }

  /* Check POINTER attribute.  */
  if (r1->attr.pointer != r2->attr.pointer)
    {
      snprintf (errmsg, err_len, "POINTER attribute mismatch in "
		"function result");
      return false;
    }

  /* Check CONTIGUOUS attribute.  */
  if (r1->attr.contiguous != r2->attr.contiguous)
    {
      snprintf (errmsg, err_len, "CONTIGUOUS attribute mismatch in "
		"function result");
      return false;
    }

  /* Check PROCEDURE POINTER attribute.  */
  if (r1 != s1 && r1->attr.proc_pointer != r2->attr.proc_pointer)
    {
      snprintf (errmsg, err_len, "PROCEDURE POINTER mismatch in "
		"function result");
      return false;
    }

  /* Check string length.  */
  if (r1->ts.type == BT_CHARACTER && r1->ts.u.cl && r2->ts.u.cl)
    {
      if (r1->ts.deferred != r2->ts.deferred)
	{
	  snprintf (errmsg, err_len, "Character length mismatch "
		    "in function result");
	  return false;
	}

      if (r1->ts.u.cl->length && r2->ts.u.cl->length)
	{
	  int compval = gfc_dep_compare_expr (r1->ts.u.cl->length,
					      r2->ts.u.cl->length);
	  switch (compval)
	  {
	    case -1:
	    case  1:
	    case -3:
	      snprintf (errmsg, err_len, "Character length mismatch "
			"in function result");
	      return false;

	    case -2:
	      if (r1->ts.u.cl->length->expr_type == EXPR_CONSTANT)
		{
		  snprintf (errmsg, err_len,
			    "Function declared with a non-constant character "
			    "length referenced with a constant length");
		  return false;
		}
	      else if (r2->ts.u.cl->length->expr_type == EXPR_CONSTANT)
		{
		  snprintf (errmsg, err_len,
			    "Function declared with a constant character "
			    "length referenced with a non-constant length");
		  return false;
		}
	      /* Warn if length expression types are different, except for
		  possibly false positives where complex expressions might have
		  been used.  */
	      else if ((r1->ts.u.cl->length->expr_type
			!= r2->ts.u.cl->length->expr_type)
		       && (r1->ts.u.cl->length->expr_type != EXPR_OP
			   || r2->ts.u.cl->length->expr_type != EXPR_OP))
		gfc_warning (0, "Possible character length mismatch in "
			     "function result between %L and %L",
			     &r1->declared_at, &r2->declared_at);
	      break;

	    case 0:
	      break;

	    default:
	      gfc_internal_error ("check_result_characteristics (1): Unexpected "
				  "result %i of gfc_dep_compare_expr", compval);
	      break;
	  }
	}
    }

  /* Check array shape.  */
  if (!r1->attr.allocatable && !r1->attr.pointer && r1->as && r2->as)
    {
      int i, compval;
      gfc_expr *shape1, *shape2;

      if (r1->as->type != r2->as->type)
	{
	  snprintf (errmsg, err_len, "Shape mismatch in function result");
	  return false;
	}

      if (r1->as->type == AS_EXPLICIT)
	for (i = 0; i < r1->as->rank + r1->as->corank; i++)
	  {
	    shape1 = gfc_subtract (gfc_copy_expr (r1->as->upper[i]),
				   gfc_copy_expr (r1->as->lower[i]));
	    shape2 = gfc_subtract (gfc_copy_expr (r2->as->upper[i]),
				   gfc_copy_expr (r2->as->lower[i]));
	    compval = gfc_dep_compare_expr (shape1, shape2);
	    gfc_free_expr (shape1);
	    gfc_free_expr (shape2);
	    switch (compval)
	    {
	      case -1:
	      case  1:
	      case -3:
		snprintf (errmsg, err_len, "Shape mismatch in dimension %i of "
			  "function result", i + 1);
		return false;

	      case -2:
		/* FIXME: Implement a warning for this case.
		gfc_warning (0, "Possible shape mismatch in return value");*/
		break;

	      case 0:
		break;

	      default:
		gfc_internal_error ("check_result_characteristics (2): "
				    "Unexpected result %i of "
				    "gfc_dep_compare_expr", compval);
		break;
	    }
	  }
    }

  return true;
}


/* 'Compare' two formal interfaces associated with a pair of symbols.
   We return true if there exists an actual argument list that
   would be ambiguous between the two interfaces, zero otherwise.
   'strict_flag' specifies whether all the characteristics are
   required to match, which is not the case for ambiguity checks.
   'p1' and 'p2' are the PASS arguments of both procedures (if applicable).  */

bool
gfc_compare_interfaces (gfc_symbol *s1, gfc_symbol *s2, const char *name2,
			int generic_flag, int strict_flag,
			char *errmsg, int err_len,
			const char *p1, const char *p2,
			bool *bad_result_characteristics)
{
  gfc_formal_arglist *f1, *f2;

  gcc_assert (name2 != NULL);

  if (bad_result_characteristics)
    *bad_result_characteristics = false;

  if (s1->attr.function && (s2->attr.subroutine
      || (!s2->attr.function && s2->ts.type == BT_UNKNOWN
	  && gfc_get_default_type (name2, s2->ns)->type == BT_UNKNOWN)))
    {
      if (errmsg != NULL)
	snprintf (errmsg, err_len, "'%s' is not a function", name2);
      return false;
    }

  if (s1->attr.subroutine && s2->attr.function)
    {
      if (errmsg != NULL)
	snprintf (errmsg, err_len, "'%s' is not a subroutine", name2);
      return false;
    }

  if (s2->attr.subroutine && s1->attr.flavor == FL_VARIABLE)
    {
      if (errmsg != NULL)
	snprintf (errmsg, err_len, "subroutine proc pointer '%s' passed "
		  "to dummy variable '%s'", name2, s1->name);
      return false;
    }

  /* Do strict checks on all characteristics
     (for dummy procedures and procedure pointer assignments).  */
  if (!generic_flag && strict_flag)
    {
      if (s1->attr.function && s2->attr.function)
	{
	  /* If both are functions, check result characteristics.  */
	  if (!gfc_check_result_characteristics (s1, s2, errmsg, err_len)
	      || !gfc_check_result_characteristics (s2, s1, errmsg, err_len))
	    {
	      if (bad_result_characteristics)
		*bad_result_characteristics = true;
	      return false;
	    }
	}

      if (s1->attr.pure && !s2->attr.pure)
	{
	  snprintf (errmsg, err_len, "Mismatch in PURE attribute");
	  return false;
	}
      if (s1->attr.elemental && !s2->attr.elemental)
	{
	  snprintf (errmsg, err_len, "Mismatch in ELEMENTAL attribute");
	  return false;
	}
    }

  if (s1->attr.if_source == IFSRC_UNKNOWN
      || s2->attr.if_source == IFSRC_UNKNOWN)
    return true;

  f1 = gfc_sym_get_dummy_args (s1);
  f2 = gfc_sym_get_dummy_args (s2);

  /* Special case: No arguments.  */
  if (f1 == NULL && f2 == NULL)
    return true;

  if (generic_flag)
    {
      if (count_types_test (f1, f2, p1, p2)
	  || count_types_test (f2, f1, p2, p1))
	return false;

      /* Special case: alternate returns.  If both f1->sym and f2->sym are
	 NULL, then the leading formal arguments are alternate returns.
	 The previous conditional should catch argument lists with
	 different number of argument.  */
      if (f1 && f1->sym == NULL && f2 && f2->sym == NULL)
	return true;

      if (generic_correspondence (f1, f2, p1, p2)
	  || generic_correspondence (f2, f1, p2, p1))
	return false;
    }
  else
    /* Perform the abbreviated correspondence test for operators (the
       arguments cannot be optional and are always ordered correctly).
       This is also done when comparing interfaces for dummy procedures and in
       procedure pointer assignments.  */

    for (; f1 || f2; f1 = f1->next, f2 = f2->next)
      {
	/* Check existence.  */
	if (f1 == NULL || f2 == NULL)
	  {
	    if (errmsg != NULL)
	      snprintf (errmsg, err_len, "'%s' has the wrong number of "
			"arguments", name2);
	    return false;
	  }

	if (strict_flag)
	  {
	    /* Check all characteristics.  */
	    if (!gfc_check_dummy_characteristics (f1->sym, f2->sym, true,
					      errmsg, err_len))
	      return false;
	  }
	else
	  {
	    /* Operators: Only check type and rank of arguments.  */
	    if (!compare_type (f2->sym, f1->sym))
	      {
		if (errmsg != NULL)
		  snprintf (errmsg, err_len, "Type mismatch in argument '%s' "
			    "(%s/%s)", f1->sym->name,
			    gfc_typename (&f1->sym->ts),
			    gfc_typename (&f2->sym->ts));
		return false;
	      }
	    if (!compare_rank (f2->sym, f1->sym))
	      {
		if (errmsg != NULL)
		  snprintf (errmsg, err_len, "Rank mismatch in argument "
			    "'%s' (%i/%i)", f1->sym->name,
			    symbol_rank (f1->sym), symbol_rank (f2->sym));
		return false;
	      }
	    if ((gfc_option.allow_std & GFC_STD_F2008)
		&& (compare_ptr_alloc(f1->sym, f2->sym)
		    || compare_ptr_alloc(f2->sym, f1->sym)))
	      {
    		if (errmsg != NULL)
		  snprintf (errmsg, err_len, "Mismatching POINTER/ALLOCATABLE "
			    "attribute in argument '%s' ", f1->sym->name);
		return false;
	      }
	  }
      }

  return true;
}


/* Given a pointer to an interface pointer, remove duplicate
   interfaces and make sure that all symbols are either functions
   or subroutines, and all of the same kind.  Returns true if
   something goes wrong.  */

static bool
check_interface0 (gfc_interface *p, const char *interface_name)
{
  gfc_interface *psave, *q, *qlast;

  psave = p;
  for (; p; p = p->next)
    {
      /* Make sure all symbols in the interface have been defined as
	 functions or subroutines.  */
      if (((!p->sym->attr.function && !p->sym->attr.subroutine)
	   || !p->sym->attr.if_source)
	  && !gfc_fl_struct (p->sym->attr.flavor))
	{
	  const char *guessed
	    = gfc_lookup_function_fuzzy (p->sym->name, p->sym->ns->sym_root);

	  if (p->sym->attr.external)
	    if (guessed)
	      gfc_error ("Procedure %qs in %s at %L has no explicit interface"
			 "; did you mean %qs?",
			 p->sym->name, interface_name, &p->sym->declared_at,
			 guessed);
	    else
	      gfc_error ("Procedure %qs in %s at %L has no explicit interface",
			 p->sym->name, interface_name, &p->sym->declared_at);
	  else
	    if (guessed)
	      gfc_error ("Procedure %qs in %s at %L is neither function nor "
			 "subroutine; did you mean %qs?", p->sym->name,
			interface_name, &p->sym->declared_at, guessed);
	    else
	      gfc_error ("Procedure %qs in %s at %L is neither function nor "
			 "subroutine", p->sym->name, interface_name,
			&p->sym->declared_at);
	  return true;
	}

      /* Verify that procedures are either all SUBROUTINEs or all FUNCTIONs.  */
      if ((psave->sym->attr.function && !p->sym->attr.function
	   && !gfc_fl_struct (p->sym->attr.flavor))
	  || (psave->sym->attr.subroutine && !p->sym->attr.subroutine))
	{
	  if (!gfc_fl_struct (p->sym->attr.flavor))
	    gfc_error ("In %s at %L procedures must be either all SUBROUTINEs"
		       " or all FUNCTIONs", interface_name,
		       &p->sym->declared_at);
	  else if (p->sym->attr.flavor == FL_DERIVED)
	    gfc_error ("In %s at %L procedures must be all FUNCTIONs as the "
		       "generic name is also the name of a derived type",
		       interface_name, &p->sym->declared_at);
	  return true;
	}

      /* F2003, C1207. F2008, C1207.  */
      if (p->sym->attr.proc == PROC_INTERNAL
	  && !gfc_notify_std (GFC_STD_F2008, "Internal procedure "
			      "%qs in %s at %L", p->sym->name,
			      interface_name, &p->sym->declared_at))
	return true;
    }
  p = psave;

  /* Remove duplicate interfaces in this interface list.  */
  for (; p; p = p->next)
    {
      qlast = p;

      for (q = p->next; q;)
	{
	  if (p->sym != q->sym)
	    {
	      qlast = q;
	      q = q->next;
	    }
	  else
	    {
	      /* Duplicate interface.  */
	      qlast->next = q->next;
	      free (q);
	      q = qlast->next;
	    }
	}
    }

  return false;
}


/* Check lists of interfaces to make sure that no two interfaces are
   ambiguous.  Duplicate interfaces (from the same symbol) are OK here.  */

static bool
check_interface1 (gfc_interface *p, gfc_interface *q0,
		  int generic_flag, const char *interface_name,
		  bool referenced)
{
  gfc_interface *q;
  for (; p; p = p->next)
    for (q = q0; q; q = q->next)
      {
	if (p->sym == q->sym)
	  continue;		/* Duplicates OK here.  */

	if (p->sym->name == q->sym->name && p->sym->module == q->sym->module)
	  continue;

	if (!gfc_fl_struct (p->sym->attr.flavor)
	    && !gfc_fl_struct (q->sym->attr.flavor)
	    && gfc_compare_interfaces (p->sym, q->sym, q->sym->name,
				       generic_flag, 0, NULL, 0, NULL, NULL))
	  {
	    if (referenced)
	      gfc_error ("Ambiguous interfaces in %s for %qs at %L "
			 "and %qs at %L", interface_name,
			 q->sym->name, &q->sym->declared_at,
			 p->sym->name, &p->sym->declared_at);
	    else if (!p->sym->attr.use_assoc && q->sym->attr.use_assoc)
	      gfc_warning (0, "Ambiguous interfaces in %s for %qs at %L "
			 "and %qs at %L", interface_name,
			 q->sym->name, &q->sym->declared_at,
			 p->sym->name, &p->sym->declared_at);
	    else
	      gfc_warning (0, "Although not referenced, %qs has ambiguous "
			   "interfaces at %L", interface_name, &p->where);
	    return true;
	  }
      }
  return false;
}


/* Check the generic and operator interfaces of symbols to make sure
   that none of the interfaces conflict.  The check has to be done
   after all of the symbols are actually loaded.  */

static void
check_sym_interfaces (gfc_symbol *sym)
{
  /* Provide sufficient space to hold "generic interface 'symbol.symbol'".  */
  char interface_name[2*GFC_MAX_SYMBOL_LEN+2 + sizeof("generic interface ''")];
  gfc_interface *p;

  if (sym->ns != gfc_current_ns)
    return;

  if (sym->generic != NULL)
    {
      size_t len = strlen (sym->name) + sizeof("generic interface ''");
      gcc_assert (len < sizeof (interface_name));
      sprintf (interface_name, "generic interface '%s'", sym->name);
      if (check_interface0 (sym->generic, interface_name))
	return;

      for (p = sym->generic; p; p = p->next)
	{
	  if (p->sym->attr.mod_proc
	      && !p->sym->attr.module_procedure
	      && (p->sym->attr.if_source != IFSRC_DECL
		  || p->sym->attr.procedure))
	    {
	      gfc_error ("%qs at %L is not a module procedure",
			 p->sym->name, &p->where);
	      return;
	    }
	}

      /* Originally, this test was applied to host interfaces too;
	 this is incorrect since host associated symbols, from any
	 source, cannot be ambiguous with local symbols.  */
      check_interface1 (sym->generic, sym->generic, 1, interface_name,
			sym->attr.referenced || !sym->attr.use_assoc);
    }
}


static void
check_uop_interfaces (gfc_user_op *uop)
{
  char interface_name[GFC_MAX_SYMBOL_LEN + sizeof("operator interface ''")];
  gfc_user_op *uop2;
  gfc_namespace *ns;

  sprintf (interface_name, "operator interface '%s'", uop->name);
  if (check_interface0 (uop->op, interface_name))
    return;

  for (ns = gfc_current_ns; ns; ns = ns->parent)
    {
      uop2 = gfc_find_uop (uop->name, ns);
      if (uop2 == NULL)
	continue;

      check_interface1 (uop->op, uop2->op, 0,
			interface_name, true);
    }
}

/* Given an intrinsic op, return an equivalent op if one exists,
   or INTRINSIC_NONE otherwise.  */

gfc_intrinsic_op
gfc_equivalent_op (gfc_intrinsic_op op)
{
  switch(op)
    {
    case INTRINSIC_EQ:
      return INTRINSIC_EQ_OS;

    case INTRINSIC_EQ_OS:
      return INTRINSIC_EQ;

    case INTRINSIC_NE:
      return INTRINSIC_NE_OS;

    case INTRINSIC_NE_OS:
      return INTRINSIC_NE;

    case INTRINSIC_GT:
      return INTRINSIC_GT_OS;

    case INTRINSIC_GT_OS:
      return INTRINSIC_GT;

    case INTRINSIC_GE:
      return INTRINSIC_GE_OS;

    case INTRINSIC_GE_OS:
      return INTRINSIC_GE;

    case INTRINSIC_LT:
      return INTRINSIC_LT_OS;

    case INTRINSIC_LT_OS:
      return INTRINSIC_LT;

    case INTRINSIC_LE:
      return INTRINSIC_LE_OS;

    case INTRINSIC_LE_OS:
      return INTRINSIC_LE;

    default:
      return INTRINSIC_NONE;
    }
}

/* For the namespace, check generic, user operator and intrinsic
   operator interfaces for consistency and to remove duplicate
   interfaces.  We traverse the whole namespace, counting on the fact
   that most symbols will not have generic or operator interfaces.  */

void
gfc_check_interfaces (gfc_namespace *ns)
{
  gfc_namespace *old_ns, *ns2;
  char interface_name[GFC_MAX_SYMBOL_LEN + sizeof("intrinsic '' operator")];
  int i;

  old_ns = gfc_current_ns;
  gfc_current_ns = ns;

  gfc_traverse_ns (ns, check_sym_interfaces);

  gfc_traverse_user_op (ns, check_uop_interfaces);

  for (i = GFC_INTRINSIC_BEGIN; i != GFC_INTRINSIC_END; i++)
    {
      if (i == INTRINSIC_USER)
	continue;

      if (i == INTRINSIC_ASSIGN)
	strcpy (interface_name, "intrinsic assignment operator");
      else
	sprintf (interface_name, "intrinsic '%s' operator",
		 gfc_op2string ((gfc_intrinsic_op) i));

      if (check_interface0 (ns->op[i], interface_name))
	continue;

      if (ns->op[i])
	gfc_check_operator_interface (ns->op[i]->sym, (gfc_intrinsic_op) i,
				      ns->op[i]->where);

      for (ns2 = ns; ns2; ns2 = ns2->parent)
	{
	  gfc_intrinsic_op other_op;

	  if (check_interface1 (ns->op[i], ns2->op[i], 0,
				interface_name, true))
	    goto done;

	  /* i should be gfc_intrinsic_op, but has to be int with this cast
	     here for stupid C++ compatibility rules.  */
	  other_op = gfc_equivalent_op ((gfc_intrinsic_op) i);
	  if (other_op != INTRINSIC_NONE
	    &&  check_interface1 (ns->op[i], ns2->op[other_op],
				  0, interface_name, true))
	    goto done;
	}
    }

done:
  gfc_current_ns = old_ns;
}


/* Given a symbol of a formal argument list and an expression, if the
   formal argument is allocatable, check that the actual argument is
   allocatable. Returns true if compatible, zero if not compatible.  */

static bool
compare_allocatable (gfc_symbol *formal, gfc_expr *actual)
{
  if (formal->attr.allocatable
      || (formal->ts.type == BT_CLASS && CLASS_DATA (formal)->attr.allocatable))
    {
      symbol_attribute attr = gfc_expr_attr (actual);
      if (actual->ts.type == BT_CLASS && !attr.class_ok)
	return true;
      else if (!attr.allocatable)
	return false;
    }

  return true;
}


/* Given a symbol of a formal argument list and an expression, if the
   formal argument is a pointer, see if the actual argument is a
   pointer. Returns nonzero if compatible, zero if not compatible.  */

static int
compare_pointer (gfc_symbol *formal, gfc_expr *actual)
{
  symbol_attribute attr;

  if (formal->attr.pointer
      || (formal->ts.type == BT_CLASS && CLASS_DATA (formal)
	  && CLASS_DATA (formal)->attr.class_pointer))
    {
      attr = gfc_expr_attr (actual);

      /* Fortran 2008 allows non-pointer actual arguments.  */
      if (!attr.pointer && attr.target && formal->attr.intent == INTENT_IN)
	return 2;

      if (!attr.pointer)
	return 0;
    }

  return 1;
}


/* Emit clear error messages for rank mismatch.  */

static void
argument_rank_mismatch (const char *name, locus *where,
			int rank1, int rank2, locus *where_formal)
{

  /* TS 29113, C407b.  */
  if (where_formal == NULL)
    {
      if (rank2 == -1)
	gfc_error ("The assumed-rank array at %L requires that the dummy "
		   "argument %qs has assumed-rank", where, name);
      else if (rank1 == 0)
	gfc_error_opt (0, "Rank mismatch in argument %qs "
		       "at %L (scalar and rank-%d)", name, where, rank2);
      else if (rank2 == 0)
	gfc_error_opt (0, "Rank mismatch in argument %qs "
		       "at %L (rank-%d and scalar)", name, where, rank1);
      else
	gfc_error_opt (0, "Rank mismatch in argument %qs "
		       "at %L (rank-%d and rank-%d)", name, where, rank1,
		       rank2);
    }
  else
    {
      if (rank2 == -1)
	/* This is an assumed rank-actual passed to a function without
	   an explicit interface, which is already diagnosed in
	   gfc_procedure_use.  */
	return;
      if (rank1 == 0)
	gfc_error_opt (0, "Rank mismatch between actual argument at %L "
		       "and actual argument at %L (scalar and rank-%d)",
		       where, where_formal, rank2);
      else if (rank2 == 0)
	gfc_error_opt (0, "Rank mismatch between actual argument at %L "
		       "and actual argument at %L (rank-%d and scalar)",
		       where, where_formal, rank1);
      else
	gfc_error_opt (0, "Rank mismatch between actual argument at %L "
		       "and actual argument at %L (rank-%d and rank-%d)", where,
		       where_formal, rank1, rank2);
    }
}


/* Under certain conditions, a scalar actual argument can be passed
   to an array dummy argument - see F2018, 15.5.2.4, paragraph 14.
   This function returns true for these conditions so that an error
   or warning for this can be suppressed later.  Always return false
   for expressions with rank > 0.  */

bool
maybe_dummy_array_arg (gfc_expr *e)
{
  gfc_symbol *s;
  gfc_ref *ref;
  bool array_pointer = false;
  bool assumed_shape = false;
  bool scalar_ref = true;

  if (e->rank > 0)
    return false;

  if (e->ts.type == BT_CHARACTER && e->ts.kind == 1)
    return true;

  /* If this comes from a constructor, it has been an array element
     originally.  */

  if (e->expr_type == EXPR_CONSTANT)
    return e->from_constructor;

  if (e->expr_type != EXPR_VARIABLE)
    return false;

  s = e->symtree->n.sym;

  if (s->attr.dimension)
    {
      scalar_ref = false;
      array_pointer = s->attr.pointer;
    }

  if (s->as && s->as->type == AS_ASSUMED_SHAPE)
    assumed_shape = true;

  for (ref=e->ref; ref; ref=ref->next)
    {
      if (ref->type == REF_COMPONENT)
	{
	  symbol_attribute *attr;
	  attr = &ref->u.c.component->attr;
	  if (attr->dimension)
	    {
	      array_pointer = attr->pointer;
	      assumed_shape = false;
	      scalar_ref = false;
	    }
	  else
	    scalar_ref = true;
	}
    }

  return !(scalar_ref || array_pointer || assumed_shape);
}

/* Given a symbol of a formal argument list and an expression, see if
   the two are compatible as arguments.  Returns true if
   compatible, false if not compatible.  */

static bool
compare_parameter (gfc_symbol *formal, gfc_expr *actual,
		   int ranks_must_agree, int is_elemental, locus *where)
{
  gfc_ref *ref;
  bool rank_check, is_pointer;
  char err[200];
  gfc_component *ppc;
  bool codimension = false;
  gfc_array_spec *formal_as;

  /* If the formal arg has type BT_VOID, it's to one of the iso_c_binding
     procs c_f_pointer or c_f_procpointer, and we need to accept most
     pointers the user could give us.  This should allow that.  */
  if (formal->ts.type == BT_VOID)
    return true;

  if (formal->ts.type == BT_DERIVED
      && formal->ts.u.derived && formal->ts.u.derived->ts.is_iso_c
      && actual->ts.type == BT_DERIVED
      && actual->ts.u.derived && actual->ts.u.derived->ts.is_iso_c)
    {
      if (formal->ts.u.derived->intmod_sym_id
	  != actual->ts.u.derived->intmod_sym_id)
	return false;

      if (ranks_must_agree
	  && symbol_rank (formal) != actual->rank
	  && symbol_rank (formal) != -1)
	{
	  if (where)
	    argument_rank_mismatch (formal->name, &actual->where,
				    symbol_rank (formal), actual->rank,
				    NULL);
	  return false;
	}
      return true;
    }

  if (formal->ts.type == BT_CLASS && actual->ts.type == BT_DERIVED)
    /* Make sure the vtab symbol is present when
       the module variables are generated.  */
    gfc_find_derived_vtab (actual->ts.u.derived);

  if (actual->ts.type == BT_PROCEDURE)
    {
      gfc_symbol *act_sym = actual->symtree->n.sym;

      if (formal->attr.flavor != FL_PROCEDURE && !act_sym->ts.interface)
	{
	  if (where)
	    gfc_error ("Invalid procedure argument at %L", &actual->where);
	  return false;
	}
      else if (act_sym->ts.interface
	       && !gfc_compare_interfaces (formal, act_sym->ts.interface,
					   act_sym->name, 0, 1, err,
					   sizeof(err),NULL, NULL))
	{
	  if (where)
	    gfc_error_opt (0, "Interface mismatch in dummy procedure %qs at %L:"
			   " %s", formal->name, &actual->where, err);
	  return false;
	}

      if (!gfc_compare_interfaces (formal, act_sym, act_sym->name, 0, 1, err,
				   sizeof(err), NULL, NULL))
	{
	  if (where)
	    gfc_error_opt (0, "Interface mismatch in dummy procedure %qs at %L:"
			   " %s", formal->name, &actual->where, err);
	  return false;
	}

      if (formal->attr.function && !act_sym->attr.function)
	{
	  gfc_add_function (&act_sym->attr, act_sym->name,
	  &act_sym->declared_at);
	  if (act_sym->ts.type == BT_UNKNOWN
	      && !gfc_set_default_type (act_sym, 1, act_sym->ns))
	    return false;
	}
      else if (formal->attr.subroutine && !act_sym->attr.subroutine)
	gfc_add_subroutine (&act_sym->attr, act_sym->name,
			    &act_sym->declared_at);

      return true;
    }

  ppc = gfc_get_proc_ptr_comp (actual);
  if (ppc && ppc->ts.interface)
    {
      if (!gfc_compare_interfaces (formal, ppc->ts.interface, ppc->name, 0, 1,
				   err, sizeof(err), NULL, NULL))
	{
	  if (where)
	    gfc_error_opt (0, "Interface mismatch in dummy procedure %qs at %L:"
			   " %s", formal->name, &actual->where, err);
	  return false;
	}
    }

  /* F2008, C1241.  */
  if (formal->attr.pointer && formal->attr.contiguous
      && !gfc_is_simply_contiguous (actual, true, false))
    {
      if (where)
	gfc_error ("Actual argument to contiguous pointer dummy %qs at %L "
		   "must be simply contiguous", formal->name, &actual->where);
      return false;
    }

  symbol_attribute actual_attr = gfc_expr_attr (actual);
  if (actual->ts.type == BT_CLASS && !actual_attr.class_ok)
    return true;

  if ((actual->expr_type != EXPR_NULL || actual->ts.type != BT_UNKNOWN)
      && actual->ts.type != BT_HOLLERITH
      && formal->ts.type != BT_ASSUMED
      && !(formal->attr.ext_attr & (1 << EXT_ATTR_NO_ARG_CHECK))
      && !gfc_compare_types (&formal->ts, &actual->ts)
      && !(formal->ts.type == BT_DERIVED && actual->ts.type == BT_CLASS
	   && gfc_compare_derived_types (formal->ts.u.derived,
					 CLASS_DATA (actual)->ts.u.derived)))
    {
      if (where)
	{
	  if (formal->attr.artificial)
	    {
	      if (!flag_allow_argument_mismatch || !formal->error)
		gfc_error_opt (0, "Type mismatch between actual argument at %L "
			       "and actual argument at %L (%s/%s).",
			       &actual->where,
			       &formal->declared_at,
			       gfc_typename (actual),
			       gfc_dummy_typename (&formal->ts));

	      formal->error = 1;
	    }
	  else
	    gfc_error_opt (0, "Type mismatch in argument %qs at %L; passed %s "
			   "to %s", formal->name, where, gfc_typename (actual),
			   gfc_dummy_typename (&formal->ts));
	}
      return false;
    }

  if (actual->ts.type == BT_ASSUMED && formal->ts.type != BT_ASSUMED)
    {
      if (where)
	gfc_error ("Assumed-type actual argument at %L requires that dummy "
		   "argument %qs is of assumed type", &actual->where,
		   formal->name);
      return false;
    }

  /* TS29113 C407c; F2018 C711.  */
  if (actual->ts.type == BT_ASSUMED
      && symbol_rank (formal) == -1
      && actual->rank != -1
      && !(actual->symtree->n.sym->as
	   && actual->symtree->n.sym->as->type == AS_ASSUMED_SHAPE))
    {
      if (where)
	gfc_error ("Assumed-type actual argument at %L corresponding to "
		   "assumed-rank dummy argument %qs must be "
		   "assumed-shape or assumed-rank",
		   &actual->where, formal->name);
      return false;
    }

  /* F2008, 12.5.2.5; IR F08/0073.  */
  if (formal->ts.type == BT_CLASS && formal->attr.class_ok
      && actual->expr_type != EXPR_NULL
      && ((CLASS_DATA (formal)->attr.class_pointer
	   && formal->attr.intent != INTENT_IN)
          || CLASS_DATA (formal)->attr.allocatable))
    {
      if (actual->ts.type != BT_CLASS)
	{
	  if (where)
	    gfc_error ("Actual argument to %qs at %L must be polymorphic",
			formal->name, &actual->where);
	  return false;
	}

      if ((!UNLIMITED_POLY (formal) || !UNLIMITED_POLY(actual))
	  && !gfc_compare_derived_types (CLASS_DATA (actual)->ts.u.derived,
					 CLASS_DATA (formal)->ts.u.derived))
	{
	  if (where)
	    gfc_error ("Actual argument to %qs at %L must have the same "
		       "declared type", formal->name, &actual->where);
	  return false;
	}
    }

  /* F08: 12.5.2.5 Allocatable and pointer dummy variables.  However, this
     is necessary also for F03, so retain error for both.
     NOTE: Other type/kind errors pre-empt this error.  Since they are F03
     compatible, no attempt has been made to channel to this one.  */
  if (UNLIMITED_POLY (formal) && !UNLIMITED_POLY (actual)
      && (CLASS_DATA (formal)->attr.allocatable
	  ||CLASS_DATA (formal)->attr.class_pointer))
    {
      if (where)
	gfc_error ("Actual argument to %qs at %L must be unlimited "
		   "polymorphic since the formal argument is a "
		   "pointer or allocatable unlimited polymorphic "
		   "entity [F2008: 12.5.2.5]", formal->name,
		   &actual->where);
      return false;
    }

  if (formal->ts.type == BT_CLASS && formal->attr.class_ok)
    codimension = CLASS_DATA (formal)->attr.codimension;
  else
    codimension = formal->attr.codimension;

  if (codimension && !gfc_is_coarray (actual))
    {
      if (where)
	gfc_error ("Actual argument to %qs at %L must be a coarray",
		       formal->name, &actual->where);
      return false;
    }

  formal_as = (formal->ts.type == BT_CLASS
	       ? CLASS_DATA (formal)->as : formal->as);

  if (codimension && formal->attr.allocatable)
    {
      gfc_ref *last = NULL;

      for (ref = actual->ref; ref; ref = ref->next)
	if (ref->type == REF_COMPONENT)
	  last = ref;

      /* F2008, 12.5.2.6.  */
      if ((last && last->u.c.component->as->corank != formal->as->corank)
	  || (!last
	      && actual->symtree->n.sym->as->corank != formal->as->corank))
	{
	  if (where)
	    gfc_error ("Corank mismatch in argument %qs at %L (%d and %d)",
		   formal->name, &actual->where, formal->as->corank,
		   last ? last->u.c.component->as->corank
			: actual->symtree->n.sym->as->corank);
	  return false;
	}
    }

  if (codimension)
    {
      /* F2008, 12.5.2.8 + Corrig 2 (IR F08/0048).  */
      /* F2018, 12.5.2.8.  */
      if (formal->attr.dimension
	  && (formal->attr.contiguous || formal->as->type != AS_ASSUMED_SHAPE)
	  && actual_attr.dimension
	  && !gfc_is_simply_contiguous (actual, true, true))
	{
	  if (where)
	    gfc_error ("Actual argument to %qs at %L must be simply "
		       "contiguous or an element of such an array",
		       formal->name, &actual->where);
	  return false;
	}

      /* F2008, C1303 and C1304.  */
      if (formal->attr.intent != INTENT_INOUT
	  && (((formal->ts.type == BT_DERIVED || formal->ts.type == BT_CLASS)
	       && formal->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
	       && formal->ts.u.derived->intmod_sym_id == ISOFORTRAN_LOCK_TYPE)
	      || formal->attr.lock_comp))

    	{
	  if (where)
	    gfc_error ("Actual argument to non-INTENT(INOUT) dummy %qs at %L, "
		       "which is LOCK_TYPE or has a LOCK_TYPE component",
		       formal->name, &actual->where);
	  return false;
	}

      /* TS18508, C702/C703.  */
      if (formal->attr.intent != INTENT_INOUT
	  && (((formal->ts.type == BT_DERIVED || formal->ts.type == BT_CLASS)
	       && formal->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
	       && formal->ts.u.derived->intmod_sym_id == ISOFORTRAN_EVENT_TYPE)
	      || formal->attr.event_comp))

    	{
	  if (where)
	    gfc_error ("Actual argument to non-INTENT(INOUT) dummy %qs at %L, "
		       "which is EVENT_TYPE or has a EVENT_TYPE component",
		       formal->name, &actual->where);
	  return false;
	}
    }

  /* F2008, C1239/C1240.  */
  if (actual->expr_type == EXPR_VARIABLE
      && (actual->symtree->n.sym->attr.asynchronous
         || actual->symtree->n.sym->attr.volatile_)
      &&  (formal->attr.asynchronous || formal->attr.volatile_)
      && actual->rank && formal->as
      && !gfc_is_simply_contiguous (actual, true, false)
      && ((formal->as->type != AS_ASSUMED_SHAPE
	   && formal->as->type != AS_ASSUMED_RANK && !formal->attr.pointer)
	  || formal->attr.contiguous))
    {
      if (where)
	gfc_error ("Dummy argument %qs has to be a pointer, assumed-shape or "
		   "assumed-rank array without CONTIGUOUS attribute - as actual"
		   " argument at %L is not simply contiguous and both are "
		   "ASYNCHRONOUS or VOLATILE", formal->name, &actual->where);
      return false;
    }

  if (formal->attr.allocatable && !codimension
      && actual_attr.codimension)
    {
      if (formal->attr.intent == INTENT_OUT)
	{
	  if (where)
	    gfc_error ("Passing coarray at %L to allocatable, noncoarray, "
		       "INTENT(OUT) dummy argument %qs", &actual->where,
		       formal->name);
	  return false;
	}
      else if (warn_surprising && where && formal->attr.intent != INTENT_IN)
	gfc_warning (OPT_Wsurprising,
		     "Passing coarray at %L to allocatable, noncoarray dummy "
		     "argument %qs, which is invalid if the allocation status"
		     " is modified",  &actual->where, formal->name);
    }

  /* If the rank is the same or the formal argument has assumed-rank.  */
  if (symbol_rank (formal) == actual->rank || symbol_rank (formal) == -1)
    return true;

  rank_check = where != NULL && !is_elemental && formal_as
    && (formal_as->type == AS_ASSUMED_SHAPE
	|| formal_as->type == AS_DEFERRED)
    && !(actual->expr_type == EXPR_NULL
	 && actual->ts.type == BT_UNKNOWN);

  /* Skip rank checks for NO_ARG_CHECK.  */
  if (formal->attr.ext_attr & (1 << EXT_ATTR_NO_ARG_CHECK))
    return true;

  /* Scalar & coindexed, see: F2008, Section 12.5.2.4.  */
  if (rank_check || ranks_must_agree
      || (formal->attr.pointer && actual->expr_type != EXPR_NULL)
      || (actual->rank != 0
	  && !(is_elemental || formal->attr.dimension
	       || (formal->ts.type == BT_CLASS
		   && CLASS_DATA (formal)->attr.dimension)))
      || (actual->rank == 0
	  && ((formal->ts.type == BT_CLASS
	       && CLASS_DATA (formal)->as->type == AS_ASSUMED_SHAPE)
	      || (formal->ts.type != BT_CLASS
		   && formal->as->type == AS_ASSUMED_SHAPE))
	  && actual->expr_type != EXPR_NULL)
      || (actual->rank == 0
	  && (formal->attr.dimension
	      || (formal->ts.type == BT_CLASS
		  && CLASS_DATA (formal)->attr.dimension))
	  && gfc_is_coindexed (actual))
      /* Assumed-rank actual argument; F2018 C838.  */
      || actual->rank == -1)
    {
      if (where
	  && (!formal->attr.artificial || (!formal->maybe_array
					   && !maybe_dummy_array_arg (actual))))
	{
	  locus *where_formal;
	  if (formal->attr.artificial)
	    where_formal = &formal->declared_at;
	  else
	    where_formal = NULL;

	  argument_rank_mismatch (formal->name, &actual->where,
				  symbol_rank (formal), actual->rank,
				  where_formal);
	}
      return false;
    }
  else if (actual->rank != 0
	   && (is_elemental || formal->attr.dimension
	       || (formal->ts.type == BT_CLASS
		   && CLASS_DATA (formal)->attr.dimension)))
    return true;

  /* At this point, we are considering a scalar passed to an array.   This
     is valid (cf. F95 12.4.1.1, F2003 12.4.1.2, and F2008 12.5.2.4),
     - if the actual argument is (a substring of) an element of a
       non-assumed-shape/non-pointer/non-polymorphic array; or
     - (F2003) if the actual argument is of type character of default/c_char
       kind.
     - (F2018) if the dummy argument is type(*).  */

  is_pointer = actual->expr_type == EXPR_VARIABLE
	       ? actual->symtree->n.sym->attr.pointer : false;

  for (ref = actual->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT)
	is_pointer = ref->u.c.component->attr.pointer;
      else if (ref->type == REF_ARRAY && ref->u.ar.type == AR_ELEMENT
	       && ref->u.ar.dimen > 0
	       && (!ref->next
		   || (ref->next->type == REF_SUBSTRING && !ref->next->next)))
        break;
    }

  if (actual->ts.type == BT_CLASS && actual->expr_type != EXPR_NULL)
    {
      if (where)
	gfc_error ("Polymorphic scalar passed to array dummy argument %qs "
		   "at %L", formal->name, &actual->where);
      return false;
    }

  if (actual->expr_type != EXPR_NULL && ref && actual->ts.type != BT_CHARACTER
      && (is_pointer || ref->u.ar.as->type == AS_ASSUMED_SHAPE))
    {
      if (where)
	{
	  if (formal->attr.artificial)
	    gfc_error ("Element of assumed-shape or pointer array "
		       "as actual argument at %L cannot correspond to "
		       "actual argument at %L",
		       &actual->where, &formal->declared_at);
	  else
	    gfc_error ("Element of assumed-shape or pointer "
		       "array passed to array dummy argument %qs at %L",
		       formal->name, &actual->where);
	}
      return false;
    }

  if (actual->ts.type == BT_CHARACTER && actual->expr_type != EXPR_NULL
      && (!ref || is_pointer || ref->u.ar.as->type == AS_ASSUMED_SHAPE))
    {
      if (formal->ts.kind != 1 && (gfc_option.allow_std & GFC_STD_GNU) == 0)
	{
	  if (where)
	    gfc_error ("Extension: Scalar non-default-kind, non-C_CHAR-kind "
		       "CHARACTER actual argument with array dummy argument "
		       "%qs at %L", formal->name, &actual->where);
	  return false;
	}

      if (where && (gfc_option.allow_std & GFC_STD_F2003) == 0)
	{
	  gfc_error ("Fortran 2003: Scalar CHARACTER actual argument with "
		     "array dummy argument %qs at %L",
		     formal->name, &actual->where);
	  return false;
	}
      else
	return ((gfc_option.allow_std & GFC_STD_F2003) != 0);
    }

  if (ref == NULL && actual->expr_type != EXPR_NULL)
    {
      if (actual->rank == 0
	  && formal->ts.type == BT_ASSUMED
	  && formal->as
	  && formal->as->type == AS_ASSUMED_SIZE)
	/* This is new in F2018, type(*) is new in TS29113, but gfortran does
	   not differentiate.  Thus, if type(*) exists, it is valid;
	   otherwise, type(*) is already rejected.  */
	return true;
      if (where
	  && (!formal->attr.artificial || (!formal->maybe_array
					   && !maybe_dummy_array_arg (actual))))
	{
	  locus *where_formal;
	  if (formal->attr.artificial)
	    where_formal = &formal->declared_at;
	  else
	    where_formal = NULL;

	  argument_rank_mismatch (formal->name, &actual->where,
				  symbol_rank (formal), actual->rank,
				  where_formal);
	}
      return false;
    }

  return true;
}


/* Returns the storage size of a symbol (formal argument) or
   zero if it cannot be determined.  */

static unsigned long
get_sym_storage_size (gfc_symbol *sym)
{
  int i;
  unsigned long strlen, elements;

  if (sym->ts.type == BT_CHARACTER)
    {
      if (sym->ts.u.cl && sym->ts.u.cl->length
	  && sym->ts.u.cl->length->expr_type == EXPR_CONSTANT
	  && sym->ts.u.cl->length->ts.type == BT_INTEGER)
	strlen = mpz_get_ui (sym->ts.u.cl->length->value.integer);
      else
	return 0;
    }
  else
    strlen = 1;

  if (symbol_rank (sym) == 0)
    return strlen;

  elements = 1;
  if (sym->as->type != AS_EXPLICIT)
    return 0;
  for (i = 0; i < sym->as->rank; i++)
    {
      if (sym->as->upper[i]->expr_type != EXPR_CONSTANT
	  || sym->as->lower[i]->expr_type != EXPR_CONSTANT
	  || sym->as->upper[i]->ts.type != BT_INTEGER
	  || sym->as->lower[i]->ts.type != BT_INTEGER)
	return 0;

      elements *= mpz_get_si (sym->as->upper[i]->value.integer)
		  - mpz_get_si (sym->as->lower[i]->value.integer) + 1L;
    }

  return strlen*elements;
}


/* Returns the storage size of an expression (actual argument) or
   zero if it cannot be determined. For an array element, it returns
   the remaining size as the element sequence consists of all storage
   units of the actual argument up to the end of the array.  */

static unsigned long
get_expr_storage_size (gfc_expr *e)
{
  int i;
  long int strlen, elements;
  long int substrlen = 0;
  bool is_str_storage = false;
  gfc_ref *ref;

  if (e == NULL)
    return 0;

  if (e->ts.type == BT_CHARACTER)
    {
      if (e->ts.u.cl && e->ts.u.cl->length
	  && e->ts.u.cl->length->expr_type == EXPR_CONSTANT
	  && e->ts.u.cl->length->ts.type == BT_INTEGER)
	strlen = mpz_get_si (e->ts.u.cl->length->value.integer);
      else if (e->expr_type == EXPR_CONSTANT
	       && (e->ts.u.cl == NULL || e->ts.u.cl->length == NULL))
	strlen = e->value.character.length;
      else
	return 0;
    }
  else
    strlen = 1; /* Length per element.  */

  if (e->rank == 0 && !e->ref)
    return strlen;

  elements = 1;
  if (!e->ref)
    {
      if (!e->shape)
	return 0;
      for (i = 0; i < e->rank; i++)
	elements *= mpz_get_si (e->shape[i]);
      return elements*strlen;
    }

  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_SUBSTRING && ref->u.ss.start
	  && ref->u.ss.start->expr_type == EXPR_CONSTANT)
	{
	  if (is_str_storage)
	    {
	      /* The string length is the substring length.
		 Set now to full string length.  */
	      if (!ref->u.ss.length || !ref->u.ss.length->length
		  || ref->u.ss.length->length->expr_type != EXPR_CONSTANT)
		return 0;

	      strlen = mpz_get_ui (ref->u.ss.length->length->value.integer);
	    }
	  substrlen = strlen - mpz_get_ui (ref->u.ss.start->value.integer) + 1;
	  continue;
	}

      if (ref->type == REF_ARRAY && ref->u.ar.type == AR_SECTION)
	for (i = 0; i < ref->u.ar.dimen; i++)
	  {
	    long int start, end, stride;
	    stride = 1;

	    if (ref->u.ar.stride[i])
	      {
		if (ref->u.ar.stride[i]->expr_type == EXPR_CONSTANT
		    && ref->u.ar.stride[i]->ts.type == BT_INTEGER)
		  stride = mpz_get_si (ref->u.ar.stride[i]->value.integer);
		else
		  return 0;
	      }

	    if (ref->u.ar.start[i])
	      {
		if (ref->u.ar.start[i]->expr_type == EXPR_CONSTANT
		    && ref->u.ar.start[i]->ts.type == BT_INTEGER)
		  start = mpz_get_si (ref->u.ar.start[i]->value.integer);
		else
		  return 0;
	      }
	    else if (ref->u.ar.as->lower[i]
		     && ref->u.ar.as->lower[i]->expr_type == EXPR_CONSTANT
		     && ref->u.ar.as->lower[i]->ts.type == BT_INTEGER)
	      start = mpz_get_si (ref->u.ar.as->lower[i]->value.integer);
	    else
	      return 0;

	    if (ref->u.ar.end[i])
	      {
		if (ref->u.ar.end[i]->expr_type == EXPR_CONSTANT
		    && ref->u.ar.end[i]->ts.type == BT_INTEGER)
		  end = mpz_get_si (ref->u.ar.end[i]->value.integer);
		else
		  return 0;
	      }
	    else if (ref->u.ar.as->upper[i]
		     && ref->u.ar.as->upper[i]->expr_type == EXPR_CONSTANT
		     && ref->u.ar.as->upper[i]->ts.type == BT_INTEGER)
	      end = mpz_get_si (ref->u.ar.as->upper[i]->value.integer);
	    else
	      return 0;

	    elements *= (end - start)/stride + 1L;
	  }
      else if (ref->type == REF_ARRAY && ref->u.ar.type == AR_FULL)
	for (i = 0; i < ref->u.ar.as->rank; i++)
	  {
	    if (ref->u.ar.as->lower[i] && ref->u.ar.as->upper[i]
		&& ref->u.ar.as->lower[i]->expr_type == EXPR_CONSTANT
		&& ref->u.ar.as->lower[i]->ts.type == BT_INTEGER
		&& ref->u.ar.as->upper[i]->expr_type == EXPR_CONSTANT
		&& ref->u.ar.as->upper[i]->ts.type == BT_INTEGER)
	      elements *= mpz_get_si (ref->u.ar.as->upper[i]->value.integer)
			  - mpz_get_si (ref->u.ar.as->lower[i]->value.integer)
			  + 1L;
	    else
	      return 0;
	  }
      else if (ref->type == REF_ARRAY && ref->u.ar.type == AR_ELEMENT
	       && e->expr_type == EXPR_VARIABLE)
	{
	  if (ref->u.ar.as->type == AS_ASSUMED_SHAPE
	      || e->symtree->n.sym->attr.pointer)
	    {
	      elements = 1;
	      continue;
	    }

	  /* Determine the number of remaining elements in the element
	     sequence for array element designators.  */
	  is_str_storage = true;
	  for (i = ref->u.ar.dimen - 1; i >= 0; i--)
	    {
	      if (ref->u.ar.start[i] == NULL
		  || ref->u.ar.start[i]->expr_type != EXPR_CONSTANT
		  || ref->u.ar.as->upper[i] == NULL
		  || ref->u.ar.as->lower[i] == NULL
		  || ref->u.ar.as->upper[i]->expr_type != EXPR_CONSTANT
		  || ref->u.ar.as->lower[i]->expr_type != EXPR_CONSTANT
		  || ref->u.ar.as->upper[i]->ts.type != BT_INTEGER
		  || ref->u.ar.as->lower[i]->ts.type != BT_INTEGER)
		return 0;

	      elements
		   = elements
		     * (mpz_get_si (ref->u.ar.as->upper[i]->value.integer)
			- mpz_get_si (ref->u.ar.as->lower[i]->value.integer)
			+ 1L)
		     - (mpz_get_si (ref->u.ar.start[i]->value.integer)
			- mpz_get_si (ref->u.ar.as->lower[i]->value.integer));
	    }
        }
      else if (ref->type == REF_COMPONENT && ref->u.c.component->attr.function
	       && ref->u.c.component->attr.proc_pointer
	       && ref->u.c.component->attr.dimension)
	{
	  /* Array-valued procedure-pointer components.  */
	  gfc_array_spec *as = ref->u.c.component->as;
	  for (i = 0; i < as->rank; i++)
	    {
	      if (!as->upper[i] || !as->lower[i]
		  || as->upper[i]->expr_type != EXPR_CONSTANT
		  || as->lower[i]->expr_type != EXPR_CONSTANT
		  || as->upper[i]->ts.type != BT_INTEGER
		  || as->lower[i]->ts.type != BT_INTEGER)
		return 0;

	      elements = elements
			 * (mpz_get_si (as->upper[i]->value.integer)
			    - mpz_get_si (as->lower[i]->value.integer) + 1L);
	    }
	}
    }

  if (substrlen)
    return (is_str_storage) ? substrlen + (elements-1)*strlen
			    : elements*strlen;
  else
    return elements*strlen;
}


/* Given an expression, check whether it is an array section
   which has a vector subscript.  */

bool
gfc_has_vector_subscript (gfc_expr *e)
{
  int i;
  gfc_ref *ref;

  if (e == NULL || e->rank == 0 || e->expr_type != EXPR_VARIABLE)
    return false;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.type == AR_SECTION)
      for (i = 0; i < ref->u.ar.dimen; i++)
	if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
	  return true;

  return false;
}


static bool
is_procptr_result (gfc_expr *expr)
{
  gfc_component *c = gfc_get_proc_ptr_comp (expr);
  if (c)
    return (c->ts.interface && (c->ts.interface->attr.proc_pointer == 1));
  else
    return ((expr->symtree->n.sym->result != expr->symtree->n.sym)
	    && (expr->symtree->n.sym->result->attr.proc_pointer == 1));
}


/* Recursively append candidate argument ARG to CANDIDATES.  Store the
   number of total candidates in CANDIDATES_LEN.  */

static void
lookup_arg_fuzzy_find_candidates (gfc_formal_arglist *arg,
				  char **&candidates,
				  size_t &candidates_len)
{
  for (gfc_formal_arglist *p = arg; p && p->sym; p = p->next)
    vec_push (candidates, candidates_len, p->sym->name);
}


/* Lookup argument ARG fuzzily, taking names in ARGUMENTS into account.  */

static const char*
lookup_arg_fuzzy (const char *arg, gfc_formal_arglist *arguments)
{
  char **candidates = NULL;
  size_t candidates_len = 0;
  lookup_arg_fuzzy_find_candidates (arguments, candidates, candidates_len);
  return gfc_closest_fuzzy_match (arg, candidates);
}


static gfc_dummy_arg *
get_nonintrinsic_dummy_arg (gfc_formal_arglist *formal)
{
  gfc_dummy_arg * const dummy_arg = gfc_get_dummy_arg ();

  dummy_arg->intrinsicness = GFC_NON_INTRINSIC_DUMMY_ARG;
  dummy_arg->u.non_intrinsic = formal;

  return dummy_arg;
}


/* Given formal and actual argument lists, see if they are compatible.
   If they are compatible, the actual argument list is sorted to
   correspond with the formal list, and elements for missing optional
   arguments are inserted. If WHERE pointer is nonnull, then we issue
   errors when things don't match instead of just returning the status
   code.  */

bool
gfc_compare_actual_formal (gfc_actual_arglist **ap, gfc_formal_arglist *formal,
			   int ranks_must_agree, int is_elemental,
			   bool in_statement_function, locus *where)
{
  gfc_actual_arglist **new_arg, *a, *actual;
  gfc_formal_arglist *f;
  int i, n, na;
  unsigned long actual_size, formal_size;
  bool full_array = false;
  gfc_array_ref *actual_arr_ref;
  gfc_array_spec *fas, *aas;
  bool pointer_dummy, pointer_arg, allocatable_arg;
  bool procptr_dummy, optional_dummy, allocatable_dummy;

  bool ok = true;

  actual = *ap;

  if (actual == NULL && formal == NULL)
    return true;

  n = 0;
  for (f = formal; f; f = f->next)
    n++;

  new_arg = XALLOCAVEC (gfc_actual_arglist *, n);

  for (i = 0; i < n; i++)
    new_arg[i] = NULL;

  na = 0;
  f = formal;
  i = 0;

  for (a = actual; a; a = a->next, f = f->next)
    {
      if (a->name != NULL && in_statement_function)
	{
	  gfc_error ("Keyword argument %qs at %L is invalid in "
		     "a statement function", a->name, &a->expr->where);
	  return false;
	}

      /* Look for keywords but ignore g77 extensions like %VAL.  */
      if (a->name != NULL && a->name[0] != '%')
	{
	  i = 0;
	  for (f = formal; f; f = f->next, i++)
	    {
	      if (f->sym == NULL)
		continue;
	      if (strcmp (f->sym->name, a->name) == 0)
		break;
	    }

	  if (f == NULL)
	    {
	      if (where)
		{
		  const char *guessed = lookup_arg_fuzzy (a->name, formal);
		  if (guessed)
		    gfc_error ("Keyword argument %qs at %L is not in "
			       "the procedure; did you mean %qs?",
			       a->name, &a->expr->where, guessed);
		  else
		    gfc_error ("Keyword argument %qs at %L is not in "
			       "the procedure", a->name, &a->expr->where);
		}
	      return false;
	    }

	  if (new_arg[i] != NULL)
	    {
	      if (where)
		gfc_error ("Keyword argument %qs at %L is already associated "
			   "with another actual argument", a->name,
			   &a->expr->where);
	      return false;
	    }
	}

      if (f == NULL)
	{
	  if (where)
	    gfc_error ("More actual than formal arguments in procedure "
		       "call at %L", where);
	  return false;
	}

      if (f->sym == NULL && a->expr == NULL)
	goto match;

      if (f->sym == NULL)
	{
	  /* These errors have to be issued, otherwise an ICE can occur.
	     See PR 78865.  */
	  if (where)
	    gfc_error_now ("Missing alternate return specifier in subroutine "
			   "call at %L", where);
	  return false;
	}
      else
	a->associated_dummy = get_nonintrinsic_dummy_arg (f);

      if (a->expr == NULL)
	{
	  if (f->sym->attr.optional)
	    continue;
	  else
	    {
	      if (where)
		gfc_error_now ("Unexpected alternate return specifier in "
			       "subroutine call at %L", where);
	      return false;
	    }
	}

      /* Make sure that intrinsic vtables exist for calls to unlimited
	 polymorphic formal arguments.  */
      if (UNLIMITED_POLY (f->sym)
	  && a->expr->ts.type != BT_DERIVED
	  && a->expr->ts.type != BT_CLASS
	  && a->expr->ts.type != BT_ASSUMED)
	gfc_find_vtab (&a->expr->ts);

      /* Interp J3/22-146:
	 "If the context of the reference to NULL is an <actual argument>
	 corresponding to an <assumed-rank> dummy argument, MOLD shall be
	 present."  */
      if (a->expr->expr_type == EXPR_NULL
	  && a->expr->ts.type == BT_UNKNOWN
	  && f->sym->as
	  && f->sym->as->type == AS_ASSUMED_RANK)
	{
	  gfc_error ("Intrinsic %<NULL()%> without %<MOLD%> argument at %L "
		     "passed to assumed-rank dummy %qs",
		     &a->expr->where, f->sym->name);
	  ok = false;
	  goto match;
	}

      if (warn_surprising
	  && a->expr->expr_type == EXPR_VARIABLE
	  && a->expr->symtree->n.sym->as
	  && a->expr->symtree->n.sym->as->type == AS_ASSUMED_SIZE
	  && f->sym->as
	  && f->sym->as->type == AS_ASSUMED_RANK)
	gfc_warning (0, "The assumed-size dummy %qs is being passed at %L to "
		     "an assumed-rank dummy %qs", a->expr->symtree->name,
		     &a->expr->where, f->sym->name);

      if (a->expr->expr_type == EXPR_NULL
	  && a->expr->ts.type == BT_UNKNOWN
	  && f->sym->ts.type == BT_CHARACTER
	  && !f->sym->ts.deferred
	  && f->sym->ts.u.cl
	  && f->sym->ts.u.cl->length == NULL)
	{
	  gfc_error ("Intrinsic %<NULL()%> without %<MOLD%> argument at %L "
		     "passed to assumed-length dummy %qs",
		     &a->expr->where, f->sym->name);
	  ok = false;
	  goto match;
	}

      /* Allow passing of NULL() as disassociated pointer, procedure
	 pointer, or unallocated allocatable (F2008+) to a respective dummy
	 argument.  */
      pointer_dummy = ((f->sym->ts.type != BT_CLASS
			&& f->sym->attr.pointer)
		       || (f->sym->ts.type == BT_CLASS
			   && CLASS_DATA (f->sym)->attr.class_pointer));

      procptr_dummy = ((f->sym->ts.type != BT_CLASS
			&& f->sym->attr.proc_pointer)
		       || (f->sym->ts.type == BT_CLASS
			   && CLASS_DATA (f->sym)->attr.proc_pointer));

      optional_dummy = f->sym->attr.optional;

      allocatable_dummy = ((f->sym->ts.type != BT_CLASS
			    && f->sym->attr.allocatable)
			   || (f->sym->ts.type == BT_CLASS
			       && CLASS_DATA (f->sym)->attr.allocatable));

      if (a->expr->expr_type == EXPR_NULL
	  && !pointer_dummy
	  && !procptr_dummy
	  && !(optional_dummy
	       && (gfc_option.allow_std & GFC_STD_F2008) != 0)
	  && !(allocatable_dummy
	       && (gfc_option.allow_std & GFC_STD_F2008) != 0))
	{
	  if (where
	      && (!f->sym->attr.optional
		  || (f->sym->ts.type != BT_CLASS && f->sym->attr.allocatable)
		  || (f->sym->ts.type == BT_CLASS
			 && CLASS_DATA (f->sym)->attr.allocatable)))
	    gfc_error ("Unexpected NULL() intrinsic at %L to dummy %qs",
		       where, f->sym->name);
	  else if (where)
	    gfc_error ("Fortran 2008: Null pointer at %L to non-pointer "
		       "dummy %qs", where, f->sym->name);
	  ok = false;
	  goto match;
	}

      if (!compare_parameter (f->sym, a->expr, ranks_must_agree,
			      is_elemental, where))
	{
	  ok = false;
	  goto match;
	}

      /* TS 29113, 6.3p2; F2018 15.5.2.4.  */
      if (f->sym->ts.type == BT_ASSUMED
	  && (a->expr->ts.type == BT_DERIVED
	      || (a->expr->ts.type == BT_CLASS && CLASS_DATA (a->expr))))
	{
	  gfc_symbol *derived = (a->expr->ts.type == BT_DERIVED
				 ? a->expr->ts.u.derived
				 : CLASS_DATA (a->expr)->ts.u.derived);
	  gfc_namespace *f2k_derived = derived->f2k_derived;
	  if (derived->attr.pdt_type
	      || (f2k_derived
		  && (f2k_derived->finalizers || f2k_derived->tb_sym_root)))
	    {
	      gfc_error ("Actual argument at %L to assumed-type dummy "
			 "has type parameters or is of "
			 "derived type with type-bound or FINAL procedures",
			 &a->expr->where);
	      ok = false;
	      goto match;
	    }
	}

      if (UNLIMITED_POLY (a->expr)
	  && !(f->sym->ts.type == BT_ASSUMED || UNLIMITED_POLY (f->sym)))
	{
	  gfc_error ("Unlimited polymorphic actual argument at %L is not "
		     "matched with either an unlimited polymorphic or "
		     "assumed type dummy argument", &a->expr->where);
	  ok = false;
	  goto match;
	}

      /* Special case for character arguments.  For allocatable, pointer
	 and assumed-shape dummies, the string length needs to match
	 exactly.  */
      if (a->expr->ts.type == BT_CHARACTER
	  && a->expr->ts.u.cl && a->expr->ts.u.cl->length
	  && a->expr->ts.u.cl->length->expr_type == EXPR_CONSTANT
	  && a->expr->ts.u.cl->length->ts.type == BT_INTEGER
	  && f->sym->ts.type == BT_CHARACTER && f->sym->ts.u.cl
	  && f->sym->ts.u.cl->length
	  && f->sym->ts.u.cl->length->expr_type == EXPR_CONSTANT
	  && f->sym->ts.u.cl->length->ts.type == BT_INTEGER
	  && (f->sym->attr.pointer || f->sym->attr.allocatable
	      || (f->sym->as && f->sym->as->type == AS_ASSUMED_SHAPE))
	  && (mpz_cmp (a->expr->ts.u.cl->length->value.integer,
		       f->sym->ts.u.cl->length->value.integer) != 0))
	{
	  if (where && (f->sym->attr.pointer || f->sym->attr.allocatable))
	    gfc_warning (0, "Character length mismatch (%ld/%ld) between actual "
			 "argument and pointer or allocatable dummy argument "
			 "%qs at %L",
			 mpz_get_si (a->expr->ts.u.cl->length->value.integer),
			 mpz_get_si (f->sym->ts.u.cl->length->value.integer),
			 f->sym->name, &a->expr->where);
	  else if (where)
	    gfc_warning (0, "Character length mismatch (%ld/%ld) between actual "
			 "argument and assumed-shape dummy argument %qs "
			 "at %L",
			 mpz_get_si (a->expr->ts.u.cl->length->value.integer),
			 mpz_get_si (f->sym->ts.u.cl->length->value.integer),
			 f->sym->name, &a->expr->where);
	  ok = false;
	  goto match;
	}

      if ((f->sym->attr.pointer || f->sym->attr.allocatable)
	  && f->sym->ts.deferred != a->expr->ts.deferred
	  && a->expr->ts.type == BT_CHARACTER)
	{
	  if (where)
	    gfc_error ("Actual argument at %L to allocatable or "
		       "pointer dummy argument %qs must have a deferred "
		       "length type parameter if and only if the dummy has one",
		       &a->expr->where, f->sym->name);
	  ok = false;
	  goto match;
	}

      if (f->sym->ts.type == BT_CLASS)
	goto skip_size_check;

      /* Skip size check for NULL() actual without MOLD argument.  */
      if (a->expr->expr_type == EXPR_NULL && a->expr->ts.type == BT_UNKNOWN)
	goto skip_size_check;

      actual_size = get_expr_storage_size (a->expr);
      formal_size = get_sym_storage_size (f->sym);
      if (actual_size != 0 && actual_size < formal_size
	  && a->expr->ts.type != BT_PROCEDURE
	  && f->sym->attr.flavor != FL_PROCEDURE)
	{
	  if (a->expr->ts.type == BT_CHARACTER && !f->sym->as && where)
	    {
	      gfc_warning (0, "Character length of actual argument shorter "
			   "than of dummy argument %qs (%lu/%lu) at %L",
			   f->sym->name, actual_size, formal_size,
			   &a->expr->where);
	      goto skip_size_check;
	    }
          else if (where)
	    {
	      /* Emit a warning for -std=legacy and an error otherwise. */
	      if (gfc_option.warn_std == 0)
	        gfc_warning (0, "Actual argument contains too few "
			     "elements for dummy argument %qs (%lu/%lu) "
			     "at %L", f->sym->name, actual_size,
			     formal_size, &a->expr->where);
	      else
	        gfc_error_now ("Actual argument contains too few "
			       "elements for dummy argument %qs (%lu/%lu) "
			       "at %L", f->sym->name, actual_size,
			       formal_size, &a->expr->where);
	    }
	  ok = false;
	  goto match;
	}

     skip_size_check:

      /* Satisfy either: F03:12.4.1.3 by ensuring that a procedure pointer
	 actual argument is provided for a procedure pointer formal argument;
	 or: F08:12.5.2.9 (F18:15.5.2.10) by ensuring that the effective
	 argument shall be an external, internal, module, or dummy procedure.
	 The interfaces are checked elsewhere.  */
      if (f->sym->attr.proc_pointer
	  && !((a->expr->expr_type == EXPR_VARIABLE
		&& (a->expr->symtree->n.sym->attr.proc_pointer
		    || gfc_is_proc_ptr_comp (a->expr)))
	       || (a->expr->ts.type == BT_PROCEDURE
		   && f->sym->ts.interface)
	       || (a->expr->expr_type == EXPR_FUNCTION
		   && is_procptr_result (a->expr))))
	{
	  if (where)
	    gfc_error ("Expected a procedure pointer for argument %qs at %L",
		       f->sym->name, &a->expr->where);
	  ok = false;
	  goto match;
	}

      /* Satisfy F03:12.4.1.3 by ensuring that a procedure actual argument is
	 provided for a procedure formal argument.  */
      if (f->sym->attr.flavor == FL_PROCEDURE
	  && !((a->expr->expr_type == EXPR_VARIABLE
		&& (a->expr->symtree->n.sym->attr.flavor == FL_PROCEDURE
		    || a->expr->symtree->n.sym->attr.proc_pointer
		    || gfc_is_proc_ptr_comp (a->expr)))
	       || (a->expr->expr_type == EXPR_FUNCTION
		   && is_procptr_result (a->expr))))
	{
	  if (where)
	    gfc_error ("Expected a procedure for argument %qs at %L",
		       f->sym->name, &a->expr->where);
	  ok = false;
	  goto match;
	}

      /* Class array variables and expressions store array info in a
	 different place from non-class objects; consolidate the logic
	 to access it here instead of repeating it below.  Note that
	 pointer_arg and allocatable_arg are not fully general and are
	 only used in a specific situation below with an assumed-rank
	 argument.  */
      if (f->sym->ts.type == BT_CLASS && CLASS_DATA (f->sym))
	{
	  gfc_component *classdata = CLASS_DATA (f->sym);
	  fas = classdata->as;
	  pointer_dummy = classdata->attr.class_pointer;
	}
      else
	{
	  fas = f->sym->as;
	  pointer_dummy = f->sym->attr.pointer;
	}

      if (a->expr->expr_type != EXPR_VARIABLE
	  && !(a->expr->expr_type == EXPR_NULL
	       && a->expr->ts.type != BT_UNKNOWN))
	{
	  aas = NULL;
	  pointer_arg = false;
	  allocatable_arg = false;
	}
      else if (a->expr->ts.type == BT_CLASS
	       && a->expr->symtree->n.sym
	       && CLASS_DATA (a->expr->symtree->n.sym))
	{
	  gfc_component *classdata = CLASS_DATA (a->expr->symtree->n.sym);
	  aas = classdata->as;
	  pointer_arg = classdata->attr.class_pointer;
	  allocatable_arg = classdata->attr.allocatable;
	}
      else
	{
	  aas = a->expr->symtree->n.sym->as;
	  pointer_arg = a->expr->symtree->n.sym->attr.pointer;
	  allocatable_arg = a->expr->symtree->n.sym->attr.allocatable;
	}

      /* F2018:9.5.2(2) permits assumed-size whole array expressions as
	 actual arguments only if the shape is not required; thus it
	 cannot be passed to an assumed-shape array dummy.
	 F2018:15.5.2.(2) permits passing a nonpointer actual to an
	 intent(in) pointer dummy argument and this is accepted by
	 the compare_pointer check below, but this also requires shape
	 information.
	 There's more discussion of this in PR94110.  */
      if (fas
	  && (fas->type == AS_ASSUMED_SHAPE
	      || fas->type == AS_DEFERRED
	      || (fas->type == AS_ASSUMED_RANK && pointer_dummy))
	  && aas
	  && aas->type == AS_ASSUMED_SIZE
	  && (a->expr->ref == NULL
	      || (a->expr->ref->type == REF_ARRAY
		  && a->expr->ref->u.ar.type == AR_FULL)))
	{
	  if (where)
	    gfc_error ("Actual argument for %qs cannot be an assumed-size"
		       " array at %L", f->sym->name, where);
	  ok = false;
	  goto match;
	}

      /* Diagnose F2018 C839 (TS29113 C535c).  Here the problem is
	 passing an assumed-size array to an INTENT(OUT) assumed-rank
	 dummy when it doesn't have the size information needed to run
	 initializers and finalizers.  */
      if (f->sym->attr.intent == INTENT_OUT
	  && fas
	  && fas->type == AS_ASSUMED_RANK
	  && aas
	  && ((aas->type == AS_ASSUMED_SIZE
	       && (a->expr->ref == NULL
		   || (a->expr->ref->type == REF_ARRAY
		       && a->expr->ref->u.ar.type == AR_FULL)))
	      || (aas->type == AS_ASSUMED_RANK
		  && !pointer_arg
		  && !allocatable_arg))
	  && (a->expr->ts.type == BT_CLASS
	      || (a->expr->ts.type == BT_DERIVED
		  && (gfc_is_finalizable (a->expr->ts.u.derived, NULL)
		      || gfc_has_ultimate_allocatable (a->expr)
		      || gfc_has_default_initializer
			   (a->expr->ts.u.derived)))))
	{
	  if (where)
	    gfc_error ("Actual argument to assumed-rank INTENT(OUT) "
		       "dummy %qs at %L cannot be of unknown size",
		       f->sym->name, where);
	  ok = false;
	  goto match;
	}

      if (a->expr->expr_type != EXPR_NULL)
	{
	  int cmp = compare_pointer (f->sym, a->expr);
	  bool pre2008 = ((gfc_option.allow_std & GFC_STD_F2008) == 0);

	  if (pre2008 && cmp == 0)
	    {
	      if (where)
		gfc_error ("Actual argument for %qs at %L must be a pointer",
			   f->sym->name, &a->expr->where);
	      ok = false;
	      goto match;
	    }

	  if (pre2008 && cmp == 2)
	    {
	      if (where)
		gfc_error ("Fortran 2008: Non-pointer actual argument at %L to "
			   "pointer dummy %qs", &a->expr->where, f->sym->name);
	      ok = false;
	      goto match;
	    }

	  if (!pre2008 && cmp == 0)
	    {
	      if (where)
		gfc_error ("Actual argument for %qs at %L must be a pointer "
			   "or a valid target for the dummy pointer in a "
			   "pointer assignment statement",
			   f->sym->name, &a->expr->where);
	      ok = false;
	      goto match;
	    }
	}


      /* Fortran 2008, C1242.  */
      if (f->sym->attr.pointer && gfc_is_coindexed (a->expr))
	{
	  if (where)
	    gfc_error ("Coindexed actual argument at %L to pointer "
		       "dummy %qs",
		       &a->expr->where, f->sym->name);
	  ok = false;
	  goto match;
	}

      /* Fortran 2008, 12.5.2.5 (no constraint).  */
      if (a->expr->expr_type == EXPR_VARIABLE
	  && f->sym->attr.intent != INTENT_IN
	  && f->sym->attr.allocatable
	  && gfc_is_coindexed (a->expr))
	{
	  if (where)
	    gfc_error ("Coindexed actual argument at %L to allocatable "
		       "dummy %qs requires INTENT(IN)",
		       &a->expr->where, f->sym->name);
	  ok = false;
	  goto match;
	}

      /* Fortran 2008, C1237.  */
      if (a->expr->expr_type == EXPR_VARIABLE
	  && (f->sym->attr.asynchronous || f->sym->attr.volatile_)
	  && gfc_is_coindexed (a->expr)
	  && (a->expr->symtree->n.sym->attr.volatile_
	      || a->expr->symtree->n.sym->attr.asynchronous))
	{
	  if (where)
	    gfc_error ("Coindexed ASYNCHRONOUS or VOLATILE actual argument at "
		       "%L requires that dummy %qs has neither "
		       "ASYNCHRONOUS nor VOLATILE", &a->expr->where,
		       f->sym->name);
	  ok = false;
	  goto match;
	}

      /* Fortran 2008, 12.5.2.4 (no constraint).  */
      if (a->expr->expr_type == EXPR_VARIABLE
	  && f->sym->attr.intent != INTENT_IN && !f->sym->attr.value
	  && gfc_is_coindexed (a->expr)
	  && gfc_has_ultimate_allocatable (a->expr))
	{
	  if (where)
	    gfc_error ("Coindexed actual argument at %L with allocatable "
		       "ultimate component to dummy %qs requires either VALUE "
		       "or INTENT(IN)", &a->expr->where, f->sym->name);
	  ok = false;
	  goto match;
	}

     if (f->sym->ts.type == BT_CLASS
	   && CLASS_DATA (f->sym)->attr.allocatable
	   && gfc_is_class_array_ref (a->expr, &full_array)
	   && !full_array)
	{
	  if (where)
	    gfc_error ("Actual CLASS array argument for %qs must be a full "
		       "array at %L", f->sym->name, &a->expr->where);
	  ok = false;
	  goto match;
	}


      if (a->expr->expr_type != EXPR_NULL
	  && !compare_allocatable (f->sym, a->expr))
	{
	  if (where)
	    gfc_error ("Actual argument for %qs must be ALLOCATABLE at %L",
		       f->sym->name, &a->expr->where);
	  ok = false;
	  goto match;
	}

      if (a->expr->expr_type == EXPR_FUNCTION
	  && a->expr->value.function.esym
	  && f->sym->attr.allocatable)
	{
	  if (where)
	    gfc_error ("Actual argument for %qs at %L is a function result "
		       "and the dummy argument is ALLOCATABLE",
		       f->sym->name, &a->expr->where);
	  ok = false;
	  goto match;
	}

      /* Check intent = OUT/INOUT for definable actual argument.  */
      if (!in_statement_function
	  && (f->sym->attr.intent == INTENT_OUT
	      || f->sym->attr.intent == INTENT_INOUT))
	{
	  const char* context = (where
				 ? _("actual argument to INTENT = OUT/INOUT")
				 : NULL);

	  if (((f->sym->ts.type == BT_CLASS && f->sym->attr.class_ok
		&& CLASS_DATA (f->sym)->attr.class_pointer)
	       || (f->sym->ts.type != BT_CLASS && f->sym->attr.pointer))
	      && !gfc_check_vardef_context (a->expr, true, false, false, context))
	    {
	      ok = false;
	      goto match;
	    }
	  if (!gfc_check_vardef_context (a->expr, false, false, false, context))
	    {
	      ok = false;
	      goto match;
	    }
	}

      if ((f->sym->attr.intent == INTENT_OUT
	   || f->sym->attr.intent == INTENT_INOUT
	   || f->sym->attr.volatile_
	   || f->sym->attr.asynchronous)
	  && gfc_has_vector_subscript (a->expr))
	{
	  if (where)
	    gfc_error ("Array-section actual argument with vector "
		       "subscripts at %L is incompatible with INTENT(OUT), "
		       "INTENT(INOUT), VOLATILE or ASYNCHRONOUS attribute "
		       "of the dummy argument %qs",
		       &a->expr->where, f->sym->name);
	  ok = false;
	  goto match;
	}

      /* C1232 (R1221) For an actual argument which is an array section or
	 an assumed-shape array, the dummy argument shall be an assumed-
	 shape array, if the dummy argument has the VOLATILE attribute.  */

      if (f->sym->attr.volatile_
	  && a->expr->expr_type == EXPR_VARIABLE
	  && a->expr->symtree->n.sym->as
	  && a->expr->symtree->n.sym->as->type == AS_ASSUMED_SHAPE
	  && !(fas && fas->type == AS_ASSUMED_SHAPE))
	{
	  if (where)
	    gfc_error ("Assumed-shape actual argument at %L is "
		       "incompatible with the non-assumed-shape "
		       "dummy argument %qs due to VOLATILE attribute",
		       &a->expr->where,f->sym->name);
	  ok = false;
	  goto match;
	}

      /* Find the last array_ref.  */
      actual_arr_ref = NULL;
      if (a->expr->ref)
	actual_arr_ref = gfc_find_array_ref (a->expr, true);

      if (f->sym->attr.volatile_
	  && actual_arr_ref && actual_arr_ref->type == AR_SECTION
	  && !(fas && fas->type == AS_ASSUMED_SHAPE))
	{
	  if (where)
	    gfc_error ("Array-section actual argument at %L is "
		       "incompatible with the non-assumed-shape "
		       "dummy argument %qs due to VOLATILE attribute",
		       &a->expr->where, f->sym->name);
	  ok = false;
	  goto match;
	}

      /* C1233 (R1221) For an actual argument which is a pointer array, the
	 dummy argument shall be an assumed-shape or pointer array, if the
	 dummy argument has the VOLATILE attribute.  */

      if (f->sym->attr.volatile_
	  && a->expr->expr_type == EXPR_VARIABLE
	  && a->expr->symtree->n.sym->attr.pointer
	  && a->expr->symtree->n.sym->as
	  && !(fas
	       && (fas->type == AS_ASSUMED_SHAPE
		   || f->sym->attr.pointer)))
	{
	  if (where)
	    gfc_error ("Pointer-array actual argument at %L requires "
		       "an assumed-shape or pointer-array dummy "
		       "argument %qs due to VOLATILE attribute",
		       &a->expr->where,f->sym->name);
	  ok = false;
	  goto match;
	}

    match:
      if (a == actual)
	na = i;

      new_arg[i++] = a;
    }

  /* Give up now if we saw any bad argument.  */
  if (!ok)
    return false;

  /* Make sure missing actual arguments are optional.  */
  i = 0;
  for (f = formal; f; f = f->next, i++)
    {
      if (new_arg[i] != NULL)
	continue;
      if (f->sym == NULL)
	{
	  if (where)
	    gfc_error ("Missing alternate return spec in subroutine call "
		       "at %L", where);
	  return false;
	}
      /* For CLASS, the optional attribute might be set at either location. */
      if (((f->sym->ts.type != BT_CLASS || !CLASS_DATA (f->sym)->attr.optional)
	   && !f->sym->attr.optional)
	  || (in_statement_function
	      && (f->sym->attr.optional
		  || (f->sym->ts.type == BT_CLASS
		      && CLASS_DATA (f->sym)->attr.optional))))
	{
	  if (where)
	    gfc_error ("Missing actual argument for argument %qs at %L",
		       f->sym->name, where);
	  return false;
	}
    }

  /* We should have handled the cases where the formal arglist is null
     already.  */
  gcc_assert (n > 0);

  /* The argument lists are compatible.  We now relink a new actual
     argument list with null arguments in the right places.  The head
     of the list remains the head.  */
  for (f = formal, i = 0; f; f = f->next, i++)
    if (new_arg[i] == NULL)
      {
	new_arg[i] = gfc_get_actual_arglist ();
	new_arg[i]->associated_dummy = get_nonintrinsic_dummy_arg (f);
      }

  if (na != 0)
    {
      std::swap (*new_arg[0], *actual);
      std::swap (new_arg[0], new_arg[na]);
    }

  for (i = 0; i < n - 1; i++)
    new_arg[i]->next = new_arg[i + 1];

  new_arg[i]->next = NULL;

  if (*ap == NULL && n > 0)
    *ap = new_arg[0];

  return true;
}


typedef struct
{
  gfc_formal_arglist *f;
  gfc_actual_arglist *a;
}
argpair;

/* qsort comparison function for argument pairs, with the following
   order:
    - p->a->expr == NULL
    - p->a->expr->expr_type != EXPR_VARIABLE
    - by gfc_symbol pointer value (larger first).  */

static int
pair_cmp (const void *p1, const void *p2)
{
  const gfc_actual_arglist *a1, *a2;

  /* *p1 and *p2 are elements of the to-be-sorted array.  */
  a1 = ((const argpair *) p1)->a;
  a2 = ((const argpair *) p2)->a;
  if (!a1->expr)
    {
      if (!a2->expr)
	return 0;
      return -1;
    }
  if (!a2->expr)
    return 1;
  if (a1->expr->expr_type != EXPR_VARIABLE)
    {
      if (a2->expr->expr_type != EXPR_VARIABLE)
	return 0;
      return -1;
    }
  if (a2->expr->expr_type != EXPR_VARIABLE)
    return 1;
  if (a1->expr->symtree->n.sym > a2->expr->symtree->n.sym)
    return -1;
  return a1->expr->symtree->n.sym < a2->expr->symtree->n.sym;
}


/* Given two expressions from some actual arguments, test whether they
   refer to the same expression. The analysis is conservative.
   Returning false will produce no warning.  */

static bool
compare_actual_expr (gfc_expr *e1, gfc_expr *e2)
{
  const gfc_ref *r1, *r2;

  if (!e1 || !e2
      || e1->expr_type != EXPR_VARIABLE
      || e2->expr_type != EXPR_VARIABLE
      || e1->symtree->n.sym != e2->symtree->n.sym)
    return false;

  /* TODO: improve comparison, see expr.cc:show_ref().  */
  for (r1 = e1->ref, r2 = e2->ref; r1 && r2; r1 = r1->next, r2 = r2->next)
    {
      if (r1->type != r2->type)
	return false;
      switch (r1->type)
	{
	case REF_ARRAY:
	  if (r1->u.ar.type != r2->u.ar.type)
	    return false;
	  /* TODO: At the moment, consider only full arrays;
	     we could do better.  */
	  if (r1->u.ar.type != AR_FULL || r2->u.ar.type != AR_FULL)
	    return false;
	  break;

	case REF_COMPONENT:
	  if (r1->u.c.component != r2->u.c.component)
	    return false;
	  break;

	case REF_SUBSTRING:
	  return false;

	case REF_INQUIRY:
	  if (e1->symtree->n.sym->ts.type == BT_COMPLEX
	      && e1->ts.type == BT_REAL && e2->ts.type == BT_REAL
	      && r1->u.i != r2->u.i)
	    return false;
	  break;

	default:
	  gfc_internal_error ("compare_actual_expr(): Bad component code");
	}
    }
  if (!r1 && !r2)
    return true;
  return false;
}


/* Given formal and actual argument lists that correspond to one
   another, check that identical actual arguments aren't not
   associated with some incompatible INTENTs.  */

static bool
check_some_aliasing (gfc_formal_arglist *f, gfc_actual_arglist *a)
{
  sym_intent f1_intent, f2_intent;
  gfc_formal_arglist *f1;
  gfc_actual_arglist *a1;
  size_t n, i, j;
  argpair *p;
  bool t = true;

  n = 0;
  for (f1 = f, a1 = a;; f1 = f1->next, a1 = a1->next)
    {
      if (f1 == NULL && a1 == NULL)
	break;
      if (f1 == NULL || a1 == NULL)
	gfc_internal_error ("check_some_aliasing(): List mismatch");
      n++;
    }
  if (n == 0)
    return t;
  p = XALLOCAVEC (argpair, n);

  for (i = 0, f1 = f, a1 = a; i < n; i++, f1 = f1->next, a1 = a1->next)
    {
      p[i].f = f1;
      p[i].a = a1;
    }

  qsort (p, n, sizeof (argpair), pair_cmp);

  for (i = 0; i < n; i++)
    {
      if (!p[i].a->expr
	  || p[i].a->expr->expr_type != EXPR_VARIABLE
	  || p[i].a->expr->ts.type == BT_PROCEDURE)
	continue;
      f1_intent = p[i].f->sym->attr.intent;
      for (j = i + 1; j < n; j++)
	{
	  /* Expected order after the sort.  */
	  if (!p[j].a->expr || p[j].a->expr->expr_type != EXPR_VARIABLE)
	    gfc_internal_error ("check_some_aliasing(): corrupted data");

	  /* Are the expression the same?  */
	  if (!compare_actual_expr (p[i].a->expr, p[j].a->expr))
	    break;
	  f2_intent = p[j].f->sym->attr.intent;
	  if ((f1_intent == INTENT_IN && f2_intent == INTENT_OUT)
	      || (f1_intent == INTENT_OUT && f2_intent == INTENT_IN)
	      || (f1_intent == INTENT_OUT && f2_intent == INTENT_OUT))
	    {
	      gfc_warning (0, "Same actual argument associated with INTENT(%s) "
			   "argument %qs and INTENT(%s) argument %qs at %L",
			   gfc_intent_string (f1_intent), p[i].f->sym->name,
			   gfc_intent_string (f2_intent), p[j].f->sym->name,
			   &p[i].a->expr->where);
	      t = false;
	    }
	}
    }

  return t;
}


/* Given formal and actual argument lists that correspond to one
   another, check that they are compatible in the sense that intents
   are not mismatched.  */

static bool
check_intents (gfc_formal_arglist *f, gfc_actual_arglist *a)
{
  sym_intent f_intent;

  for (;; f = f->next, a = a->next)
    {
      gfc_expr *expr;

      if (f == NULL && a == NULL)
	break;
      if (f == NULL || a == NULL)
	gfc_internal_error ("check_intents(): List mismatch");

      if (a->expr && a->expr->expr_type == EXPR_FUNCTION
	  && a->expr->value.function.isym
	  && a->expr->value.function.isym->id == GFC_ISYM_CAF_GET)
	expr = a->expr->value.function.actual->expr;
      else
	expr = a->expr;

      if (expr == NULL || expr->expr_type != EXPR_VARIABLE)
	continue;

      f_intent = f->sym->attr.intent;

      if (gfc_pure (NULL) && gfc_impure_variable (expr->symtree->n.sym))
	{
	  if ((f->sym->ts.type == BT_CLASS && f->sym->attr.class_ok
	       && CLASS_DATA (f->sym)->attr.class_pointer)
	      || (f->sym->ts.type != BT_CLASS && f->sym->attr.pointer))
	    {
	      gfc_error ("Procedure argument at %L is local to a PURE "
			 "procedure and has the POINTER attribute",
			 &expr->where);
	      return false;
	    }
	}

       /* Fortran 2008, C1283.  */
       if (gfc_pure (NULL) && gfc_is_coindexed (expr))
	{
	  if (f_intent == INTENT_INOUT || f_intent == INTENT_OUT)
	    {
	      gfc_error ("Coindexed actual argument at %L in PURE procedure "
			 "is passed to an INTENT(%s) argument",
			 &expr->where, gfc_intent_string (f_intent));
	      return false;
	    }

	  if ((f->sym->ts.type == BT_CLASS && f->sym->attr.class_ok
               && CLASS_DATA (f->sym)->attr.class_pointer)
              || (f->sym->ts.type != BT_CLASS && f->sym->attr.pointer))
	    {
	      gfc_error ("Coindexed actual argument at %L in PURE procedure "
			 "is passed to a POINTER dummy argument",
			 &expr->where);
	      return false;
	    }
	}

       /* F2008, Section 12.5.2.4.  */
       if (expr->ts.type == BT_CLASS && f->sym->ts.type == BT_CLASS
	   && gfc_is_coindexed (expr))
	 {
	   gfc_error ("Coindexed polymorphic actual argument at %L is passed "
		      "polymorphic dummy argument %qs",
			 &expr->where, f->sym->name);
	   return false;
	 }
    }

  return true;
}


/* Check how a procedure is used against its interface.  If all goes
   well, the actual argument list will also end up being properly
   sorted.  */

bool
gfc_procedure_use (gfc_symbol *sym, gfc_actual_arglist **ap, locus *where)
{
  gfc_actual_arglist *a;
  gfc_formal_arglist *dummy_args;
  bool implicit = false;

  /* Warn about calls with an implicit interface.  Special case
     for calling a ISO_C_BINDING because c_loc and c_funloc
     are pseudo-unknown.  Additionally, warn about procedures not
     explicitly declared at all if requested.  */
  if (sym->attr.if_source == IFSRC_UNKNOWN && !sym->attr.is_iso_c)
    {
      bool has_implicit_none_export = false;
      implicit = true;
      if (sym->attr.proc == PROC_UNKNOWN)
	for (gfc_namespace *ns = sym->ns; ns; ns = ns->parent)
	  if (ns->has_implicit_none_export)
	    {
	      has_implicit_none_export = true;
	      break;
	    }
      if (has_implicit_none_export)
	{
	  const char *guessed
	    = gfc_lookup_function_fuzzy (sym->name, sym->ns->sym_root);
	  if (guessed)
	    gfc_error ("Procedure %qs called at %L is not explicitly declared"
		       "; did you mean %qs?",
		       sym->name, where, guessed);
	  else
	    gfc_error ("Procedure %qs called at %L is not explicitly declared",
		       sym->name, where);
	  return false;
	}
      if (warn_implicit_interface)
	gfc_warning (OPT_Wimplicit_interface,
		     "Procedure %qs called with an implicit interface at %L",
		     sym->name, where);
      else if (warn_implicit_procedure && sym->attr.proc == PROC_UNKNOWN)
	gfc_warning (OPT_Wimplicit_procedure,
		     "Procedure %qs called at %L is not explicitly declared",
		     sym->name, where);
      gfc_find_proc_namespace (sym->ns)->implicit_interface_calls = 1;
    }

  if (sym->attr.if_source == IFSRC_UNKNOWN)
    {
      if (sym->attr.pointer)
	{
	  gfc_error ("The pointer object %qs at %L must have an explicit "
		     "function interface or be declared as array",
		     sym->name, where);
	  return false;
	}

      if (sym->attr.allocatable && !sym->attr.external)
	{
	  gfc_error ("The allocatable object %qs at %L must have an explicit "
		     "function interface or be declared as array",
		     sym->name, where);
	  return false;
	}

      if (sym->attr.allocatable)
	{
	  gfc_error ("Allocatable function %qs at %L must have an explicit "
		     "function interface", sym->name, where);
	  return false;
	}

      for (a = *ap; a; a = a->next)
	{
	  if (a->expr && a->expr->error)
	    return false;

	  /* F2018, 15.4.2.2 Explicit interface is required for a
	     polymorphic dummy argument, so there is no way to
	     legally have a class appear in an argument with an
	     implicit interface.  */

	  if (implicit && a->expr && a->expr->ts.type == BT_CLASS)
	    {
	      gfc_error ("Explicit interface required for polymorphic "
			 "argument at %L",&a->expr->where);
	      a->expr->error = 1;
	      break;
	    }

	  /* Skip g77 keyword extensions like %VAL, %REF, %LOC.  */
	  if (a->name != NULL && a->name[0] != '%')
	    {
	      gfc_error ("Keyword argument requires explicit interface "
			 "for procedure %qs at %L", sym->name, &a->expr->where);
	      break;
	    }

	  /* TS 29113, 6.2.  */
	  if (a->expr && a->expr->ts.type == BT_ASSUMED
	      && sym->intmod_sym_id != ISOCBINDING_LOC)
	    {
	      gfc_error ("Assumed-type argument %s at %L requires an explicit "
			 "interface", a->expr->symtree->n.sym->name,
			 &a->expr->where);
	      a->expr->error = 1;
	      break;
	    }

	  /* F2008, C1303 and C1304.  */
	  if (a->expr
	      && (a->expr->ts.type == BT_DERIVED || a->expr->ts.type == BT_CLASS)
	      && a->expr->ts.u.derived
	      && ((a->expr->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
		   && a->expr->ts.u.derived->intmod_sym_id == ISOFORTRAN_LOCK_TYPE)
		  || gfc_expr_attr (a->expr).lock_comp))
	    {
	      gfc_error ("Actual argument of LOCK_TYPE or with LOCK_TYPE "
			 "component at %L requires an explicit interface for "
			 "procedure %qs", &a->expr->where, sym->name);
	      a->expr->error = 1;
	      break;
	    }

	  if (a->expr
	      && (a->expr->ts.type == BT_DERIVED || a->expr->ts.type == BT_CLASS)
	      && a->expr->ts.u.derived
	      && ((a->expr->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
		   && a->expr->ts.u.derived->intmod_sym_id
		      == ISOFORTRAN_EVENT_TYPE)
		  || gfc_expr_attr (a->expr).event_comp))
	    {
	      gfc_error ("Actual argument of EVENT_TYPE or with EVENT_TYPE "
			 "component at %L requires an explicit interface for "
			 "procedure %qs", &a->expr->where, sym->name);
	      a->expr->error = 1;
	      break;
	    }

	  if (a->expr && a->expr->expr_type == EXPR_NULL
	      && a->expr->ts.type == BT_UNKNOWN)
	    {
	      gfc_error ("MOLD argument to NULL required at %L",
			 &a->expr->where);
	      a->expr->error = 1;
	      return false;
	    }

	  if (a->expr && a->expr->expr_type == EXPR_NULL)
	    {
	      gfc_error ("Passing intrinsic NULL as actual argument at %L "
			 "requires an explicit interface", &a->expr->where);
	      a->expr->error = 1;
	      return false;
	    }

	  /* TS 29113, C407b.  */
	  if (a->expr && a->expr->expr_type == EXPR_VARIABLE
	      && symbol_rank (a->expr->symtree->n.sym) == -1)
	    {
	      gfc_error ("Assumed-rank argument requires an explicit interface "
			 "at %L", &a->expr->where);
	      a->expr->error = 1;
	      return false;
	    }
	}

      return true;
    }

  dummy_args = gfc_sym_get_dummy_args (sym);

  /* For a statement function, check that types and type parameters of actual
     arguments and dummy arguments match.  */
  if (!gfc_compare_actual_formal (ap, dummy_args, 0, sym->attr.elemental,
				  sym->attr.proc == PROC_ST_FUNCTION, where))
    return false;

  if (!check_intents (dummy_args, *ap))
    return false;

  if (warn_aliasing)
    check_some_aliasing (dummy_args, *ap);

  return true;
}


/* Check how a procedure pointer component is used against its interface.
   If all goes well, the actual argument list will also end up being properly
   sorted. Completely analogous to gfc_procedure_use.  */

void
gfc_ppc_use (gfc_component *comp, gfc_actual_arglist **ap, locus *where)
{
  /* Warn about calls with an implicit interface.  Special case
     for calling a ISO_C_BINDING because c_loc and c_funloc
     are pseudo-unknown.  */
  if (warn_implicit_interface
      && comp->attr.if_source == IFSRC_UNKNOWN
      && !comp->attr.is_iso_c)
    gfc_warning (OPT_Wimplicit_interface,
		 "Procedure pointer component %qs called with an implicit "
		 "interface at %L", comp->name, where);

  if (comp->attr.if_source == IFSRC_UNKNOWN)
    {
      gfc_actual_arglist *a;
      for (a = *ap; a; a = a->next)
	{
	  /* Skip g77 keyword extensions like %VAL, %REF, %LOC.  */
	  if (a->name != NULL && a->name[0] != '%')
	    {
	      gfc_error ("Keyword argument requires explicit interface "
			 "for procedure pointer component %qs at %L",
			 comp->name, &a->expr->where);
	      break;
	    }
	}

      return;
    }

  if (!gfc_compare_actual_formal (ap, comp->ts.interface->formal, 0,
			      comp->attr.elemental, false, where))
    return;

  check_intents (comp->ts.interface->formal, *ap);
  if (warn_aliasing)
    check_some_aliasing (comp->ts.interface->formal, *ap);
}


/* Try if an actual argument list matches the formal list of a symbol,
   respecting the symbol's attributes like ELEMENTAL.  This is used for
   GENERIC resolution.  */

bool
gfc_arglist_matches_symbol (gfc_actual_arglist** args, gfc_symbol* sym)
{
  gfc_formal_arglist *dummy_args;
  bool r;

  if (sym->attr.flavor != FL_PROCEDURE)
    return false;

  dummy_args = gfc_sym_get_dummy_args (sym);

  r = !sym->attr.elemental;
  if (gfc_compare_actual_formal (args, dummy_args, r, !r, false, NULL))
    {
      check_intents (dummy_args, *args);
      if (warn_aliasing)
	check_some_aliasing (dummy_args, *args);
      return true;
    }

  return false;
}


/* Given an interface pointer and an actual argument list, search for
   a formal argument list that matches the actual.  If found, returns
   a pointer to the symbol of the correct interface.  Returns NULL if
   not found.  */

gfc_symbol *
gfc_search_interface (gfc_interface *intr, int sub_flag,
		      gfc_actual_arglist **ap)
{
  gfc_symbol *elem_sym = NULL;
  gfc_symbol *null_sym = NULL;
  locus null_expr_loc;
  gfc_actual_arglist *a;
  bool has_null_arg = false;

  for (a = *ap; a; a = a->next)
    if (a->expr && a->expr->expr_type == EXPR_NULL
	&& a->expr->ts.type == BT_UNKNOWN)
      {
	has_null_arg = true;
	null_expr_loc = a->expr->where;
	break;
      }

  for (; intr; intr = intr->next)
    {
      if (gfc_fl_struct (intr->sym->attr.flavor))
	continue;
      if (sub_flag && intr->sym->attr.function)
	continue;
      if (!sub_flag && intr->sym->attr.subroutine)
	continue;

      if (gfc_arglist_matches_symbol (ap, intr->sym))
	{
	  if (has_null_arg && null_sym)
	    {
	      gfc_error ("MOLD= required in NULL() argument at %L: Ambiguity "
			 "between specific functions %s and %s",
			 &null_expr_loc, null_sym->name, intr->sym->name);
	      return NULL;
	    }
	  else if (has_null_arg)
	    {
	      null_sym = intr->sym;
	      continue;
	    }

	  /* Satisfy 12.4.4.1 such that an elemental match has lower
	     weight than a non-elemental match.  */
	  if (intr->sym->attr.elemental)
	    {
	      elem_sym = intr->sym;
	      continue;
	    }
	  return intr->sym;
	}
    }

  if (null_sym)
    return null_sym;

  return elem_sym ? elem_sym : NULL;
}


/* Do a brute force recursive search for a symbol.  */

static gfc_symtree *
find_symtree0 (gfc_symtree *root, gfc_symbol *sym)
{
  gfc_symtree * st;

  if (root->n.sym == sym)
    return root;

  st = NULL;
  if (root->left)
    st = find_symtree0 (root->left, sym);
  if (root->right && ! st)
    st = find_symtree0 (root->right, sym);
  return st;
}


/* Find a symtree for a symbol.  */

gfc_symtree *
gfc_find_sym_in_symtree (gfc_symbol *sym)
{
  gfc_symtree *st;
  gfc_namespace *ns;

  /* First try to find it by name.  */
  gfc_find_sym_tree (sym->name, gfc_current_ns, 1, &st);
  if (st && st->n.sym == sym)
    return st;

  /* If it's been renamed, resort to a brute-force search.  */
  /* TODO: avoid having to do this search.  If the symbol doesn't exist
     in the symtree for the current namespace, it should probably be added.  */
  for (ns = gfc_current_ns; ns; ns = ns->parent)
    {
      st = find_symtree0 (ns->sym_root, sym);
      if (st)
	return st;
    }
  gfc_internal_error ("Unable to find symbol %qs", sym->name);
  /* Not reached.  */
}


/* See if the arglist to an operator-call contains a derived-type argument
   with a matching type-bound operator.  If so, return the matching specific
   procedure defined as operator-target as well as the base-object to use
   (which is the found derived-type argument with operator).  The generic
   name, if any, is transmitted to the final expression via 'gname'.  */

static gfc_typebound_proc*
matching_typebound_op (gfc_expr** tb_base,
		       gfc_actual_arglist* args,
		       gfc_intrinsic_op op, const char* uop,
		       const char ** gname)
{
  gfc_actual_arglist* base;

  for (base = args; base; base = base->next)
    if (base->expr->ts.type == BT_DERIVED || base->expr->ts.type == BT_CLASS)
      {
	gfc_typebound_proc* tb;
	gfc_symbol* derived;
	bool result;

	while (base->expr->expr_type == EXPR_OP
	       && base->expr->value.op.op == INTRINSIC_PARENTHESES)
	  base->expr = base->expr->value.op.op1;

	if (base->expr->ts.type == BT_CLASS)
	  {
	    if (!base->expr->ts.u.derived || CLASS_DATA (base->expr) == NULL
		|| !gfc_expr_attr (base->expr).class_ok)
	      continue;
	    derived = CLASS_DATA (base->expr)->ts.u.derived;
	  }
	else
	  derived = base->expr->ts.u.derived;

	if (op == INTRINSIC_USER)
	  {
	    gfc_symtree* tb_uop;

	    gcc_assert (uop);
	    tb_uop = gfc_find_typebound_user_op (derived, &result, uop,
						 false, NULL);

	    if (tb_uop)
	      tb = tb_uop->n.tb;
	    else
	      tb = NULL;
	  }
	else
	  tb = gfc_find_typebound_intrinsic_op (derived, &result, op,
						false, NULL);

	/* This means we hit a PRIVATE operator which is use-associated and
	   should thus not be seen.  */
	if (!result)
	  tb = NULL;

	/* Look through the super-type hierarchy for a matching specific
	   binding.  */
	for (; tb; tb = tb->overridden)
	  {
	    gfc_tbp_generic* g;

	    gcc_assert (tb->is_generic);
	    for (g = tb->u.generic; g; g = g->next)
	      {
		gfc_symbol* target;
		gfc_actual_arglist* argcopy;
		bool matches;

		gcc_assert (g->specific);
		if (g->specific->error)
		  continue;

		target = g->specific->u.specific->n.sym;

		/* Check if this arglist matches the formal.  */
		argcopy = gfc_copy_actual_arglist (args);
		matches = gfc_arglist_matches_symbol (&argcopy, target);
		gfc_free_actual_arglist (argcopy);

		/* Return if we found a match.  */
		if (matches)
		  {
		    *tb_base = base->expr;
		    *gname = g->specific_st->name;
		    return g->specific;
		  }
	      }
	  }
      }

  return NULL;
}


/* For the 'actual arglist' of an operator call and a specific typebound
   procedure that has been found the target of a type-bound operator, build the
   appropriate EXPR_COMPCALL and resolve it.  We take this indirection over
   type-bound procedures rather than resolving type-bound operators 'directly'
   so that we can reuse the existing logic.  */

static void
build_compcall_for_operator (gfc_expr* e, gfc_actual_arglist* actual,
			     gfc_expr* base, gfc_typebound_proc* target,
			     const char *gname)
{
  e->expr_type = EXPR_COMPCALL;
  e->value.compcall.tbp = target;
  e->value.compcall.name = gname ? gname : "$op";
  e->value.compcall.actual = actual;
  e->value.compcall.base_object = base;
  e->value.compcall.ignore_pass = 1;
  e->value.compcall.assign = 0;
  if (e->ts.type == BT_UNKNOWN
	&& target->function)
    {
      if (target->is_generic)
	e->ts = target->u.generic->specific->u.specific->n.sym->ts;
      else
	e->ts = target->u.specific->n.sym->ts;
    }
}


/* This subroutine is called when an expression is being resolved.
   The expression node in question is either a user defined operator
   or an intrinsic operator with arguments that aren't compatible
   with the operator.  This subroutine builds an actual argument list
   corresponding to the operands, then searches for a compatible
   interface.  If one is found, the expression node is replaced with
   the appropriate function call. We use the 'match' enum to specify
   whether a replacement has been made or not, or if an error occurred.  */

match
gfc_extend_expr (gfc_expr *e)
{
  gfc_actual_arglist *actual;
  gfc_symbol *sym;
  gfc_namespace *ns;
  gfc_user_op *uop;
  gfc_intrinsic_op i;
  const char *gname;
  gfc_typebound_proc* tbo;
  gfc_expr* tb_base;

  sym = NULL;

  actual = gfc_get_actual_arglist ();
  actual->expr = e->value.op.op1;

  gname = NULL;

  if (e->value.op.op2 != NULL)
    {
      actual->next = gfc_get_actual_arglist ();
      actual->next->expr = e->value.op.op2;
    }

  i = fold_unary_intrinsic (e->value.op.op);

  /* See if we find a matching type-bound operator.  */
  if (i == INTRINSIC_USER)
    tbo = matching_typebound_op (&tb_base, actual,
				  i, e->value.op.uop->name, &gname);
  else
    switch (i)
      {
#define CHECK_OS_COMPARISON(comp) \
  case INTRINSIC_##comp: \
  case INTRINSIC_##comp##_OS: \
    tbo = matching_typebound_op (&tb_base, actual, \
				 INTRINSIC_##comp, NULL, &gname); \
    if (!tbo) \
      tbo = matching_typebound_op (&tb_base, actual, \
				   INTRINSIC_##comp##_OS, NULL, &gname); \
    break;
	CHECK_OS_COMPARISON(EQ)
	CHECK_OS_COMPARISON(NE)
	CHECK_OS_COMPARISON(GT)
	CHECK_OS_COMPARISON(GE)
	CHECK_OS_COMPARISON(LT)
	CHECK_OS_COMPARISON(LE)
#undef CHECK_OS_COMPARISON

	default:
	  tbo = matching_typebound_op (&tb_base, actual, i, NULL, &gname);
	  break;
      }

  /* If there is a matching typebound-operator, replace the expression with
      a call to it and succeed.  */
  if (tbo)
    {
      gcc_assert (tb_base);
      build_compcall_for_operator (e, actual, tb_base, tbo, gname);

      if (!gfc_resolve_expr (e))
	return MATCH_ERROR;
      else
	return MATCH_YES;
    }

  if (i == INTRINSIC_USER)
    {
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	{
	  uop = gfc_find_uop (e->value.op.uop->name, ns);
	  if (uop == NULL)
	    continue;

	  sym = gfc_search_interface (uop->op, 0, &actual);
	  if (sym != NULL)
	    break;
	}
    }
  else
    {
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	{
	  /* Due to the distinction between '==' and '.eq.' and friends, one has
	     to check if either is defined.  */
	  switch (i)
	    {
#define CHECK_OS_COMPARISON(comp) \
  case INTRINSIC_##comp: \
  case INTRINSIC_##comp##_OS: \
    sym = gfc_search_interface (ns->op[INTRINSIC_##comp], 0, &actual); \
    if (!sym) \
      sym = gfc_search_interface (ns->op[INTRINSIC_##comp##_OS], 0, &actual); \
    break;
	      CHECK_OS_COMPARISON(EQ)
	      CHECK_OS_COMPARISON(NE)
	      CHECK_OS_COMPARISON(GT)
	      CHECK_OS_COMPARISON(GE)
	      CHECK_OS_COMPARISON(LT)
	      CHECK_OS_COMPARISON(LE)
#undef CHECK_OS_COMPARISON

	      default:
		sym = gfc_search_interface (ns->op[i], 0, &actual);
	    }

	  if (sym != NULL)
	    break;
	}

      /* F2018(15.4.3.4.2) requires that the use of unlimited polymorphic
	 formal arguments does not override the intrinsic uses.  */
      gfc_push_suppress_errors ();
      if (sym
	  && (UNLIMITED_POLY (sym->formal->sym)
	      || (sym->formal->next
		  && UNLIMITED_POLY (sym->formal->next->sym)))
	  && !gfc_check_operator_interface (sym, e->value.op.op, e->where))
	sym = NULL;
      gfc_pop_suppress_errors ();
    }

  /* TODO: Do an ambiguity-check and error if multiple matching interfaces are
     found rather than just taking the first one and not checking further.  */

  if (sym == NULL)
    {
      /* Don't use gfc_free_actual_arglist().  */
      free (actual->next);
      free (actual);
      return MATCH_NO;
    }

  /* Change the expression node to a function call.  */
  e->expr_type = EXPR_FUNCTION;
  e->symtree = gfc_find_sym_in_symtree (sym);
  e->value.function.actual = actual;
  e->value.function.esym = NULL;
  e->value.function.isym = NULL;
  e->value.function.name = NULL;
  e->user_operator = 1;

  if (!gfc_resolve_expr (e))
    return MATCH_ERROR;

  return MATCH_YES;
}


/* Tries to replace an assignment code node with a subroutine call to the
   subroutine associated with the assignment operator. Return true if the node
   was replaced. On false, no error is generated.  */

bool
gfc_extend_assign (gfc_code *c, gfc_namespace *ns)
{
  gfc_actual_arglist *actual;
  gfc_expr *lhs, *rhs, *tb_base;
  gfc_symbol *sym = NULL;
  const char *gname = NULL;
  gfc_typebound_proc* tbo;

  lhs = c->expr1;
  rhs = c->expr2;

  /* Don't allow an intrinsic assignment with a BOZ rhs to be replaced.  */
  if (c->op == EXEC_ASSIGN
      && c->expr1->expr_type == EXPR_VARIABLE
      && c->expr2->expr_type == EXPR_CONSTANT && c->expr2->ts.type == BT_BOZ)
    return false;

  /* Don't allow an intrinsic assignment to be replaced.  */
  if (lhs->ts.type != BT_DERIVED && lhs->ts.type != BT_CLASS
      && (rhs->rank == 0 || rhs->rank == lhs->rank)
      && (lhs->ts.type == rhs->ts.type
	  || (gfc_numeric_ts (&lhs->ts) && gfc_numeric_ts (&rhs->ts))))
    return false;

  actual = gfc_get_actual_arglist ();
  actual->expr = lhs;

  actual->next = gfc_get_actual_arglist ();
  actual->next->expr = rhs;

  /* TODO: Ambiguity-check, see above for gfc_extend_expr.  */

  /* See if we find a matching type-bound assignment.  */
  tbo = matching_typebound_op (&tb_base, actual, INTRINSIC_ASSIGN,
			       NULL, &gname);

  if (tbo)
    {
      /* Success: Replace the expression with a type-bound call.  */
      gcc_assert (tb_base);
      c->expr1 = gfc_get_expr ();
      build_compcall_for_operator (c->expr1, actual, tb_base, tbo, gname);
      c->expr1->value.compcall.assign = 1;
      c->expr1->where = c->loc;
      c->expr2 = NULL;
      c->op = EXEC_COMPCALL;
      return true;
    }

  /* See if we find an 'ordinary' (non-typebound) assignment procedure.  */
  for (; ns; ns = ns->parent)
    {
      sym = gfc_search_interface (ns->op[INTRINSIC_ASSIGN], 1, &actual);
      if (sym != NULL)
	break;
    }

  if (sym)
    {
      /* Success: Replace the assignment with the call.  */
      c->op = EXEC_ASSIGN_CALL;
      c->symtree = gfc_find_sym_in_symtree (sym);
      c->expr1 = NULL;
      c->expr2 = NULL;
      c->ext.actual = actual;
      return true;
    }

  /* Failure: No assignment procedure found.  */
  free (actual->next);
  free (actual);
  return false;
}


/* Make sure that the interface just parsed is not already present in
   the given interface list.  Ambiguity isn't checked yet since module
   procedures can be present without interfaces.  */

bool
gfc_check_new_interface (gfc_interface *base, gfc_symbol *new_sym, locus loc)
{
  gfc_interface *ip;

  for (ip = base; ip; ip = ip->next)
    {
      if (ip->sym == new_sym)
	{
	  gfc_error ("Entity %qs at %L is already present in the interface",
		     new_sym->name, &loc);
	  return false;
	}
    }

  return true;
}


/* Add a symbol to the current interface.  */

bool
gfc_add_interface (gfc_symbol *new_sym)
{
  gfc_interface **head, *intr;
  gfc_namespace *ns;
  gfc_symbol *sym;

  switch (current_interface.type)
    {
    case INTERFACE_NAMELESS:
    case INTERFACE_ABSTRACT:
      return true;

    case INTERFACE_INTRINSIC_OP:
      for (ns = current_interface.ns; ns; ns = ns->parent)
	switch (current_interface.op)
	  {
	    case INTRINSIC_EQ:
	    case INTRINSIC_EQ_OS:
	      if (!gfc_check_new_interface (ns->op[INTRINSIC_EQ], new_sym,
					    gfc_current_locus)
	          || !gfc_check_new_interface (ns->op[INTRINSIC_EQ_OS],
					       new_sym, gfc_current_locus))
		return false;
	      break;

	    case INTRINSIC_NE:
	    case INTRINSIC_NE_OS:
	      if (!gfc_check_new_interface (ns->op[INTRINSIC_NE], new_sym,
					    gfc_current_locus)
	          || !gfc_check_new_interface (ns->op[INTRINSIC_NE_OS],
					       new_sym, gfc_current_locus))
		return false;
	      break;

	    case INTRINSIC_GT:
	    case INTRINSIC_GT_OS:
	      if (!gfc_check_new_interface (ns->op[INTRINSIC_GT],
					    new_sym, gfc_current_locus)
	          || !gfc_check_new_interface (ns->op[INTRINSIC_GT_OS],
					       new_sym, gfc_current_locus))
		return false;
	      break;

	    case INTRINSIC_GE:
	    case INTRINSIC_GE_OS:
	      if (!gfc_check_new_interface (ns->op[INTRINSIC_GE],
					    new_sym, gfc_current_locus)
	          || !gfc_check_new_interface (ns->op[INTRINSIC_GE_OS],
					       new_sym, gfc_current_locus))
		return false;
	      break;

	    case INTRINSIC_LT:
	    case INTRINSIC_LT_OS:
	      if (!gfc_check_new_interface (ns->op[INTRINSIC_LT],
					    new_sym, gfc_current_locus)
	          || !gfc_check_new_interface (ns->op[INTRINSIC_LT_OS],
					       new_sym, gfc_current_locus))
		return false;
	      break;

	    case INTRINSIC_LE:
	    case INTRINSIC_LE_OS:
	      if (!gfc_check_new_interface (ns->op[INTRINSIC_LE],
					    new_sym, gfc_current_locus)
	          || !gfc_check_new_interface (ns->op[INTRINSIC_LE_OS],
					       new_sym, gfc_current_locus))
		return false;
	      break;

	    default:
	      if (!gfc_check_new_interface (ns->op[current_interface.op],
					    new_sym, gfc_current_locus))
		return false;
	  }

      head = &current_interface.ns->op[current_interface.op];
      break;

    case INTERFACE_GENERIC:
    case INTERFACE_DTIO:
      for (ns = current_interface.ns; ns; ns = ns->parent)
	{
	  gfc_find_symbol (current_interface.sym->name, ns, 0, &sym);
	  if (sym == NULL)
	    continue;

	  if (!gfc_check_new_interface (sym->generic,
					new_sym, gfc_current_locus))
	    return false;
	}

      head = &current_interface.sym->generic;
      break;

    case INTERFACE_USER_OP:
      if (!gfc_check_new_interface (current_interface.uop->op,
				    new_sym, gfc_current_locus))
	return false;

      head = &current_interface.uop->op;
      break;

    default:
      gfc_internal_error ("gfc_add_interface(): Bad interface type");
    }

  intr = gfc_get_interface ();
  intr->sym = new_sym;
  intr->where = gfc_current_locus;

  intr->next = *head;
  *head = intr;

  return true;
}


gfc_interface *&
gfc_current_interface_head (void)
{
  switch (current_interface.type)
    {
      case INTERFACE_INTRINSIC_OP:
	return current_interface.ns->op[current_interface.op];

      case INTERFACE_GENERIC:
      case INTERFACE_DTIO:
	return current_interface.sym->generic;

      case INTERFACE_USER_OP:
	return current_interface.uop->op;

      default:
	gcc_unreachable ();
    }
}


void
gfc_set_current_interface_head (gfc_interface *i)
{
  switch (current_interface.type)
    {
      case INTERFACE_INTRINSIC_OP:
	current_interface.ns->op[current_interface.op] = i;
	break;

      case INTERFACE_GENERIC:
      case INTERFACE_DTIO:
	current_interface.sym->generic = i;
	break;

      case INTERFACE_USER_OP:
	current_interface.uop->op = i;
	break;

      default:
	gcc_unreachable ();
    }
}


/* Gets rid of a formal argument list.  We do not free symbols.
   Symbols are freed when a namespace is freed.  */

void
gfc_free_formal_arglist (gfc_formal_arglist *p)
{
  gfc_formal_arglist *q;

  for (; p; p = q)
    {
      q = p->next;
      free (p);
    }
}


/* Check that it is ok for the type-bound procedure 'proc' to override the
   procedure 'old', cf. F08:4.5.7.3.  */

bool
gfc_check_typebound_override (gfc_symtree* proc, gfc_symtree* old)
{
  locus where;
  gfc_symbol *proc_target, *old_target;
  unsigned proc_pass_arg, old_pass_arg, argpos;
  gfc_formal_arglist *proc_formal, *old_formal;
  bool check_type;
  char err[200];

  /* This procedure should only be called for non-GENERIC proc.  */
  gcc_assert (!proc->n.tb->is_generic);

  /* If the overwritten procedure is GENERIC, this is an error.  */
  if (old->n.tb->is_generic)
    {
      gfc_error ("Cannot overwrite GENERIC %qs at %L",
		 old->name, &proc->n.tb->where);
      return false;
    }

  where = proc->n.tb->where;
  proc_target = proc->n.tb->u.specific->n.sym;
  old_target = old->n.tb->u.specific->n.sym;

  /* Check that overridden binding is not NON_OVERRIDABLE.  */
  if (old->n.tb->non_overridable)
    {
      gfc_error ("%qs at %L overrides a procedure binding declared"
		 " NON_OVERRIDABLE", proc->name, &where);
      return false;
    }

  /* It's an error to override a non-DEFERRED procedure with a DEFERRED one.  */
  if (!old->n.tb->deferred && proc->n.tb->deferred)
    {
      gfc_error ("%qs at %L must not be DEFERRED as it overrides a"
		 " non-DEFERRED binding", proc->name, &where);
      return false;
    }

  /* If the overridden binding is PURE, the overriding must be, too.  */
  if (old_target->attr.pure && !proc_target->attr.pure)
    {
      gfc_error ("%qs at %L overrides a PURE procedure and must also be PURE",
		 proc->name, &where);
      return false;
    }

  /* If the overridden binding is ELEMENTAL, the overriding must be, too.  If it
     is not, the overriding must not be either.  */
  if (old_target->attr.elemental && !proc_target->attr.elemental)
    {
      gfc_error ("%qs at %L overrides an ELEMENTAL procedure and must also be"
		 " ELEMENTAL", proc->name, &where);
      return false;
    }
  if (!old_target->attr.elemental && proc_target->attr.elemental)
    {
      gfc_error ("%qs at %L overrides a non-ELEMENTAL procedure and must not"
		 " be ELEMENTAL, either", proc->name, &where);
      return false;
    }

  /* If the overridden binding is a SUBROUTINE, the overriding must also be a
     SUBROUTINE.  */
  if (old_target->attr.subroutine && !proc_target->attr.subroutine)
    {
      gfc_error ("%qs at %L overrides a SUBROUTINE and must also be a"
		 " SUBROUTINE", proc->name, &where);
      return false;
    }

  /* If the overridden binding is a FUNCTION, the overriding must also be a
     FUNCTION and have the same characteristics.  */
  if (old_target->attr.function)
    {
      if (!proc_target->attr.function)
	{
	  gfc_error ("%qs at %L overrides a FUNCTION and must also be a"
		     " FUNCTION", proc->name, &where);
	  return false;
	}

      if (!gfc_check_result_characteristics (proc_target, old_target,
					     err, sizeof(err)))
	{
	  gfc_error ("Result mismatch for the overriding procedure "
		     "%qs at %L: %s", proc->name, &where, err);
	  return false;
	}
    }

  /* If the overridden binding is PUBLIC, the overriding one must not be
     PRIVATE.  */
  if (old->n.tb->access == ACCESS_PUBLIC
      && proc->n.tb->access == ACCESS_PRIVATE)
    {
      gfc_error ("%qs at %L overrides a PUBLIC procedure and must not be"
		 " PRIVATE", proc->name, &where);
      return false;
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
  proc_formal = gfc_sym_get_dummy_args (proc_target);
  old_formal = gfc_sym_get_dummy_args (old_target);
  for ( ; proc_formal && old_formal;
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
	  gfc_error ("Dummy argument %qs of %qs at %L should be named %qs as"
		     " to match the corresponding argument of the overridden"
		     " procedure", proc_formal->sym->name, proc->name, &where,
		     old_formal->sym->name);
	  return false;
	}

      check_type = proc_pass_arg != argpos && old_pass_arg != argpos;
      if (!gfc_check_dummy_characteristics (proc_formal->sym, old_formal->sym,
					check_type, err, sizeof(err)))
	{
	  gfc_error_opt (0, "Argument mismatch for the overriding procedure "
			 "%qs at %L: %s", proc->name, &where, err);
	  return false;
	}

      ++argpos;
    }
  if (proc_formal || old_formal)
    {
      gfc_error ("%qs at %L must have the same number of formal arguments as"
		 " the overridden procedure", proc->name, &where);
      return false;
    }

  /* If the overridden binding is NOPASS, the overriding one must also be
     NOPASS.  */
  if (old->n.tb->nopass && !proc->n.tb->nopass)
    {
      gfc_error ("%qs at %L overrides a NOPASS binding and must also be"
		 " NOPASS", proc->name, &where);
      return false;
    }

  /* If the overridden binding is PASS(x), the overriding one must also be
     PASS and the passed-object dummy arguments must correspond.  */
  if (!old->n.tb->nopass)
    {
      if (proc->n.tb->nopass)
	{
	  gfc_error ("%qs at %L overrides a binding with PASS and must also be"
		     " PASS", proc->name, &where);
	  return false;
	}

      if (proc_pass_arg != old_pass_arg)
	{
	  gfc_error ("Passed-object dummy argument of %qs at %L must be at"
		     " the same position as the passed-object dummy argument of"
		     " the overridden procedure", proc->name, &where);
	  return false;
	}
    }

  return true;
}


/* The following three functions check that the formal arguments
   of user defined derived type IO procedures are compliant with
   the requirements of the standard, see F03:9.5.3.7.2 (F08:9.6.4.8.3).  */

static void
check_dtio_arg_TKR_intent (gfc_symbol *fsym, bool typebound, bt type,
			   int kind, int rank, sym_intent intent)
{
  if (fsym->ts.type != type)
    {
      gfc_error ("DTIO dummy argument at %L must be of type %s",
		 &fsym->declared_at, gfc_basic_typename (type));
      return;
    }

  if (fsym->ts.type != BT_CLASS && fsym->ts.type != BT_DERIVED
      && fsym->ts.kind != kind)
    gfc_error ("DTIO dummy argument at %L must be of KIND = %d",
	       &fsym->declared_at, kind);

  if (!typebound
      && rank == 0
      && (((type == BT_CLASS) && CLASS_DATA (fsym)->attr.dimension)
	  || ((type != BT_CLASS) && fsym->attr.dimension)))
    gfc_error ("DTIO dummy argument at %L must be a scalar",
	       &fsym->declared_at);
  else if (rank == 1
	   && (fsym->as == NULL || fsym->as->type != AS_ASSUMED_SHAPE))
    gfc_error ("DTIO dummy argument at %L must be an "
	       "ASSUMED SHAPE ARRAY", &fsym->declared_at);

  if (type == BT_CHARACTER && fsym->ts.u.cl->length != NULL)
    gfc_error ("DTIO character argument at %L must have assumed length",
               &fsym->declared_at);

  if (fsym->attr.intent != intent)
    gfc_error ("DTIO dummy argument at %L must have INTENT %s",
	       &fsym->declared_at, gfc_code2string (intents, (int)intent));
  return;
}


static void
check_dtio_interface1 (gfc_symbol *derived, gfc_symtree *tb_io_st,
		       bool typebound, bool formatted, int code)
{
  gfc_symbol *dtio_sub, *generic_proc, *fsym;
  gfc_typebound_proc *tb_io_proc, *specific_proc;
  gfc_interface *intr;
  gfc_formal_arglist *formal;
  int arg_num;

  bool read = ((dtio_codes)code == DTIO_RF)
	       || ((dtio_codes)code == DTIO_RUF);
  bt type;
  sym_intent intent;
  int kind;

  dtio_sub = NULL;
  if (typebound)
    {
      /* Typebound DTIO binding.  */
      tb_io_proc = tb_io_st->n.tb;
      if (tb_io_proc == NULL)
	return;

      gcc_assert (tb_io_proc->is_generic);

      specific_proc = tb_io_proc->u.generic->specific;
      if (specific_proc == NULL || specific_proc->is_generic)
	return;

      dtio_sub = specific_proc->u.specific->n.sym;
    }
  else
    {
      generic_proc = tb_io_st->n.sym;
      if (generic_proc == NULL || generic_proc->generic == NULL)
	return;

      for (intr = tb_io_st->n.sym->generic; intr; intr = intr->next)
	{
	  if (intr->sym && intr->sym->formal && intr->sym->formal->sym
	      && ((intr->sym->formal->sym->ts.type == BT_CLASS
	           && CLASS_DATA (intr->sym->formal->sym)->ts.u.derived
							     == derived)
		  || (intr->sym->formal->sym->ts.type == BT_DERIVED
		      && intr->sym->formal->sym->ts.u.derived == derived)))
	    {
	      dtio_sub = intr->sym;
	      break;
	    }
	  else if (intr->sym && intr->sym->formal && !intr->sym->formal->sym)
	    {
	      gfc_error ("Alternate return at %L is not permitted in a DTIO "
			 "procedure", &intr->sym->declared_at);
	      return;
	    }
	}

      if (dtio_sub == NULL)
	return;
    }

  gcc_assert (dtio_sub);
  if (!dtio_sub->attr.subroutine)
    gfc_error ("DTIO procedure %qs at %L must be a subroutine",
	       dtio_sub->name, &dtio_sub->declared_at);

  if (!dtio_sub->resolve_symbol_called)
    gfc_resolve_formal_arglist (dtio_sub);

  arg_num = 0;
  for (formal = dtio_sub->formal; formal; formal = formal->next)
    arg_num++;

  if (arg_num < (formatted ? 6 : 4))
    {
      gfc_error ("Too few dummy arguments in DTIO procedure %qs at %L",
		 dtio_sub->name, &dtio_sub->declared_at);
      return;
    }

  if (arg_num > (formatted ? 6 : 4))
    {
      gfc_error ("Too many dummy arguments in DTIO procedure %qs at %L",
		 dtio_sub->name, &dtio_sub->declared_at);
      return;
    }

  /* Now go through the formal arglist.  */
  arg_num = 1;
  for (formal = dtio_sub->formal; formal; formal = formal->next, arg_num++)
    {
      if (!formatted && arg_num == 3)
	arg_num = 5;
      fsym = formal->sym;

      if (fsym == NULL)
	{
	  gfc_error ("Alternate return at %L is not permitted in a DTIO "
		     "procedure", &dtio_sub->declared_at);
	  return;
	}

      switch (arg_num)
	{
	case(1):			/* DTV  */
	  type = derived->attr.sequence || derived->attr.is_bind_c ?
		 BT_DERIVED : BT_CLASS;
	  kind = 0;
	  intent = read ? INTENT_INOUT : INTENT_IN;
	  check_dtio_arg_TKR_intent (fsym, typebound, type, kind,
				     0, intent);
	  break;

	case(2):			/* UNIT  */
	  type = BT_INTEGER;
	  kind = gfc_default_integer_kind;
	  intent = INTENT_IN;
	  check_dtio_arg_TKR_intent (fsym, typebound, type, kind,
				     0, intent);
	  break;
	case(3):			/* IOTYPE  */
	  type = BT_CHARACTER;
	  kind = gfc_default_character_kind;
	  intent = INTENT_IN;
	  check_dtio_arg_TKR_intent (fsym, typebound, type, kind,
				     0, intent);
	  break;
	case(4):			/* VLIST  */
	  type = BT_INTEGER;
	  kind = gfc_default_integer_kind;
	  intent = INTENT_IN;
	  check_dtio_arg_TKR_intent (fsym, typebound, type, kind,
				     1, intent);
	  break;
	case(5):			/* IOSTAT  */
	  type = BT_INTEGER;
	  kind = gfc_default_integer_kind;
	  intent = INTENT_OUT;
	  check_dtio_arg_TKR_intent (fsym, typebound, type, kind,
				     0, intent);
	  break;
	case(6):			/* IOMSG  */
	  type = BT_CHARACTER;
	  kind = gfc_default_character_kind;
	  intent = INTENT_INOUT;
	  check_dtio_arg_TKR_intent (fsym, typebound, type, kind,
				     0, intent);
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  derived->attr.has_dtio_procs = 1;
  return;
}

void
gfc_check_dtio_interfaces (gfc_symbol *derived)
{
  gfc_symtree *tb_io_st;
  bool t = false;
  int code;
  bool formatted;

  if (derived->attr.is_class == 1 || derived->attr.vtype == 1)
    return;

  /* Check typebound DTIO bindings.  */
  for (code = 0; code < 4; code++)
    {
      formatted = ((dtio_codes)code == DTIO_RF)
		   || ((dtio_codes)code == DTIO_WF);

      tb_io_st = gfc_find_typebound_proc (derived, &t,
					  gfc_code2string (dtio_procs, code),
					  true, &derived->declared_at);
      if (tb_io_st != NULL)
	check_dtio_interface1 (derived, tb_io_st, true, formatted, code);
    }

  /* Check generic DTIO interfaces.  */
  for (code = 0; code < 4; code++)
    {
      formatted = ((dtio_codes)code == DTIO_RF)
		   || ((dtio_codes)code == DTIO_WF);

      tb_io_st = gfc_find_symtree (derived->ns->sym_root,
				   gfc_code2string (dtio_procs, code));
      if (tb_io_st != NULL)
	check_dtio_interface1 (derived, tb_io_st, false, formatted, code);
    }
}


gfc_symtree*
gfc_find_typebound_dtio_proc (gfc_symbol *derived, bool write, bool formatted)
{
  gfc_symtree *tb_io_st = NULL;
  bool t = false;

  if (!derived || !derived->resolve_symbol_called
      || derived->attr.flavor != FL_DERIVED)
    return NULL;

  /* Try to find a typebound DTIO binding.  */
  if (formatted == true)
    {
      if (write == true)
        tb_io_st = gfc_find_typebound_proc (derived, &t,
					    gfc_code2string (dtio_procs,
							     DTIO_WF),
					    true,
					    &derived->declared_at);
      else
        tb_io_st = gfc_find_typebound_proc (derived, &t,
					    gfc_code2string (dtio_procs,
							     DTIO_RF),
					    true,
					    &derived->declared_at);
    }
  else
    {
      if (write == true)
        tb_io_st = gfc_find_typebound_proc (derived, &t,
					    gfc_code2string (dtio_procs,
							     DTIO_WUF),
					    true,
					    &derived->declared_at);
      else
        tb_io_st = gfc_find_typebound_proc (derived, &t,
					    gfc_code2string (dtio_procs,
							     DTIO_RUF),
					    true,
					    &derived->declared_at);
    }
  return tb_io_st;
}


gfc_symbol *
gfc_find_specific_dtio_proc (gfc_symbol *derived, bool write, bool formatted)
{
  gfc_symtree *tb_io_st = NULL;
  gfc_symbol *dtio_sub = NULL;
  gfc_symbol *extended;
  gfc_typebound_proc *tb_io_proc, *specific_proc;

  tb_io_st = gfc_find_typebound_dtio_proc (derived, write, formatted);

  if (tb_io_st != NULL)
    {
      const char *genname;
      gfc_symtree *st;

      tb_io_proc = tb_io_st->n.tb;
      gcc_assert (tb_io_proc != NULL);
      gcc_assert (tb_io_proc->is_generic);
      gcc_assert (tb_io_proc->u.generic->next == NULL);

      specific_proc = tb_io_proc->u.generic->specific;
      gcc_assert (!specific_proc->is_generic);

      /* Go back and make sure that we have the right specific procedure.
	 Here we most likely have a procedure from the parent type, which
	 can be overridden in extensions.  */
      genname = tb_io_proc->u.generic->specific_st->name;
      st = gfc_find_typebound_proc (derived, NULL, genname,
				    true, &tb_io_proc->where);
      if (st)
	dtio_sub = st->n.tb->u.specific->n.sym;
      else
	dtio_sub = specific_proc->u.specific->n.sym;

      goto finish;
    }

  /* If there is not a typebound binding, look for a generic
     DTIO interface.  */
  for (extended = derived; extended;
       extended = gfc_get_derived_super_type (extended))
    {
      if (extended == NULL || extended->ns == NULL
	  || extended->attr.flavor == FL_UNKNOWN)
	return NULL;

      if (formatted == true)
	{
	  if (write == true)
	    tb_io_st = gfc_find_symtree (extended->ns->sym_root,
					 gfc_code2string (dtio_procs,
							  DTIO_WF));
	  else
	    tb_io_st = gfc_find_symtree (extended->ns->sym_root,
					 gfc_code2string (dtio_procs,
							  DTIO_RF));
	}
      else
	{
	  if (write == true)
	    tb_io_st = gfc_find_symtree (extended->ns->sym_root,
					 gfc_code2string (dtio_procs,
							  DTIO_WUF));
	  else
	    tb_io_st = gfc_find_symtree (extended->ns->sym_root,
					 gfc_code2string (dtio_procs,
							  DTIO_RUF));
	}

      if (tb_io_st != NULL
	  && tb_io_st->n.sym
	  && tb_io_st->n.sym->generic)
	{
	  for (gfc_interface *intr = tb_io_st->n.sym->generic;
	       intr && intr->sym; intr = intr->next)
	    {
	      if (intr->sym->formal)
		{
		  gfc_symbol *fsym = intr->sym->formal->sym;
		  if ((fsym->ts.type == BT_CLASS
		      && CLASS_DATA (fsym)->ts.u.derived == extended)
		      || (fsym->ts.type == BT_DERIVED
			  && fsym->ts.u.derived == extended))
		    {
		      dtio_sub = intr->sym;
		      break;
		    }
		}
	    }
	}
    }

finish:
  if (dtio_sub
      && dtio_sub->formal->sym->ts.type == BT_CLASS
      && derived != CLASS_DATA (dtio_sub->formal->sym)->ts.u.derived)
    gfc_find_derived_vtab (derived);

  return dtio_sub;
}

/* Helper function - if we do not find an interface for a procedure,
   construct it from the actual arglist.  Luckily, this can only
   happen for call by reference, so the information we actually need
   to provide (and which would be impossible to guess from the call
   itself) is not actually needed.  */

void
gfc_get_formal_from_actual_arglist (gfc_symbol *sym,
				    gfc_actual_arglist *actual_args)
{
  gfc_actual_arglist *a;
  gfc_formal_arglist **f;
  gfc_symbol *s;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  static int var_num;

  f = &sym->formal;
  for (a = actual_args; a != NULL; a = a->next)
    {
      (*f) = gfc_get_formal_arglist ();
      if (a->expr)
	{
	  snprintf (name, GFC_MAX_SYMBOL_LEN, "_formal_%d", var_num ++);
	  gfc_get_symbol (name, gfc_current_ns, &s);
	  if (a->expr->ts.type == BT_PROCEDURE)
	    {
	      s->attr.flavor = FL_PROCEDURE;
	    }
	  else
	    {
	      s->ts = a->expr->ts;

	      if (s->ts.type == BT_CHARACTER)
		s->ts.u.cl = gfc_get_charlen ();

	      s->ts.deferred = 0;
	      s->ts.is_iso_c = 0;
	      s->ts.is_c_interop = 0;
	      s->attr.flavor = FL_VARIABLE;
	      if (a->expr->rank > 0)
		{
		  s->attr.dimension = 1;
		  s->as = gfc_get_array_spec ();
		  s->as->rank = 1;
		  s->as->lower[0] = gfc_get_int_expr (gfc_index_integer_kind,
						      &a->expr->where, 1);
		  s->as->upper[0] = NULL;
		  s->as->type = AS_ASSUMED_SIZE;
		}
	      else
		s->maybe_array = maybe_dummy_array_arg (a->expr);
	    }
	  s->attr.dummy = 1;
	  s->attr.artificial = 1;
	  s->declared_at = a->expr->where;
	  s->attr.intent = INTENT_UNKNOWN;
	  (*f)->sym = s;
	}
      else  /* If a->expr is NULL, this is an alternate rerturn.  */
	(*f)->sym = NULL;

      f = &((*f)->next);
    }
}


const char *
gfc_dummy_arg_get_name (gfc_dummy_arg & dummy_arg)
{
  switch (dummy_arg.intrinsicness)
    {
    case GFC_INTRINSIC_DUMMY_ARG:
      return dummy_arg.u.intrinsic->name;

    case GFC_NON_INTRINSIC_DUMMY_ARG:
      return dummy_arg.u.non_intrinsic->sym->name;

    default:
      gcc_unreachable ();
    }
}


const gfc_typespec &
gfc_dummy_arg_get_typespec (gfc_dummy_arg & dummy_arg)
{
  switch (dummy_arg.intrinsicness)
    {
    case GFC_INTRINSIC_DUMMY_ARG:
      return dummy_arg.u.intrinsic->ts;

    case GFC_NON_INTRINSIC_DUMMY_ARG:
      return dummy_arg.u.non_intrinsic->sym->ts;

    default:
      gcc_unreachable ();
    }
}


bool
gfc_dummy_arg_is_optional (gfc_dummy_arg & dummy_arg)
{
  switch (dummy_arg.intrinsicness)
    {
    case GFC_INTRINSIC_DUMMY_ARG:
      return dummy_arg.u.intrinsic->optional;

    case GFC_NON_INTRINSIC_DUMMY_ARG:
      return dummy_arg.u.non_intrinsic->sym->attr.optional;

    default:
      gcc_unreachable ();
    }
}
