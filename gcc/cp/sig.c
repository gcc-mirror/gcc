/* Functions dealing with signatures and signature pointers/references.
   Copyright (C) 1992 Free Software Foundation, Inc.
   Contributed by Gerald Baumgartner (gb@cs.purdue.edu)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include <stdio.h>
#include "obstack.h"
#include "tree.h"
#include "cp-tree.h"
#include "flags.h"
#include "assert.h"

extern struct obstack *current_obstack;
extern struct obstack permanent_obstack;
extern struct obstack *saveable_obstack;

extern void error ();
extern void sorry ();
extern void compiler_error ();
extern void make_decl_rtl			PROTO((tree, char *, int));

/* Used to help generate globally unique names for signature tables.  */

static int global_sigtable_name_counter;

/* Build an identifier for a signature pointer or reference, so we
   can use it's name in function name mangling.  */

static tree
build_signature_pointer_or_reference_name (to_type, constp, volatilep, refp)
     tree to_type;
     int constp, volatilep, refp;
{
  char * sig_name = TYPE_NAME_STRING (to_type);
  int name_len = TYPE_NAME_LENGTH (to_type) + constp + volatilep;
  char * name;

  if (refp)
    {
      name = (char *) alloca (name_len + sizeof (SIGNATURE_REFERENCE_NAME) +2);
      sprintf (name, SIGNATURE_REFERENCE_NAME_FORMAT,
	       constp ? "C" : "", volatilep ? "V": "", sig_name);
    }
  else
    {
      name = (char *) alloca (name_len + sizeof (SIGNATURE_POINTER_NAME) + 2);
      sprintf (name, SIGNATURE_POINTER_NAME_FORMAT,
	       constp ? "C" : "", volatilep ? "V": "", sig_name);
    }
  return get_identifier (name);
}

/* Build a DECL node for a signature pointer or reference, so we can
   tell the debugger the structure of signature pointers/references.
   This function is called at most eight times for a given signature,
   once for each [const] [volatile] signature pointer/reference.  */

static void
build_signature_pointer_or_reference_decl (type, name)
     tree type, name;
{
  tree decl;

  /* We don't enter this declaration in any sort of symbol table.  */
  decl = build_decl (TYPE_DECL, name, type);
  TYPE_NAME (type) = decl;
  TREE_CHAIN (type) = decl;
}

/* Construct, lay out and return the type of pointers or references
   to signature TO_TYPE.  If such a type has already been constructed,
   reuse it. If CONSTP or VOLATILEP is specified, make the `optr' const
   or volatile, respectively.   If we are constructing a const/volatile
   type variant and the main type variant doesn't exist yet, it is built
   as well.  If REFP is 1, we construct a signature reference, otherwise
   a signature pointer is constructed.

   This function is a subroutine of `build_signature_pointer_type' and
   `build_signature_reference_type'.  */

static tree
build_signature_pointer_or_reference_type (to_type, constp, volatilep, refp)
     tree to_type;
     int constp, volatilep, refp;
{
  register tree t, m;
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;

  m = refp ? SIGNATURE_REFERENCE_TO (to_type) : SIGNATURE_POINTER_TO (to_type);

  /* If we don't have the main variant yet, construct it.  */
  if (m == NULL_TREE
      && (constp || volatilep))
    m = build_signature_pointer_or_reference_type (to_type, 0, 0, refp);

  /* Treat any nonzero argument as 1.  */
  constp = !!constp;
  volatilep = !!volatilep;
  refp = !!refp;

  /* If not generating auxiliary info, search the chain of variants to see
     if there is already one there just like the one we need to have.  If so,
     use that existing one.

     We don't do this in the case where we are generating aux info because
     in that case we want each typedef names to get it's own distinct type
     node, even if the type of this new typedef is the same as some other
     (existing) type.  */

  if (m && !flag_gen_aux_info)
    for (t = m; t; t = TYPE_NEXT_VARIANT (t))
      if (constp == TYPE_READONLY (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (t))))
	  && volatilep == TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (t)))))
        return t;

  /* We need a new one.  If TO_TYPE is permanent, make this permanent too.  */
  if (TREE_PERMANENT (to_type))
    {
      current_obstack = &permanent_obstack;
      saveable_obstack = &permanent_obstack;
    }

  /* A signature pointer or reference to a signature `s' looks like this:

       struct {
         void * optr;
	 const s * sptr;
	 vtbl_type_node * vptr;
       };

     A `const' signature pointer/reference is a

       struct {
         const void * optr;
	 const s * sptr;
	 vtbl_type_node * vptr;
       };

     Similarly, for `volatile' and `const volatile'.
   */

  t = make_lang_type (RECORD_TYPE);
  {
    tree obj_type = build_type_variant (void_type_node, constp, volatilep);
    tree optr_type = build_pointer_type (obj_type);
    tree optr, sptr, vptr;

    optr = build_lang_field_decl (FIELD_DECL,
				  get_identifier (SIGNATURE_OPTR_NAME),
				  optr_type);
    DECL_FIELD_CONTEXT (optr) = t;
    DECL_CLASS_CONTEXT (optr) = t;

    if (m)
      {
	/* We can share `sptr' and `vptr' among type variants.  */
	sptr = TREE_CHAIN (TYPE_FIELDS (m));
	vptr = TREE_CHAIN (sptr);
      }
    else
      {
	tree sig_tbl_type = cp_build_type_variant (to_type, 1, 0);
	
	sptr = build_lang_field_decl (FIELD_DECL,
				      get_identifier (SIGNATURE_SPTR_NAME),
				      build_pointer_type (sig_tbl_type));
	vptr = build_lang_field_decl (FIELD_DECL,
				      get_identifier (SIGNATURE_VPTR_NAME),
				      build_pointer_type (vtbl_type_node));
	DECL_FIELD_CONTEXT (sptr) = t;
	DECL_CLASS_CONTEXT (sptr) = t;
	DECL_FIELD_CONTEXT (vptr) = t;
	DECL_CLASS_CONTEXT (vptr) = t;
	TREE_CHAIN (sptr) = vptr;
	TREE_CHAIN (vptr) = NULL_TREE;
      }

    TREE_CHAIN (optr) = sptr;
    TYPE_FIELDS (t) = optr;
    /* To make `build_vfn_ref' work when building a signature method call.  */
    CLASSTYPE_VFIELD (t) = vptr;
    DECL_FCONTEXT (CLASSTYPE_VFIELD (t)) = t;
    TYPE_ALIGN (t) = TYPE_ALIGN (optr_type);
  }

  {
    tree name = build_signature_pointer_or_reference_name (to_type, constp,
							   volatilep, refp);

    /* Build a DECL node for this type, so the debugger has access to it.  */
    build_signature_pointer_or_reference_decl (t, name);
  }

  CLASSTYPE_GOT_SEMICOLON (t) = 1;
  IS_SIGNATURE_POINTER (t) = ! refp;
  IS_SIGNATURE_REFERENCE (t) = refp;
  SIGNATURE_TYPE (t) = to_type;

  if (m)
    {
      /* Add this type to the chain of variants of TYPE.
	 Every type has to be its own TYPE_MAIN_VARIANT.  */
      TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
      TYPE_NEXT_VARIANT (m) = t;
    }
  else if (refp)
    /* Record this type as the reference to TO_TYPE.  */
    SIGNATURE_REFERENCE_TO (to_type) = t;
  else
    /* Record this type as the pointer to TO_TYPE.  */
    SIGNATURE_POINTER_TO (to_type) = t;

  /* Lay out the type.  This function has many callers that are concerned
     with expression-construction, and this simplifies them all.
     Also, it guarantees the TYPE_SIZE is permanent if the type is.  */
  layout_type (t);

  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;

  /* Ouput debug information for this type.  */
  rest_of_type_compilation (t, 1);

  return t;
}

/* Construct, lay out and return the type of pointers to signature TO_TYPE.  */

tree
build_signature_pointer_type (to_type, constp, volatilep)
     tree to_type;
     int constp, volatilep;
{
  return
    build_signature_pointer_or_reference_type (to_type, constp, volatilep, 0);
}

/* Construct, lay out and return the type of pointers to signature TO_TYPE.  */

tree
build_signature_reference_type (to_type, constp, volatilep)
     tree to_type;
     int constp, volatilep;
{
  return
    build_signature_pointer_or_reference_type (to_type, constp, volatilep, 1);
}

/* Return the name of the signature table (as an IDENTIFIER_NODE)
   for the given signature type SIG_TYPE and rhs type RHS_TYPE.  */

static tree
get_sigtable_name (sig_type, rhs_type)
     tree sig_type, rhs_type;
{
  tree sig_type_id = build_typename_overload (sig_type);
  tree rhs_type_id = build_typename_overload (rhs_type);
  char *buf = (char *) alloca (sizeof (SIGTABLE_NAME_FORMAT_LONG)
			       + IDENTIFIER_LENGTH (sig_type_id)
			       + IDENTIFIER_LENGTH (rhs_type_id) + 20);
  char *sig_ptr = IDENTIFIER_POINTER (sig_type_id);
  char *rhs_ptr = IDENTIFIER_POINTER (rhs_type_id);
  int i, j;

  for (i = 0; sig_ptr[i] == OPERATOR_TYPENAME_FORMAT[i]; i++)
    /* do nothing */;
  while (sig_ptr[i] >= '0' && sig_ptr[i] <= '9')
    i += 1;

  for (j = 0; rhs_ptr[j] == OPERATOR_TYPENAME_FORMAT[j]; j++)
    /* do nothing */;
  while (rhs_ptr[j] >= '0' && rhs_ptr[j] <= '9')
    j += 1;

  if (IS_SIGNATURE (rhs_type))
    sprintf (buf, SIGTABLE_NAME_FORMAT_LONG, sig_ptr+i, rhs_ptr+j,
	     global_sigtable_name_counter++);
  else
    sprintf (buf, SIGTABLE_NAME_FORMAT, sig_ptr+i, rhs_ptr+j);
  return get_identifier (buf);
}

/* Build a field decl that points to a signature member function.  */

static tree
build_member_function_pointer (member)
     tree member;
{
  char *namstr = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (member));
  int namlen = IDENTIFIER_LENGTH (DECL_ASSEMBLER_NAME (member));
  char *name;
  tree entry;
  
  name = (char *) alloca (namlen + sizeof (SIGNATURE_FIELD_NAME) + 2);
  sprintf (name, SIGNATURE_FIELD_NAME_FORMAT, namstr);

  /* @@ Do we really want to xref signature table fields?  */
  GNU_xref_ref (current_function_decl, name);

  entry = build_lang_field_decl (FIELD_DECL, get_identifier (name),
				 TYPE_MAIN_VARIANT (sigtable_entry_type));
  TREE_CONSTANT (entry) = 1;
  TREE_READONLY (entry) = 1;

  /* @@ Do we really want to xref signature table fields?  */
  GNU_xref_decl (current_function_decl, entry);

  return entry;
}

/* For each FUNCTION_DECL in a signature we construct a member function
   pointer of the appropriate type.  We also need two flags to test
   whether the member function pointer points to a virtual function or
   to a default implementation.  Those flags will be the two lower order
   bits of the member function pointer (or the two higher order bits,
   based on the configuration).

   The new FIELD_DECLs are appended at the end of the last (and only)
   sublist of `list_of_fieldlists.'

   As a side effect, each member function in the signature gets the
   `decl.ignored' bit turned on, so we don't output debug info for it.  */

void
append_signature_fields (list_of_fieldlists)
     tree list_of_fieldlists;
{
  tree l, x;
  tree last_x = NULL_TREE;
  tree mfptr;
  tree last_mfptr;
  tree mfptr_list = NULL_TREE;
	      
  /* For signatures it should actually be only a list with one element.  */
  for (l = list_of_fieldlists; l; l = TREE_CHAIN (l))
    {
      for (x = TREE_VALUE (l); x; x = TREE_CHAIN (x))
	{
	  if (TREE_CODE (x) == FUNCTION_DECL)
	    {
	      mfptr = build_member_function_pointer (x);
	      DECL_MEMFUNC_POINTER_TO (x) = mfptr;
	      DECL_MEMFUNC_POINTING_TO (mfptr) = x;
	      DECL_IGNORED_P (x) = 1;
	      DECL_IN_AGGR_P (mfptr) = 1;
	      if (! mfptr_list)
		mfptr_list = last_mfptr = mfptr;
	      else
		{
		  TREE_CHAIN (last_mfptr) = mfptr;
		  last_mfptr = mfptr;
		}
	    }
	  last_x = x;
	}
    }

  /* Append the lists.  */
  if (last_x && mfptr_list)
    {
      TREE_CHAIN (last_x) = mfptr_list;
      TREE_CHAIN (last_mfptr) = NULL_TREE;
    }
}

/* Compare the types of a signature member function and a class member
   function.  Returns 1 if the types are in the C++ `<=' relationship.

   If we have a signature pointer/reference as argument or return type
   we don't want to do a recursive conformance check.  The conformance
   check only succeeds if both LHS and RHS refer to the same signature
   pointer.  Otherwise we need to keep information about parameter types
   around at run time to initialize the signature table correctly.  */

static int
match_method_types (sig_mtype, class_mtype)
     tree sig_mtype, class_mtype;
{
  tree sig_return_type = TREE_TYPE (sig_mtype);
  tree sig_arg_types = TYPE_ARG_TYPES (sig_mtype);
  tree class_return_type = TREE_TYPE (class_mtype);
  tree class_arg_types = TYPE_ARG_TYPES (class_mtype);

  /* The return types have to be the same.  */
  if (! comptypes (sig_return_type, class_return_type, 1))
    return 0;

  /* Compare the first argument `this.'  */
  {
    /* Get the type of what the `optr' is pointing to.  */
    tree sig_this =
      TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_VALUE (sig_arg_types))));
    tree class_this = TREE_VALUE (class_arg_types);

    if (TREE_CODE (class_this) == RECORD_TYPE)	/* Is `this' a sig ptr?  */
      class_this = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (class_this)));
    else
      class_this = TREE_TYPE (class_this);

    /* If a signature method's `this' is const or volatile, so has to be
       the corresponding class method's `this.'  */
    if ((TYPE_READONLY (sig_this) && ! TYPE_READONLY (class_this))
	|| (TYPE_VOLATILE (sig_this) && ! TYPE_VOLATILE (class_this)))
      return 0;
  }

  sig_arg_types = TREE_CHAIN (sig_arg_types);
  class_arg_types = TREE_CHAIN (class_arg_types);

  /* The number of arguments and the argument types have to be the same.  */
  return compparms (sig_arg_types, class_arg_types, 3);
}

/* Undo casts of opaque type variables to the RHS types.  */
static void
undo_casts (sig_ty)
     tree sig_ty;
{
  tree field = TYPE_FIELDS (sig_ty);

  /* Since all the FIELD_DECLs for the signature table entries are at the end
     of the chain (see `append_signature_fields'), we can do it this way.  */
  for (; field && TREE_CODE (field) != FIELD_DECL; field = TREE_CHAIN (field))
    if (TYPE_MAIN_VARIANT (TREE_TYPE (field)) == opaque_type_node)
      TREE_TYPE (TREE_TYPE (field)) = TREE_TYPE (ptr_type_node);
}

/* Do the type checking necessary to see whether the `rhs' conforms to
   the lhs's `sig_ty'.  Depending on the type of `rhs' return a NULL_TREE,
   an integer_zero_node, a constructor, or an expression offsetting the
   `rhs' signature table.  */

static tree
build_signature_table_constructor (sig_ty, rhs)
     tree sig_ty, rhs;
{
  tree rhstype = TREE_TYPE (rhs);
  tree sig_field = TYPE_FIELDS (sig_ty);
  tree result = NULL_TREE;
  tree first_rhs_field = NULL_TREE;
  tree last_rhs_field;
  int sig_ptr_p = IS_SIGNATURE (rhstype);
  int offset_p = sig_ptr_p;

  rhstype = sig_ptr_p ? rhstype : TREE_TYPE (rhstype);

  if (CLASSTYPE_TAGS (sig_ty))
    {
      sorry ("conformance check with signature containing class declarations");
      return error_mark_node;
    }

  for (; sig_field; sig_field = TREE_CHAIN (sig_field))
    {
      tree basetype_path, baselink, basetypes;
      tree sig_method, sig_mname, sig_mtype;
      tree rhs_method, tbl_entry;

      if (TREE_CODE (sig_field) == TYPE_DECL)
	{
	  tree sig_field_type = TREE_TYPE (sig_field);

	  if (TYPE_MAIN_VARIANT (sig_field_type) == opaque_type_node)
	    {
	      /* We've got an opaque type here.  */
	      tree oty_name = DECL_NAME (sig_field);
	      tree oty_type = lookup_field (rhstype, oty_name, 1, 1);

	      if (oty_type == NULL_TREE || oty_type == error_mark_node)
		{
		  cp_error ("class `%T' does not contain type `%T'",
			    rhstype, oty_type);
		  undo_casts (sig_ty);
		  return error_mark_node;
		}
	      oty_type = TREE_TYPE (oty_type);

	      /* Cast `sig_field' to be of type `oty_type'.  This will be
		 undone in `undo_casts' by walking over all the TYPE_DECLs.  */
	      TREE_TYPE (sig_field_type) = TREE_TYPE (oty_type);
	    }
	  /* If we don't have an opaque type, we can ignore the `typedef'.  */
	  continue;
	}

      /* Find the signature method corresponding to `sig_field'.  */
      sig_method = DECL_MEMFUNC_POINTING_TO (sig_field);
      sig_mname = DECL_NAME (sig_method);
      sig_mtype = TREE_TYPE (sig_method);

      basetype_path = TYPE_BINFO (rhstype);
      baselink = lookup_fnfields (basetype_path, sig_mname, 0);
      if (baselink == NULL_TREE || baselink == error_mark_node)
	{
	  if (! IS_DEFAULT_IMPLEMENTATION (sig_method))
	    {
	      cp_error ("class `%T' does not contain method `%D'",
			rhstype, sig_mname);
	      undo_casts (sig_ty);
	      return error_mark_node;
	    }
	  else
	    {
	      /* We use the signature's default implementation.  */
	      rhs_method = sig_method;
	    }
	}
      else
	{
	  /* Find the class method of the correct type.  */

	  basetypes = TREE_PURPOSE (baselink);
	  if (TREE_CODE (basetypes) == TREE_LIST)
	    basetypes = TREE_VALUE (basetypes);

	  rhs_method = TREE_VALUE (baselink);
	  for (; rhs_method; rhs_method = TREE_CHAIN (rhs_method))
	    if (sig_mname == DECL_NAME (rhs_method)
		&& ! DECL_STATIC_FUNCTION_P (rhs_method)
		&& match_method_types (sig_mtype, TREE_TYPE (rhs_method)))
	      break;

	  if (rhs_method == NULL_TREE
	      || (compute_access (basetypes, rhs_method)
		  != access_public))
	    {
	      error ("class `%s' does not contain a method conforming to `%s'",
		     TYPE_NAME_STRING (rhstype),
		     fndecl_as_string (NULL, sig_method, 1));
	      undo_casts (sig_ty);
	      return error_mark_node;
	    }
	}

      if (sig_ptr_p && rhs_method != sig_method)
	{
	  tree rhs_field = DECL_MEMFUNC_POINTER_TO (rhs_method);

	  if (first_rhs_field == NULL_TREE)
	    {
	      first_rhs_field = rhs_field;
	      last_rhs_field = rhs_field;
	    }
	  else if (TREE_CHAIN (last_rhs_field) == rhs_field)
	    last_rhs_field = rhs_field;
	  else
	    offset_p = 0;
	  
	  tbl_entry = build_component_ref (rhs, DECL_NAME (rhs_field),
					   NULL_TREE, 1);
	}
      else
	{
	  tree code, offset, pfn;

	  if (rhs_method == sig_method)
	    {
	      code = integer_two_node;
	      offset = integer_zero_node;
	      pfn = build_unary_op (ADDR_EXPR, rhs_method, 0);
	      TREE_TYPE (pfn) = ptr_type_node;
	      offset_p = 0;	/* we can't offset the rhs sig table */
	    }
	  else if (DECL_VINDEX (rhs_method))
	    {
	      code = integer_one_node;
	      offset = DECL_VINDEX (rhs_method);
	      pfn = null_pointer_node;
	    }
	  else
	    {
	      code = integer_zero_node;
	      offset = integer_zero_node;
	      pfn = build_unary_op (ADDR_EXPR, rhs_method, 0);
	      TREE_TYPE (pfn) = ptr_type_node;
	      TREE_ADDRESSABLE (rhs_method) = 1;
	    }

	  tbl_entry = tree_cons (NULL_TREE, code,
				 tree_cons (NULL_TREE, offset,
					    build_tree_list (NULL_TREE, pfn)));
	  tbl_entry = build_nt (CONSTRUCTOR, NULL_TREE, tbl_entry);
	  TREE_HAS_CONSTRUCTOR (tbl_entry) = 1;
	  TREE_CONSTANT (tbl_entry) = 1;
	}

      /* Chain those function address expressions together.  */
      if (result)
	result = tree_cons (NULL_TREE, tbl_entry, result);
      else
	result = build_tree_list (NULL_TREE, tbl_entry);
    }

  if (result == NULL_TREE)
    {
      /* The signature was empty, we don't need a signature table.  */
      undo_casts (sig_ty);
      return NULL_TREE;
    }

  if (offset_p)
    {
      if (first_rhs_field == TYPE_FIELDS (rhstype))
	{
	  /* The sptr field on the lhs can be copied from the rhs.  */
	  undo_casts (sig_ty);
	  return integer_zero_node;
	}
      else
	{
	  /* The sptr field on the lhs will point into the rhs sigtable.  */
	  undo_casts (sig_ty);
	  return build_component_ref (rhs, DECL_NAME (first_rhs_field),
				      NULL_TREE, 0);
	}
    }

  /* We need to construct a new signature table.  */
  result = build_nt (CONSTRUCTOR, NULL_TREE, nreverse (result));
  TREE_HAS_CONSTRUCTOR (result) = 1;
  TREE_CONSTANT (result) = !sig_ptr_p;

  undo_casts (sig_ty);
  return result;
}

/* Build a signature table declaration and initialize it or return an
   existing one if we built one already.  If we don't get a constructor
   as initialization expression, we don't need a new signature table
   variable and just hand back the init expression.

   The declaration processing is done by hand instead of using `finish_decl'
   so that we can make signature pointers global variables instead of
   static ones.  */

static tree
build_sigtable (sig_type, rhs_type, init_from)
     tree sig_type, rhs_type, init_from;
{
  tree name = NULL_TREE;
  tree decl = NULL_TREE;
  tree init_expr;

  push_obstacks_nochange ();
  end_temporary_allocation ();

  if (! IS_SIGNATURE (rhs_type))
    {
      name = get_sigtable_name (sig_type, rhs_type);
      decl = IDENTIFIER_GLOBAL_VALUE (name);
    }
  if (decl == NULL_TREE)
    {
      tree init;

      /* We allow only one signature table to be generated for signatures
	 with opaque types.  Otherwise we create a loophole in the type
	 system since we could cast data from one classes implementation
	 of the opaque type to that of another class.  */
      if (SIGNATURE_HAS_OPAQUE_TYPEDECLS (sig_type)
	  && SIGTABLE_HAS_BEEN_GENERATED (sig_type))
	{
	  error ("signature with opaque type implemented by multiple classes");
	  return error_mark_node;
	}
      SIGTABLE_HAS_BEEN_GENERATED (sig_type) = 1;

      init_expr = build_signature_table_constructor (sig_type, init_from);
      if (init_expr == NULL_TREE || TREE_CODE (init_expr) != CONSTRUCTOR)
	return init_expr;

      if (name == NULL_TREE)
	name = get_sigtable_name (sig_type, rhs_type);
      {
	tree context = current_function_decl;

	/* Make the signature table global, not just static in whichever
	   function a signature pointer/ref is used for the first time.  */
	current_function_decl = NULL_TREE;
	decl = pushdecl_top_level (build_decl (VAR_DECL, name, sig_type));
	current_function_decl = context;
      }
      IDENTIFIER_GLOBAL_VALUE (name) = decl;
      store_init_value (decl, init_expr);
      if (IS_SIGNATURE (rhs_type))
	{
	  init = DECL_INITIAL (decl);
	  DECL_INITIAL (decl) = error_mark_node;
	}

      DECL_ALIGN (decl) = MAX (TYPE_ALIGN (double_type_node),
			       DECL_ALIGN (decl));
#if 0
      /* GDB-4.7 doesn't find the initialization value of a signature table
	 when it is constant.  */
      TREE_READONLY (decl) = 1;
#endif
      TREE_STATIC (decl) = 1;
      TREE_USED (decl) = 1;

      make_decl_rtl (decl, NULL, 1);
      if (IS_SIGNATURE (rhs_type))
	expand_static_init (decl, init);
    }

  pop_obstacks ();

  return decl;
}

/* Create a constructor or modify expression if the LHS of an assignment
   is a signature pointer or a signature reference.  If LHS is a record
   type node, we build a constructor, otherwise a compound expression.  */

tree
build_signature_pointer_constructor (lhs, rhs)
     tree lhs, rhs;
{
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;
  int initp = (TREE_CODE (lhs) == RECORD_TYPE);
  tree lhstype = initp ? lhs : TREE_TYPE (lhs);
  tree rhstype = TREE_TYPE (rhs);
  tree sig_ty  = SIGNATURE_TYPE (lhstype);
  tree sig_tbl, sptr_expr, optr_expr, vptr_expr;
  tree result;

  if (! ((TREE_CODE (rhstype) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (rhstype)) == RECORD_TYPE)
	 || (TYPE_LANG_SPECIFIC (rhstype) &&
	     (IS_SIGNATURE_POINTER (rhstype)
	      || IS_SIGNATURE_REFERENCE (rhstype)))))
    {
      error ("invalid assignment to signature pointer or reference");
      return error_mark_node;
    }

  if (TYPE_SIZE (sig_ty) == NULL_TREE)
    {
      cp_error ("undefined signature `%T' used in signature %s declaration",
		sig_ty,
		IS_SIGNATURE_POINTER (lhstype) ? "pointer" : "reference");
      return error_mark_node;
    }

  /* If SIG_TY is permanent, make the signature table constructor and
     the signature pointer/reference constructor permanent too.  */
  if (TREE_PERMANENT (sig_ty))
    {
      current_obstack = &permanent_obstack;
      saveable_obstack = &permanent_obstack;
    }

  if (TYPE_LANG_SPECIFIC (rhstype) &&
      (IS_SIGNATURE_POINTER (rhstype) || IS_SIGNATURE_REFERENCE (rhstype)))
    {
      if (SIGNATURE_TYPE (rhstype) == sig_ty)
	{
	  /* LHS and RHS are signature pointers/refs of the same signature.  */
	  optr_expr = build_optr_ref (rhs);
	  sptr_expr = build_sptr_ref (rhs);
	  vptr_expr = build_vptr_ref (rhs);
	}
      else
	{
	  /* We need to create a new signature table and copy
	     elements from the rhs signature table.  */
	  tree rhs_sptr_ref = build_sptr_ref (rhs);
	  tree rhs_tbl = build1 (INDIRECT_REF, SIGNATURE_TYPE (rhstype),
				 rhs_sptr_ref);

	  sig_tbl = build_sigtable (sig_ty, SIGNATURE_TYPE (rhstype), rhs_tbl);
	  if (sig_tbl == error_mark_node)
	    return error_mark_node;

	  optr_expr = build_optr_ref (rhs);
	  if (sig_tbl == NULL_TREE)
	    /* The signature was empty.  The signature pointer is
	       pretty useless, but the user has been warned.  */
	    sptr_expr = copy_node (null_pointer_node);
	  else if (sig_tbl == integer_zero_node)
	    sptr_expr = rhs_sptr_ref;
	  else
	    sptr_expr = build_unary_op (ADDR_EXPR, sig_tbl, 0);
	  TREE_TYPE (sptr_expr) = build_pointer_type (sig_ty);
	  vptr_expr = build_vptr_ref (rhs);
	}
    }
  else
    {
      tree rhs_vptr;

      if (TYPE_USES_COMPLEX_INHERITANCE (TREE_TYPE (rhstype)))
	{
	  sorry ("class with multiple inheritance as implementation of signature");
	  return error_mark_node;
	}

      sig_tbl = build_sigtable (sig_ty, TREE_TYPE (rhstype), rhs);
      if (sig_tbl == error_mark_node)
	return error_mark_node;

      optr_expr = rhs;
      if (sig_tbl == NULL_TREE)
	/* The signature was empty.  The signature pointer is
	   pretty useless, but the user has been warned.  */
	{
	  sptr_expr = copy_node (null_pointer_node);
	  TREE_TYPE (sptr_expr) = build_pointer_type (sig_ty);
	}
      else
	sptr_expr = build_unary_op (ADDR_EXPR, sig_tbl, 0);
      if (CLASSTYPE_VFIELD (TREE_TYPE (rhstype)))
	{
	  rhs_vptr = DECL_NAME (CLASSTYPE_VFIELD (TREE_TYPE (rhstype)));
	  vptr_expr = build_component_ref (build_indirect_ref (rhs, 0),
					   rhs_vptr, NULL_TREE, 0);
	}
      else
	vptr_expr = copy_node (null_pointer_node);
      TREE_TYPE (vptr_expr) = build_pointer_type (vtbl_type_node);
    }

  if (initp)
    {
      result = tree_cons (NULL_TREE, optr_expr,
			  tree_cons (NULL_TREE, sptr_expr,
				     build_tree_list (NULL_TREE, vptr_expr)));
      result = build_nt (CONSTRUCTOR, NULL_TREE, result);
      TREE_HAS_CONSTRUCTOR (result) = 1;
      result = digest_init (lhstype, result, 0);
    }
  else
    {
      if (TREE_READONLY (lhs) || TYPE_READONLY (lhstype))
	  readonly_error (lhs, "assignment", 0);

      optr_expr = build_modify_expr (build_optr_ref (lhs), NOP_EXPR,
				     optr_expr);
      sptr_expr = build_modify_expr (build_sptr_ref (lhs), NOP_EXPR,
				     sptr_expr);
      vptr_expr = build_modify_expr (build_vptr_ref (lhs), NOP_EXPR,
				     vptr_expr);

      result = tree_cons (NULL_TREE, optr_expr,
			  tree_cons (NULL_TREE, sptr_expr,
				     tree_cons (NULL_TREE, vptr_expr,
						build_tree_list (NULL_TREE,
								 lhs))));
      result = build_compound_expr (result);
    }

  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;
  return result;
}

/* Build a temporary variable declaration for the instance of a signature
   member function call if it isn't a declaration node already.  Simply
   using a SAVE_EXPR doesn't work since we need `this' in both branches
   of a conditional expression.  */

static tree
save_this (instance)
     tree instance;
{
  tree decl;

  if (TREE_CODE_CLASS (TREE_CODE (instance)) == 'd')
    decl = instance;
  else
    {
      decl = build_decl (VAR_DECL, NULL_TREE, TREE_TYPE (instance));
      DECL_REGISTER (decl) = 1;
      layout_decl (decl, 0);
      expand_decl (decl);
    }

  return decl;
}

/* Build a signature member function call.  Looks up the signature table
   entry corresponding to FUNCTION.  Depending on the value of the CODE
   field, either call the function in PFN directly, or use OFFSET to
   index INSTANCE's virtual function table.  */

tree
build_signature_method_call (basetype, instance, function, parms)
     tree basetype, instance, function, parms;
{
  tree saved_instance = save_this (instance);	/* Create temp for `this'.  */
  tree signature_tbl_ptr = build_sptr_ref (saved_instance);
  tree sig_field_name = DECL_NAME (DECL_MEMFUNC_POINTER_TO (function));
  tree basetype_path = TYPE_BINFO (basetype);
  tree tbl_entry = build_component_ref (build1 (INDIRECT_REF, basetype,
						signature_tbl_ptr),
					sig_field_name, basetype_path, 1);
  tree code, offset, pfn, vfn;
  tree deflt_call = NULL_TREE, direct_call, virtual_call, result;

  code = build_component_ref (tbl_entry, get_identifier (SIGTABLE_CODE_NAME),
			     NULL_TREE, 1);
  offset = build_component_ref (tbl_entry,
				get_identifier (SIGTABLE_OFFSET_NAME),
			     NULL_TREE, 1);
  pfn = build_component_ref (tbl_entry, get_identifier (SIGTABLE_PFN_NAME),
			     NULL_TREE, 1);
  TREE_TYPE (pfn) = build_pointer_type (TREE_TYPE (function)); 

  if (IS_DEFAULT_IMPLEMENTATION (function))
    {
      pfn = save_expr (pfn);
      deflt_call = build_function_call (pfn,
					tree_cons (NULL_TREE, saved_instance,
						   TREE_CHAIN (parms)));
    }

  {
    /* Cast the signature method to have `this' of a normal pointer type.  */
    tree old_this = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (TREE_TYPE (pfn))));

    TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (TREE_TYPE (pfn)))) =
      build_type_variant (TYPE_POINTER_TO (basetype),
			  TYPE_READONLY (old_this),
			  TYPE_VOLATILE (old_this));

    direct_call = build_function_call (pfn, parms);

    vfn = build_vfn_ref (&TREE_VALUE (parms), saved_instance, offset);
    TREE_TYPE (vfn) = build_pointer_type (TREE_TYPE (function));
    virtual_call = build_function_call (vfn, parms);

    /* Undo the cast, make `this' a signature pointer again.  */
    TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (TREE_TYPE (pfn)))) = old_this;
  }

  /* Once the function was found, there should be no reason why we
     couldn't build the member function pointer call.  */
  if (!direct_call || direct_call == error_mark_node
      || !virtual_call || virtual_call == error_mark_node
      || (IS_DEFAULT_IMPLEMENTATION (function)
	  && (!deflt_call || deflt_call == error_mark_node)))
    {
      compiler_error ("cannot build call of signature member function `%s'",
		      fndecl_as_string (NULL, function, 1));
      return error_mark_node;
    }

  if (IS_DEFAULT_IMPLEMENTATION (function))
    {
      tree test = build_binary_op_nodefault (EQ_EXPR, code, integer_one_node,
					     EQ_EXPR);
      result = build_conditional_expr (code,
				       build_conditional_expr (test,
							       virtual_call,
							       deflt_call),
				       direct_call);
    }
  else
    result = build_conditional_expr (code, virtual_call, direct_call);

  /* If we created a temporary variable for `this', initialize it first.  */
  if (instance != saved_instance)
    result = build (COMPOUND_EXPR, TREE_TYPE (result),
		    build_modify_expr (saved_instance, NOP_EXPR, instance),
		    result);

  return result;
}

/* Create a COMPONENT_REF expression for referencing the OPTR field
   of a signature pointer or reference.  */

tree
build_optr_ref (instance)
     tree instance;
{
  tree field = get_identifier (SIGNATURE_OPTR_NAME);

  return build_component_ref (instance, field, NULL_TREE, 1);
}

/* Create a COMPONENT_REF expression for referencing the SPTR field
   of a signature pointer or reference.  */

tree
build_sptr_ref (instance)
     tree instance;
{
  tree field = get_identifier (SIGNATURE_SPTR_NAME);

  return build_component_ref (instance, field, NULL_TREE, 1);
}

/* Create a COMPONENT_REF expression for referencing the VPTR field
   of a signature pointer or reference.  */

tree
build_vptr_ref (instance)
     tree instance;
{
  tree field = get_identifier (SIGNATURE_VPTR_NAME);

  return build_component_ref (instance, field, NULL_TREE, 1);
}
