/* RunTime Type Identification
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.
   Mostly written by Jason Merrill (jason@cygnus.com).

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "flags.h"
#include "output.h"
#include "assert.h"
#include "toplev.h"

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

/* Accessors for the type_info objects. We need to remember several things
   about each of the type_info types. The global tree nodes such as
   bltn_desc_type_node are TREE_LISTs, and these macros are used to access
   the required information. */
/* The RECORD_TYPE of a type_info derived class. */
#define TINFO_PSEUDO_TYPE(NODE) TREE_TYPE (NODE)
/* The VAR_DECL of the vtable for the type_info derived class. */
#define TINFO_VTABLE_DECL(NODE) TREE_VALUE (NODE)

extern struct obstack permanent_obstack;

static tree build_headof_sub PARAMS((tree));
static tree build_headof PARAMS((tree));
static tree get_tinfo_var PARAMS((tree));
static tree ifnonnull PARAMS((tree, tree));
static tree tinfo_name PARAMS((tree));
static tree get_base_offset PARAMS((tree, tree));
static tree build_dynamic_cast_1 PARAMS((tree, tree));
static void expand_si_desc PARAMS((tree, tree));
static void expand_class_desc PARAMS((tree, tree));
static void expand_attr_desc PARAMS((tree, tree));
static void expand_ptr_desc PARAMS((tree, tree));
static void expand_generic_desc PARAMS((tree, tree, const char *));
static tree throw_bad_cast PARAMS((void));
static tree throw_bad_typeid PARAMS((void));
static tree get_tinfo_decl_dynamic PARAMS((tree));
static tree tinfo_from_decl PARAMS((tree));
static int qualifier_flags PARAMS((tree));
static int target_incomplete_p PARAMS((tree));
static tree tinfo_base_init PARAMS((tree, tree));
static tree generic_initializer PARAMS((tree, tree));
static tree ptr_initializer PARAMS((tree, tree, int *));
static tree ptmd_initializer PARAMS((tree, tree, int *));
static tree dfs_class_hint_mark PARAMS ((tree, void *));
static tree dfs_class_hint_unmark PARAMS ((tree, void *));
static int class_hint_flags PARAMS((tree));
static tree class_initializer PARAMS((tree, tree, tree));
static tree synthesize_tinfo_var PARAMS((tree, tree));
static tree create_real_tinfo_var PARAMS((tree, tree, tree, int));
static tree create_pseudo_type_info PARAMS((const char *, int, ...));
static tree get_vmi_pseudo_type_info PARAMS((int));
static void create_tinfo_types PARAMS((void));

static int doing_runtime = 0;

void
init_rtti_processing ()
{
  if (flag_honor_std)
    push_namespace (get_identifier ("std"));
  type_info_type_node = xref_tag
    (class_type_node, get_identifier ("type_info"), 1);
  if (flag_honor_std)
    pop_namespace ();
  if (!new_abi_rtti_p ())
    {
      tinfo_decl_id = get_identifier ("__tf");
      tinfo_decl_type = build_function_type
        (build_reference_type
          (build_qualified_type
            (type_info_type_node, TYPE_QUAL_CONST)),
         void_list_node);
      tinfo_var_id = get_identifier ("__ti");
    }
  else
    {
      /* FIXME: These identifier prefixes are not set in stone yet.  */
      tinfo_decl_id = get_identifier ("__ti");
      tinfo_var_id = get_identifier ("__tn");
      tinfo_decl_type = build_qualified_type
                          (type_info_type_node, TYPE_QUAL_CONST);
    }
}

/* Given a pointer to an object with at least one virtual table
   pointer somewhere, return a pointer to a possible sub-object that
   has a virtual table pointer in it that is the vtable parent for
   that sub-object.  */

static tree
build_headof_sub (exp)
     tree exp;
{
  tree type = TREE_TYPE (TREE_TYPE (exp));
  tree basetype = CLASSTYPE_RTTI (type);
  tree binfo = get_binfo (basetype, type, 0);

  exp = convert_pointer_to_real (binfo, exp);
  return exp;
}

/* Given the expression EXP of type `class *', return the head of the
   object pointed to by EXP with type cv void*, if the class has any
   virtual functions (TYPE_POLYMORPHIC_P), else just return the
   expression.  */

static tree
build_headof (exp)
     tree exp;
{
  tree type = TREE_TYPE (exp);
  tree aref;
  tree offset;
  tree index;

  my_friendly_assert (TREE_CODE (type) == POINTER_TYPE, 20000112);
  type = TREE_TYPE (type);

  if (!TYPE_POLYMORPHIC_P (type))
    return exp;
  if (CLASSTYPE_COM_INTERFACE (type))
    {
      cp_error ("RTTI not supported for COM interface type `%T'", type);
      return error_mark_node;
    }

  /* If we don't have rtti stuff, get to a sub-object that does.  */
  if (!CLASSTYPE_VFIELDS (TREE_TYPE (TREE_TYPE (exp))))
    exp = build_headof_sub (exp);

  /* We use this a couple of times below, protect it.  */
  exp = save_expr (exp);

  /* Under the new ABI, the offset-to-top field is at index -2 from
     the vptr.  */
  if (new_abi_rtti_p ())
    index = build_int_2 (-2, -1);
  /* But under the old ABI, it is at offset zero.  */
  else
    index = integer_zero_node;

  aref = build_vtbl_ref (build_indirect_ref (exp, NULL_PTR), index);

  if (flag_vtable_thunks)
    offset = aref;
  else
    offset = build_component_ref (aref, delta_identifier, NULL_TREE, 0);

  type = build_qualified_type (ptr_type_node, 
			       CP_TYPE_QUALS (TREE_TYPE (exp)));
  return build (PLUS_EXPR, type, exp,
		cp_convert (ptrdiff_type_node, offset));
}

/* Get a bad_cast node for the program to throw...

   See libstdc++/exception.cc for __throw_bad_cast */

static tree
throw_bad_cast ()
{
  tree fn = get_identifier ("__throw_bad_cast");
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    fn = push_throw_library_fn (fn, build_function_type (ptr_type_node,
							 void_list_node));
  
  return build_call (fn, NULL_TREE);
}

static tree
throw_bad_typeid ()
{
  tree fn = get_identifier ("__throw_bad_typeid");
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    {
      tree t = build_qualified_type (type_info_type_node, TYPE_QUAL_CONST);
      t = build_function_type (build_reference_type (t), void_list_node);
      fn = push_throw_library_fn (fn, t);
    }

  return build_call (fn, NULL_TREE);
}

/* Return a pointer to type_info function associated with the expression EXP.
   If EXP is a reference to a polymorphic class, return the dynamic type;
   otherwise return the static type of the expression.  */

static tree
get_tinfo_decl_dynamic (exp)
     tree exp;
{
  tree type;
  
  if (exp == error_mark_node)
    return error_mark_node;

  type = TREE_TYPE (exp);

  /* peel back references, so they match.  */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* Peel off cv qualifiers.  */
  type = TYPE_MAIN_VARIANT (type);
  
  if (type != void_type_node)
    type = complete_type_or_else (type, exp);
  
  if (!type)
    return error_mark_node;

  /* If exp is a reference to polymorphic type, get the real type_info.  */
  if (TYPE_POLYMORPHIC_P (type) && ! resolves_to_fixed_type_p (exp, 0))
    {
      /* build reference to type_info from vtable.  */
      tree t;
      tree index;

      if (! flag_rtti)
	error ("taking dynamic typeid of object with -fno-rtti");
      if (CLASSTYPE_COM_INTERFACE (type))
	{
	  cp_error ("RTTI not supported for COM interface type `%T'", type);
	  return error_mark_node;
	}

      /* If we don't have rtti stuff, get to a sub-object that does.  */
      if (! CLASSTYPE_VFIELDS (type))
	{
      	  exp = build_unary_op (ADDR_EXPR, exp, 0);
	  exp = build_headof_sub (exp);
	  exp = build_indirect_ref (exp, NULL_PTR);
	}

      /* The RTTI information is always in the vtable, but it's at
	 different indices depending on the ABI.  */
      if (new_abi_rtti_p ())
	index = minus_one_node;
      else if (flag_vtable_thunks)
	index = integer_one_node;
      else
	index = integer_zero_node;
      t = build_vfn_ref ((tree *) 0, exp, index);
      TREE_TYPE (t) = build_pointer_type (tinfo_decl_type);
      return t;
    }

  /* otherwise return the type_info for the static type of the expr.  */
  exp = get_tinfo_decl (TYPE_MAIN_VARIANT (type));
  return build_unary_op (ADDR_EXPR, exp, 0);
}

tree
build_typeid (exp)
     tree exp;
{
  tree cond = NULL_TREE;
  int nonnull = 0;

  if (! flag_rtti)
    {
      error ("cannot use typeid with -fno-rtti");
      return error_mark_node;
    }
  
  if (!COMPLETE_TYPE_P (type_info_type_node))
    {
      error ("must #include <typeinfo> before using typeid");
      return error_mark_node;
    }
  
  if (processing_template_decl)
    return build_min_nt (TYPEID_EXPR, exp);

  if (TREE_CODE (exp) == INDIRECT_REF
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == POINTER_TYPE
      && TYPE_POLYMORPHIC_P (TREE_TYPE (exp))
      && ! resolves_to_fixed_type_p (exp, &nonnull)
      && ! nonnull)
    {
      exp = stabilize_reference (exp);
      cond = cp_convert (boolean_type_node, TREE_OPERAND (exp, 0));
    }

  exp = get_tinfo_decl_dynamic (exp);

  if (exp == error_mark_node)
    return error_mark_node;

  exp = tinfo_from_decl (exp);

  if (cond)
    {
      tree bad = throw_bad_typeid ();

      exp = build (COND_EXPR, TREE_TYPE (exp), cond, exp, bad);
    }

  return convert_from_reference (exp);
}

static tree
get_tinfo_var (type)
     tree type;
{
  tree tname = build_overload_with_type (tinfo_var_id, type);
  tree arrtype;
  int size;

  my_friendly_assert (!new_abi_rtti_p (), 20000118);
  if (IDENTIFIER_GLOBAL_VALUE (tname))
    return IDENTIFIER_GLOBAL_VALUE (tname);
    
  /* Figure out how much space we need to allocate for the type_info object.
     If our struct layout or the type_info classes are changed, this will
     need to be modified.  */
  if (TYPE_QUALS (type) != TYPE_UNQUALIFIED)
    size = 3 * POINTER_SIZE + INT_TYPE_SIZE;
  else if (TREE_CODE (type) == POINTER_TYPE
	   && ! (TREE_CODE (TREE_TYPE (type)) == OFFSET_TYPE
		 || TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE))
    size = 3 * POINTER_SIZE;
  else if (IS_AGGR_TYPE (type))
    {
      if (CLASSTYPE_N_BASECLASSES (type) == 0)
	size = 2 * POINTER_SIZE;
      else if (! TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (type)
	       && (TREE_VIA_PUBLIC
		   (TREE_VEC_ELT (TYPE_BINFO_BASETYPES (type), 0))))
	size = 3 * POINTER_SIZE;
      else
	size = 3 * POINTER_SIZE + TYPE_PRECISION (sizetype);
    }
  else
    size = 2 * POINTER_SIZE;

  /* The type for a character array of the appropriate size.  */
  arrtype = build_cplus_array_type
    (unsigned_char_type_node,
     build_index_type (size_int (size / BITS_PER_UNIT - 1)));

  return declare_global_var (tname, arrtype);
}

/* Generate the NTBS name of a type.  */
static tree
tinfo_name (type)
     tree type;
{
  const char *name = build_overload_name (type, 1, 1);
  tree name_string = combine_strings (build_string (strlen (name) + 1, name));
  return name_string;
}

/* Returns a decl for a function or variable which can be used to obtain a
   type_info object for TYPE.  The old-abi uses functions, the new-abi
   uses the type_info object directly.  You can take the address of the
   returned decl, to save the decl.  To use the decl call
   tinfo_from_decl.  You must arrange that the decl is mark_used, if
   actually use it --- decls in vtables are only used if the vtable is
   output.  */

tree
get_tinfo_decl (type)
     tree type;
{
  tree name;
  tree d;

  if (TREE_CODE (type) == OFFSET_TYPE)
    type = TREE_TYPE (type);
  if (TREE_CODE (type) == METHOD_TYPE)
    type = build_function_type (TREE_TYPE (type),
				TREE_CHAIN (TYPE_ARG_TYPES (type)));

  name = build_overload_with_type (tinfo_decl_id, type);

  d = IDENTIFIER_GLOBAL_VALUE (name);
  if (d)
    /* OK */;
  else if (!new_abi_rtti_p ())
    {
      /* The tinfo decl is a function returning a reference to the type_info
         object.  */
      d = push_library_fn (name, tinfo_decl_type);
      DECL_NOT_REALLY_EXTERN (d) = 1;
      SET_DECL_TINFO_FN_P (d);
      TREE_TYPE (name) = type;
      defer_fn (d);
    }
  else
    {
      /* The tinfo decl is the type_info object itself.  We make all
         tinfo objects look as type_info, even though they will end up
         being a subclass of that when emitted.  This means the we'll
         erroneously think we know the dynamic type -- be careful in the
         runtime.  */
      d = build_lang_decl (VAR_DECL, name, tinfo_decl_type);
      
      DECL_ARTIFICIAL (d) = 1;
      DECL_ALIGN (d) = TYPE_ALIGN (ptr_type_node);
      TREE_READONLY (d) = 1;
      TREE_STATIC (d) = 1;
      DECL_EXTERNAL (d) = 1;
      TREE_PUBLIC (d) = 1;
      comdat_linkage (d);
      DECL_ASSEMBLER_NAME (d) = DECL_NAME (d);
      cp_finish_decl (d, NULL_TREE, NULL_TREE, 0);

      pushdecl_top_level (d);
      /* Remember the type it is for.  */
      TREE_TYPE (name) = type;
      TREE_USED (name) = 1;
    }
  return d;
}

/* Given an expr produced by get_tinfo_decl, return an expr which
   produces a reference to the type_info object.  */

static tree
tinfo_from_decl (expr)
     tree expr;
{
  tree t;
  
  if (!new_abi_rtti_p ())
    t = build_call (expr, NULL_TREE);
  else if (TREE_CODE (TREE_TYPE (expr)) == POINTER_TYPE)
    t = build_indirect_ref (expr, NULL);
  else
    t = expr;
  
  return t;
}

tree
get_typeid_1 (type)
     tree type;
{
  tree t;

  t = get_tinfo_decl (type);
  t = tinfo_from_decl (t);
  return convert_from_reference (t);
}
  
/* Return the type_info object for TYPE.  */

tree
get_typeid (type)
     tree type;
{
  if (type == error_mark_node)
    return error_mark_node;

  if (!COMPLETE_TYPE_P (type_info_type_node))
    {
      error ("must #include <typeinfo> before using typeid");
      return error_mark_node;
    }
  
  if (processing_template_decl)
    return build_min_nt (TYPEID_EXPR, type);

  /* If the type of the type-id is a reference type, the result of the
     typeid expression refers to a type_info object representing the
     referenced type.  */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* The top-level cv-qualifiers of the lvalue expression or the type-id
     that is the operand of typeid are always ignored.  */
  type = TYPE_MAIN_VARIANT (type);

  if (type != void_type_node)
    type = complete_type_or_else (type, NULL_TREE);
  
  if (!type)
    return error_mark_node;

  return get_typeid_1 (type);
}

/* Check whether TEST is null before returning RESULT.  If TEST is used in
   RESULT, it must have previously had a save_expr applied to it.  */

static tree
ifnonnull (test, result)
     tree test, result;
{
  return build (COND_EXPR, TREE_TYPE (result),
		build (EQ_EXPR, boolean_type_node, test, integer_zero_node),
		cp_convert (TREE_TYPE (result), integer_zero_node),
		result);
}

/* Generate the constant expression describing where direct base BINFO
   appears within the PARENT. How to interpret this expression depends on
   details of the ABI, which the runtime must be aware of.  */

static tree
get_base_offset (binfo, parent)
     tree binfo;
     tree parent;
{
  if (! TREE_VIA_VIRTUAL (binfo))
    return BINFO_OFFSET (binfo);
  else if (! vbase_offsets_in_vtable_p ())
    {
      const char *name;
    
      FORMAT_VBASE_NAME (name, BINFO_TYPE (binfo));
      return byte_position (lookup_field (parent, get_identifier (name),
					  0, 0));
    }
  else
    /* Under the new ABI, we store the vtable offset at which
       the virtual base offset can be found.  */
    return convert (sizetype,
		    BINFO_VPTR_FIELD (BINFO_FOR_VBASE (BINFO_TYPE (binfo),
						       parent)));

}

/* Execute a dynamic cast, as described in section 5.2.6 of the 9/93 working
   paper.  */

static tree
build_dynamic_cast_1 (type, expr)
     tree type, expr;
{
  enum tree_code tc = TREE_CODE (type);
  tree exprtype = TREE_TYPE (expr);
  tree dcast_fn;
  tree old_expr = expr;
  const char *errstr = NULL;

  /* T shall be a pointer or reference to a complete class type, or
     `pointer to cv void''.  */
  switch (tc)
    {
    case POINTER_TYPE:
      if (TREE_CODE (TREE_TYPE (type)) == VOID_TYPE)
	break;
    case REFERENCE_TYPE:
      if (! IS_AGGR_TYPE (TREE_TYPE (type)))
	{
	  errstr = "target is not pointer or reference to class";
	  goto fail;
	}
      if (!COMPLETE_TYPE_P (complete_type (TREE_TYPE (type))))
	{
	  errstr = "target is not pointer or reference to complete type";
	  goto fail;
	}
      break;

    default:
      errstr = "target is not pointer or reference";
      goto fail;
    }

  if (TREE_CODE (expr) == OFFSET_REF)
    {
      expr = resolve_offset_ref (expr);
      exprtype = TREE_TYPE (expr);
    }

  if (tc == POINTER_TYPE)
    expr = convert_from_reference (expr);
  else if (TREE_CODE (exprtype) != REFERENCE_TYPE)
    {
      /* Apply trivial conversion T -> T& for dereferenced ptrs.  */
      exprtype = build_reference_type (exprtype);
      expr = convert_to_reference (exprtype, expr, CONV_IMPLICIT,
				   LOOKUP_NORMAL, NULL_TREE);
    }

  exprtype = TREE_TYPE (expr);

  if (tc == POINTER_TYPE)
    {
      /* If T is a pointer type, v shall be an rvalue of a pointer to
	 complete class type, and the result is an rvalue of type T.  */

      if (TREE_CODE (exprtype) != POINTER_TYPE)
	{
	  errstr = "source is not a pointer";
	  goto fail;
	}
      if (! IS_AGGR_TYPE (TREE_TYPE (exprtype)))
	{
	  errstr = "source is not a pointer to class";
	  goto fail;
	}
      if (!COMPLETE_TYPE_P (complete_type (TREE_TYPE (exprtype))))
	{
	  errstr = "source is a pointer to incomplete type";
	  goto fail;
	}
    }
  else
    {
      /* T is a reference type, v shall be an lvalue of a complete class
	 type, and the result is an lvalue of the type referred to by T.  */

      if (! IS_AGGR_TYPE (TREE_TYPE (exprtype)))
	{
	  errstr = "source is not of class type";
	  goto fail;
	}
      if (!COMPLETE_TYPE_P (complete_type (TREE_TYPE (exprtype))))
	{
	  errstr = "source is of incomplete class type";
	  goto fail;
	}
      
    }

  /* The dynamic_cast operator shall not cast away constness.  */
  if (!at_least_as_qualified_p (TREE_TYPE (type),
				TREE_TYPE (exprtype)))
    {
      errstr = "conversion casts away constness";
      goto fail;
    }

  /* If *type is an unambiguous accessible base class of *exprtype,
     convert statically.  */
  {
    int distance;
    tree path;

    distance = get_base_distance (TREE_TYPE (type), TREE_TYPE (exprtype), 1,
				  &path);

    if (distance == -2)
      {
	cp_error ("dynamic_cast from `%T' to ambiguous base class `%T'",
		  TREE_TYPE (exprtype), TREE_TYPE (type));
	return error_mark_node;
      }
    if (distance == -3)
      {
	cp_error ("dynamic_cast from `%T' to private base class `%T'",
		  TREE_TYPE (exprtype), TREE_TYPE (type));
	return error_mark_node;
      }

    if (distance >= 0)
      {
	expr = build_vbase_path (PLUS_EXPR, type, expr, path, 0);
	if (TREE_CODE (exprtype) == POINTER_TYPE)
	  expr = non_lvalue (expr);
	return expr;
      }
  }

  /* Otherwise *exprtype must be a polymorphic class (have a vtbl).  */
  if (TYPE_POLYMORPHIC_P (TREE_TYPE (exprtype)))
    {
      tree expr1;
      /* if TYPE is `void *', return pointer to complete object.  */
      if (tc == POINTER_TYPE
	  && TYPE_MAIN_VARIANT (TREE_TYPE (type)) == void_type_node)
	{
	  /* if b is an object, dynamic_cast<void *>(&b) == (void *)&b.  */
	  if (TREE_CODE (expr) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (expr, 0)) == VAR_DECL
	      && TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == RECORD_TYPE)
	    return build1 (NOP_EXPR, type, expr);

	  /* Since expr is used twice below, save it.  */
	  expr = save_expr (expr);

	  expr1 = build_headof (expr);
	  if (TREE_TYPE (expr1) != type)
	    expr1 = build1 (NOP_EXPR, type, expr1);
	  return ifnonnull (expr, expr1);
	}
      else
	{
	  tree retval;
          tree result, td2, td3, elems;
          tree static_type, target_type, boff;

 	  /* If we got here, we can't convert statically.  Therefore,
	     dynamic_cast<D&>(b) (b an object) cannot succeed.  */
	  if (tc == REFERENCE_TYPE)
	    {
	      if (TREE_CODE (old_expr) == VAR_DECL
		  && TREE_CODE (TREE_TYPE (old_expr)) == RECORD_TYPE)
		{
	          tree expr = throw_bad_cast ();
		  cp_warning ("dynamic_cast of `%#D' to `%#T' can never succeed",
			      old_expr, type);
	          /* Bash it to the expected type.  */
	          TREE_TYPE (expr) = type;
		  return expr;
		}
	    }
	  /* Ditto for dynamic_cast<D*>(&b).  */
	  else if (TREE_CODE (expr) == ADDR_EXPR)
	    {
	      tree op = TREE_OPERAND (expr, 0);
	      if (TREE_CODE (op) == VAR_DECL
		  && TREE_CODE (TREE_TYPE (op)) == RECORD_TYPE)
		{
		  cp_warning ("dynamic_cast of `%#D' to `%#T' can never succeed",
			      op, type);
		  retval = build_int_2 (0, 0); 
		  TREE_TYPE (retval) = type; 
		  return retval;
		}
	    }

	  target_type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
	  static_type = TYPE_MAIN_VARIANT (TREE_TYPE (exprtype));
	  td2 = build_unary_op (ADDR_EXPR, get_tinfo_decl (target_type), 0);
	  td3 = build_unary_op (ADDR_EXPR, get_tinfo_decl (static_type), 0);

          /* Determine how T and V are related.  */
          boff = get_dynamic_cast_base_type (static_type, target_type);
          
	  /* Since expr is used twice below, save it.  */
	  expr = save_expr (expr);

	  expr1 = expr;
	  if (tc == REFERENCE_TYPE)
	    expr1 = build_unary_op (ADDR_EXPR, expr1, 0);

          if (!new_abi_rtti_p ())
            {
	      tree expr2 = build_headof (expr1);
	      tree td1 = expr;

	      if (tc == POINTER_TYPE)
	        td1 = build_indirect_ref (td1, NULL_PTR);
  	      td1 = get_tinfo_decl_dynamic (td1);
	  
              elems = tree_cons
	        (NULL_TREE, td1, tree_cons
	          (NULL_TREE, td2, tree_cons
	            (NULL_TREE, boff, tree_cons
	              (NULL_TREE, expr2, tree_cons
	                (NULL_TREE, td3, tree_cons
		          (NULL_TREE, expr1, NULL_TREE))))));
	    }
	  else
	    elems = tree_cons
	      (NULL_TREE, expr1, tree_cons
	        (NULL_TREE, td3, tree_cons
  	          (NULL_TREE, td2, tree_cons
                    (NULL_TREE, boff, NULL_TREE))));

	  dcast_fn = dynamic_cast_node;
	  if (!dcast_fn)
	    {
	      tree tmp;
	      tree tinfo_ptr;
	      tree ns = new_abi_rtti_p () ? abi_node : global_namespace;
	      const char *name;
	      
	      push_nested_namespace (ns);
	      if (!new_abi_rtti_p ())
	        {
    	          tinfo_ptr = build_pointer_type (tinfo_decl_type);
  	          name = "__dynamic_cast_2";
	          tmp = tree_cons
		    (NULL_TREE, tinfo_ptr, tree_cons
		      (NULL_TREE, tinfo_ptr, tree_cons
	                (NULL_TREE, integer_type_node, tree_cons
		          (NULL_TREE, ptr_type_node, tree_cons
		            (NULL_TREE, tinfo_ptr, tree_cons
		              (NULL_TREE, ptr_type_node, void_list_node))))));
	        }
	      else
	        {
                  tinfo_ptr = xref_tag (class_type_node,
                                        get_identifier ("__class_type_info"),
                                        1);
                    
                  tinfo_ptr = build_pointer_type
                                (build_qualified_type
                                  (tinfo_ptr, TYPE_QUAL_CONST));
  	          name = "__dynamic_cast";
  	          tmp = tree_cons
	            (NULL_TREE, const_ptr_type_node, tree_cons
	              (NULL_TREE, tinfo_ptr, tree_cons
	                (NULL_TREE, tinfo_ptr, tree_cons
	                  (NULL_TREE, ptrdiff_type_node, void_list_node))));
	        }
	      tmp = build_function_type (ptr_type_node, tmp);
	      if (new_abi_rtti_p ())
		/* We want its name mangling.  */
		dcast_fn = build_cp_library_fn_ptr (name, tmp);
	      else
		dcast_fn = build_library_fn_ptr (name, tmp);
              pop_nested_namespace (ns);
              dynamic_cast_node = dcast_fn;
	    }
          result = build_call (dcast_fn, elems);

	  if (tc == REFERENCE_TYPE)
	    {
	      tree bad = throw_bad_cast ();
	      
	      result = save_expr (result);
	      return build (COND_EXPR, type, result, result, bad);
	    }

	  /* Now back to the type we want from a void*.  */
	  result = cp_convert (type, result);
          return ifnonnull (expr, result);
	}
    }
  else
    errstr = "source type is not polymorphic";

 fail:
  cp_error ("cannot dynamic_cast `%E' (of type `%#T') to type `%#T' (%s)",
	    expr, exprtype, type, errstr);
  return error_mark_node;
}

tree
build_dynamic_cast (type, expr)
     tree type, expr;
{
  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;
  
  if (processing_template_decl)
    return build_min (DYNAMIC_CAST_EXPR, type, expr);

  return convert_from_reference (build_dynamic_cast_1 (type, expr));
}

/* Build and initialize various sorts of descriptors.  Every descriptor
   node has a name associated with it (the name created by mangling).
   For this reason, we use the identifier as our access to the __*_desc
   nodes, instead of sticking them directly in the types.  Otherwise we
   would burden all built-in types (and pointer types) with slots that
   we don't necessarily want to use.

   For each descriptor we build, we build a variable that contains
   the descriptor's information.  When we need this info at runtime,
   all we need is access to these variables.

   Note: these constructors always return the address of the descriptor
   info, since that is simplest for their mutual interaction.  */

/* Build an initializer for a __si_type_info node.  */

static void
expand_si_desc (tdecl, type)
     tree tdecl;
     tree type;
{
  tree t, elems, fn;
  tree name_string = tinfo_name (type);

  type = BINFO_TYPE (TREE_VEC_ELT (TYPE_BINFO_BASETYPES (type), 0));
  finish_expr_stmt (get_typeid_1 (type));
  t = decay_conversion (get_tinfo_var (type));
  elems = tree_cons
    (NULL_TREE, decay_conversion (tdecl), tree_cons
     (NULL_TREE, decay_conversion (name_string), tree_cons
      (NULL_TREE, t, NULL_TREE)));

  fn = get_identifier ("__rtti_si");
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    {
      tree tmp;
      tmp = tree_cons
	(NULL_TREE, ptr_type_node, tree_cons
	 (NULL_TREE, const_string_type_node, tree_cons
	  (NULL_TREE, build_pointer_type (type_info_type_node),
	   void_list_node)));
      fn = push_void_library_fn (fn, tmp);
    }

  fn = build_call (fn, elems);
  finish_expr_stmt (fn);
}

/* Build an initializer for a __class_type_info node.  */

static void
expand_class_desc (tdecl, type)
     tree tdecl;
     tree type;
{
  tree name_string;
  tree fn, tmp;

  int i = CLASSTYPE_N_BASECLASSES (type);
  int base_cnt = 0;
  tree binfos = TYPE_BINFO_BASETYPES (type);
#if 0
  /* See code below that used these.  */
  tree vb = CLASSTYPE_VBASECLASSES (type);
  int n_base = i;
#endif
  tree base, elems, access, offset, isvir;
  tree elt, elts = NULL_TREE;

  if (base_desc_type_node == NULL_TREE)
    {
      tree fields [4];

      /* A reasonably close approximation of __class_type_info::base_info */

      base_desc_type_node = make_aggr_type (RECORD_TYPE);

      /* Actually const __user_type_info * */
      fields [0] = build_lang_decl
	(FIELD_DECL, NULL_TREE,
	 build_pointer_type (build_qualified_type
			     (type_info_type_node,
			      TYPE_QUAL_CONST)));
      fields [1] = build_lang_decl
	(FIELD_DECL, NULL_TREE, 
	 flag_new_abi ? intSI_type_node : unsigned_intSI_type_node);
      DECL_BIT_FIELD (fields[1]) = 1;
      DECL_SIZE (fields[1]) = bitsize_int (29);

      fields [2] = build_lang_decl (FIELD_DECL, NULL_TREE, boolean_type_node);
      DECL_BIT_FIELD (fields[2]) = 1;
      DECL_SIZE (fields[2]) = bitsize_one_node;

      /* Actually enum access */
      fields [3] = build_lang_decl (FIELD_DECL, NULL_TREE, integer_type_node);
      DECL_BIT_FIELD (fields[3]) = 1;
      DECL_SIZE (fields[3]) = bitsize_int (2);

      finish_builtin_type (base_desc_type_node, "__base_info", fields,
			   3, ptr_type_node);
    }

  while (--i >= 0)
    {
      tree binfo = TREE_VEC_ELT (binfos, i);

      finish_expr_stmt (get_typeid_1 (BINFO_TYPE (binfo)));
      base = decay_conversion (get_tinfo_var (BINFO_TYPE (binfo)));
      offset = get_base_offset (binfo, type);
      
      if (TREE_VIA_PUBLIC (binfo))
        access = access_public_node;
      else if (TREE_VIA_PROTECTED (binfo))
	access = access_protected_node;
      else
	access = access_private_node;
      if (TREE_VIA_VIRTUAL (binfo))
	isvir = boolean_true_node;
      else
	isvir = boolean_false_node;

      elt = build
	(CONSTRUCTOR, base_desc_type_node, NULL_TREE, tree_cons
	 (NULL_TREE, base, tree_cons
	  (NULL_TREE, offset, tree_cons
	   (NULL_TREE, isvir, tree_cons
	    (NULL_TREE, access, NULL_TREE)))));
      TREE_HAS_CONSTRUCTOR (elt) = TREE_CONSTANT (elt) = TREE_STATIC (elt) = 1;
      elts = tree_cons (NULL_TREE, elt, elts);
      base_cnt++;
    }
#if 0
  i = n_base;
  while (vb)
    {
      tree b;
      access = access_public_node;
      while (--i >= 0)
	{
	  b = TREE_VEC_ELT (binfos, i);
	  if (BINFO_TYPE (vb) == BINFO_TYPE (b) && TREE_VIA_VIRTUAL (b))
	    {
	      if (TREE_VIA_PUBLIC (b))
		access = access_public_node;
	      else if (TREE_VIA_PROTECTED (b))
		access = access_protected_node;
	      else
		access = access_private_node;
	      break;
	    }
	}
      base = build_t_desc (BINFO_TYPE (vb), 1);
      offset = BINFO_OFFSET (vb);
      isvir = build_int_2 (1, 0);

      base_list = tree_cons (NULL_TREE, base, base_list);
      isvir_list = tree_cons (NULL_TREE, isvir, isvir_list);
      acc_list = tree_cons (NULL_TREE, access, acc_list);
      off_list = tree_cons (NULL_TREE, offset, off_list);

      base_cnt++;
      vb = TREE_CHAIN (vb);
    }
#endif

  name_string = tinfo_name (type);

  {
    tree arrtype = build_array_type (base_desc_type_node, NULL_TREE);
    elts = build (CONSTRUCTOR, arrtype, NULL_TREE, elts);
    TREE_HAS_CONSTRUCTOR (elts) = TREE_CONSTANT (elts)
      = TREE_STATIC (elts) = 1;
    complete_array_type (arrtype, elts, 1);
  }

  elems = tree_cons
    (NULL_TREE, decay_conversion (tdecl), tree_cons
     (NULL_TREE, decay_conversion (name_string), tree_cons
      (NULL_TREE, decay_conversion (elts), tree_cons
       (NULL_TREE, cp_convert (sizetype, build_int_2 (base_cnt, 0)),
	NULL_TREE))));

  fn = get_identifier ("__rtti_class");
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    {
      tmp = tree_cons
	(NULL_TREE, ptr_type_node, tree_cons
	 (NULL_TREE, const_string_type_node, tree_cons
	  (NULL_TREE, build_pointer_type (base_desc_type_node), tree_cons
	   (NULL_TREE, sizetype, void_list_node))));

      fn = push_void_library_fn (fn, tmp);
    }

  fn = build_call (fn, elems);
  finish_expr_stmt (fn);
}

/* Build an initializer for a __pointer_type_info node.  */

static void
expand_ptr_desc (tdecl, type)
     tree tdecl;
     tree type;
{
  tree t, elems, fn;
  tree name_string = tinfo_name (type);

  type = TREE_TYPE (type);
  finish_expr_stmt (get_typeid_1 (type));
  t = decay_conversion (get_tinfo_var (type));
  elems = tree_cons
    (NULL_TREE, decay_conversion (tdecl), tree_cons
     (NULL_TREE, decay_conversion (name_string), tree_cons
      (NULL_TREE, t, NULL_TREE)));

  fn = get_identifier ("__rtti_ptr");
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    {
      tree tmp;
      tmp = tree_cons
	(NULL_TREE, ptr_type_node, tree_cons
	 (NULL_TREE, const_string_type_node, tree_cons
	  (NULL_TREE, build_pointer_type (type_info_type_node),
	   void_list_node)));
      fn = push_void_library_fn (fn, tmp);
    }

  fn = build_call (fn, elems);
  finish_expr_stmt (fn);
}

/* Build an initializer for a __attr_type_info node.  */

static void
expand_attr_desc (tdecl, type)
     tree tdecl;
     tree type;
{
  tree elems, t, fn;
  tree name_string = tinfo_name (type);
  tree attrval = build_int_2 (TYPE_QUALS (type), 0);

  finish_expr_stmt (get_typeid_1 (TYPE_MAIN_VARIANT (type)));
  t = decay_conversion (get_tinfo_var (TYPE_MAIN_VARIANT (type)));
  elems = tree_cons
    (NULL_TREE, decay_conversion (tdecl), tree_cons
     (NULL_TREE, decay_conversion (name_string), tree_cons
      (NULL_TREE, attrval, tree_cons (NULL_TREE, t, NULL_TREE))));

  fn = get_identifier ("__rtti_attr");
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    {
      tree tmp;
      tmp = tree_cons
	(NULL_TREE, ptr_type_node, tree_cons
	 (NULL_TREE, const_string_type_node, tree_cons
	  (NULL_TREE, integer_type_node, tree_cons
	   (NULL_TREE, build_pointer_type (type_info_type_node),
	    void_list_node))));
      fn = push_void_library_fn (fn, tmp);
    }

  fn = build_call (fn, elems);
  finish_expr_stmt (fn);
}

/* Build an initializer for a type_info node that just has a name.  */

static void
expand_generic_desc (tdecl, type, fnname)
     tree tdecl;
     tree type;
     const char *fnname;
{
  tree name_string = tinfo_name (type);
  tree elems = tree_cons
    (NULL_TREE, decay_conversion (tdecl), tree_cons
     (NULL_TREE, decay_conversion (name_string), NULL_TREE));

  tree fn = get_identifier (fnname);
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    {
      tree tmp;
      tmp = tree_cons
	(NULL_TREE, ptr_type_node, tree_cons
	 (NULL_TREE, const_string_type_node, void_list_node));
      fn = push_void_library_fn (fn, tmp);
    }

  fn = build_call (fn, elems);
  finish_expr_stmt (fn);
}

/* Generate the code for a type_info initialization function.
   Note that we take advantage of the passage

   5.2.7  Type identification                               [expr.typeid]
   
   Whether or not the destructor is called for the type_info object at the
   end of the program is unspecified.

   and don't bother to arrange for these objects to be destroyed.  It
   doesn't matter, anyway, since the destructors don't do anything.
       
   This must only be called from toplevel (i.e. from finish_file)!  */

void
synthesize_tinfo_fn (fndecl)
     tree fndecl;
{
  tree type = TREE_TYPE (DECL_NAME (fndecl));
  tree tmp, addr, tdecl;
  tree compound_stmt;
  tree if_stmt;
  tree then_clause;

  my_friendly_assert (!new_abi_rtti_p (), 20000118);
  if (at_eof)
    {
      import_export_decl (fndecl);
      if (DECL_REALLY_EXTERN (fndecl))
	return;
    }

  /* Declare the static typeinfo variable.  */
  tdecl = get_tinfo_var (type);
  DECL_EXTERNAL (tdecl) = 0;
  TREE_STATIC (tdecl) = 1;
  DECL_COMMON (tdecl) = 1;
  TREE_USED (tdecl) = 1;
  DECL_ALIGN (tdecl) = TYPE_ALIGN (ptr_type_node);
  cp_finish_decl (tdecl, NULL_TREE, NULL_TREE, 0);

  /* Begin processing the function.  */
  start_function (NULL_TREE, fndecl, NULL_TREE, 
		  SF_DEFAULT | SF_PRE_PARSED);
  DECL_DEFER_OUTPUT (fndecl) = 1;
  store_parm_decls ();
  clear_last_expr ();

  /* Begin the body of the function.  */
  compound_stmt = begin_compound_stmt (/*has_no_scope=*/0);

  /* For convenience, we save away the address of the static
     variable.  */
  addr = decay_conversion (tdecl);

  /* If the first word of the array (the vtable) is non-zero, we've already
     initialized the object, so don't do it again.  */
  if_stmt = begin_if_stmt ();
  tmp = cp_convert (build_pointer_type (ptr_type_node), addr);
  tmp = build_indirect_ref (tmp, 0);
  tmp = build_binary_op (EQ_EXPR, tmp, integer_zero_node);
  finish_if_stmt_cond (tmp, if_stmt);
  then_clause = begin_compound_stmt (/*has_no_scope=*/0);

  if (TREE_CODE (type) == FUNCTION_TYPE)
    expand_generic_desc (tdecl, type, "__rtti_func");
  else if (TREE_CODE (type) == ARRAY_TYPE)
    expand_generic_desc (tdecl, type, "__rtti_array");
  else if (TYPE_QUALS (type) != TYPE_UNQUALIFIED)
    expand_attr_desc (tdecl, type);
  else if (TREE_CODE (type) == POINTER_TYPE)
    {
      if (TREE_CODE (TREE_TYPE (type)) == OFFSET_TYPE)
	expand_generic_desc (tdecl, type, "__rtti_ptmd");
      else if (TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE)
	expand_generic_desc (tdecl, type, "__rtti_ptmf");
      else
	expand_ptr_desc (tdecl, type);
    }
  else if (TYPE_PTRMEMFUNC_P (type))
    expand_generic_desc (tdecl, type, "__rtti_ptmf");
  else if (IS_AGGR_TYPE (type))
    {
      if (CLASSTYPE_N_BASECLASSES (type) == 0)
	expand_generic_desc (tdecl, type, "__rtti_user");
      else if (! TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (type)
	       && (TREE_VIA_PUBLIC
		   (TREE_VEC_ELT (TYPE_BINFO_BASETYPES (type), 0))))
	expand_si_desc (tdecl, type);
      else
	expand_class_desc (tdecl, type);
    }
  else if (TREE_CODE (type) == ENUMERAL_TYPE)
    expand_generic_desc (tdecl, type, "__rtti_user");
  else
    my_friendly_abort (252);

  finish_compound_stmt (/*has_no_scope=*/0, then_clause);
  finish_then_clause (if_stmt);
  finish_if_stmt ();

  /* OK, now return the type_info object.  */
  tmp = cp_convert (build_pointer_type (type_info_type_node), addr);
  tmp = build_indirect_ref (tmp, 0);
  finish_return_stmt (tmp);
  /* Finish the function body.  */
  finish_compound_stmt (/*has_no_scope=*/0, compound_stmt);
  expand_body (finish_function (0));
}

/* Return the runtime bit mask encoding the qualifiers of TYPE.  */

static int
qualifier_flags (type)
     tree type;
{
  int flags = 0;
  /* we want the qualifiers on this type, not any array core, it might have */
  int quals = TYPE_QUALS (type);
  
  if (quals & TYPE_QUAL_CONST)
    flags |= 1;
  if (quals & TYPE_QUAL_VOLATILE)
    flags |= 2;
  if (quals & TYPE_QUAL_RESTRICT)
    flags |= 4;
  return flags;
}

/* Return non-zero, if the pointer chain TYPE ends at an incomplete type, or
   contains a pointer to member of an incomplete class.  */

static int
target_incomplete_p (type)
     tree type;
{
  while (TREE_CODE (type) == POINTER_TYPE)
    if (TYPE_PTRMEM_P (type))
      {
        if (!COMPLETE_TYPE_P (TYPE_PTRMEM_CLASS_TYPE (type)))
          return 1;
        type = TYPE_PTRMEM_POINTED_TO_TYPE (type);
      }
    else
      type = TREE_TYPE (type);
  if (!COMPLETE_OR_VOID_TYPE_P (type))
    return 1;
  
  return 0;
}

/* Return a CONSTRUCTOR for the common part of the type_info objects. This
   is the vtable pointer and NTBS name.  The NTBS name is emitted as a
   comdat const char array, so it becomes a unique key for the type. Generate
   and emit that VAR_DECL here.  (We can't always emit the type_info itself
   as comdat, because of pointers to incomplete.) */

static tree
tinfo_base_init (desc, target)
     tree desc;
     tree target;
{
  tree init = NULL_TREE;
  tree name_decl;
  
  {
    /* Generate the NTBS array variable.  */
    tree name_name = build_overload_with_type (tinfo_var_id, target);
    tree name_type = build_cplus_array_type
                     (build_qualified_type (char_type_node, TYPE_QUAL_CONST),
                     NULL_TREE);
    tree name_string = tinfo_name (target);
    name_decl = build_lang_decl (VAR_DECL, name_name, name_type);
    
    DECL_ARTIFICIAL (name_decl) = 1;
    TREE_READONLY (name_decl) = 1;
    TREE_STATIC (name_decl) = 1;
    DECL_EXTERNAL (name_decl) = 0;
    TREE_PUBLIC (name_decl) = 1;
    comdat_linkage (name_decl);
    DECL_ASSEMBLER_NAME (name_decl) = DECL_NAME (name_decl);
    DECL_INITIAL (name_decl) = name_string;
    cp_finish_decl (name_decl, name_string, NULL_TREE, 0);
  }
  
  if (TINFO_VTABLE_DECL (desc))
    {
      tree vtbl_ptr = TINFO_VTABLE_DECL (desc);
      init = tree_cons (NULL_TREE, vtbl_ptr, init);
    }
  
  init = tree_cons (NULL_TREE, decay_conversion (name_decl), init);
  
  init = build (CONSTRUCTOR, NULL_TREE, NULL_TREE, nreverse (init));
  TREE_HAS_CONSTRUCTOR (init) = TREE_CONSTANT (init) = TREE_STATIC (init) = 1;
  init = tree_cons (NULL_TREE, init, NULL_TREE);
  
  return init;
}

/* Return the CONSTRUCTOR expr for a type_info of TYPE. DESC provides the
   information about the particular type_info derivation, which adds no
   additional fields to the type_info base.  */

static tree
generic_initializer (desc, target)
     tree desc;
     tree target;
{
  tree init = tinfo_base_init (desc, target);
  
  init = build (CONSTRUCTOR, NULL_TREE, NULL_TREE, init);
  TREE_HAS_CONSTRUCTOR (init) = TREE_CONSTANT (init) = TREE_STATIC (init) = 1;
  return init;
}

/* Return the CONSTRUCTOR expr for a type_info of pointer TYPE.
   DESC provides information about the particular type_info derivation,
   which adds target type and qualifier flags members to the type_info base.  */

static tree
ptr_initializer (desc, target, non_public_ptr)
     tree desc;
     tree target;
     int *non_public_ptr;
{
  tree init = tinfo_base_init (desc, target);
  tree to = TREE_TYPE (target);
  int flags = qualifier_flags (to);
  int incomplete = target_incomplete_p (to);
  
  if (incomplete)
    {
      flags |= 8;
      *non_public_ptr = 1;
    }
  init = tree_cons (NULL_TREE, build_int_2 (flags, 0), init);
  init = tree_cons (NULL_TREE,
                    build_unary_op (ADDR_EXPR,
                                    get_tinfo_decl (TYPE_MAIN_VARIANT (to)), 0),
                    init);
  
  init = build (CONSTRUCTOR, NULL_TREE, NULL_TREE, nreverse (init));
  TREE_HAS_CONSTRUCTOR (init) = TREE_CONSTANT (init) = TREE_STATIC (init) = 1;
  return init;
}

/* Return the CONSTRUCTOR expr for a type_info of pointer to member data TYPE.
   DESC provides information about the particular type_info derivation,
   which adds class, target type and qualifier flags members to the type_info
   base.  */

static tree
ptmd_initializer (desc, target, non_public_ptr)
     tree desc;
     tree target;
     int *non_public_ptr;
{
  tree init = tinfo_base_init (desc, target);
  tree to = TYPE_PTRMEM_POINTED_TO_TYPE (target);
  tree klass = TYPE_PTRMEM_CLASS_TYPE (target);
  int flags = qualifier_flags (to);
  int incomplete = target_incomplete_p (to);
  
  if (incomplete)
    {
      flags |= 0x8;
      *non_public_ptr = 1;
    }
  if (!COMPLETE_TYPE_P (klass))
    flags |= 0x10;
  init = tree_cons (NULL_TREE, build_int_2 (flags, 0), init);
  init = tree_cons (NULL_TREE,
                    build_unary_op (ADDR_EXPR,
                                    get_tinfo_decl (TYPE_MAIN_VARIANT (to)), 0),
                    init);
  init = tree_cons (NULL_TREE,
                    build_unary_op (ADDR_EXPR, get_tinfo_decl (klass), 0),
                    init);  
  
  init = build (CONSTRUCTOR, NULL_TREE, NULL_TREE, nreverse (init));
  TREE_HAS_CONSTRUCTOR (init) = TREE_CONSTANT (init) = TREE_STATIC (init) = 1;
  return init;  
}

/* Check base BINFO to set hint flags in *DATA, which is really an int.
   We use CLASSTYPE_MARKED to tag types we've found as non-virtual bases and
   CLASSTYPE_MARKED2 to tag those which are virtual bases. Remember it is
   possible for a type to be both a virtual and non-virtual base.  */

static tree
dfs_class_hint_mark (binfo, data)
     tree binfo;
     void *data;
{
  tree basetype = BINFO_TYPE (binfo);
  int *hint = (int *) data;
  
  if (TREE_VIA_VIRTUAL (binfo))
    {
      if (CLASSTYPE_MARKED (basetype))
        *hint |= 1;
      if (CLASSTYPE_MARKED2 (basetype))
        *hint |= 2;
      SET_CLASSTYPE_MARKED2 (basetype);
    }
  else
    {
      if (CLASSTYPE_MARKED (basetype) || CLASSTYPE_MARKED2 (basetype))
        *hint |= 1;
      SET_CLASSTYPE_MARKED (basetype);
    }
  if (!TREE_VIA_PUBLIC (binfo) && TYPE_BINFO (basetype) != binfo)
    *hint |= 4;
  return NULL_TREE;
};

/* Clear the base's dfs marks, after searching for duplicate bases. */

static tree
dfs_class_hint_unmark (binfo, data)
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{
  tree basetype = BINFO_TYPE (binfo);
  
  CLEAR_CLASSTYPE_MARKED (basetype);
  CLEAR_CLASSTYPE_MARKED2 (basetype);
  return NULL_TREE;
}

/* Determine the hint flags describing the features of a class's heirarchy.  */

static int
class_hint_flags (type)
     tree type;
{
  int hint_flags = 0;
  int i;
  
  dfs_walk (TYPE_BINFO (type), dfs_class_hint_mark, NULL, &hint_flags);
  dfs_walk (TYPE_BINFO (type), dfs_class_hint_unmark, NULL, NULL);
  
  for (i = 0; i < CLASSTYPE_N_BASECLASSES (type); ++i)
    {
      tree base_binfo = BINFO_BASETYPE (TYPE_BINFO (type), i);
      
      if (TREE_VIA_PUBLIC (base_binfo))
        hint_flags |= 0x8;
    }
  return hint_flags;
}
        
/* Return the CONSTRUCTOR expr for a type_info of class TYPE.
   DESC provides information about the particular __class_type_info derivation,
   which adds hint flags and TRAIL initializers to the type_info base.  */

static tree
class_initializer (desc, target, trail)
     tree desc;
     tree target;
     tree trail;
{
  tree init = tinfo_base_init (desc, target);
  
  TREE_CHAIN (init) = trail;
  init = build (CONSTRUCTOR, NULL_TREE, NULL_TREE, init);
  TREE_HAS_CONSTRUCTOR (init) = TREE_CONSTANT (init) = TREE_STATIC (init) = 1;
  return init;  
}

/* Generate a pseudo_type_info VAR_DECL suitable for the supplied
   TARGET_TYPE and given the REAL_NAME. This is the structure expected by
   the runtime, and therefore has additional fields.  If we need not emit a
   definition (because the runtime must contain it), return NULL_TREE,
   otherwise return the VAR_DECL.  */

static tree
synthesize_tinfo_var (target_type, real_name)
     tree target_type;
     tree real_name;
{
  tree var_init = NULL_TREE;
  tree var_type = NULL_TREE;
  int non_public = 0;
  
  my_friendly_assert (new_abi_rtti_p (), 20000118);

  switch (TREE_CODE (target_type))
    {
    case POINTER_TYPE:
      if (TYPE_PTRMEM_P (target_type))
        {
          var_type = ptmd_desc_type_node;
          var_init = ptmd_initializer (var_type, target_type, &non_public);
        }
      else
        {
          int code = TREE_CODE (TREE_TYPE (target_type));
          
          if ((CP_TYPE_QUALS (TREE_TYPE (target_type)) | TYPE_QUAL_CONST)
              == TYPE_QUAL_CONST
              && (code == INTEGER_TYPE || code == BOOLEAN_TYPE
                  || code == CHAR_TYPE || code == REAL_TYPE
                  || code == VOID_TYPE)
              && !doing_runtime)
            /* These are in the runtime.  */
            return NULL_TREE;
          var_type = ptr_desc_type_node;
          var_init = ptr_initializer (var_type, target_type, &non_public);
        }
      break;
    case ENUMERAL_TYPE:
      var_type = enum_desc_type_node;
      var_init = generic_initializer (var_type, target_type);
      break;
    case FUNCTION_TYPE:
      var_type = func_desc_type_node;
      var_init = generic_initializer (var_type, target_type);
      break;
    case ARRAY_TYPE:
      var_type = ary_desc_type_node;
      var_init = generic_initializer (var_type, target_type);
      break;
    case UNION_TYPE:
    case RECORD_TYPE:
      if (!COMPLETE_TYPE_P (target_type))
        {
          /* Emit a non-public class_type_info.  */
          non_public = 1;
          var_type = class_desc_type_node;
          var_init = class_initializer (var_type, target_type, NULL_TREE);
        }
      else if (!CLASSTYPE_N_BASECLASSES (target_type))
        {
          var_type = class_desc_type_node;
          var_init = class_initializer (var_type, target_type, NULL_TREE);
        }
      else
        {
          /* if this has a single public non-virtual base, it's easier */
          tree binfo = TYPE_BINFO (target_type);
          int nbases = BINFO_N_BASETYPES (binfo);
          tree base_binfos = BINFO_BASETYPES (binfo);
          tree base_inits = NULL_TREE;
          int is_simple = nbases == 1;
          int ix;
          
          /* Generate the base information initializer.  */
          for (ix = nbases; ix--;)
            {
              tree base_binfo = TREE_VEC_ELT (base_binfos, ix);
              tree base_init = NULL_TREE;
              int flags = 0;
              tree tinfo;
              tree offset;
              
              if (TREE_VIA_VIRTUAL (base_binfo))
                flags |= 1;
              if (TREE_PUBLIC (base_binfo))
                flags |= 2;
              tinfo = get_tinfo_decl (BINFO_TYPE (base_binfo));
              tinfo = build_unary_op (ADDR_EXPR, tinfo, 0);
              offset = get_base_offset (base_binfo, target_type);
              
              /* is it a single public inheritance? */
              if (is_simple && flags == 2 && integer_zerop (offset))
                {
                  base_inits = tree_cons (NULL_TREE, tinfo, NULL_TREE);
                  break;
                }
              is_simple = 0;
              
              /* combine offset and flags into one field */
              offset = build_binary_op (LSHIFT_EXPR, offset,
                                        build_int_2 (8, 0));
              offset = build_binary_op (BIT_IOR_EXPR, offset,
                                        build_int_2 (flags, 0));
              base_init = tree_cons (NULL_TREE, offset, base_init);
              base_init = tree_cons (NULL_TREE, tinfo, base_init);
              base_init = build (CONSTRUCTOR, NULL_TREE, NULL_TREE, base_init);
              base_inits = tree_cons (NULL_TREE, base_init, base_inits);
            }
          
          if (is_simple)
            var_type = si_class_desc_type_node;
          else
            {
              int hint = class_hint_flags (target_type);
              
              base_inits = build (CONSTRUCTOR, NULL_TREE, NULL_TREE, base_inits);
              base_inits = tree_cons (NULL_TREE, base_inits, NULL_TREE);
              /* Prepend the number of bases.  */
              base_inits = tree_cons (NULL_TREE,
                                      build_int_2 (nbases, 0), base_inits);
              /* Prepend the hint flags. */
              base_inits = tree_cons (NULL_TREE,
                                      build_int_2 (hint, 0), base_inits);
              var_type = get_vmi_pseudo_type_info (nbases);
            }
          var_init = class_initializer (var_type, target_type, base_inits);
        }
      break;
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
    case REAL_TYPE:
    case VOID_TYPE:
      if (!doing_runtime)
        /* These are guaranteed to be in the runtime.  */
        return NULL_TREE;
      var_type = bltn_desc_type_node;
      var_init = generic_initializer (var_type, target_type);
      break;
    default:
      my_friendly_abort (20000117);
    }
  
  
  return create_real_tinfo_var (real_name, TINFO_PSEUDO_TYPE (var_type),
                                var_init, non_public);
}

/* Create the real typeinfo variable.  NON_PUBLIC indicates that we cannot
   make this variable public (comdat). */

static tree
create_real_tinfo_var (name, type, init, non_public)
     tree name;
     tree type;
     tree init;
     int non_public;
{
  static int count = 0;
  tree decl;
  tree hidden_name;
  char hidden[30];
  
  sprintf (hidden, "%.*s_%d",
           IDENTIFIER_LENGTH (tinfo_decl_id), IDENTIFIER_POINTER (tinfo_decl_id),
           count++);
  hidden_name = get_identifier (hidden);
  
  decl = build_lang_decl (VAR_DECL, hidden_name,
                          build_qualified_type (type, TYPE_QUAL_CONST));
  DECL_ARTIFICIAL (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_STATIC (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  
  if (!non_public)
    {
      TREE_PUBLIC (decl) = 1;
      comdat_linkage (decl);
    }
  DECL_ASSEMBLER_NAME (decl) = name;
  DECL_INITIAL (decl) = init;
  cp_finish_decl (decl, init, NULL_TREE, 0);
  pushdecl_top_level (decl);
  TREE_USED (decl) = 1;
  return decl;
}

/* Generate the RECORD_TYPE containing the data layout of a type_info
   derivative as used by the runtime. This layout must be consistent with
   that defined in the runtime support. Also generate the VAR_DECL for the
   type's vtable. We explicitly manage the vtable member, and name it for
   real type as used in the runtime. The RECORD type has a different name,
   to avoid collisions.  Return a TREE_LIST who's TINFO_PSEUDO_TYPE
   is the generated type and TINFO_VTABLE_DECL is the vtable decl.
   
   REAL_NAME is the runtime's name of the type. Trailing arguments are
   additional FIELD_DECL's for the structure. The final argument must be
   NULL.  */

static tree
create_pseudo_type_info VPARAMS((const char *real_name, int ident, ...))
{
#ifndef ANSI_PROTOTYPES
  char const *real_name;
  int ident;
#endif
  va_list ap;
  tree real_type, pseudo_type;
  char *pseudo_name;
  tree vtable_decl;
  int ix;
  tree fields[10];
  tree field_decl;
  tree result;
  
  VA_START (ap, ident);
#ifndef ANSI_PROTOTYPES
  real_name = va_arg (ap, char const *);
  ident = va_arg (app, int);
#endif

  /* Generate the pseudo type name. */
  pseudo_name = (char *)alloca (strlen (real_name) + 30);
  strcpy (pseudo_name, real_name);
  strcat (pseudo_name, "_pseudo");
  if (ident)
    sprintf (pseudo_name + strlen (pseudo_name), "%d", ident);
  
  /* Get the vtable decl. */
  real_type = xref_tag (class_type_node, get_identifier (real_name), 1);
  vtable_decl = get_vtable_decl (real_type, /*complete=*/1);
  vtable_decl = build_unary_op (ADDR_EXPR, vtable_decl, 0);

  /* Under the new ABI, we need to point into the middle of the
     vtable.  */
  if (flag_new_abi)
    {
      vtable_decl = build (PLUS_EXPR,
			   TREE_TYPE (vtable_decl),
			   vtable_decl,
			   size_binop (MULT_EXPR,
				       size_int (2),
				       TYPE_SIZE_UNIT (vtable_entry_type)));
      TREE_CONSTANT (vtable_decl) = 1;
    }

  /* First field is the pseudo type_info base class. */
  fields[0] = build_lang_decl (FIELD_DECL, NULL_TREE, ti_desc_type_node);
  
  /* Now add the derived fields.  */
  for (ix = 0; (field_decl = va_arg (ap, tree));)
    fields[++ix] = field_decl;
  
  /* Create the pseudo type. */
  pseudo_type = make_aggr_type (RECORD_TYPE);
  finish_builtin_type (pseudo_type, pseudo_name, fields, ix, ptr_type_node);
  TYPE_HAS_CONSTRUCTOR (pseudo_type) = 1;
  va_end (ap);
  
  result = tree_cons (NULL_TREE, NULL_TREE, NULL_TREE);
  TINFO_VTABLE_DECL (result) = vtable_decl;
  TINFO_PSEUDO_TYPE (result) = pseudo_type;
  
  return result;
}

/* Return a descriptor for a vmi type with NUM_BASES bases.  */

static tree
get_vmi_pseudo_type_info (num_bases)
     int num_bases;
{
  tree desc;
  tree array_domain, base_array;
  
  if (TREE_VEC_LENGTH (vmi_class_desc_type_node) <= num_bases)
    {
      int ix;
      tree extend = make_tree_vec (num_bases + 5);
      
      for (ix = TREE_VEC_LENGTH (vmi_class_desc_type_node); ix--;)
        TREE_VEC_ELT (extend, ix) = TREE_VEC_ELT (vmi_class_desc_type_node, ix);
      vmi_class_desc_type_node = extend;
    }
  desc = TREE_VEC_ELT (vmi_class_desc_type_node, num_bases);
  
  if (desc)
    return desc;
  
  /* Add number of bases and trailing array of base_class_type_info.  */
  array_domain = build_index_type (build_int_2 (num_bases, 0));
  base_array = build_array_type (base_desc_type_node, array_domain);

  push_nested_namespace (abi_node);

  desc = create_pseudo_type_info
            ("__vmi_class_type_info", num_bases,
             build_lang_decl (FIELD_DECL, NULL_TREE, integer_type_node),
             build_lang_decl (FIELD_DECL, NULL_TREE, integer_type_node),
             build_lang_decl (FIELD_DECL, NULL_TREE, base_array),
             NULL);

  pop_nested_namespace (abi_node);

  TREE_VEC_ELT (vmi_class_desc_type_node, num_bases) = desc;
  return desc;
}

/* Make sure the required builtin types exist for generating the type_info
   varable definitions.  */

static void
create_tinfo_types ()
{
  tree ptr_type_info;
  
  if (bltn_desc_type_node)
    return;
  push_nested_namespace (abi_node);

  ptr_type_info = build_pointer_type
                    (build_qualified_type
                      (type_info_type_node, TYPE_QUAL_CONST));
  
  /* Create the internal type_info structure. This is used as a base for
     the other structures.  */
  {
    tree fields[2];

    ti_desc_type_node = make_aggr_type (RECORD_TYPE);
    fields[0] = build_lang_decl (FIELD_DECL, NULL_TREE, const_ptr_type_node);
    fields[1] = build_lang_decl (FIELD_DECL, NULL_TREE, const_string_type_node);
    finish_builtin_type (ti_desc_type_node, "__type_info_pseudo",
                         fields, 1, ptr_type_node);
    TYPE_HAS_CONSTRUCTOR (ti_desc_type_node) = 1;
  }
  
  /* Fundamental type_info */
  bltn_desc_type_node = create_pseudo_type_info
      ("__fundamental_type_info", 0,
       NULL);

  /* Pointer type_info. Adds two fields, qualification mask
     and pointer to the pointed to type.  */
  ptr_desc_type_node = create_pseudo_type_info
      ("__pointer_type_info", 0,
       build_lang_decl (FIELD_DECL, NULL_TREE, integer_type_node),
       build_lang_decl (FIELD_DECL, NULL_TREE, ptr_type_info),
       NULL);

  /* Array, function and enum type_info. No additional fields. */
  ary_desc_type_node = create_pseudo_type_info
      ("__array_type_info", 0,
       NULL);
  func_desc_type_node = create_pseudo_type_info
       ("__function_type_info", 0,
        NULL);
  enum_desc_type_node = create_pseudo_type_info
       ("__enum_type_info", 0,
        NULL);
  
  /* Class type_info. Add a flags field.  */
  class_desc_type_node = create_pseudo_type_info
        ("__class_type_info", 0,
         NULL);
  
  /* Single public non-virtual base class. Add pointer to base class. 
     This is really a descendant of __class_type_info.  */
  si_class_desc_type_node = create_pseudo_type_info
           ("__si_class_type_info", 0,
            build_lang_decl (FIELD_DECL, NULL_TREE, ptr_type_info),
            NULL);
  
  /* Base class internal helper. Pointer to base type, offset to base,
     flags. */
  {
    tree fields[2];
    
    fields[0] = build_lang_decl (FIELD_DECL, NULL_TREE, ptr_type_info);
    fields[1] = build_lang_decl (FIELD_DECL, NULL_TREE, integer_types[itk_long]);
    base_desc_type_node = make_aggr_type (RECORD_TYPE);
    finish_builtin_type (base_desc_type_node, "__base_class_type_info_pseudo",
                         fields, 1, ptr_type_node);
    TYPE_HAS_CONSTRUCTOR (base_desc_type_node) = 1;
  }
  
  /* General heirarchy is created as necessary in this vector. */
  vmi_class_desc_type_node = make_tree_vec (10);
  
  /* Pointer to member data type_info.  Add qualifications flags,
     pointer to the member's type info and pointer to the class.
     This is really a descendant of __pointer_type_info.  */
  ptmd_desc_type_node = create_pseudo_type_info
       ("__pointer_to_member_type_info", 0,
        build_lang_decl (FIELD_DECL, NULL_TREE, integer_type_node),
        build_lang_decl (FIELD_DECL, NULL_TREE, ptr_type_info),
        build_lang_decl (FIELD_DECL, NULL_TREE, ptr_type_info),
        NULL);

  pop_nested_namespace (abi_node);
}

/* Emit the type_info descriptors which are guaranteed to be in the runtime
   support.  Generating them here guarantees consistency with the other
   structures.  We use the following heuristic to determine when the runtime
   is being generated.  If std::__fundamental_type_info is defined, and it's
   destructor is defined, then the runtime is being built.  */

void
emit_support_tinfos ()
{
  static tree *const fundamentals[] =
  {
    &void_type_node,
    &boolean_type_node,
    &wchar_type_node,
#if 0
    &signed_wchar_type_node, &unsigned_wchar_type_node,
#endif
    &char_type_node, &signed_char_type_node, &unsigned_char_type_node,
    &short_integer_type_node, &short_unsigned_type_node,
    &integer_type_node, &unsigned_type_node,
    &long_integer_type_node, &long_unsigned_type_node,
    &long_long_integer_type_node, &long_long_unsigned_type_node,
    &float_type_node, &double_type_node, &long_double_type_node,

    /* GCC extension types */
#if 0
    &complex_integer_type_node,
    &complex_float_type_node, &complex_double_type_node,
    &complex_long_double_type_node,
#endif
    
    0
  };
  int ix;
  tree bltn_type, dtor;
  
  push_nested_namespace (abi_node);
  bltn_type = xref_tag (class_type_node,
                        get_identifier ("__fundamental_type_info"), 1);
  pop_nested_namespace (abi_node);
  if (!COMPLETE_TYPE_P (bltn_type))
    return;
  dtor = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (bltn_type), 1);
  if (DECL_EXTERNAL (dtor))
    return;
  doing_runtime = 1;
  for (ix = 0; fundamentals[ix]; ix++)
    {
      tree bltn = *fundamentals[ix];
      tree bltn_ptr = build_pointer_type (bltn);
      tree bltn_const_ptr = build_pointer_type
              (build_qualified_type (bltn, TYPE_QUAL_CONST));
      tree tinfo;
      
      tinfo = get_tinfo_decl (bltn);
      TREE_USED (tinfo) = 1;
      TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (tinfo)) = 1;
      
      tinfo = get_tinfo_decl (bltn_ptr);
      TREE_USED (tinfo) = 1;
      TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (tinfo)) = 1;
      
      tinfo = get_tinfo_decl (bltn_const_ptr);
      TREE_USED (tinfo) = 1;
      TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (tinfo)) = 1;
    }
}

/* Return non-zero, iff T is a type_info variable which has not had a
   definition emitted for it.  */

int
tinfo_decl_p (t, data)
     tree t;
     void *data ATTRIBUTE_UNUSED;
{
  return TREE_CODE (t) == VAR_DECL
         && IDENTIFIER_GLOBAL_VALUE (DECL_NAME (t)) == (t)
         && TREE_TYPE (t) == tinfo_decl_type
         && TREE_TYPE (DECL_NAME (t));
}

/* Emit a suitable type_info definition for the type_info decl pointed to by
   DECL_PTR. We emit a completely new variable, of the correct type for the
   actual type this is describing. The DECL_ASSEMBLER_NAME of the generated
   definition is set to that of the supplied decl, so that they can be tied
   up. Mark the supplied decl as having been dealt with. Emitting one
   definition might cause other definitions to be required.
   
   We need to do things this way, because we're trying to do something like
   
      struct B : A {
        ...
      };
   
      extern const A tinfo_var;
   
      const B tinfo_var = {...};
   
   which is not permitted. Also, we've not necessarily seen the definition of B.
   So we do something like the following,
   
      extern const A tinfo_var;
   
      struct pseudo_A {
        const void *vtable_ptr;
        const char *name;
      };
      struct pseudo_B {
        pseudo_A base;
        ...
      };
      
      const pseudo_B proxy_tinfo_var attribute((assembler_name="tinfo_var")) =
      {
        {&B::vtable, "..."},
        ...
      };
   
   pseudo_A and pseudo_B must be layout equivalent to the real definitions in
   the runtime.  */

int
emit_tinfo_decl (decl_ptr, data)
     tree *decl_ptr;
     void *data ATTRIBUTE_UNUSED;
{
  tree tinfo_decl = *decl_ptr;
  tree tinfo_type, decl;
  
  my_friendly_assert (TREE_TYPE (tinfo_decl) == tinfo_decl_type, 20000121);
  tinfo_type = TREE_TYPE (DECL_NAME (tinfo_decl));
  my_friendly_assert (tinfo_type != NULL_TREE, 20000120);
  
  if (!DECL_NEEDED_P (tinfo_decl))
    return 0;
  /* Say we've dealt with it.  */
  TREE_TYPE (DECL_NAME (tinfo_decl)) = NULL_TREE;
  
  create_tinfo_types ();
  decl = synthesize_tinfo_var (tinfo_type, DECL_ASSEMBLER_NAME (tinfo_decl));
  
  return decl != 0;
}
