/* RunTime Type Identification
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
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

/* C++ returns type information to the user in struct type_info
   objects. We also use type information to implement dynamic_cast and
   exception handlers. Type information for a particular type is
   indicated with an ABI defined structure derived from type_info.
   This would all be very straight forward, but for the fact that the
   runtime library provides the definitions of the type_info structure
   and the ABI defined derived classes. We cannot build declarations
   of them directly in the compiler, but we need to layout objects of
   their type.  Somewhere we have to lie.

   We define layout compatible POD-structs with compiler-defined names
   and generate the appropriate initializations for them (complete
   with explicit mention of their vtable). When we have to provide a
   type_info to the user we reinterpret_cast the internal compiler
   type to type_info.  A well formed program can only explicitly refer
   to the type_infos of complete types (& cv void).  However, we chain
   pointer type_infos to the pointed-to-type, and that can be
   incomplete.  We only need the addresses of such incomplete
   type_info objects for static initialization.

   The type information VAR_DECL of a type is held on the
   IDENTIFIER_GLOBAL_VALUE of the type's mangled name. That VAR_DECL
   will be the internal type.  It will usually have the correct
   internal type reflecting the kind of type it represents (pointer,
   array, function, class, inherited class, etc).  When the type it
   represents is incomplete, it will have the internal type
   corresponding to type_info.  That will only happen at the end of
   translation, when we are emitting the type info objects.  */

/* Accessors for the type_info objects. We need to remember several things
   about each of the type_info types. The global tree nodes such as
   bltn_desc_type_node are TREE_LISTs, and these macros are used to access
   the required information. */
/* The RECORD_TYPE of a type_info derived class. */
#define TINFO_PSEUDO_TYPE(NODE) TREE_TYPE (NODE)
/* The VAR_DECL of the vtable for the type_info derived class.
   This is only filled in at the end of the translation. */
#define TINFO_VTABLE_DECL(NODE) TREE_VALUE (NODE)
/* The IDENTIFIER_NODE naming the real class. */
#define TINFO_REAL_NAME(NODE) TREE_PURPOSE (NODE)

static tree build_headof PARAMS((tree));
static tree ifnonnull PARAMS((tree, tree));
static tree tinfo_name PARAMS((tree));
static tree build_dynamic_cast_1 PARAMS((tree, tree));
static tree throw_bad_cast PARAMS((void));
static tree throw_bad_typeid PARAMS((void));
static tree get_tinfo_decl_dynamic PARAMS((tree));
static tree get_tinfo_ptr PARAMS((tree));
static bool typeid_ok_p PARAMS((void));
static int qualifier_flags PARAMS((tree));
static int target_incomplete_p PARAMS((tree));
static tree tinfo_base_init PARAMS((tree, tree));
static tree generic_initializer PARAMS((tree, tree));
static tree ptr_initializer PARAMS((tree, tree, int *));
static tree ptm_initializer PARAMS((tree, tree, int *));
static tree dfs_class_hint_mark PARAMS ((tree, void *));
static tree dfs_class_hint_unmark PARAMS ((tree, void *));
static int class_hint_flags PARAMS((tree));
static tree class_initializer PARAMS((tree, tree, tree));
static tree create_pseudo_type_info PARAMS((const char *, int, ...));
static tree get_pseudo_ti_init PARAMS ((tree, tree, int *));
static tree get_pseudo_ti_desc PARAMS((tree));
static void create_tinfo_types PARAMS((void));
static int typeinfo_in_lib_p PARAMS((tree));

static int doing_runtime = 0;


/* Declare language defined type_info type and a pointer to const
   type_info.  This is incomplete here, and will be completed when
   the user #includes <typeinfo>.  There are language defined
   restrictions on what can be done until that is included.  Create
   the internal versions of the ABI types.  */

void
init_rtti_processing ()
{
  push_namespace (std_identifier);
  type_info_type_node = xref_tag
    (class_type_node, get_identifier ("type_info"), 1);
  pop_namespace ();
  type_info_ptr_type = 
    build_pointer_type
     (build_qualified_type (type_info_type_node, TYPE_QUAL_CONST));

  create_tinfo_types ();
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
  tree offset;
  tree index;

  my_friendly_assert (TREE_CODE (type) == POINTER_TYPE, 20000112);
  type = TREE_TYPE (type);

  if (!TYPE_POLYMORPHIC_P (type))
    return exp;

  /* We use this a couple of times below, protect it.  */
  exp = save_expr (exp);

  /* The offset-to-top field is at index -2 from the vptr.  */
  index = build_int_2 (-2, -1);

  offset = build_vtbl_ref (build_indirect_ref (exp, NULL), index);

  type = build_qualified_type (ptr_type_node, 
			       cp_type_quals (TREE_TYPE (exp)));
  return build (PLUS_EXPR, type, exp,
		cp_convert (ptrdiff_type_node, offset));
}

/* Get a bad_cast node for the program to throw...

   See libstdc++/exception.cc for __throw_bad_cast */

static tree
throw_bad_cast ()
{
  tree fn = get_identifier ("__cxa_bad_cast");
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
  tree fn = get_identifier ("__cxa_bad_typeid");
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
  
  if (!VOID_TYPE_P (type))
    type = complete_type_or_else (type, exp);
  
  if (!type)
    return error_mark_node;

  /* If exp is a reference to polymorphic type, get the real type_info.  */
  if (TYPE_POLYMORPHIC_P (type) && ! resolves_to_fixed_type_p (exp, 0))
    {
      /* build reference to type_info from vtable.  */
      tree t;
      tree index;

      /* The RTTI information is at index -1.  */
      index = integer_minus_one_node;
      t = build_vtbl_ref (exp, index);
      TREE_TYPE (t) = type_info_ptr_type;
      return t;
    }

  /* Otherwise return the type_info for the static type of the expr.  */
  return get_tinfo_ptr (TYPE_MAIN_VARIANT (type));
}

static bool
typeid_ok_p ()
{
  if (! flag_rtti)
    {
      error ("cannot use typeid with -fno-rtti");
      return false;
    }
  
  if (!COMPLETE_TYPE_P (type_info_type_node))
    {
      error ("must #include <typeinfo> before using typeid");
      return false;
    }
  
  return true;
}

tree
build_typeid (exp)
     tree exp;
{
  tree cond = NULL_TREE;
  int nonnull = 0;

  if (exp == error_mark_node || !typeid_ok_p ())
    return error_mark_node;

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

  exp = build_indirect_ref (exp, NULL);

  if (cond)
    {
      tree bad = throw_bad_typeid ();

      exp = build (COND_EXPR, TREE_TYPE (exp), cond, exp, bad);
    }

  return convert_from_reference (exp);
}

/* Generate the NTBS name of a type.  */
static tree
tinfo_name (type)
     tree type;
{
  const char *name;
  tree name_string;

  name = mangle_type_string (type);
  name_string = combine_strings (build_string (strlen (name) + 1, name));
  return name_string;
}

/* Return a VAR_DECL for the internal ABI defined type_info object for
   TYPE. You must arrange that the decl is mark_used, if actually use
   it --- decls in vtables are only used if the vtable is output.  */ 

tree
get_tinfo_decl (type)
     tree type;
{
  tree name;
  tree d;

  if (COMPLETE_TYPE_P (type) 
      && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    {
      error ("cannot create type information for type `%T' because its size is variable", 
	     type);
      return error_mark_node;
    }

  if (TREE_CODE (type) == OFFSET_TYPE)
    type = TREE_TYPE (type);
  if (TREE_CODE (type) == METHOD_TYPE)
    type = build_function_type (TREE_TYPE (type),
				TREE_CHAIN (TYPE_ARG_TYPES (type)));

  /* For a class type, the variable is cached in the type node
     itself.  */
  if (CLASS_TYPE_P (type))
    {
      d = CLASSTYPE_TYPEINFO_VAR (TYPE_MAIN_VARIANT (type));
      if (d)
	return d;
    }
    
  name = mangle_typeinfo_for_type (type);

  d = IDENTIFIER_GLOBAL_VALUE (name);
  if (!d)
    {
      tree var_desc = get_pseudo_ti_desc (type);

      d = build_lang_decl (VAR_DECL, name, TINFO_PSEUDO_TYPE (var_desc));
      
      DECL_ARTIFICIAL (d) = 1;
      TREE_READONLY (d) = 1;
      TREE_STATIC (d) = 1;
      DECL_EXTERNAL (d) = 1;
      SET_DECL_ASSEMBLER_NAME (d, name);
      DECL_COMDAT (d) = 1;
      cp_finish_decl (d, NULL_TREE, NULL_TREE, 0);

      pushdecl_top_level (d);

      if (CLASS_TYPE_P (type))
	CLASSTYPE_TYPEINFO_VAR (TYPE_MAIN_VARIANT (type)) = d;

      /* Remember the type it is for.  */
      TREE_TYPE (name) = type;
    }

  return d;
}

/* Return a pointer to a type_info object describing TYPE, suitably
   cast to the language defined type.  */

static tree
get_tinfo_ptr (type)
     tree type;
{
  tree exp = get_tinfo_decl (type);
  
   /* Convert to type_info type.  */
  exp = build_unary_op (ADDR_EXPR, exp, 0);
  exp = ocp_convert (type_info_ptr_type, exp, CONV_REINTERPRET, 0);

  return exp;
}

/* Return the type_info object for TYPE.  */

tree
get_typeid (type)
     tree type;
{
  if (type == error_mark_node || !typeid_ok_p ())
    return error_mark_node;
  
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

  if (!VOID_TYPE_P (type))
    type = complete_type_or_else (type, NULL_TREE);
  
  if (!type)
    return error_mark_node;

  return build_indirect_ref (get_tinfo_ptr (type), NULL);
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
    tree binfo;

    binfo = lookup_base (TREE_TYPE (exprtype), TREE_TYPE (type),
			 ba_not_special, NULL);

    if (binfo)
      {
	expr = build_base_path (PLUS_EXPR, convert_from_reference (expr),
				binfo, 0);
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
      if (tc == POINTER_TYPE && VOID_TYPE_P (TREE_TYPE (type)))
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
		  warning ("dynamic_cast of `%#D' to `%#T' can never succeed",
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
		  warning ("dynamic_cast of `%#D' to `%#T' can never succeed",
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
	      tree ns = abi_node;
	      const char *name;
	      
	      push_nested_namespace (ns);
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
	      tmp = build_function_type (ptr_type_node, tmp);
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
  error ("cannot dynamic_cast `%E' (of type `%#T') to type `%#T' (%s)",
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
  tree vtable_ptr;
  
  {
    tree name_name;
    
    /* Generate the NTBS array variable.  */
    tree name_type = build_cplus_array_type
                     (build_qualified_type (char_type_node, TYPE_QUAL_CONST),
                     NULL_TREE);
    tree name_string = tinfo_name (target);

    name_name = mangle_typeinfo_string_for_type (target);
    name_decl = build_lang_decl (VAR_DECL, name_name, name_type);
    
    DECL_ARTIFICIAL (name_decl) = 1;
    TREE_READONLY (name_decl) = 1;
    TREE_STATIC (name_decl) = 1;
    DECL_EXTERNAL (name_decl) = 0;
    TREE_PUBLIC (name_decl) = 1;
    comdat_linkage (name_decl);
    /* External name of the string containing the type's name has a
       special name.  */
    SET_DECL_ASSEMBLER_NAME (name_decl,
			     mangle_typeinfo_string_for_type (target));
    DECL_INITIAL (name_decl) = name_string;
    cp_finish_decl (name_decl, name_string, NULL_TREE, 0);
    pushdecl_top_level (name_decl);
  }

  vtable_ptr = TINFO_VTABLE_DECL (desc);
  if (!vtable_ptr)
    {
      tree real_type;
  
      push_nested_namespace (abi_node);
      real_type = xref_tag (class_type_node, TINFO_REAL_NAME (desc), 1);
      pop_nested_namespace (abi_node);

      if (!COMPLETE_TYPE_P (real_type))
	{
          /* We never saw a definition of this type, so we need to
	     tell the compiler that this is an exported class, as
	     indeed all of the __*_type_info classes are.  */
	  SET_CLASSTYPE_INTERFACE_KNOWN (real_type);
	  CLASSTYPE_INTERFACE_ONLY (real_type) = 1;
	}

      vtable_ptr = get_vtable_decl (real_type, /*complete=*/1);
      vtable_ptr = build_unary_op (ADDR_EXPR, vtable_ptr, 0);

      /* We need to point into the middle of the vtable.  */
      vtable_ptr = build
	(PLUS_EXPR, TREE_TYPE (vtable_ptr), vtable_ptr,
	 size_binop (MULT_EXPR,
		     size_int (2),
		     TYPE_SIZE_UNIT (vtable_entry_type)));
      TREE_CONSTANT (vtable_ptr) = 1;

      TINFO_VTABLE_DECL (desc) = vtable_ptr;
    }

  init = tree_cons (NULL_TREE, vtable_ptr, init);
  
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
                    get_tinfo_ptr (TYPE_MAIN_VARIANT (to)),
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
ptm_initializer (desc, target, non_public_ptr)
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
    {
      flags |= 0x10;
      *non_public_ptr = 1;
    }
  init = tree_cons (NULL_TREE, build_int_2 (flags, 0), init);
  init = tree_cons (NULL_TREE,
		    get_tinfo_ptr (TYPE_MAIN_VARIANT (to)),
                    init);
  init = tree_cons (NULL_TREE,
		    get_tinfo_ptr (klass),
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

/* Determine the hint flags describing the features of a class's hierarchy.  */

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

/* Returns non-zero if the typeinfo for type should be placed in 
   the runtime library.  */

static int
typeinfo_in_lib_p (type)
     tree type;
{
  /* The typeinfo objects for `T*' and `const T*' are in the runtime
     library for simple types T.  */
  if (TREE_CODE (type) == POINTER_TYPE
      && (cp_type_quals (TREE_TYPE (type)) == TYPE_QUAL_CONST
	  || cp_type_quals (TREE_TYPE (type)) == TYPE_UNQUALIFIED))
    type = TREE_TYPE (type);

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
    case REAL_TYPE:
    case VOID_TYPE:
      return 1;
    
    default:
      return 0;
    }
}

/* Generate the initializer for the type info describing
   TYPE. VAR_DESC is a . NON_PUBLIC_P is set non-zero, if the VAR_DECL
   should not be exported from this object file.  This should only be
   called at the end of translation, when we know that no further
   types will be completed.  */

static tree
get_pseudo_ti_init (type, var_desc, non_public_p)
     tree type;
     tree var_desc;
     int *non_public_p;
{
  my_friendly_assert (at_eof, 20021120);
  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
      if (TYPE_PTRMEM_P (type))
	return ptm_initializer (var_desc, type, non_public_p);
      else
	return ptr_initializer (var_desc, type, non_public_p);
      break;
    case ENUMERAL_TYPE:
      return generic_initializer (var_desc, type);
      break;
    case FUNCTION_TYPE:
      return generic_initializer (var_desc, type);
      break;
    case ARRAY_TYPE:
      return generic_initializer (var_desc, type);
      break;
    case UNION_TYPE:
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (type))
	return ptm_initializer (var_desc, type, non_public_p);
      else if (var_desc == class_desc_type_node)
        {
	  if (!COMPLETE_TYPE_P (type))
	    /* Emit a non-public class_type_info.  */
	    *non_public_p = 1;
	  return class_initializer (var_desc, type, NULL_TREE);
        }
      else if (var_desc == si_class_desc_type_node)
	{
          tree base_binfos = BINFO_BASETYPES (TYPE_BINFO (type));
	  tree base_binfo = TREE_VEC_ELT (base_binfos, 0);
	  tree tinfo = get_tinfo_ptr (BINFO_TYPE (base_binfo));
	  tree base_inits = tree_cons (NULL_TREE, tinfo, NULL_TREE);
	  
	  return class_initializer (var_desc, type, base_inits);
	}
      else
        {
	  int hint = class_hint_flags (type);
	  tree binfo = TYPE_BINFO (type);
          int nbases = BINFO_N_BASETYPES (binfo);
          tree base_binfos = BINFO_BASETYPES (binfo);
          tree base_inits = NULL_TREE;
          int ix;
          
          /* Generate the base information initializer.  */
          for (ix = nbases; ix--;)
            {
              tree base_binfo = TREE_VEC_ELT (base_binfos, ix);
              tree base_init = NULL_TREE;
              int flags = 0;
              tree tinfo;
              tree offset;
              
              if (TREE_PUBLIC (base_binfo))
                flags |= 2;
              tinfo = get_tinfo_ptr (BINFO_TYPE (base_binfo));
	      if (TREE_VIA_VIRTUAL (base_binfo))
		{
		   /* We store the vtable offset at which the virtual
       		      base offset can be found.  */
		  offset = BINFO_VPTR_FIELD
		    (binfo_for_vbase (BINFO_TYPE (base_binfo), type));
		  offset = convert (sizetype, offset);
		  flags |= 1;
		}
	      else
		offset = BINFO_OFFSET (base_binfo);
              
              /* combine offset and flags into one field */
              offset = cp_build_binary_op (LSHIFT_EXPR, offset,
					   build_int_2 (8, 0));
              offset = cp_build_binary_op (BIT_IOR_EXPR, offset,
					   build_int_2 (flags, 0));
              base_init = tree_cons (NULL_TREE, offset, base_init);
              base_init = tree_cons (NULL_TREE, tinfo, base_init);
              base_init = build (CONSTRUCTOR, NULL_TREE, NULL_TREE, base_init);
              base_inits = tree_cons (NULL_TREE, base_init, base_inits);
            }
	  base_inits = build (CONSTRUCTOR,
			      NULL_TREE, NULL_TREE, base_inits);
	  base_inits = tree_cons (NULL_TREE, base_inits, NULL_TREE);
	  /* Prepend the number of bases.  */
	  base_inits = tree_cons (NULL_TREE,
				  build_int_2 (nbases, 0), base_inits);
	  /* Prepend the hint flags. */
	  base_inits = tree_cons (NULL_TREE,
				  build_int_2 (hint, 0), base_inits);

          return class_initializer (var_desc, type, base_inits);
        }
      break;

    default:
      return generic_initializer (var_desc, type);
    }
}

/* Generate the RECORD_TYPE containing the data layout of a type_info
   derivative as used by the runtime. This layout must be consistent with
   that defined in the runtime support. Also generate the VAR_DECL for the
   type's vtable. We explicitly manage the vtable member, and name it for
   real type as used in the runtime. The RECORD type has a different name,
   to avoid collisions.  Return a TREE_LIST who's TINFO_PSEUDO_TYPE
   is the generated type and TINFO_VTABLE_NAME is the name of the
   vtable.  We have to delay generating the VAR_DECL of the vtable
   until the end of the translation, when we'll have seen the library
   definition, if there was one.
   
   REAL_NAME is the runtime's name of the type. Trailing arguments are
   additional FIELD_DECL's for the structure. The final argument must be
   NULL.  */

static tree
create_pseudo_type_info VPARAMS((const char *real_name, int ident, ...))
{
  tree pseudo_type;
  char *pseudo_name;
  int ix;
  tree fields[10];
  tree field_decl;
  tree result;

  VA_OPEN (ap, ident);
  VA_FIXEDARG (ap, const char *, real_name);
  VA_FIXEDARG (ap, int, ident);

  /* Generate the pseudo type name. */
  pseudo_name = (char *)alloca (strlen (real_name) + 30);
  strcpy (pseudo_name, real_name);
  strcat (pseudo_name, "_pseudo");
  if (ident)
    sprintf (pseudo_name + strlen (pseudo_name), "%d", ident);
  
  /* First field is the pseudo type_info base class. */
  fields[0] = build_decl (FIELD_DECL, NULL_TREE, ti_desc_type_node);
  
  /* Now add the derived fields.  */
  for (ix = 0; (field_decl = va_arg (ap, tree));)
    fields[++ix] = field_decl;
  
  /* Create the pseudo type. */
  pseudo_type = make_aggr_type (RECORD_TYPE);
  finish_builtin_type (pseudo_type, pseudo_name, fields, ix, ptr_type_node);
  TYPE_HAS_CONSTRUCTOR (pseudo_type) = 1;

  result = tree_cons (NULL_TREE, NULL_TREE, NULL_TREE);
  TINFO_REAL_NAME (result) = get_identifier (real_name);
  TINFO_PSEUDO_TYPE (result) =
    cp_build_qualified_type (pseudo_type, TYPE_QUAL_CONST);
  
  VA_CLOSE (ap);
  return result;
}

/* Return a pseudo type info type node used to describe TYPE.  TYPE
   must be a complete type (or cv void), except at the end of the
   translation unit.  */

static tree
get_pseudo_ti_desc (type)
     tree type;
{
  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
      return TYPE_PTRMEM_P (type) ? ptm_desc_type_node : ptr_desc_type_node;
    case ENUMERAL_TYPE:
      return enum_desc_type_node;
    case FUNCTION_TYPE:
      return func_desc_type_node;
    case ARRAY_TYPE:
      return ary_desc_type_node;
    case UNION_TYPE:
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (type))
	return ptm_desc_type_node;
      else if (!COMPLETE_TYPE_P (type))
	{
	  my_friendly_assert (at_eof, 20020609);
	  return class_desc_type_node;
	}
      else if (!CLASSTYPE_N_BASECLASSES (type))
	return class_desc_type_node;
      else
	{
	  tree base_binfo =
	    TREE_VEC_ELT (BINFO_BASETYPES (TYPE_BINFO (type)), 0);
	  int num_bases = BINFO_N_BASETYPES (TYPE_BINFO (type));
	  
	  if (num_bases == 1
	      && TREE_PUBLIC (base_binfo)
	      && !TREE_VIA_VIRTUAL (base_binfo)
	      && integer_zerop (BINFO_OFFSET (base_binfo)))
	    /* single non-virtual public. */
	    return si_class_desc_type_node;
	  else
	    {
	      tree var_desc;
	      tree array_domain, base_array;
	      
	      if (TREE_VEC_LENGTH (vmi_class_desc_type_node) <= num_bases)
		{
		  int ix;
		  tree extend = make_tree_vec (num_bases + 5);
		  
		  for (ix = TREE_VEC_LENGTH (vmi_class_desc_type_node); ix--;)
		    TREE_VEC_ELT (extend, ix)
		      = TREE_VEC_ELT (vmi_class_desc_type_node, ix);
		  vmi_class_desc_type_node = extend;
		}
	      var_desc = TREE_VEC_ELT (vmi_class_desc_type_node, num_bases);
	      if (var_desc)
		return var_desc;
  
	      /* Add number of bases and trailing array of
		 base_class_type_info.  */
	      array_domain = build_index_type (size_int (num_bases));
	      base_array =
		build_array_type (base_desc_type_node, array_domain);

	      push_nested_namespace (abi_node);
	      var_desc = create_pseudo_type_info
		("__vmi_class_type_info", num_bases,
		 build_decl (FIELD_DECL, NULL_TREE, integer_type_node),
		 build_decl (FIELD_DECL, NULL_TREE, integer_type_node),
		 build_decl (FIELD_DECL, NULL_TREE, base_array),
		 NULL);
	      pop_nested_namespace (abi_node);

	      TREE_VEC_ELT (vmi_class_desc_type_node, num_bases) = var_desc;
	      return var_desc;
	    }
	}
    default:
      return bltn_desc_type_node;
    }
}

/* Make sure the required builtin types exist for generating the type_info
   varable definitions.  */

static void
create_tinfo_types ()
{
  my_friendly_assert (!ti_desc_type_node, 20020609);

  push_nested_namespace (abi_node);
  
  /* Create the internal type_info structure. This is used as a base for
     the other structures.  */
  {
    tree fields[2];

    ti_desc_type_node = make_aggr_type (RECORD_TYPE);
    fields[0] = build_decl (FIELD_DECL, NULL_TREE, const_ptr_type_node);
    fields[1] = build_decl (FIELD_DECL, NULL_TREE, const_string_type_node);
    finish_builtin_type (ti_desc_type_node, "__type_info_pseudo",
                         fields, 1, ptr_type_node);
    TYPE_HAS_CONSTRUCTOR (ti_desc_type_node) = 1;
  }
  
  /* Fundamental type_info */
  bltn_desc_type_node = create_pseudo_type_info
      ("__fundamental_type_info", 0,
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
            build_decl (FIELD_DECL, NULL_TREE, type_info_ptr_type),
            NULL);
  
  /* Base class internal helper. Pointer to base type, offset to base,
     flags. */
  {
    tree fields[2];
    
    fields[0] = build_decl (FIELD_DECL, NULL_TREE, type_info_ptr_type);
    fields[1] = build_decl (FIELD_DECL, NULL_TREE, integer_types[itk_long]);
    base_desc_type_node = make_aggr_type (RECORD_TYPE);
    finish_builtin_type (base_desc_type_node, "__base_class_type_info_pseudo",
                         fields, 1, ptr_type_node);
    TYPE_HAS_CONSTRUCTOR (base_desc_type_node) = 1;
  }
  
  /* General hierarchy is created as necessary in this vector. */
  vmi_class_desc_type_node = make_tree_vec (10);
  
  /* Pointer type_info. Adds two fields, qualification mask
     and pointer to the pointed to type.  This is really a descendant of
     __pbase_type_info. */
  ptr_desc_type_node = create_pseudo_type_info
      ("__pointer_type_info", 0,
       build_decl (FIELD_DECL, NULL_TREE, integer_type_node),
       build_decl (FIELD_DECL, NULL_TREE, type_info_ptr_type),
       NULL);

  /* Pointer to member data type_info.  Add qualifications flags,
     pointer to the member's type info and pointer to the class.
     This is really a descendant of __pbase_type_info.  */
  ptm_desc_type_node = create_pseudo_type_info
       ("__pointer_to_member_type_info", 0,
        build_decl (FIELD_DECL, NULL_TREE, integer_type_node),
        build_decl (FIELD_DECL, NULL_TREE, type_info_ptr_type),
        build_decl (FIELD_DECL, NULL_TREE, type_info_ptr_type),
        NULL);

  pop_nested_namespace (abi_node);
}

/* Emit the type_info descriptors which are guaranteed to be in the runtime
   support.  Generating them here guarantees consistency with the other
   structures.  We use the following heuristic to determine when the runtime
   is being generated.  If std::__fundamental_type_info is defined, and its
   destructor is defined, then the runtime is being built.  */

void
emit_support_tinfos ()
{
  static tree *const fundamentals[] =
  {
    &void_type_node,
    &boolean_type_node,
    &wchar_type_node,
    &char_type_node, &signed_char_type_node, &unsigned_char_type_node,
    &short_integer_type_node, &short_unsigned_type_node,
    &integer_type_node, &unsigned_type_node,
    &long_integer_type_node, &long_unsigned_type_node,
    &long_long_integer_type_node, &long_long_unsigned_type_node,
    &float_type_node, &double_type_node, &long_double_type_node,
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
unemitted_tinfo_decl_p (t, data)
     tree t;
     void *data ATTRIBUTE_UNUSED;
{
  if (/* It's a var decl */
      TREE_CODE (t) == VAR_DECL
      /* whos name points back to itself */
      && IDENTIFIER_GLOBAL_VALUE (DECL_NAME (t)) == t
      /* whose name's type is non-null */
      && TREE_TYPE (DECL_NAME (t))
      /* and whose type is a struct */
      && TREE_CODE (TREE_TYPE (t)) == RECORD_TYPE
      /* with a field */
      && TYPE_FIELDS (TREE_TYPE (t))
      /* which is our pseudo type info */
      && TREE_TYPE (TYPE_FIELDS (TREE_TYPE (t))) == ti_desc_type_node)
    return 1;
  return 0;
}

/* Finish a type info decl. DECL_PTR is a pointer to an unemitted
   tinfo decl.  Determine whether it needs emitting, and if so
   generate the initializer.  */

int
emit_tinfo_decl (decl_ptr, data)
     tree *decl_ptr;
     void *data ATTRIBUTE_UNUSED;
{
  tree decl = *decl_ptr;
  tree type = TREE_TYPE (DECL_NAME (decl));
  int non_public;
  int in_library = typeinfo_in_lib_p (type);
  tree var_desc, var_init;
  
  import_export_tinfo (decl, type, in_library);
  if (DECL_REALLY_EXTERN (decl) || !DECL_NEEDED_P (decl))
    return 0;

  if (!doing_runtime && in_library)
    return 0;

  non_public = 0;
  var_desc = get_pseudo_ti_desc (type);
  var_init = get_pseudo_ti_init (type, var_desc, &non_public);
  
  DECL_EXTERNAL (decl) = 0;
  TREE_PUBLIC (decl) = !non_public;
  if (non_public)
    DECL_COMDAT (decl) = 0;

  DECL_INITIAL (decl) = var_init;
  cp_finish_decl (decl, var_init, NULL_TREE, 0);
  /* cp_finish_decl will have dealt with linkage. */
  
  /* Say we've dealt with it.  */
  TREE_TYPE (DECL_NAME (decl)) = NULL_TREE;

  return 1;
}
