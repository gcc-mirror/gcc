/* RunTime Type Identification
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.

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
#include "tree.h"
#include "cp-tree.h"
#include "flags.h"
#include "output.h"

#undef NULL
#define NULL 0

extern tree define_function ();
extern tree build_t_desc_overload ();
extern struct obstack *permanent_obstack;

/* in c-common.c */
extern tree combine_strings PROTO((tree));

/* Given the expression EXP of type `class *', return the head
   of the object pointed to by EXP.  */
static tree
build_headof (exp)
     tree exp;
{
  tree type = TREE_TYPE (exp);
  tree vptr, offset;

  if (TREE_CODE (type) != POINTER_TYPE)
    {
      error ("`headof' applied to non-pointer type");
      return error_mark_node;
    }
  type = TREE_TYPE (type);

  if (!TYPE_VIRTUAL_P (type) || CLASSTYPE_VFIELD (type) == NULL_TREE)
    return exp;

  vptr = fold (size_binop (PLUS_EXPR, 
	   size_binop (FLOOR_DIV_EXPR, 
  	     DECL_FIELD_BITPOS (CLASSTYPE_VFIELD (type)),
	     size_int (BITS_PER_UNIT)),
 	   exp)); 
  vptr = build1 (INDIRECT_REF, build_pointer_type (vtable_entry_type), vptr);

  if (flag_vtable_thunks)
    offset = build_array_ref (vptr, integer_zero_node);
  else
    offset = build_component_ref (build_array_ref (vptr, integer_zero_node),
				  delta_identifier,
				  NULL_TREE, 0);

  type = build_type_variant (ptr_type_node, TREE_READONLY (exp),
			     TREE_THIS_VOLATILE (exp));
  return build (PLUS_EXPR, type, exp,
		convert (ptrdiff_type_node, offset));
}

/* Return the type_info node associated with the expression EXP.  If EXP is
   a reference to a polymorphic class, return the dynamic type; otherwise
   return the static type of the expression.  */
tree
build_typeid (exp)
     tree exp;
{
  tree type;

  if (!flag_rtti)
    cp_error ("cannot take typeid of object when -frtti is not specified");

  if (exp == error_mark_node)
    return error_mark_node;

  type = TREE_TYPE (exp);

  /* Strip top-level cv-qualifiers.  */
  type = TYPE_MAIN_VARIANT (type);

  /* if b is an instance of B, typeid(b) == typeid(B).  Do this before
     reference trickiness.  */
  if (TREE_CODE (exp) == VAR_DECL && TREE_CODE (type) == RECORD_TYPE)
    return get_typeid (type);

  /* peel back references, so they match. */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* Peel off cv qualifiers. */
  type = TYPE_MAIN_VARIANT (type);

  /* Apply trivial conversion T -> T& for dereferenced ptrs.  */
  if (TREE_CODE (type) == RECORD_TYPE)
    type = build_reference_type (type);

  /* If exp is a reference to polymorphic type, get the real type_info.  */
  if (TREE_CODE (type) == REFERENCE_TYPE && TYPE_VIRTUAL_P (TREE_TYPE (type)))
    {
      /* build reference to type_info from vtable.  */
      tree t;

      if (flag_vtable_thunks)
	t = build_vfn_ref ((tree *) NULL_TREE, exp, integer_one_node);
      else
	t = build_vfn_ref ((tree *) NULL_TREE, exp, integer_zero_node);

      TREE_TYPE (t) = build_pointer_type (__class_desc_type_node);
      t = build_indirect_ref (t, NULL);
      return t;
    }

  /* otherwise return the type_info for the static type of the expr.  */
  return get_typeid (type);
}

/* Return the type_info object for TYPE, creating it if necessary.  */
tree
get_typeid (type)
     tree type;
{
  tree t, td;

  if (type == error_mark_node)
    return error_mark_node;
  
  /* Is it useful (and/or correct) to have different typeids for `T &'
     and `T'?  */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  td = build_t_desc (type, 1);
  if (td == error_mark_node)
    return error_mark_node;

  t = TREE_OPERAND (td, 0);
  return t;
}

/* Get a bad_cast node for the program to throw...

   See libstdc++::exception{,.cc} for __bad_cast_object */
static tree
get_bad_cast_node ()
{
  static tree t;
  if (t == NULL_TREE
      && (t = lookup_name (get_identifier ("__bad_cast_object"), 0))
         == NULL_TREE)
    {
      error ("you must #include <typeinfo>");
      return error_mark_node;
    }
  return t;
}

/* Execute a dynamic cast, as described in section 5.2.6 of the 9/93 working
   paper.  */
tree
build_dynamic_cast (type, expr)
     tree type, expr;
{
  enum tree_code tc = TREE_CODE (type);
  tree exprtype = TREE_TYPE (expr);
  enum tree_code ec = TREE_CODE (exprtype);

  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;
  
  switch (tc)
    {
    case POINTER_TYPE:
      if (ec == REFERENCE_TYPE)
	{
	  expr = convert_from_reference (expr);
	  exprtype = TREE_TYPE (expr);
	  ec = TREE_CODE (exprtype);
	}
      if (ec != POINTER_TYPE)
	goto fail;
      if (TREE_CODE (TREE_TYPE (exprtype)) != RECORD_TYPE)
	goto fail;
      if (TYPE_SIZE (TREE_TYPE (exprtype)) == 0)
	goto fail;
      if (TREE_READONLY (TREE_TYPE (exprtype)) &&
	  ! TYPE_READONLY (TREE_TYPE (type)))
	goto fail;
      if (TYPE_MAIN_VARIANT (TREE_TYPE (type)) == void_type_node)
	break;
      /* else fall through */
    case REFERENCE_TYPE:
      if (TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE
	  && TYPE_SIZE (TREE_TYPE (type)) != NULL_TREE)
	break;
      /* else fall through */
    default:
      goto fail;
    }

  /* Apply trivial conversion T -> T& for dereferenced ptrs.  */
  if (ec == RECORD_TYPE)
    {
      exprtype = build_type_variant (exprtype, TREE_READONLY (expr),
				     TREE_THIS_VOLATILE (expr));
      exprtype = build_reference_type (exprtype);
      expr = convert_to_reference (exprtype, expr, CONV_IMPLICIT,
				   LOOKUP_NORMAL, NULL_TREE);
      ec = REFERENCE_TYPE;
    }

  if (tc == REFERENCE_TYPE)
    {
      if (ec != REFERENCE_TYPE)
	goto fail;
      if (TREE_CODE (TREE_TYPE (exprtype)) != RECORD_TYPE)
	goto fail;
      if (TYPE_SIZE (TREE_TYPE (exprtype)) == 0)
	goto fail;
      if (TREE_READONLY (TREE_TYPE (exprtype)) &&
	  ! TYPE_READONLY (TREE_TYPE (type)))
	goto fail;
    }

  /* If *type is an unambiguous accessible base class of *exprtype,
     convert statically.  */
  {
    int distance;
    tree path;

    distance = get_base_distance (TREE_TYPE (type), TREE_TYPE (exprtype), 1,
				  &path);
    if (distance >= 0)
      return build_vbase_path (PLUS_EXPR, type, expr, path, 0);
  }

  /* Otherwise *exprtype must be a polymorphic class (have a vtbl).  */
  if (TYPE_VIRTUAL_P (TREE_TYPE (exprtype)))
    {
      /* if TYPE is `void *', return pointer to complete object.  */
      if (tc == POINTER_TYPE
	  && TYPE_MAIN_VARIANT (TREE_TYPE (type)) == void_type_node)
	{
	  /* if b is an object, dynamic_cast<void *>(&b) == (void *)&b.  */
	  if (TREE_CODE (expr) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (expr, 0)) == VAR_DECL
	      && TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == RECORD_TYPE)
	    return build1 (NOP_EXPR, type, expr);

	  return build_headof (expr);
	}
      else
	{
	  tree retval;
          tree result, td1, td2, elems, expr1;

 	  /* If we got here, we can't convert statically.  Therefore,
	     dynamic_cast<D&>(b) (b an object) cannot succeed.  */
	  if (ec == REFERENCE_TYPE)
	    {
	      if (TREE_CODE (expr) == VAR_DECL
		  && TREE_CODE (TREE_TYPE (expr)) == RECORD_TYPE)
		{
		  cp_warning ("dynamic_cast of `%#D' to `%#T' can never succeed",
			      expr, type);
		  return build_throw (get_bad_cast_node ());
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
			      expr, type);
		  retval = build_int_2 (0, 0); 
		  TREE_TYPE (retval) = type; 
		  return retval;
		}
	    }

	  expr1 = expr;
	  if (tc == REFERENCE_TYPE)
	    expr1 = build_unary_op (ADDR_EXPR, expr1, 0);

	  /* Build run-time conversion.  */
	  expr1 = build_headof (expr1);

	  if (ec == POINTER_TYPE)
	    td1 = build_typeid (build_indirect_ref (expr, NULL_PTR));
	  else
	    td1 = build_typeid (expr);
	  
	  if (tc == POINTER_TYPE)
	    td2 = get_typeid (TREE_TYPE (type));
	  else
	    td2 = get_typeid (TYPE_MAIN_VARIANT (TREE_TYPE (type)));

          elems = tree_cons (NULL_TREE, td2,
            tree_cons (NULL_TREE, build_int_2 (1, 0),
	      tree_cons (NULL_TREE, expr1, NULL_TREE)));
          result = build_method_call (td1,
            get_identifier ("__rtti_match"), elems, NULL_TREE, LOOKUP_NORMAL);

	  if (tc == REFERENCE_TYPE)
	    {
	      expr1 = build_throw (get_bad_cast_node ());
	      expr1 = build_compound_expr (tree_cons (NULL_TREE, expr1,
						      build_tree_list (NULL_TREE, convert (type, integer_zero_node))));
	      TREE_TYPE (expr1) = type;
	      result = save_expr (result);
	      return build (COND_EXPR, type, result, result, expr1);
	    }

	  /* Now back to the type we want from a void*. */
	  result = convert (type, result);
          return result;
	}
    }

 fail:
  cp_error ("cannot dynamic_cast `%E' (of type `%#T') to type `%#T'",
	    expr, exprtype, type);
  return error_mark_node;
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

static tree
build_generic_desc (tdecl, type, elems)
     tree tdecl;
     tree type;
     tree elems;
{
  tree init = elems;
  int toplev = global_bindings_p ();

  TREE_CONSTANT (init) = 1;
  TREE_STATIC (init) = 1;
  TREE_READONLY (init) = 1;

  TREE_TYPE (tdecl) = type;
  DECL_INITIAL (tdecl) = init;
  TREE_STATIC (tdecl) = 1;
  DECL_SIZE (tdecl) = NULL_TREE;
  layout_decl (tdecl, 0);
  if (! toplev)
    push_to_top_level ();
  cp_finish_decl (tdecl, init, NULL_TREE, 0, 0);
  if (! toplev)
    pop_from_top_level ();

  if (! TREE_USED (tdecl))
    {
      assemble_external (tdecl);
      TREE_USED (tdecl) = 1;
    }

  return IDENTIFIER_AS_DESC (DECL_NAME (tdecl));
}

/* Build an initializer for a __bltn_desc node.  */
static tree
build_bltn_desc (tdecl, type)
     tree tdecl;
     tree type;
{
  tree elems, t;

  if (type == boolean_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_BOOL"),
		      0, 0);
  else if (type == char_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_CHAR"),
		      0, 0);
  else if (type == short_integer_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_SHORT"),
		      0, 0);
  else if (type == integer_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_INT"),
		      0, 0);
  else if (type == long_integer_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_LONG"),
		      0, 0);
  else if (type == long_long_integer_type_node)
    t = lookup_field (__bltn_desc_type_node, 
		      get_identifier("_RTTI_BI_LONGLONG"), 0, 0);
  else if (type == float_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_FLOAT"),
		      0, 0);
  else if (type == double_type_node)
    t = lookup_field (__bltn_desc_type_node, 
		      get_identifier("_RTTI_BI_DOUBLE"), 0, 0);
  else if (type == long_double_type_node)
    t = lookup_field (__bltn_desc_type_node, 
		      get_identifier("_RTTI_BI_LDOUBLE"), 0, 0);
  else if (type == unsigned_char_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_UCHAR"),
		      0, 0);
  else if (type == short_unsigned_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_USHORT"),
		      0, 0);
  else if (type == unsigned_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_UINT"),
		      0, 0);
  else if (type == long_unsigned_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_ULONG"),
		      0, 0);
  else if (type == long_long_unsigned_type_node)
    t = lookup_field (__bltn_desc_type_node, 
		      get_identifier("_RTTI_BI_ULONGLONG"), 0, 0);
  else if (type == signed_char_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_SCHAR"),
		      0, 0);
  else if (type == wchar_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_WCHAR"),
		      0, 0);
  else if (type == void_type_node)
    t = lookup_field (__bltn_desc_type_node, get_identifier("_RTTI_BI_VOID"),
		      0, 0);
  else
    {
      cp_compiler_error ("type `%T' not handled as a built-in type");
    }

  elems = tree_cons (NULL_TREE, t, NULL_TREE);
  return build_generic_desc (tdecl, __bltn_desc_type_node, elems);
}

/* Build an initializer for a __user_desc node.  */
static tree
build_user_desc (tdecl)
     tree tdecl;
{
  tree elems, name_string;
  tree tname = DECL_NAME (tdecl);

  name_string = combine_strings (build_string 
    (IDENTIFIER_LENGTH (tname)+1, IDENTIFIER_POINTER (tname)));
  elems = name_string;
  return build_generic_desc (tdecl, __user_desc_type_node, elems);
}

/* Build an initializer for a __class_type_info node. */
static tree
build_class_desc (tdecl, type)
     tree tdecl;
     tree type;
{
  tree tname = DECL_NAME (tdecl);
  tree name_string;

  int i = CLASSTYPE_N_BASECLASSES (type);
  int base_cnt = 0;
  tree binfos = TYPE_BINFO_BASETYPES (type);
#if 0
  /* See code below that used these.  */
  tree vb = CLASSTYPE_VBASECLASSES (type);
  int n_base = i;
#endif
  tree base, elems, access, offset, isvir;
  tree base_list, off_list, acc_list, isvir_list;
  static tree acc_pub = NULL_TREE;
  static tree acc_pro = NULL_TREE;
  static tree acc_pri = NULL_TREE;

  if (acc_pub == NULL_TREE) 
    {
      acc_pub = lookup_field (__class_desc_type_node, 
                                get_identifier("_RTTI_ACCESS_PUBLIC"), 0, 0);
      acc_pro = lookup_field (__class_desc_type_node,
                                get_identifier("_RTTI_ACCESS_PROTECTED"), 0, 0);
      acc_pri = lookup_field (__class_desc_type_node,
                                get_identifier("_RTTI_ACCESS_PRIVATE"), 0, 0);
    }

  base_list = build_tree_list (NULL_TREE, integer_zero_node);
  off_list = build_tree_list (NULL_TREE, integer_zero_node);
  acc_list = build_tree_list (NULL_TREE, integer_zero_node);
  isvir_list = build_tree_list (NULL_TREE, integer_zero_node);
  while (--i >= 0)
    {
      tree binfo = TREE_VEC_ELT (binfos, i);

      base = build_t_desc (BINFO_TYPE (binfo), 1);
      if (TREE_VIA_VIRTUAL (binfo))
	{
	  tree t = BINFO_TYPE (binfo);
	  char *name;
	  tree field;

	  name = (char *) alloca (TYPE_NAME_LENGTH (t)+sizeof (VBASE_NAME)+1);
	  sprintf (name, VBASE_NAME_FORMAT, TYPE_NAME_STRING (t));
	  field = lookup_field (type, get_identifier (name), 0, 0);
	  offset = size_binop (FLOOR_DIV_EXPR, 
		DECL_FIELD_BITPOS (field), size_int (BITS_PER_UNIT));
	}
      else
	offset = BINFO_OFFSET (binfo);

      if (TREE_VIA_PUBLIC (binfo))
        access = acc_pub;
      else if (TREE_VIA_PROTECTED (binfo))
	access = acc_pro;
      else
	access = acc_pri;
      if (TREE_VIA_VIRTUAL (binfo))
	isvir = build_int_2 (1, 0);
      else
	isvir = build_int_2 (0, 0);

      base_list = tree_cons (NULL_TREE, base, base_list);
      isvir_list = tree_cons (NULL_TREE, isvir, isvir_list);
      acc_list = tree_cons (NULL_TREE, access, acc_list);
      off_list = tree_cons (NULL_TREE, offset, off_list);
      base_cnt++;
    }
#if 0
  i = n_base;
  while (vb)
    {
      tree b;
      access = acc_pub;
      while (--i >= 0)
	{
	  b = TREE_VEC_ELT (binfos, i);
	  if (BINFO_TYPE (vb) == BINFO_TYPE (b) && TREE_VIA_VIRTUAL (b))
	    {
	      if (TREE_VIA_PUBLIC (b))
		access = acc_pub;
	      else if (TREE_VIA_PROTECTED (b))
		access = acc_pro;
	      else
		access = acc_pri;
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
  base_list = finish_table (NULL_TREE, build_pointer_type (__t_desc_type_node), 
			    base_list, 0);
  off_list = finish_table (NULL_TREE, integer_type_node, 
			    off_list, 0);
  isvir_list = finish_table (NULL_TREE, integer_type_node, 
			    isvir_list, 0);
  acc_list = finish_table (NULL_TREE, __access_mode_type_node, 
			    acc_list, 0);


  name_string = combine_strings (build_string (IDENTIFIER_LENGTH (tname)+1, IDENTIFIER_POINTER (tname)));

  elems = tree_cons (NULL_TREE, name_string,
	    tree_cons (NULL_TREE, default_conversion (base_list),
	      tree_cons (NULL_TREE, default_conversion (off_list),
		tree_cons (NULL_TREE, default_conversion (isvir_list),
		  tree_cons (NULL_TREE, default_conversion (acc_list),
	      	    tree_cons (NULL_TREE, build_int_2 (base_cnt, 0), NULL_TREE))))));

  return build_generic_desc (tdecl, __class_desc_type_node, elems);
}

/* Build an initializer for a __pointer_type_info node.  */
static tree
build_ptr_desc (tdecl, type)
     tree tdecl;
     tree type;
{
  tree t, elems;

  t = TREE_TYPE (type);
  t = build_t_desc (t, 1);
  t = build_indirect_ref (t, NULL);
  elems = tree_cons (NULL_TREE, t, NULL_TREE);
  return build_generic_desc (tdecl, __ptr_desc_type_node,  elems);
}

/* Build an initializer for a __attr_type_info node.  */
static tree
build_attr_desc (tdecl, type)
     tree tdecl;
     tree type;
{
  tree elems, t, attrval;

  if (TYPE_READONLY (type))
    {
      if (TYPE_VOLATILE (type))
	attrval = lookup_field (__attr_desc_type_node, 
				get_identifier("_RTTI_ATTR_CONSTVOL"), 0, 0);
      else
	attrval = lookup_field (__attr_desc_type_node, 
				get_identifier("_RTTI_ATTR_CONST"), 0, 0);
    }
  else
    {
      if (TYPE_VOLATILE (type))
	attrval = lookup_field (__attr_desc_type_node, 
				get_identifier("_RTTI_ATTR_VOLATILE"), 0, 0);
    }
  t = build_t_desc (TYPE_MAIN_VARIANT (type), 1);
  t = build_indirect_ref (t , NULL);
  elems = tree_cons (NULL_TREE, attrval, tree_cons (NULL_TREE, t, NULL_TREE));
  return build_generic_desc (tdecl, __attr_desc_type_node,  elems);
}

/* Build an initializer for a __func_type_info node.  */
static tree
build_func_desc (tdecl)
     tree tdecl;
{
  tree elems, name_string;
  tree tname = DECL_NAME (tdecl);

  name_string = combine_strings (build_string 
    (IDENTIFIER_LENGTH (tname)+1, IDENTIFIER_POINTER (tname)));
  elems = name_string; 
  return build_generic_desc (tdecl, __func_desc_type_node,  elems);
}

/* Build an initializer for a __ptmf_type_info node.  */
static tree
build_ptmf_desc (tdecl)
     tree tdecl;
{ 
  tree elems, name_string;
  tree tname = DECL_NAME (tdecl);

  name_string = combine_strings (build_string 
    (IDENTIFIER_LENGTH (tname)+1, IDENTIFIER_POINTER (tname)));
  elems = name_string; 
  return build_generic_desc (tdecl, __ptmf_desc_type_node,  elems);
}

/* Build an initializer for a __ptmd_type_info node.  */
static tree
build_ptmd_desc (tdecl, type)
     tree tdecl;
     tree type;
{
  tree tc, t, elems;
  tc = build_t_desc (TYPE_OFFSET_BASETYPE (type), 1);
  tc = build_indirect_ref (tc , NULL);
  t = build_t_desc (TREE_TYPE (type), 1);
  t = build_indirect_ref (t , NULL);
  elems = tree_cons (NULL_TREE, tc,
	    tree_cons (NULL_TREE, t, NULL_TREE));
  return build_generic_desc (tdecl, __ptmd_desc_type_node,  elems);
}

struct uninst_st {
  tree type;
  struct uninst_st *next;
};
typedef struct uninst_st uninst_node;
static uninst_node * uninst_desc = (uninst_node *)NULL;

static void
add_uninstantiated_desc (type)
     tree type;
{
  uninst_node *t;

  t = (uninst_node *) xmalloc (sizeof (struct uninst_st));
  t->type = type;
  t->next = uninst_desc;
  uninst_desc = t;
}

/* We may choose to link the emitting of certain high use TDs for certain
   objects, we do that here.  Return the type to link against if such a
   link exists, otherwise just return TYPE.  */

static tree
get_def_to_follow (type)
     tree type;
{
#if 0
  /* For now we don't lay out T&, T* TDs with the main TD for the object.  */
  /* Let T* and T& be written only when T is written (if T is an aggr).
     We do this for const, but not for volatile, since volatile
     is rare and const is not.  */
  if (!TYPE_VOLATILE (taggr)
      && (TREE_CODE (taggr) == POINTER_TYPE
	  || TREE_CODE (taggr) == REFERENCE_TYPE)
      && IS_AGGR_TYPE (TREE_TYPE (taggr)))
    taggr = TREE_TYPE (taggr);
#endif
  return type;
}

/* build a general type_info node. */
tree
build_t_desc (type, definition)
     tree type;
     int definition;
{
  tree tdecl, tname;
  tree t, taggr;

  if (__ptmd_desc_type_node == NULL_TREE)
    {
      init_type_desc();
      if (__ptmd_desc_type_node)
	{
          for ( ; uninst_desc; uninst_desc = uninst_desc->next )
	    build_t_desc (uninst_desc->type, 1);
	}
    }
  if (__t_desc_type_node == NULL_TREE)
    {
      static int warned = 0;
      if (! warned)
	{
	  cp_error ("failed to build type descriptor node of '%T', maybe <typeinfo> not included", type);
	}
      warned = 1;
      return error_mark_node;
    }
  if (__ptmd_desc_type_node == NULL_TREE)
    {
      add_uninstantiated_desc (type);
      definition = 0;
    }

  push_obstacks (&permanent_obstack, &permanent_obstack);
  tname = build_t_desc_overload (type);

  if (!IDENTIFIER_AS_DESC (tname))
    {
      tdecl = build_decl (VAR_DECL, tname, __t_desc_type_node);
      DECL_EXTERNAL (tdecl) = 1;
      TREE_PUBLIC (tdecl) = 1;
      tdecl = pushdecl_top_level (tdecl);
      SET_IDENTIFIER_AS_DESC (tname, build_unary_op (ADDR_EXPR, tdecl, 0));
      if (!definition)
	cp_finish_decl (tdecl, NULL_TREE, NULL_TREE, 0, 0);
    }
  else
    tdecl = TREE_OPERAND (IDENTIFIER_AS_DESC (tname), 0);

  /* If it's not a definition, don't do anything more.  */
  if (!definition)
    return IDENTIFIER_AS_DESC (tname);

  /* If it has already been written, don't to anything more.  */
  /* Should this be on tdecl? */
  if (TREE_ASM_WRITTEN (IDENTIFIER_AS_DESC (tname)))
    return IDENTIFIER_AS_DESC (tname);

  /* If we previously defined it, return the defined result.  */
  if (DECL_INITIAL (tdecl))
    return IDENTIFIER_AS_DESC (tname);
    
  taggr = get_def_to_follow (type);

  /* If we know that we don't need to write out this type's
     vtable, then don't write out it's type_info.  Somebody
     else will take care of that.  */
  if (IS_AGGR_TYPE (taggr) && CLASSTYPE_VFIELD (taggr))
    {
      /* Let's play follow the vtable. */
      TREE_PUBLIC (tdecl) = CLASSTYPE_INTERFACE_KNOWN (taggr);
      DECL_EXTERNAL (tdecl) = CLASSTYPE_INTERFACE_ONLY (taggr);
    }
  else
    {
      DECL_EXTERNAL (tdecl) = 0;
      TREE_PUBLIC (tdecl) = (definition > 1);
    }

  if (DECL_EXTERNAL (tdecl))
    return IDENTIFIER_AS_DESC (tname);

  /* Show that we are defining the t_desc for this type.  */
  DECL_INITIAL (tdecl) = error_mark_node;
  t = DECL_CONTEXT (tdecl);
  if ( t && TREE_CODE_CLASS (TREE_CODE (t)) == 't') 
    pushclass (t, 2);

  if (TYPE_VOLATILE (type) || TYPE_READONLY (type))
    t = build_attr_desc (tdecl, type);
  else if (TREE_CODE (type) == ARRAY_TYPE)
    t = build_ptr_desc (tdecl, type);
  else if (TREE_CODE (type) == POINTER_TYPE)
    {
      if (TREE_CODE (TREE_TYPE (type)) == OFFSET_TYPE)
	{
	  type = TREE_TYPE (type);
	  t = build_ptmd_desc (tdecl, type);
	}
      else
	{
	  t = build_ptr_desc (tdecl, type);
	}
    }
  else if (TYPE_BUILT_IN (type))
    t = build_bltn_desc (tdecl, type);
  else if (IS_AGGR_TYPE (type))
    {
      if (TYPE_PTRMEMFUNC_P (type))
	t = build_ptmf_desc (tdecl);
      else
	t = build_class_desc (tdecl, type);
    }
  else if (TREE_CODE (type) == FUNCTION_TYPE)
    t = build_func_desc (tdecl);
  else 
    t = build_user_desc (tdecl);

  pop_obstacks ();
  return t;
}

#if 0
/* This is the old dossier type descriptor generation code, it's much
   more extended than rtti. It's reserved for later use. */
/* Build an initializer for a __t_desc node.  So that we can take advantage
   of recursion, we accept NULL for TYPE.
   DEFINITION is greater than zero iff we must define the type descriptor
   (as opposed to merely referencing it).  1 means treat according to
   #pragma interface/#pragma implementation rules.  2 means define as
   global and public, no matter what.  */
tree
build_t_desc (type, definition)
     tree type;
     int definition;
{
  tree tdecl;
  tree tname, name_string;
  tree elems, fields;
  tree parents, vbases, offsets, ivars, methods, target_type;
  int method_count = 0, field_count = 0;

  if (type == NULL_TREE)
    return NULL_TREE;

  tname = build_t_desc_overload (type);
  if (IDENTIFIER_AS_DESC (tname)
      && (!definition || TREE_ASM_WRITTEN (IDENTIFIER_AS_DESC (tname))))
    return IDENTIFIER_AS_DESC (tname);

  tdecl = lookup_name (tname, 0);
  if (tdecl == NULL_TREE)
    {
      tdecl = build_decl (VAR_DECL, tname, __t_desc_type_node);
      DECL_EXTERNAL (tdecl) = 1;
      TREE_PUBLIC (tdecl) = 1;
      tdecl = pushdecl_top_level (tdecl);
    }
  /* If we previously defined it, return the defined result.  */
  else if (definition && DECL_INITIAL (tdecl))
    return IDENTIFIER_AS_DESC (tname);

  if (definition)
    {
      tree taggr = type;
      /* Let T* and T& be written only when T is written (if T is an aggr).
         We do this for const, but not for volatile, since volatile
	 is rare and const is not.  */
      if (!TYPE_VOLATILE (taggr)
	  && (TREE_CODE (taggr) == POINTER_TYPE
	      || TREE_CODE (taggr) == REFERENCE_TYPE)
	  && IS_AGGR_TYPE (TREE_TYPE (taggr)))
	taggr = TREE_TYPE (taggr);

      /* If we know that we don't need to write out this type's
	 vtable, then don't write out it's dossier.  Somebody
	 else will take care of that.  */
      if (IS_AGGR_TYPE (taggr) && CLASSTYPE_VFIELD (taggr))
	{
	  if (CLASSTYPE_VTABLE_NEEDS_WRITING (taggr))
	    {
	      TREE_PUBLIC (tdecl) = ! CLASSTYPE_INTERFACE_ONLY (taggr)
		&& CLASSTYPE_INTERFACE_KNOWN (taggr);
	      DECL_EXTERNAL (tdecl) = 0;
	    }
	  else
	    {
	      if (write_virtuals != 0)
		TREE_PUBLIC (tdecl) = 1;
	    }
	}
      else
	{
	  DECL_EXTERNAL (tdecl) = 0;
	  TREE_PUBLIC (tdecl) = (definition > 1);
	}
    }
  SET_IDENTIFIER_AS_DESC (tname, build_unary_op (ADDR_EXPR, tdecl, 0));

  if (!definition || DECL_EXTERNAL (tdecl))
    {
      /* That's it!  */
      cp_finish_decl (tdecl, NULL_TREE, NULL_TREE, 0, 0);
      return IDENTIFIER_AS_DESC (tname);
    }

  /* Show that we are defining the t_desc for this type.  */
  DECL_INITIAL (tdecl) = error_mark_node;

  parents = build_tree_list (NULL_TREE, integer_zero_node);
  vbases = build_tree_list (NULL_TREE, integer_zero_node);
  offsets = build_tree_list (NULL_TREE, integer_zero_node);
  methods = NULL_TREE;
  ivars = NULL_TREE;

  if (TYPE_LANG_SPECIFIC (type))
    {
      int i = CLASSTYPE_N_BASECLASSES (type);
      tree method_vec = CLASSTYPE_METHOD_VEC (type);
      tree *meth, *end;
      tree binfos = TYPE_BINFO_BASETYPES (type);
      tree vb = CLASSTYPE_VBASECLASSES (type);

      while (--i >= 0)
	parents = tree_cons (NULL_TREE, build_t_desc (BINFO_TYPE (TREE_VEC_ELT (binfos, i)), 0), parents);

      while (vb)
	{
	  vbases = tree_cons (NULL_TREE, build_t_desc (BINFO_TYPE (vb), 0), vbases);
	  offsets = tree_cons (NULL_TREE, BINFO_OFFSET (vb), offsets);
	  vb = TREE_CHAIN (vb);
	}

      if (method_vec)
	for (meth = TREE_VEC_END (method_vec),
	     end = &TREE_VEC_ELT (method_vec, 0); meth-- != end; )
	  if (*meth)
	    {
	      methods = tree_cons (NULL_TREE, build_m_desc (*meth), methods);
	      method_count++;
	    }
    }

  if (IS_AGGR_TYPE (type))
    {
      for (fields = TYPE_FIELDS (type); fields; fields = TREE_CHAIN (fields))
	if (TREE_CODE (fields) == FIELD_DECL
	    || TREE_CODE (fields) == VAR_DECL)
	  {
	    ivars = tree_cons (NULL_TREE, build_i_desc (fields), ivars);
	    field_count++;
	  }
      ivars = nreverse (ivars);
    }

  parents = finish_table (NULL_TREE, build_pointer_type (__t_desc_type_node), parents, 0);
  vbases = finish_table (NULL_TREE, build_pointer_type (__t_desc_type_node), vbases, 0);
  offsets = finish_table (NULL_TREE, integer_type_node, offsets, 0);
  if (methods == NULL_TREE)
    methods = null_pointer_node;
  else
    methods = build_unary_op (ADDR_EXPR,
			      finish_table (NULL_TREE, __m_desc_type_node, methods, 0),
			      0);
  if (ivars == NULL_TREE)
    ivars = null_pointer_node;
  else
    ivars = build_unary_op (ADDR_EXPR,
			    finish_table (NULL_TREE, __i_desc_type_node, ivars, 0),
			    0);
  if (TREE_TYPE (type))
    target_type = build_t_desc (TREE_TYPE (type), definition);
  else
    target_type = integer_zero_node;

  name_string = combine_strings (build_string (IDENTIFIER_LENGTH (tname)+1, IDENTIFIER_POINTER (tname)));

  elems = tree_cons (NULL_TREE, build_unary_op (ADDR_EXPR, name_string, 0),
	   tree_cons (NULL_TREE,
		      TYPE_SIZE(type)? size_in_bytes(type) : integer_zero_node,
	     /* really should use bitfield initialization here.  */
	     tree_cons (NULL_TREE, integer_zero_node,
	      tree_cons (NULL_TREE, target_type,
	       tree_cons (NULL_TREE, build_int_2 (field_count, 2),
		tree_cons (NULL_TREE, build_int_2 (method_count, 2),
		 tree_cons (NULL_TREE, ivars,
		  tree_cons (NULL_TREE, methods,
		   tree_cons (NULL_TREE, build_unary_op (ADDR_EXPR, parents, 0),
		    tree_cons (NULL_TREE, build_unary_op (ADDR_EXPR, vbases, 0),
		     build_tree_list (NULL_TREE, build_unary_op (ADDR_EXPR, offsets, 0))))))))))));
  return build_generic_desc (tdecl, elems);
}

/* Build an initializer for a __i_desc node.  */
tree
build_i_desc (decl)
     tree decl;
{
  tree elems, name_string;
  tree taggr;

  name_string = DECL_NAME (decl);
  name_string = combine_strings (build_string (IDENTIFIER_LENGTH (name_string)+1, IDENTIFIER_POINTER (name_string)));

  /* Now decide whether this ivar should cause it's type to get
     def'd or ref'd in this file.  If the type we are looking at
     has a proxy definition, we look at the proxy (i.e., a
     `foo *' is equivalent to a `foo').  */
  taggr = TREE_TYPE (decl);

  if ((TREE_CODE (taggr) == POINTER_TYPE
       || TREE_CODE (taggr) == REFERENCE_TYPE)
      && TYPE_VOLATILE (taggr) == 0)
    taggr = TREE_TYPE (taggr);

  elems = tree_cons (NULL_TREE, build_unary_op (ADDR_EXPR, name_string, 0),
	     tree_cons (NULL_TREE, DECL_FIELD_BITPOS (decl),
		build_tree_list (NULL_TREE, build_t_desc (TREE_TYPE (decl),
							  ! IS_AGGR_TYPE (taggr)))));
  taggr = build (CONSTRUCTOR, __i_desc_type_node, NULL_TREE, elems);
  TREE_CONSTANT (taggr) = 1;
  TREE_STATIC (taggr) = 1;
  TREE_READONLY (taggr) = 1;
  return taggr;
}

/* Build an initializer for a __m_desc node.  */
tree
build_m_desc (decl)
     tree decl;
{
  tree taggr, elems, name_string;
  tree parm_count, req_count, vindex, vcontext;
  tree parms;
  int p_count, r_count;
  tree parm_types = NULL_TREE;

  for (parms = TYPE_ARG_TYPES (TREE_TYPE (decl)), p_count = 0, r_count = 0;
       parms != NULL_TREE; parms = TREE_CHAIN (parms), p_count++)
    {
      taggr = TREE_VALUE (parms);
      if ((TREE_CODE (taggr) == POINTER_TYPE
	   || TREE_CODE (taggr) == REFERENCE_TYPE)
	  && TYPE_VOLATILE (taggr) == 0)
	taggr = TREE_TYPE (taggr);

      parm_types = tree_cons (NULL_TREE, build_t_desc (TREE_VALUE (parms),
						       ! IS_AGGR_TYPE (taggr)),
			      parm_types);
      if (TREE_PURPOSE (parms) == NULL_TREE)
	r_count++;
    }

  parm_types = finish_table (NULL_TREE, build_pointer_type (__t_desc_type_node),
			     nreverse (parm_types), 0);
  parm_count = build_int_2 (p_count, 0);
  req_count = build_int_2 (r_count, 0);

  if (DECL_VINDEX (decl))
    vindex = DECL_VINDEX (decl);
  else
    vindex = integer_zero_node;
  if (DECL_CONTEXT (decl)
      && TREE_CODE_CLASS (TREE_CODE (DECL_CONTEXT (decl))) == 't')
    vcontext = build_t_desc (DECL_CONTEXT (decl), 0);
  else
    vcontext = integer_zero_node;
  name_string = DECL_NAME (decl);
  if (name_string == NULL)
      name_string = DECL_ASSEMBLER_NAME (decl);
  name_string = combine_strings (build_string (IDENTIFIER_LENGTH (name_string)+1, IDENTIFIER_POINTER (name_string)));

  /* Now decide whether the return type of this mvar
     should cause it's type to get def'd or ref'd in this file.
     If the type we are looking at has a proxy definition,
     we look at the proxy (i.e., a `foo *' is equivalent to a `foo').  */
  taggr = TREE_TYPE (TREE_TYPE (decl));

  if ((TREE_CODE (taggr) == POINTER_TYPE
       || TREE_CODE (taggr) == REFERENCE_TYPE)
      && TYPE_VOLATILE (taggr) == 0)
    taggr = TREE_TYPE (taggr);

  elems = tree_cons (NULL_TREE, build_unary_op (ADDR_EXPR, name_string, 0),
	     tree_cons (NULL_TREE, vindex,
		tree_cons (NULL_TREE, vcontext,
		   tree_cons (NULL_TREE, build_t_desc (TREE_TYPE (TREE_TYPE (decl)),
						       ! IS_AGGR_TYPE (taggr)),
		      tree_cons (NULL_TREE, build_c_cast (build_pointer_type (default_function_type), build_unary_op (ADDR_EXPR, decl, 0), 0),
			 tree_cons (NULL_TREE, parm_count,
			    tree_cons (NULL_TREE, req_count,
			       build_tree_list (NULL_TREE, build_unary_op (ADDR_EXPR, parm_types, 0)))))))));

  taggr = build (CONSTRUCTOR, __m_desc_type_node, NULL_TREE, elems);
  TREE_CONSTANT (taggr) = 1;
  TREE_STATIC (taggr) = 1;
  TREE_READONLY (taggr) = 1;
  return taggr;
}
#endif /* dossier */
