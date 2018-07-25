/* RunTime Type Identification
   Copyright (C) 1995-2018 Free Software Foundation, Inc.
   Mostly written by Jason Merrill (jason@cygnus.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "cp-tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "intl.h"
#include "stor-layout.h"
#include "c-family/c-pragma.h"
#include "gcc-rich-location.h"

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
   get_global_binding of the type's mangled name. That VAR_DECL
   will be the internal type.  It will usually have the correct
   internal type reflecting the kind of type it represents (pointer,
   array, function, class, inherited class, etc).  When the type it
   represents is incomplete, it will have the internal type
   corresponding to type_info.  That will only happen at the end of
   translation, when we are emitting the type info objects.  */

/* Auxiliary data we hold for each type_info derived object we need.  */
struct GTY (()) tinfo_s {
  tree type;  /* The RECORD_TYPE for this type_info object */

  tree vtable; /* The VAR_DECL of the vtable.  Only filled at end of
		  translation.  */

  tree name;  /* IDENTIFIER_NODE for the ABI specified name of
		 the type_info derived type.  */
};


enum tinfo_kind
{
  TK_TYPE_INFO_TYPE,    /* abi::__type_info_pseudo */
  TK_BASE_TYPE,		/* abi::__base_class_type_info */
  TK_DERIVED_TYPES,	/* Start of types derived from abi::__type_info  */
  TK_BUILTIN_TYPE = TK_DERIVED_TYPES,	/* abi::__fundamental_type_info */
  TK_ARRAY_TYPE,	/* abi::__array_type_info */
  TK_FUNCTION_TYPE,	/* abi::__function_type_info */
  TK_ENUMERAL_TYPE,	/* abi::__enum_type_info */
  TK_POINTER_TYPE,	/* abi::__pointer_type_info */
  TK_POINTER_MEMBER_TYPE, /* abi::__pointer_to_member_type_info */
  TK_CLASS_TYPE,	/* abi::__class_type_info */
  TK_SI_CLASS_TYPE,	/* abi::__si_class_type_info */
  TK_VMI_CLASS_TYPES,	/* abi::__vmi_class_type_info<int> */
  TK_MAX
};

/* Names of the tinfo types.  Must be same order as TK enumeration
   above.  */

static const char *const tinfo_names[TK_MAX] =
{
  "__type_info",
  "__base_class_type_info",
  "__fundamental_type_info",
  "__array_type_info",
  "__function_type_info",
  "__enum_type_info",
  "__pointer_type_info",
  "__pointer_to_member_type_info",
  "__class_type_info",
  "__si_class_type_info",
  "__vmi_class_type_info"
};

/* Helper macro to get maximum scalar-width of pointer or of the 'long'-type.
   This of interest for llp64 targets.  */
#define LONGPTR_T \
  integer_types[(POINTER_SIZE <= TYPE_PRECISION (integer_types[itk_long]) \
		 ? itk_long : itk_long_long)]

/* A vector of all tinfo decls that haven't yet been emitted.  */
vec<tree, va_gc> *unemitted_tinfo_decls;

/* A vector of all type_info derived types we need.  The first few are
   fixed and created early. The remainder are for multiple inheritance
   and are generated as needed. */
static GTY (()) vec<tinfo_s, va_gc> *tinfo_descs;

static tree ifnonnull (tree, tree, tsubst_flags_t);
static tree tinfo_name (tree, bool);
static tree build_dynamic_cast_1 (tree, tree, tsubst_flags_t);
static tree throw_bad_cast (void);
static tree throw_bad_typeid (void);
static tree get_tinfo_ptr (tree);
static bool typeid_ok_p (void);
static int qualifier_flags (tree);
static bool target_incomplete_p (tree);
static tree tinfo_base_init (tinfo_s *, tree);
static tree generic_initializer (tinfo_s *, tree);
static tree ptr_initializer (tinfo_s *, tree);
static tree ptm_initializer (tinfo_s *, tree);
static tree class_initializer (tinfo_s *, tree, unsigned, ...);
static tree get_pseudo_ti_init (tree, unsigned);
static unsigned get_pseudo_ti_index (tree);
static tinfo_s *get_tinfo_desc (unsigned);
static void create_tinfo_types (void);
static bool typeinfo_in_lib_p (tree);

static int doing_runtime = 0;

static void
push_abi_namespace (void)
{
  push_nested_namespace (abi_node);
  push_visibility ("default", 2);
}

static void
pop_abi_namespace (void)
{
  pop_visibility (2);
  pop_nested_namespace (abi_node);
}

/* Declare language defined type_info type and a pointer to const
   type_info.  This is incomplete here, and will be completed when
   the user #includes <typeinfo>.  There are language defined
   restrictions on what can be done until that is included.  Create
   the internal versions of the ABI types.  */

void
init_rtti_processing (void)
{
  tree type_info_type;

  push_namespace (std_identifier);
  type_info_type = xref_tag (class_type, get_identifier ("type_info"),
			     /*tag_scope=*/ts_current, false);
  pop_namespace ();
  const_type_info_type_node
    = cp_build_qualified_type (type_info_type, TYPE_QUAL_CONST);
  type_info_ptr_type = build_pointer_type (const_type_info_type_node);

  vec_alloc (unemitted_tinfo_decls, 124);

  create_tinfo_types ();
}

/* Given the expression EXP of type `class *', return the head of the
   object pointed to by EXP with type cv void*, if the class has any
   virtual functions (TYPE_POLYMORPHIC_P), else just return the
   expression.  */

tree
build_headof (tree exp)
{
  tree type = TREE_TYPE (exp);
  tree offset;
  tree index;

  gcc_assert (TYPE_PTR_P (type));
  type = TREE_TYPE (type);

  if (!TYPE_POLYMORPHIC_P (type))
    return exp;

  /* We use this a couple of times below, protect it.  */
  exp = save_expr (exp);

  /* The offset-to-top field is at index -2 from the vptr.  */
  index = build_int_cst (NULL_TREE,
			 -2 * TARGET_VTABLE_DATA_ENTRY_DISTANCE);

  offset = build_vtbl_ref (cp_build_fold_indirect_ref (exp),
                           index);

  type = cp_build_qualified_type (ptr_type_node,
				  cp_type_quals (TREE_TYPE (exp)));
  return fold_build_pointer_plus (exp, offset);
}

/* Get a bad_cast node for the program to throw...

   See libstdc++/exception.cc for __throw_bad_cast */

static tree
throw_bad_cast (void)
{
  static tree fn;
  if (!fn)
    {
      tree name = get_identifier ("__cxa_bad_cast");
      fn = get_global_binding (name);
      if (!fn)
	fn = push_throw_library_fn
	  (name, build_function_type_list (ptr_type_node, NULL_TREE));
    }

  return build_cxx_call (fn, 0, NULL, tf_warning_or_error);
}

/* Return an expression for "__cxa_bad_typeid()".  The expression
   returned is an lvalue of type "const std::type_info".  */

static tree
throw_bad_typeid (void)
{
  static tree fn;
  if (!fn)
    {
      tree name = get_identifier ("__cxa_bad_typeid");
      fn = get_global_binding (name);
      if (!fn)
	{
	  tree t = build_reference_type (const_type_info_type_node);
	  t = build_function_type_list (t, NULL_TREE);
	  fn = push_throw_library_fn (name, t);
	}
    }

  return build_cxx_call (fn, 0, NULL, tf_warning_or_error);
}

/* Return an lvalue expression whose type is "const std::type_info"
   and whose value indicates the type of the expression EXP.  If EXP
   is a reference to a polymorphic class, return the dynamic type;
   otherwise return the static type of the expression.  */

static tree
get_tinfo_decl_dynamic (tree exp, tsubst_flags_t complain)
{
  tree type;
  tree t;

  if (error_operand_p (exp))
    return error_mark_node;

  exp = resolve_nondeduced_context (exp, complain);

  /* peel back references, so they match.  */
  type = non_reference (TREE_TYPE (exp));

  /* Peel off cv qualifiers.  */
  type = TYPE_MAIN_VARIANT (type);

  /* For UNKNOWN_TYPEs call complete_type_or_else to get diagnostics.  */
  if (CLASS_TYPE_P (type) || type == unknown_type_node
      || type == init_list_type_node)
    type = complete_type_or_maybe_complain (type, exp, complain);

  if (!type)
    return error_mark_node;

  /* If exp is a reference to polymorphic type, get the real type_info.  */
  if (TYPE_POLYMORPHIC_P (type) && ! resolves_to_fixed_type_p (exp, 0))
    {
      /* build reference to type_info from vtable.  */
      tree index;

      /* The RTTI information is at index -1.  */
      index = build_int_cst (NULL_TREE,
			     -1 * TARGET_VTABLE_DATA_ENTRY_DISTANCE);
      t = build_vtbl_ref (exp, index);
      t = convert (type_info_ptr_type, t);
    }
  else
    /* Otherwise return the type_info for the static type of the expr.  */
    t = get_tinfo_ptr (TYPE_MAIN_VARIANT (type));

  return cp_build_fold_indirect_ref (t);
}

static bool
typeid_ok_p (void)
{
  if (! flag_rtti)
    {
      error ("cannot use %<typeid%> with -fno-rtti");
      return false;
    }

  if (!COMPLETE_TYPE_P (const_type_info_type_node))
    {
      gcc_rich_location richloc (input_location);
      maybe_add_include_fixit (&richloc, "<typeinfo>");
      error_at (&richloc,
		"must %<#include <typeinfo>%> before using"
		" %<typeid%>");

      return false;
    }

  tree pseudo = TYPE_MAIN_VARIANT (get_tinfo_desc (TK_TYPE_INFO_TYPE)->type);
  tree real = TYPE_MAIN_VARIANT (const_type_info_type_node);

  /* Make sure abi::__type_info_pseudo has the same alias set
     as std::type_info.  */
  if (! TYPE_ALIAS_SET_KNOWN_P (pseudo))
    TYPE_ALIAS_SET (pseudo) = get_alias_set (real);
  else
    gcc_assert (TYPE_ALIAS_SET (pseudo) == get_alias_set (real));

  return true;
}

/* Return an expression for "typeid(EXP)".  The expression returned is
   an lvalue of type "const std::type_info".  */

tree
build_typeid (tree exp, tsubst_flags_t complain)
{
  tree cond = NULL_TREE, initial_expr = exp;
  int nonnull = 0;

  if (exp == error_mark_node || !typeid_ok_p ())
    return error_mark_node;

  if (processing_template_decl)
    return build_min (TYPEID_EXPR, const_type_info_type_node, exp);

  /* FIXME when integrating with c_fully_fold, mark
     resolves_to_fixed_type_p case as a non-constant expression.  */
  if (TYPE_POLYMORPHIC_P (TREE_TYPE (exp))
      && ! resolves_to_fixed_type_p (exp, &nonnull)
      && ! nonnull)
    {
      /* So we need to look into the vtable of the type of exp.
         Make sure it isn't a null lvalue.  */
      exp = cp_build_addr_expr (exp, complain);
      exp = save_expr (exp);
      cond = cp_convert (boolean_type_node, exp, complain);
      exp = cp_build_fold_indirect_ref (exp);
    }

  exp = get_tinfo_decl_dynamic (exp, complain);

  if (exp == error_mark_node)
    return error_mark_node;

  if (cond)
    {
      tree bad = throw_bad_typeid ();

      exp = build3 (COND_EXPR, TREE_TYPE (exp), cond, exp, bad);
    }
  else
    mark_type_use (initial_expr);

  return exp;
}

/* Generate the NTBS name of a type.  If MARK_PRIVATE, put a '*' in front so that
   comparisons will be done by pointer rather than string comparison.  */
static tree
tinfo_name (tree type, bool mark_private)
{
  const char *name;
  int length;
  tree name_string;

  name = mangle_type_string (type);
  length = strlen (name);

  if (mark_private)
    {
      /* Inject '*' at beginning of name to force pointer comparison.  */
      char* buf = (char*) XALLOCAVEC (char, length + 2);
      buf[0] = '*';
      memcpy (buf + 1, name, length + 1);
      name_string = build_string (length + 2, buf);
    }
  else
    name_string = build_string (length + 1, name);

  return fix_string_type (name_string);
}

/* Return a VAR_DECL for the internal ABI defined type_info object for
   TYPE. You must arrange that the decl is mark_used, if actually use
   it --- decls in vtables are only used if the vtable is output.  */

tree
get_tinfo_decl (tree type)
{
  tree name;
  tree d;

  if (variably_modified_type_p (type, /*fn=*/NULL_TREE))
    {
      error ("cannot create type information for type %qT because "
	     "it involves types of variable size",
	     type);
      return error_mark_node;
    }

  if (TREE_CODE (type) == METHOD_TYPE)
    type = build_function_type (TREE_TYPE (type),
				TREE_CHAIN (TYPE_ARG_TYPES (type)));

  type = complete_type (type);

  /* For a class type, the variable is cached in the type node
     itself.  */
  if (CLASS_TYPE_P (type))
    {
      d = CLASSTYPE_TYPEINFO_VAR (TYPE_MAIN_VARIANT (type));
      if (d)
	return d;
    }

  name = mangle_typeinfo_for_type (type);

  d = get_global_binding (name);
  if (!d)
    {
      int ix = get_pseudo_ti_index (type);
      const tinfo_s *ti = get_tinfo_desc (ix);
      
      d = build_lang_decl (VAR_DECL, name, ti->type);
      SET_DECL_ASSEMBLER_NAME (d, name);
      /* Remember the type it is for.  */
      TREE_TYPE (name) = type;
      DECL_TINFO_P (d) = 1;
      DECL_ARTIFICIAL (d) = 1;
      DECL_IGNORED_P (d) = 1;
      TREE_READONLY (d) = 1;
      TREE_STATIC (d) = 1;
      /* Mark the variable as undefined -- but remember that we can
	 define it later if we need to do so.  */
      DECL_EXTERNAL (d) = 1;
      DECL_NOT_REALLY_EXTERN (d) = 1;
      set_linkage_according_to_type (type, d);

      d = pushdecl_top_level_and_finish (d, NULL_TREE);
      if (CLASS_TYPE_P (type))
	CLASSTYPE_TYPEINFO_VAR (TYPE_MAIN_VARIANT (type)) = d;

      /* Add decl to the global array of tinfo decls.  */
      vec_safe_push (unemitted_tinfo_decls, d);
    }

  return d;
}

/* Return a pointer to a type_info object describing TYPE, suitably
   cast to the language defined type.  */

static tree
get_tinfo_ptr (tree type)
{
  tree decl = get_tinfo_decl (type);

  mark_used (decl);
  return build_nop (type_info_ptr_type,
		    build_address (decl));
}

/* Return the type_info object for TYPE.  */

tree
get_typeid (tree type, tsubst_flags_t complain)
{
  if (type == error_mark_node || !typeid_ok_p ())
    return error_mark_node;

  if (processing_template_decl)
    return build_min (TYPEID_EXPR, const_type_info_type_node, type);

  /* If the type of the type-id is a reference type, the result of the
     typeid expression refers to a type_info object representing the
     referenced type.  */
  type = non_reference (type);

  /* This is not one of the uses of a qualified function type in 8.3.5.  */
  if (TREE_CODE (type) == FUNCTION_TYPE
      && (type_memfn_quals (type) != TYPE_UNQUALIFIED
	  || type_memfn_rqual (type) != REF_QUAL_NONE))
    {
      if (complain & tf_error)
	error ("typeid of qualified function type %qT", type);
      return error_mark_node;
    }

  /* The top-level cv-qualifiers of the lvalue expression or the type-id
     that is the operand of typeid are always ignored.  */
  type = TYPE_MAIN_VARIANT (type);

  /* For UNKNOWN_TYPEs call complete_type_or_else to get diagnostics.  */
  if (CLASS_TYPE_P (type) || type == unknown_type_node
      || type == init_list_type_node)
    type = complete_type_or_maybe_complain (type, NULL_TREE, complain);

  if (!type)
    return error_mark_node;

  return cp_build_fold_indirect_ref (get_tinfo_ptr (type));
}

/* Check whether TEST is null before returning RESULT.  If TEST is used in
   RESULT, it must have previously had a save_expr applied to it.  */

static tree
ifnonnull (tree test, tree result, tsubst_flags_t complain)
{
  tree cond = build2 (NE_EXPR, boolean_type_node, test,
		      cp_convert (TREE_TYPE (test), nullptr_node, complain));
  /* This is a compiler generated comparison, don't emit
     e.g. -Wnonnull-compare warning for it.  */
  TREE_NO_WARNING (cond) = 1;
  return build3 (COND_EXPR, TREE_TYPE (result), cond, result,
		 cp_convert (TREE_TYPE (result), nullptr_node, complain));
}

/* Execute a dynamic cast, as described in section 5.2.6 of the 9/93 working
   paper.  */

static tree
build_dynamic_cast_1 (tree type, tree expr, tsubst_flags_t complain)
{
  enum tree_code tc = TREE_CODE (type);
  tree exprtype;
  tree dcast_fn;
  tree old_expr = expr;
  const char *errstr = NULL;

  /* Save casted types in the function's used types hash table.  */
  used_types_insert (type);

  /* T shall be a pointer or reference to a complete class type, or
     `pointer to cv void''.  */
  switch (tc)
    {
    case POINTER_TYPE:
      if (VOID_TYPE_P (TREE_TYPE (type)))
	break;
      /* Fall through.  */
    case REFERENCE_TYPE:
      if (! MAYBE_CLASS_TYPE_P (TREE_TYPE (type)))
	{
	  errstr = _("target is not pointer or reference to class");
	  goto fail;
	}
      if (!COMPLETE_TYPE_P (complete_type (TREE_TYPE (type))))
	{
	  errstr = _("target is not pointer or reference to complete type");
	  goto fail;
	}
      break;

    default:
      errstr = _("target is not pointer or reference");
      goto fail;
    }

  if (tc == POINTER_TYPE)
    {
      expr = decay_conversion (expr, complain);
      exprtype = TREE_TYPE (expr);

      /* If T is a pointer type, v shall be an rvalue of a pointer to
	 complete class type, and the result is an rvalue of type T.  */

      expr = mark_rvalue_use (expr);

      if (!TYPE_PTR_P (exprtype))
	{
	  errstr = _("source is not a pointer");
	  goto fail;
	}
      if (! MAYBE_CLASS_TYPE_P (TREE_TYPE (exprtype)))
	{
	  errstr = _("source is not a pointer to class");
	  goto fail;
	}
      if (!COMPLETE_TYPE_P (complete_type (TREE_TYPE (exprtype))))
	{
	  errstr = _("source is a pointer to incomplete type");
	  goto fail;
	}
    }
  else
    {
      expr = mark_lvalue_use (expr);
      exprtype = TREE_TYPE (expr);

      /* T is a reference type, v shall be an lvalue of a complete class
	 type, and the result is an lvalue of the type referred to by T.  */
      if (! MAYBE_CLASS_TYPE_P (exprtype))
	{
	  errstr = _("source is not of class type");
	  goto fail;
	}
      if (!COMPLETE_TYPE_P (complete_type (exprtype)))
	{
	  errstr = _("source is of incomplete class type");
	  goto fail;
	}

      exprtype = cp_build_reference_type (exprtype, !lvalue_p (expr));
    }

  /* The dynamic_cast operator shall not cast away constness.  */
  if (!at_least_as_qualified_p (TREE_TYPE (type),
				TREE_TYPE (exprtype)))
    {
      errstr = _("conversion casts away constness");
      goto fail;
    }

  /* If *type is an unambiguous accessible base class of *exprtype,
     convert statically.  */
  {
    tree binfo = lookup_base (TREE_TYPE (exprtype), TREE_TYPE (type),
			      ba_check, NULL, complain);
    if (binfo)
      return build_static_cast (type, expr, complain);
  }

  /* Apply trivial conversion T -> T& for dereferenced ptrs.  */
  if (tc == REFERENCE_TYPE)
    expr = convert_to_reference (exprtype, expr, CONV_IMPLICIT,
				 LOOKUP_NORMAL, NULL_TREE, complain);

  /* Otherwise *exprtype must be a polymorphic class (have a vtbl).  */
  if (TYPE_POLYMORPHIC_P (TREE_TYPE (exprtype)))
    {
      tree expr1;
      /* if TYPE is `void *', return pointer to complete object.  */
      if (tc == POINTER_TYPE && VOID_TYPE_P (TREE_TYPE (type)))
	{
	  /* if b is an object, dynamic_cast<void *>(&b) == (void *)&b.  */
	  if (TREE_CODE (expr) == ADDR_EXPR
	      && VAR_P (TREE_OPERAND (expr, 0))
	      && TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == RECORD_TYPE)
	    return build1 (NOP_EXPR, type, expr);

	  /* Since expr is used twice below, save it.  */
	  expr = save_expr (expr);

	  expr1 = build_headof (expr);
	  if (TREE_TYPE (expr1) != type)
	    expr1 = build1 (NOP_EXPR, type, expr1);
	  return ifnonnull (expr, expr1, complain);
	}
      else
	{
	  tree retval;
	  tree result, td2, td3;
	  tree elems[4];
	  tree static_type, target_type, boff;

	  /* If we got here, we can't convert statically.  Therefore,
	     dynamic_cast<D&>(b) (b an object) cannot succeed.  */
	  if (tc == REFERENCE_TYPE)
	    {
	      if (VAR_P (old_expr)
		  && TREE_CODE (TREE_TYPE (old_expr)) == RECORD_TYPE)
		{
		  tree expr = throw_bad_cast ();
                  if (complain & tf_warning)
                    warning (0, "dynamic_cast of %q#D to %q#T can never succeed",
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
	      if (VAR_P (op)
		  && TREE_CODE (TREE_TYPE (op)) == RECORD_TYPE)
		{
                  if (complain & tf_warning)
                    warning (0, "dynamic_cast of %q#D to %q#T can never succeed",
                             op, type);
		  retval = build_int_cst (type, 0);
		  return retval;
		}
	    }

	  /* Use of dynamic_cast when -fno-rtti is prohibited.  */
	  if (!flag_rtti)
	    {
              if (complain & tf_error)
                error ("%<dynamic_cast%> not permitted with -fno-rtti");
	      return error_mark_node;
	    }

	  target_type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
	  static_type = TYPE_MAIN_VARIANT (TREE_TYPE (exprtype));
	  td2 = get_tinfo_decl (target_type);
	  if (!mark_used (td2, complain) && !(complain & tf_error))
	    return error_mark_node;
	  td2 = cp_build_addr_expr (td2, complain);
	  td3 = get_tinfo_decl (static_type);
	  if (!mark_used (td3, complain) && !(complain & tf_error))
	    return error_mark_node;
	  td3 = cp_build_addr_expr (td3, complain);

	  /* Determine how T and V are related.  */
	  boff = dcast_base_hint (static_type, target_type);

	  /* Since expr is used twice below, save it.  */
	  expr = save_expr (expr);

	  expr1 = expr;
	  if (tc == REFERENCE_TYPE)
	    expr1 = cp_build_addr_expr (expr1, complain);

	  elems[0] = expr1;
	  elems[1] = td3;
	  elems[2] = td2;
	  elems[3] = boff;

	  dcast_fn = dynamic_cast_node;
	  if (!dcast_fn)
	    {
	      tree tmp;
	      tree tinfo_ptr;
	      const char *name;

	      push_abi_namespace ();
	      tinfo_ptr = xref_tag (class_type,
				    get_identifier ("__class_type_info"),
				    /*tag_scope=*/ts_current, false);

	      tinfo_ptr = build_pointer_type
		(cp_build_qualified_type
		 (tinfo_ptr, TYPE_QUAL_CONST));
	      name = "__dynamic_cast";
	      tmp = build_function_type_list (ptr_type_node,
					      const_ptr_type_node,
					      tinfo_ptr, tinfo_ptr,
					      ptrdiff_type_node, NULL_TREE);
	      dcast_fn = build_library_fn_ptr (name, tmp,
					       ECF_LEAF | ECF_PURE | ECF_NOTHROW);
	      pop_abi_namespace ();
	      dynamic_cast_node = dcast_fn;
	    }
	  result = build_cxx_call (dcast_fn, 4, elems, complain);

	  if (tc == REFERENCE_TYPE)
	    {
	      tree bad = throw_bad_cast ();
	      tree neq;

	      result = save_expr (result);
	      neq = cp_truthvalue_conversion (result);
	      return cp_convert (type,
				 build3 (COND_EXPR, TREE_TYPE (result),
					 neq, result, bad), complain);
	    }

	  /* Now back to the type we want from a void*.  */
	  result = cp_convert (type, result, complain);
	  return ifnonnull (expr, result, complain);
	}
    }
  else
    errstr = _("source type is not polymorphic");

 fail:
  if (complain & tf_error)
    error ("cannot dynamic_cast %qE (of type %q#T) to type %q#T (%s)",
           old_expr, TREE_TYPE (old_expr), type, errstr);
  return error_mark_node;
}

tree
build_dynamic_cast (tree type, tree expr, tsubst_flags_t complain)
{
  tree r;

  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      expr = build_min (DYNAMIC_CAST_EXPR, type, expr);
      TREE_SIDE_EFFECTS (expr) = 1;
      return convert_from_reference (expr);
    }

  r = convert_from_reference (build_dynamic_cast_1 (type, expr, complain));
  if (r != error_mark_node)
    maybe_warn_about_useless_cast (type, expr, complain);
  return r;
}

/* Return the runtime bit mask encoding the qualifiers of TYPE.  */

static int
qualifier_flags (tree type)
{
  int flags = 0;
  int quals = cp_type_quals (type);

  if (quals & TYPE_QUAL_CONST)
    flags |= 1;
  if (quals & TYPE_QUAL_VOLATILE)
    flags |= 2;
  if (quals & TYPE_QUAL_RESTRICT)
    flags |= 4;
  return flags;
}

/* Return true, if the pointer chain TYPE ends at an incomplete type, or
   contains a pointer to member of an incomplete class.  */

static bool
target_incomplete_p (tree type)
{
  while (true)
    if (TYPE_PTRDATAMEM_P (type))
      {
	if (!COMPLETE_TYPE_P (TYPE_PTRMEM_CLASS_TYPE (type)))
	  return true;
	type = TYPE_PTRMEM_POINTED_TO_TYPE (type);
      }
    else if (TYPE_PTR_P (type))
      type = TREE_TYPE (type);
    else
      return !COMPLETE_OR_VOID_TYPE_P (type);
}

/* Returns true if TYPE involves an incomplete class type; in that
   case, typeinfo variables for TYPE should be emitted with internal
   linkage.  */

static bool
involves_incomplete_p (tree type)
{
  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
      return target_incomplete_p (TREE_TYPE (type));

    case OFFSET_TYPE:
    ptrmem:
      return
	(target_incomplete_p (TYPE_PTRMEM_POINTED_TO_TYPE (type))
	 || !COMPLETE_TYPE_P (TYPE_PTRMEM_CLASS_TYPE (type)));

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (type))
	goto ptrmem;
      /* Fall through.  */
    case UNION_TYPE:
      if (!COMPLETE_TYPE_P (type))
	return true;
      /* Fall through.  */
    default:
      /* All other types do not involve incomplete class types.  */
      return false;
    }
}

/* Return a CONSTRUCTOR for the common part of the type_info objects. This
   is the vtable pointer and NTBS name.  The NTBS name is emitted as a
   comdat const char array, so it becomes a unique key for the type. Generate
   and emit that VAR_DECL here.  (We can't always emit the type_info itself
   as comdat, because of pointers to incomplete.) */

static tree
tinfo_base_init (tinfo_s *ti, tree target)
{
  tree init;
  tree name_decl;
  tree vtable_ptr;
  vec<constructor_elt, va_gc> *v;

  {
    tree name_name, name_string;

    /* Generate the NTBS array variable.  */
    tree name_type = build_cplus_array_type
		     (cp_build_qualified_type (char_type_node, TYPE_QUAL_CONST),
		     NULL_TREE);

    /* Determine the name of the variable -- and remember with which
       type it is associated.  */
    name_name = mangle_typeinfo_string_for_type (target);
    TREE_TYPE (name_name) = target;

    name_decl = build_lang_decl (VAR_DECL, name_name, name_type);
    SET_DECL_ASSEMBLER_NAME (name_decl, name_name);
    DECL_ARTIFICIAL (name_decl) = 1;
    DECL_IGNORED_P (name_decl) = 1;
    TREE_READONLY (name_decl) = 1;
    TREE_STATIC (name_decl) = 1;
    DECL_EXTERNAL (name_decl) = 0;
    DECL_TINFO_P (name_decl) = 1;
    set_linkage_according_to_type (target, name_decl);
    import_export_decl (name_decl);
    name_string = tinfo_name (target, !TREE_PUBLIC (name_decl));
    DECL_INITIAL (name_decl) = name_string;
    mark_used (name_decl);
    pushdecl_top_level_and_finish (name_decl, name_string);
  }

  vtable_ptr = ti->vtable;
  if (!vtable_ptr)
    {
      tree real_type;
      push_abi_namespace ();
      real_type = xref_tag (class_type, ti->name,
			    /*tag_scope=*/ts_current, false);
      pop_abi_namespace ();

      if (!COMPLETE_TYPE_P (real_type))
	{
	  /* We never saw a definition of this type, so we need to
	     tell the compiler that this is an exported class, as
	     indeed all of the __*_type_info classes are.  */
	  SET_CLASSTYPE_INTERFACE_KNOWN (real_type);
	  CLASSTYPE_INTERFACE_ONLY (real_type) = 1;
	}

      vtable_ptr = get_vtable_decl (real_type, /*complete=*/1);
      vtable_ptr = cp_build_addr_expr (vtable_ptr, tf_warning_or_error);

      /* We need to point into the middle of the vtable.  */
      vtable_ptr = fold_build_pointer_plus
	(vtable_ptr,
	 size_binop (MULT_EXPR,
		     size_int (2 * TARGET_VTABLE_DATA_ENTRY_DISTANCE),
		     TYPE_SIZE_UNIT (vtable_entry_type)));

      ti->vtable = vtable_ptr;
    }

  vec_alloc (v, 2);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, vtable_ptr);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  decay_conversion (name_decl, tf_warning_or_error));

  init = build_constructor (init_list_type_node, v);
  TREE_CONSTANT (init) = 1;
  TREE_STATIC (init) = 1;

  return init;
}

/* Return the CONSTRUCTOR expr for a type_info of TYPE. TI provides the
   information about the particular type_info derivation, which adds no
   additional fields to the type_info base.  */

static tree
generic_initializer (tinfo_s *ti, tree target)
{
  tree init = tinfo_base_init (ti, target);

  init = build_constructor_single (init_list_type_node, NULL_TREE, init);
  TREE_CONSTANT (init) = 1;
  TREE_STATIC (init) = 1;
  return init;
}

/* Return the CONSTRUCTOR expr for a type_info of pointer TYPE.
   TI provides information about the particular type_info derivation,
   which adds target type and qualifier flags members to the type_info base.  */

static tree
ptr_initializer (tinfo_s *ti, tree target)
{
  tree init = tinfo_base_init (ti, target);
  tree to = TREE_TYPE (target);
  int flags = qualifier_flags (to);
  bool incomplete = target_incomplete_p (to);
  vec<constructor_elt, va_gc> *v;
  vec_alloc (v, 3);

  if (incomplete)
    flags |= 8;
  if (tx_safe_fn_type_p (to))
    {
      flags |= 0x20;
      to = tx_unsafe_fn_variant (to);
    }
  if (flag_noexcept_type
      && (TREE_CODE (to) == FUNCTION_TYPE
	  || TREE_CODE (to) == METHOD_TYPE)
      && TYPE_NOTHROW_P (to))
    {
      flags |= 0x40;
      to = build_exception_variant (to, NULL_TREE);
    }
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, init);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, flags));
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                          get_tinfo_ptr (TYPE_MAIN_VARIANT (to)));

  init = build_constructor (init_list_type_node, v);
  TREE_CONSTANT (init) = 1;
  TREE_STATIC (init) = 1;
  return init;
}

/* Return the CONSTRUCTOR expr for a type_info of pointer to member data TYPE.
   TI provides information about the particular type_info derivation,
   which adds class, target type and qualifier flags members to the type_info
   base.  */

static tree
ptm_initializer (tinfo_s *ti, tree target)
{
  tree init = tinfo_base_init (ti, target);
  tree to = TYPE_PTRMEM_POINTED_TO_TYPE (target);
  tree klass = TYPE_PTRMEM_CLASS_TYPE (target);
  int flags = qualifier_flags (to);
  bool incomplete = target_incomplete_p (to);
  vec<constructor_elt, va_gc> *v;
  vec_alloc (v, 4);

  if (incomplete)
    flags |= 0x8;
  if (!COMPLETE_TYPE_P (klass))
    flags |= 0x10;
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, init);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, flags));
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                          get_tinfo_ptr (TYPE_MAIN_VARIANT (to)));
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, get_tinfo_ptr (klass));

  init = build_constructor (init_list_type_node, v);
  TREE_CONSTANT (init) = 1;
  TREE_STATIC (init) = 1;
  return init;
}

/* Return the CONSTRUCTOR expr for a type_info of class TYPE.
   TI provides information about the particular __class_type_info derivation,
   which adds hint flags and N extra initializers to the type_info base.  */

static tree
class_initializer (tinfo_s *ti, tree target, unsigned n, ...)
{
  tree init = tinfo_base_init (ti, target);
  va_list extra_inits;
  unsigned i;
  vec<constructor_elt, va_gc> *v;
  vec_alloc (v, n+1);

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, init);
  va_start (extra_inits, n);
  for (i = 0; i < n; i++)
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, va_arg (extra_inits, tree));
  va_end (extra_inits);

  init = build_constructor (init_list_type_node, v);
  TREE_CONSTANT (init) = 1;
  TREE_STATIC (init) = 1;
  return init;
}

/* Returns true if the typeinfo for type should be placed in
   the runtime library.  */

static bool
typeinfo_in_lib_p (tree type)
{
  /* The typeinfo objects for `T*' and `const T*' are in the runtime
     library for simple types T.  */
  if (TYPE_PTR_P (type)
      && (cp_type_quals (TREE_TYPE (type)) == TYPE_QUAL_CONST
	  || cp_type_quals (TREE_TYPE (type)) == TYPE_UNQUALIFIED))
    type = TREE_TYPE (type);

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case REAL_TYPE:
    case VOID_TYPE:
    case NULLPTR_TYPE:
      return true;

    case LANG_TYPE:
      /* fall through.  */

    default:
      return false;
    }
}

/* Generate the initializer for the type info describing TYPE.  TK_INDEX is
   the index of the descriptor in the tinfo_desc vector. */

static tree
get_pseudo_ti_init (tree type, unsigned tk_index)
{
  tinfo_s *ti = get_tinfo_desc (tk_index);

  gcc_assert (at_eof);
  switch (tk_index)
    {
    case TK_POINTER_MEMBER_TYPE:
      return ptm_initializer (ti, type);

    case TK_POINTER_TYPE:
      return ptr_initializer (ti, type);

    case TK_BUILTIN_TYPE:
    case TK_ENUMERAL_TYPE:
    case TK_FUNCTION_TYPE:
    case TK_ARRAY_TYPE:
      return generic_initializer (ti, type);

    case TK_CLASS_TYPE:
      return class_initializer (ti, type, 0);

    case TK_SI_CLASS_TYPE:
      {
	tree base_binfo = BINFO_BASE_BINFO (TYPE_BINFO (type), 0);
	tree tinfo = get_tinfo_ptr (BINFO_TYPE (base_binfo));

	/* get_tinfo_ptr might have reallocated the tinfo_descs vector.  */
	ti = &(*tinfo_descs)[tk_index];
	return class_initializer (ti, type, 1, tinfo);
      }

    default:
      {
	int hint = ((CLASSTYPE_REPEATED_BASE_P (type) << 0)
		    | (CLASSTYPE_DIAMOND_SHAPED_P (type) << 1));
	tree binfo = TYPE_BINFO (type);
	unsigned nbases = BINFO_N_BASE_BINFOS (binfo);
	vec<tree, va_gc> *base_accesses = BINFO_BASE_ACCESSES (binfo);
	tree offset_type = LONGPTR_T;
	vec<constructor_elt, va_gc> *init_vec = NULL;

	gcc_assert (tk_index - TK_VMI_CLASS_TYPES + 1 == nbases);

	vec_safe_grow (init_vec, nbases);
	/* Generate the base information initializer.  */
	for (unsigned ix = nbases; ix--;)
	  {
	    tree base_binfo = BINFO_BASE_BINFO (binfo, ix);
	    int flags = 0;
	    tree tinfo;
	    tree offset;
	    vec<constructor_elt, va_gc> *v;

	    if ((*base_accesses)[ix] == access_public_node)
	      flags |= 2;
	    tinfo = get_tinfo_ptr (BINFO_TYPE (base_binfo));
	    if (BINFO_VIRTUAL_P (base_binfo))
	      {
		/* We store the vtable offset at which the virtual
		   base offset can be found.  */
		offset = BINFO_VPTR_FIELD (base_binfo);
		flags |= 1;
	      }
	    else
	      offset = BINFO_OFFSET (base_binfo);

	    /* Combine offset and flags into one field.  */
	    offset = fold_convert (offset_type, offset);
	    offset = fold_build2_loc (input_location,
				  LSHIFT_EXPR, offset_type, offset,
				  build_int_cst (offset_type, 8));
	    offset = fold_build2_loc (input_location,
				  BIT_IOR_EXPR, offset_type, offset,
				  build_int_cst (offset_type, flags));
	    vec_alloc (v, 2);
	    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, tinfo);
	    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, offset);
	    tree base_init = build_constructor (init_list_type_node, v);
	    constructor_elt *e = &(*init_vec)[ix];
	    e->index = NULL_TREE;
	    e->value = base_init;
	  }
	tree base_inits = build_constructor (init_list_type_node, init_vec);

	/* get_tinfo_ptr might have reallocated the tinfo_descs vector.  */
	ti = &(*tinfo_descs)[tk_index];
	return class_initializer (ti, type, 3,
				  build_int_cst (NULL_TREE, hint),
				  build_int_cst (NULL_TREE, nbases),
				  base_inits);
      }
    }
}

/* Return the index of a pseudo type info type node used to describe
   TYPE.  TYPE must be a complete type (or cv void), except at the end
   of the translation unit.  */

static unsigned
get_pseudo_ti_index (tree type)
{
  unsigned ix;

  switch (TREE_CODE (type))
    {
    case OFFSET_TYPE:
      ix = TK_POINTER_MEMBER_TYPE;
      break;

    case POINTER_TYPE:
      ix = TK_POINTER_TYPE;
      break;

    case ENUMERAL_TYPE:
      ix = TK_ENUMERAL_TYPE;
      break;

    case FUNCTION_TYPE:
      ix = TK_FUNCTION_TYPE;
      break;

    case ARRAY_TYPE:
      ix = TK_ARRAY_TYPE;
      break;

    case UNION_TYPE:
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (type))
	ix = TK_POINTER_MEMBER_TYPE;
      else if (!COMPLETE_TYPE_P (type))
	{
	  if (!at_eof)
	    cxx_incomplete_type_error (NULL_TREE, type);
	  ix = TK_CLASS_TYPE;
	}
      else if (!TYPE_BINFO (type)
	       || !BINFO_N_BASE_BINFOS (TYPE_BINFO (type)))
	ix = TK_CLASS_TYPE;
      else
	{
	  tree binfo = TYPE_BINFO (type);
	  vec<tree, va_gc> *base_accesses = BINFO_BASE_ACCESSES (binfo);
	  tree base_binfo = BINFO_BASE_BINFO (binfo, 0);
	  int num_bases = BINFO_N_BASE_BINFOS (binfo);

	  if (num_bases == 1
	      && (*base_accesses)[0] == access_public_node
	      && !BINFO_VIRTUAL_P (base_binfo)
	      && integer_zerop (BINFO_OFFSET (base_binfo)))
	    /* single non-virtual public.  */
	    ix = TK_SI_CLASS_TYPE;
	  else
	    ix = TK_VMI_CLASS_TYPES + num_bases - 1;
	}
      break;

    default:
      ix = TK_BUILTIN_TYPE;
      break;
    }
  return ix;
}

/* Return pointer to tinfo descriptor.  Possibly creating the tinfo
   descriptor in the first place.  */

static tinfo_s *
get_tinfo_desc (unsigned ix)
{
  unsigned len = tinfo_descs->length ();

  if (len <= ix)
    {
      /* too short, extend.  */
      len = ix + 1 - len;
      vec_safe_reserve (tinfo_descs, len);
      tinfo_s elt;
      elt.type = elt.vtable = elt.name = NULL_TREE;
      while (len--)
	tinfo_descs->quick_push (elt);
    }

  tinfo_s *res = &(*tinfo_descs)[ix];

  if (res->type)
    return res;

  /* Ok, we have to create it.  This layout must be consistent with
     that defined in the runtime support.  We explicitly manage the
     vtable member, and name it for real type as used in the runtime.
     The RECORD type has a different name, to avoid collisions.  We
     have to delay generating the VAR_DECL of the vtable until the end
     of the translation, when we'll have seen the library definition,
     if there was one.  */

  /* Fields to add, chained in reverse order.  */
  tree fields = NULL_TREE;

  if (ix >= TK_DERIVED_TYPES)
    {
      /* First field is the pseudo type_info base class.  */
      tree fld_base = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
				  get_tinfo_desc (TK_TYPE_INFO_TYPE)->type);

      DECL_CHAIN (fld_base) = fields;
      fields = fld_base;
    }

  switch (ix)
    {
    case TK_TYPE_INFO_TYPE:
      {
	tree fld_ptr = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				   NULL_TREE, const_ptr_type_node);
	fields = fld_ptr;

	tree fld_str = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				   NULL_TREE, const_string_type_node);
	DECL_CHAIN (fld_str) = fields;
	fields = fld_str;
	break;
      }

    case TK_BASE_TYPE:
      {
	/* Base class internal helper. Pointer to base type, offset to
	   base, flags.  */
	tree fld_ptr = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				   NULL_TREE, type_info_ptr_type);
	DECL_CHAIN (fld_ptr) = fields;
	fields = fld_ptr;

	tree fld_flag = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				    NULL_TREE, LONGPTR_T);
	DECL_CHAIN (fld_flag) = fields;
	fields = fld_flag;
	break;
      }

    case TK_BUILTIN_TYPE:
      /* Fundamental type_info */
      break;

    case TK_ARRAY_TYPE:
      break;

    case TK_FUNCTION_TYPE:
      break;

    case TK_ENUMERAL_TYPE:
      break;

    case TK_POINTER_TYPE:
    case TK_POINTER_MEMBER_TYPE:
      {
	/* Pointer type_info. Adds two fields, qualification mask and
	   pointer to the pointed to type.  This is really a
	   descendant of __pbase_type_info.  */
	tree fld_mask = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				    NULL_TREE, integer_type_node);
	DECL_CHAIN (fld_mask) = fields;
	fields = fld_mask;

	tree fld_ptr = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				   NULL_TREE, type_info_ptr_type);
	DECL_CHAIN (fld_ptr) = fields;
	fields = fld_ptr;

	if (ix == TK_POINTER_MEMBER_TYPE)
	  {
	    /* Add a pointer to the class too.  */
	    tree fld_cls = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				   NULL_TREE, type_info_ptr_type);
	    DECL_CHAIN (fld_cls) = fields;
	    fields = fld_cls;
	  }
	break;
      }

    case TK_CLASS_TYPE:
      /* Class type_info.  No additional fields.  */
      break;

    case TK_SI_CLASS_TYPE:
      {
	/* Single public non-virtual base class. Add pointer to base
	   class.  This is really a descendant of
	   __class_type_info.  */
	tree fld_ptr = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				   NULL_TREE, type_info_ptr_type);
	DECL_CHAIN (fld_ptr) = fields;
	fields = fld_ptr;
	break;
      }

    default: /* Multiple inheritance.  */
      {
	unsigned num_bases = ix - TK_VMI_CLASS_TYPES + 1;

	tree fld_flg = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				   NULL_TREE, integer_type_node);
	DECL_CHAIN (fld_flg) = fields;
	fields = fld_flg;
	
	tree fld_cnt = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				   NULL_TREE, integer_type_node);
	DECL_CHAIN (fld_cnt) = fields;
	fields = fld_cnt;

	/* Create the array of __base_class_type_info entries.  */
	tree domain = build_index_type (size_int (num_bases - 1));
	tree array = build_array_type (get_tinfo_desc (TK_BASE_TYPE)->type,
				       domain);
	tree fld_ary = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				   NULL_TREE, array);
	DECL_CHAIN (fld_ary) = fields;
	fields = fld_ary;
	break;
      }
    }

  push_abi_namespace ();

  /* Generate the pseudo type name.  */
  const char *real_name = tinfo_names[ix < TK_VMI_CLASS_TYPES
				      ? ix : unsigned (TK_VMI_CLASS_TYPES)];
  size_t name_len = strlen (real_name);
  char *pseudo_name = (char *) alloca (name_len + 30);
  memcpy (pseudo_name, real_name, name_len);
  /* Those >= TK_VMI_CLASS_TYPES need a discriminator, may as well
     apply it to all.  See get_peudo_tinfo_index where we make use of
     this.  */
  sprintf (pseudo_name + name_len, "_pseudo_%d", ix);

  /* Create the pseudo type.  */
  tree pseudo_type = make_class_type (RECORD_TYPE);
  /* Pass the fields chained in reverse.  */
  finish_builtin_struct (pseudo_type, pseudo_name, fields, NULL_TREE);
  CLASSTYPE_AS_BASE (pseudo_type) = pseudo_type;

  res->type = cp_build_qualified_type (pseudo_type, TYPE_QUAL_CONST);
  res->name = get_identifier (real_name);

  /* Pretend this is public so determine_visibility doesn't give vtables
     internal linkage.  */
  TREE_PUBLIC (TYPE_MAIN_DECL (res->type)) = 1;

  pop_abi_namespace ();
  return res;
}

/* We lazily create the type info types.  */

static void
create_tinfo_types (void)
{
  gcc_assert (!tinfo_descs);

  vec_alloc (tinfo_descs, TK_MAX + 20);
}

/* Helper for emit_support_tinfos. Emits the type_info descriptor of
   a single type.  */

void
emit_support_tinfo_1 (tree bltn)
{
  tree types[3];

  if (bltn == NULL_TREE)
    return;
  types[0] = bltn;
  types[1] = build_pointer_type (bltn);
  types[2] = build_pointer_type (cp_build_qualified_type (bltn,
							  TYPE_QUAL_CONST));

  for (int i = 0; i < 3; ++i)
    {
      tree tinfo = get_tinfo_decl (types[i]);
      TREE_USED (tinfo) = 1;
      mark_needed (tinfo);
      /* The C++ ABI requires that these objects be COMDAT.  But,
	 On systems without weak symbols, initialized COMDAT
	 objects are emitted with internal linkage.  (See
	 comdat_linkage for details.)  Since we want these objects
	 to have external linkage so that copies do not have to be
	 emitted in code outside the runtime library, we make them
	 non-COMDAT here.  

	 It might also not be necessary to follow this detail of the
	 ABI.  */
      if (!flag_weak || ! targetm.cxx.library_rtti_comdat ())
	{
	  gcc_assert (TREE_PUBLIC (tinfo) && !DECL_COMDAT (tinfo));
	  DECL_INTERFACE_KNOWN (tinfo) = 1;
	}
    }
}

/* Emit the type_info descriptors which are guaranteed to be in the runtime
   support.  Generating them here guarantees consistency with the other
   structures.  We use the following heuristic to determine when the runtime
   is being generated.  If std::__fundamental_type_info is defined, and its
   destructor is defined, then the runtime is being built.  */

void
emit_support_tinfos (void)
{
  /* Dummy static variable so we can put nullptr in the array; it will be
     set before we actually start to walk the array.  */
  static tree *const fundamentals[] =
  {
    &void_type_node,
    &boolean_type_node,
    &wchar_type_node, &char16_type_node, &char32_type_node,
    &char_type_node, &signed_char_type_node, &unsigned_char_type_node,
    &short_integer_type_node, &short_unsigned_type_node,
    &integer_type_node, &unsigned_type_node,
    &long_integer_type_node, &long_unsigned_type_node,
    &long_long_integer_type_node, &long_long_unsigned_type_node,
    &float_type_node, &double_type_node, &long_double_type_node,
    &dfloat32_type_node, &dfloat64_type_node, &dfloat128_type_node,
    &nullptr_type_node,
    0
  };
  int ix;

  /* Look for a defined class.  */
  tree bltn_type = lookup_qualified_name
    (abi_node, get_identifier ("__fundamental_type_info"), true, false, false);
  if (TREE_CODE (bltn_type) != TYPE_DECL)
    return;

  bltn_type = TREE_TYPE (bltn_type);
  if (!COMPLETE_TYPE_P (bltn_type))
    return;
  tree dtor = CLASSTYPE_DESTRUCTOR (bltn_type);
  if (!dtor || DECL_EXTERNAL (dtor))
    return;

  /* All these are really builtins.  So set the location.  */
  location_t saved_loc = input_location;
  input_location = BUILTINS_LOCATION;
  doing_runtime = 1;
  for (ix = 0; fundamentals[ix]; ix++)
    emit_support_tinfo_1 (*fundamentals[ix]);
  for (ix = 0; ix < NUM_INT_N_ENTS; ix ++)
    if (int_n_enabled_p[ix])
      {
	emit_support_tinfo_1 (int_n_trees[ix].signed_type);
	emit_support_tinfo_1 (int_n_trees[ix].unsigned_type);
      }
  for (tree t = registered_builtin_types; t; t = TREE_CHAIN (t))
    emit_support_tinfo_1 (TREE_VALUE (t));
  input_location = saved_loc;
}

/* Finish a type info decl. DECL_PTR is a pointer to an unemitted
   tinfo decl.  Determine whether it needs emitting, and if so
   generate the initializer.  */

bool
emit_tinfo_decl (tree decl)
{
  tree type = TREE_TYPE (DECL_NAME (decl));
  int in_library = typeinfo_in_lib_p (type);

  gcc_assert (DECL_TINFO_P (decl));

  if (in_library)
    {
      if (doing_runtime)
	DECL_EXTERNAL (decl) = 0;
      else
	{
	  /* If we're not in the runtime, then DECL (which is already
	     DECL_EXTERNAL) will not be defined here.  */
	  DECL_INTERFACE_KNOWN (decl) = 1;
	  return false;
	}
    }
  else if (involves_incomplete_p (type))
    {
      if (!decl_needed_p (decl))
	return false;
      /* If TYPE involves an incomplete class type, then the typeinfo
	 object will be emitted with internal linkage.  There is no
	 way to know whether or not types are incomplete until the end
	 of the compilation, so this determination must be deferred
	 until this point.  */
      TREE_PUBLIC (decl) = 0;
      DECL_EXTERNAL (decl) = 0;
      DECL_INTERFACE_KNOWN (decl) = 1;
    }

  import_export_decl (decl);
  if (DECL_NOT_REALLY_EXTERN (decl) && decl_needed_p (decl))
    {
      tree init;

      DECL_EXTERNAL (decl) = 0;
      init = get_pseudo_ti_init (type, get_pseudo_ti_index (type));
      DECL_INITIAL (decl) = init;
      mark_used (decl);
      cp_finish_decl (decl, init, false, NULL_TREE, 0);
      /* Avoid targets optionally bumping up the alignment to improve
	 vector instruction accesses, tinfo are never accessed this way.  */
#ifdef DATA_ABI_ALIGNMENT
      SET_DECL_ALIGN (decl, DATA_ABI_ALIGNMENT (decl, TYPE_ALIGN (TREE_TYPE (decl))));
      DECL_USER_ALIGN (decl) = true;
#endif
      return true;
    }
  else
    return false;
}

#include "gt-cp-rtti.h"
