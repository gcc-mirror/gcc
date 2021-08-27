/* Some code common to C++ and ObjC++ front ends.
   Copyright (C) 2004-2021 Free Software Foundation, Inc.
   Contributed by Ziemowit Laski  <zlaski@apple.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "cp-objcp-common.h"
#include "dwarf2.h"
#include "stringpool.h"

/* Special routine to get the alias set for C++.  */

alias_set_type
cxx_get_alias_set (tree t)
{
  if (IS_FAKE_BASE_TYPE (t))
    /* The base variant of a type must be in the same alias set as the
       complete type.  */
    return get_alias_set (TYPE_CONTEXT (t));

  /* Punt on PMFs until we canonicalize functions properly.  */
  if (TYPE_PTRMEMFUNC_P (t)
      || (INDIRECT_TYPE_P (t)
	  && TYPE_PTRMEMFUNC_P (TREE_TYPE (t))))
    return 0;

  return c_common_get_alias_set (t);
}

/* Called from check_global_declaration.  */

bool
cxx_warn_unused_global_decl (const_tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (decl))
    return false;
  if (DECL_IN_SYSTEM_HEADER (decl))
    return false;

  return true;
}

/* Langhook for tree_size: determine size of our 'x' and 'c' nodes.  */
size_t
cp_tree_size (enum tree_code code)
{
  gcc_checking_assert (code >= NUM_TREE_CODES);
  switch (code)
    {
    case PTRMEM_CST:		return sizeof (ptrmem_cst);
    case BASELINK:		return sizeof (tree_baselink);
    case TEMPLATE_PARM_INDEX:	return sizeof (template_parm_index);
    case DEFERRED_PARSE:	return sizeof (tree_deferred_parse);
    case DEFERRED_NOEXCEPT:	return sizeof (tree_deferred_noexcept);
    case OVERLOAD:		return sizeof (tree_overload);
    case STATIC_ASSERT:         return sizeof (tree_static_assert);
#if 0
      /* This would match cp_common_init_ts, but breaks GC because
	 tree_node_structure_for_code returns TS_TYPE_NON_COMMON for all
	 types.  */
    case UNBOUND_CLASS_TEMPLATE:
    case TYPE_ARGUMENT_PACK:	return sizeof (tree_type_common);
#endif
    case ARGUMENT_PACK_SELECT:	return sizeof (tree_argument_pack_select);
    case TRAIT_EXPR:		return sizeof (tree_trait_expr);
    case LAMBDA_EXPR:           return sizeof (tree_lambda_expr);
    case TEMPLATE_INFO:         return sizeof (tree_template_info);
    case CONSTRAINT_INFO:       return sizeof (tree_constraint_info);
    case USERDEF_LITERAL:	return sizeof (tree_userdef_literal);
    case TEMPLATE_DECL:		return sizeof (tree_template_decl);
    default:
      switch (TREE_CODE_CLASS (code))
	{
	case tcc_declaration:	return sizeof (tree_decl_non_common);
	case tcc_type:		return sizeof (tree_type_non_common);
	default: gcc_unreachable ();
	}
    }
  /* NOTREACHED */
}

/* Returns true if T is a variably modified type, in the sense of C99.
   FN is as passed to variably_modified_p.
   This routine needs only check cases that cannot be handled by the
   language-independent logic in tree.c.  */

bool
cp_var_mod_type_p (tree type, tree fn)
{
  /* If TYPE is a pointer-to-member, it is variably modified if either
     the class or the member are variably modified.  */
  if (TYPE_PTRMEM_P (type))
    return (variably_modified_type_p (TYPE_PTRMEM_CLASS_TYPE (type), fn)
	    || variably_modified_type_p (TYPE_PTRMEM_POINTED_TO_TYPE (type),
					 fn));

  /* All other types are not variably modified.  */
  return false;
}

/* This compares two types for equivalence ("compatible" in C-based languages).
   This routine should only return 1 if it is sure.  It should not be used
   in contexts where erroneously returning 0 causes problems.  */

int
cxx_types_compatible_p (tree x, tree y)
{
  return same_type_ignoring_top_level_qualifiers_p (x, y);
}

static GTY((cache)) type_tree_cache_map *debug_type_map;

/* Return a type to use in the debug info instead of TYPE, or NULL_TREE to
   keep TYPE.  */

tree
cp_get_debug_type (const_tree type)
{
  tree dtype = NULL_TREE;

  if (TYPE_PTRMEMFUNC_P (type) && !typedef_variant_p (type))
    dtype = build_offset_type (TYPE_PTRMEMFUNC_OBJECT_TYPE (type),
			       TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (type)));

  /* We cannot simply return the debug type here because the function uses
     the type canonicalization hashtable, which is GC-ed, so its behavior
     depends on the actual collection points.  Since we are building these
     types on the fly for the debug info only, they would not be attached
     to any GC root and always be swept, so we would make the contents of
     the debug info depend on the collection points.  */
  if (dtype)
    {
      tree ktype = CONST_CAST_TREE (type);
      if (tree *slot = hash_map_safe_get (debug_type_map, ktype))
	return *slot;
      hash_map_safe_put<hm_ggc> (debug_type_map, ktype, dtype);
    }

  return dtype;
}

/* Return -1 if dwarf ATTR shouldn't be added for DECL, or the attribute
   value otherwise.  */
int
cp_decl_dwarf_attribute (const_tree decl, int attr)
{
  if (decl == NULL_TREE)
    return -1;

  switch (attr)
    {
    case DW_AT_explicit:
      if (TREE_CODE (decl) == FUNCTION_DECL
	  && DECL_LANG_SPECIFIC (STRIP_TEMPLATE (decl))
	  && DECL_NONCONVERTING_P (decl))
	return 1;
      break;

    case DW_AT_deleted:
      if (TREE_CODE (decl) == FUNCTION_DECL
	  && DECL_LANG_SPECIFIC (STRIP_TEMPLATE (decl))
	  && DECL_DELETED_FN (decl))
	return 1;
      break;

    case DW_AT_defaulted:
      if (TREE_CODE (decl) == FUNCTION_DECL
	  && DECL_LANG_SPECIFIC (STRIP_TEMPLATE (decl))
	  && DECL_DEFAULTED_FN (decl))
	{
	  if (DECL_DEFAULTED_IN_CLASS_P (decl))
	    return DW_DEFAULTED_in_class;

	  if (DECL_DEFAULTED_OUTSIDE_CLASS_P (decl))
	    return DW_DEFAULTED_out_of_class;
	}
      break;

    case DW_AT_const_expr:
      if (VAR_OR_FUNCTION_DECL_P (decl) && DECL_DECLARED_CONSTEXPR_P (decl))
	return 1;
      break;

    case DW_AT_reference:
      if (TREE_CODE (decl) == FUNCTION_DECL
	  && DECL_NONSTATIC_MEMBER_FUNCTION_P (decl)
	  && FUNCTION_REF_QUALIFIED (TREE_TYPE (decl))
	  && !FUNCTION_RVALUE_QUALIFIED (TREE_TYPE (decl)))
	return 1;
      break;

    case DW_AT_rvalue_reference:
      if (TREE_CODE (decl) == FUNCTION_DECL
	  && DECL_NONSTATIC_MEMBER_FUNCTION_P (decl)
	  && FUNCTION_REF_QUALIFIED (TREE_TYPE (decl))
	  && FUNCTION_RVALUE_QUALIFIED (TREE_TYPE (decl)))
	return 1;
      break;

    case DW_AT_inline:
      if (VAR_P (decl) && DECL_INLINE_VAR_P (decl))
	{
	  if (DECL_VAR_DECLARED_INLINE_P (decl))
	    return DW_INL_declared_inlined;
	  else
	    return DW_INL_inlined;
	}
      break;

    case DW_AT_export_symbols:
      if (TREE_CODE (decl) == NAMESPACE_DECL
	  && (DECL_NAMESPACE_INLINE_P (decl)
	      || (DECL_NAME (decl) == NULL_TREE && dwarf_version >= 5)))
	return 1;
      break;

    default:
      break;
    }

  return -1;
}

/* Return -1 if dwarf ATTR shouldn't be added for TYPE, or the attribute
   value otherwise.  */
int
cp_type_dwarf_attribute (const_tree type, int attr)
{
  if (type == NULL_TREE)
    return -1;

  switch (attr)
    {
    case DW_AT_reference:
      if (FUNC_OR_METHOD_TYPE_P (type)
	  && FUNCTION_REF_QUALIFIED (type)
	  && !FUNCTION_RVALUE_QUALIFIED (type))
	return 1;
      break;

    case DW_AT_rvalue_reference:
      if (FUNC_OR_METHOD_TYPE_P (type)
	  && FUNCTION_REF_QUALIFIED (type)
	  && FUNCTION_RVALUE_QUALIFIED (type))
	return 1;
      break;

    default:
      break;
    }

  return -1;
}

/* Return the unit size of TYPE without reusable tail padding.  */

tree
cp_unit_size_without_reusable_padding (tree type)
{
  if (CLASS_TYPE_P (type))
    return CLASSTYPE_SIZE_UNIT (type);
  return TYPE_SIZE_UNIT (type);
}

/* Stubs to keep c-opts.c happy.  */
void
push_file_scope (void)
{
}

void
pop_file_scope (void)
{
}

/* c-pragma.c needs to query whether a decl has extern "C" linkage.  */
bool
has_c_linkage (const_tree decl)
{
  return DECL_EXTERN_C_P (decl);
}

/* Return true if stmt can fall through.  Used by block_may_fallthru
   default case.  */

bool
cxx_block_may_fallthru (const_tree stmt)
{
  switch (TREE_CODE (stmt))
    {
    case EXPR_STMT:
      return block_may_fallthru (EXPR_STMT_EXPR (stmt));

    case THROW_EXPR:
      return false;

    case IF_STMT:
      if (block_may_fallthru (THEN_CLAUSE (stmt)))
	return true;
      return block_may_fallthru (ELSE_CLAUSE (stmt));

    case CLEANUP_STMT:
      /* Just handle the try/finally cases.  */
      if (!CLEANUP_EH_ONLY (stmt))
	{
	  return (block_may_fallthru (CLEANUP_BODY (stmt))
		  && block_may_fallthru (CLEANUP_EXPR (stmt)));
	}
      return true;

    default:
      return c_block_may_fallthru (stmt);
    }
}

/* Return the list of decls in the global namespace.  */

tree
cp_get_global_decls ()
{
  return NAMESPACE_LEVEL (global_namespace)->names;
}

/* Push DECL into the current (namespace) scope.  */

tree
cp_pushdecl (tree decl)
{
  DECL_CONTEXT (decl) = FROB_CONTEXT (current_namespace);
  return pushdecl (decl);
}

/* Get the global value binding of NAME.  Called directly from
   c-common.c, not via a hook. */

tree
identifier_global_value (tree name)
{
  return get_global_binding (name);
}

/* Similarly, but return struct/class/union NAME instead.  */

tree
identifier_global_tag (tree name)
{
  tree ret = lookup_qualified_name (global_namespace, name, LOOK_want::TYPE,
				    /*complain*/false);
  if (ret == error_mark_node)
    return NULL_TREE;
  return ret;
}

/* Returns true if NAME refers to a built-in function or function-like
   operator.  */

bool
names_builtin_p (const char *name)
{
  tree id = get_identifier (name);
  if (tree binding = get_global_binding (id))
    {
      if (TREE_CODE (binding) == FUNCTION_DECL
	  && DECL_IS_UNDECLARED_BUILTIN (binding))
	return true;

      /* Handle the case when an overload for a  built-in name exists.  */
      if (TREE_CODE (binding) != OVERLOAD)
	return false;

      for (ovl_iterator it (binding); it; ++it)
	{
	  tree decl = *it;
	  if (DECL_IS_UNDECLARED_BUILTIN (decl))
	    return true;
	}
    }

  /* Also detect common reserved C++ words that aren't strictly built-in
     functions.  */
  switch (C_RID_CODE (id))
    {
    case RID_ADDRESSOF:
    case RID_BUILTIN_CONVERTVECTOR:
    case RID_BUILTIN_HAS_ATTRIBUTE:
    case RID_BUILTIN_SHUFFLE:
    case RID_BUILTIN_SHUFFLEVECTOR:
    case RID_BUILTIN_LAUNDER:
    case RID_BUILTIN_BIT_CAST:
    case RID_OFFSETOF:
    case RID_HAS_NOTHROW_ASSIGN:
    case RID_HAS_NOTHROW_CONSTRUCTOR:
    case RID_HAS_NOTHROW_COPY:
    case RID_HAS_TRIVIAL_ASSIGN:
    case RID_HAS_TRIVIAL_CONSTRUCTOR:
    case RID_HAS_TRIVIAL_COPY:
    case RID_HAS_TRIVIAL_DESTRUCTOR:
    case RID_HAS_UNIQUE_OBJ_REPRESENTATIONS:
    case RID_HAS_VIRTUAL_DESTRUCTOR:
    case RID_IS_ABSTRACT:
    case RID_IS_AGGREGATE:
    case RID_IS_BASE_OF:
    case RID_IS_CLASS:
    case RID_IS_EMPTY:
    case RID_IS_ENUM:
    case RID_IS_FINAL:
    case RID_IS_LAYOUT_COMPATIBLE:
    case RID_IS_LITERAL_TYPE:
    case RID_IS_POINTER_INTERCONVERTIBLE_BASE_OF:
    case RID_IS_POD:
    case RID_IS_POLYMORPHIC:
    case RID_IS_SAME_AS:
    case RID_IS_STD_LAYOUT:
    case RID_IS_TRIVIAL:
    case RID_IS_TRIVIALLY_ASSIGNABLE:
    case RID_IS_TRIVIALLY_CONSTRUCTIBLE:
    case RID_IS_TRIVIALLY_COPYABLE:
    case RID_IS_UNION:
    case RID_IS_ASSIGNABLE:
    case RID_IS_CONSTRUCTIBLE:
    case RID_UNDERLYING_TYPE:
      return true;
    default:
      break;
    }

  return false;
}

/* Register c++-specific dumps.  */

void
cp_register_dumps (gcc::dump_manager *dumps)
{
  class_dump_id = dumps->dump_register
    (".class", "lang-class", "lang-class", DK_lang, OPTGROUP_NONE, false);

  module_dump_id = dumps->dump_register
    (".module", "lang-module", "lang-module", DK_lang, OPTGROUP_NONE, false);

  raw_dump_id = dumps->dump_register
    (".raw", "lang-raw", "lang-raw", DK_lang, OPTGROUP_NONE, false);
}

void
cp_common_init_ts (void)
{
  /* With type.  */
  MARK_TS_TYPED (PTRMEM_CST);
  MARK_TS_TYPED (LAMBDA_EXPR);
  MARK_TS_TYPED (TYPE_ARGUMENT_PACK);

  /* Random new trees.  */
  MARK_TS_COMMON (BASELINK);
  MARK_TS_COMMON (OVERLOAD);
  MARK_TS_COMMON (TEMPLATE_PARM_INDEX);

  /* New decls.  */
  MARK_TS_DECL_COMMON (TEMPLATE_DECL);
  MARK_TS_DECL_COMMON (WILDCARD_DECL);
  MARK_TS_DECL_COMMON (CONCEPT_DECL);

  MARK_TS_DECL_NON_COMMON (USING_DECL);

  /* New Types.  */
  MARK_TS_TYPE_COMMON (UNBOUND_CLASS_TEMPLATE);
  MARK_TS_TYPE_COMMON (TYPE_ARGUMENT_PACK);

  MARK_TS_TYPE_NON_COMMON (DECLTYPE_TYPE);
  MARK_TS_TYPE_NON_COMMON (TYPENAME_TYPE);
  MARK_TS_TYPE_NON_COMMON (TYPEOF_TYPE);
  MARK_TS_TYPE_NON_COMMON (UNDERLYING_TYPE);
  MARK_TS_TYPE_NON_COMMON (BOUND_TEMPLATE_TEMPLATE_PARM);
  MARK_TS_TYPE_NON_COMMON (TEMPLATE_TEMPLATE_PARM);
  MARK_TS_TYPE_NON_COMMON (TEMPLATE_TYPE_PARM);
  MARK_TS_TYPE_NON_COMMON (TYPE_PACK_EXPANSION);

  /* Statements.  */
  MARK_TS_EXP (CLEANUP_STMT);
  MARK_TS_EXP (EH_SPEC_BLOCK);
  MARK_TS_EXP (HANDLER);
  MARK_TS_EXP (IF_STMT);
  MARK_TS_EXP (OMP_DEPOBJ);
  MARK_TS_EXP (RANGE_FOR_STMT);
  MARK_TS_EXP (TRY_BLOCK);
  MARK_TS_EXP (USING_STMT);

  /* Random expressions.  */
  MARK_TS_EXP (ADDRESSOF_EXPR);
  MARK_TS_EXP (AGGR_INIT_EXPR);
  MARK_TS_EXP (ALIGNOF_EXPR);
  MARK_TS_EXP (ARROW_EXPR);
  MARK_TS_EXP (AT_ENCODE_EXPR);
  MARK_TS_EXP (BIT_CAST_EXPR);
  MARK_TS_EXP (CAST_EXPR);
  MARK_TS_EXP (CONST_CAST_EXPR);
  MARK_TS_EXP (CTOR_INITIALIZER);
  MARK_TS_EXP (DELETE_EXPR);
  MARK_TS_EXP (DOTSTAR_EXPR);
  MARK_TS_EXP (DYNAMIC_CAST_EXPR);
  MARK_TS_EXP (EMPTY_CLASS_EXPR);
  MARK_TS_EXP (EXPR_STMT);
  MARK_TS_EXP (IMPLICIT_CONV_EXPR);
  MARK_TS_EXP (MEMBER_REF);
  MARK_TS_EXP (MODOP_EXPR);
  MARK_TS_EXP (MUST_NOT_THROW_EXPR);
  MARK_TS_EXP (NEW_EXPR);
  MARK_TS_EXP (NOEXCEPT_EXPR);
  MARK_TS_EXP (NON_DEPENDENT_EXPR);
  MARK_TS_EXP (OFFSETOF_EXPR);
  MARK_TS_EXP (OFFSET_REF);
  MARK_TS_EXP (PSEUDO_DTOR_EXPR);
  MARK_TS_EXP (REINTERPRET_CAST_EXPR);
  MARK_TS_EXP (SCOPE_REF);
  MARK_TS_EXP (STATIC_CAST_EXPR);
  MARK_TS_EXP (STMT_EXPR);
  MARK_TS_EXP (TAG_DEFN);
  MARK_TS_EXP (TEMPLATE_ID_EXPR);
  MARK_TS_EXP (THROW_EXPR);
  MARK_TS_EXP (TRAIT_EXPR);
  MARK_TS_EXP (TYPEID_EXPR);
  MARK_TS_EXP (TYPE_EXPR);
  MARK_TS_EXP (UNARY_PLUS_EXPR);
  MARK_TS_EXP (VEC_DELETE_EXPR);
  MARK_TS_EXP (VEC_INIT_EXPR);
  MARK_TS_EXP (VEC_NEW_EXPR);
  MARK_TS_EXP (SPACESHIP_EXPR);

  /* Fold expressions.  */
  MARK_TS_EXP (BINARY_LEFT_FOLD_EXPR);
  MARK_TS_EXP (BINARY_RIGHT_FOLD_EXPR);
  MARK_TS_EXP (EXPR_PACK_EXPANSION);
  MARK_TS_EXP (NONTYPE_ARGUMENT_PACK);
  MARK_TS_EXP (UNARY_LEFT_FOLD_EXPR);
  MARK_TS_EXP (UNARY_RIGHT_FOLD_EXPR);

  /* Constraints.  */
  MARK_TS_EXP (CHECK_CONSTR);
  MARK_TS_EXP (COMPOUND_REQ);
  MARK_TS_EXP (CONJ_CONSTR);
  MARK_TS_EXP (DISJ_CONSTR);
  MARK_TS_EXP (ATOMIC_CONSTR);
  MARK_TS_EXP (NESTED_REQ);
  MARK_TS_EXP (REQUIRES_EXPR);
  MARK_TS_EXP (SIMPLE_REQ);
  MARK_TS_EXP (TYPE_REQ);

  MARK_TS_EXP (CO_AWAIT_EXPR);
  MARK_TS_EXP (CO_YIELD_EXPR);
  MARK_TS_EXP (CO_RETURN_EXPR);

  c_common_init_ts ();
}

/* Handle C++-specficic options here.  Punt to c_common otherwise.  */

bool
cp_handle_option (size_t scode, const char *arg, HOST_WIDE_INT value,
		  int kind, location_t loc,
		  const struct cl_option_handlers *handlers)
{
  if (handle_module_option (unsigned (scode), arg, value))
    return true;
  return c_common_handle_option (scode, arg, value, kind, loc, handlers);
}

#include "gt-cp-cp-objcp-common.h"
