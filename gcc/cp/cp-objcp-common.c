/* Some code common to C++ and ObjC++ front ends.
   Copyright (C) 2004-2018 Free Software Foundation, Inc.
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
    case DEFAULT_ARG:		return sizeof (tree_default_arg);
    case DEFERRED_NOEXCEPT:	return sizeof (tree_deferred_noexcept);
    case OVERLOAD:		return sizeof (tree_overload);
    case STATIC_ASSERT:         return sizeof (tree_static_assert);
    case TYPE_ARGUMENT_PACK:
    case TYPE_PACK_EXPANSION:	return sizeof (tree_type_non_common);
    case NONTYPE_ARGUMENT_PACK:
    case EXPR_PACK_EXPANSION:	return sizeof (tree_exp);
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

static GTY((cache)) tree_cache_map *debug_type_map;

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
      if (debug_type_map == NULL)
	debug_type_map = tree_cache_map::create_ggc (512);
      else if (tree *slot = debug_type_map->get (ktype))
	return *slot;
      debug_type_map->put (ktype, dtype);
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
      if ((TREE_CODE (type) == FUNCTION_TYPE
	   || TREE_CODE (type) == METHOD_TYPE)
	  && FUNCTION_REF_QUALIFIED (type)
	  && !FUNCTION_RVALUE_QUALIFIED (type))
	return 1;
      break;

    case DW_AT_rvalue_reference:
      if ((TREE_CODE (type) == FUNCTION_TYPE
	   || TREE_CODE (type) == METHOD_TYPE)
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

    case SWITCH_STMT:
      return (!SWITCH_STMT_ALL_CASES_P (stmt)
	      || !SWITCH_STMT_NO_BREAK_P (stmt)
	      || block_may_fallthru (SWITCH_STMT_BODY (stmt)));

    default:
      return true;
    }
}

/* Return the list of decls in the global namespace.  */

tree
cp_get_global_decls ()
{
  return NAMESPACE_LEVEL (global_namespace)->names;
}

/* Push DECL into the current scope.  */

tree
cp_pushdecl (tree decl)
{
  return pushdecl (decl);
}

/* Get the global value binding of NAME.  Called directly from
   c-common.c, not via a hook. */

tree
identifier_global_value (tree name)
{
  return get_global_binding (name);
}

/* Register c++-specific dumps.  */

void
cp_register_dumps (gcc::dump_manager *dumps)
{
  class_dump_id = dumps->dump_register
    (".class", "lang-class", "lang-class", DK_lang, OPTGROUP_NONE, false);

  raw_dump_id = dumps->dump_register
    (".raw", "lang-raw", "lang-raw", DK_lang, OPTGROUP_NONE, false);
}

void
cp_common_init_ts (void)
{
  MARK_TS_DECL_NON_COMMON (USING_DECL);
  MARK_TS_DECL_COMMON (TEMPLATE_DECL);
  MARK_TS_DECL_COMMON (WILDCARD_DECL);

  MARK_TS_COMMON (TEMPLATE_TEMPLATE_PARM);
  MARK_TS_COMMON (TEMPLATE_TYPE_PARM);
  MARK_TS_COMMON (TEMPLATE_PARM_INDEX);
  MARK_TS_COMMON (OVERLOAD);
  MARK_TS_COMMON (TEMPLATE_INFO);
  MARK_TS_COMMON (TYPENAME_TYPE);
  MARK_TS_COMMON (TYPEOF_TYPE);
  MARK_TS_COMMON (UNDERLYING_TYPE);
  MARK_TS_COMMON (BASELINK);
  MARK_TS_COMMON (TYPE_PACK_EXPANSION);
  MARK_TS_COMMON (TYPE_ARGUMENT_PACK);
  MARK_TS_COMMON (DECLTYPE_TYPE);
  MARK_TS_COMMON (BOUND_TEMPLATE_TEMPLATE_PARM);
  MARK_TS_COMMON (UNBOUND_CLASS_TEMPLATE);

  MARK_TS_TYPED (EXPR_PACK_EXPANSION);
  MARK_TS_TYPED (SWITCH_STMT);
  MARK_TS_TYPED (IF_STMT);
  MARK_TS_TYPED (FOR_STMT);
  MARK_TS_TYPED (RANGE_FOR_STMT);
  MARK_TS_TYPED (AGGR_INIT_EXPR);
  MARK_TS_TYPED (EXPR_STMT);
  MARK_TS_TYPED (EH_SPEC_BLOCK);
  MARK_TS_TYPED (CLEANUP_STMT);
  MARK_TS_TYPED (SCOPE_REF);
  MARK_TS_TYPED (CAST_EXPR);
  MARK_TS_TYPED (NON_DEPENDENT_EXPR);
  MARK_TS_TYPED (MODOP_EXPR);
  MARK_TS_TYPED (TRY_BLOCK);
  MARK_TS_TYPED (THROW_EXPR);
  MARK_TS_TYPED (HANDLER);
  MARK_TS_TYPED (REINTERPRET_CAST_EXPR);
  MARK_TS_TYPED (CONST_CAST_EXPR);
  MARK_TS_TYPED (STATIC_CAST_EXPR);
  MARK_TS_TYPED (DYNAMIC_CAST_EXPR);
  MARK_TS_TYPED (IMPLICIT_CONV_EXPR);
  MARK_TS_TYPED (TEMPLATE_ID_EXPR);
  MARK_TS_TYPED (ARROW_EXPR);
  MARK_TS_TYPED (SIZEOF_EXPR);
  MARK_TS_TYPED (ALIGNOF_EXPR);
  MARK_TS_TYPED (AT_ENCODE_EXPR);
  MARK_TS_TYPED (UNARY_PLUS_EXPR);
  MARK_TS_TYPED (TRAIT_EXPR);
  MARK_TS_TYPED (TYPE_ARGUMENT_PACK);
  MARK_TS_TYPED (NOEXCEPT_EXPR);
  MARK_TS_TYPED (NONTYPE_ARGUMENT_PACK);
  MARK_TS_TYPED (WHILE_STMT);
  MARK_TS_TYPED (NEW_EXPR);
  MARK_TS_TYPED (VEC_NEW_EXPR);
  MARK_TS_TYPED (BREAK_STMT);
  MARK_TS_TYPED (MEMBER_REF);
  MARK_TS_TYPED (DOTSTAR_EXPR);
  MARK_TS_TYPED (DO_STMT);
  MARK_TS_TYPED (DELETE_EXPR);
  MARK_TS_TYPED (VEC_DELETE_EXPR);
  MARK_TS_TYPED (CONTINUE_STMT);
  MARK_TS_TYPED (TAG_DEFN);
  MARK_TS_TYPED (PSEUDO_DTOR_EXPR);
  MARK_TS_TYPED (TYPEID_EXPR);
  MARK_TS_TYPED (MUST_NOT_THROW_EXPR);
  MARK_TS_TYPED (STMT_EXPR);
  MARK_TS_TYPED (OFFSET_REF);
  MARK_TS_TYPED (OFFSETOF_EXPR);
  MARK_TS_TYPED (ADDRESSOF_EXPR);
  MARK_TS_TYPED (PTRMEM_CST);
  MARK_TS_TYPED (EMPTY_CLASS_EXPR);
  MARK_TS_TYPED (VEC_INIT_EXPR);
  MARK_TS_TYPED (USING_STMT);
  MARK_TS_TYPED (LAMBDA_EXPR);
  MARK_TS_TYPED (CTOR_INITIALIZER);
  MARK_TS_TYPED (REQUIRES_EXPR);
  MARK_TS_TYPED (UNARY_LEFT_FOLD_EXPR);
  MARK_TS_TYPED (UNARY_RIGHT_FOLD_EXPR);
  MARK_TS_TYPED (BINARY_LEFT_FOLD_EXPR);
  MARK_TS_TYPED (BINARY_RIGHT_FOLD_EXPR);
}

#include "gt-cp-cp-objcp-common.h"
