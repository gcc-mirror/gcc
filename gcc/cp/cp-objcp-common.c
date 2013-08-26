/* Some code common to C++ and ObjC++ front ends.
   Copyright (C) 2004-2013 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "cp-tree.h"
#include "c-family/c-common.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "diagnostic.h"
#include "debug.h"
#include "cxx-pretty-print.h"
#include "cp-objcp-common.h"

#include <new>                       // For placement new.

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
      || (POINTER_TYPE_P (t)
	  && TYPE_PTRMEMFUNC_P (TREE_TYPE (t))))
    return 0;

  return c_common_get_alias_set (t);
}

/* Called from check_global_declarations.  */

bool
cxx_warn_unused_global_decl (const_tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (decl))
    return false;
  if (DECL_IN_SYSTEM_HEADER (decl))
    return false;

  /* Const variables take the place of #defines in C++.  */
  if (VAR_P (decl) && TREE_READONLY (decl))
    return false;

  return true;
}

/* Langhook for tree_size: determine size of our 'x' and 'c' nodes.  */
size_t
cp_tree_size (enum tree_code code)
{
  switch (code)
    {
    case PTRMEM_CST:		return sizeof (struct ptrmem_cst);
    case BASELINK:		return sizeof (struct tree_baselink);
    case TEMPLATE_PARM_INDEX:	return sizeof (template_parm_index);
    case DEFAULT_ARG:		return sizeof (struct tree_default_arg);
    case DEFERRED_NOEXCEPT:	return sizeof (struct tree_deferred_noexcept);
    case OVERLOAD:		return sizeof (struct tree_overload);
    case STATIC_ASSERT:         return sizeof (struct tree_static_assert);
    case TYPE_ARGUMENT_PACK:
    case TYPE_PACK_EXPANSION:
      return sizeof (struct tree_common);

    case NONTYPE_ARGUMENT_PACK:
    case EXPR_PACK_EXPANSION:
      return sizeof (struct tree_exp);

    case ARGUMENT_PACK_SELECT:
      return sizeof (struct tree_argument_pack_select);

    case TRAIT_EXPR:
      return sizeof (struct tree_trait_expr);

    case LAMBDA_EXPR:           return sizeof (struct tree_lambda_expr);

    case TEMPLATE_INFO:         return sizeof (struct tree_template_info);

    case USERDEF_LITERAL:	return sizeof (struct tree_userdef_literal);

    default:
      gcc_unreachable ();
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

/* Construct a C++-aware pretty-printer for CONTEXT.  It is assumed
   that CONTEXT->printer is an already constructed basic pretty_printer.  */
void
cxx_initialize_diagnostics (diagnostic_context *context)
{
  c_common_initialize_diagnostics (context);

  pretty_printer *base = context->printer;
  cxx_pretty_printer *pp = XNEW (cxx_pretty_printer);
  context->printer = new (pp) cxx_pretty_printer ();

  /* It is safe to free this object because it was previously XNEW()'d.  */
  base->~pretty_printer ();
  XDELETE (base);
}

/* This compares two types for equivalence ("compatible" in C-based languages).
   This routine should only return 1 if it is sure.  It should not be used
   in contexts where erroneously returning 0 causes problems.  */

int
cxx_types_compatible_p (tree x, tree y)
{
  return same_type_ignoring_top_level_qualifiers_p (x, y);
}

/* Return true if DECL is explicit member function.  */

bool
cp_function_decl_explicit_p (tree decl)
{
  return (decl
	  && FUNCTION_FIRST_USER_PARMTYPE (decl) != void_list_node
	  && DECL_LANG_SPECIFIC (STRIP_TEMPLATE (decl))
	  && DECL_NONCONVERTING_P (decl));
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

static GTY ((if_marked ("tree_decl_map_marked_p"), param_is (struct tree_decl_map)))
     htab_t shadowed_var_for_decl;

/* Lookup a shadowed var for FROM, and return it if we find one.  */

tree
decl_shadowed_for_var_lookup (tree from)
{
  struct tree_decl_map *h, in;
  in.base.from = from;

  h = (struct tree_decl_map *)
      htab_find_with_hash (shadowed_var_for_decl, &in, DECL_UID (from));
  if (h)
    return h->to;
  return NULL_TREE;
}

/* Insert a mapping FROM->TO in the shadowed var hashtable.  */

void
decl_shadowed_for_var_insert (tree from, tree to)
{
  struct tree_decl_map *h;
  void **loc;

  h = ggc_alloc_tree_decl_map ();
  h->base.from = from;
  h->to = to;
  loc = htab_find_slot_with_hash (shadowed_var_for_decl, h, DECL_UID (from),
				  INSERT);
  *(struct tree_decl_map **) loc = h;
}

void
init_shadowed_var_for_decl (void)
{
  shadowed_var_for_decl = htab_create_ggc (512, tree_decl_map_hash,
					   tree_decl_map_eq, 0);
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

    default:
      return true;
    }
}

void
cp_common_init_ts (void)
{
  MARK_TS_DECL_NON_COMMON (NAMESPACE_DECL);
  MARK_TS_DECL_NON_COMMON (USING_DECL);
  MARK_TS_DECL_NON_COMMON (TEMPLATE_DECL);

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
  MARK_TS_TYPED (PTRMEM_CST);
  MARK_TS_TYPED (EMPTY_CLASS_EXPR);
  MARK_TS_TYPED (VEC_INIT_EXPR);
  MARK_TS_TYPED (USING_STMT);
  MARK_TS_TYPED (LAMBDA_EXPR);
  MARK_TS_TYPED (CTOR_INITIALIZER);
  MARK_TS_TYPED (ARRAY_NOTATION_REF);
}

#include "gt-cp-cp-objcp-common.h"
