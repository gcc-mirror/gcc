/* Language-dependent hooks for C++.
   Copyright 2001 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva  <aoliva@redhat.com>

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
#include "c-common.h"
#include "toplev.h"
#include "langhooks.h"
#include "langhooks-def.h"

static HOST_WIDE_INT cxx_get_alias_set PARAMS ((tree));
static bool ok_to_generate_alias_set_for_type PARAMS ((tree));

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU C++"
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT cxx_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH cxx_finish
#undef LANG_HOOKS_CLEAR_BINDING_STACK
#define LANG_HOOKS_CLEAR_BINDING_STACK pop_everything
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS cxx_init_options
#undef LANG_HOOKS_DECODE_OPTION
#define LANG_HOOKS_DECODE_OPTION cxx_decode_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS cxx_post_options
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET cxx_get_alias_set
#undef LANG_HOOKS_EXPAND_CONSTANT
#define LANG_HOOKS_EXPAND_CONSTANT cplus_expand_constant
#undef LANG_HOOKS_EXPAND_EXPR
#define LANG_HOOKS_EXPAND_EXPR cxx_expand_expr
#undef LANG_HOOKS_SAFE_FROM_P
#define LANG_HOOKS_SAFE_FROM_P c_safe_from_p
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE c_common_parse_file
#undef LANG_HOOKS_DUP_LANG_SPECIFIC_DECL
#define LANG_HOOKS_DUP_LANG_SPECIFIC_DECL cxx_dup_lang_specific_decl
#undef LANG_HOOKS_UNSAVE_EXPR_NOW
#define LANG_HOOKS_UNSAVE_EXPR_NOW cxx_unsave_expr_now
#undef LANG_HOOKS_MAYBE_BUILD_CLEANUP
#define LANG_HOOKS_MAYBE_BUILD_CLEANUP cxx_maybe_build_cleanup
#undef LANG_HOOKS_INSERT_DEFAULT_ATTRIBUTES
#define LANG_HOOKS_INSERT_DEFAULT_ATTRIBUTES cxx_insert_default_attributes
#undef LANG_HOOKS_MARK_TREE
#define LANG_HOOKS_MARK_TREE cxx_mark_tree
#undef LANG_HOOKS_UNSAFE_FOR_REEVAL
#define LANG_HOOKS_UNSAFE_FOR_REEVAL c_common_unsafe_for_reeval
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE cxx_mark_addressable
#undef LANG_HOOKS_PRINT_STATISTICS
#define LANG_HOOKS_PRINT_STATISTICS cxx_print_statistics
#undef LANG_HOOKS_PRINT_XNODE
#define LANG_HOOKS_PRINT_XNODE cxx_print_xnode
#undef LANG_HOOKS_PRINT_DECL
#define LANG_HOOKS_PRINT_DECL cxx_print_decl
#undef LANG_HOOKS_PRINT_TYPE
#define LANG_HOOKS_PRINT_TYPE cxx_print_type
#undef LANG_HOOKS_PRINT_IDENTIFIER
#define LANG_HOOKS_PRINT_IDENTIFIER cxx_print_identifier
#undef LANG_HOOKS_DECL_PRINTABLE_NAME
#define LANG_HOOKS_DECL_PRINTABLE_NAME	cxx_printable_name
#undef LANG_HOOKS_PRINT_ERROR_FUNCTION
#define LANG_HOOKS_PRINT_ERROR_FUNCTION	cxx_print_error_function
#undef LANG_HOOKS_SET_YYDEBUG
#define LANG_HOOKS_SET_YYDEBUG cxx_set_yydebug

#undef LANG_HOOKS_TREE_INLINING_WALK_SUBTREES
#define LANG_HOOKS_TREE_INLINING_WALK_SUBTREES \
  cp_walk_subtrees
#undef LANG_HOOKS_TREE_INLINING_CANNOT_INLINE_TREE_FN
#define LANG_HOOKS_TREE_INLINING_CANNOT_INLINE_TREE_FN \
  cp_cannot_inline_tree_fn
#undef LANG_HOOKS_TREE_INLINING_ADD_PENDING_FN_DECLS
#define LANG_HOOKS_TREE_INLINING_ADD_PENDING_FN_DECLS \
  cp_add_pending_fn_decls
#undef LANG_HOOKS_TREE_INLINING_TREE_CHAIN_MATTERS_P
#define LANG_HOOKS_TREE_INLINING_TREE_CHAIN_MATTERS_P \
  cp_is_overload_p
#undef LANG_HOOKS_TREE_INLINING_AUTO_VAR_IN_FN_P
#define LANG_HOOKS_TREE_INLINING_AUTO_VAR_IN_FN_P \
  cp_auto_var_in_fn_p
#undef LANG_HOOKS_TREE_INLINING_COPY_RES_DECL_FOR_INLINING
#define LANG_HOOKS_TREE_INLINING_COPY_RES_DECL_FOR_INLINING \
  cp_copy_res_decl_for_inlining
#undef LANG_HOOKS_TREE_INLINING_ANON_AGGR_TYPE_P
#define LANG_HOOKS_TREE_INLINING_ANON_AGGR_TYPE_P anon_aggr_type_p
#undef LANG_HOOKS_TREE_INLINING_START_INLINING
#define LANG_HOOKS_TREE_INLINING_START_INLINING cp_start_inlining
#undef LANG_HOOKS_TREE_INLINING_END_INLINING
#define LANG_HOOKS_TREE_INLINING_END_INLINING cp_end_inlining
#undef LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN
#define LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN cp_dump_tree
#undef LANG_HOOKS_TREE_DUMP_TYPE_QUALS_FN
#define LANG_HOOKS_TREE_DUMP_TYPE_QUALS_FN cp_type_quals

#undef LANG_HOOKS_MAKE_TYPE
#define LANG_HOOKS_MAKE_TYPE cxx_make_type
#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE c_common_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE c_common_type_for_size
#undef LANG_HOOKS_SIGNED_TYPE
#define LANG_HOOKS_SIGNED_TYPE c_common_signed_type
#undef LANG_HOOKS_UNSIGNED_TYPE
#define LANG_HOOKS_UNSIGNED_TYPE c_common_unsigned_type
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE c_common_signed_or_unsigned_type

/* Each front end provides its own hooks, for toplev.c.  */
const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Check if a C++ type is safe for aliasing.
   Return TRUE if T safe for aliasing FALSE otherwise.  */

static bool
ok_to_generate_alias_set_for_type (t)
     tree t;
{
  if (TYPE_PTRMEMFUNC_P (t))
    return true;
  if (AGGREGATE_TYPE_P (t))
    {
      if ((TREE_CODE (t) == RECORD_TYPE) || (TREE_CODE (t) == UNION_TYPE))
	{
	  tree fields;
	  /* PODs are safe.  */
	  if (! CLASSTYPE_NON_POD_P(t))
	    return true;
	  /* Classes with virtual baseclasses are not.  */
	  if (TYPE_USES_VIRTUAL_BASECLASSES (t))
	    return false;
	  /* Recursively check the base classes.  */
	  if (TYPE_BINFO (t) != NULL && TYPE_BINFO_BASETYPES (t) != NULL)
	    {
	      int i;
	      for (i = 0; i < TREE_VEC_LENGTH (TYPE_BINFO_BASETYPES (t)); i++)
		{
		  tree binfo = TREE_VEC_ELT (TYPE_BINFO_BASETYPES (t), i);
		  if (!ok_to_generate_alias_set_for_type (BINFO_TYPE (binfo)))
		    return false;
		}
	    }
	  /* Check all the fields.  */
	  for (fields = TYPE_FIELDS (t); fields; fields = TREE_CHAIN (fields))
	    {
	      if (TREE_CODE (fields) != FIELD_DECL)
		continue;
	      if (! ok_to_generate_alias_set_for_type (TREE_TYPE (fields)))
		return false;
	    }
	  return true;
	}
      else if (TREE_CODE (t) == ARRAY_TYPE)
	return ok_to_generate_alias_set_for_type (TREE_TYPE (t));
      else
	/* This should never happen, we dealt with all the aggregate
	   types that can appear in C++ above.  */
	abort ();
    }
  else
    return true;
}

/* Tree code classes. */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const char tree_code_type[] = {
#include "tree.def"
  'x',
#include "c-common.def"
  'x',
#include "cp-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

const unsigned char tree_code_length[] = {
#include "tree.def"
  0,
#include "c-common.def"
  0,
#include "cp-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

const char *const tree_code_name[] = {
#include "tree.def"
  "@@dummy",
#include "c-common.def"
  "@@dummy",
#include "cp-tree.def"
};
#undef DEFTREECODE

/* Special routine to get the alias set for C++.  */

static HOST_WIDE_INT
cxx_get_alias_set (t)
     tree t;
{
  /* It's not yet safe to use alias sets for classes in C++.  */
  if (!ok_to_generate_alias_set_for_type(t))
    return 0;

  return c_common_get_alias_set (t);
}
