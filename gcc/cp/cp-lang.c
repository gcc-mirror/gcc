/* Language-dependent hooks for C++.
   Copyright 2001, 2002 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva  <aoliva@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "cp-tree.h"
#include "c-common.h"
#include "toplev.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "diagnostic.h"
#include "cxx-pretty-print.h"

enum c_language_kind c_language = clk_cxx;

static HOST_WIDE_INT cxx_get_alias_set (tree);
static bool ok_to_generate_alias_set_for_type (tree);
static bool cxx_warn_unused_global_decl (tree);
static tree cp_expr_size (tree);
static size_t cp_tree_size (enum tree_code);
static bool cp_var_mod_type_p (tree);
static void cxx_initialize_diagnostics (diagnostic_context *);

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU C++"
#undef LANG_HOOKS_TREE_SIZE
#define LANG_HOOKS_TREE_SIZE cp_tree_size
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT cxx_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH cxx_finish
#undef LANG_HOOKS_CLEAR_BINDING_STACK
#define LANG_HOOKS_CLEAR_BINDING_STACK pop_everything
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS c_common_init_options
#undef LANG_HOOKS_INITIALIZE_DIAGNOSTICS
#define LANG_HOOKS_INITIALIZE_DIAGNOSTICS cxx_initialize_diagnostics
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION c_common_handle_option
#undef LANG_HOOKS_HANDLE_FILENAME
#define LANG_HOOKS_HANDLE_FILENAME c_common_handle_filename
#undef LANG_HOOKS_MISSING_ARGUMENT
#define LANG_HOOKS_MISSING_ARGUMENT c_common_missing_argument
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS c_common_post_options
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
#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#define LANG_HOOKS_TRUTHVALUE_CONVERSION c_common_truthvalue_conversion
#undef LANG_HOOKS_UNSAFE_FOR_REEVAL
#define LANG_HOOKS_UNSAFE_FOR_REEVAL c_common_unsafe_for_reeval
#undef LANG_HOOKS_SET_DECL_ASSEMBLER_NAME
#define LANG_HOOKS_SET_DECL_ASSEMBLER_NAME mangle_decl
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
#undef LANG_HOOKS_BUILTIN_TYPE_DECLS
#define LANG_HOOKS_BUILTIN_TYPE_DECLS cxx_builtin_type_decls
#undef LANG_HOOKS_PUSHLEVEL
#define LANG_HOOKS_PUSHLEVEL lhd_do_nothing_i
#undef LANG_HOOKS_POPLEVEL
#define LANG_HOOKS_POPLEVEL lhd_do_nothing_iii_return_null_tree
#undef LANG_HOOKS_WARN_UNUSED_GLOBAL_DECL
#define LANG_HOOKS_WARN_UNUSED_GLOBAL_DECL cxx_warn_unused_global_decl
#undef LANG_HOOKS_WRITE_GLOBALS
#define LANG_HOOKS_WRITE_GLOBALS lhd_do_nothing
#undef LANG_HOOKS_DECL_UNINIT
#define LANG_HOOKS_DECL_UNINIT c_decl_uninit


#undef LANG_HOOKS_FUNCTION_INIT
#define LANG_HOOKS_FUNCTION_INIT cxx_push_function_context
#undef LANG_HOOKS_FUNCTION_FINAL
#define LANG_HOOKS_FUNCTION_FINAL cxx_pop_function_context

#undef LANG_HOOKS_RTL_EXPAND_START
#define LANG_HOOKS_RTL_EXPAND_START cxx_expand_function_start
#undef LANG_HOOKS_RTL_EXPAND_STMT
#define LANG_HOOKS_RTL_EXPAND_STMT expand_stmt

/* Attribute hooks.  */
#undef LANG_HOOKS_COMMON_ATTRIBUTE_TABLE
#define LANG_HOOKS_COMMON_ATTRIBUTE_TABLE c_common_attribute_table
#undef LANG_HOOKS_FORMAT_ATTRIBUTE_TABLE
#define LANG_HOOKS_FORMAT_ATTRIBUTE_TABLE c_common_format_attribute_table
#undef LANG_HOOKS_ATTRIBUTE_TABLE
#define LANG_HOOKS_ATTRIBUTE_TABLE cxx_attribute_table

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
#undef LANG_HOOKS_TREE_INLINING_VAR_MOD_TYPE_P
#define LANG_HOOKS_TREE_INLINING_VAR_MOD_TYPE_P cp_var_mod_type_p
#undef LANG_HOOKS_TREE_INLINING_ESTIMATE_NUM_INSNS
#define LANG_HOOKS_TREE_INLINING_ESTIMATE_NUM_INSNS c_estimate_num_insns
#undef LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN
#define LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN cp_dump_tree
#undef LANG_HOOKS_TREE_DUMP_TYPE_QUALS_FN
#define LANG_HOOKS_TREE_DUMP_TYPE_QUALS_FN cp_type_quals
#undef LANG_HOOKS_EXPR_SIZE
#define LANG_HOOKS_EXPR_SIZE cp_expr_size

#undef LANG_HOOKS_CALLGRAPH_ANALYZE_EXPR
#define LANG_HOOKS_CALLGRAPH_ANALYZE_EXPR cxx_callgraph_analyze_expr
#undef LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION
#define LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION expand_body

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
#undef LANG_HOOKS_INCOMPLETE_TYPE_ERROR
#define LANG_HOOKS_INCOMPLETE_TYPE_ERROR cxx_incomplete_type_error
#undef LANG_HOOKS_TYPE_PROMOTES_TO
#define LANG_HOOKS_TYPE_PROMOTES_TO cxx_type_promotes_to
#undef LANG_HOOKS_REGISTER_BUILTIN_TYPE
#define LANG_HOOKS_REGISTER_BUILTIN_TYPE c_register_builtin_type

/* Each front end provides its own hooks, for toplev.c.  */
const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Tree code classes.  */

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

/* Check if a C++ type is safe for aliasing.
   Return TRUE if T safe for aliasing FALSE otherwise.  */

static bool
ok_to_generate_alias_set_for_type (tree t)
{
  if (TYPE_PTRMEMFUNC_P (t))
    return true;
  if (AGGREGATE_TYPE_P (t))
    {
      if ((TREE_CODE (t) == RECORD_TYPE) || (TREE_CODE (t) == UNION_TYPE))
	{
	  tree fields;
	  /* Backend-created structs are safe.  */
	  if (! CLASS_TYPE_P (t))
	    return true;
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

/* Special routine to get the alias set for C++.  */

static HOST_WIDE_INT
cxx_get_alias_set (tree t)
{
  if (TREE_CODE (t) == RECORD_TYPE
      && TYPE_CONTEXT (t) && CLASS_TYPE_P (TYPE_CONTEXT (t))
      && CLASSTYPE_AS_BASE (TYPE_CONTEXT (t)) == t)
    /* The base variant of a type must be in the same alias set as the
       complete type.  */
    return get_alias_set (TYPE_CONTEXT (t));
  
  if (/* It's not yet safe to use alias sets for some classes in C++.  */
      !ok_to_generate_alias_set_for_type (t)
      /* Nor is it safe to use alias sets for pointers-to-member
	 functions, due to the fact that there may be more than one
	 RECORD_TYPE type corresponding to the same pointer-to-member
	 type.  */
      || TYPE_PTRMEMFUNC_P (t))
    return 0;

  return c_common_get_alias_set (t);
}

/* Called from check_global_declarations.  */

static bool
cxx_warn_unused_global_decl (tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (decl))
    return false;
  if (DECL_IN_SYSTEM_HEADER (decl))
    return false;

  /* Const variables take the place of #defines in C++.  */
  if (TREE_CODE (decl) == VAR_DECL && TREE_READONLY (decl))
    return false;

  return true;
}

/* Langhook for expr_size: Tell the backend that the value of an expression
   of non-POD class type does not include any tail padding; a derived class
   might have allocated something there.  */

static tree
cp_expr_size (tree exp)
{
  if (CLASS_TYPE_P (TREE_TYPE (exp)))
    {
      /* The backend should not be interested in the size of an expression
	 of a type with both of these set; all copies of such types must go
	 through a constructor or assignment op.  */
      if (TYPE_HAS_COMPLEX_INIT_REF (TREE_TYPE (exp))
	  && TYPE_HAS_COMPLEX_ASSIGN_REF (TREE_TYPE (exp))
	  /* But storing a CONSTRUCTOR isn't a copy.  */
	  && TREE_CODE (exp) != CONSTRUCTOR)
	abort ();
      /* This would be wrong for a type with virtual bases, but they are
	 caught by the abort above.  */
      return (is_empty_class (TREE_TYPE (exp))
	      ? size_zero_node 
	      : CLASSTYPE_SIZE_UNIT (TREE_TYPE (exp)));
    }
  else
    /* Use the default code.  */
    return lhd_expr_size (exp);
}

/* Langhook for tree_size: determine size of our 'x' and 'c' nodes.  */
static size_t
cp_tree_size (enum tree_code code)
{
  switch (code)
    {
    case PTRMEM_CST: 		return sizeof (struct ptrmem_cst);
    case BASELINK:		return sizeof (struct tree_baselink);
    case TEMPLATE_PARM_INDEX: 	return sizeof (template_parm_index);
    case DEFAULT_ARG:		return sizeof (struct tree_default_arg);
    case OVERLOAD:		return sizeof (struct tree_overload);
    case WRAPPER:		return sizeof (struct tree_wrapper);
    default:
      abort ();
    }
  /* NOTREACHED */
}

/* Returns true if T is a variably modified type, in the sense of C99.
   This routine needs only check cases that cannot be handled by the
   language-independent logic in tree-inline.c.  */

static bool
cp_var_mod_type_p (tree type)
{
  /* If TYPE is a pointer-to-member, it is variably modified if either
     the class or the member are variably modified.  */
  if (TYPE_PTR_TO_MEMBER_P (type))
    return (variably_modified_type_p (TYPE_PTRMEM_CLASS_TYPE (type))
	    || variably_modified_type_p (TYPE_PTRMEM_POINTED_TO_TYPE (type)));

  /* All other types are not variably modified.  */
  return false;
}

/* Stub routine to tell people that this doesn't work yet.  */
void
c_reset_state (void)
{
  sorry ("inter-module optimisations not implemented yet");
}

/* Construct a C++-aware pretty-printer for CONTEXT.  It is assumed
   that CONTEXT->printer is an already constructed basic pretty_printer.  */
static void
cxx_initialize_diagnostics (diagnostic_context *context)
{
  pretty_printer *base = context->printer;
  cxx_pretty_printer *pp = xmalloc (sizeof (cxx_pretty_printer));
  memcpy (pp_base (pp), base, sizeof (pretty_printer));
  pp_cxx_pretty_printer_init (pp);
  context->printer = (pretty_printer *) pp;

  /* It is safe to free this object because it was previously malloc()'d.  */
  free (base);
}
