/* Language specific subroutines used for code generation on IBM S/390
   and zSeries
   Copyright (C) 2015-2018 Free Software Foundation, Inc.

   Contributed by Andreas Krebbel (Andreas.Krebbel@de.ibm.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.

   Based on gcc/config/rs6000/rs6000-c.c.

   In GCC terms this file belongs to the frontend.  It will be
   compiled with -DIN_GCC_FRONTEND.  With that rtl.h cannot be
   included anymore - a mechanism supposed to avoid adding frontend -
   backend dependencies.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "c-family/c-common.h"
#include "c/c-tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "c-family/c-pragma.h"
#include "langhooks.h"
#include "tree-pretty-print.h"

#include "s390-builtins.h"

static GTY(()) tree __vector_keyword;
static GTY(()) tree vector_keyword;
static GTY(()) tree __bool_keyword;
static GTY(()) tree bool_keyword;
static GTY(()) tree _Bool_keyword;


/* Generate an array holding all the descriptions of variants of
   overloaded builtins defined with OB_DEF_VAR in
   s390-builtins.def.  */
static enum s390_builtin_ov_type_index
type_for_overloaded_builtin_var[S390_OVERLOADED_BUILTIN_VAR_MAX + 1] =
  {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(...)
#define OB_DEF(...)
#define OB_DEF_VAR(NAME, PATTERN, FLAGS, OPFLAGS, FNTYPE) FNTYPE,
#include "s390-builtins.def"
    BT_OV_MAX
  };


/* Generate an array indexed by an overloaded builtin index returning
   the first index in desc_for_overloaded_builtin_var where the
   variants for the builtin can be found.  */
static enum s390_overloaded_builtin_vars
desc_start_for_overloaded_builtin[S390_OVERLOADED_BUILTIN_MAX + 1] =
  {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(...)
#define OB_DEF(NAME, FIRST_VAR_NAME,...)	\
    S390_OVERLOADED_BUILTIN_VAR_##FIRST_VAR_NAME,
#define OB_DEF_VAR(...)
    #include "s390-builtins.def"
    S390_OVERLOADED_BUILTIN_VAR_MAX
  };

/* Generate an array indexed by an overloaded builtin index returning
   the last index in desc_for_overloaded_builtin_var where the
   variants for the builtin can be found.  */
static enum s390_overloaded_builtin_vars
desc_end_for_overloaded_builtin[S390_OVERLOADED_BUILTIN_MAX + 1] =
  {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(...)
#define OB_DEF(NAME, FIRST_VAR_NAME, LAST_VAR_NAME,...)	\
    S390_OVERLOADED_BUILTIN_VAR_##LAST_VAR_NAME,
#define OB_DEF_VAR(...)
    #include "s390-builtins.def"
    S390_OVERLOADED_BUILTIN_VAR_MAX
  };

static enum s390_builtin_type_index
s390_builtin_ov_types[BT_OV_MAX][MAX_OV_OPERANDS] =
  {
#undef DEF_TYPE
#undef DEF_POINTER_TYPE
#undef DEF_DISTINCT_TYPE
#undef DEF_VECTOR_TYPE
#undef DEF_OPAQUE_VECTOR_TYPE
#undef DEF_FN_TYPE
#undef DEF_OV_TYPE
#define DEF_TYPE(...)
#define DEF_POINTER_TYPE(...)
#define DEF_DISTINCT_TYPE(...)
#define DEF_VECTOR_TYPE(...)
#define DEF_OPAQUE_VECTOR_TYPE(...)
#define DEF_FN_TYPE(...)
#define DEF_OV_TYPE(INDEX, args...) { args },
#include "s390-builtin-types.def"
  };

static const enum s390_builtins
bt_for_overloaded_builtin_var[S390_OVERLOADED_BUILTIN_VAR_MAX] = {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(...)
#define OB_DEF(...)
#define OB_DEF_VAR(NAME, BT, ...) S390_BUILTIN_##BT,

#include "s390-builtins.def"
  };

/* In addition to calling fold_convert for EXPR of type TYPE, also
   call c_fully_fold to remove any C_MAYBE_CONST_EXPRs that could be
   hiding there (PR47197).  */
tree
fully_fold_convert (tree type, tree expr)
{
  tree result = fold_convert (type, expr);
  bool maybe_const = true;

  if (!c_dialect_cxx ())
    result = c_fully_fold (result, false, &maybe_const);

  return result;
}

/* Unify the different variants to the same nodes in order to keep the
   code working with it simple.  */
static cpp_hashnode *
s390_categorize_keyword (const cpp_token *tok)
{
  if (tok->type == CPP_NAME)
    {
      cpp_hashnode *ident = tok->val.node.node;

      if (ident == C_CPP_HASHNODE (vector_keyword))
	return C_CPP_HASHNODE (__vector_keyword);

      if (ident == C_CPP_HASHNODE (bool_keyword))
	return C_CPP_HASHNODE (__bool_keyword);

      if (ident == C_CPP_HASHNODE (_Bool_keyword))
	return C_CPP_HASHNODE (__bool_keyword);
      return ident;
    }

  return 0;
}


/* Called to decide whether a conditional macro should be expanded.
   Since we have exactly one such macro (i.e, 'vector'), we do not
   need to examine the 'tok' parameter.  */

static cpp_hashnode *
s390_macro_to_expand (cpp_reader *pfile, const cpp_token *tok)
{
  cpp_hashnode *expand_this = tok->val.node.node;
  cpp_hashnode *ident;
  static bool expand_bool_p = false;
  int idx = 0;
  enum rid rid_code;

  /* The vector keyword is only expanded if the machine actually
     provides hardware support.  */
  if (!TARGET_ZVECTOR)
    return NULL;

  ident = s390_categorize_keyword (tok);

  /* Triggered when we picked a different variant in
     s390_categorize_keyword.  */
  if (ident != expand_this)
    expand_this = NULL;

  /* The vector keyword has been found already and we remembered to
     expand the next bool.  */
  if (expand_bool_p && ident == C_CPP_HASHNODE (__bool_keyword))
    {
      expand_bool_p = false;
      return ident;
    }

  if (ident != C_CPP_HASHNODE (__vector_keyword))
    return expand_this;

  do
    tok = cpp_peek_token (pfile, idx++);
  while (tok->type == CPP_PADDING);
  ident = s390_categorize_keyword (tok);

  if (!ident)
    return expand_this;

  /* vector bool - remember to expand the next bool. */
  if (ident == C_CPP_HASHNODE (__bool_keyword))
    {
      expand_bool_p = true;
      return C_CPP_HASHNODE (__vector_keyword);
    }

  /* The boost libraries have code with Iterator::vector vector in it.
     If we allow the normal handling, this module will be called
     recursively, and the vector will be skipped.; */
  if (ident == C_CPP_HASHNODE (__vector_keyword))
    return expand_this;

  rid_code = (enum rid)(ident->rid_code);

  if (cpp_macro_p (ident))
    {
      /* Now actually fetch the tokens we "peeked" before and do a
	 lookahead for the next.  */
      do
	(void) cpp_get_token (pfile);
      while (--idx > 0);
      do
	tok = cpp_peek_token (pfile, idx++);
      while (tok->type == CPP_PADDING);
      ident = s390_categorize_keyword (tok);

      if (ident == C_CPP_HASHNODE (__bool_keyword))
	{
	  expand_bool_p = true;
	  return C_CPP_HASHNODE (__vector_keyword);
	}
      else if (ident)
	rid_code = (enum rid)(ident->rid_code);
    }

  /* vector keyword followed by type identifier: vector unsigned,
     vector long, ...
     Types consisting of more than one identifier are not supported by
     zvector e.g. long long, long double, unsigned long int.  */
  if (rid_code == RID_UNSIGNED || rid_code == RID_LONG
      || rid_code == RID_SHORT || rid_code == RID_SIGNED
      || rid_code == RID_INT || rid_code == RID_CHAR
      || (rid_code == RID_FLOAT && TARGET_VXE)
      || rid_code == RID_DOUBLE)
    {
      expand_this = C_CPP_HASHNODE (__vector_keyword);
      /* If the next keyword is bool, it will need to be expanded as
	 well.  */
      do
	tok = cpp_peek_token (pfile, idx++);
      while (tok->type == CPP_PADDING);
      ident = s390_categorize_keyword (tok);

      /* __vector long __bool a; */
      if (ident == C_CPP_HASHNODE (__bool_keyword))
	expand_bool_p = true;
      else
	{
	  /* Triggered with: __vector long long __bool a; */
	  do
	    tok = cpp_peek_token (pfile, idx++);
	  while (tok->type == CPP_PADDING);
	  ident = s390_categorize_keyword (tok);

	  if (ident == C_CPP_HASHNODE (__bool_keyword))
	    expand_bool_p = true;
	}
    }

  return expand_this;
}

/* Helper function that defines or undefines macros.  If SET is true, the macro
   MACRO_DEF is defined.  If SET is false, the macro MACRO_UNDEF is undefined.
   Nothing is done if SET and WAS_SET have the same value.  */
static void
s390_def_or_undef_macro (cpp_reader *pfile,
			 unsigned int mask,
			 const struct cl_target_option *old_opts,
			 const struct cl_target_option *new_opts,
			 const char *macro_def, const char *macro_undef)
{
  bool was_set;
  bool set;

  was_set = (!old_opts) ? false : old_opts->x_target_flags & mask;
  set = new_opts->x_target_flags & mask;
  if (was_set == set)
    return;
  if (set)
    cpp_define (pfile, macro_def);
  else
    cpp_undef (pfile, macro_undef);
}

/* Internal function to either define or undef the appropriate system
   macros.  */
static void
s390_cpu_cpp_builtins_internal (cpp_reader *pfile,
				struct cl_target_option *opts,
				const struct cl_target_option *old_opts)
{
  s390_def_or_undef_macro (pfile, MASK_OPT_HTM, old_opts, opts,
			   "__HTM__", "__HTM__");
  s390_def_or_undef_macro (pfile, MASK_OPT_VX, old_opts, opts,
			   "__VX__", "__VX__");
  s390_def_or_undef_macro (pfile, MASK_ZVECTOR, old_opts, opts,
			   "__VEC__=10302", "__VEC__");
  s390_def_or_undef_macro (pfile, MASK_ZVECTOR, old_opts, opts,
			   "__vector=__attribute__((vector_size(16)))",
			   "__vector__");
  s390_def_or_undef_macro (pfile, MASK_ZVECTOR, old_opts, opts,
			   "__bool=__attribute__((s390_vector_bool)) unsigned",
			   "__bool");
  {
    char macro_def[64];
    gcc_assert (s390_arch != PROCESSOR_NATIVE);
    sprintf (macro_def, "__ARCH__=%d", processor_table[s390_arch].arch_level);
    cpp_undef (pfile, "__ARCH__");
    cpp_define (pfile, macro_def);
  }

  if (!flag_iso)
    {
      s390_def_or_undef_macro (pfile, MASK_ZVECTOR, old_opts, opts,
			       "__VECTOR_KEYWORD_SUPPORTED__",
			       "__VECTOR_KEYWORD_SUPPORTED__");
      s390_def_or_undef_macro (pfile, MASK_ZVECTOR, old_opts, opts,
			       "vector=vector", "vector");
      s390_def_or_undef_macro (pfile, MASK_ZVECTOR, old_opts, opts,
			       "bool=bool", "bool");
      if (TARGET_ZVECTOR_P (opts->x_target_flags) && __vector_keyword == NULL)
	{
	  __vector_keyword = get_identifier ("__vector");
	  C_CPP_HASHNODE (__vector_keyword)->flags |= NODE_CONDITIONAL;

	  vector_keyword = get_identifier ("vector");
	  C_CPP_HASHNODE (vector_keyword)->flags |= NODE_CONDITIONAL;

	  __bool_keyword = get_identifier ("__bool");
	  C_CPP_HASHNODE (__bool_keyword)->flags |= NODE_CONDITIONAL;

	  bool_keyword = get_identifier ("bool");
	  C_CPP_HASHNODE (bool_keyword)->flags |= NODE_CONDITIONAL;

	  _Bool_keyword = get_identifier ("_Bool");
	  C_CPP_HASHNODE (_Bool_keyword)->flags |= NODE_CONDITIONAL;

	  /* Enable context-sensitive macros.  */
	  cpp_get_callbacks (pfile)->macro_to_expand = s390_macro_to_expand;
	}
    }
}

/* Define platform dependent macros.  */
void
s390_cpu_cpp_builtins (cpp_reader *pfile)
{
  struct cl_target_option opts;

  cpp_assert (pfile, "cpu=s390");
  cpp_assert (pfile, "machine=s390");
  cpp_define (pfile, "__s390__");
  if (TARGET_ZARCH)
    cpp_define (pfile, "__zarch__");
  if (TARGET_64BIT)
    cpp_define (pfile, "__s390x__");
  if (TARGET_LONG_DOUBLE_128)
    cpp_define (pfile, "__LONG_DOUBLE_128__");
  cl_target_option_save (&opts, &global_options);
  s390_cpu_cpp_builtins_internal (pfile, &opts, NULL);
}

#if S390_USE_TARGET_ATTRIBUTE
/* Hook to validate the current #pragma GCC target and set the state, and
   update the macros based on what was changed.  If ARGS is NULL, then
   POP_TARGET is used to reset the options.  */

static bool
s390_pragma_target_parse (tree args, tree pop_target)
{
  tree prev_tree = build_target_option_node (&global_options);
  tree cur_tree;

  if (! args)
    cur_tree = pop_target;
  else
    {
      cur_tree = s390_valid_target_attribute_tree (args, &global_options,
						   &global_options_set, true);
      if (!cur_tree || cur_tree == error_mark_node)
	{
	  cl_target_option_restore (&global_options,
				    TREE_TARGET_OPTION (prev_tree));
	  return false;
	}
    }

  target_option_current_node = cur_tree;
  s390_activate_target_options (target_option_current_node);

  {
    struct cl_target_option *prev_opt;
    struct cl_target_option *cur_opt;

    /* Figure out the previous/current differences.  */
    prev_opt = TREE_TARGET_OPTION (prev_tree);
    cur_opt = TREE_TARGET_OPTION (cur_tree);

    /* For the definitions, ensure all newly defined macros are considered
       as used for -Wunused-macros.  There is no point warning about the
       compiler predefined macros.  */
    cpp_options *cpp_opts = cpp_get_options (parse_in);
    unsigned char saved_warn_unused_macros = cpp_opts->warn_unused_macros;

    cpp_opts->warn_unused_macros = 0;

    /* Define all of the macros for new options that were just turned on.  */
    s390_cpu_cpp_builtins_internal (parse_in, cur_opt, prev_opt);

    cpp_opts->warn_unused_macros = saved_warn_unused_macros;
  }

  return true;
}
#endif

/* Expand builtins which can directly be mapped to tree expressions.
   LOC - location information
   FCODE - function code of the builtin
   ARGLIST - value supposed to be passed as arguments
   RETURN-TYPE - expected return type of the builtin */
static tree
s390_expand_overloaded_builtin (location_t loc,
				unsigned fcode,
				vec<tree, va_gc> *arglist,
				tree return_type)
{
  switch (fcode)
    {
    case S390_OVERLOADED_BUILTIN_s390_vec_step:
      if (TREE_CODE (TREE_TYPE ((*arglist)[0])) != VECTOR_TYPE)
	{
	  error_at (loc, "builtin vec_step can only be used on vector types.");
	  return error_mark_node;
	}
      return build_int_cst (NULL_TREE,
			    TYPE_VECTOR_SUBPARTS (TREE_TYPE ((*arglist)[0])));
    case S390_OVERLOADED_BUILTIN_s390_vec_xl:
    case S390_OVERLOADED_BUILTIN_s390_vec_xld2:
    case S390_OVERLOADED_BUILTIN_s390_vec_xlw4:
      return build2 (MEM_REF, return_type,
		     fold_build_pointer_plus ((*arglist)[1], (*arglist)[0]),
		     build_int_cst (TREE_TYPE ((*arglist)[1]), 0));
    case S390_OVERLOADED_BUILTIN_s390_vec_xst:
    case S390_OVERLOADED_BUILTIN_s390_vec_xstd2:
    case S390_OVERLOADED_BUILTIN_s390_vec_xstw4:
      return build2 (MODIFY_EXPR, TREE_TYPE((*arglist)[0]),
		     build1 (INDIRECT_REF, TREE_TYPE((*arglist)[0]),
			     fold_build_pointer_plus ((*arglist)[2], (*arglist)[1])),
		     (*arglist)[0]);
    case S390_OVERLOADED_BUILTIN_s390_vec_load_pair:
      return build_constructor_va (return_type, 2,
				   NULL_TREE, (*arglist)[0],
				   NULL_TREE, (*arglist)[1]);
    default:
      gcc_unreachable ();
    }
}

/* invert result */
#define __VSTRING_FLAG_IN         8
/* result type */
#define __VSTRING_FLAG_RT         4
/* zero search */
#define __VSTRING_FLAG_ZS         2
/* set condition code */
#define __VSTRING_FLAG_CS         1

/* Return the flags value to be used for string low-level builtins
   when expanded from overloaded builtin OB_FCODE.  */
static unsigned int
s390_get_vstring_flags (int ob_fcode)
{
  unsigned int flags = 0;

  switch (ob_fcode)
    {
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_idx:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_or_0_idx:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_or_0_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_idx:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_or_0_idx:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_or_0_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_cc:
      flags |= __VSTRING_FLAG_IN;
      break;
    default:
      break;
    }
  switch (ob_fcode)
    {

    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmprg:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_cc:
      flags |= __VSTRING_FLAG_RT;
      break;
    default:
      break;
    }
  switch (ob_fcode)
    {

    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_or_0_idx:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_or_0_idx:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_or_0_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_or_0_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_or_0_idx:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_or_0_idx:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_or_0_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_or_0_idx_cc:
      flags |= __VSTRING_FLAG_ZS;
      break;
    default:
      break;
    }
  switch (ob_fcode)
    {
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_or_0_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_or_0_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_or_0_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_or_0_idx_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_cc:
    case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_cc:
      flags |= __VSTRING_FLAG_CS;
      break;
    default:
      break;
    }
  return flags;
}
#undef __VSTRING_FLAG_IN
#undef __VSTRING_FLAG_RT
#undef __VSTRING_FLAG_ZS
#undef __VSTRING_FLAG_CS

/* For several overloaded builtins the argument lists do not match
   exactly the signature of a low-level builtin.  This function
   adjusts the argument list ARGLIST for the overloaded builtin
   OB_FCODE to the signature of the low-level builtin given by
   DECL.  */
static void
s390_adjust_builtin_arglist (unsigned int ob_fcode, tree decl,
			     vec<tree, va_gc> **arglist)
{
  tree arg_chain;
  int src_arg_index, dest_arg_index;
  vec<tree, va_gc> *folded_args = NULL;

  /* We at most add one more operand to the list.  */
  vec_alloc (folded_args, (*arglist)->allocated () + 1);
  for (arg_chain = TYPE_ARG_TYPES (TREE_TYPE (decl)),
	 src_arg_index = 0, dest_arg_index = 0;
       !VOID_TYPE_P (TREE_VALUE (arg_chain));
       arg_chain = TREE_CHAIN (arg_chain), dest_arg_index++)
    {
      bool arg_assigned_p = false;
      switch (ob_fcode)
	{
	  /* For all these the low level builtin needs an additional flags parameter.  */
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_idx:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_idx:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_or_0_idx:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_or_0_idx:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_idx_cc:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_idx_cc:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_or_0_idx_cc:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_or_0_idx_cc:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_eq_cc:
	case S390_OVERLOADED_BUILTIN_s390_vec_find_any_ne_cc:
	  if (dest_arg_index == 2)
	    {
	      folded_args->quick_push (build_int_cst (integer_type_node,
				       s390_get_vstring_flags (ob_fcode)));
	      arg_assigned_p = true;
	    }
	  break;
	case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_idx:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_idx:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_or_0_idx:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_or_0_idx:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmprg:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_idx_cc:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_idx_cc:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_or_0_idx_cc:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_or_0_idx_cc:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmprg_cc:
	case S390_OVERLOADED_BUILTIN_s390_vec_cmpnrg_cc:
	  if (dest_arg_index == 3)
	    {
	      folded_args->quick_push (build_int_cst (integer_type_node,
				       s390_get_vstring_flags (ob_fcode)));
	      arg_assigned_p = true;
	    }
	  break;
	case S390_OVERLOADED_BUILTIN_s390_vec_sel:
	case S390_OVERLOADED_BUILTIN_s390_vec_insert:
	case S390_OVERLOADED_BUILTIN_s390_vec_load_len:
	  /* Swap the first to arguments. It is better to do it here
	     instead of the header file to avoid operand checking
	     throwing error messages for a weird operand index.  */
	  if (dest_arg_index < 2)
	    {
	      folded_args->quick_push (fully_fold_convert (TREE_VALUE (arg_chain),
					 (**arglist)[1 - dest_arg_index]));
	      src_arg_index++;
	      arg_assigned_p = true;
	    }
	  break;
	case S390_OVERLOADED_BUILTIN_s390_vec_store_len:
	  if (dest_arg_index == 1 || dest_arg_index == 2)
	    {
	      folded_args->quick_push (fully_fold_convert (TREE_VALUE (arg_chain),
					 (**arglist)[3 - dest_arg_index]));
	      src_arg_index++;
	      arg_assigned_p = true;
	    }
	  break;

	case S390_OVERLOADED_BUILTIN_s390_vec_load_bndry:
	  {
	    int code;
	    if (dest_arg_index == 1)
	      {
		tree arg = (**arglist)[src_arg_index];

		if (TREE_CODE (arg) != INTEGER_CST)
		  {
		    error ("constant value required for builtin %qF argument %d",
			   decl, src_arg_index + 1);
		    return;
		  }

		switch (tree_to_uhwi (arg))
		  {
		  case 64: code = 0; break;
		  case 128: code = 1; break;
		  case 256: code = 2; break;
		  case 512: code = 3; break;
		  case 1024: code = 4; break;
		  case 2048: code = 5; break;
		  case 4096: code = 6; break;
		  default:
		    error ("valid values for builtin %qF argument %d are 64, "
			   "128, 256, 512, 1024, 2048, and 4096", decl,
			   src_arg_index + 1);
		    return;
		  }
		folded_args->quick_push (build_int_cst (integer_type_node,
							code));
		src_arg_index++;
		arg_assigned_p = true;
	      }
	  }
	  break;
	case S390_OVERLOADED_BUILTIN_s390_vec_rl_mask:
	  /* Duplicate the first src arg.  */
	  if (dest_arg_index == 0)
	    {
	      folded_args->quick_push (fully_fold_convert (TREE_VALUE (arg_chain),
					   (**arglist)[src_arg_index]));
	      arg_assigned_p = true;
	    }
	  break;
	default:
	  break;
	}
      if (!arg_assigned_p)
	{
	  folded_args->quick_push (fully_fold_convert (TREE_VALUE (arg_chain),
						 (**arglist)[src_arg_index]));
	  src_arg_index++;
	}
    }
  *arglist = folded_args;
}

/* Check whether the arguments in ARGLIST match the function type
   DEF_TYPE. Return the number of argument types which required
   conversion/promotion in order to make it match.
   0 stands for a perfect match - all operand types match without changes
   INT_MAX stands for a mismatch.  */
static int
s390_fn_types_compatible (enum s390_builtin_ov_type_index typeindex,
			  vec<tree, va_gc> *arglist)
{
  unsigned int i;
  int match_type = 0;

  for (i = 0; i < vec_safe_length (arglist); i++)
    {
      tree b_arg_type = s390_builtin_types[s390_builtin_ov_types[typeindex][i + 1]];
      tree in_arg = (*arglist)[i];
      tree in_type = TREE_TYPE (in_arg);

      if (TREE_CODE (b_arg_type) == VECTOR_TYPE)
	{
	  /* Vector types have to match precisely.  */
	  if (b_arg_type != in_type
	      && TYPE_MAIN_VARIANT (b_arg_type) != TYPE_MAIN_VARIANT (in_type))
	    goto mismatch;
	}

      if (lang_hooks.types_compatible_p (in_type, b_arg_type))
	continue;

      if (lang_hooks.types_compatible_p (
	    lang_hooks.types.type_promotes_to (in_type),
	    lang_hooks.types.type_promotes_to (b_arg_type)))
	{
	  match_type++;
	  continue;
	}

      /* In this stage the C++ frontend would go ahead trying to find
	 implicit conversion chains for the argument to match the
	 target type.  We will mimic this here only for our limited
	 subset of argument types.  */
      if (TREE_CODE (b_arg_type) == INTEGER_TYPE
	  && TREE_CODE (in_type) == INTEGER_TYPE)
	{
	  match_type++;
	  continue;
	}

      /* If the incoming pointer argument has more qualifiers than the
	 argument type it can still be an imperfect match.  */
      if (POINTER_TYPE_P (b_arg_type) && POINTER_TYPE_P (in_type)
	  && !(TYPE_QUALS (TREE_TYPE (in_type))
	       & ~TYPE_QUALS (TREE_TYPE (b_arg_type)))
	  && (TYPE_QUALS (TREE_TYPE (b_arg_type))
	      & ~TYPE_QUALS (TREE_TYPE (in_type))))
	{
	  tree qual_in_type =
	    build_qualified_type (TREE_TYPE (in_type),
				  TYPE_QUALS (TREE_TYPE (b_arg_type)));

	  if (lang_hooks.types_compatible_p (qual_in_type,
					     TREE_TYPE (b_arg_type)))
	    {
	      match_type++;
	      continue;
	    }
	}

    mismatch:
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, " mismatch in operand: %d\n", i + 1);
      return INT_MAX;
    }

  return match_type;
}

/* Return the number of elements in the vector arguments of FNDECL in
   case all it matches for all vector arguments, -1 otherwise.  */
static int
s390_vec_n_elem (tree fndecl)
{
  tree b_arg_chain;
  int n_elem = -1;

  if (TREE_CODE (TREE_TYPE (TREE_TYPE (fndecl))) == VECTOR_TYPE)
    n_elem = TYPE_VECTOR_SUBPARTS (TREE_TYPE (TREE_TYPE ((fndecl))));

  for (b_arg_chain = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
       !VOID_TYPE_P (TREE_VALUE (b_arg_chain));
       b_arg_chain = TREE_CHAIN (b_arg_chain))
    {
      int tmp_n_elem;
      if (TREE_CODE (TREE_VALUE (b_arg_chain)) != VECTOR_TYPE)
	continue;
      tmp_n_elem = TYPE_VECTOR_SUBPARTS (TREE_VALUE (b_arg_chain));
      if (n_elem != -1 && n_elem != tmp_n_elem)
	return -1;
      n_elem = tmp_n_elem;
    }
  return n_elem;
}


/* Return a tree expression for a call to the overloaded builtin
   function OB_FNDECL at LOC with arguments PASSED_ARGLIST.  */
tree
s390_resolve_overloaded_builtin (location_t loc,
				 tree ob_fndecl,
				 void *passed_arglist)
{
  vec<tree, va_gc> *arglist = static_cast<vec<tree, va_gc> *> (passed_arglist);
  unsigned int in_args_num = vec_safe_length (arglist);
  unsigned int ob_args_num = 0;
  unsigned int ob_fcode = DECL_FUNCTION_CODE (ob_fndecl);
  enum s390_overloaded_builtin_vars bindex;
  unsigned int i;
  int last_match_type = INT_MAX;
  int last_match_index = -1;
  unsigned int all_op_flags;
  const unsigned int ob_flags = bflags_for_builtin(ob_fcode);
  int num_matches = 0;
  tree target_builtin_decl, b_arg_chain, return_type;
  enum s390_builtin_ov_type_index last_match_fntype_index = BT_OV_MAX;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
      "s390_resolve_overloaded_builtin, code = %4d, %s - %s overloaded\n",
      (int)ob_fcode, IDENTIFIER_POINTER (DECL_NAME (ob_fndecl)),
     ob_fcode < S390_BUILTIN_MAX ? "not" : "");

  /* 0...S390_BUILTIN_MAX-1 is for non-overloaded builtins.  */
  if (ob_fcode < S390_BUILTIN_MAX)
    {
      if (ob_flags & B_INT)
	{
	  error_at (loc,
		    "builtin %qF is for GCC internal use only.",
		    ob_fndecl);
	  return error_mark_node;
	}
      return NULL_TREE;
    }

  if (ob_flags & B_DEP)
    warning_at (loc, 0, "builtin %qF is deprecated.", ob_fndecl);

  if (!TARGET_VX && (ob_flags & B_VX))
    {
      error_at (loc, "%qF requires -mvx", ob_fndecl);
      return error_mark_node;
    }

  if (!TARGET_VXE && (ob_flags & B_VXE))
    {
      error_at (loc, "%qF requires z14 or higher", ob_fndecl);
      return error_mark_node;
    }

  ob_fcode -= S390_BUILTIN_MAX;

  for (b_arg_chain = TYPE_ARG_TYPES (TREE_TYPE (ob_fndecl));
       !VOID_TYPE_P (TREE_VALUE (b_arg_chain));
       b_arg_chain = TREE_CHAIN (b_arg_chain))
    ob_args_num++;

  if (ob_args_num != in_args_num)
    {
      error_at (loc,
		"mismatch in number of arguments for builtin %qF. "
		"Expected: %d got %d", ob_fndecl,
		ob_args_num, in_args_num);
      return error_mark_node;
    }

  for (i = 0; i < in_args_num; i++)
    if ((*arglist)[i] == error_mark_node)
      return error_mark_node;

  /* Overloaded builtins without any variants are directly expanded here.  */
  if (desc_start_for_overloaded_builtin[ob_fcode] ==
      S390_OVERLOADED_BUILTIN_VAR_MAX)
    return s390_expand_overloaded_builtin (loc, ob_fcode, arglist, NULL_TREE);

  for (bindex = desc_start_for_overloaded_builtin[ob_fcode];
       bindex <= desc_end_for_overloaded_builtin[ob_fcode];
       bindex = (enum s390_overloaded_builtin_vars)((int)bindex + 1))
  {
    int match_type;
    enum s390_builtin_ov_type_index type_index =
      type_for_overloaded_builtin_var[bindex];

    if (TARGET_DEBUG_ARG)
      fprintf (stderr, "checking variant number: %d", (int)bindex);

    match_type = s390_fn_types_compatible (type_index, arglist);

    if (match_type == INT_MAX)
      continue;

    if (TARGET_DEBUG_ARG)
      fprintf (stderr,
	       " %s match score: %d\n", match_type == 0 ? "perfect" : "imperfect",
	       match_type);

    if (match_type < last_match_type)
      {
	num_matches = 1;
	last_match_type = match_type;
	last_match_fntype_index = type_index;
	last_match_index = bindex;
      }
    else if (match_type == last_match_type)
      num_matches++;
  }

  if (last_match_type == INT_MAX)
    {
      error_at (loc, "invalid parameter combination for intrinsic %qs",
		IDENTIFIER_POINTER (DECL_NAME (ob_fndecl)));
      return error_mark_node;
    }
  else if (num_matches > 1)
    {
      error_at (loc, "ambiguous overload for intrinsic %qs",
		IDENTIFIER_POINTER (DECL_NAME (ob_fndecl)));
      return error_mark_node;
    }

  if (!TARGET_VXE
      && bflags_overloaded_builtin_var[last_match_index] & B_VXE)
    {
      error_at (loc, "%qs matching variant requires z14 or higher",
		IDENTIFIER_POINTER (DECL_NAME (ob_fndecl)));
      return error_mark_node;
    }

  if (bflags_overloaded_builtin_var[last_match_index] & B_DEP)
    warning_at (loc, 0, "%qs matching variant is deprecated.",
		IDENTIFIER_POINTER (DECL_NAME (ob_fndecl)));

  /* Overloaded variants which have MAX set as low level builtin are
     supposed to be replaced during expansion with something else.  */
  if (bt_for_overloaded_builtin_var[last_match_index] == S390_BUILTIN_MAX)
    target_builtin_decl = ob_fndecl;
  else
    target_builtin_decl = s390_builtin_decls[bt_for_overloaded_builtin_var[last_match_index]];

  all_op_flags = opflags_overloaded_builtin_var[last_match_index];
  return_type = s390_builtin_types[s390_builtin_ov_types[last_match_fntype_index][0]];

  /* Check for the operand flags in the overloaded builtin variant.  */
  for (i = 0; i < ob_args_num; i++)
    {
      unsigned int op_flags = all_op_flags & ((1 << O_SHIFT) - 1);
      tree arg = (*arglist)[i];
      tree type = s390_builtin_types[s390_builtin_ov_types[last_match_fntype_index][i + 1]];

      all_op_flags = all_op_flags >> O_SHIFT;

      if (op_flags == O_ELEM)
	{
	  int n_elem = s390_vec_n_elem (target_builtin_decl);
	  gcc_assert (n_elem > 0);
	  gcc_assert (type == integer_type_node);
	  (*arglist)[i] = build2 (BIT_AND_EXPR, integer_type_node,
				  fold_convert (integer_type_node, arg),
				  build_int_cst (NULL_TREE, n_elem - 1));
	}

      if (TREE_CODE (arg) != INTEGER_CST || !O_IMM_P (op_flags))
	continue;

      if ((TYPE_UNSIGNED (type)
	   && !int_fits_type_p (arg, c_common_unsigned_type (type)))
	  || (!TYPE_UNSIGNED (type)
	      && !int_fits_type_p (arg, c_common_signed_type (type))))
	{
	  error("constant argument %d for builtin %qF is out "
		"of range for target type",
		i + 1, target_builtin_decl);
	  return error_mark_node;
	}

      if (TREE_CODE (arg) == INTEGER_CST
	  && !s390_const_operand_ok (arg, i + 1, op_flags, target_builtin_decl))
	return error_mark_node;
    }

  /* Handle builtins we expand directly - without mapping it to a low
     level builtin.  */
  if (bt_for_overloaded_builtin_var[last_match_index] == S390_BUILTIN_MAX)
    return s390_expand_overloaded_builtin (loc, ob_fcode, arglist, return_type);

  s390_adjust_builtin_arglist (ob_fcode, target_builtin_decl, &arglist);

  if (VOID_TYPE_P (return_type))
    return build_function_call_vec (loc, vNULL, target_builtin_decl,
				    arglist, NULL);
  else
    return fully_fold_convert (return_type,
			       build_function_call_vec (loc, vNULL, target_builtin_decl,
							arglist, NULL));
}

/* This is used to define the REGISTER_TARGET_PRAGMAS macro in s390.h.  */
void
s390_register_target_pragmas (void)
{
  targetm.resolve_overloaded_builtin = s390_resolve_overloaded_builtin;
#if S390_USE_TARGET_ATTRIBUTE
  /* Update pragma hook to allow parsing #pragma GCC target.  */
  targetm.target_option.pragma_parse = s390_pragma_target_parse;
#endif
}
