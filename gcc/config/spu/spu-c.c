/* Copyright (C) 2006-2018 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "c-family/c-common.h"
#include "stringpool.h"
#include "langhooks.h"


/* Keep the vector keywords handy for fast comparisons.  */
static GTY(()) tree __vector_keyword;
static GTY(()) tree vector_keyword;

static cpp_hashnode *
spu_categorize_keyword (const cpp_token *tok)
{
  if (tok->type == CPP_NAME)
    {
      cpp_hashnode *ident = tok->val.node.node;

      if (ident == C_CPP_HASHNODE (vector_keyword)
	  || ident == C_CPP_HASHNODE (__vector_keyword))
	return C_CPP_HASHNODE (__vector_keyword);
      else
	return ident;
    }
  return 0;
}

/* Called to decide whether a conditional macro should be expanded.
   Since we have exactly one such macro (i.e, 'vector'), we do not
   need to examine the 'tok' parameter.  */

static cpp_hashnode *
spu_macro_to_expand (cpp_reader *pfile, const cpp_token *tok)
{
  cpp_hashnode *expand_this = tok->val.node.node;
  cpp_hashnode *ident;

  ident = spu_categorize_keyword (tok);
  if (ident == C_CPP_HASHNODE (__vector_keyword))
    {
      tok = cpp_peek_token (pfile, 0);
      ident = spu_categorize_keyword (tok);

      if (ident)
	{
	  enum rid rid_code = (enum rid)(ident->rid_code);
	  if (cpp_macro_p (ident))
	    {
	      (void) cpp_get_token (pfile);
	      tok = cpp_peek_token (pfile, 0);
	      ident = spu_categorize_keyword (tok);
	      if (ident)
		rid_code = (enum rid)(ident->rid_code);
	    }
	  
	  if (rid_code == RID_UNSIGNED || rid_code == RID_LONG
	      || rid_code == RID_SHORT || rid_code == RID_SIGNED
	      || rid_code == RID_INT || rid_code == RID_CHAR
	      || rid_code == RID_FLOAT || rid_code == RID_DOUBLE)
	    expand_this = C_CPP_HASHNODE (__vector_keyword);
	}
    }
  return expand_this;
}

/* target hook for resolve_overloaded_builtin(). Returns a function call
   RTX if we can resolve the overloaded builtin */
tree
spu_resolve_overloaded_builtin (location_t loc, tree fndecl, void *passed_args)
{
#define SCALAR_TYPE_P(t) (INTEGRAL_TYPE_P (t) \
			  || SCALAR_FLOAT_TYPE_P (t) \
			  || POINTER_TYPE_P (t))
  vec<tree, va_gc> *fnargs = static_cast <vec<tree, va_gc> *> (passed_args);
  unsigned int nargs = vec_safe_length (fnargs);
  int new_fcode, fcode = DECL_FUNCTION_CODE (fndecl);
  struct spu_builtin_description *desc;
  tree match = NULL_TREE;

  /* The vector types are not available if the backend is not initialized.  */
  gcc_assert (!flag_preprocess_only);

  desc = &spu_builtins[fcode];
  if (desc->type != B_OVERLOAD)
    return NULL_TREE;

  /* Compare the signature of each internal builtin function with the
     function arguments until a match is found. */

  for (new_fcode = fcode + 1; spu_builtins[new_fcode].type == B_INTERNAL;
       new_fcode++)
    {
      tree decl = targetm.builtin_decl (new_fcode, true);
      tree params = TYPE_ARG_TYPES (TREE_TYPE (decl));
      tree param;
      bool all_scalar;
      unsigned int p;

      /* Check whether all parameters are scalar.  */
      all_scalar = true;
      for (param = params; param != void_list_node; param = TREE_CHAIN (param))
      if (!SCALAR_TYPE_P (TREE_VALUE (param)))
	all_scalar = false;

      for (param = params, p = 0;
	   param != void_list_node;
	   param = TREE_CHAIN (param), p++)
	{
	  tree var, arg_type, param_type = TREE_VALUE (param);

	  if (p >= nargs)
	    {
	      error ("insufficient arguments to overloaded function %s",
		     desc->name);
	      return error_mark_node;
	    }

	  var = (*fnargs)[p];

	  if (TREE_CODE (var) == NON_LVALUE_EXPR)
	    var = TREE_OPERAND (var, 0);

	  if (TREE_CODE (var) == ERROR_MARK)
	    return NULL_TREE;	/* Let somebody else deal with the problem. */

	  arg_type = TREE_TYPE (var);

	  /* The intrinsics spec does not specify precisely how to
	     resolve generic intrinsics.  We require an exact match
	     for vector types and let C do it's usual parameter type
	     checking/promotions for scalar arguments, except for the
	     first argument of intrinsics which don't have a vector
	     parameter. */
	  if ((!SCALAR_TYPE_P (param_type)
	       || !SCALAR_TYPE_P (arg_type)
	       || (all_scalar && p == 0))
	      && !lang_hooks.types_compatible_p (param_type, arg_type))
	    break;
	}
      if (param == void_list_node)
	{
	  if (p != nargs)
	    {
	      error ("too many arguments to overloaded function %s",
		     desc->name);
	      return error_mark_node;
	    }

	  match = decl;
	  break;
	}
    }

  if (match == NULL_TREE)
    {
      error ("parameter list does not match a valid signature for %s()",
	     desc->name);
      return error_mark_node;
    }

  return build_function_call_vec (loc, vNULL, match, fnargs, NULL);
#undef SCALAR_TYPE_P
}


void
spu_cpu_cpp_builtins (struct cpp_reader *pfile)
{
  cpp_define (pfile, "__SPU__");
  cpp_assert (pfile, "cpu=spu");
  cpp_assert (pfile, "machine=spu");
  if (spu_arch == PROCESSOR_CELLEDP)
    cpp_define (pfile, "__SPU_EDP__");
  if (cpp_get_options (pfile)->lang != CLK_ASM)
    cpp_define (pfile, "__vector=__attribute__((__spu_vector__))");
  switch (spu_ea_model)
    {
    case 32:
      cpp_define (pfile, "__EA32__");
      break;
    case 64:
      cpp_define (pfile, "__EA64__");
      break;
    default:
       gcc_unreachable ();
    }

  if (!flag_iso && cpp_get_options (pfile)->lang != CLK_ASM)
    {
      /* Define this when supporting context-sensitive keywords.  */
      cpp_define (pfile, "__VECTOR_KEYWORD_SUPPORTED__");
      cpp_define (pfile, "vector=vector");

      /* Initialize vector keywords.  */
      __vector_keyword = get_identifier ("__vector");
      C_CPP_HASHNODE (__vector_keyword)->flags |= NODE_CONDITIONAL;
      vector_keyword = get_identifier ("vector");
      C_CPP_HASHNODE (vector_keyword)->flags |= NODE_CONDITIONAL;

      /* Enable context-sensitive macros.  */
      cpp_get_callbacks (pfile)->macro_to_expand = spu_macro_to_expand;
    }
}

void
spu_c_common_override_options (void)
{ 
  if (!TARGET_STD_MAIN)
    {
      /* Don't give warnings about the main() function.  */
      warn_main = 0;
    }
}
