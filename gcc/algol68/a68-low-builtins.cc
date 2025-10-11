/* GCC built-ins support for Algol 68.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

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
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"

/* Define a built-in function.  */

static void
local_define_builtin (const char *name, tree type, enum built_in_function code,
                      const char *library_name, int ecf_flags)
{
  tree decl;

  decl = add_builtin_function (name, type, code, BUILT_IN_NORMAL,
			       library_name, NULL_TREE);
  set_call_expr_flags (decl, ecf_flags);
  set_builtin_decl (code, decl, true);
}

/* Install the GCC built-ins so the front-end can use them.  */

void
a68_install_builtins (void)
{
  if (!builtin_decl_explicit_p (BUILT_IN_IROUNDF))
    {
      tree ftype = build_function_type_list (integer_type_node,
					     float_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_iroundf", ftype, BUILT_IN_IROUNDF,
			    "iroundf", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LROUNDF))
    {
      tree ftype = build_function_type_list (long_integer_type_node,
					     float_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_lroundf", ftype, BUILT_IN_LROUNDF,
			    "lroundf", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LLROUNDF))
    {
      tree ftype = build_function_type_list (long_long_integer_type_node,
					     float_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_llroundf", ftype, BUILT_IN_LLROUNDF,
			    "llroundf", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_IROUND))
    {
      tree ftype = build_function_type_list (integer_type_node,
					     double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_iround", ftype, BUILT_IN_IROUND,
			    "iround", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LROUND))
    {
      tree ftype = build_function_type_list (long_integer_type_node,
					     double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_lround", ftype, BUILT_IN_LROUND,
			    "lround", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LLROUND))
    {
      tree ftype = build_function_type_list (long_long_integer_type_node,
					     double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_llround", ftype, BUILT_IN_LLROUND,
			    "llround", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_IROUNDL))
    {
      tree ftype = build_function_type_list (integer_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_iroundl", ftype, BUILT_IN_IROUNDL,
			    "iroundl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LROUNDL))
    {
      tree ftype = build_function_type_list (long_integer_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_lroundl", ftype, BUILT_IN_LROUNDL,
			    "lroundl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LLROUNDL))
    {
      tree ftype = build_function_type_list (long_long_integer_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_llroundl", ftype, BUILT_IN_LLROUNDL,
			    "llroundl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_IFLOORF))
    {
      tree ftype = build_function_type_list (integer_type_node,
					     float_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_ifloorf", ftype, BUILT_IN_IFLOORF,
			    "ifloorf", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LFLOORF))
    {
      tree ftype = build_function_type_list (long_integer_type_node,
					     float_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_lfloorf", ftype, BUILT_IN_LFLOORF,
			    "lfloorf", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LLFLOORF))
    {
      tree ftype = build_function_type_list (long_long_integer_type_node,
					     float_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_llfloorf", ftype, BUILT_IN_LLFLOORF,
			    "llfloorf", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_IFLOOR))
    {
      tree ftype = build_function_type_list (integer_type_node,
					     double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_ifloor", ftype, BUILT_IN_IFLOOR,
			    "ifloor", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LFLOOR))
    {
      tree ftype = build_function_type_list (long_integer_type_node,
					     double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_lfloor", ftype, BUILT_IN_LFLOOR,
			    "lfloor", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LLFLOOR))
    {
      tree ftype = build_function_type_list (long_long_integer_type_node,
					     double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_llfloor", ftype, BUILT_IN_LLFLOOR,
			    "llfloor", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_IFLOORL))
    {
      tree ftype = build_function_type_list (integer_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_ifloorl", ftype, BUILT_IN_IFLOORL,
			    "ifloorl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LFLOORL))
    {
      tree ftype = build_function_type_list (long_integer_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_lfloorl", ftype, BUILT_IN_LFLOORL,
			    "lfloorl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LLFLOORL))
    {
      tree ftype = build_function_type_list (long_long_integer_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_llfloorl", ftype, BUILT_IN_LLFLOORL,
			    "llfloorl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_POWF))
    {
      tree ftype = build_function_type_list (float_type_node,
					     float_type_node, float_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_powf", ftype, BUILT_IN_POWF,
			    "powf", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_POWIF))
    {
      tree ftype = build_function_type_list (float_type_node,
					     float_type_node, integer_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_powif", ftype, BUILT_IN_POWIF,
			    "powif", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_POW))
    {
      tree ftype = build_function_type_list (double_type_node,
					     double_type_node, double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_pow", ftype, BUILT_IN_POW,
			    "pow", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_POWI))
    {
      tree ftype = build_function_type_list (double_type_node,
					     double_type_node, integer_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_powi", ftype, BUILT_IN_POWI,
			    "powi", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_POWL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node, long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_powl", ftype, BUILT_IN_POWL,
			    "powl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_POWIL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node, integer_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_powil", ftype, BUILT_IN_POWIL,
			    "powil", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_CALLOC))
    {
      tree ftype = build_function_type_list (ptr_type_node,
					     size_type_node, size_type_node, NULL_TREE);
      local_define_builtin ("__builtin_calloc", ftype, BUILT_IN_CALLOC,
			    "calloc", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_MEMCPY))
    {
      tree ftype = build_function_type_list (ptr_type_node,
					     ptr_type_node, const_ptr_type_node, size_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_memcpy", ftype, BUILT_IN_MEMSET,
			    "memcpy", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_MEMSET))
    {
      tree ftype = build_function_type_list (ptr_type_node,
					     ptr_type_node, integer_type_node, size_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_memset", ftype, BUILT_IN_MEMSET,
			    "memset", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_SQRTF))
    {
      tree ftype = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
      local_define_builtin ("__builtin_sqrtf", ftype, BUILT_IN_SQRTF,
			    "sqrtf", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_SQRT))
    {
      tree ftype = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
      local_define_builtin ("__builtin_sqrt", ftype, BUILT_IN_SQRT,
			    "sqrt", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_SQRTL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_sqrtl", ftype, BUILT_IN_SQRTL,
			    "sqrtl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_TANF))
    {
      tree ftype = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
      local_define_builtin ("__builtin_tanf", ftype, BUILT_IN_TANF,
			    "tanf", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_TAN))
    {
      tree ftype = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
      local_define_builtin ("__builtin_tan", ftype, BUILT_IN_TAN,
			    "tan", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_TANL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_tanl", ftype, BUILT_IN_TANL,
			    "tanl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_SINF))
    {
      tree ftype = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
      local_define_builtin ("__builtin_sinf", ftype, BUILT_IN_SINF,
			    "sinf", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_SIN))
    {
      tree ftype = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
      local_define_builtin ("__builtin_sin", ftype, BUILT_IN_SIN,
			    "sin", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_SINL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_sinl", ftype, BUILT_IN_SINL,
			    "sinl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_COSF))
    {
      tree ftype = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
      local_define_builtin ("__builtin_cosf", ftype, BUILT_IN_COSF,
			    "cosf", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_COS))
    {
      tree ftype = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
      local_define_builtin ("__builtin_cos", ftype, BUILT_IN_COS,
			    "cos", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_COSL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_cosl", ftype, BUILT_IN_COSL,
			    "cosl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_ACOSF))
    {
      tree ftype = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
      local_define_builtin ("__builtin_acosf", ftype, BUILT_IN_ACOSF,
			    "acosf", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_ACOS))
    {
      tree ftype = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
      local_define_builtin ("__builtin_acos", ftype, BUILT_IN_ACOS,
			    "acos", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_ACOSL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_acosl", ftype, BUILT_IN_ACOSL,
			    "acosl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_ASINF))
    {
      tree ftype = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
      local_define_builtin ("__builtin_asinf", ftype, BUILT_IN_ASINF,
			    "asinf", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_ASIN))
    {
      tree ftype = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
      local_define_builtin ("__builtin_asin", ftype, BUILT_IN_ASIN,
			    "asin", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_ASINL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_asinl", ftype, BUILT_IN_ASINL,
			    "asinl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_ATANF))
    {
      tree ftype = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
      local_define_builtin ("__builtin_atanf", ftype, BUILT_IN_ATANF,
			    "atanf", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_ATAN))
    {
      tree ftype = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
      local_define_builtin ("__builtin_atan", ftype, BUILT_IN_ATAN,
			    "atan", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_ATANL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_atanl", ftype, BUILT_IN_ATANL,
			    "atanl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LOGF))
    {
      tree ftype = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
      local_define_builtin ("__builtin_logf", ftype, BUILT_IN_LOGF,
			    "logf", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LOG))
    {
      tree ftype = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
      local_define_builtin ("__builtin_log", ftype, BUILT_IN_LOG,
			    "log", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LOGL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_logl", ftype, BUILT_IN_LOGL,
			    "logl", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LOG10F))
    {
      tree ftype = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
      local_define_builtin ("__builtin_log10f", ftype, BUILT_IN_LOG10F,
			    "log10f", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LOG10))
    {
      tree ftype = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
      local_define_builtin ("__builtin_log10", ftype, BUILT_IN_LOG10,
			    "log10", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_LOG10L))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_log10l", ftype, BUILT_IN_LOG10L,
			    "log10l", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_EXPF))
    {
      tree ftype = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
      local_define_builtin ("__builtin_expf", ftype, BUILT_IN_EXPF,
			    "expf", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_EXP))
    {
      tree ftype = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
      local_define_builtin ("__builtin_exp", ftype, BUILT_IN_EXP,
			    "exp", ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_EXPL))
    {
      tree ftype = build_function_type_list (long_double_type_node,
					     long_double_type_node,
					     NULL_TREE);
      local_define_builtin ("__builtin_expl", ftype, BUILT_IN_EXPL,
			    "expl", ECF_NOTHROW | ECF_LEAF);
    }
}
