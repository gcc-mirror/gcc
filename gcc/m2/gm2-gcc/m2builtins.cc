/* m2builtins.cc provides an interface to the GCC builtins.

Copyright (C) 2012-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gcc-consolidation.h"

#include "m2block.h"
#include "m2convert.h"
#include "m2decl.h"
#include "m2expr.h"
#include "m2statement.h"
#include "m2tree.h"
#include "m2treelib.h"
#include "m2type.h"
#include "m2configure.h"

#undef DEBUGGING

#define GM2
#define GM2_BUG_REPORT                                                        \
  "Please report this crash to the GNU Modula-2 mailing list "                \
  "<gm2@nongnu.org>\n"

#define ASSERT(X, Y)                                                          \
  {                                                                           \
    if (!(X))                                                                 \
      {                                                                       \
        debug_tree (Y);                                                       \
        internal_error ("%s:%d:assertion of condition %qs failed", __FILE__, __LINE__,  \
                        #X);                                                  \
      }                                                                       \
  }
#define ERROR(X)                                                              \
  {                                                                           \
    internal_error ("%s:%d:%s", __FILE__, __LINE__, X);                     \
  }

typedef enum {
  BT_FN_NONE,
  BT_FN_PTR_SIZE,
  BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE,
  BT_FN_FLOAT,
  BT_FN_DOUBLE,
  BT_FN_LONG_DOUBLE,
  BT_FN_FLOAT_FLOAT,
  BT_FN_DOUBLE_DOUBLE,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE,
  BT_FN_STRING_CONST_STRING_INT,
  BT_FN_INT_CONST_PTR_CONST_PTR_SIZE,
  BT_FN_TRAD_PTR_PTR_INT_SIZE,
  BT_FN_STRING_STRING_CONST_STRING,
  BT_FN_STRING_STRING_CONST_STRING_SIZE,
  BT_FN_INT_CONST_STRING_CONST_STRING,
  BT_FN_INT_CONST_STRING_CONST_STRING_SIZE,
  BT_FN_INT_CONST_STRING,
  BT_FN_STRING_CONST_STRING_CONST_STRING,
  BT_FN_SIZE_CONST_STRING_CONST_STRING,
  BT_FN_PTR_UNSIGNED,
  BT_FN_VOID_PTR_INT,
  BT_FN_INT_PTR,
  BT_FN_INT_FLOAT,
  BT_FN_INT_DOUBLE,
  BT_FN_INT_LONG_DOUBLE,
  BT_FN_FLOAT_FCOMPLEX,
  BT_FN_DOUBLE_DCOMPLEX,
  BT_FN_LONG_DOUBLE_LDCOMPLEX,

  BT_FN_FCOMPLEX_FCOMPLEX,
  BT_FN_DCOMPLEX_DCOMPLEX,
  BT_FN_LDCOMPLEX_LDCOMPLEX,

  BT_FN_DCOMPLEX_DOUBLE_DCOMPLEX,
  BT_FN_FCOMPLEX_FLOAT_FCOMPLEX,
  BT_FN_LDCOMPLEX_LONG_DOUBLE_LDCOMPLEX,

  BT_FN_FLOAT_FLOAT_FLOATPTR,
  BT_FN_DOUBLE_DOUBLE_DOUBLEPTR,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLEPTR,

  BT_FN_FLOAT_FLOAT_LONG_DOUBLE,
  BT_FN_DOUBLE_DOUBLE_LONG_DOUBLE,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE,

  BT_FN_FLOAT_FLOAT_LONG,
  BT_FN_DOUBLE_DOUBLE_LONG,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG,

  BT_FN_FLOAT_FLOAT_INT,
  BT_FN_DOUBLE_DOUBLE_INT,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE_INT,

  BT_FN_FLOAT_FLOAT_FLOAT,
  BT_FN_DOUBLE_DOUBLE_DOUBLE,
} builtin_prototype;

typedef enum
{
  bf_true,
  bf_false,
  bf_extension_lib,
  bf_default_lib,
  bf_gcc,
  bf_c99,
  bf_c99_c90res,
  bf_extension_lib_floatn,
  bf_c99_compl,
} bf_category;

struct builtin_function_entry
{
  const char *name;
  builtin_prototype defn;
  int function_code;
  enum built_in_class fclass;
  const char *library_name;
  tree function_node;
  tree return_node;
  bf_category function_avail;
};

/* Entries are added by examining gcc/builtins.def and copying those
   functions which can be applied to Modula-2.  */

static struct builtin_function_entry list_of_builtins[] = {
  { "__builtin_alloca", BT_FN_PTR_SIZE, BUILT_IN_ALLOCA, BUILT_IN_NORMAL,
    "alloca", NULL, NULL, bf_extension_lib },
  { "__builtin_memcpy", BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE, BUILT_IN_MEMCPY,
    BUILT_IN_NORMAL, "memcpy", NULL, NULL, bf_default_lib },
  { "__builtin_isfinite", BT_FN_INT_DOUBLE, BUILT_IN_ISFINITE, BUILT_IN_NORMAL,
    "isfinite", NULL, NULL, bf_gcc },
  { "__builtin_isnan", BT_FN_INT_DOUBLE, BUILT_IN_ISNAN, BUILT_IN_NORMAL,
    "isnan", NULL, NULL, bf_gcc },
  { "__builtin_sinf", BT_FN_FLOAT_FLOAT, BUILT_IN_SINF, BUILT_IN_NORMAL,
    "sinf", NULL, NULL, bf_c99_c90res },
  { "__builtin_sin", BT_FN_DOUBLE_DOUBLE, BUILT_IN_SIN, BUILT_IN_NORMAL, "sin",
    NULL, NULL, bf_c99_c90res },
  { "__builtin_sinl", BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_SINL,
    BUILT_IN_NORMAL, "sinl", NULL, NULL, bf_c99_c90res },
  { "__builtin_cosf", BT_FN_FLOAT_FLOAT, BUILT_IN_SINF, BUILT_IN_NORMAL,
    "cosf", NULL, NULL, bf_c99_c90res },
  { "__builtin_cos", BT_FN_DOUBLE_DOUBLE, BUILT_IN_COS, BUILT_IN_NORMAL, "cos",
    NULL, NULL, bf_c99_c90res },
  { "__builtin_cosl", BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_COSL,
    BUILT_IN_NORMAL, "cosl", NULL, NULL, bf_c99_c90res },
  { "__builtin_sqrtf", BT_FN_FLOAT_FLOAT, BUILT_IN_SQRTF, BUILT_IN_NORMAL,
    "sqrtf", NULL, NULL, bf_c99_c90res },
  { "__builtin_sqrt", BT_FN_DOUBLE_DOUBLE, BUILT_IN_SQRT, BUILT_IN_NORMAL,
    "sqrt", NULL, NULL, bf_default_lib },
  { "__builtin_sqrtl", BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_SQRTL,
    BUILT_IN_NORMAL, "sqrtl", NULL, NULL, bf_c99_c90res },
  { "__builtin_fabsf", BT_FN_FLOAT_FLOAT, BUILT_IN_FABSF, BUILT_IN_NORMAL,
    "fabsf", NULL, NULL, bf_c99_c90res },
  { "__builtin_fabs", BT_FN_DOUBLE_DOUBLE, BUILT_IN_FABS, BUILT_IN_NORMAL,
    "fabs", NULL, NULL, bf_default_lib },
  { "__builtin_fabsl", BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_FABSL,
    BUILT_IN_NORMAL, "fabsl", NULL, NULL, bf_c99_c90res },
  { "__builtin_logf", BT_FN_FLOAT_FLOAT, BUILT_IN_LOGF, BUILT_IN_NORMAL,
    "logf", NULL, NULL, bf_c99_c90res },
  { "__builtin_log", BT_FN_DOUBLE_DOUBLE, BUILT_IN_LOG, BUILT_IN_NORMAL, "log",
    NULL, NULL, bf_extension_lib_floatn },
  { "__builtin_logl", BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_LOGL,
    BUILT_IN_NORMAL, "logl", NULL, NULL, bf_c99_c90res },
  { "__builtin_expf", BT_FN_FLOAT_FLOAT, BUILT_IN_EXPF, BUILT_IN_NORMAL,
    "expf", NULL, NULL, bf_c99_c90res },
  { "__builtin_exp", BT_FN_DOUBLE_DOUBLE, BUILT_IN_EXP, BUILT_IN_NORMAL, "exp",
    NULL, NULL, bf_extension_lib_floatn },
  { "__builtin_expl", BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_EXPL,
    BUILT_IN_NORMAL, "expl", NULL, NULL, bf_c99_c90res },
  { "__builtin_log10f", BT_FN_FLOAT_FLOAT, BUILT_IN_LOG10F, BUILT_IN_NORMAL,
    "log10f", NULL, NULL, bf_c99_c90res },
  { "__builtin_log10", BT_FN_DOUBLE_DOUBLE, BUILT_IN_LOG10, BUILT_IN_NORMAL,
    "log10", NULL, NULL, bf_default_lib },
  { "__builtin_log10l", BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_LOG10L,
    BUILT_IN_NORMAL, "log10l", NULL, NULL, bf_c99_c90res },
  { "__builtin_ilogbf", BT_FN_INT_FLOAT, BUILT_IN_ILOGBF, BUILT_IN_NORMAL,
    "ilogbf", NULL, NULL, bf_c99 },
  { "__builtin_ilogb", BT_FN_INT_DOUBLE, BUILT_IN_ILOGB, BUILT_IN_NORMAL,
    "ilogb", NULL, NULL, bf_c99 },
  { "__builtin_ilogbl", BT_FN_INT_LONG_DOUBLE, BUILT_IN_ILOGBL,
    BUILT_IN_NORMAL, "ilogbl", NULL, NULL, bf_c99 },

  { "__builtin_atan2f", BT_FN_FLOAT_FLOAT_FLOAT, BUILT_IN_ATAN2F,
    BUILT_IN_NORMAL, "atan2f", NULL, NULL, bf_c99_c90res },
  { "__builtin_atan2", BT_FN_DOUBLE_DOUBLE_DOUBLE, BUILT_IN_ATAN2,
    BUILT_IN_NORMAL, "atan2", NULL, NULL, bf_default_lib },
  { "__builtin_atan2l", BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE,
    BUILT_IN_ATAN2L, BUILT_IN_NORMAL, "atan2l", NULL, NULL, bf_c99_c90res },

  { "__builtin_signbit", BT_FN_INT_DOUBLE, BUILT_IN_SIGNBIT, BUILT_IN_NORMAL,
    "signbit", NULL, NULL, bf_extension_lib },
  { "__builtin_signbitf", BT_FN_INT_FLOAT, BUILT_IN_SIGNBITF, BUILT_IN_NORMAL,
    "signbitf", NULL, NULL, bf_extension_lib },
  { "__builtin_signbitl", BT_FN_INT_LONG_DOUBLE, BUILT_IN_SIGNBITL,
    BUILT_IN_NORMAL, "signbitl", NULL, NULL, bf_extension_lib },
  { "__builtin_modf", BT_FN_DOUBLE_DOUBLE_DOUBLEPTR, BUILT_IN_MODF,
    BUILT_IN_NORMAL, "modf", NULL, NULL, bf_default_lib },
  { "__builtin_modff", BT_FN_FLOAT_FLOAT_FLOATPTR, BUILT_IN_MODFF,
    BUILT_IN_NORMAL, "modff", NULL, NULL, bf_c99_c90res },
  { "__builtin_modfl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLEPTR,
    BUILT_IN_MODFL, BUILT_IN_NORMAL, "modfl", NULL, NULL, bf_c99_c90res },
  { "__builtin_nextafter", BT_FN_DOUBLE_DOUBLE_DOUBLE, BUILT_IN_NEXTAFTER,
    BUILT_IN_NORMAL, "nextafter", NULL, NULL, bf_c99 },
  { "__builtin_nextafterf", BT_FN_FLOAT_FLOAT_FLOAT, BUILT_IN_NEXTAFTERF,
    BUILT_IN_NORMAL, "nextafterf", NULL, NULL, bf_c99 },
  { "__builtin_nextafterl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE,
    BUILT_IN_NEXTAFTERL, BUILT_IN_NORMAL, "nextafterl", NULL, NULL, bf_c99 },
  { "__builtin_nexttoward", BT_FN_DOUBLE_DOUBLE_LONG_DOUBLE,
    BUILT_IN_NEXTTOWARD, BUILT_IN_NORMAL, "nexttoward", NULL, NULL, bf_c99 },
  { "__builtin_nexttowardf", BT_FN_FLOAT_FLOAT_LONG_DOUBLE,
    BUILT_IN_NEXTTOWARDF, BUILT_IN_NORMAL, "nexttowardf", NULL, NULL, bf_c99 },
  { "__builtin_nexttowardl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE,
    BUILT_IN_NEXTTOWARDL, BUILT_IN_NORMAL, "nexttowardl", NULL, NULL, bf_c99 },
  { "__builtin_scalbln", BT_FN_DOUBLE_DOUBLE_LONG, BUILT_IN_SCALBLN,
    BUILT_IN_NORMAL, "scalbln", NULL, NULL, bf_extension_lib },
  { "__builtin_scalblnf", BT_FN_FLOAT_FLOAT_LONG, BUILT_IN_SCALBLNF,
    BUILT_IN_NORMAL, "scalblnf", NULL, NULL, bf_extension_lib },
  { "__builtin_scalblnl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG,
    BUILT_IN_SCALBLNL, BUILT_IN_NORMAL, "scalblnl", NULL, NULL, bf_extension_lib },
  { "__builtin_scalbn", BT_FN_DOUBLE_DOUBLE_INT, BUILT_IN_SCALBN,
    BUILT_IN_NORMAL, "scalbln", NULL, NULL, bf_extension_lib },
  { "__builtin_scalbnf", BT_FN_FLOAT_FLOAT_INT, BUILT_IN_SCALBNF,
    BUILT_IN_NORMAL, "scalblnf", NULL, NULL, bf_extension_lib },
  { "__builtin_scalbnl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_INT, BUILT_IN_SCALBNL,
    BUILT_IN_NORMAL, "scalblnl", NULL, NULL, bf_extension_lib },

  /* Complex intrinsic functions.  */
  { "__builtin_cabs", BT_FN_DOUBLE_DCOMPLEX, BUILT_IN_CABS, BUILT_IN_NORMAL,
    "cabs", NULL, NULL, bf_c99_compl },
  { "__builtin_cabsf", BT_FN_FLOAT_FCOMPLEX, BUILT_IN_CABSF, BUILT_IN_NORMAL,
    "cabsf", NULL, NULL, bf_c99_compl },
  { "__builtin_cabsl", BT_FN_LONG_DOUBLE_LDCOMPLEX, BUILT_IN_CABSL,
    BUILT_IN_NORMAL, "cabsl", NULL, NULL, bf_c99_compl },

  { "__builtin_carg", BT_FN_DOUBLE_DCOMPLEX, BUILT_IN_CABS, BUILT_IN_NORMAL,
    "carg", NULL, NULL, bf_c99_compl },
  { "__builtin_cargf", BT_FN_FLOAT_FCOMPLEX, BUILT_IN_CABSF, BUILT_IN_NORMAL,
    "cargf", NULL, NULL, bf_c99_compl },
  { "__builtin_cargl", BT_FN_LONG_DOUBLE_LDCOMPLEX, BUILT_IN_CABSL,
    BUILT_IN_NORMAL, "cargl", NULL, NULL, bf_c99_compl },

  { "__builtin_conj", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CONJ, BUILT_IN_NORMAL,
    "carg", NULL, NULL, bf_c99_compl },
  { "__builtin_conjf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CONJF,
    BUILT_IN_NORMAL, "conjf", NULL, NULL, bf_c99_compl },
  { "__builtin_conjl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CONJL,
    BUILT_IN_NORMAL, "conjl", NULL, NULL, bf_c99_compl },

  { "__builtin_cpow", BT_FN_DCOMPLEX_DOUBLE_DCOMPLEX, BUILT_IN_CPOW,
    BUILT_IN_NORMAL, "cpow", NULL, NULL, bf_c99_compl },
  { "__builtin_cpowf", BT_FN_FCOMPLEX_FLOAT_FCOMPLEX, BUILT_IN_CPOWF,
    BUILT_IN_NORMAL, "cpowf", NULL, NULL, bf_c99_compl },
  { "__builtin_cpowl", BT_FN_LDCOMPLEX_LONG_DOUBLE_LDCOMPLEX, BUILT_IN_CPOWL,
    BUILT_IN_NORMAL, "cpowl", NULL, NULL, bf_c99_compl },

  { "__builtin_csqrt", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CSQRT,
    BUILT_IN_NORMAL, "csqrt", NULL, NULL, bf_c99_compl },
  { "__builtin_csqrtf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CSQRTF,
    BUILT_IN_NORMAL, "csqrtf", NULL, NULL, bf_c99_compl },
  { "__builtin_csqrtl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CSQRTL,
    BUILT_IN_NORMAL, "csqrtl", NULL, NULL, bf_c99_compl },

  { "__builtin_cexp", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CEXP, BUILT_IN_NORMAL,
    "cexp", NULL, NULL, bf_c99_compl },
  { "__builtin_cexpf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CEXPF,
    BUILT_IN_NORMAL, "cexpf", NULL, NULL, bf_c99_compl },
  { "__builtin_cexpl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CEXPL,
    BUILT_IN_NORMAL, "cexpl", NULL, NULL, bf_c99_compl },

  { "__builtin_clog", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CLOG, BUILT_IN_NORMAL,
    "clog", NULL, NULL, bf_c99_compl },
  { "__builtin_clogf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CLOGF, BUILT_IN_NORMAL,
    "clogf", NULL, NULL, bf_c99_compl },
  { "__builtin_clogl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CLOGL,
    BUILT_IN_NORMAL, "clogl", NULL, NULL, bf_c99_compl },

  { "__builtin_csin", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CSIN, BUILT_IN_NORMAL,
    "csin", NULL, NULL, bf_c99_compl },
  { "__builtin_csinf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CSINF,
    BUILT_IN_NORMAL, "csinf", NULL, NULL, bf_c99_compl },
  { "__builtin_csinl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CSINL,
    BUILT_IN_NORMAL, "csinl", NULL, NULL, bf_c99_compl },

  { "__builtin_ccos", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CCOS, BUILT_IN_NORMAL,
    "ccos", NULL, NULL, bf_c99_compl },
  { "__builtin_ccosf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CCOSF,
    BUILT_IN_NORMAL, "ccosf", NULL, NULL, bf_c99_compl },
  { "__builtin_ccosl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CCOSL,
    BUILT_IN_NORMAL, "ccosl", NULL, NULL, bf_c99_compl },

  { "__builtin_ctan", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CTAN, BUILT_IN_NORMAL,
    "ctan", NULL, NULL, bf_c99_compl },
  { "__builtin_ctanf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CTANF,
    BUILT_IN_NORMAL, "ctanf", NULL, NULL, bf_c99_compl },
  { "__builtin_ctanl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CTANL,
    BUILT_IN_NORMAL, "ctanl", NULL, NULL, bf_c99_compl },

  { "__builtin_casin", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CASIN,
    BUILT_IN_NORMAL, "casin", NULL, NULL, bf_c99_compl },
  { "__builtin_casinf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CASINF,
    BUILT_IN_NORMAL, "casinf", NULL, NULL, bf_c99_compl },
  { "__builtin_casinl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CASINL,
    BUILT_IN_NORMAL, "casinl", NULL, NULL, bf_c99_compl },

  { "__builtin_cacos", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CACOS,
    BUILT_IN_NORMAL, "cacos", NULL, NULL, bf_c99_compl },
  { "__builtin_cacosf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CACOSF,
    BUILT_IN_NORMAL, "cacosf", NULL, NULL, bf_c99_compl },
  { "__builtin_cacosl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CACOSL,
    BUILT_IN_NORMAL, "cacosl", NULL, NULL, bf_c99_compl },

  { "__builtin_catan", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CATAN,
    BUILT_IN_NORMAL, "catan", NULL, NULL, bf_c99_compl },
  { "__builtin_catanf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CATANF,
    BUILT_IN_NORMAL, "catanf", NULL, NULL, bf_c99_compl },
  { "__builtin_catanl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CATANL,
    BUILT_IN_NORMAL, "catanl", NULL, NULL, bf_c99_compl },

  { "__builtin_huge_val", BT_FN_DOUBLE, BUILT_IN_HUGE_VAL, BUILT_IN_NORMAL,
    "huge_val", NULL, NULL, bf_gcc },
  { "__builtin_huge_valf", BT_FN_FLOAT, BUILT_IN_HUGE_VALF, BUILT_IN_NORMAL,
    "huge_valf", NULL, NULL, bf_gcc },
  { "__builtin_huge_vall", BT_FN_LONG_DOUBLE, BUILT_IN_HUGE_VALL,
    BUILT_IN_NORMAL, "huge_vall", NULL, NULL, bf_gcc },

  { "__builtin_index", BT_FN_STRING_CONST_STRING_INT, BUILT_IN_INDEX,
    BUILT_IN_NORMAL, "index", NULL, NULL, bf_extension_lib },
  { "__builtin_rindex", BT_FN_STRING_CONST_STRING_INT, BUILT_IN_RINDEX,
    BUILT_IN_NORMAL, "rindex", NULL, NULL, bf_extension_lib },
  { "__builtin_memcmp", BT_FN_INT_CONST_PTR_CONST_PTR_SIZE, BUILT_IN_MEMCMP,
    BUILT_IN_NORMAL, "memcmp", NULL, NULL, bf_default_lib },
  { "__builtin_memmove", BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE, BUILT_IN_MEMMOVE,
    BUILT_IN_NORMAL, "memmove", NULL, NULL, bf_default_lib },
  { "__builtin_memset", BT_FN_TRAD_PTR_PTR_INT_SIZE, BUILT_IN_MEMSET,
    BUILT_IN_NORMAL, "memset", NULL, NULL, bf_default_lib },
  { "__builtin_strcat", BT_FN_STRING_STRING_CONST_STRING, BUILT_IN_STRCAT,
    BUILT_IN_NORMAL, "strcat", NULL, NULL, bf_default_lib },
  { "__builtin_strncat", BT_FN_STRING_STRING_CONST_STRING_SIZE,
    BUILT_IN_STRNCAT, BUILT_IN_NORMAL, "strncat", NULL, NULL, bf_default_lib },
  { "__builtin_strcpy", BT_FN_STRING_STRING_CONST_STRING, BUILT_IN_STRCPY,
    BUILT_IN_NORMAL, "strcpy", NULL, NULL, bf_default_lib },
  { "__builtin_strncpy", BT_FN_STRING_STRING_CONST_STRING_SIZE,
    BUILT_IN_STRNCPY, BUILT_IN_NORMAL, "strncpy", NULL, NULL, bf_default_lib },
  { "__builtin_strcmp", BT_FN_INT_CONST_STRING_CONST_STRING, BUILT_IN_STRCMP,
    BUILT_IN_NORMAL, "strcmp", NULL, NULL, bf_default_lib },
  { "__builtin_strncmp", BT_FN_INT_CONST_STRING_CONST_STRING_SIZE,
    BUILT_IN_STRNCMP, BUILT_IN_NORMAL, "strncmp", NULL, NULL, bf_default_lib },
  { "__builtin_strlen", BT_FN_INT_CONST_STRING, BUILT_IN_STRLEN,
    BUILT_IN_NORMAL, "strlen", NULL, NULL, bf_default_lib },
  { "__builtin_strstr", BT_FN_STRING_CONST_STRING_CONST_STRING,
    BUILT_IN_STRSTR, BUILT_IN_NORMAL, "strstr", NULL, NULL, bf_default_lib },
  { "__builtin_strpbrk", BT_FN_STRING_CONST_STRING_CONST_STRING,
    BUILT_IN_STRPBRK, BUILT_IN_NORMAL, "strpbrk", NULL, NULL, bf_default_lib },
  { "__builtin_strspn", BT_FN_SIZE_CONST_STRING_CONST_STRING, BUILT_IN_STRSPN,
    BUILT_IN_NORMAL, "strspn", NULL, NULL, bf_default_lib },
  { "__builtin_strcspn", BT_FN_SIZE_CONST_STRING_CONST_STRING,
    BUILT_IN_STRCSPN, BUILT_IN_NORMAL, "strcspn", NULL, NULL, bf_default_lib },
  { "__builtin_strchr", BT_FN_STRING_CONST_STRING_INT, BUILT_IN_STRCHR,
    BUILT_IN_NORMAL, "strchr", NULL, NULL, bf_default_lib },
  { "__builtin_strrchr", BT_FN_STRING_CONST_STRING_INT, BUILT_IN_STRCHR,
    BUILT_IN_NORMAL, "strrchr", NULL, NULL, bf_default_lib },
  { "__builtin_frame_address", BT_FN_PTR_UNSIGNED, BUILT_IN_FRAME_ADDRESS,
    BUILT_IN_NORMAL, "frame_address", NULL, NULL, bf_gcc },
  { "__builtin_return_address", BT_FN_PTR_UNSIGNED, BUILT_IN_RETURN_ADDRESS,
    BUILT_IN_NORMAL, "return_address", NULL, NULL, bf_gcc },
  { "__builtin_longjmp", BT_FN_VOID_PTR_INT, BUILT_IN_LONGJMP, BUILT_IN_NORMAL,
    "longjmp", NULL, NULL, bf_gcc },
  { "__builtin_setjmp", BT_FN_INT_PTR, BUILT_IN_SETJMP, BUILT_IN_NORMAL,
    "setjmp", NULL, NULL, bf_gcc },
  { NULL, BT_FN_NONE, 0, NOT_BUILT_IN, "", NULL, NULL, bf_false}
};

struct builtin_type_info
{
  const char *name;
  unsigned int returnType;
  tree (*functionHandler) (location_t, tree);
};

struct GTY(()) builtin_macro_definition
{
  const char *name;
  tree function_node;
  tree return_node;
};

static GTY (()) tree sizetype_endlink;
static GTY (()) tree unsigned_endlink;
static GTY (()) tree endlink;
static GTY (()) tree math_endlink;
static GTY (()) tree int_endlink;
static GTY (()) tree ptr_endlink;
static GTY (()) tree const_ptr_endlink;
static GTY (()) tree double_ftype_void;
static GTY (()) tree float_ftype_void;
static GTY (()) tree ldouble_ftype_void;
static GTY (()) tree float_ftype_float;
static GTY (()) tree double_ftype_double;
static GTY (()) tree ldouble_ftype_ldouble;
static GTY (()) tree gm2_alloca_node;
static GTY (()) tree gm2_memcpy_node;
static GTY (()) tree gm2_memset_node;
static GTY (()) tree gm2_isfinite_node;
static GTY (()) tree gm2_isnan_node;
static GTY (()) tree gm2_huge_valf_node;
static GTY (()) tree gm2_huge_val_node;
static GTY (()) tree gm2_huge_vall_node;
static GTY (()) tree long_doubleptr_type_node;
static GTY (()) tree doubleptr_type_node;
static GTY (()) tree floatptr_type_node;
static GTY (()) tree builtin_ftype_int_var;
static GTY (()) vec<builtin_macro_definition, va_gc> *builtin_macros;

/* Prototypes for locally defined functions.  */
static tree DoBuiltinAlloca (location_t location, tree n);
static tree DoBuiltinMemCopy (location_t location, tree dest, tree src,
                              tree n);
static tree DoBuiltinIsfinite (location_t location, tree value);
static tree DoBuiltinIsnan (location_t location, tree value);
static void create_function_prototype (location_t location,
                                       struct builtin_function_entry *fe);
static tree doradix (location_t location, tree type);
static tree doplaces (location_t location, tree type);
static tree doexponentmin (location_t location, tree type);
static tree doexponentmax (location_t location, tree type);
static tree dolarge (location_t location, tree type);
static tree dosmall (location_t location, tree type);
static tree doiec559 (location_t location, tree type);
static tree dolia1 (location_t location, tree type);
static tree doiso (location_t location, tree type);
static tree doieee (location_t location, tree type);
static tree dorounds (location_t location, tree type);
static tree dogUnderflow (location_t location, tree type);
static tree doexception (location_t location, tree type);
static tree doextend (location_t location, tree type);
static tree donModes (location_t location, tree type);
/* Prototypes finish here.  */

#define m2builtins_c
#include "m2builtins.h"

static struct builtin_type_info m2_type_info[] = {
  { "radix", 2, doradix },
  { "places", 2, doplaces },
  { "expoMin", 2, doexponentmin },
  { "expoMax", 2, doexponentmax },
  { "large", 3, dolarge },
  { "small", 3, dosmall },
  { "IEC559", 1, doiec559 },
  { "LIA1", 1, dolia1 },
  { "ISO", 1, doiso },
  { "IEEE", 1, doieee },
  { "rounds", 1, dorounds },
  { "gUnderflow", 1, dogUnderflow },
  { "exception", 1, doexception },
  { "extend", 1, doextend },
  { "nModes", 2, donModes },
  { NULL, 0, NULL },
};

/* Return a definition for a builtin function named NAME and whose
data type is TYPE.  TYPE should be a function type with argument
types.  FUNCTION_CODE tells later passes how to compile calls to this
function.  See tree.h for its possible values.

If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME, the
name to be called if we can't opencode the function.  */

tree
builtin_function (location_t location, const char *name, tree type,
                  int function_code, enum built_in_class fclass,
                  const char *library_name, tree attrs)
{
  tree decl = add_builtin_function (name, type, function_code, fclass,
                                    library_name, attrs);
  DECL_SOURCE_LOCATION (decl) = location;

  m2block_pushDecl (decl);
  return decl;
}

/* GetBuiltinConst - returns the gcc tree of a builtin constant,
   name.  NIL is returned if the constant is unknown.  */

tree
m2builtins_GetBuiltinConst (char *name)
{
  if (strcmp (name, "BITS_PER_UNIT") == 0)
    return m2decl_BuildIntegerConstant (BITS_PER_UNIT);
  if (strcmp (name, "BITS_PER_WORD") == 0)
    return m2decl_BuildIntegerConstant (BITS_PER_WORD);
  if (strcmp (name, "BITS_PER_CHAR") == 0)
    return m2decl_BuildIntegerConstant (CHAR_TYPE_SIZE);
  if (strcmp (name, "UNITS_PER_WORD") == 0)
    return m2decl_BuildIntegerConstant (UNITS_PER_WORD);

  return NULL_TREE;
}

/* GetBuiltinConstType - returns the type of a builtin constant,
   name.  0 = unknown constant name 1 = integer 2 = real.  */

unsigned int
m2builtins_GetBuiltinConstType (char *name)
{
  if (strcmp (name, "BITS_PER_UNIT") == 0)
    return 1;
  if (strcmp (name, "BITS_PER_WORD") == 0)
    return 1;
  if (strcmp (name, "BITS_PER_CHAR") == 0)
    return 1;
  if (strcmp (name, "UNITS_PER_WORD") == 0)
    return 1;

  return 0;
}

/* GetBuiltinTypeInfoType - returns value: 0 is ident is unknown.  1
   if ident is IEC559, LIA1, ISO, IEEE, rounds, underflow, exception,
   extend.  2 if ident is radix, places, exponentmin, exponentmax,
   noofmodes.  3 if ident is large, small.  */

unsigned int
m2builtins_GetBuiltinTypeInfoType (const char *ident)
{
  int i = 0;

  while (m2_type_info[i].name != NULL)
    if (strcmp (m2_type_info[i].name, ident) == 0)
      return m2_type_info[i].returnType;
    else
      i++;
  return 0;
}

/* GetBuiltinTypeInfo - returns value: NULL_TREE if ident is unknown.
   boolean Tree if ident is IEC559, LIA1, ISO, IEEE, rounds,
   underflow, exception, extend.  ZType Tree if ident is radix,
   places, exponentmin, exponentmax, noofmodes.
   RType Tree if ident is large, small.  */

tree
m2builtins_GetBuiltinTypeInfo (location_t location, tree type,
                               const char *ident)
{
  int i = 0;

  type = m2tree_skip_type_decl (type);
  while (m2_type_info[i].name != NULL)
    if (strcmp (m2_type_info[i].name, ident) == 0)
      return (*m2_type_info[i].functionHandler) (location, type);
    else
      i++;
  return NULL_TREE;
}

/* doradix - returns the radix of the floating point, type.  */

static tree
doradix (location_t location ATTRIBUTE_UNUSED, tree type)
{
  if (SCALAR_FLOAT_TYPE_P (type))
    {
      enum machine_mode mode = TYPE_MODE (type);
      int radix = REAL_MODE_FORMAT (mode)->b;
      return m2decl_BuildIntegerConstant (radix);
    }
  else
    return NULL_TREE;
}

/* doplaces - returns the whole number value of the number of radix
   places used to store values of the corresponding real number type.  */

static tree
doplaces (location_t location ATTRIBUTE_UNUSED, tree type)
{
  if (SCALAR_FLOAT_TYPE_P (type))
    {
      /* Taken from c-family/c-cppbuiltin.cc.  */
      /* The number of decimal digits, q, such that any floating-point
         number with q decimal digits can be rounded into a
         floating-point number with p radix b digits and back again
         without change to the q decimal digits, p log10 b if b is a
         power of 10 floor((p - 1) log10 b) otherwise.  */
      enum machine_mode mode = TYPE_MODE (type);
      const struct real_format *fmt = REAL_MODE_FORMAT (mode);
      const double log10_2 = .30102999566398119521;
      double log10_b = log10_2;
      int digits = (fmt->p - 1) * log10_b;
      return m2decl_BuildIntegerConstant (digits);
    }
  else
    return NULL_TREE;
}

/* doexponentmin - returns the whole number of the exponent minimum.  */

static tree
doexponentmin (location_t location ATTRIBUTE_UNUSED, tree type)
{
  if (SCALAR_FLOAT_TYPE_P (type))
    {
      enum machine_mode mode = TYPE_MODE (type);
      int emin = REAL_MODE_FORMAT (mode)->emin;
      return m2decl_BuildIntegerConstant (emin);
    }
  else
    return NULL_TREE;
}

/* doexponentmax - returns the whole number of the exponent maximum.  */

static tree
doexponentmax (location_t location ATTRIBUTE_UNUSED, tree type)
{
  if (SCALAR_FLOAT_TYPE_P (type))
    {
      enum machine_mode mode = TYPE_MODE (type);
      int emax = REAL_MODE_FORMAT (mode)->emax;
      return m2decl_BuildIntegerConstant (emax);
    }
  else
    return NULL_TREE;
}

static tree
computeLarge (tree type)
{
  enum machine_mode mode = TYPE_MODE (type);
  const struct real_format *fmt = REAL_MODE_FORMAT (mode);
  REAL_VALUE_TYPE real;
  char buf[128];

  /* Shamelessly taken from c-cppbuiltin.cc:builtin_define_float_constants.  */

  /* Since, for the supported formats, B is always a power of 2, we
  construct the following numbers directly as a hexadecimal constants.  */

  get_max_float (fmt, buf, sizeof (buf), false);
  real_from_string (&real, buf);
  return build_real (type, real);
}

/* dolarge - return the largest value of the corresponding real type.  */

static tree
dolarge (location_t location ATTRIBUTE_UNUSED, tree type)
{
  if (SCALAR_FLOAT_TYPE_P (type))
    return computeLarge (type);
  return NULL_TREE;
}

static tree
computeSmall (tree type)
{
  enum machine_mode mode = TYPE_MODE (type);
  const struct real_format *fmt = REAL_MODE_FORMAT (mode);
  REAL_VALUE_TYPE real;
  char buf[128];

  /* The minimum normalized positive floating-point number,
  b**(emin-1).  */

  sprintf (buf, "0x1p%d", fmt->emin - 1);
  real_from_string (&real, buf);
  return build_real (type, real);
}

/* dosmall - return the smallest positive value of the corresponding
   real type.  */

static tree
dosmall (location_t location ATTRIBUTE_UNUSED, tree type)
{
  if (SCALAR_FLOAT_TYPE_P (type))
    return computeSmall (type);
  return NULL_TREE;
}

/* doiec559 - a boolean value that is true if and only if the
   implementation of the corresponding real number type conforms to
   IEC 559:1989 (also known as IEEE 754:1987) in all regards.  */

static tree
doiec559 (location_t location, tree type)
{
  if (m2expr_IsTrue (m2expr_BuildEqualTo (location,
                                          m2decl_BuildIntegerConstant (32),
                                          m2expr_GetSizeOfInBits (type))))
    return m2type_GetBooleanTrue ();
  if (m2expr_IsTrue (m2expr_BuildEqualTo (location,
                                          m2decl_BuildIntegerConstant (64),
                                          m2expr_GetSizeOfInBits (type))))
    return m2type_GetBooleanTrue ();
  return m2type_GetBooleanFalse ();
}

/* dolia1 - returns TRUE if using ieee (currently always TRUE).  */

static tree
dolia1 (location_t location, tree type)
{
  return doieee (location, type);
}

/* doiso - returns TRUE if using ieee (--fixme--).  */

static tree
doiso (location_t location, tree type)
{
  return doieee (location, type);
}

/* doieee - returns TRUE if ieee arithmetic is being used.  */

static tree
doieee (location_t location ATTRIBUTE_UNUSED, tree type ATTRIBUTE_UNUSED)
{
  /* --fixme-- maybe we should look for the -mno-ieee flag and return this
     result.  */
  return m2type_GetBooleanTrue ();
}

/* dorounds - returns TRUE if and only if each operation produces a
   result that is one of the values of the corresponding real number
   type nearest to the mathematical result.  */

static tree
dorounds (location_t location ATTRIBUTE_UNUSED, tree type ATTRIBUTE_UNUSED)
{
  if (FLT_ROUNDS)
    return m2type_GetBooleanTrue ();
  else
    return m2type_GetBooleanFalse ();
}

/* dogUnderflow - returns TRUE if and only if there are values of the
   corresponding real number type between 0.0 and small.  */

static tree
dogUnderflow (location_t location ATTRIBUTE_UNUSED, tree type)
{
  if (SCALAR_FLOAT_TYPE_P (type))
    {
      enum machine_mode mode = TYPE_MODE (type);
      const struct real_format *fmt = REAL_MODE_FORMAT (mode);
      if (fmt->has_denorm)
        return m2type_GetBooleanTrue ();
      else
        return m2type_GetBooleanFalse ();
    }
  return NULL_TREE;
}

/* doexception - */

static tree
doexception (location_t location ATTRIBUTE_UNUSED, tree type ATTRIBUTE_UNUSED)
{
  return m2type_GetBooleanTrue ();
}

/* doextend - */

static tree
doextend (location_t location ATTRIBUTE_UNUSED, tree type ATTRIBUTE_UNUSED)
{
  return m2type_GetBooleanTrue ();
}

/* donModes - */

static tree
donModes (location_t location ATTRIBUTE_UNUSED, tree type ATTRIBUTE_UNUSED)
{
  return m2decl_BuildIntegerConstant (1);
}

/* BuiltinMemCopy - copy n bytes of memory efficiently from address
   src to dest.  */

tree
m2builtins_BuiltinMemCopy (location_t location, tree dest, tree src, tree n)
{
  return DoBuiltinMemCopy (location, dest, src, n);
}


static tree
DoBuiltinMemSet (location_t location, tree ptr, tree bytevalue, tree nbytes)
{
  tree functype = TREE_TYPE (gm2_memset_node);
  tree funcptr
      = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_memset_node);
  tree call
      = m2treelib_DoCall3 (location, ptr_type_node, funcptr, ptr, bytevalue, nbytes);
  return call;
}

/* BuiltinMemSet set copy n bytes of memory efficiently from address
   src to dest.  */

tree
m2builtins_BuiltinMemSet (location_t location, tree ptr, tree bytevalue, tree nbytes)
{
  return DoBuiltinMemSet (location, ptr, bytevalue, nbytes);
}

/* BuiltInAlloca - given an expression, n, allocate, n, bytes on the
   stack for the life of the current function.  */

tree
m2builtins_BuiltInAlloca (location_t location, tree n)
{
  return DoBuiltinAlloca (location, n);
}

/* BuiltInIsfinite - return integer 1 if the real expression is
   finite otherwise return integer 0.  */

tree
m2builtins_BuiltInIsfinite (location_t location, tree expression)
{
  return DoBuiltinIsfinite (location, expression);
}

/* BuiltInIsnan - return integer 1 if the real expression is
   nan otherwise return integer 0.  */

tree
m2builtins_BuiltInIsnan (location_t location, tree expression)
{
  return DoBuiltinIsnan (location, expression);
}


/* do_target_support_exists returns true if the builting function
   is supported by the target.  */

static
bool
do_target_support_exists (struct builtin_function_entry *fe)
{
  tree type = TREE_TYPE (fe->function_node);

  switch (fe->function_avail)
    {
    case bf_true:
      return true;
    case bf_false:
      return false;
    case bf_extension_lib:
      return true;
    case bf_default_lib:
      return true;
    case bf_gcc:
      return true;
    case bf_c99:
      return targetm.libc_has_function (function_c99_misc, type);
    case bf_c99_c90res:
      return targetm.libc_has_function (function_c99_misc, type);
    case bf_extension_lib_floatn:
      return true;
    case bf_c99_compl:
      return targetm.libc_has_function (function_c99_math_complex, type);
    default:
      gcc_unreachable ();
    }
  return false;
}


static
bool
target_support_exists (struct builtin_function_entry *fe)
{
#if defined(DEBUGGING)
  printf ("target_support_exists (%s): ", fe->library_name);
#endif
  if (do_target_support_exists (fe))
    {
#if defined(DEBUGGING)
      printf ("yes\n");
#endif
      return true;
    }
  else
    {
#if defined(DEBUGGING)
      printf ("no\n");
#endif
      return false;
    }
}


/* BuiltinExists - returns TRUE if the builtin function, name, exists
   for this target architecture.  */

bool
m2builtins_BuiltinExists (char *name)
{
  struct builtin_function_entry *fe;

  for (fe = &list_of_builtins[0]; fe->name != NULL; fe++)
    if (strcmp (name, fe->name) == 0)
      return true;
      // return target_support_exists (fe);
  int length = vec_safe_length (builtin_macros);
  for (int idx = 0; idx < length; idx++)
    if (strcmp ((*builtin_macros)[idx].name, name) == 0)
      return true;
  return false;
}

/* lookup_builtin_function returns a builtin macro.  */

static
tree
lookup_builtin_macro (location_t location, char *name)
{
  int length = vec_safe_length (builtin_macros);
  for (int idx = 0; idx < length; idx++)
    if (strcmp ((*builtin_macros)[idx].name, name) == 0)
      {
        tree functype = TREE_TYPE ((*builtin_macros)[idx].function_node);
        tree funcptr = build1 (ADDR_EXPR, build_pointer_type (functype),
                               (*builtin_macros)[idx].function_node);
	tree call = m2treelib_DoCall (
	   location, (*builtin_macros)[idx].return_node,
	   funcptr, m2statement_GetParamList ());
        m2statement_SetLastFunction (call);
        m2statement_SetParamList (NULL_TREE);
        if ((*builtin_macros)[idx].return_node == void_type_node)
          m2statement_SetLastFunction (NULL_TREE);
        return call;
      }
  return NULL_TREE;
}

/* lookup_builtin_function returns a builtin function.  */

static
tree
lookup_builtin_function (location_t location, char *name)
{
  struct builtin_function_entry *fe;

  for (fe = &list_of_builtins[0]; fe->name != NULL; fe++)
    if ((strcmp (name, fe->name) == 0) && target_support_exists (fe))
      {
        tree functype = TREE_TYPE (fe->function_node);
        tree funcptr = build1 (ADDR_EXPR, build_pointer_type (functype),
                               fe->function_node);
	tree call = m2treelib_DoCall (
	   location, fe->return_node, funcptr, m2statement_GetParamList ());
        m2statement_SetLastFunction (call);
        m2statement_SetParamList (NULL_TREE);
        if (fe->return_node == void_type_node)
          m2statement_SetLastFunction (NULL_TREE);
        return call;
      }
  return NULL_TREE;
}

/* BuildBuiltinTree - returns a Tree containing the builtin function,
   name.  */

tree
m2builtins_BuildBuiltinTree (location_t location, char *name)
{
  tree call;
  m2statement_SetLastFunction (NULL_TREE);

  call = lookup_builtin_function (location, name);
  if (call == NULL_TREE)
    {
      call = lookup_builtin_macro (location, name);
      if (call == NULL_TREE)
	{
	  m2statement_SetParamList (NULL_TREE);
	  return m2statement_GetLastFunction ();
	}
    }
  return call;
}

static tree
DoBuiltinMemCopy (location_t location, tree dest, tree src, tree bytes)
{
  tree functype = TREE_TYPE (gm2_memcpy_node);
  tree funcptr
      = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_memcpy_node);
  tree call
      = m2treelib_DoCall3 (location, ptr_type_node, funcptr, dest, src, bytes);
  return call;
}

static tree
DoBuiltinAlloca (location_t location, tree bytes)
{
  tree functype = TREE_TYPE (gm2_alloca_node);
  tree funcptr
      = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_alloca_node);
  tree call = m2treelib_DoCall1 (location, ptr_type_node, funcptr, bytes);

  return call;
}

static tree
DoBuiltinIsfinite (location_t location, tree value)
{
  tree functype = TREE_TYPE (gm2_isfinite_node);
  tree funcptr
      = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_isfinite_node);
  tree call = m2treelib_DoCall1 (location, ptr_type_node, funcptr, value);

  return call;
}

static tree
DoBuiltinIsnan (location_t location, tree value)
{
  tree functype = TREE_TYPE (gm2_isnan_node);
  tree funcptr
      = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_isnan_node);
  tree call = m2treelib_DoCall1 (location, ptr_type_node, funcptr, value);

  return call;
}

tree
m2builtins_BuiltInHugeVal (location_t location)
{
  tree functype = TREE_TYPE (gm2_huge_val_node);
  tree funcptr
      = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_huge_val_node);
  tree call = m2treelib_DoCall0 (location, ptr_type_node, funcptr);
  return call;
}

tree
m2builtins_BuiltInHugeValShort (location_t location)
{
  tree functype = TREE_TYPE (gm2_huge_valf_node);
  tree funcptr
      = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_huge_valf_node);
  tree call = m2treelib_DoCall0 (location, ptr_type_node, funcptr);
  return call;
}

tree
m2builtins_BuiltInHugeValLong (location_t location)
{
  tree functype = TREE_TYPE (gm2_huge_vall_node);
  tree funcptr
      = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_huge_vall_node);
  tree call = m2treelib_DoCall0 (location, ptr_type_node, funcptr);
  return call;
}

static void
create_function_prototype (location_t location,
                           struct builtin_function_entry *fe)
{
  tree ftype;

  switch (fe->defn)
    {

    case BT_FN_PTR_SIZE:
      ftype = build_function_type (ptr_type_node, sizetype_endlink);
      fe->return_node = ptr_type_node;
      break;

    case BT_FN_STRING_STRING_CONST_STRING_SIZE:
    case BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE:
      ftype = build_function_type (
          ptr_type_node, tree_cons (NULL_TREE, ptr_type_node,
                                    tree_cons (NULL_TREE, const_ptr_type_node,
                                               sizetype_endlink)));
      fe->return_node = ptr_type_node;
      break;
    case BT_FN_FLOAT:
      ftype = float_ftype_void;
      fe->return_node = float_type_node;
      break;
    case BT_FN_DOUBLE:
      ftype = double_ftype_void;
      fe->return_node = double_type_node;
      break;
    case BT_FN_LONG_DOUBLE:
      ftype = ldouble_ftype_void;
      fe->return_node = m2type_GetM2LongRealType ();
      break;
    case BT_FN_FLOAT_FLOAT:
      ftype = float_ftype_float;
      fe->return_node = float_type_node;
      break;
    case BT_FN_DOUBLE_DOUBLE:
      ftype = double_ftype_double;
      fe->return_node = double_type_node;
      break;
    case BT_FN_LONG_DOUBLE_LONG_DOUBLE:
      ftype = ldouble_ftype_ldouble;
      fe->return_node = m2type_GetM2LongRealType ();
      break;
    case BT_FN_STRING_CONST_STRING_INT:
      ftype = build_function_type (
          ptr_type_node, tree_cons (NULL_TREE, ptr_type_node, int_endlink));
      fe->return_node = ptr_type_node;
      break;
    case BT_FN_INT_CONST_PTR_CONST_PTR_SIZE:
      ftype = build_function_type (
          integer_type_node,
          tree_cons (NULL_TREE, const_ptr_type_node,
                     tree_cons (NULL_TREE, const_ptr_type_node, int_endlink)));
      fe->return_node = integer_type_node;
      break;
    case BT_FN_TRAD_PTR_PTR_INT_SIZE:
      ftype = build_function_type (
          ptr_type_node, tree_cons (NULL_TREE, ptr_type_node,
                                    tree_cons (NULL_TREE, integer_type_node,
                                               sizetype_endlink)));
      fe->return_node = ptr_type_node;
      break;
    case BT_FN_STRING_STRING_CONST_STRING:
      ftype = build_function_type (
          ptr_type_node, tree_cons (NULL_TREE, ptr_type_node, ptr_endlink));
      fe->return_node = ptr_type_node;
      break;
    case BT_FN_INT_CONST_STRING_CONST_STRING:
      ftype = build_function_type (
          integer_type_node,
          tree_cons (NULL_TREE, const_ptr_type_node, ptr_endlink));
      fe->return_node = integer_type_node;
      break;
    case BT_FN_INT_CONST_STRING_CONST_STRING_SIZE:
      ftype = build_function_type (
          integer_type_node,
          tree_cons (
              NULL_TREE, const_ptr_type_node,
              tree_cons (NULL_TREE, const_ptr_type_node, sizetype_endlink)));
      fe->return_node = integer_type_node;
      break;
    case BT_FN_INT_CONST_STRING:
      ftype = build_function_type (integer_type_node, ptr_endlink);
      fe->return_node = integer_type_node;
      break;
    case BT_FN_STRING_CONST_STRING_CONST_STRING:
      ftype = build_function_type (
          ptr_type_node,
          tree_cons (NULL_TREE, const_ptr_type_node, const_ptr_endlink));
      fe->return_node = ptr_type_node;
      break;
    case BT_FN_SIZE_CONST_STRING_CONST_STRING:
      ftype = build_function_type (
          sizetype,
          tree_cons (NULL_TREE, const_ptr_type_node, const_ptr_endlink));
      fe->return_node = sizetype;
      break;
    case BT_FN_PTR_UNSIGNED:
      ftype = build_function_type (ptr_type_node, unsigned_endlink);
      fe->return_node = ptr_type_node;
      break;
    case BT_FN_VOID_PTR_INT:
      ftype = build_function_type (
          void_type_node, tree_cons (NULL_TREE, ptr_type_node, int_endlink));
      fe->return_node = void_type_node;
      break;
    case BT_FN_INT_PTR:
      ftype = build_function_type (integer_type_node, ptr_endlink);
      fe->return_node = integer_type_node;
      break;
    case BT_FN_INT_FLOAT:
      ftype = build_function_type (
          integer_type_node, tree_cons (NULL_TREE, float_type_node, endlink));
      fe->return_node = integer_type_node;
      break;
    case BT_FN_INT_DOUBLE:
      ftype = build_function_type (
          integer_type_node, tree_cons (NULL_TREE, double_type_node, endlink));
      fe->return_node = integer_type_node;
      break;
    case BT_FN_INT_LONG_DOUBLE:
      ftype = build_function_type (
          integer_type_node,
          tree_cons (NULL_TREE, m2type_GetM2LongRealType (), endlink));
      fe->return_node = integer_type_node;
      break;
    case BT_FN_FLOAT_FCOMPLEX:
      ftype = build_function_type (
          float_type_node,
          tree_cons (NULL_TREE, complex_float_type_node, endlink));
      fe->return_node = float_type_node;
      break;
    case BT_FN_DOUBLE_DCOMPLEX:
      ftype = build_function_type (
          double_type_node,
          tree_cons (NULL_TREE, complex_double_type_node, endlink));
      fe->return_node = double_type_node;
      break;
    case BT_FN_LONG_DOUBLE_LDCOMPLEX:
      ftype = build_function_type (
          m2type_GetM2LongRealType (),
          tree_cons (NULL_TREE, m2type_GetM2LongComplexType (), endlink));
      fe->return_node = m2type_GetM2LongRealType ();
      break;
    case BT_FN_FCOMPLEX_FCOMPLEX:
      ftype = build_function_type (
          complex_float_type_node,
          tree_cons (NULL_TREE, complex_float_type_node, endlink));
      fe->return_node = complex_float_type_node;
      break;
    case BT_FN_DCOMPLEX_DCOMPLEX:
      ftype = build_function_type (
          complex_double_type_node,
          tree_cons (NULL_TREE, complex_double_type_node, endlink));
      fe->return_node = complex_double_type_node;
      break;
    case BT_FN_LDCOMPLEX_LDCOMPLEX:
      ftype = build_function_type (
          m2type_GetM2LongComplexType (),
          tree_cons (NULL_TREE, m2type_GetM2LongComplexType (), endlink));
      fe->return_node = m2type_GetM2LongComplexType ();
      break;
    case BT_FN_DCOMPLEX_DOUBLE_DCOMPLEX:
      ftype = build_function_type (
          complex_double_type_node,
          tree_cons (NULL_TREE, complex_double_type_node,
                     tree_cons (NULL_TREE, double_type_node, endlink)));
      fe->return_node = complex_double_type_node;
      break;
    case BT_FN_FCOMPLEX_FLOAT_FCOMPLEX:
      ftype = build_function_type (
          complex_float_type_node,
          tree_cons (NULL_TREE, complex_float_type_node,
                     tree_cons (NULL_TREE, float_type_node, endlink)));
      fe->return_node = complex_float_type_node;
      break;
    case BT_FN_LDCOMPLEX_LONG_DOUBLE_LDCOMPLEX:
      ftype = build_function_type (
          m2type_GetM2LongComplexType (),
          tree_cons (NULL_TREE, m2type_GetM2LongComplexType (),
                     tree_cons (NULL_TREE, m2type_GetM2LongRealType (), endlink)));
      fe->return_node = m2type_GetM2LongComplexType ();
      break;
    case BT_FN_FLOAT_FLOAT_FLOATPTR:
      ftype = build_function_type (
          float_type_node,
          tree_cons (NULL_TREE, float_type_node,
                     tree_cons (NULL_TREE, floatptr_type_node, endlink)));
      fe->return_node = float_type_node;
      break;
    case BT_FN_DOUBLE_DOUBLE_DOUBLEPTR:
      ftype = build_function_type (
          double_type_node,
          tree_cons (NULL_TREE, double_type_node,
                     tree_cons (NULL_TREE, doubleptr_type_node, endlink)));
      fe->return_node = double_type_node;
      break;
    case BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLEPTR:
      ftype = build_function_type (
          m2type_GetM2LongRealType (),
          tree_cons (
              NULL_TREE, m2type_GetM2LongRealType (),
              tree_cons (NULL_TREE, long_doubleptr_type_node, endlink)));
      fe->return_node = m2type_GetM2LongRealType ();
      break;
    case BT_FN_FLOAT_FLOAT_LONG_DOUBLE:
      ftype = build_function_type (
          float_type_node,
          tree_cons (NULL_TREE, float_type_node,
                     tree_cons (NULL_TREE, m2type_GetM2LongRealType (), endlink)));
      fe->return_node = float_type_node;
      break;
    case BT_FN_DOUBLE_DOUBLE_LONG_DOUBLE:
      ftype = build_function_type (
          double_type_node,
          tree_cons (NULL_TREE, double_type_node,
                     tree_cons (NULL_TREE, m2type_GetM2LongRealType (), endlink)));
      fe->return_node = double_type_node;
      break;
    case BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE:
      ftype = build_function_type (
          m2type_GetM2LongRealType (),
          tree_cons (NULL_TREE, m2type_GetM2LongRealType (),
                     tree_cons (NULL_TREE, m2type_GetM2LongRealType (), endlink)));
      fe->return_node = m2type_GetM2LongRealType ();
      break;
    case BT_FN_FLOAT_FLOAT_LONG:
      ftype = build_function_type (
          float_type_node,
          tree_cons (NULL_TREE, float_type_node,
                     tree_cons (NULL_TREE, long_integer_type_node, endlink)));
      fe->return_node = float_type_node;
      break;
    case BT_FN_DOUBLE_DOUBLE_LONG:
      ftype = build_function_type (
          double_type_node,
          tree_cons (NULL_TREE, double_type_node,
                     tree_cons (NULL_TREE, long_integer_type_node, endlink)));
      fe->return_node = double_type_node;
      break;
    case BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG:
      ftype = build_function_type (
          m2type_GetM2LongRealType (),
          tree_cons (NULL_TREE, m2type_GetM2LongRealType (),
                     tree_cons (NULL_TREE, long_integer_type_node, endlink)));
      fe->return_node = m2type_GetM2LongRealType ();
      break;
    case BT_FN_FLOAT_FLOAT_INT:
      ftype = build_function_type (
          float_type_node,
          tree_cons (NULL_TREE, float_type_node,
                     tree_cons (NULL_TREE, integer_type_node, endlink)));
      fe->return_node = float_type_node;
      break;
    case BT_FN_DOUBLE_DOUBLE_INT:
      ftype = build_function_type (
          double_type_node,
          tree_cons (NULL_TREE, double_type_node,
                     tree_cons (NULL_TREE, integer_type_node, endlink)));
      fe->return_node = double_type_node;
      break;
    case BT_FN_LONG_DOUBLE_LONG_DOUBLE_INT:
      ftype = build_function_type (
          m2type_GetM2LongRealType (),
          tree_cons (NULL_TREE, m2type_GetM2LongRealType (),
                     tree_cons (NULL_TREE, integer_type_node, endlink)));
      fe->return_node = m2type_GetM2LongRealType ();
      break;
    case BT_FN_FLOAT_FLOAT_FLOAT:
      ftype = build_function_type (
          float_type_node,
          tree_cons (NULL_TREE, float_type_node,
                     tree_cons (NULL_TREE, float_type_node, endlink)));
      fe->return_node = float_type_node;
      break;
    case BT_FN_DOUBLE_DOUBLE_DOUBLE:
      ftype = build_function_type (
          double_type_node,
          tree_cons (NULL_TREE, double_type_node,
                     tree_cons (NULL_TREE, double_type_node, endlink)));
      fe->return_node = double_type_node;
      break;
    default:
      ERROR ("enum has no case");
    }
  fe->function_node
      = builtin_function (location, fe->name, ftype, fe->function_code,
                          fe->fclass, fe->library_name, NULL);
}

static tree
find_builtin_tree (const char *name)
{
  struct builtin_function_entry *fe;

  for (fe = &list_of_builtins[0]; fe->name != NULL; fe++)
    if (strcmp (name, fe->name) == 0)
      return fe->function_node;

  ERROR ("cannot find builtin function");
  return NULL_TREE;
}


static void
set_decl_built_in_class (tree decl, built_in_class c)
{
  FUNCTION_DECL_CHECK (decl)->function_decl.built_in_class = c;
}


static void
set_decl_function_code (tree decl, built_in_function f)
{
  tree_function_decl &fndecl = FUNCTION_DECL_CHECK (decl)->function_decl;
  fndecl.function_code = f;
}

/* Define a single builtin.  */

static void
define_builtin (enum built_in_function val, const char *name, tree prototype,
                const char *libname, int flags)
{
  tree decl;
  builtin_macro_definition bmd;

  decl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL, get_identifier (name),
                     prototype);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  SET_DECL_ASSEMBLER_NAME (decl, get_identifier (libname));
  m2block_pushDecl (decl);
  set_decl_built_in_class (decl, BUILT_IN_NORMAL);
  set_decl_function_code (decl, val);
  set_call_expr_flags (decl, flags);
  set_builtin_decl (val, decl, true);
  bmd.name = name;
  bmd.function_node = decl;
  bmd.return_node = TREE_TYPE (prototype);
  vec_safe_push (builtin_macros, bmd);
}

/* Define a math type variant of the builtin function.  */

static
void
define_builtin_ext  (enum built_in_function val, const char *name, tree type,
		     const char *libname, int flags, const char *ext)
{
  char *newname = (char *) xmalloc (strlen (name) + strlen (ext) + 1);
  char *newlibname = (char *) xmalloc (strlen (libname) + strlen (ext) + 1);
  strcpy (newname, name);
  strcat (newname, ext);
  strcpy (newlibname, libname);
  strcat (newlibname, ext);
  define_builtin (val, newname, type, newlibname, flags);
}

/* Define all support math type versions of this builtin.  */

static void
define_builtin_math (enum built_in_function val, const char *name, tree type,
		     const char *libname, int flags)
{
  /* SHORTREAL version.  */
  define_builtin_ext (val, name, type, libname, flags, "f");
  /* LONGREAL version.  */
  define_builtin_ext (val, name, type, libname, flags, "l");
  /* REAL version.  */
  define_builtin (val, name, type, libname, flags);
  /* Perhaps it should declare SYSTEM.def types size floating point
     versions as well?  */
}

void
m2builtins_init (location_t location)
{
  int i;

  m2block_pushGlobalScope ();
  endlink = void_list_node;
  sizetype_endlink = tree_cons (NULL_TREE, sizetype, endlink);
  math_endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  int_endlink = tree_cons (NULL_TREE, integer_type_node, NULL_TREE);
  ptr_endlink = tree_cons (NULL_TREE, ptr_type_node, NULL_TREE);
  const_ptr_endlink = tree_cons (NULL_TREE, const_ptr_type_node, NULL_TREE);
  unsigned_endlink = tree_cons (NULL_TREE, unsigned_type_node, NULL_TREE);

  float_ftype_void = build_function_type (float_type_node, math_endlink);
  double_ftype_void = build_function_type (double_type_node, math_endlink);
  ldouble_ftype_void
      = build_function_type (m2type_GetM2LongRealType (), math_endlink);

  long_doubleptr_type_node = build_pointer_type (m2type_GetM2LongRealType ());
  doubleptr_type_node = build_pointer_type (double_type_node);
  floatptr_type_node = build_pointer_type (float_type_node);

  float_ftype_float = build_function_type (
      float_type_node, tree_cons (NULL_TREE, float_type_node, math_endlink));

  double_ftype_double = build_function_type (
      double_type_node, tree_cons (NULL_TREE, double_type_node, math_endlink));

  ldouble_ftype_ldouble = build_function_type (
      m2type_GetM2LongRealType (),
      tree_cons (NULL_TREE, m2type_GetM2LongRealType (), endlink));

  builtin_ftype_int_var = build_function_type (
      integer_type_node, tree_cons (NULL_TREE, double_type_node, endlink));

  for (i = 0; list_of_builtins[i].name != NULL; i++)
    create_function_prototype (location, &list_of_builtins[i]);

  define_builtin (BUILT_IN_TRAP, "__builtin_trap",
                  build_function_type_list (void_type_node, NULL_TREE),
                  "__builtin_trap", ECF_NOTHROW | ECF_LEAF | ECF_NORETURN);
  define_builtin_math (BUILT_IN_ISGREATER, "isgreater", builtin_ftype_int_var,
		       "__builtin_isgreater", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
  define_builtin_math (BUILT_IN_ISGREATEREQUAL, "isgreaterequal",
		       builtin_ftype_int_var, "__builtin_isgreaterequal",
		       ECF_CONST | ECF_NOTHROW | ECF_LEAF);
  define_builtin_math (BUILT_IN_ISLESS, "isless", builtin_ftype_int_var,
		       "__builtin_isless", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
  define_builtin_math (BUILT_IN_ISLESSEQUAL, "islessequal", builtin_ftype_int_var,
		       "__builtin_islessequal", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
  define_builtin_math (BUILT_IN_ISLESSGREATER, "islessgreater",
		       builtin_ftype_int_var, "__builtin_islessgreater",
		       ECF_CONST | ECF_NOTHROW | ECF_LEAF);
  define_builtin_math (BUILT_IN_ISUNORDERED, "isunordered", builtin_ftype_int_var,
		       "__builtin_isunordered", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
  define_builtin_math (BUILT_IN_ISNORMAL, "isnormal", builtin_ftype_int_var,
		       "__builtin_isnormal", ECF_CONST | ECF_NOTHROW | ECF_LEAF);
  define_builtin_math (BUILT_IN_ISINF_SIGN, "isinf_sign", builtin_ftype_int_var,
		       "__builtin_isinf_sign", ECF_CONST | ECF_NOTHROW | ECF_LEAF);

  gm2_alloca_node = find_builtin_tree ("__builtin_alloca");
  gm2_memcpy_node = find_builtin_tree ("__builtin_memcpy");
  gm2_memset_node = find_builtin_tree ("__builtin_memset");
  gm2_huge_valf_node = find_builtin_tree ("__builtin_huge_valf");
  gm2_huge_val_node = find_builtin_tree ("__builtin_huge_val");
  gm2_huge_vall_node = find_builtin_tree ("__builtin_huge_vall");
  gm2_isfinite_node = find_builtin_tree ("__builtin_isfinite");
  gm2_isnan_node = find_builtin_tree ("__builtin_isnan");
  m2block_popGlobalScope ();
}

#include "gt-m2-m2builtins.h"

/* END m2builtins.  */
