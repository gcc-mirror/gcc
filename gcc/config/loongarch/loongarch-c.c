/* LoongArch-specific code for C family languages.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "c-family/c-common.h"
#include "cpplib.h"

#define preprocessing_asm_p() (cpp_get_options (pfile)->lang == CLK_ASM)
#define builtin_define(TXT) cpp_define (pfile, TXT)
#define builtin_assert(TXT) cpp_assert (pfile, TXT)

/* Define preprocessor macros for the -march and -mtune options.
   PREFIX is either _LOONGARCH_ARCH or _LOONGARCH_TUNE, INFO is
   the selected processor.  If INFO's canonical name is "foo",
   define PREFIX to be "foo", and define an additional macro
   PREFIX_FOO.  */
#define LARCH_CPP_SET_PROCESSOR(PREFIX, CPU_TYPE) \
  do \
    { \
      char *macro, *p; \
      int cpu_type = (CPU_TYPE); \
\
      if (cpu_type == CPU_NATIVE) \
	cpu_type = loongarch_native_cpu_type; \
\
      macro = concat ((PREFIX), "_", loongarch_cpu_strings[cpu_type], NULL); \
      for (p = macro; *p != 0; p++) \
	*p = TOUPPER (*p); \
\
      builtin_define (macro); \
      builtin_define_with_value ((PREFIX), loongarch_cpu_strings[cpu_type], 1); \
      free (macro); \
    } \
  while (0)

/* TODO: what is the pfile technique ??? !!! */

void
loongarch_cpu_cpp_builtins (cpp_reader *pfile)
{
  builtin_assert ("machine=loongarch");
  builtin_assert ("cpu=loongarch");
  builtin_define ("__loongarch__");

  if (TARGET_FLOAT64)
    builtin_define ("__loongarch_fpr=64");
  else
    builtin_define ("__loongarch_fpr=32");

  LARCH_CPP_SET_PROCESSOR ("_LOONGARCH_ARCH", loongarch_cpu_arch);
  LARCH_CPP_SET_PROCESSOR ("_LOONGARCH_TUNE", loongarch_cpu_tune);

  switch (loongarch_abi_int)
    {
    case ABI_LP64:
      builtin_define ("_ABILP64=3");
      builtin_define ("_LOONGARCH_SIM=_ABILP64");
      builtin_define ("__loongarch64");
      break;
    }

  builtin_define_with_int_value ("_LOONGARCH_SZINT", INT_TYPE_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_SZLONG", LONG_TYPE_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_SZPTR", POINTER_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_FPSET", 32 / MAX_FPRS_PER_FMT);
  builtin_define_with_int_value ("_LOONGARCH_SPFPSET", 32);

  /* These defines reflect the ABI in use, not whether the
     FPU is directly accessible.  */
  if (TARGET_HARD_FLOAT_ABI)
    builtin_define ("__loongarch_hard_float");
  else
    builtin_define ("__loongarch_soft_float");

  if (TARGET_SINGLE_FLOAT)
    builtin_define ("__loongarch_single_float");

  if (TARGET_FIX_LOONGSON3_LLSC)
    builtin_define ("__fix_loongson3_llsc");

  /* Macros dependent on the C dialect.  */
  if (preprocessing_asm_p ())
    {
      builtin_define_std ("LANGUAGE_ASSEMBLY");
      builtin_define ("_LANGUAGE_ASSEMBLY");
    }
  else if (c_dialect_cxx ())
    {
      builtin_define ("_LANGUAGE_C_PLUS_PLUS");
      builtin_define ("__LANGUAGE_C_PLUS_PLUS");
      builtin_define ("__LANGUAGE_C_PLUS_PLUS__");
    }
  else
    {
      builtin_define_std ("LANGUAGE_C");
      builtin_define ("_LANGUAGE_C");
    }
  if (c_dialect_objc ())
    {
      builtin_define ("_LANGUAGE_OBJECTIVE_C");
      builtin_define ("__LANGUAGE_OBJECTIVE_C");
      /* Bizarre, but retained for backwards compatibility.  */
      builtin_define_std ("LANGUAGE_C");
      builtin_define ("_LANGUAGE_C");
    }
}
