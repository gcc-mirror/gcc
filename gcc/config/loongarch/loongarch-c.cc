/* LoongArch-specific code for C family languages.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.

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
#define LARCH_CPP_SET_PROCESSOR(PREFIX, CPU_TYPE)			\
  do									\
    {									\
      char *macro, *p;							\
      int cpu_type = (CPU_TYPE);					\
									\
      macro = concat ((PREFIX), "_",					\
		      loongarch_cpu_strings[cpu_type], NULL);		\
      for (p = macro; *p != 0; p++)					\
	*p = TOUPPER (*p);						\
									\
      builtin_define (macro);						\
      builtin_define_with_value ((PREFIX),				\
				 loongarch_cpu_strings[cpu_type], 1);	\
      free (macro);							\
    }									\
  while (0)

void
loongarch_cpu_cpp_builtins (cpp_reader *pfile)
{
  builtin_assert ("machine=loongarch");
  builtin_assert ("cpu=loongarch");
  builtin_define ("__loongarch__");

  LARCH_CPP_SET_PROCESSOR ("_LOONGARCH_ARCH", LARCH_ACTUAL_ARCH);
  LARCH_CPP_SET_PROCESSOR ("_LOONGARCH_TUNE", LARCH_ACTUAL_TUNE);

  /* Base architecture / ABI.  */
  if (TARGET_64BIT)
    {
      builtin_define ("__loongarch_grlen=64");
      builtin_define ("__loongarch64");
    }

  if (TARGET_ABI_LP64)
    {
      builtin_define ("_ABILP64=3");
      builtin_define ("_LOONGARCH_SIM=_ABILP64");
      builtin_define ("__loongarch_lp64");
    }

  /* These defines reflect the ABI in use, not whether the
     FPU is directly accessible.  */
  if (TARGET_DOUBLE_FLOAT_ABI)
    builtin_define ("__loongarch_double_float=1");
  else if (TARGET_SINGLE_FLOAT_ABI)
    builtin_define ("__loongarch_single_float=1");

  if (TARGET_DOUBLE_FLOAT_ABI || TARGET_SINGLE_FLOAT_ABI)
    builtin_define ("__loongarch_hard_float=1");
  else
    builtin_define ("__loongarch_soft_float=1");


  /* ISA Extensions.  */
  if (TARGET_DOUBLE_FLOAT)
    builtin_define ("__loongarch_frlen=64");
  else if (TARGET_SINGLE_FLOAT)
    builtin_define ("__loongarch_frlen=32");
  else
    builtin_define ("__loongarch_frlen=0");

  /* Native Data Sizes.  */
  builtin_define_with_int_value ("_LOONGARCH_SZINT", INT_TYPE_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_SZLONG", LONG_TYPE_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_SZPTR", POINTER_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_FPSET", 32);
  builtin_define_with_int_value ("_LOONGARCH_SPFPSET", 32);

}
