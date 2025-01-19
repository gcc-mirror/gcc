/* LoongArch-specific code for C family languages.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

void
loongarch_cpu_cpp_builtins (cpp_reader *pfile)
{
  builtin_assert ("machine=loongarch");
  builtin_assert ("cpu=loongarch");
  builtin_define ("__loongarch__");

  builtin_define_with_value ("__loongarch_arch",
			     loongarch_arch_strings[la_target.cpu_arch], 1);

  builtin_define_with_value ("__loongarch_tune",
			     loongarch_tune_strings[la_target.cpu_tune], 1);

  builtin_define_with_value ("_LOONGARCH_ARCH",
			     loongarch_arch_strings[la_target.cpu_arch], 1);

  builtin_define_with_value ("_LOONGARCH_TUNE",
			     loongarch_tune_strings[la_target.cpu_tune], 1);

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

  if (TARGET_HARD_FLOAT && ISA_HAS_FRECIPE)
    builtin_define ("__loongarch_frecipe");

  if (ISA_HAS_LSX)
    {
      builtin_define ("__loongarch_simd");
      builtin_define ("__loongarch_sx");

      if (!ISA_HAS_LASX)
	builtin_define ("__loongarch_simd_width=128");
    }

  if (ISA_HAS_LASX)
    {
      builtin_define ("__loongarch_asx");
      builtin_define ("__loongarch_simd_width=256");
    }

  /* ISA evolution features */
  int max_v_major = 1, max_v_minor = 0;

  for (int i = 0; i < N_EVO_FEATURES; i++)
    if (la_target.isa.evolution & la_evo_feature_masks[i])
      {
	builtin_define (la_evo_macro_name[i]);

	int major = la_evo_version_major[i],
	    minor = la_evo_version_minor[i];

	max_v_major = major > max_v_major ? major : max_v_major;
	max_v_minor = major == max_v_major
	  ? (minor > max_v_minor ? minor : max_v_minor) : max_v_minor;
      }

  /* Find the minimum ISA version required to run the target program.  */
  if (!(max_v_major == 1 && max_v_minor <= 1 && ISA_HAS_LASX))
    {
      builtin_define_with_int_value ("__loongarch_version_major", max_v_major);
      builtin_define_with_int_value ("__loongarch_version_minor", max_v_minor);
    }

  /* Add support for FLOAT128_TYPE on the LoongArch architecture.  */
  builtin_define ("__FLOAT128_TYPE__");

  /* Map the old _Float128 'q' builtins into the new 'f128' builtins.  */
  builtin_define ("__builtin_fabsq=__builtin_fabsf128");
  builtin_define ("__builtin_copysignq=__builtin_copysignf128");
  builtin_define ("__builtin_nanq=__builtin_nanf128");
  builtin_define ("__builtin_nansq=__builtin_nansf128");
  builtin_define ("__builtin_infq=__builtin_inff128");
  builtin_define ("__builtin_huge_valq=__builtin_huge_valf128");

  /* Native Data Sizes.  */
  builtin_define_with_int_value ("_LOONGARCH_SZINT", INT_TYPE_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_SZLONG", LONG_TYPE_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_SZPTR", POINTER_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_FPSET", 32);
  builtin_define_with_int_value ("_LOONGARCH_SPFPSET", 32);

}
