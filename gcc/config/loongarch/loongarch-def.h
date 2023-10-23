/* LoongArch definitions.
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

/* Definition of standard codes for:
    - base architecture types	(isa_base),
    - ISA extensions		(isa_ext),
    - base ABI types		(abi_base),
    - ABI extension types	(abi_ext).

    - code models		      (cmodel)
    - other command-line switches     (switch)

   These values are primarily used for implementing option handling
   logic in "loongarch.opt", "loongarch-driver.c" and "loongarch-opt.c".

   As for the result of this option handling process, the following
   scheme is adopted to represent the final configuration:

    - The target ABI is encoded with a tuple (abi_base, abi_ext)
      using the code defined below.

    - The target ISA is encoded with a "struct loongarch_isa" defined
      in loongarch-cpu.h.

    - The target microarchitecture is represented with a cpu model
      index defined in loongarch-cpu.h.
*/

#ifndef LOONGARCH_DEF_H
#define LOONGARCH_DEF_H

#include "loongarch-tune.h"

#ifdef __cplusplus
extern "C" {
#endif

/* enum isa_base */
extern const char* loongarch_isa_base_strings[];
#define ISA_BASE_LA64V100     0
#define N_ISA_BASE_TYPES      1

/* enum isa_ext_* */
extern const char* loongarch_isa_ext_strings[];
#define ISA_EXT_NONE	      0
#define ISA_EXT_FPU32	      1
#define ISA_EXT_FPU64	      2
#define N_ISA_EXT_FPU_TYPES   3
#define ISA_EXT_SIMD_LSX      3
#define ISA_EXT_SIMD_LASX     4
#define N_ISA_EXT_TYPES	      5

/* enum abi_base */
extern const char* loongarch_abi_base_strings[];
#define ABI_BASE_LP64D	      0
#define ABI_BASE_LP64F	      1
#define ABI_BASE_LP64S	      2
#define N_ABI_BASE_TYPES      3

#define TO_LP64_ABI_BASE(C) (C)

#define ABI_FPU_64(abi_base) \
  (abi_base == ABI_BASE_LP64D)
#define ABI_FPU_32(abi_base) \
  (abi_base == ABI_BASE_LP64F)
#define ABI_FPU_NONE(abi_base) \
  (abi_base == ABI_BASE_LP64S)


/* enum abi_ext */
extern const char* loongarch_abi_ext_strings[];
#define ABI_EXT_BASE	      0
#define N_ABI_EXT_TYPES	      1

/* enum cmodel */
extern const char* loongarch_cmodel_strings[];
#define CMODEL_NORMAL	      0
#define CMODEL_TINY	      1
#define CMODEL_TINY_STATIC    2
#define CMODEL_MEDIUM	      3
#define CMODEL_LARGE	      4
#define CMODEL_EXTREME	      5
#define N_CMODEL_TYPES	      6

/* enum explicit_relocs */
#define EXPLICIT_RELOCS_AUTO	0
#define EXPLICIT_RELOCS_NONE	1
#define EXPLICIT_RELOCS_ALWAYS	2
#define N_EXPLICIT_RELOCS_TYPES	3

/* The common default value for variables whose assignments
   are triggered by command-line options.  */

#define M_OPT_UNSET -1
#define M_OPT_ABSENT(opt_enum)  ((opt_enum) == M_OPT_UNSET)


/* Internal representation of the target.  */
struct loongarch_isa
{
  int base;	    /* ISA_BASE_ */
  int fpu;	    /* ISA_EXT_FPU_ */
  int simd;	    /* ISA_EXT_SIMD_ */
};

struct loongarch_abi
{
  int base;	    /* ABI_BASE_ */
  int ext;	    /* ABI_EXT_ */
};

struct loongarch_target
{
  struct loongarch_isa isa;
  struct loongarch_abi abi;
  int cpu_arch;	    /* CPU_ */
  int cpu_tune;	    /* same */
  int cmodel;	    /* CMODEL_ */
};

/* CPU properties.  */
/* index */
#define CPU_NATIVE	  0
#define CPU_ABI_DEFAULT   1
#define CPU_LOONGARCH64	  2
#define CPU_LA464	  3
#define N_ARCH_TYPES	  4
#define N_TUNE_TYPES	  4

/* parallel tables.  */
extern const char* loongarch_cpu_strings[];
extern struct loongarch_isa loongarch_cpu_default_isa[];
extern int loongarch_cpu_issue_rate[];
extern int loongarch_cpu_multipass_dfa_lookahead[];

extern struct loongarch_cache loongarch_cpu_cache[];
extern struct loongarch_align loongarch_cpu_align[];
extern struct loongarch_rtx_cost_data loongarch_cpu_rtx_cost_data[];

#ifdef __cplusplus
}
#endif
#endif /* LOONGARCH_DEF_H */
