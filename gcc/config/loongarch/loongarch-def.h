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
#define ISA_EXT_NOFPU	      0
#define ISA_EXT_FPU32	      1
#define ISA_EXT_FPU64	      2
#define N_ISA_EXT_FPU_TYPES   3
#define N_ISA_EXT_TYPES	      3

/* enum abi_base */
extern const char* loongarch_abi_base_strings[];
#define ABI_BASE_LP64D	      0
#define ABI_BASE_LP64F	      1
#define ABI_BASE_LP64S	      2
#define N_ABI_BASE_TYPES      3

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

/* enum switches */
/* The "SW_" codes represent command-line switches (options that
   accept no parameters). Definition for other switches that affects
   the target ISA / ABI configuration will also be appended here
   in the future.  */

extern const char* loongarch_switch_strings[];
#define SW_SOFT_FLOAT	      0
#define SW_SINGLE_FLOAT	      1
#define SW_DOUBLE_FLOAT	      2
#define N_SWITCH_TYPES	      3

/* The common default value for variables whose assignments
   are triggered by command-line options.  */

#define M_OPTION_NOT_SEEN -1
#define M_OPT_ABSENT(opt_enum)  ((opt_enum) == M_OPTION_NOT_SEEN)


/* Internal representation of the target.  */
struct loongarch_isa
{
  unsigned char base;	    /* ISA_BASE_ */
  unsigned char fpu;	    /* ISA_EXT_FPU_ */
};

struct loongarch_abi
{
  unsigned char base;	    /* ABI_BASE_ */
  unsigned char ext;	    /* ABI_EXT_ */
};

struct loongarch_target
{
  struct loongarch_isa isa;
  struct loongarch_abi abi;
  unsigned char cpu_arch;   /* CPU_ */
  unsigned char cpu_tune;   /* same */
  unsigned char cpu_native; /* same */
  unsigned char cmodel;	    /* CMODEL_ */
};

/* CPU properties.  */
/* index */
#define CPU_NATIVE	  0
#define CPU_LOONGARCH64	  1
#define CPU_LA464	  2
#define N_ARCH_TYPES	  3
#define N_TUNE_TYPES	  3

/* parallel tables.  */
extern const char* loongarch_cpu_strings[];
extern struct loongarch_isa loongarch_cpu_default_isa[];
extern int loongarch_cpu_issue_rate[];
extern int loongarch_cpu_multipass_dfa_lookahead[];

extern struct loongarch_cache loongarch_cpu_cache[];
extern struct loongarch_rtx_cost_data loongarch_cpu_rtx_cost_data[];

#ifdef __cplusplus
}
#endif
#endif /* LOONGARCH_DEF_H */
