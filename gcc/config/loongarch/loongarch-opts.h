/* Definitions for loongarch-specific option handling.
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

#ifndef LOONGARCH_OPTS_H
#define LOONGARCH_OPTS_H

/* Enum value definitions for various ABI/ISA options,
   these values are not used directly. (see loongarch.opt) */

/* enum loongarch_isa_int */
#define ISA_LA64          0
#define N_INT_ISA_TYPES   1

/* enum loongarch_isa_float */
#define ISA_SOFT_FLOAT    0
#define ISA_SINGLE_FLOAT  1
#define ISA_DOUBLE_FLOAT  2
#define N_FLOAT_ISA_TYPES 3

/* enum loongarch_abi_int */
#define ABI_LP64          0
#define N_INT_ABI_TYPES   1

/* enum loongarch_abi_float */
#define ABI_SOFT_FLOAT    0
#define ABI_SINGLE_FLOAT  1
#define ABI_DOUBLE_FLOAT  2
#define N_FLOAT_ABI_TYPES 3

extern const char* loongarch_isa_int_strings[];
extern const char* loongarch_isa_float_strings[];
extern const char* loongarch_abi_int_strings[];
extern const char* loongarch_abi_float_strings[];

/* "Option not encountered" common default value (for computing override) */
#define M_OPTION_NOT_SEEN -1
#define LARCH_OPT_ABSENT(opt_enum)  ((opt_enum) == M_OPTION_NOT_SEEN)

/* CPU properties */
#include "loongarch-cpu.h"

/* "Default-default" init values for ABI/ISA option enum variables,
   optionally overridden by config.gcc and "--with" config
   options via "tm_defines".  */

#ifndef DEFAULT_CPU_ARCH
#define DEFAULT_CPU_ARCH CPU_LOONGARCH64
#endif

#ifndef DEFAULT_ISA_INT
#define DEFAULT_ISA_INT M_OPTION_NOT_SEEN
#endif

#ifndef DEFAULT_ISA_FLOAT
#define DEFAULT_ISA_FLOAT M_OPTION_NOT_SEEN
#endif

#ifndef DEFAULT_ABI_INT
#define DEFAULT_ABI_INT M_OPTION_NOT_SEEN
#endif

#ifndef DEFAULT_ABI_FLOAT
#define DEFAULT_ABI_FLOAT M_OPTION_NOT_SEEN
#endif

/* Default-default values for ISA extensions */
#ifndef DEFAULT_EXT_fix_lns3_llsc
#define DEFAULT_EXT_fix_lns3_llsc M_OPTION_NOT_SEEN
#endif

#ifndef IN_LIBGCC2
/* Handler for "-m" option combinations,
   shared by the driver and the compiler proper.
*/
void
loongarch_handle_m_option_combinations (
  int* cpu_arch, int* cpu_tune, int* isa_int, int* isa_float,
  int* abi_int, int* abi_float, int* native_cpu_type,
  int* ext_fix_lns3_llsc);
#endif


/* Enum value definitions for loongarch code models.  */
#define CMODEL_NORMAL             0
#define CMODEL_TINY               1
#define CMODEL_TINY_STATIC        2
#define CMODEL_LARGE              3
#define CMODEL_EXTREME            4


/* Macros for common conditional expressions used in loongarch.{c,md} */
#define TARGET_CMODEL_NORMAL        (loongarch_cmodel_var == CMODEL_NORMAL)
#define TARGET_CMODEL_TINY          (loongarch_cmodel_var == CMODEL_TINY)
#define TARGET_CMODEL_TINY_STATIC   (loongarch_cmodel_var == CMODEL_TINY_STATIC)
#define TARGET_CMODEL_LARGE         (loongarch_cmodel_var == CMODEL_LARGE)
#define TARGET_CMODEL_EXTREME       (loongarch_cmodel_var == CMODEL_EXTREME)


/* Legacy conditional expression macros (TODO: cleanup) */
#define TARGET_HARD_FLOAT      (loongarch_isa_float != ISA_SOFT_FLOAT)
#define TARGET_HARD_FLOAT_ABI  (loongarch_abi_float != ABI_SOFT_FLOAT)
#define TARGET_SOFT_FLOAT      (loongarch_isa_float == ISA_SOFT_FLOAT)
#define TARGET_SOFT_FLOAT_ABI  (loongarch_abi_float == ABI_SOFT_FLOAT)
#define TARGET_SINGLE_FLOAT    (loongarch_abi_float == ABI_SINGLE_FLOAT)
#define TARGET_DOUBLE_FLOAT    (loongarch_abi_float == ABI_DOUBLE_FLOAT)
#define TARGET_FLOAT64         \
  (loongarch_isa_float == ISA_DOUBLE_FLOAT || loongarch_isa_float == ISA_SOFT_FLOAT)
#define TARGET_ABI_LP64        (loongarch_abi_int == ABI_LP64)
#define TARGET_64BIT           (loongarch_isa_int == ISA_LA64)

#define TARGET_ARCH_NATIVE          (loongarch_cpu_arch == CPU_NATIVE)
#define TARGET_ARCH_LOONGARCH64     (loongarch_cpu_arch == CPU_LOONGARCH64)
#define TARGET_ARCH_GS464V          (loongarch_cpu_arch == CPU_GS464V)

#define TARGET_TUNE_NATIVE          (loongarch_cpu_tune == CPU_NATIVE)
#define TARGET_TUNE_LOONGARCH64     (loongarch_cpu_tune == CPU_LOONGARCH64)
#define TARGET_TUNE_GS464V          (loongarch_cpu_tune == CPU_GS464V)

#define TARGET_FIX_LOONGSON3_LLSC   (loongarch_ext_fix_lns3_llsc)

#endif /* LOONGARCH_OPTS_H */
