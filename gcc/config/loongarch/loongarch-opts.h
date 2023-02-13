/* Definitions for loongarch-specific option handling.
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

#ifndef LOONGARCH_OPTS_H
#define LOONGARCH_OPTS_H


/* Target configuration */
extern struct loongarch_target la_target;

/* Switch masks */
extern const int loongarch_switch_mask[];

#include "loongarch-def.h"

#if !defined(IN_LIBGCC2) && !defined(IN_TARGET_LIBS) && !defined(IN_RTS)
/* Handler for "-m" option combinations,
   shared by the driver and the compiler proper.  */
void
loongarch_config_target (struct loongarch_target *target,
			 HOST_WIDE_INT opt_switches,
			 int opt_arch, int opt_tune, int opt_fpu,
			 int opt_abi_base, int opt_abi_ext,
			 int opt_cmodel, int follow_multilib_list);
#endif


/* Macros for common conditional expressions used in loongarch.{c,h,md} */
#define TARGET_CMODEL_NORMAL	    (la_target.cmodel == CMODEL_NORMAL)
#define TARGET_CMODEL_TINY	    (la_target.cmodel == CMODEL_TINY)
#define TARGET_CMODEL_TINY_STATIC   (la_target.cmodel == CMODEL_TINY_STATIC)
#define TARGET_CMODEL_MEDIUM	    (la_target.cmodel == CMODEL_MEDIUM)
#define TARGET_CMODEL_LARGE	    (la_target.cmodel == CMODEL_LARGE)
#define TARGET_CMODEL_EXTREME	    (la_target.cmodel == CMODEL_EXTREME)

#define TARGET_HARD_FLOAT	    (la_target.isa.fpu != ISA_EXT_NOFPU)
#define TARGET_HARD_FLOAT_ABI	    (la_target.abi.base == ABI_BASE_LP64D \
				     || la_target.abi.base == ABI_BASE_LP64F)

#define TARGET_SOFT_FLOAT	  (la_target.isa.fpu == ISA_EXT_NOFPU)
#define TARGET_SOFT_FLOAT_ABI	  (la_target.abi.base == ABI_BASE_LP64S)
#define TARGET_SINGLE_FLOAT	  (la_target.isa.fpu == ISA_EXT_FPU32)
#define TARGET_SINGLE_FLOAT_ABI	  (la_target.abi.base == ABI_BASE_LP64F)
#define TARGET_DOUBLE_FLOAT	  (la_target.isa.fpu == ISA_EXT_FPU64)
#define TARGET_DOUBLE_FLOAT_ABI	  (la_target.abi.base == ABI_BASE_LP64D)

#define TARGET_64BIT		  (la_target.isa.base == ISA_BASE_LA64V100)
#define TARGET_ABI_LP64		  (la_target.abi.base == ABI_BASE_LP64D	\
				   || la_target.abi.base == ABI_BASE_LP64F \
				   || la_target.abi.base == ABI_BASE_LP64S)

#define TARGET_ARCH_NATIVE	  (la_target.cpu_arch == CPU_NATIVE)
#define LARCH_ACTUAL_ARCH	  (TARGET_ARCH_NATIVE \
				   ? (la_target.cpu_native < N_ARCH_TYPES \
				      ? (la_target.cpu_native) : (CPU_NATIVE)) \
				      : (la_target.cpu_arch))

#define TARGET_TUNE_NATIVE	(la_target.cpu_tune == CPU_NATIVE)
#define LARCH_ACTUAL_TUNE		(TARGET_TUNE_NATIVE \
				 ? (la_target.cpu_native < N_TUNE_TYPES \
				    ? (la_target.cpu_native) : (CPU_NATIVE)) \
				    : (la_target.cpu_tune))

#define TARGET_ARCH_LOONGARCH64	  (LARCH_ACTUAL_ARCH == CPU_LOONGARCH64)
#define TARGET_ARCH_LA464	  (LARCH_ACTUAL_ARCH == CPU_LA464)

#define TARGET_TUNE_LOONGARCH64	  (LARCH_ACTUAL_TUNE == CPU_LOONGARCH64)
#define TARGET_TUNE_LA464	  (LARCH_ACTUAL_TUNE == CPU_LA464)

/* Note: optimize_size may vary across functions,
   while -m[no]-memcpy imposes a global constraint.  */
#define TARGET_DO_OPTIMIZE_BLOCK_MOVE_P  loongarch_do_optimize_block_move_p()

#ifndef HAVE_AS_EXPLICIT_RELOCS
#define HAVE_AS_EXPLICIT_RELOCS 0
#endif

#endif /* LOONGARCH_OPTS_H */
