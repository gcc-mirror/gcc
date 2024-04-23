/* LoongArch static properties.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"

#include "loongarch-def.h"
#include "loongarch-str.h"

template <class T, int N>
using array = loongarch_def_array<T, N>;

template <class T>
using array_arch = array<T, N_ARCH_TYPES>;

template <class T>
using array_tune = array<T, N_TUNE_TYPES>;

array_arch<const char *> loongarch_arch_strings = array_arch<const char *> ()
  .set (ARCH_NATIVE, STR_CPU_NATIVE)
  .set (ARCH_ABI_DEFAULT, STR_ARCH_ABI_DEFAULT)
  .set (ARCH_LOONGARCH64, STR_CPU_LOONGARCH64)
  .set (ARCH_LA464, STR_CPU_LA464)
  .set (ARCH_LA664, STR_CPU_LA664)
  .set (ARCH_LA64V1_0, STR_ARCH_LA64V1_0)
  .set (ARCH_LA64V1_1, STR_ARCH_LA64V1_1);

array_tune<const char *> loongarch_tune_strings = array_tune<const char *> ()
  .set (TUNE_NATIVE, STR_CPU_NATIVE)
  .set (TUNE_GENERIC, STR_TUNE_GENERIC)
  .set (TUNE_LOONGARCH64, STR_CPU_LOONGARCH64)
  .set (TUNE_LA464, STR_CPU_LA464)
  .set (TUNE_LA664, STR_CPU_LA664);

array_arch<loongarch_isa> loongarch_cpu_default_isa =
  array_arch<loongarch_isa> ()
    .set (ARCH_LOONGARCH64,
	  loongarch_isa ()
	    .base_ (ISA_BASE_LA64)
	    .fpu_ (ISA_EXT_FPU64))

    .set (ARCH_LA464,
	  loongarch_isa ()
	    .base_ (ISA_BASE_LA64)
	    .fpu_ (ISA_EXT_FPU64)
	    .simd_ (ISA_EXT_SIMD_LASX))

    .set (ARCH_LA664,
	  loongarch_isa ()
	    .base_ (ISA_BASE_LA64)
	    .fpu_ (ISA_EXT_FPU64)
	    .simd_ (ISA_EXT_SIMD_LASX)
	    .evolution_ (OPTION_MASK_ISA_DIV32 | OPTION_MASK_ISA_LD_SEQ_SA
			 | OPTION_MASK_ISA_LAM_BH | OPTION_MASK_ISA_LAMCAS
			 | OPTION_MASK_ISA_FRECIPE))
    .set (ARCH_LA64V1_0,
	  loongarch_isa ()
	    .base_ (ISA_BASE_LA64)
	    .fpu_ (ISA_EXT_FPU64)
	    .simd_ (ISA_EXT_SIMD_LSX))

    .set (ARCH_LA64V1_1,
	  loongarch_isa ()
	    .base_ (ISA_BASE_LA64)
	    .fpu_ (ISA_EXT_FPU64)
	    .simd_ (ISA_EXT_SIMD_LSX)
	    .evolution_ (OPTION_MASK_ISA_DIV32 | OPTION_MASK_ISA_LD_SEQ_SA
			 | OPTION_MASK_ISA_LAM_BH | OPTION_MASK_ISA_LAMCAS
			 | OPTION_MASK_ISA_FRECIPE));


static inline loongarch_cache la464_cache ()
{
  return loongarch_cache ()
    .l1d_line_size_ (64)
    .l1d_size_ (64)
    .l2d_size_ (256)
    .simultaneous_prefetches_ (4);
}

array_tune<loongarch_cache> loongarch_cpu_cache =
  array_tune<loongarch_cache> ()
    .set (TUNE_GENERIC, la464_cache ())
    .set (TUNE_LOONGARCH64, la464_cache ())
    .set (TUNE_LA464, la464_cache ())
    .set (TUNE_LA664, la464_cache ());

static inline loongarch_align la464_align ()
{
  return loongarch_align ().function_ ("32").loop_ ("16").jump_ ("16");
}

static inline loongarch_align la664_align ()
{
  return loongarch_align ().function_ ("8").loop_ ("8").jump_ ("32");
}

array_tune<loongarch_align> loongarch_cpu_align =
  array_tune<loongarch_align> ()
    .set (TUNE_GENERIC, la664_align ())
    .set (TUNE_LOONGARCH64, la664_align ())
    .set (TUNE_LA464, la464_align ())
    .set (TUNE_LA664, la664_align ());

/* Default RTX cost initializer.  */
loongarch_rtx_cost_data::loongarch_rtx_cost_data ()
  : fp_add (COSTS_N_INSNS (5)),
    fp_mult_sf (COSTS_N_INSNS (5)),
    fp_mult_df (COSTS_N_INSNS (5)),
    fp_div_sf (COSTS_N_INSNS (8)),
    fp_div_df (COSTS_N_INSNS (8)),
    int_mult_si (COSTS_N_INSNS (4)),
    int_mult_di (COSTS_N_INSNS (4)),
    int_div_si (COSTS_N_INSNS (5)),
    int_div_di (COSTS_N_INSNS (5)),
    movcf2gr (COSTS_N_INSNS (7)),
    movgr2cf (COSTS_N_INSNS (15)),
    branch_cost (6),
    memory_latency (4) {}

/* The following properties cannot be looked up directly using "cpucfg".
 So it is necessary to provide a default value for "unknown native"
 tune targets (i.e. -mtune=native while PRID does not correspond to
 any known "-mtune" type).  */
array_tune<loongarch_rtx_cost_data> loongarch_cpu_rtx_cost_data =
  array_tune<loongarch_rtx_cost_data> ()
    .set (TUNE_LA664,
	  loongarch_rtx_cost_data ()
	    .movcf2gr_ (COSTS_N_INSNS (1))
	    .movgr2cf_ (COSTS_N_INSNS (1)));

/* RTX costs to use when optimizing for size.
   We use a value slightly larger than COSTS_N_INSNS (1) for all of them
   because they are slower than simple instructions.  */
#define COST_COMPLEX_INSN (COSTS_N_INSNS (1) + 1)
const loongarch_rtx_cost_data loongarch_rtx_cost_optimize_size =
  loongarch_rtx_cost_data ()
    .fp_add_ (COST_COMPLEX_INSN)
    .fp_mult_sf_ (COST_COMPLEX_INSN)
    .fp_mult_df_ (COST_COMPLEX_INSN)
    .fp_div_sf_ (COST_COMPLEX_INSN)
    .fp_div_df_ (COST_COMPLEX_INSN)
    .int_mult_si_ (COST_COMPLEX_INSN)
    .int_mult_di_ (COST_COMPLEX_INSN)
    .int_div_si_ (COST_COMPLEX_INSN)
    .int_div_di_ (COST_COMPLEX_INSN)
    .movcf2gr_ (COST_COMPLEX_INSN);

array_tune<int> loongarch_cpu_issue_rate = array_tune<int> ()
  .set (TUNE_NATIVE, 4)
  .set (TUNE_GENERIC, 4)
  .set (TUNE_LOONGARCH64, 4)
  .set (TUNE_LA464, 4)
  .set (TUNE_LA664, 6);

array_tune<int> loongarch_cpu_multipass_dfa_lookahead = array_tune<int> ()
  .set (TUNE_NATIVE, 4)
  .set (TUNE_GENERIC, 4)
  .set (TUNE_LOONGARCH64, 4)
  .set (TUNE_LA464, 4)
  .set (TUNE_LA664, 6);

/* Wiring string definitions from loongarch-str.h to global arrays
   with standard index values from loongarch-opts.h, so we can
   print config-related messages and do ABI self-spec filtering
   from the driver in a self-consistent manner.  */

array<const char *, N_ISA_BASE_TYPES> loongarch_isa_base_strings =
  array<const char *, N_ISA_BASE_TYPES> ()
    .set (ISA_BASE_LA64, STR_ISA_BASE_LA64);

array<const char *, N_ISA_EXT_TYPES> loongarch_isa_ext_strings =
  array<const char *, N_ISA_EXT_TYPES> ()
    .set (ISA_EXT_NONE, STR_NONE)
    .set (ISA_EXT_FPU32, STR_ISA_EXT_FPU32)
    .set (ISA_EXT_FPU64, STR_ISA_EXT_FPU64)
    .set (ISA_EXT_SIMD_LSX, STR_ISA_EXT_LSX)
    .set (ISA_EXT_SIMD_LASX, STR_ISA_EXT_LASX);

array<const char *, N_ABI_BASE_TYPES> loongarch_abi_base_strings =
  array<const char *, N_ABI_BASE_TYPES> ()
    .set (ABI_BASE_LP64D, STR_ABI_BASE_LP64D)
    .set (ABI_BASE_LP64F, STR_ABI_BASE_LP64F)
    .set (ABI_BASE_LP64S, STR_ABI_BASE_LP64S);

array<const char *, N_ABI_EXT_TYPES> loongarch_abi_ext_strings =
  array<const char *, N_ABI_EXT_TYPES> ()
    .set (ABI_EXT_BASE, STR_ABI_EXT_BASE);

array<const char *, N_CMODEL_TYPES> loongarch_cmodel_strings =
  array<const char *, N_CMODEL_TYPES> ()
    .set (CMODEL_NORMAL,		STR_CMODEL_NORMAL)
    .set (CMODEL_TINY,		STR_CMODEL_TINY)
    .set (CMODEL_TINY_STATIC,	STR_CMODEL_TS)
    .set (CMODEL_MEDIUM,		STR_CMODEL_MEDIUM)
    .set (CMODEL_LARGE,		STR_CMODEL_LARGE)
    .set (CMODEL_EXTREME,		STR_CMODEL_EXTREME);

array<array<loongarch_isa, N_ABI_EXT_TYPES>, N_ABI_BASE_TYPES>
  abi_minimal_isa = array<array<loongarch_isa, N_ABI_EXT_TYPES>,
			  N_ABI_BASE_TYPES> ()
    .set (ABI_BASE_LP64D,
	  array<loongarch_isa, N_ABI_EXT_TYPES> ()
	    .set (ABI_EXT_BASE,
		  loongarch_isa ()
		    .base_ (ISA_BASE_LA64)
		    .fpu_ (ISA_EXT_FPU64)))
    .set (ABI_BASE_LP64F,
	  array<loongarch_isa, N_ABI_EXT_TYPES> ()
	    .set (ABI_EXT_BASE,
		  loongarch_isa ()
		    .base_ (ISA_BASE_LA64)
		    .fpu_ (ISA_EXT_FPU32)))
    .set (ABI_BASE_LP64S,
	  array<loongarch_isa, N_ABI_EXT_TYPES> ()
	    .set (ABI_EXT_BASE,
		  loongarch_isa ().base_ (ISA_BASE_LA64)));
