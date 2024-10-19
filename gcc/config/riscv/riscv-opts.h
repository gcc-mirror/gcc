/* Definition of RISC-V target for GNU compiler.
   Copyright (C) 2016-2024 Free Software Foundation, Inc.
   Contributed by Andrew Waterman (andrew@sifive.com).

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

#ifndef GCC_RISCV_OPTS_H
#define GCC_RISCV_OPTS_H

enum riscv_abi_type {
  ABI_ILP32,
  ABI_ILP32E,
  ABI_ILP32F,
  ABI_ILP32D,
  ABI_LP64,
  ABI_LP64E,
  ABI_LP64F,
  ABI_LP64D
};
extern enum riscv_abi_type riscv_abi;

enum riscv_code_model {
  CM_MEDLOW,
  CM_MEDANY,
  CM_LARGE,
  CM_PIC
};
extern enum riscv_code_model riscv_cmodel;

enum riscv_isa_spec_class {
  ISA_SPEC_CLASS_NONE,

  ISA_SPEC_CLASS_2P2,
  ISA_SPEC_CLASS_20190608,
  ISA_SPEC_CLASS_20191213
};

extern enum riscv_isa_spec_class riscv_isa_spec;

/* Keep this list in sync with define_attr "tune" in riscv.md.  */
enum riscv_microarchitecture_type {
  generic,
  sifive_7,
  sifive_p400,
  sifive_p600,
  xiangshan,
  generic_ooo
};
extern enum riscv_microarchitecture_type riscv_microarchitecture;

enum riscv_align_data {
  riscv_align_data_type_xlen,
  riscv_align_data_type_natural
};

/* Where to get the canary for the stack protector.  */
enum stack_protector_guard {
  SSP_TLS,			/* per-thread canary in TLS block */
  SSP_GLOBAL			/* global canary */
};

/* RISC-V auto-vectorization RVV LMUL.  */
enum rvv_max_lmul_enum {
  RVV_M1 = 1,
  RVV_M2 = 2,
  RVV_M4 = 4,
  RVV_M8 = 8,
  /* For dynamic LMUL, we compare COST start with LMUL8.  */
  RVV_DYNAMIC = 9
};

enum riscv_multilib_select_kind {
  /* Select multilib by builtin way.  */
  select_by_builtin,
  /* Select multilib by ABI, arch and code model.  */
  select_by_abi_arch_cmodel,
  /* Select multilib by ABI only.  */
  select_by_abi,
};

/* ENTITIES in mode switching.  */
enum riscv_entity
{
  RISCV_VXRM = 0,
  RISCV_FRM,
  MAX_RISCV_ENTITIES
};

/* RISC-V stringop strategy. */
enum stringop_strategy_enum {
  /* No expansion. */
  STRATEGY_LIBCALL = 1,
  /* Use scalar expansion if possible. */
  STRATEGY_SCALAR = 2,
  /* Only vector expansion if possible. */
  STRATEGY_VECTOR = 4,
  /* Use any. */
  STRATEGY_AUTO = STRATEGY_SCALAR | STRATEGY_VECTOR
};

/* Behavior of VSETVL Pass.  */
enum vsetvl_strategy_enum {
  /* Optimized: Run LCM dataflow analysis to reduce vsetvl* insns and
     delete any redundant ones generated in the process.  */
  VSETVL_OPT,
  /* Simple: Insert a vsetvl* instruction for each Vector instruction.  */
  VSETVL_SIMPLE,
  /* No fusion: Disable Phase 2 earliest global fusion.  */
  VSETVL_OPT_NO_FUSION,
};

/* RVV vector bits for option -mrvv-vector-bits, default is scalable.  */
enum rvv_vector_bits_enum {
  /* scalable indicates taking the value of zvl*b as the minimal vlen.  */
  RVV_VECTOR_BITS_SCALABLE,
  /* zvl indicates taking the value of zvl*b as the exactly vlen.  */
  RVV_VECTOR_BITS_ZVL,
};

#define TARGET_ZICOND_LIKE (TARGET_ZICOND || (TARGET_XVENTANACONDOPS && TARGET_64BIT))

/* Bit of riscv_zvl_flags will set continually, N-1 bit will set if N-bit is
   set, e.g. MASK_ZVL64B has set then MASK_ZVL32B is set, so we can use
   popcount to calculate the minimal VLEN.  */
#define TARGET_MIN_VLEN \
  ((riscv_zvl_flags == 0) \
   ? 0 \
   : 32 << (__builtin_popcount (riscv_zvl_flags) - 1))

/* Same as TARGET_MIN_VLEN, but take an OPTS as gcc_options.  */
#define TARGET_MIN_VLEN_OPTS(opts)                                             \
  ((opts->x_riscv_zvl_flags == 0)                                              \
     ? 0                                                                       \
     : 32 << (__builtin_popcount (opts->x_riscv_zvl_flags) - 1))

/* The maximum LMUL according to user configuration.  */
#define TARGET_MAX_LMUL                                                        \
  (int) (rvv_max_lmul == RVV_DYNAMIC ? RVV_M8 : rvv_max_lmul)

/* TLS types.  */
enum riscv_tls_type {
  TLS_TRADITIONAL,
  TLS_DESCRIPTORS
};

/* On some microarchitectures, vector segment loads and stores are excessively
   expensive, so predicate the generation of those instrunctions.  */
#define TARGET_VECTOR_AUTOVEC_SEGMENT					       \
  (TARGET_VECTOR && riscv_mautovec_segment)

#endif /* ! GCC_RISCV_OPTS_H */
