/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2023 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


#ifndef GCC_AARCH64_PROTOS_H
#define GCC_AARCH64_PROTOS_H

#include "input.h"
#include "config/arm/aarch-common.h"

/* SYMBOL_SMALL_ABSOLUTE: Generate symbol accesses through
   high and lo relocs that calculate the base address using a PC
   relative reloc.
   So to get the address of foo, we generate
   adrp x0, foo
   add  x0, x0, :lo12:foo

   To load or store something to foo, we could use the corresponding
   load store variants that generate an
   ldr x0, [x0,:lo12:foo]
   or
   str x1, [x0, :lo12:foo]

   This corresponds to the small code model of the compiler.

   SYMBOL_SMALL_GOT_4G: Similar to the one above but this
   gives us the GOT entry of the symbol being referred to :
   Thus calculating the GOT entry for foo is done using the
   following sequence of instructions.  The ADRP instruction
   gets us to the page containing the GOT entry of the symbol
   and the got_lo12 gets us the actual offset in it, together
   the base and offset, we can address 4G size GOT table.

   adrp  x0, :got:foo
   ldr   x0, [x0, :gotoff_lo12:foo]

   This corresponds to the small PIC model of the compiler.

   SYMBOL_SMALL_GOT_28K: Similar to SYMBOL_SMALL_GOT_4G, but used for symbol
   restricted within 28K GOT table size.

   ldr reg, [gp, #:gotpage_lo15:sym]

   This corresponds to -fpic model for small memory model of the compiler.

   SYMBOL_SMALL_TLSGD
   SYMBOL_SMALL_TLSDESC
   SYMBOL_SMALL_TLSIE
   SYMBOL_TINY_TLSIE
   SYMBOL_TLSLE12
   SYMBOL_TLSLE24
   SYMBOL_TLSLE32
   SYMBOL_TLSLE48
   Each of these represents a thread-local symbol, and corresponds to the
   thread local storage relocation operator for the symbol being referred to.

   SYMBOL_TINY_ABSOLUTE

   Generate symbol accesses as a PC relative address using a single
   instruction.  To compute the address of symbol foo, we generate:

   ADR x0, foo

   SYMBOL_TINY_GOT

   Generate symbol accesses via the GOT using a single PC relative
   instruction.  To compute the address of symbol foo, we generate:

   ldr t0, :got:foo

   The value of foo can subsequently read using:

   ldrb    t0, [t0]

   SYMBOL_FORCE_TO_MEM : Global variables are addressed using
   constant pool.  All variable addresses are spilled into constant
   pools.  The constant pools themselves are addressed using PC
   relative accesses.  This only works for the large code model.
 */
enum aarch64_symbol_type
{
  SYMBOL_SMALL_ABSOLUTE,
  SYMBOL_SMALL_GOT_28K,
  SYMBOL_SMALL_GOT_4G,
  SYMBOL_SMALL_TLSGD,
  SYMBOL_SMALL_TLSDESC,
  SYMBOL_SMALL_TLSIE,
  SYMBOL_TINY_ABSOLUTE,
  SYMBOL_TINY_GOT,
  SYMBOL_TINY_TLSIE,
  SYMBOL_TLSLE12,
  SYMBOL_TLSLE24,
  SYMBOL_TLSLE32,
  SYMBOL_TLSLE48,
  SYMBOL_FORCE_TO_MEM
};

/* Classifies the type of an address query.

   ADDR_QUERY_M
      Query what is valid for an "m" constraint and a memory_operand
      (the rules are the same for both).

   ADDR_QUERY_LDP_STP
      Query what is valid for a load/store pair.

   ADDR_QUERY_LDP_STP_N
      Query what is valid for a load/store pair, but narrow the incoming mode
      for address checking.  This is used for the store_pair_lanes patterns.

   ADDR_QUERY_ANY
      Query what is valid for at least one memory constraint, which may
      allow things that "m" doesn't.  For example, the SVE LDR and STR
      addressing modes allow a wider range of immediate offsets than "m"
      does.  */
enum aarch64_addr_query_type {
  ADDR_QUERY_M,
  ADDR_QUERY_LDP_STP,
  ADDR_QUERY_LDP_STP_N,
  ADDR_QUERY_ANY
};

/* Enumerates values that can be arbitrarily mixed into a calculation
   in order to make the result of the calculation unique to its use case.

   AARCH64_SALT_SSP_SET
   AARCH64_SALT_SSP_TEST
      Used when calculating the address of the stack protection canary value.
      There is a separate value for setting and testing the canary, meaning
      that these two operations produce unique addresses: they are different
      from each other, and from all other address calculations.

      The main purpose of this is to prevent the SET address being spilled
      to the stack and reloaded for the TEST, since that would give an
      attacker the opportunity to change the address of the expected
      canary value.  */
enum aarch64_salt_type {
  AARCH64_SALT_SSP_SET,
  AARCH64_SALT_SSP_TEST
};

/* A set of tuning parameters contains references to size and time
   cost models and vectors for address cost calculations, register
   move costs and memory move costs.  */

/* Scaled addressing modes can vary cost depending on the mode of the
   value to be loaded/stored.  QImode values cannot use scaled
   addressing modes.  */

struct scale_addr_mode_cost
{
  const int hi;
  const int si;
  const int di;
  const int ti;
};

/* Additional cost for addresses.  */
struct cpu_addrcost_table
{
  const struct scale_addr_mode_cost addr_scale_costs;
  const int pre_modify;
  const int post_modify;
  const int post_modify_ld3_st3;
  const int post_modify_ld4_st4;
  const int register_offset;
  const int register_sextend;
  const int register_zextend;
  const int imm_offset;
};

/* Additional costs for register copies.  Cost is for one register.  */
struct cpu_regmove_cost
{
  const int GP2GP;
  const int GP2FP;
  const int FP2GP;
  const int FP2FP;
};

struct simd_vec_cost
{
  /* Cost of any integer vector operation, excluding the ones handled
     specially below.  */
  const int int_stmt_cost;

  /* Cost of any fp vector operation, excluding the ones handled
     specially below.  */
  const int fp_stmt_cost;

  /* Per-vector cost of permuting vectors after an LD2, LD3 or LD4,
     as well as the per-vector cost of permuting vectors before
     an ST2, ST3 or ST4.  */
  const int ld2_st2_permute_cost;
  const int ld3_st3_permute_cost;
  const int ld4_st4_permute_cost;

  /* Cost of a permute operation.  */
  const int permute_cost;

  /* Cost of reductions for various vector types: iN is for N-bit
     integer elements and fN is for N-bit floating-point elements.
     We need to single out the element type because it affects the
     depth of the reduction.  */
  const int reduc_i8_cost;
  const int reduc_i16_cost;
  const int reduc_i32_cost;
  const int reduc_i64_cost;
  const int reduc_f16_cost;
  const int reduc_f32_cost;
  const int reduc_f64_cost;

  /* Additional cost of storing a single vector element, on top of the
     normal cost of a scalar store.  */
  const int store_elt_extra_cost;

  /* Cost of a vector-to-scalar operation.  */
  const int vec_to_scalar_cost;

  /* Cost of a scalar-to-vector operation.  */
  const int scalar_to_vec_cost;

  /* Cost of an aligned vector load.  */
  const int align_load_cost;

  /* Cost of an unaligned vector load.  */
  const int unalign_load_cost;

  /* Cost of an unaligned vector store.  */
  const int unalign_store_cost;

  /* Cost of a vector store.  */
  const int store_cost;
};

typedef struct simd_vec_cost advsimd_vec_cost;

/* SVE-specific extensions to the information provided by simd_vec_cost.  */
struct sve_vec_cost : simd_vec_cost
{
  CONSTEXPR sve_vec_cost (const simd_vec_cost &base,
			  unsigned int clast_cost,
			  unsigned int fadda_f16_cost,
			  unsigned int fadda_f32_cost,
			  unsigned int fadda_f64_cost,
			  unsigned int gather_load_x32_cost,
			  unsigned int gather_load_x64_cost,
			  unsigned int scatter_store_elt_cost)
    : simd_vec_cost (base),
      clast_cost (clast_cost),
      fadda_f16_cost (fadda_f16_cost),
      fadda_f32_cost (fadda_f32_cost),
      fadda_f64_cost (fadda_f64_cost),
      gather_load_x32_cost (gather_load_x32_cost),
      gather_load_x64_cost (gather_load_x64_cost),
      scatter_store_elt_cost (scatter_store_elt_cost)
  {}

  /* The cost of a vector-to-scalar CLASTA or CLASTB instruction,
     with the scalar being stored in FP registers.  This cost is
     assumed to be a cycle latency.  */
  const int clast_cost;

  /* The costs of FADDA for the three data types that it supports.
     These costs are assumed to be cycle latencies.  */
  const int fadda_f16_cost;
  const int fadda_f32_cost;
  const int fadda_f64_cost;

  /* The cost of a gather load instruction.  The x32 value is for loads
     of 32-bit elements and the x64 value is for loads of 64-bit elements.  */
  const int gather_load_x32_cost;
  const int gather_load_x64_cost;

  /* The per-element cost of a scatter store.  */
  const int scatter_store_elt_cost;
};

/* Base information about how the CPU issues code, containing
   information that is relevant to scalar, Advanced SIMD and SVE
   operations.

   The structure uses the general term "operation" to refer to
   whichever subdivision of an instruction makes sense for the CPU.
   These operations would typically be micro operations or macro
   operations.

   Note that this structure and the ones derived from it are only
   as general as they need to be for the CPUs that currently use them.
   They will probably need to be extended or refined as more CPUs are
   added.  */
struct aarch64_base_vec_issue_info
{
  /* How many loads and stores can be issued per cycle.  */
  const unsigned int loads_stores_per_cycle;

  /* How many stores can be issued per cycle.  */
  const unsigned int stores_per_cycle;

  /* How many integer or FP/SIMD operations can be issued per cycle.

     Currently we don't try to distinguish the two.  For vector code,
     we only really track FP/SIMD operations during vector costing;
     we don't for example try to cost arithmetic operations like
     address calculations, which are only decided later during ivopts.

     For scalar code, we effectively assume that code operates entirely
     on integers or entirely on floating-point values.  Again, we don't
     try to take address calculations into account.

     This is not very precise, but it's only meant to be a heuristic.
     We could certainly try to do better in future if there's an example
     of something that would benefit.  */
  const unsigned int general_ops_per_cycle;

  /* How many FP/SIMD operations to count for a floating-point or
     vector load operation.

     When constructing an Advanced SIMD vector from elements that have
     been loaded from memory, these values apply to each individual load.
     When using an SVE gather load, the values apply to each element of
     the gather.  */
  const unsigned int fp_simd_load_general_ops;

  /* How many FP/SIMD operations to count for a floating-point or
     vector store operation.

     When storing individual elements of an Advanced SIMD vector out to
     memory, these values apply to each individual store.  When using an
     SVE scatter store, these values apply to each element of the scatter.  */
  const unsigned int fp_simd_store_general_ops;
};

using aarch64_scalar_vec_issue_info = aarch64_base_vec_issue_info;

/* Base information about the issue stage for vector operations.
   This structure contains information that is relevant to both
   Advanced SIMD and SVE.  */
struct aarch64_simd_vec_issue_info : aarch64_base_vec_issue_info
{
  CONSTEXPR aarch64_simd_vec_issue_info (aarch64_base_vec_issue_info base,
					 unsigned int ld2_st2_general_ops,
					 unsigned int ld3_st3_general_ops,
					 unsigned int ld4_st4_general_ops)
    : aarch64_base_vec_issue_info (base),
      ld2_st2_general_ops (ld2_st2_general_ops),
      ld3_st3_general_ops (ld3_st3_general_ops),
      ld4_st4_general_ops (ld4_st4_general_ops)
  {}

  /* How many FP/SIMD operations to count for each vector loaded or
     stored by an LD[234] or ST[234] operation, in addition to the
     base costs given in the parent class.  For example, the full
     number of operations for an LD3 would be:

       load ops:    3
       general ops: 3 * (fp_simd_load_general_ops + ld3_st3_general_ops).  */
  const unsigned int ld2_st2_general_ops;
  const unsigned int ld3_st3_general_ops;
  const unsigned int ld4_st4_general_ops;
};

using aarch64_advsimd_vec_issue_info = aarch64_simd_vec_issue_info;

/* Information about the issue stage for SVE.  The main thing this adds
   is a concept of "predicate operations".  */
struct aarch64_sve_vec_issue_info : aarch64_simd_vec_issue_info
{
  CONSTEXPR aarch64_sve_vec_issue_info
    (aarch64_simd_vec_issue_info base,
     unsigned int pred_ops_per_cycle,
     unsigned int while_pred_ops,
     unsigned int int_cmp_pred_ops,
     unsigned int fp_cmp_pred_ops,
     unsigned int gather_scatter_pair_general_ops,
     unsigned int gather_scatter_pair_pred_ops)
    : aarch64_simd_vec_issue_info (base),
      pred_ops_per_cycle (pred_ops_per_cycle),
      while_pred_ops (while_pred_ops),
      int_cmp_pred_ops (int_cmp_pred_ops),
      fp_cmp_pred_ops (fp_cmp_pred_ops),
      gather_scatter_pair_general_ops (gather_scatter_pair_general_ops),
      gather_scatter_pair_pred_ops (gather_scatter_pair_pred_ops)
  {}

  /* How many predicate operations can be issued per cycle.  */
  const unsigned int pred_ops_per_cycle;

  /* How many predicate operations are generated by a WHILExx
     instruction.  */
  const unsigned int while_pred_ops;

  /* How many predicate operations are generated by an integer
     comparison instruction.  */
  const unsigned int int_cmp_pred_ops;

  /* How many predicate operations are generated by a floating-point
     comparison instruction.  */
  const unsigned int fp_cmp_pred_ops;

  /* How many general and predicate operations are generated by each pair
     of elements in a gather load or scatter store.  These values apply
     on top of the per-element counts recorded in fp_simd_load_general_ops
     and fp_simd_store_general_ops.

     The reason for using pairs is that that is the largest possible
     granule size for 128-bit SVE, which can load and store 2 64-bit
     elements or 4 32-bit elements.  */
  const unsigned int gather_scatter_pair_general_ops;
  const unsigned int gather_scatter_pair_pred_ops;
};

/* Information related to instruction issue for a particular CPU.  */
struct aarch64_vec_issue_info
{
  const aarch64_base_vec_issue_info *const scalar;
  const aarch64_simd_vec_issue_info *const advsimd;
  const aarch64_sve_vec_issue_info *const sve;
};

/* Cost for vector insn classes.  */
struct cpu_vector_cost
{
  /* Cost of any integer scalar operation, excluding load and store.  */
  const int scalar_int_stmt_cost;

  /* Cost of any fp scalar operation, excluding load and store.  */
  const int scalar_fp_stmt_cost;

  /* Cost of a scalar load.  */
  const int scalar_load_cost;

  /* Cost of a scalar store.  */
  const int scalar_store_cost;

  /* Cost of a taken branch.  */
  const int cond_taken_branch_cost;

  /* Cost of a not-taken branch.  */
  const int cond_not_taken_branch_cost;

  /* Cost of an Advanced SIMD operations.  */
  const advsimd_vec_cost *advsimd;

  /* Cost of an SVE operations, or null if SVE is not implemented.  */
  const sve_vec_cost *sve;

  /* Issue information, or null if none is provided.  */
  const aarch64_vec_issue_info *const issue_info;
};

/* Branch costs.  */
struct cpu_branch_cost
{
  const int predictable;    /* Predictable branch or optimizing for size.  */
  const int unpredictable;  /* Unpredictable branch or optimizing for speed.  */
};

/* Control approximate alternatives to certain FP operators.  */
#define AARCH64_APPROX_MODE(MODE) \
  ((MIN_MODE_FLOAT <= (MODE) && (MODE) <= MAX_MODE_FLOAT) \
   ? ((uint64_t) 1 << ((MODE) - MIN_MODE_FLOAT)) \
   : (MIN_MODE_VECTOR_FLOAT <= (MODE) && (MODE) <= MAX_MODE_VECTOR_FLOAT) \
     ? ((uint64_t) 1 << ((MODE) - MIN_MODE_VECTOR_FLOAT \
			 + MAX_MODE_FLOAT - MIN_MODE_FLOAT + 1)) \
     : (0))
#define AARCH64_APPROX_NONE ((uint64_t) 0)
#define AARCH64_APPROX_ALL (~(uint64_t) 0)

/* Allowed modes for approximations.  */
struct cpu_approx_modes
{
  const uint64_t division;	/* Division.  */
  const uint64_t sqrt;		/* Square root.  */
  const uint64_t recip_sqrt;	/* Reciprocal square root.  */
};

/* Cache prefetch settings for prefetch-loop-arrays.  */
struct cpu_prefetch_tune
{
  const int num_slots;
  const int l1_cache_size;
  const int l1_cache_line_size;
  const int l2_cache_size;
  /* Whether software prefetch hints should be issued for non-constant
     strides.  */
  const bool prefetch_dynamic_strides;
  /* The minimum constant stride beyond which we should use prefetch
     hints for.  */
  const int minimum_stride;
  const int default_opt_level;
};

/* Model the costs for loads/stores for the register allocators so that it can
   do more accurate spill heuristics.  */
struct cpu_memmov_cost
{
  int load_int;
  int store_int;
  int load_fp;
  int store_fp;
  int load_pred;
  int store_pred;
};

struct tune_params
{
  const struct cpu_cost_table *insn_extra_cost;
  const struct cpu_addrcost_table *addr_cost;
  const struct cpu_regmove_cost *regmove_cost;
  const struct cpu_vector_cost *vec_costs;
  const struct cpu_branch_cost *branch_costs;
  const struct cpu_approx_modes *approx_modes;
  /* A bitmask of the possible SVE register widths in bits,
     or SVE_NOT_IMPLEMENTED if not applicable.  Only used for tuning
     decisions, does not disable VLA vectorization.  */
  unsigned int sve_width;
  /* Structure used by reload to cost spills.  */
  struct cpu_memmov_cost memmov_cost;
  int issue_rate;
  unsigned int fusible_ops;
  const char *function_align;
  const char *jump_align;
  const char *loop_align;
  int int_reassoc_width;
  int fp_reassoc_width;
  int fma_reassoc_width;
  int vec_reassoc_width;
  int min_div_recip_mul_sf;
  int min_div_recip_mul_df;
  /* Value for aarch64_case_values_threshold; or 0 for the default.  */
  unsigned int max_case_values;
/* An enum specifying how to take into account CPU autoprefetch capabilities
   during instruction scheduling:
   - AUTOPREFETCHER_OFF: Do not take autoprefetch capabilities into account.
   - AUTOPREFETCHER_WEAK: Attempt to sort sequences of loads/store in order of
   offsets but allow the pipeline hazard recognizer to alter that order to
   maximize multi-issue opportunities.
   - AUTOPREFETCHER_STRONG: Attempt to sort sequences of loads/store in order of
   offsets and prefer this even if it restricts multi-issue opportunities.  */

  enum aarch64_autoprefetch_model
  {
    AUTOPREFETCHER_OFF,
    AUTOPREFETCHER_WEAK,
    AUTOPREFETCHER_STRONG
  } autoprefetcher_model;

  unsigned int extra_tuning_flags;

  /* Place prefetch struct pointer at the end to enable type checking
     errors when tune_params misses elements (e.g., from erroneous merges).  */
  const struct cpu_prefetch_tune *prefetch;
};

/* Classifies an address.

   ADDRESS_REG_IMM
       A simple base register plus immediate offset.

   ADDRESS_REG_WB
       A base register indexed by immediate offset with writeback.

   ADDRESS_REG_REG
       A base register indexed by (optionally scaled) register.

   ADDRESS_REG_UXTW
       A base register indexed by (optionally scaled) zero-extended register.

   ADDRESS_REG_SXTW
       A base register indexed by (optionally scaled) sign-extended register.

   ADDRESS_LO_SUM
       A LO_SUM rtx with a base register and "LO12" symbol relocation.

   ADDRESS_SYMBOLIC:
       A constant symbolic address, in pc-relative literal pool.  */

enum aarch64_address_type {
  ADDRESS_REG_IMM,
  ADDRESS_REG_WB,
  ADDRESS_REG_REG,
  ADDRESS_REG_UXTW,
  ADDRESS_REG_SXTW,
  ADDRESS_LO_SUM,
  ADDRESS_SYMBOLIC
};

/* Address information.  */
struct aarch64_address_info {
  enum aarch64_address_type type;
  rtx base;
  rtx offset;
  poly_int64 const_offset;
  int shift;
  enum aarch64_symbol_type symbol_type;
};

#define AARCH64_FUSION_PAIR(x, name) \
  AARCH64_FUSE_##name##_index, 
/* Supported fusion operations.  */
enum aarch64_fusion_pairs_index
{
#include "aarch64-fusion-pairs.def"
  AARCH64_FUSE_index_END
};

#define AARCH64_FUSION_PAIR(x, name) \
  AARCH64_FUSE_##name = (1u << AARCH64_FUSE_##name##_index),
/* Supported fusion operations.  */
enum aarch64_fusion_pairs
{
  AARCH64_FUSE_NOTHING = 0,
#include "aarch64-fusion-pairs.def"
  AARCH64_FUSE_ALL = (1u << AARCH64_FUSE_index_END) - 1
};

#define AARCH64_EXTRA_TUNING_OPTION(x, name) \
  AARCH64_EXTRA_TUNE_##name##_index,
/* Supported tuning flags indexes.  */
enum aarch64_extra_tuning_flags_index
{
#include "aarch64-tuning-flags.def"
  AARCH64_EXTRA_TUNE_index_END
};


#define AARCH64_EXTRA_TUNING_OPTION(x, name) \
  AARCH64_EXTRA_TUNE_##name = (1u << AARCH64_EXTRA_TUNE_##name##_index),
/* Supported tuning flags.  */
enum aarch64_extra_tuning_flags
{
  AARCH64_EXTRA_TUNE_NONE = 0,
#include "aarch64-tuning-flags.def"
  AARCH64_EXTRA_TUNE_ALL = (1u << AARCH64_EXTRA_TUNE_index_END) - 1
};

/* Enum to distinguish which type of check is to be done in
   aarch64_simd_valid_immediate.  This is used as a bitmask where
   AARCH64_CHECK_MOV has both bits set.  Thus AARCH64_CHECK_MOV will
   perform all checks.  Adding new types would require changes accordingly.  */
enum simd_immediate_check {
  AARCH64_CHECK_ORR  = 1 << 0,
  AARCH64_CHECK_BIC  = 1 << 1,
  AARCH64_CHECK_MOV  = AARCH64_CHECK_ORR | AARCH64_CHECK_BIC
};

extern struct tune_params aarch64_tune_params;

/* The available SVE predicate patterns, known in the ACLE as "svpattern".  */
#define AARCH64_FOR_SVPATTERN(T) \
  T (POW2, pow2, 0) \
  T (VL1, vl1, 1) \
  T (VL2, vl2, 2) \
  T (VL3, vl3, 3) \
  T (VL4, vl4, 4) \
  T (VL5, vl5, 5) \
  T (VL6, vl6, 6) \
  T (VL7, vl7, 7) \
  T (VL8, vl8, 8) \
  T (VL16, vl16, 9) \
  T (VL32, vl32, 10) \
  T (VL64, vl64, 11) \
  T (VL128, vl128, 12) \
  T (VL256, vl256, 13) \
  T (MUL4, mul4, 29) \
  T (MUL3, mul3, 30) \
  T (ALL, all, 31)

/* The available SVE prefetch operations, known in the ACLE as "svprfop".  */
#define AARCH64_FOR_SVPRFOP(T) \
  T (PLDL1KEEP, pldl1keep, 0) \
  T (PLDL1STRM, pldl1strm, 1) \
  T (PLDL2KEEP, pldl2keep, 2) \
  T (PLDL2STRM, pldl2strm, 3) \
  T (PLDL3KEEP, pldl3keep, 4) \
  T (PLDL3STRM, pldl3strm, 5) \
  T (PSTL1KEEP, pstl1keep, 8) \
  T (PSTL1STRM, pstl1strm, 9) \
  T (PSTL2KEEP, pstl2keep, 10) \
  T (PSTL2STRM, pstl2strm, 11) \
  T (PSTL3KEEP, pstl3keep, 12) \
  T (PSTL3STRM, pstl3strm, 13)

#define AARCH64_SVENUM(UPPER, LOWER, VALUE) AARCH64_SV_##UPPER = VALUE,
enum aarch64_svpattern {
  AARCH64_FOR_SVPATTERN (AARCH64_SVENUM)
  AARCH64_NUM_SVPATTERNS
};

enum aarch64_svprfop {
  AARCH64_FOR_SVPRFOP (AARCH64_SVENUM)
  AARCH64_NUM_SVPRFOPS
};
#undef AARCH64_SVENUM

/* It's convenient to divide the built-in function codes into groups,
   rather than having everything in a single enum.  This type enumerates
   those groups.  */
enum aarch64_builtin_class
{
  AARCH64_BUILTIN_GENERAL,
  AARCH64_BUILTIN_SVE
};

/* Built-in function codes are structured so that the low
   AARCH64_BUILTIN_SHIFT bits contain the aarch64_builtin_class
   and the upper bits contain a group-specific subcode.  */
const unsigned int AARCH64_BUILTIN_SHIFT = 1;

/* Mask that selects the aarch64_builtin_class part of a function code.  */
const unsigned int AARCH64_BUILTIN_CLASS = (1 << AARCH64_BUILTIN_SHIFT) - 1;

/* RAII class for enabling enough features to define built-in types
   and implement the arm_neon.h pragma.  */
class aarch64_simd_switcher
{
public:
  aarch64_simd_switcher (aarch64_feature_flags extra_flags = 0);
  ~aarch64_simd_switcher ();

private:
  aarch64_feature_flags m_old_asm_isa_flags;
  bool m_old_general_regs_only;
};

void aarch64_post_cfi_startproc (void);
poly_int64 aarch64_initial_elimination_offset (unsigned, unsigned);
int aarch64_get_condition_code (rtx);
bool aarch64_address_valid_for_prefetch_p (rtx, bool);
bool aarch64_bitmask_imm (unsigned HOST_WIDE_INT val, machine_mode);
unsigned HOST_WIDE_INT aarch64_and_split_imm1 (HOST_WIDE_INT val_in);
unsigned HOST_WIDE_INT aarch64_and_split_imm2 (HOST_WIDE_INT val_in);
bool aarch64_and_bitmask_imm (unsigned HOST_WIDE_INT val_in, machine_mode mode);
int aarch64_branch_cost (bool, bool);
enum aarch64_symbol_type aarch64_classify_symbolic_expression (rtx);
bool aarch64_advsimd_struct_mode_p (machine_mode mode);
opt_machine_mode aarch64_vq_mode (scalar_mode);
opt_machine_mode aarch64_full_sve_mode (scalar_mode);
bool aarch64_can_const_movi_rtx_p (rtx x, machine_mode mode);
bool aarch64_const_vec_all_same_int_p (rtx, HOST_WIDE_INT);
bool aarch64_const_vec_all_same_in_range_p (rtx, HOST_WIDE_INT,
					    HOST_WIDE_INT);
bool aarch64_constant_address_p (rtx);
bool aarch64_emit_approx_div (rtx, rtx, rtx);
bool aarch64_emit_approx_sqrt (rtx, rtx, bool);
tree aarch64_vector_load_decl (tree);
void aarch64_expand_call (rtx, rtx, rtx, bool);
bool aarch64_expand_cpymem (rtx *);
bool aarch64_expand_setmem (rtx *);
bool aarch64_float_const_zero_rtx_p (rtx);
bool aarch64_float_const_rtx_p (rtx);
bool aarch64_function_arg_regno_p (unsigned);
bool aarch64_fusion_enabled_p (enum aarch64_fusion_pairs);
bool aarch64_gen_cpymemqi (rtx *);
bool aarch64_is_extend_from_extract (scalar_int_mode, rtx, rtx);
bool aarch64_is_long_call_p (rtx);
bool aarch64_is_noplt_call_p (rtx);
bool aarch64_label_mentioned_p (rtx);
void aarch64_declare_function_name (FILE *, const char*, tree);
void aarch64_asm_output_alias (FILE *, const tree, const tree);
void aarch64_asm_output_external (FILE *, tree, const char*);
bool aarch64_legitimate_pic_operand_p (rtx);
bool aarch64_mask_and_shift_for_ubfiz_p (scalar_int_mode, rtx, rtx);
bool aarch64_masks_and_shift_for_bfi_p (scalar_int_mode, unsigned HOST_WIDE_INT,
					unsigned HOST_WIDE_INT,
					unsigned HOST_WIDE_INT);
bool aarch64_zero_extend_const_eq (machine_mode, rtx, machine_mode, rtx);
bool aarch64_move_imm (unsigned HOST_WIDE_INT, machine_mode);
machine_mode aarch64_sve_int_mode (machine_mode);
opt_machine_mode aarch64_sve_pred_mode (unsigned int);
machine_mode aarch64_sve_pred_mode (machine_mode);
opt_machine_mode aarch64_sve_data_mode (scalar_mode, poly_uint64);
bool aarch64_sve_mode_p (machine_mode);
HOST_WIDE_INT aarch64_fold_sve_cnt_pat (aarch64_svpattern, unsigned int);
bool aarch64_sve_cnt_immediate_p (rtx);
bool aarch64_sve_scalar_inc_dec_immediate_p (rtx);
bool aarch64_sve_addvl_addpl_immediate_p (rtx);
bool aarch64_sve_vector_inc_dec_immediate_p (rtx);
int aarch64_add_offset_temporaries (rtx);
void aarch64_split_add_offset (scalar_int_mode, rtx, rtx, rtx, rtx, rtx);
bool aarch64_mov_operand_p (rtx, machine_mode);
rtx aarch64_reverse_mask (machine_mode, unsigned int);
bool aarch64_offset_7bit_signed_scaled_p (machine_mode, poly_int64);
bool aarch64_offset_9bit_signed_unscaled_p (machine_mode, poly_int64);
char *aarch64_output_sve_prefetch (const char *, rtx, const char *);
char *aarch64_output_sve_cnt_immediate (const char *, const char *, rtx);
char *aarch64_output_sve_cnt_pat_immediate (const char *, const char *, rtx *);
char *aarch64_output_sve_scalar_inc_dec (rtx);
char *aarch64_output_sve_addvl_addpl (rtx);
char *aarch64_output_sve_vector_inc_dec (const char *, rtx);
char *aarch64_output_scalar_simd_mov_immediate (rtx, scalar_int_mode);
char *aarch64_output_simd_mov_immediate (rtx, unsigned,
			enum simd_immediate_check w = AARCH64_CHECK_MOV);
char *aarch64_output_sve_mov_immediate (rtx);
char *aarch64_output_sve_ptrues (rtx);
bool aarch64_pad_reg_upward (machine_mode, const_tree, bool);
bool aarch64_regno_ok_for_base_p (int, bool);
bool aarch64_regno_ok_for_index_p (int, bool);
bool aarch64_reinterpret_float_as_int (rtx value, unsigned HOST_WIDE_INT *fail);
bool aarch64_simd_check_vect_par_cnst_half (rtx op, machine_mode mode,
					    bool high);
bool aarch64_simd_scalar_immediate_valid_for_move (rtx, scalar_int_mode);
bool aarch64_simd_shift_imm_p (rtx, machine_mode, bool);
bool aarch64_sve_ptrue_svpattern_p (rtx, struct simd_immediate_info *);
bool aarch64_simd_valid_immediate (rtx, struct simd_immediate_info *,
			enum simd_immediate_check w = AARCH64_CHECK_MOV);
rtx aarch64_check_zero_based_sve_index_immediate (rtx);
bool aarch64_sve_index_immediate_p (rtx);
bool aarch64_sve_arith_immediate_p (machine_mode, rtx, bool);
bool aarch64_sve_sqadd_sqsub_immediate_p (machine_mode, rtx, bool);
bool aarch64_sve_bitmask_immediate_p (rtx);
bool aarch64_sve_dup_immediate_p (rtx);
bool aarch64_sve_cmp_immediate_p (rtx, bool);
bool aarch64_sve_float_arith_immediate_p (rtx, bool);
bool aarch64_sve_float_mul_immediate_p (rtx);
bool aarch64_split_dimode_const_store (rtx, rtx);
bool aarch64_symbolic_address_p (rtx);
bool aarch64_uimm12_shift (unsigned HOST_WIDE_INT);
int aarch64_movk_shift (const wide_int_ref &, const wide_int_ref &);
bool aarch64_is_mov_xn_imm (unsigned HOST_WIDE_INT);
bool aarch64_use_return_insn_p (void);
const char *aarch64_output_casesi (rtx *);

unsigned int aarch64_tlsdesc_abi_id ();
enum aarch64_symbol_type aarch64_classify_symbol (rtx, HOST_WIDE_INT);
enum aarch64_symbol_type aarch64_classify_tls_symbol (rtx);
enum reg_class aarch64_regno_regclass (unsigned);
int aarch64_asm_preferred_eh_data_format (int, int);
int aarch64_fpconst_pow_of_2 (rtx);
int aarch64_fpconst_pow2_recip (rtx);
machine_mode aarch64_hard_regno_caller_save_mode (unsigned, unsigned,
						       machine_mode);
int aarch64_uxt_size (int, HOST_WIDE_INT);
int aarch64_vec_fpconst_pow_of_2 (rtx);
rtx aarch64_eh_return_handler_rtx (void);
rtx aarch64_mask_from_zextract_ops (rtx, rtx);
const char *aarch64_output_move_struct (rtx *operands);
rtx aarch64_return_addr_rtx (void);
rtx aarch64_return_addr (int, rtx);
rtx aarch64_simd_gen_const_vector_dup (machine_mode, HOST_WIDE_INT);
rtx aarch64_gen_shareable_zero (machine_mode);
bool aarch64_simd_mem_operand_p (rtx);
bool aarch64_sve_ld1r_operand_p (rtx);
bool aarch64_sve_ld1rq_operand_p (rtx);
bool aarch64_sve_ld1ro_operand_p (rtx, scalar_mode);
bool aarch64_sve_ldff1_operand_p (rtx);
bool aarch64_sve_ldnf1_operand_p (rtx);
bool aarch64_sve_ldr_operand_p (rtx);
bool aarch64_sve_prefetch_operand_p (rtx, machine_mode);
bool aarch64_sve_struct_memory_operand_p (rtx);
rtx aarch64_simd_vect_par_cnst_half (machine_mode, int, bool);
rtx aarch64_gen_stepped_int_parallel (unsigned int, int, int);
bool aarch64_stepped_int_parallel_p (rtx, int);
rtx aarch64_tls_get_addr (void);
unsigned aarch64_debugger_regno (unsigned);
unsigned aarch64_trampoline_size (void);
void aarch64_asm_output_labelref (FILE *, const char *);
void aarch64_cpu_cpp_builtins (cpp_reader *);
const char * aarch64_gen_far_branch (rtx *, int, const char *, const char *);
const char * aarch64_output_probe_stack_range (rtx, rtx);
const char * aarch64_output_probe_sve_stack_clash (rtx, rtx, rtx, rtx);
void aarch64_err_no_fpadvsimd (machine_mode);
void aarch64_expand_epilogue (bool);
rtx aarch64_ptrue_all (unsigned int);
opt_machine_mode aarch64_ptrue_all_mode (rtx);
rtx aarch64_convert_sve_data_to_pred (rtx, machine_mode, rtx);
rtx aarch64_expand_sve_dupq (rtx, machine_mode, rtx);
void aarch64_expand_mov_immediate (rtx, rtx);
rtx aarch64_stack_protect_canary_mem (machine_mode, rtx, aarch64_salt_type);
rtx aarch64_ptrue_reg (machine_mode);
rtx aarch64_pfalse_reg (machine_mode);
bool aarch64_sve_same_pred_for_ptest_p (rtx *, rtx *);
void aarch64_emit_sve_pred_move (rtx, rtx, rtx);
void aarch64_expand_sve_mem_move (rtx, rtx, machine_mode);
bool aarch64_maybe_expand_sve_subreg_move (rtx, rtx);
rtx aarch64_replace_reg_mode (rtx, machine_mode);
void aarch64_split_sve_subreg_move (rtx, rtx, rtx);
void aarch64_expand_prologue (void);
void aarch64_expand_vector_init (rtx, rtx);
void aarch64_sve_expand_vector_init (rtx, rtx);
void aarch64_init_cumulative_args (CUMULATIVE_ARGS *, const_tree, rtx,
				   const_tree, unsigned, bool = false);
void aarch64_init_expanders (void);
void aarch64_emit_call_insn (rtx);
void aarch64_register_pragmas (void);
void aarch64_relayout_simd_types (void);
void aarch64_reset_previous_fndecl (void);
bool aarch64_return_address_signing_enabled (void);
void aarch64_save_restore_target_globals (tree);
void aarch64_addti_scratch_regs (rtx, rtx, rtx *,
				 rtx *, rtx *,
				 rtx *, rtx *,
				 rtx *);
void aarch64_subvti_scratch_regs (rtx, rtx, rtx *,
				  rtx *, rtx *,
				  rtx *, rtx *, rtx *);
void aarch64_expand_subvti (rtx, rtx, rtx,
			    rtx, rtx, rtx, rtx, bool);


/* Initialize builtins for SIMD intrinsics.  */
void init_aarch64_simd_builtins (void);

void aarch64_simd_emit_reg_reg_move (rtx *, machine_mode, unsigned int);

/* Expand builtins for SIMD intrinsics.  */
rtx aarch64_simd_expand_builtin (int, tree, rtx);

void aarch64_simd_lane_bounds (rtx, HOST_WIDE_INT, HOST_WIDE_INT, const_tree);
rtx aarch64_endian_lane_rtx (machine_mode, unsigned int);

void aarch64_split_128bit_move (rtx, rtx);

bool aarch64_split_128bit_move_p (rtx, rtx);

bool aarch64_mov128_immediate (rtx);

void aarch64_split_simd_move (rtx, rtx);

/* Check for a legitimate floating point constant for FMOV.  */
bool aarch64_float_const_representable_p (rtx);

extern int aarch64_epilogue_uses (int);

#if defined (RTX_CODE)
void aarch64_gen_unlikely_cbranch (enum rtx_code, machine_mode cc_mode,
				   rtx label_ref);
bool aarch64_legitimate_address_p (machine_mode, rtx, bool,
				   aarch64_addr_query_type = ADDR_QUERY_M);
machine_mode aarch64_select_cc_mode (RTX_CODE, rtx, rtx);
rtx aarch64_gen_compare_reg (RTX_CODE, rtx, rtx);
bool aarch64_maxmin_plus_const (rtx_code, rtx *, bool);
rtx aarch64_load_tp (rtx);

void aarch64_expand_compare_and_swap (rtx op[]);
void aarch64_split_compare_and_swap (rtx op[]);

void aarch64_split_atomic_op (enum rtx_code, rtx, rtx, rtx, rtx, rtx, rtx);

bool aarch64_gen_adjusted_ldpstp (rtx *, bool, machine_mode, RTX_CODE);

void aarch64_expand_sve_vec_cmp_int (rtx, rtx_code, rtx, rtx);
bool aarch64_expand_sve_vec_cmp_float (rtx, rtx_code, rtx, rtx, bool);
void aarch64_expand_sve_vcond (machine_mode, machine_mode, rtx *);

bool aarch64_prepare_sve_int_fma (rtx *, rtx_code);
bool aarch64_prepare_sve_cond_int_fma (rtx *, rtx_code);
#endif /* RTX_CODE */

bool aarch64_process_target_attr (tree);
void aarch64_override_options_internal (struct gcc_options *);

const char *aarch64_general_mangle_builtin_type (const_tree);
void aarch64_general_init_builtins (void);
tree aarch64_general_fold_builtin (unsigned int, tree, unsigned int, tree *);
gimple *aarch64_general_gimple_fold_builtin (unsigned int, gcall *,
					     gimple_stmt_iterator *);
rtx aarch64_general_expand_builtin (unsigned int, tree, rtx, int);
tree aarch64_general_builtin_decl (unsigned, bool);
tree aarch64_general_builtin_rsqrt (unsigned int);
void handle_arm_acle_h (void);
void handle_arm_neon_h (void);

namespace aarch64_sve {
  void init_builtins ();
  void handle_arm_sve_h ();
  tree builtin_decl (unsigned, bool);
  bool builtin_type_p (const_tree);
  bool builtin_type_p (const_tree, unsigned int *, unsigned int *);
  const char *mangle_builtin_type (const_tree);
  tree resolve_overloaded_builtin (location_t, unsigned int,
				   vec<tree, va_gc> *);
  bool check_builtin_call (location_t, vec<location_t>, unsigned int,
			   tree, unsigned int, tree *);
  gimple *gimple_fold_builtin (unsigned int, gimple_stmt_iterator *, gcall *);
  rtx expand_builtin (unsigned int, tree, rtx);
  tree handle_arm_sve_vector_bits_attribute (tree *, tree, tree, int, bool *);
#ifdef GCC_TARGET_H
  bool verify_type_context (location_t, type_context_kind, const_tree, bool);
#endif
}

extern void aarch64_split_combinev16qi (rtx operands[3]);
extern void aarch64_expand_vec_perm (rtx, rtx, rtx, rtx, unsigned int);
extern void aarch64_expand_sve_vec_perm (rtx, rtx, rtx, rtx);
extern bool aarch64_madd_needs_nop (rtx_insn *);
extern void aarch64_final_prescan_insn (rtx_insn *);
void aarch64_atomic_assign_expand_fenv (tree *, tree *, tree *);
int aarch64_ccmp_mode_to_code (machine_mode mode);

bool extract_base_offset_in_addr (rtx mem, rtx *base, rtx *offset);
bool aarch64_mergeable_load_pair_p (machine_mode, rtx, rtx);
bool aarch64_operands_ok_for_ldpstp (rtx *, bool, machine_mode);
bool aarch64_operands_adjust_ok_for_ldpstp (rtx *, bool, machine_mode);
void aarch64_swap_ldrstr_operands (rtx *, bool);

extern void aarch64_asm_output_pool_epilogue (FILE *, const char *,
					      tree, HOST_WIDE_INT);


extern bool aarch64_classify_address (struct aarch64_address_info *, rtx,
				      machine_mode, bool,
				      aarch64_addr_query_type = ADDR_QUERY_M);

void aarch64_set_asm_isa_flags (aarch64_feature_flags);

/* Defined in common/config/aarch64-common.cc.  */
void aarch64_set_asm_isa_flags (gcc_options *, aarch64_feature_flags);
bool aarch64_handle_option (struct gcc_options *, struct gcc_options *,
			     const struct cl_decoded_option *, location_t);
const char *aarch64_rewrite_selected_cpu (const char *name);
enum aarch_parse_opt_result aarch64_parse_extension (const char *,
                                                     aarch64_feature_flags *,
                                                     std::string *);
void aarch64_get_all_extension_candidates (auto_vec<const char *> *candidates);
std::string aarch64_get_extension_string_for_isa_flags (aarch64_feature_flags,
							aarch64_feature_flags);

rtl_opt_pass *make_pass_fma_steering (gcc::context *);
rtl_opt_pass *make_pass_track_speculation (gcc::context *);
rtl_opt_pass *make_pass_tag_collision_avoidance (gcc::context *);
rtl_opt_pass *make_pass_insert_bti (gcc::context *ctxt);
rtl_opt_pass *make_pass_cc_fusion (gcc::context *ctxt);

poly_uint64 aarch64_regmode_natural_size (machine_mode);

bool aarch64_high_bits_all_ones_p (HOST_WIDE_INT);

struct atomic_ool_names
{
    const char *str[5][5];
};

rtx aarch64_atomic_ool_func(machine_mode mode, rtx model_rtx,
			    const atomic_ool_names *names);
extern const atomic_ool_names aarch64_ool_swp_names;
extern const atomic_ool_names aarch64_ool_ldadd_names;
extern const atomic_ool_names aarch64_ool_ldset_names;
extern const atomic_ool_names aarch64_ool_ldclr_names;
extern const atomic_ool_names aarch64_ool_ldeor_names;

tree aarch64_resolve_overloaded_builtin_general (location_t, tree, void *);

const char *aarch64_sls_barrier (int);
const char *aarch64_indirect_call_asm (rtx);
extern bool aarch64_harden_sls_retbr_p (void);
extern bool aarch64_harden_sls_blr_p (void);

extern void aarch64_output_patchable_area (unsigned int, bool);

#endif /* GCC_AARCH64_PROTOS_H */
