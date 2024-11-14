/* Builtins' description for AArch64 SIMD architecture.
   Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "function.h"
#include "basic-block.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "memmodel.h"
#include "tm_p.h"
#include "expmed.h"
#include "optabs.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "gimple-iterator.h"
#include "case-cfn-macros.h"
#include "emit-rtl.h"
#include "stringpool.h"
#include "attribs.h"
#include "gimple-fold.h"
#include "builtins.h"
#include "aarch64-builtins.h"

#define v8qi_UP  E_V8QImode
#define v8di_UP  E_V8DImode
#define v4hi_UP  E_V4HImode
#define v4hf_UP  E_V4HFmode
#define v2si_UP  E_V2SImode
#define v2sf_UP  E_V2SFmode
#define v1df_UP  E_V1DFmode
#define v1di_UP  E_V1DImode
#define di_UP    E_DImode
#define df_UP    E_DFmode
#define v16qi_UP E_V16QImode
#define v8hi_UP  E_V8HImode
#define v8hf_UP  E_V8HFmode
#define v4si_UP  E_V4SImode
#define v4sf_UP  E_V4SFmode
#define v2di_UP  E_V2DImode
#define v2df_UP  E_V2DFmode
#define ti_UP	 E_TImode
#define oi_UP	 E_OImode
#define ci_UP	 E_CImode
#define xi_UP	 E_XImode
#define si_UP    E_SImode
#define sf_UP    E_SFmode
#define hi_UP    E_HImode
#define hf_UP    E_HFmode
#define qi_UP    E_QImode
#define bf_UP    E_BFmode
#define v4bf_UP  E_V4BFmode
#define v8bf_UP  E_V8BFmode
#define v2x8qi_UP  E_V2x8QImode
#define v2x4hi_UP  E_V2x4HImode
#define v2x4hf_UP  E_V2x4HFmode
#define v2x4bf_UP  E_V2x4BFmode
#define v2x2si_UP  E_V2x2SImode
#define v2x2sf_UP  E_V2x2SFmode
#define v2x1di_UP  E_V2x1DImode
#define v2x1df_UP  E_V2x1DFmode
#define v2x16qi_UP E_V2x16QImode
#define v2x8hi_UP  E_V2x8HImode
#define v2x8hf_UP  E_V2x8HFmode
#define v2x8bf_UP  E_V2x8BFmode
#define v2x4si_UP  E_V2x4SImode
#define v2x4sf_UP  E_V2x4SFmode
#define v2x2di_UP  E_V2x2DImode
#define v2x2df_UP  E_V2x2DFmode
#define v3x8qi_UP  E_V3x8QImode
#define v3x4hi_UP  E_V3x4HImode
#define v3x4hf_UP  E_V3x4HFmode
#define v3x4bf_UP  E_V3x4BFmode
#define v3x2si_UP  E_V3x2SImode
#define v3x2sf_UP  E_V3x2SFmode
#define v3x1di_UP  E_V3x1DImode
#define v3x1df_UP  E_V3x1DFmode
#define v3x16qi_UP E_V3x16QImode
#define v3x8hi_UP  E_V3x8HImode
#define v3x8hf_UP  E_V3x8HFmode
#define v3x8bf_UP  E_V3x8BFmode
#define v3x4si_UP  E_V3x4SImode
#define v3x4sf_UP  E_V3x4SFmode
#define v3x2di_UP  E_V3x2DImode
#define v3x2df_UP  E_V3x2DFmode
#define v4x8qi_UP  E_V4x8QImode
#define v4x4hi_UP  E_V4x4HImode
#define v4x4hf_UP  E_V4x4HFmode
#define v4x4bf_UP  E_V4x4BFmode
#define v4x2si_UP  E_V4x2SImode
#define v4x2sf_UP  E_V4x2SFmode
#define v4x1di_UP  E_V4x1DImode
#define v4x1df_UP  E_V4x1DFmode
#define v4x16qi_UP E_V4x16QImode
#define v4x8hi_UP  E_V4x8HImode
#define v4x8hf_UP  E_V4x8HFmode
#define v4x8bf_UP  E_V4x8BFmode
#define v4x4si_UP  E_V4x4SImode
#define v4x4sf_UP  E_V4x4SFmode
#define v4x2di_UP  E_V4x2DImode
#define v4x2df_UP  E_V4x2DFmode
#define UP(X) X##_UP

#define MODE_d_bf16 E_V4BFmode
#define MODE_d_f16 E_V4HFmode
#define MODE_d_f32 E_V2SFmode
#define MODE_d_f64 E_V1DFmode
#define MODE_d_mf8 E_V8QImode
#define MODE_d_s8 E_V8QImode
#define MODE_d_s16 E_V4HImode
#define MODE_d_s32 E_V2SImode
#define MODE_d_s64 E_V1DImode
#define MODE_d_u8 E_V8QImode
#define MODE_d_u16 E_V4HImode
#define MODE_d_u32 E_V2SImode
#define MODE_d_u64 E_V1DImode
#define MODE_d_p8 E_V8QImode
#define MODE_d_p16 E_V4HImode
#define MODE_d_p64 E_V1DImode
#define MODE_q_bf16 E_V8BFmode
#define MODE_q_f16 E_V8HFmode
#define MODE_q_f32 E_V4SFmode
#define MODE_q_f64 E_V2DFmode
#define MODE_q_mf8 E_V16QImode
#define MODE_q_s8 E_V16QImode
#define MODE_q_s16 E_V8HImode
#define MODE_q_s32 E_V4SImode
#define MODE_q_s64 E_V2DImode
#define MODE_q_u8 E_V16QImode
#define MODE_q_u16 E_V8HImode
#define MODE_q_u32 E_V4SImode
#define MODE_q_u64 E_V2DImode
#define MODE_q_p8 E_V16QImode
#define MODE_q_p16 E_V8HImode
#define MODE_q_p64 E_V2DImode
#define MODE_q_p128 E_TImode

#define QUAL_bf16 qualifier_none
#define QUAL_f16 qualifier_none
#define QUAL_f32 qualifier_none
#define QUAL_f64 qualifier_none
#define QUAL_s8 qualifier_none
#define QUAL_s16 qualifier_none
#define QUAL_s32 qualifier_none
#define QUAL_s64 qualifier_none
#define QUAL_u8 qualifier_unsigned
#define QUAL_u16 qualifier_unsigned
#define QUAL_u32 qualifier_unsigned
#define QUAL_u64 qualifier_unsigned
#define QUAL_p8 qualifier_poly
#define QUAL_p16 qualifier_poly
#define QUAL_p64 qualifier_poly
#define QUAL_p128 qualifier_poly
#define QUAL_mf8 qualifier_modal_float

#define LENGTH_d ""
#define LENGTH_q "q"

#define SIMD_INTR_MODE(suffix, length) MODE_##length##_##suffix
#define SIMD_INTR_QUAL(suffix) QUAL_##suffix
#define SIMD_INTR_LENGTH_CHAR(length) LENGTH_##length

#define SIMD_MAX_BUILTIN_ARGS 5

/* Flags that describe what a function might do.  */
const unsigned int FLAG_NONE = 0U;
const unsigned int FLAG_READ_FPCR = 1U << 0;
const unsigned int FLAG_RAISE_FP_EXCEPTIONS = 1U << 1;
const unsigned int FLAG_READ_MEMORY = 1U << 2;
const unsigned int FLAG_PREFETCH_MEMORY = 1U << 3;
const unsigned int FLAG_WRITE_MEMORY = 1U << 4;

/* Not all FP intrinsics raise FP exceptions or read FPCR register,
   use this flag to suppress it.  */
const unsigned int FLAG_AUTO_FP = 1U << 5;

const unsigned int FLAG_FP = FLAG_READ_FPCR | FLAG_RAISE_FP_EXCEPTIONS;
const unsigned int FLAG_ALL = FLAG_READ_FPCR | FLAG_RAISE_FP_EXCEPTIONS
  | FLAG_READ_MEMORY | FLAG_PREFETCH_MEMORY | FLAG_WRITE_MEMORY;
const unsigned int FLAG_STORE = FLAG_WRITE_MEMORY | FLAG_AUTO_FP;
const unsigned int FLAG_LOAD = FLAG_READ_MEMORY | FLAG_AUTO_FP;

typedef struct
{
  const char *name;
  machine_mode mode;
  const enum insn_code code;
  unsigned int fcode;
  enum aarch64_type_qualifiers *qualifiers;
  unsigned int flags;
} aarch64_simd_builtin_datum;

static enum aarch64_type_qualifiers
aarch64_types_unop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none };
#define TYPES_UNOP (aarch64_types_unop_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_unopu_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned };
#define TYPES_UNOPU (aarch64_types_unopu_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_unopus_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_none };
#define TYPES_UNOPUS (aarch64_types_unopus_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_binop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_maybe_immediate };
#define TYPES_BINOP (aarch64_types_binop_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_binopu_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned };
#define TYPES_BINOPU (aarch64_types_binopu_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_binop_uus_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_none };
#define TYPES_BINOP_UUS (aarch64_types_binop_uus_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_binop_ssu_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_unsigned };
#define TYPES_BINOP_SSU (aarch64_types_binop_ssu_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_binop_uss_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_none, qualifier_none };
#define TYPES_BINOP_USS (aarch64_types_binop_uss_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_binopp_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_poly, qualifier_poly, qualifier_poly };
#define TYPES_BINOPP (aarch64_types_binopp_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_binop_ppu_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_poly, qualifier_poly, qualifier_unsigned };
#define TYPES_BINOP_PPU (aarch64_types_binop_ppu_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_ternop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_none };
#define TYPES_TERNOP (aarch64_types_ternop_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_ternop_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_lane_index };
#define TYPES_TERNOP_LANE (aarch64_types_ternop_lane_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_ternopu_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned,
      qualifier_unsigned, qualifier_unsigned };
#define TYPES_TERNOPU (aarch64_types_ternopu_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_ternopu_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned,
      qualifier_unsigned, qualifier_lane_index };
#define TYPES_TERNOPU_LANE (aarch64_types_ternopu_lane_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_ternopu_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned,
      qualifier_unsigned, qualifier_immediate };
#define TYPES_TERNOPUI (aarch64_types_ternopu_imm_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_ternop_sssu_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_unsigned };
#define TYPES_TERNOP_SSSU (aarch64_types_ternop_sssu_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_ternop_ssus_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_unsigned, qualifier_none };
#define TYPES_TERNOP_SSUS (aarch64_types_ternop_ssus_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_ternop_suss_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_unsigned, qualifier_none, qualifier_none };
#define TYPES_TERNOP_SUSS (aarch64_types_ternop_suss_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_binop_pppu_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_poly, qualifier_poly, qualifier_poly, qualifier_unsigned };
#define TYPES_TERNOP_PPPU (aarch64_types_binop_pppu_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_quadop_lane_pair_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none,
      qualifier_none, qualifier_lane_pair_index };
#define TYPES_QUADOP_LANE_PAIR (aarch64_types_quadop_lane_pair_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_quadop_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none,
      qualifier_none, qualifier_lane_index };
#define TYPES_QUADOP_LANE (aarch64_types_quadop_lane_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_quadopu_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
      qualifier_unsigned, qualifier_lane_index };
#define TYPES_QUADOPU_LANE (aarch64_types_quadopu_lane_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_quadopssus_lane_quadtup_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_unsigned,
      qualifier_none, qualifier_lane_quadtup_index };
#define TYPES_QUADOPSSUS_LANE_QUADTUP \
	(aarch64_types_quadopssus_lane_quadtup_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_quadopsssu_lane_quadtup_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none,
      qualifier_unsigned, qualifier_lane_quadtup_index };
#define TYPES_QUADOPSSSU_LANE_QUADTUP \
	(aarch64_types_quadopsssu_lane_quadtup_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_quadopu_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
      qualifier_unsigned, qualifier_immediate };
#define TYPES_QUADOPUI (aarch64_types_quadopu_imm_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_binop_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_immediate };
#define TYPES_GETREG (aarch64_types_binop_imm_qualifiers)
#define TYPES_SHIFTIMM (aarch64_types_binop_imm_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_shift_to_unsigned_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_none, qualifier_immediate };
#define TYPES_SHIFTIMM_USS (aarch64_types_shift_to_unsigned_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_fcvt_from_unsigned_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_unsigned, qualifier_immediate };
#define TYPES_FCVTIMM_SUS (aarch64_types_fcvt_from_unsigned_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_unsigned_shift_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_immediate };
#define TYPES_USHIFTIMM (aarch64_types_unsigned_shift_qualifiers)
#define TYPES_USHIFT2IMM (aarch64_types_ternopu_imm_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_shift2_to_unsigned_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_none, qualifier_immediate };
#define TYPES_SHIFT2IMM_UUSS (aarch64_types_shift2_to_unsigned_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_ternop_s_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_immediate};
#define TYPES_SETREG (aarch64_types_ternop_s_imm_qualifiers)
#define TYPES_SHIFTINSERT (aarch64_types_ternop_s_imm_qualifiers)
#define TYPES_SHIFTACC (aarch64_types_ternop_s_imm_qualifiers)
#define TYPES_SHIFT2IMM (aarch64_types_ternop_s_imm_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_ternop_p_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_poly, qualifier_poly, qualifier_poly, qualifier_immediate};
#define TYPES_SHIFTINSERTP (aarch64_types_ternop_p_imm_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_unsigned_shiftacc_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
      qualifier_immediate };
#define TYPES_USHIFTACC (aarch64_types_unsigned_shiftacc_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_load1_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_const_pointer_map_mode };
#define TYPES_LOAD1 (aarch64_types_load1_qualifiers)
#define TYPES_LOADSTRUCT (aarch64_types_load1_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_load1_u_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_const_pointer_map_mode };
#define TYPES_LOAD1_U (aarch64_types_load1_u_qualifiers)
#define TYPES_LOADSTRUCT_U (aarch64_types_load1_u_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_load1_p_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_poly, qualifier_const_pointer_map_mode };
#define TYPES_LOAD1_P (aarch64_types_load1_p_qualifiers)
#define TYPES_LOADSTRUCT_P (aarch64_types_load1_p_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_loadstruct_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_const_pointer_map_mode,
      qualifier_none, qualifier_struct_load_store_lane_index };
#define TYPES_LOADSTRUCT_LANE (aarch64_types_loadstruct_lane_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_loadstruct_lane_u_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_const_pointer_map_mode,
      qualifier_unsigned, qualifier_struct_load_store_lane_index };
#define TYPES_LOADSTRUCT_LANE_U (aarch64_types_loadstruct_lane_u_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_loadstruct_lane_p_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_poly, qualifier_const_pointer_map_mode,
      qualifier_poly, qualifier_struct_load_store_lane_index };
#define TYPES_LOADSTRUCT_LANE_P (aarch64_types_loadstruct_lane_p_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_bsl_p_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_poly, qualifier_unsigned,
      qualifier_poly, qualifier_poly };
#define TYPES_BSL_P (aarch64_types_bsl_p_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_bsl_s_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_unsigned,
      qualifier_none, qualifier_none };
#define TYPES_BSL_S (aarch64_types_bsl_s_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_bsl_u_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned,
      qualifier_unsigned, qualifier_unsigned };
#define TYPES_BSL_U (aarch64_types_bsl_u_qualifiers)

/* The first argument (return type) of a store should be void type,
   which we represent with qualifier_void.  Their first operand will be
   a DImode pointer to the location to store to, so we must use
   qualifier_map_mode | qualifier_pointer to build a pointer to the
   element type of the vector.  */
static enum aarch64_type_qualifiers
aarch64_types_store1_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_pointer_map_mode, qualifier_none };
#define TYPES_STORE1 (aarch64_types_store1_qualifiers)
#define TYPES_STORESTRUCT (aarch64_types_store1_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_store1_u_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_pointer_map_mode, qualifier_unsigned };
#define TYPES_STORE1_U (aarch64_types_store1_u_qualifiers)
#define TYPES_STORESTRUCT_U (aarch64_types_store1_u_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_store1_p_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_pointer_map_mode, qualifier_poly };
#define TYPES_STORE1_P (aarch64_types_store1_p_qualifiers)
#define TYPES_STORESTRUCT_P (aarch64_types_store1_p_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_storestruct_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_pointer_map_mode,
      qualifier_none, qualifier_struct_load_store_lane_index };
#define TYPES_STORESTRUCT_LANE (aarch64_types_storestruct_lane_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_storestruct_lane_u_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_pointer_map_mode,
      qualifier_unsigned, qualifier_struct_load_store_lane_index };
#define TYPES_STORESTRUCT_LANE_U (aarch64_types_storestruct_lane_u_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_storestruct_lane_p_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_pointer_map_mode,
      qualifier_poly, qualifier_struct_load_store_lane_index };
#define TYPES_STORESTRUCT_LANE_P (aarch64_types_storestruct_lane_p_qualifiers)

constexpr insn_code CODE_FOR_aarch64_sdot_prodv8qi
  = CODE_FOR_sdot_prodv2siv8qi;
constexpr insn_code CODE_FOR_aarch64_udot_prodv8qi
  = CODE_FOR_udot_prodv2siv8qi;
constexpr insn_code CODE_FOR_aarch64_usdot_prodv8qi
  = CODE_FOR_usdot_prodv2siv8qi;
constexpr insn_code CODE_FOR_aarch64_sdot_prodv16qi
  = CODE_FOR_sdot_prodv4siv16qi;
constexpr insn_code CODE_FOR_aarch64_udot_prodv16qi
  = CODE_FOR_udot_prodv4siv16qi;
constexpr insn_code CODE_FOR_aarch64_usdot_prodv16qi
  = CODE_FOR_usdot_prodv4siv16qi;

#define CF0(N, X) CODE_FOR_aarch64_##N##X
#define CF1(N, X) CODE_FOR_##N##X##1
#define CF2(N, X) CODE_FOR_##N##X##2
#define CF3(N, X) CODE_FOR_##N##X##3
#define CF4(N, X) CODE_FOR_##N##X##4
#define CF10(N, X) CODE_FOR_##N##X

/* Define cascading VAR<N> macros that are used from
   aarch64-builtin-iterators.h to iterate over modes.  These definitions
   will end up generating a number of VAR1 expansions and code later on in the
   file should redefine VAR1 to whatever it needs to process on a per-mode
   basis.  */
#define VAR2(T, N, MAP, FLAG, A, B) \
  VAR1 (T, N, MAP, FLAG, A) \
  VAR1 (T, N, MAP, FLAG, B)
#define VAR3(T, N, MAP, FLAG, A, B, C) \
  VAR2 (T, N, MAP, FLAG, A, B) \
  VAR1 (T, N, MAP, FLAG, C)
#define VAR4(T, N, MAP, FLAG, A, B, C, D) \
  VAR3 (T, N, MAP, FLAG, A, B, C) \
  VAR1 (T, N, MAP, FLAG, D)
#define VAR5(T, N, MAP, FLAG, A, B, C, D, E) \
  VAR4 (T, N, MAP, FLAG, A, B, C, D) \
  VAR1 (T, N, MAP, FLAG, E)
#define VAR6(T, N, MAP, FLAG, A, B, C, D, E, F) \
  VAR5 (T, N, MAP, FLAG, A, B, C, D, E) \
  VAR1 (T, N, MAP, FLAG, F)
#define VAR7(T, N, MAP, FLAG, A, B, C, D, E, F, G) \
  VAR6 (T, N, MAP, FLAG, A, B, C, D, E, F) \
  VAR1 (T, N, MAP, FLAG, G)
#define VAR8(T, N, MAP, FLAG, A, B, C, D, E, F, G, H) \
  VAR7 (T, N, MAP, FLAG, A, B, C, D, E, F, G) \
  VAR1 (T, N, MAP, FLAG, H)
#define VAR9(T, N, MAP, FLAG, A, B, C, D, E, F, G, H, I) \
  VAR8 (T, N, MAP, FLAG, A, B, C, D, E, F, G, H) \
  VAR1 (T, N, MAP, FLAG, I)
#define VAR10(T, N, MAP, FLAG, A, B, C, D, E, F, G, H, I, J) \
  VAR9 (T, N, MAP, FLAG, A, B, C, D, E, F, G, H, I) \
  VAR1 (T, N, MAP, FLAG, J)
#define VAR11(T, N, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K) \
  VAR10 (T, N, MAP, FLAG, A, B, C, D, E, F, G, H, I, J) \
  VAR1 (T, N, MAP, FLAG, K)
#define VAR12(T, N, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K, L) \
  VAR11 (T, N, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K) \
  VAR1 (T, N, MAP, FLAG, L)
#define VAR13(T, N, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K, L, M) \
  VAR12 (T, N, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K, L) \
  VAR1 (T, N, MAP, FLAG, M)
#define VAR14(T, X, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K, L, M, N) \
  VAR13 (T, X, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K, L, M) \
  VAR1 (T, X, MAP, FLAG, N)
#define VAR15(T, X, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) \
  VAR14 (T, X, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K, L, M, N) \
  VAR1 (T, X, MAP, FLAG, O)
#define VAR16(T, X, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) \
  VAR15 (T, X, MAP, FLAG, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) \
  VAR1 (T, X, MAP, FLAG, P)

#include "aarch64-builtin-iterators.h"

/* The builtins below should be expanded through the standard optabs
   CODE_FOR_[u]avg<mode>3_[floor,ceil].  However the mapping scheme in
   aarch64-simd-builtins.def does not easily allow us to have a pre-mode
   ("uavg") and post-mode string ("_ceil") in the CODE_FOR_* construction.
   So the builtins use a name that is natural for AArch64 instructions
   e.g. "aarch64_srhadd<mode>" and we re-map these to the optab-related
   CODE_FOR_ here.  */
#undef VAR1
#define VAR1(F,T1,T2,I,M) \
constexpr insn_code CODE_FOR_aarch64_##F##M = CODE_FOR_##T1##M##3##T2;

BUILTIN_VDQ_BHSI (srhadd, avg, _ceil, 0)
BUILTIN_VDQ_BHSI (urhadd, uavg, _ceil, 0)
BUILTIN_VDQ_BHSI (shadd, avg, _floor, 0)
BUILTIN_VDQ_BHSI (uhadd, uavg, _floor, 0)

/* The builtins below should be expanded through the standard optabs
   CODE_FOR_extend<mode><Vwide>2. */
#undef VAR1
#define VAR1(F,T,N,M) \
  constexpr insn_code CODE_FOR_aarch64_##F##M = CODE_FOR_##T##N##M##2;

VAR1 (float_extend_lo_, extend, v2sf, v2df)
VAR1 (float_extend_lo_, extend, v4hf, v4sf)

/* __builtin_aarch64_float_truncate_lo_<mode> should be expanded through the
   standard optabs CODE_FOR_trunc<Vwide><mode>2. */
constexpr insn_code CODE_FOR_aarch64_float_truncate_lo_v4hf
    = CODE_FOR_truncv4sfv4hf2;
constexpr insn_code CODE_FOR_aarch64_float_truncate_lo_v2sf
    = CODE_FOR_truncv2dfv2sf2;

#undef VAR1
#define VAR1(T, N, MAP, FLAG, A) \
  {#N #A, UP (A), CF##MAP (N, A), 0, TYPES_##T, FLAG_##FLAG},

static aarch64_simd_builtin_datum aarch64_simd_builtin_data[] = {
#include "aarch64-simd-builtins.def"
};

/* There's only 8 CRC32 builtins.  Probably not worth their own .def file.  */
#define AARCH64_CRC32_BUILTINS \
  CRC32_BUILTIN (crc32b, QI) \
  CRC32_BUILTIN (crc32h, HI) \
  CRC32_BUILTIN (crc32w, SI) \
  CRC32_BUILTIN (crc32x, DI) \
  CRC32_BUILTIN (crc32cb, QI) \
  CRC32_BUILTIN (crc32ch, HI) \
  CRC32_BUILTIN (crc32cw, SI) \
  CRC32_BUILTIN (crc32cx, DI)

/* The next 8 FCMLA instrinsics require some special handling compared the
   normal simd intrinsics.  */
#define AARCH64_SIMD_FCMLA_LANEQ_BUILTINS \
  FCMLA_LANEQ_BUILTIN (0, v2sf, fcmla, V2SF, false) \
  FCMLA_LANEQ_BUILTIN (90, v2sf, fcmla, V2SF, false) \
  FCMLA_LANEQ_BUILTIN (180, v2sf, fcmla, V2SF, false) \
  FCMLA_LANEQ_BUILTIN (270, v2sf, fcmla, V2SF, false) \
  FCMLA_LANEQ_BUILTIN (0, v4hf, fcmla_laneq, V4HF, true) \
  FCMLA_LANEQ_BUILTIN (90, v4hf, fcmla_laneq, V4HF, true) \
  FCMLA_LANEQ_BUILTIN (180, v4hf, fcmla_laneq, V4HF, true) \
  FCMLA_LANEQ_BUILTIN (270, v4hf, fcmla_laneq, V4HF, true) \


/* vreinterpret intrinsics are defined for any pair of element types.
   {     _bf16           }   {     _bf16           }
   {      _f16 _f32 _f64 }   {      _f16 _f32 _f64 }
   { _mf8                }   { _mf8                }
   { _s8  _s16 _s32 _s64 } x { _s8  _s16 _s32 _s64 }
   { _u8  _u16 _u32 _u64 }   { _u8  _u16 _u32 _u64 }
   { _p8  _p16      _p64 }   { _p8  _p16      _p64 }.  */
#define VREINTERPRET_BUILTIN2(A, B) \
  VREINTERPRET_BUILTIN (A, B, d)

#define VREINTERPRET_BUILTINS1(A) \
  VREINTERPRET_BUILTIN2 (A, bf16) \
  VREINTERPRET_BUILTIN2 (A, f16) \
  VREINTERPRET_BUILTIN2 (A, f32) \
  VREINTERPRET_BUILTIN2 (A, f64) \
  VREINTERPRET_BUILTIN2 (A, mf8) \
  VREINTERPRET_BUILTIN2 (A, s8) \
  VREINTERPRET_BUILTIN2 (A, s16) \
  VREINTERPRET_BUILTIN2 (A, s32) \
  VREINTERPRET_BUILTIN2 (A, s64) \
  VREINTERPRET_BUILTIN2 (A, u8) \
  VREINTERPRET_BUILTIN2 (A, u16) \
  VREINTERPRET_BUILTIN2 (A, u32) \
  VREINTERPRET_BUILTIN2 (A, u64) \
  VREINTERPRET_BUILTIN2 (A, p8) \
  VREINTERPRET_BUILTIN2 (A, p16) \
  VREINTERPRET_BUILTIN2 (A, p64)

#define VREINTERPRET_BUILTINS \
  VREINTERPRET_BUILTINS1 (bf16) \
  VREINTERPRET_BUILTINS1 (f16) \
  VREINTERPRET_BUILTINS1 (f32) \
  VREINTERPRET_BUILTINS1 (f64) \
  VREINTERPRET_BUILTINS1 (mf8) \
  VREINTERPRET_BUILTINS1 (s8) \
  VREINTERPRET_BUILTINS1 (s16) \
  VREINTERPRET_BUILTINS1 (s32) \
  VREINTERPRET_BUILTINS1 (s64) \
  VREINTERPRET_BUILTINS1 (u8) \
  VREINTERPRET_BUILTINS1 (u16) \
  VREINTERPRET_BUILTINS1 (u32) \
  VREINTERPRET_BUILTINS1 (u64) \
  VREINTERPRET_BUILTINS1 (p8) \
  VREINTERPRET_BUILTINS1 (p16) \
  VREINTERPRET_BUILTINS1 (p64)

/* vreinterpretq intrinsics are additionally defined for p128.
   {     _bf16                 }   {     _bf16                 }
   {      _f16 _f32 _f64       }   {      _f16 _f32 _f64       }
   { _mf8                      }   { _mf8                      }
   { _s8  _s16 _s32 _s64       } x { _s8  _s16 _s32 _s64       }
   { _u8  _u16 _u32 _u64       }   { _u8  _u16 _u32 _u64       }
   { _p8  _p16      _p64 _p128 }   { _p8  _p16      _p64 _p128 }.  */
#define VREINTERPRETQ_BUILTIN2(A, B) \
  VREINTERPRET_BUILTIN (A, B, q)

#define VREINTERPRETQ_BUILTINS1(A) \
  VREINTERPRETQ_BUILTIN2 (A, bf16) \
  VREINTERPRETQ_BUILTIN2 (A, f16) \
  VREINTERPRETQ_BUILTIN2 (A, f32) \
  VREINTERPRETQ_BUILTIN2 (A, f64) \
  VREINTERPRETQ_BUILTIN2 (A, mf8) \
  VREINTERPRETQ_BUILTIN2 (A, s8) \
  VREINTERPRETQ_BUILTIN2 (A, s16) \
  VREINTERPRETQ_BUILTIN2 (A, s32) \
  VREINTERPRETQ_BUILTIN2 (A, s64) \
  VREINTERPRETQ_BUILTIN2 (A, u8) \
  VREINTERPRETQ_BUILTIN2 (A, u16) \
  VREINTERPRETQ_BUILTIN2 (A, u32) \
  VREINTERPRETQ_BUILTIN2 (A, u64) \
  VREINTERPRETQ_BUILTIN2 (A, p8) \
  VREINTERPRETQ_BUILTIN2 (A, p16) \
  VREINTERPRETQ_BUILTIN2 (A, p64) \
  VREINTERPRETQ_BUILTIN2 (A, p128)

#define VREINTERPRETQ_BUILTINS \
  VREINTERPRETQ_BUILTINS1 (bf16) \
  VREINTERPRETQ_BUILTINS1 (f16) \
  VREINTERPRETQ_BUILTINS1 (f32) \
  VREINTERPRETQ_BUILTINS1 (f64) \
  VREINTERPRETQ_BUILTINS1 (mf8) \
  VREINTERPRETQ_BUILTINS1 (s8) \
  VREINTERPRETQ_BUILTINS1 (s16) \
  VREINTERPRETQ_BUILTINS1 (s32) \
  VREINTERPRETQ_BUILTINS1 (s64) \
  VREINTERPRETQ_BUILTINS1 (u8) \
  VREINTERPRETQ_BUILTINS1 (u16) \
  VREINTERPRETQ_BUILTINS1 (u32) \
  VREINTERPRETQ_BUILTINS1 (u64) \
  VREINTERPRETQ_BUILTINS1 (p8) \
  VREINTERPRETQ_BUILTINS1 (p16) \
  VREINTERPRETQ_BUILTINS1 (p64) \
  VREINTERPRETQ_BUILTINS1 (p128)

#define AARCH64_SIMD_VREINTERPRET_BUILTINS \
  VREINTERPRET_BUILTINS \
  VREINTERPRETQ_BUILTINS

#define AARCH64_SIMD_VGET_LOW_BUILTINS \
  VGET_LOW_BUILTIN(f16) \
  VGET_LOW_BUILTIN(f32) \
  VGET_LOW_BUILTIN(f64) \
  VGET_LOW_BUILTIN(p8) \
  VGET_LOW_BUILTIN(p16) \
  VGET_LOW_BUILTIN(p64) \
  VGET_LOW_BUILTIN(s8) \
  VGET_LOW_BUILTIN(s16) \
  VGET_LOW_BUILTIN(s32) \
  VGET_LOW_BUILTIN(s64) \
  VGET_LOW_BUILTIN(u8) \
  VGET_LOW_BUILTIN(u16) \
  VGET_LOW_BUILTIN(u32) \
  VGET_LOW_BUILTIN(u64) \
  VGET_LOW_BUILTIN(bf16)

#define AARCH64_SIMD_VGET_HIGH_BUILTINS \
  VGET_HIGH_BUILTIN(f16) \
  VGET_HIGH_BUILTIN(f32) \
  VGET_HIGH_BUILTIN(f64) \
  VGET_HIGH_BUILTIN(p8) \
  VGET_HIGH_BUILTIN(p16) \
  VGET_HIGH_BUILTIN(p64) \
  VGET_HIGH_BUILTIN(s8) \
  VGET_HIGH_BUILTIN(s16) \
  VGET_HIGH_BUILTIN(s32) \
  VGET_HIGH_BUILTIN(s64) \
  VGET_HIGH_BUILTIN(u8) \
  VGET_HIGH_BUILTIN(u16) \
  VGET_HIGH_BUILTIN(u32) \
  VGET_HIGH_BUILTIN(u64) \
  VGET_HIGH_BUILTIN(bf16)

typedef struct
{
  const char *name;
  machine_mode mode;
  const enum insn_code icode;
  unsigned int fcode;
} aarch64_crc_builtin_datum;

/* Hold information about how to expand the FCMLA_LANEQ builtins.  */
typedef struct
{
  const char *name;
  machine_mode mode;
  const enum insn_code icode;
  unsigned int fcode;
  bool lane;
} aarch64_fcmla_laneq_builtin_datum;

/* Hold information about how to declare SIMD intrinsics.  */
typedef struct
{
  const char *name;
  unsigned int fcode;
  unsigned int op_count;
  machine_mode op_modes[SIMD_MAX_BUILTIN_ARGS];
  enum aarch64_type_qualifiers qualifiers[SIMD_MAX_BUILTIN_ARGS];
  unsigned int flags;
  bool skip;
} aarch64_simd_intrinsic_datum;

#define CRC32_BUILTIN(N, M) \
  AARCH64_BUILTIN_##N,

#define FCMLA_LANEQ_BUILTIN(I, N, X, M, T) \
  AARCH64_SIMD_BUILTIN_FCMLA_LANEQ##I##_##M,

#define VREINTERPRET_BUILTIN(A, B, L) \
  AARCH64_SIMD_BUILTIN_VREINTERPRET##L##_##A##_##B,

#define VGET_LOW_BUILTIN(A) \
  AARCH64_SIMD_BUILTIN_VGET_LOW_##A,

#define VGET_HIGH_BUILTIN(A)                     \
  AARCH64_SIMD_BUILTIN_VGET_HIGH_##A,

#undef VAR1
#define VAR1(T, N, MAP, FLAG, A) \
  AARCH64_SIMD_BUILTIN_##T##_##N##A,

#undef ENTRY
#define ENTRY(N, S, M, U) \
  AARCH64_##N,

enum aarch64_builtins
{
  AARCH64_BUILTIN_MIN,

  AARCH64_BUILTIN_GET_FPCR,
  AARCH64_BUILTIN_SET_FPCR,
  AARCH64_BUILTIN_GET_FPSR,
  AARCH64_BUILTIN_SET_FPSR,

  AARCH64_BUILTIN_GET_FPCR64,
  AARCH64_BUILTIN_SET_FPCR64,
  AARCH64_BUILTIN_GET_FPSR64,
  AARCH64_BUILTIN_SET_FPSR64,

  AARCH64_BUILTIN_RSQRT_DF,
  AARCH64_BUILTIN_RSQRT_SF,
  AARCH64_BUILTIN_RSQRT_V2DF,
  AARCH64_BUILTIN_RSQRT_V2SF,
  AARCH64_BUILTIN_RSQRT_V4SF,
  AARCH64_SIMD_BUILTIN_BASE,
  AARCH64_SIMD_BUILTIN_LANE_CHECK,
#include "aarch64-simd-builtins.def"
  /* The first enum element which is based on an insn_data pattern.  */
  AARCH64_SIMD_PATTERN_START = AARCH64_SIMD_BUILTIN_LANE_CHECK + 1,
  AARCH64_SIMD_BUILTIN_MAX = AARCH64_SIMD_PATTERN_START
			      + ARRAY_SIZE (aarch64_simd_builtin_data) - 1,
  AARCH64_CRC32_BUILTIN_BASE,
  AARCH64_CRC32_BUILTINS
  AARCH64_CRC32_BUILTIN_MAX,
  /* SIMD intrinsic builtins.  */
  AARCH64_SIMD_VREINTERPRET_BUILTINS
  AARCH64_SIMD_VGET_LOW_BUILTINS
  AARCH64_SIMD_VGET_HIGH_BUILTINS
  /* ARMv8.3-A Pointer Authentication Builtins.  */
  AARCH64_PAUTH_BUILTIN_AUTIA1716,
  AARCH64_PAUTH_BUILTIN_PACIA1716,
  AARCH64_PAUTH_BUILTIN_AUTIB1716,
  AARCH64_PAUTH_BUILTIN_PACIB1716,
  AARCH64_PAUTH_BUILTIN_XPACLRI,
  /* Special cased Armv8.3-A Complex FMA by Lane quad Builtins.  */
  AARCH64_SIMD_FCMLA_LANEQ_BUILTIN_BASE,
  AARCH64_SIMD_FCMLA_LANEQ_BUILTINS
  /* Builtin for Arm8.3-a Javascript conversion instruction.  */
  AARCH64_JSCVT,
  /* TME builtins.  */
  AARCH64_TME_BUILTIN_TSTART,
  AARCH64_TME_BUILTIN_TCOMMIT,
  AARCH64_TME_BUILTIN_TTEST,
  AARCH64_TME_BUILTIN_TCANCEL,
  /* Armv8.5-a RNG instruction builtins.  */
  AARCH64_BUILTIN_RNG_RNDR,
  AARCH64_BUILTIN_RNG_RNDRRS,
  /* MEMTAG builtins.  */
  AARCH64_MEMTAG_BUILTIN_START,
  AARCH64_MEMTAG_BUILTIN_IRG,
  AARCH64_MEMTAG_BUILTIN_GMI,
  AARCH64_MEMTAG_BUILTIN_SUBP,
  AARCH64_MEMTAG_BUILTIN_INC_TAG,
  AARCH64_MEMTAG_BUILTIN_SET_TAG,
  AARCH64_MEMTAG_BUILTIN_GET_TAG,
  AARCH64_MEMTAG_BUILTIN_END,
  /* LS64 builtins.  */
  AARCH64_LS64_BUILTIN_LD64B,
  AARCH64_LS64_BUILTIN_ST64B,
  AARCH64_LS64_BUILTIN_ST64BV,
  AARCH64_LS64_BUILTIN_ST64BV0,
  AARCH64_REV16,
  AARCH64_REV16L,
  AARCH64_REV16LL,
  AARCH64_RBIT,
  AARCH64_RBITL,
  AARCH64_RBITLL,
  /* Pragma builtins.  */
  AARCH64_PRAGMA_BUILTIN_START,
#include "aarch64-simd-pragma-builtins.def"
  AARCH64_PRAGMA_BUILTIN_END,
  /* System register builtins.  */
  AARCH64_RSR,
  AARCH64_RSRP,
  AARCH64_RSR64,
  AARCH64_RSRF,
  AARCH64_RSRF64,
  AARCH64_RSR128,
  AARCH64_WSR,
  AARCH64_WSRP,
  AARCH64_WSR64,
  AARCH64_WSRF,
  AARCH64_WSRF64,
  AARCH64_WSR128,
  AARCH64_PLD,
  AARCH64_PLDX,
  AARCH64_PLI,
  AARCH64_PLIX,
  /* Armv8.9-A / Armv9.4-A builtins.  */
  AARCH64_BUILTIN_CHKFEAT,
  AARCH64_BUILTIN_GCSPR,
  AARCH64_BUILTIN_GCSPOPM,
  AARCH64_BUILTIN_GCSSS,
  AARCH64_BUILTIN_MAX
};

#undef CRC32_BUILTIN
#define CRC32_BUILTIN(N, M) \
  {"__builtin_aarch64_"#N, E_##M##mode, CODE_FOR_aarch64_##N, AARCH64_BUILTIN_##N},

static aarch64_crc_builtin_datum aarch64_crc_builtin_data[] = {
  AARCH64_CRC32_BUILTINS
};


#undef FCMLA_LANEQ_BUILTIN
#define FCMLA_LANEQ_BUILTIN(I, N, X, M, T) \
  {"__builtin_aarch64_fcmla_laneq"#I#N, E_##M##mode, CODE_FOR_aarch64_##X##I##N, \
   AARCH64_SIMD_BUILTIN_FCMLA_LANEQ##I##_##M, T},

/* This structure contains how to manage the mapping form the builtin to the
   instruction to generate in the backend and how to invoke the instruction.  */
static aarch64_fcmla_laneq_builtin_datum aarch64_fcmla_lane_builtin_data[] = {
  AARCH64_SIMD_FCMLA_LANEQ_BUILTINS
};

#undef VREINTERPRET_BUILTIN
#define VREINTERPRET_BUILTIN(A, B, L) \
  {"vreinterpret" SIMD_INTR_LENGTH_CHAR(L) "_" #A "_" #B, \
   AARCH64_SIMD_BUILTIN_VREINTERPRET##L##_##A##_##B, \
   2, \
   { SIMD_INTR_MODE(A, L), SIMD_INTR_MODE(B, L) }, \
   { SIMD_INTR_QUAL(A), SIMD_INTR_QUAL(B) }, \
   FLAG_AUTO_FP, \
   SIMD_INTR_MODE(A, L) == SIMD_INTR_MODE(B, L) \
     && SIMD_INTR_QUAL(A) == SIMD_INTR_QUAL(B) \
  },

#undef VGET_LOW_BUILTIN
#define VGET_LOW_BUILTIN(A) \
  {"vget_low_" #A, \
   AARCH64_SIMD_BUILTIN_VGET_LOW_##A, \
   2, \
   { SIMD_INTR_MODE(A, d), SIMD_INTR_MODE(A, q) }, \
   { SIMD_INTR_QUAL(A), SIMD_INTR_QUAL(A) }, \
   FLAG_AUTO_FP, \
   false \
  },

#undef VGET_HIGH_BUILTIN
#define VGET_HIGH_BUILTIN(A) \
  {"vget_high_" #A, \
   AARCH64_SIMD_BUILTIN_VGET_HIGH_##A, \
   2, \
   { SIMD_INTR_MODE(A, d), SIMD_INTR_MODE(A, q) }, \
   { SIMD_INTR_QUAL(A), SIMD_INTR_QUAL(A) }, \
   FLAG_AUTO_FP, \
   false \
  },

static const aarch64_simd_intrinsic_datum aarch64_simd_intrinsic_data[] = {
  AARCH64_SIMD_VREINTERPRET_BUILTINS
  AARCH64_SIMD_VGET_LOW_BUILTINS
  AARCH64_SIMD_VGET_HIGH_BUILTINS
};


#undef CRC32_BUILTIN

static GTY(()) tree aarch64_builtin_decls[AARCH64_BUILTIN_MAX];

#define NUM_DREG_TYPES 6
#define NUM_QREG_TYPES 6

/* Internal scalar builtin types.  These types are used to support
   neon intrinsic builtins.  They are _not_ user-visible types.  Therefore
   the mangling for these types are implementation defined.  */
const char *aarch64_scalar_builtin_types[] = {
  "__builtin_aarch64_simd_qi",
  "__builtin_aarch64_simd_hi",
  "__builtin_aarch64_simd_si",
  "__builtin_aarch64_simd_hf",
  "__builtin_aarch64_simd_sf",
  "__builtin_aarch64_simd_di",
  "__builtin_aarch64_simd_df",
  "__builtin_aarch64_simd_poly8",
  "__builtin_aarch64_simd_poly16",
  "__builtin_aarch64_simd_poly64",
  "__builtin_aarch64_simd_poly128",
  "__builtin_aarch64_simd_ti",
  "__builtin_aarch64_simd_uqi",
  "__builtin_aarch64_simd_uhi",
  "__builtin_aarch64_simd_usi",
  "__builtin_aarch64_simd_udi",
  "__builtin_aarch64_simd_ei",
  "__builtin_aarch64_simd_oi",
  "__builtin_aarch64_simd_ci",
  "__builtin_aarch64_simd_xi",
  "__builtin_aarch64_simd_bf",
  NULL
};

extern GTY(()) aarch64_simd_type_info aarch64_simd_types[];

#undef ENTRY
#define ENTRY(E, M, Q, G)  \
  {E, "__" #E, #G "__" #E, NULL_TREE, NULL_TREE, E_##M##mode, qualifier_##Q},
struct aarch64_simd_type_info aarch64_simd_types [] = {
#include "aarch64-simd-builtin-types.def"
};
#undef ENTRY

static machine_mode aarch64_simd_tuple_modes[ARM_NEON_H_TYPES_LAST][3];
static GTY(()) tree aarch64_simd_tuple_types[ARM_NEON_H_TYPES_LAST][3];

static GTY(()) tree aarch64_simd_intOI_type_node = NULL_TREE;
static GTY(()) tree aarch64_simd_intCI_type_node = NULL_TREE;
static GTY(()) tree aarch64_simd_intXI_type_node = NULL_TREE;

/* The user-visible __mfp8 type, and a pointer to that type.  Used
   across the back-end.  */
tree aarch64_mfp8_type_node = NULL_TREE;
tree aarch64_mfp8_ptr_type_node = NULL_TREE;

/* The user-visible __fp16 type, and a pointer to that type.  Used
   across the back-end.  */
tree aarch64_fp16_type_node = NULL_TREE;
tree aarch64_fp16_ptr_type_node = NULL_TREE;

/* Back-end node type for brain float (bfloat) types.  */
tree aarch64_bf16_ptr_type_node = NULL_TREE;

/* Wrapper around add_builtin_function.  NAME is the name of the built-in
   function, TYPE is the function type, CODE is the function subcode
   (relative to AARCH64_BUILTIN_GENERAL), and ATTRS is the function
   attributes.  */
static tree
aarch64_general_add_builtin (const char *name, tree type, unsigned int code,
			     tree attrs = NULL_TREE)
{
  code = (code << AARCH64_BUILTIN_SHIFT) | AARCH64_BUILTIN_GENERAL;
  return add_builtin_function (name, type, code, BUILT_IN_MD,
			       NULL, attrs);
}

static tree
aarch64_general_simulate_builtin (const char *name, tree fntype,
				  unsigned int code,
				  tree attrs = NULL_TREE)
{
  code = (code << AARCH64_BUILTIN_SHIFT) | AARCH64_BUILTIN_GENERAL;
  return simulate_builtin_function_decl (input_location, name, fntype,
					 code, NULL, attrs);
}

static const char *
aarch64_mangle_builtin_scalar_type (const_tree type)
{
  int i = 0;

  while (aarch64_scalar_builtin_types[i] != NULL)
    {
      const char *name = aarch64_scalar_builtin_types[i];

      if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	  && DECL_NAME (TYPE_NAME (type))
	  && !strcmp (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))), name))
	return aarch64_scalar_builtin_types[i];
      i++;
    }
  return NULL;
}

static const char *
aarch64_mangle_builtin_vector_type (const_tree type)
{
  tree attrs = TYPE_ATTRIBUTES (type);
  if (tree attr = lookup_attribute ("Advanced SIMD type", attrs))
    {
      tree mangled_name = TREE_VALUE (TREE_VALUE (attr));
      return IDENTIFIER_POINTER (mangled_name);
    }

  return NULL;
}

const char *
aarch64_general_mangle_builtin_type (const_tree type)
{
  const char *mangle;
  /* Walk through all the AArch64 builtins types tables to filter out the
     incoming type.  */
  if ((mangle = aarch64_mangle_builtin_vector_type (type))
      || (mangle = aarch64_mangle_builtin_scalar_type (type)))
    return mangle;

  return NULL;
}

/* Helper function for aarch64_simd_builtin_type.  */
static tree
aarch64_int_or_fp_type (machine_mode mode,
			enum aarch64_type_qualifiers qualifiers)
{
#define QUAL_TYPE(M) ((qualifiers & qualifier_unsigned) \
		       ? unsigned_int##M##_type_node : int##M##_type_node);
  switch (mode)
    {
    case E_QImode:
      return QUAL_TYPE (QI);
    case E_HImode:
      return QUAL_TYPE (HI);
    case E_SImode:
      return QUAL_TYPE (SI);
    case E_DImode:
      return QUAL_TYPE (DI);
    case E_TImode:
      return QUAL_TYPE (TI);
    case E_OImode:
      return aarch64_simd_intOI_type_node;
    case E_CImode:
      return aarch64_simd_intCI_type_node;
    case E_XImode:
      return aarch64_simd_intXI_type_node;
    case E_HFmode:
      return aarch64_fp16_type_node;
    case E_SFmode:
      return float_type_node;
    case E_DFmode:
      return double_type_node;
    case E_BFmode:
      return bfloat16_type_node;
    default:
      gcc_unreachable ();
    }
#undef QUAL_TYPE
}

/* Helper function for aarch64_simd_builtin_type.  */
static tree
aarch64_lookup_simd_type_in_table (machine_mode mode,
				   enum aarch64_type_qualifiers qualifiers)
{
  int i;
  int nelts = ARRAY_SIZE (aarch64_simd_types);
  int q = qualifiers
    & (qualifier_poly | qualifier_unsigned | qualifier_modal_float);

  for (i = 0; i < nelts; i++)
    {
      if (aarch64_simd_types[i].mode == mode
	  && aarch64_simd_types[i].q == q)
	return aarch64_simd_types[i].itype;
      if (aarch64_simd_tuple_types[i][0] != NULL_TREE)
	for (int j = 0; j < 3; j++)
	  if (aarch64_simd_tuple_modes[i][j] == mode
	      && aarch64_simd_types[i].q == q)
	    return aarch64_simd_tuple_types[i][j];
    }

  return NULL_TREE;
}

/* Return a type for an operand with specified mode and qualifiers.  */
static tree
aarch64_simd_builtin_type (machine_mode mode,
			   enum aarch64_type_qualifiers qualifiers)
{
  tree type = NULL_TREE;

  /* For pointers, we want a pointer to the basic type of the vector.  */
  if ((qualifiers & qualifier_pointer) && VECTOR_MODE_P (mode))
    mode = GET_MODE_INNER (mode);

  /* Non-poly scalar modes map to standard types not in the table.  */
  if ((qualifiers & qualifier_poly) || VECTOR_MODE_P (mode))
    type = aarch64_lookup_simd_type_in_table (mode, qualifiers);
  else
    type = aarch64_int_or_fp_type (mode, qualifiers);

  gcc_assert (type != NULL_TREE);

  /* Add qualifiers.  */
  if (qualifiers & qualifier_const)
    type = build_qualified_type (type, TYPE_QUAL_CONST);
  if (qualifiers & qualifier_pointer)
    type = build_pointer_type (type);

  return type;
}

static void
aarch64_init_simd_builtin_types (void)
{
  int i;
  int nelts = ARRAY_SIZE (aarch64_simd_types);
  tree tdecl;

  /* Init all the element types built by the front-end.  */
  aarch64_simd_types[Int8x8_t].eltype = intQI_type_node;
  aarch64_simd_types[Int8x16_t].eltype = intQI_type_node;
  aarch64_simd_types[Int16x4_t].eltype = intHI_type_node;
  aarch64_simd_types[Int16x8_t].eltype = intHI_type_node;
  aarch64_simd_types[Int32x2_t].eltype = intSI_type_node;
  aarch64_simd_types[Int32x4_t].eltype = intSI_type_node;
  aarch64_simd_types[Int64x1_t].eltype = intDI_type_node;
  aarch64_simd_types[Int64x2_t].eltype = intDI_type_node;
  aarch64_simd_types[Uint8x8_t].eltype = unsigned_intQI_type_node;
  aarch64_simd_types[Uint8x16_t].eltype = unsigned_intQI_type_node;
  aarch64_simd_types[Uint16x4_t].eltype = unsigned_intHI_type_node;
  aarch64_simd_types[Uint16x8_t].eltype = unsigned_intHI_type_node;
  aarch64_simd_types[Uint32x2_t].eltype = unsigned_intSI_type_node;
  aarch64_simd_types[Uint32x4_t].eltype = unsigned_intSI_type_node;
  aarch64_simd_types[Uint64x1_t].eltype = unsigned_intDI_type_node;
  aarch64_simd_types[Uint64x2_t].eltype = unsigned_intDI_type_node;

  /* Poly types are a world of their own.  */
  aarch64_simd_types[Poly8_t].eltype = aarch64_simd_types[Poly8_t].itype =
    build_distinct_type_copy (unsigned_intQI_type_node);
  /* Prevent front-ends from transforming Poly8_t arrays into string
     literals.  */
  TYPE_STRING_FLAG (aarch64_simd_types[Poly8_t].eltype) = false;

  aarch64_simd_types[Poly16_t].eltype = aarch64_simd_types[Poly16_t].itype =
    build_distinct_type_copy (unsigned_intHI_type_node);
  aarch64_simd_types[Poly64_t].eltype = aarch64_simd_types[Poly64_t].itype =
    build_distinct_type_copy (unsigned_intDI_type_node);
  aarch64_simd_types[Poly128_t].eltype = aarch64_simd_types[Poly128_t].itype =
    build_distinct_type_copy (unsigned_intTI_type_node);
  /* Init poly vector element types with scalar poly types.  */
  aarch64_simd_types[Poly8x8_t].eltype = aarch64_simd_types[Poly8_t].itype;
  aarch64_simd_types[Poly8x16_t].eltype = aarch64_simd_types[Poly8_t].itype;
  aarch64_simd_types[Poly16x4_t].eltype = aarch64_simd_types[Poly16_t].itype;
  aarch64_simd_types[Poly16x8_t].eltype = aarch64_simd_types[Poly16_t].itype;
  aarch64_simd_types[Poly64x1_t].eltype = aarch64_simd_types[Poly64_t].itype;
  aarch64_simd_types[Poly64x2_t].eltype = aarch64_simd_types[Poly64_t].itype;

  /* Continue with standard types.  */
  aarch64_simd_types[Float16x4_t].eltype = aarch64_fp16_type_node;
  aarch64_simd_types[Float16x8_t].eltype = aarch64_fp16_type_node;
  aarch64_simd_types[Float32x2_t].eltype = float_type_node;
  aarch64_simd_types[Float32x4_t].eltype = float_type_node;
  aarch64_simd_types[Float64x1_t].eltype = double_type_node;
  aarch64_simd_types[Float64x2_t].eltype = double_type_node;

  /* Init Bfloat vector types with underlying __bf16 type.  */
  aarch64_simd_types[Bfloat16x4_t].eltype = bfloat16_type_node;
  aarch64_simd_types[Bfloat16x8_t].eltype = bfloat16_type_node;

  /* Init FP8 element types.  */
  aarch64_simd_types[Mfloat8x8_t].eltype = aarch64_mfp8_type_node;
  aarch64_simd_types[Mfloat8x16_t].eltype = aarch64_mfp8_type_node;

  for (i = 0; i < nelts; i++)
    {
      tree eltype = aarch64_simd_types[i].eltype;
      machine_mode mode = aarch64_simd_types[i].mode;

      if (aarch64_simd_types[i].itype == NULL)
	{
	  tree type = build_vector_type (eltype, GET_MODE_NUNITS (mode));
	  type = build_distinct_type_copy (type);
	  SET_TYPE_STRUCTURAL_EQUALITY (type);

	  tree mangled_name = get_identifier (aarch64_simd_types[i].mangle);
	  tree value = tree_cons (NULL_TREE, mangled_name, NULL_TREE);
	  TYPE_ATTRIBUTES (type)
	    = tree_cons (get_identifier ("Advanced SIMD type"), value,
			 TYPE_ATTRIBUTES (type));
	  aarch64_simd_types[i].itype = type;
	}

      tdecl = add_builtin_type (aarch64_simd_types[i].name,
				aarch64_simd_types[i].itype);
      TYPE_NAME (aarch64_simd_types[i].itype) = tdecl;
    }

#define AARCH64_BUILD_SIGNED_TYPE(mode)  \
  make_signed_type (GET_MODE_PRECISION (mode));
  aarch64_simd_intOI_type_node = AARCH64_BUILD_SIGNED_TYPE (OImode);
  aarch64_simd_intCI_type_node = AARCH64_BUILD_SIGNED_TYPE (CImode);
  aarch64_simd_intXI_type_node = AARCH64_BUILD_SIGNED_TYPE (XImode);
#undef AARCH64_BUILD_SIGNED_TYPE

  tdecl = add_builtin_type
	    ("__builtin_aarch64_simd_oi" , aarch64_simd_intOI_type_node);
  TYPE_NAME (aarch64_simd_intOI_type_node) = tdecl;
  tdecl = add_builtin_type
	    ("__builtin_aarch64_simd_ci" , aarch64_simd_intCI_type_node);
  TYPE_NAME (aarch64_simd_intCI_type_node) = tdecl;
  tdecl = add_builtin_type
	    ("__builtin_aarch64_simd_xi" , aarch64_simd_intXI_type_node);
  TYPE_NAME (aarch64_simd_intXI_type_node) = tdecl;
}

static void
aarch64_init_simd_builtin_scalar_types (void)
{
  /* Define typedefs for all the standard scalar types.  */
  (*lang_hooks.types.register_builtin_type) (intQI_type_node,
					     "__builtin_aarch64_simd_qi");
  (*lang_hooks.types.register_builtin_type) (intHI_type_node,
					     "__builtin_aarch64_simd_hi");
  (*lang_hooks.types.register_builtin_type) (aarch64_fp16_type_node,
					     "__builtin_aarch64_simd_hf");
  (*lang_hooks.types.register_builtin_type) (intSI_type_node,
					     "__builtin_aarch64_simd_si");
  (*lang_hooks.types.register_builtin_type) (float_type_node,
					     "__builtin_aarch64_simd_sf");
  (*lang_hooks.types.register_builtin_type) (intDI_type_node,
					     "__builtin_aarch64_simd_di");
  (*lang_hooks.types.register_builtin_type) (double_type_node,
					     "__builtin_aarch64_simd_df");
  (*lang_hooks.types.register_builtin_type) (unsigned_intQI_type_node,
					     "__builtin_aarch64_simd_poly8");
  (*lang_hooks.types.register_builtin_type) (unsigned_intHI_type_node,
					     "__builtin_aarch64_simd_poly16");
  (*lang_hooks.types.register_builtin_type) (unsigned_intDI_type_node,
					     "__builtin_aarch64_simd_poly64");
  (*lang_hooks.types.register_builtin_type) (unsigned_intTI_type_node,
					     "__builtin_aarch64_simd_poly128");
  (*lang_hooks.types.register_builtin_type) (intTI_type_node,
					     "__builtin_aarch64_simd_ti");
  (*lang_hooks.types.register_builtin_type) (bfloat16_type_node,
					     "__builtin_aarch64_simd_bf");
  /* Unsigned integer types for various mode sizes.  */
  (*lang_hooks.types.register_builtin_type) (unsigned_intQI_type_node,
					     "__builtin_aarch64_simd_uqi");
  (*lang_hooks.types.register_builtin_type) (unsigned_intHI_type_node,
					     "__builtin_aarch64_simd_uhi");
  (*lang_hooks.types.register_builtin_type) (unsigned_intSI_type_node,
					     "__builtin_aarch64_simd_usi");
  (*lang_hooks.types.register_builtin_type) (unsigned_intDI_type_node,
					     "__builtin_aarch64_simd_udi");
}

/* Return a set of FLAG_* flags derived from FLAGS
   that describe what a function with result MODE could do,
   taking the command-line flags into account.  */
static unsigned int
aarch64_call_properties (unsigned int flags, machine_mode mode)
{
  if (!(flags & FLAG_AUTO_FP) && FLOAT_MODE_P (mode))
    flags |= FLAG_FP;

  /* -fno-trapping-math means that we can assume any FP exceptions
     are not user-visible.  */
  if (!flag_trapping_math)
    flags &= ~FLAG_RAISE_FP_EXCEPTIONS;

  return flags;
}

/* Return true if calls to a function with flags F and mode MODE
   could modify some form of global state.  */
static bool
aarch64_modifies_global_state_p (unsigned int f, machine_mode mode)
{
  unsigned int flags = aarch64_call_properties (f, mode);

  if (flags & FLAG_RAISE_FP_EXCEPTIONS)
    return true;

  if (flags & FLAG_PREFETCH_MEMORY)
    return true;

  return flags & FLAG_WRITE_MEMORY;
}

/* Return true if calls to a function with flags F and mode MODE
   could read some form of global state.  */
static bool
aarch64_reads_global_state_p (unsigned int f, machine_mode mode)
{
  unsigned int flags = aarch64_call_properties (f,  mode);

  if (flags & FLAG_READ_FPCR)
    return true;

  return flags & FLAG_READ_MEMORY;
}

/* Return true if calls to a function with flags F and mode MODE
   could raise a signal.  */
static bool
aarch64_could_trap_p (unsigned int f, machine_mode mode)
{
  unsigned int flags = aarch64_call_properties (f, mode);

  if (flags & FLAG_RAISE_FP_EXCEPTIONS)
    return true;

  if (flags & (FLAG_READ_MEMORY | FLAG_WRITE_MEMORY))
    return true;

  return false;
}

/* Add attribute NAME to ATTRS.  */
static tree
aarch64_add_attribute (const char *name, tree attrs)
{
  return tree_cons (get_identifier (name), NULL_TREE, attrs);
}

/* Return the appropriate attributes for a function that has
   flags F and mode MODE.  */
static tree
aarch64_get_attributes (unsigned int f, machine_mode mode)
{
  tree attrs = NULL_TREE;

  if (!aarch64_modifies_global_state_p (f, mode))
    {
      if (aarch64_reads_global_state_p (f, mode))
	attrs = aarch64_add_attribute ("pure", attrs);
      else
	attrs = aarch64_add_attribute ("const", attrs);
    }

  if (!flag_non_call_exceptions || !aarch64_could_trap_p (f, mode))
    attrs = aarch64_add_attribute ("nothrow", attrs);

  return aarch64_add_attribute ("leaf", attrs);
}

/* Due to the architecture not providing lane variant of the lane instructions
   for fcmla we can't use the standard simd builtin expansion code, but we
   still want the majority of the validation that would normally be done.  */

void
aarch64_init_fcmla_laneq_builtins (void)
{
  unsigned int i = 0;

  for (i = 0; i < ARRAY_SIZE (aarch64_fcmla_lane_builtin_data); ++i)
    {
      aarch64_fcmla_laneq_builtin_datum* d
	= &aarch64_fcmla_lane_builtin_data[i];
      tree argtype = aarch64_simd_builtin_type (d->mode, qualifier_none);
      machine_mode quadmode = GET_MODE_2XWIDER_MODE (d->mode).require ();
      tree quadtype = aarch64_simd_builtin_type (quadmode, qualifier_none);
      tree lanetype
	= aarch64_simd_builtin_type (SImode, qualifier_lane_pair_index);
      tree ftype = build_function_type_list (argtype, argtype, argtype,
					     quadtype, lanetype, NULL_TREE);
      tree attrs = aarch64_get_attributes (FLAG_FP, d->mode);
      tree fndecl
	= aarch64_general_add_builtin (d->name, ftype, d->fcode, attrs);

      aarch64_builtin_decls[d->fcode] = fndecl;
    }
}

void
aarch64_init_simd_intrinsics (void)
{
  unsigned int i = 0;

  for (i = 0; i < ARRAY_SIZE (aarch64_simd_intrinsic_data); ++i)
    {
      auto d = &aarch64_simd_intrinsic_data[i];

      if (d->skip)
	continue;

      tree return_type = void_type_node;
      tree args = void_list_node;

      for (int op_num = d->op_count - 1; op_num >= 0; op_num--)
	{
	  machine_mode op_mode = d->op_modes[op_num];
	  enum aarch64_type_qualifiers qualifiers = d->qualifiers[op_num];

	  tree eltype = aarch64_simd_builtin_type (op_mode, qualifiers);

	  if (op_num == 0)
	    return_type = eltype;
	  else
	    args = tree_cons (NULL_TREE, eltype, args);
	}

      tree ftype = build_function_type (return_type, args);
      tree attrs = aarch64_get_attributes (d->flags, d->op_modes[0]);
      unsigned int code
	      = (d->fcode << AARCH64_BUILTIN_SHIFT | AARCH64_BUILTIN_GENERAL);
      tree fndecl = simulate_builtin_function_decl (input_location, d->name,
						    ftype, code, NULL, attrs);
      aarch64_builtin_decls[d->fcode] = fndecl;
    }
}

void
aarch64_init_simd_builtin_functions (bool called_from_pragma)
{
  unsigned int i, fcode = AARCH64_SIMD_PATTERN_START;

  if (!called_from_pragma)
    {
      tree lane_check_fpr = build_function_type_list (void_type_node,
						      size_type_node,
						      size_type_node,
						      intSI_type_node,
						      NULL);
      aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_LANE_CHECK]
	= aarch64_general_add_builtin ("__builtin_aarch64_im_lane_boundsi",
				       lane_check_fpr,
				       AARCH64_SIMD_BUILTIN_LANE_CHECK);
    }

  for (i = 0; i < ARRAY_SIZE (aarch64_simd_builtin_data); i++, fcode++)
    {
      bool print_type_signature_p = false;
      char type_signature[SIMD_MAX_BUILTIN_ARGS + 1] = { 0 };
      aarch64_simd_builtin_datum *d = &aarch64_simd_builtin_data[i];
      char namebuf[60];
      tree ftype = NULL;
      tree fndecl = NULL;

      d->fcode = fcode;

      /* We must track two variables here.  op_num is
	 the operand number as in the RTL pattern.  This is
	 required to access the mode (e.g. V4SF mode) of the
	 argument, from which the base type can be derived.
	 arg_num is an index in to the qualifiers data, which
	 gives qualifiers to the type (e.g. const unsigned).
	 The reason these two variables may differ by one is the
	 void return type.  While all return types take the 0th entry
	 in the qualifiers array, there is no operand for them in the
	 RTL pattern.  */
      int op_num = insn_data[d->code].n_operands - 1;
      int arg_num = d->qualifiers[0] & qualifier_void
		      ? op_num + 1
		      : op_num;
      tree return_type = void_type_node, args = void_list_node;
      tree eltype;

      int struct_mode_args = 0;
      for (int j = op_num; j >= 0; j--)
	{
	  machine_mode op_mode = insn_data[d->code].operand[j].mode;
	  if (aarch64_advsimd_struct_mode_p (op_mode))
	    struct_mode_args++;
	}

      if ((called_from_pragma && struct_mode_args == 0)
	  || (!called_from_pragma && struct_mode_args > 0))
	continue;

      /* Build a function type directly from the insn_data for this
	 builtin.  The build_function_type () function takes care of
	 removing duplicates for us.  */
      for (; op_num >= 0; arg_num--, op_num--)
	{
	  machine_mode op_mode = insn_data[d->code].operand[op_num].mode;
	  enum aarch64_type_qualifiers qualifiers = d->qualifiers[arg_num];

	  if (qualifiers & qualifier_unsigned)
	    {
	      type_signature[op_num] = 'u';
	      print_type_signature_p = true;
	    }
	  else if (qualifiers & qualifier_poly)
	    {
	      type_signature[op_num] = 'p';
	      print_type_signature_p = true;
	    }
	  else
	    type_signature[op_num] = 's';

	  /* Some builtins have different user-facing types
	     for certain arguments, encoded in d->mode.  */
	  if (qualifiers & qualifier_map_mode)
	      op_mode = d->mode;

	  eltype = aarch64_simd_builtin_type (op_mode, qualifiers);

	  /* If we have reached arg_num == 0, we are at a non-void
	     return type.  Otherwise, we are still processing
	     arguments.  */
	  if (arg_num == 0)
	    return_type = eltype;
	  else
	    args = tree_cons (NULL_TREE, eltype, args);
	}

      ftype = build_function_type (return_type, args);

      gcc_assert (ftype != NULL);

      if (print_type_signature_p)
	snprintf (namebuf, sizeof (namebuf), "__builtin_aarch64_%s_%s",
		  d->name, type_signature);
      else
	snprintf (namebuf, sizeof (namebuf), "__builtin_aarch64_%s",
		  d->name);

      tree attrs = aarch64_get_attributes (d->flags, d->mode);

      if (called_from_pragma)
	{
	  unsigned int raw_code
		= (fcode << AARCH64_BUILTIN_SHIFT) | AARCH64_BUILTIN_GENERAL;
	  fndecl = simulate_builtin_function_decl (input_location, namebuf,
						   ftype, raw_code, NULL,
						   attrs);
	}
      else
	fndecl = aarch64_general_add_builtin (namebuf, ftype, fcode, attrs);

      aarch64_builtin_decls[fcode] = fndecl;
    }
}

enum class aarch64_builtin_signatures
{
  binary,
};

#undef ENTRY
#define ENTRY(N, S, M, U) \
  {#N, aarch64_builtin_signatures::S, E_##M##mode, U, \
   aarch64_required_extensions::REQUIRED_EXTENSIONS},

/* Initialize pragma builtins.  */

struct aarch64_pragma_builtins_data
{
  const char *name;
  aarch64_builtin_signatures signature;
  machine_mode mode;
  int unspec;
  aarch64_required_extensions required_extensions;
};

static aarch64_pragma_builtins_data aarch64_pragma_builtins[] = {
#include "aarch64-simd-pragma-builtins.def"
};

static tree
aarch64_fntype (const aarch64_pragma_builtins_data &builtin_data)
{
  auto type = aarch64_simd_builtin_type (builtin_data.mode, qualifier_none);
  switch (builtin_data.signature)
    {
    case aarch64_builtin_signatures::binary:
      return build_function_type_list (type, type, type, NULL_TREE);
    default:
      gcc_unreachable ();
    }
}

static void
aarch64_init_pragma_builtins ()
{
  for (size_t i = 0; i < ARRAY_SIZE (aarch64_pragma_builtins); ++i)
    {
      auto data = aarch64_pragma_builtins[i];
      auto fntype = aarch64_fntype (data);
      auto code = AARCH64_PRAGMA_BUILTIN_START + i + 1;
      aarch64_builtin_decls[code]
	= aarch64_general_simulate_builtin (data.name, fntype, code);
    }
}

/* If the builtin function with code CODE has an entry in
   aarch64_pragma_builtins, return its entry, otherwise return null.  */

static const aarch64_pragma_builtins_data*
aarch64_get_pragma_builtin (int code)
{
  if (!(code > AARCH64_PRAGMA_BUILTIN_START
	&& code < AARCH64_PRAGMA_BUILTIN_END))
    return NULL;

  auto idx = code - (AARCH64_PRAGMA_BUILTIN_START + 1);
  return &aarch64_pragma_builtins[idx];
}

/* Register the tuple type that contains NUM_VECTORS of the AdvSIMD type
   indexed by TYPE_INDEX.  */
static void
register_tuple_type (unsigned int num_vectors, unsigned int type_index)
{
  aarch64_simd_type_info *type = &aarch64_simd_types[type_index];

  /* Synthesize the name of the user-visible vector tuple type.  */
  const char *vector_type_name = type->name;
  char tuple_type_name[sizeof ("bfloat16x4x2_t")];
  snprintf (tuple_type_name, sizeof (tuple_type_name), "%.*sx%d_t",
	    (int) strlen (vector_type_name) - 4, vector_type_name + 2,
	    num_vectors);
  tuple_type_name[0] = TOLOWER (tuple_type_name[0]);

  tree vector_type = type->itype;
  tree array_type = build_array_type_nelts (vector_type, num_vectors);
  if (type->mode == DImode)
    {
      if (num_vectors == 2)
	SET_TYPE_MODE (array_type, V2x1DImode);
      else if (num_vectors == 3)
	SET_TYPE_MODE (array_type, V3x1DImode);
      else if (num_vectors == 4)
	SET_TYPE_MODE (array_type, V4x1DImode);
    }

  unsigned int alignment
    = known_eq (GET_MODE_SIZE (type->mode), 16) ? 128 : 64;
  machine_mode tuple_mode = TYPE_MODE_RAW (array_type);
  gcc_assert (VECTOR_MODE_P (tuple_mode)
	      && TYPE_MODE (array_type) == tuple_mode
	      && TYPE_ALIGN (array_type) == alignment);

  tree field = build_decl (input_location, FIELD_DECL,
			   get_identifier ("val"), array_type);

  tree t = lang_hooks.types.simulate_record_decl (input_location,
						  tuple_type_name,
						  make_array_slice (&field,
								    1));
  gcc_assert (TYPE_MODE_RAW (t) == TYPE_MODE (t)
	      && (flag_pack_struct
		  || maximum_field_alignment
		  || (TYPE_MODE_RAW (t) == tuple_mode
		      && TYPE_ALIGN (t) == alignment)));

  aarch64_simd_tuple_modes[type_index][num_vectors - 2] = tuple_mode;
  aarch64_simd_tuple_types[type_index][num_vectors - 2] = t;
}

static bool
aarch64_scalar_builtin_type_p (aarch64_simd_type t)
{
  return (t == Poly8_t || t == Poly16_t || t == Poly64_t || t == Poly128_t);
}

/* Enable AARCH64_FL_* flags EXTRA_FLAGS on top of the base Advanced SIMD
   set.  */
aarch64_simd_switcher::aarch64_simd_switcher (aarch64_feature_flags extra_flags)
  : m_old_asm_isa_flags (aarch64_asm_isa_flags),
    m_old_general_regs_only (TARGET_GENERAL_REGS_ONLY)
{
  /* Changing the ISA flags should be enough here.  We shouldn't need to
     pay the compile-time cost of a full target switch.  */
  global_options.x_target_flags &= ~MASK_GENERAL_REGS_ONLY;
  aarch64_set_asm_isa_flags (AARCH64_FL_FP | AARCH64_FL_SIMD | extra_flags);
}

aarch64_simd_switcher::~aarch64_simd_switcher ()
{
  if (m_old_general_regs_only)
    global_options.x_target_flags |= MASK_GENERAL_REGS_ONLY;
  aarch64_set_asm_isa_flags (m_old_asm_isa_flags);
}

/* Implement #pragma GCC aarch64 "arm_neon.h".

   The types and functions defined here need to be available internally
   during LTO as well.  */
void
handle_arm_neon_h (void)
{
  aarch64_simd_switcher simd;

  /* Register the AdvSIMD vector tuple types.  */
  for (unsigned int i = 0; i < ARM_NEON_H_TYPES_LAST; i++)
    for (unsigned int count = 2; count <= 4; ++count)
      if (!aarch64_scalar_builtin_type_p (aarch64_simd_types[i].type))
	register_tuple_type (count, i);

  aarch64_init_simd_builtin_functions (true);
  aarch64_init_simd_intrinsics ();
  aarch64_init_pragma_builtins ();
}

static void
aarch64_init_simd_builtins (void)
{
  aarch64_init_simd_builtin_types ();

  /* Strong-typing hasn't been implemented for all AdvSIMD builtin intrinsics.
     Therefore we need to preserve the old __builtin scalar types.  It can be
     removed once all the intrinsics become strongly typed using the qualifier
     system.  */
  aarch64_init_simd_builtin_scalar_types ();

  aarch64_init_simd_builtin_functions (false);
  if (in_lto_p)
    handle_arm_neon_h ();

  /* Initialize the remaining fcmla_laneq intrinsics.  */
  aarch64_init_fcmla_laneq_builtins ();
}

static void
aarch64_init_crc32_builtins ()
{
  tree usi_type = aarch64_simd_builtin_type (SImode, qualifier_unsigned);
  unsigned int i = 0;

  for (i = 0; i < ARRAY_SIZE (aarch64_crc_builtin_data); ++i)
    {
      aarch64_crc_builtin_datum* d = &aarch64_crc_builtin_data[i];
      tree argtype = aarch64_simd_builtin_type (d->mode, qualifier_unsigned);
      tree ftype = build_function_type_list (usi_type, usi_type, argtype, NULL_TREE);
      tree attrs = aarch64_get_attributes (FLAG_NONE, d->mode);
      tree fndecl
	= aarch64_general_add_builtin (d->name, ftype, d->fcode, attrs);

      aarch64_builtin_decls[d->fcode] = fndecl;
    }
}

/* Add builtins for reciprocal square root.  */

void
aarch64_init_builtin_rsqrt (void)
{
  tree fndecl = NULL;
  tree ftype = NULL;

  tree V2SF_type_node = build_vector_type (float_type_node, 2);
  tree V2DF_type_node = build_vector_type (double_type_node, 2);
  tree V4SF_type_node = build_vector_type (float_type_node, 4);

  struct builtin_decls_data
  {
    tree type_node;
    const char *builtin_name;
    int function_code;
  };

  builtin_decls_data bdda[] =
  {
    { double_type_node, "__builtin_aarch64_rsqrt_df", AARCH64_BUILTIN_RSQRT_DF },
    { float_type_node, "__builtin_aarch64_rsqrt_sf", AARCH64_BUILTIN_RSQRT_SF },
    { V2DF_type_node, "__builtin_aarch64_rsqrt_v2df", AARCH64_BUILTIN_RSQRT_V2DF },
    { V2SF_type_node, "__builtin_aarch64_rsqrt_v2sf", AARCH64_BUILTIN_RSQRT_V2SF },
    { V4SF_type_node, "__builtin_aarch64_rsqrt_v4sf", AARCH64_BUILTIN_RSQRT_V4SF }
  };

  builtin_decls_data *bdd = bdda;
  builtin_decls_data *bdd_end = bdd + (ARRAY_SIZE (bdda));

  for (; bdd < bdd_end; bdd++)
  {
    ftype = build_function_type_list (bdd->type_node, bdd->type_node, NULL_TREE);
    tree attrs = aarch64_get_attributes (FLAG_FP, TYPE_MODE (bdd->type_node));
    fndecl = aarch64_general_add_builtin (bdd->builtin_name,
					  ftype, bdd->function_code, attrs);
    aarch64_builtin_decls[bdd->function_code] = fndecl;
  }
}

/* Initialize the backend type that supports the user-visible __mfp8
   type and its relative pointer type.  */

static void
aarch64_init_fp8_types (void)
{
  aarch64_mfp8_type_node = make_unsigned_type (8);
  SET_TYPE_MODE (aarch64_mfp8_type_node, QImode);

  lang_hooks.types.register_builtin_type (aarch64_mfp8_type_node, "__mfp8");
  aarch64_mfp8_ptr_type_node = build_pointer_type (aarch64_mfp8_type_node);
}

/* Initialize the backend types that support the user-visible __fp16
   type, also initialize a pointer to that type, to be used when
   forming HFAs.  */

static void
aarch64_init_fp16_types (void)
{
  aarch64_fp16_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (aarch64_fp16_type_node) = 16;
  layout_type (aarch64_fp16_type_node);

  (*lang_hooks.types.register_builtin_type) (aarch64_fp16_type_node, "__fp16");
  aarch64_fp16_ptr_type_node = build_pointer_type (aarch64_fp16_type_node);
}

/* Initialize the backend REAL_TYPE type supporting bfloat types.  */
static void
aarch64_init_bf16_types (void)
{
  lang_hooks.types.register_builtin_type (bfloat16_type_node, "__bf16");
  aarch64_bf16_ptr_type_node = build_pointer_type (bfloat16_type_node);
}

/* Pointer authentication builtins that will become NOP on legacy platform.
   Currently, these builtins are for internal use only (libgcc EH unwinder).  */

void
aarch64_init_pauth_hint_builtins (void)
{
  /* Pointer Authentication builtins.  */
  tree ftype_pointer_auth
    = build_function_type_list (ptr_type_node, ptr_type_node,
				unsigned_intDI_type_node, NULL_TREE);
  tree ftype_pointer_strip
    = build_function_type_list (ptr_type_node, ptr_type_node, NULL_TREE);

  aarch64_builtin_decls[AARCH64_PAUTH_BUILTIN_AUTIA1716]
    = aarch64_general_add_builtin ("__builtin_aarch64_autia1716",
				   ftype_pointer_auth,
				   AARCH64_PAUTH_BUILTIN_AUTIA1716);
  aarch64_builtin_decls[AARCH64_PAUTH_BUILTIN_PACIA1716]
    = aarch64_general_add_builtin ("__builtin_aarch64_pacia1716",
				   ftype_pointer_auth,
				   AARCH64_PAUTH_BUILTIN_PACIA1716);
  aarch64_builtin_decls[AARCH64_PAUTH_BUILTIN_AUTIB1716]
    = aarch64_general_add_builtin ("__builtin_aarch64_autib1716",
				   ftype_pointer_auth,
				   AARCH64_PAUTH_BUILTIN_AUTIB1716);
  aarch64_builtin_decls[AARCH64_PAUTH_BUILTIN_PACIB1716]
    = aarch64_general_add_builtin ("__builtin_aarch64_pacib1716",
				   ftype_pointer_auth,
				   AARCH64_PAUTH_BUILTIN_PACIB1716);
  aarch64_builtin_decls[AARCH64_PAUTH_BUILTIN_XPACLRI]
    = aarch64_general_add_builtin ("__builtin_aarch64_xpaclri",
				   ftype_pointer_strip,
				   AARCH64_PAUTH_BUILTIN_XPACLRI);
}

/* Initialize the transactional memory extension (TME) builtins.  */
static void
aarch64_init_tme_builtins (void)
{
  tree ftype_uint64_void
    = build_function_type_list (uint64_type_node, NULL);
  tree ftype_void_void
    = build_function_type_list (void_type_node, NULL);
  tree ftype_void_uint64
    = build_function_type_list (void_type_node, uint64_type_node, NULL);

  aarch64_builtin_decls[AARCH64_TME_BUILTIN_TSTART]
    = aarch64_general_simulate_builtin ("__tstart", ftype_uint64_void,
					AARCH64_TME_BUILTIN_TSTART);
  aarch64_builtin_decls[AARCH64_TME_BUILTIN_TTEST]
    = aarch64_general_simulate_builtin ("__ttest", ftype_uint64_void,
					AARCH64_TME_BUILTIN_TTEST);
  aarch64_builtin_decls[AARCH64_TME_BUILTIN_TCOMMIT]
    = aarch64_general_simulate_builtin ("__tcommit", ftype_void_void,
					AARCH64_TME_BUILTIN_TCOMMIT);
  aarch64_builtin_decls[AARCH64_TME_BUILTIN_TCANCEL]
    = aarch64_general_simulate_builtin ("__tcancel", ftype_void_uint64,
					AARCH64_TME_BUILTIN_TCANCEL);
}

/* Add builtins for Random Number instructions.  */

static void
aarch64_init_rng_builtins (void)
{
  tree unsigned_ptr_type
    = build_pointer_type (get_typenode_from_name (UINT64_TYPE));
  tree ftype
    = build_function_type_list (integer_type_node, unsigned_ptr_type, NULL);
  aarch64_builtin_decls[AARCH64_BUILTIN_RNG_RNDR]
    = aarch64_general_add_builtin ("__builtin_aarch64_rndr", ftype,
				   AARCH64_BUILTIN_RNG_RNDR);
  aarch64_builtin_decls[AARCH64_BUILTIN_RNG_RNDRRS]
    = aarch64_general_add_builtin ("__builtin_aarch64_rndrrs", ftype,
				   AARCH64_BUILTIN_RNG_RNDRRS);
}

/* Add builtins for reading system register.  */
static void
aarch64_init_rwsr_builtins (void)
{
  tree fntype = NULL;
  tree const_char_ptr_type
    = build_pointer_type (build_type_variant (char_type_node, true, false));

#define AARCH64_INIT_RWSR_BUILTINS_DECL(F, N, T) \
  aarch64_builtin_decls[AARCH64_##F] \
    = aarch64_general_add_builtin ("__builtin_aarch64_"#N, T, AARCH64_##F);

  fntype
    = build_function_type_list (uint32_type_node, const_char_ptr_type, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (RSR, rsr, fntype);

  fntype
    = build_function_type_list (ptr_type_node, const_char_ptr_type, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (RSRP, rsrp, fntype);

  fntype
    = build_function_type_list (uint64_type_node, const_char_ptr_type, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (RSR64, rsr64, fntype);

  fntype
    = build_function_type_list (float_type_node, const_char_ptr_type, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (RSRF, rsrf, fntype);

  fntype
    = build_function_type_list (double_type_node, const_char_ptr_type, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (RSRF64, rsrf64, fntype);

  fntype
    = build_function_type_list (uint128_type_node, const_char_ptr_type, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (RSR128, rsr128, fntype);

  fntype
    = build_function_type_list (void_type_node, const_char_ptr_type,
				uint32_type_node, NULL);

  AARCH64_INIT_RWSR_BUILTINS_DECL (WSR, wsr, fntype);

  fntype
    = build_function_type_list (void_type_node, const_char_ptr_type,
				const_ptr_type_node, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (WSRP, wsrp, fntype);

  fntype
    = build_function_type_list (void_type_node, const_char_ptr_type,
				uint64_type_node, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (WSR64, wsr64, fntype);

  fntype
    = build_function_type_list (void_type_node, const_char_ptr_type,
				float_type_node, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (WSRF, wsrf, fntype);

  fntype
    = build_function_type_list (void_type_node, const_char_ptr_type,
				double_type_node, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (WSRF64, wsrf64, fntype);

  fntype
    = build_function_type_list (void_type_node, const_char_ptr_type,
				uint128_type_node, NULL);
  AARCH64_INIT_RWSR_BUILTINS_DECL (WSR128, wsr128, fntype);
}

/* Add builtins for data and instrution prefetch.  */
static void
aarch64_init_prefetch_builtin (void)
{
#define AARCH64_INIT_PREFETCH_BUILTIN(INDEX, N)				\
  aarch64_builtin_decls[INDEX] =					\
    aarch64_general_add_builtin ("__builtin_aarch64_" N, ftype, INDEX)

  tree ftype;
  tree cv_argtype;
  cv_argtype = build_qualified_type (void_type_node, TYPE_QUAL_CONST
						     | TYPE_QUAL_VOLATILE);
  cv_argtype = build_pointer_type (cv_argtype);

  ftype = build_function_type_list (void_type_node, cv_argtype, NULL);
  AARCH64_INIT_PREFETCH_BUILTIN (AARCH64_PLD, "pld");
  AARCH64_INIT_PREFETCH_BUILTIN (AARCH64_PLI, "pli");

  ftype = build_function_type_list (void_type_node, unsigned_type_node,
				    unsigned_type_node, unsigned_type_node,
				    cv_argtype, NULL);
  AARCH64_INIT_PREFETCH_BUILTIN (AARCH64_PLDX, "pldx");

  ftype = build_function_type_list (void_type_node, unsigned_type_node,
				    unsigned_type_node, cv_argtype, NULL);
  AARCH64_INIT_PREFETCH_BUILTIN (AARCH64_PLIX, "plix");
}

/* Initialize the memory tagging extension (MTE) builtins.  */
static GTY(()) struct GTY(())
{
  tree ftype;
  enum insn_code icode;
} aarch64_memtag_builtin_data[AARCH64_MEMTAG_BUILTIN_END -
			      AARCH64_MEMTAG_BUILTIN_START - 1];

static void
aarch64_init_memtag_builtins (void)
{
  tree fntype = NULL;

#define AARCH64_INIT_MEMTAG_BUILTINS_DECL(F, N, I, T) \
  aarch64_builtin_decls[AARCH64_MEMTAG_BUILTIN_##F] \
    = aarch64_general_simulate_builtin ("__arm_mte_"#N, T, \
					AARCH64_MEMTAG_BUILTIN_##F); \
  aarch64_memtag_builtin_data[AARCH64_MEMTAG_BUILTIN_##F - \
			      AARCH64_MEMTAG_BUILTIN_START - 1] = \
				{T, CODE_FOR_##I};

  fntype = build_function_type_list (ptr_type_node, ptr_type_node,
				     uint64_type_node, NULL);
  AARCH64_INIT_MEMTAG_BUILTINS_DECL (IRG, create_random_tag, irg, fntype);

  fntype = build_function_type_list (uint64_type_node, ptr_type_node,
				     uint64_type_node, NULL);
  AARCH64_INIT_MEMTAG_BUILTINS_DECL (GMI, exclude_tag, gmi, fntype);

  fntype = build_function_type_list (ptrdiff_type_node, ptr_type_node,
				     ptr_type_node, NULL);
  AARCH64_INIT_MEMTAG_BUILTINS_DECL (SUBP, ptrdiff, subp, fntype);

  fntype = build_function_type_list (ptr_type_node, ptr_type_node,
				     unsigned_type_node, NULL);
  AARCH64_INIT_MEMTAG_BUILTINS_DECL (INC_TAG, increment_tag, addg, fntype);

  fntype = build_function_type_list (void_type_node, ptr_type_node, NULL);
  AARCH64_INIT_MEMTAG_BUILTINS_DECL (SET_TAG, set_tag, stg, fntype);

  fntype = build_function_type_list (ptr_type_node, ptr_type_node, NULL);
  AARCH64_INIT_MEMTAG_BUILTINS_DECL (GET_TAG, get_tag, ldg, fntype);

#undef AARCH64_INIT_MEMTAG_BUILTINS_DECL
}

/* Add builtins for Load/store 64 Byte instructions.  */

typedef struct
{
  const char *name;
  unsigned int code;
  tree type;
} ls64_builtins_data;

static GTY(()) tree ls64_arm_data_t = NULL_TREE;

static void
aarch64_init_ls64_builtins_types (void)
{
  /* Synthesize:

     typedef struct {
       uint64_t val[8];
     } __arm_data512_t;  */
  const char *tuple_type_name = "__arm_data512_t";
  tree node_type = get_typenode_from_name (UINT64_TYPE);
  tree array_type = build_array_type_nelts (node_type, 8);
  SET_TYPE_MODE (array_type, V8DImode);

  gcc_assert (TYPE_MODE_RAW (array_type) == TYPE_MODE (array_type));
  gcc_assert (TYPE_ALIGN (array_type) == 64);

  tree field = build_decl (input_location, FIELD_DECL,
			   get_identifier ("val"), array_type);

  ls64_arm_data_t = lang_hooks.types.simulate_record_decl (input_location,
			 tuple_type_name,
			 make_array_slice (&field, 1));

  gcc_assert (TYPE_MODE (ls64_arm_data_t) == V8DImode);
  gcc_assert (TYPE_MODE_RAW (ls64_arm_data_t) == TYPE_MODE (ls64_arm_data_t));
  gcc_assert (TYPE_ALIGN (ls64_arm_data_t) == 64);
}

static void
aarch64_init_ls64_builtins (void)
{
  aarch64_init_ls64_builtins_types ();

  ls64_builtins_data data[4] = {
    {"__arm_ld64b", AARCH64_LS64_BUILTIN_LD64B,
     build_function_type_list (ls64_arm_data_t,
			       const_ptr_type_node, NULL_TREE)},
    {"__arm_st64b", AARCH64_LS64_BUILTIN_ST64B,
     build_function_type_list (void_type_node, ptr_type_node,
			       ls64_arm_data_t, NULL_TREE)},
    {"__arm_st64bv", AARCH64_LS64_BUILTIN_ST64BV,
     build_function_type_list (uint64_type_node, ptr_type_node,
			       ls64_arm_data_t, NULL_TREE)},
    {"__arm_st64bv0", AARCH64_LS64_BUILTIN_ST64BV0,
     build_function_type_list (uint64_type_node, ptr_type_node,
			       ls64_arm_data_t, NULL_TREE)},
  };

  for (size_t i = 0; i < ARRAY_SIZE (data); ++i)
    aarch64_builtin_decls[data[i].code]
      = aarch64_general_simulate_builtin (data[i].name, data[i].type,
					  data[i].code);
}

static void
aarch64_init_data_intrinsics (void)
{
  tree uint32_fntype = build_function_type_list (uint32_type_node,
						 uint32_type_node, NULL_TREE);
  tree ulong_fntype = build_function_type_list (long_unsigned_type_node,
						long_unsigned_type_node,
						NULL_TREE);
  tree uint64_fntype = build_function_type_list (uint64_type_node,
						 uint64_type_node, NULL_TREE);
  aarch64_builtin_decls[AARCH64_REV16]
    = aarch64_general_add_builtin ("__builtin_aarch64_rev16", uint32_fntype,
				   AARCH64_REV16);
  aarch64_builtin_decls[AARCH64_REV16L]
    = aarch64_general_add_builtin ("__builtin_aarch64_rev16l", ulong_fntype,
				   AARCH64_REV16L);
  aarch64_builtin_decls[AARCH64_REV16LL]
    = aarch64_general_add_builtin ("__builtin_aarch64_rev16ll", uint64_fntype,
				   AARCH64_REV16LL);
  aarch64_builtin_decls[AARCH64_RBIT]
    = aarch64_general_add_builtin ("__builtin_aarch64_rbit", uint32_fntype,
				   AARCH64_RBIT);
  aarch64_builtin_decls[AARCH64_RBITL]
    = aarch64_general_add_builtin ("__builtin_aarch64_rbitl", ulong_fntype,
				   AARCH64_RBITL);
  aarch64_builtin_decls[AARCH64_RBITLL]
    = aarch64_general_add_builtin ("__builtin_aarch64_rbitll", uint64_fntype,
				   AARCH64_RBITLL);
}

/* Implement #pragma GCC aarch64 "arm_acle.h".  */
void
handle_arm_acle_h (void)
{
  aarch64_init_ls64_builtins ();
  aarch64_init_tme_builtins ();
  aarch64_init_memtag_builtins ();
}

/* Initialize fpsr fpcr getters and setters.  */

static void
aarch64_init_fpsr_fpcr_builtins (void)
{
  tree ftype_set
    = build_function_type_list (void_type_node, unsigned_type_node, NULL);
  tree ftype_get
    = build_function_type_list (unsigned_type_node, NULL);

  aarch64_builtin_decls[AARCH64_BUILTIN_GET_FPCR]
    = aarch64_general_add_builtin ("__builtin_aarch64_get_fpcr",
				   ftype_get,
				   AARCH64_BUILTIN_GET_FPCR);
  aarch64_builtin_decls[AARCH64_BUILTIN_SET_FPCR]
    = aarch64_general_add_builtin ("__builtin_aarch64_set_fpcr",
				   ftype_set,
				   AARCH64_BUILTIN_SET_FPCR);
  aarch64_builtin_decls[AARCH64_BUILTIN_GET_FPSR]
    = aarch64_general_add_builtin ("__builtin_aarch64_get_fpsr",
				   ftype_get,
				   AARCH64_BUILTIN_GET_FPSR);
  aarch64_builtin_decls[AARCH64_BUILTIN_SET_FPSR]
    = aarch64_general_add_builtin ("__builtin_aarch64_set_fpsr",
				   ftype_set,
				   AARCH64_BUILTIN_SET_FPSR);

  ftype_set
    = build_function_type_list (void_type_node, long_long_unsigned_type_node,
				NULL);
  ftype_get
    = build_function_type_list (long_long_unsigned_type_node, NULL);

  aarch64_builtin_decls[AARCH64_BUILTIN_GET_FPCR64]
    = aarch64_general_add_builtin ("__builtin_aarch64_get_fpcr64",
				   ftype_get,
				   AARCH64_BUILTIN_GET_FPCR64);
  aarch64_builtin_decls[AARCH64_BUILTIN_SET_FPCR64]
    = aarch64_general_add_builtin ("__builtin_aarch64_set_fpcr64",
				   ftype_set,
				   AARCH64_BUILTIN_SET_FPCR64);
  aarch64_builtin_decls[AARCH64_BUILTIN_GET_FPSR64]
    = aarch64_general_add_builtin ("__builtin_aarch64_get_fpsr64",
				   ftype_get,
				   AARCH64_BUILTIN_GET_FPSR64);
  aarch64_builtin_decls[AARCH64_BUILTIN_SET_FPSR64]
    = aarch64_general_add_builtin ("__builtin_aarch64_set_fpsr64",
				   ftype_set,
				   AARCH64_BUILTIN_SET_FPSR64);
}

/* Add builtins for Guarded Control Stack instructions.  */

static void
aarch64_init_gcs_builtins (void)
{
  tree ftype;

  ftype = build_function_type_list (ptr_type_node, NULL);
  aarch64_builtin_decls[AARCH64_BUILTIN_GCSPR]
    = aarch64_general_add_builtin ("__builtin_aarch64_gcspr", ftype,
				   AARCH64_BUILTIN_GCSPR);

  ftype = build_function_type_list (uint64_type_node, NULL);
  aarch64_builtin_decls[AARCH64_BUILTIN_GCSPOPM]
    = aarch64_general_add_builtin ("__builtin_aarch64_gcspopm", ftype,
				   AARCH64_BUILTIN_GCSPOPM);

  ftype = build_function_type_list (ptr_type_node, ptr_type_node, NULL);
  aarch64_builtin_decls[AARCH64_BUILTIN_GCSSS]
    = aarch64_general_add_builtin ("__builtin_aarch64_gcsss", ftype,
				   AARCH64_BUILTIN_GCSSS);
}

/* Initialize all builtins in the AARCH64_BUILTIN_GENERAL group.  */

void
aarch64_general_init_builtins (void)
{
  aarch64_init_fpsr_fpcr_builtins ();

  aarch64_init_fp8_types ();

  aarch64_init_fp16_types ();

  aarch64_init_bf16_types ();

  {
    aarch64_simd_switcher simd;
    aarch64_init_simd_builtins ();
  }

  aarch64_init_crc32_builtins ();
  aarch64_init_builtin_rsqrt ();
  aarch64_init_rng_builtins ();
  aarch64_init_data_intrinsics ();

  aarch64_init_rwsr_builtins ();
  aarch64_init_prefetch_builtin ();

  tree ftype_jcvt
    = build_function_type_list (intSI_type_node, double_type_node, NULL);
  aarch64_builtin_decls[AARCH64_JSCVT]
    = aarch64_general_add_builtin ("__builtin_aarch64_jcvtzs", ftype_jcvt,
				   AARCH64_JSCVT);

  /* Initialize pointer authentication builtins which are backed by instructions
     in NOP encoding space.

     NOTE: these builtins are supposed to be used by libgcc unwinder only, as
     there is no support on return address signing under ILP32, we don't
     register them.  */
  if (!TARGET_ILP32)
    aarch64_init_pauth_hint_builtins ();

  tree ftype_chkfeat
    = build_function_type_list (uint64_type_node, uint64_type_node, NULL);
  aarch64_builtin_decls[AARCH64_BUILTIN_CHKFEAT]
    = aarch64_general_add_builtin ("__builtin_aarch64_chkfeat", ftype_chkfeat,
				   AARCH64_BUILTIN_CHKFEAT);

  aarch64_init_gcs_builtins ();

  if (in_lto_p)
    handle_arm_acle_h ();
}

/* Implement TARGET_BUILTIN_DECL for the AARCH64_BUILTIN_GENERAL group.  */
tree
aarch64_general_builtin_decl (unsigned code, bool)
{
  if (code >= AARCH64_BUILTIN_MAX)
    return error_mark_node;

  return aarch64_builtin_decls[code];
}

/* True if we've already complained about attempts to use functions
   when the required extension is disabled.  */
static bool reported_missing_extension_p;

/* True if we've already complained about attempts to use functions
   which require registers that are missing.  */
static bool reported_missing_registers_p;

/* Report an error against LOCATION that the user has tried to use
   function FNDECL when extension EXTENSION is disabled.  */
static void
aarch64_report_missing_extension (location_t location, tree fndecl,
				  const char *extension)
{
  /* Avoid reporting a slew of messages for a single oversight.  */
  if (reported_missing_extension_p)
    return;

  error_at (location, "ACLE function %qD requires ISA extension %qs",
	    fndecl, extension);
  inform (location, "you can enable %qs using the command-line"
	  " option %<-march%>, or by using the %<target%>"
	  " attribute or pragma", extension);
  reported_missing_extension_p = true;
}

/* Report an error against LOCATION that the user has tried to use
   function FNDECL when non-general registers are disabled.  */
static void
aarch64_report_missing_registers (location_t location, tree fndecl)
{
  /* Avoid reporting a slew of messages for a single oversight.  */
  if (reported_missing_registers_p)
    return;

  error_at (location,
	    "ACLE function %qD is incompatible with the use of %qs",
	    fndecl, "-mgeneral-regs-only");
  reported_missing_registers_p = true;
}

/* Check whether the requirements in REQUIRED_EXTENSIONS are met, given that
   those requirements come from calling function FNDECL.  Report an error
   against LOCATION if not.  */
bool
aarch64_check_required_extensions (location_t location, tree fndecl,
				   aarch64_required_extensions
				     required_extensions)
{
  aarch64_feature_flags sm_state_extensions = 0;
  if (!TARGET_STREAMING)
    {
      if (required_extensions.sm_off == 0)
	{
	  error_at (location, "ACLE function %qD can only be called when"
		    " SME streaming mode is enabled", fndecl);
	  return false;
	}
      sm_state_extensions |= required_extensions.sm_off & ~AARCH64_FL_SM_OFF;
    }
  if (!TARGET_NON_STREAMING)
    {
      if (required_extensions.sm_on == 0)
	{
	  error_at (location, "ACLE function %qD cannot be called when"
		    " SME streaming mode is enabled", fndecl);
	  return false;
	}
      sm_state_extensions |= required_extensions.sm_on & ~AARCH64_FL_SM_ON;
    }

  if ((sm_state_extensions & ~aarch64_isa_flags) == 0)
    return true;

  auto missing_extensions = sm_state_extensions & ~aarch64_asm_isa_flags;
  if (missing_extensions == 0)
    {
      /* All required extensions are enabled in aarch64_asm_isa_flags, so the
	 error must be the use of general-regs-only.  */
      aarch64_report_missing_registers (location, fndecl);
      return false;
    }

  if (missing_extensions & AARCH64_FL_ZA_ON)
    {
      error_at (location, "ACLE function %qD can only be called from"
		" a function that has %qs state", fndecl, "za");
      return false;
    }

  static const struct {
    aarch64_feature_flags flag;
    const char *name;
  } extensions[] = {
#define AARCH64_OPT_EXTENSION(EXT_NAME, IDENT, C, D, E, F) \
    { AARCH64_FL_##IDENT, EXT_NAME },
#include "aarch64-option-extensions.def"
  };

  for (unsigned int i = 0; i < ARRAY_SIZE (extensions); ++i)
    if (missing_extensions & extensions[i].flag)
      {
	aarch64_report_missing_extension (location, fndecl, extensions[i].name);
	return false;
      }
  gcc_unreachable ();
}

/* Return the ISA extensions required by function CODE.  */
static aarch64_required_extensions
aarch64_general_required_extensions (unsigned int code)
{
  using ext = aarch64_required_extensions;
  switch (code)
    {
    case AARCH64_TME_BUILTIN_TSTART:
    case AARCH64_TME_BUILTIN_TCOMMIT:
    case AARCH64_TME_BUILTIN_TTEST:
    case AARCH64_TME_BUILTIN_TCANCEL:
      return ext::streaming_compatible (AARCH64_FL_TME);

    case AARCH64_LS64_BUILTIN_LD64B:
    case AARCH64_LS64_BUILTIN_ST64B:
    case AARCH64_LS64_BUILTIN_ST64BV:
    case AARCH64_LS64_BUILTIN_ST64BV0:
      return ext::streaming_compatible (AARCH64_FL_LS64);

    default:
      if (code >= AARCH64_MEMTAG_BUILTIN_START
	  && code <= AARCH64_MEMTAG_BUILTIN_END)
	return ext::streaming_compatible (AARCH64_FL_MEMTAG);

      if (auto builtin_data = aarch64_get_pragma_builtin (code))
	return builtin_data->required_extensions;
    }
  return ext::streaming_compatible (0);
}

bool
aarch64_general_check_builtin_call (location_t location, vec<location_t>,
				    unsigned int code, tree fndecl,
				    unsigned int nargs ATTRIBUTE_UNUSED,
				    tree *args)
{
  tree decl = aarch64_builtin_decls[code];
  auto required_extensions = aarch64_general_required_extensions (code);
  if (!aarch64_check_required_extensions (location, decl, required_extensions))
    return false;

  switch (code)
    {
    case AARCH64_RSR:
    case AARCH64_RSRP:
    case AARCH64_RSR64:
    case AARCH64_RSRF:
    case AARCH64_RSRF64:
    case AARCH64_WSR:
    case AARCH64_WSRP:
    case AARCH64_WSR64:
    case AARCH64_WSRF:
    case AARCH64_WSRF64:
      {
	tree addr = STRIP_NOPS (args[0]);
	if (TREE_CODE (TREE_TYPE (addr)) != POINTER_TYPE
	    || TREE_CODE (addr) != ADDR_EXPR
	    || TREE_CODE (TREE_OPERAND (addr, 0)) != STRING_CST)
	  {
	    error_at (location,
		      "first argument to %qD must be a string literal",
		      fndecl);
	    return false;
	  }
	break;
      }
    }

  return true;
}

typedef enum
{
  SIMD_ARG_COPY_TO_REG,
  SIMD_ARG_CONSTANT,
  SIMD_ARG_LANE_INDEX,
  SIMD_ARG_STRUCT_LOAD_STORE_LANE_INDEX,
  SIMD_ARG_LANE_PAIR_INDEX,
  SIMD_ARG_LANE_QUADTUP_INDEX,
  SIMD_ARG_STOP
} builtin_simd_arg;


static rtx
aarch64_simd_expand_args (rtx target, int icode, int have_retval,
			  tree exp, builtin_simd_arg *args,
			  machine_mode builtin_mode)
{
  rtx pat;
  rtx op[SIMD_MAX_BUILTIN_ARGS + 1]; /* First element for result operand.  */
  int opc = 0;

  if (have_retval)
    {
      machine_mode tmode = insn_data[icode].operand[0].mode;
      if (!target
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      op[opc++] = target;
    }

  for (;;)
    {
      builtin_simd_arg thisarg = args[opc - have_retval];

      if (thisarg == SIMD_ARG_STOP)
	break;
      else
	{
	  tree arg = CALL_EXPR_ARG (exp, opc - have_retval);
	  machine_mode mode = insn_data[icode].operand[opc].mode;
	  op[opc] = expand_normal (arg);

	  switch (thisarg)
	    {
	    case SIMD_ARG_COPY_TO_REG:
	      if (POINTER_TYPE_P (TREE_TYPE (arg)))
		op[opc] = convert_memory_address (Pmode, op[opc]);
	      /*gcc_assert (GET_MODE (op[opc]) == mode); */
	      if (!(*insn_data[icode].operand[opc].predicate)
		  (op[opc], mode))
		op[opc] = copy_to_mode_reg (mode, op[opc]);
	      break;

	    case SIMD_ARG_STRUCT_LOAD_STORE_LANE_INDEX:
	      gcc_assert (opc > 1);
	      if (CONST_INT_P (op[opc]))
		{
		  unsigned int nunits
		    = GET_MODE_NUNITS (builtin_mode).to_constant ();
		  aarch64_simd_lane_bounds (op[opc], 0, nunits, exp);
		  /* Keep to GCC-vector-extension lane indices in the RTL.  */
		  op[opc] = aarch64_endian_lane_rtx (builtin_mode,
						     INTVAL (op[opc]));
		}
	      goto constant_arg;

	    case SIMD_ARG_LANE_INDEX:
	      /* Must be a previous operand into which this is an index.  */
	      gcc_assert (opc > 0);
	      if (CONST_INT_P (op[opc]))
		{
		  machine_mode vmode = insn_data[icode].operand[opc - 1].mode;
		  unsigned int nunits
		    = GET_MODE_NUNITS (vmode).to_constant ();
		  aarch64_simd_lane_bounds (op[opc], 0, nunits, exp);
		  /* Keep to GCC-vector-extension lane indices in the RTL.  */
		  op[opc] = aarch64_endian_lane_rtx (vmode, INTVAL (op[opc]));
		}
	      /* If the lane index isn't a constant then error out.  */
	      goto constant_arg;

	    case SIMD_ARG_LANE_PAIR_INDEX:
	      /* Must be a previous operand into which this is an index and
		 index is restricted to nunits / 2.  */
	      gcc_assert (opc > 0);
	      if (CONST_INT_P (op[opc]))
		{
		  machine_mode vmode = insn_data[icode].operand[opc - 1].mode;
		  unsigned int nunits
		    = GET_MODE_NUNITS (vmode).to_constant ();
		  aarch64_simd_lane_bounds (op[opc], 0, nunits / 2, exp);
		  /* Keep to GCC-vector-extension lane indices in the RTL.  */
		  int lane = INTVAL (op[opc]);
		  op[opc] = gen_int_mode (ENDIAN_LANE_N (nunits / 2, lane),
					  SImode);
		}
	      /* If the lane index isn't a constant then error out.  */
	      goto constant_arg;
	    case SIMD_ARG_LANE_QUADTUP_INDEX:
	      /* Must be a previous operand into which this is an index and
		 index is restricted to nunits / 4.  */
	      gcc_assert (opc > 0);
	      if (CONST_INT_P (op[opc]))
		{
		  machine_mode vmode = insn_data[icode].operand[opc - 1].mode;
		  unsigned int nunits
		    = GET_MODE_NUNITS (vmode).to_constant ();
		  aarch64_simd_lane_bounds (op[opc], 0, nunits / 4, exp);
		  /* Keep to GCC-vector-extension lane indices in the RTL.  */
		  int lane = INTVAL (op[opc]);
		  op[opc] = gen_int_mode (ENDIAN_LANE_N (nunits / 4, lane),
					  SImode);
		}
	      /* If the lane index isn't a constant then error out.  */
	      goto constant_arg;
	    case SIMD_ARG_CONSTANT:
constant_arg:
	      if (!(*insn_data[icode].operand[opc].predicate)
		  (op[opc], mode))
	      {
		error_at (EXPR_LOCATION (exp),
			  "argument %d must be a constant immediate",
			  opc + 1 - have_retval);
		return const0_rtx;
	      }
	      break;

	    case SIMD_ARG_STOP:
	      gcc_unreachable ();
	    }

	  opc++;
	}
    }

  switch (opc)
    {
    case 1:
      pat = GEN_FCN (icode) (op[0]);
      break;

    case 2:
      pat = GEN_FCN (icode) (op[0], op[1]);
      break;

    case 3:
      pat = GEN_FCN (icode) (op[0], op[1], op[2]);
      break;

    case 4:
      pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3]);
      break;

    case 5:
      pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3], op[4]);
      break;

    case 6:
      pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3], op[4], op[5]);
      break;

    default:
      gcc_unreachable ();
    }

  if (!pat)
    return NULL_RTX;

  emit_insn (pat);

  return target;
}

/* Expand an AArch64 AdvSIMD builtin(intrinsic).  */
rtx
aarch64_simd_expand_builtin (int fcode, tree exp, rtx target)
{
  if (fcode == AARCH64_SIMD_BUILTIN_LANE_CHECK)
    {
      rtx totalsize = expand_normal (CALL_EXPR_ARG (exp, 0));
      rtx elementsize = expand_normal (CALL_EXPR_ARG (exp, 1));
      if (CONST_INT_P (totalsize) && CONST_INT_P (elementsize)
	  && UINTVAL (elementsize) != 0
	  && UINTVAL (totalsize) != 0)
	{
	  rtx lane_idx = expand_normal (CALL_EXPR_ARG (exp, 2));
          if (CONST_INT_P (lane_idx))
	    aarch64_simd_lane_bounds (lane_idx, 0,
				      UINTVAL (totalsize)
				       / UINTVAL (elementsize),
				      exp);
          else
	    error_at (EXPR_LOCATION (exp),
		      "lane index must be a constant immediate");
	}
      else
	error_at (EXPR_LOCATION (exp),
		  "total size and element size must be a nonzero "
		  "constant immediate");
      /* Don't generate any RTL.  */
      return const0_rtx;
    }
  aarch64_simd_builtin_datum *d =
		&aarch64_simd_builtin_data[fcode - AARCH64_SIMD_PATTERN_START];
  enum insn_code icode = d->code;
  builtin_simd_arg args[SIMD_MAX_BUILTIN_ARGS + 1];
  int num_args = insn_data[d->code].n_operands;
  int is_void = 0;
  int k;

  is_void = !!(d->qualifiers[0] & qualifier_void);

  num_args += is_void;

  for (k = 1; k < num_args; k++)
    {
      /* We have four arrays of data, each indexed in a different fashion.
	 qualifiers - element 0 always describes the function return type.
	 operands - element 0 is either the operand for return value (if
	   the function has a non-void return type) or the operand for the
	   first argument.
	 expr_args - element 0 always holds the first argument.
	 args - element 0 is always used for the return type.  */
      int qualifiers_k = k;
      int operands_k = k - is_void;
      int expr_args_k = k - 1;

      if (d->qualifiers[qualifiers_k] & qualifier_lane_index)
	args[k] = SIMD_ARG_LANE_INDEX;
      else if (d->qualifiers[qualifiers_k] & qualifier_lane_pair_index)
	args[k] = SIMD_ARG_LANE_PAIR_INDEX;
      else if (d->qualifiers[qualifiers_k] & qualifier_lane_quadtup_index)
	args[k] = SIMD_ARG_LANE_QUADTUP_INDEX;
      else if (d->qualifiers[qualifiers_k] & qualifier_struct_load_store_lane_index)
	args[k] = SIMD_ARG_STRUCT_LOAD_STORE_LANE_INDEX;
      else if (d->qualifiers[qualifiers_k] & qualifier_immediate)
	args[k] = SIMD_ARG_CONSTANT;
      else if (d->qualifiers[qualifiers_k] & qualifier_maybe_immediate)
	{
	  rtx arg
	    = expand_normal (CALL_EXPR_ARG (exp,
					    (expr_args_k)));
	  /* Handle constants only if the predicate allows it.  */
	  bool op_const_int_p =
	    (CONST_INT_P (arg)
	     && (*insn_data[icode].operand[operands_k].predicate)
		(arg, insn_data[icode].operand[operands_k].mode));
	  args[k] = op_const_int_p ? SIMD_ARG_CONSTANT : SIMD_ARG_COPY_TO_REG;
	}
      else
	args[k] = SIMD_ARG_COPY_TO_REG;

    }
  args[k] = SIMD_ARG_STOP;

  /* The interface to aarch64_simd_expand_args expects a 0 if
     the function is void, and a 1 if it is not.  */
  return aarch64_simd_expand_args
	  (target, icode, !is_void, exp, &args[1], d->mode);
}

rtx
aarch64_crc32_expand_builtin (int fcode, tree exp, rtx target)
{
  rtx pat;
  aarch64_crc_builtin_datum *d
    = &aarch64_crc_builtin_data[fcode - (AARCH64_CRC32_BUILTIN_BASE + 1)];
  enum insn_code icode = d->icode;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  machine_mode tmode = insn_data[icode].operand[0].mode;
  machine_mode mode0 = insn_data[icode].operand[1].mode;
  machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  gcc_assert ((GET_MODE (op0) == mode0 || GET_MODE (op0) == VOIDmode)
	      && (GET_MODE (op1) == mode1 || GET_MODE (op1) == VOIDmode));

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (icode) (target, op0, op1);
  if (!pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Function to expand reciprocal square root builtins.  */

static rtx
aarch64_expand_builtin_rsqrt (int fcode, tree exp, rtx target)
{
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op0 = expand_normal (arg0);

  rtx (*gen) (rtx, rtx);

  switch (fcode)
    {
      case AARCH64_BUILTIN_RSQRT_DF:
	gen = gen_rsqrtdf2;
	break;
      case AARCH64_BUILTIN_RSQRT_SF:
	gen = gen_rsqrtsf2;
	break;
      case AARCH64_BUILTIN_RSQRT_V2DF:
	gen = gen_rsqrtv2df2;
	break;
      case AARCH64_BUILTIN_RSQRT_V2SF:
	gen = gen_rsqrtv2sf2;
	break;
      case AARCH64_BUILTIN_RSQRT_V4SF:
	gen = gen_rsqrtv4sf2;
	break;
      default: gcc_unreachable ();
    }

  if (!target)
    target = gen_reg_rtx (GET_MODE (op0));

  emit_insn (gen (target, op0));

  return target;
}

/* Expand a FCMLA lane expression EXP with code FCODE and
   result going to TARGET if that is convenient.  */

rtx
aarch64_expand_fcmla_builtin (tree exp, rtx target, int fcode)
{
  int bcode = fcode - AARCH64_SIMD_FCMLA_LANEQ_BUILTIN_BASE - 1;
  aarch64_fcmla_laneq_builtin_datum* d
    = &aarch64_fcmla_lane_builtin_data[bcode];
  machine_mode quadmode = GET_MODE_2XWIDER_MODE (d->mode).require ();
  rtx op0 = force_reg (d->mode, expand_normal (CALL_EXPR_ARG (exp, 0)));
  rtx op1 = force_reg (d->mode, expand_normal (CALL_EXPR_ARG (exp, 1)));
  rtx op2 = force_reg (quadmode, expand_normal (CALL_EXPR_ARG (exp, 2)));
  tree tmp = CALL_EXPR_ARG (exp, 3);
  rtx lane_idx = expand_expr (tmp, NULL_RTX, VOIDmode, EXPAND_INITIALIZER);

  /* Validate that the lane index is a constant.  */
  if (!CONST_INT_P (lane_idx))
    {
      error_at (EXPR_LOCATION (exp),
		"argument %d must be a constant immediate", 4);
      return const0_rtx;
    }

  /* Validate that the index is within the expected range.  */
  int nunits = GET_MODE_NUNITS (quadmode).to_constant ();
  aarch64_simd_lane_bounds (lane_idx, 0, nunits / 2, exp);

  /* Generate the correct register and mode.  */
  int lane = INTVAL (lane_idx);

  if (lane < nunits / 4)
    op2 = force_lowpart_subreg (d->mode, op2, quadmode);
  else
    {
      /* Select the upper 64 bits, either a V2SF or V4HF, this however
	 is quite messy, as the operation required even though simple
	 doesn't have a simple RTL pattern, and seems it's quite hard to
	 define using a single RTL pattern.  The target generic version
	 gen_highpart_mode generates code that isn't optimal.  */
      rtx temp1 = gen_reg_rtx (d->mode);
      rtx temp2 = gen_reg_rtx (DImode);
      temp1 = force_lowpart_subreg (d->mode, op2, quadmode);
      temp1 = force_subreg (V2DImode, temp1, d->mode, 0);
      if (BYTES_BIG_ENDIAN)
	emit_insn (gen_aarch64_get_lanev2di (temp2, temp1, const0_rtx));
      else
	emit_insn (gen_aarch64_get_lanev2di (temp2, temp1, const1_rtx));
      op2 = force_subreg (d->mode, temp2, GET_MODE (temp2), 0);

      /* And recalculate the index.  */
      lane -= nunits / 4;
    }

  /* Keep to GCC-vector-extension lane indices in the RTL, only nunits / 4
     (max nunits in range check) are valid.  Which means only 0-1, so we
     only need to know the order in a V2mode.  */
  lane_idx = aarch64_endian_lane_rtx (V2DImode, lane);

  if (!target
      || !REG_P (target)
      || GET_MODE (target) != d->mode)
    target = gen_reg_rtx (d->mode);

  rtx pat = NULL_RTX;

  if (d->lane)
    pat = GEN_FCN (d->icode) (target, op0, op1, op2, lane_idx);
  else
    pat = GEN_FCN (d->icode) (target, op0, op1, op2);

  if (!pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Function to expand an expression EXP which calls one of the Transactional
   Memory Extension (TME) builtins FCODE with the result going to TARGET.  */
static rtx
aarch64_expand_builtin_tme (int fcode, tree exp, rtx target)
{
  switch (fcode)
    {
    case AARCH64_TME_BUILTIN_TSTART:
      target = gen_reg_rtx (DImode);
      emit_insn (GEN_FCN (CODE_FOR_tstart) (target));
      break;

    case AARCH64_TME_BUILTIN_TTEST:
      target = gen_reg_rtx (DImode);
      emit_insn (GEN_FCN (CODE_FOR_ttest) (target));
      break;

    case AARCH64_TME_BUILTIN_TCOMMIT:
      emit_insn (GEN_FCN (CODE_FOR_tcommit) ());
      break;

    case AARCH64_TME_BUILTIN_TCANCEL:
      {
	tree arg0 = CALL_EXPR_ARG (exp, 0);
	rtx op0 = expand_normal (arg0);
	if (CONST_INT_P (op0) && UINTVAL (op0) <= 65536)
	  emit_insn (GEN_FCN (CODE_FOR_tcancel) (op0));
	else
	  {
	    error_at (EXPR_LOCATION (exp),
		      "argument must be a 16-bit constant immediate");
	    return const0_rtx;
	  }
      }
      break;

    default :
      gcc_unreachable ();
    }
    return target;
}

/* Function to expand an expression EXP which calls one of the Load/Store
   64 Byte extension (LS64) builtins FCODE with the result going to TARGET.  */
static rtx
aarch64_expand_builtin_ls64 (int fcode, tree exp, rtx target)
{
  expand_operand ops[3];

  switch (fcode)
    {
    case AARCH64_LS64_BUILTIN_LD64B:
      {
	rtx op0 = expand_normal (CALL_EXPR_ARG (exp, 0));
	create_output_operand (&ops[0], target, V8DImode);
	create_input_operand (&ops[1], op0, DImode);
	expand_insn (CODE_FOR_ld64b, 2, ops);
	return ops[0].value;
      }
    case AARCH64_LS64_BUILTIN_ST64B:
      {
	rtx op0 = expand_normal (CALL_EXPR_ARG (exp, 0));
	rtx op1 = expand_normal (CALL_EXPR_ARG (exp, 1));
	create_input_operand (&ops[0], op0, DImode);
	create_input_operand (&ops[1], op1, V8DImode);
	expand_insn (CODE_FOR_st64b, 2, ops);
	return const0_rtx;
      }
    case AARCH64_LS64_BUILTIN_ST64BV:
      {
	rtx op0 = expand_normal (CALL_EXPR_ARG (exp, 0));
	rtx op1 = expand_normal (CALL_EXPR_ARG (exp, 1));
	create_output_operand (&ops[0], target, DImode);
	create_input_operand (&ops[1], op0, DImode);
	create_input_operand (&ops[2], op1, V8DImode);
	expand_insn (CODE_FOR_st64bv, 3, ops);
	return ops[0].value;
      }
    case AARCH64_LS64_BUILTIN_ST64BV0:
      {
	rtx op0 = expand_normal (CALL_EXPR_ARG (exp, 0));
	rtx op1 = expand_normal (CALL_EXPR_ARG (exp, 1));
	create_output_operand (&ops[0], target, DImode);
	create_input_operand (&ops[1], op0, DImode);
	create_input_operand (&ops[2], op1, V8DImode);
	expand_insn (CODE_FOR_st64bv0, 3, ops);
	return ops[0].value;
      }
    }

  gcc_unreachable ();
}

/* Expand a random number builtin EXP with code FCODE, putting the result
   int TARGET.  If IGNORE is true the return value is ignored.  */

rtx
aarch64_expand_rng_builtin (tree exp, rtx target, int fcode, int ignore)
{
  rtx pat;
  enum insn_code icode;
  if (fcode == AARCH64_BUILTIN_RNG_RNDR)
    icode = CODE_FOR_aarch64_rndr;
  else if (fcode == AARCH64_BUILTIN_RNG_RNDRRS)
    icode = CODE_FOR_aarch64_rndrrs;
  else
    gcc_unreachable ();

  rtx rand = gen_reg_rtx (DImode);
  pat = GEN_FCN (icode) (rand);
  if (!pat)
    return NULL_RTX;

  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx res_addr = expand_normal (arg0);
  res_addr = convert_memory_address (Pmode, res_addr);
  rtx res_mem = gen_rtx_MEM (DImode, res_addr);
  emit_insn (pat);
  emit_move_insn (res_mem, rand);
  /* If the status result is unused don't generate the CSET code.  */
  if (ignore)
    return target;

  rtx cc_reg = gen_rtx_REG (CC_Zmode, CC_REGNUM);
  rtx cmp_rtx = gen_rtx_fmt_ee (EQ, SImode, cc_reg, const0_rtx);
  emit_insn (gen_aarch64_cstoresi (target, cmp_rtx, cc_reg));
  return target;
}

/* Expand the read/write system register builtin EXPs.  */
rtx
aarch64_expand_rwsr_builtin (tree exp, rtx target, int fcode)
{
  tree arg0, arg1;
  rtx const_str, input_val, subreg;
  enum machine_mode mode;
  enum insn_code icode;
  class expand_operand ops[2];

  arg0 = CALL_EXPR_ARG (exp, 0);

  bool write_op = (fcode == AARCH64_WSR
		   || fcode == AARCH64_WSRP
		   || fcode == AARCH64_WSR64
		   || fcode == AARCH64_WSRF
		   || fcode == AARCH64_WSRF64
		   || fcode == AARCH64_WSR128);

  bool op128 = (fcode == AARCH64_RSR128 || fcode == AARCH64_WSR128);
  enum machine_mode sysreg_mode = op128 ? TImode : DImode;

  if (op128 && !TARGET_D128)
    {
      error_at (EXPR_LOCATION (exp), "128-bit system register support requires"
				     " the %<d128%> extension");
      return const0_rtx;
    }

  /* Argument 0 (system register name) must be a string literal.  */
  gcc_assert (TREE_CODE (arg0) == ADDR_EXPR
	      && TREE_CODE (TREE_TYPE (arg0)) == POINTER_TYPE
	      && TREE_CODE (TREE_OPERAND (arg0, 0)) == STRING_CST);

  const char *name_input = TREE_STRING_POINTER (TREE_OPERAND (arg0, 0));

  tree len_tree = c_strlen (arg0, 1);
  if (len_tree == NULL_TREE)
    {
      error_at (EXPR_LOCATION (exp), "invalid system register name provided");
      return const0_rtx;
    }

  size_t len = TREE_INT_CST_LOW (len_tree);
  char *sysreg_name = xstrdup (name_input);

  for (unsigned pos = 0; pos <= len; pos++)
    sysreg_name[pos] = TOLOWER (sysreg_name[pos]);

  const char* name_output = aarch64_retrieve_sysreg ((const char *) sysreg_name,
						     write_op, op128);
  if (name_output == NULL)
    {
      error_at (EXPR_LOCATION (exp), "invalid system register name %qs",
		sysreg_name);
      return const0_rtx;
    }

  /* Assign the string corresponding to the system register name to an RTX.  */
  const_str = rtx_alloc (CONST_STRING);
  PUT_CODE (const_str, CONST_STRING);
  XSTR (const_str, 0) = ggc_strdup (name_output);

  /* Set up expander operands and call instruction expansion.  */
  if (write_op)
    {
      arg1 = CALL_EXPR_ARG (exp, 1);
      mode = TYPE_MODE (TREE_TYPE (arg1));
      input_val = copy_to_mode_reg (mode, expand_normal (arg1));

      icode = (op128 ? CODE_FOR_aarch64_write_sysregti
		     : CODE_FOR_aarch64_write_sysregdi);

      switch (fcode)
	{
	case AARCH64_WSR:
	case AARCH64_WSRP:
	case AARCH64_WSR64:
	case AARCH64_WSRF64:
	case AARCH64_WSR128:
	  subreg = force_lowpart_subreg (sysreg_mode, input_val, mode);
	  break;
	case AARCH64_WSRF:
	  subreg = gen_lowpart_SUBREG (SImode, input_val);
	  subreg = gen_lowpart_SUBREG (DImode, subreg);
	  break;
	}

      create_fixed_operand (&ops[0], const_str);
      create_input_operand (&ops[1], subreg, sysreg_mode);
      expand_insn (icode, 2, ops);

      return target;
    }

  /* Read operations are implied by !write_op.  */
  gcc_assert (call_expr_nargs (exp) == 1);

  icode = (op128 ? CODE_FOR_aarch64_read_sysregti
		 : CODE_FOR_aarch64_read_sysregdi);

  /* Emit the initial read_sysregdi rtx.  */
  create_output_operand (&ops[0], target, sysreg_mode);
  create_fixed_operand (&ops[1], const_str);
  expand_insn (icode, 2, ops);
  target = ops[0].value;

  /* Do any necessary post-processing on the result.  */
  switch (fcode)
    {
    case AARCH64_RSR:
    case AARCH64_RSRP:
    case AARCH64_RSR64:
    case AARCH64_RSRF64:
    case AARCH64_RSR128:
      return force_lowpart_subreg (TYPE_MODE (TREE_TYPE (exp)),
				   target, sysreg_mode);
    case AARCH64_RSRF:
      subreg = gen_lowpart_SUBREG (SImode, target);
      return gen_lowpart_SUBREG (SFmode, subreg);
    default:
      gcc_unreachable ();
    }
}

/* Ensure argument ARGNO in EXP represents a const-type argument in the range
   [MINVAL, MAXVAL).  */
static HOST_WIDE_INT
require_const_argument (tree exp, unsigned int argno, HOST_WIDE_INT minval,
			HOST_WIDE_INT maxval)
{
  maxval--;
  tree arg = CALL_EXPR_ARG (exp, argno);
  if (TREE_CODE (arg) != INTEGER_CST)
      error_at (EXPR_LOCATION (exp), "Constant-type argument expected");

  auto argval = wi::to_widest (arg);

  if (argval < minval || argval > maxval)
    error_at (EXPR_LOCATION (exp),
	      "argument %d must be a constant immediate "
	      "in range [%wd,%wd]", argno + 1, minval, maxval);

  HOST_WIDE_INT retval = argval.to_shwi ();
  return retval;
}


/* Expand a prefetch builtin EXP.  */
void
aarch64_expand_prefetch_builtin (tree exp, int fcode)
{
  int kind_id = -1;
  int level_id = -1;
  int rettn_id = -1;
  char prfop[11];
  class expand_operand ops[2];

  static const char *kind_s[] = {"PLD", "PST", "PLI"};
  static const char *level_s[] = {"L1", "L2", "L3", "SLC"};
  static const char *rettn_s[] = {"KEEP", "STRM"};

  /* Each of the four prefetch builtins takes a different number of arguments,
     but proceeds to call the PRFM insn which requires 4 pieces of information
     to be fully defined.  Where one of these takes less than 4 arguments, set
     sensible defaults.  */
  switch (fcode)
    {
    case AARCH64_PLDX:
      break;
    case AARCH64_PLIX:
      kind_id = 2;
      break;
    case AARCH64_PLI:
    case AARCH64_PLD:
      kind_id  = (fcode == AARCH64_PLD) ? 0 : 2;
      level_id = 0;
      rettn_id = 0;
      break;
    default:
      gcc_unreachable ();
    }

  /* Any -1 id variable is to be user-supplied.  Here we fill these in and run
     bounds checks on them.  "PLI" is used only implicitly by AARCH64_PLI &
     AARCH64_PLIX, never explicitly.  */
  int argno = 0;
  if (kind_id < 0)
    kind_id = require_const_argument (exp, argno++, 0, ARRAY_SIZE (kind_s) - 1);
  if (level_id < 0)
    level_id = require_const_argument (exp, argno++, 0, ARRAY_SIZE (level_s));
  if (rettn_id < 0)
    rettn_id = require_const_argument (exp, argno++, 0, ARRAY_SIZE (rettn_s));
  rtx address = expand_expr (CALL_EXPR_ARG (exp, argno), NULL_RTX, Pmode,
			     EXPAND_NORMAL);

  if (seen_error ())
    return;

  sprintf (prfop, "%s%s%s", kind_s[kind_id],
			    level_s[level_id],
			    rettn_s[rettn_id]);

  rtx const_str = rtx_alloc (CONST_STRING);
  PUT_CODE (const_str, CONST_STRING);
  XSTR (const_str, 0) = ggc_strdup (prfop);

  create_fixed_operand (&ops[0], const_str);
  create_address_operand (&ops[1], address);
  maybe_expand_insn (CODE_FOR_aarch64_pldx, 2, ops);
}

/* Expand an expression EXP that calls a MEMTAG built-in FCODE
   with result going to TARGET.  */
static rtx
aarch64_expand_builtin_memtag (int fcode, tree exp, rtx target)
{
  if (TARGET_ILP32)
    {
      error ("Memory Tagging Extension does not support %<-mabi=ilp32%>");
      return const0_rtx;
    }

  rtx pat = NULL;
  enum insn_code icode = aarch64_memtag_builtin_data[fcode -
			   AARCH64_MEMTAG_BUILTIN_START - 1].icode;

  rtx op0 = expand_normal (CALL_EXPR_ARG (exp, 0));
  machine_mode mode0 = GET_MODE (op0);
  op0 = force_reg (mode0 == VOIDmode ? DImode : mode0, op0);
  op0 = convert_to_mode (DImode, op0, true);

  switch (fcode)
    {
      case AARCH64_MEMTAG_BUILTIN_IRG:
      case AARCH64_MEMTAG_BUILTIN_GMI:
      case AARCH64_MEMTAG_BUILTIN_SUBP:
      case AARCH64_MEMTAG_BUILTIN_INC_TAG:
	{
	  if (! target
	      || GET_MODE (target) != DImode
	      || ! (*insn_data[icode].operand[0].predicate) (target, DImode))
	    target = gen_reg_rtx (DImode);

	  if (fcode == AARCH64_MEMTAG_BUILTIN_INC_TAG)
	    {
	      rtx op1 = expand_normal (CALL_EXPR_ARG (exp, 1));

	      if ((*insn_data[icode].operand[3].predicate) (op1, QImode))
		{
		  pat = GEN_FCN (icode) (target, op0, const0_rtx, op1);
		  break;
		}
	      error_at (EXPR_LOCATION (exp),
			"argument %d must be a constant immediate "
			"in range [0,15]", 2);
	      return const0_rtx;
	    }
	  else
	    {
	      rtx op1 = expand_normal (CALL_EXPR_ARG (exp, 1));
	      machine_mode mode1 = GET_MODE (op1);
	      op1 = force_reg (mode1 == VOIDmode ? DImode : mode1, op1);
	      op1 = convert_to_mode (DImode, op1, true);
	      pat = GEN_FCN (icode) (target, op0, op1);
	    }
	  break;
	}
      case AARCH64_MEMTAG_BUILTIN_GET_TAG:
	target = op0;
	pat = GEN_FCN (icode) (target, op0, const0_rtx);
	break;
      case AARCH64_MEMTAG_BUILTIN_SET_TAG:
	pat = GEN_FCN (icode) (op0, op0, const0_rtx);
	break;
      default:
	gcc_unreachable();
    }

  if (!pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Function to expand an expression EXP which calls one of the ACLE Data
   Intrinsic builtins FCODE with the result going to TARGET.  */
static rtx
aarch64_expand_builtin_data_intrinsic (unsigned int fcode, tree exp, rtx target)
{
  expand_operand ops[2];
  machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], expand_normal (CALL_EXPR_ARG (exp, 0)), mode);
  enum insn_code icode;

  switch (fcode)
    {
    case AARCH64_REV16:
    case AARCH64_REV16L:
    case AARCH64_REV16LL:
      icode = code_for_aarch64_rev16 (mode);
      break;
    case AARCH64_RBIT:
    case AARCH64_RBITL:
    case AARCH64_RBITLL:
      icode = code_for_aarch64_rbit (mode);
      break;
    default:
      gcc_unreachable ();
    }

  expand_insn (icode, 2, ops);
  return ops[0].value;
}

static rtx
aarch64_expand_pragma_builtin (tree exp, rtx target,
			       const aarch64_pragma_builtins_data *builtin_data)
{
  expand_operand ops[3];
  auto mode = builtin_data->mode;
  auto op1 = expand_normal (CALL_EXPR_ARG (exp, 0));
  auto op2 = expand_normal (CALL_EXPR_ARG (exp, 1));
  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], op1, mode);
  create_input_operand (&ops[2], op2, mode);

  auto unspec = builtin_data->unspec;
  auto icode = code_for_aarch64 (unspec, mode);
  expand_insn (icode, 3, ops);

  return target;
}

/* Expand an expression EXP as fpsr or fpcr setter (depending on
   UNSPEC) using MODE.  */
static void
aarch64_expand_fpsr_fpcr_setter (int unspec, machine_mode mode, tree exp)
{
  tree arg = CALL_EXPR_ARG (exp, 0);
  rtx op = force_reg (mode, expand_normal (arg));
  emit_insn (gen_aarch64_set (unspec, mode, op));
}

/* Expand a fpsr or fpcr getter (depending on UNSPEC) using MODE.
   Return the target.  */
static rtx
aarch64_expand_fpsr_fpcr_getter (enum insn_code icode, machine_mode mode,
				 rtx target)
{
  expand_operand op;
  create_output_operand (&op, target, mode);
  expand_insn (icode, 1, &op);
  return op.value;
}

/* Expand GCS builtin EXP with code FCODE, putting the result
   into TARGET.  If IGNORE is true the return value is ignored.  */

rtx
aarch64_expand_gcs_builtin (tree exp, rtx target, int fcode, int ignore)
{
  if (fcode == AARCH64_BUILTIN_GCSPR)
    {
      expand_operand op;
      create_output_operand (&op, target, DImode);
      expand_insn (CODE_FOR_aarch64_load_gcspr, 1, &op);
      return op.value;
    }
  if (fcode == AARCH64_BUILTIN_GCSPOPM && ignore)
    {
      expand_insn (CODE_FOR_aarch64_gcspopm_xzr, 0, 0);
      return target;
    }
  if (fcode == AARCH64_BUILTIN_GCSPOPM)
    {
      expand_operand ops[2];
      create_output_operand (&ops[0], target, DImode);
      create_input_operand (&ops[1], const0_rtx, DImode);
      expand_insn (CODE_FOR_aarch64_gcspopm, 2, ops);
      return gen_lowpart (ptr_mode, ops[0].value);
    }
  if (fcode == AARCH64_BUILTIN_GCSSS)
    {
      expand_operand opnd;
      rtx arg = expand_normal (CALL_EXPR_ARG (exp, 0));
      arg = convert_modes (DImode, ptr_mode, arg, true);
      create_input_operand (&opnd, arg, DImode);
      expand_insn (CODE_FOR_aarch64_gcsss1, 1, &opnd);
      expand_operand ops[2];
      create_output_operand (&ops[0], target, DImode);
      create_input_operand (&ops[1], const0_rtx, DImode);
      expand_insn (CODE_FOR_aarch64_gcsss2, 2, ops);
      return gen_lowpart (ptr_mode, ops[0].value);
    }
  gcc_unreachable ();
}

/* Expand an expression EXP that calls built-in function FCODE,
   with result going to TARGET if that's convenient.  IGNORE is true
   if the result of the builtin is ignored.  */
rtx
aarch64_general_expand_builtin (unsigned int fcode, tree exp, rtx target,
				int ignore)
{
  int icode;
  rtx op0;
  tree arg0;

  switch (fcode)
    {
    case AARCH64_BUILTIN_GET_FPCR:
      return aarch64_expand_fpsr_fpcr_getter (CODE_FOR_aarch64_get_fpcrsi,
					      SImode, target);
    case AARCH64_BUILTIN_SET_FPCR:
      aarch64_expand_fpsr_fpcr_setter (UNSPECV_SET_FPCR, SImode, exp);
      return target;
    case AARCH64_BUILTIN_GET_FPSR:
      return aarch64_expand_fpsr_fpcr_getter (CODE_FOR_aarch64_get_fpsrsi,
					      SImode, target);
    case AARCH64_BUILTIN_SET_FPSR:
      aarch64_expand_fpsr_fpcr_setter (UNSPECV_SET_FPSR, SImode, exp);
      return target;
    case AARCH64_BUILTIN_GET_FPCR64:
      return aarch64_expand_fpsr_fpcr_getter (CODE_FOR_aarch64_get_fpcrdi,
					      DImode, target);
    case AARCH64_BUILTIN_SET_FPCR64:
      aarch64_expand_fpsr_fpcr_setter (UNSPECV_SET_FPCR, DImode, exp);
      return target;
    case AARCH64_BUILTIN_GET_FPSR64:
      return aarch64_expand_fpsr_fpcr_getter (CODE_FOR_aarch64_get_fpsrdi,
					      DImode, target);
    case AARCH64_BUILTIN_SET_FPSR64:
      aarch64_expand_fpsr_fpcr_setter (UNSPECV_SET_FPSR, DImode, exp);
      return target;
    case AARCH64_PAUTH_BUILTIN_AUTIA1716:
    case AARCH64_PAUTH_BUILTIN_PACIA1716:
    case AARCH64_PAUTH_BUILTIN_AUTIB1716:
    case AARCH64_PAUTH_BUILTIN_PACIB1716:
    case AARCH64_PAUTH_BUILTIN_XPACLRI:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = force_reg (Pmode, expand_normal (arg0));

      if (fcode == AARCH64_PAUTH_BUILTIN_XPACLRI)
	{
	  rtx lr = gen_rtx_REG (Pmode, R30_REGNUM);
	  icode = CODE_FOR_xpaclri;
	  emit_move_insn (lr, op0);
	  emit_insn (GEN_FCN (icode) ());
	  return lr;
	}
      else
	{
	  tree arg1 = CALL_EXPR_ARG (exp, 1);
	  rtx op1 = force_reg (Pmode, expand_normal (arg1));
	  switch (fcode)
	    {
	    case AARCH64_PAUTH_BUILTIN_AUTIA1716:
	      icode = CODE_FOR_autia1716;
	      break;
	    case AARCH64_PAUTH_BUILTIN_AUTIB1716:
	      icode = CODE_FOR_autib1716;
	      break;
	    case AARCH64_PAUTH_BUILTIN_PACIA1716:
	      icode = CODE_FOR_pacia1716;
	      break;
	    case AARCH64_PAUTH_BUILTIN_PACIB1716:
	      icode = CODE_FOR_pacib1716;
	      break;
	    default:
	      icode = 0;
	      gcc_unreachable ();
	    }

	  rtx x16_reg = gen_rtx_REG (Pmode, R16_REGNUM);
	  rtx x17_reg = gen_rtx_REG (Pmode, R17_REGNUM);
	  emit_move_insn (x17_reg, op0);
	  emit_move_insn (x16_reg, op1);
	  emit_insn (GEN_FCN (icode) ());
	  return x17_reg;
	}

    case AARCH64_JSCVT:
      {
	expand_operand ops[2];
	create_output_operand (&ops[0], target, SImode);
	op0 = expand_normal (CALL_EXPR_ARG (exp, 0));
	create_input_operand (&ops[1], op0, DFmode);
	expand_insn (CODE_FOR_aarch64_fjcvtzs, 2, ops);
	return ops[0].value;
      }

    case AARCH64_SIMD_BUILTIN_FCMLA_LANEQ0_V2SF:
    case AARCH64_SIMD_BUILTIN_FCMLA_LANEQ90_V2SF:
    case AARCH64_SIMD_BUILTIN_FCMLA_LANEQ180_V2SF:
    case AARCH64_SIMD_BUILTIN_FCMLA_LANEQ270_V2SF:
    case AARCH64_SIMD_BUILTIN_FCMLA_LANEQ0_V4HF:
    case AARCH64_SIMD_BUILTIN_FCMLA_LANEQ90_V4HF:
    case AARCH64_SIMD_BUILTIN_FCMLA_LANEQ180_V4HF:
    case AARCH64_SIMD_BUILTIN_FCMLA_LANEQ270_V4HF:
      return aarch64_expand_fcmla_builtin (exp, target, fcode);
    case AARCH64_BUILTIN_RNG_RNDR:
    case AARCH64_BUILTIN_RNG_RNDRRS:
      return aarch64_expand_rng_builtin (exp, target, fcode, ignore);
    case AARCH64_RSR:
    case AARCH64_RSRP:
    case AARCH64_RSR64:
    case AARCH64_RSRF:
    case AARCH64_RSRF64:
    case AARCH64_RSR128:
    case AARCH64_WSR:
    case AARCH64_WSRP:
    case AARCH64_WSR64:
    case AARCH64_WSRF:
    case AARCH64_WSRF64:
    case AARCH64_WSR128:
      return aarch64_expand_rwsr_builtin (exp, target, fcode);
    case AARCH64_PLD:
    case AARCH64_PLDX:
    case AARCH64_PLI:
    case AARCH64_PLIX:
      aarch64_expand_prefetch_builtin (exp, fcode);
      return target;

    case AARCH64_BUILTIN_CHKFEAT:
      {
	rtx x16_reg = gen_rtx_REG (DImode, R16_REGNUM);
	op0 = expand_normal (CALL_EXPR_ARG (exp, 0));
	emit_move_insn (x16_reg, op0);
	expand_insn (CODE_FOR_aarch64_chkfeat, 0, 0);
	return copy_to_reg (x16_reg);
      }

    case AARCH64_BUILTIN_GCSPR:
    case AARCH64_BUILTIN_GCSPOPM:
    case AARCH64_BUILTIN_GCSSS:
      return aarch64_expand_gcs_builtin (exp, target, fcode, ignore);
    }

  if (fcode >= AARCH64_SIMD_BUILTIN_BASE && fcode <= AARCH64_SIMD_BUILTIN_MAX)
    return aarch64_simd_expand_builtin (fcode, exp, target);
  else if (fcode >= AARCH64_CRC32_BUILTIN_BASE && fcode <= AARCH64_CRC32_BUILTIN_MAX)
    return aarch64_crc32_expand_builtin (fcode, exp, target);

  if (fcode == AARCH64_BUILTIN_RSQRT_DF
      || fcode == AARCH64_BUILTIN_RSQRT_SF
      || fcode == AARCH64_BUILTIN_RSQRT_V2DF
      || fcode == AARCH64_BUILTIN_RSQRT_V2SF
      || fcode == AARCH64_BUILTIN_RSQRT_V4SF)
    return aarch64_expand_builtin_rsqrt (fcode, exp, target);

  if (fcode == AARCH64_TME_BUILTIN_TSTART
      || fcode == AARCH64_TME_BUILTIN_TCOMMIT
      || fcode == AARCH64_TME_BUILTIN_TTEST
      || fcode == AARCH64_TME_BUILTIN_TCANCEL)
    return aarch64_expand_builtin_tme (fcode, exp, target);

  if (fcode == AARCH64_LS64_BUILTIN_LD64B
      || fcode == AARCH64_LS64_BUILTIN_ST64B
      || fcode == AARCH64_LS64_BUILTIN_ST64BV
      || fcode == AARCH64_LS64_BUILTIN_ST64BV0)
    return aarch64_expand_builtin_ls64 (fcode, exp, target);

  if (fcode >= AARCH64_MEMTAG_BUILTIN_START
      && fcode <= AARCH64_MEMTAG_BUILTIN_END)
    return aarch64_expand_builtin_memtag (fcode, exp, target);
  if (fcode >= AARCH64_REV16
      && fcode <= AARCH64_RBITLL)
    return aarch64_expand_builtin_data_intrinsic (fcode, exp, target);

  if (auto builtin_data = aarch64_get_pragma_builtin (fcode))
    return aarch64_expand_pragma_builtin (exp, target, builtin_data);

  gcc_unreachable ();
}

/* Return builtin for reciprocal square root.  */

tree
aarch64_general_builtin_rsqrt (unsigned int fn)
{
  if (fn == AARCH64_SIMD_BUILTIN_UNOP_sqrtv2df)
    return aarch64_builtin_decls[AARCH64_BUILTIN_RSQRT_V2DF];
  if (fn == AARCH64_SIMD_BUILTIN_UNOP_sqrtv2sf)
    return aarch64_builtin_decls[AARCH64_BUILTIN_RSQRT_V2SF];
  if (fn == AARCH64_SIMD_BUILTIN_UNOP_sqrtv4sf)
    return aarch64_builtin_decls[AARCH64_BUILTIN_RSQRT_V4SF];
  return NULL_TREE;
}

/* Return true if the lane check can be removed as there is no
   error going to be emitted.  */
static bool
aarch64_fold_builtin_lane_check (tree arg0, tree arg1, tree arg2)
{
  if (TREE_CODE (arg0) != INTEGER_CST)
    return false;
  if (TREE_CODE (arg1) != INTEGER_CST)
    return false;
  if (TREE_CODE (arg2) != INTEGER_CST)
    return false;

  auto totalsize = wi::to_widest (arg0);
  auto elementsize = wi::to_widest (arg1);
  if (totalsize == 0 || elementsize == 0)
    return false;
  auto lane = wi::to_widest (arg2);
  auto high = wi::udiv_trunc (totalsize, elementsize);
  return wi::ltu_p (lane, high);
}

#undef VAR1
#define VAR1(T, N, MAP, FLAG, A) \
  case AARCH64_SIMD_BUILTIN_##T##_##N##A:

#undef VREINTERPRET_BUILTIN
#define VREINTERPRET_BUILTIN(A, B, L) \
  case AARCH64_SIMD_BUILTIN_VREINTERPRET##L##_##A##_##B:

#undef VGET_LOW_BUILTIN
#define VGET_LOW_BUILTIN(A) \
  case AARCH64_SIMD_BUILTIN_VGET_LOW_##A:

#undef VGET_HIGH_BUILTIN
#define VGET_HIGH_BUILTIN(A) \
  case AARCH64_SIMD_BUILTIN_VGET_HIGH_##A:

/* Try to fold a call to the built-in function with subcode FCODE.  The
   function is passed the N_ARGS arguments in ARGS and it returns a value
   of type TYPE.  Return the new expression on success and NULL_TREE on
   failure.  */
tree
aarch64_general_fold_builtin (unsigned int fcode, tree type,
			      unsigned int n_args ATTRIBUTE_UNUSED, tree *args)
{
  switch (fcode)
    {
      BUILTIN_VDQF (UNOP, abs, 2, ALL)
	return fold_build1 (ABS_EXPR, type, args[0]);
      VAR1 (UNOP, floatv2si, 2, ALL, v2sf)
      VAR1 (UNOP, floatv4si, 2, ALL, v4sf)
      VAR1 (UNOP, floatv2di, 2, ALL, v2df)
	return fold_build1 (FLOAT_EXPR, type, args[0]);
      AARCH64_SIMD_VREINTERPRET_BUILTINS
	return fold_build1 (VIEW_CONVERT_EXPR, type, args[0]);
      AARCH64_SIMD_VGET_LOW_BUILTINS
	{
	  auto pos = BYTES_BIG_ENDIAN ? 64 : 0;

	  return fold_build3 (BIT_FIELD_REF, type, args[0], bitsize_int (64),
			      bitsize_int (pos));
	}
      AARCH64_SIMD_VGET_HIGH_BUILTINS
	{
	  auto pos = BYTES_BIG_ENDIAN ? 0 : 64;

	  return fold_build3 (BIT_FIELD_REF, type, args[0], bitsize_int (64),
			      bitsize_int (pos));
	}
      case AARCH64_SIMD_BUILTIN_LANE_CHECK:
	gcc_assert (n_args == 3);
	if (aarch64_fold_builtin_lane_check (args[0], args[1], args[2]))
	  return void_node;
	break;
      default:
	break;
    }

  return NULL_TREE;
}

enum aarch64_simd_type
get_mem_type_for_load_store (unsigned int fcode)
{
  switch (fcode)
  {
    VAR1 (LOAD1, ld1, 0, LOAD, v8qi)
    VAR1 (STORE1, st1, 0, STORE, v8qi)
      return Int8x8_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v16qi)
    VAR1 (STORE1, st1, 0, STORE, v16qi)
      return Int8x16_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v4hi)
    VAR1 (STORE1, st1, 0, STORE, v4hi)
      return Int16x4_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v8hi)
    VAR1 (STORE1, st1, 0, STORE, v8hi)
      return Int16x8_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v2si)
    VAR1 (STORE1, st1, 0, STORE, v2si)
      return Int32x2_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v4si)
    VAR1 (STORE1, st1, 0, STORE, v4si)
      return Int32x4_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v2di)
    VAR1 (STORE1, st1, 0, STORE, v2di)
      return Int64x2_t;
    VAR1 (LOAD1_U, ld1, 0, LOAD, v8qi)
    VAR1 (STORE1_U, st1, 0, STORE, v8qi)
      return Uint8x8_t;
    VAR1 (LOAD1_U, ld1, 0, LOAD, v16qi)
    VAR1 (STORE1_U, st1, 0, STORE, v16qi)
      return Uint8x16_t;
    VAR1 (LOAD1_U, ld1, 0, LOAD, v4hi)
    VAR1 (STORE1_U, st1, 0, STORE, v4hi)
      return Uint16x4_t;
    VAR1 (LOAD1_U, ld1, 0, LOAD, v8hi)
    VAR1 (STORE1_U, st1, 0, STORE, v8hi)
      return Uint16x8_t;
    VAR1 (LOAD1_U, ld1, 0, LOAD, v2si)
    VAR1 (STORE1_U, st1, 0, STORE, v2si)
      return Uint32x2_t;
    VAR1 (LOAD1_U, ld1, 0, LOAD, v4si)
    VAR1 (STORE1_U, st1, 0, STORE, v4si)
      return Uint32x4_t;
    VAR1 (LOAD1_U, ld1, 0, LOAD, v2di)
    VAR1 (STORE1_U, st1, 0, STORE, v2di)
      return Uint64x2_t;
    VAR1 (LOAD1_P, ld1, 0, LOAD, v8qi)
    VAR1 (STORE1_P, st1, 0, STORE, v8qi)
      return Poly8x8_t;
    VAR1 (LOAD1_P, ld1, 0, LOAD, v16qi)
    VAR1 (STORE1_P, st1, 0, STORE, v16qi)
      return Poly8x16_t;
    VAR1 (LOAD1_P, ld1, 0, LOAD, v4hi)
    VAR1 (STORE1_P, st1, 0, STORE, v4hi)
      return Poly16x4_t;
    VAR1 (LOAD1_P, ld1, 0, LOAD, v8hi)
    VAR1 (STORE1_P, st1, 0, STORE, v8hi)
      return Poly16x8_t;
    VAR1 (LOAD1_P, ld1, 0, LOAD, v2di)
    VAR1 (STORE1_P, st1, 0, STORE, v2di)
      return Poly64x2_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v4hf)
    VAR1 (STORE1, st1, 0, STORE, v4hf)
      return Float16x4_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v8hf)
    VAR1 (STORE1, st1, 0, STORE, v8hf)
      return Float16x8_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v4bf)
    VAR1 (STORE1, st1, 0, STORE, v4bf)
      return Bfloat16x4_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v8bf)
    VAR1 (STORE1, st1, 0, STORE, v8bf)
      return Bfloat16x8_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v2sf)
    VAR1 (STORE1, st1, 0, STORE, v2sf)
      return Float32x2_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v4sf)
    VAR1 (STORE1, st1, 0, STORE, v4sf)
      return Float32x4_t;
    VAR1 (LOAD1, ld1, 0, LOAD, v2df)
    VAR1 (STORE1, st1, 0, STORE, v2df)
      return Float64x2_t;
    default:
      gcc_unreachable ();
      break;
  }
}

/* We've seen a vector load from address ADDR.  Record it in
   vector_load_decls, if appropriate.  */
static void
aarch64_record_vector_load_arg (tree addr)
{
  tree decl = aarch64_vector_load_decl (addr);
  if (!decl)
    return;
  if (!cfun->machine->vector_load_decls)
    cfun->machine->vector_load_decls = hash_set<tree>::create_ggc (31);
  cfun->machine->vector_load_decls->add (decl);
}

/* Try to fold STMT, given that it's a call to the built-in function with
   subcode FCODE.  Return the new statement on success and null on
   failure.  */
gimple *
aarch64_general_gimple_fold_builtin (unsigned int fcode, gcall *stmt,
				     gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED)
{
  gimple *new_stmt = NULL;
  unsigned nargs = gimple_call_num_args (stmt);
  tree *args = (nargs > 0
		? gimple_call_arg_ptr (stmt, 0)
		: &error_mark_node);

  /* We use gimple's IFN_REDUC_(PLUS|MIN|MAX)s for float, signed int
     and unsigned int; it will distinguish according to the types of
     the arguments to the __builtin.  */
  switch (fcode)
    {
      BUILTIN_VALL (UNOP, reduc_plus_scal_, 10, ALL)
      BUILTIN_VDQ_I (UNOPU, reduc_plus_scal_, 10, NONE)
	new_stmt = gimple_build_call_internal (IFN_REDUC_PLUS,
					       1, args[0]);
	gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));
	break;

      /* Lower sqrt builtins to gimple/internal function sqrt. */
      BUILTIN_VHSDF_DF (UNOP, sqrt, 2, FP)
	new_stmt = gimple_build_call_internal (IFN_SQRT,
					       1, args[0]);
	gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));
	break;

     BUILTIN_VDC (BINOP, combine, 0, AUTO_FP)
     BUILTIN_VD_I (BINOPU, combine, 0, NONE)
     BUILTIN_VDC_P (BINOPP, combine, 0, NONE)
	{
	  tree first_part, second_part;
	  if (BYTES_BIG_ENDIAN)
	    {
	      second_part = args[0];
	      first_part = args[1];
	    }
	  else
	    {
	      first_part = args[0];
	      second_part = args[1];
	    }
	  tree ret_type = gimple_call_return_type (stmt);
	  tree ctor = build_constructor_va (ret_type, 2, NULL_TREE, first_part,
					    NULL_TREE, second_part);
	  new_stmt = gimple_build_assign (gimple_call_lhs (stmt), ctor);
	}
	break;

     /*lower store and load neon builtins to gimple.  */
     BUILTIN_VALL_F16 (LOAD1, ld1, 0, LOAD)
     BUILTIN_VDQ_I (LOAD1_U, ld1, 0, LOAD)
     BUILTIN_VALLP_NO_DI (LOAD1_P, ld1, 0, LOAD)
	/* Punt until after inlining, so that we stand more chance of
	   recording something meaningful in vector_load_decls.  */
	if (!cfun->after_inlining)
	  break;
	aarch64_record_vector_load_arg (args[0]);
	if (!BYTES_BIG_ENDIAN)
	  {
	    enum aarch64_simd_type mem_type
	      = get_mem_type_for_load_store(fcode);
	    aarch64_simd_type_info simd_type
	      = aarch64_simd_types[mem_type];
	    tree elt_ptr_type = build_pointer_type_for_mode (simd_type.eltype,
							     VOIDmode, true);
	    tree zero = build_zero_cst (elt_ptr_type);
	    /* Use element type alignment.  */
	    tree access_type
	      = build_aligned_type (simd_type.itype,
				    TYPE_ALIGN (simd_type.eltype));
	    new_stmt
	      = gimple_build_assign (gimple_get_lhs (stmt),
				     fold_build2 (MEM_REF,
						  access_type,
						  args[0], zero));
	    gimple_set_vuse (new_stmt, gimple_vuse (stmt));
	    gimple_set_vdef (new_stmt, gimple_vdef (stmt));
	  }
	break;

      BUILTIN_VALL_F16 (STORE1, st1, 0, STORE)
      BUILTIN_VDQ_I (STORE1_U, st1, 0, STORE)
      BUILTIN_VALLP_NO_DI (STORE1_P, st1, 0, STORE)
	if (!BYTES_BIG_ENDIAN)
	  {
	    enum aarch64_simd_type mem_type
	      = get_mem_type_for_load_store(fcode);
	    aarch64_simd_type_info simd_type
	      = aarch64_simd_types[mem_type];
	    tree elt_ptr_type = build_pointer_type_for_mode (simd_type.eltype,
							     VOIDmode, true);
	    tree zero = build_zero_cst (elt_ptr_type);
	    /* Use element type alignment.  */
	    tree access_type
	      = build_aligned_type (simd_type.itype,
				    TYPE_ALIGN (simd_type.eltype));
	    new_stmt
	      = gimple_build_assign (fold_build2 (MEM_REF, access_type,
						  args[0], zero),
				     args[1]);
	    gimple_set_vuse (new_stmt, gimple_vuse (stmt));
	    gimple_set_vdef (new_stmt, gimple_vdef (stmt));
	  }
	break;

      BUILTIN_VDQIF (UNOP, reduc_smax_scal_, 10, ALL)
      BUILTIN_VDQ_BHSI (UNOPU, reduc_umax_scal_, 10, ALL)
	new_stmt = gimple_build_call_internal (IFN_REDUC_MAX,
					       1, args[0]);
	gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));
	break;
      BUILTIN_VDQIF (UNOP, reduc_smin_scal_, 10, ALL)
      BUILTIN_VDQ_BHSI (UNOPU, reduc_umin_scal_, 10, ALL)
	new_stmt = gimple_build_call_internal (IFN_REDUC_MIN,
					       1, args[0]);
	gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));
	break;
      BUILTIN_VSDQ_I_DI (BINOP, ashl, 3, NONE)
	if (TREE_CODE (args[1]) == INTEGER_CST
	    && wi::ltu_p (wi::to_wide (args[1]), element_precision (args[0])))
	  new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
					  LSHIFT_EXPR, args[0], args[1]);
	break;
      BUILTIN_VSDQ_I_DI (BINOP, sshl, 0, NONE)
      BUILTIN_VSDQ_I_DI (BINOP_UUS, ushl, 0, NONE)
	{
	  tree cst = args[1];
	  tree ctype = TREE_TYPE (cst);
	  /* Left shifts can be both scalar or vector, e.g. uint64x1_t is
	     treated as a scalar type not a vector one.  */
	  if ((cst = uniform_integer_cst_p (cst)) != NULL_TREE)
	    {
	      wide_int wcst = wi::to_wide (cst);
	      tree unit_ty = TREE_TYPE (cst);

	      wide_int abs_cst = wi::abs (wcst);
	      if (wi::geu_p (abs_cst, element_precision (args[0])))
		break;

	      if (wi::neg_p (wcst, TYPE_SIGN (ctype)))
		{
		  tree final_cst;
		  final_cst = wide_int_to_tree (unit_ty, abs_cst);
		  if (TREE_CODE (cst) != INTEGER_CST)
		    final_cst = build_uniform_cst (ctype, final_cst);

		  new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
						  RSHIFT_EXPR, args[0],
						  final_cst);
		}
	      else
		new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
						LSHIFT_EXPR, args[0], args[1]);
	    }
	}
	break;
      BUILTIN_VDQ_I (SHIFTIMM, ashr, 3, NONE)
      VAR1 (SHIFTIMM, ashr_simd, 0, NONE, di)
      BUILTIN_VDQ_I (USHIFTIMM, lshr, 3, NONE)
      VAR1 (USHIFTIMM, lshr_simd, 0, NONE, di)
	if (TREE_CODE (args[1]) == INTEGER_CST
	    && wi::ltu_p (wi::to_wide (args[1]), element_precision (args[0])))
	  new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
					  RSHIFT_EXPR, args[0], args[1]);
	break;
      BUILTIN_GPF (BINOP, fmulx, 0, ALL)
	{
	  gcc_assert (nargs == 2);
	  bool a0_cst_p = TREE_CODE (args[0]) == REAL_CST;
	  bool a1_cst_p = TREE_CODE (args[1]) == REAL_CST;
	  if (a0_cst_p || a1_cst_p)
	    {
	      if (a0_cst_p && a1_cst_p)
		{
		  tree t0 = TREE_TYPE (args[0]);
		  real_value a0 = (TREE_REAL_CST (args[0]));
		  real_value a1 = (TREE_REAL_CST (args[1]));
		  if (real_equal (&a1, &dconst0))
		    std::swap (a0, a1);
		  /* According to real_equal (), +0 equals -0.  */
		  if (real_equal (&a0, &dconst0) && real_isinf (&a1))
		    {
		      real_value res = dconst2;
		      res.sign = a0.sign ^ a1.sign;
		      new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
						      REAL_CST,
						      build_real (t0, res));
		    }
		  else
		    new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
						    MULT_EXPR,
						    args[0], args[1]);
		}
	      else /* a0_cst_p ^ a1_cst_p.  */
		{
		  real_value const_part = a0_cst_p
		    ? TREE_REAL_CST (args[0]) : TREE_REAL_CST (args[1]);
		  if (!real_equal (&const_part, &dconst0)
		      && !real_isinf (&const_part))
		    new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
						    MULT_EXPR, args[0],
						    args[1]);
		}
	    }
	  if (new_stmt)
	    {
	      gimple_set_vuse (new_stmt, gimple_vuse (stmt));
	      gimple_set_vdef (new_stmt, gimple_vdef (stmt));
	    }
	  break;
	}
    case AARCH64_SIMD_BUILTIN_LANE_CHECK:
      if (aarch64_fold_builtin_lane_check (args[0], args[1], args[2]))
	{
	  unlink_stmt_vdef (stmt);
	  release_defs (stmt);
	  new_stmt = gimple_build_nop ();
	}
      break;
    default:
      break;
    }

  /* GIMPLE assign statements (unlike calls) require a non-null lhs. If we
     created an assign statement with a null lhs, then fix this by assigning
     to a new (and subsequently unused) variable. */
  if (new_stmt && is_gimple_assign (new_stmt) && !gimple_assign_lhs (new_stmt))
    {
      tree new_lhs = make_ssa_name (gimple_call_return_type (stmt));
      gimple_assign_set_lhs (new_stmt, new_lhs);
    }

  return new_stmt;
}

void
aarch64_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update)
{
  const unsigned AARCH64_FE_INVALID = 1;
  const unsigned AARCH64_FE_DIVBYZERO = 2;
  const unsigned AARCH64_FE_OVERFLOW = 4;
  const unsigned AARCH64_FE_UNDERFLOW = 8;
  const unsigned AARCH64_FE_INEXACT = 16;
  const unsigned HOST_WIDE_INT AARCH64_FE_ALL_EXCEPT = (AARCH64_FE_INVALID
							| AARCH64_FE_DIVBYZERO
							| AARCH64_FE_OVERFLOW
							| AARCH64_FE_UNDERFLOW
							| AARCH64_FE_INEXACT);
  const unsigned HOST_WIDE_INT AARCH64_FE_EXCEPT_SHIFT = 8;
  tree fenv_cr, fenv_sr, get_fpcr, set_fpcr, mask_cr, mask_sr;
  tree ld_fenv_cr, ld_fenv_sr, masked_fenv_cr, masked_fenv_sr, hold_fnclex_cr;
  tree hold_fnclex_sr, new_fenv_var, reload_fenv, restore_fnenv, get_fpsr, set_fpsr;
  tree update_call, atomic_feraiseexcept, hold_fnclex, masked_fenv, ld_fenv;

  /* Generate the equivalence of :
       unsigned int fenv_cr;
       fenv_cr = __builtin_aarch64_get_fpcr ();

       unsigned int fenv_sr;
       fenv_sr = __builtin_aarch64_get_fpsr ();

       Now set all exceptions to non-stop
       unsigned int mask_cr
		= ~(AARCH64_FE_ALL_EXCEPT << AARCH64_FE_EXCEPT_SHIFT);
       unsigned int masked_cr;
       masked_cr = fenv_cr & mask_cr;

       And clear all exception flags
       unsigned int maske_sr = ~AARCH64_FE_ALL_EXCEPT;
       unsigned int masked_cr;
       masked_sr = fenv_sr & mask_sr;

       __builtin_aarch64_set_cr (masked_cr);
       __builtin_aarch64_set_sr (masked_sr);  */

  fenv_cr = create_tmp_var_raw (unsigned_type_node);
  fenv_sr = create_tmp_var_raw (unsigned_type_node);

  get_fpcr = aarch64_builtin_decls[AARCH64_BUILTIN_GET_FPCR];
  set_fpcr = aarch64_builtin_decls[AARCH64_BUILTIN_SET_FPCR];
  get_fpsr = aarch64_builtin_decls[AARCH64_BUILTIN_GET_FPSR];
  set_fpsr = aarch64_builtin_decls[AARCH64_BUILTIN_SET_FPSR];

  mask_cr = build_int_cst (unsigned_type_node,
			   ~(AARCH64_FE_ALL_EXCEPT << AARCH64_FE_EXCEPT_SHIFT));
  mask_sr = build_int_cst (unsigned_type_node,
			   ~(AARCH64_FE_ALL_EXCEPT));

  ld_fenv_cr = build4 (TARGET_EXPR, unsigned_type_node,
		       fenv_cr, build_call_expr (get_fpcr, 0),
		       NULL_TREE, NULL_TREE);
  ld_fenv_sr = build4 (TARGET_EXPR, unsigned_type_node,
		       fenv_sr, build_call_expr (get_fpsr, 0),
		       NULL_TREE, NULL_TREE);

  masked_fenv_cr = build2 (BIT_AND_EXPR, unsigned_type_node, fenv_cr, mask_cr);
  masked_fenv_sr = build2 (BIT_AND_EXPR, unsigned_type_node, fenv_sr, mask_sr);

  hold_fnclex_cr = build_call_expr (set_fpcr, 1, masked_fenv_cr);
  hold_fnclex_sr = build_call_expr (set_fpsr, 1, masked_fenv_sr);

  hold_fnclex = build2 (COMPOUND_EXPR, void_type_node, hold_fnclex_cr,
			hold_fnclex_sr);
  masked_fenv = build2 (COMPOUND_EXPR, void_type_node, masked_fenv_cr,
			masked_fenv_sr);
  ld_fenv = build2 (COMPOUND_EXPR, void_type_node, ld_fenv_cr, ld_fenv_sr);

  *hold = build2 (COMPOUND_EXPR, void_type_node,
		  build2 (COMPOUND_EXPR, void_type_node, masked_fenv, ld_fenv),
		  hold_fnclex);

  /* Store the value of masked_fenv to clear the exceptions:
     __builtin_aarch64_set_fpsr (masked_fenv_sr);  */

  *clear = build_call_expr (set_fpsr, 1, masked_fenv_sr);

  /* Generate the equivalent of :
       unsigned int new_fenv_var;
       new_fenv_var = __builtin_aarch64_get_fpsr ();

       __builtin_aarch64_set_fpsr (fenv_sr);

       __atomic_feraiseexcept (new_fenv_var);  */

  new_fenv_var = create_tmp_var_raw (unsigned_type_node);
  reload_fenv = build4 (TARGET_EXPR, unsigned_type_node,
			new_fenv_var, build_call_expr (get_fpsr, 0),
			NULL_TREE, NULL_TREE);
  restore_fnenv = build_call_expr (set_fpsr, 1, fenv_sr);
  atomic_feraiseexcept = builtin_decl_implicit (BUILT_IN_ATOMIC_FERAISEEXCEPT);
  update_call = build_call_expr (atomic_feraiseexcept, 1,
				 fold_convert (integer_type_node, new_fenv_var));
  *update = build2 (COMPOUND_EXPR, void_type_node,
		    build2 (COMPOUND_EXPR, void_type_node,
			    reload_fenv, restore_fnenv), update_call);
}

/* Resolve overloaded MEMTAG build-in functions.  */
#define AARCH64_BUILTIN_SUBCODE(F) \
  (DECL_MD_FUNCTION_CODE (F) >> AARCH64_BUILTIN_SHIFT)

static tree
aarch64_resolve_overloaded_memtag (location_t loc,
				   tree fndecl, void *pass_params)
{
  vec<tree, va_gc> *params = static_cast<vec<tree, va_gc> *> (pass_params);
  unsigned param_num = params ? params->length() : 0;
  unsigned int fcode = AARCH64_BUILTIN_SUBCODE (fndecl);
  tree inittype = aarch64_memtag_builtin_data[
		    fcode - AARCH64_MEMTAG_BUILTIN_START - 1].ftype;
  unsigned arg_num = list_length (TYPE_ARG_TYPES (inittype)) - 1;

  if (param_num != arg_num)
    {
      TREE_TYPE (fndecl) = inittype;
      return NULL_TREE;
    }
  tree retype = NULL;

  if (fcode == AARCH64_MEMTAG_BUILTIN_SUBP)
    {
      tree t0 = TREE_TYPE ((*params)[0]);
      tree t1 = TREE_TYPE ((*params)[1]);

      if (t0 == error_mark_node || TREE_CODE (t0) != POINTER_TYPE)
	t0 = ptr_type_node;
      if (t1 == error_mark_node || TREE_CODE (t1) != POINTER_TYPE)
	t1 = ptr_type_node;

      if (TYPE_MODE (t0) != DImode)
	warning_at (loc, 1, "expected 64-bit address but argument 1 is %d-bit",
	    (int)tree_to_shwi (DECL_SIZE ((*params)[0])));

      if (TYPE_MODE (t1) != DImode)
	warning_at (loc, 1, "expected 64-bit address but argument 2 is %d-bit",
	    (int)tree_to_shwi (DECL_SIZE ((*params)[1])));

      retype = build_function_type_list (ptrdiff_type_node, t0, t1, NULL);
    }
  else
    {
      tree t0 = TREE_TYPE ((*params)[0]);

      if (t0 == error_mark_node || TREE_CODE (t0) != POINTER_TYPE)
	{
	  TREE_TYPE (fndecl) = inittype;
	  return NULL_TREE;
	}

      if (TYPE_MODE (t0) != DImode)
	warning_at (loc, 1, "expected 64-bit address but argument 1 is %d-bit",
	    (int)tree_to_shwi (DECL_SIZE ((*params)[0])));

      switch (fcode)
	{
	case AARCH64_MEMTAG_BUILTIN_IRG:
	  retype = build_function_type_list (t0, t0, uint64_type_node, NULL);
	  break;
	case AARCH64_MEMTAG_BUILTIN_GMI:
	  retype = build_function_type_list (uint64_type_node, t0,
	      uint64_type_node, NULL);
	  break;
	case AARCH64_MEMTAG_BUILTIN_INC_TAG:
	  retype = build_function_type_list (t0, t0, unsigned_type_node, NULL);
	  break;
	case AARCH64_MEMTAG_BUILTIN_SET_TAG:
	  retype = build_function_type_list (void_type_node, t0, NULL);
	  break;
	case AARCH64_MEMTAG_BUILTIN_GET_TAG:
	  retype = build_function_type_list (t0, t0, NULL);
	  break;
	default:
	  return NULL_TREE;
	}
    }

  if (!retype || retype == error_mark_node)
    TREE_TYPE (fndecl) = inittype;
  else
    TREE_TYPE (fndecl) = retype;

  return NULL_TREE;
}

/* Called at aarch64_resolve_overloaded_builtin in aarch64-c.cc.  */
tree
aarch64_resolve_overloaded_builtin_general (location_t loc, tree function,
					    void *pass_params)
{
  unsigned int fcode = AARCH64_BUILTIN_SUBCODE (function);

  if (fcode >= AARCH64_MEMTAG_BUILTIN_START
      && fcode <= AARCH64_MEMTAG_BUILTIN_END)
    return aarch64_resolve_overloaded_memtag(loc, function, pass_params);

  return NULL_TREE;
}

#undef AARCH64_CHECK_BUILTIN_MODE
#undef AARCH64_FIND_FRINT_VARIANT
#undef CF0
#undef CF1
#undef CF2
#undef CF3
#undef CF4
#undef CF10
#undef ENTRY_VHSDF
#undef VAR1
#undef VAR2
#undef VAR3
#undef VAR4
#undef VAR5
#undef VAR6
#undef VAR7
#undef VAR8
#undef VAR9
#undef VAR10
#undef VAR11

#include "gt-aarch64-builtins.h"
