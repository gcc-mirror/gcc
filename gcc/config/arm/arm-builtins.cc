/* Description of builtins used by the ARM backend.
   Copyright (C) 2014-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "rtl.h"
#include "tree.h"
#include "gimple-expr.h"
#include "memmodel.h"
#include "tm_p.h"
#include "profile-count.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "case-cfn-macros.h"
#include "sbitmap.h"
#include "stringpool.h"
#include "arm-builtins.h"
#include "stringpool.h"
#include "attribs.h"
#include "basic-block.h"
#include "gimple.h"
#include "ssa.h"

#define SIMD_MAX_BUILTIN_ARGS 7

/*  The qualifier_internal allows generation of a unary builtin from
    a pattern with a third pseudo-operand such as a match_scratch.
    T (T).  */
static enum arm_type_qualifiers
arm_unop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_internal };
#define UNOP_QUALIFIERS (arm_unop_qualifiers)

/* unsigned T (unsigned T).  */
static enum arm_type_qualifiers
arm_bswap_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned };
#define BSWAP_QUALIFIERS (arm_bswap_qualifiers)

/* T (T, T [maybe_immediate]).  */
static enum arm_type_qualifiers
arm_binop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_maybe_immediate };
#define BINOP_QUALIFIERS (arm_binop_qualifiers)

/* T (T, T, T).  */
static enum arm_type_qualifiers
arm_ternop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_none };
#define TERNOP_QUALIFIERS (arm_ternop_qualifiers)

/* unsigned T (unsigned T, unsigned T, unsigned T).  */
static enum arm_type_qualifiers
arm_unsigned_uternop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
      qualifier_unsigned };
#define UTERNOP_QUALIFIERS (arm_unsigned_uternop_qualifiers)

/* T (T, unsigned T, T).  */
static enum arm_type_qualifiers
arm_usternop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_unsigned,
      qualifier_none };
#define USTERNOP_QUALIFIERS (arm_usternop_qualifiers)

/* T (T, immediate).  */
static enum arm_type_qualifiers
arm_binop_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_immediate };
#define BINOP_IMM_QUALIFIERS (arm_binop_imm_qualifiers)

/* T (T, unsigned immediate).  */
static enum arm_type_qualifiers
arm_sat_binop_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_unsigned_immediate };
#define SAT_BINOP_UNSIGNED_IMM_QUALIFIERS \
  (arm_sat_binop_imm_qualifiers)

/* unsigned T (T, unsigned immediate).  */
static enum arm_type_qualifiers
arm_unsigned_sat_binop_unsigned_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_none, qualifier_unsigned_immediate };
#define UNSIGNED_SAT_BINOP_UNSIGNED_IMM_QUALIFIERS \
  (arm_unsigned_sat_binop_unsigned_imm_qualifiers)

/* T (T, lane index).  */
static enum arm_type_qualifiers
arm_getlane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_lane_index };
#define GETLANE_QUALIFIERS (arm_getlane_qualifiers)

/* T (T, T, T, immediate).  */
static enum arm_type_qualifiers
arm_mac_n_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none,
      qualifier_none, qualifier_immediate };
#define MAC_N_QUALIFIERS (arm_mac_n_qualifiers)

/* T (T, T, T, lane index).  */
static enum arm_type_qualifiers
arm_mac_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none,
      qualifier_none, qualifier_lane_index };
#define MAC_LANE_QUALIFIERS (arm_mac_lane_qualifiers)

/* T (T, T, T, lane pair index).  */
static enum arm_type_qualifiers
arm_mac_lane_pair_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none,
      qualifier_none, qualifier_lane_pair_index };
#define MAC_LANE_PAIR_QUALIFIERS (arm_mac_lane_pair_qualifiers)

/* unsigned T (unsigned T, unsigned T, unsigend T, lane index).  */
static enum arm_type_qualifiers
arm_umac_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
      qualifier_unsigned, qualifier_lane_index };
#define UMAC_LANE_QUALIFIERS (arm_umac_lane_qualifiers)

/* T (T, unsigned T, T, lane index).  */
static enum arm_type_qualifiers
arm_usmac_lane_quadtup_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_unsigned,
      qualifier_none, qualifier_lane_quadtup_index };
#define USMAC_LANE_QUADTUP_QUALIFIERS (arm_usmac_lane_quadtup_qualifiers)

/* T (T, T, unsigend T, lane index).  */
static enum arm_type_qualifiers
arm_sumac_lane_quadtup_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none,
      qualifier_unsigned, qualifier_lane_quadtup_index };
#define SUMAC_LANE_QUADTUP_QUALIFIERS (arm_sumac_lane_quadtup_qualifiers)

/* T (T, T, immediate).  */
static enum arm_type_qualifiers
arm_ternop_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_immediate };
#define TERNOP_IMM_QUALIFIERS (arm_ternop_imm_qualifiers)

/* T (T, T, lane index).  */
static enum arm_type_qualifiers
arm_setlane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_lane_index };
#define SETLANE_QUALIFIERS (arm_setlane_qualifiers)

/* T (T, T).  */
static enum arm_type_qualifiers
arm_combine_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none };
#define COMBINE_QUALIFIERS (arm_combine_qualifiers)

/* T ([T element type] *).  */
static enum arm_type_qualifiers
arm_load1_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_const_pointer_map_mode };
#define LOAD1_QUALIFIERS (arm_load1_qualifiers)

/* T ([T element type] *, T, immediate).  */
static enum arm_type_qualifiers
arm_load1_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_const_pointer_map_mode,
      qualifier_none, qualifier_struct_load_store_lane_index };
#define LOAD1LANE_QUALIFIERS (arm_load1_lane_qualifiers)

/* unsigned T (unsigned T, unsigned T, unsigned T).  */
static enum arm_type_qualifiers
arm_unsigned_binop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
      qualifier_unsigned };
#define UBINOP_QUALIFIERS (arm_unsigned_binop_qualifiers)

/* void (unsigned immediate, unsigned immediate, unsigned immediate,
	 unsigned immediate, unsigned immediate, unsigned immediate).  */
static enum arm_type_qualifiers
arm_cdp_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_unsigned_immediate,
      qualifier_unsigned_immediate,
      qualifier_unsigned_immediate,
      qualifier_unsigned_immediate,
      qualifier_unsigned_immediate,
      qualifier_unsigned_immediate };
#define CDP_QUALIFIERS \
  (arm_cdp_qualifiers)

/* void (unsigned immediate, unsigned immediate,  const void *).  */
static enum arm_type_qualifiers
arm_ldc_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_unsigned_immediate,
      qualifier_unsigned_immediate, qualifier_const_void_pointer };
#define LDC_QUALIFIERS \
  (arm_ldc_qualifiers)

/* void (unsigned immediate, unsigned immediate,  void *).  */
static enum arm_type_qualifiers
arm_stc_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_unsigned_immediate,
      qualifier_unsigned_immediate, qualifier_void_pointer };
#define STC_QUALIFIERS \
  (arm_stc_qualifiers)

/* void (unsigned immediate, unsigned immediate,  T, unsigned immediate,
	 unsigned immediate, unsigned immediate).  */
static enum arm_type_qualifiers
arm_mcr_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_unsigned_immediate,
      qualifier_unsigned_immediate, qualifier_none,
      qualifier_unsigned_immediate, qualifier_unsigned_immediate,
      qualifier_unsigned_immediate };
#define MCR_QUALIFIERS \
  (arm_mcr_qualifiers)

/* T (unsigned immediate, unsigned immediate, unsigned immediate,
      unsigned immediate, unsigned immediate).  */
static enum arm_type_qualifiers
arm_mrc_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_unsigned_immediate,
      qualifier_unsigned_immediate, qualifier_unsigned_immediate,
      qualifier_unsigned_immediate, qualifier_unsigned_immediate };
#define MRC_QUALIFIERS \
  (arm_mrc_qualifiers)

/* void (unsigned immediate, unsigned immediate,  T, unsigned immediate).  */
static enum arm_type_qualifiers
arm_mcrr_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_unsigned_immediate,
      qualifier_unsigned_immediate, qualifier_none,
      qualifier_unsigned_immediate };
#define MCRR_QUALIFIERS \
  (arm_mcrr_qualifiers)

/* T (unsigned immediate, unsigned immediate, unsigned immediate).  */
static enum arm_type_qualifiers
arm_mrrc_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_unsigned_immediate,
      qualifier_unsigned_immediate, qualifier_unsigned_immediate };
#define MRRC_QUALIFIERS \
  (arm_mrrc_qualifiers)

/* T (immediate, unsigned immediate).  */
static enum arm_type_qualifiers
arm_cx_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_immediate, qualifier_unsigned_immediate };
#define CX_IMM_QUALIFIERS (arm_cx_imm_qualifiers)

/* T (immediate, T, unsigned immediate).  */
static enum arm_type_qualifiers
arm_cx_unary_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_immediate, qualifier_none,
      qualifier_unsigned_immediate };
#define CX_UNARY_QUALIFIERS (arm_cx_unary_qualifiers)

/* T (immediate, T, T, unsigned immediate).  */
static enum arm_type_qualifiers
arm_cx_binary_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_immediate,
      qualifier_none, qualifier_none,
      qualifier_unsigned_immediate };
#define CX_BINARY_QUALIFIERS (arm_cx_binary_qualifiers)

/* T (immediate, T, T, T, unsigned immediate).  */
static enum arm_type_qualifiers
arm_cx_ternary_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_immediate,
      qualifier_none, qualifier_none, qualifier_none,
      qualifier_unsigned_immediate };
#define CX_TERNARY_QUALIFIERS (arm_cx_ternary_qualifiers)

/* T (immediate, T, unsigned immediate).  */
static enum arm_type_qualifiers
arm_cx_unary_unone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_immediate, qualifier_none,
      qualifier_unsigned_immediate,
      qualifier_predicate };
#define CX_UNARY_UNONE_QUALIFIERS (arm_cx_unary_unone_qualifiers)

/* T (immediate, T, T, unsigned immediate).  */
static enum arm_type_qualifiers
arm_cx_binary_unone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_immediate,
      qualifier_none, qualifier_none,
      qualifier_unsigned_immediate,
      qualifier_predicate };
#define CX_BINARY_UNONE_QUALIFIERS (arm_cx_binary_unone_qualifiers)

/* T (immediate, T, T, T, unsigned immediate).  */
static enum arm_type_qualifiers
arm_cx_ternary_unone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_immediate,
      qualifier_none, qualifier_none, qualifier_none,
      qualifier_unsigned_immediate,
      qualifier_predicate };
#define CX_TERNARY_UNONE_QUALIFIERS (arm_cx_ternary_unone_qualifiers)

/* The first argument (return type) of a store should be void type,
   which we represent with qualifier_void.  Their first operand will be
   a DImode pointer to the location to store to, so we must use
   qualifier_map_mode | qualifier_pointer to build a pointer to the
   element type of the vector.

   void ([T element type] *, T).  */
static enum arm_type_qualifiers
arm_store1_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_pointer_map_mode, qualifier_none };
#define STORE1_QUALIFIERS (arm_store1_qualifiers)

/* Qualifiers for MVE builtins.  */

static enum arm_type_qualifiers
arm_unop_none_none_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none };
#define UNOP_NONE_NONE_QUALIFIERS \
  (arm_unop_none_none_qualifiers)

static enum arm_type_qualifiers
arm_unop_none_snone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none };
#define UNOP_NONE_SNONE_QUALIFIERS \
  (arm_unop_none_snone_qualifiers)

static enum arm_type_qualifiers
arm_unop_none_unone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_unsigned };
#define UNOP_NONE_UNONE_QUALIFIERS \
  (arm_unop_none_unone_qualifiers)

static enum arm_type_qualifiers
arm_unop_snone_snone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none };
#define UNOP_SNONE_SNONE_QUALIFIERS \
  (arm_unop_snone_snone_qualifiers)

static enum arm_type_qualifiers
arm_unop_snone_none_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none };
#define UNOP_SNONE_NONE_QUALIFIERS \
  (arm_unop_snone_none_qualifiers)

static enum arm_type_qualifiers
arm_unop_snone_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_immediate };
#define UNOP_SNONE_IMM_QUALIFIERS \
  (arm_unop_snone_imm_qualifiers)

static enum arm_type_qualifiers
arm_unop_unone_none_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_none };
#define UNOP_UNONE_NONE_QUALIFIERS \
  (arm_unop_unone_none_qualifiers)

static enum arm_type_qualifiers
arm_unop_unone_unone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned };
#define UNOP_UNONE_UNONE_QUALIFIERS \
  (arm_unop_unone_unone_qualifiers)

static enum arm_type_qualifiers
arm_unop_unone_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_immediate };
#define UNOP_UNONE_IMM_QUALIFIERS \
  (arm_unop_unone_imm_qualifiers)

static enum arm_type_qualifiers
arm_unop_pred_unone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_predicate, qualifier_unsigned };
#define UNOP_PRED_UNONE_QUALIFIERS \
  (arm_unop_pred_unone_qualifiers)

static enum arm_type_qualifiers
arm_unop_pred_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_predicate, qualifier_predicate };
#define UNOP_PRED_PRED_QUALIFIERS \
  (arm_unop_pred_pred_qualifiers)


static enum arm_type_qualifiers
arm_binop_none_none_none_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none };
#define BINOP_NONE_NONE_NONE_QUALIFIERS \
  (arm_binop_none_none_none_qualifiers)

static enum arm_type_qualifiers
arm_binop_none_none_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_immediate };
#define BINOP_NONE_NONE_IMM_QUALIFIERS \
  (arm_binop_none_none_imm_qualifiers)

static enum arm_type_qualifiers
arm_binop_none_unone_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_unsigned, qualifier_immediate };
#define BINOP_NONE_UNONE_IMM_QUALIFIERS \
  (arm_binop_none_unone_imm_qualifiers)

static enum arm_type_qualifiers
arm_binop_none_unone_unone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_unsigned, qualifier_unsigned };
#define BINOP_NONE_UNONE_UNONE_QUALIFIERS \
  (arm_binop_none_unone_unone_qualifiers)

static enum arm_type_qualifiers
arm_binop_unone_unone_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_immediate };
#define BINOP_UNONE_UNONE_IMM_QUALIFIERS \
  (arm_binop_unone_unone_imm_qualifiers)

static enum arm_type_qualifiers
arm_binop_unone_unone_unone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned };
#define BINOP_UNONE_UNONE_UNONE_QUALIFIERS \
  (arm_binop_unone_unone_unone_qualifiers)

static enum arm_type_qualifiers
arm_binop_pred_unone_unone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_predicate, qualifier_unsigned, qualifier_unsigned };
#define BINOP_PRED_UNONE_UNONE_QUALIFIERS \
  (arm_binop_pred_unone_unone_qualifiers)

static enum arm_type_qualifiers
arm_binop_pred_unone_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_predicate, qualifier_unsigned, qualifier_predicate };
#define BINOP_PRED_UNONE_PRED_QUALIFIERS \
  (arm_binop_pred_unone_pred_qualifiers)

static enum arm_type_qualifiers
arm_binop_unone_none_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_none, qualifier_immediate };
#define BINOP_UNONE_NONE_IMM_QUALIFIERS \
  (arm_binop_unone_none_imm_qualifiers)

static enum arm_type_qualifiers
arm_binop_pred_none_none_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_predicate, qualifier_none, qualifier_none };
#define BINOP_PRED_NONE_NONE_QUALIFIERS \
  (arm_binop_pred_none_none_qualifiers)

static enum arm_type_qualifiers
arm_binop_unone_unone_none_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_none };
#define BINOP_UNONE_UNONE_NONE_QUALIFIERS \
  (arm_binop_unone_unone_none_qualifiers)

static enum arm_type_qualifiers
arm_ternop_unone_unone_unone_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
    qualifier_immediate };
#define TERNOP_UNONE_UNONE_UNONE_IMM_QUALIFIERS \
  (arm_ternop_unone_unone_unone_imm_qualifiers)

static enum arm_type_qualifiers
arm_ternop_unone_unone_none_none_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_none, qualifier_none };
#define TERNOP_UNONE_UNONE_NONE_NONE_QUALIFIERS \
  (arm_ternop_unone_unone_none_none_qualifiers)

static enum arm_type_qualifiers
arm_ternop_unone_unone_none_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_none,
    qualifier_immediate };
#define TERNOP_UNONE_UNONE_NONE_IMM_QUALIFIERS \
  (arm_ternop_unone_unone_none_imm_qualifiers)

static enum arm_type_qualifiers
arm_ternop_unone_unone_none_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_none,
      qualifier_predicate };
#define TERNOP_UNONE_UNONE_NONE_PRED_QUALIFIERS \
  (arm_ternop_unone_unone_none_pred_qualifiers)

static enum arm_type_qualifiers
arm_ternop_unone_unone_imm_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_immediate,
    qualifier_predicate };
#define TERNOP_UNONE_UNONE_IMM_PRED_QUALIFIERS \
  (arm_ternop_unone_unone_imm_pred_qualifiers)

static enum arm_type_qualifiers
arm_ternop_pred_none_none_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_predicate, qualifier_none, qualifier_none, qualifier_predicate };
#define TERNOP_PRED_NONE_NONE_PRED_QUALIFIERS \
  (arm_ternop_pred_none_none_pred_qualifiers)

static enum arm_type_qualifiers
arm_ternop_none_none_none_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_immediate };
#define TERNOP_NONE_NONE_NONE_IMM_QUALIFIERS \
  (arm_ternop_none_none_none_imm_qualifiers)

static enum arm_type_qualifiers
arm_ternop_none_none_none_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_predicate };
#define TERNOP_NONE_NONE_NONE_PRED_QUALIFIERS \
  (arm_ternop_none_none_none_pred_qualifiers)

static enum arm_type_qualifiers
arm_ternop_none_none_imm_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_immediate, qualifier_predicate };
#define TERNOP_NONE_NONE_IMM_PRED_QUALIFIERS \
  (arm_ternop_none_none_imm_pred_qualifiers)

static enum arm_type_qualifiers
arm_ternop_none_none_unone_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_unsigned, qualifier_predicate };
#define TERNOP_NONE_NONE_UNONE_PRED_QUALIFIERS \
  (arm_ternop_none_none_unone_pred_qualifiers)

static enum arm_type_qualifiers
arm_ternop_unone_unone_unone_unone_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
    qualifier_unsigned };
#define TERNOP_UNONE_UNONE_UNONE_UNONE_QUALIFIERS \
  (arm_ternop_unone_unone_unone_unone_qualifiers)

static enum arm_type_qualifiers
arm_ternop_unone_unone_unone_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
    qualifier_predicate };
#define TERNOP_UNONE_UNONE_UNONE_PRED_QUALIFIERS \
  (arm_ternop_unone_unone_unone_pred_qualifiers)

static enum arm_type_qualifiers
arm_ternop_pred_unone_unone_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_predicate, qualifier_unsigned, qualifier_unsigned,
    qualifier_predicate };
#define TERNOP_PRED_UNONE_UNONE_PRED_QUALIFIERS \
  (arm_ternop_pred_unone_unone_pred_qualifiers)

static enum arm_type_qualifiers
arm_ternop_none_none_none_none_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_none };
#define TERNOP_NONE_NONE_NONE_NONE_QUALIFIERS \
  (arm_ternop_none_none_none_none_qualifiers)

static enum arm_type_qualifiers
arm_quadop_unone_unone_none_none_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_none, qualifier_none,
    qualifier_predicate };
#define QUADOP_UNONE_UNONE_NONE_NONE_PRED_QUALIFIERS \
  (arm_quadop_unone_unone_none_none_pred_qualifiers)

static enum arm_type_qualifiers
arm_quadop_none_none_none_none_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_none,
    qualifier_predicate };
#define QUADOP_NONE_NONE_NONE_NONE_PRED_QUALIFIERS \
  (arm_quadop_none_none_none_none_pred_qualifiers)

static enum arm_type_qualifiers
arm_quadop_none_none_none_imm_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_immediate,
    qualifier_predicate };
#define QUADOP_NONE_NONE_NONE_IMM_PRED_QUALIFIERS \
  (arm_quadop_none_none_none_imm_pred_qualifiers)

static enum arm_type_qualifiers
arm_quadop_unone_unone_unone_unone_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
    qualifier_unsigned, qualifier_predicate };
#define QUADOP_UNONE_UNONE_UNONE_UNONE_PRED_QUALIFIERS \
  (arm_quadop_unone_unone_unone_unone_pred_qualifiers)

static enum arm_type_qualifiers
arm_quadop_unone_unone_none_imm_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_none,
    qualifier_immediate, qualifier_predicate };
#define QUADOP_UNONE_UNONE_NONE_IMM_PRED_QUALIFIERS \
  (arm_quadop_unone_unone_none_imm_pred_qualifiers)

static enum arm_type_qualifiers
arm_quadop_none_none_unone_imm_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_unsigned, qualifier_immediate,
    qualifier_predicate };
#define QUADOP_NONE_NONE_UNONE_IMM_PRED_QUALIFIERS \
  (arm_quadop_none_none_unone_imm_pred_qualifiers)

static enum arm_type_qualifiers
arm_quadop_unone_unone_unone_imm_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
    qualifier_immediate, qualifier_predicate };
#define QUADOP_UNONE_UNONE_UNONE_IMM_PRED_QUALIFIERS \
  (arm_quadop_unone_unone_unone_imm_pred_qualifiers)

static enum arm_type_qualifiers
arm_quadop_unone_unone_unone_none_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
    qualifier_none, qualifier_predicate };
#define QUADOP_UNONE_UNONE_UNONE_NONE_PRED_QUALIFIERS \
  (arm_quadop_unone_unone_unone_none_pred_qualifiers)

static enum arm_type_qualifiers
arm_lsll_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_none};
#define LSLL_QUALIFIERS (arm_lsll_qualifiers)

static enum arm_type_qualifiers
arm_uqshl_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_const};
#define UQSHL_QUALIFIERS (arm_uqshl_qualifiers)

static enum arm_type_qualifiers
arm_asrl_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none};
#define ASRL_QUALIFIERS (arm_asrl_qualifiers)

static enum arm_type_qualifiers
arm_sqshl_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_const};
#define SQSHL_QUALIFIERS (arm_sqshl_qualifiers)

static enum arm_type_qualifiers
arm_binop_none_none_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_predicate };
#define BINOP_NONE_NONE_PRED_QUALIFIERS \
  (arm_binop_none_none_pred_qualifiers)

static enum arm_type_qualifiers
arm_binop_unone_unone_pred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_predicate };
#define BINOP_UNONE_UNONE_PRED_QUALIFIERS \
  (arm_binop_unone_unone_pred_qualifiers)

/* End of Qualifier for MVE builtins.  */

   /* void ([T element type] *, T, immediate).  */
static enum arm_type_qualifiers
arm_storestruct_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_pointer_map_mode,
      qualifier_none, qualifier_struct_load_store_lane_index };
#define STORE1LANE_QUALIFIERS (arm_storestruct_lane_qualifiers)

   /* int (void).  */
static enum arm_type_qualifiers
arm_sat_occurred_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_void };
#define SAT_OCCURRED_QUALIFIERS (arm_sat_occurred_qualifiers)

   /* void (int).  */
static enum arm_type_qualifiers
arm_set_sat_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_none };
#define SET_SAT_QUALIFIERS (arm_set_sat_qualifiers)

#define v2qi_UP  E_V2QImode
#define v4bi_UP  E_V4BImode
#define v8bi_UP  E_V8BImode
#define v16bi_UP E_V16BImode
#define v8qi_UP  E_V8QImode
#define v4hi_UP  E_V4HImode
#define v4hf_UP  E_V4HFmode
#define v4bf_UP  E_V4BFmode
#define v2si_UP  E_V2SImode
#define v2sf_UP  E_V2SFmode
#define v2bf_UP  E_V2BFmode
#define di_UP    E_DImode
#define v16qi_UP E_V16QImode
#define v8hi_UP  E_V8HImode
#define v8hf_UP  E_V8HFmode
#define v8bf_UP  E_V8BFmode
#define v4si_UP  E_V4SImode
#define v4sf_UP  E_V4SFmode
#define v2di_UP  E_V2DImode
#define ti_UP	 E_TImode
#define ei_UP	 E_EImode
#define oi_UP	 E_OImode
#define hf_UP	 E_HFmode
#define bf_UP    E_BFmode
#define si_UP	 E_SImode
#define hi_UP    E_HImode
#define void_UP	 E_VOIDmode
#define sf_UP	 E_SFmode
#define UP(X) X##_UP

typedef struct {
  const char *name;
  machine_mode mode;
  const enum insn_code code;
  unsigned int fcode;
  enum arm_type_qualifiers *qualifiers;
} arm_builtin_datum;

constexpr insn_code CODE_FOR_neon_sdotv8qi = CODE_FOR_neon_sdotv2siv8qi;
constexpr insn_code CODE_FOR_neon_udotv8qi = CODE_FOR_neon_udotv2siv8qi;
constexpr insn_code CODE_FOR_neon_usdotv8qi = CODE_FOR_neon_usdotv2siv8qi;
constexpr insn_code CODE_FOR_neon_sdotv16qi = CODE_FOR_neon_sdotv4siv16qi;
constexpr insn_code CODE_FOR_neon_udotv16qi = CODE_FOR_neon_udotv4siv16qi;
constexpr insn_code CODE_FOR_neon_usdotv16qi = CODE_FOR_neon_usdotv4siv16qi;

#define CF(N,X) CODE_FOR_neon_##N##X

#define VAR1(T, N, A) \
  {#N #A, UP (A), CF (N, A), 0, T##_QUALIFIERS},
#define VAR2(T, N, A, B) \
  VAR1 (T, N, A) \
  VAR1 (T, N, B)
#define VAR3(T, N, A, B, C) \
  VAR2 (T, N, A, B) \
  VAR1 (T, N, C)
#define VAR4(T, N, A, B, C, D) \
  VAR3 (T, N, A, B, C) \
  VAR1 (T, N, D)
#define VAR5(T, N, A, B, C, D, E) \
  VAR4 (T, N, A, B, C, D) \
  VAR1 (T, N, E)
#define VAR6(T, N, A, B, C, D, E, F) \
  VAR5 (T, N, A, B, C, D, E) \
  VAR1 (T, N, F)
#define VAR7(T, N, A, B, C, D, E, F, G) \
  VAR6 (T, N, A, B, C, D, E, F) \
  VAR1 (T, N, G)
#define VAR8(T, N, A, B, C, D, E, F, G, H) \
  VAR7 (T, N, A, B, C, D, E, F, G) \
  VAR1 (T, N, H)
#define VAR9(T, N, A, B, C, D, E, F, G, H, I) \
  VAR8 (T, N, A, B, C, D, E, F, G, H) \
  VAR1 (T, N, I)
#define VAR10(T, N, A, B, C, D, E, F, G, H, I, J) \
  VAR9 (T, N, A, B, C, D, E, F, G, H, I) \
  VAR1 (T, N, J)
#define VAR11(T, N, A, B, C, D, E, F, G, H, I, J, K) \
  VAR10 (T, N, A, B, C, D, E, F, G, H, I, J) \
  VAR1 (T, N, K)
#define VAR12(T, N, A, B, C, D, E, F, G, H, I, J, K, L) \
  VAR11 (T, N, A, B, C, D, E, F, G, H, I, J, K) \
  VAR1 (T, N, L)
#define VAR13(T, N, A, B, C, D, E, F, G, H, I, J, K, L, M) \
  VAR12 (T, N, A, B, C, D, E, F, G, H, I, J, K, L) \
  VAR1 (T, N, M)
#define VAR14(T, N, A, B, C, D, E, F, G, H, I, J, K, L, M, O) \
  VAR13 (T, N, A, B, C, D, E, F, G, H, I, J, K, L, M) \
  VAR1 (T, N, O)

/* The builtin data can be found in arm_neon_builtins.def, arm_vfp_builtins.def
   and arm_acle_builtins.def.  The entries in arm_neon_builtins.def require
   TARGET_NEON to be true.  The feature tests are checked when the builtins are
   expanded.

   The mode entries in the following table correspond to the "key" type of the
   instruction variant, i.e. equivalent to that which would be specified after
   the assembler mnemonic for neon instructions, which usually refers to the
   last vector operand.  The modes listed per instruction should be the same as
   those defined for that instruction's pattern, for instance in neon.md.  */

static arm_builtin_datum vfp_builtin_data[] =
{
#include "arm_vfp_builtins.def"
};

static arm_builtin_datum neon_builtin_data[] =
{
#include "arm_neon_builtins.def"
};

#undef CF
#define CF(N,X) CODE_FOR_mve_##N##X
static arm_builtin_datum mve_builtin_data[] =
{
#include "arm_mve_builtins.def"
};

#undef CF
#undef VAR1
#define VAR1(T, N, A) \
  {#N, UP (A), CODE_FOR_arm_##N, 0, T##_QUALIFIERS},

static arm_builtin_datum acle_builtin_data[] =
{
#include "arm_acle_builtins.def"
};

#undef VAR1
/* IMM_MAX sets the maximum valid value of the CDE immediate operand.
   ECF_FLAG sets the flag used for set_call_expr_flags.  */
#define VAR1(T, N, A, IMM_MAX, ECF_FLAG) \
  {{#N #A, UP (A), CODE_FOR_arm_##N##A, 0, T##_QUALIFIERS}, IMM_MAX, ECF_FLAG},

typedef struct {
  arm_builtin_datum base;
  unsigned int imm_max;
  int ecf_flag;
} arm_builtin_cde_datum;

static arm_builtin_cde_datum cde_builtin_data[] =
{
#include "arm_cde_builtins.def"
};

#undef VAR1
#define VAR1(T, N, X) \
  ARM_BUILTIN_NEON_##N##X,

enum arm_builtins
{
  ARM_BUILTIN_GET_FPSCR,
  ARM_BUILTIN_SET_FPSCR,
  ARM_BUILTIN_GET_FPSCR_NZCVQC,
  ARM_BUILTIN_SET_FPSCR_NZCVQC,

  ARM_BUILTIN_CMSE_NONSECURE_CALLER,
  ARM_BUILTIN_SIMD_LANE_CHECK,

#undef CRYPTO1
#undef CRYPTO2
#undef CRYPTO3

#define CRYPTO1(L, U, M1, M2) \
  ARM_BUILTIN_CRYPTO_##U,
#define CRYPTO2(L, U, M1, M2, M3) \
  ARM_BUILTIN_CRYPTO_##U,
#define CRYPTO3(L, U, M1, M2, M3, M4) \
  ARM_BUILTIN_CRYPTO_##U,

  ARM_BUILTIN_CRYPTO_BASE,

#include "crypto.def"

#undef CRYPTO1
#undef CRYPTO2
#undef CRYPTO3

  ARM_BUILTIN_VFP_BASE,

#include "arm_vfp_builtins.def"

  ARM_BUILTIN_NEON_BASE,

#include "arm_neon_builtins.def"

#undef VAR1
#define VAR1(T, N, X) \
  ARM_BUILTIN_##N,

  ARM_BUILTIN_ACLE_BASE,
  ARM_BUILTIN_SAT_IMM_CHECK = ARM_BUILTIN_ACLE_BASE,

#include "arm_acle_builtins.def"

#undef VAR1
#define VAR1(T, N, X, ... ) \
  ARM_BUILTIN_##N##X,

  ARM_BUILTIN_CDE_BASE,

#include "arm_cde_builtins.def"

  ARM_BUILTIN_MVE_BASE,

#undef VAR1
#define VAR1(T, N, X) \
  ARM_BUILTIN_MVE_##N##X,
#include "arm_mve_builtins.def"

  ARM_BUILTIN_MAX
};

#define ARM_BUILTIN_VFP_PATTERN_START \
  (ARM_BUILTIN_VFP_BASE + 1)

#define ARM_BUILTIN_NEON_PATTERN_START \
  (ARM_BUILTIN_NEON_BASE + 1)

#define ARM_BUILTIN_MVE_PATTERN_START \
  (ARM_BUILTIN_MVE_BASE + 1)

#define ARM_BUILTIN_ACLE_PATTERN_START \
  (ARM_BUILTIN_ACLE_BASE + 1)

#define ARM_BUILTIN_CDE_PATTERN_START \
  (ARM_BUILTIN_CDE_BASE + 1)

#define ARM_BUILTIN_CDE_PATTERN_END \
  (ARM_BUILTIN_CDE_BASE + ARRAY_SIZE (cde_builtin_data))

#undef CF
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

static GTY(()) tree arm_builtin_decls[ARM_BUILTIN_MAX];

#define NUM_DREG_TYPES 5
#define NUM_QREG_TYPES 6

/* Internal scalar builtin types.  These types are used to support
   neon intrinsic builtins.  They are _not_ user-visible types.  Therefore
   the mangling for these types are implementation defined.  */
const char *arm_scalar_builtin_types[] = {
  "__builtin_neon_qi",
  "__builtin_neon_hi",
  "__builtin_neon_si",
  "__builtin_neon_sf",
  "__builtin_neon_di",
  "__builtin_neon_df",
  "__builtin_neon_ti",
  "__builtin_neon_uqi",
  "__builtin_neon_uhi",
  "__builtin_neon_usi",
  "__builtin_neon_udi",
  "__builtin_neon_ei",
  "__builtin_neon_oi",
  "__builtin_neon_ci",
  "__builtin_neon_xi",
  "__builtin_neon_bf",
  NULL
};

#define ENTRY(E, M, Q, S, T, G)		\
  {E,					\
   "__simd" #S "_" #T "_t",		\
   #G "__simd" #S "_" #T "_t",		\
   NULL_TREE, NULL_TREE, M##mode, qualifier_##Q},
struct arm_simd_type_info arm_simd_types [] = {
#include "arm-simd-builtin-types.def"
};
#undef ENTRY

/* The user-visible __fp16 type.  */
tree arm_fp16_type_node = NULL_TREE;

/* Back-end node type for brain float (bfloat) types.  */
tree arm_bf16_type_node = NULL_TREE;
tree arm_bf16_ptr_type_node = NULL_TREE;

static tree arm_simd_intOI_type_node = NULL_TREE;
static tree arm_simd_intEI_type_node = NULL_TREE;
static tree arm_simd_intCI_type_node = NULL_TREE;
static tree arm_simd_intXI_type_node = NULL_TREE;
static tree arm_simd_polyQI_type_node = NULL_TREE;
static tree arm_simd_polyHI_type_node = NULL_TREE;
static tree arm_simd_polyDI_type_node = NULL_TREE;
static tree arm_simd_polyTI_type_node = NULL_TREE;

/* Wrapper around add_builtin_function.  NAME is the name of the built-in
   function, TYPE is the function type, CODE is the function subcode
   (relative to ARM_BUILTIN_GENERAL), and ATTRS is the function
   attributes.  */
static tree
arm_general_add_builtin_function (const char* name, tree type,
				  unsigned int code, tree attrs = NULL_TREE)
{
  code = (code << ARM_BUILTIN_SHIFT) | ARM_BUILTIN_GENERAL;
  return add_builtin_function (name, type, code, BUILT_IN_MD, NULL, attrs);
}

static const char *
arm_mangle_builtin_scalar_type (const_tree type)
{
  int i = 0;

  while (arm_scalar_builtin_types[i] != NULL)
    {
      const char *name = arm_scalar_builtin_types[i];

      if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	  && DECL_NAME (TYPE_NAME (type))
	  && !strcmp (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))), name))
	return arm_scalar_builtin_types[i];
      i++;
    }
  return NULL;
}

static const char *
arm_mangle_builtin_vector_type (const_tree type)
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
arm_mangle_builtin_type (const_tree type)
{
  const char *mangle;
  /* Walk through all the Arm builtins types tables to filter out the
     incoming type.  */
  if ((mangle = arm_mangle_builtin_vector_type (type))
      || (mangle = arm_mangle_builtin_scalar_type (type)))
    return mangle;

  return NULL;
}

static tree
arm_simd_builtin_std_type (machine_mode mode,
			   enum arm_type_qualifiers q)
{
#define QUAL_TYPE(M)  \
  ((q == qualifier_none) ? int##M##_type_node : unsigned_int##M##_type_node);
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
      return arm_simd_intOI_type_node;
    case E_EImode:
      return arm_simd_intEI_type_node;
    case E_CImode:
      return arm_simd_intCI_type_node;
    case E_XImode:
      return arm_simd_intXI_type_node;
    case E_HFmode:
      return arm_fp16_type_node;
    case E_SFmode:
      return float_type_node;
    case E_DFmode:
      return double_type_node;
    case E_BFmode:
      return arm_bf16_type_node;
    default:
      gcc_unreachable ();
    }
#undef QUAL_TYPE
}

static tree
arm_lookup_simd_builtin_type (machine_mode mode,
			      enum arm_type_qualifiers q)
{
  int i;
  int nelts = ARRAY_SIZE (arm_simd_types);

  /* Non-poly scalar modes map to standard types not in the table.  */
  if (q != qualifier_poly && !VECTOR_MODE_P (mode))
    return arm_simd_builtin_std_type (mode, q);

  for (i = 0; i < nelts; i++)
    if (arm_simd_types[i].mode == mode
	&& arm_simd_types[i].q == q)
      return arm_simd_types[i].itype;

  /* Note that we won't have caught the underlying type for poly64x2_t
     in the above table.  This gets default mangling.  */

  return NULL_TREE;
}

static tree
arm_simd_builtin_type (machine_mode mode, arm_type_qualifiers qualifiers)
{
  if ((qualifiers & qualifier_poly) != 0)
    return arm_lookup_simd_builtin_type (mode, qualifier_poly);
  else if ((qualifiers & qualifier_unsigned) != 0)
    return arm_lookup_simd_builtin_type (mode, qualifier_unsigned);
  else if ((qualifiers & qualifier_predicate) != 0)
    return unsigned_intHI_type_node;
  else
    return arm_lookup_simd_builtin_type (mode, qualifier_none);
}

static void
arm_init_simd_builtin_types (void)
{
  int i;
  int nelts = ARRAY_SIZE (arm_simd_types);
  tree tdecl;

  /* Poly types are a world of their own.  In order to maintain legacy
     ABI, they get initialized using the old interface, and don't get
     an entry in our mangling table, consequently, they get default
     mangling.  As a further gotcha, poly8_t and poly16_t are signed
     types, poly64_t and poly128_t are unsigned types.  */
  if (!TARGET_HAVE_MVE)
    {
      arm_simd_polyQI_type_node
	= build_distinct_type_copy (intQI_type_node);
      (*lang_hooks.types.register_builtin_type) (arm_simd_polyQI_type_node,
						 "__builtin_neon_poly8");
      arm_simd_polyHI_type_node
	= build_distinct_type_copy (intHI_type_node);
      (*lang_hooks.types.register_builtin_type) (arm_simd_polyHI_type_node,
						 "__builtin_neon_poly16");
      arm_simd_polyDI_type_node
	= build_distinct_type_copy (unsigned_intDI_type_node);
      (*lang_hooks.types.register_builtin_type) (arm_simd_polyDI_type_node,
						 "__builtin_neon_poly64");
      arm_simd_polyTI_type_node
	= build_distinct_type_copy (unsigned_intTI_type_node);
      (*lang_hooks.types.register_builtin_type) (arm_simd_polyTI_type_node,
						 "__builtin_neon_poly128");
      /* Init poly vector element types with scalar poly types.  */
      arm_simd_types[Poly8x8_t].eltype = arm_simd_polyQI_type_node;
      arm_simd_types[Poly8x16_t].eltype = arm_simd_polyQI_type_node;
      arm_simd_types[Poly16x4_t].eltype = arm_simd_polyHI_type_node;
      arm_simd_types[Poly16x8_t].eltype = arm_simd_polyHI_type_node;
      /* Note: poly64x2_t is defined in arm_neon.h, to ensure it gets default
	 mangling.  */

      /* Prevent front-ends from transforming poly vectors into string
	 literals.  */
      TYPE_STRING_FLAG (arm_simd_polyQI_type_node) = false;
      TYPE_STRING_FLAG (arm_simd_polyHI_type_node) = false;
    }
  /* Init all the element types built by the front-end.  */
  arm_simd_types[Int8x8_t].eltype = get_typenode_from_name (INT8_TYPE);
  arm_simd_types[Int8x16_t].eltype = get_typenode_from_name (INT8_TYPE);
  arm_simd_types[Int16x4_t].eltype = get_typenode_from_name (INT16_TYPE);
  arm_simd_types[Int16x8_t].eltype = get_typenode_from_name (INT16_TYPE);
  arm_simd_types[Int32x2_t].eltype = get_typenode_from_name (INT32_TYPE);
  arm_simd_types[Int32x4_t].eltype = get_typenode_from_name (INT32_TYPE);
  arm_simd_types[Int64x2_t].eltype = get_typenode_from_name (INT64_TYPE);
  arm_simd_types[Uint8x8_t].eltype = get_typenode_from_name (UINT8_TYPE);
  arm_simd_types[Uint8x16_t].eltype = get_typenode_from_name (UINT8_TYPE);
  arm_simd_types[Uint16x4_t].eltype = get_typenode_from_name (UINT16_TYPE);
  arm_simd_types[Uint16x8_t].eltype = get_typenode_from_name (UINT16_TYPE);
  arm_simd_types[Uint32x2_t].eltype = get_typenode_from_name (UINT32_TYPE);
  arm_simd_types[Uint32x4_t].eltype = get_typenode_from_name (UINT32_TYPE);
  arm_simd_types[Uint64x2_t].eltype = get_typenode_from_name (UINT64_TYPE);

  /* Note: poly64x2_t is defined in arm_neon.h, to ensure it gets default
     mangling.  */

  /* Continue with standard types.  */
  /* The __builtin_simd{64,128}_float16 types are kept private unless
     we have a scalar __fp16 type.  */
  arm_simd_types[Float16x4_t].eltype = arm_fp16_type_node;
  arm_simd_types[Float16x8_t].eltype = arm_fp16_type_node;
  arm_simd_types[Float32x2_t].eltype = float_type_node;
  arm_simd_types[Float32x4_t].eltype = float_type_node;

  /* Init Bfloat vector types with underlying __bf16 scalar type.  */
  arm_simd_types[Bfloat16x2_t].eltype = arm_bf16_type_node;
  arm_simd_types[Bfloat16x4_t].eltype = arm_bf16_type_node;
  arm_simd_types[Bfloat16x8_t].eltype = arm_bf16_type_node;

  for (i = 0; i < nelts; i++)
    {
      tree eltype = arm_simd_types[i].eltype;
      machine_mode mode = arm_simd_types[i].mode;

      if (eltype == NULL
	  /* VECTOR_BOOL is not supported unless MVE is activated,
	     this would make build_truth_vector_type_for_mode
	     crash.  */
	  && ((GET_MODE_CLASS (mode) != MODE_VECTOR_BOOL)
	      || !TARGET_HAVE_MVE))
	continue;
      if (arm_simd_types[i].itype == NULL)
	{
	  tree type;
	  if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
	    {
	      /* Handle MVE predicates: they are internally stored as
		 16 bits, but are used as vectors of 1, 2 or 4-bit
		 elements.  */
	      type = build_truth_vector_type_for_mode (GET_MODE_NUNITS (mode),
						       mode);
	      eltype = TREE_TYPE (type);
	    }
	  else
	    type = build_vector_type (eltype, GET_MODE_NUNITS (mode));

	  type = build_distinct_type_copy (type);
	  SET_TYPE_STRUCTURAL_EQUALITY (type);

	  tree mangled_name = get_identifier (arm_simd_types[i].mangle);
	  tree value = tree_cons (NULL_TREE, mangled_name, NULL_TREE);
	  TYPE_ATTRIBUTES (type)
	    = tree_cons (get_identifier ("Advanced SIMD type"), value,
			 TYPE_ATTRIBUTES (type));
	  arm_simd_types[i].itype = type;
	}

      tdecl = add_builtin_type (arm_simd_types[i].name,
				arm_simd_types[i].itype);
      TYPE_NAME (arm_simd_types[i].itype) = tdecl;
      SET_TYPE_STRUCTURAL_EQUALITY (arm_simd_types[i].itype);
    }

#define AARCH_BUILD_SIGNED_TYPE(mode)  \
  make_signed_type (GET_MODE_PRECISION (mode));
  arm_simd_intOI_type_node = AARCH_BUILD_SIGNED_TYPE (OImode);
  arm_simd_intEI_type_node = AARCH_BUILD_SIGNED_TYPE (EImode);
  arm_simd_intCI_type_node = AARCH_BUILD_SIGNED_TYPE (CImode);
  arm_simd_intXI_type_node = AARCH_BUILD_SIGNED_TYPE (XImode);
#undef AARCH_BUILD_SIGNED_TYPE

  tdecl = add_builtin_type
	    ("__builtin_neon_ei" , arm_simd_intEI_type_node);
  TYPE_NAME (arm_simd_intEI_type_node) = tdecl;
  tdecl = add_builtin_type
	    ("__builtin_neon_oi" , arm_simd_intOI_type_node);
  TYPE_NAME (arm_simd_intOI_type_node) = tdecl;
  tdecl = add_builtin_type
	    ("__builtin_neon_ci" , arm_simd_intCI_type_node);
  TYPE_NAME (arm_simd_intCI_type_node) = tdecl;
  tdecl = add_builtin_type
	    ("__builtin_neon_xi" , arm_simd_intXI_type_node);
  TYPE_NAME (arm_simd_intXI_type_node) = tdecl;
}

static void
arm_init_simd_builtin_scalar_types (void)
{
  /* Define typedefs for all the standard scalar types.  */
  (*lang_hooks.types.register_builtin_type) (intQI_type_node,
					     "__builtin_neon_qi");
  (*lang_hooks.types.register_builtin_type) (intHI_type_node,
					     "__builtin_neon_hi");
  (*lang_hooks.types.register_builtin_type) (intSI_type_node,
					     "__builtin_neon_si");
  (*lang_hooks.types.register_builtin_type) (float_type_node,
					     "__builtin_neon_sf");
  (*lang_hooks.types.register_builtin_type) (intDI_type_node,
					     "__builtin_neon_di");
  (*lang_hooks.types.register_builtin_type) (double_type_node,
					     "__builtin_neon_df");
  (*lang_hooks.types.register_builtin_type) (intTI_type_node,
					     "__builtin_neon_ti");
  (*lang_hooks.types.register_builtin_type) (arm_bf16_type_node,
                                             "__builtin_neon_bf");
  /* Unsigned integer types for various mode sizes.  */
  (*lang_hooks.types.register_builtin_type) (unsigned_intQI_type_node,
					     "__builtin_neon_uqi");
  (*lang_hooks.types.register_builtin_type) (unsigned_intHI_type_node,
					     "__builtin_neon_uhi");
  (*lang_hooks.types.register_builtin_type) (unsigned_intSI_type_node,
					     "__builtin_neon_usi");
  (*lang_hooks.types.register_builtin_type) (unsigned_intDI_type_node,
					     "__builtin_neon_udi");
  (*lang_hooks.types.register_builtin_type) (unsigned_intTI_type_node,
					     "__builtin_neon_uti");
}

/* Set up a builtin.  It will use information stored in the argument struct D to
   derive the builtin's type signature and name.  It will append the name in D
   to the PREFIX passed and use these to create a builtin declaration that is
   then stored in 'arm_builtin_decls' under index FCODE.  This FCODE is also
   written back to D for future use.  */

static void
arm_init_builtin (unsigned int fcode, arm_builtin_datum *d,
		  const char * prefix)
{
  bool print_type_signature_p = false;
  char type_signature[SIMD_MAX_BUILTIN_ARGS] = { 0 };
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

  /* Build a function type directly from the insn_data for this
     builtin.  The build_function_type () function takes care of
     removing duplicates for us.  */
  for (; op_num >= 0; arg_num--, op_num--)
    {
      machine_mode op_mode = insn_data[d->code].operand[op_num].mode;
      enum arm_type_qualifiers qualifiers = d->qualifiers[arg_num];

      if (qualifiers & qualifier_unsigned)
	{
	  type_signature[arg_num] = 'u';
	  print_type_signature_p = true;
	}
      else if (qualifiers & qualifier_poly)
	{
	  type_signature[arg_num] = 'p';
	  print_type_signature_p = true;
	}
      else
	type_signature[arg_num] = 's';

      /* Skip an internal operand for vget_{low, high}.  */
      if (qualifiers & qualifier_internal)
	continue;

      /* Some builtins have different user-facing types
	 for certain arguments, encoded in d->mode.  */
      if (qualifiers & qualifier_map_mode)
	op_mode = d->mode;

      /* MVE Predicates use HImode as mandated by the ABI: pred16_t is
	 unsigned short.  */
      if (qualifiers & qualifier_predicate)
	op_mode = HImode;

      /* For pointers, we want a pointer to the basic type
	 of the vector.  */
      if (qualifiers & qualifier_pointer && VECTOR_MODE_P (op_mode))
	op_mode = GET_MODE_INNER (op_mode);

      /* For void pointers we already have nodes constructed by the midend.  */
      if (qualifiers & qualifier_void_pointer)
	eltype = qualifiers & qualifier_const
		 ? const_ptr_type_node : ptr_type_node;
      else
	{
	  eltype
	    = arm_simd_builtin_type (op_mode, qualifiers);
	  gcc_assert (eltype != NULL);

	  /* Add qualifiers.  */
	  if (qualifiers & qualifier_const)
	    eltype = build_qualified_type (eltype, TYPE_QUAL_CONST);

	  if (qualifiers & qualifier_pointer)
	    eltype = build_pointer_type (eltype);
	}
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

  if (print_type_signature_p
      && IN_RANGE (fcode, ARM_BUILTIN_VFP_BASE, ARM_BUILTIN_ACLE_BASE - 1))
    snprintf (namebuf, sizeof (namebuf), "%s_%s_%s",
	      prefix, d->name, type_signature);
  else
    snprintf (namebuf, sizeof (namebuf), "%s_%s",
	      prefix, d->name);

  fndecl = arm_general_add_builtin_function (namebuf, ftype, fcode);
  arm_builtin_decls[fcode] = fndecl;
}

/* Initialize the backend REAL_TYPE type supporting bfloat types.  */
static void
arm_init_bf16_types (void)
{
  arm_bf16_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (arm_bf16_type_node) = 16;
  SET_TYPE_MODE (arm_bf16_type_node, BFmode);
  layout_type (arm_bf16_type_node);

  lang_hooks.types.register_builtin_type (arm_bf16_type_node, "__bf16");
  arm_bf16_ptr_type_node = build_pointer_type (arm_bf16_type_node);
}

/* Set up ACLE builtins, even builtins for instructions that are not
   in the current target ISA to allow the user to compile particular modules
   with different target specific options that differ from the command line
   options.  Such builtins will be rejected in arm_general_expand_builtin.  */

static void
arm_init_acle_builtins (void)
{
  unsigned int i, fcode = ARM_BUILTIN_ACLE_PATTERN_START;

  tree sat_check_fpr = build_function_type_list (void_type_node,
						 intSI_type_node,
						 intSI_type_node,
						 intSI_type_node,
						 NULL);
  arm_builtin_decls[ARM_BUILTIN_SAT_IMM_CHECK]
    = arm_general_add_builtin_function ("__builtin_sat_imm_check",
					sat_check_fpr,
					ARM_BUILTIN_SAT_IMM_CHECK);

  for (i = 0; i < ARRAY_SIZE (acle_builtin_data); i++, fcode++)
    {
      arm_builtin_datum *d = &acle_builtin_data[i];
      arm_init_builtin (fcode, d, "__builtin_arm");
    }
}

static void
arm_init_cde_builtins (void)
{
  unsigned int i, fcode = ARM_BUILTIN_CDE_PATTERN_START;
  for (i = 0; i < ARRAY_SIZE (cde_builtin_data); i++, fcode++)
    {
      /* Only define CDE floating point builtins if the target has floating
	 point registers.  NOTE: without HARD_FLOAT we don't have MVE, so we
	 can break out of this loop directly here.  */
      if (!TARGET_MAYBE_HARD_FLOAT && fcode >= ARM_BUILTIN_vcx1si)
	break;
      /* Only define CDE/MVE builtins if MVE is available.  */
      if (!TARGET_HAVE_MVE && fcode >= ARM_BUILTIN_vcx1qv16qi)
	break;
      arm_builtin_cde_datum *cde = &cde_builtin_data[i];
      arm_builtin_datum *d = &cde->base;
      arm_init_builtin (fcode, d, "__builtin_arm");
      set_call_expr_flags (arm_builtin_decls[fcode], cde->ecf_flag);
    }
}

/* Set up all the MVE builtins mentioned in arm_mve_builtins.def file.  */
static void
arm_init_mve_builtins (void)
{
  volatile unsigned int i, fcode = ARM_BUILTIN_MVE_PATTERN_START;

  arm_init_simd_builtin_scalar_types ();
  arm_init_simd_builtin_types ();

  /* Add support for __builtin_{get,set}_fpscr_nzcvqc, used by MVE intrinsics
     that read and/or write the carry bit.  */
  tree get_fpscr_nzcvqc = build_function_type_list (intSI_type_node,
						    NULL);
  tree set_fpscr_nzcvqc = build_function_type_list (void_type_node,
						    intSI_type_node,
						    NULL);
  arm_builtin_decls[ARM_BUILTIN_GET_FPSCR_NZCVQC]
    = arm_general_add_builtin_function ("__builtin_arm_get_fpscr_nzcvqc",
					get_fpscr_nzcvqc,
					ARM_BUILTIN_GET_FPSCR_NZCVQC);
  arm_builtin_decls[ARM_BUILTIN_SET_FPSCR_NZCVQC]
    = arm_general_add_builtin_function ("__builtin_arm_set_fpscr_nzcvqc",
					set_fpscr_nzcvqc,
					ARM_BUILTIN_SET_FPSCR_NZCVQC);

  for (i = 0; i < ARRAY_SIZE (mve_builtin_data); i++, fcode++)
    {
      arm_builtin_datum *d = &mve_builtin_data[i];
      arm_init_builtin (fcode, d, "__builtin_mve");
    }

  if (in_lto_p)
    {
      arm_mve::handle_arm_mve_types_h ();
      /* Under LTO, we cannot know whether
	 __ARM_MVE_PRESERVE_USER_NAMESPACE was defined, so assume it
	 was not.  */
      arm_mve::handle_arm_mve_h (false);
    }
}

/* Set up all the NEON builtins, even builtins for instructions that are not
   in the current target ISA to allow the user to compile particular modules
   with different target specific options that differ from the command line
   options.  Such builtins will be rejected in arm_general_expand_builtin.  */

static void
arm_init_neon_builtins (void)
{
  unsigned int i, fcode = ARM_BUILTIN_NEON_PATTERN_START;

  arm_init_simd_builtin_types ();

  /* Strong-typing hasn't been implemented for all AdvSIMD builtin intrinsics.
     Therefore we need to preserve the old __builtin scalar types.  It can be
     removed once all the intrinsics become strongly typed using the qualifier
     system.  */
  arm_init_simd_builtin_scalar_types ();

  for (i = 0; i < ARRAY_SIZE (neon_builtin_data); i++, fcode++)
    {
      arm_builtin_datum *d = &neon_builtin_data[i];
      arm_init_builtin (fcode, d, "__builtin_neon");
    }
}

/* Set up all the scalar floating point builtins.  */

static void
arm_init_vfp_builtins (void)
{
  unsigned int i, fcode = ARM_BUILTIN_VFP_PATTERN_START;

  for (i = 0; i < ARRAY_SIZE (vfp_builtin_data); i++, fcode++)
    {
      arm_builtin_datum *d = &vfp_builtin_data[i];
      arm_init_builtin (fcode, d, "__builtin_neon");
    }
}

static void
arm_init_crypto_builtins (void)
{
  tree V16UQI_type_node
    = arm_simd_builtin_type (V16QImode, qualifier_unsigned);

  tree V4USI_type_node
    = arm_simd_builtin_type (V4SImode, qualifier_unsigned);

  tree v16uqi_ftype_v16uqi
    = build_function_type_list (V16UQI_type_node, V16UQI_type_node,
				NULL_TREE);

  tree v16uqi_ftype_v16uqi_v16uqi
	= build_function_type_list (V16UQI_type_node, V16UQI_type_node,
				    V16UQI_type_node, NULL_TREE);

  tree v4usi_ftype_v4usi
    = build_function_type_list (V4USI_type_node, V4USI_type_node,
				NULL_TREE);

  tree v4usi_ftype_v4usi_v4usi
    = build_function_type_list (V4USI_type_node, V4USI_type_node,
				V4USI_type_node, NULL_TREE);

  tree v4usi_ftype_v4usi_v4usi_v4usi
    = build_function_type_list (V4USI_type_node, V4USI_type_node,
				V4USI_type_node, V4USI_type_node,
				NULL_TREE);

  tree uti_ftype_udi_udi
    = build_function_type_list (unsigned_intTI_type_node,
				unsigned_intDI_type_node,
				unsigned_intDI_type_node,
				NULL_TREE);

  #undef CRYPTO1
  #undef CRYPTO2
  #undef CRYPTO3
  #undef C
  #undef N
  #undef CF
  #undef FT1
  #undef FT2
  #undef FT3

  #define C(U) \
    ARM_BUILTIN_CRYPTO_##U
  #define N(L) \
    "__builtin_arm_crypto_"#L
  #define FT1(R, A) \
    R##_ftype_##A
  #define FT2(R, A1, A2) \
    R##_ftype_##A1##_##A2
  #define FT3(R, A1, A2, A3) \
    R##_ftype_##A1##_##A2##_##A3
  #define CRYPTO1(L, U, R, A) \
    arm_builtin_decls[C (U)] \
      = arm_general_add_builtin_function (N (L), FT1 (R, A), C (U));
  #define CRYPTO2(L, U, R, A1, A2)  \
    arm_builtin_decls[C (U)]	\
      = arm_general_add_builtin_function (N (L), FT2 (R, A1, A2), C (U));

  #define CRYPTO3(L, U, R, A1, A2, A3) \
    arm_builtin_decls[C (U)]	   \
      = arm_general_add_builtin_function (N (L), FT3 (R, A1, A2, A3), C (U));
  #include "crypto.def"

  #undef CRYPTO1
  #undef CRYPTO2
  #undef CRYPTO3
  #undef C
  #undef N
  #undef FT1
  #undef FT2
  #undef FT3
}

#undef NUM_DREG_TYPES
#undef NUM_QREG_TYPES

#define def_mbuiltin(FLAG, NAME, TYPE, CODE)				\
  do									\
    {									\
      if (FLAG == isa_nobit						\
	  || bitmap_bit_p (arm_active_target.isa, FLAG))		\
	{								\
	  tree bdecl;							\
	  bdecl  = arm_general_add_builtin_function ((NAME), (TYPE),    \
						     (CODE));		\
	  arm_builtin_decls[CODE] = bdecl;				\
	}								\
    }									\
  while (0)

struct builtin_description
{
  const enum isa_feature   feature;
  const enum insn_code     icode;
  const char * const       name;
  const enum arm_builtins  code;
  const enum rtx_code      comparison;
  const unsigned int       flag;
};

static const struct builtin_description bdesc_2arg[] =
{
#define FP_BUILTIN(L, U) \
  {isa_nobit, CODE_FOR_##L, "__builtin_arm_"#L, ARM_BUILTIN_##U, \
   UNKNOWN, 0},

  FP_BUILTIN (get_fpscr, GET_FPSCR)
  FP_BUILTIN (set_fpscr, SET_FPSCR)
#undef FP_BUILTIN

#define CRYPTO_BUILTIN(L, U)					   \
  {isa_nobit, CODE_FOR_crypto_##L,	"__builtin_arm_crypto_"#L, \
   ARM_BUILTIN_CRYPTO_##U, UNKNOWN, 0},
#undef CRYPTO1
#undef CRYPTO2
#undef CRYPTO3
#define CRYPTO2(L, U, R, A1, A2) CRYPTO_BUILTIN (L, U)
#define CRYPTO1(L, U, R, A)
#define CRYPTO3(L, U, R, A1, A2, A3)
#include "crypto.def"
#undef CRYPTO1
#undef CRYPTO2
#undef CRYPTO3

};

static const struct builtin_description bdesc_1arg[] =
{
#define CRYPTO1(L, U, R, A) CRYPTO_BUILTIN (L, U)
#define CRYPTO2(L, U, R, A1, A2)
#define CRYPTO3(L, U, R, A1, A2, A3)
#include "crypto.def"
#undef CRYPTO1
#undef CRYPTO2
#undef CRYPTO3
};

static const struct builtin_description bdesc_3arg[] =
{
#define CRYPTO3(L, U, R, A1, A2, A3) CRYPTO_BUILTIN (L, U)
#define CRYPTO1(L, U, R, A)
#define CRYPTO2(L, U, R, A1, A2)
#include "crypto.def"
#undef CRYPTO1
#undef CRYPTO2
#undef CRYPTO3
 };
#undef CRYPTO_BUILTIN

static void
arm_init_fp16_builtins (void)
{
  arm_fp16_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (arm_fp16_type_node) = GET_MODE_PRECISION (HFmode);
  layout_type (arm_fp16_type_node);
  if (arm_fp16_format)
    (*lang_hooks.types.register_builtin_type) (arm_fp16_type_node,
					       "__fp16");
}

void
arm_init_builtins (void)
{
  /* This creates the arm_simd_floatHF_type_node so must come before
     arm_init_neon_builtins which uses it.  */
  arm_init_fp16_builtins ();

  arm_init_bf16_types ();

  if (TARGET_MAYBE_HARD_FLOAT)
    {
      tree lane_check_fpr = build_function_type_list (void_type_node,
						      intSI_type_node,
						      intSI_type_node,
						      NULL);
      arm_builtin_decls[ARM_BUILTIN_SIMD_LANE_CHECK]
      = arm_general_add_builtin_function ("__builtin_arm_lane_check",
					  lane_check_fpr,
					  ARM_BUILTIN_SIMD_LANE_CHECK);
      if (TARGET_HAVE_MVE)
	arm_init_mve_builtins ();
      else
	arm_init_neon_builtins ();
      arm_init_vfp_builtins ();
      arm_init_crypto_builtins ();
    }

  if (TARGET_CDE)
    arm_init_cde_builtins ();

  arm_init_acle_builtins ();

  if (TARGET_MAYBE_HARD_FLOAT)
    {
      tree ftype_set_fpscr
	= build_function_type_list (void_type_node, unsigned_type_node, NULL);
      tree ftype_get_fpscr
	= build_function_type_list (unsigned_type_node, NULL);

      arm_builtin_decls[ARM_BUILTIN_GET_FPSCR]
	= arm_general_add_builtin_function ("__builtin_arm_get_fpscr",
					    ftype_get_fpscr,
					    ARM_BUILTIN_GET_FPSCR);
      arm_builtin_decls[ARM_BUILTIN_SET_FPSCR]
	= arm_general_add_builtin_function ("__builtin_arm_set_fpscr",
					    ftype_set_fpscr,
					    ARM_BUILTIN_SET_FPSCR);
    }

  if (use_cmse)
    {
      tree ftype_cmse_nonsecure_caller
	= build_function_type_list (unsigned_type_node, NULL);
      arm_builtin_decls[ARM_BUILTIN_CMSE_NONSECURE_CALLER]
	= arm_general_add_builtin_function ("__builtin_arm_cmse_nonsecure_caller",
					    ftype_cmse_nonsecure_caller,
					    ARM_BUILTIN_CMSE_NONSECURE_CALLER);
    }
}

/* Implement TARGET_BUILTIN_DECL for general builtins.  */
tree
arm_general_builtin_decl (unsigned code)
{
  if (code >= ARM_BUILTIN_MAX)
    return error_mark_node;

  return arm_builtin_decls[code];
}

/* Implement TARGET_BUILTIN_DECL.  */
/* Return the ARM builtin for CODE.  */
tree
arm_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  unsigned subcode = code >> ARM_BUILTIN_SHIFT;
  switch (code & ARM_BUILTIN_CLASS)
    {
    case ARM_BUILTIN_GENERAL:
      return arm_general_builtin_decl (subcode);
    case ARM_BUILTIN_MVE:
      return arm_mve::builtin_decl (subcode);
    default:
      gcc_unreachable ();
    }
}

/* Errors in the source file can cause expand_expr to return const0_rtx
   where we expect a vector.  To avoid crashing, use one of the vector
   clear instructions.  */

static rtx
safe_vector_operand (rtx x, machine_mode mode ATTRIBUTE_UNUSED)
{
  if (x != const0_rtx)
    return x;
  __builtin_unreachable ();
}

/* Function to expand ternary builtins.  */
static rtx
arm_expand_ternop_builtin (enum insn_code icode,
                           tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);

  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);

  machine_mode tmode = insn_data[icode].operand[0].mode;
  machine_mode mode0 = insn_data[icode].operand[1].mode;
  machine_mode mode1 = insn_data[icode].operand[2].mode;
  machine_mode mode2 = insn_data[icode].operand[3].mode;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);
  if (VECTOR_MODE_P (mode2))
    op2 = safe_vector_operand (op2, mode2);

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  gcc_assert ((GET_MODE (op0) == mode0 || GET_MODE (op0) == VOIDmode)
	      && (GET_MODE (op1) == mode1 || GET_MODE (op1) == VOIDmode)
	      && (GET_MODE (op2) == mode2 || GET_MODE (op2) == VOIDmode));

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);
  if (! (*insn_data[icode].operand[3].predicate) (op2, mode2))
    op2 = copy_to_mode_reg (mode2, op2);

  pat = GEN_FCN (icode) (target, op0, op1, op2);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of arm_general_expand_builtin to take care of binop insns.  */

static rtx
arm_expand_binop_builtin (enum insn_code icode,
			  tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  machine_mode tmode = insn_data[icode].operand[0].mode;
  machine_mode mode0 = insn_data[icode].operand[1].mode;
  machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

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
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of arm_general_expand_builtin to take care of unop insns.  */

static rtx
arm_expand_unop_builtin (enum insn_code icode,
			 tree exp, rtx target, int do_load)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op0 = expand_normal (arg0);
  machine_mode tmode = insn_data[icode].operand[0].mode;
  machine_mode mode0 = insn_data[icode].operand[1].mode;

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);
  if (do_load)
    op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
  else
    {
      if (VECTOR_MODE_P (mode0))
	op0 = safe_vector_operand (op0, mode0);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
    }

  pat = GEN_FCN (icode) (target, op0);

  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

typedef enum {
  ARG_BUILTIN_COPY_TO_REG,
  ARG_BUILTIN_CONSTANT,
  ARG_BUILTIN_LANE_INDEX,
  ARG_BUILTIN_STRUCT_LOAD_STORE_LANE_INDEX,
  ARG_BUILTIN_LANE_PAIR_INDEX,
  ARG_BUILTIN_LANE_QUADTUP_INDEX,
  ARG_BUILTIN_NEON_MEMORY,
  ARG_BUILTIN_MEMORY,
  ARG_BUILTIN_STOP
} builtin_arg;


/* EXP is a pointer argument to a Neon load or store intrinsic.  Derive
   and return an expression for the accessed memory.

   The intrinsic function operates on a block of registers that has
   mode REG_MODE.  This block contains vectors of type TYPE_MODE.  The
   function references the memory at EXP of type TYPE and in mode
   MEM_MODE; this mode may be BLKmode if no more suitable mode is
   available.  */

static tree
neon_dereference_pointer (tree exp, tree type, machine_mode mem_mode,
			  machine_mode reg_mode,
			  machine_mode vector_mode)
{
  HOST_WIDE_INT reg_size, vector_size, nvectors, nelems;
  tree elem_type, upper_bound, array_type;

  /* Work out the size of the register block in bytes.  */
  reg_size = GET_MODE_SIZE (reg_mode);

  /* Work out the size of each vector in bytes.  */
  vector_size = GET_MODE_SIZE (vector_mode);

  /* Work out how many vectors there are.  */
  gcc_assert (reg_size % vector_size == 0);
  nvectors = reg_size / vector_size;

  /* Work out the type of each element.  */
  gcc_assert (POINTER_TYPE_P (type));
  elem_type = TREE_TYPE (type);

  /* Work out how many elements are being loaded or stored.
     MEM_MODE == REG_MODE implies a one-to-one mapping between register
     and memory elements; anything else implies a lane load or store.  */
  if (mem_mode == reg_mode)
    nelems = vector_size * nvectors / int_size_in_bytes (elem_type);
  else
    nelems = nvectors;

  /* Create a type that describes the full access.  */
  upper_bound = build_int_cst (size_type_node, nelems - 1);
  array_type = build_array_type (elem_type, build_index_type (upper_bound));

  /* Dereference EXP using that type.  */
  return fold_build2 (MEM_REF, array_type, exp,
		      build_int_cst (build_pointer_type (array_type), 0));
}

/* EXP is a pointer argument to a vector scatter store intrinsics.

   Consider the following example:
	VSTRW<v>.<dt> Qd, [Qm{, #+/-<imm>}]!
   When <Qm> used as the base register for the target address,
   this function is used to derive and return an expression for the
   accessed memory.

   The intrinsic function operates on a block of registers that has mode
   REG_MODE.  This block contains vectors of type TYPE_MODE.  The function
   references the memory at EXP of type TYPE and in mode MEM_MODE.  This
   mode may be BLKmode if no more suitable mode is available.  */

static tree
mve_dereference_pointer (tree exp, tree type, machine_mode reg_mode,
			 machine_mode vector_mode)
{
  HOST_WIDE_INT reg_size, vector_size, nelems;
  tree elem_type, upper_bound, array_type;

  /* Work out the size of each vector in bytes.  */
  vector_size = GET_MODE_SIZE (vector_mode);

  /* Work out the size of the register block in bytes.  */
  reg_size = GET_MODE_SIZE (reg_mode);

  /* Work out the type of each element.  */
  gcc_assert (POINTER_TYPE_P (type));
  elem_type = TREE_TYPE (type);

  nelems = reg_size / vector_size;

  /* Create a type that describes the full access.  */
  upper_bound = build_int_cst (size_type_node, nelems - 1);
  array_type = build_array_type (elem_type, build_index_type (upper_bound));

  /* Dereference EXP using that type.  */
  return fold_build2 (MEM_REF, array_type, exp,
		      build_int_cst (build_pointer_type (array_type), 0));
}

/* Implement TARGET_EXPAND_BUILTIN for general builtins.  */
static rtx
arm_general_expand_builtin_args (rtx target, machine_mode map_mode, int fcode,
				 int icode, int have_retval, tree exp,
				 builtin_arg *args)
{
  rtx pat;
  tree arg[SIMD_MAX_BUILTIN_ARGS];
  rtx op[SIMD_MAX_BUILTIN_ARGS];
  machine_mode tmode = insn_data[icode].operand[0].mode;
  machine_mode mode[SIMD_MAX_BUILTIN_ARGS];
  tree formals;
  int argc = 0;
  rtx_insn * insn;

  if (have_retval
      && (!target
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode)))
    target = gen_reg_rtx (tmode);

  formals = TYPE_ARG_TYPES (TREE_TYPE (arm_builtin_decls[fcode]));

  for (;;)
    {
      builtin_arg thisarg = args[argc];

      if (thisarg == ARG_BUILTIN_STOP)
	break;
      else
	{
	  int opno = argc + have_retval;
	  arg[argc] = CALL_EXPR_ARG (exp, argc);
	  mode[argc] = insn_data[icode].operand[opno].mode;
	  if (thisarg == ARG_BUILTIN_NEON_MEMORY)
            {
              machine_mode other_mode
		= insn_data[icode].operand[1 - opno].mode;
	      if (TARGET_HAVE_MVE && mode[argc] != other_mode)
		{
		  arg[argc] = mve_dereference_pointer (arg[argc],
						    TREE_VALUE (formals),
						    other_mode, map_mode);
		}
	      else
		arg[argc] = neon_dereference_pointer (arg[argc],
						      TREE_VALUE (formals),
						      mode[argc], other_mode,
						      map_mode);
            }

	  /* Use EXPAND_MEMORY for ARG_BUILTIN_MEMORY and
	     ARG_BUILTIN_NEON_MEMORY to ensure a MEM_P be returned.  */
	  op[argc] = expand_expr (arg[argc], NULL_RTX, VOIDmode,
				  ((thisarg == ARG_BUILTIN_MEMORY
				    || thisarg == ARG_BUILTIN_NEON_MEMORY)
				   ? EXPAND_MEMORY : EXPAND_NORMAL));

	  switch (thisarg)
	    {
	    case ARG_BUILTIN_MEMORY:
	    case ARG_BUILTIN_COPY_TO_REG:
	      if (POINTER_TYPE_P (TREE_TYPE (arg[argc])))
		op[argc] = convert_memory_address (Pmode, op[argc]);

	      /* MVE uses mve_pred16_t (aka HImode) for vectors of
		 predicates, but internally we use V16BI/V8BI/V4BI/V2QI for
		 MVE predicate modes.  */
	      if (TARGET_HAVE_MVE && VALID_MVE_PRED_MODE (mode[argc]))
		op[argc] = gen_lowpart (mode[argc], op[argc]);

	      gcc_assert (GET_MODE (op[argc]) == mode[argc]
			  || (GET_MODE(op[argc]) == E_VOIDmode
			      && CONSTANT_P (op[argc])));
	      if (!(*insn_data[icode].operand[opno].predicate)
		  (op[argc], mode[argc]))
		op[argc] = copy_to_mode_reg (mode[argc], op[argc]);
	      break;

	    case ARG_BUILTIN_STRUCT_LOAD_STORE_LANE_INDEX:
	      gcc_assert (argc > 1);
	      if (CONST_INT_P (op[argc]))
		{
		  neon_lane_bounds (op[argc], 0,
				    GET_MODE_NUNITS (map_mode), exp);
		  /* Keep to GCC-vector-extension lane indices in the RTL.  */
		  op[argc] =
		    GEN_INT (NEON_ENDIAN_LANE_N (map_mode, INTVAL (op[argc])));
		}
	      goto constant_arg;

	    case ARG_BUILTIN_LANE_INDEX:
	      /* Previous argument must be a vector, which this indexes.  */
	      gcc_assert (argc > 0);
	      if (CONST_INT_P (op[argc]))
		{
		  machine_mode vmode = mode[argc - 1];
		  neon_lane_bounds (op[argc], 0, GET_MODE_NUNITS (vmode), exp);
		}
	      /* If the lane index isn't a constant then error out.  */
	      goto constant_arg;

	    case ARG_BUILTIN_LANE_PAIR_INDEX:
	      /* Previous argument must be a vector, which this indexes. The
		 indexing will always select i and i+1 out of the vector, which
		 puts a limit on i.  */
	      gcc_assert (argc > 0);
	      if (CONST_INT_P (op[argc]))
		{
		  machine_mode vmode = mode[argc - 1];
		  neon_lane_bounds (op[argc], 0,
				    GET_MODE_NUNITS (vmode) / 2, exp);
		}
	      /* If the lane index isn't a constant then error out.  */
	      goto constant_arg;

	    case ARG_BUILTIN_LANE_QUADTUP_INDEX:
	      /* Previous argument must be a vector, which this indexes.  */
	      gcc_assert (argc > 0);
	      if (CONST_INT_P (op[argc]))
		{
		  machine_mode vmode = mode[argc - 1];
		  neon_lane_bounds (op[argc], 0,
				    GET_MODE_NUNITS (vmode) / 4, exp);
		}
	      /* If the lane index isn't a constant then error out.  */
	      goto constant_arg;

	    case ARG_BUILTIN_CONSTANT:
constant_arg:
	      if (!(*insn_data[icode].operand[opno].predicate)
		  (op[argc], mode[argc]))
		{
		  if (IN_RANGE (fcode, ARM_BUILTIN_CDE_PATTERN_START,
				ARM_BUILTIN_CDE_PATTERN_END))
		    {
		      if (argc == 0)
			{
			  unsigned int cp_bit = (CONST_INT_P (op[argc])
						 ? UINTVAL (op[argc]) : -1);
			  if (IN_RANGE (cp_bit, 0, ARM_CDE_CONST_COPROC))
			    error_at (EXPR_LOCATION (exp),
				      "coprocessor %d is not enabled "
				      "with +cdecp%d", cp_bit, cp_bit);
			  else
			    error_at (EXPR_LOCATION (exp),
				      "coproc must be a constant immediate in "
				      "range [0-%d] enabled with %<+cdecp<N>%>",
				      ARM_CDE_CONST_COPROC);
			}
		      else
			/* Here we mention the builtin name to follow the same
			   format that the C/C++ frontends use for referencing
			   a given argument index.  */
			error_at (EXPR_LOCATION (exp),
				  "argument %d to %qE must be a constant "
				  "immediate in range [0-%d]", argc + 1,
			       arm_builtin_decls[fcode],
			       cde_builtin_data[fcode -
			       ARM_BUILTIN_CDE_PATTERN_START].imm_max);
		    }
		  else
		    error_at (EXPR_LOCATION (exp),
			      "argument %d must be a constant immediate",
			      argc + 1);
		  /* We have failed to expand the pattern, and are safely
		     in to invalid code.  But the mid-end will still try to
		     build an assignment for this node while it expands,
		     before stopping for the error, just pass it back
		     TARGET to ensure a valid assignment.  */
		  return target;
		}
	      break;

	      case ARG_BUILTIN_NEON_MEMORY:
	      /* Check if expand failed.  */
	      if (op[argc] == const0_rtx)
		return 0;
	      gcc_assert (MEM_P (op[argc]));
	      PUT_MODE (op[argc], mode[argc]);
	      /* ??? arm_neon.h uses the same built-in functions for signed
		 and unsigned accesses, casting where necessary.  This isn't
		 alias safe.  */
	      set_mem_alias_set (op[argc], 0);
	      if (!(*insn_data[icode].operand[opno].predicate)
                   (op[argc], mode[argc]))
		op[argc] = (replace_equiv_address
			    (op[argc],
			     copy_to_mode_reg (Pmode, XEXP (op[argc], 0))));
              break;

	    case ARG_BUILTIN_STOP:
	      gcc_unreachable ();
	    }

	  argc++;
	}
    }

  if (have_retval)
    switch (argc)
      {
      case 0:
	pat = GEN_FCN (icode) (target);
	break;
      case 1:
	pat = GEN_FCN (icode) (target, op[0]);
	break;

      case 2:
	pat = GEN_FCN (icode) (target, op[0], op[1]);
	break;

      case 3:
	pat = GEN_FCN (icode) (target, op[0], op[1], op[2]);
	break;

      case 4:
	pat = GEN_FCN (icode) (target, op[0], op[1], op[2], op[3]);
	break;

      case 5:
	pat = GEN_FCN (icode) (target, op[0], op[1], op[2], op[3], op[4]);
	break;

      case 6:
	pat = GEN_FCN (icode) (target, op[0], op[1], op[2], op[3], op[4], op[5]);
	break;

      default:
	gcc_unreachable ();
      }
  else
    switch (argc)
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
    return 0;

  /* Check whether our current target implements the pattern chosen for this
     builtin and error out if not.  */
  start_sequence ();
  emit_insn (pat);
  insn = end_sequence ();

  if (recog_memoized (insn) < 0)
    error ("this builtin is not supported for this target");
  else
    emit_insn (insn);

  if (TARGET_HAVE_MVE && VALID_MVE_PRED_MODE (tmode))
    {
      rtx HItarget = gen_reg_rtx (HImode);
      emit_move_insn (HItarget, gen_lowpart (HImode, target));
      return HItarget;
    }

  return target;
}

/* Expand a general builtin.  These builtins are "special" because they don't
   have symbolic constants defined per-instruction or per instruction-variant.
   Instead, the required info is looked up in the ARM_BUILTIN_DATA record that
   is passed into the function.  */

static rtx
arm_general_expand_builtin_1 (int fcode, tree exp, rtx target,
			   arm_builtin_datum *d)
{
  enum insn_code icode = d->code;
  builtin_arg args[SIMD_MAX_BUILTIN_ARGS + 1];
  int num_args = insn_data[d->code].n_operands;
  int is_void = 0;
  int k;
  bool neon = false;
  bool mve = false;

  if (IN_RANGE (fcode, ARM_BUILTIN_VFP_BASE, ARM_BUILTIN_ACLE_BASE - 1))
    neon = true;

  if (IN_RANGE (fcode, ARM_BUILTIN_MVE_BASE, ARM_BUILTIN_MAX - 1))
    mve = true;

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
	args[k] = ARG_BUILTIN_LANE_INDEX;
      else if (d->qualifiers[qualifiers_k] & qualifier_lane_pair_index)
	args[k] = ARG_BUILTIN_LANE_PAIR_INDEX;
      else if (d->qualifiers[qualifiers_k] & qualifier_lane_quadtup_index)
	args[k] = ARG_BUILTIN_LANE_QUADTUP_INDEX;
      else if (d->qualifiers[qualifiers_k] & qualifier_struct_load_store_lane_index)
	args[k] = ARG_BUILTIN_STRUCT_LOAD_STORE_LANE_INDEX;
      else if (d->qualifiers[qualifiers_k] & qualifier_immediate)
	args[k] = ARG_BUILTIN_CONSTANT;
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
	  args[k] = op_const_int_p ? ARG_BUILTIN_CONSTANT : ARG_BUILTIN_COPY_TO_REG;
	}
      else if (d->qualifiers[qualifiers_k] & qualifier_pointer)
	{
	  if (neon || mve)
	    args[k] = ARG_BUILTIN_NEON_MEMORY;
	  else
	    args[k] = ARG_BUILTIN_MEMORY;
	}
      else
	args[k] = ARG_BUILTIN_COPY_TO_REG;
    }
  args[k] = ARG_BUILTIN_STOP;

  /* The interface to arm_general_expand_builtin_args expects a 0 if
     the function is void, and a 1 if it is not.  */
  return arm_general_expand_builtin_args
    (target, d->mode, fcode, icode, !is_void, exp,
     &args[1]);
}

/* Expand an ACLE builtin, i.e. those registered only if their respective
   target constraints are met.  This check happens within
   arm_general_expand_builtin_args.  */

static rtx
arm_expand_acle_builtin (int fcode, tree exp, rtx target)
{
  if (fcode == ARM_BUILTIN_SAT_IMM_CHECK)
    {
      /* Check the saturation immediate bounds.  */

      rtx min_sat = expand_normal (CALL_EXPR_ARG (exp, 1));
      rtx max_sat = expand_normal (CALL_EXPR_ARG (exp, 2));
      gcc_assert (CONST_INT_P (min_sat));
      gcc_assert (CONST_INT_P (max_sat));
      rtx sat_imm = expand_normal (CALL_EXPR_ARG (exp, 0));
      if (CONST_INT_P (sat_imm))
	{
	  if (!IN_RANGE (sat_imm, min_sat, max_sat))
	    error_at (EXPR_LOCATION (exp),
		      "saturation bit range must be in the range [%wd, %wd]",
		      UINTVAL (min_sat), UINTVAL (max_sat));
	}
      else
	error_at (EXPR_LOCATION (exp),
		  "saturation bit range must be a constant immediate");
      /* Don't generate any RTL.  */
      return const0_rtx;
    }

  gcc_assert (fcode != ARM_BUILTIN_CDE_BASE);
  arm_builtin_datum *d
    = (fcode < ARM_BUILTIN_CDE_BASE)
      ? &acle_builtin_data[fcode - ARM_BUILTIN_ACLE_PATTERN_START]
      : &cde_builtin_data[fcode - ARM_BUILTIN_CDE_PATTERN_START].base;

  return arm_general_expand_builtin_1 (fcode, exp, target, d);
}

/* Expand an MVE builtin, i.e. those registered only if their respective
   target constraints are met.  This check happens within
   arm_general_expand_builtin.  */

static rtx
arm_expand_mve_builtin (int fcode, tree exp, rtx target)
{
  if (fcode >= ARM_BUILTIN_MVE_BASE && !TARGET_HAVE_MVE)
  {
    fatal_error (input_location,
		"You must enable MVE instructions"
		" to use these intrinsics");
    return const0_rtx;
  }

  arm_builtin_datum *d
    = &mve_builtin_data[fcode - ARM_BUILTIN_MVE_PATTERN_START];

  return arm_general_expand_builtin_1 (fcode, exp, target, d);
}

/* Expand a Neon builtin, i.e. those registered only if TARGET_NEON holds.
   Most of these are "special" because they don't have symbolic
   constants defined per-instruction or per instruction-variant.  Instead, the
   required info is looked up in the table neon_builtin_data.  */

static rtx
arm_expand_neon_builtin (int fcode, tree exp, rtx target)
{
  if (fcode >= ARM_BUILTIN_NEON_BASE && ! TARGET_NEON)
    {
      fatal_error (input_location,
		   "You must enable NEON instructions"
		   " (e.g. %<-mfloat-abi=softfp%> %<-mfpu=neon%>)"
		   " to use these intrinsics.");
      return const0_rtx;
    }

  arm_builtin_datum *d
    = &neon_builtin_data[fcode - ARM_BUILTIN_NEON_PATTERN_START];

  return arm_general_expand_builtin_1 (fcode, exp, target, d);
}

/* Expand a VFP builtin.  These builtins are treated like
   neon builtins except that the data is looked up in table
   VFP_BUILTIN_DATA.  */

static rtx
arm_expand_vfp_builtin (int fcode, tree exp, rtx target)
{
  if (fcode >= ARM_BUILTIN_VFP_BASE && ! TARGET_HARD_FLOAT)
    {
      fatal_error (input_location,
		   "You must enable VFP instructions"
		   " to use these intrinsics.");
      return const0_rtx;
    }

  arm_builtin_datum *d
    = &vfp_builtin_data[fcode - ARM_BUILTIN_VFP_PATTERN_START];

  return arm_general_expand_builtin_1 (fcode, exp, target, d);
}

/* Implement TARGET_EXPAND_BUILTIN for general builtins.  */
rtx
arm_general_expand_builtin (unsigned int fcode,
			    tree exp,
		    rtx target,
		    int ignore ATTRIBUTE_UNUSED)
{
  const struct builtin_description * d;
  enum insn_code    icode;
  tree              arg0;
  rtx               op0;
  rtx               op1;
  rtx               pat;
  size_t            i;

  if (fcode == ARM_BUILTIN_SIMD_LANE_CHECK)
    {
      /* Builtin is only to check bounds of the lane passed to some intrinsics
	 that are implemented with gcc vector extensions in arm_neon.h.  */

      tree nlanes = CALL_EXPR_ARG (exp, 0);
      gcc_assert (TREE_CODE (nlanes) == INTEGER_CST);
      rtx lane_idx = expand_normal (CALL_EXPR_ARG (exp, 1));
      if (CONST_INT_P (lane_idx))
	neon_lane_bounds (lane_idx, 0, TREE_INT_CST_LOW (nlanes), exp);
      else
	error_at (EXPR_LOCATION (exp),
		  "lane index must be a constant immediate");
      /* Don't generate any RTL.  */
      return const0_rtx;
    }
  if (fcode >= ARM_BUILTIN_MVE_BASE)
    return arm_expand_mve_builtin (fcode, exp, target);

  if (fcode >= ARM_BUILTIN_ACLE_BASE)
    return arm_expand_acle_builtin (fcode, exp, target);

  if (fcode >= ARM_BUILTIN_NEON_BASE)
    return arm_expand_neon_builtin (fcode, exp, target);

  if (fcode >= ARM_BUILTIN_VFP_BASE)
    return arm_expand_vfp_builtin (fcode, exp, target);

  /* Check in the context of the function making the call whether the
     builtin is supported.  */
  if (fcode >= ARM_BUILTIN_CRYPTO_BASE
      && (!TARGET_CRYPTO || !TARGET_HARD_FLOAT))
    {
      fatal_error (input_location,
		   "You must enable crypto instructions"
		   " (e.g. include %<-mfloat-abi=softfp%> "
		   "%<-mfpu=crypto-neon%>)"
		   " to use these intrinsics.");
      return const0_rtx;
    }

  switch (fcode)
    {
    case ARM_BUILTIN_GET_FPSCR_NZCVQC:
    case ARM_BUILTIN_SET_FPSCR_NZCVQC:
      if (fcode == ARM_BUILTIN_GET_FPSCR_NZCVQC)
	{
	  icode = CODE_FOR_get_fpscr_nzcvqc;
	  target = gen_reg_rtx (SImode);
	  emit_insn (GEN_FCN (icode) (target));
	  return target;
	}
      else
	{
	  icode = CODE_FOR_set_fpscr_nzcvqc;
	  op0 = expand_normal (CALL_EXPR_ARG (exp, 0));
	  emit_insn (GEN_FCN (icode) (force_reg (SImode, op0)));
	  return NULL_RTX;
	}

    case ARM_BUILTIN_GET_FPSCR:
    case ARM_BUILTIN_SET_FPSCR:
      if (fcode == ARM_BUILTIN_GET_FPSCR)
	{
	  icode = CODE_FOR_get_fpscr;
	  target = gen_reg_rtx (SImode);
	  pat = GEN_FCN (icode) (target);
	}
      else
	{
	  target = NULL_RTX;
	  icode = CODE_FOR_set_fpscr;
	  arg0 = CALL_EXPR_ARG (exp, 0);
	  op0 = expand_normal (arg0);
	  pat = GEN_FCN (icode) (force_reg (SImode, op0));
	}
      emit_insn (pat);
      return target;

    case ARM_BUILTIN_CMSE_NONSECURE_CALLER:
      target = gen_reg_rtx (SImode);
      op0 = arm_return_addr (0, NULL_RTX);
      emit_insn (gen_andsi3 (target, op0, const1_rtx));
      op1 = gen_rtx_EQ (SImode, target, const0_rtx);
      emit_insn (gen_cstoresi4 (target, op1, target, const0_rtx));
      return target;

    default:
      break;
    }

  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    if (d->code == (enum arm_builtins) fcode)
      return arm_expand_binop_builtin (d->icode, exp, target);

  for (i = 0, d = bdesc_1arg; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    if (d->code == (enum arm_builtins) fcode)
      return arm_expand_unop_builtin (d->icode, exp, target, 0);

  for (i = 0, d = bdesc_3arg; i < ARRAY_SIZE (bdesc_3arg); i++, d++)
    if (d->code == (enum arm_builtins) fcode)
      return arm_expand_ternop_builtin (d->icode, exp, target);

  /* @@@ Should really do something sensible here.  */
  return NULL_RTX;
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

rtx
arm_expand_builtin (tree exp,
		    rtx target,
		    rtx subtarget ATTRIBUTE_UNUSED,
		    machine_mode mode ATTRIBUTE_UNUSED,
		    int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> ARM_BUILTIN_SHIFT;
  switch (code & ARM_BUILTIN_CLASS)
    {
    case ARM_BUILTIN_GENERAL:
      return arm_general_expand_builtin (subcode, exp, target, ignore);
    case ARM_BUILTIN_MVE:
      return arm_mve::expand_builtin (subcode, exp, target);
    default:
      gcc_unreachable ();
    }
}

void
arm_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update)
{
  const unsigned ARM_FE_INVALID = 1;
  const unsigned ARM_FE_DIVBYZERO = 2;
  const unsigned ARM_FE_OVERFLOW = 4;
  const unsigned ARM_FE_UNDERFLOW = 8;
  const unsigned ARM_FE_INEXACT = 16;
  const unsigned HOST_WIDE_INT ARM_FE_ALL_EXCEPT = (ARM_FE_INVALID
						    | ARM_FE_DIVBYZERO
						    | ARM_FE_OVERFLOW
						    | ARM_FE_UNDERFLOW
						    | ARM_FE_INEXACT);
  const unsigned HOST_WIDE_INT ARM_FE_EXCEPT_SHIFT = 8;
  tree fenv_var, get_fpscr, set_fpscr, mask, ld_fenv, masked_fenv;
  tree new_fenv_var, reload_fenv, restore_fnenv;
  tree update_call, atomic_feraiseexcept, hold_fnclex;

  if (!TARGET_HARD_FLOAT)
    return;

  /* Generate the equivalent of :
       unsigned int fenv_var;
       fenv_var = __builtin_arm_get_fpscr ();

       unsigned int masked_fenv;
       masked_fenv = fenv_var & mask;

       __builtin_arm_set_fpscr (masked_fenv);  */

  fenv_var = create_tmp_var_raw (unsigned_type_node);
  get_fpscr = arm_builtin_decls[ARM_BUILTIN_GET_FPSCR];
  set_fpscr = arm_builtin_decls[ARM_BUILTIN_SET_FPSCR];
  mask = build_int_cst (unsigned_type_node,
			~((ARM_FE_ALL_EXCEPT << ARM_FE_EXCEPT_SHIFT)
			  | ARM_FE_ALL_EXCEPT));
  ld_fenv = build4 (TARGET_EXPR, unsigned_type_node,
		    fenv_var, build_call_expr (get_fpscr, 0),
		    NULL_TREE, NULL_TREE);
  masked_fenv = build2 (BIT_AND_EXPR, unsigned_type_node, fenv_var, mask);
  hold_fnclex = build_call_expr (set_fpscr, 1, masked_fenv);
  *hold = build2 (COMPOUND_EXPR, void_type_node,
		  build2 (COMPOUND_EXPR, void_type_node, masked_fenv, ld_fenv),
		  hold_fnclex);

  /* Store the value of masked_fenv to clear the exceptions:
     __builtin_arm_set_fpscr (masked_fenv);  */

  *clear = build_call_expr (set_fpscr, 1, masked_fenv);

  /* Generate the equivalent of :
       unsigned int new_fenv_var;
       new_fenv_var = __builtin_arm_get_fpscr ();

       __builtin_arm_set_fpscr (fenv_var);

       __atomic_feraiseexcept (new_fenv_var);  */

  new_fenv_var = create_tmp_var_raw (unsigned_type_node);
  reload_fenv = build4 (TARGET_EXPR, unsigned_type_node, new_fenv_var,
			build_call_expr (get_fpscr, 0), NULL_TREE, NULL_TREE);
  restore_fnenv = build_call_expr (set_fpscr, 1, fenv_var);
  atomic_feraiseexcept = builtin_decl_implicit (BUILT_IN_ATOMIC_FERAISEEXCEPT);
  update_call = build_call_expr (atomic_feraiseexcept, 1,
				 fold_convert (integer_type_node, new_fenv_var));
  *update = build2 (COMPOUND_EXPR, void_type_node,
		    build2 (COMPOUND_EXPR, void_type_node,
			    reload_fenv, restore_fnenv), update_call);
}

/* Implement TARGET_CHECK_BUILTIN_CALL for general builtins.  Record a read of
   the Q bit through intrinsics in the machine function for general built-in
   functions.  */
bool
arm_general_check_builtin_call (unsigned int code)
{
  if (code == ARM_BUILTIN_saturation_occurred
     || code == ARM_BUILTIN_set_saturation)
    {
      if (cfun && cfun->decl)
	DECL_ATTRIBUTES (cfun->decl)
	  = tree_cons (get_identifier ("acle qbit"), NULL_TREE,
		       DECL_ATTRIBUTES (cfun->decl));
    }
  else if (code == ARM_BUILTIN_sel)
    {
      if (cfun && cfun->decl)
	DECL_ATTRIBUTES (cfun->decl)
	  = tree_cons (get_identifier ("acle gebits"), NULL_TREE,
		       DECL_ATTRIBUTES (cfun->decl));
    }
  return true;
}

/* Implement TARGET_CHECK_BUILTIN_CALL.  */
bool
arm_check_builtin_call (location_t loc, vec<location_t> arg_loc, tree fndecl,
			tree orig_fndecl, unsigned int nargs, tree *args, bool)
{
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> ARM_BUILTIN_SHIFT;
  switch (code & ARM_BUILTIN_CLASS)
    {
    case ARM_BUILTIN_GENERAL:
      return arm_general_check_builtin_call (subcode);
    case ARM_BUILTIN_MVE:
      return arm_mve::check_builtin_call (loc, arg_loc, subcode,
					  orig_fndecl, nargs, args);
    default:
      gcc_unreachable ();
    }

}

enum resolver_ident
arm_describe_resolver (tree fndecl)
{
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> ARM_BUILTIN_SHIFT;
  switch (code & ARM_BUILTIN_CLASS)
    {
    case ARM_BUILTIN_GENERAL:
      if (subcode >= ARM_BUILTIN_vcx1qv16qi
	&& subcode < ARM_BUILTIN_MVE_BASE)
	return arm_cde_resolver;
      return arm_no_resolver;
    case ARM_BUILTIN_MVE:
      return arm_mve_resolver;
    default:
      gcc_unreachable ();
    }
}

unsigned
arm_cde_end_args (tree fndecl)
{
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> ARM_BUILTIN_SHIFT;
  switch (code & ARM_BUILTIN_CLASS)
    {
    case ARM_BUILTIN_GENERAL:
      return subcode >= ARM_BUILTIN_vcx1q_p_v16qi ? 2 : 1;
    default:
      gcc_unreachable ();
    }
}

/* Fold a call to vaeseq_u8 and vaesdq_u8.
   That is `vaeseq_u8 (x ^ y, 0)` gets folded
   into `vaeseq_u8 (x, y)`.*/
static gimple *
arm_fold_aes_op (gcall *stmt)
{
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  if (integer_zerop (arg0))
    arg0 = arg1;
  else if (!integer_zerop (arg1))
    return nullptr;
  if (TREE_CODE (arg0) != SSA_NAME)
    return nullptr;
  if (!has_single_use (arg0))
    return nullptr;
  auto *s = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (arg0));
  if (!s || gimple_assign_rhs_code (s) != BIT_XOR_EXPR)
    return nullptr;
  gimple_call_set_arg (stmt, 0, gimple_assign_rhs1 (s));
  gimple_call_set_arg (stmt, 1, gimple_assign_rhs2 (s));
  return stmt;
}

/* Try to fold STMT, given that it's a call to the built-in function with
   subcode FCODE.  Return the new statement on success and null on
   failure.  */
gimple *
arm_general_gimple_fold_builtin (unsigned int fcode, gcall *stmt)
{
  gimple *new_stmt = NULL;

  switch (fcode)
    {
    case ARM_BUILTIN_CRYPTO_AESE:
    case ARM_BUILTIN_CRYPTO_AESD:
      new_stmt = arm_fold_aes_op (stmt);
      break;
    }

  /* GIMPLE assign statements (unlike calls) require a non-null lhs.  If we
     created an assign statement with a null lhs, then fix this by assigning
     to a new (and subsequently unused) variable.  */
  if (new_stmt && is_gimple_assign (new_stmt) && !gimple_assign_lhs (new_stmt))
    {
      tree new_lhs = make_ssa_name (gimple_call_return_type (stmt));
      gimple_assign_set_lhs (new_stmt, new_lhs);
    }

  return new_stmt;
}

#include "gt-arm-builtins.h"
