/* Builtins' description for AArch64 SIMD architecture.
   Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "calls.h"
#include "expr.h"
#include "tm_p.h"
#include "recog.h"
#include "langhooks.h"
#include "diagnostic-core.h"
#include "optabs.h"
#include "pointer-set.h"
#include "hash-table.h"
#include "vec.h"
#include "ggc.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"

enum aarch64_simd_builtin_type_mode
{
  T_V8QI,
  T_V4HI,
  T_V2SI,
  T_V2SF,
  T_DI,
  T_DF,
  T_V16QI,
  T_V8HI,
  T_V4SI,
  T_V4SF,
  T_V2DI,
  T_V2DF,
  T_TI,
  T_EI,
  T_OI,
  T_XI,
  T_SI,
  T_SF,
  T_HI,
  T_QI,
  T_MAX
};

#define v8qi_UP  T_V8QI
#define v4hi_UP  T_V4HI
#define v2si_UP  T_V2SI
#define v2sf_UP  T_V2SF
#define di_UP    T_DI
#define df_UP    T_DF
#define v16qi_UP T_V16QI
#define v8hi_UP  T_V8HI
#define v4si_UP  T_V4SI
#define v4sf_UP  T_V4SF
#define v2di_UP  T_V2DI
#define v2df_UP  T_V2DF
#define ti_UP	 T_TI
#define ei_UP	 T_EI
#define oi_UP	 T_OI
#define xi_UP	 T_XI
#define si_UP    T_SI
#define sf_UP    T_SF
#define hi_UP    T_HI
#define qi_UP    T_QI

#define UP(X) X##_UP

#define SIMD_MAX_BUILTIN_ARGS 5

enum aarch64_type_qualifiers
{
  /* T foo.  */
  qualifier_none = 0x0,
  /* unsigned T foo.  */
  qualifier_unsigned = 0x1, /* 1 << 0  */
  /* const T foo.  */
  qualifier_const = 0x2, /* 1 << 1  */
  /* T *foo.  */
  qualifier_pointer = 0x4, /* 1 << 2  */
  /* const T *foo.  */
  qualifier_const_pointer = 0x6, /* qualifier_const | qualifier_pointer  */
  /* Used when expanding arguments if an operand could
     be an immediate.  */
  qualifier_immediate = 0x8, /* 1 << 3  */
  qualifier_maybe_immediate = 0x10, /* 1 << 4  */
  /* void foo (...).  */
  qualifier_void = 0x20, /* 1 << 5  */
  /* Some patterns may have internal operands, this qualifier is an
     instruction to the initialisation code to skip this operand.  */
  qualifier_internal = 0x40, /* 1 << 6  */
  /* Some builtins should use the T_*mode* encoded in a simd_builtin_datum
     rather than using the type of the operand.  */
  qualifier_map_mode = 0x80, /* 1 << 7  */
  /* qualifier_pointer | qualifier_map_mode  */
  qualifier_pointer_map_mode = 0x84,
  /* qualifier_const_pointer | qualifier_map_mode  */
  qualifier_const_pointer_map_mode = 0x86,
  /* Polynomial types.  */
  qualifier_poly = 0x100
};

typedef struct
{
  const char *name;
  enum aarch64_simd_builtin_type_mode mode;
  const enum insn_code code;
  unsigned int fcode;
  enum aarch64_type_qualifiers *qualifiers;
} aarch64_simd_builtin_datum;

static enum aarch64_type_qualifiers
aarch64_types_unop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none };
#define TYPES_UNOP (aarch64_types_unop_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_unopu_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned };
#define TYPES_UNOPU (aarch64_types_unopu_qualifiers)
#define TYPES_CREATE (aarch64_types_unop_qualifiers)
#define TYPES_REINTERP_SS (aarch64_types_unop_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_unop_su_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_unsigned };
#define TYPES_REINTERP_SU (aarch64_types_unop_su_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_unop_sp_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_poly };
#define TYPES_REINTERP_SP (aarch64_types_unop_sp_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_unop_us_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_none };
#define TYPES_REINTERP_US (aarch64_types_unop_us_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_unop_ps_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_poly, qualifier_none };
#define TYPES_REINTERP_PS (aarch64_types_unop_ps_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_binop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_maybe_immediate };
#define TYPES_BINOP (aarch64_types_binop_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_binopv_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_none, qualifier_none };
#define TYPES_BINOPV (aarch64_types_binopv_qualifiers)
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
aarch64_types_binopp_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_poly, qualifier_poly, qualifier_poly };
#define TYPES_BINOPP (aarch64_types_binopp_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_ternop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_none };
#define TYPES_TERNOP (aarch64_types_ternop_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_ternopu_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned,
      qualifier_unsigned, qualifier_unsigned };
#define TYPES_TERNOPU (aarch64_types_ternopu_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_quadop_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none,
      qualifier_none, qualifier_none };
#define TYPES_QUADOP (aarch64_types_quadop_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_getlane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_immediate };
#define TYPES_GETLANE (aarch64_types_getlane_qualifiers)
#define TYPES_SHIFTIMM (aarch64_types_getlane_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_shift_to_unsigned_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_none, qualifier_immediate };
#define TYPES_SHIFTIMM_USS (aarch64_types_shift_to_unsigned_qualifiers)
static enum aarch64_type_qualifiers
aarch64_types_unsigned_shift_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_immediate };
#define TYPES_USHIFTIMM (aarch64_types_unsigned_shift_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_setlane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_immediate };
#define TYPES_SETLANE (aarch64_types_setlane_qualifiers)
#define TYPES_SHIFTINSERT (aarch64_types_setlane_qualifiers)
#define TYPES_SHIFTACC (aarch64_types_setlane_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_unsigned_shiftacc_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_unsigned, qualifier_unsigned, qualifier_unsigned,
      qualifier_immediate };
#define TYPES_USHIFTACC (aarch64_types_unsigned_shiftacc_qualifiers)


static enum aarch64_type_qualifiers
aarch64_types_combine_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none };
#define TYPES_COMBINE (aarch64_types_combine_qualifiers)

static enum aarch64_type_qualifiers
aarch64_types_load1_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_const_pointer_map_mode };
#define TYPES_LOAD1 (aarch64_types_load1_qualifiers)
#define TYPES_LOADSTRUCT (aarch64_types_load1_qualifiers)

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
aarch64_types_storestruct_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_void, qualifier_pointer_map_mode,
      qualifier_none, qualifier_none };
#define TYPES_STORESTRUCT_LANE (aarch64_types_storestruct_lane_qualifiers)

#define CF0(N, X) CODE_FOR_aarch64_##N##X
#define CF1(N, X) CODE_FOR_##N##X##1
#define CF2(N, X) CODE_FOR_##N##X##2
#define CF3(N, X) CODE_FOR_##N##X##3
#define CF4(N, X) CODE_FOR_##N##X##4
#define CF10(N, X) CODE_FOR_##N##X

#define VAR1(T, N, MAP, A) \
  {#N, UP (A), CF##MAP (N, A), 0, TYPES_##T},
#define VAR2(T, N, MAP, A, B) \
  VAR1 (T, N, MAP, A) \
  VAR1 (T, N, MAP, B)
#define VAR3(T, N, MAP, A, B, C) \
  VAR2 (T, N, MAP, A, B) \
  VAR1 (T, N, MAP, C)
#define VAR4(T, N, MAP, A, B, C, D) \
  VAR3 (T, N, MAP, A, B, C) \
  VAR1 (T, N, MAP, D)
#define VAR5(T, N, MAP, A, B, C, D, E) \
  VAR4 (T, N, MAP, A, B, C, D) \
  VAR1 (T, N, MAP, E)
#define VAR6(T, N, MAP, A, B, C, D, E, F) \
  VAR5 (T, N, MAP, A, B, C, D, E) \
  VAR1 (T, N, MAP, F)
#define VAR7(T, N, MAP, A, B, C, D, E, F, G) \
  VAR6 (T, N, MAP, A, B, C, D, E, F) \
  VAR1 (T, N, MAP, G)
#define VAR8(T, N, MAP, A, B, C, D, E, F, G, H) \
  VAR7 (T, N, MAP, A, B, C, D, E, F, G) \
  VAR1 (T, N, MAP, H)
#define VAR9(T, N, MAP, A, B, C, D, E, F, G, H, I) \
  VAR8 (T, N, MAP, A, B, C, D, E, F, G, H) \
  VAR1 (T, N, MAP, I)
#define VAR10(T, N, MAP, A, B, C, D, E, F, G, H, I, J) \
  VAR9 (T, N, MAP, A, B, C, D, E, F, G, H, I) \
  VAR1 (T, N, MAP, J)
#define VAR11(T, N, MAP, A, B, C, D, E, F, G, H, I, J, K) \
  VAR10 (T, N, MAP, A, B, C, D, E, F, G, H, I, J) \
  VAR1 (T, N, MAP, K)
#define VAR12(T, N, MAP, A, B, C, D, E, F, G, H, I, J, K, L) \
  VAR11 (T, N, MAP, A, B, C, D, E, F, G, H, I, J, K) \
  VAR1 (T, N, MAP, L)

/* BUILTIN_<ITERATOR> macros should expand to cover the same range of
   modes as is given for each define_mode_iterator in
   config/aarch64/iterators.md.  */

#define BUILTIN_DX(T, N, MAP) \
  VAR2 (T, N, MAP, di, df)
#define BUILTIN_GPF(T, N, MAP) \
  VAR2 (T, N, MAP, sf, df)
#define BUILTIN_SDQ_I(T, N, MAP) \
  VAR4 (T, N, MAP, qi, hi, si, di)
#define BUILTIN_SD_HSI(T, N, MAP) \
  VAR2 (T, N, MAP, hi, si)
#define BUILTIN_V2F(T, N, MAP) \
  VAR2 (T, N, MAP, v2sf, v2df)
#define BUILTIN_VALL(T, N, MAP) \
  VAR10 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, \
	 v4si, v2di, v2sf, v4sf, v2df)
#define BUILTIN_VALLDI(T, N, MAP) \
  VAR11 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, \
	 v4si, v2di, v2sf, v4sf, v2df, di)
#define BUILTIN_VALLDIF(T, N, MAP) \
  VAR12 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, \
	 v4si, v2di, v2sf, v4sf, v2df, di, df)
#define BUILTIN_VB(T, N, MAP) \
  VAR2 (T, N, MAP, v8qi, v16qi)
#define BUILTIN_VD(T, N, MAP) \
  VAR4 (T, N, MAP, v8qi, v4hi, v2si, v2sf)
#define BUILTIN_VDC(T, N, MAP) \
  VAR6 (T, N, MAP, v8qi, v4hi, v2si, v2sf, di, df)
#define BUILTIN_VDIC(T, N, MAP) \
  VAR3 (T, N, MAP, v8qi, v4hi, v2si)
#define BUILTIN_VDN(T, N, MAP) \
  VAR3 (T, N, MAP, v4hi, v2si, di)
#define BUILTIN_VDQ(T, N, MAP) \
  VAR7 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, v4si, v2di)
#define BUILTIN_VDQF(T, N, MAP) \
  VAR3 (T, N, MAP, v2sf, v4sf, v2df)
#define BUILTIN_VDQF_DF(T, N, MAP) \
  VAR4 (T, N, MAP, v2sf, v4sf, v2df, df)
#define BUILTIN_VDQH(T, N, MAP) \
  VAR2 (T, N, MAP, v4hi, v8hi)
#define BUILTIN_VDQHS(T, N, MAP) \
  VAR4 (T, N, MAP, v4hi, v8hi, v2si, v4si)
#define BUILTIN_VDQIF(T, N, MAP) \
  VAR9 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, v4si, v2sf, v4sf, v2df)
#define BUILTIN_VDQM(T, N, MAP) \
  VAR6 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, v4si)
#define BUILTIN_VDQV(T, N, MAP) \
  VAR5 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v4si)
#define BUILTIN_VDQQH(T, N, MAP) \
  VAR4 (T, N, MAP, v8qi, v16qi, v4hi, v8hi)
#define BUILTIN_VDQ_BHSI(T, N, MAP) \
  VAR6 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, v4si)
#define BUILTIN_VDQ_I(T, N, MAP) \
  VAR7 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, v4si, v2di)
#define BUILTIN_VDW(T, N, MAP) \
  VAR3 (T, N, MAP, v8qi, v4hi, v2si)
#define BUILTIN_VD_BHSI(T, N, MAP) \
  VAR3 (T, N, MAP, v8qi, v4hi, v2si)
#define BUILTIN_VD_HSI(T, N, MAP) \
  VAR2 (T, N, MAP, v4hi, v2si)
#define BUILTIN_VD_RE(T, N, MAP) \
  VAR6 (T, N, MAP, v8qi, v4hi, v2si, v2sf, di, df)
#define BUILTIN_VQ(T, N, MAP) \
  VAR6 (T, N, MAP, v16qi, v8hi, v4si, v2di, v4sf, v2df)
#define BUILTIN_VQN(T, N, MAP) \
  VAR3 (T, N, MAP, v8hi, v4si, v2di)
#define BUILTIN_VQW(T, N, MAP) \
  VAR3 (T, N, MAP, v16qi, v8hi, v4si)
#define BUILTIN_VQ_HSI(T, N, MAP) \
  VAR2 (T, N, MAP, v8hi, v4si)
#define BUILTIN_VQ_S(T, N, MAP) \
  VAR6 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, v4si)
#define BUILTIN_VSDQ_HSI(T, N, MAP) \
  VAR6 (T, N, MAP, v4hi, v8hi, v2si, v4si, hi, si)
#define BUILTIN_VSDQ_I(T, N, MAP) \
  VAR11 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, v4si, v2di, qi, hi, si, di)
#define BUILTIN_VSDQ_I_BHSI(T, N, MAP) \
  VAR10 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, v4si, v2di, qi, hi, si)
#define BUILTIN_VSDQ_I_DI(T, N, MAP) \
  VAR8 (T, N, MAP, v8qi, v16qi, v4hi, v8hi, v2si, v4si, v2di, di)
#define BUILTIN_VSD_HSI(T, N, MAP) \
  VAR4 (T, N, MAP, v4hi, v2si, hi, si)
#define BUILTIN_VSQN_HSDI(T, N, MAP) \
  VAR6 (T, N, MAP, v8hi, v4si, v2di, hi, si, di)
#define BUILTIN_VSTRUCT(T, N, MAP) \
  VAR3 (T, N, MAP, oi, ci, xi)

static aarch64_simd_builtin_datum aarch64_simd_builtin_data[] = {
#include "aarch64-simd-builtins.def"
};

#undef VAR1
#define VAR1(T, N, MAP, A) \
  AARCH64_SIMD_BUILTIN_##T##_##N##A,

enum aarch64_builtins
{
  AARCH64_BUILTIN_MIN,

  AARCH64_BUILTIN_GET_FPCR,
  AARCH64_BUILTIN_SET_FPCR,
  AARCH64_BUILTIN_GET_FPSR,
  AARCH64_BUILTIN_SET_FPSR,

  AARCH64_SIMD_BUILTIN_BASE,
#include "aarch64-simd-builtins.def"
  AARCH64_SIMD_BUILTIN_MAX = AARCH64_SIMD_BUILTIN_BASE
			      + ARRAY_SIZE (aarch64_simd_builtin_data),
  AARCH64_BUILTIN_MAX
};

static GTY(()) tree aarch64_builtin_decls[AARCH64_BUILTIN_MAX];

#define NUM_DREG_TYPES 6
#define NUM_QREG_TYPES 6

/* Return a tree for a signed or unsigned argument of either
   the mode specified by MODE, or the inner mode of MODE.  */
tree
aarch64_build_scalar_type (enum machine_mode mode,
			   bool unsigned_p,
			   bool poly_p)
{
#undef INT_TYPES
#define INT_TYPES \
  AARCH64_TYPE_BUILDER (QI) \
  AARCH64_TYPE_BUILDER (HI) \
  AARCH64_TYPE_BUILDER (SI) \
  AARCH64_TYPE_BUILDER (DI) \
  AARCH64_TYPE_BUILDER (EI) \
  AARCH64_TYPE_BUILDER (OI) \
  AARCH64_TYPE_BUILDER (CI) \
  AARCH64_TYPE_BUILDER (XI) \
  AARCH64_TYPE_BUILDER (TI) \

/* Statically declare all the possible types we might need.  */
#undef AARCH64_TYPE_BUILDER
#define AARCH64_TYPE_BUILDER(X) \
  static tree X##_aarch64_type_node_p = NULL; \
  static tree X##_aarch64_type_node_s = NULL; \
  static tree X##_aarch64_type_node_u = NULL;

  INT_TYPES

  static tree float_aarch64_type_node = NULL;
  static tree double_aarch64_type_node = NULL;

  gcc_assert (!VECTOR_MODE_P (mode));

/* If we've already initialised this type, don't initialise it again,
   otherwise ask for a new type of the correct size.  */
#undef AARCH64_TYPE_BUILDER
#define AARCH64_TYPE_BUILDER(X) \
  case X##mode: \
    if (unsigned_p) \
      return (X##_aarch64_type_node_u \
	      ? X##_aarch64_type_node_u \
	      : X##_aarch64_type_node_u \
		  = make_unsigned_type (GET_MODE_PRECISION (mode))); \
    else if (poly_p) \
       return (X##_aarch64_type_node_p \
	      ? X##_aarch64_type_node_p \
	      : X##_aarch64_type_node_p \
		  = make_unsigned_type (GET_MODE_PRECISION (mode))); \
    else \
       return (X##_aarch64_type_node_s \
	      ? X##_aarch64_type_node_s \
	      : X##_aarch64_type_node_s \
		  = make_signed_type (GET_MODE_PRECISION (mode))); \
    break;

  switch (mode)
    {
      INT_TYPES
      case SFmode:
	if (!float_aarch64_type_node)
	  {
	    float_aarch64_type_node = make_node (REAL_TYPE);
	    TYPE_PRECISION (float_aarch64_type_node) = FLOAT_TYPE_SIZE;
	    layout_type (float_aarch64_type_node);
	  }
	return float_aarch64_type_node;
	break;
      case DFmode:
	if (!double_aarch64_type_node)
	  {
	    double_aarch64_type_node = make_node (REAL_TYPE);
	    TYPE_PRECISION (double_aarch64_type_node) = DOUBLE_TYPE_SIZE;
	    layout_type (double_aarch64_type_node);
	  }
	return double_aarch64_type_node;
	break;
      default:
	gcc_unreachable ();
    }
}

tree
aarch64_build_vector_type (enum machine_mode mode,
			   bool unsigned_p,
			   bool poly_p)
{
  tree eltype;

#define VECTOR_TYPES \
  AARCH64_TYPE_BUILDER (V16QI) \
  AARCH64_TYPE_BUILDER (V8HI) \
  AARCH64_TYPE_BUILDER (V4SI) \
  AARCH64_TYPE_BUILDER (V2DI) \
  AARCH64_TYPE_BUILDER (V8QI) \
  AARCH64_TYPE_BUILDER (V4HI) \
  AARCH64_TYPE_BUILDER (V2SI) \
  \
  AARCH64_TYPE_BUILDER (V4SF) \
  AARCH64_TYPE_BUILDER (V2DF) \
  AARCH64_TYPE_BUILDER (V2SF) \
/* Declare our "cache" of values.  */
#undef AARCH64_TYPE_BUILDER
#define AARCH64_TYPE_BUILDER(X) \
  static tree X##_aarch64_type_node_s = NULL; \
  static tree X##_aarch64_type_node_u = NULL; \
  static tree X##_aarch64_type_node_p = NULL;

  VECTOR_TYPES

  gcc_assert (VECTOR_MODE_P (mode));

#undef AARCH64_TYPE_BUILDER
#define AARCH64_TYPE_BUILDER(X) \
  case X##mode: \
    if (unsigned_p) \
      return X##_aarch64_type_node_u \
	     ? X##_aarch64_type_node_u \
	     : X##_aarch64_type_node_u \
		= build_vector_type_for_mode (aarch64_build_scalar_type \
						(GET_MODE_INNER (mode), \
						 unsigned_p, poly_p), mode); \
    else if (poly_p) \
       return X##_aarch64_type_node_p \
	      ? X##_aarch64_type_node_p \
	      : X##_aarch64_type_node_p \
		= build_vector_type_for_mode (aarch64_build_scalar_type \
						(GET_MODE_INNER (mode), \
						 unsigned_p, poly_p), mode); \
    else \
       return X##_aarch64_type_node_s \
	      ? X##_aarch64_type_node_s \
	      : X##_aarch64_type_node_s \
		= build_vector_type_for_mode (aarch64_build_scalar_type \
						(GET_MODE_INNER (mode), \
						 unsigned_p, poly_p), mode); \
    break;

  switch (mode)
    {
      default:
	eltype = aarch64_build_scalar_type (GET_MODE_INNER (mode),
					    unsigned_p, poly_p);
	return build_vector_type_for_mode (eltype, mode);
	break;
      VECTOR_TYPES
   }
}

tree
aarch64_build_type (enum machine_mode mode, bool unsigned_p, bool poly_p)
{
  if (VECTOR_MODE_P (mode))
    return aarch64_build_vector_type (mode, unsigned_p, poly_p);
  else
    return aarch64_build_scalar_type (mode, unsigned_p, poly_p);
}

tree
aarch64_build_signed_type (enum machine_mode mode)
{
  return aarch64_build_type (mode, false, false);
}

tree
aarch64_build_unsigned_type (enum machine_mode mode)
{
  return aarch64_build_type (mode, true, false);
}

tree
aarch64_build_poly_type (enum machine_mode mode)
{
  return aarch64_build_type (mode, false, true);
}

static void
aarch64_init_simd_builtins (void)
{
  unsigned int i, fcode = AARCH64_SIMD_BUILTIN_BASE + 1;

  /* Signed scalar type nodes.  */
  tree aarch64_simd_intQI_type_node = aarch64_build_signed_type (QImode);
  tree aarch64_simd_intHI_type_node = aarch64_build_signed_type (HImode);
  tree aarch64_simd_intSI_type_node = aarch64_build_signed_type (SImode);
  tree aarch64_simd_intDI_type_node = aarch64_build_signed_type (DImode);
  tree aarch64_simd_intTI_type_node = aarch64_build_signed_type (TImode);
  tree aarch64_simd_intEI_type_node = aarch64_build_signed_type (EImode);
  tree aarch64_simd_intOI_type_node = aarch64_build_signed_type (OImode);
  tree aarch64_simd_intCI_type_node = aarch64_build_signed_type (CImode);
  tree aarch64_simd_intXI_type_node = aarch64_build_signed_type (XImode);

  /* Unsigned scalar type nodes.  */
  tree aarch64_simd_intUQI_type_node = aarch64_build_unsigned_type (QImode);
  tree aarch64_simd_intUHI_type_node = aarch64_build_unsigned_type (HImode);
  tree aarch64_simd_intUSI_type_node = aarch64_build_unsigned_type (SImode);
  tree aarch64_simd_intUDI_type_node = aarch64_build_unsigned_type (DImode);

  /* Poly scalar type nodes.  */
  tree aarch64_simd_polyQI_type_node = aarch64_build_poly_type (QImode);
  tree aarch64_simd_polyHI_type_node = aarch64_build_poly_type (HImode);
  tree aarch64_simd_polyDI_type_node = aarch64_build_poly_type (DImode);
  tree aarch64_simd_polyTI_type_node = aarch64_build_poly_type (TImode);

  /* Float type nodes.  */
  tree aarch64_simd_float_type_node = aarch64_build_signed_type (SFmode);
  tree aarch64_simd_double_type_node = aarch64_build_signed_type (DFmode);

  /* Define typedefs which exactly correspond to the modes we are basing vector
     types on.  If you change these names you'll need to change
     the table used by aarch64_mangle_type too.  */
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intQI_type_node,
					     "__builtin_aarch64_simd_qi");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intHI_type_node,
					     "__builtin_aarch64_simd_hi");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intSI_type_node,
					     "__builtin_aarch64_simd_si");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_float_type_node,
					     "__builtin_aarch64_simd_sf");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intDI_type_node,
					     "__builtin_aarch64_simd_di");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_double_type_node,
					     "__builtin_aarch64_simd_df");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_polyQI_type_node,
					     "__builtin_aarch64_simd_poly8");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_polyHI_type_node,
					     "__builtin_aarch64_simd_poly16");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_polyDI_type_node,
					     "__builtin_aarch64_simd_poly64");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_polyTI_type_node,
					     "__builtin_aarch64_simd_poly128");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intTI_type_node,
					     "__builtin_aarch64_simd_ti");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intEI_type_node,
					     "__builtin_aarch64_simd_ei");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intOI_type_node,
					     "__builtin_aarch64_simd_oi");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intCI_type_node,
					     "__builtin_aarch64_simd_ci");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intXI_type_node,
					     "__builtin_aarch64_simd_xi");

  /* Unsigned integer types for various mode sizes.  */
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intUQI_type_node,
					     "__builtin_aarch64_simd_uqi");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intUHI_type_node,
					     "__builtin_aarch64_simd_uhi");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intUSI_type_node,
					     "__builtin_aarch64_simd_usi");
  (*lang_hooks.types.register_builtin_type) (aarch64_simd_intUDI_type_node,
					     "__builtin_aarch64_simd_udi");

  for (i = 0; i < ARRAY_SIZE (aarch64_simd_builtin_data); i++, fcode++)
    {
      bool print_type_signature_p = false;
      char type_signature[SIMD_MAX_BUILTIN_ARGS] = { 0 };
      aarch64_simd_builtin_datum *d = &aarch64_simd_builtin_data[i];
      const char *const modenames[] =
	{
	  "v8qi", "v4hi", "v2si", "v2sf", "di", "df",
	  "v16qi", "v8hi", "v4si", "v4sf", "v2di", "v2df",
	  "ti", "ei", "oi", "xi", "si", "sf", "hi", "qi"
	};
      const enum machine_mode modes[] =
	{
	  V8QImode, V4HImode, V2SImode, V2SFmode, DImode, DFmode,
	  V16QImode, V8HImode, V4SImode, V4SFmode, V2DImode,
	  V2DFmode, TImode, EImode, OImode, XImode, SImode,
	  SFmode, HImode, QImode
	};
      char namebuf[60];
      tree ftype = NULL;
      tree fndecl = NULL;

      gcc_assert (ARRAY_SIZE (modenames) == T_MAX);

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
	  enum machine_mode op_mode = insn_data[d->code].operand[op_num].mode;
	  enum aarch64_type_qualifiers qualifiers = d->qualifiers[arg_num];

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
	      op_mode = modes[d->mode];

	  /* For pointers, we want a pointer to the basic type
	     of the vector.  */
	  if (qualifiers & qualifier_pointer && VECTOR_MODE_P (op_mode))
	    op_mode = GET_MODE_INNER (op_mode);

	  eltype = aarch64_build_type (op_mode,
				       qualifiers & qualifier_unsigned,
				       qualifiers & qualifier_poly);

	  /* Add qualifiers.  */
	  if (qualifiers & qualifier_const)
	    eltype = build_qualified_type (eltype, TYPE_QUAL_CONST);

	  if (qualifiers & qualifier_pointer)
	      eltype = build_pointer_type (eltype);

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
	snprintf (namebuf, sizeof (namebuf), "__builtin_aarch64_%s%s_%s",
		  d->name, modenames[d->mode], type_signature);
      else
	snprintf (namebuf, sizeof (namebuf), "__builtin_aarch64_%s%s",
		  d->name, modenames[d->mode]);

      fndecl = add_builtin_function (namebuf, ftype, fcode, BUILT_IN_MD,
				     NULL, NULL_TREE);
      aarch64_builtin_decls[fcode] = fndecl;
    }
}

void
aarch64_init_builtins (void)
{
  tree ftype_set_fpr
    = build_function_type_list (void_type_node, unsigned_type_node, NULL);
  tree ftype_get_fpr
    = build_function_type_list (unsigned_type_node, NULL);

  aarch64_builtin_decls[AARCH64_BUILTIN_GET_FPCR]
    = add_builtin_function ("__builtin_aarch64_get_fpcr", ftype_get_fpr,
			    AARCH64_BUILTIN_GET_FPCR, BUILT_IN_MD, NULL, NULL_TREE);
  aarch64_builtin_decls[AARCH64_BUILTIN_SET_FPCR]
    = add_builtin_function ("__builtin_aarch64_set_fpcr", ftype_set_fpr,
			    AARCH64_BUILTIN_SET_FPCR, BUILT_IN_MD, NULL, NULL_TREE);
  aarch64_builtin_decls[AARCH64_BUILTIN_GET_FPSR]
    = add_builtin_function ("__builtin_aarch64_get_fpsr", ftype_get_fpr,
			    AARCH64_BUILTIN_GET_FPSR, BUILT_IN_MD, NULL, NULL_TREE);
  aarch64_builtin_decls[AARCH64_BUILTIN_SET_FPSR]
    = add_builtin_function ("__builtin_aarch64_set_fpsr", ftype_set_fpr,
			    AARCH64_BUILTIN_SET_FPSR, BUILT_IN_MD, NULL, NULL_TREE);

  if (TARGET_SIMD)
    aarch64_init_simd_builtins ();
}

tree
aarch64_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= AARCH64_BUILTIN_MAX)
    return error_mark_node;

  return aarch64_builtin_decls[code];
}

typedef enum
{
  SIMD_ARG_COPY_TO_REG,
  SIMD_ARG_CONSTANT,
  SIMD_ARG_STOP
} builtin_simd_arg;

static rtx
aarch64_simd_expand_args (rtx target, int icode, int have_retval,
			  tree exp, ...)
{
  va_list ap;
  rtx pat;
  tree arg[SIMD_MAX_BUILTIN_ARGS];
  rtx op[SIMD_MAX_BUILTIN_ARGS];
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode[SIMD_MAX_BUILTIN_ARGS];
  int argc = 0;

  if (have_retval
      && (!target
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode)))
    target = gen_reg_rtx (tmode);

  va_start (ap, exp);

  for (;;)
    {
      builtin_simd_arg thisarg = (builtin_simd_arg) va_arg (ap, int);

      if (thisarg == SIMD_ARG_STOP)
	break;
      else
	{
	  arg[argc] = CALL_EXPR_ARG (exp, argc);
	  op[argc] = expand_normal (arg[argc]);
	  mode[argc] = insn_data[icode].operand[argc + have_retval].mode;

	  switch (thisarg)
	    {
	    case SIMD_ARG_COPY_TO_REG:
	      if (POINTER_TYPE_P (TREE_TYPE (arg[argc])))
		op[argc] = convert_memory_address (Pmode, op[argc]);
	      /*gcc_assert (GET_MODE (op[argc]) == mode[argc]); */
	      if (!(*insn_data[icode].operand[argc + have_retval].predicate)
		  (op[argc], mode[argc]))
		op[argc] = copy_to_mode_reg (mode[argc], op[argc]);
	      break;

	    case SIMD_ARG_CONSTANT:
	      if (!(*insn_data[icode].operand[argc + have_retval].predicate)
		  (op[argc], mode[argc]))
		error_at (EXPR_LOCATION (exp), "incompatible type for argument %d, "
		       "expected %<const int%>", argc + 1);
	      break;

	    case SIMD_ARG_STOP:
	      gcc_unreachable ();
	    }

	  argc++;
	}
    }

  va_end (ap);

  if (have_retval)
    switch (argc)
      {
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

      default:
	gcc_unreachable ();
      }

  if (!pat)
    return 0;

  emit_insn (pat);

  return target;
}

/* Expand an AArch64 AdvSIMD builtin(intrinsic).  */
rtx
aarch64_simd_expand_builtin (int fcode, tree exp, rtx target)
{
  aarch64_simd_builtin_datum *d =
		&aarch64_simd_builtin_data[fcode - (AARCH64_SIMD_BUILTIN_BASE + 1)];
  enum insn_code icode = d->code;
  builtin_simd_arg args[SIMD_MAX_BUILTIN_ARGS];
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

      if (d->qualifiers[qualifiers_k] & qualifier_immediate)
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
	  (target, icode, !is_void, exp,
	   args[1],
	   args[2],
	   args[3],
	   args[4],
	   SIMD_ARG_STOP);
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient.  */
rtx
aarch64_expand_builtin (tree exp,
		     rtx target,
		     rtx subtarget ATTRIBUTE_UNUSED,
		     enum machine_mode mode ATTRIBUTE_UNUSED,
		     int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  int fcode = DECL_FUNCTION_CODE (fndecl);
  int icode;
  rtx pat, op0;
  tree arg0;

  switch (fcode)
    {
    case AARCH64_BUILTIN_GET_FPCR:
    case AARCH64_BUILTIN_SET_FPCR:
    case AARCH64_BUILTIN_GET_FPSR:
    case AARCH64_BUILTIN_SET_FPSR:
      if ((fcode == AARCH64_BUILTIN_GET_FPCR)
	  || (fcode == AARCH64_BUILTIN_GET_FPSR))
	{
	  icode = (fcode == AARCH64_BUILTIN_GET_FPSR) ?
	    CODE_FOR_get_fpsr : CODE_FOR_get_fpcr;
	  target = gen_reg_rtx (SImode);
	  pat = GEN_FCN (icode) (target);
	}
      else
	{
	  target = NULL_RTX;
	  icode = (fcode == AARCH64_BUILTIN_SET_FPSR) ?
	    CODE_FOR_set_fpsr : CODE_FOR_set_fpcr;
	  arg0 = CALL_EXPR_ARG (exp, 0);
	  op0 = expand_normal (arg0);
	  pat = GEN_FCN (icode) (op0);
	}
      emit_insn (pat);
      return target;
    }

  if (fcode >= AARCH64_SIMD_BUILTIN_BASE)
    return aarch64_simd_expand_builtin (fcode, exp, target);

  return NULL_RTX;
}

tree
aarch64_builtin_vectorized_function (tree fndecl, tree type_out, tree type_in)
{
  enum machine_mode in_mode, out_mode;
  int in_n, out_n;

  if (TREE_CODE (type_out) != VECTOR_TYPE
      || TREE_CODE (type_in) != VECTOR_TYPE)
    return NULL_TREE;

  out_mode = TYPE_MODE (TREE_TYPE (type_out));
  out_n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);

#undef AARCH64_CHECK_BUILTIN_MODE
#define AARCH64_CHECK_BUILTIN_MODE(C, N) 1
#define AARCH64_FIND_FRINT_VARIANT(N) \
  (AARCH64_CHECK_BUILTIN_MODE (2, D) \
    ? aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOP_##N##v2df] \
    : (AARCH64_CHECK_BUILTIN_MODE (4, S) \
	? aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOP_##N##v4sf] \
	: (AARCH64_CHECK_BUILTIN_MODE (2, S) \
	   ? aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOP_##N##v2sf] \
	   : NULL_TREE)))
  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    {
      enum built_in_function fn = DECL_FUNCTION_CODE (fndecl);
      switch (fn)
	{
#undef AARCH64_CHECK_BUILTIN_MODE
#define AARCH64_CHECK_BUILTIN_MODE(C, N) \
  (out_mode == N##Fmode && out_n == C \
   && in_mode == N##Fmode && in_n == C)
	case BUILT_IN_FLOOR:
	case BUILT_IN_FLOORF:
	  return AARCH64_FIND_FRINT_VARIANT (floor);
	case BUILT_IN_CEIL:
	case BUILT_IN_CEILF:
	  return AARCH64_FIND_FRINT_VARIANT (ceil);
	case BUILT_IN_TRUNC:
	case BUILT_IN_TRUNCF:
	  return AARCH64_FIND_FRINT_VARIANT (btrunc);
	case BUILT_IN_ROUND:
	case BUILT_IN_ROUNDF:
	  return AARCH64_FIND_FRINT_VARIANT (round);
	case BUILT_IN_NEARBYINT:
	case BUILT_IN_NEARBYINTF:
	  return AARCH64_FIND_FRINT_VARIANT (nearbyint);
	case BUILT_IN_SQRT:
	case BUILT_IN_SQRTF:
	  return AARCH64_FIND_FRINT_VARIANT (sqrt);
#undef AARCH64_CHECK_BUILTIN_MODE
#define AARCH64_CHECK_BUILTIN_MODE(C, N) \
  (out_mode == SImode && out_n == C \
   && in_mode == N##Imode && in_n == C)
        case BUILT_IN_CLZ:
          {
            if (AARCH64_CHECK_BUILTIN_MODE (4, S))
              return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOP_clzv4si];
            return NULL_TREE;
          }
#undef AARCH64_CHECK_BUILTIN_MODE
#define AARCH64_CHECK_BUILTIN_MODE(C, N) \
  (out_mode == N##Imode && out_n == C \
   && in_mode == N##Fmode && in_n == C)
	case BUILT_IN_LFLOOR:
	case BUILT_IN_LFLOORF:
	case BUILT_IN_LLFLOOR:
	case BUILT_IN_IFLOORF:
	  {
	    enum aarch64_builtins builtin;
	    if (AARCH64_CHECK_BUILTIN_MODE (2, D))
	      builtin = AARCH64_SIMD_BUILTIN_UNOP_lfloorv2dfv2di;
	    else if (AARCH64_CHECK_BUILTIN_MODE (4, S))
	      builtin = AARCH64_SIMD_BUILTIN_UNOP_lfloorv4sfv4si;
	    else if (AARCH64_CHECK_BUILTIN_MODE (2, S))
	      builtin = AARCH64_SIMD_BUILTIN_UNOP_lfloorv2sfv2si;
	    else
	      return NULL_TREE;

	    return aarch64_builtin_decls[builtin];
	  }
	case BUILT_IN_LCEIL:
	case BUILT_IN_LCEILF:
	case BUILT_IN_LLCEIL:
	case BUILT_IN_ICEILF:
	  {
	    enum aarch64_builtins builtin;
	    if (AARCH64_CHECK_BUILTIN_MODE (2, D))
	      builtin = AARCH64_SIMD_BUILTIN_UNOP_lceilv2dfv2di;
	    else if (AARCH64_CHECK_BUILTIN_MODE (4, S))
	      builtin = AARCH64_SIMD_BUILTIN_UNOP_lceilv4sfv4si;
	    else if (AARCH64_CHECK_BUILTIN_MODE (2, S))
	      builtin = AARCH64_SIMD_BUILTIN_UNOP_lceilv2sfv2si;
	    else
	      return NULL_TREE;

	    return aarch64_builtin_decls[builtin];
	  }
	case BUILT_IN_LROUND:
	case BUILT_IN_IROUNDF:
	  {
	    enum aarch64_builtins builtin;
	    if (AARCH64_CHECK_BUILTIN_MODE (2, D))
	      builtin =	AARCH64_SIMD_BUILTIN_UNOP_lroundv2dfv2di;
	    else if (AARCH64_CHECK_BUILTIN_MODE (4, S))
	      builtin =	AARCH64_SIMD_BUILTIN_UNOP_lroundv4sfv4si;
	    else if (AARCH64_CHECK_BUILTIN_MODE (2, S))
	      builtin =	AARCH64_SIMD_BUILTIN_UNOP_lroundv2sfv2si;
	    else
	      return NULL_TREE;

	    return aarch64_builtin_decls[builtin];
	  }
	case BUILT_IN_BSWAP16:
#undef AARCH64_CHECK_BUILTIN_MODE
#define AARCH64_CHECK_BUILTIN_MODE(C, N) \
  (out_mode == N##Imode && out_n == C \
   && in_mode == N##Imode && in_n == C)
	  if (AARCH64_CHECK_BUILTIN_MODE (4, H))
	    return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOPU_bswapv4hi];
	  else if (AARCH64_CHECK_BUILTIN_MODE (8, H))
	    return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOPU_bswapv8hi];
	  else
	    return NULL_TREE;
	case BUILT_IN_BSWAP32:
	  if (AARCH64_CHECK_BUILTIN_MODE (2, S))
	    return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOPU_bswapv2si];
	  else if (AARCH64_CHECK_BUILTIN_MODE (4, S))
	    return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOPU_bswapv4si];
	  else
	    return NULL_TREE;
	case BUILT_IN_BSWAP64:
	  if (AARCH64_CHECK_BUILTIN_MODE (2, D))
	    return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOPU_bswapv2di];
	  else
	    return NULL_TREE;
	default:
	  return NULL_TREE;
      }
    }

  return NULL_TREE;
}

#undef VAR1
#define VAR1(T, N, MAP, A) \
  case AARCH64_SIMD_BUILTIN_##T##_##N##A:

tree
aarch64_fold_builtin (tree fndecl, int n_args ATTRIBUTE_UNUSED, tree *args,
		      bool ignore ATTRIBUTE_UNUSED)
{
  int fcode = DECL_FUNCTION_CODE (fndecl);
  tree type = TREE_TYPE (TREE_TYPE (fndecl));

  switch (fcode)
    {
      BUILTIN_VALLDI (UNOP, abs, 2)
	return fold_build1 (ABS_EXPR, type, args[0]);
	break;
      BUILTIN_VALLDI (BINOP, cmge, 0)
	return fold_build2 (GE_EXPR, type, args[0], args[1]);
	break;
      BUILTIN_VALLDI (BINOP, cmgt, 0)
	return fold_build2 (GT_EXPR, type, args[0], args[1]);
	break;
      BUILTIN_VALLDI (BINOP, cmeq, 0)
	return fold_build2 (EQ_EXPR, type, args[0], args[1]);
	break;
      BUILTIN_VSDQ_I_DI (BINOP, cmtst, 0)
	{
	  tree and_node = fold_build2 (BIT_AND_EXPR, type, args[0], args[1]);
	  tree vec_zero_node = build_zero_cst (type);
	  return fold_build2 (NE_EXPR, type, and_node, vec_zero_node);
	  break;
	}
      VAR1 (REINTERP_SS, reinterpretdi, 0, df)
      VAR1 (REINTERP_SS, reinterpretv8qi, 0, df)
      VAR1 (REINTERP_SS, reinterpretv4hi, 0, df)
      VAR1 (REINTERP_SS, reinterpretv2si, 0, df)
      VAR1 (REINTERP_SS, reinterpretv2sf, 0, df)
      BUILTIN_VD (REINTERP_SS, reinterpretdf, 0)
      BUILTIN_VD (REINTERP_SU, reinterpretdf, 0)
      VAR1 (REINTERP_US, reinterpretdi, 0, df)
      VAR1 (REINTERP_US, reinterpretv8qi, 0, df)
      VAR1 (REINTERP_US, reinterpretv4hi, 0, df)
      VAR1 (REINTERP_US, reinterpretv2si, 0, df)
      VAR1 (REINTERP_US, reinterpretv2sf, 0, df)
      BUILTIN_VD (REINTERP_SP, reinterpretdf, 0)
      VAR1 (REINTERP_PS, reinterpretdi, 0, df)
      VAR1 (REINTERP_PS, reinterpretv8qi, 0, df)
      VAR1 (REINTERP_PS, reinterpretv4hi, 0, df)
      VAR1 (REINTERP_PS, reinterpretv2si, 0, df)
      VAR1 (REINTERP_PS, reinterpretv2sf, 0, df)
	return fold_build1 (VIEW_CONVERT_EXPR, type, args[0]);
      VAR1 (UNOP, floatv2si, 2, v2sf)
      VAR1 (UNOP, floatv4si, 2, v4sf)
      VAR1 (UNOP, floatv2di, 2, v2df)
	return fold_build1 (FLOAT_EXPR, type, args[0]);
      default:
	break;
    }

  return NULL_TREE;
}

bool
aarch64_gimple_fold_builtin (gimple_stmt_iterator *gsi)
{
  bool changed = false;
  gimple stmt = gsi_stmt (*gsi);
  tree call = gimple_call_fn (stmt);
  tree fndecl;
  gimple new_stmt = NULL;
  if (call)
    {
      fndecl = gimple_call_fndecl (stmt);
      if (fndecl)
	{
	  int fcode = DECL_FUNCTION_CODE (fndecl);
	  int nargs = gimple_call_num_args (stmt);
	  tree *args = (nargs > 0
			? gimple_call_arg_ptr (stmt, 0)
			: &error_mark_node);

	  switch (fcode)
	    {
	      BUILTIN_VALL (UNOP, reduc_splus_, 10)
		new_stmt = gimple_build_assign_with_ops (
						REDUC_PLUS_EXPR,
						gimple_call_lhs (stmt),
						args[0],
						NULL_TREE);
		break;
	      BUILTIN_VDQIF (UNOP, reduc_smax_, 10)
		new_stmt = gimple_build_assign_with_ops (
						REDUC_MAX_EXPR,
						gimple_call_lhs (stmt),
						args[0],
						NULL_TREE);
		break;
	      BUILTIN_VDQIF (UNOP, reduc_smin_, 10)
		new_stmt = gimple_build_assign_with_ops (
						REDUC_MIN_EXPR,
						gimple_call_lhs (stmt),
						args[0],
						NULL_TREE);
		break;

	    default:
	      break;
	    }
	}
    }

  if (new_stmt)
    {
      gsi_replace (gsi, new_stmt, true);
      changed = true;
    }

  return changed;
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

  fenv_cr = create_tmp_var (unsigned_type_node, NULL);
  fenv_sr = create_tmp_var (unsigned_type_node, NULL);

  get_fpcr = aarch64_builtin_decls[AARCH64_BUILTIN_GET_FPCR];
  set_fpcr = aarch64_builtin_decls[AARCH64_BUILTIN_SET_FPCR];
  get_fpsr = aarch64_builtin_decls[AARCH64_BUILTIN_GET_FPSR];
  set_fpsr = aarch64_builtin_decls[AARCH64_BUILTIN_SET_FPSR];

  mask_cr = build_int_cst (unsigned_type_node,
			   ~(AARCH64_FE_ALL_EXCEPT << AARCH64_FE_EXCEPT_SHIFT));
  mask_sr = build_int_cst (unsigned_type_node,
			   ~(AARCH64_FE_ALL_EXCEPT));

  ld_fenv_cr = build2 (MODIFY_EXPR, unsigned_type_node,
		    fenv_cr, build_call_expr (get_fpcr, 0));
  ld_fenv_sr = build2 (MODIFY_EXPR, unsigned_type_node,
		    fenv_sr, build_call_expr (get_fpsr, 0));

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

  new_fenv_var = create_tmp_var (unsigned_type_node, NULL);
  reload_fenv = build2 (MODIFY_EXPR, unsigned_type_node,
			new_fenv_var, build_call_expr (get_fpsr, 0));
  restore_fnenv = build_call_expr (set_fpsr, 1, fenv_sr);
  atomic_feraiseexcept = builtin_decl_implicit (BUILT_IN_ATOMIC_FERAISEEXCEPT);
  update_call = build_call_expr (atomic_feraiseexcept, 1,
				 fold_convert (integer_type_node, new_fenv_var));
  *update = build2 (COMPOUND_EXPR, void_type_node,
		    build2 (COMPOUND_EXPR, void_type_node,
			    reload_fenv, restore_fnenv), update_call);
}


#undef AARCH64_CHECK_BUILTIN_MODE
#undef AARCH64_FIND_FRINT_VARIANT
#undef BUILTIN_DX
#undef BUILTIN_SDQ_I
#undef BUILTIN_SD_HSI
#undef BUILTIN_V2F
#undef BUILTIN_VALL
#undef BUILTIN_VB
#undef BUILTIN_VD
#undef BUILTIN_VDC
#undef BUILTIN_VDIC
#undef BUILTIN_VDN
#undef BUILTIN_VDQ
#undef BUILTIN_VDQF
#undef BUILTIN_VDQH
#undef BUILTIN_VDQHS
#undef BUILTIN_VDQIF
#undef BUILTIN_VDQM
#undef BUILTIN_VDQV
#undef BUILTIN_VDQ_BHSI
#undef BUILTIN_VDQ_I
#undef BUILTIN_VDW
#undef BUILTIN_VD_BHSI
#undef BUILTIN_VD_HSI
#undef BUILTIN_VD_RE
#undef BUILTIN_VQ
#undef BUILTIN_VQN
#undef BUILTIN_VQW
#undef BUILTIN_VQ_HSI
#undef BUILTIN_VQ_S
#undef BUILTIN_VSDQ_HSI
#undef BUILTIN_VSDQ_I
#undef BUILTIN_VSDQ_I_BHSI
#undef BUILTIN_VSDQ_I_DI
#undef BUILTIN_VSD_HSI
#undef BUILTIN_VSQN_HSDI
#undef BUILTIN_VSTRUCT
#undef CF0
#undef CF1
#undef CF2
#undef CF3
#undef CF4
#undef CF10
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

