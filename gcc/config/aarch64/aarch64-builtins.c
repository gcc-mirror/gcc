/* Builtins' description for AArch64 SIMD architecture.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
#include "expr.h"
#include "tm_p.h"
#include "recog.h"
#include "langhooks.h"
#include "diagnostic-core.h"
#include "optabs.h"
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

typedef enum
{
  AARCH64_SIMD_BINOP,
  AARCH64_SIMD_TERNOP,
  AARCH64_SIMD_QUADOP,
  AARCH64_SIMD_UNOP,
  AARCH64_SIMD_GETLANE,
  AARCH64_SIMD_SETLANE,
  AARCH64_SIMD_CREATE,
  AARCH64_SIMD_DUP,
  AARCH64_SIMD_DUPLANE,
  AARCH64_SIMD_COMBINE,
  AARCH64_SIMD_SPLIT,
  AARCH64_SIMD_LANEMUL,
  AARCH64_SIMD_LANEMULL,
  AARCH64_SIMD_LANEMULH,
  AARCH64_SIMD_LANEMAC,
  AARCH64_SIMD_SCALARMUL,
  AARCH64_SIMD_SCALARMULL,
  AARCH64_SIMD_SCALARMULH,
  AARCH64_SIMD_SCALARMAC,
  AARCH64_SIMD_CONVERT,
  AARCH64_SIMD_FIXCONV,
  AARCH64_SIMD_SELECT,
  AARCH64_SIMD_RESULTPAIR,
  AARCH64_SIMD_REINTERP,
  AARCH64_SIMD_VTBL,
  AARCH64_SIMD_VTBX,
  AARCH64_SIMD_LOAD1,
  AARCH64_SIMD_LOAD1LANE,
  AARCH64_SIMD_STORE1,
  AARCH64_SIMD_STORE1LANE,
  AARCH64_SIMD_LOADSTRUCT,
  AARCH64_SIMD_LOADSTRUCTLANE,
  AARCH64_SIMD_STORESTRUCT,
  AARCH64_SIMD_STORESTRUCTLANE,
  AARCH64_SIMD_LOGICBINOP,
  AARCH64_SIMD_SHIFTINSERT,
  AARCH64_SIMD_SHIFTIMM,
  AARCH64_SIMD_SHIFTACC
} aarch64_simd_itype;

typedef struct
{
  const char *name;
  const aarch64_simd_itype itype;
  enum aarch64_simd_builtin_type_mode mode;
  const enum insn_code code;
  unsigned int fcode;
} aarch64_simd_builtin_datum;

#define CF0(N, X) CODE_FOR_aarch64_##N##X
#define CF1(N, X) CODE_FOR_##N##X##1
#define CF2(N, X) CODE_FOR_##N##X##2
#define CF3(N, X) CODE_FOR_##N##X##3
#define CF4(N, X) CODE_FOR_##N##X##4
#define CF10(N, X) CODE_FOR_##N##X

#define VAR1(T, N, MAP, A) \
  {#N, AARCH64_SIMD_##T, UP (A), CF##MAP (N, A), 0},
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
  AARCH64_SIMD_BUILTIN_##N##A,

enum aarch64_builtins
{
  AARCH64_BUILTIN_MIN,
  AARCH64_SIMD_BUILTIN_BASE,
#include "aarch64-simd-builtins.def"
  AARCH64_SIMD_BUILTIN_MAX = AARCH64_SIMD_BUILTIN_BASE
			      + ARRAY_SIZE (aarch64_simd_builtin_data),
  AARCH64_BUILTIN_MAX
};

static GTY(()) tree aarch64_builtin_decls[AARCH64_BUILTIN_MAX];

#define NUM_DREG_TYPES 6
#define NUM_QREG_TYPES 6

static void
aarch64_init_simd_builtins (void)
{
  unsigned int i, fcode = AARCH64_SIMD_BUILTIN_BASE + 1;

  /* Scalar type nodes.  */
  tree aarch64_simd_intQI_type_node;
  tree aarch64_simd_intHI_type_node;
  tree aarch64_simd_polyQI_type_node;
  tree aarch64_simd_polyHI_type_node;
  tree aarch64_simd_intSI_type_node;
  tree aarch64_simd_intDI_type_node;
  tree aarch64_simd_float_type_node;
  tree aarch64_simd_double_type_node;

  /* Pointer to scalar type nodes.  */
  tree intQI_pointer_node;
  tree intHI_pointer_node;
  tree intSI_pointer_node;
  tree intDI_pointer_node;
  tree float_pointer_node;
  tree double_pointer_node;

  /* Const scalar type nodes.  */
  tree const_intQI_node;
  tree const_intHI_node;
  tree const_intSI_node;
  tree const_intDI_node;
  tree const_float_node;
  tree const_double_node;

  /* Pointer to const scalar type nodes.  */
  tree const_intQI_pointer_node;
  tree const_intHI_pointer_node;
  tree const_intSI_pointer_node;
  tree const_intDI_pointer_node;
  tree const_float_pointer_node;
  tree const_double_pointer_node;

  /* Vector type nodes.  */
  tree V8QI_type_node;
  tree V4HI_type_node;
  tree V2SI_type_node;
  tree V2SF_type_node;
  tree V16QI_type_node;
  tree V8HI_type_node;
  tree V4SI_type_node;
  tree V4SF_type_node;
  tree V2DI_type_node;
  tree V2DF_type_node;

  /* Scalar unsigned type nodes.  */
  tree intUQI_type_node;
  tree intUHI_type_node;
  tree intUSI_type_node;
  tree intUDI_type_node;

  /* Opaque integer types for structures of vectors.  */
  tree intEI_type_node;
  tree intOI_type_node;
  tree intCI_type_node;
  tree intXI_type_node;

  /* Pointer to vector type nodes.  */
  tree V8QI_pointer_node;
  tree V4HI_pointer_node;
  tree V2SI_pointer_node;
  tree V2SF_pointer_node;
  tree V16QI_pointer_node;
  tree V8HI_pointer_node;
  tree V4SI_pointer_node;
  tree V4SF_pointer_node;
  tree V2DI_pointer_node;
  tree V2DF_pointer_node;

  /* Operations which return results as pairs.  */
  tree void_ftype_pv8qi_v8qi_v8qi;
  tree void_ftype_pv4hi_v4hi_v4hi;
  tree void_ftype_pv2si_v2si_v2si;
  tree void_ftype_pv2sf_v2sf_v2sf;
  tree void_ftype_pdi_di_di;
  tree void_ftype_pv16qi_v16qi_v16qi;
  tree void_ftype_pv8hi_v8hi_v8hi;
  tree void_ftype_pv4si_v4si_v4si;
  tree void_ftype_pv4sf_v4sf_v4sf;
  tree void_ftype_pv2di_v2di_v2di;
  tree void_ftype_pv2df_v2df_v2df;

  tree reinterp_ftype_dreg[NUM_DREG_TYPES][NUM_DREG_TYPES];
  tree reinterp_ftype_qreg[NUM_QREG_TYPES][NUM_QREG_TYPES];
  tree dreg_types[NUM_DREG_TYPES], qreg_types[NUM_QREG_TYPES];

  /* Create distinguished type nodes for AARCH64_SIMD vector element types,
     and pointers to values of such types, so we can detect them later.  */
  aarch64_simd_intQI_type_node =
    make_signed_type (GET_MODE_PRECISION (QImode));
  aarch64_simd_intHI_type_node =
    make_signed_type (GET_MODE_PRECISION (HImode));
  aarch64_simd_polyQI_type_node =
    make_signed_type (GET_MODE_PRECISION (QImode));
  aarch64_simd_polyHI_type_node =
    make_signed_type (GET_MODE_PRECISION (HImode));
  aarch64_simd_intSI_type_node =
    make_signed_type (GET_MODE_PRECISION (SImode));
  aarch64_simd_intDI_type_node =
    make_signed_type (GET_MODE_PRECISION (DImode));
  aarch64_simd_float_type_node = make_node (REAL_TYPE);
  aarch64_simd_double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (aarch64_simd_float_type_node) = FLOAT_TYPE_SIZE;
  TYPE_PRECISION (aarch64_simd_double_type_node) = DOUBLE_TYPE_SIZE;
  layout_type (aarch64_simd_float_type_node);
  layout_type (aarch64_simd_double_type_node);

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

  intQI_pointer_node = build_pointer_type (aarch64_simd_intQI_type_node);
  intHI_pointer_node = build_pointer_type (aarch64_simd_intHI_type_node);
  intSI_pointer_node = build_pointer_type (aarch64_simd_intSI_type_node);
  intDI_pointer_node = build_pointer_type (aarch64_simd_intDI_type_node);
  float_pointer_node = build_pointer_type (aarch64_simd_float_type_node);
  double_pointer_node = build_pointer_type (aarch64_simd_double_type_node);

  /* Next create constant-qualified versions of the above types.  */
  const_intQI_node = build_qualified_type (aarch64_simd_intQI_type_node,
					   TYPE_QUAL_CONST);
  const_intHI_node = build_qualified_type (aarch64_simd_intHI_type_node,
					   TYPE_QUAL_CONST);
  const_intSI_node = build_qualified_type (aarch64_simd_intSI_type_node,
					   TYPE_QUAL_CONST);
  const_intDI_node = build_qualified_type (aarch64_simd_intDI_type_node,
					   TYPE_QUAL_CONST);
  const_float_node = build_qualified_type (aarch64_simd_float_type_node,
					   TYPE_QUAL_CONST);
  const_double_node = build_qualified_type (aarch64_simd_double_type_node,
					    TYPE_QUAL_CONST);

  const_intQI_pointer_node = build_pointer_type (const_intQI_node);
  const_intHI_pointer_node = build_pointer_type (const_intHI_node);
  const_intSI_pointer_node = build_pointer_type (const_intSI_node);
  const_intDI_pointer_node = build_pointer_type (const_intDI_node);
  const_float_pointer_node = build_pointer_type (const_float_node);
  const_double_pointer_node = build_pointer_type (const_double_node);

  /* Now create vector types based on our AARCH64 SIMD element types.  */
  /* 64-bit vectors.  */
  V8QI_type_node =
    build_vector_type_for_mode (aarch64_simd_intQI_type_node, V8QImode);
  V4HI_type_node =
    build_vector_type_for_mode (aarch64_simd_intHI_type_node, V4HImode);
  V2SI_type_node =
    build_vector_type_for_mode (aarch64_simd_intSI_type_node, V2SImode);
  V2SF_type_node =
    build_vector_type_for_mode (aarch64_simd_float_type_node, V2SFmode);
  /* 128-bit vectors.  */
  V16QI_type_node =
    build_vector_type_for_mode (aarch64_simd_intQI_type_node, V16QImode);
  V8HI_type_node =
    build_vector_type_for_mode (aarch64_simd_intHI_type_node, V8HImode);
  V4SI_type_node =
    build_vector_type_for_mode (aarch64_simd_intSI_type_node, V4SImode);
  V4SF_type_node =
    build_vector_type_for_mode (aarch64_simd_float_type_node, V4SFmode);
  V2DI_type_node =
    build_vector_type_for_mode (aarch64_simd_intDI_type_node, V2DImode);
  V2DF_type_node =
    build_vector_type_for_mode (aarch64_simd_double_type_node, V2DFmode);

  /* Unsigned integer types for various mode sizes.  */
  intUQI_type_node = make_unsigned_type (GET_MODE_PRECISION (QImode));
  intUHI_type_node = make_unsigned_type (GET_MODE_PRECISION (HImode));
  intUSI_type_node = make_unsigned_type (GET_MODE_PRECISION (SImode));
  intUDI_type_node = make_unsigned_type (GET_MODE_PRECISION (DImode));

  (*lang_hooks.types.register_builtin_type) (intUQI_type_node,
					     "__builtin_aarch64_simd_uqi");
  (*lang_hooks.types.register_builtin_type) (intUHI_type_node,
					     "__builtin_aarch64_simd_uhi");
  (*lang_hooks.types.register_builtin_type) (intUSI_type_node,
					     "__builtin_aarch64_simd_usi");
  (*lang_hooks.types.register_builtin_type) (intUDI_type_node,
					     "__builtin_aarch64_simd_udi");

  /* Opaque integer types for structures of vectors.  */
  intEI_type_node = make_signed_type (GET_MODE_PRECISION (EImode));
  intOI_type_node = make_signed_type (GET_MODE_PRECISION (OImode));
  intCI_type_node = make_signed_type (GET_MODE_PRECISION (CImode));
  intXI_type_node = make_signed_type (GET_MODE_PRECISION (XImode));

  (*lang_hooks.types.register_builtin_type) (intTI_type_node,
					     "__builtin_aarch64_simd_ti");
  (*lang_hooks.types.register_builtin_type) (intEI_type_node,
					     "__builtin_aarch64_simd_ei");
  (*lang_hooks.types.register_builtin_type) (intOI_type_node,
					     "__builtin_aarch64_simd_oi");
  (*lang_hooks.types.register_builtin_type) (intCI_type_node,
					     "__builtin_aarch64_simd_ci");
  (*lang_hooks.types.register_builtin_type) (intXI_type_node,
					     "__builtin_aarch64_simd_xi");

  /* Pointers to vector types.  */
  V8QI_pointer_node = build_pointer_type (V8QI_type_node);
  V4HI_pointer_node = build_pointer_type (V4HI_type_node);
  V2SI_pointer_node = build_pointer_type (V2SI_type_node);
  V2SF_pointer_node = build_pointer_type (V2SF_type_node);
  V16QI_pointer_node = build_pointer_type (V16QI_type_node);
  V8HI_pointer_node = build_pointer_type (V8HI_type_node);
  V4SI_pointer_node = build_pointer_type (V4SI_type_node);
  V4SF_pointer_node = build_pointer_type (V4SF_type_node);
  V2DI_pointer_node = build_pointer_type (V2DI_type_node);
  V2DF_pointer_node = build_pointer_type (V2DF_type_node);

  /* Operations which return results as pairs.  */
  void_ftype_pv8qi_v8qi_v8qi =
    build_function_type_list (void_type_node, V8QI_pointer_node,
			      V8QI_type_node, V8QI_type_node, NULL);
  void_ftype_pv4hi_v4hi_v4hi =
    build_function_type_list (void_type_node, V4HI_pointer_node,
			      V4HI_type_node, V4HI_type_node, NULL);
  void_ftype_pv2si_v2si_v2si =
    build_function_type_list (void_type_node, V2SI_pointer_node,
			      V2SI_type_node, V2SI_type_node, NULL);
  void_ftype_pv2sf_v2sf_v2sf =
    build_function_type_list (void_type_node, V2SF_pointer_node,
			      V2SF_type_node, V2SF_type_node, NULL);
  void_ftype_pdi_di_di =
    build_function_type_list (void_type_node, intDI_pointer_node,
			      aarch64_simd_intDI_type_node,
			      aarch64_simd_intDI_type_node, NULL);
  void_ftype_pv16qi_v16qi_v16qi =
    build_function_type_list (void_type_node, V16QI_pointer_node,
			      V16QI_type_node, V16QI_type_node, NULL);
  void_ftype_pv8hi_v8hi_v8hi =
    build_function_type_list (void_type_node, V8HI_pointer_node,
			      V8HI_type_node, V8HI_type_node, NULL);
  void_ftype_pv4si_v4si_v4si =
    build_function_type_list (void_type_node, V4SI_pointer_node,
			      V4SI_type_node, V4SI_type_node, NULL);
  void_ftype_pv4sf_v4sf_v4sf =
    build_function_type_list (void_type_node, V4SF_pointer_node,
			      V4SF_type_node, V4SF_type_node, NULL);
  void_ftype_pv2di_v2di_v2di =
    build_function_type_list (void_type_node, V2DI_pointer_node,
			      V2DI_type_node, V2DI_type_node, NULL);
  void_ftype_pv2df_v2df_v2df =
    build_function_type_list (void_type_node, V2DF_pointer_node,
			      V2DF_type_node, V2DF_type_node, NULL);

  dreg_types[0] = V8QI_type_node;
  dreg_types[1] = V4HI_type_node;
  dreg_types[2] = V2SI_type_node;
  dreg_types[3] = V2SF_type_node;
  dreg_types[4] = aarch64_simd_intDI_type_node;
  dreg_types[5] = aarch64_simd_double_type_node;

  qreg_types[0] = V16QI_type_node;
  qreg_types[1] = V8HI_type_node;
  qreg_types[2] = V4SI_type_node;
  qreg_types[3] = V4SF_type_node;
  qreg_types[4] = V2DI_type_node;
  qreg_types[5] = V2DF_type_node;

  /* If NUM_DREG_TYPES != NUM_QREG_TYPES, we will need separate nested loops
     for qreg and dreg reinterp inits.  */
  for (i = 0; i < NUM_DREG_TYPES; i++)
    {
      int j;
      for (j = 0; j < NUM_DREG_TYPES; j++)
	{
	  reinterp_ftype_dreg[i][j]
	    = build_function_type_list (dreg_types[i], dreg_types[j], NULL);
	  reinterp_ftype_qreg[i][j]
	    = build_function_type_list (qreg_types[i], qreg_types[j], NULL);
	}
    }

  for (i = 0; i < ARRAY_SIZE (aarch64_simd_builtin_data); i++, fcode++)
    {
      aarch64_simd_builtin_datum *d = &aarch64_simd_builtin_data[i];
      const char *const modenames[] =
      {
	"v8qi", "v4hi", "v2si", "v2sf", "di", "df",
	"v16qi", "v8hi", "v4si", "v4sf", "v2di", "v2df",
	"ti", "ei", "oi", "xi", "si", "sf", "hi", "qi"
      };
      char namebuf[60];
      tree ftype = NULL;
      tree fndecl = NULL;
      int is_load = 0;
      int is_store = 0;

      gcc_assert (ARRAY_SIZE (modenames) == T_MAX);

      d->fcode = fcode;

      switch (d->itype)
	{
	case AARCH64_SIMD_LOAD1:
	case AARCH64_SIMD_LOAD1LANE:
	case AARCH64_SIMD_LOADSTRUCT:
	case AARCH64_SIMD_LOADSTRUCTLANE:
	    is_load = 1;
	  /* Fall through.  */
	case AARCH64_SIMD_STORE1:
	case AARCH64_SIMD_STORE1LANE:
	case AARCH64_SIMD_STORESTRUCT:
	case AARCH64_SIMD_STORESTRUCTLANE:
	    if (!is_load)
	      is_store = 1;
	  /* Fall through.  */
	case AARCH64_SIMD_UNOP:
	case AARCH64_SIMD_BINOP:
	case AARCH64_SIMD_TERNOP:
	case AARCH64_SIMD_QUADOP:
	case AARCH64_SIMD_COMBINE:
	case AARCH64_SIMD_CONVERT:
	case AARCH64_SIMD_CREATE:
	case AARCH64_SIMD_DUP:
	case AARCH64_SIMD_DUPLANE:
	case AARCH64_SIMD_FIXCONV:
	case AARCH64_SIMD_GETLANE:
	case AARCH64_SIMD_LANEMAC:
	case AARCH64_SIMD_LANEMUL:
	case AARCH64_SIMD_LANEMULH:
	case AARCH64_SIMD_LANEMULL:
	case AARCH64_SIMD_LOGICBINOP:
	case AARCH64_SIMD_SCALARMAC:
	case AARCH64_SIMD_SCALARMUL:
	case AARCH64_SIMD_SCALARMULH:
	case AARCH64_SIMD_SCALARMULL:
	case AARCH64_SIMD_SELECT:
	case AARCH64_SIMD_SETLANE:
	case AARCH64_SIMD_SHIFTACC:
	case AARCH64_SIMD_SHIFTIMM:
	case AARCH64_SIMD_SHIFTINSERT:
	case AARCH64_SIMD_SPLIT:
	case AARCH64_SIMD_VTBL:
	case AARCH64_SIMD_VTBX:
	  {
	    int k;
	    tree return_type = void_type_node, args = void_list_node;
	    tree eltype;
	    /* Build a function type directly from the insn_data for this
	       builtin.  The build_function_type () function takes care of
	       removing duplicates for us.  */

	    for (k = insn_data[d->code].n_operands -1; k >= 0; k--)
	      {
		/* Skip an internal operand for vget_{low, high}.  */
		if (k == 2 && d->itype == AARCH64_SIMD_SPLIT)
		  continue;

		if (is_load && k == 1)
		  {
		    /* AdvSIMD load patterns always have the memory operand
		       (a DImode pointer) in the operand 1 position.  We
		       want a const pointer to the element type in that
		       position.  */
		    gcc_assert (insn_data[d->code].operand[k].mode == DImode);

		    switch (d->mode)
		      {
		      case T_V8QI:
		      case T_V16QI:
			eltype = const_intQI_pointer_node;
			break;

		      case T_V4HI:
		      case T_V8HI:
			eltype = const_intHI_pointer_node;
			break;

		      case T_V2SI:
		      case T_V4SI:
			eltype = const_intSI_pointer_node;
			break;

		      case T_V2SF:
		      case T_V4SF:
			eltype = const_float_pointer_node;
			break;

		      case T_DI:
		      case T_V2DI:
			eltype = const_intDI_pointer_node;
			break;

		      case T_DF:
		      case T_V2DF:
			eltype = const_double_pointer_node;
			break;

		      default:
			gcc_unreachable ();
		      }
		  }
		else if (is_store && k == 0)
		  {
		    /* Similarly, AdvSIMD store patterns use operand 0 as
		       the memory location to store to (a DImode pointer).
		       Use a pointer to the element type of the store in
		       that position.  */
		    gcc_assert (insn_data[d->code].operand[k].mode == DImode);

		    switch (d->mode)
		      {
		      case T_V8QI:
		      case T_V16QI:
			eltype = intQI_pointer_node;
			break;

		      case T_V4HI:
		      case T_V8HI:
			eltype = intHI_pointer_node;
			break;

		      case T_V2SI:
		      case T_V4SI:
			eltype = intSI_pointer_node;
			break;

		      case T_V2SF:
		      case T_V4SF:
			eltype = float_pointer_node;
			break;

		      case T_DI:
		      case T_V2DI:
			eltype = intDI_pointer_node;
			break;

		      case T_DF:
		      case T_V2DF:
			eltype = double_pointer_node;
			break;

		      default:
			gcc_unreachable ();
		      }
		  }
		else
		  {
		    switch (insn_data[d->code].operand[k].mode)
		      {
		      case VOIDmode:
			eltype = void_type_node;
			break;
			/* Scalars.  */
		      case QImode:
			eltype = aarch64_simd_intQI_type_node;
			break;
		      case HImode:
			eltype = aarch64_simd_intHI_type_node;
			break;
		      case SImode:
			eltype = aarch64_simd_intSI_type_node;
			break;
		      case SFmode:
			eltype = aarch64_simd_float_type_node;
			break;
		      case DFmode:
			eltype = aarch64_simd_double_type_node;
			break;
		      case DImode:
			eltype = aarch64_simd_intDI_type_node;
			break;
		      case TImode:
			eltype = intTI_type_node;
			break;
		      case EImode:
			eltype = intEI_type_node;
			break;
		      case OImode:
			eltype = intOI_type_node;
			break;
		      case CImode:
			eltype = intCI_type_node;
			break;
		      case XImode:
			eltype = intXI_type_node;
			break;
			/* 64-bit vectors.  */
		      case V8QImode:
			eltype = V8QI_type_node;
			break;
		      case V4HImode:
			eltype = V4HI_type_node;
			break;
		      case V2SImode:
			eltype = V2SI_type_node;
			break;
		      case V2SFmode:
			eltype = V2SF_type_node;
			break;
			/* 128-bit vectors.  */
		      case V16QImode:
			eltype = V16QI_type_node;
			break;
		      case V8HImode:
			eltype = V8HI_type_node;
			break;
		      case V4SImode:
			eltype = V4SI_type_node;
			break;
		      case V4SFmode:
			eltype = V4SF_type_node;
			break;
		      case V2DImode:
			eltype = V2DI_type_node;
			break;
		      case V2DFmode:
			eltype = V2DF_type_node;
			break;
		      default:
			gcc_unreachable ();
		      }
		  }

		if (k == 0 && !is_store)
		  return_type = eltype;
		else
		  args = tree_cons (NULL_TREE, eltype, args);
	      }
	    ftype = build_function_type (return_type, args);
	  }
	  break;

	case AARCH64_SIMD_RESULTPAIR:
	  {
	    switch (insn_data[d->code].operand[1].mode)
	      {
	      case V8QImode:
		ftype = void_ftype_pv8qi_v8qi_v8qi;
		break;
	      case V4HImode:
		ftype = void_ftype_pv4hi_v4hi_v4hi;
		break;
	      case V2SImode:
		ftype = void_ftype_pv2si_v2si_v2si;
		break;
	      case V2SFmode:
		ftype = void_ftype_pv2sf_v2sf_v2sf;
		break;
	      case DImode:
		ftype = void_ftype_pdi_di_di;
		break;
	      case V16QImode:
		ftype = void_ftype_pv16qi_v16qi_v16qi;
		break;
	      case V8HImode:
		ftype = void_ftype_pv8hi_v8hi_v8hi;
		break;
	      case V4SImode:
		ftype = void_ftype_pv4si_v4si_v4si;
		break;
	      case V4SFmode:
		ftype = void_ftype_pv4sf_v4sf_v4sf;
		break;
	      case V2DImode:
		ftype = void_ftype_pv2di_v2di_v2di;
		break;
	      case V2DFmode:
		ftype = void_ftype_pv2df_v2df_v2df;
		break;
	      default:
		gcc_unreachable ();
	      }
	  }
	  break;

	case AARCH64_SIMD_REINTERP:
	  {
	    /* We iterate over 6 doubleword types, then 6 quadword
	       types.  */
	    int rhs_d = d->mode % NUM_DREG_TYPES;
	    int rhs_q = (d->mode - NUM_DREG_TYPES) % NUM_QREG_TYPES;
	    switch (insn_data[d->code].operand[0].mode)
	      {
	      case V8QImode:
		ftype = reinterp_ftype_dreg[0][rhs_d];
		break;
	      case V4HImode:
		ftype = reinterp_ftype_dreg[1][rhs_d];
		break;
	      case V2SImode:
		ftype = reinterp_ftype_dreg[2][rhs_d];
		break;
	      case V2SFmode:
		ftype = reinterp_ftype_dreg[3][rhs_d];
		break;
	      case DImode:
		ftype = reinterp_ftype_dreg[4][rhs_d];
		break;
	      case DFmode:
		ftype = reinterp_ftype_dreg[5][rhs_d];
		break;
	      case V16QImode:
		ftype = reinterp_ftype_qreg[0][rhs_q];
		break;
	      case V8HImode:
		ftype = reinterp_ftype_qreg[1][rhs_q];
		break;
	      case V4SImode:
		ftype = reinterp_ftype_qreg[2][rhs_q];
		break;
	      case V4SFmode:
		ftype = reinterp_ftype_qreg[3][rhs_q];
		break;
	      case V2DImode:
		ftype = reinterp_ftype_qreg[4][rhs_q];
		break;
	      case V2DFmode:
		ftype = reinterp_ftype_qreg[5][rhs_q];
		break;
	      default:
		gcc_unreachable ();
	      }
	  }
	  break;

	default:
	  gcc_unreachable ();
	}
      gcc_assert (ftype != NULL);

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

#define SIMD_MAX_BUILTIN_ARGS 5

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
  aarch64_simd_itype itype = d->itype;
  enum insn_code icode = d->code;

  switch (itype)
    {
    case AARCH64_SIMD_UNOP:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_STOP);

    case AARCH64_SIMD_BINOP:
      {
        rtx arg2 = expand_normal (CALL_EXPR_ARG (exp, 1));
        /* Handle constants only if the predicate allows it.  */
	bool op1_const_int_p =
	  (CONST_INT_P (arg2)
	   && (*insn_data[icode].operand[2].predicate)
		(arg2, insn_data[icode].operand[2].mode));
	return aarch64_simd_expand_args
	  (target, icode, 1, exp,
	   SIMD_ARG_COPY_TO_REG,
	   op1_const_int_p ? SIMD_ARG_CONSTANT : SIMD_ARG_COPY_TO_REG,
	   SIMD_ARG_STOP);
      }

    case AARCH64_SIMD_TERNOP:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_STOP);

    case AARCH64_SIMD_QUADOP:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_STOP);
    case AARCH64_SIMD_LOAD1:
    case AARCH64_SIMD_LOADSTRUCT:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG, SIMD_ARG_STOP);

    case AARCH64_SIMD_STORE1:
    case AARCH64_SIMD_STORESTRUCT:
      return aarch64_simd_expand_args (target, icode, 0, exp,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_COPY_TO_REG, SIMD_ARG_STOP);

    case AARCH64_SIMD_REINTERP:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG, SIMD_ARG_STOP);

    case AARCH64_SIMD_CREATE:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG, SIMD_ARG_STOP);

    case AARCH64_SIMD_COMBINE:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_COPY_TO_REG, SIMD_ARG_STOP);

    case AARCH64_SIMD_GETLANE:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_CONSTANT,
				       SIMD_ARG_STOP);

    case AARCH64_SIMD_SETLANE:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_CONSTANT,
				       SIMD_ARG_STOP);

    case AARCH64_SIMD_SHIFTIMM:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_CONSTANT,
				       SIMD_ARG_STOP);

    case AARCH64_SIMD_SHIFTACC:
    case AARCH64_SIMD_SHIFTINSERT:
      return aarch64_simd_expand_args (target, icode, 1, exp,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_COPY_TO_REG,
				       SIMD_ARG_CONSTANT,
				       SIMD_ARG_STOP);

    default:
      gcc_unreachable ();
    }
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
    ? aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_##N##v2df] \
    : (AARCH64_CHECK_BUILTIN_MODE (4, S) \
	? aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_##N##v4sf] \
	: (AARCH64_CHECK_BUILTIN_MODE (2, S) \
	   ? aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_##N##v2sf] \
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
              return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_clzv4si];
            return NULL_TREE;
          }
#undef AARCH64_CHECK_BUILTIN_MODE
#define AARCH64_CHECK_BUILTIN_MODE(C, N) \
  (out_mode == N##Imode && out_n == C \
   && in_mode == N##Fmode && in_n == C)
	case BUILT_IN_LFLOOR:
	case BUILT_IN_IFLOORF:
	  {
	    tree new_tree = NULL_TREE;
	    if (AARCH64_CHECK_BUILTIN_MODE (2, D))
	      new_tree =
		aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_lfloorv2dfv2di];
	    else if (AARCH64_CHECK_BUILTIN_MODE (4, S))
	      new_tree =
		aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_lfloorv4sfv4si];
	    else if (AARCH64_CHECK_BUILTIN_MODE (2, S))
	      new_tree =
		aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_lfloorv2sfv2si];
	    return new_tree;
	  }
	case BUILT_IN_LCEIL:
	case BUILT_IN_ICEILF:
	  {
	    tree new_tree = NULL_TREE;
	    if (AARCH64_CHECK_BUILTIN_MODE (2, D))
	      new_tree =
		aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_lceilv2dfv2di];
	    else if (AARCH64_CHECK_BUILTIN_MODE (4, S))
	      new_tree =
		aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_lceilv4sfv4si];
	    else if (AARCH64_CHECK_BUILTIN_MODE (2, S))
	      new_tree =
		aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_lceilv2sfv2si];
	    return new_tree;
	  }
	case BUILT_IN_LROUND:
	case BUILT_IN_IROUNDF:
	  {
	    tree new_tree = NULL_TREE;
	    if (AARCH64_CHECK_BUILTIN_MODE (2, D))
	      new_tree =
		aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_lroundv2dfv2di];
	    else if (AARCH64_CHECK_BUILTIN_MODE (4, S))
	      new_tree =
		aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_lroundv4sfv4si];
	    else if (AARCH64_CHECK_BUILTIN_MODE (2, S))
	      new_tree =
		aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_lroundv2sfv2si];
	    return new_tree;
	  }

	default:
	  return NULL_TREE;
      }
    }

  return NULL_TREE;
}

#undef VAR1
#define VAR1(T, N, MAP, A) \
  case AARCH64_SIMD_BUILTIN_##N##A:

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

