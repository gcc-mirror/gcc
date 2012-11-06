/* Builtins' description for AArch64 SIMD architecture.
   Copyright (C) 2011, 2012 Free Software Foundation, Inc.
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

enum aarch64_simd_builtin_type_bits
{
  T_V8QI = 0x0001,
  T_V4HI = 0x0002,
  T_V2SI = 0x0004,
  T_V2SF = 0x0008,
  T_DI = 0x0010,
  T_DF = 0x0020,
  T_V16QI = 0x0040,
  T_V8HI = 0x0080,
  T_V4SI = 0x0100,
  T_V4SF = 0x0200,
  T_V2DI = 0x0400,
  T_V2DF = 0x0800,
  T_TI = 0x1000,
  T_EI = 0x2000,
  T_OI = 0x4000,
  T_XI = 0x8000,
  T_SI = 0x10000,
  T_HI = 0x20000,
  T_QI = 0x40000
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
#define hi_UP    T_HI
#define qi_UP    T_QI

#define UP(X) X##_UP

#define T_MAX 19

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
  const int bits;
  const enum insn_code codes[T_MAX];
  const unsigned int num_vars;
  unsigned int base_fcode;
} aarch64_simd_builtin_datum;

#define CF(N, X) CODE_FOR_aarch64_##N##X

#define VAR1(T, N, A) \
  #N, AARCH64_SIMD_##T, UP (A), { CF (N, A) }, 1, 0
#define VAR2(T, N, A, B) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B), { CF (N, A), CF (N, B) }, 2, 0
#define VAR3(T, N, A, B, C) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B) | UP (C), \
  { CF (N, A), CF (N, B), CF (N, C) }, 3, 0
#define VAR4(T, N, A, B, C, D) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B) | UP (C) | UP (D), \
  { CF (N, A), CF (N, B), CF (N, C), CF (N, D) }, 4, 0
#define VAR5(T, N, A, B, C, D, E) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B) | UP (C) | UP (D) | UP (E), \
  { CF (N, A), CF (N, B), CF (N, C), CF (N, D), CF (N, E) }, 5, 0
#define VAR6(T, N, A, B, C, D, E, F) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B) | UP (C) | UP (D) | UP (E) | UP (F), \
  { CF (N, A), CF (N, B), CF (N, C), CF (N, D), CF (N, E), CF (N, F) }, 6, 0
#define VAR7(T, N, A, B, C, D, E, F, G) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B) | UP (C) | UP (D) \
			| UP (E) | UP (F) | UP (G), \
  { CF (N, A), CF (N, B), CF (N, C), CF (N, D), CF (N, E), CF (N, F), \
    CF (N, G) }, 7, 0
#define VAR8(T, N, A, B, C, D, E, F, G, H) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B) | UP (C) | UP (D) \
		| UP (E) | UP (F) | UP (G) \
		| UP (H), \
  { CF (N, A), CF (N, B), CF (N, C), CF (N, D), CF (N, E), CF (N, F), \
    CF (N, G), CF (N, H) }, 8, 0
#define VAR9(T, N, A, B, C, D, E, F, G, H, I) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B) | UP (C) | UP (D) \
		| UP (E) | UP (F) | UP (G) \
		| UP (H) | UP (I), \
  { CF (N, A), CF (N, B), CF (N, C), CF (N, D), CF (N, E), CF (N, F), \
    CF (N, G), CF (N, H), CF (N, I) }, 9, 0
#define VAR10(T, N, A, B, C, D, E, F, G, H, I, J) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B) | UP (C) | UP (D) \
		| UP (E) | UP (F) | UP (G) \
		| UP (H) | UP (I) | UP (J), \
  { CF (N, A), CF (N, B), CF (N, C), CF (N, D), CF (N, E), CF (N, F), \
    CF (N, G), CF (N, H), CF (N, I), CF (N, J) }, 10, 0

#define VAR11(T, N, A, B, C, D, E, F, G, H, I, J, K) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B) | UP (C) | UP (D) \
		| UP (E) | UP (F) | UP (G) \
		| UP (H) | UP (I) | UP (J) | UP (K), \
  { CF (N, A), CF (N, B), CF (N, C), CF (N, D), CF (N, E), CF (N, F), \
    CF (N, G), CF (N, H), CF (N, I), CF (N, J), CF (N, K) }, 11, 0

#define VAR12(T, N, A, B, C, D, E, F, G, H, I, J, K, L) \
  #N, AARCH64_SIMD_##T, UP (A) | UP (B) | UP (C) | UP (D) \
		| UP (E) | UP (F) | UP (G) \
		| UP (H) | UP (I) | UP (J) | UP (K) | UP (L), \
  { CF (N, A), CF (N, B), CF (N, C), CF (N, D), CF (N, E), CF (N, F), \
    CF (N, G), CF (N, H), CF (N, I), CF (N, J), CF (N, K), CF (N, L) }, 12, 0


/* The mode entries in the following table correspond to the "key" type of the
   instruction variant, i.e. equivalent to that which would be specified after
   the assembler mnemonic, which usually refers to the last vector operand.
   (Signed/unsigned/polynomial types are not differentiated between though, and
   are all mapped onto the same mode for a given element size.) The modes
   listed per instruction should be the same as those defined for that
   instruction's pattern in aarch64_simd.md.
   WARNING: Variants should be listed in the same increasing order as
   aarch64_simd_builtin_type_bits.  */

static aarch64_simd_builtin_datum aarch64_simd_builtin_data[] = {
  {VAR6 (CREATE, create, v8qi, v4hi, v2si, v2sf, di, df)},
  {VAR6 (GETLANE, get_lane_signed,
	  v8qi, v4hi, v2si, v16qi, v8hi, v4si)},
  {VAR7 (GETLANE, get_lane_unsigned,
	  v8qi, v4hi, v2si, v16qi, v8hi, v4si, v2di)},
  {VAR4 (GETLANE, get_lane, v2sf, di, v4sf, v2df)},
  {VAR6 (GETLANE, get_dregoi, v8qi, v4hi, v2si, v2sf, di, df)},
  {VAR6 (GETLANE, get_qregoi, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR6 (GETLANE, get_dregci, v8qi, v4hi, v2si, v2sf, di, df)},
  {VAR6 (GETLANE, get_qregci, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR6 (GETLANE, get_dregxi, v8qi, v4hi, v2si, v2sf, di, df)},
  {VAR6 (GETLANE, get_qregxi, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR6 (SETLANE, set_qregoi, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR6 (SETLANE, set_qregci, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR6 (SETLANE, set_qregxi, v16qi, v8hi, v4si, v4sf, v2di, v2df)},

  {VAR5 (REINTERP, reinterpretv8qi, v8qi, v4hi, v2si, v2sf, di)},
  {VAR5 (REINTERP, reinterpretv4hi, v8qi, v4hi, v2si, v2sf, di)},
  {VAR5 (REINTERP, reinterpretv2si, v8qi, v4hi, v2si, v2sf, di)},
  {VAR5 (REINTERP, reinterpretv2sf, v8qi, v4hi, v2si, v2sf, di)},
  {VAR5 (REINTERP, reinterpretdi, v8qi, v4hi, v2si, v2sf, di)},
  {VAR6 (REINTERP, reinterpretv16qi, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR6 (REINTERP, reinterpretv8hi, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR6 (REINTERP, reinterpretv4si, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR6 (REINTERP, reinterpretv4sf, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR6 (REINTERP, reinterpretv2di, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR6 (COMBINE, combine, v8qi, v4hi, v2si, v2sf, di, df)},

  {VAR3 (BINOP, saddl, v8qi, v4hi, v2si)},
  {VAR3 (BINOP, uaddl, v8qi, v4hi, v2si)},
  {VAR3 (BINOP, saddl2, v16qi, v8hi, v4si)},
  {VAR3 (BINOP, uaddl2, v16qi, v8hi, v4si)},
  {VAR3 (BINOP, saddw, v8qi, v4hi, v2si)},
  {VAR3 (BINOP, uaddw, v8qi, v4hi, v2si)},
  {VAR3 (BINOP, saddw2, v16qi, v8hi, v4si)},
  {VAR3 (BINOP, uaddw2, v16qi, v8hi, v4si)},
  {VAR6 (BINOP, shadd, v8qi, v4hi, v2si, v16qi, v8hi, v4si)},
  {VAR6 (BINOP, uhadd, v8qi, v4hi, v2si, v16qi, v8hi, v4si)},
  {VAR6 (BINOP, srhadd, v8qi, v4hi, v2si, v16qi, v8hi, v4si)},
  {VAR6 (BINOP, urhadd, v8qi, v4hi, v2si, v16qi, v8hi, v4si)},
  {VAR3 (BINOP, addhn, v8hi, v4si, v2di)},
  {VAR3 (BINOP, raddhn, v8hi, v4si, v2di)},
  {VAR3 (TERNOP, addhn2, v8hi, v4si, v2di)},
  {VAR3 (TERNOP, raddhn2, v8hi, v4si, v2di)},
  {VAR3 (BINOP, ssubl, v8qi, v4hi, v2si)},
  {VAR3 (BINOP, usubl, v8qi, v4hi, v2si)},
  {VAR3 (BINOP, ssubl2, v16qi, v8hi, v4si) },
  {VAR3 (BINOP, usubl2, v16qi, v8hi, v4si) },
  {VAR3 (BINOP, ssubw, v8qi, v4hi, v2si) },
  {VAR3 (BINOP, usubw, v8qi, v4hi, v2si) },
  {VAR3 (BINOP, ssubw2, v16qi, v8hi, v4si) },
  {VAR3 (BINOP, usubw2, v16qi, v8hi, v4si) },
  {VAR11 (BINOP, sqadd, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi)},
  {VAR11 (BINOP, uqadd, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi)},
  {VAR11 (BINOP, sqsub, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi)},
  {VAR11 (BINOP, uqsub, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi)},
  {VAR11 (BINOP, suqadd, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi)},
  {VAR11 (BINOP, usqadd, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi)},
  {VAR6 (UNOP, sqmovun, di, v8hi, v4si, v2di, si, hi)},
  {VAR6 (UNOP, sqmovn, di, v8hi, v4si, v2di, si, hi)},
  {VAR6 (UNOP, uqmovn, di, v8hi, v4si, v2di, si, hi)},
  {VAR10 (UNOP, sqabs, v8qi, v4hi, v2si, v16qi, v8hi, v4si, v2di, si, hi, qi)},
  {VAR10 (UNOP, sqneg, v8qi, v4hi, v2si, v16qi, v8hi, v4si, v2di, si, hi, qi)},
  {VAR2 (BINOP, pmul, v8qi, v16qi)},
  {VAR4 (TERNOP, sqdmlal, v4hi, v2si, si, hi)},
  {VAR4 (QUADOP, sqdmlal_lane, v4hi, v2si, si, hi) },
  {VAR2 (QUADOP, sqdmlal_laneq, v4hi, v2si) },
  {VAR2 (TERNOP, sqdmlal_n, v4hi, v2si) },
  {VAR2 (TERNOP, sqdmlal2, v8hi, v4si)},
  {VAR2 (QUADOP, sqdmlal2_lane, v8hi, v4si) },
  {VAR2 (QUADOP, sqdmlal2_laneq, v8hi, v4si) },
  {VAR2 (TERNOP, sqdmlal2_n, v8hi, v4si) },
  {VAR4 (TERNOP, sqdmlsl, v4hi, v2si, si, hi)},
  {VAR4 (QUADOP, sqdmlsl_lane, v4hi, v2si, si, hi) },
  {VAR2 (QUADOP, sqdmlsl_laneq, v4hi, v2si) },
  {VAR2 (TERNOP, sqdmlsl_n, v4hi, v2si) },
  {VAR2 (TERNOP, sqdmlsl2, v8hi, v4si)},
  {VAR2 (QUADOP, sqdmlsl2_lane, v8hi, v4si) },
  {VAR2 (QUADOP, sqdmlsl2_laneq, v8hi, v4si) },
  {VAR2 (TERNOP, sqdmlsl2_n, v8hi, v4si) },
  {VAR4 (BINOP, sqdmull, v4hi, v2si, si, hi)},
  {VAR4 (TERNOP, sqdmull_lane, v4hi, v2si, si, hi) },
  {VAR2 (TERNOP, sqdmull_laneq, v4hi, v2si) },
  {VAR2 (BINOP, sqdmull_n, v4hi, v2si) },
  {VAR2 (BINOP, sqdmull2, v8hi, v4si) },
  {VAR2 (TERNOP, sqdmull2_lane, v8hi, v4si) },
  {VAR2 (TERNOP, sqdmull2_laneq, v8hi, v4si) },
  {VAR2 (BINOP, sqdmull2_n, v8hi, v4si) },
  {VAR6 (BINOP, sqdmulh, v4hi, v2si, v8hi, v4si, si, hi)},
  {VAR6 (BINOP, sqrdmulh, v4hi, v2si, v8hi, v4si, si, hi)},
  {VAR8 (BINOP, sshl, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR3 (SHIFTIMM, sshll_n, v8qi, v4hi, v2si) },
  {VAR3 (SHIFTIMM, ushll_n, v8qi, v4hi, v2si) },
  {VAR3 (SHIFTIMM, sshll2_n, v16qi, v8hi, v4si) },
  {VAR3 (SHIFTIMM, ushll2_n, v16qi, v8hi, v4si) },
  {VAR8 (BINOP, ushl, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (BINOP, sshl_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (BINOP, ushl_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR11 (BINOP, sqshl, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi) },
  {VAR11 (BINOP, uqshl, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi) },
  {VAR8 (BINOP, srshl, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (BINOP, urshl, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR11 (BINOP, sqrshl, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi) },
  {VAR11 (BINOP, uqrshl, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi) },
  {VAR8 (SHIFTIMM, sshr_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTIMM, ushr_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTIMM, srshr_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTIMM, urshr_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTACC, ssra_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTACC, usra_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTACC, srsra_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTACC, ursra_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTINSERT, ssri_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTINSERT, usri_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTINSERT, ssli_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR8 (SHIFTINSERT, usli_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  {VAR11 (SHIFTIMM, sqshlu_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi) },
  {VAR11 (SHIFTIMM, sqshl_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi) },
  {VAR11 (SHIFTIMM, uqshl_n, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi) },
  { VAR6 (SHIFTIMM, sqshrun_n, di, v8hi, v4si, v2di, si, hi) },
  { VAR6 (SHIFTIMM, sqrshrun_n, di, v8hi, v4si, v2di, si, hi) },
  { VAR6 (SHIFTIMM, sqshrn_n, di, v8hi, v4si, v2di, si, hi) },
  { VAR6 (SHIFTIMM, uqshrn_n, di, v8hi, v4si, v2di, si, hi) },
  { VAR6 (SHIFTIMM, sqrshrn_n, di, v8hi, v4si, v2di, si, hi) },
  { VAR6 (SHIFTIMM, uqrshrn_n, di, v8hi, v4si, v2di, si, hi) },
  { VAR8 (BINOP, cmeq, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  { VAR8 (BINOP, cmge, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  { VAR8 (BINOP, cmgt, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  { VAR8 (BINOP, cmle, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  { VAR8 (BINOP, cmlt, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  { VAR8 (BINOP, cmhs, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  { VAR8 (BINOP, cmhi, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  { VAR8 (BINOP, cmtst, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di) },
  { VAR6 (TERNOP, sqdmulh_lane, v4hi, v2si, v8hi, v4si, si, hi) },
  { VAR6 (TERNOP, sqrdmulh_lane, v4hi, v2si, v8hi, v4si, si, hi) },
  { VAR3 (BINOP, addp, v8qi, v4hi, v2si) },
  { VAR1 (UNOP, addp, di) },
  { VAR11 (BINOP, dup_lane, v8qi, v4hi, v2si, di, v16qi, v8hi, v4si, v2di,
	  si, hi, qi) },
  { VAR3 (BINOP, fmax, v2sf, v4sf, v2df) },
  { VAR3 (BINOP, fmin, v2sf, v4sf, v2df) },
  { VAR6 (BINOP, smax, v8qi, v4hi, v2si, v16qi, v8hi, v4si) },
  { VAR6 (BINOP, smin, v8qi, v4hi, v2si, v16qi, v8hi, v4si) },
  { VAR6 (BINOP, umax, v8qi, v4hi, v2si, v16qi, v8hi, v4si) },
  { VAR6 (BINOP, umin, v8qi, v4hi, v2si, v16qi, v8hi, v4si) },
  { VAR3 (UNOP, sqrt, v2sf, v4sf, v2df) },
  {VAR12 (LOADSTRUCT, ld2,
	 v8qi, v4hi, v2si, v2sf, di, df, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR12 (LOADSTRUCT, ld3,
	 v8qi, v4hi, v2si, v2sf, di, df, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR12 (LOADSTRUCT, ld4,
	 v8qi, v4hi, v2si, v2sf, di, df, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR12 (STORESTRUCT, st2,
	 v8qi, v4hi, v2si, v2sf, di, df, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR12 (STORESTRUCT, st3,
	 v8qi, v4hi, v2si, v2sf, di, df, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
  {VAR12 (STORESTRUCT, st4,
	 v8qi, v4hi, v2si, v2sf, di, df, v16qi, v8hi, v4si, v4sf, v2di, v2df)},
};

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
#undef VAR11

#define NUM_DREG_TYPES 6
#define NUM_QREG_TYPES 6

void
init_aarch64_simd_builtins (void)
{
  unsigned int i, fcode = AARCH64_SIMD_BUILTIN_BASE;

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

  for (i = 0; i < ARRAY_SIZE (aarch64_simd_builtin_data); i++)
    {
      aarch64_simd_builtin_datum *d = &aarch64_simd_builtin_data[i];
      unsigned int j, codeidx = 0;

      d->base_fcode = fcode;

      for (j = 0; j < T_MAX; j++)
	{
	  const char *const modenames[] = {
	    "v8qi", "v4hi", "v2si", "v2sf", "di", "df",
	    "v16qi", "v8hi", "v4si", "v4sf", "v2di", "v2df",
	    "ti", "ei", "oi", "xi", "si", "hi", "qi"
	  };
	  char namebuf[60];
	  tree ftype = NULL;
	  enum insn_code icode;
	  int is_load = 0;
	  int is_store = 0;

	  /* Skip if particular mode not supported.  */
	  if ((d->bits & (1 << j)) == 0)
	    continue;

	  icode = d->codes[codeidx++];

	  switch (d->itype)
	    {
	    case AARCH64_SIMD_LOAD1:
	    case AARCH64_SIMD_LOAD1LANE:
	    case AARCH64_SIMD_LOADSTRUCTLANE:
	    case AARCH64_SIMD_LOADSTRUCT:
	      is_load = 1;
	      /* Fall through.  */
	    case AARCH64_SIMD_STORE1:
	    case AARCH64_SIMD_STORE1LANE:
	    case AARCH64_SIMD_STORESTRUCTLANE:
	    case AARCH64_SIMD_STORESTRUCT:
	      if (!is_load)
		is_store = 1;
	      /* Fall through.  */
	    case AARCH64_SIMD_UNOP:
	    case AARCH64_SIMD_BINOP:
	    case AARCH64_SIMD_LOGICBINOP:
	    case AARCH64_SIMD_SHIFTINSERT:
	    case AARCH64_SIMD_TERNOP:
	    case AARCH64_SIMD_QUADOP:
	    case AARCH64_SIMD_GETLANE:
	    case AARCH64_SIMD_SETLANE:
	    case AARCH64_SIMD_CREATE:
	    case AARCH64_SIMD_DUP:
	    case AARCH64_SIMD_DUPLANE:
	    case AARCH64_SIMD_SHIFTIMM:
	    case AARCH64_SIMD_SHIFTACC:
	    case AARCH64_SIMD_COMBINE:
	    case AARCH64_SIMD_SPLIT:
	    case AARCH64_SIMD_CONVERT:
	    case AARCH64_SIMD_FIXCONV:
	    case AARCH64_SIMD_LANEMUL:
	    case AARCH64_SIMD_LANEMULL:
	    case AARCH64_SIMD_LANEMULH:
	    case AARCH64_SIMD_LANEMAC:
	    case AARCH64_SIMD_SCALARMUL:
	    case AARCH64_SIMD_SCALARMULL:
	    case AARCH64_SIMD_SCALARMULH:
	    case AARCH64_SIMD_SCALARMAC:
	    case AARCH64_SIMD_SELECT:
	    case AARCH64_SIMD_VTBL:
	    case AARCH64_SIMD_VTBX:
	      {
		int k;
		tree return_type = void_type_node, args = void_list_node;

		/* Build a function type directly from the insn_data for this
		   builtin.  The build_function_type() function takes care of
		   removing duplicates for us.  */
		for (k = insn_data[icode].n_operands - 1; k >= 0; k--)
		  {
		    tree eltype;

		    /* Skip an internal operand for vget_{low, high}.  */
		    if (k == 2 && d->itype == AARCH64_SIMD_SPLIT)
		      continue;

		    if (is_load && k == 1)
		      {
			/* AdvSIMD load patterns always have the memory operand
			   (a DImode pointer) in the operand 1 position.  We
			   want a const pointer to the element type in that
			   position.  */
			gcc_assert (insn_data[icode].operand[k].mode ==
				    DImode);

			switch (1 << j)
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
			gcc_assert (insn_data[icode].operand[k].mode ==
				    DImode);

			switch (1 << j)
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
			switch (insn_data[icode].operand[k].mode)
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
		switch (insn_data[icode].operand[1].mode)
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
		int rhs_d = j % NUM_DREG_TYPES;
		int rhs_q = (j - NUM_DREG_TYPES) % NUM_QREG_TYPES;
		switch (insn_data[icode].operand[0].mode)
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
		    d->name, modenames[j]);

	  add_builtin_function (namebuf, ftype, fcode++, BUILT_IN_MD, NULL,
				NULL_TREE);
	}
    }
}

static int
aarch64_simd_builtin_compare (const void *a, const void *b)
{
  const aarch64_simd_builtin_datum *const key =
    (const aarch64_simd_builtin_datum *) a;
  const aarch64_simd_builtin_datum *const memb =
    (const aarch64_simd_builtin_datum *) b;
  unsigned int soughtcode = key->base_fcode;

  if (soughtcode >= memb->base_fcode
      && soughtcode < memb->base_fcode + memb->num_vars)
    return 0;
  else if (soughtcode < memb->base_fcode)
    return -1;
  else
    return 1;
}


static enum insn_code
locate_simd_builtin_icode (int fcode, aarch64_simd_itype * itype)
{
  aarch64_simd_builtin_datum key
    = { NULL, (aarch64_simd_itype) 0, 0, {CODE_FOR_nothing}, 0, 0};
  aarch64_simd_builtin_datum *found;
  int idx;

  key.base_fcode = fcode;
  found = (aarch64_simd_builtin_datum *)
    bsearch (&key, &aarch64_simd_builtin_data[0],
	     ARRAY_SIZE (aarch64_simd_builtin_data),
	     sizeof (aarch64_simd_builtin_data[0]),
	     aarch64_simd_builtin_compare);
  gcc_assert (found);
  idx = fcode - (int) found->base_fcode;
  gcc_assert (idx >= 0 && idx < T_MAX && idx < (int) found->num_vars);

  if (itype)
    *itype = found->itype;

  return found->codes[idx];
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
  aarch64_simd_itype itype;
  enum insn_code icode = locate_simd_builtin_icode (fcode, &itype);

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
