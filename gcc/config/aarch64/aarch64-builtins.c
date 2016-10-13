/* Builtins' description for AArch64 SIMD architecture.
   Copyright (C) 2011-2016 Free Software Foundation, Inc.
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
#include "function.h"
#include "basic-block.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
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

#define v8qi_UP  V8QImode
#define v4hi_UP  V4HImode
#define v4hf_UP  V4HFmode
#define v2si_UP  V2SImode
#define v2sf_UP  V2SFmode
#define v1df_UP  V1DFmode
#define di_UP    DImode
#define df_UP    DFmode
#define v16qi_UP V16QImode
#define v8hi_UP  V8HImode
#define v8hf_UP  V8HFmode
#define v4si_UP  V4SImode
#define v4sf_UP  V4SFmode
#define v2di_UP  V2DImode
#define v2df_UP  V2DFmode
#define ti_UP	 TImode
#define oi_UP	 OImode
#define ci_UP	 CImode
#define xi_UP	 XImode
#define si_UP    SImode
#define sf_UP    SFmode
#define hi_UP    HImode
#define hf_UP    HFmode
#define qi_UP    QImode
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
  /* qualifier_const | qualifier_pointer | qualifier_map_mode  */
  qualifier_const_pointer_map_mode = 0x86,
  /* Polynomial types.  */
  qualifier_poly = 0x100,
  /* Lane indices - must be in range, and flipped for bigendian.  */
  qualifier_lane_index = 0x200,
  /* Lane indices for single lane structure loads and stores.  */
  qualifier_struct_load_store_lane_index = 0x400
};

typedef struct
{
  const char *name;
  machine_mode mode;
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
aarch64_types_quadop_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none,
      qualifier_none, qualifier_lane_index };
#define TYPES_QUADOP_LANE (aarch64_types_quadop_lane_qualifiers)

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

static enum aarch64_type_qualifiers
aarch64_types_ternop_imm_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_none, qualifier_none, qualifier_immediate };
#define TYPES_SETREG (aarch64_types_ternop_imm_qualifiers)
#define TYPES_SHIFTINSERT (aarch64_types_ternop_imm_qualifiers)
#define TYPES_SHIFTACC (aarch64_types_ternop_imm_qualifiers)

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
aarch64_types_loadstruct_lane_qualifiers[SIMD_MAX_BUILTIN_ARGS]
  = { qualifier_none, qualifier_const_pointer_map_mode,
      qualifier_none, qualifier_struct_load_store_lane_index };
#define TYPES_LOADSTRUCT_LANE (aarch64_types_loadstruct_lane_qualifiers)

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
      qualifier_none, qualifier_struct_load_store_lane_index };
#define TYPES_STORESTRUCT_LANE (aarch64_types_storestruct_lane_qualifiers)

#define CF0(N, X) CODE_FOR_aarch64_##N##X
#define CF1(N, X) CODE_FOR_##N##X##1
#define CF2(N, X) CODE_FOR_##N##X##2
#define CF3(N, X) CODE_FOR_##N##X##3
#define CF4(N, X) CODE_FOR_##N##X##4
#define CF10(N, X) CODE_FOR_##N##X

#define VAR1(T, N, MAP, A) \
  {#N #A, UP (A), CF##MAP (N, A), 0, TYPES_##T},
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
#define VAR13(T, N, MAP, A, B, C, D, E, F, G, H, I, J, K, L, M) \
  VAR12 (T, N, MAP, A, B, C, D, E, F, G, H, I, J, K, L) \
  VAR1 (T, N, MAP, M)
#define VAR14(T, X, MAP, A, B, C, D, E, F, G, H, I, J, K, L, M, N) \
  VAR13 (T, X, MAP, A, B, C, D, E, F, G, H, I, J, K, L, M) \
  VAR1 (T, X, MAP, N)

#include "aarch64-builtin-iterators.h"

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

typedef struct
{
  const char *name;
  machine_mode mode;
  const enum insn_code icode;
  unsigned int fcode;
} aarch64_crc_builtin_datum;

#define CRC32_BUILTIN(N, M) \
  AARCH64_BUILTIN_##N,

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
  AARCH64_BUILTIN_MAX
};

#undef CRC32_BUILTIN
#define CRC32_BUILTIN(N, M) \
  {"__builtin_aarch64_"#N, M##mode, CODE_FOR_aarch64_##N, AARCH64_BUILTIN_##N},

static aarch64_crc_builtin_datum aarch64_crc_builtin_data[] = {
  AARCH64_CRC32_BUILTINS
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
  NULL
};

#define ENTRY(E, M, Q, G) E,
enum aarch64_simd_type
{
#include "aarch64-simd-builtin-types.def"
  ARM_NEON_H_TYPES_LAST
};
#undef ENTRY

struct aarch64_simd_type_info
{
  enum aarch64_simd_type type;

  /* Internal type name.  */
  const char *name;

  /* Internal type name(mangled).  The mangled names conform to the
     AAPCS64 (see "Procedure Call Standard for the ARM 64-bit Architecture",
     Appendix A).  To qualify for emission with the mangled names defined in
     that document, a vector type must not only be of the correct mode but also
     be of the correct internal AdvSIMD vector type (e.g. __Int8x8_t); these
     types are registered by aarch64_init_simd_builtin_types ().  In other
     words, vector types defined in other ways e.g. via vector_size attribute
     will get default mangled names.  */
  const char *mangle;

  /* Internal type.  */
  tree itype;

  /* Element type.  */
  tree eltype;

  /* Machine mode the internal type maps to.  */
  enum machine_mode mode;

  /* Qualifiers.  */
  enum aarch64_type_qualifiers q;
};

#define ENTRY(E, M, Q, G)  \
  {E, "__" #E, #G "__" #E, NULL_TREE, NULL_TREE, M##mode, qualifier_##Q},
static struct aarch64_simd_type_info aarch64_simd_types [] = {
#include "aarch64-simd-builtin-types.def"
};
#undef ENTRY

static tree aarch64_simd_intOI_type_node = NULL_TREE;
static tree aarch64_simd_intCI_type_node = NULL_TREE;
static tree aarch64_simd_intXI_type_node = NULL_TREE;

/* The user-visible __fp16 type, and a pointer to that type.  Used
   across the back-end.  */
tree aarch64_fp16_type_node = NULL_TREE;
tree aarch64_fp16_ptr_type_node = NULL_TREE;

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
  int i;
  int nelts = sizeof (aarch64_simd_types) / sizeof (aarch64_simd_types[0]);

  for (i = 0; i < nelts; i++)
    if (aarch64_simd_types[i].mode ==  TYPE_MODE (type)
	&& TYPE_NAME (type)
	&& TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	&& DECL_NAME (TYPE_NAME (type))
	&& !strcmp
	     (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))),
	      aarch64_simd_types[i].name))
      return aarch64_simd_types[i].mangle;

  return NULL;
}

const char *
aarch64_mangle_builtin_type (const_tree type)
{
  const char *mangle;
  /* Walk through all the AArch64 builtins types tables to filter out the
     incoming type.  */
  if ((mangle = aarch64_mangle_builtin_vector_type (type))
      || (mangle = aarch64_mangle_builtin_scalar_type (type)))
    return mangle;

  return NULL;
}

static tree
aarch64_simd_builtin_std_type (enum machine_mode mode,
			       enum aarch64_type_qualifiers q)
{
#define QUAL_TYPE(M)  \
  ((q == qualifier_none) ? int##M##_type_node : unsigned_int##M##_type_node);
  switch (mode)
    {
    case QImode:
      return QUAL_TYPE (QI);
    case HImode:
      return QUAL_TYPE (HI);
    case SImode:
      return QUAL_TYPE (SI);
    case DImode:
      return QUAL_TYPE (DI);
    case TImode:
      return QUAL_TYPE (TI);
    case OImode:
      return aarch64_simd_intOI_type_node;
    case CImode:
      return aarch64_simd_intCI_type_node;
    case XImode:
      return aarch64_simd_intXI_type_node;
    case HFmode:
      return aarch64_fp16_type_node;
    case SFmode:
      return float_type_node;
    case DFmode:
      return double_type_node;
    default:
      gcc_unreachable ();
    }
#undef QUAL_TYPE
}

static tree
aarch64_lookup_simd_builtin_type (enum machine_mode mode,
				  enum aarch64_type_qualifiers q)
{
  int i;
  int nelts = sizeof (aarch64_simd_types) / sizeof (aarch64_simd_types[0]);

  /* Non-poly scalar modes map to standard types not in the table.  */
  if (q != qualifier_poly && !VECTOR_MODE_P (mode))
    return aarch64_simd_builtin_std_type (mode, q);

  for (i = 0; i < nelts; i++)
    if (aarch64_simd_types[i].mode == mode
	&& aarch64_simd_types[i].q == q)
      return aarch64_simd_types[i].itype;

  return NULL_TREE;
}

static tree
aarch64_simd_builtin_type (enum machine_mode mode,
			   bool unsigned_p, bool poly_p)
{
  if (poly_p)
    return aarch64_lookup_simd_builtin_type (mode, qualifier_poly);
  else if (unsigned_p)
    return aarch64_lookup_simd_builtin_type (mode, qualifier_unsigned);
  else
    return aarch64_lookup_simd_builtin_type (mode, qualifier_none);
}
 
static void
aarch64_init_simd_builtin_types (void)
{
  int i;
  int nelts = sizeof (aarch64_simd_types) / sizeof (aarch64_simd_types[0]);
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

  for (i = 0; i < nelts; i++)
    {
      tree eltype = aarch64_simd_types[i].eltype;
      enum machine_mode mode = aarch64_simd_types[i].mode;

      if (aarch64_simd_types[i].itype == NULL)
	{
	  aarch64_simd_types[i].itype
	    = build_distinct_type_copy
	      (build_vector_type (eltype, GET_MODE_NUNITS (mode)));
	  SET_TYPE_STRUCTURAL_EQUALITY (aarch64_simd_types[i].itype);
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

static bool aarch64_simd_builtins_initialized_p = false;

void
aarch64_init_simd_builtins (void)
{
  unsigned int i, fcode = AARCH64_SIMD_PATTERN_START;

  if (aarch64_simd_builtins_initialized_p)
    return;

  aarch64_simd_builtins_initialized_p = true;

  aarch64_init_simd_builtin_types ();

  /* Strong-typing hasn't been implemented for all AdvSIMD builtin intrinsics.
     Therefore we need to preserve the old __builtin scalar types.  It can be
     removed once all the intrinsics become strongly typed using the qualifier
     system.  */
  aarch64_init_simd_builtin_scalar_types ();
 
  tree lane_check_fpr = build_function_type_list (void_type_node,
						  size_type_node,
						  size_type_node,
						  intSI_type_node,
						  NULL);
  aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_LANE_CHECK] =
      add_builtin_function ("__builtin_aarch64_im_lane_boundsi", lane_check_fpr,
			    AARCH64_SIMD_BUILTIN_LANE_CHECK, BUILT_IN_MD,
			    NULL, NULL_TREE);

  for (i = 0; i < ARRAY_SIZE (aarch64_simd_builtin_data); i++, fcode++)
    {
      bool print_type_signature_p = false;
      char type_signature[SIMD_MAX_BUILTIN_ARGS] = { 0 };
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

	  /* Skip an internal operand for vget_{low, high}.  */
	  if (qualifiers & qualifier_internal)
	    continue;

	  /* Some builtins have different user-facing types
	     for certain arguments, encoded in d->mode.  */
	  if (qualifiers & qualifier_map_mode)
	      op_mode = d->mode;

	  /* For pointers, we want a pointer to the basic type
	     of the vector.  */
	  if (qualifiers & qualifier_pointer && VECTOR_MODE_P (op_mode))
	    op_mode = GET_MODE_INNER (op_mode);

	  eltype = aarch64_simd_builtin_type
		     (op_mode,
		      (qualifiers & qualifier_unsigned) != 0,
		      (qualifiers & qualifier_poly) != 0);
	  gcc_assert (eltype != NULL);

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
	snprintf (namebuf, sizeof (namebuf), "__builtin_aarch64_%s_%s",
		  d->name, type_signature);
      else
	snprintf (namebuf, sizeof (namebuf), "__builtin_aarch64_%s",
		  d->name);

      fndecl = add_builtin_function (namebuf, ftype, fcode, BUILT_IN_MD,
				     NULL, NULL_TREE);
      aarch64_builtin_decls[fcode] = fndecl;
    }
}

static void
aarch64_init_crc32_builtins ()
{
  tree usi_type = aarch64_simd_builtin_std_type (SImode, qualifier_unsigned);
  unsigned int i = 0;

  for (i = 0; i < ARRAY_SIZE (aarch64_crc_builtin_data); ++i)
    {
      aarch64_crc_builtin_datum* d = &aarch64_crc_builtin_data[i];
      tree argtype = aarch64_simd_builtin_std_type (d->mode,
						    qualifier_unsigned);
      tree ftype = build_function_type_list (usi_type, usi_type, argtype, NULL_TREE);
      tree fndecl = add_builtin_function (d->name, ftype, d->fcode,
                                          BUILT_IN_MD, NULL, NULL_TREE);

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
  builtin_decls_data *bdd_end = bdd + (sizeof (bdda) / sizeof (builtin_decls_data));

  for (; bdd < bdd_end; bdd++)
  {
    ftype = build_function_type_list (bdd->type_node, bdd->type_node, NULL_TREE);
    fndecl = add_builtin_function (bdd->builtin_name,
      ftype, bdd->function_code, BUILT_IN_MD, NULL, NULL_TREE);
    aarch64_builtin_decls[bdd->function_code] = fndecl;
  }
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

  aarch64_init_fp16_types ();

  if (TARGET_SIMD)
    aarch64_init_simd_builtins ();

  aarch64_init_crc32_builtins ();
  aarch64_init_builtin_rsqrt ();
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
  SIMD_ARG_LANE_INDEX,
  SIMD_ARG_STRUCT_LOAD_STORE_LANE_INDEX,
  SIMD_ARG_STOP
} builtin_simd_arg;


static rtx
aarch64_simd_expand_args (rtx target, int icode, int have_retval,
			  tree exp, builtin_simd_arg *args,
			  enum machine_mode builtin_mode)
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
	  enum machine_mode mode = insn_data[icode].operand[opc].mode;
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
		  aarch64_simd_lane_bounds (op[opc], 0,
					    GET_MODE_NUNITS (builtin_mode),
					    exp);
		  /* Keep to GCC-vector-extension lane indices in the RTL.  */
		  op[opc] =
		    GEN_INT (ENDIAN_LANE_N (builtin_mode, INTVAL (op[opc])));
		}
	      goto constant_arg;

	    case SIMD_ARG_LANE_INDEX:
	      /* Must be a previous operand into which this is an index.  */
	      gcc_assert (opc > 0);
	      if (CONST_INT_P (op[opc]))
		{
		  machine_mode vmode = insn_data[icode].operand[opc - 1].mode;
		  aarch64_simd_lane_bounds (op[opc],
					    0, GET_MODE_NUNITS (vmode), exp);
		  /* Keep to GCC-vector-extension lane indices in the RTL.  */
		  op[opc] = GEN_INT (ENDIAN_LANE_N (vmode, INTVAL (op[opc])));
		}
	      /* Fall through - if the lane index isn't a constant then
		 the next case will error.  */
	      /* FALLTHRU */
	    case SIMD_ARG_CONSTANT:
constant_arg:
	      if (!(*insn_data[icode].operand[opc].predicate)
		  (op[opc], mode))
	      {
		error ("%Kargument %d must be a constant immediate",
		       exp, opc + 1 - have_retval);
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
	    error ("%Klane index must be a constant immediate", exp);
	}
      else
	error ("%Ktotal size and element size must be a non-zero constant immediate", exp);
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

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient.  */
rtx
aarch64_expand_builtin (tree exp,
		     rtx target,
		     rtx subtarget ATTRIBUTE_UNUSED,
		     machine_mode mode ATTRIBUTE_UNUSED,
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
	  op0 = force_reg (SImode, expand_normal (arg0));
	  pat = GEN_FCN (icode) (op0);
	}
      emit_insn (pat);
      return target;
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

  gcc_unreachable ();
}

tree
aarch64_builtin_vectorized_function (unsigned int fn, tree type_out,
				     tree type_in)
{
  machine_mode in_mode, out_mode;
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
  switch (fn)
    {
#undef AARCH64_CHECK_BUILTIN_MODE
#define AARCH64_CHECK_BUILTIN_MODE(C, N) \
  (out_mode == N##Fmode && out_n == C \
   && in_mode == N##Fmode && in_n == C)
    CASE_CFN_FLOOR:
      return AARCH64_FIND_FRINT_VARIANT (floor);
    CASE_CFN_CEIL:
      return AARCH64_FIND_FRINT_VARIANT (ceil);
    CASE_CFN_TRUNC:
      return AARCH64_FIND_FRINT_VARIANT (btrunc);
    CASE_CFN_ROUND:
      return AARCH64_FIND_FRINT_VARIANT (round);
    CASE_CFN_NEARBYINT:
      return AARCH64_FIND_FRINT_VARIANT (nearbyint);
    CASE_CFN_SQRT:
      return AARCH64_FIND_FRINT_VARIANT (sqrt);
#undef AARCH64_CHECK_BUILTIN_MODE
#define AARCH64_CHECK_BUILTIN_MODE(C, N) \
  (out_mode == SImode && out_n == C \
   && in_mode == N##Imode && in_n == C)
    CASE_CFN_CLZ:
      {
	if (AARCH64_CHECK_BUILTIN_MODE (4, S))
	  return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOP_clzv4si];
	return NULL_TREE;
      }
    CASE_CFN_CTZ:
      {
	if (AARCH64_CHECK_BUILTIN_MODE (2, S))
	  return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOP_ctzv2si];
	else if (AARCH64_CHECK_BUILTIN_MODE (4, S))
	  return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOP_ctzv4si];
	return NULL_TREE;
      }
#undef AARCH64_CHECK_BUILTIN_MODE
#define AARCH64_CHECK_BUILTIN_MODE(C, N) \
  (out_mode == N##Imode && out_n == C \
   && in_mode == N##Fmode && in_n == C)
    CASE_CFN_IFLOOR:
    CASE_CFN_LFLOOR:
    CASE_CFN_LLFLOOR:
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
    CASE_CFN_ICEIL:
    CASE_CFN_LCEIL:
    CASE_CFN_LLCEIL:
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
    CASE_CFN_IROUND:
    CASE_CFN_LROUND:
    CASE_CFN_LLROUND:
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
    case CFN_BUILT_IN_BSWAP16:
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
    case CFN_BUILT_IN_BSWAP32:
      if (AARCH64_CHECK_BUILTIN_MODE (2, S))
	return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOPU_bswapv2si];
      else if (AARCH64_CHECK_BUILTIN_MODE (4, S))
	return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOPU_bswapv4si];
      else
	return NULL_TREE;
    case CFN_BUILT_IN_BSWAP64:
      if (AARCH64_CHECK_BUILTIN_MODE (2, D))
	return aarch64_builtin_decls[AARCH64_SIMD_BUILTIN_UNOPU_bswapv2di];
      else
	return NULL_TREE;
    default:
      return NULL_TREE;
    }

  return NULL_TREE;
}

/* Return builtin for reciprocal square root.  */

tree
aarch64_builtin_rsqrt (unsigned int fn)
{
  if (fn == AARCH64_SIMD_BUILTIN_UNOP_sqrtv2df)
    return aarch64_builtin_decls[AARCH64_BUILTIN_RSQRT_V2DF];
  if (fn == AARCH64_SIMD_BUILTIN_UNOP_sqrtv2sf)
    return aarch64_builtin_decls[AARCH64_BUILTIN_RSQRT_V2SF];
  if (fn == AARCH64_SIMD_BUILTIN_UNOP_sqrtv4sf)
    return aarch64_builtin_decls[AARCH64_BUILTIN_RSQRT_V4SF];
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
      BUILTIN_VDQF (UNOP, abs, 2)
	return fold_build1 (ABS_EXPR, type, args[0]);
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
  gimple *stmt = gsi_stmt (*gsi);
  tree call = gimple_call_fn (stmt);
  tree fndecl;
  gimple *new_stmt = NULL;

  if (call)
    {
      fndecl = gimple_call_fndecl (stmt);
      if (fndecl)
	{
	  int fcode = DECL_FUNCTION_CODE (fndecl);
	  unsigned nargs = gimple_call_num_args (stmt);
	  tree *args = (nargs > 0
			? gimple_call_arg_ptr (stmt, 0)
			: &error_mark_node);

	  /* We use gimple's REDUC_(PLUS|MIN|MAX)_EXPRs for float, signed int
	     and unsigned int; it will distinguish according to the types of
	     the arguments to the __builtin.  */
	  switch (fcode)
	    {
	      BUILTIN_VALL (UNOP, reduc_plus_scal_, 10)
	        new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
						REDUC_PLUS_EXPR, args[0]);
		break;
	      BUILTIN_VDQIF (UNOP, reduc_smax_scal_, 10)
	      BUILTIN_VDQ_BHSI (UNOPU, reduc_umax_scal_, 10)
		new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
						REDUC_MAX_EXPR, args[0]);
		break;
	      BUILTIN_VDQIF (UNOP, reduc_smin_scal_, 10)
	      BUILTIN_VDQ_BHSI (UNOPU, reduc_umin_scal_, 10)
		new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
						REDUC_MIN_EXPR, args[0]);
		break;
	      BUILTIN_GPF (BINOP, fmulx, 0)
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
			      new_stmt =
				gimple_build_assign (gimple_call_lhs (stmt),
						     REAL_CST,
						     build_real (t0, res));
			    }
			  else
			    new_stmt =
			      gimple_build_assign (gimple_call_lhs (stmt),
						   MULT_EXPR,
						   args[0], args[1]);
			}
		      else /* a0_cst_p ^ a1_cst_p.  */
			{
			  real_value const_part = a0_cst_p
			    ? TREE_REAL_CST (args[0]) : TREE_REAL_CST (args[1]);
			  if (!real_equal (&const_part, &dconst0)
			      && !real_isinf (&const_part))
			    new_stmt =
			      gimple_build_assign (gimple_call_lhs (stmt),
						   MULT_EXPR, args[0], args[1]);
			}
		    }
		  if (new_stmt)
		    {
		      gimple_set_vuse (new_stmt, gimple_vuse (stmt));
		      gimple_set_vdef (new_stmt, gimple_vdef (stmt));
		    }
		  break;
		}
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

  new_fenv_var = create_tmp_var_raw (unsigned_type_node);
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

#include "gt-aarch64-builtins.h"
