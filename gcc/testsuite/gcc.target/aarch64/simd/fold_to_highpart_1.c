/* { dg-do compile } */
/* { dg-options "-O -march=armv9-a+bf16" } */

#include <arm_neon.h>

/* We should use the highpart instruction where doing so would avoid data
   movement instructions.  This case, where all the arguments are non-constant
   vector highparts, can be handled by either gimple_fold_builtin or combine.  */

#ifndef TEST_UN_HIGHPARTS
#define TEST_UN_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF) \
  RETTYPE test_##FN##_##SUFF (INTYPE a)		     \
  {                                                  \
    return FN##_##SUFF (vget_high_##SUFF (a));	     \
  }
#endif

#ifndef TEST_BIN_W_HIGHPARTS
#define TEST_BIN_W_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)  \
  RETTYPE test_##FN##_##SUFF (RETTYPE a, INTYPE b)	 \
  {                                                      \
    return FN##_##SUFF (a, vget_high_##SUFF (b));	 \
  }
#endif

#ifndef TEST_BIN_N_HIGHPARTS
#define TEST_BIN_N_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)    \
  RETTYPE test_##FN##_##SUFF (INTYPE a)			   \
  {                                                        \
    return FN##_##SUFF (vget_high_##SUFF (a), a[1]);	   \
  }
#endif

#ifndef TEST_TERN_N_HIGHPARTS
#define TEST_TERN_N_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)      \
  RETTYPE test_##FN##_##SUFF (RETTYPE a, INTYPE b)	      \
  {                                                           \
    return FN##_##SUFF (a, vget_high_##SUFF (b), b[1]);	      \
  }
#endif

#ifndef TEST_BIN_HIGHPARTS
#define TEST_BIN_HIGHPARTS(FN, RETTYPE, INTYPE, H_INTYPE, SUFF) \
  RETTYPE test_##FN##_##SUFF (INTYPE a, INTYPE b)		\
  {                                                             \
    return FN##_##SUFF (vget_high_##SUFF (a),			\
			vget_high_##SUFF (b));			\
  }
#endif

#ifndef TEST_TERN_HIGHPARTS
#define TEST_TERN_HIGHPARTS(FN, RETTYPE, INTYPE, H_INTYPE, SUFF)   \
  RETTYPE test_##FN##_##SUFF (RETTYPE a, INTYPE b, INTYPE c)	   \
  {                                                                \
    return FN##_##SUFF(a, vget_high_##SUFF (b),			   \
		       vget_high_##SUFF (c));			   \
  }
#endif

#define TEST_UNOP(FN) \
  TEST_UN_HIGHPARTS (FN, int16x8_t,  int8x16_t,  s8)  \
  TEST_UN_HIGHPARTS (FN, uint16x8_t, uint8x16_t, u8)  \
  TEST_UN_HIGHPARTS (FN, int32x4_t,  int16x8_t,  s16) \
  TEST_UN_HIGHPARTS (FN, uint32x4_t, uint16x8_t, u16) \
  TEST_UN_HIGHPARTS (FN, int64x2_t,  int32x4_t,  s32) \
  TEST_UN_HIGHPARTS (FN, uint64x2_t, uint32x4_t, u32)

#define TEST_BINOP(FN)						   \
  TEST_BIN_HIGHPARTS (FN, int16x8_t,  int8x16_t,  int8x8_t,   s8)  \
  TEST_BIN_HIGHPARTS (FN, uint16x8_t, uint8x16_t, uint8x8_t,  u8)  \
  TEST_BIN_HIGHPARTS (FN, int32x4_t,  int16x8_t,  int16x4_t,  s16) \
  TEST_BIN_HIGHPARTS (FN, uint32x4_t, uint16x8_t, uint16x4_t, u16) \
  TEST_BIN_HIGHPARTS (FN, int64x2_t,  int32x4_t,  int32x2_t,  s32) \
  TEST_BIN_HIGHPARTS (FN, uint64x2_t, uint32x4_t, uint32x2_t, u32)

#define TEST_BINOP_N(FN)				 \
  TEST_BIN_N_HIGHPARTS (FN, int32x4_t,  int16x8_t,  s16) \
  TEST_BIN_N_HIGHPARTS (FN, uint32x4_t, uint16x8_t, u16) \
  TEST_BIN_N_HIGHPARTS (FN, int64x2_t,  int32x4_t,  s32) \
  TEST_BIN_N_HIGHPARTS (FN, uint64x2_t, uint32x4_t, u32)

#define TEST_BINOP_W(FN)				 \
  TEST_BIN_W_HIGHPARTS (FN, int16x8_t,  int8x16_t,   s8) \
  TEST_BIN_W_HIGHPARTS (FN, uint16x8_t, uint8x16_t,  u8) \
  TEST_BIN_W_HIGHPARTS (FN, int32x4_t,  int16x8_t,  s16) \
  TEST_BIN_W_HIGHPARTS (FN, uint32x4_t, uint16x8_t, u16) \
  TEST_BIN_W_HIGHPARTS (FN, int64x2_t,  int32x4_t,  s32) \
  TEST_BIN_W_HIGHPARTS (FN, uint64x2_t, uint32x4_t, u32)

#define TEST_TERNOP_N(FN)				  \
  TEST_TERN_N_HIGHPARTS (FN, int32x4_t,  int16x8_t,  s16) \
  TEST_TERN_N_HIGHPARTS (FN, uint32x4_t, uint16x8_t, u16) \
  TEST_TERN_N_HIGHPARTS (FN, int64x2_t,  int32x4_t,  s32) \
  TEST_TERN_N_HIGHPARTS (FN, uint64x2_t, uint32x4_t, u32)

#define TEST_TERNOP(FN)                                           \
  TEST_TERN_HIGHPARTS (FN, int16x8_t,  int8x16_t,  int8x8_t,   s8)  \
  TEST_TERN_HIGHPARTS (FN, uint16x8_t, uint8x16_t, uint8x8_t,  u8)  \
  TEST_TERN_HIGHPARTS (FN, int32x4_t,  int16x8_t,  int16x4_t,  s16) \
  TEST_TERN_HIGHPARTS (FN, uint32x4_t, uint16x8_t, uint16x4_t, u16) \
  TEST_TERN_HIGHPARTS (FN, int64x2_t,  int32x4_t,  int32x2_t,  s32) \
  TEST_TERN_HIGHPARTS (FN, uint64x2_t, uint32x4_t, uint32x2_t, u32)

#define TEST_VQDMULL                                                 \
  TEST_BIN_HIGHPARTS (vqdmull, int32x4_t, int16x8_t, int16x4_t, s16) \
  TEST_BIN_HIGHPARTS (vqdmull, int64x2_t, int32x4_t, int32x2_t, s32)

#define TEST_VQDMULL_N                                        \
  TEST_BIN_N_HIGHPARTS (vqdmull_n, int32x4_t, int16x8_t, s16) \
  TEST_BIN_N_HIGHPARTS (vqdmull_n, int64x2_t, int32x4_t, s32)

#define TEST_VQMLAL                                                   \
  TEST_TERN_HIGHPARTS (vqdmlal, int32x4_t, int16x8_t, int16x4_t, s16) \
  TEST_TERN_HIGHPARTS (vqdmlal, int64x2_t, int32x4_t, int32x2_t, s32)

#define TEST_VQMLAL_N                                          \
  TEST_TERN_N_HIGHPARTS (vqdmlal_n, int32x4_t, int16x8_t, s16) \
  TEST_TERN_N_HIGHPARTS (vqdmlal_n, int64x2_t, int32x4_t, s32)

#define TEST_VQMLSL                                                   \
  TEST_TERN_HIGHPARTS (vqdmlsl, int32x4_t, int16x8_t, int16x4_t, s16) \
  TEST_TERN_HIGHPARTS (vqdmlsl, int64x2_t, int32x4_t, int32x2_t, s32)

#define TEST_VQMLSL_N                                          \
  TEST_TERN_N_HIGHPARTS (vqdmlsl_n, int32x4_t, int16x8_t, s16) \
  TEST_TERN_N_HIGHPARTS (vqdmlsl_n, int64x2_t, int32x4_t, s32)

#define TEST_VMOVL \
  TEST_UNOP (vmovl)

#define TEST_VMULL \
  TEST_BINOP (vmull) \
  TEST_BIN_HIGHPARTS (vmull, poly16x8_t, poly8x16_t, poly8x8_t, p8)

#define TEST_VMULL_N \
  TEST_BINOP_N (vmull_n)

#define TEST_VADDL \
  TEST_BINOP (vaddl)

#define TEST_VSUBL \
  TEST_BINOP (vsubl)

#define TEST_VMLAL \
  TEST_TERNOP (vmlal)

#define TEST_VMLAL_N \
  TEST_TERNOP_N (vmlal_n)

#define TEST_VMLSL \
  TEST_TERNOP (vmlsl)

#define TEST_VMLSL_N \
  TEST_TERNOP_N (vmlsl_n)

#define TEST_VABDL \
  TEST_BINOP (vabdl)

#define TEST_VABAL \
  TEST_TERNOP (vabal)

#define TEST_VSUBW \
  TEST_BINOP_W (vsubw)

#define TEST_VADDW \
  TEST_BINOP_W (vaddw)

/*
** test_vmovl_s8:
**	sxtl2	v0\.8h, v0\.16b
**	ret
*/

/*
** test_vmovl_u8:
**	uxtl2	v0\.8h, v0\.16b
**	ret
*/

/*
** test_vmovl_s16:
**	sxtl2	v0\.4s, v0\.8h
**	ret
*/

/*
** test_vmovl_u16:
**	uxtl2	v0\.4s, v0\.8h
**	ret
*/

/*
** test_vmovl_s32:
**	sxtl2	v0\.2d, v0\.4s
**	ret
*/

/*
** test_vmovl_u32:
**	uxtl2	v0\.2d, v0\.4s
**	ret
*/

TEST_VMOVL

/*
** test_vmull_s8:
**	smull2	v0\.8h, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
**	ret
*/

/*
** test_vmull_u8:
**	umull2	v0\.8h, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
**	ret
*/

/*
** test_vmull_s16:
**	smull2	v0\.4s, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
**	ret
*/

/*
** test_vmull_u16:
**	umull2	v0\.4s, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
**	ret
*/

/*
** test_vmull_s32:
**	smull2	v0\.2d, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
**	ret
*/

/*
** test_vmull_u32:
**	umull2	v0\.2d, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
**	ret
*/

/*
** test_vmull_p8:
**	pmull2	v0\.8h, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
**	ret
*/

TEST_VMULL

/*
** test_vmull_n_s16:
**	smull2	v0\.4s, v0\.8h, v0\.h\[[0-7]\]
**	ret
*/

/*
** test_vmull_n_u16:
**	umull2	v0\.4s, v0\.8h, v0\.h\[[0-7]\]
**	ret
*/

/*
** test_vmull_n_s32:
**	smull2	v0\.2d, v0\.4s, v0\.s\[[0-3]\]
**	ret
*/

/*
** test_vmull_n_u32:
**	umull2	v0\.2d, v0\.4s, v0\.s\[[0-3]\]
**	ret
*/

TEST_VMULL_N

/*
** test_vaddl_s8:
**	saddl2	v0\.8h, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
**	ret
*/

/*
** test_vaddl_u8:
**	uaddl2	v0\.8h, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
**	ret
*/

/*
** test_vaddl_s16:
**	saddl2	v0\.4s, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
**	ret
*/

/*
** test_vaddl_u16:
**	uaddl2	v0\.4s, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
**	ret
*/

/*
** test_vaddl_s32:
**	saddl2	v0\.2d, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
**	ret
*/

/*
** test_vaddl_u32:
**	uaddl2	v0\.2d, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
**	ret
*/

TEST_VADDL

/*
** test_vsubl_s8:
**	ssubl2	v0\.8h, v0\.16b, v1\.16b
**	ret
*/

/*
** test_vsubl_u8:
**	usubl2	v0\.8h, v0\.16b, v1\.16b
**	ret
*/

/*
** test_vsubl_s16:
**	ssubl2	v0\.4s, v0\.8h, v1\.8h
**	ret
*/

/*
** test_vsubl_u16:
**	usubl2	v0\.4s, v0\.8h, v1\.8h
**	ret
*/

/*
** test_vsubl_s32:
**	ssubl2	v0\.2d, v0\.4s, v1\.4s
**	ret
*/

/*
** test_vsubl_u32:
**	usubl2	v0\.2d, v0\.4s, v1\.4s
**	ret
*/

TEST_VSUBL

/*
** test_vabal_s8:
**	sabal2	v0\.8h, (v1\.16b, v2\.16b|v2\.16b, v1\.16b)
**	ret
*/

/*
** test_vabal_u8:
**	uabal2	v0\.8h, (v1\.16b, v2\.16b|v2\.16b, v1\.16b)
**	ret
*/

/*
** test_vabal_s16:
**	sabal2	v0\.4s, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
**	ret
*/

/*
** test_vabal_u16:
**	uabal2	v0\.4s, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
**	ret
*/

/*
** test_vabal_s32:
**	sabal2	v0\.2d, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
**	ret
*/

/*
** test_vabal_u32:
**	uabal2	v0\.2d, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
**	ret
*/

TEST_VABAL

/*
** test_vsubw_s8:
**	ssubw2	v0\.8h, v0\.8h, v1\.16b
**	ret
*/

/*
** test_vsubw_u8:
**	usubw2	v0\.8h, v0\.8h, v1\.16b
**	ret
*/

/*
** test_vsubw_s16:
**	ssubw2	v0\.4s, v0\.4s, v1\.8h
**	ret
*/

/*
** test_vsubw_u16:
**	usubw2	v0\.4s, v0\.4s, v1\.8h
**	ret
*/

/*
** test_vsubw_s32:
**	ssubw2	v0\.2d, v0\.2d, v1\.4s
**	ret
*/

/*
** test_vsubw_u32:
**	usubw2	v0\.2d, v0\.2d, v1\.4s
**	ret
*/

TEST_VSUBW

/*
** test_vaddw_s8:
**	saddw2	v0\.8h, v0\.8h, v1\.16b
**	ret
*/

/*
** test_vaddw_u8:
**	uaddw2	v0\.8h, v0\.8h, v1\.16b
**	ret
*/

/*
** test_vaddw_s16:
**	saddw2	v0\.4s, v0\.4s, v1\.8h
**	ret
*/

/*
** test_vaddw_u16:
**	uaddw2	v0\.4s, v0\.4s, v1\.8h
**	ret
*/

/*
** test_vaddw_s32:
**	saddw2	v0\.2d, v0\.2d, v1\.4s
**	ret
*/

/*
** test_vaddw_u32:
**	uaddw2	v0\.2d, v0\.2d, v1\.4s
**	ret
*/

TEST_VADDW

/*
** test_vabdl_s8:
**	sabdl2	v0\.8h, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
**	ret
*/

/*
** test_vabdl_u8:
**	uabdl2	v0\.8h, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
**	ret
*/

/*
** test_vabdl_s16:
**	sabdl2	v0\.4s, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
**	ret
*/

/*
** test_vabdl_u16:
**	uabdl2	v0\.4s, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
**	ret
*/

/*
** test_vabdl_s32:
**	sabdl2	v0\.2d, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
**	ret
*/

/*
** test_vabdl_u32:
**	uabdl2	v0\.2d, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
**	ret
*/

TEST_VABDL

/*
** test_vmlal_s8:
**	smlal2	v0\.8h, (v1\.16b, v2\.16b|v2\.16b, v1\.16b)
**	ret
*/

/*
** test_vmlal_u8:
**	umlal2	v0\.8h, (v1\.16b, v2\.16b|v2\.16b, v1\.16b)
**	ret
*/

/*
** test_vmlal_s16:
**	smlal2	v0\.4s, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
**	ret
*/

/*
** test_vmlal_u16:
**	umlal2	v0\.4s, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
**	ret
*/

/*
** test_vmlal_s32:
**	smlal2	v0\.2d, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
**	ret
*/

/*
** test_vmlal_u32:
**	umlal2	v0\.2d, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
**	ret
*/

TEST_VMLAL

/*
** test_vmlal_n_s16:
**	smlal2	v0\.4s, v1\.8h, v1\.h\[[0-7]\]
**	ret
*/

/*
** test_vmlal_n_u16:
**	umlal2	v0\.4s, v1\.8h, v1\.h\[[0-7]\]
**	ret
*/

/*
** test_vmlal_n_s32:
**	smlal2	v0\.2d, v1\.4s, v1\.s\[[0-3]\]
**	ret
*/

/*
** test_vmlal_n_u32:
**	umlal2	v0\.2d, v1\.4s, v1\.s\[[0-3]\]
**	ret
*/

TEST_VMLAL_N

/*
** test_vmlsl_s8:
**	smlsl2	v0\.8h, v1\.16b, v2\.16b
**	ret
*/

/*
** test_vmlsl_u8:
**	umlsl2	v0\.8h, v1\.16b, v2\.16b
**	ret
*/

/*
** test_vmlsl_s16:
**	smlsl2	v0\.4s, v1\.8h, v2\.8h
**	ret
*/

/*
** test_vmlsl_u16:
**	umlsl2	v0\.4s, v1\.8h, v2\.8h
**	ret
*/

/*
** test_vmlsl_s32:
**	smlsl2	v0\.2d, v1\.4s, v2\.4s
**	ret
*/

/*
** test_vmlsl_u32:
**	umlsl2	v0\.2d, v1\.4s, v2\.4s
**	ret
*/

TEST_VMLSL

/*
** test_vmlsl_n_s16:
**	smlsl2	v0\.4s, v1\.8h, v1\.h\[[0-7]\]
**	ret
*/

/*
** test_vmlsl_n_u16:
**	umlsl2	v0\.4s, v1\.8h, v1\.h\[[0-7]\]
**	ret
*/

/*
** test_vmlsl_n_s32:
**	smlsl2	v0\.2d, v1\.4s, v1\.s\[[0-3]\]
**	ret
*/

/*
** test_vmlsl_n_u32:
**	umlsl2	v0\.2d, v1\.4s, v1\.s\[[0-3]\]
**	ret
*/

TEST_VMLSL_N

/*
** test_vqdmull_s16:
**	sqdmull2	v0\.4s, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
**	ret
*/

/*
** test_vqdmull_s32:
**	sqdmull2	v0\.2d, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
**	ret
*/

TEST_VQDMULL

/*
** test_vqdmull_n_s16:
**	sqdmull2	v0\.4s, v0\.8h, v0\.h\[[0-7]\]
**	ret
*/

/*
** test_vqdmull_n_s32:
**	sqdmull2	v0\.2d, v0\.4s, v0\.s\[[0-3]\]
**	ret
*/

TEST_VQDMULL_N

/*
** test_vqdmlal_s16:
**	sqdmlal2	v0\.4s, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
**	ret
*/

/*
** test_vqdmlal_s32:
**	sqdmlal2	v0\.2d, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
**	ret
*/

TEST_VQMLAL

/*
** test_vqdmlal_n_s16:
**	sqdmlal2	v0\.4s, v1\.8h, v1\.h\[[0-7]\]
**	ret
*/

/*
** test_vqdmlal_n_s32:
**	sqdmlal2	v0\.2d, v1\.4s, v1\.s\[[0-3]\]
**	ret
*/

TEST_VQMLAL_N

/*
** test_vqdmlsl_s16:
**	sqdmlsl2	v0\.4s, v1\.8h, v2\.8h
**	ret
*/

/*
** test_vqdmlsl_s32:
**	sqdmlsl2	v0\.2d, v1\.4s, v2\.4s
**	ret
*/

TEST_VQMLSL

/*
** test_vqdmlsl_n_s16:
**	sqdmlsl2	v0\.4s, v1\.8h, v1\.h\[[0-7]\]
**	ret
*/

/*
** test_vqdmlsl_n_s32:
**	sqdmlsl2	v0\.2d, v1\.4s, v1\.s\[[0-3]\]
**	ret
*/

TEST_VQMLSL_N

/* { dg-final { check-function-bodies "**" ""} } */
