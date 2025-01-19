/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -march=armv8.2-a+sve+bf16" } */

#include <arm_sve.h>

#define T(TYPE1, TY1, TYPE2, TY2)					\
  TYPE1 cvt_##TY1##_##TY2##_x_f (TYPE2 op)				\
  {									\
    return svcvt_##TY1##_##TY2##_x (svpfalse_b (), op);			\
  }									\
  TYPE1 cvt_##TY1##_##TY2##_z_f (TYPE2 op)				\
  {									\
    return svcvt_##TY1##_##TY2##_z (svpfalse_b (), op);			\
  }									\
  TYPE1 cvt_##TY1##_##TY2##_m_f (TYPE1 inactive, TYPE2 op)		\
  {									\
    return svcvt_##TY1##_##TY2##_m (inactive, svpfalse_b (), op);	\
  }

#define SWAP(TYPE1, TY1, TYPE2, TY2)					\
  T (TYPE1, TY1, TYPE2, TY2)						\
  T (TYPE2, TY2, TYPE1, TY1)

#define TEST_ALL							\
  T (svbfloat16_t, bf16, svfloat32_t, f32)				\
  SWAP (svfloat16_t, f16, svfloat32_t, f32)				\
  SWAP (svfloat16_t, f16, svfloat64_t, f64)				\
  SWAP (svfloat32_t, f32, svfloat64_t, f64)				\
  SWAP (svint16_t, s16, svfloat16_t, f16)				\
  SWAP (svint32_t, s32, svfloat16_t, f16)				\
  SWAP (svint32_t, s32, svfloat32_t, f32)				\
  SWAP (svint32_t, s32, svfloat64_t, f64)				\
  SWAP (svint64_t, s64, svfloat16_t, f16)				\
  SWAP (svint64_t, s64, svfloat32_t, f32)				\
  SWAP (svint64_t, s64, svfloat64_t, f64)				\
  SWAP (svuint16_t, u16, svfloat16_t, f16)				\
  SWAP (svuint32_t, u32, svfloat16_t, f16)				\
  SWAP (svuint32_t, u32, svfloat32_t, f32)				\
  SWAP (svuint32_t, u32, svfloat64_t, f64)				\
  SWAP (svuint64_t, u64, svfloat16_t, f16)				\
  SWAP (svuint64_t, u64, svfloat32_t, f32)				\
  SWAP (svuint64_t, u64, svfloat64_t, f64)

TEST_ALL


/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n}  35 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 70 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 105 } } */
