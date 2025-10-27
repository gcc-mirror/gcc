/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define T(F, TY)					\
  void F##_f (TY *base, sv##TY data)			\
  {							\
    return sv##F (svpfalse_b (), base, data);		\
  }

#define ALL_DATA(F)					\
  T (F##_bf16, bfloat16_t)				\
  T (F##_f16, float16_t)				\
  T (F##_f32, float32_t)				\
  T (F##_f64, float64_t)				\
  T (F##_s8, int8_t)					\
  T (F##_s16, int16_t)					\
  T (F##_s32, int32_t)					\
  T (F##_s64, int64_t)					\
  T (F##_u8, uint8_t)					\
  T (F##_u16, uint16_t)					\
  T (F##_u32, uint32_t)					\
  T (F##_u64, uint64_t)

ALL_DATA (stnt1)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 12 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 12 } } */
