/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define T(F, TY1, TY2)					\
  void F##_f (TY1 *base, sv##TY2 data)			\
  {							\
    return sv##F (svpfalse_b (), base, data);		\
  }

#define D_INTEGER(F, TY)				\
  T (F##_s64, TY, int64_t)				\
  T (F##_u64, TY, uint64_t)

#define SD_INTEGER(F, TY)				\
  D_INTEGER (F, TY)					\
  T (F##_s32, TY, int32_t)				\
  T (F##_u32, TY, uint32_t)

#define HSD_INTEGER(F, TY)				\
  SD_INTEGER (F, TY)					\
  T (F##_s16, TY, int16_t)				\
  T (F##_u16, TY, uint16_t)

#define ALL_DATA(F, A)					\
  T (F##_bf16, bfloat16_t, bfloat16##A)			\
  T (F##_f16, float16_t, float16##A)			\
  T (F##_f32, float32_t, float32##A)			\
  T (F##_f64, float64_t, float64##A)			\
  T (F##_s8, int8_t, int8##A)				\
  T (F##_s16, int16_t, int16##A)			\
  T (F##_s32, int32_t, int32##A)			\
  T (F##_s64, int64_t, int64##A)			\
  T (F##_u8, uint8_t, uint8##A)				\
  T (F##_u16, uint16_t, uint16##A)			\
  T (F##_u32, uint32_t, uint32##A)			\
  T (F##_u64, uint64_t, uint64##A)			\

HSD_INTEGER (st1b, int8_t)
SD_INTEGER (st1h, int16_t)
D_INTEGER (st1w, int32_t)
ALL_DATA (st1, _t)
ALL_DATA (st2, x2_t)
ALL_DATA (st3, x3_t)
ALL_DATA (st4, x4_t)

/* FIXME: Currently, st1/2/3/4 are not folded with a pfalse
   predicate, which is the reason for the 48 missing cases below. Once
   folding is implemented for these intrinsics, the sum should be 60.  */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 12 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 60 } } */
