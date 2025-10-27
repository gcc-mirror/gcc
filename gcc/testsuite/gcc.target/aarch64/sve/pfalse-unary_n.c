/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define M(F, TY)					\
  sv##TY F##_f (sv##TY inactive, TY op)			\
  {							\
    return sv##F (inactive, svpfalse_b (), op);		\
  }
  
#define XZ(F, TY)					\
  sv##TY F##_f (TY op)					\
  {							\
    return sv##F (svpfalse_b (), op);			\
  }

#define PRED_MXZ(F, TYPE, TY)				\
  M (F##_##TY##_m, TYPE)				\
  XZ (F##_##TY##_x, TYPE)				\
  XZ (F##_##TY##_z, TYPE)

#define ALL_DATA(F, P)					\
  PRED_##P (F, uint8_t, u8)				\
  PRED_##P (F, uint16_t, u16)				\
  PRED_##P (F, uint32_t, u32)				\
  PRED_##P (F, uint64_t, u64)				\
  PRED_##P (F, int8_t, s8)				\
  PRED_##P (F, int16_t, s16)				\
  PRED_##P (F, int32_t, s32)				\
  PRED_##P (F, int64_t, s64)				\
  PRED_##P (F, float16_t, f16)				\
  PRED_##P (F, float32_t, f32)				\
  PRED_##P (F, float64_t, f64)				\
  PRED_##P (F, bfloat16_t, bf16)			\

ALL_DATA (dup, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n}  12 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 12 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmov\tz0(?:\.[bhsd])?, [wxhsd]0\n\tret\n} 12 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 36 } } */
