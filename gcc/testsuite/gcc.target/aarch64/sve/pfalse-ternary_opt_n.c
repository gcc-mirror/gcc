/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define MXZ(F, TY12, TY3)					\
  TY12 F##_f (TY12 op1, TY12 op2, TY3 op3)			\
  {								\
    return sv##F (svpfalse_b (), op1, op2, op3);		\
  }

#define PRED_MXZ(F, TYPE, TY)					\
  MXZ (F##_##TY##_m, sv##TYPE, sv##TYPE)			\
  MXZ (F##_n_##TY##_m, sv##TYPE, TYPE)				\
  MXZ (F##_##TY##_x, sv##TYPE, sv##TYPE)			\
  MXZ (F##_n_##TY##_x, sv##TYPE, TYPE)				\
  MXZ (F##_##TY##_z, sv##TYPE, sv##TYPE)			\
  MXZ (F##_n_##TY##_z, sv##TYPE, TYPE)

#define ALL_FLOAT(F, P)						\
  PRED_##P (F, float16_t, f16)					\
  PRED_##P (F, float32_t, f32)					\
  PRED_##P (F, float64_t, f64)

#define ALL_INTEGER(F, P)					\
  PRED_##P (F, uint8_t, u8)					\
  PRED_##P (F, uint16_t, u16)					\
  PRED_##P (F, uint32_t, u32)					\
  PRED_##P (F, uint64_t, u64)					\
  PRED_##P (F, int8_t, s8)					\
  PRED_##P (F, int16_t, s16)					\
  PRED_##P (F, int32_t, s32)					\
  PRED_##P (F, int64_t, s64)					\

#define ALL_ARITH(F, P)						\
  ALL_INTEGER (F, P)						\
  ALL_FLOAT (F, P)

ALL_ARITH (mad, MXZ)
ALL_ARITH (mla, MXZ)
ALL_ARITH (mls, MXZ)
ALL_ARITH (msb, MXZ)
ALL_FLOAT (nmad, MXZ)
ALL_FLOAT (nmla, MXZ)
ALL_FLOAT (nmls, MXZ)
ALL_FLOAT (nmsb, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 112 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 224 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 336 } } */
