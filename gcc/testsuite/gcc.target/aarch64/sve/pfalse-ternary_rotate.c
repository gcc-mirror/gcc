/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define MXZ(F, TY)						\
  TY F##_f (TY op1, TY op2, TY op3)				\
  {								\
    return sv##F (svpfalse_b (), op1, op2, op3, 90);		\
  }

#define PRED_MXZ(F, TYPE, TY)					\
  MXZ (F##_##TY##_m, sv##TYPE)					\
  MXZ (F##_##TY##_x, sv##TYPE)					\
  MXZ (F##_##TY##_z, sv##TYPE)

#define ALL_FLOAT(F, P)						\
  PRED_##P (F, float16_t, f16)					\
  PRED_##P (F, float32_t, f32)					\
  PRED_##P (F, float64_t, f64)

ALL_FLOAT (cmla, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 3 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 6 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 9 } } */
