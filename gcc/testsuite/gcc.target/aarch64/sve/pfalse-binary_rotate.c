/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define MXZ4(F, TYPE)						\
  TYPE F##_f (TYPE op1, TYPE op2)				\
  {								\
    return sv##F (svpfalse_b (), op1, op2, 90);	\
  }

#define PRED_MXZ(F, TYPE, TY)					\
  MXZ4 (F##_##TY##_m, TYPE)					\
  MXZ4 (F##_##TY##_x, TYPE)					\
  MXZ4 (F##_##TY##_z, TYPE)

#define ALL_FLOAT(F, P)						\
  PRED_##P (F, svfloat16_t, f16)				\
  PRED_##P (F, svfloat32_t, f32)				\
  PRED_##P (F, svfloat64_t, f64)

ALL_FLOAT (cadd, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 3 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 6 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 9 } } */
