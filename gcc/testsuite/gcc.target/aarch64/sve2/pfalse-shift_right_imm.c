/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define MXZ(F, TY)				\
  TY F##_f (TY op1)				\
  {						\
    return sv##F (svpfalse_b (), op1, 2);	\
  }

#define PRED_MXZn(F, TYPE, TY)			\
  MXZ (F##_n_##TY##_m, TYPE)			\
  MXZ (F##_n_##TY##_x, TYPE)			\
  MXZ (F##_n_##TY##_z, TYPE)

#define ALL_INTEGER_IMM(F, P)			\
  PRED_##P (F, svuint8_t, u8)			\
  PRED_##P (F, svuint16_t, u16)			\
  PRED_##P (F, svuint32_t, u32)			\
  PRED_##P (F, svuint64_t, u64)			\
  PRED_##P (F, svint8_t, s8)			\
  PRED_##P (F, svint16_t, s16)			\
  PRED_##P (F, svint32_t, s32)			\
  PRED_##P (F, svint64_t, s64)

ALL_INTEGER_IMM (rshr, MXZn)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 8 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 16 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 24 } } */
