/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define MXZ(F, TY)				\
  TY F##_f (TY op1)				\
  {						\
    return sv##F (svpfalse_b (), op1, 2);	\
  }

#define PRED_MXZn(F, TYPE, TY)		\
  MXZ (F##_n_##TY##_m, TYPE)			\
  MXZ (F##_n_##TY##_x, TYPE)			\
  MXZ (F##_n_##TY##_z, TYPE)

#define ALL_SIGNED_IMM(F, P)			\
  PRED_##P (F, svint8_t, s8)			\
  PRED_##P (F, svint16_t, s16)			\
  PRED_##P (F, svint32_t, s32)			\
  PRED_##P (F, svint64_t, s64)

ALL_SIGNED_IMM (asrd, MXZn)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 4 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 8 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 12 } } */
