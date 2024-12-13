/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define MXZ(F, RTY, TY)				\
  RTY F##_f (TY op1)				\
  {						\
    return sv##F (svpfalse_b (), op1, 2);	\
  }

#define PRED_MXZn(F, RTY, TYPE, TY)		\
  MXZ (F##_n_##TY##_m, RTY, TYPE)		\
  MXZ (F##_n_##TY##_x, RTY, TYPE)		\
  MXZ (F##_n_##TY##_z, RTY, TYPE)

#define ALL_SIGNED_IMM(F, P)			\
  PRED_##P (F, svuint8_t, svint8_t, s8)		\
  PRED_##P (F, svuint16_t, svint16_t, s16)	\
  PRED_##P (F, svuint32_t, svint32_t, s32)	\
  PRED_##P (F, svuint64_t, svint64_t, s64)

ALL_SIGNED_IMM (qshlu, MXZn)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 4 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 8 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 12 } } */
