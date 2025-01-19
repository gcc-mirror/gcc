/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define T2(F, RTY, TY1, TY2)					\
  RTY F##_f (TY1 *base, TY2 values)				\
  {								\
    return sv##F (svpfalse_b (), base, values);			\
  }

#define T4(F, TY, B)						\
  T2 (F##_f##B, svfloat##B##_t, float##B##_t, TY) 		\
  T2 (F##_s##B, svint##B##_t, int##B##_t, TY)	 		\
  T2 (F##_u##B, svuint##B##_t, uint##B##_t, TY)			\

#define SD_DATA(F)						\
  T4 (F##_gather_s64index, svint64_t, 64)			\
  T4 (F##_gather_u64index, svuint64_t, 64)			\
  T4 (F##_gather_u32offset, svuint32_t, 32)			\
  T4 (F##_gather_u64offset, svuint64_t, 64)			\
  T4 (F##_gather_s64offset, svint64_t, 64)			\

SD_DATA (ldnt1)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 15 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 15 } } */
