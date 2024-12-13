/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define T1(F, RTY, TY1, TY2)					\
  RTY F##_f (TY1 *base, TY2 values)				\
  {								\
    return sv##F (svpfalse_b (), base, values);			\
  }

#define T3(F, TY, B)						\
  T1 (F##_f##B, svfloat##B##_t, float##B##_t, TY) 		\
  T1 (F##_s##B, svint##B##_t, int##B##_t, TY)	 		\
  T1 (F##_u##B, svuint##B##_t, uint##B##_t, TY)			\

#define T2(F, B)						\
  T3 (F##_gather_u##B##offset, svuint##B##_t, B)		\
  T3 (F##_gather_u##B##index, svuint##B##_t, B)			\
  T3 (F##_gather_s##B##offset, svint##B##_t, B)			\
  T3 (F##_gather_s##B##index, svint##B##_t, B)

#define SD_DATA(F)						\
  T2 (F, 32)							\
  T2 (F, 64)

SD_DATA (ld1)
SD_DATA (ldff1)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 48 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 48 } } */
