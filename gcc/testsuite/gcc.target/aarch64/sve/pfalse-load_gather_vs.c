/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define T1(F, RTY, TY)						\
  RTY F##_f (TY bases)						\
  {								\
    return sv##F (svpfalse_b (), bases);			\
  }

#define T2(F, RTY, TY)						\
  RTY F##_f (TY bases, int64_t value)				\
  {								\
    return sv##F (svpfalse_b (), bases, value);			\
  }

#define T4(F, TY, TEST, B)					\
  TEST (F##_f##B, svfloat##B##_t, TY) 				\
  TEST (F##_s##B, svint##B##_t, TY) 				\
  TEST (F##_u##B, svuint##B##_t, TY)				\

#define T3(F, B)						\
  T4 (F##_gather_u##B##base, svuint##B##_t, T1, B)		\
  T4 (F##_gather_u##B##base_offset, svuint##B##_t, T2, B)	\
  T4 (F##_gather_u##B##base_index, svuint##B##_t, T2, B)

#define SD_DATA(F)						\
  T3 (F, 32)							\
  T3 (F, 64)

SD_DATA (ld1)
SD_DATA (ldff1)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 36 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 36 } } */
