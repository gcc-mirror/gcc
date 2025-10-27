/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define T1(F, RTY, TY1, TY2)					\
  RTY F##_f (const TY1 *base, TY2 indices)			\
  {								\
    return sv##F (svpfalse_b (), base, indices);		\
  }

#define T2(F, RTY, TY1)						\
  RTY F##_f (TY1 bases, int64_t index)				\
  {								\
    return sv##F (svpfalse_b (), bases, index);			\
  }

#define T3(F, B, RTY, TY, TYPE)					\
  T1 (F##_gather_s##B##index_##TY, RTY, TYPE, svint##B##_t)	\
  T1 (F##_gather_u##B##index_##TY, RTY, TYPE, svuint##B##_t)

#define T4(F, B, RTY, TY)					\
  T2 (F##_gather_##TY##base_index_s##B, svint##B##_t, RTY)	\
  T2 (F##_gather_##TY##base_index_u##B, svuint##B##_t, RTY)

#define TEST(F)							\
  T3 (F##sh, 64, svint64_t, s64, int16_t)			\
  T3 (F##sh, 64, svuint64_t, u64, int16_t)			\
  T4 (F##sh, 32, svuint32_t, u32)				\
  T4 (F##sh, 64, svuint64_t, u64)				\
  T3 (F##sw, 64, svint64_t, s64, int32_t)			\
  T3 (F##sw, 64, svuint64_t, u64, int32_t)			\
  T4 (F##sw, 64, svuint64_t, u64)				\
  T3 (F##uh, 64, svint64_t, s64, uint16_t)			\
  T3 (F##uh, 64, svuint64_t, u64, uint16_t)			\
  T4 (F##uh, 32, svuint32_t, u32)				\
  T4 (F##uh, 64, svuint64_t, u64)				\
  T3 (F##uw, 64, svint64_t, s64, uint32_t)			\
  T3 (F##uw, 64, svuint64_t, u64, uint32_t)			\
  T4 (F##uw, 64, svuint64_t, u64)				\

TEST (ldnt1)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 28 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 28 } } */
