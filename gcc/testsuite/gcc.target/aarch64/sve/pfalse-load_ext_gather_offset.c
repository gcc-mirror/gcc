/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define T1(F, RTY, TY1, TY2)					\
  RTY F##_f (const TY1 *base, TY2 offsets)			\
  {								\
    return sv##F (svpfalse_b (), base, offsets);		\
  }

#define T2(F, RTY, TY1)						\
  RTY F##_f (TY1 bases, int64_t offset)				\
  {								\
    return sv##F (svpfalse_b (), bases, offset);		\
  }

#define T5(F, RTY, TY1)						\
  RTY F##_f (TY1 bases)						\
  {								\
    return sv##F (svpfalse_b (), bases);			\
  }

#define T3(F, B, RTY, TY, TYPE)					\
  T1 (F##_gather_s##B##offset_##TY, RTY, TYPE, svint##B##_t)	\
  T1 (F##_gather_u##B##offset_##TY, RTY, TYPE, svuint##B##_t)

#define T4(F, B, RTY, TY)					\
  T2 (F##_gather_##TY##base_offset_s##B, svint##B##_t, RTY)	\
  T2 (F##_gather_##TY##base_offset_u##B, svuint##B##_t, RTY)	\
  T5 (F##_gather_##TY##base_s##B, svint##B##_t, RTY)		\
  T5 (F##_gather_##TY##base_u##B, svuint##B##_t, RTY)

#define TEST(F)							\
  T3 (F##sb, 32, svint32_t, s32, int8_t)			\
  T3 (F##sb, 32, svuint32_t, u32, int8_t)			\
  T3 (F##sb, 64, svint64_t, s64, int8_t)			\
  T3 (F##sb, 64, svuint64_t, u64, int8_t)			\
  T4 (F##sb, 32, svuint32_t, u32)				\
  T4 (F##sb, 64, svuint64_t, u64)				\
  T3 (F##sh, 32, svint32_t, s32, int16_t)			\
  T3 (F##sh, 32, svuint32_t, u32, int16_t)			\
  T3 (F##sh, 64, svint64_t, s64, int16_t)			\
  T3 (F##sh, 64, svuint64_t, u64, int16_t)			\
  T4 (F##sh, 32, svuint32_t, u32)				\
  T4 (F##sh, 64, svuint64_t, u64)				\
  T3 (F##sw, 64, svint64_t, s64, int32_t)			\
  T3 (F##sw, 64, svuint64_t, u64, int32_t)			\
  T4 (F##sw, 64, svuint64_t, u64)				\
  T3 (F##ub, 32, svint32_t, s32, uint8_t)			\
  T3 (F##ub, 32, svuint32_t, u32, uint8_t)			\
  T3 (F##ub, 64, svint64_t, s64, uint8_t)			\
  T3 (F##ub, 64, svuint64_t, u64, uint8_t)			\
  T4 (F##ub, 32, svuint32_t, u32)				\
  T4 (F##ub, 64, svuint64_t, u64)				\
  T3 (F##uh, 32, svint32_t, s32, uint16_t)			\
  T3 (F##uh, 32, svuint32_t, u32, uint16_t)			\
  T3 (F##uh, 64, svint64_t, s64, uint16_t)			\
  T3 (F##uh, 64, svuint64_t, u64, uint16_t)			\
  T4 (F##uh, 32, svuint32_t, u32)				\
  T4 (F##uh, 64, svuint64_t, u64)				\
  T3 (F##uw, 64, svint64_t, s64, uint32_t)			\
  T3 (F##uw, 64, svuint64_t, u64, uint32_t)			\
  T4 (F##uw, 64, svuint64_t, u64)				\

TEST (ld1)
TEST (ldff1)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 160 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 160 } } */
