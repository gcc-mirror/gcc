/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define T(F, RTY, TY)					\
  RTY F##_f (const TY *base)				\
  {							\
    return sv##F (svpfalse_b (), base);			\
  }

#define D_INTEGER(F, TY)				\
  T (F##_s64, svint64_t, TY)				\
  T (F##_u64, svuint64_t, TY)

#define SD_INTEGER(F, TY)				\
  D_INTEGER (F, TY)					\
  T (F##_s32, svint32_t, TY)				\
  T (F##_u32, svuint32_t, TY)

#define HSD_INTEGER(F, TY)				\
  SD_INTEGER (F, TY)					\
  T (F##_s16, svint16_t, TY)				\
  T (F##_u16, svuint16_t, TY)

#define TEST(F)						\
  HSD_INTEGER (F##sb, int8_t)				\
  SD_INTEGER (F##sh, int16_t)				\
  D_INTEGER (F##sw, int32_t)				\
  HSD_INTEGER (F##ub, uint8_t)				\
  SD_INTEGER (F##uh, uint16_t)				\
  D_INTEGER (F##uw, uint32_t)				\

TEST (ld1)
TEST (ldff1)
TEST (ldnf1)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 72 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 72 } } */
