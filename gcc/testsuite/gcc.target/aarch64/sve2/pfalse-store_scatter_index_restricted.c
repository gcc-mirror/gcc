/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define T1(F, TY1, TY2, TY3)					\
  void F##_f (TY1 *base, TY2 indices, TY3 data)			\
  {								\
    sv##F (svpfalse_b (), base, indices, data);			\
  }

#define T2(F, TY1, TY3)						\
  void F##_f (TY1 bases, int64_t index, TY3 data)		\
  {								\
    sv##F (svpfalse_b (), bases, index, data);			\
  }

#define T3(F, B, TYPE1, TY, TYPE2)				\
  T1 (F##_scatter_s##B##index_##TY, TYPE2, svint##B##_t, TYPE1)	\
  T1 (F##_scatter_u##B##index_##TY, TYPE2, svuint##B##_t, TYPE1)

#define T4(F, B, TYPE1, TY)					\
  T2 (F##_scatter_u##B##base_index_##TY, svuint##B##_t, TYPE1)

#define TEST(F)							\
  T3 (F##h, 64, svint64_t, s64, int16_t)			\
  T3 (F##h, 64, svuint64_t, u64, int16_t)			\
  T4 (F##h, 32, svuint32_t, u32)				\
  T4 (F##h, 64, svuint64_t, u64)				\
  T3 (F##w, 64, svint64_t, s64, int32_t)			\
  T3 (F##w, 64, svuint64_t, u64, int32_t)			\
  T4 (F##w, 64, svuint64_t, u64)				\

#define SD_DATA(F)						\
  T3 (F, 64, svfloat64_t, f64, float64_t)			\
  T3 (F, 64, svint64_t, s64, int64_t)				\
  T3 (F, 64, svuint64_t, u64, uint64_t)				\
  T4 (F, 32, svfloat32_t, f32)					\
  T4 (F, 32, svint32_t, s32)					\
  T4 (F, 32, svuint32_t, u32)					\
  T4 (F, 64, svfloat64_t, f64)					\
  T4 (F, 64, svint64_t, s64)					\
  T4 (F, 64, svuint64_t, u64)					\

TEST (stnt1)
SD_DATA (stnt1)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 23 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 23 } } */
