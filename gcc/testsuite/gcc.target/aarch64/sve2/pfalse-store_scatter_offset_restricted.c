/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define T1(F, TY1, TY2, TY3)					\
  void F##_f (TY1 *base, TY2 offsets, TY3 data)		\
  {								\
    sv##F (svpfalse_b (), base, offsets, data);			\
  }

#define T2(F, TY1, TY3)						\
  void F##_f (TY1 bases, int64_t offset, TY3 data)		\
  {								\
    sv##F (svpfalse_b (), bases, offset, data);			\
  }

#define T5(F, TY1, TY3)						\
  void F##_f (TY1 bases, TY3 data)				\
  {								\
    sv##F (svpfalse_b (), bases, data);				\
  }


#define T3(F, B, TYPE1, TY, TYPE2)				\
  T1 (F##_scatter_u##B##offset_##TY, TYPE2, svuint##B##_t, TYPE1)

#define T4(F, B, TYPE1, TY)					\
  T2 (F##_scatter_u##B##base_offset_##TY, svuint##B##_t, TYPE1)	\
  T5 (F##_scatter_u##B##base_##TY, svuint##B##_t, TYPE1)

#define D_INTEGER(F, BHW)					\
  T3 (F, 64, svint64_t, s64, int##BHW##_t)			\
  T3 (F, 64, svuint64_t, u64, int##BHW##_t)			\
  T4 (F, 64, svint64_t, s64)					\
  T4 (F, 64, svuint64_t, u64)

#define SD_INTEGER(F, BHW)					\
  D_INTEGER (F, BHW)						\
  T3 (F, 32, svuint32_t, u32, int##BHW##_t)			\
  T4 (F, 32, svint32_t, s32)					\
  T4 (F, 32, svuint32_t, u32)

#define SD_DATA(F)						\
  T3 (F, 64, svint64_t, s64, int64_t)				\
  T4 (F, 32, svint32_t, s32)					\
  T4 (F, 64, svint64_t, s64)					\
  T3 (F, 32, svuint32_t, u32, uint32_t)				\
  T3 (F, 64, svuint64_t, u64, uint64_t)				\
  T4 (F, 32, svuint32_t, u32)					\
  T4 (F, 64, svuint64_t, u64)					\
  T3 (F, 32, svfloat32_t, f32, float32_t)			\
  T3 (F, 64, svfloat64_t, f64, float64_t)			\
  T4 (F, 32, svfloat32_t, f32)					\
  T4 (F, 64, svfloat64_t, f64)


SD_DATA (stnt1)
SD_INTEGER (stnt1b, 8)
SD_INTEGER (stnt1h, 16)
D_INTEGER (stnt1w, 32)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 45 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 45 } } */
