/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define T(F, TYPE, TY)							\
  void F##_##TY##_f (const void *base, sv##TYPE offsets)		\
  {									\
    return sv##F##_##TY##offset (svpfalse_b (), base, offsets, 0);	\
  }

#define T1(F)								\
  T (F, uint32_t, u32)							\
  T (F, uint64_t, u64)							\
  T (F, int32_t, s32)							\
  T (F, int64_t, s64)

#define ALL_PREFETCH_GATHER_OFFSET					\
  T1 (prfb_gather)							\

ALL_PREFETCH_GATHER_OFFSET

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 4 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 4 } } */
