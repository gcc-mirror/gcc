/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define T(F, TYPE, TY)						\
  void F##_##TY##_f (const void *base, sv##TYPE indices)	\
  {								\
    return sv##F##_##TY##index (svpfalse_b (), base, indices, 0);\
  }

#define T1(F)							\
  T (F, uint32_t, u32)						\
  T (F, uint64_t, u64)						\
  T (F, int32_t, s32)						\
  T (F, int64_t, s64)

#define ALL_PREFETCH_GATHER_INDEX				\
  T1 (prfh_gather)						\
  T1 (prfw_gather)						\
  T1 (prfd_gather)

ALL_PREFETCH_GATHER_INDEX

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 12 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 12 } } */
