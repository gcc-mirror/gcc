/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define T(F)						\
  void F##_f (const void *base)				\
  {							\
    return sv##F (svpfalse_b (), base, 0);		\
  }

#define ALL_PREFETCH					\
  T (prfb)						\
  T (prfh)						\
  T (prfw)						\
  T (prfd)

ALL_PREFETCH

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 4 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 4 } } */
