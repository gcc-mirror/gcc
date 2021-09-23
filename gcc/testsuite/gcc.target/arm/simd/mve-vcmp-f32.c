/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3 -funsafe-math-optimizations" } */

#include <stdint.h>

#define NB 4

#define FUNC(OP, NAME)							\
  void test_ ## NAME ##_f (float * __restrict__ dest, float *a, float *b) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = a[i] OP b[i];						\
    }									\
  }

FUNC(==, vcmpeq)
FUNC(!=, vcmpne)
FUNC(<, vcmplt)
FUNC(<=, vcmple)
FUNC(>, vcmpgt)
FUNC(>=, vcmpge)

/* { dg-final { scan-assembler-times {\tvcmp.f32\teq, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f32\tne, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f32\tlt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f32\tle, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f32\tgt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f32\tge, q[0-9]+, q[0-9]+\n} 1 } } */
