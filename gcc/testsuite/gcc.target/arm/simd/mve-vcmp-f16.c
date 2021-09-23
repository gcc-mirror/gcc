/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3 -funsafe-math-optimizations" } */

#include <stdint.h>

#define NB 8

#define FUNC(OP, NAME)							\
  void test_ ## NAME ##_f (__fp16 * __restrict__ dest, __fp16 *a, __fp16 *b) { \
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

/* { dg-final { scan-assembler-times {\tvcmp.f16\teq, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f16\tne, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f16\tlt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f16\tle, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f16\tgt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f16\tge, q[0-9]+, q[0-9]+\n} 1 } } */
