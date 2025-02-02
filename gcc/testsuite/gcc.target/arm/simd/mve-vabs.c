/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3 -funsafe-math-optimizations -fdump-tree-optimized" } */

#include <stdint.h>
#include <arm_mve.h>

#define ABS(a) ((a < 0) ? -a : a)

#define FUNC(SIGN, TYPE, BITS, NB, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS ## x ## NB (TYPE##BITS##_t * __restrict__ dest, TYPE##BITS##_t *a) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = ABS(a[i]);						\
    }									\
}

#define FUNC_FLOAT(SIGN, TYPE, BITS, NB, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS ## x ## NB (TYPE * __restrict__ dest, TYPE *a) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = ABS(a[i]);						\
    }									\
}

/* 128-bit vectors.  */
FUNC(s, int, 32, 4, vabs)
FUNC(u, uint, 32, 4, vabs)
FUNC(s, int, 16, 8, vabs)
FUNC(u, uint, 16, 8, vabs)
FUNC(s, int, 8, 16, vabs)
FUNC(u, uint, 8, 16, vabs)
FUNC_FLOAT(f, float, 32, 4, vabs)
FUNC(f, float, 16, 8, vabs)

/* Taking the absolute value of an unsigned value is a no-op, so half of the
   integer optimizations actually generate a call to memcpy, the other ones a
   'vabs'.  */
/* { dg-final { scan-assembler-times {vabs.s[0-9]+\tq[0-9]+, q[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vabs.f[0-9]+\tq[0-9]+, q[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vldr[bhw].[0-9]+\tq[0-9]+} 5 } } */
/* { dg-final { scan-assembler-times {vstr[bhw].[0-9]+\tq[0-9]+} 5 } } */
/* { dg-final { scan-tree-dump-times "memcpy" 3 "optimized" } } */
