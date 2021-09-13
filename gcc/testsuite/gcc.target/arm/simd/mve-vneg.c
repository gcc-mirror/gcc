/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>
#include <arm_mve.h>

#define FUNC(SIGN, TYPE, BITS, NB, OP, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS ## x ## NB (TYPE##BITS##_t * __restrict__ dest, TYPE##BITS##_t *a) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = OP a[i];						\
    }									\
}

#define FUNC_FLOAT(SIGN, TYPE, BITS, NB, OP, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS ## x ## NB (TYPE * __restrict__ dest, TYPE *a) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = OP a[i];						\
    }									\
}

/* vmnvq supports only 16-bit and 32-bit elements.  */
/* 64-bit vectors.  */
FUNC(s, int, 32, 2, -, vneg)
FUNC(u, uint, 32, 2, -, vneg)
FUNC(s, int, 16, 4, -, vneg)
FUNC(u, uint, 16, 4, -, vneg)
FUNC(s, int, 8, 8, -, vneg)
FUNC(u, uint, 8, 8, -, vneg)
FUNC_FLOAT(f, float, 32, 2, -, vneg)
FUNC(f, float, 16, 4, -, vneg)

/* 128-bit vectors.  */
FUNC(s, int, 32, 4, -, vneg)
FUNC(u, uint, 32, 4, -, vneg)
FUNC(s, int, 16, 8, -, vneg)
FUNC(u, uint, 16, 8, -, vneg)
FUNC(s, int, 8, 16, -, vneg)
FUNC(u, uint, 8, 16, -, vneg)
FUNC_FLOAT(f, float, 32, 4, -, vneg)
FUNC(f, float, 16, 8, -, vneg)

/* MVE has only 128-bit vectors, so we can vectorize only half of the
   functions above.  */
/* { dg-final { scan-assembler-times {vneg.s[0-9]+  q[0-9]+, q[0-9]+} 6 } } */
/* { dg-final { scan-assembler-times {vneg.f[0-9]+  q[0-9]+, q[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vldr[bhw].[0-9]+\tq[0-9]+} 8 } } */
/* { dg-final { scan-assembler-times {vstr[bhw].[0-9]+\tq[0-9]+} 8 } } */
/* { dg-final { scan-assembler-not {orr\tr[0-9]+, r[0-9]+, r[0-9]+} } } */
