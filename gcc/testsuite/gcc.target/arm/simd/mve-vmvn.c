/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define FUNC(SIGN, TYPE, BITS, NB, OP, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS ## x ## NB (TYPE##BITS##_t * __restrict__ dest, TYPE##BITS##_t *a) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = OP a[i];						\
    }									\
}

/* vmnvq supports only 16-bit and 32-bit elements.  */
/* 64-bit vectors.  */
FUNC(s, int, 32, 2, ~, vmvn)
FUNC(u, uint, 32, 2, ~, vmvn)
FUNC(s, int, 16, 4, ~, vmvn)
FUNC(u, uint, 16, 4, ~, vmvn)
FUNC(s, int, 8, 8, ~, vmvn)
FUNC(u, uint, 8, 8, ~, vmvn)

/* 128-bit vectors.  */
FUNC(s, int, 32, 4, ~, vmvn)
FUNC(u, uint, 32, 4, ~, vmvn)
FUNC(s, int, 16, 8, ~, vmvn)
FUNC(u, uint, 16, 8, ~, vmvn)
FUNC(s, int, 8, 16, ~, vmvn)
FUNC(u, uint, 8, 16, ~, vmvn)

/* MVE has only 128-bit vectors, so we can vectorize only half of the
   functions above.  */
/* { dg-final { scan-assembler-times {vmvn\tq[0-9]+, q[0-9]+} 6 } } */
