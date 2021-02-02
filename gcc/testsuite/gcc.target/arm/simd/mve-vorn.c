/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define FUNC(SIGN, TYPE, BITS, NB, OP, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS ## x ## NB (TYPE##BITS##_t * __restrict__ dest, TYPE##BITS##_t *a, TYPE##BITS##_t *b) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = a[i] OP b[i];						\
    }									\
}

/* 64-bit vectors.  */
FUNC(s, int, 32, 2, | ~ , vorn)
FUNC(u, uint, 32, 2, | ~ , vorn)
FUNC(s, int, 16, 4, | ~ , vorn)
FUNC(u, uint, 16, 4, | ~ , vorn)
FUNC(s, int, 8, 8, | ~ , vorn)
FUNC(u, uint, 8, 8, | ~ , vorn)

/* 128-bit vectors.  */
FUNC(s, int, 32, 4, | ~ , vorn)
FUNC(u, uint, 32, 4, | ~ , vorn)
FUNC(s, int, 16, 8, | ~ , vorn)
FUNC(u, uint, 16, 8, | ~ , vorn)
FUNC(s, int, 8, 16, | ~ , vorn)
FUNC(u, uint, 8, 16, | ~ , vorn)

/* MVE has only 128-bit vectors, so we can vectorize only half of the
   functions above.  */
/* Although float16 and float32 types are supported at assembly level,
   we cannot test them with the '| ~ ' operator, so we check only the
   integer variants.  */
/* No need to test immediates as second operand, they are covered by vorr.  */
/* { dg-final { scan-assembler-times {vorn\tq[0-9]+, q[0-9]+, q[0-9]+} 6 } } */
