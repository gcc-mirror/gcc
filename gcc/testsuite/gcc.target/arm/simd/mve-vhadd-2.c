/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

/* We force a cast to int64_t to enable the vectorizer when dealing with 32-bit
   inputs.  */
#define FUNC(SIGN, TYPE, BITS, OP, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS (TYPE##BITS##_t * __restrict__ dest, \
					  TYPE##BITS##_t *a, TYPE##BITS##_t *b) { \
    int i;								\
    for (i=0; i < (128 / BITS); i++) {					\
      dest[i] = ((int64_t)a[i] OP b[i] + 1) >> 1;			\
    }									\
}

FUNC(s, int, 32, +, vrhadd)
FUNC(u, uint, 32, +, vrhadd)
FUNC(s, int, 16, +, vrhadd)
FUNC(u, uint, 16, +, vrhadd)
FUNC(s, int, 8, +, vrhadd)
FUNC(u, uint, 8, +, vrhadd)

/* { dg-final { scan-assembler-times {vrhadd\.s32\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vrhadd\.u32\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vrhadd\.s16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vrhadd\.u16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vrhadd\.s8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vrhadd\.u8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
