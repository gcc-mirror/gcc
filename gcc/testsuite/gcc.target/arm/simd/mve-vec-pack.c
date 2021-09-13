/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define FUNC(SIGN, TYPE, DSTBITS, BITS, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS (TYPE##DSTBITS##_t * __restrict__ dest, \
					  TYPE##BITS##_t *a) {		\
    int i;								\
    for (i=0; i < (256 / BITS); i++) {					\
      dest[i] = a[i];							\
    }									\
  }

FUNC(s, int, 16, 32, pack)
FUNC(u, uint, 16, 32, pack)
FUNC(s, int, 8, 16, pack)
FUNC(u, uint, 8, 16, pack)

/* { dg-final { scan-assembler-times {vmovnt\.i32\tq[0-9]+, q[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmovnb\.i32\tq[0-9]+, q[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmovnt\.i16\tq[0-9]+, q[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmovnb\.i16\tq[0-9]+, q[0-9]+} 2 } } */
/* { dg-final { scan-assembler-not {vldr\.64\td[0-9]+, \.L} } } */
