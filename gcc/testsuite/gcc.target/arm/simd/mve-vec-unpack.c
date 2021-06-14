/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define FUNC(SIGN, TYPE, DSTBITS, BITS, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS (TYPE##DSTBITS##_t * __restrict__ dest, \
					  TYPE##BITS##_t *a) {		\
    int i;								\
    for (i=0; i < (128 / BITS); i++) {					\
      dest[i] = a[i];							\
    }									\
  }

FUNC(s, int, 32, 16, unpack)
FUNC(u, uint, 32, 16, unpack)
FUNC(s, int, 16, 8, unpack)
FUNC(u, uint, 16, 8, unpack)

/* { dg-final { scan-assembler-times {vmovlt\.s16 q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vmovlb\.s16 q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vmovlt\.u16 q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vmovlb\.u16 q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vmovlt\.s8 q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vmovlb\.s8 q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vmovlt\.u8 q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vmovlb\.u8 q[0-9]+, q[0-9]+} 1 } } */
