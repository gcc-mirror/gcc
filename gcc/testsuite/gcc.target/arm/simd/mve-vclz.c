/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define FUNC(SIGN, TYPE, BITS, NAME)					\
  void test_ ## NAME ##_ ## SIGN ## BITS (TYPE##BITS##_t * __restrict__ dest, \
					  TYPE##BITS##_t *a) {		\
    int i;								\
    for (i=0; i < (128 / BITS); i++) {					\
      dest[i] = (TYPE##BITS##_t)__builtin_clz(a[i]);			\
    }									\
}

FUNC(s, int, 32, clz)
FUNC(u, uint, 32, clz)
FUNC(s, int, 16, clz)
FUNC(u, uint, 16, clz)
FUNC(s, int, 8, clz)
FUNC(u, uint, 8, clz)

/* 16 and 8-bit versions are not vectorized because they need pack/unpack
   patterns since __builtin_clz uses 32-bit parameter and return value.  */
/* { dg-final { scan-assembler-times {vclz\.i32\tq[0-9]+, q[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vclz\.i16\tq[0-9]+, q[0-9]+} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {vclz\.i8\tq[0-9]+, q[0-9]+} 2 { xfail *-*-* } } } */
