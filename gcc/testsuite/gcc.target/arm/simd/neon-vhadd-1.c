/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

/* Since we have implemented the avg* optabs for 128-bit vectors only, use
   enough iterations to check that vectorization works as expected.  */

/* We force a cast to int64_t to enable the vectorizer when dealing with 32-bit
   inputs.  */
#define FUNC(SIGN, TYPE, BITS, OP, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS (TYPE##BITS##_t * __restrict__ dest, \
					  TYPE##BITS##_t *a, TYPE##BITS##_t *b) { \
    int i;								\
    for (i=0; i < (128 / BITS); i++) {					\
      dest[i] = ((int64_t)a[i] OP b[i]) >> 1;				\
    }									\
}

FUNC(s, int, 32, +, vhadd)
FUNC(u, uint, 32, +, vhadd)
FUNC(s, int, 16, +, vhadd)
FUNC(u, uint, 16, +, vhadd)
FUNC(s, int, 8, +, vhadd)
FUNC(u, uint, 8, +, vhadd)
  
/* { dg-final { scan-assembler-times {vhadd\.s32\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vhadd\.u32\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vhadd\.s16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vhadd\.u16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vhadd\.s8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vhadd\.u8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
