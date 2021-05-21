/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define FUNC(SIGN, TYPE, BITS, NB)					\
  TYPE##32_t test_ ##_ ## SIGN ## BITS ## x ## NB (TYPE##BITS##_t *a) { \
    int i;								\
    TYPE##BITS##_t result = 0;						\
    for (i=0; i<NB; i++) {						\
      result += a[i];							\
    }									\
    return result;							\
}

/* 128-bit vectors.  */
FUNC(s, int, 8, 16)
FUNC(u, uint, 8, 16)
FUNC(s, int, 16, 8)
FUNC(u, uint, 16, 8)
FUNC(s, int, 32, 4)
FUNC(u, uint, 32, 4)

/* { dg-final { scan-assembler-times {vaddv\.s} 6 } } */
