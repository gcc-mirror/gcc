/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define FUNC(SIGN, TYPE, BITS, NB, OP, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS ## x ## NB (TYPE##BITS##_t * __restrict__ dest, \
						     TYPE##BITS##_t *a, TYPE##BITS##_t *b) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = a[i] OP b[i];						\
    }									\
}

/* 128-bit vectors.  */
FUNC(s, int, 32, 4, -, vsub)
FUNC(u, uint, 32, 4, -, vsub)
FUNC(s, int, 16, 8, -, vsub)
FUNC(u, uint, 16, 8, -, vsub)
FUNC(s, int, 8, 16, -, vsub)
FUNC(u, uint, 8, 16, -, vsub)

/* { dg-final { scan-assembler-times {vsub\.i32\tq[0-9]+, q[0-9]+, q[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsub\.i16\tq[0-9]+, q[0-9]+, q[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsub\.i8\tq[0-9]+, q[0-9]+, q[0-9]+} 2 } } */

void test_vsub_f32 (float * dest, float * a, float * b) {
  int i;
  for (i=0; i<4; i++) {
    dest[i] = a[i] - b[i];
  }
}
/* { dg-final { scan-assembler-times {vsub\.f32\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */


void test_vsub_f16 (__fp16 * dest, __fp16 * a, __fp16 * b) {
  int i;
  for (i=0; i<8; i++) {
    dest[i] = a[i] - b[i];
  }
}

/* { dg-final { scan-assembler-times {vsub\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */

