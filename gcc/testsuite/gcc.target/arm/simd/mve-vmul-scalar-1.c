/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define FUNC_IMM(SIGN, TYPE, BITS, NB, OP, NAME)			\
  void test_ ## NAME ##_ ## SIGN ## BITS ## x ## NB (TYPE##BITS##_t * __restrict__ dest, \
						     TYPE##BITS##_t *a) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = a[i] OP 5;						\
    }									\
}

/* 128-bit vectors.  */
FUNC_IMM(s, int, 32, 4, *, vmulimm)
FUNC_IMM(u, uint, 32, 4, *, vmulimm)
FUNC_IMM(s, int, 16, 8, *, vmulimm)
FUNC_IMM(u, uint, 16, 8, *, vmulimm)
FUNC_IMM(s, int, 8, 16, *, vmulimm)
FUNC_IMM(u, uint, 8, 16, *, vmulimm)

/* For the moment we do not select the T2 vmul variant operating on a scalar
   final argument.  */
/* { dg-final { scan-assembler-times {vmul\.i32\tq[0-9]+, q[0-9]+, r[0-9]+} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {vmul\.i16\tq[0-9]+, q[0-9]+, r[0-9]+} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {vmul\.i8\tq[0-9]+, q[0-9]+, r[0-9]+} 2 { xfail *-*-* } } } */

void test_vmul_f32 (float * dest, float * a, float * b) {
  int i;
  for (i=0; i<4; i++) {
    dest[i] = a[i] * b[1];
  }
}
void test_vmulimm_f32 (float * dest, float * a) {
  int i;
  for (i=0; i<4; i++) {
    dest[i] = a[i] * 5.0;
  }
}
/* { dg-final { scan-assembler-times {vmul\.f32\tq[0-9]+, q[0-9]+, r[0-9]+} 2 { xfail *-*-* } } } */

void test_vmul_f16 (__fp16 * dest, __fp16 * a, __fp16 * b) {
  int i;
  for (i=0; i<8; i++) {
    dest[i] = a[i] * b[i];
  }
}

/* Note that dest[i] = a[i] * 5.0f16 is not vectorized.  */
void test_vmulimm_f16 (__fp16 * dest, __fp16 * a) {
  int i;
  __fp16 b = 5.0f16;
  for (i=0; i<8; i++) {
    dest[i] = a[i] * b;
  }
}
/* { dg-final { scan-assembler-times {vmul\.f16\tq[0-9]+, q[0-9]+, r[0-9]+} 2 { xfail *-*-* } } } */
