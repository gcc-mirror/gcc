/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

void test_vmul_i32 (int32_t * dest, int32_t * a, int32_t * b) {
  int i;
  for (i=0; i<4; i++) {
    dest[i] = a[i] * b[i];
  }
}

void test_vmul_i32_u (uint32_t * dest, uint32_t * a, uint32_t * b) {
  int i;
  for (i=0; i<4; i++) {
    dest[i] = a[i] * b[i];
  }
}

/* { dg-final { scan-assembler-times {vmul\.i32\tq[0-9]+, q[0-9]+, q[0-9]+} 2 } } */

void test_vmul_i16 (int16_t * dest, int16_t * a, int16_t * b) {
  int i;
  for (i=0; i<8; i++) {
    dest[i] = a[i] * b[i];
  }
}

void test_vmul_i16_u (uint16_t * dest, uint16_t * a, uint16_t * b) {
  int i;
  for (i=0; i<8; i++) {
    dest[i] = a[i] * b[i];
  }
}

/* { dg-final { scan-assembler-times {vmul\.i16\tq[0-9]+, q[0-9]+, q[0-9]+} 2 } } */

void test_vmul_i8 (int8_t * dest, int8_t * a, int8_t * b) {
  int i;
  for (i=0; i<16; i++) {
    dest[i] = a[i] * b[i];
  }
}

void test_vmul_i8_u (uint8_t * dest, uint8_t * a, uint8_t * b) {
  int i;
  for (i=0; i<16; i++) {
    dest[i] = a[i] * b[i];
  }
}

/* { dg-final { scan-assembler-times {vmul\.i8\tq[0-9]+, q[0-9]+, q[0-9]+} 2 } } */

void test_vmul_f32 (float * dest, float * a, float * b) {
  int i;
  for (i=0; i<4; i++) {
    dest[i] = a[i] * b[i];
  }
}

/* { dg-final { scan-assembler-times {vmul\.f32\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */

