/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=cortex-a57" } */

typedef __Float32x2_t float32x2_t;
float32x2_t
foo1 (float32x2_t __a, float32x2_t __b, float32x2_t __c) {
  return __b * __c + __a;
}

