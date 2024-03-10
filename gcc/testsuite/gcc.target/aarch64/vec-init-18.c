/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

int16x8_t foo(int16_t x, int y)
{
  int16x8_t v = (int16x8_t) {x, y, x, y, x, y, x, y}; 
  return v;
}

int16x8_t foo2(int16_t x) 
{
  int16x8_t v = (int16x8_t) {x, 1, x, 1, x, 1, x, 1}; 
  return v;
}

/* { dg-final { scan-assembler-times {\tdup\tv[0-9]+\.4h, w[0-9]+} 3 } } */
/* { dg-final { scan-assembler {\tmovi\tv[0-9]+\.4h, 0x1} } } */
/* { dg-final { scan-assembler-times {\tzip1\tv[0-9]+\.8h, v[0-9]+\.8h, v[0-9]+\.8h} 2 } } */
