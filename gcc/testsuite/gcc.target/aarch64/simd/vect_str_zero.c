/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <arm_neon.h>

void
f (uint32x4_t *p)
{
  uint32x4_t x = { 0, 0, 0, 0};
  p[1] = x;

  /* { dg-final { scan-assembler "stp\txzr, xzr," } } */
}

void
g (float32x2_t *p)
{
  float32x2_t x = {0.0, 0.0};
  p[0] = x;

  /* { dg-final { scan-assembler "str\txzr, " } } */
}
