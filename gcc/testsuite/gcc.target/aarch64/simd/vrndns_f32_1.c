/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

float32_t
test (float32_t a)
{
  return vrndns_f32 (a);
}

/* { dg-final { scan-assembler-times "frintn\\ts\[0-9\]+, s\[0-9\]+" 1 } } */

