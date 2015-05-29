
/* { dg-do compile } */
/* { dg-options "-march=armv8-a+crypto" } */

#include "arm_neon.h"

poly128_t
test_vmull_p64 (poly64_t a, poly64_t b)
{
  return vmull_p64 (a, b);
}

/* { dg-final { scan-assembler-times "pmull\\tv" 1 } } */

poly128_t
test_vmull_high_p64 (poly64x2_t a, poly64x2_t b)
{
  return vmull_high_p64 (a, b);
}

/* { dg-final { scan-assembler-times "pmull2\\tv" 1 } } */

