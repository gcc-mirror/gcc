/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

poly8x8_t
foo (poly8x8_t a, poly8x8_t b)
{
  return vadd_p8 (a, b);
}

poly16x4_t
foo16 (poly16x4_t a, poly16x4_t b)
{
  return vadd_p16 (a, b);
}

poly64x1_t
foo64 (poly64x1_t a, poly64x1_t b)
{
  return vadd_p64 (a, b);
}

poly8x16_t
fooq (poly8x16_t a, poly8x16_t b)
{
  return vaddq_p8 (a, b);
}

poly16x8_t
fooq16 (poly16x8_t a, poly16x8_t b)
{
  return vaddq_p16 (a, b);
}

poly64x2_t
fooq64 (poly64x2_t a, poly64x2_t b)
{
  return vaddq_p64 (a, b);
}

poly128_t
fooq128 (poly128_t a, poly128_t b)
{
  return vaddq_p128 (a, b);
}

/* { dg-final { scan-assembler-times "eor\\tv\[0-9\]+\.8b, v\[0-9\]+\.8b, v\[0-9\]+\.8b" 3 } } */
/* { dg-final { scan-assembler-times "eor\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 3 } } */
/* { dg-final { scan-assembler-times "eor\\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" 2 } } */
