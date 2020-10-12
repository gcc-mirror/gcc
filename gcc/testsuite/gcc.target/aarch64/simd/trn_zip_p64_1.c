/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

poly64x2_t
foo (poly64x2_t a, poly64x2_t b)
{
  return vtrn1q_p64 (a, b);
}

poly64x2_t
foo1 (poly64x2_t a, poly64x2_t b)
{
  return vtrn2q_p64 (a, b);
}

poly64x2_t
foo2 (poly64x2_t a, poly64x2_t b)
{
  return vuzp1q_p64 (a, b);
}

poly64x2_t
foo3 (poly64x2_t a, poly64x2_t b)
{
  return vuzp2q_p64 (a, b);
}

poly64x2_t
foo4 (poly64x2_t a, poly64x2_t b)
{
  return vzip1q_p64 (a, b);
}

poly64x2_t
foo5 (poly64x2_t a, poly64x2_t b)
{
  return vzip2q_p64 (a, b);
}

/* { dg-final { scan-assembler-times {zip1\tv0.2d, v0.2d, v1.2d} 3 } } */
/* { dg-final { scan-assembler-times {zip2\tv0.2d, v0.2d, v1.2d} 3 } } */

