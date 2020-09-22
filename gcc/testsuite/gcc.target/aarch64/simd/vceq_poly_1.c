/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

uint64x2_t
foo (poly64x2_t a, poly64x2_t b)
{
  return vceqq_p64 (a, b);
}

/* { dg-final { scan-assembler-times "cmeq\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.2d" 1 } } */

uint64x1_t
fooz (poly64x1_t a)
{
  return vceqz_p64 (a);
}

/* { dg-final { scan-assembler-times "cmeq\\td\[0-9\]+, d\[0-9\]+, #0" 1 } } */

uint64x2_t
fooqz (poly64x2_t a)
{
  return vceqzq_p64 (a);
}

/* { dg-final { scan-assembler-times "cmeq\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, #0" 1 } } */

