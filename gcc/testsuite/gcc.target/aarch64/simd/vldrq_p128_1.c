/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

poly128_t
test (poly128_t * p)
{
  return vldrq_p128 (p);
}

/* { dg-final { scan-assembler-times {ldp.*x0,.*x1,.*[x0]} 1 } } */

