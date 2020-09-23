/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

void
test (poly128_t *ptr, poly128_t a)
{
  vstrq_p128 (ptr, a);
}

/* { dg-final { scan-assembler-times {stp.*x2,.*x3,.*[x0]} 1 } } */
