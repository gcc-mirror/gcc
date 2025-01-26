/* { dg-options "-O" } */

#include <stdint.h>
#include <arm_acle.h>

void f();
void g()
{
  int64_t x = 0;
  __arm_wsr64("tpidr_el0", x);
  f();
  __arm_wsr64("tpidr_el0", x);
}

/* { dg-final { scan-assembler-times {\tmsr\t[^,]+, xzr\n} 2 } } */
