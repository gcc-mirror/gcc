/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+ls64 -mgeneral-regs-only" } */

#include <arm_acle.h>

data512_t foo (void * p)
{
  return __arm_ld64b (p);
}
