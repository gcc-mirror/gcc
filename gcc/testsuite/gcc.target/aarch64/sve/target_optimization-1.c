/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <arm_sve.h>

/* Turn off SVE overall */
#pragma GCC target("+nosve")

/* But the function turns it on again so it should work.
   Even if changing the optimization level from O1 to O2. */
int __attribute__((target ("+sve"), optimize(2)))
bar (void)
{
  svfloat32_t xseg;
  return svlen_f32(xseg);
}
