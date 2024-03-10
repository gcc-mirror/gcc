/* PR tree-optimization/109505 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+sve" } */

#pragma GCC aarch64 "arm_sve.h"

unsigned long
foo (unsigned long x)
{
  unsigned long y = svcntb ();
  return (x | 15) & y;
}
