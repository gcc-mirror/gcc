/* Test that we generate pause instruction.  */
/* { dg-do compile } */
/* { dg-options "-O2 -dp" } */
/* { dg-final { scan-assembler-times "\\*pause" 1 } } */

#include <x86intrin.h>

void foo(void)
{
  __pause();
}
