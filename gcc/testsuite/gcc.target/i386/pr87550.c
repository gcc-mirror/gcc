/* PR target/87550 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <x86intrin.h>

int
foo (int x)
{
  return __rdtsc () + __rdtsc ();
}

/* { dg-final { scan-assembler-times "\trdtsc\[\n\r]" 2 } } */

int
bar (int x)
{
  return __rdpmc (0) + __rdpmc (0);
}

/* { dg-final { scan-assembler-times "\trdpmc\[\n\r]" 2 } } */
