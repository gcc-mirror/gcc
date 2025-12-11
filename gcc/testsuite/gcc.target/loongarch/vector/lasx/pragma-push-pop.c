/* { dg-do compile } */
/* { dg-options "-O2 -mlasx" } */
/* { dg-final { scan-assembler-not "xvadd\\\.w" } } */
/* { dg-final { scan-assembler "xvsll\\\.w" } } */

#include <lasxintrin.h>

extern v8i32 a, b, c;
#pragma GCC push_options
#pragma GCC target ("no-lasx")
void
test (void)
{
  a = b + c;
}
#pragma GCC pop_options

void
test1 (void)
{
   c = __builtin_lasx_xvsll_w (a, b);
}
