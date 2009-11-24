/* { dg-do compile { target { powerpc-*-linux*paired* && ilp32 } } } */
/* { dg-options "-mpaired -ffinite-math-only " } */

/* Test PowerPC PAIRED extensions.  */

#include <paired.h>

static float out[2] __attribute__ ((aligned (8)));
void
test_api (float x)
{
  vector float c = {x, x};
  vector float b = {60.0, 88.0};
  vector float a;

  a = paired_sub (b, c);
  paired_stx (a, 0, out);
}


int main ()
{
  test_api (6);
  return (0); 
}
