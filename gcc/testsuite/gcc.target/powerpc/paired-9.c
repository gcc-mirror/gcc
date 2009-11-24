/* { dg-do compile { target { powerpc-*-linux*paired* && ilp32 } } } */
/* { dg-options "-mpaired -ffinite-math-only " } */

/* Test PowerPC PAIRED extensions.  */

#include <paired.h>

static float out[2] __attribute__ ((aligned (8)));
void
test_api (float y, float x)
{
  vector float c = {x, 7.0};
  vector float b = {0.0, 8.0};
  vector float a;

  a = paired_sub (b, c);
  paired_stx (a, 0, out);
}


int main ()
{
  test_api (6, 7);
  return (0); 
}
