/* { dg-do compile { target powerpc-*-linux*paired* } } */
/* { dg-options "-mpaired -m32 -ffinite-math-only" } */

/* Test PowerPC PAIRED extensions.  */

#include <paired.h>
#include <stdlib.h>

static float out[2] __attribute__ ((aligned (8)));

vector float b = { 3.0, 8.0 };
vector float c = { 3.0, 5.0 };

vector float a = { 0.0, 0.0 };
void
test_api ()
{

  if (paired_cmpu0_eq (b, c))
    {
      a = paired_sub (b, c);
      paired_stx (a, 0, out);
    }

  if ((out[1]) != 3.0)
    abort ();
}

int
main ()
{
  test_api ();
  return (0);
}

