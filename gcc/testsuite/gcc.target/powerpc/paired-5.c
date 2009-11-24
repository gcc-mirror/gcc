/* { dg-do compile { target { powerpc-*-linux*paired* && ilp32 } } } */
/* { dg-options "-mpaired -ffinite-math-only" } */

/* Test PowerPC PAIRED extensions.  */

#include <paired.h>
#include <stdlib.h>

static float out[2] __attribute__ ((aligned (8)));

vector float b = { 3.0, 5.0 };
vector float c = { 2.0, 5.0 };

vector float a = { 0.0, 0.0 };
void
test_api ()
{

  if (paired_cmpu1_eq (b, c))
    {
      a = paired_add (b, c);
      paired_stx (a, 0, out);
    }
  if ((out[0] != 5.0) || (out[1] != 10.0))
    abort ();
}

int
main ()
{
  test_api ();
  return (0);
}

