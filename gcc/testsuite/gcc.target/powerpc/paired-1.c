/* { dg-do compile { target powerpc-*-linux*paired* } } */
/* { dg-options "-mpaired -m32 -ffinite-math-only " } */

/* Test PowerPC PAIRED extensions.  */

#include <paired.h>

static float in1[2] __attribute__ ((aligned (8))) =
{6.0, 7.0};
static float in2[2] __attribute__ ((aligned (8))) =
{4.0, 3.0};

static float out[2] __attribute__ ((aligned (8)));

vector float a, b, c, d;
void
test_api ()
{
  b = paired_lx (0, in1);
  c = paired_lx (0, in2);

  a = paired_sub (b, c);

  paired_stx (a, 0, out);
}

int
main ()
{
  test_api ();
  return (0);
}

