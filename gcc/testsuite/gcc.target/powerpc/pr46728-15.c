/* { dg-do run } */
/* { dg-skip-if "-mpowerpc-gpopt not supported" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -ffast-math -fno-inline -fno-unroll-loops -lm -mpowerpc-gpopt" } */

#include <math.h>

extern void abort (void);

#define NVALS 6

static double
convert_it_1 (double x)
{
  return pow (x, 10.0 / 3.0);
}

static double
convert_it_2 (double x)
{
  return pow (x, 11.0 / 3.0);
}

static double
convert_it_3 (double x)
{
  return pow (x, -7.0 / 3.0);
}

static double
convert_it_4 (double x)
{
  return pow (x, -8.0 / 3.0);
}

int
main (int argc, char *argv[])
{
  double values[NVALS] = { 3.0, 1.95, 2.227, 4.0, 256.0, .0008797 };
  double PREC = .999999;
  unsigned i;

  for (i = 0; i < NVALS; i++)
    {
      volatile double x, y;

      x = __builtin_powi (values[i], 3);
      y = __builtin_powi (cbrt (values[i]), 1);
      if (fabs (convert_it_1 (values[i]) / (x * y)) < PREC)
	abort ();

      x = __builtin_powi (values[i], 3);
      y = __builtin_powi (cbrt (values[i]), 2);
      if (fabs (convert_it_2 (values[i]) / (x * y)) < PREC)
	abort ();

      x = __builtin_powi (values[i], -3);
      y = __builtin_powi (cbrt (values[i]), 2);
      if (fabs (convert_it_3 (values[i]) / (x * y)) < PREC)
	abort ();

      x = __builtin_powi (values[i], -3);
      y = __builtin_powi (cbrt (values[i]), 1);
      if (fabs (convert_it_4 (values[i]) / (x * y)) < PREC)
	abort ();
    }

  return 0;
}
