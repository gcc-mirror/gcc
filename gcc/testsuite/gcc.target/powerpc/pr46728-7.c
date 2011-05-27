/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fno-inline -fno-unroll-loops -lm -mpowerpc-gpopt" } */

#include <math.h>

extern void abort (void);

#define NVALS 6

static double
convert_it_1 (double x)
{
  return pow (x, 1.5);
}

static double
convert_it_2 (double x)
{
  return pow (x, 2.5);
}

static double
convert_it_3 (double x)
{
  return pow (x, -0.5);
}

static double
convert_it_4 (double x)
{
  return pow (x, 10.5);
}

int
main (int argc, char *argv[])
{
  double values[NVALS] = { 3.0, 1.95, 2.227, 4.0, 256.0, .0008797 };
  unsigned i;

  for (i = 0; i < NVALS; i++)
    {
      if (convert_it_1 (values[i]) != sqrt (values[i]) * __builtin_powi (values[i], 1))
	abort ();
      if (convert_it_2 (values[i]) != sqrt (values[i]) * __builtin_powi (values[i], 2))
	abort ();
      if (convert_it_3 (values[i]) != sqrt (values[i]) * __builtin_powi (values[i], -1))
	abort ();
      if (convert_it_4 (values[i]) != sqrt (values[i]) * __builtin_powi (values[i], 10))
	abort ();
    }

  return 0;
}


/* { dg-final { scan-assembler-times "sqrt" 5 { target powerpc*-*-* } } } */
/* { dg-final { scan-assembler-not "pow" { target powerpc*-*-* } } } */
