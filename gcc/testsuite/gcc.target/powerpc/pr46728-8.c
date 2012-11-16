/* { dg-do compile } */
/* { dg-skip-if "No __builtin_cbrt" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -ffast-math -fno-inline -fno-unroll-loops -lm -mpowerpc-gpopt -fno-ident" } */

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
  unsigned i;

  for (i = 0; i < NVALS; i++)
    {
      if (convert_it_1 (values[i]) != 
	  __builtin_powi (values[i], 3) * __builtin_powi (cbrt (values[i]), 1))
	abort ();
      if (convert_it_2 (values[i]) != 
	  __builtin_powi (values[i], 3) * __builtin_powi (cbrt (values[i]), 2))
	abort ();
      if (convert_it_3 (values[i]) != 
	  __builtin_powi (values[i], -3) * __builtin_powi (cbrt (values[i]), 2))
	abort ();
      if (convert_it_4 (values[i]) !=
	  __builtin_powi (values[i], -3) * __builtin_powi (cbrt (values[i]), 1))
	abort ();
    }

  return 0;
}


/* { dg-final { scan-assembler-times "cbrt" 5 { target powerpc*-*-* } } } */
/* { dg-final { scan-assembler-not "bl\[\\. \]+pow" { target powerpc*-*-* } } } */
