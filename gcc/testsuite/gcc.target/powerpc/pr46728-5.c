/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fno-inline -fno-unroll-loops -lm -mpowerpc-gpopt" } */

#include <math.h>

extern void abort (void);

#define NVALS 6

static double
convert_it (double x)
{
  return pow (x, 1.0 / 6.0);
}

int
main (int argc, char *argv[])
{
  double values[NVALS] = { 3.0, 1.95, 2.227, 729.0, 64.0, .0008797 };
  unsigned i;

  for (i = 0; i < NVALS; i++)
    if (convert_it (values[i]) != cbrt (sqrt (values[i])))
      abort ();

  return 0;
}


/* { dg-final { scan-assembler-times "cbrt" 2 { target powerpc*-*-* } } } */
/* { dg-final { scan-assembler-not " pow " { target powerpc*-*-* } } } */
