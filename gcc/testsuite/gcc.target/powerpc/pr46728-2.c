/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fno-inline -fno-unroll-loops -lm -mpowerpc-gpopt -fno-ident" } */

#include <math.h>

extern void abort (void);

#define NVALS 6

static double
convert_it (double x)
{
  return pow (x, 0.25);
}

int
main (int argc, char *argv[])
{
  double values[NVALS] = { 3.0, 1.95, 2.227, 4.0, 256.0, .0008797 };
  unsigned i;

  for (i = 0; i < NVALS; i++)
    if (convert_it (values[i]) != sqrt (sqrt (values[i])))
      abort ();

  return 0;
}


/* { dg-final { scan-assembler-times "fsqrt|xssqrtdp" 4 { target powerpc*-*-* } } } */
/* { dg-final { scan-assembler-not "bl\[\\. \]+pow" { target powerpc*-*-* } } } */
