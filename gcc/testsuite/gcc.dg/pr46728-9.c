/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -fno-inline -fno-unroll-loops -lm" } */

#include <math.h>

extern void abort (void);

#define NVALS 6

static double
convert_it (double x)
{
  return pow (x, 0.5);
}

int
main (int argc, char *argv[])
{
  double values[NVALS] = { 3.0, 1.95, 2.227, 4.0, 256.0, .0008797 };
  double PREC = 0.999999;
  unsigned i;

  for (i = 0; i < NVALS; i++)
    if (fabs (convert_it (values[i]) / sqrt (values[i])) < PREC)
      abort ();

  return 0;
}

