/* { dg-do run { xfail vax-*-* powerpc-*-*spe } } */
/* { dg-options "-O2 -funroll-loops -funsafe-math-optimizations -fvariable-expansion-in-unroller -dL" } */

extern void abort (void);
extern void exit (int);

float
foo (float d, int n)
{
  unsigned i;
  float accum = d;

  for (i = 0; i < n; i++)
    accum += d;

  return accum;
}

int
main ()
{
  if (__builtin_copysignf (1.0, foo (0.0 / -5.0, 10)) != -1.0)
    abort ();
  exit (0);
}

/* { dg-final { scan-rtl-dump "Expanding Accumulator" "loop2_unroll" } } */
/* { dg-final { cleanup-rtl-dump "loop*" } } */


