/* { dg-do run { xfail *-*-* } } */
/* We don't (and don't want to) perform this optimisation on soft-float
   targets, where each addition is a library call.  This test requires
   -fassociative-math for enabling the variable-expansion as well as
   -fsigned-zeros for honoring the sign of zero; but
   they can not co-exist; also under -funsafe-math-optimizations, so we
   expect it to fail.  */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2 -funroll-loops -funsafe-math-optimizations -fvariable-expansion-in-unroller -fdump-rtl-loop2_unroll" } */

extern void abort (void);
extern void exit (int);

float __attribute__((noinline))
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


