/* { dg-do run } */
/* We don't (and don't want to) perform this optimisation on soft-float targets,
   where each addition is a library call.  /
/* { dg-require-effective-target hard_float } */
/* -fassociative-math requires -fno-trapping-math and -fno-signed-zeros. */
/* { dg-options "-O2 -funroll-loops -fassociative-math -fno-trapping-math -fno-signed-zeros -fvariable-expansion-in-unroller -fdump-rtl-loop2_unroll" } */

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
  /* When compiling standard compliant we expect foo to return -0.0.  But the
     variable expansion during unrolling optimization (for this testcase enabled
     by non-compliant -fassociative-math) instantiates copy(s) of the
     accumulator which it initializes with +0.0.  Hence we expect that foo
     returns +0.0.  */
  if (__builtin_copysignf (1.0, foo (0.0 / -5.0, 10)) != 1.0)
    abort ();
  exit (0);
}

/* { dg-final { scan-rtl-dump "Expanding Accumulator" "loop2_unroll" } } */
