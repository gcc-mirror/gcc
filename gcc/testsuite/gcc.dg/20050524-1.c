/* This test case used to abort due to a reload bug with
   elimination offsets.  */

/* { dg-do run { target s390*-*-* } } */
/* { dg-options "-O2 -mpacked-stack" } */

extern void abort (void);

double bar (double) __attribute__ ((noinline));
double bar (double x) { return x; }

double
foo (int j, double f0, double f2, double f4, double f6, double x) __attribute__ ((noinline));

double
foo (int j, double f0, double f2, double f4, double f6, double x)
{
  if (j)
    return bar (x) + 4.0;
  else
    return bar (x);
}

int
main (void)
{
  if (foo (0, 0, 0, 0, 0, 10) != 10)
    abort ();
  if (foo (1, 0, 0, 0, 0, 10) != 14)
    abort ();

  return 0;
}

