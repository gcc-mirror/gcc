/* { dg-do run } */
/* { dg-options "-Ofast -fdump-tree-reassoc1" } */
/* { dg-require-effective-target int32 } */

/* The accumulated exponent does not fit the int argument of
   __builtin_powi and has to be split over multiple calls.  */

extern double pow (double, double);

__attribute__((noipa)) double
f (double x)
{
  return x * x * pow (x, 2147483647.0);
}

int
main (void)
{
  if (f (-1.0) != -1.0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "__builtin_powi \\(x_\\d+\\(D\\), 2147483647\\)" "reassoc1" } } */
/* { dg-final { scan-tree-dump "__builtin_powi \\(x_\\d+\\(D\\), 2\\)" "reassoc1" } } */
