// { dg-do compile }
// { dg-options "-O2" }

static double
quux (double x)
{
  return __builtin_fabs (x);
}

__attribute__ ((flatten, optimize ("-ffinite-math-only"))) static int
bar (int *p)
{
  *p = quux (0.0);

  return 0;
}

void
foo (int *p)
{
  (void) bar (p);
}
