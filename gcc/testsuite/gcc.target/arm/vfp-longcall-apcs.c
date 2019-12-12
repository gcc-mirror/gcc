/* { dg-do run } */
/* { dg-skip-if "FDPIC does not support sibcall optimization" { arm*-*-uclinuxfdpiceabi } "*" "" } */
/* { dg-options "-mapcs-frame -O -foptimize-sibling-calls -ffunction-sections" } */

extern void abort (void);

static __attribute__((noclone, noinline, long_call))
int foo (int a, int b, int c, int d, double i)
{
  return a;
}

static __attribute__((noclone, noinline))
double baz (double i)
{
  return i;
}

static __attribute__((noclone, noinline))
int bar (int a, int b, int c, int d, double i, double j)
{
  double l = baz (i) * j;
  return foo (a, b, c, d, l);
}

int
main (void)
{
  if (bar (0, 0, 0, 0, 0.0, 0.0) != 0)
    abort ();

  return 0;
}
