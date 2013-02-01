/* PR tree-optimization/56125 */
/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

extern void abort (void);
extern double fabs (double);

__attribute__((cold)) double
foo (double x, double n)
{
  double u = x / (n * n);
  return u;
}

int
main ()
{
  if (fabs (foo (29, 2) - 7.25) > 0.001)
    abort ();
  return 0;
}
