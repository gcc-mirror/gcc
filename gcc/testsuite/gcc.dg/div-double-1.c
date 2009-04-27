/* { dg-do run } */
/* { dg-options "-O2 -ffinite-math-only" } */
/* { dg-require-effective-target double64plus } */

extern void abort (void);

volatile double a = 2.002083e-146;
volatile double b;

int
main()
{
  b = 1. / a;

  if (b != (1. / 2.002083e-146))
    abort ();
  return 0;
}
