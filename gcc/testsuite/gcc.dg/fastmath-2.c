/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

extern void abort (void);

volatile double a = 2.002083e-146;
double b;

int
main()
{
  b = 1. / a;

  if (b != (1. / 2.002083e-146))
    abort ();
  return 0;
}

