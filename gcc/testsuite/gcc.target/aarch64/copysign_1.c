/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

double fabs (double);

double
check (double x, double y)
{
  return __builtin_copysign (x, y);
}

double
check1 (double x)
{
  return __builtin_copysign (x, 1.0);
}

double
check2 (double x)
{
  return __builtin_copysign (1.0, x);
}

double
check3 (double x)
{
  return -__builtin_copysign (x, 1.0);
}

double
check4 (double x, double y)
{
  return x * __builtin_copysign (x, y);
}

double
check5 (double x, double y)
{
  return __builtin_copysign (-x, -y);
}

int
main (int argc, char** argv)
{
  double x = 2.0;
  double y = -5.0;
  double epsilon = 0.00001;

  double expected = -2.0;

  if (fabs (check (x, y) - expected) >= epsilon)
    __builtin_abort ();

  expected = 2.0;

  if (fabs (check1 (x) - expected) >= epsilon)
    __builtin_abort ();

  expected = 1.0;

  if (fabs (check2 (x) - expected) >= epsilon)
    __builtin_abort ();

  expected = -2.0;

  if (fabs (check3 (x) - expected) >= epsilon)
    __builtin_abort ();

  expected = -4.0;

  if (fabs (check4 (x, y) - expected) >= epsilon)
    __builtin_abort ();

  expected = 2.0;

  if (fabs (check5 (x, y) - expected) >= epsilon)
    __builtin_abort ();
}

/* { dg-final { scan-assembler-not "copysign\tw" } } */

