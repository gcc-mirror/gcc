/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

float fabsf (float);

float
check (float x, float y)
{
  return __builtin_copysignf (x, y);
}

float
check1 (float x)
{
  return __builtin_copysignf (x, 1.0);
}

float
check2 (float x)
{
  return __builtin_copysignf (1.0, x);
}

float
check3 (float x)
{
  return -__builtin_copysignf (x, 1.0);
}

float
check4 (float x, float y)
{
  return x * __builtin_copysignf (x, y);
}

float
check5 (float x, float y)
{
  return __builtin_copysignf (-x, -y);
}

int
main (int argc, char** argv)
{
  float x = 2.0f;
  float y = -5.0f;
  float epsilon = 0.00001f;

  float expected = -2.0f;

  if (fabsf (check (x, y) - expected) >= epsilon)
    __builtin_abort ();

  expected = 2.0f;

  if (fabsf (check1 (x) - expected) >= epsilon)
    __builtin_abort ();

  expected = 1.0f;

  if (fabsf (check2 (x) - expected) >= epsilon)
    __builtin_abort ();

  expected = -2.0f;

  if (fabsf (check3 (x) - expected) >= epsilon)
    __builtin_abort ();

  expected = -4.0f;

  if (fabsf (check4 (x, y) - expected) >= epsilon)
    __builtin_abort ();

  expected = 2.0f;

  if (fabsf (check5 (x, y) - expected) >= epsilon)
    __builtin_abort ();
}

/* { dg-final { scan-assembler-not "copysign\tw" } } */

