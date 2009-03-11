extern void abort (void);

void test(double f, double i)
{
  if (f == __builtin_inf())
    abort ();
  if (f == -__builtin_inf())
    abort ();
  if (i == -__builtin_inf())
    abort ();
  if (i != __builtin_inf())
    abort ();

  if (f >= __builtin_inf())
    abort ();
  if (f > __builtin_inf())
    abort ();
  if (i > __builtin_inf())
    abort ();
  if (f <= -__builtin_inf())
    abort ();
  if (f < -__builtin_inf())
    abort ();
}

void testf(float f, float i)
{
#ifndef __SPU__
  /* The SPU single-precision floating point format does not support Inf.  */

  if (f == __builtin_inff())
    abort ();
  if (f == -__builtin_inff())
    abort ();
  if (i == -__builtin_inff())
    abort ();
  if (i != __builtin_inff())
    abort ();

  if (f >= __builtin_inff())
    abort ();
  if (f > __builtin_inff())
    abort ();
  if (i > __builtin_inff())
    abort ();
  if (f <= -__builtin_inff())
    abort ();
  if (f < -__builtin_inff())
    abort ();
#endif
}

void testl(long double f, long double i)
{
  if (f == __builtin_infl())
    abort ();
  if (f == -__builtin_infl())
    abort ();
  if (i == -__builtin_infl())
    abort ();
  if (i != __builtin_infl())
    abort ();

  if (f >= __builtin_infl())
    abort ();
  if (f > __builtin_infl())
    abort ();
  if (i > __builtin_infl())
    abort ();
  if (f <= -__builtin_infl())
    abort ();
  if (f < -__builtin_infl())
    abort ();
}

int main()
{
  test (34.0, __builtin_inf());
  testf (34.0f, __builtin_inff());
  testl (34.0l, __builtin_infl());
  return 0;
}

