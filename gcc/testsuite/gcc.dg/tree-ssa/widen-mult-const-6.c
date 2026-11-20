/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target longlong64 } */
/* { dg-options "-O2" } */

__attribute__((noipa)) long long
cancel (int a, long long b)
{
  return (long long) (a * 65536) * b / 65536;
}

__attribute__((noipa)) long long
partial (int a, long long b)
{
  return (long long) (a * 30) * b / 6;
}

__attribute__((noipa)) long long
multiply (int a, long long b)
{
  return (long long) (a * 30) * b * 7;
}

__attribute__((noipa)) long long
negative (int a, long long b)
{
  return (long long) (a * -30) * b / 6;
}

__attribute__((noipa)) long long
constant_overflow (int a, long long b)
{
  return (long long) (a * 2) * b * 4611686018427387904LL;
}

__attribute__((noipa)) long long
constant_min (int a, long long b)
{
  return (long long) (a * -2) * b * 4611686018427387904LL;
}

int
main (void)
{
  const int inputs[] = {-32768, -32767, -1, 0, 1, 32767};
  for (unsigned int i = 0; i < sizeof (inputs) / sizeof (inputs[0]); ++i)
    for (unsigned int j = 0; j < sizeof (inputs) / sizeof (inputs[0]); ++j)
      {
	int a = inputs[i];
	long long b = inputs[j];
	long long product = (long long) a * b;
	if (cancel (a, b) != product
	    || partial (a, b) != product * 5
	    || multiply (a, b) != product * 210
	    || negative (a, b) != product * -5)
	  __builtin_abort ();
      }

  long long min = -__LONG_LONG_MAX__ - 1;
  if (cancel (-32768, 4294967296LL) != min / 65536
      || partial (1, __LONG_LONG_MAX__ / 30)
	 != (__LONG_LONG_MAX__ / 30) * 5
      || multiply (1, __LONG_LONG_MAX__ / 210)
	 != (__LONG_LONG_MAX__ / 210) * 210
      || constant_overflow (-1, 1) != min
      || constant_overflow (1, -1) != min
      || constant_overflow (1, 0) != 0
      || constant_min (1, 1) != min
      || constant_min (-1, -1) != min)
    __builtin_abort ();

  return 0;
}
