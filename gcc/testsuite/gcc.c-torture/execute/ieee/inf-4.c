__attribute__((noipa)) int
foo (double a, double b)
{
  double c = a - b;
  if (!__builtin_isfinite (c))
    {
      if (__builtin_isnan (c))
	{
	  if (!__builtin_isnan (a) && !__builtin_isnan (b))
	    return 1;
	}
      else if (__builtin_isfinite (a) && __builtin_isfinite (b))
	return 2;
    }
  else if (c == 0 && a != b)
    return 3;
  return 4;
}

int
main ()
{
  double a = __builtin_inf ();
  if (foo (a, a) != 1)
    __builtin_abort ();
}
