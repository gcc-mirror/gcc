/* { dg-do compile } */
/* { dg-options "" } */

float test_powif(float x)
{
  return __builtin_powif(x, -1)
	 + __builtin_powif(x, 0)
	 + __builtin_powif(x, 1)
	 + __builtin_powif(x, 2);
}

double test_powi(double x)
{
  return __builtin_powi(x, -1)
	 + __builtin_powi(x, 0)
	 + __builtin_powi(x, 1)
	 + __builtin_powi(x, 2);
}

long double test_powil(long double x)
{
  return __builtin_powil(x, -1)
	 + __builtin_powil(x, 0)
	 + __builtin_powil(x, 1)
	 + __builtin_powil(x, 2);
}

/* { dg-final { scan-assembler-not "__builtin_" } } */

