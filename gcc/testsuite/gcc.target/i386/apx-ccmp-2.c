/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target apxf } */
/* { dg-options "-O3" } */

__attribute__((noinline, noclone, target("apxf")))
int foo_apx(int a, int b, int c, int d)
{
  int sum = a;

  if (a != c)
    {
      c += d;
      a += b;
      sum += a + c;
      if (b > d && sum != 0 || sum > d)
	{
	  b += d;
	  sum += b;
	}
    }

  return sum;
}

__attribute__((noinline, noclone, target("no-apxf")))
int foo_noapx(int a, int b, int c, int d)
{
  int sum = a;

  if (a != c)
    {
      c += d;
      a += b;
      sum += a + c;
      if (b > d && sum != 0 || sum > d)
	{
	  b += d;
	  sum += b;
	}
    }

  return sum;
}

__attribute__((noinline, noclone,
	       optimize(("finite-math-only")), target("apxf")))
double foo_fp_apx(int a, double b, int c, double d)
{
  int sum = a;
  double sumd = b;

  if (a != c)
    {
      sum += a;
      if (a < c || sumd != d || sum > c)
	{
	  c += a;
	  sum += a + c;
	}
    }

  return sum + sumd;
}

__attribute__((noinline, noclone,
	       optimize(("finite-math-only")), target("no-apxf")))
double foo_fp_noapx(int a, double b, int c, double d)
{
  int sum = a;
  double sumd = b;

  if (a != c)
    {
      sum += a;
      if (a < c || sumd != d || sum > c)
	{
	  c += a;
	  sum += a + c;
	}
    }

  return sum + sumd;
}


int main (void)
{
  if (!__builtin_cpu_supports ("apxf"))
    return 0;

  int val1 = foo_noapx (23, 17, 32, 44);
  int val2 = foo_apx (23, 17, 32, 44);

  if (val1 != val2)
    __builtin_abort ();

  double val3 = foo_fp_noapx (24, 7.5, 32, 2.0);
  double val4 = foo_fp_apx (24, 7.5, 32, 2.0);

  if (val3 != val4)
    __builtin_abort ();

  return 0;
}
