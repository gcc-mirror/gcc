/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target apxf } */
/* { dg-options "-O3 -mno-apxf" } */

__attribute__((noinline, noclone, target("apxf")))
int foo_apx(int a, int b, int c, int d)
{
  int sum = a;

  if (a != c)
    {
      c += d;
      a += b;
      sum += a + c;
      if (b != d && sum < c || sum > d)
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
      if (b != d && sum < c || sum > d)
	{
	  b += d;
	  sum += b;
	}
    }

  return sum;
}

int main (void)
{
  if (!__builtin_cpu_supports ("apxf"))
    return 0;

  int val1 = foo_noapx (23, 17, 32, 44);
  int val2 = foo_apx (23, 17, 32, 44);

  if (val1 != val2)
    __builtin_abort ();

  return 0;
}
