/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -mapxf" } */

int foo(int a, int b, int c, int d)
{
  int sum = a;

  if (a != c)
    {
      c += d;
      a += b;
      sum += a + c;
      if (b != d && sum < c || sum > d)
	{
	  b -= d;
	  sum -= b;
	}
    }

  return sum;
}

int foo2 (unsigned a, unsigned b, unsigned d, unsigned e, int *p)
{
  unsigned r;
  int c = __builtin_mul_overflow (a, b, &r);
  *p += a;
  return c ? d : e;
}

/* { dg-final { scan-assembler-not "set" } } */

