/* { dg-do compile } */
/* { dg-options "-O1" } */

int *ptr;
int *fn1 (int *);
int fn2 (int, int);
int fn3 (void);
int fn4 (int);

static int
foo (int x, int y, int z)
{
  int b;
  asm ("" : "=a" (b), "=&d" (x) : "0" (y), "1" (x), "mr" (z));
  return x;
}

static int
bar (int x, int y)
{
  int a;
  if (!y)
    {
      for (a = 0; a <= (x >> 1); )
	;
      a = foo (y, fn2 (2, x), x);
      if (x)
	a = x;
      return a;
    }
}

static int
baz (int x, int y)
{
  int *a = ptr;
  int t, xk1 = fn3 (), xk = x * xk1;
  for (t = 0; t < xk; t += xk1)
    {
      if (fn4 (a[2]))
	return -y;
      a = fn1 (a);
    }
  return 0;
}

void
test (int x, long y, int z)
{
  int a = fn3 ();
  int b;
  int c = bar (x, z);
  for (b = 0; b <= y; b++)
    c = baz (x, c);
  fn2 (c, a);
}
