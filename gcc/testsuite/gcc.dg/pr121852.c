/* PR rtl-optimization/121852 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-gcse" } */

int a[] = { 0 }, d, e, h, i, j, k, l, n[1], *o = n;
volatile int m;

int
foo (char q)
{
  return a[e ^ (q & 5)];
}

int
bar (int q[])
{
  int b = 5;
  for (int g = 0; g < d; ++g)
    {
      int c = foo (q[g] >> 6);
      int f = (c & 4095) ^ a[c & 5];
      b = f;
    }
  return b;
}

int
baz (volatile int q)
{
  k = 5 % q;
  int r[] = { h, i, k, j };
  return bar (r);
}

int
main ()
{
  int t;
  do
    {
      if (baz (5))
	m = 4;
      l--;
      t = l - 1 % m + 1;
    }
  while (!baz (5));
  o[0] = 2 % t;
  return 0;
}
