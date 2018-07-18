/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

int t[1];
int a (void);
int fct (int r, long e, int neg)
{
  int d = 0;
  if (r == 4)
    r = neg ? 3 : 2;
  if (__builtin_expect(e < -52, 0))
    d = r == 0 && a () ? 1 : 2;
  else
    {
      int i, n = 53;
      if (e < 0)
	n += e;
      for (i = 1 ; i < n / 64 + 1 ; i++)
	d = t[i]; /* { dg-bogus "array bounds" } */
    }
  return d;
}
