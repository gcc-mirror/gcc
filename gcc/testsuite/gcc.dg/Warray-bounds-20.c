/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

int t[1];
int fct (int r, long e)
{
  int d = 0;
  if (r == 4)
    r = 1;
  if (e < -52)
    d = r == 0 ? 1 : 2;
  else
    {
      int i, n = 53;
      if (__builtin_expect (e < 0, 0))
	n += e;
      for (i = 1 ; i < n / 64 + 1 ; i++)
	t[i] = 0; /* { dg-bogus "array bounds" } */
    }
  return d;
}
