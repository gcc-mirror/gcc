/* { dg-do compile } */
/* { dg-options "-O2" } */

/* An increment inside a longer arm does not make the select a CSINC, so
   the cost limit still applies.  */

void
chain (const int *restrict in, int *restrict out, int n, int threshold)
{
  for (int i = 0; i < n; ++i)
    {
      int v = in[i];
      if (v > threshold)
	{
	  int t = v * 3;
	  t += 1;
	  t ^= 0x55;
	  t *= 0x55;
	  t -= 0x5;
	  t &= 0xFE;
	  t ^= 0x55;
	  out[i] = t;
	}
      else
	out[i] = v;
    }
}

int
multi (const int *a, int n, int lim)
{
  int cnt = 0, s = 0, t = 0;
  for (int i = 0; i < n; i++)
    {
      int v = a[i];
      if (v > lim)
	{
	  cnt++;
	  s += v * 3;
	  t ^= v * 5;
	}
    }
  return cnt + s + t;
}

/* A lone conditional increment is still converted.  */

int
count (const int *a, int n, int lim)
{
  int cnt = 0;
  for (int i = 0; i < n; i++)
    if (a[i] > lim)
      cnt++;
  return cnt;
}

/* { dg-final { scan-assembler-not "csel\t" } } */
/* { dg-final { scan-assembler-times "cinc\t" 1 } } */
