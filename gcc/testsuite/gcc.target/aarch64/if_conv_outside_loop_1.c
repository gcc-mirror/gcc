/* { dg-do compile } */
/* { dg-options "-O2" } */

/* The target accepts if-conversion outside loops without a cost limit.  */

int
outside_loop (int v, int threshold)
{
  int t = v;
  if (v > threshold)
    {
      t = v * 3;
      t += 7;
      t ^= 0x55;
      t *= 0x55;
      t -= 5;
      t &= 0xFE;
      t ^= 0x55;
    }
  return t;
}

/* The same sequence inside a loop exceeds the cost limit.  */

int
inside_loop (int n, const int *a, int threshold)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    {
      int v = a[i], t = v;
      if (v > threshold)
	{
	  t = v * 3;
	  t += 7;
	  t ^= 0x55;
	  t *= 0x55;
	  t -= 5;
	  t &= 0xFE;
	  t ^= 0x55;
	}
      s += t;
    }
  return s;
}

/* { dg-final { scan-assembler-times "\\tcsel\\t" 1 } } */
