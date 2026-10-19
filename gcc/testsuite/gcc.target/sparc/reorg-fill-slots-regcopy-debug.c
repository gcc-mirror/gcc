/* PR rtl-optimization/127725 */
/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v8 -fno-var-tracking -fcompare-debug --param=max-delay-slot-live-search=100000 --param=max-delay-slot-insn-search=100000" } */

int h (int);

int
f (void)
{
  unsigned int mid;
  unsigned int lo = 0;
  unsigned int hi = 3;
  unsigned int last = -1;

  for (;;)
    {
      mid = (lo + hi) / 2;
      if (last == mid)
	return 0;
      last = mid;
      int r = h (mid);
      if (r < 0)
	hi = mid;
      else if (r > 0)
	lo = mid;
      else
	return 1;
    }
}
