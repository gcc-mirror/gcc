/* { dg-options "-O2 -fmodulo-sched" } */


extern long *x1, *x2, *x3;

int
foo ()
{
  /* Switching the following two lines prevents the ICE.  */
  long *p1, *p2;
  long m, n, i;

  p1 = x1;
  p2 = x2;
  n = 0;
  for (i = *x3; 0 < i; i--)
    {
      m = (*p1++) ^ (*p2++);
      m = (m & 0x55555555) + ((m >> 1) & 0x55555555);
      m = (m & 0x33333333) + ((m >> 2) & 0x33333333);
      m = (m + (m >> 4)) & 0x0f0f0f0f;
      m = (m + (m >> 8));
      n += m;
    }
  return n;
}
