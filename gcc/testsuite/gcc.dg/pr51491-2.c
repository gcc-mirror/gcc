/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1" } */

int g (int *);

int
f (int n)
{
  int tt = 0;
  int t = 4;
  {
    int a[t
          + (tt != 0 ? 6 : 0)
         ];
    tt = g (a);
    {
      int b[n];
      tt += g (b);
      if (n > 20)
	tt += 148 * g (b);
      tt += b[0];
    }
    tt += a[0];
  }
  {
    int a[4];
    tt += g (a);
    tt += a[0];
  }
  return tt;
}

/* { dg-final { scan-tree-dump-times "CLOBBER" 2 "ccp1"} } */
