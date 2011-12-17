/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand" } */


int g(int*);

int f(void)
{
  int tt = 0;
  int t = 4;
  {
    int a[t];
    tt = g(a);
    tt += a[0];
  }
  {
    int a[4];
    tt += g(a);
    tt += a[0];
  }
  return tt;
}

/* { dg-final { scan-rtl-dump-times "Partition" 1 "expand"} } */
/* { dg-final { cleanup-rtl-dump "expand" } } */
