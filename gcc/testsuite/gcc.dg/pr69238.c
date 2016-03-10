/* { dg-do run } */
/* { dg-options "-O2 -fno-dce -fno-forward-propagate -fno-rerun-cse-after-loop -funroll-loops" } */


#define N 32

short sa[N];
short sb[N];
int ia[N];
int ib[N];

int __attribute__ ((noinline, noclone))
main1 (int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      sa[i+7] = sb[i];
      ia[i+3] = ib[i+1];
    }
  return 0;
}

int
main (void)
{ 
  return main1 (N-7);
}
