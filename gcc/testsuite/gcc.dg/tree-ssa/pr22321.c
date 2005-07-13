/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-ccp -fdump-tree-optimized" } */


volatile int x;

int main ()
{
  volatile int *vip;
  vip = &x;
  volatile int *cvip;
  cvip = vip;

  if (vip != cvip) return -1;
  return 0;
}

/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized" } } */
