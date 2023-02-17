/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details -fdump-tree-optimized" } */

extern volatile int *x;
static int gCrc;

static int __attribute__((noinline)) crc16Add(int crc, int b)
{
  return crc + b;
}

void f(int data, int dataSz)
{
  int i;

  for(i=0;i<dataSz;i++)
  {
    gCrc = crc16Add(gCrc, data);
    *x = data;
  }
}

/* { dg-final { scan-tree-dump "Executing store motion of gCrc" "lim2" } } */
/* { dg-final { scan-tree-dump-not "Re-issueing" "lim2" } } */
/* { dg-final { scan-tree-dump-times "\\*x" 1 "optimized" } } */
