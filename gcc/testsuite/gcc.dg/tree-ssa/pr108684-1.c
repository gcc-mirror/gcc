/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */


static int t;

int f (int *a)
{
  int t1, t2 = 0;
  asm ("shouldshowupstill %1" : "=r" (t1), "=m"(t2) : : );
  t = t1;
  return t2;
}

/* Check to make sure DCE does not remove the inline-asm as it writes to t2. */
/* We used to DCE this inline-asm when removing the store to t. */
/* { dg-final { scan-assembler "shouldshowupstill" } } */
/* { dg-final { scan-tree-dump-times "shouldshowupstill" 1 "optimized" } } */
