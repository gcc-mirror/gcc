/* { dg-additional-options "-O -fdump-tree-phiprop -fdump-tree-fre" } */

#pragma acc routine
extern void foo (int *ptr, int val);

int main (void)
{
  int r, a[32];
  #pragma acc parallel copyin(readonly: a[:32]) copyout(r)
  {
    foo (a, a[8]);
    r = a[8];
  }
}
/* { dg-final { scan-tree-dump-times "r\.\[_0-9\]+ = \\(\\*_\[0-9\]+\\(ptro\\)\\)\\\[8\\\];" 2 "phiprop1" } } */
/* { dg-final { scan-tree-dump-times "r\.\[_0-9\]+ = \\(\\*_\[0-9\]+\\(ptro\\)\\)\\\[8\\\];" 1 "fre1" } } */
