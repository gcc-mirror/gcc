/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-cunroll-details -fno-tree-vectorize" } */
int *a;
void
test(int c)
{ 
  int i;
  for (i=0;i<6;i++)
    a[i]=5;
}
/* Basic testcase for complette unrolling.  */
/* { dg-final { scan-tree-dump "loop with 5 iterations completely unrolled" "cunroll"} } */
/* { dg-final { scan-tree-dump "Exit condition of peeled iterations was eliminated." "cunroll"} } */
/* { dg-final { scan-tree-dump "Last iteration exit edge was proved true." "cunroll"} } */
