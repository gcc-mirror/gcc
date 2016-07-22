/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-loop2_unroll -funroll-loops" } */
struct a {int a[7];};
int t(struct a *a, int n)
{
  int i;
  for (i=0;i<n;i++)
    a->a[i]++;
}
/* { dg-final { scan-rtl-dump-not "Unrolled loop" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "likely upper bound: 7" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "realistic bound: -1" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "Not unrolling loop, doesn't roll" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump-not "Invalid sum" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump-not "upper bound: -1" "loop2_unroll" } } */
