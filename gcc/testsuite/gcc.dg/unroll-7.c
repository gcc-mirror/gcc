/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-loop2_unroll-details -funroll-loops" } */
/* { dg-require-effective-target int32plus } */

extern int *a;

int t(void)
{
  int i;
  for (i=0;i<1000000;i++)
    a[i]++;
}
/* { dg-final { scan-rtl-dump "Unrolled loop" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "number of iterations: .const_int 999999" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "upper bound: 999999" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "realistic bound: 999999" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "considering unrolling loop with constant number of iterations" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump-not "Invalid sum" "loop2_unroll" } } */
