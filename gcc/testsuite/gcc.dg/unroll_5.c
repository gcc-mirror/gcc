/* { dg-do compile } */
/* { dg-options "-O3 -fdump-rtl-loop2_unroll -funroll-loops" } */
void abort (void);
int *a;
int t()
{
   int i;
  for (i=0;i<1000000;i++)
    if (a[i])
      return 1;
  return 0;
}
int t2()
{
   int i;
  for (i=0;i<3000000;i++)
    if (a[i])
        abort ();
  return 0;
}
/* { dg-final { scan-rtl-dump-times "upper bound: 999999" 1 "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump-not "realistic bound: 999999" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump-times "upper bound: 2999999" 1 "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump-times "realistic bound: 2999999" 1 "loop2_unroll" } } */
/* { dg-final { cleanup-rtl-dump "loop2_unroll" } } */
