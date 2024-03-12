/* { dg-options "-O3 -fdump-rtl-loop2_unroll-details-blocks -funroll-loops -fno-peel-loops" } */
void abort ();

int a[1000];
int
__attribute__ ((noinline))
t()
{
  int i;
  for (i=0;i<1000;i++)
    if (!a[i])
      return 1;
  abort ();
}
int
main()
{
  int i;
  for (i=0;i<1000;i++)
    t();
  return 0;
}
/* { dg-final-use-not-autofdo { scan-rtl-dump "considering unrolling loop with constant number of iterations" "loop2_unroll" } } */
/* { dg-final-use-not-autofdo { scan-rtl-dump-not "Invalid sum" "loop2_unroll" } } */
