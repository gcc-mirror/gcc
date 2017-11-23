/* { dg-options "-O3 -fdump-rtl-loop2_unroll-details -funroll-loops -fno-peel-loops" } */
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
/* { dg-final-use { scan-rtl-dump "considering unrolling loop with constant number of iterations" "loop2_unroll" } } */
