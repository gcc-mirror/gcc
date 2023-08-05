/* { dg-options "-O3 -fdump-tree-cunroll-details-blocks -fdump-tree-optimized-details-blocks -fno-unroll-loops -fpeel-loops" } */
void abort();

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
/* { dg-final-use { scan-tree-dump "Peeled loop ., 1 times" "cunroll" } } */
/* { dg-final-use-not-autofdo { scan-tree-dump-not "Invalid sum" "cunroll" } } */
/* { dg-final-use-not-autofdo { scan-tree-dump-not "Invalid sum" "optimized" } } */
