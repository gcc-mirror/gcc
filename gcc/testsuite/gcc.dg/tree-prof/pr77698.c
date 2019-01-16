/* { dg-options "-O2 -fno-tree-vectorize -funroll-loops --param max-unroll-times=4 -fno-inline -fdump-rtl-alignments" } */

volatile long int g;
volatile long int j = 0;

void foo(long int *a, long int *b, long int n)
{
  long int i;

  for (i = 0; i < n; i++)
    a[j] = *b;
}

long int a, b;
int main()
{
  a = 1; b = 2;
  foo(&a, &b, 1000000);
  g = a+b;
  return 0;
}

/* { dg-final-use-not-autofdo { scan-rtl-dump-times "internal loop alignment added" 1 "alignments"} } */
