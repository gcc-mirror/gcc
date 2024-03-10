/* { dg-do compile } */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-vectorize -funroll-loops --param max-unroll-times=8 -fpredictive-commoning -fdump-tree-pcom-details-blocks" } */

void abort (void);

unsigned fib[1000];

__attribute__ ((noinline))
void count_fib(void)
{
  int i;

  fib[0] = 0;
  fib[1] = 1;
  for (i = 2; i < 1000; i++)
    fib[i] = (fib[i-1] + fib[i - 2]) & 0xffff;
}

unsigned avg[1000];

__attribute__ ((noinline))
void count_averages(int n)
{
  int i;

  for (i = 1; i < n; i++)
    avg[i] = (((unsigned long) fib[i - 1] + fib[i] + fib[i + 1]) / 3) & 0xffff;
}

int main(void)
{
  count_fib ();
  count_averages (999);

  if (fib[19] != 4181 || avg[19] != 4510)
    abort ();

  if (fib[999] != 162 || avg[998] != 21953)
    abort ();

  return 0;
}

/* Verify that both loops were transformed and unrolled.  */
/* { dg-final { scan-tree-dump-times "Unrolling 2 times." 2 "pcom"} } */

/* Also check that we undid the transformation previously made by PRE.  */
/* { dg-final { scan-tree-dump-times "looparound ref" 1 "pcom" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "pcom" } } */
