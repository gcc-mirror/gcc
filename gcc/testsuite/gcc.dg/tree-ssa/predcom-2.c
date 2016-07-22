/* { dg-do run } */
/* { dg-options "-O2 -funroll-loops --param max-unroll-times=8 -fpredictive-commoning -fdump-tree-pcom-details" } */

void abort (void);

long int fib[1000];

__attribute__ ((noinline))
void count_fib(void)
{
  int i;

  fib[0] = 0;
  fib[1] = 1;
  for (i = 2; i < 1000; i++)
    fib[i] = (fib[i-1] + fib[i - 2]) & 0xffff;
}

int avg[1000];

__attribute__ ((noinline))
void count_averages(void)
{
  int i;

  for (i = 1; i < 999; i++)
    avg[i] = ((fib[i - 1] + fib[i] + fib[i + 1]) / 3) & 0xffff;
}

int main(void)
{
  count_fib ();
  count_averages ();

  if (fib[19] != 4181 || avg[19] != 4510)
    abort ();

  if (fib[999] != 162 || avg[998] != 21953)
    abort ();

  return 0;
}

/* Verify that both loops were transformed and unrolled.  */
/* { dg-final { scan-tree-dump-times "Unrolling 2 times." 2 "pcom"} } */
