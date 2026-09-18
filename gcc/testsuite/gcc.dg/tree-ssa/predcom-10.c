/* PR/127405 */
/* Predictive commoning used to unroll the loop below and wrongly record an
   upper bound of UNROLL_FACTOR - 1 iterations on the epilogue loop, even
   though the main unrolled loop is never entered because the control IV
   wraps.  Later loop peeling then dropped all but the first three
   iterations.  */

/* { dg-do run } */
/* { dg-options "-O2 -fpredictive-commoning -fpeel-loops" } */

extern void abort (void);

int a[12], b[12];

void __attribute__((noipa))
test (unsigned long n, int *a, int *b)
{
  /* I wraps around zero, the exit test is I + 2 != 0.  */
  for (unsigned long i = n - 2; i + 2 != 0; --i)
    a[i + 2] = b[i + 2] - a[i + 3] - a[i + 5] + (int) (i + 2);
}

int __attribute__ ((noipa))
check (unsigned long n)
{
  for (int i = 0; i < 12; i++)
    {
      a[i] = 0;
      b[i] = i + 1;
    }

  test (n, a, b);

  if (a[6] != 13 || a[5] != -2 || a[4] != 11
      || a[3] != -17 || a[2] != 24 || a[1] != -32)
    abort ();

  return 0;
}

int
main ()
{
  return check (6);
}
