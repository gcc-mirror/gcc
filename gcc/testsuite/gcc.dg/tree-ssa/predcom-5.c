/* { dg-do run } */
/* { dg-options "-O2 -funroll-loops --param max-unroll-times=8 -fpredictive-commoning -fdump-tree-pcom-details" } */

/* Test for predictive commoning of expressions, with reassociation.  */

void abort (void);

unsigned a[1000], b[1000], c[1000], d[1000];

int main(void)
{
  unsigned i;

  for (i = 0; i < 1000; i++)
    a[i] = b[i] = d[i] = i;

  for (i = 1; i < 998; i++)
    c[i] = d[i + 1] * a[i + 2] * b[i + 1] - b[i - 1] * a[i] * d[i - 1];

  for (i = 1; i < 998; i++)
    if (c[i] != (i+1)*(i+2)*(i+1) - (i - 1) * i * (i - 1))
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Combination" 2 "pcom"} } */
/* { dg-final { scan-tree-dump-times "Unrolling 3 times." 1 "pcom"} } */
