/* { dg-do compile } */
/* { dg-do run } */
/* { dg-options "-O2 -funroll-loops --param max-unroll-times=8 -fpredictive-commoning -fdump-tree-pcom-details" } */

/* Test for predictive commoning of expressions, without reassociation.  */

void abort (void);

int a[1000], b[1000], c[1000];

int main(void)
{
  int i;

  for (i = 0; i < 1000; i++)
    a[i] = b[i] = i;

  for (i = 1; i < 998; i++)
    c[i] = a[i + 2] * b[i + 1] - b[i - 1] * a[i];

  for (i = 1; i < 998; i++)
    if (c[i] != 4 * i + 2)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Combination" 1 "pcom"} } */
/* { dg-final { scan-tree-dump-times "Unrolling 3 times." 1 "pcom"} } */
/* { dg-final { cleanup-tree-dump "pcom" } } */
