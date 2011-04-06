/* { dg-options "-O2 -march=5kc" } */
/* { dg-final { scan-assembler-not "\tmul\t" } } */
/* { dg-final { scan-assembler "\tmsub\t" } } */

NOMIPS16 int
f1 (int *a, int *b, int n)
{
  int x, i;

  x = 100;
  for (i = 0; i < n; i++)
    x -= a[i] * b[i];
  return x;
}
