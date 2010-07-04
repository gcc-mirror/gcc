/* { dg-do compile } */
/* { dg-options "-O2 isa_rev>=1 -mgp32" } */
/* { dg-final { scan-assembler-not "\tmul\t" } } */
/* { dg-final { scan-assembler "\tmadd\t" } } */

NOMIPS16 long long
f1 (int *a, int *b, int n)
{
  long long int x;
  int i;

  x = 0;
  for (i = 0; i < n; i++)
    x += (long long) a[i] * b[i];
  return x;
}
