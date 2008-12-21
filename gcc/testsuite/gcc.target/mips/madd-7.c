/* -mlong32 added because of PR target/38598.  */
/* { dg-options "-O2 -march=5kc -mlong32" } */
/* { dg-final { scan-assembler-not "\tmul\t" } } */
/* { dg-final { scan-assembler "\tmadd\t" } } */

NOMIPS16 int
f1 (int *a, int *b, int n)
{
  int x, i;

  x = 0;
  for (i = 0; i < n; i++)
    x += a[i] * b[i];
  return x;
}
