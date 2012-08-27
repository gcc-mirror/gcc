/* { dg-options "-march=5kc" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-skip-if "requires -fira-region=all or =mixed" { *-*-* } { "-Os" } { "" } } */
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
