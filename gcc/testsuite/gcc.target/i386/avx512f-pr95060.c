/* PR tree-optimization/95060 */
/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -mavx512f" } */
/* { dg-final { scan-assembler "\tvfnmsub" } } */
/* { dg-final { scan-assembler-not "\tvfmadd" } } */

#define N 32
float r[N], a[N], b[N], c[N];

void
foo (void)
{
  for (int i = 0; i < N; i++)
    r[i] = -(a[i] * b[i]) - c[i];
}

void
bar (void)
{
  for (int i = 0; i < N; i++)
    r[i] = -(a[i] * b[i] + c[i]);
}
