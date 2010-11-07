/* { dg-options "-mpaired-single -O -ffast-math -ftree-vectorize" } */
/* { dg-final { scan-assembler-times "\tmadd\\.ps\t" 1 } } */
/* { dg-final { scan-assembler-times "\tmsub\\.ps\t" 1 } } */
/* { dg-final { scan-assembler-times "\tnmadd\\.ps\t" 2 } } */
/* { dg-final { scan-assembler-times "\tnmsub\\.ps\t" 2 } } */

#define N 512
float a[N], b[N], c[N], d[N];

NOMIPS16 void
madd_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = __builtin_fmaf (b[i], c[i], d[i]);
}

NOMIPS16 float
msub_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = __builtin_fmaf (b[i], c[i], -d[i]);
}

NOMIPS16 float
nmadd_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -__builtin_fmaf (b[i], c[i], d[i]);
}

NOMIPS16 float
nmsub_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -__builtin_fmaf (b[i], c[i], -d[i]);
}

NOMIPS16 float
nmadd_ps_2 (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = __builtin_fmaf (-b[i], c[i], -d[i]);
}

NOMIPS16 float
nmsub_ps_2 (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = __builtin_fmaf (-b[i], c[i], d[i]);
}
