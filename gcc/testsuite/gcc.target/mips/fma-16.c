/* { dg-options "-mpaired-single -O2 -ffast-math -ftree-vectorize" } */
/* { dg-final { scan-assembler-times "\tmadd\\.ps" 1 } } */
/* { dg-final { scan-assembler-times "\tmsub\\.ps" 1 } } */
/* { dg-final { scan-assembler-times "\tnmadd\\.ps" 2 } } */
/* { dg-final { scan-assembler-times "\tnmsub\\.ps" 2 } } */

#define N 512
float a[N], b[N], c[N], d[N];

NOMIPS16 void
madd_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = b[i] * c[i] + d[i];
}

NOMIPS16 float
msub_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = b[i] * c[i] - d[i];
}

NOMIPS16 float
nmadd_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -(b[i] * c[i] + d[i]);
}

NOMIPS16 float
nmsub_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -(b[i] * c[i] - d[i]);
}

NOMIPS16 float
nmadd_ps_2 (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -b[i] * c[i] - d[i];
}

NOMIPS16 float
nmsub_ps_2 (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -b[i] * c[i] + d[i];
}
