/* { dg-options "-mpaired-single -O3 -fno-fast-math -ftree-vectorize -ffp-contract=off" } */
/* { dg-final { scan-assembler-not "\tmadd\\." } } */
/* { dg-final { scan-assembler-not "\tmsub\\." } } */
/* { dg-final { scan-assembler-not "\tnmadd\\." } } */
/* { dg-final { scan-assembler-not "\tnmsub\\." } } */

/* No function should use fused operations, however high the -O level.  */

#define N 512
float a[N], b[N], c[N], d[N];

NOMIPS16 void
not_madd_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = b[i] * c[i] + d[i];
}

NOMIPS16 float
not_msub_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = b[i] * c[i] - d[i];
}

NOMIPS16 float
not_nmadd_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -(b[i] * c[i] + d[i]);
}

NOMIPS16 float
not_nmsub_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -(b[i] * c[i] - d[i]);
}

NOMIPS16 float
not_nmadd_ps_2 (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -b[i] * c[i] - d[i];
}

NOMIPS16 float
not_nmsub_ps_2 (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -b[i] * c[i] + d[i];
}
