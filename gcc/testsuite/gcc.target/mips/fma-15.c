/* { dg-options "-mpaired-single -O2 -fno-fast-math -ftree-vectorize -ffp-contract=fast" } */
/* { dg-final { scan-assembler "\tmadd\\.ps" } } */
/* { dg-final { scan-assembler "\tmsub\\.ps" } } */
/* { dg-final { scan-assembler-not "\tnmadd\\." } } */
/* { dg-final { scan-assembler-not "\tnmsub\\." } } */

/* We should not use NMADD or NMSUB without -ffinite-math-only because
   those instructions may perform arithmetic negation.  */

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
