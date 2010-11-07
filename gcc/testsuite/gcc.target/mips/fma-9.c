/* { dg-options "-mpaired-single -O3 -fno-fast-math -ftree-vectorize -ffp-contract=off" } */
/* { dg-final { scan-assembler "\tmadd\\.ps\t" } } */
/* { dg-final { scan-assembler "\tmsub\\.s\t" } } */
/* { dg-final { scan-assembler-not "\tmsub\\.ps\t" } } */
/* { dg-final { scan-assembler-not "\tnmadd\\." } } */
/* { dg-final { scan-assembler-not "\tnmsub\\." } } */

/* We should not use NMADD or NMSUB without -ffinite-math-only because
   those instructions may perform arithmetic negation.  We don't really
   expect the nmadd_ps and nmsub_ps functions to use MADD.PS and MSUB.PS,
   but there's no reason in principle why they shouldn't.

   ??? At the moment, we don't vectorize msub_ps, but we probably should.  */

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
not_nmadd_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -__builtin_fmaf (b[i], c[i], d[i]);
}

NOMIPS16 float
not_nmsub_ps (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = -__builtin_fmaf (b[i], c[i], -d[i]);
}

NOMIPS16 float
not_nmadd_ps_2 (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = __builtin_fmaf (-b[i], c[i], -d[i]);
}

NOMIPS16 float
not_nmsub_ps_2 (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = __builtin_fmaf (-b[i], c[i], d[i]);
}
