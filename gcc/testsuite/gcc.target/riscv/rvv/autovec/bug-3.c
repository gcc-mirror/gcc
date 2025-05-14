/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d -mrvv-max-lmul=m8 -mrvv-vector-bits=scalable -fno-vect-cost-model -O2 -ffast-math" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define N 16

_Complex float a[N] =
    { 10.0F + 20.0iF, 11.0F + 21.0iF, 12.0F + 22.0iF, 13.0F + 23.0iF,
      14.0F + 24.0iF, 15.0F + 25.0iF, 16.0F + 26.0iF, 17.0F + 27.0iF,
      18.0F + 28.0iF, 19.0F + 29.0iF, 20.0F + 30.0iF, 21.0F + 31.0iF,
      22.0F + 32.0iF, 23.0F + 33.0iF, 24.0F + 34.0iF, 25.0F + 35.0iF };
_Complex float b[N] =
    { 30.0F + 40.0iF, 31.0F + 41.0iF, 32.0F + 42.0iF, 33.0F + 43.0iF,
      34.0F + 44.0iF, 35.0F + 45.0iF, 36.0F + 46.0iF, 37.0F + 47.0iF,
      38.0F + 48.0iF, 39.0F + 49.0iF, 40.0F + 50.0iF, 41.0F + 51.0iF,
      42.0F + 52.0iF, 43.0F + 53.0iF, 44.0F + 54.0iF, 45.0F + 55.0iF };

_Complex float c[N];
_Complex float res[N] =
    { -500.0F + 1000.0iF, -520.0F + 1102.0iF,
      -540.0F + 1208.0iF, -560.0F + 1318.0iF,
      -580.0F + 1432.0iF, -600.0F + 1550.0iF,
      -620.0F + 1672.0iF, -640.0F + 1798.0iF,
      -660.0F + 1928.0iF, -680.0F + 2062.0iF,
      -700.0F + 2200.0iF, -720.0F + 2342.0iF,
      -740.0F + 2488.0iF, -760.0F + 2638.0iF,
      -780.0F + 2792.0iF, -800.0F + 2950.0iF };

/*
** foo:
** ...
** csrr\s+[atx][0-9]+,\s*vlenb
** slli\s+[atx][0-9]+,\s*[atx][0-9],\s*1
** ...
** slli\s+[atx][0-9]+,\s*[atx][0-9],\s*32
** ...
*/
void
foo (void)
{
  int i;

  for (i = 0; i < N; i++)
    c[i] = a[i] * b[i];
}

/* { dg-final { scan-assembler-not {li\s+[a-x0-9]+,\s*0} } } */
