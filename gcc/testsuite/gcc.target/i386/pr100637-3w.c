/* PR target/100637 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse4 -fno-vect-cost-model" } */

short r[2], a[2], b[2];
unsigned short ur[2], ua[2], ub[2];

void mulh (void)
{
  int i;

  for (i = 0; i < 2; i++)
    r[i] = ((int) a[i] * b[i]) >> 16;
}

/* { dg-final { scan-assembler "pmulhw" } } */

void mulhu (void)
{
  int i;

  for (i = 0; i < 2; i++)
    ur[i] = ((unsigned int) ua[i] * ub[i]) >> 16;
}

/* { dg-final { scan-assembler "pmulhuw" } } */

void mulhrs (void)
{
  int i;

  for (i = 0; i < 2; i++)
    r[i] = ((((int) a[i] * b[i]) >> 14) + 1) >> 1;
}

/* { dg-final { scan-assembler "pmulhrsw" } } */

void maxs (void)
{
  int i;

  for (i = 0; i < 2; i++)
    r[i] = a[i] > b[i] ? a[i] : b[i];
}

/* { dg-final { scan-assembler "pmaxsw" } } */

void maxu (void)
{
  int i;

  for (i = 0; i < 2; i++)
    ur[i] = ua[i] > ub[i] ? ua[i] : ub[i];
}

/* { dg-final { scan-assembler "pmaxuw" } } */

void mins (void)
{
  int i;

  for (i = 0; i < 2; i++)
    r[i] = a[i] < b[i] ? a[i] : b[i];
}

/* { dg-final { scan-assembler "pminsw" } } */

void minu (void)
{
  int i;

  for (i = 0; i < 2; i++)
    ur[i] = ua[i] < ub[i] ? ua[i] : ub[i];
}

/* { dg-final { scan-assembler "pminuw" } } */

void _abs (void)
{
  int i;

  for (i = 0; i < 2; i++)
    r[i] = a[i] < 0 ? -a[i] : a[i];
}

/* { dg-final { scan-assembler "pabsw" } } */

void avgu (void)
{
  int i;

  for (i = 0; i < 2; i++)
    ur[i] = (ua[i] + ub[i] + 1) >> 1;
}

/* { dg-final { scan-assembler "pavgw" } } */
