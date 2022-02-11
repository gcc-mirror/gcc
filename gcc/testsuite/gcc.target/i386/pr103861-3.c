/* PR target/103861 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse4 -fno-vect-cost-model" } */

char r[2], a[2], b[2];
unsigned char ur[2], ua[2], ub[2];

void maxs (void)
{
  int i;

  for (i = 0; i < 2; i++)
    r[i] = a[i] > b[i] ? a[i] : b[i];
}

/* { dg-final { scan-assembler "pmaxsb" } } */

void maxu (void)
{
  int i;

  for (i = 0; i < 2; i++)
    ur[i] = ua[i] > ub[i] ? ua[i] : ub[i];
}

/* { dg-final { scan-assembler "pmaxub" } } */

void mins (void)
{
  int i;

  for (i = 0; i < 2; i++)
    r[i] = a[i] < b[i] ? a[i] : b[i];
}

/* { dg-final { scan-assembler "pminsb" } } */

void minu (void)
{
  int i;

  for (i = 0; i < 2; i++)
    ur[i] = ua[i] < ub[i] ? ua[i] : ub[i];
}

/* { dg-final { scan-assembler "pminub" } } */

void _abs (void)
{
  int i;

  for (i = 0; i < 2; i++)
    r[i] = a[i] < 0 ? -a[i] : a[i];
}

/* { dg-final { scan-assembler "pabsb" } } */

void avgu (void)
{
  int i;

  for (i = 0; i < 2; i++)
    ur[i] = (ua[i] + ub[i] + 1) >> 1;
}

/* { dg-final { scan-assembler "pavgb" } } */
