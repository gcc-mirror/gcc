/* PR target/100637 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse4" } */

char r[4], a[4], b[4];
unsigned char ur[4], ua[4], ub[4];

void maxs (void)
{
  int i;

  for (i = 0; i < 4; i++)
    r[i] = a[i] > b[i] ? a[i] : b[i];
}

/* { dg-final { scan-assembler "pmaxsb" } } */

void maxu (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ur[i] = ua[i] > ub[i] ? ua[i] : ub[i];
}

/* { dg-final { scan-assembler "pmaxub" } } */

void mins (void)
{
  int i;

  for (i = 0; i < 4; i++)
    r[i] = a[i] < b[i] ? a[i] : b[i];
}

/* { dg-final { scan-assembler "pminsb" } } */

void minu (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ur[i] = ua[i] < ub[i] ? ua[i] : ub[i];
}

/* { dg-final { scan-assembler "pminub" } } */

void _abs (void)
{
  int i;

  for (i = 0; i < 4; i++)
    r[i] = a[i] < 0 ? -a[i] : a[i];
}

/* { dg-final { scan-assembler "pabsb" } } */

void avgu (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ur[i] = (ua[i] + ub[i] + 1) >> 1;
}

/* { dg-final { scan-assembler "pavgb" } } */
