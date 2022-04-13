/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

unsigned char ur[16], ua[16], ub[16];

void avgu_v2qi (void)
{
  int i;

  for (i = 0; i < 2; i++)
    ur[i] = (ua[i] + ub[i] + 1) >> 1;
}

/* { dg-final { scan-assembler "pavgb" } } */
