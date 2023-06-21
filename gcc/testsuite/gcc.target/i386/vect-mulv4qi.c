/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */

#define N 4

unsigned char ur[N], ua[N], ub[N];

void mul (void)
{
  int i;

  for (i = 0; i < N; i++)
    ur[i] = ua[i] * ub[i];
}

void mul_slp (void)
{
  ur[0] = ua[0] * ub[0];
  ur[1] = ua[1] * ub[1];
  ur[2] = ua[2] * ub[2];
  ur[3] = ua[3] * ub[3];
}

/* { dg-final { scan-assembler-times "pmullw" 2 } } */
