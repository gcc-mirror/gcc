/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */

#define N 8

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
  ur[4] = ua[4] * ub[4];
  ur[5] = ua[5] * ub[5];
  ur[6] = ua[6] * ub[6];
  ur[7] = ua[7] * ub[7];
}

/* { dg-final { scan-assembler-times "pmullw" 2 } } */
