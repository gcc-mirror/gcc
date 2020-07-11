/* PR target/95046 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -msse2" } */


float r[2], a[2], b[2];

float copysignf (float, float);

void
test_copysign (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = copysignf (a[i], b[i]);
}

/* { dg-final { scan-assembler "\tv?andnps" } } */

void
test_xorsign (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = a[i] * copysignf (1.0f, b[i]);
}

/* { dg-final { scan-assembler "\tv?xorps" } } */

int s[2];

int signbitf (float);

void
test_signbitf (void)
{
  for (int i = 0; i < 2; i++)
    s[i] = signbitf (a[i]);
}

/* { dg-final { scan-assembler "\tv?psrld" } } */
