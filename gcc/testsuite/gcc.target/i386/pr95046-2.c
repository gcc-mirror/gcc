/* PR target/95046 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -msse2" } */


float r[2], a[2];

float fabsf (float);

void
test_abs (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = fabsf (a[i]);
}

/* { dg-final { scan-assembler "\tv?andps" } } */

void
test_neg (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = -a[i];
}

/* { dg-final { scan-assembler "\tv?xorps" } } */

void
test_nabs (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = -fabsf (a[i]);
}

/* { dg-final { scan-assembler "\tv?orps" } } */
