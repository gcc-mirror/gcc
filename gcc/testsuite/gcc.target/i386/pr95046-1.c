/* PR target/95046 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -ffast-math -msse2 -mno-recip" } */


float r[2], a[2], b[2];

void
test_plus (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = a[i] + b[i];
}

/* { dg-final { scan-assembler "\tv?addps" } } */

void
test_minus (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = a[i] - b[i];
}

/* { dg-final { scan-assembler "\tv?subps" } } */

void
test_mult (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = a[i] * b[i];
}

/* { dg-final { scan-assembler "\tv?mulps" } } */

void
test_div (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = a[i] / b[i];
}

/* { dg-final { scan-assembler "\tv?divps" } } */

void
test_min (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = a[i] < b[i] ? a[i] : b[i];
}

/* { dg-final { scan-assembler "\tv?minps" } } */

void
test_max (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = a[i] > b[i] ? a[i] : b[i];
}

/* { dg-final { scan-assembler "\tv?maxps" } } */

float sqrtf (float);

void
test_sqrt (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = sqrtf (a[i]);
}

/* { dg-final { scan-assembler "\tv?sqrtps" } } */
