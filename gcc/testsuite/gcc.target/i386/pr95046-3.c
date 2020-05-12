/* PR target/95046 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -mfma" } */


float r[2], a[2], b[2], c[2];

void
test_fma (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = a[i] * b[i] + c[i];
}

/* { dg-final { scan-assembler "\tvfmadd\[123\]+ps" } } */

void
test_fms (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = a[i] * b[i] - c[i];
}

/* { dg-final { scan-assembler "\tvfmsub\[123\]+ps" } } */

void
test_fnma (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = -(a[i] * b[i]) + c[i];
}

/* { dg-final { scan-assembler "\tvfnmadd\[123\]+ps" } } */

void
test_fnms (void)
{
  for (int i = 0; i < 2; i++)
    r[i] = -(a[i] * b[i]) - c[i];
}

/* { dg-final { scan-assembler "\tvfnmsub\[123\]+ps" } } */
