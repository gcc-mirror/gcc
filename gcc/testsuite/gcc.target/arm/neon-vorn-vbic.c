/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2 -ftree-vectorize" } */
/* { dg-add-options arm_neon } */

void bor (int *__restrict__ c, int *__restrict__ a, int *__restrict__ b)
{
  int i;
  for (i = 0; i < 9; i++)
    c[i] = b[i] | (~a[i]);
}
void bic (int *__restrict__ c, int *__restrict__ a, int *__restrict__ b)
{
  int i;
  for (i = 0; i < 9; i++)
    c[i] = b[i] & (~a[i]);
}

/* { dg-final { scan-assembler "vorn\\t" } } */
/* { dg-final { scan-assembler "vbic\\t" } } */
