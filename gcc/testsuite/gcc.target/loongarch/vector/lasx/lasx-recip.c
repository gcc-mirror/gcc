/* { dg-do compile } */
/* { dg-options "-O2 -mlasx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler "xvfrecip.s" } } */
/* { dg-final { scan-assembler "xvfrecip.d" } } */
/* { dg-final { scan-assembler-not "xvfdiv.s" } } */
/* { dg-final { scan-assembler-not "xvfdiv.d" } } */

float a[8], b[8];

void 
foo1(void)
{
  for (int i = 0; i < 8; i++)
    a[i] = 1 / (b[i]);
}

double da[4], db[4];

void
foo2(void)
{
  for (int i = 0; i < 4; i++)
    da[i] = 1 / (db[i]);
}
