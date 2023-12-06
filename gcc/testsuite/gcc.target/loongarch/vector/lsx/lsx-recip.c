/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler "vfrecip.s" } } */
/* { dg-final { scan-assembler "vfrecip.d" } } */
/* { dg-final { scan-assembler-not "vfdiv.s" } } */
/* { dg-final { scan-assembler-not "vfdiv.d" } } */

float a[4], b[4];

void
foo1(void)
{
  for (int i = 0; i < 4; i++)
    a[i] = 1 / (b[i]);
}

double da[2], db[2];

void
foo2(void)
{
  for (int i = 0; i < 2; i++)
    da[i] = 1 / (db[i]);
}
