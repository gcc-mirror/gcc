/* Test MIPS32 DSP REV 2 MULTU instruction */
/* { dg-do compile } */
/* { dg-mips-options "-march=mips32r2 -mdspr2 -O2 -ffixed-hi -ffixed-lo" } */

/* { dg-final { scan-assembler "\tmultu\t" } } */
/* { dg-final { scan-assembler "ac1" } } */
/* { dg-final { scan-assembler "ac2" } } */
/* { dg-final { scan-assembler "ac3" } } */

typedef long long a64;
a64 a[4];
unsigned int b[4], c[4];

NOMIPS16 void test ()
{
  a[0] = (a64) b[0] * c[0];
  a[1] = (a64) b[1] * c[1];
  a[2] = (a64) b[2] * c[2];
  a[3] = (a64) b[3] * c[3];
}

