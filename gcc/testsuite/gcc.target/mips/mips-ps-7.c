/* mips-ps-5.c with -mips32r2 instead of -mips64.  */
/* { dg-do compile } */
/* { dg-mips-options "-mips32r2 -O2 -mpaired-single -ftree-vectorize" } */

extern float a[], b[], c[];

NOMIPS16 void
foo (void)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] = b[i] == c[i] + 1 ? b[i] : c[i];
}

/* { dg-final { scan-assembler "add\\.ps" } } */
/* { dg-final { scan-assembler "c\\.eq\\.ps" } } */
/* { dg-final { scan-assembler "mov\[tf\]\\.ps" } } */
