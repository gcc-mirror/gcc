/* { dg-do compile } */
/* { dg-options "-O2 -mpaired-single -mgp64 -ftree-vectorize" } */

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
