/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

void
foo (int *c)
{
  c[0] = 0;
  c[1] = 1;
  c[2] = 2;
  c[3] = 3;
}

/* { dg-final { scan-assembler-times "movdqa\[ \\t\]+\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-times "movups\[ \\t\]+\[^\n\]*%xmm" 1 } } */
