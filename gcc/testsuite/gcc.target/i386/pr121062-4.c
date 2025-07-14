/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

typedef long long int __attribute__((__vector_size__ (8))) S;

void
foo (S *c)
{
  *c = (S){0x12345678badbeefULL};
}


/* { dg-final { scan-assembler-times "movq\[ \\t\]+%xmm\[0-9\]+, " 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movabsq\[ \\t\]+\\\$81985529250168559, %r\[a-z0-9\]+" 1 { target { ! ia32 } } } } */
