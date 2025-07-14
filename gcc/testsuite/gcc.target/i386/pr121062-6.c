/* { dg-do compile } */
/* { dg-options "-Og -fno-dce -mtune=generic" } */

typedef int __attribute__((__vector_size__ (8))) S;

void
foo (S *c)
{
  *c = (S){0x12345678,0xbadbeefULL};
}

/* { dg-final { scan-assembler-times "movq\[ \\t\]+%xmm\[0-9\]+, " 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movabsq\[ \\t\]+\\\$841538639400031864, %r\[a-z0-9\]+" 1 { target { ! ia32 } } } } */
