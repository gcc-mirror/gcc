/* { dg-do compile } */
/* { dg-options "-Og -fno-dce -mtune=generic" } */

typedef int __attribute__((__vector_size__ (4))) S;
extern void bar (S);

void
foo ()
{
  bar ((S){-1});
}

/* { dg-final { scan-assembler-times "movl\[ \\t\]+\\\$-1, \\(%esp\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[ \\t\]+\\\$-1, %edi" 1 { target { ! ia32 } } } } */
