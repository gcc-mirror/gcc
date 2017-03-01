/* { dg-do compile { target ia32 } } */
/* { dg-options "-O" } */

void f()
{
  asm ("push %0" : : "r" ((unsigned long long) 456 >> 32));
} /* { dg-warning "unsupported size" }  */

/* { dg-final { scan-assembler-not "push\[ \t]+%r" } } */
