/* { dg-do compile { target ia32 } } */
/* { dg-options "-O" } */

void f()
{
  asm ("push %0" : : "r" ((unsigned long long) 456));
}

/* { dg-final { scan-assembler-not "push %r" } } */
