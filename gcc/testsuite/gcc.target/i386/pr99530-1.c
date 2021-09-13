/* { dg-do compile { target { i?86-*-linux* x86_64-*-linux* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fpic -mcmodel=large -fno-plt" } */
/* { dg-final { scan-assembler-not "foo@GOTPCREL" } } */

extern void foo (void); 
void
bar (void)
{
  asm ("call %P0" : : "X" (foo));
} 
