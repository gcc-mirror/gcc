/* { dg-do compile { target { i?86-*-linux* x86_64-*-linux* } } } */
/* { dg-options "-O2 -fno-pic" } */
/* { dg-final { scan-assembler "call\[ \t\]+\[*\]foo@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]+\[*\]foo@GOT" { target { ia32 && got32x_reloc } } } } */

extern void foo (void) __attribute__ ((noplt)); 
void
bar (void)
{
  asm ("call %P0" : : "X" (foo));
} 
