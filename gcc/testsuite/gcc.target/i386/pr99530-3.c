/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fno-pic -fno-plt" } */
/* { dg-final { scan-assembler "call\[ \t\]+\[*\]foo@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]+\[*\]foo@GOT" { target { ia32 && got32x_reloc } } } } */

extern void foo (void); 
void
bar (void)
{
  asm ("call %P0" : : "X" (foo));
} 
