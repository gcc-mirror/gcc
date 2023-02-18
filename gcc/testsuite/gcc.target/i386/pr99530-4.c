/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fno-plt" } */
/* { dg-final { scan-assembler "call\[ \t\]+\[*\]foo@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "foo@GOT" { target { ia32 } } } } */

extern void foo (void);
void
bar (void)
{
  asm ("call %P0" : : "X" (foo));
} 
