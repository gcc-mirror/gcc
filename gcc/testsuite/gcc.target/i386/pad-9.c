/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=atom" } } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom" } */
/* { dg-final { scan-assembler-times "nop" 4 } } */
/* { dg-final { scan-assembler-not "rep" } } */

extern void bar (void);

void
foo (int x)
{
  if (x)
    bar ();
}
