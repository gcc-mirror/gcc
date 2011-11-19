/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-require-effective-target fpic } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=atom" } } */
/* { dg-skip-if "No Windows PIC" { *-*-mingw* *-*-cygwin } { "*" } { "" } } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom -fPIC" } */
/* { dg-final { scan-assembler-times "nop" 8 } } */
/* { dg-final { scan-assembler-not "rep" } } */

extern int bar;

int
foo ()
{
  asm volatile ("");
  return bar;
}
