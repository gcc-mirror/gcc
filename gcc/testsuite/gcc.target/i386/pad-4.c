/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target fpic } */
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
