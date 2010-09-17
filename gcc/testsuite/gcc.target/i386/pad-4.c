/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom -S -fPIC" } */
/* { dg-final { scan-assembler-times "nop; nop; nop; nop; nop; nop; nop; nop" 1 } } */
/* { dg-final { scan-assembler-not "rep" } } */

extern int bar;

int
foo ()
{
  return bar;
}
