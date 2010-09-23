/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom" } */
/* { dg-final { scan-assembler-times "nop" 6 } } */
/* { dg-final { scan-assembler-not "rep" } } */

int
foo (int x, int y)
{
   return x + y;
}
