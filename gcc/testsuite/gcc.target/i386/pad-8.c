/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom -S" } */
/* { dg-final { scan-assembler-times "nop; nop; nop; nop; nop; nop" 1 } } */
/* { dg-final { scan-assembler-not "nop; nop; nop; nop; nop; nop; nop" } } */
/* { dg-final { scan-assembler-not "rep" } } */

int
foo (int x, int y)
{
   return y;
}
