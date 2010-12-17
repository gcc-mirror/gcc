/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom" } */
/* { dg-final { scan-assembler-times "nop" 8 } } */
/* { dg-final { scan-assembler-not "rep" } } */

void
foo ()
{
}
