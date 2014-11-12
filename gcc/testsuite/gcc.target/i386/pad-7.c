/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=atom" } } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom" } */
/* { dg-final { scan-assembler-not "nop" } } */
/* { dg-final { scan-assembler-not "rep" } } */

int
foo (int x, int y, int z)
{
   return x + y + z + y;
}
