/* { dg-do compile } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=atom" } } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom" } */
/* { dg-final { scan-assembler-times "nop" 6 { target { ! x86_64-*-mingw* } } } } */
/* { dg-final { scan-assembler-times "nop" 4 { target { x86_64-*-mingw* } } } } */
/* { dg-final { scan-assembler-not "rep" } } */

int
foo (int x, int y)
{
   return y;
}
