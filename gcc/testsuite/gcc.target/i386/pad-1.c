/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -mtune=generic" } */
/* { dg-final { scan-assembler "rep" { target { ! x86_64-*-mingw* } } } } */
/* { dg-final { scan-assembler-not "nop" } } */

void
foo ()
{
}
