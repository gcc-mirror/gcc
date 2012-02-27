/* Test that we generate aligned load when memory is aligned.  */
/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O -march=x86-64 -mtune=generic -std=gnu99" } */
/* { dg-final { scan-assembler-not "movdqu" } } */
/* { dg-final { scan-assembler "movdqa" { target { ! x86_64-*-mingw* } } } } */

extern _Decimal128 foo (_Decimal128, _Decimal128, _Decimal128);

void
bar (void)
{
  foo (0, 0, 0);
}
