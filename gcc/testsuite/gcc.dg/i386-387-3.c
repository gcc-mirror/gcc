/* Verify that 387 mathematical constants are recognized.  */
/* { dg-do compile { target "i?86-*-*" } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */
/* { dg-options "-O2 -march=i686" } */
/* { dg-final { scan-assembler "fldpi" } } */

long double add_pi(long double x)
{
  return x + 3.1415926535897932385128089594061862044L;
}

