/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O -mpowerpc-gfxopt -fno-trapping-math" } */
/* { dg-final { scan-assembler "fsel" } } */

/* If the user doesn't care about signals, fsel can be used in many cases.  */

double foo(double a, double b, double c, double d)
{
  return a < b ? c : d;
}
