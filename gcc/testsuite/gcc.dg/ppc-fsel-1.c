/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O -mpowerpc-gfxopt" } */
/* { dg-final { scan-assembler "fsel" } } */

/* Check that fsel can be generated even without -ffast-math.  */

double foo(double a, double b, double c, double d)
{
  return a < b ? c : d;
}
