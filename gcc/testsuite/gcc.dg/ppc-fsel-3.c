/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O -mpowerpc-gfxopt" } */
/* { dg-final { scan-assembler-not "fsub" } } */

/* Check that an fsub isn't generated when no arithmetic was requested;
   such an fsub might incorrectly set floating-point exception flags.  */

double foo(double a, double b, double c, double d)
{
  return a < b ? c : d;
}
