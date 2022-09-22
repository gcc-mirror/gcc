/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx" } */

/* It's to verify no ICE here, ignore error messages about
   mismatch argument number since they are not test points
   here.  */
/* { dg-excess-errors "pr104482" } */

__attribute__ ((altivec (vector__))) int vsi;

double
testXXPERMDI (void)
{
  return __builtin_vsx_xxpermdi (vsi, vsi, 2, 4);
}

