/* { dg-options "-maltivec -mdejagnu-cpu=power6" } */
/* { dg-require-effective-target powerpc_altivec } */
/* If the default cpu type is power10 or later, MMA is enabled by default.
   To keep the test point available all the time, this case specifies
   -mdejagnu-cpu=power6 to make it be tested without MMA.  */

/* Verify there is no ICE and don't check the error messages on MMA
   requirement since they could be fragile and are not test points
   of this case.  */
/* { dg-excess-errors "pr103353" } */

void
foo (__vector_pair *dst, double *x)
{
  dst[0] = __builtin_vsx_lxvp (0, (__vector_pair *)(void *)x);
}

void
bar (__vector_pair *src, double *x)
{
  __builtin_vsx_stxvp (src[0], 0, (__vector_pair *)(void *)x);
}
