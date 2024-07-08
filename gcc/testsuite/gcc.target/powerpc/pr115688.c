/* { dg-do compile { target powerpc*-*-linux* } } */
/* { dg-options "-mdejagnu-cpu=power5 -O2" } */

/* Ignore some error messages on "target attribute or
   pragma changes AltiVec ABI".  */
/* { dg-excess-errors "pr115688" { target ilp32 } } */

/* Verify there is no ICE under 32 bit env.  */

__attribute__((target("vsx")))
int test (void)
{
  return 0;
}
