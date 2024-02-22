/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */

int
main()
{
  __attribute__((altivec(vector__))) unsigned int test, res;
  const int s0 = 0;
  int mask;

  /* Argument 2 must be 0 or 1.  Argument 3 must be in range 0..15.  */
  res = __builtin_crypto_vshasigmaw (test, 1, 0xff); /* { dg-error {argument 3 must be a literal between 0 and 15, inclusive} } */
  return 0;
}
