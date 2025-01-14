/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -Os -fno-forward-propagate -ftrivial-auto-var-init=zero" } */
/* { dg-require-effective-target dfp } */

/* Verify we do not ICE on the tests below.  */

/* { dg-final { scan-assembler-not "rldicr" { target { le } } } } */
/* { dg-final { scan-assembler-not "stxvd2x" { target { le } } } } */

union U128
{
  _Decimal128 d;
  unsigned long long int u[2];
};

union U128
foo ()
{
  volatile union U128 u128;
  u128.d = 0.9999999999999999999999999999999999e+39DL;
  return u128;
}
