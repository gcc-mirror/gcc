/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

int doTestBCDSignificance (_Decimal128 *p, unsigned int significance)
{
  _Decimal128 source = *p;

  return __builtin_dfp_dtstsfi_ov (significance, source);	/* { dg-error "argument 1 must be a literal between 0 and 63, inclusive" } */
}
