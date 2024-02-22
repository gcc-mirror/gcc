/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

int doTestBCDSignificance (_Decimal128 *p, unsigned int significance)
{
  _Decimal128 source = *p;

  return __builtin_dfp_dtstsfi_lt_td (significance, source);	/* { dg-error "argument 1 must be a literal between 0 and 63, inclusive" } */
}
