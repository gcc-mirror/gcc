/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

int doTestBCDSignificance (_Decimal128 *p)
{
  _Decimal128 source = *p;

  return __builtin_dfp_dtstsfi_ov_td (5, source);	/* { dg-error "'__builtin_dtstsfi_ov_td' requires" } */
}


