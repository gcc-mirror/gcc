/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <altivec.h>

int doTestBCDSignificance (_Decimal64 *p)
{
  _Decimal64 source = *p;

  return __builtin_dfp_dtstsfi_eq (5, source);	/* { dg-error "'__builtin_dtstsfi_eq_dd' requires" } */
}

