/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>

int doTestBCDSignificance (_Decimal64 *p)
{
  _Decimal64 source = *p;

  if (__builtin_dfp_dtstsfi_eq_dd (63, source))
    return 3;
  else
    return 5;
}

/* { dg-final { scan-assembler	   "dtstsfi" } } */

