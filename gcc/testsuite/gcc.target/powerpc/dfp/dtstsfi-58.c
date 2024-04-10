/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

int doTestBCDSignificance (_Decimal128 *p)
{
  _Decimal128 source = *p;

  if (__builtin_dfp_dtstsfi_eq_td (63, source))
    return 3;
  else
    return 5;
}

/* { dg-final { scan-assembler	   "dtstsfiq" } } */

