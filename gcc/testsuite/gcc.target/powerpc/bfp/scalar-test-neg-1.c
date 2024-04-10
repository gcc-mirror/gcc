/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>
#include <stdbool.h>

bool
test_neg (float *p)
{
  float source = *p;

  return scalar_test_neg (source);
}

/* { dg-final { scan-assembler "xststdcsp" } } */
