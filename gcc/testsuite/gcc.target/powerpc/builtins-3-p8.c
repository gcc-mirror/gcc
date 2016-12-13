/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8" } */

#include <altivec.h>

vector bool long long
test_eq_long_long (vector bool long long x, vector bool long long y)
{
	return vec_cmpeq (x, y);
}

vector float
test_pack_float (vector double x, vector double y)
{
  return vec_pack (x, y);
}

/* Expected test results:

     test_eq_long_long          1 vcmpequd inst
     test_pack_float            1 vpkudum inst */

/* { dg-final { scan-assembler-times "vcmpequd" 1 } } */
/* { dg-final { scan-assembler-times "vpkudum"  1 } } */
