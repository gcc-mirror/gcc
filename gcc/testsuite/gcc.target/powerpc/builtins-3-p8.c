/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8" } */

#include <altivec.h>

vector bool long long
test_eq_long_long (vector bool long long x, vector bool long long y)
{
	return vec_cmpeq (x, y);
}

/* Expected test results:

     test_eq_long_long          1 vcmpequd inst */

/* { dg-final { scan-assembler-times "vcmpequd" 1 } } */
