/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -mvsx" } */

#include <altivec.h>

vector bool char
test_eq_char (vector bool char x, vector bool char y)
{
	return vec_cmpeq (x, y);
}

vector bool short
test_eq_short (vector bool short x, vector bool short y)
{
	return vec_cmpeq (x, y);
}

vector bool int
test_eq_int (vector bool int x, vector bool int y)
{
	return vec_cmpeq (x, y);
}

vector bool long
test_eq_long (vector bool long x, vector bool long y)
{
	return vec_cmpeq (x, y);
}

vector bool char
test_ne_char (vector bool char x, vector bool char y)
{
	return vec_cmpne (x, y);
}

vector bool short
test_ne_short (vector bool short x, vector bool short y)
{
	return vec_cmpne (x, y);
}

vector bool int
test_ne_int (vector bool int x, vector bool int y)
{
	return vec_cmpne (x, y);
}

vector bool long
test_ne_long (vector bool long x, vector bool long y)
{
	return vec_cmpne (x, y);
}

/* Note: vec_cmpne is implemented as vcmpeq and then NOT'ed
   using the xxlnor instruction.

   Expected test results:
   test_eq_char              1 vcmpeq inst
   test_eq_short             1 vcmpeq inst
   test_eq_int               1 vcmpeq inst
   test_eq_long              1 vcmpeq inst
   test_ne_char              1 vcmpeq, 1 xxlnor inst
   test_ne_short             1 vcmpeq, 1 xxlnor inst
   test_ne_int               1 vcmpeq, 1 xxlnor inst
   test_ne_long              1 vcmpeq, 1 xxlnor inst */

/* { dg-final { scan-assembler-times "vcmpeq" 8 } } */
/* { dg-final { scan-assembler-times "xxlnor" 4 } } */
