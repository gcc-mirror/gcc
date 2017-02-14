/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>

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

vector bool long long
test_ne_long (vector bool long long x, vector bool long long y)
{
	return vec_cmpne (x, y);
}

/* Expected test results:

     test_ne_char              1 vcmpneb
     test_ne_short             1 vcmpneh
     test_ne_int               1 vcmpnew
     test_ne_long              1 vcmpequd, 1 xxlnor inst */

/* { dg-final { scan-assembler-times "vcmpneb"  1 } } */
/* { dg-final { scan-assembler-times "vcmpneh"  1 } } */
/* { dg-final { scan-assembler-times "vcmpnew"  1 } } */
/* { dg-final { scan-assembler-times "vcmpequd" 1 } } */
/* { dg-final { scan-assembler-times "xxlnor"   1 } } */
