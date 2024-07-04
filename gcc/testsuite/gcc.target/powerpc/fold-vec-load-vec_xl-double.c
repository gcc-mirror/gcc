/* Verify that overloaded built-ins for vec_xl with double
   inputs produce the right code.  */

/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

#define BUILD_VAR_TEST(TESTNAME1, RETTYPE, VAR_OFFSET, LOADFROM)	\
RETTYPE									\
TESTNAME1 ## _var (VAR_OFFSET offset, LOADFROM * loadfrom) 		\
{									\
	return vec_xl (offset, loadfrom);				\
}

#define BUILD_CST_TEST(TESTNAME1, RETTYPE, CST_OFFSET, LOADFROM)	\
RETTYPE									\
TESTNAME1 ## _cst (LOADFROM * loadfrom) 				\
{									\
	return vec_xl (CST_OFFSET, loadfrom);				\
}

BUILD_VAR_TEST( test1,  vector double, signed long long, vector double);
BUILD_VAR_TEST( test2,  vector double, signed int, vector double);
BUILD_CST_TEST( test3,  vector double, 12, vector double);

BUILD_VAR_TEST( test4,  vector double, signed long long, double);
BUILD_VAR_TEST( test5,  vector double, signed int, double);
BUILD_CST_TEST( test6,  vector double, 12, double);

/* { dg-final { scan-assembler-times {\mlxvd2x\M|\mlxvx\M|\mlvx\M|\mplxv\M} 6 } } */
