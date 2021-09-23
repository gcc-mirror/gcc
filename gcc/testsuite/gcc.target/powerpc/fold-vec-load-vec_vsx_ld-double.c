/* Verify that overloaded built-ins for vec_vsx_ld with double
   inputs produce the right code.  */

/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>
#define BUILD_VAR_TEST(TESTNAME1, RETTYPE, VAR_OFFSET, LOADFROM)	\
RETTYPE									\
TESTNAME1 ## _var (VAR_OFFSET offset, LOADFROM * loadfrom) 		\
{									\
	return vec_vsx_ld (offset, loadfrom);				\
}

#define BUILD_CST_TEST(TESTNAME1, RETTYPE, CST_OFFSET, LOADFROM)	\
RETTYPE									\
TESTNAME1 ## _cst (LOADFROM * loadfrom) 				\
{									\
	return vec_vsx_ld (CST_OFFSET, loadfrom);			\
}

BUILD_VAR_TEST( test1, vector  double, long long, double);
BUILD_VAR_TEST( test2, vector  double, int, double);
BUILD_CST_TEST( test3, vector  double, 12, double);

BUILD_VAR_TEST( test4, vector  double, int, vector double);
BUILD_VAR_TEST( test5, vector  double, long long, vector double);
BUILD_CST_TEST( test6, vector  double, 12, vector double);

/* { dg-final { scan-assembler-times {\mlxvd2x\M|\mlxvx\M|\mlvx\M|\mplxv\M} 6 } } */
