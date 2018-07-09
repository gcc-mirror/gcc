/* Verify that overloaded built-ins for vec_vsx_ld with float
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

BUILD_VAR_TEST( test1,  vector float, signed long long, float);
BUILD_VAR_TEST( test3,  vector float, signed int, float);
BUILD_CST_TEST( test4,  vector float, 12, float);

BUILD_VAR_TEST( test5,  vector float, signed long long, vector float);
BUILD_VAR_TEST( test7,  vector float, signed int, vector float);
BUILD_CST_TEST( test8,  vector float, 12, vector float);

/* { dg-final { scan-assembler-times {\mlxvw4x\M|\mlxvd2x\M|\mlxvx\M|\mlvx\M} 6 } } */
