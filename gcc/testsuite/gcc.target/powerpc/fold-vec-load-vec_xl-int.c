/* Verify that overloaded built-ins for vec_xl with int
   inputs produce the right code.  */

/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

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

BUILD_VAR_TEST( test1,  vector int, signed long long, int);
BUILD_VAR_TEST( test2,  vector int, signed int, int);
BUILD_CST_TEST( test3,  vector int, 12, int);

BUILD_VAR_TEST( test4,  vector int, signed long long, vector int);
BUILD_VAR_TEST( test5,  vector int, signed int, vector int);
BUILD_CST_TEST( test6,  vector int, 12, vector int);

BUILD_VAR_TEST( test7,  vector unsigned int, signed long long, unsigned int);
BUILD_VAR_TEST( test8,  vector unsigned int, signed int, unsigned int);
BUILD_CST_TEST( test9,  vector unsigned int, 12, unsigned int);

BUILD_VAR_TEST( test10,  vector unsigned int, signed long long, vector unsigned int);
BUILD_VAR_TEST( test11,  vector unsigned int, signed int, vector unsigned int);
BUILD_CST_TEST( test12,  vector unsigned int, 12, vector unsigned int);

/* { dg-final { scan-assembler-times {\mlxvw4x\M|\mlxvd2x\M|\mlxvx\M|\mlvx\M} 12 } } */
