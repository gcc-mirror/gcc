/* Verify that overloaded built-ins for __builtin_vec_xl with short
   inputs produce the right code.  */

/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

#define BUILD_VAR_TEST(TESTNAME1, RETTYPE, VAR_OFFSET, LOADFROM)\
RETTYPE								\
TESTNAME1 ## _var (VAR_OFFSET offset, LOADFROM * loadfrom) 		\
{								\
	return __builtin_vec_xl (offset, loadfrom);		\
}

#define BUILD_CST_TEST(TESTNAME1, RETTYPE, CST_OFFSET, LOADFROM)	\
RETTYPE								\
TESTNAME1 ## _cst (LOADFROM * loadfrom) 				\
{								\
	return __builtin_vec_xl (CST_OFFSET, loadfrom);		\
}

BUILD_VAR_TEST( test1,  vector signed short, signed long long, signed short);
BUILD_VAR_TEST( test2,  vector signed short, signed int, signed short);
BUILD_CST_TEST( test3,  vector signed short, 12, signed short);

BUILD_VAR_TEST( test4,  vector unsigned short, signed long long, unsigned short);
BUILD_VAR_TEST( test5,  vector unsigned short, signed int, unsigned short);
BUILD_CST_TEST( test6,  vector unsigned short, 12, unsigned short);

BUILD_VAR_TEST( test7,  vector signed short, signed long long, vector signed short);
BUILD_VAR_TEST( test8,  vector signed short, signed int, vector signed short);
BUILD_CST_TEST( test9,  vector signed short, 12, vector signed short);

BUILD_VAR_TEST( test10,  vector unsigned short, signed long long, vector unsigned short);
BUILD_VAR_TEST( test11,  vector unsigned short, signed int, vector unsigned short);
BUILD_CST_TEST( test12,  vector unsigned short, 12, vector unsigned short);

/* { dg-final { scan-assembler-times {\mlxvw4x\M|\mlxvd2x\M|\mlxvx\M|\mp?lxv\M|\mlvx\M} 12 } } */
