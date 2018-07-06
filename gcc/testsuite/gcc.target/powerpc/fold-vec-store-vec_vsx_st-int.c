/* Verify that overloaded built-ins for vec_vsx_st with int
   inputs produce the right code.  */

/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

#define BUILD_VAR_TEST(TESTNAME1, VALUE, VAR_OFFSET, SAVETO)		\
void									\
TESTNAME1 ## _var (VALUE value, VAR_OFFSET offset, SAVETO * saveto) 	\
{									\
	vec_vsx_st (value, offset, saveto);		\
}

#define BUILD_CST_TEST(TESTNAME1, VALUE, CST_OFFSET, SAVETO)		\
void									\
TESTNAME1 ## _cst (VALUE value, SAVETO * saveto) 			\
{									\
	vec_vsx_st (value, CST_OFFSET, saveto);		\
}

BUILD_VAR_TEST( test1,  vector signed int, signed long long, signed int );
BUILD_VAR_TEST( test2,  vector signed int, signed int, signed int );
BUILD_CST_TEST( test3,  vector signed int, 12, signed int );

BUILD_VAR_TEST( test4,  vector unsigned int, signed long long, unsigned int );
BUILD_VAR_TEST( test5,  vector unsigned int, signed int, unsigned int );
BUILD_CST_TEST( test6,  vector unsigned int, 12, unsigned int );

BUILD_VAR_TEST( test7,  vector signed int, signed long long, vector signed int );
BUILD_VAR_TEST( test8,  vector signed int, signed int, vector signed int );
BUILD_CST_TEST( test9,  vector signed int, 12, vector signed int );

BUILD_VAR_TEST( test10, vector unsigned int, signed long long, vector unsigned int );
BUILD_VAR_TEST( test11, vector unsigned int, signed int, vector unsigned int );
BUILD_CST_TEST( test12, vector unsigned int, 12, vector unsigned int );

/* { dg-final { scan-assembler-times {\mstxvw4x\M|\mstxvd2x\M|\mstxvx\M|\mstvx\M} 12 } } */
