/* Verify that overloaded built-ins for vec_xst with char
   inputs produce the right code.  */

/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>
#define BUILD_VAR_TEST(TESTNAME1, VALUE, VAR_OFFSET, SAVETO)		\
void									\
TESTNAME1 ## _var (VALUE value, VAR_OFFSET offset, SAVETO * saveto) 	\
{									\
	vec_xst (value, offset, saveto);		\
}

#define BUILD_CST_TEST(TESTNAME1, VALUE, CST_OFFSET, SAVETO)		\
void									\
TESTNAME1 ## _cst (VALUE value, SAVETO * saveto) 			\
{									\
	vec_xst (value, CST_OFFSET, saveto);		\
}

BUILD_VAR_TEST( test1,  vector signed char, signed long long, signed char );
BUILD_VAR_TEST( test2,  vector signed char, signed int, signed char );
BUILD_CST_TEST( test3,  vector signed char, 12, signed char );

BUILD_VAR_TEST( test4,  vector unsigned char, signed long long, unsigned char );
BUILD_VAR_TEST( test5,  vector unsigned char, signed int, unsigned char );
BUILD_CST_TEST( test6,  vector unsigned char, 12, unsigned char );

BUILD_VAR_TEST( test7,  vector signed char, signed long long, vector signed char );
BUILD_VAR_TEST( test8,  vector signed char, signed int, vector signed char );
BUILD_CST_TEST( test9,  vector signed char, 12, vector signed char );

BUILD_VAR_TEST( test10, vector unsigned char, signed long long, vector unsigned char );
BUILD_VAR_TEST( test11, vector unsigned char, signed int, vector unsigned char );
BUILD_CST_TEST( test12, vector unsigned char, 12, vector unsigned char );

/* { dg-final { scan-assembler-times {\mstxvw4x\M|\mstxvd2x\M|\mstxvx\M|\mstvx\M|\mpstxv\M} 12 } } */
