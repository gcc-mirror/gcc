/* Verify that overloaded built-ins for __builtin_vec_xst with long long
   inputs produce the right code.  */

/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mvsx -O2 -mcpu=power8" } */

#include <altivec.h>

#define BUILD_VAR_TEST(TESTNAME1, VALUE, VAR_OFFSET, SAVETO)		\
void									\
TESTNAME1 ## _var (VALUE value, VAR_OFFSET offset, SAVETO * saveto) 	\
{									\
	__builtin_vec_xst (value, offset, saveto);		\
}

#define BUILD_CST_TEST(TESTNAME1, VALUE, CST_OFFSET, SAVETO)		\
void									\
TESTNAME1 ## _cst (VALUE value, SAVETO * saveto) 			\
{									\
	__builtin_vec_xst (value, CST_OFFSET, saveto);		\
}

BUILD_VAR_TEST( test1,  vector signed long long, signed long long, signed long long );
BUILD_VAR_TEST( test2,  vector signed long long, signed int, signed long long );
BUILD_CST_TEST( test3,  vector signed long long, 12, signed long long );

BUILD_VAR_TEST( test4,  vector unsigned long long, signed long long, unsigned long long );
BUILD_VAR_TEST( test5,  vector unsigned long long, signed int, unsigned long long );
BUILD_CST_TEST( test6,  vector unsigned long long, 12, unsigned long long );

BUILD_VAR_TEST( test7,  vector signed long long, signed long long, vector signed long long );
BUILD_VAR_TEST( test8,  vector signed long long, signed int, vector signed long long );
BUILD_CST_TEST( test9,  vector signed long long, 12, vector signed long long );

BUILD_VAR_TEST( test10,  vector unsigned long long, signed long long, vector unsigned long long );
BUILD_VAR_TEST( test11,  vector unsigned long long, signed int, vector unsigned long long );
BUILD_CST_TEST( test12,  vector unsigned long long, 12, vector unsigned long long );

/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstxvx\M|\mstvx\M} 12 } } */
