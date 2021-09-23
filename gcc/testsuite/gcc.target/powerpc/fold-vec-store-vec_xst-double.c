/* Verify that overloaded built-ins for vec_xst with double
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

BUILD_VAR_TEST( test1,  vector double, signed long long, double );
BUILD_VAR_TEST( test2,  vector double, signed int, double );
BUILD_CST_TEST( test3,  vector double, 12, double );

BUILD_VAR_TEST( test7,  vector double, signed long long, vector double );
BUILD_VAR_TEST( test8,  vector double, signed int, vector double );
BUILD_CST_TEST( test9,  vector double, 12, vector double );

/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstxvx\M|\mstvx\M|\mpstxv\M} 6 } } */
