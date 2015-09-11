#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff6, 0xfff7, 0xfff8, 0xfff9,
					0xfffa, 0xfffb, 0xfffc, 0xfffd };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x16, 0x17, 0x18, 0x19 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x20, 0x21 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x53, 0x54, 0x55, 0x56,
					 0x57, 0x58, 0x59, 0x5a };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x907, 0x908, 0x909, 0x90a };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffe7,
					 0xffffffe8 };

/* Expected results for cases with input values chosen to test
   possible intermediate overflow.  */
VECT_VAR_DECL(expected2,int,16,8) [] = { 0xef, 0xf0, 0xf1, 0xf2,
					 0xf3, 0xf4, 0xf5, 0xf6 };
VECT_VAR_DECL(expected2,int,32,4) [] = { 0xffef, 0xfff0, 0xfff1, 0xfff2 };
VECT_VAR_DECL(expected2,int,64,2) [] = { 0xffffffef, 0xfffffff0 };
VECT_VAR_DECL(expected2,uint,16,8) [] = { 0xee, 0xef, 0xf0, 0xf1,
					  0xf2, 0xf3, 0xf4, 0xf5 };
VECT_VAR_DECL(expected2,uint,32,4) [] = { 0xffe2, 0xffe3, 0xffe4, 0xffe5 };
VECT_VAR_DECL(expected2,uint,64,2) [] = { 0xffffffe7, 0xffffffe8 };

#define TEST_MSG "VABAL"
void exec_vabal (void)
{
  /* Basic test: v4=vabal(v1,v2,v3), then store the result.  */
#define TEST_VABAL(T1, T2, W, W2, N)					\
  VECT_VAR(vector_res, T1, W2, N) =					\
    vabal_##T2##W(VECT_VAR(vector1, T1, W2, N),				\
		  VECT_VAR(vector2, T1, W, N),				\
		  VECT_VAR(vector3, T1, W, N));				\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector_res, T1, W2, N))

#define DECL_VABAL_VAR_LONG(VAR)		\
  DECL_VARIABLE(VAR, int, 16, 8);		\
  DECL_VARIABLE(VAR, int, 32, 4);		\
  DECL_VARIABLE(VAR, int, 64, 2);		\
  DECL_VARIABLE(VAR, uint, 16, 8);		\
  DECL_VARIABLE(VAR, uint, 32, 4);		\
  DECL_VARIABLE(VAR, uint, 64, 2)

#define DECL_VABAL_VAR_SHORT(VAR)		\
  DECL_VARIABLE(VAR, int, 8, 8);		\
  DECL_VARIABLE(VAR, int, 16, 4);		\
  DECL_VARIABLE(VAR, int, 32, 2);		\
  DECL_VARIABLE(VAR, uint, 8, 8);		\
  DECL_VARIABLE(VAR, uint, 16, 4);		\
  DECL_VARIABLE(VAR, uint, 32, 2)

  DECL_VABAL_VAR_LONG(vector1);
  DECL_VABAL_VAR_SHORT(vector2);
  DECL_VABAL_VAR_SHORT(vector3);
  DECL_VABAL_VAR_LONG(vector_res);

  clean_results ();

  /* Initialize input "vector1" from "buffer".  */
  VLOAD(vector1, buffer, q, int, s, 16, 8);
  VLOAD(vector1, buffer, q, int, s, 32, 4);
  VLOAD(vector1, buffer, q, int, s, 64, 2);
  VLOAD(vector1, buffer, q, uint, u, 16, 8);
  VLOAD(vector1, buffer, q, uint, u, 32, 4);
  VLOAD(vector1, buffer, q, uint, u, 64, 2);


  /* Choose init value arbitrarily.  */
  VDUP(vector2, , int, s, 8, 8, 1);
  VDUP(vector2, , int, s, 16, 4, -13);
  VDUP(vector2, , int, s, 32, 2, 8);
  VDUP(vector2, , uint, u, 8, 8, 1);
  VDUP(vector2, , uint, u, 16, 4, 13);
  VDUP(vector2, , uint, u, 32, 2, 8);

  /* Choose init value arbitrarily.  */
  VDUP(vector3, , int, s, 8, 8, -5);
  VDUP(vector3, , int, s, 16, 4, 25);
  VDUP(vector3, , int, s, 32, 2, -40);
  VDUP(vector3, , uint, u, 8, 8, 100);
  VDUP(vector3, , uint, u, 16, 4, 2340);
  VDUP(vector3, , uint, u, 32, 2, 0xffffffff);

  /* Execute the tests.  */
  TEST_VABAL(int, s, 8, 16, 8);
  TEST_VABAL(int, s, 16, 32, 4);
  TEST_VABAL(int, s, 32, 64, 2);
  TEST_VABAL(uint, u, 8, 16, 8);
  TEST_VABAL(uint, u, 16, 32, 4);
  TEST_VABAL(uint, u, 32, 64, 2);

  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");

  /* Use values that could lead to overflow intermediate
   * calculations.  */
  VDUP(vector2, , int, s, 8, 8, 0x80);
  VDUP(vector2, , int, s, 16, 4, 0x8000);
  VDUP(vector2, , int, s, 32, 2, 0x80000000);
  VDUP(vector2, , uint, u, 8, 8, 1);
  VDUP(vector2, , uint, u, 16, 4, 13);
  VDUP(vector2, , uint, u, 32, 2, 8);

  VDUP(vector3, , int, s, 8, 8, 0x7f);
  VDUP(vector3, , int, s, 16, 4, 0x7fff);
  VDUP(vector3, , int, s, 32, 2, 0x7fffffff);
  VDUP(vector3, , uint, u, 8, 8, 0xff);
  VDUP(vector3, , uint, u, 16, 4, 0xffff);
  VDUP(vector3, , uint, u, 32, 2, 0xffffffff);

  TEST_VABAL(int, s, 8, 16, 8);
  TEST_VABAL(int, s, 16, 32, 4);
  TEST_VABAL(int, s, 32, 64, 2);
  TEST_VABAL(uint, u, 8, 16, 8);
  TEST_VABAL(uint, u, 16, 32, 4);
  TEST_VABAL(uint, u, 32, 64, 2);

  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected2, " test intermediate overflow");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected2, " test intermediate overflow");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected2, " test intermediate overflow");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected2, " test intermediate overflow");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected2, " test intermediate overflow");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected2, " test intermediate overflow");
}

int main (void)
{
  exec_vabal ();
  return 0;
}
