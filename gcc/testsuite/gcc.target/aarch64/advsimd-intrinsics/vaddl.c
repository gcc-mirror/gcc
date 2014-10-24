#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x33, 0x33, 0x33, 0x33,
				       0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x33, 0x33 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x3, 0x3, 0x3, 0x3,
					0x3, 0x3, 0x3, 0x3 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x37, 0x37, 0x37, 0x37 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x3, 0x3 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,16,8) [] = {  0xffe3, 0xffe4, 0xffe5, 0xffe6,
					 0xffe7, 0xffe8, 0xffe9, 0xffea };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffe2, 0xffffffe3,
					0xffffffe4, 0xffffffe5 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffffe0,
					0xffffffffffffffe1 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x1e3, 0x1e4, 0x1e5, 0x1e6,
					 0x1e7, 0x1e8, 0x1e9, 0x1ea };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x1ffe1, 0x1ffe2,
					 0x1ffe3, 0x1ffe4 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x1ffffffe0, 0x1ffffffe1 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x33333333, 0x33333333,
					   0x33333333, 0x33333333 };

#ifndef INSN_NAME
#define INSN_NAME vaddl
#define TEST_MSG "VADDL"
#endif

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=vaddl(x1,x2), then store the result.  */
#define TEST_VADDL1(INSN, T1, T2, W, W2, N)				\
  VECT_VAR(vector_res, T1, W2, N) =					\
    INSN##_##T2##W(VECT_VAR(vector, T1, W, N),				\
		   VECT_VAR(vector2, T1, W, N));			\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector_res, T1, W2, N))

#define TEST_VADDL(INSN, T1, T2, W, W2, N)	\
  TEST_VADDL1(INSN, T1, T2, W, W2, N)

  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, uint, 8, 8);
  DECL_VARIABLE(vector, uint, 16, 4);
  DECL_VARIABLE(vector, uint, 32, 2);

  DECL_VARIABLE(vector2, int, 8, 8);
  DECL_VARIABLE(vector2, int, 16, 4);
  DECL_VARIABLE(vector2, int, 32, 2);
  DECL_VARIABLE(vector2, uint, 8, 8);
  DECL_VARIABLE(vector2, uint, 16, 4);
  DECL_VARIABLE(vector2, uint, 32, 2);

  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, uint, 64, 2);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  VLOAD(vector, buffer, , int, s, 8, 8);
  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, , uint, u, 8, 8);
  VLOAD(vector, buffer, , uint, u, 16, 4);
  VLOAD(vector, buffer, , uint, u, 32, 2);

  /* Choose init value arbitrarily.  */
  VDUP(vector2, , int, s, 8, 8, -13);
  VDUP(vector2, , int, s, 16, 4, -14);
  VDUP(vector2, , int, s, 32, 2, -16);
  VDUP(vector2, , uint, u, 8, 8, 0xf3);
  VDUP(vector2, , uint, u, 16, 4, 0xfff1);
  VDUP(vector2, , uint, u, 32, 2, 0xfffffff0);

  /* Execute the tests.  */
  TEST_VADDL(INSN_NAME, int, s, 8, 16, 8);
  TEST_VADDL(INSN_NAME, int, s, 16, 32, 4);
  TEST_VADDL(INSN_NAME, int, s, 32, 64, 2);
  TEST_VADDL(INSN_NAME, uint, u, 8, 16, 8);
  TEST_VADDL(INSN_NAME, uint, u, 16, 32, 4);
  TEST_VADDL(INSN_NAME, uint, u, 32, 64, 2);

  CHECK_RESULTS (TEST_MSG, "");
}

int main (void)
{
  FNNAME (INSN_NAME);
  return 0;
}
