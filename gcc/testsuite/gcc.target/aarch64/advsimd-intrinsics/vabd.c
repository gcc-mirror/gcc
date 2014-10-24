#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x11, 0x10, 0xf, 0xe,
				       0xd, 0xc, 0xb, 0xa };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x3, 0x2, 0x1, 0x0 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x18, 0x17 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xef, 0xf0, 0xf1, 0xf2,
					0xf3, 0xf4, 0xf5, 0xf6 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xffe3, 0xffe4, 0xffe5, 0xffe6 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffe8, 0xffffffe9 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x41c26666, 0x41ba6666 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x1a, 0x19, 0x18, 0x17,
					0x16, 0x15, 0x14, 0x13,
					0x12, 0x11, 0x10, 0xf,
					0xe, 0xd, 0xc, 0xb };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x4, 0x3, 0x2, 0x1,
					0x0, 0x1, 0x2, 0x3 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x30, 0x2f, 0x2e, 0x2d };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x3333333333333333,
					0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xe6, 0xe7, 0xe8, 0xe9,
					 0xea, 0xeb, 0xec, 0xed,
					 0xee, 0xef, 0xf0, 0xf1,
					 0xf2, 0xf3, 0xf4, 0xf5 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xffe4, 0xffe5, 0xffe6, 0xffe7,
					 0xffe8, 0xffe9, 0xffea, 0xffeb };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffffd0, 0xffffffd1,
					 0xffffffd2, 0xffffffd3 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x3333333333333333,
					 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x42407ae1, 0x423c7ae1,
					   0x42387ae1, 0x42347ae1 };

/* Additional expected results for float32 variants with specially
   chosen input values.  */
VECT_VAR_DECL(expected_float32,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

#define TEST_MSG "VABD/VABDQ"
void exec_vabd (void)
{
  /* Basic test: v4=vabd(v1,v2), then store the result.  */
#define TEST_VABD(Q, T1, T2, W, N)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vabd##Q##_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define DECL_VABD_VAR(VAR)			\
  DECL_VARIABLE(VAR, int, 8, 8);		\
  DECL_VARIABLE(VAR, int, 16, 4);		\
  DECL_VARIABLE(VAR, int, 32, 2);		\
  DECL_VARIABLE(VAR, uint, 8, 8);		\
  DECL_VARIABLE(VAR, uint, 16, 4);		\
  DECL_VARIABLE(VAR, uint, 32, 2);		\
  DECL_VARIABLE(VAR, float, 32, 2);		\
  DECL_VARIABLE(VAR, int, 8, 16);		\
  DECL_VARIABLE(VAR, int, 16, 8);		\
  DECL_VARIABLE(VAR, int, 32, 4);		\
  DECL_VARIABLE(VAR, uint, 8, 16);		\
  DECL_VARIABLE(VAR, uint, 16, 8);		\
  DECL_VARIABLE(VAR, uint, 32, 4);		\
  DECL_VARIABLE(VAR, float, 32, 4)

  DECL_VABD_VAR(vector1);
  DECL_VABD_VAR(vector2);
  DECL_VABD_VAR(vector_res);

  clean_results ();

  /* Initialize input "vector1" from "buffer".  */
  VLOAD(vector1, buffer, , int, s, 8, 8);
  VLOAD(vector1, buffer, , int, s, 16, 4);
  VLOAD(vector1, buffer, , int, s, 32, 2);
  VLOAD(vector1, buffer, , uint, u, 8, 8);
  VLOAD(vector1, buffer, , uint, u, 16, 4);
  VLOAD(vector1, buffer, , uint, u, 32, 2);
  VLOAD(vector1, buffer, , float, f, 32, 2);
  VLOAD(vector1, buffer, q, int, s, 8, 16);
  VLOAD(vector1, buffer, q, int, s, 16, 8);
  VLOAD(vector1, buffer, q, int, s, 32, 4);
  VLOAD(vector1, buffer, q, uint, u, 8, 16);
  VLOAD(vector1, buffer, q, uint, u, 16, 8);
  VLOAD(vector1, buffer, q, uint, u, 32, 4);
  VLOAD(vector1, buffer, q, float, f, 32, 4);

  /* Choose init value arbitrarily.  */
  VDUP(vector2, , int, s, 8, 8, 1);
  VDUP(vector2, , int, s, 16, 4, -13);
  VDUP(vector2, , int, s, 32, 2, 8);
  VDUP(vector2, , uint, u, 8, 8, 1);
  VDUP(vector2, , uint, u, 16, 4, 13);
  VDUP(vector2, , uint, u, 32, 2, 8);
  VDUP(vector2, , float, f, 32, 2, 8.3f);
  VDUP(vector2, q, int, s, 8, 16, 10);
  VDUP(vector2, q, int, s, 16, 8, -12);
  VDUP(vector2, q, int, s, 32, 4, 32);
  VDUP(vector2, q, uint, u, 8, 16, 10);
  VDUP(vector2, q, uint, u, 16, 8, 12);
  VDUP(vector2, q, uint, u, 32, 4, 32);
  VDUP(vector2, q, float, f, 32, 4, 32.12f);

  /* Execute the tests.  */
  TEST_VABD(, int, s, 8, 8);
  TEST_VABD(, int, s, 16, 4);
  TEST_VABD(, int, s, 32, 2);
  TEST_VABD(, uint, u, 8, 8);
  TEST_VABD(, uint, u, 16, 4);
  TEST_VABD(, uint, u, 32, 2);
  TEST_VABD(, float, f, 32, 2);
  TEST_VABD(q, int, s, 8, 16);
  TEST_VABD(q, int, s, 16, 8);
  TEST_VABD(q, int, s, 32, 4);
  TEST_VABD(q, uint, u, 8, 16);
  TEST_VABD(q, uint, u, 16, 8);
  TEST_VABD(q, uint, u, 32, 4);
  TEST_VABD(q, float, f, 32, 4);

  CHECK_RESULTS (TEST_MSG, "");


  /* Extra FP tests with special values (-0.0, ....) */
  VDUP(vector1, q, float, f, 32, 4, -0.0f);
  VDUP(vector2, q, float, f, 32, 4, 0.0);
  TEST_VABD(q, float, f, 32, 4);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_float32, " FP special (-0.0)");

  /* Extra FP tests with special values (-0.0, ....) */
  VDUP(vector1, q, float, f, 32, 4, 0.0f);
  VDUP(vector2, q, float, f, 32, 4, -0.0);
  TEST_VABD(q, float, f, 32, 4);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_float32, " FP special (-0.0)");
}

int main (void)
{
  exec_vabd ();
  return 0;
}
