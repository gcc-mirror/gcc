#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf6, 0xf7, 0xf8, 0xf9,
				       0xfa, 0xfb, 0xfc, 0xfd };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x16, 0x17, 0x18, 0x19 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x20, 0x21 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x53, 0x54, 0x55, 0x56,
					0x57, 0x58, 0x59, 0x5a };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x907, 0x908, 0x909, 0x90a };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffe7, 0xffffffe8 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x5e, 0x5f, 0x60, 0x61,
					0x62, 0x63, 0x64, 0x65,
					0x66, 0x67, 0x68, 0x69,
					0x6a, 0x6b, 0x6c, 0x6d };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xb9c, 0xb9d, 0xb9e, 0xb9f,
					0xba0, 0xba1, 0xba2, 0xba3 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x26e0, 0x26e1, 0x26e2, 0x26e3 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					 0xfc, 0xfd, 0xfe, 0xff,
					 0x0, 0x1, 0x2, 0x3,
					 0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff9, 0xfffa, 0xfffb, 0xfffc,
					 0xfffd, 0xfffe, 0xffff, 0x0 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xc, 0xd, 0xe, 0xf };

#define TEST_MSG "VABA/VABAQ"
void exec_vaba (void)
{
  /* Basic test: v4=vaba(v1,v2,v3), then store the result.  */
#define TEST_VABA(Q, T1, T2, W, N)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vaba##Q##_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N),			\
		      VECT_VAR(vector3, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define DECL_VABA_VAR(VAR)			\
  DECL_VARIABLE(VAR, int, 8, 8);		\
  DECL_VARIABLE(VAR, int, 16, 4);		\
  DECL_VARIABLE(VAR, int, 32, 2);		\
  DECL_VARIABLE(VAR, uint, 8, 8);		\
  DECL_VARIABLE(VAR, uint, 16, 4);		\
  DECL_VARIABLE(VAR, uint, 32, 2);		\
  DECL_VARIABLE(VAR, int, 8, 16);		\
  DECL_VARIABLE(VAR, int, 16, 8);		\
  DECL_VARIABLE(VAR, int, 32, 4);		\
  DECL_VARIABLE(VAR, uint, 8, 16);		\
  DECL_VARIABLE(VAR, uint, 16, 8);		\
  DECL_VARIABLE(VAR, uint, 32, 4)

  DECL_VABA_VAR(vector1);
  DECL_VABA_VAR(vector2);
  DECL_VABA_VAR(vector3);
  DECL_VABA_VAR(vector_res);

  clean_results ();

  /* Initialize input "vector1" from "buffer".  */
  VLOAD(vector1, buffer, , int, s, 8, 8);
  VLOAD(vector1, buffer, , int, s, 16, 4);
  VLOAD(vector1, buffer, , int, s, 32, 2);
  VLOAD(vector1, buffer, , uint, u, 8, 8);
  VLOAD(vector1, buffer, , uint, u, 16, 4);
  VLOAD(vector1, buffer, , uint, u, 32, 2);
  VLOAD(vector1, buffer, q, int, s, 8, 16);
  VLOAD(vector1, buffer, q, int, s, 16, 8);
  VLOAD(vector1, buffer, q, int, s, 32, 4);
  VLOAD(vector1, buffer, q, uint, u, 8, 16);
  VLOAD(vector1, buffer, q, uint, u, 16, 8);
  VLOAD(vector1, buffer, q, uint, u, 32, 4);

  /* Choose init value arbitrarily.  */
  VDUP(vector2, , int, s, 8, 8, 1);
  VDUP(vector2, , int, s, 16, 4, -13);
  VDUP(vector2, , int, s, 32, 2, 8);
  VDUP(vector2, , uint, u, 8, 8, 1);
  VDUP(vector2, , uint, u, 16, 4, 13);
  VDUP(vector2, , uint, u, 32, 2, 8);
  VDUP(vector2, q, int, s, 8, 16, 10);
  VDUP(vector2, q, int, s, 16, 8, -12);
  VDUP(vector2, q, int, s, 32, 4, 32);
  VDUP(vector2, q, uint, u, 8, 16, 10);
  VDUP(vector2, q, uint, u, 16, 8, 12);
  VDUP(vector2, q, uint, u, 32, 4, 32);

  /* Choose init value arbitrarily.  */
  VDUP(vector3, , int, s, 8, 8, -5);
  VDUP(vector3, , int, s, 16, 4, 25);
  VDUP(vector3, , int, s, 32, 2, -40);
  VDUP(vector3, , uint, u, 8, 8, 100);
  VDUP(vector3, , uint, u, 16, 4, 2340);
  VDUP(vector3, , uint, u, 32, 2, 0xffffffff);
  VDUP(vector3, q, int, s, 8, 16, -100);
  VDUP(vector3, q, int, s, 16, 8, -3000);
  VDUP(vector3, q, int, s, 32, 4, 10000);
  VDUP(vector3, q, uint, u, 8, 16, 2);
  VDUP(vector3, q, uint, u, 16, 8, 3);
  VDUP(vector3, q, uint, u, 32, 4, 4);

  /* Execute the tests.  */
  TEST_VABA(, int, s, 8, 8);
  TEST_VABA(, int, s, 16, 4);
  TEST_VABA(, int, s, 32, 2);
  TEST_VABA(, uint, u, 8, 8);
  TEST_VABA(, uint, u, 16, 4);
  TEST_VABA(, uint, u, 32, 2);
  TEST_VABA(q, int, s, 8, 16);
  TEST_VABA(q, int, s, 16, 8);
  TEST_VABA(q, int, s, 32, 4);
  TEST_VABA(q, uint, u, 8, 16);
  TEST_VABA(q, uint, u, 16, 8);
  TEST_VABA(q, uint, u, 32, 4);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
}

int main (void)
{
  exec_vaba ();
  return 0;
}
