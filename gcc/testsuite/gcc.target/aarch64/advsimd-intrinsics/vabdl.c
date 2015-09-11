#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = { 0x11, 0x10, 0xf, 0xe,
					0xd, 0xc, 0xb, 0xa };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x3, 0x2, 0x1, 0x0 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x18, 0x17 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xef, 0xf0, 0xf1, 0xf2,
					 0xf3, 0xf4, 0xf5, 0xf6 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffe3, 0xffe4, 0xffe5, 0xffe6 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffe8,
					 0xffffffe9 };

#define TEST_MSG "VABDL"
void exec_vabdl (void)
{
  /* Basic test: v4=vabdl(v1,v2), then store the result.  */
#define TEST_VABDL(T1, T2, W, W2, N)					\
  VECT_VAR(vector_res, T1, W2, N) =					\
    vabdl_##T2##W(VECT_VAR(vector1, T1, W, N),				\
		  VECT_VAR(vector2, T1, W, N));				\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector_res, T1, W2, N))

#define DECL_VABDL_VAR_LONG(VAR)		\
  DECL_VARIABLE(VAR, int, 16, 8);		\
  DECL_VARIABLE(VAR, int, 32, 4);		\
  DECL_VARIABLE(VAR, int, 64, 2);		\
  DECL_VARIABLE(VAR, uint, 16, 8);		\
  DECL_VARIABLE(VAR, uint, 32, 4);		\
  DECL_VARIABLE(VAR, uint, 64, 2)

#define DECL_VABDL_VAR_SHORT(VAR)		\
  DECL_VARIABLE(VAR, int, 8, 8);		\
  DECL_VARIABLE(VAR, int, 16, 4);		\
  DECL_VARIABLE(VAR, int, 32, 2);		\
  DECL_VARIABLE(VAR, uint, 8, 8);		\
  DECL_VARIABLE(VAR, uint, 16, 4);		\
  DECL_VARIABLE(VAR, uint, 32, 2)

  DECL_VABDL_VAR_SHORT(vector1);
  DECL_VABDL_VAR_SHORT(vector2);
  DECL_VABDL_VAR_LONG(vector_res);

  clean_results ();

  /* Initialize input "vector1" from "buffer".  */
  VLOAD(vector1, buffer, , int, s, 8, 8);
  VLOAD(vector1, buffer, , int, s, 16, 4);
  VLOAD(vector1, buffer, , int, s, 32, 2);
  VLOAD(vector1, buffer, , uint, u, 8, 8);
  VLOAD(vector1, buffer, , uint, u, 16, 4);
  VLOAD(vector1, buffer, , uint, u, 32, 2);

  /* Choose init value arbitrarily.  */
  VDUP(vector2, , int, s, 8, 8, 1);
  VDUP(vector2, , int, s, 16, 4, -13);
  VDUP(vector2, , int, s, 32, 2, 8);
  VDUP(vector2, , uint, u, 8, 8, 1);
  VDUP(vector2, , uint, u, 16, 4, 13);
  VDUP(vector2, , uint, u, 32, 2, 8);

  /* Execute the tests.  */
  TEST_VABDL(int, s, 8, 16, 8);
  TEST_VABDL(int, s, 16, 32, 4);
  TEST_VABDL(int, s, 32, 64, 2);
  TEST_VABDL(uint, u, 8, 16, 8);
  TEST_VABDL(uint, u, 16, 32, 4);
  TEST_VABDL(uint, u, 32, 64, 2);

  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
}

int main (void)
{
  exec_vabdl ();
  return 0;
}
