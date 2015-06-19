#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = { 0xffe0, 0xffe2, 0xffe4, 0xffe6,
					0xffe8, 0xffea, 0xffec, 0xffee };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffe0, 0xffffffe2,
					0xffffffe4, 0xffffffe6 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffff80, 0xffffffffffffff88 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x3c0, 0x3c4, 0x3c8, 0x3cc,
					 0x3d0, 0x3d4, 0x3d8, 0x3dc };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfff00, 0xfff10, 0xfff20, 0xfff30 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x7ffffff80, 0x7ffffff88 };

#define TEST_MSG "VSHLL_N"
void exec_vshll_n (void)
{
  /* Basic test: v2=vshll_n(v1,v), then store the result.  */
#define TEST_VSHLL_N(T1, T2, W, W2, N, V)				\
  VECT_VAR(vector_res, T1, W2, N) =					\
    vshll##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
		      V);						\
  vst1q##_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector_res, T1, W2, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Choose shift amount arbitrarily.  */
  TEST_VSHLL_N(int, s, 8, 16, 8, 1);
  TEST_VSHLL_N(int, s, 16, 32, 4, 1);
  TEST_VSHLL_N(int, s, 32, 64, 2, 3);
  TEST_VSHLL_N(uint, u, 8, 16, 8, 2);
  TEST_VSHLL_N(uint, u, 16, 32, 4, 4);
  TEST_VSHLL_N(uint, u, 32, 64, 2, 3);

#undef CMT
#define CMT ""
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, CMT);
}

int main (void)
{
  exec_vshll_n ();
  return 0;
}
