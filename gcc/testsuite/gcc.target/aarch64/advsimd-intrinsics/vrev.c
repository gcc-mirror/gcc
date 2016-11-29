#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results for vrev16.  */
VECT_VAR_DECL(expected_vrev16,int,8,8) [] = { 0xf1, 0xf0, 0xf3, 0xf2,
					      0xf5, 0xf4, 0xf7, 0xf6 };
VECT_VAR_DECL(expected_vrev16,uint,8,8) [] = { 0xf1, 0xf0, 0xf3, 0xf2,
					       0xf5, 0xf4, 0xf7, 0xf6 };
VECT_VAR_DECL(expected_vrev16,poly,8,8) [] = { 0xf1, 0xf0, 0xf3, 0xf2,
					       0xf5, 0xf4, 0xf7, 0xf6 };
VECT_VAR_DECL(expected_vrev16,int,8,16) [] = { 0xf1, 0xf0, 0xf3, 0xf2,
					       0xf5, 0xf4, 0xf7, 0xf6,
					       0xf9, 0xf8, 0xfb, 0xfa,
					       0xfd, 0xfc, 0xff, 0xfe };
VECT_VAR_DECL(expected_vrev16,uint,8,16) [] = { 0xf1, 0xf0, 0xf3, 0xf2,
						0xf5, 0xf4, 0xf7, 0xf6,
						0xf9, 0xf8, 0xfb, 0xfa,
						0xfd, 0xfc, 0xff, 0xfe };
VECT_VAR_DECL(expected_vrev16,poly,8,16) [] = { 0xf1, 0xf0, 0xf3, 0xf2,
						0xf5, 0xf4, 0xf7, 0xf6,
						0xf9, 0xf8, 0xfb, 0xfa,
						0xfd, 0xfc, 0xff, 0xfe };

/* Expected results for vrev32.  */
VECT_VAR_DECL(expected_vrev32,int,8,8) [] = { 0xf3, 0xf2, 0xf1, 0xf0,
					      0xf7, 0xf6, 0xf5, 0xf4 };
VECT_VAR_DECL(expected_vrev32,int,16,4) [] = { 0xfff1, 0xfff0, 0xfff3, 0xfff2 };
VECT_VAR_DECL(expected_vrev32,uint,8,8) [] = { 0xf3, 0xf2, 0xf1, 0xf0,
					       0xf7, 0xf6, 0xf5, 0xf4 };
VECT_VAR_DECL(expected_vrev32,uint,16,4) [] = { 0xfff1, 0xfff0, 0xfff3, 0xfff2 };
VECT_VAR_DECL(expected_vrev32,poly,8,8) [] = { 0xf3, 0xf2, 0xf1, 0xf0,
					       0xf7, 0xf6, 0xf5, 0xf4 };
VECT_VAR_DECL(expected_vrev32,poly,16,4) [] = { 0xfff1, 0xfff0, 0xfff3, 0xfff2 };
VECT_VAR_DECL(expected_vrev32,int,8,16) [] = { 0xf3, 0xf2, 0xf1, 0xf0,
					       0xf7, 0xf6, 0xf5, 0xf4,
					       0xfb, 0xfa, 0xf9, 0xf8,
					       0xff, 0xfe, 0xfd, 0xfc };
VECT_VAR_DECL(expected_vrev32,int,16,8) [] = { 0xfff1, 0xfff0, 0xfff3, 0xfff2,
					       0xfff5, 0xfff4, 0xfff7, 0xfff6 };
VECT_VAR_DECL(expected_vrev32,uint,8,16) [] = { 0xf3, 0xf2, 0xf1, 0xf0,
						0xf7, 0xf6, 0xf5, 0xf4,
						0xfb, 0xfa, 0xf9, 0xf8,
						0xff, 0xfe, 0xfd, 0xfc };
VECT_VAR_DECL(expected_vrev32,uint,16,8) [] = { 0xfff1, 0xfff0, 0xfff3, 0xfff2,
						0xfff5, 0xfff4, 0xfff7, 0xfff6 };
VECT_VAR_DECL(expected_vrev32,poly,8,16) [] = { 0xf3, 0xf2, 0xf1, 0xf0,
						0xf7, 0xf6, 0xf5, 0xf4,
						0xfb, 0xfa, 0xf9, 0xf8,
						0xff, 0xfe, 0xfd, 0xfc };
VECT_VAR_DECL(expected_vrev32,poly,16,8) [] = { 0xfff1, 0xfff0, 0xfff3, 0xfff2,
						0xfff5, 0xfff4, 0xfff7, 0xfff6 };

/* Expected results for vrev64.  */
VECT_VAR_DECL(expected_vrev64,int,8,8) [] = { 0xf7, 0xf6, 0xf5, 0xf4,
					      0xf3, 0xf2, 0xf1, 0xf0 };
VECT_VAR_DECL(expected_vrev64,int,16,4) [] = { 0xfff3, 0xfff2, 0xfff1, 0xfff0 };
VECT_VAR_DECL(expected_vrev64,int,32,2) [] = { 0xfffffff1, 0xfffffff0 };
VECT_VAR_DECL(expected_vrev64,uint,8,8) [] = { 0xf7, 0xf6, 0xf5, 0xf4, 0xf3,
					       0xf2, 0xf1, 0xf0 };
VECT_VAR_DECL(expected_vrev64,uint,16,4) [] = { 0xfff3, 0xfff2, 0xfff1, 0xfff0 };
VECT_VAR_DECL(expected_vrev64,uint,32,2) [] = { 0xfffffff1, 0xfffffff0 };
VECT_VAR_DECL(expected_vrev64,poly,8,8) [] = { 0xf7, 0xf6, 0xf5, 0xf4,
					       0xf3, 0xf2, 0xf1, 0xf0 };
VECT_VAR_DECL(expected_vrev64,poly,16,4) [] = { 0xfff3, 0xfff2, 0xfff1, 0xfff0 };
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected_vrev64, hfloat, 16, 4) [] = { 0xca80, 0xcb00,
						      0xcb80, 0xcc00 };
#endif
VECT_VAR_DECL(expected_vrev64,hfloat,32,2) [] = { 0xc1700000, 0xc1800000 };
VECT_VAR_DECL(expected_vrev64,int,8,16) [] = { 0xf7, 0xf6, 0xf5, 0xf4,
					       0xf3, 0xf2, 0xf1, 0xf0,
					       0xff, 0xfe, 0xfd, 0xfc,
					       0xfb, 0xfa, 0xf9, 0xf8 };
VECT_VAR_DECL(expected_vrev64,int,16,8) [] = { 0xfff3, 0xfff2, 0xfff1, 0xfff0,
					       0xfff7, 0xfff6, 0xfff5, 0xfff4 };
VECT_VAR_DECL(expected_vrev64,int,32,4) [] = { 0xfffffff1, 0xfffffff0,
					       0xfffffff3, 0xfffffff2 };
VECT_VAR_DECL(expected_vrev64,uint,8,16) [] = { 0xf7, 0xf6, 0xf5, 0xf4,
						0xf3, 0xf2, 0xf1, 0xf0,
						0xff, 0xfe, 0xfd, 0xfc,
						0xfb, 0xfa, 0xf9, 0xf8 };
VECT_VAR_DECL(expected_vrev64,uint,16,8) [] = { 0xfff3, 0xfff2, 0xfff1, 0xfff0,
						0xfff7, 0xfff6, 0xfff5, 0xfff4 };
VECT_VAR_DECL(expected_vrev64,uint,32,4) [] = { 0xfffffff1, 0xfffffff0,
						0xfffffff3, 0xfffffff2 };
VECT_VAR_DECL(expected_vrev64,poly,8,16) [] = { 0xf7, 0xf6, 0xf5, 0xf4,
						0xf3, 0xf2, 0xf1, 0xf0,
						0xff, 0xfe, 0xfd, 0xfc,
						0xfb, 0xfa, 0xf9, 0xf8 };
VECT_VAR_DECL(expected_vrev64,poly,16,8) [] = { 0xfff3, 0xfff2, 0xfff1, 0xfff0,
						0xfff7, 0xfff6, 0xfff5, 0xfff4 };
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected_vrev64, hfloat, 16, 8) [] = { 0xca80, 0xcb00,
						      0xcb80, 0xcc00,
						      0xc880, 0xc900,
						      0xc980, 0xca00 };
#endif
VECT_VAR_DECL(expected_vrev64,hfloat,32,4) [] = { 0xc1700000, 0xc1800000,
						  0xc1500000, 0xc1600000 };

void exec_vrev (void)
{
  /* Basic test: y=vrev(x), then store the result.  */
#define TEST_VREV(Q, T1, T2, W, N, W2)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vrev##W2##Q##_##T2##W(VECT_VAR(vector, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);
#if defined (FP16_SUPPORTED)
  VLOAD (vector, buffer, , float, f, 16, 4);
  VLOAD (vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);

  /* Check vrev in each of the existing combinations.  */
#define TEST_MSG "VREV16"
  TEST_VREV(, int, s, 8, 8, 16);
  TEST_VREV(, uint, u, 8, 8, 16);
  TEST_VREV(, poly, p, 8, 8, 16);
  TEST_VREV(q, int, s, 8, 16, 16);
  TEST_VREV(q, uint, u, 8, 16, 16);
  TEST_VREV(q, poly, p, 8, 16, 16);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vrev16, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vrev16, "");
  CHECK(TEST_MSG, poly, 8, 8, PRIx8, expected_vrev16, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vrev16, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vrev16, "");
  CHECK(TEST_MSG, poly, 8, 16, PRIx8, expected_vrev16, "");

#undef TEST_MSG
#define TEST_MSG "VREV32"
  TEST_VREV(, int, s, 8, 8, 32);
  TEST_VREV(, int, s, 16, 4, 32);
  TEST_VREV(, uint, u, 8, 8, 32);
  TEST_VREV(, uint, u, 16, 4, 32);
  TEST_VREV(, poly, p, 8, 8, 32);
  TEST_VREV(, poly, p, 16, 4, 32);
  TEST_VREV(q, int, s, 8, 16, 32);
  TEST_VREV(q, int, s, 16, 8, 32);
  TEST_VREV(q, uint, u, 8, 16, 32);
  TEST_VREV(q, uint, u, 16, 8, 32);
  TEST_VREV(q, poly, p, 8, 16, 32);
  TEST_VREV(q, poly, p, 16, 8, 32);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vrev32, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_vrev32, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vrev32, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_vrev32, "");
  CHECK(TEST_MSG, poly, 8, 8, PRIx8, expected_vrev32, "");
  CHECK(TEST_MSG, poly, 16, 4, PRIx16, expected_vrev32, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vrev32, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_vrev32, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vrev32, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_vrev32, "");
  CHECK(TEST_MSG, poly, 8, 16, PRIx8, expected_vrev32, "");
  CHECK(TEST_MSG, poly, 16, 8, PRIx16, expected_vrev32, "");

#undef TEST_MSG
#define TEST_MSG "VREV64"
  TEST_VREV(, int, s, 8, 8, 64);
  TEST_VREV(, int, s, 16, 4, 64);
  TEST_VREV(, int, s, 32, 2, 64);
  TEST_VREV(, uint, u, 8, 8, 64);
  TEST_VREV(, uint, u, 16, 4, 64);
  TEST_VREV(, uint, u, 32, 2, 64);
  TEST_VREV(, poly, p, 8, 8, 64);
  TEST_VREV(, poly, p, 16, 4, 64);
  TEST_VREV(q, int, s, 8, 16, 64);
  TEST_VREV(q, int, s, 16, 8, 64);
  TEST_VREV(q, int, s, 32, 4, 64);
  TEST_VREV(q, uint, u, 8, 16, 64);
  TEST_VREV(q, uint, u, 16, 8, 64);
  TEST_VREV(q, uint, u, 32, 4, 64);
  TEST_VREV(q, poly, p, 8, 16, 64);
  TEST_VREV(q, poly, p, 16, 8, 64);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vrev64, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_vrev64, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_vrev64, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vrev64, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_vrev64, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_vrev64, "");
  CHECK(TEST_MSG, poly, 8, 8, PRIx8, expected_vrev64, "");
  CHECK(TEST_MSG, poly, 16, 4, PRIx16, expected_vrev64, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vrev64, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_vrev64, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_vrev64, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vrev64, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_vrev64, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_vrev64, "");
  CHECK(TEST_MSG, poly, 8, 16, PRIx8, expected_vrev64, "");
  CHECK(TEST_MSG, poly, 16, 8, PRIx16, expected_vrev64, "");

#if defined (FP16_SUPPORTED)
  TEST_VREV (, float, f, 16, 4, 64);
  TEST_VREV (q, float, f, 16, 8, 64);
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx32, expected_vrev64, "");
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx32, expected_vrev64, "");
#endif
  TEST_VREV(, float, f, 32, 2, 64);
  TEST_VREV(q, float, f, 32, 4, 64);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_vrev64, "");
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_vrev64, "");
}

int main (void)
{
  exec_vrev ();
  return 0;
}
