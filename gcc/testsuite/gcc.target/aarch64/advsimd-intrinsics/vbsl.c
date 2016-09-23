#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
				       0xf6, 0xf6, 0xf6, 0xf6 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff0, 0xfff0, 0xfff2, 0xfff2 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffffd };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf3, 0xf3, 0xf3, 0xf3,
					0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff0, 0xfff0, 0xfff2, 0xfff2 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xfffffff1 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xf3, 0xf3, 0xf3, 0xf3,
					0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xfff0, 0xfff0, 0xfff2, 0xfff2 };
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected, hfloat, 16, 4) [] = { 0xcc09, 0xcb89,
					       0xcb09, 0xca89 };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1800004, 0xc1700004 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
					0xf6, 0xf6, 0xf6, 0xf6,
					0xf2, 0xf2, 0xf2, 0xf2,
					0xf6, 0xf6, 0xf6, 0xf6 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff0, 0xfff0, 0xfff2, 0xfff2,
					0xfff4, 0xfff4, 0xfff6, 0xfff6 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff0, 0xfffffff0,
					0xfffffff2, 0xfffffff2 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffffd,
					0xfffffffffffffffd };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xf3, 0xf3, 0xf3, 0xf3,
					 0xf7, 0xf7, 0xf7, 0xf7,
					 0xf3, 0xf3, 0xf3, 0xf3,
					 0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff0, 0xfff0, 0xfff2, 0xfff2,
					 0xfff4, 0xfff4, 0xfff6, 0xfff6 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff0, 0xfffffff0,
					 0xfffffff2, 0xfffffff2 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffff1,
					 0xfffffff1 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xf3, 0xf3, 0xf3, 0xf3,
					 0xf7, 0xf7, 0xf7, 0xf7,
					 0xf3, 0xf3, 0xf3, 0xf3,
					 0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0xfff0, 0xfff0, 0xfff2, 0xfff2,
					 0xfff4, 0xfff4, 0xfff6, 0xfff6 };
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected, hfloat, 16, 8) [] = { 0xcc09, 0xcb89,
					       0xcb09, 0xca89,
					       0xca09, 0xc989,
					       0xc909, 0xc889 };
#endif
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1800001, 0xc1700001,
					   0xc1600001, 0xc1500001 };

#define TEST_MSG "VBSL/VBSLQ"
void exec_vbsl (void)
{
  /* Basic test: y=vbsl(unsigned_vec,x1,x2), then store the result.  */
#define TEST_VBSL(T3, Q, T1, T2, W, N)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vbsl##Q##_##T2##W(VECT_VAR(vector_first, T3, W, N),			\
		      VECT_VAR(vector, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  DECL_VARIABLE_UNSIGNED_VARIANTS(vector_first);

  clean_results ();

  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);
#if defined (FP16_SUPPORTED)
  VLOAD(vector, buffer, , float, f, 16, 4);
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);

  /* Choose init value arbitrarily, will be used for vector
     comparison. As we want different values for each type variant, we
     can't use generic initialization macros.  */
  VDUP(vector2, , int, s, 8, 8, -10);
  VDUP(vector2, , int, s, 16, 4, -14);
  VDUP(vector2, , int, s, 32, 2, -30);
  VDUP(vector2, , int, s, 64, 1, -33);
  VDUP(vector2, , uint, u, 8, 8, 0xF3);
  VDUP(vector2, , uint, u, 16, 4, 0xFFF2);
  VDUP(vector2, , uint, u, 32, 2, 0xFFFFFFF0);
  VDUP(vector2, , uint, u, 64, 1, 0xFFFFFFF3);
#if defined (FP16_SUPPORTED)
  VDUP(vector2, , float, f, 16, 4, -2.4f);   /* -2.4f is 0xC0CD.  */
#endif
  VDUP(vector2, , float, f, 32, 2, -30.3f);
  VDUP(vector2, , poly, p, 8, 8, 0xF3);
  VDUP(vector2, , poly, p, 16, 4, 0xFFF2);

  VDUP(vector2, q, int, s, 8, 16, -10);
  VDUP(vector2, q, int, s, 16, 8, -14);
  VDUP(vector2, q, int, s, 32, 4, -30);
  VDUP(vector2, q, int, s, 64, 2, -33);
  VDUP(vector2, q, uint, u, 8, 16, 0xF3);
  VDUP(vector2, q, uint, u, 16, 8, 0xFFF2);
  VDUP(vector2, q, uint, u, 32, 4, 0xFFFFFFF0);
  VDUP(vector2, q, uint, u, 64, 2, 0xFFFFFFF3);
  VDUP(vector2, q, poly, p, 8, 16, 0xF3);
  VDUP(vector2, q, poly, p, 16, 8, 0xFFF2);
#if defined (FP16_SUPPORTED)
  VDUP(vector2, q, float, f, 16, 8, -2.4f);
#endif
  VDUP(vector2, q, float, f, 32, 4, -30.4f);

  VDUP(vector_first, , uint, u, 8, 8, 0xF4);
  VDUP(vector_first, , uint, u, 16, 4, 0xFFF6);
  VDUP(vector_first, , uint, u, 32, 2, 0xFFFFFFF2);
  VDUP(vector_first, , uint, u, 64, 1, 0xFFFFFFF2);
  VDUP(vector_first, q, uint, u, 8, 16, 0xF4);
  VDUP(vector_first, q, uint, u, 16, 8, 0xFFF6);
  VDUP(vector_first, q, uint, u, 32, 4, 0xFFFFFFF2);
  VDUP(vector_first, q, uint, u, 64, 2, 0xFFFFFFF2);

  /* Execute the tests.  */
  TEST_MACRO_ALL_VARIANTS_1_5(TEST_VBSL, uint);
  TEST_VBSL(uint, , poly, p, 8, 8);
  TEST_VBSL(uint, , poly, p, 16, 4);
  TEST_VBSL(uint, q, poly, p, 8, 16);
  TEST_VBSL(uint, q, poly, p, 16, 8);
#if defined (FP16_SUPPORTED)
  TEST_VBSL(uint, , float, f, 16, 4);
  TEST_VBSL(uint, q, float, f, 16, 8);
#endif
  TEST_VBSL(uint, , float, f, 32, 2);
  TEST_VBSL(uint, q, float, f, 32, 4);

#if defined (FP16_SUPPORTED)
  CHECK_RESULTS (TEST_MSG, "");
#else
  CHECK_RESULTS_NO_FP16 (TEST_MSG, "");
#endif
}

int main (void)
{
  exec_vbsl ();
  return 0;
}
