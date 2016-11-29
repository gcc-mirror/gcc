#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffc0, 0xffc4, 0xffc8, 0xffcc };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffde0, 0xfffffe02 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xbbc0, 0xc004, 0xc448, 0xc88c };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffface0, 0xffffb212 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 4) [] = { 0xddb3, 0xdd58, 0xdcfd, 0xdca1 };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc3b66666, 0xc3ab0000 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xffc0, 0xffc4, 0xffc8, 0xffcc,
					0xffd0, 0xffd4, 0xffd8, 0xffdc };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffde0, 0xfffffe02,
					0xfffffe24, 0xfffffe46 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xbbc0, 0xc004, 0xc448, 0xc88c,
					 0xccd0, 0xd114, 0xd558, 0xd99c };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffface0, 0xffffb212,
					 0xffffb744, 0xffffbc76 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 8) [] = { 0xddb3, 0xdd58, 0xdcfd, 0xdca1,
					      0xdc46, 0xdbd6, 0xdb20, 0xda69 };
#endif
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc3b66666, 0xc3ab0000,
					   0xc39f9999, 0xc3943333 };

#define TEST_MSG "VMUL_LANE"
void exec_vmul_lane (void)
{
#define DECL_VMUL(VAR)				\
  DECL_VARIABLE(VAR, int, 16, 4);		\
  DECL_VARIABLE(VAR, int, 32, 2);		\
  DECL_VARIABLE(VAR, uint, 16, 4);		\
  DECL_VARIABLE(VAR, uint, 32, 2);		\
  DECL_VARIABLE(VAR, float, 32, 2);		\
  DECL_VARIABLE(VAR, int, 16, 8);		\
  DECL_VARIABLE(VAR, int, 32, 4);		\
  DECL_VARIABLE(VAR, uint, 16, 8);		\
  DECL_VARIABLE(VAR, uint, 32, 4);		\
  DECL_VARIABLE(VAR, float, 32, 4)

  /* vector_res = vmul_lane(vector,vector2,lane), then store the result.  */
#define TEST_VMUL_LANE(Q, T1, T2, W, N, N2, L)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    vmul##Q##_lane_##T2##W(VECT_VAR(vector, T1, W, N),			\
			   VECT_VAR(vector2, T1, W, N2),		\
			   L);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N))

  DECL_VMUL(vector);
  DECL_VMUL(vector_res);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector, float, 16, 4);
  DECL_VARIABLE(vector, float, 16, 8);
  DECL_VARIABLE(vector_res, float, 16, 4);
  DECL_VARIABLE(vector_res, float, 16, 8);
#endif

  DECL_VARIABLE(vector2, int, 16, 4);
  DECL_VARIABLE(vector2, int, 32, 2);
  DECL_VARIABLE(vector2, uint, 16, 4);
  DECL_VARIABLE(vector2, uint, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector2, float, 16, 4);
#endif
  DECL_VARIABLE(vector2, float, 32, 2);

  clean_results ();

  /* Initialize vector from pre-initialized values.  */
  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, , uint, u, 16, 4);
  VLOAD(vector, buffer, , uint, u, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD(vector, buffer, , float, f, 16, 4);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, q, float, f, 32, 4);

  /* Initialize vector2.  */
  VDUP(vector2, , int, s, 16, 4, 0x4);
  VDUP(vector2, , int, s, 32, 2, 0x22);
  VDUP(vector2, , uint, u, 16, 4, 0x444);
  VDUP(vector2, , uint, u, 32, 2, 0x532);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector2, , float, f, 16, 4, 22.8f);
#endif
  VDUP(vector2, , float, f, 32, 2, 22.8f);

  /* Choose lane arbitrarily.  */
  TEST_VMUL_LANE(, int, s, 16, 4, 4, 2);
  TEST_VMUL_LANE(, int, s, 32, 2, 2, 1);
  TEST_VMUL_LANE(, uint, u, 16, 4, 4, 2);
  TEST_VMUL_LANE(, uint, u, 32, 2, 2, 1);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VMUL_LANE(, float, f, 16, 4, 4, 1);
#endif
  TEST_VMUL_LANE(, float, f, 32, 2, 2, 1);
  TEST_VMUL_LANE(q, int, s, 16, 8, 4, 2);
  TEST_VMUL_LANE(q, int, s, 32, 4, 2, 0);
  TEST_VMUL_LANE(q, uint, u, 16, 8, 4, 2);
  TEST_VMUL_LANE(q, uint, u, 32, 4, 2, 1);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VMUL_LANE(q, float, f, 16, 8, 4, 0);
#endif
  TEST_VMUL_LANE(q, float, f, 32, 4, 2, 0);

  CHECK(TEST_MSG, int, 16, 4, PRIx64, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected, "");
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx64, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected, "");
#endif
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected, "");
}

int main (void)
{
  exec_vmul_lane ();
  return 0;
}
