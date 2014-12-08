#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x4438ca3d, 0x44390a3d };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x44869eb8, 0x4486beb8, 0x4486deb8, 0x4486feb8 };
VECT_VAR_DECL(expected,hfloat,64,2) [] = { 0x408906e1532b8520, 0x40890ee1532b8520 };

#define VECT_VAR_ASSIGN(S,Q,T1,W) S##Q##_##T1##W
#define ASSIGN(S, Q, T, W, V) T##W##_t S##Q##_##T##W = V
#define TEST_MSG "VFMA/VFMAQ"
void exec_vfma_n (void)
{
  /* Basic test: v4=vfma_n(v1,v2), then store the result.  */
#define TEST_VFMA(Q, T1, T2, W, N)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vfma##Q##_n_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N),			\
			  VECT_VAR_ASSIGN(Scalar, Q, T1, W));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define CHECK_VFMA_RESULTS(test_name,comment)				\
  {									\
    CHECK_FP(test_name, float, 32, 2, PRIx32, expected, comment);	\
    CHECK_FP(test_name, float, 32, 4, PRIx32, expected, comment);	\
	CHECK_FP(test_name, float, 64, 2, PRIx64, expected, comment);	\
  }	

#define DECL_VABD_VAR(VAR)			\
  DECL_VARIABLE(VAR, float, 32, 2);		\
  DECL_VARIABLE(VAR, float, 32, 4);		\
  DECL_VARIABLE(VAR, float, 64, 2);		

  DECL_VABD_VAR(vector1);
  DECL_VABD_VAR(vector2);
  DECL_VABD_VAR(vector3);
  DECL_VABD_VAR(vector_res);

  clean_results ();

  /* Initialize input "vector1" from "buffer".  */
  VLOAD(vector1, buffer, , float, f, 32, 2);
  VLOAD(vector1, buffer, q, float, f, 32, 4);
  VLOAD(vector1, buffer, q, float, f, 64, 2);

  /* Choose init value arbitrarily.  */
  VDUP(vector2, , float, f, 32, 2, 9.3f);
  VDUP(vector2, q, float, f, 32, 4, 29.7f);
  VDUP(vector2, q, float, f, 64, 2, 15.8f);
  
  /* Choose init value arbitrarily.  */
  ASSIGN(Scalar, , float, 32, 81.2f);
  ASSIGN(Scalar, q, float, 32, 36.8f);
  ASSIGN(Scalar, q, float, 64, 51.7f);

  /* Execute the tests.  */
  TEST_VFMA(, float, f, 32, 2);
  TEST_VFMA(q, float, f, 32, 4);
  TEST_VFMA(q, float, f, 64, 2);

  CHECK_VFMA_RESULTS (TEST_MSG, "");
}

int main (void)
{
  exec_vfma_n ();
  return 0;
}
