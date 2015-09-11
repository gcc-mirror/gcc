#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#ifdef __ARM_FEATURE_FMA
/* Expected results.  */
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x4438ca3d, 0x44390a3d };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x44869eb8, 0x4486beb8, 0x4486deb8, 0x4486feb8 };
#ifdef __aarch64__
VECT_VAR_DECL(expected,hfloat,64,2) [] = { 0x408906e1532b8520, 0x40890ee1532b8520 };
#endif

#define TEST_MSG "VFMA/VFMAQ"

void exec_vfma (void)
{
  /* Basic test: v4=vfma(v1,v2), then store the result.  */
#define TEST_VFMA(Q, T1, T2, W, N)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vfma##Q##_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N),			\
		      VECT_VAR(vector3, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#ifdef __aarch64__
#define CHECK_VFMA_RESULTS(test_name,comment)				\
  {									\
    CHECK_FP(test_name, float, 32, 2, PRIx32, expected, comment);	\
    CHECK_FP(test_name, float, 32, 4, PRIx32, expected, comment);	\
    CHECK_FP(test_name, float, 64, 2, PRIx64, expected, comment);	\
  }
#define DECL_VFMA_VAR(VAR)			\
  DECL_VARIABLE(VAR, float, 32, 2);		\
  DECL_VARIABLE(VAR, float, 32, 4);		\
  DECL_VARIABLE(VAR, float, 64, 2);
#else
#define CHECK_VFMA_RESULTS(test_name,comment)				\
  {									\
    CHECK_FP(test_name, float, 32, 2, PRIx32, expected, comment);	\
    CHECK_FP(test_name, float, 32, 4, PRIx32, expected, comment);	\
  }
#define DECL_VFMA_VAR(VAR)			\
  DECL_VARIABLE(VAR, float, 32, 2);		\
  DECL_VARIABLE(VAR, float, 32, 4);
#endif

  DECL_VFMA_VAR(vector1);
  DECL_VFMA_VAR(vector2);
  DECL_VFMA_VAR(vector3);
  DECL_VFMA_VAR(vector_res);

  clean_results ();

  /* Initialize input "vector1" from "buffer".  */
  VLOAD(vector1, buffer, , float, f, 32, 2);
  VLOAD(vector1, buffer, q, float, f, 32, 4);
#ifdef __aarch64__
  VLOAD(vector1, buffer, q, float, f, 64, 2);
#endif

  /* Choose init value arbitrarily.  */
  VDUP(vector2, , float, f, 32, 2, 9.3f);
  VDUP(vector2, q, float, f, 32, 4, 29.7f);
#ifdef __aarch64__
  VDUP(vector2, q, float, f, 64, 2, 15.8f);
#endif
  
  /* Choose init value arbitrarily.  */
  VDUP(vector3, , float, f, 32, 2, 81.2f);
  VDUP(vector3, q, float, f, 32, 4, 36.8f);
#ifdef __aarch64__
  VDUP(vector3, q, float, f, 64, 2, 51.7f);
#endif

  /* Execute the tests.  */
  TEST_VFMA(, float, f, 32, 2);
  TEST_VFMA(q, float, f, 32, 4);
#ifdef __aarch64__
  TEST_VFMA(q, float, f, 64, 2);
#endif

  CHECK_VFMA_RESULTS (TEST_MSG, "");
}
#endif

int main (void)
{
#ifdef __ARM_FEATURE_FMA
  exec_vfma ();
#endif
  return 0;
}
