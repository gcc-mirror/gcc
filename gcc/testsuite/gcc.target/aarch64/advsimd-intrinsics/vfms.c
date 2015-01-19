#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#ifdef __ARM_FEATURE_FMA
/* Expected results.  */
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc440ca3d, 0xc4408a3d };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc48a9eb8, 0xc48a7eb8, 0xc48a5eb8, 0xc48a3eb8 };
#ifdef __aarch64__
VECT_VAR_DECL(expected,hfloat,64,2) [] = { 0xc08a06e1532b8520, 0xc089fee1532b8520 };
#endif

#define TEST_MSG "VFMS/VFMSQ"

void exec_vfms (void)
{
  /* Basic test: v4=vfms(v1,v2), then store the result.  */
#define TEST_VFMS(Q, T1, T2, W, N)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vfms##Q##_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N),			\
		      VECT_VAR(vector3, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#ifdef __aarch64__
#define CHECK_VFMS_RESULTS(test_name,comment)				\
  {									\
    CHECK_FP(test_name, float, 32, 2, PRIx32, expected, comment);	\
    CHECK_FP(test_name, float, 32, 4, PRIx32, expected, comment);	\
    CHECK_FP(test_name, float, 64, 2, PRIx64, expected, comment);	\
  }
#define DECL_VFMS_VAR(VAR)			\
  DECL_VARIABLE(VAR, float, 32, 2);		\
  DECL_VARIABLE(VAR, float, 32, 4);		\
  DECL_VARIABLE(VAR, float, 64, 2);
#else
#define CHECK_VFMS_RESULTS(test_name,comment)				\
  {									\
    CHECK_FP(test_name, float, 32, 2, PRIx32, expected, comment);	\
    CHECK_FP(test_name, float, 32, 4, PRIx32, expected, comment);	\
  }
#define DECL_VFMS_VAR(VAR)			\
  DECL_VARIABLE(VAR, float, 32, 2);		\
  DECL_VARIABLE(VAR, float, 32, 4);
#endif

  DECL_VFMS_VAR(vector1);
  DECL_VFMS_VAR(vector2);
  DECL_VFMS_VAR(vector3);
  DECL_VFMS_VAR(vector_res);

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
  TEST_VFMS(, float, f, 32, 2);
  TEST_VFMS(q, float, f, 32, 4);
#ifdef __aarch64__
  TEST_VFMS(q, float, f, 64, 2);
#endif

  CHECK_VFMS_RESULTS (TEST_MSG, "");
}
#endif

int main (void)
{
#ifdef __ARM_FEATURE_FMA
  exec_vfms ();
#endif
  return 0;
}
