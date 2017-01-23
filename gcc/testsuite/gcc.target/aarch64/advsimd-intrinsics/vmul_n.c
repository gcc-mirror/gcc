#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfef0, 0xff01, 0xff12, 0xff23 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffde0, 0xfffffe02 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfcd0, 0xfd03, 0xfd36, 0xfd69 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffbc0, 0xfffffc04 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 4) [] = { 0xdd93, 0xdd3a, 0xdce1, 0xdc87 };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc3b26666, 0xc3a74000 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfab0, 0xfb05, 0xfb5a, 0xfbaf,
					0xfc04, 0xfc59, 0xfcae, 0xfd03 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffff9a0, 0xfffffa06,
					0xfffffa6c, 0xfffffad2 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xf890, 0xf907, 0xf97e, 0xf9f5,
					 0xfa6c, 0xfae3, 0xfb5a, 0xfbd1 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffff780, 0xfffff808,
					 0xfffff890, 0xfffff918 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 8) [] = { 0xe58e, 0xe535, 0xe4dc, 0xe483,
					      0xe42a, 0xe3a3, 0xe2f2, 0xe240 };
#endif
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc4b1cccd, 0xc4a6b000,
					   0xc49b9333, 0xc4907667 };

#define INSN_NAME vmul_n
#define TEST_MSG "VMUL_N"

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
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

  /* vector_res = vmul_n(vector,val), then store the result.  */
#define TEST_VMUL_N(Q, T1, T2, W, N, L)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vmul##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
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

  /* Choose multiplier arbitrarily.  */
  TEST_VMUL_N(, int, s, 16, 4, 0x11);
  TEST_VMUL_N(, int, s, 32, 2, 0x22);
  TEST_VMUL_N(, uint, u, 16, 4, 0x33);
  TEST_VMUL_N(, uint, u, 32, 2, 0x44);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VMUL_N(, float, f, 16, 4, 22.3f);
#endif
  TEST_VMUL_N(, float, f, 32, 2, 22.3f);
  TEST_VMUL_N(q, int, s, 16, 8, 0x55);
  TEST_VMUL_N(q, int, s, 32, 4, 0x66);
  TEST_VMUL_N(q, uint, u, 16, 8, 0x77);
  TEST_VMUL_N(q, uint, u, 32, 4, 0x88);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VMUL_N(q, float, f, 16, 8, 88.9f);
#endif
  TEST_VMUL_N(q, float, f, 32, 4, 88.9f);

  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected, "");
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected, "");
#endif
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected, "");
}

int main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}
