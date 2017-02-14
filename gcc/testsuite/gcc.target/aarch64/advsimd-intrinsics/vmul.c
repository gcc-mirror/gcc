#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf0, 0x1, 0x12, 0x23,
				       0x34, 0x45, 0x56, 0x67 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfde0, 0xfe02, 0xfe24, 0xfe46 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffcd0, 0xfffffd03 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xc0, 0x4, 0x48, 0x8c,
					0xd0, 0x14, 0x58, 0x9c };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfab0, 0xfb05, 0xfb5a, 0xfbaf };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffff9a0, 0xfffffa06 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xc0, 0x84, 0x48, 0xc,
					0xd0, 0x94, 0x58, 0x1c };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 4) [] = { 0xe02a, 0xdfcf,
					      0xdf4a, 0xdec4 };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc4053333, 0xc3f9c000 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x90, 0x7, 0x7e, 0xf5,
					0x6c, 0xe3, 0x5a, 0xd1,
					0x48, 0xbf, 0x36, 0xad,
					0x24, 0x9b, 0x12, 0x89 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xf780, 0xf808, 0xf890, 0xf918,
					0xf9a0, 0xfa28, 0xfab0, 0xfb38 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffff670, 0xfffff709,
					0xfffff7a2, 0xfffff83b };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x60, 0xa, 0xb4, 0x5e,
					 0x8, 0xb2, 0x5c, 0x6,
					 0xb0, 0x5a, 0x4, 0xae,
					 0x58, 0x2, 0xac, 0x56 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xf450, 0xf50b, 0xf5c6, 0xf681,
					 0xf73c, 0xf7f7, 0xf8b2, 0xf96d };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffff340, 0xfffff40c,
					 0xfffff4d8, 0xfffff5a4 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0x60, 0xca, 0x34, 0x9e,
					 0xc8, 0x62, 0x9c, 0x36,
					 0x30, 0x9a, 0x64, 0xce,
					 0x98, 0x32, 0xcc, 0x66 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 8) [] = { 0xe63a, 0xe5d6, 0xe573, 0xe50f,
					      0xe4ac, 0xe448, 0xe3c8, 0xe301 };
#endif
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc4c73333, 0xc4bac000,
					   0xc4ae4ccd, 0xc4a1d999 };

#define INSN_NAME vmul
#define TEST_MSG "VMUL"

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
#define DECL_VMUL(T, W, N)			\
  DECL_VARIABLE(vector1, T, W, N);		\
  DECL_VARIABLE(vector2, T, W, N);		\
  DECL_VARIABLE(vector_res, T, W, N)

  /* vector_res = OP(vector1, vector2), then store the result.  */
#define TEST_VMUL1(INSN, Q, T1, T2, W, N)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N))

#define TEST_VMUL(INSN, Q, T1, T2, W, N)	\
  TEST_VMUL1(INSN, Q, T1, T2, W, N)

  DECL_VMUL(int, 8, 8);
  DECL_VMUL(int, 16, 4);
  DECL_VMUL(int, 32, 2);
  DECL_VMUL(uint, 8, 8);
  DECL_VMUL(uint, 16, 4);
  DECL_VMUL(uint, 32, 2);
  DECL_VMUL(poly, 8, 8);
  DECL_VMUL(float, 32, 2);
  DECL_VMUL(int, 8, 16);
  DECL_VMUL(int, 16, 8);
  DECL_VMUL(int, 32, 4);
  DECL_VMUL(uint, 8, 16);
  DECL_VMUL(uint, 16, 8);
  DECL_VMUL(uint, 32, 4);
  DECL_VMUL(poly, 8, 16);
  DECL_VMUL(float, 32, 4);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector1, float, 16, 4);
  DECL_VARIABLE(vector1, float, 16, 8);

  DECL_VARIABLE(vector2, float, 16, 4);
  DECL_VARIABLE(vector2, float, 16, 8);

  DECL_VARIABLE(vector_res, float, 16, 4);
  DECL_VARIABLE(vector_res, float, 16, 8);
#endif

  clean_results ();

  /* Initialize input "vector1" from "buffer".  */
  VLOAD(vector1, buffer, , int, s, 8, 8);
  VLOAD(vector1, buffer, , int, s, 16, 4);
  VLOAD(vector1, buffer, , int, s, 32, 2);
  VLOAD(vector1, buffer, , uint, u, 8, 8);
  VLOAD(vector1, buffer, , uint, u, 16, 4);
  VLOAD(vector1, buffer, , uint, u, 32, 2);
  VLOAD(vector1, buffer, , poly, p, 8, 8);
  VLOAD(vector1, buffer, , float, f, 32, 2);
  VLOAD(vector1, buffer, q, int, s, 8, 16);
  VLOAD(vector1, buffer, q, int, s, 16, 8);
  VLOAD(vector1, buffer, q, int, s, 32, 4);
  VLOAD(vector1, buffer, q, uint, u, 8, 16);
  VLOAD(vector1, buffer, q, uint, u, 16, 8);
  VLOAD(vector1, buffer, q, uint, u, 32, 4);
  VLOAD(vector1, buffer, q, poly, p, 8, 16);
  VLOAD(vector1, buffer, q, float, f, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD(vector1, buffer, , float, f, 16, 4);
  VLOAD(vector1, buffer, q, float, f, 16, 8);
#endif

  /* Choose init value arbitrarily.  */
  VDUP(vector2, , int, s, 8, 8, 0x11);
  VDUP(vector2, , int, s, 16, 4, 0x22);
  VDUP(vector2, , int, s, 32, 2, 0x33);
  VDUP(vector2, , uint, u, 8, 8, 0x44);
  VDUP(vector2, , uint, u, 16, 4, 0x55);
  VDUP(vector2, , uint, u, 32, 2, 0x66);
  VDUP(vector2, , poly, p, 8, 8, 0x44);
  VDUP(vector2, , float, f, 32, 2, 33.3f);
  VDUP(vector2, q, int, s, 8, 16, 0x77);
  VDUP(vector2, q, int, s, 16, 8, 0x88);
  VDUP(vector2, q, int, s, 32, 4, 0x99);
  VDUP(vector2, q, uint, u, 8, 16, 0xAA);
  VDUP(vector2, q, uint, u, 16, 8, 0xBB);
  VDUP(vector2, q, uint, u, 32, 4, 0xCC);
  VDUP(vector2, q, poly, p, 8, 16, 0xAA);
  VDUP(vector2, q, float, f, 32, 4, 99.6f);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector2, , float, f, 16, 4, 33.3f);
  VDUP(vector2, q, float, f, 16, 8, 99.6f);
#endif

  /* Execute the tests.  */
  TEST_VMUL(INSN_NAME, , int, s, 8, 8);
  TEST_VMUL(INSN_NAME, , int, s, 16, 4);
  TEST_VMUL(INSN_NAME, , int, s, 32, 2);
  TEST_VMUL(INSN_NAME, , uint, u, 8, 8);
  TEST_VMUL(INSN_NAME, , uint, u, 16, 4);
  TEST_VMUL(INSN_NAME, , uint, u, 32, 2);
  TEST_VMUL(INSN_NAME, , poly, p, 8, 8);
  TEST_VMUL(INSN_NAME, , float, f, 32, 2);
  TEST_VMUL(INSN_NAME, q, int, s, 8, 16);
  TEST_VMUL(INSN_NAME, q, int, s, 16, 8);
  TEST_VMUL(INSN_NAME, q, int, s, 32, 4);
  TEST_VMUL(INSN_NAME, q, uint, u, 8, 16);
  TEST_VMUL(INSN_NAME, q, uint, u, 16, 8);
  TEST_VMUL(INSN_NAME, q, uint, u, 32, 4);
  TEST_VMUL(INSN_NAME, q, poly, p, 8, 16);
  TEST_VMUL(INSN_NAME, q, float, f, 32, 4);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VMUL(INSN_NAME, , float, f, 16, 4);
  TEST_VMUL(INSN_NAME, q, float, f, 16, 8);
#endif

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected, "");
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected, "");
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected, "");
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected, "");
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected, "");
#endif
}

int main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}
