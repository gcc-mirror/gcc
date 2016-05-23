/* This file contains tests for the vreinterpret *p64 intrinsics.  */

/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results: vreinterpret_p64_*.  */
VECT_VAR_DECL(vreint_expected_p64_s8,poly,64,1) [] = { 0xf7f6f5f4f3f2f1f0 };
VECT_VAR_DECL(vreint_expected_p64_s16,poly,64,1) [] = { 0xfff3fff2fff1fff0 };
VECT_VAR_DECL(vreint_expected_p64_s32,poly,64,1) [] = { 0xfffffff1fffffff0 };
VECT_VAR_DECL(vreint_expected_p64_s64,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vreint_expected_p64_u8,poly,64,1) [] = { 0xf7f6f5f4f3f2f1f0 };
VECT_VAR_DECL(vreint_expected_p64_u16,poly,64,1) [] = { 0xfff3fff2fff1fff0 };
VECT_VAR_DECL(vreint_expected_p64_u32,poly,64,1) [] = { 0xfffffff1fffffff0 };
VECT_VAR_DECL(vreint_expected_p64_u64,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vreint_expected_p64_p8,poly,64,1) [] = { 0xf7f6f5f4f3f2f1f0 };
VECT_VAR_DECL(vreint_expected_p64_p16,poly,64,1) [] = { 0xfff3fff2fff1fff0 };
VECT_VAR_DECL(vreint_expected_p64_f32,poly,64,1) [] = { 0xc1700000c1800000 };

/* Expected results: vreinterpretq_p64_*.  */
VECT_VAR_DECL(vreint_expected_q_p64_s8,poly,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
							 0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(vreint_expected_q_p64_s16,poly,64,2) [] = { 0xfff3fff2fff1fff0,
							  0xfff7fff6fff5fff4 };
VECT_VAR_DECL(vreint_expected_q_p64_s32,poly,64,2) [] = { 0xfffffff1fffffff0,
							  0xfffffff3fffffff2 };
VECT_VAR_DECL(vreint_expected_q_p64_s64,poly,64,2) [] = { 0xfffffffffffffff0,
							  0xfffffffffffffff1 };
VECT_VAR_DECL(vreint_expected_q_p64_u8,poly,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
							 0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(vreint_expected_q_p64_u16,poly,64,2) [] = { 0xfff3fff2fff1fff0,
							  0xfff7fff6fff5fff4 };
VECT_VAR_DECL(vreint_expected_q_p64_u32,poly,64,2) [] = { 0xfffffff1fffffff0,
							  0xfffffff3fffffff2 };
VECT_VAR_DECL(vreint_expected_q_p64_u64,poly,64,2) [] = { 0xfffffffffffffff0,
							  0xfffffffffffffff1 };
VECT_VAR_DECL(vreint_expected_q_p64_p8,poly,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
							 0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(vreint_expected_q_p64_p16,poly,64,2) [] = { 0xfff3fff2fff1fff0,
							  0xfff7fff6fff5fff4 };
VECT_VAR_DECL(vreint_expected_q_p64_f32,poly,64,2) [] = { 0xc1700000c1800000,
							  0xc1500000c1600000 };

/* Expected results: vreinterpret_*_p64.  */
VECT_VAR_DECL(vreint_expected_s8_p64,int,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
						     0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(vreint_expected_s16_p64,int,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(vreint_expected_s32_p64,int,32,2) [] = { 0xfffffff0, 0xffffffff };
VECT_VAR_DECL(vreint_expected_s64_p64,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vreint_expected_u8_p64,uint,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
						      0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(vreint_expected_u16_p64,uint,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(vreint_expected_u32_p64,uint,32,2) [] = { 0xfffffff0, 0xffffffff };
VECT_VAR_DECL(vreint_expected_u64_p64,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vreint_expected_p8_p64,poly,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
						      0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(vreint_expected_p16_p64,poly,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(vreint_expected_f32_p64,hfloat,32,2) [] = { 0xfffffff0, 0xffffffff };

/* Expected results: vreinterpretq_*_p64.  */
VECT_VAR_DECL(vreint_expected_q_s8_p64,int,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
							0xff, 0xff, 0xff, 0xff,
							0xf1, 0xff, 0xff, 0xff,
							0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(vreint_expected_q_s16_p64,int,16,8) [] = { 0xfff0, 0xffff,
							 0xffff, 0xffff,
							 0xfff1, 0xffff,
							 0xffff, 0xffff };
VECT_VAR_DECL(vreint_expected_q_s32_p64,int,32,4) [] = { 0xfffffff0, 0xffffffff,
							 0xfffffff1, 0xffffffff };
VECT_VAR_DECL(vreint_expected_q_s64_p64,int,64,2) [] = { 0xfffffffffffffff0,
							 0xfffffffffffffff1 };
VECT_VAR_DECL(vreint_expected_q_u8_p64,uint,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
							 0xff, 0xff, 0xff, 0xff,
							 0xf1, 0xff, 0xff, 0xff,
							 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(vreint_expected_q_u16_p64,uint,16,8) [] = { 0xfff0, 0xffff,
							  0xffff, 0xffff,
							  0xfff1, 0xffff,
							  0xffff, 0xffff };
VECT_VAR_DECL(vreint_expected_q_u32_p64,uint,32,4) [] = { 0xfffffff0, 0xffffffff,
							  0xfffffff1, 0xffffffff };
VECT_VAR_DECL(vreint_expected_q_u64_p64,uint,64,2) [] = { 0xfffffffffffffff0,
							  0xfffffffffffffff1 };
VECT_VAR_DECL(vreint_expected_q_p8_p64,poly,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
							 0xff, 0xff, 0xff, 0xff,
							 0xf1, 0xff, 0xff, 0xff,
							 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(vreint_expected_q_p16_p64,poly,16,8) [] = { 0xfff0, 0xffff,
							  0xffff, 0xffff,
							  0xfff1, 0xffff,
							  0xffff, 0xffff };
VECT_VAR_DECL(vreint_expected_q_f32_p64,hfloat,32,4) [] = { 0xfffffff0, 0xffffffff,
							    0xfffffff1, 0xffffffff };

int main (void)
{
#define TEST_VREINTERPRET(Q, T1, T2, W, N, TS1, TS2, WS, NS, EXPECTED)	\
  VECT_VAR(vreint_vector_res, T1, W, N) =				\
    vreinterpret##Q##_##T2##W##_##TS2##WS(VECT_VAR(vreint_vector, TS1, WS, NS)); \
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vreint_vector_res, T1, W, N));		\
  CHECK(TEST_MSG, T1, W, N, PRIx##W, EXPECTED, "");

#define TEST_VREINTERPRET_FP(Q, T1, T2, W, N, TS1, TS2, WS, NS, EXPECTED) \
  VECT_VAR(vreint_vector_res, T1, W, N) =				\
    vreinterpret##Q##_##T2##W##_##TS2##WS(VECT_VAR(vreint_vector, TS1, WS, NS)); \
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vreint_vector_res, T1, W, N));		\
  CHECK_FP(TEST_MSG, T1, W, N, PRIx##W, EXPECTED, "");

  DECL_VARIABLE_ALL_VARIANTS(vreint_vector);
  DECL_VARIABLE(vreint_vector, poly, 64, 1);
  DECL_VARIABLE(vreint_vector, poly, 64, 2);
  DECL_VARIABLE_ALL_VARIANTS(vreint_vector_res);
  DECL_VARIABLE(vreint_vector_res, poly, 64, 1);
  DECL_VARIABLE(vreint_vector_res, poly, 64, 2);

  clean_results ();

  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vreint_vector, buffer);
  VLOAD(vreint_vector, buffer, , poly, p, 64, 1);
  VLOAD(vreint_vector, buffer, q, poly, p, 64, 2);
  VLOAD(vreint_vector, buffer, , float, f, 32, 2);
  VLOAD(vreint_vector, buffer, q, float, f, 32, 4);

  /* vreinterpret_p64_* tests.  */
#undef TEST_MSG
#define TEST_MSG "VREINTERPRET_P64_*"
  TEST_VREINTERPRET(, poly, p, 64, 1, int, s, 8, 8, vreint_expected_p64_s8);
  TEST_VREINTERPRET(, poly, p, 64, 1, int, s, 16, 4, vreint_expected_p64_s16);
  TEST_VREINTERPRET(, poly, p, 64, 1, int, s, 32, 2, vreint_expected_p64_s32);
  TEST_VREINTERPRET(, poly, p, 64, 1, int, s, 64, 1, vreint_expected_p64_s64);
  TEST_VREINTERPRET(, poly, p, 64, 1, uint, u, 8, 8, vreint_expected_p64_u8);
  TEST_VREINTERPRET(, poly, p, 64, 1, uint, u, 16, 4, vreint_expected_p64_u16);
  TEST_VREINTERPRET(, poly, p, 64, 1, uint, u, 32, 2, vreint_expected_p64_u32);
  TEST_VREINTERPRET(, poly, p, 64, 1, uint, u, 64, 1, vreint_expected_p64_u64);
  TEST_VREINTERPRET(, poly, p, 64, 1, poly, p, 8, 8, vreint_expected_p64_p8);
  TEST_VREINTERPRET(, poly, p, 64, 1, poly, p, 16, 4, vreint_expected_p64_p16);
  TEST_VREINTERPRET(, poly, p, 64, 1, float, f, 32, 2, vreint_expected_p64_f32);

  /* vreinterpretq_p64_* tests.  */
#undef TEST_MSG
#define TEST_MSG "VREINTERPRETQ_P64_*"
  TEST_VREINTERPRET(q, poly, p, 64, 2, int, s, 8, 16, vreint_expected_q_p64_s8);
  TEST_VREINTERPRET(q, poly, p, 64, 2, int, s, 16, 8, vreint_expected_q_p64_s16);
  TEST_VREINTERPRET(q, poly, p, 64, 2, int, s, 32, 4, vreint_expected_q_p64_s32);
  TEST_VREINTERPRET(q, poly, p, 64, 2, int, s, 64, 2, vreint_expected_q_p64_s64);
  TEST_VREINTERPRET(q, poly, p, 64, 2, uint, u, 8, 16, vreint_expected_q_p64_u8);
  TEST_VREINTERPRET(q, poly, p, 64, 2, uint, u, 16, 8, vreint_expected_q_p64_u16);
  TEST_VREINTERPRET(q, poly, p, 64, 2, uint, u, 32, 4, vreint_expected_q_p64_u32);
  TEST_VREINTERPRET(q, poly, p, 64, 2, uint, u, 64, 2, vreint_expected_q_p64_u64);
  TEST_VREINTERPRET(q, poly, p, 64, 2, poly, p, 8, 16, vreint_expected_q_p64_p8);
  TEST_VREINTERPRET(q, poly, p, 64, 2, poly, p, 16, 8, vreint_expected_q_p64_p16);
  TEST_VREINTERPRET(q, poly, p, 64, 2, float, f, 32, 4, vreint_expected_q_p64_f32);

  /* vreinterpret_*_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VREINTERPRET_*_P64"

  TEST_VREINTERPRET(, int, s, 8, 8, poly, p, 64, 1, vreint_expected_s8_p64);
  TEST_VREINTERPRET(, int, s, 16, 4, poly, p, 64, 1, vreint_expected_s16_p64);
  TEST_VREINTERPRET(, int, s, 32, 2, poly, p, 64, 1, vreint_expected_s32_p64);
  TEST_VREINTERPRET(, int, s, 64, 1, poly, p, 64, 1, vreint_expected_s64_p64);
  TEST_VREINTERPRET(, uint, u, 8, 8, poly, p, 64, 1, vreint_expected_u8_p64);
  TEST_VREINTERPRET(, uint, u, 16, 4, poly, p, 64, 1, vreint_expected_u16_p64);
  TEST_VREINTERPRET(, uint, u, 32, 2, poly, p, 64, 1, vreint_expected_u32_p64);
  TEST_VREINTERPRET(, uint, u, 64, 1, poly, p, 64, 1, vreint_expected_u64_p64);
  TEST_VREINTERPRET(, poly, p, 8, 8, poly, p, 64, 1, vreint_expected_p8_p64);
  TEST_VREINTERPRET(, poly, p, 16, 4, poly, p, 64, 1, vreint_expected_p16_p64);
  TEST_VREINTERPRET_FP(, float, f, 32, 2, poly, p, 64, 1, vreint_expected_f32_p64);
  TEST_VREINTERPRET(q, int, s, 8, 16, poly, p, 64, 2, vreint_expected_q_s8_p64);
  TEST_VREINTERPRET(q, int, s, 16, 8, poly, p, 64, 2, vreint_expected_q_s16_p64);
  TEST_VREINTERPRET(q, int, s, 32, 4, poly, p, 64, 2, vreint_expected_q_s32_p64);
  TEST_VREINTERPRET(q, int, s, 64, 2, poly, p, 64, 2, vreint_expected_q_s64_p64);
  TEST_VREINTERPRET(q, uint, u, 8, 16, poly, p, 64, 2, vreint_expected_q_u8_p64);
  TEST_VREINTERPRET(q, uint, u, 16, 8, poly, p, 64, 2, vreint_expected_q_u16_p64);
  TEST_VREINTERPRET(q, uint, u, 32, 4, poly, p, 64, 2, vreint_expected_q_u32_p64);
  TEST_VREINTERPRET(q, uint, u, 64, 2, poly, p, 64, 2, vreint_expected_q_u64_p64);
  TEST_VREINTERPRET(q, poly, p, 8, 16, poly, p, 64, 2, vreint_expected_q_p8_p64);
  TEST_VREINTERPRET(q, poly, p, 16, 8, poly, p, 64, 2, vreint_expected_q_p16_p64);
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, poly, p, 64, 2, vreint_expected_q_f32_p64);

  return 0;
}
