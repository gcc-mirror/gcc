/* This file contains tests for the vreinterpret *p128 intrinsics.  */

/* { dg-require-effective-target arm_crypto_ok { target { arm*-*-* } } } */
/* { dg-add-options arm_crypto } */
/* { dg-additional-options "-march=armv8-a+crypto" { target { aarch64*-*-* } } }*/

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results: vreinterpretq_p128_*.  */
VECT_VAR_DECL(vreint_expected_q_p128_s8,poly,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
							  0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(vreint_expected_q_p128_s16,poly,64,2) [] = { 0xfff3fff2fff1fff0,
							   0xfff7fff6fff5fff4 };
VECT_VAR_DECL(vreint_expected_q_p128_s32,poly,64,2) [] = { 0xfffffff1fffffff0,
							   0xfffffff3fffffff2 };
VECT_VAR_DECL(vreint_expected_q_p128_s64,poly,64,2) [] = { 0xfffffffffffffff0,
							   0xfffffffffffffff1 };
VECT_VAR_DECL(vreint_expected_q_p128_u8,poly,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
							  0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(vreint_expected_q_p128_u16,poly,64,2) [] = { 0xfff3fff2fff1fff0,
							   0xfff7fff6fff5fff4 };
VECT_VAR_DECL(vreint_expected_q_p128_u32,poly,64,2) [] = { 0xfffffff1fffffff0,
							   0xfffffff3fffffff2 };
VECT_VAR_DECL(vreint_expected_q_p128_u64,poly,64,2) [] = { 0xfffffffffffffff0,
							   0xfffffffffffffff1 };
VECT_VAR_DECL(vreint_expected_q_p128_p8,poly,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
							  0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(vreint_expected_q_p128_p16,poly,64,2) [] = { 0xfff3fff2fff1fff0,
							   0xfff7fff6fff5fff4 };
VECT_VAR_DECL(vreint_expected_q_p128_f32,poly,64,2) [] = { 0xc1700000c1800000,
							   0xc1500000c1600000 };
VECT_VAR_DECL(vreint_expected_q_p128_f16,poly,64,2) [] = { 0xca80cb00cb80cc00,
							   0xc880c900c980ca00 };
#ifdef __aarch64__
VECT_VAR_DECL(vreint_expected_q_p128_f64,poly,64,2) [] = { 0xc030000000000000,
							   0xc02e000000000000 };
#endif

/* Expected results: vreinterpretq_*_p128.  */
VECT_VAR_DECL(vreint_expected_q_s8_p128,int,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
							 0xff, 0xff, 0xff, 0xff,
							 0xf1, 0xff, 0xff, 0xff,
							 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(vreint_expected_q_s16_p128,int,16,8) [] = { 0xfff0, 0xffff,
							  0xffff, 0xffff,
							  0xfff1, 0xffff,
							  0xffff, 0xffff };
VECT_VAR_DECL(vreint_expected_q_s32_p128,int,32,4) [] = { 0xfffffff0, 0xffffffff,
							  0xfffffff1, 0xffffffff };
VECT_VAR_DECL(vreint_expected_q_s64_p128,int,64,2) [] = { 0xfffffffffffffff0,
							  0xfffffffffffffff1 };
VECT_VAR_DECL(vreint_expected_q_u8_p128,uint,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
							  0xff, 0xff, 0xff, 0xff,
							  0xf1, 0xff, 0xff, 0xff,
							  0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(vreint_expected_q_u16_p128,uint,16,8) [] = { 0xfff0, 0xffff,
							   0xffff, 0xffff,
							   0xfff1, 0xffff,
							   0xffff, 0xffff };
VECT_VAR_DECL(vreint_expected_q_u32_p128,uint,32,4) [] = { 0xfffffff0, 0xffffffff,
							   0xfffffff1, 0xffffffff };
VECT_VAR_DECL(vreint_expected_q_u64_p128,uint,64,2) [] = { 0xfffffffffffffff0,
							   0xfffffffffffffff1 };
VECT_VAR_DECL(vreint_expected_q_p8_p128,poly,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
							  0xff, 0xff, 0xff, 0xff,
							  0xf1, 0xff, 0xff, 0xff,
							  0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(vreint_expected_q_p16_p128,poly,16,8) [] = { 0xfff0, 0xffff,
							   0xffff, 0xffff,
							   0xfff1, 0xffff,
							   0xffff, 0xffff };
VECT_VAR_DECL(vreint_expected_q_p64_p128,uint,64,2) [] = { 0xfffffffffffffff0,
							   0xfffffffffffffff1 };
VECT_VAR_DECL(vreint_expected_q_f32_p128,hfloat,32,4) [] = { 0xfffffff0, 0xffffffff,
							     0xfffffff1, 0xffffffff };
VECT_VAR_DECL(vreint_expected_q_f16_p128,hfloat,16,8) [] = { 0xfff0, 0xffff,
							     0xffff, 0xffff,
							     0xfff1, 0xffff,
							     0xffff, 0xffff };
#ifdef __aarch64__
VECT_VAR_DECL(vreint_expected_q_f64_p128,hfloat,64,2) [] = { 0xfffffffffffffff0,
							     0xfffffffffffffff1 };
#endif

int main (void)
{
  DECL_VARIABLE_128BITS_VARIANTS(vreint_vector);
  DECL_VARIABLE_128BITS_VARIANTS(vreint_vector_res);

  clean_results ();

  TEST_MACRO_128BITS_VARIANTS_2_5(VLOAD, vreint_vector, buffer);
  VLOAD(vreint_vector, buffer, q, poly, p, 64, 2);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VLOAD(vreint_vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vreint_vector, buffer, q, float, f, 32, 4);

#ifdef __aarch64__
  VLOAD(vreint_vector, buffer, q, float, f, 64, 2);
#endif

  /* vreinterpretq_p128_* tests.  */
#undef TEST_MSG
#define TEST_MSG "VREINTERPRETQ_P128_*"

  /* Since there is no way to store a poly128_t value, convert to
     poly64x2_t before storing. This means that we are not able to
     test vreinterpretq_p128* alone, and that errors in
     vreinterpretq_p64_p128 could compensate for errors in
     vreinterpretq_p128*.  */
#define TEST_VREINTERPRET128(Q, T1, T2, W, N, TS1, TS2, WS, NS, EXPECTED) \
  VECT_VAR(vreint_vector_res, poly, 64, 2) =  vreinterpretq_p64_p128(	\
    vreinterpret##Q##_##T2##W##_##TS2##WS(VECT_VAR(vreint_vector, TS1, WS, NS))); \
  vst1##Q##_##T2##64(VECT_VAR(result, poly, 64, 2),			\
                     VECT_VAR(vreint_vector_res, poly, 64, 2));		\
  CHECK_POLY(TEST_MSG, T1, 64, 2, PRIx##64, EXPECTED, "");

  TEST_VREINTERPRET128(q, poly, p, 128, 1, int, s, 8, 16, vreint_expected_q_p128_s8);
  TEST_VREINTERPRET128(q, poly, p, 128, 1, int, s, 16, 8, vreint_expected_q_p128_s16);
  TEST_VREINTERPRET128(q, poly, p, 128, 1, int, s, 32, 4, vreint_expected_q_p128_s32);
  TEST_VREINTERPRET128(q, poly, p, 128, 1, int, s, 64, 2, vreint_expected_q_p128_s64);
  TEST_VREINTERPRET128(q, poly, p, 128, 1, uint, u, 8, 16, vreint_expected_q_p128_u8);
  TEST_VREINTERPRET128(q, poly, p, 128, 1, uint, u, 16, 8, vreint_expected_q_p128_u16);
  TEST_VREINTERPRET128(q, poly, p, 128, 1, uint, u, 32, 4, vreint_expected_q_p128_u32);
  TEST_VREINTERPRET128(q, poly, p, 128, 1, uint, u, 64, 2, vreint_expected_q_p128_u64);
  TEST_VREINTERPRET128(q, poly, p, 128, 1, poly, p, 8, 16, vreint_expected_q_p128_p8);
  TEST_VREINTERPRET128(q, poly, p, 128, 1, poly, p, 16, 8, vreint_expected_q_p128_p16);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET128(q, poly, p, 128, 1, float, f, 16, 8, vreint_expected_q_p128_f16);
#endif
  TEST_VREINTERPRET128(q, poly, p, 128, 1, float, f, 32, 4, vreint_expected_q_p128_f32);

#ifdef __aarch64__
  TEST_VREINTERPRET128(q, poly, p, 128, 1, float, f, 64, 2, vreint_expected_q_p128_f64);
#endif

  /* vreinterpretq_*_p128 tests.  */
#undef TEST_MSG
#define TEST_MSG "VREINTERPRETQ_*_P128"

  /* Since there is no way to load a poly128_t value, load a
     poly64x2_t and convert it to poly128_t. This means that we are
     not able to test vreinterpretq_*_p128 alone, and that errors in
     vreinterpretq_p128_p64 could compensate for errors in
     vreinterpretq_*_p128*.  */
#define TEST_VREINTERPRET_FROM_P128(Q, T1, T2, W, N, TS1, TS2, WS, NS, EXPECTED) \
  VECT_VAR(vreint_vector_res, T1, W, N) =				\
    vreinterpret##Q##_##T2##W##_##TS2##WS(				\
  vreinterpretq_p128_p64(VECT_VAR(vreint_vector, TS1, 64, 2)));		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vreint_vector_res, T1, W, N));		\
  CHECK(TEST_MSG, T1, W, N, PRIx##W, EXPECTED, "");

#define TEST_VREINTERPRET_FP_FROM_P128(Q, T1, T2, W, N, TS1, TS2, WS, NS, EXPECTED) \
  VECT_VAR(vreint_vector_res, T1, W, N) =				\
    vreinterpret##Q##_##T2##W##_##TS2##WS(				\
  vreinterpretq_p128_p64(VECT_VAR(vreint_vector, TS1, 64, 2)));		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vreint_vector_res, T1, W, N));		\
  CHECK_FP(TEST_MSG, T1, W, N, PRIx##W, EXPECTED, "");

  TEST_VREINTERPRET_FROM_P128(q, int, s, 8, 16, poly, p, 128, 1, vreint_expected_q_s8_p128);
  TEST_VREINTERPRET_FROM_P128(q, int, s, 16, 8, poly, p, 128, 1, vreint_expected_q_s16_p128);
  TEST_VREINTERPRET_FROM_P128(q, int, s, 32, 4, poly, p, 128, 1, vreint_expected_q_s32_p128);
  TEST_VREINTERPRET_FROM_P128(q, int, s, 64, 2, poly, p, 128, 1, vreint_expected_q_s64_p128);
  TEST_VREINTERPRET_FROM_P128(q, uint, u, 8, 16, poly, p, 128, 1, vreint_expected_q_u8_p128);
  TEST_VREINTERPRET_FROM_P128(q, uint, u, 16, 8, poly, p, 128, 1, vreint_expected_q_u16_p128);
  TEST_VREINTERPRET_FROM_P128(q, uint, u, 32, 4, poly, p, 128, 1, vreint_expected_q_u32_p128);
  TEST_VREINTERPRET_FROM_P128(q, uint, u, 64, 2, poly, p, 128, 1, vreint_expected_q_u64_p128);
  TEST_VREINTERPRET_FROM_P128(q, poly, p, 8, 16, poly, p, 128, 1, vreint_expected_q_p8_p128);
  TEST_VREINTERPRET_FROM_P128(q, poly, p, 16, 8, poly, p, 128, 1, vreint_expected_q_p16_p128);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET_FP_FROM_P128(q, float, f, 16, 8, poly, p, 128, 1, vreint_expected_q_f16_p128);
#endif
  TEST_VREINTERPRET_FP_FROM_P128(q, float, f, 32, 4, poly, p, 128, 1, vreint_expected_q_f32_p128);

#ifdef __aarch64__
  TEST_VREINTERPRET_FP_FROM_P128(q, float, f, 64, 2, poly, p, 128, 1, vreint_expected_q_f64_p128);
#endif
  return 0;
}
