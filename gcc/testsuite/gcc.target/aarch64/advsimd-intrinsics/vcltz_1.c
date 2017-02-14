/* This file tests an intrinsic which currently has only an f16 variant and that
   is only available when FP16 arithmetic instructions are supported.  */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */

#define INSN_NAME vcltz
#define TEST_MSG "VCLTZ/VCLTZQ"

#include "cmp_zero_op.inc"

/* Expected results.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL (expected_float, uint, 16, 4) [] = { 0xffff, 0xffff,
						   0xffff, 0xffff };
VECT_VAR_DECL (expected_q_float, uint, 16, 8) [] = { 0x0, 0x0, 0x0, 0x0 };
#endif

/* Extra FP tests with special values (NaN, ....).  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL (expected_nan, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0  };
VECT_VAR_DECL (expected_mnan, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL (expected_inf, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };

VECT_VAR_DECL (expected_minf, uint, 16, 4) [] = { 0xffff, 0xffff,
						  0xffff, 0xffff };
VECT_VAR_DECL (expected_zero, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL (expected_mzero, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };
#endif
