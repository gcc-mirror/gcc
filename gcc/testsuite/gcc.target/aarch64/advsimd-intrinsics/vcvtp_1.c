/* This file tests an intrinsic which currently has only an f16 variant and that
   is only available when FP16 arithmetic instructions are supported.  */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, int, 16, 4) [] = { 0xfff1, 0x6, 0xfff1, 0x6 };
VECT_VAR_DECL(expected, uint, 16, 4) [] = { 0x0, 0x6, 0x0, 0x6 };
VECT_VAR_DECL(expected, int, 16, 8) [] = { 0x0, 0x0, 0x10, 0xfff1,
					   0x0, 0x0, 0x10, 0xfff1 };
VECT_VAR_DECL(expected, uint, 16, 8) [] = { 0x0, 0x0, 0x10, 0x0,
					    0x0, 0x0, 0x10, 0x0 };
#endif

/* Expected results with rounding.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_rounding, int, 16, 4) [] = { 0xb, 0xb, 0xb, 0xb };
VECT_VAR_DECL(expected_rounding, uint, 16, 4) [] = { 0xb, 0xb, 0xb, 0xb };
VECT_VAR_DECL(expected_rounding, int, 16, 8) [] = { 0x7e, 0x7e, 0x7e, 0x7e,
						    0x7e, 0x7e, 0x7e, 0x7e };
VECT_VAR_DECL(expected_rounding, uint, 16, 8) [] = { 0x7e, 0x7e, 0x7e, 0x7e,
						     0x7e, 0x7e, 0x7e, 0x7e };
#endif

#define TEST_MSG "VCVTP/VCVTPQ"
#define INSN_NAME vcvtp

#include "vcvtX.inc"
