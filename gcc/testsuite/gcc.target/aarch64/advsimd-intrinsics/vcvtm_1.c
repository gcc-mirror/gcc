/* This file tests an intrinsic which currently has only an f16 variant and that
   is only available when FP16 arithmetic instructions are supported.  */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, int, 16, 4) [] = { 0xfff0, 0x5, 0xfff0, 0x5 };
VECT_VAR_DECL(expected, uint, 16, 4) [] = { 0x0, 0x5, 0x0, 0x5 };
VECT_VAR_DECL(expected, int, 16, 8) [] = { 0x0, 0x0, 0xf, 0xfff0, 0x0,
					   0x0, 0xf, 0xfff0 };
VECT_VAR_DECL(expected, uint, 16, 8) [] = { 0x0, 0x0, 0xf, 0x0,
					    0x0, 0x0, 0xf, 0x0 };
#endif

/* Expected results with rounding.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_rounding, int, 16, 4) [] = { 0xa, 0xa, 0xa, 0xa };
VECT_VAR_DECL(expected_rounding, uint, 16, 4) [] = { 0xa, 0xa, 0xa, 0xa };
VECT_VAR_DECL(expected_rounding, int, 16, 8) [] = { 0x7d, 0x7d, 0x7d, 0x7d,
						    0x7d, 0x7d, 0x7d, 0x7d };
VECT_VAR_DECL(expected_rounding, uint, 16, 8) [] = { 0x7d, 0x7d, 0x7d, 0x7d,
						     0x7d, 0x7d, 0x7d, 0x7d };
#endif

#define TEST_MSG "VCVTM/VCVTMQ"
#define INSN_NAME vcvtm

#include "vcvtX.inc"
