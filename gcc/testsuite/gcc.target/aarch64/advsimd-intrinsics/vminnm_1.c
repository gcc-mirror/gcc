/* This file tests an intrinsic which currently has only an f16 variant and that
   is only available when FP16 arithmetic instructions are supported.  */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vminnm
#define TEST_MSG "VMINNM/VMINMQ"

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
#define HAS_FLOAT16_VARIANT
#endif

/* Expected results.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 4) [] = { 0xcc00, 0xcbc0, 0xcbc0, 0xcbc0 };
VECT_VAR_DECL(expected, hfloat, 16, 8) [] = { 0xcc00, 0xcb80, 0xcb40, 0xcb40,
					      0xcb40, 0xcb40, 0xcb40, 0xcb40 };
#endif

/* Expected results with special FP values.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_nan, hfloat, 16, 8) [] = { 0x3c00, 0x3c00,
						  0x3c00, 0x3c00,
						  0x3c00, 0x3c00,
						  0x3c00, 0x3c00 };
VECT_VAR_DECL(expected_mnan, hfloat, 16, 8) [] = { 0x3c00, 0x3c00,
						   0x3c00, 0x3c00,
						   0x3c00, 0x3c00,
						   0x3c00, 0x3c00 };
VECT_VAR_DECL(expected_inf, hfloat, 16, 8) [] = { 0x3c00, 0x3c00,
						  0x3c00, 0x3c00,
						  0x3c00, 0x3c00,
						  0x3c00, 0x3c00 };
VECT_VAR_DECL(expected_minf, hfloat, 16, 8) [] = { 0xfc00, 0xfc00,
						   0xfc00, 0xfc00,
						   0xfc00, 0xfc00,
						   0xfc00, 0xfc00 };
VECT_VAR_DECL(expected_zero1, hfloat, 16, 8) [] = { 0x8000, 0x8000,
						    0x8000, 0x8000,
						    0x8000, 0x8000,
						    0x8000, 0x8000 };
VECT_VAR_DECL(expected_zero2, hfloat, 16, 8) [] = { 0x8000, 0x8000,
						    0x8000, 0x8000,
						    0x8000, 0x8000,
						    0x8000, 0x8000 };
#endif

#include "binary_op_float.inc"
