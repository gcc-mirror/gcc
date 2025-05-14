/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

/* Input values.  */
float16_t input[] = { 123.4, 67.8, 34.8, 24.0, 66.1, 144.0, 4.8, 77.0 };
uint16_t expected[] = { 0x2DC4 /* FP16_C (1/__builtin_sqrtf (123.4)).  */,
			0x2FC8 /* FP16_C (1/__builtin_sqrtf (67.8)).  */,
			0x316C /* FP16_C (1/__builtin_sqrtf (34.8)).  */,
			0x3288 /* FP16_C (1/__builtin_sqrtf (24.0)).  */,
			0x2FDC /* FP16_C (1/__builtin_sqrtf (66.1)).  */,
			0x2D54 /* FP16_C (1/__builtin_sqrtf (144.0)).  */,
			0x3750 /* FP16_C (1/__builtin_sqrtf (4.8)).  */,
			0x2F48 /* FP16_C (1/__builtin_sqrtf (77.0)).  */ };

#define TEST_MSG "VRSQRTEH_F16"
#define INSN_NAME vrsqrteh_f16

#define INPUT input
#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
