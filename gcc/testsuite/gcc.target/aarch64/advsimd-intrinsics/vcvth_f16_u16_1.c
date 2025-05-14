/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

uint16_t input[] = { 123, 567, 0, 1024, 63, 169, 4, 77 };
uint16_t expected[] = { 0x57B0 /* 123.0.  */, 0x606E /* 567.0.  */,
			0x0000 /* 0.0.  */, 0x6400 /* 1024.0.  */,
			0x53E0 /* 63.0.  */, 0x5948 /* 169.0.  */,
			0x4400 /* 4.0.  */, 0x54D0 /* 77.0.  */ };

#define TEST_MSG "VCVTH_F16_U16"
#define INSN_NAME vcvth_f16_u16

#define EXPECTED expected

#define INPUT input
#define INPUT_TYPE uint16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for binary scalar operations.  */
#include "unary_scalar_op.inc"
