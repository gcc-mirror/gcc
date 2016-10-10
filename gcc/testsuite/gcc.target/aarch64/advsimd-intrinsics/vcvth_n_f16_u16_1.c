/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

/* Input values.  */
uint16_t input[] = { 1, 10, 48, 100, 1000, 0, 500, 9 };

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected_1[] = { 0x3800 /* 0.5.  */,
			  0x4500 /* 5.  */,
			  0x4E00 /* 24.  */,
			  0x5240 /* 50.  */,
			  0x5FD0 /* 500.  */,
			  0x0000 /* 0.0.  */,
			  0x5BD0 /* 250.  */,
			  0x4480 /* 4.5.  */ };

uint16_t expected_2[] = { 0x3400 /* 0.25.  */,
			  0x4100 /* 2.5.  */,
			  0x4A00 /* 12.  */,
			  0x4E40 /* 25.  */,
			  0x5BD0 /* 250.  */,
			  0x0000 /* 0.0.  */,
			  0x57D0 /* 125.  */,
			  0x4080 /* 2.25.  */ };

#define TEST_MSG "VCVTH_N_F16_U16"
#define INSN_NAME vcvth_n_f16_u16

#define INPUT input
#define EXPECTED_1 expected_1
#define EXPECTED_2 expected_2

#define INPUT_TYPE uint16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

#define SCALAR_OPERANDS
#define SCALAR_1 1
#define SCALAR_2 2

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
