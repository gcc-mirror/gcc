/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

/* Input values.  */
#define A 13.4
#define B __builtin_inff ()
#define C -34.8
#define D -__builtin_inff ()
#define E 63.1
#define F 0.0
#define G -4.8
#define H 0.0

#define I 0.7
#define J -__builtin_inff ()
#define K 11.23
#define L 98
#define M 87.1
#define N -0.0
#define O -1.1
#define P 7

float16_t input_1[] = { A, B, C, D, I, J, K, L };
float16_t input_2[] = { E, F, G, H, M, N, O, P };
uint16_t expected[] = { 0x629B /* A * E.  */,
			0x4000 /* FP16_C (2.0f).  */,
			0x5939 /* C * G.  */,
			0xC000 /* FP16_C (-2.0f).  */,
			0x53A0 /* I * M.  */,
			0x4000 /* FP16_C (2.0f).  */,
			0xCA2C /* K * O.  */,
			0x615C /* L * P.  */ };

#define TEST_MSG "VMULXH_F16"
#define INSN_NAME vmulxh_f16

#define INPUT_1 input_1
#define INPUT_2 input_2
#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "binary_scalar_op.inc"
