/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

/* Input values.  */
#define A 12.4
#define B -5.8
#define C -3.8
#define D 10
#define E 66.1
#define F 16.1
#define G -4.8
#define H -77

#define I 0.7
#define J -78
#define K 10.23
#define L 98
#define M 87
#define N -87.81
#define O -1.1
#define P 47.8

float16_t input_1[] = { A, B, C, D, I, J, K, L };
float16_t input_2[] = { E, F, G, H, M, N, O, P };
uint16_t expected[] = { 0xE264 /* 2.0f - A * E.  */,
			0x55F6 /* 2.0f - B * F.  */,
			0xCC10 /* 2.0f - C * G.  */,
			0x6208 /* 2.0f - D * H.  */,
			0xD35D /* 2.0f - I * M.  */,
			0xEEB0 /* 2.0f - J * N.  */,
			0x4A9F /* 2.0f - K * O.  */,
			0xEC93 /* 2.0f - L * P.  */ };

#define TEST_MSG "VRECPSH_F16"
#define INSN_NAME vrecpsh_f16

#define INPUT_1 input_1
#define INPUT_2 input_2
#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "binary_scalar_op.inc"
