/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

/* Input values.  */
#define A 123.4
#define B 567.8
#define C 34.8
#define D 1024
#define E 663.1
#define F 144.0
#define G 4.8
#define H 77

#define RECP_A 0x2028 /* 1/A.  */
#define RECP_B 0x1734 /* 1/B.  */
#define RECP_C 0x275C /* 1/C.  */
#define RECP_D 0x13FC /* 1/D.  */
#define RECP_E 0x162C /* 1/E.  */
#define RECP_F 0x1F18 /* 1/F.  */
#define RECP_G 0x32A8 /* 1/G.  */
#define RECP_H 0x22A4 /* 1/H.  */

float16_t input[] = { A, B, C, D, E, F, G, H };
uint16_t expected[] = { RECP_A, RECP_B, RECP_C, RECP_D,
		        RECP_E, RECP_F, RECP_G, RECP_H };

#define TEST_MSG "VRECPEH_F16"
#define INSN_NAME vrecpeh_f16

#define INPUT input
#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
