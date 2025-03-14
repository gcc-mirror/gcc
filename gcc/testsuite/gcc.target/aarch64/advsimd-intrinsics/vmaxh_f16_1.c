/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

/* Input values.  */
#define A 123.4
#define B -567.8
#define C -34.8
#define D 1024
#define E 663.1
#define F 169.1
#define G -4.8
#define H 77

float16_t input_1[] = { A, B, C, D };
float16_t input_2[] = { E, F, G, H };
float16_t expected[] = { E, F, G, D };

#define TEST_MSG "VMAXH_F16"
#define INSN_NAME vmaxh_f16

#define INPUT_1 input_1
#define INPUT_2 input_2
#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "binary_scalar_op.inc"
