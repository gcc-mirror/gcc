/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

/* Input values.  */
float16_t input[] = { 2.5, 100, 7.1, -9.9, -5.0, 9.1, -4.8, 77 };
int16_t expected_1[] = { 5, 200, 14, -19, -10, 18, -9, 154 };
int16_t expected_2[] = { 10, 400, 28, -39, -20, 36, -19, 308 };

#define TEST_MSG "VCVTH_N_S16_F16"
#define INSN_NAME vcvth_n_s16_f16

#define INPUT input
#define EXPECTED_1 expected_1
#define EXPECTED_2 expected_2

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE int16_t
#define OUTPUT_TYPE_SIZE 16

#define SCALAR_OPERANDS
#define SCALAR_1 1
#define SCALAR_2 2

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
