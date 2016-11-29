/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

/* Input values.  */
float16_t input[] = { 123.9, -56.8, 0.7, 24.6, -63.5, 169.4, -4.3, 77.0 };
int16_t expected[] = { 124, -57, 1, 25, -64, 169, -4, 77 };

#define TEST_MSG "VCVTAH_S16_F16"
#define INSN_NAME vcvtah_s16_f16

#define INPUT input
#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE int16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
