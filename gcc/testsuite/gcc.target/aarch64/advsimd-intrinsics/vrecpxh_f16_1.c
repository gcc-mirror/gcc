/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

/* Input values.  */

float16_t input[] = { 123.4, 567.8, 34.8, 1024, 663.1, 144.0, 4.8, 77 };
/*  Expected results are calculated by:
  for (index = 0; index < 8; index++)
    {
      uint16_t src_cast = * (uint16_t *) &src[index];
      * (uint16_t *) &expected[index] =
	(src_cast & 0x8000) | (~src_cast & 0x7C00);
    }  */
uint16_t expected[8] = { 0x2800, 0x1C00, 0x2C00, 0x1800,
			 0x1C00, 0x2400, 0x3800, 0x2800 };

#define TEST_MSG "VRECPXH_F16"
#define INSN_NAME vrecpxh_f16

#define INPUT input
#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
