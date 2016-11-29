/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

/* Input values.  */
float16_t input[] =
{
  0.0, -0.0,
  123.4, -567.8,
  -34.8, 1024,
  663.1, 169.1,
  -4.8, 77.0,
  -144.5, -56.8,

  (float16_t) -16, (float16_t) -15,
  (float16_t) -14, (float16_t) -13,
};

/* Expected results (32-bit hexadecimal representation).  */
uint32_t expected[] =
{
  0x00000000,
  0x00000000,
  0x0000007c,
  0x00000000,
  0x00000000,
  0x00000400,
  0x00000297,
  0x000000aa,
  0x00000000,
  0x0000004d,
  0x00000000,
  0x00000000,
  0x00000000,
  0x00000000,
  0x00000000,
  0x00000000,
};

#define TEST_MSG "VCVTPH_U32_F16"
#define INSN_NAME vcvtph_u32_f16

#define INPUT input
#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE uint32_t
#define OUTPUT_TYPE_SIZE 32

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
