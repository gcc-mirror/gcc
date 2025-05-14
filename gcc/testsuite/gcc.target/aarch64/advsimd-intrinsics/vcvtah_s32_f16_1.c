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
  0x0000007b,
  0xfffffdc8,
  0xffffffdd,
  0x00000400,
  0x00000297,
  0x000000a9,
  0xfffffffb,
  0x0000004d,
  0xffffff6f,
  0xffffffc7,
  0xfffffff0,
  0xfffffff1,
  0xfffffff2,
  0xfffffff3
};

#define TEST_MSG "VCVTAH_S32_F16"
#define INSN_NAME vcvtah_s32_f16

#define INPUT input
#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE int32_t
#define OUTPUT_TYPE_SIZE 32

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
