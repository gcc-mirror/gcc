/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

/* Input values.  */
uint32_t input[] =
{
  0, -0,
  123, -567,
  -34, 1024,
  -63, 169,
  -4, 77,
  -144, -56,
  -16, -15,
  -14, -13,
};

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected[] =
{
  0x0000 /* 0.000000 */,
  0x0000 /* 0.000000 */,
  0x57b0 /* 123.000000 */,
  0xe06e /* -567.000000 */,
  0xd040 /* -34.000000 */,
  0x6400 /* 1024.000000 */,
  0xd3e0 /* -63.000000 */,
  0x5948 /* 169.000000 */,
  0xc400 /* -4.000000 */,
  0x54d0 /* 77.000000 */,
  0xd880 /* -144.000000 */,
  0xd300 /* -56.000000 */,
  0xcc00 /* -16.000000 */,
  0xcb80 /* -15.000000 */,
  0xcb00 /* -14.000000 */,
  0xca80 /* -13.000000 */
};

#define TEST_MSG "VCVTH_F16_S32"
#define INSN_NAME vcvth_f16_s32

#define INPUT input
#define EXPECTED expected

#define INPUT_TYPE uint32_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
