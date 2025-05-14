/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected[] =
{
  0x0000 /* 0.000000 */,
  0x8000 /* -0.000000 */,
  0x4000 /* 2.000000 */,
  0x4400 /* 4.000000 */,
  0x4d00 /* 20.000000 */,
  0x3c00 /* 1.000000 */,
  0xc000 /* -2.000000 */,
  0x4000 /* 2.000000 */,
  0xc700 /* -7.000000 */,
  0x3c00 /* 1.000000 */,
  0x3c00 /* 1.000000 */,
  0x3c00 /* 1.000000 */,
  0x3c00 /* 1.000000 */,
  0x4b00 /* 14.000000 */,
  0xc600 /* -6.000000 */,
  0x4d00 /* 20.000000 */,
  0x7c00 /* inf */,
  0xfc00 /* -inf */
};

#define TEST_MSG "VRNDPH_F16"
#define INSN_NAME vrndph_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
