/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

#define INFF __builtin_inf ()

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected[] =
{
  0x0000 /* 0.000000 */,
  0x8000 /* -0.000000 */,
  0xc454 /* -4.328125 */,
  0x4233 /* 3.099609 */,
  0x4d00 /* 20.000000 */,
  0xa51f /* -0.020004 */,
  0xc09a /* -2.300781 */,
  0xc73b /* -7.230469 */,
  0xc79a /* -7.601562 */,
  0x34f6 /* 0.310059 */,
  0xc73b /* -7.230469 */,
  0x3800 /* 0.500000 */,
  0xc79a /* -7.601562 */,
  0x451a /* 5.101562 */,
  0xc64d /* -6.300781 */,
  0x3556 /* 0.333496 */,
  0xfc00 /* -inf */,
  0xfc00 /* -inf */
};

#define TEST_MSG "VMINNMH_F16"
#define INSN_NAME vminnmh_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for binary scalar operations.  */
#include "binary_scalar_op.inc"
