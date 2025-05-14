/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

#define INFF __builtin_inf ()

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected[] =
{
  0x0000 /* 0.000000 */,
  0x8000 /* -0.000000 */,
  0xc854 /* -8.656250 */,
  0x5cd8 /* 310.000000 */,
  0x60b0 /* 600.000000 */,
  0xa019 /* -0.008003 */,
  0xbc9a /* -1.150391 */,
  0xc8cf /* -9.617188 */,
  0x51fd /* 47.906250 */,
  0x4634 /* 6.203125 */,
  0xc0d9 /* -2.423828 */,
  0x3c9a /* 1.150391 */,
  0xc79a /* -7.601562 */,
  0x5430 /* 67.000000 */,
  0xbfd0 /* -1.953125 */,
  0x46ac /* 6.671875 */,
  0xfc00 /* -inf */,
  0xfc00 /* -inf */
};

#define TEST_MSG "VMULH_F16"
#define INSN_NAME vmulh_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for binary scalar operations.  */
#include "binary_scalar_op.inc"
