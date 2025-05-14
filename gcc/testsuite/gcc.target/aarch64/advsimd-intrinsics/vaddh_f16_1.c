/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected[] =
{
  0x3c00 /* 1.000000 */,
  0x3c00 /* 1.000000 */,
  0xc0a8 /* -2.328125 */,
  0x5672 /* 103.125000 */,
  0x5240 /* 50.000000 */,
  0x3614 /* 0.379883 */,
  0xbf34 /* -1.800781 */,
  0xc5e6 /* -5.898438 */,
  0xcaf4 /* -13.906250 */,
  0x4d14 /* 20.312500 */,
  0xc6e5 /* -6.894531 */,
  0x419a /* 2.800781 */,
  0xc69a /* -6.601562 */,
  0x4c8f /* 18.234375 */,
  0xc5fe /* -5.992188 */,
  0x4d15 /* 20.328125 */,
  0x7e00 /* nan */,
  0x7e00 /* nan */,
};

#define TEST_MSG "VADDH_F16"
#define INSN_NAME vaddh_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for binary scalar operations.  */
#include "binary_scalar_op.inc"
