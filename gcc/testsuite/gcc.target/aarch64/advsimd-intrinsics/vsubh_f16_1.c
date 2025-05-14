/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

#define INFF __builtin_inf ()

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected[] =
{
  0xbc00 /* -1.000000 */,
  0xbc00 /* -1.000000 */,
  0x4654 /* 6.328125 */,
  0xd60e /* -96.875000 */,
  0xc900 /* -10.000000 */,
  0x36b8 /* 0.419922 */,
  0xc19a /* -2.800781 */,
  0x4848 /* 8.562500 */,
  0xbd34 /* -1.300781 */,
  0xccec /* -19.687500 */,
  0x4791 /* 7.566406 */,
  0xbf34 /* -1.800781 */,
  0x484d /* 8.601562 */,
  0x4804 /* 8.031250 */,
  0xc69c /* -6.609375 */,
  0x4ceb /* 19.671875 */,
  0x7c00 /* inf */,
  0xfc00 /* -inf */
};

#define TEST_MSG "VSUB_F16"
#define INSN_NAME vsubh_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for binary scalar operations.  */
#include "binary_scalar_op.inc"
