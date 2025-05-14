/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

uint16_t expected[] =
{
  0x8000 /* -0.000000 */,
  0x0000 /* 0.000000 */,
  0xc000 /* -2.000000 */,
  0xc233 /* -3.099609 */,
  0xcd00 /* -20.000000 */,
  0xb666 /* -0.399902 */,
  0x409a /* 2.300781 */,
  0xbd52 /* -1.330078 */,
  0x479a /* 7.601562 */,
  0xb4f6 /* -0.310059 */,
  0xb55d /* -0.335205 */,
  0xb800 /* -0.500000 */,
  0xbc00 /* -1.000000 */,
  0xca91 /* -13.132812 */,
  0x464d /* 6.300781 */,
  0xcd00 /* -20.000000 */,
  0xfc00 /* -inf */,
  0x7c00 /* inf */
};

#define TEST_MSG "VNEGH_F16"
#define INSN_NAME vnegh_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
