/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected[] =
{
 0x0000 /* 0.000000 */,
 0x0000 /* 0.000000 */,
 0x3944 /* 0.658203 */,
 0xcefa /* -27.906250 */,
 0x5369 /* 59.281250 */,
 0x35ba /* 0.357910 */,
 0xc574 /* -5.453125 */,
 0xc5e6 /* -5.898438 */,
 0x3f66 /* 1.849609 */,
 0x5665 /* 102.312500 */,
 0xc02d /* -2.087891 */,
 0x4d79 /* 21.890625 */,
 0x547b /* 71.687500 */,
 0xcdf0 /* -23.750000 */,
 0xc625 /* -6.144531 */,
 0x4cf9 /* 19.890625 */,
 0x7e00 /* nan */,
 0x7e00 /* nan */
};

#define TEST_MSG "VFMAH_F16"
#define INSN_NAME vfmah_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for binary scalar operations.  */
#include "ternary_scalar_op.inc"
