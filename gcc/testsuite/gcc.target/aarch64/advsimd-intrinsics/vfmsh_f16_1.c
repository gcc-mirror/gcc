/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected[] =
{
  0x0000 /* 0.000000 */,
  0x8000 /* -0.000000 */,
  0x42af /* 3.341797 */,
  0x5043 /* 34.093750 */,
  0xccd2 /* -19.281250 */,
  0x3712 /* 0.441895 */,
  0x3acc /* 0.849609 */,
  0x4848 /* 8.562500 */,
  0xcc43 /* -17.046875 */,
  0xd65c /* -101.750000 */,
  0x4185 /* 2.759766 */,
  0xcd39 /* -20.890625 */,
  0xd45b /* -69.687500 */,
  0x5241 /* 50.031250 */,
  0xc675 /* -6.457031 */,
  0x4d07 /* 20.109375 */,
  0x7c00 /* inf */,
  0xfc00 /* -inf */
};

#define TEST_MSG "VFMSH_F16"
#define INSN_NAME vfmsh_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for binary scalar operations.  */
#include "ternary_scalar_op.inc"
