/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

#define INFF __builtin_inf ()

/* Expected results.
   Absolute difference between INPUT1 and INPUT2 in binary_scalar_op.inc.  */
uint16_t expected[] =
{
  0x3C00,
  0x3C00,
  0x4654,
  0x560E,
  0x4900,
  0x36B8,
  0x419a,
  0x4848,
  0x3d34,
  0x4cec,
  0x4791,
  0x3f34,
  0x484d,
  0x4804,
  0x469c,
  0x4ceb,
  0x7c00,
  0x7c00
};

#define TEST_MSG "VABDH_F16"
#define INSN_NAME vabdh_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for binary scalar operations.  */
#include "binary_scalar_op.inc"
