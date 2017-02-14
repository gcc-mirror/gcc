/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

#define INFF __builtin_inf ()

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected[] =
{
  0x0000 /* 0.000000 */,
  0x8000 /* -0.000000 */,
  0xb765 /* -0.462158 */,
  0x27ef /* 0.030991 */,
  0x3955 /* 0.666504 */,
  0xccff /* -19.984375 */,
  0xc49a /* -4.601562 */,
  0xb1e3 /* -0.183960 */,
  0x3cd3 /* 1.206055 */,
  0x23f0 /* 0.015503 */,
  0xa9ef /* -0.046356 */,
  0x32f4 /* 0.217285 */,
  0xb036 /* -0.131592 */,
  0x4126 /* 2.574219 */,
  0xcd15 /* -20.328125 */,
  0x537f /* 59.968750 */,
  0x7e00 /* nan */,
  0x7e00 /* nan */
};

#define TEST_MSG "VDIVH_F16"
#define INSN_NAME vdivh_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for binary scalar operations.  */
#include "binary_scalar_op.inc"
