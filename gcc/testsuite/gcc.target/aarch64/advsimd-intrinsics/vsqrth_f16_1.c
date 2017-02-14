/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected[] =
{
  0x0000 /* 0.000000 */,
  0x8000 /* -0.000000 */,
  0x3da8 /* 1.414062 */,
  0x3f0b /* 1.760742 */,
  0x4479 /* 4.472656 */,
  0x390f /* 0.632324 */,
  0x7e00 /* nan */,
  0x3c9d /* 1.153320 */,
  0x7e00 /* nan */,
  0x3874 /* 0.556641 */,
  0x38a2 /* 0.579102 */,
  0x39a8 /* 0.707031 */,
  0x3c00 /* 1.000000 */,
  0x433f /* 3.623047 */,
  0x7e00 /* nan */,
  0x4479 /* 4.472656 */,
  0x7c00 /* inf */,
  0x7e00 /* nan */
};

#define TEST_MSG "VSQRTH_F16"
#define INSN_NAME vsqrth_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
