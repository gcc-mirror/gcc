/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

/* Input values.  */
uint32_t input[] =
{
  0, -0,
  123, -567,
  -34, 1024,
  -63, 169,
  -4, 77,
  -144, -56,
  -16, -15,
  -14, -13,
};

/* Expected results (16-bit hexadecimal representation).  */
uint16_t expected_1[] =
{
  0x0000 /* 0.000000 */,
  0x0000 /* 0.000000 */,
  0x53b0 /* 61.500000 */,
  0xdc6e /* -283.500000 */,
  0xcc40 /* -17.000000 */,
  0x6000 /* 512.000000 */,
  0xcfe0 /* -31.500000 */,
  0x5548 /* 84.500000 */,
  0xc000 /* -2.000000 */,
  0x50d0 /* 38.500000 */,
  0xd480 /* -72.000000 */,
  0xcf00 /* -28.000000 */,
  0xc800 /* -8.000000 */,
  0xc780 /* -7.500000 */,
  0xc700 /* -7.000000 */,
  0xc680 /* -6.500000 */
};

uint16_t expected_2[] =
{
  0x0000 /* 0.000000 */,
  0x0000 /* 0.000000 */,
  0x4fb0 /* 30.750000 */,
  0xd86e /* -141.750000 */,
  0xc840 /* -8.500000 */,
  0x5c00 /* 256.000000 */,
  0xcbe0 /* -15.750000 */,
  0x5148 /* 42.250000 */,
  0xbc00 /* -1.000000 */,
  0x4cd0 /* 19.250000 */,
  0xd080 /* -36.000000 */,
  0xcb00 /* -14.000000 */,
  0xc400 /* -4.000000 */,
  0xc380 /* -3.750000 */,
  0xc300 /* -3.500000 */,
  0xc280 /* -3.250000 */
};

uint16_t expected_3[] =
{
 0x0000 /* 0.000000 */,
 0x0000 /* 0.000000 */,
 0x0000 /* 0.000000 */,
 0x8002 /* -0.000000 */,
 0x8000 /* -0.000000 */,
 0x0004 /* 0.000000 */,
 0x8000 /* -0.000000 */,
 0x0001 /* 0.000000 */,
 0x8000 /* -0.000000 */,
 0x0000 /* 0.000000 */,
 0x8001 /* -0.000000 */,
 0x8000 /* -0.000000 */,
 0x8000 /* -0.000000 */,
 0x8000 /* -0.000000 */,
 0x8000 /* -0.000000 */,
 0x8000 /* -0.000000 */
};

#define TEST_MSG "VCVTH_N_F16_S32"
#define INSN_NAME vcvth_n_f16_s32

#define INPUT input
#define EXPECTED_1 expected_1
#define EXPECTED_2 expected_2
#define EXPECTED_3 expected_3

#define INPUT_TYPE int32_t
#define OUTPUT_TYPE float16_t
#define OUTPUT_TYPE_SIZE 16

#define SCALAR_OPERANDS
#define SCALAR_1 1
#define SCALAR_2 2
#define SCALAR_3 32

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
