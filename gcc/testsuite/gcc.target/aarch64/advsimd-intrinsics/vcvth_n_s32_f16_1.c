/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

#include <arm_fp16.h>

/* Input values.  */
float16_t input[] =
{
  0.0, -0.0,
  123.4, -567.8,
  -34.8, 1024,
  663.1, 169.1,
  -4.8, 77.0,
  -144.5, -56.8,

  (float16_t) -16, (float16_t) -15,
  (float16_t) -14, (float16_t) -13,
};

/* Expected results (32-bit hexadecimal representation).  */
uint32_t expected_1[] =
{
  0x00000000,
  0x00000000,
  0x000000f6,
  0xfffffb90,
  0xffffffbb,
  0x00000800,
  0x0000052e,
  0x00000152,
  0xfffffff7,
  0x0000009a,
  0xfffffedf,
  0xffffff8f,
  0xffffffe0,
  0xffffffe2,
  0xffffffe4,
  0xffffffe6,
};

uint32_t expected_2[] =
{
  0x00000000,
  0x00000000,
  0x000001ed,
  0xfffff720,
  0xffffff75,
  0x00001000,
  0x00000a5c,
  0x000002a4,
  0xffffffed,
  0x00000134,
  0xfffffdbe,
  0xffffff1d,
  0xffffffc0,
  0xffffffc4,
  0xffffffc8,
  0xffffffcc,
};

uint32_t expected_3[] =
{
  0x00000000,
  0x00000000,
  0x7fffffff,
  0x80000000,
  0x80000000,
  0x7fffffff,
  0x7fffffff,
  0x7fffffff,
  0x80000000,
  0x7fffffff,
  0x80000000,
  0x80000000,
  0x80000000,
  0x80000000,
  0x80000000,
  0x80000000,
};

#define TEST_MSG "VCVTH_N_S32_F16"
#define INSN_NAME vcvth_n_s32_f16

#define INPUT input
#define EXPECTED_1 expected_1
#define EXPECTED_2 expected_2
#define EXPECTED_3 expected_3

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE uint32_t
#define OUTPUT_TYPE_SIZE 32

#define SCALAR_OPERANDS
#define SCALAR_1 1
#define SCALAR_2 2
#define SCALAR_3 32

/* Include the template for unary scalar operations.  */
#include "unary_scalar_op.inc"
