/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_fp16.h>

uint16_t expected[] = { 0x0, 0x0, 0x0, 0x0, 0x0, 0xFFFF, 0xFFFF, 0x0, 0xFFFF,
			0x0, 0x0, 0x0, 0x0, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
			0xFFFF};

#define TEST_MSG "VCAGEH_F16"
#define INSN_NAME vcageh_f16

#define EXPECTED expected

#define INPUT_TYPE float16_t
#define OUTPUT_TYPE uint16_t
#define OUTPUT_TYPE_SIZE 16

/* Include the template for binary scalar operations.  */
#include "binary_scalar_op.inc"
