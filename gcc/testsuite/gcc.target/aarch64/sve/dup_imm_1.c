/* { dg-do compile } */
/* -fno-tree-loop-distribute-patterns prevents conversion to memset.  */
/* { dg-options "-O3 -fno-tree-loop-distribute-patterns" } */

#include <stdint.h>

#define NUM_ELEMS(TYPE) (1024 / sizeof (TYPE))

#define DEF_SET_IMM(TYPE, IMM, SUFFIX)		\
void __attribute__ ((noinline, noclone))	\
set_##TYPE##_##SUFFIX (TYPE *a)			\
{						\
  for (int i = 0; i < NUM_ELEMS (TYPE); i++)	\
    a[i] = IMM;					\
}

/* --- VALID --- */

DEF_SET_IMM (int8_t, 0, imm_0)
DEF_SET_IMM (int16_t, 0, imm_0)
DEF_SET_IMM (int32_t, 0, imm_0)
DEF_SET_IMM (int64_t, 0, imm_0)

DEF_SET_IMM (int8_t, -1, imm_m1)
DEF_SET_IMM (int16_t, -1, imm_m1)
DEF_SET_IMM (int32_t, -1, imm_m1)
DEF_SET_IMM (int64_t, -1, imm_m1)

DEF_SET_IMM (int8_t, 1, imm_1)
DEF_SET_IMM (int16_t, 1, imm_1)
DEF_SET_IMM (int32_t, 1, imm_1)
DEF_SET_IMM (int64_t, 1, imm_1)

DEF_SET_IMM (int8_t, 127, imm_127)
DEF_SET_IMM (int16_t, 127, imm_127)
DEF_SET_IMM (int32_t, 127, imm_127)
DEF_SET_IMM (int64_t, 127, imm_127)

DEF_SET_IMM (int8_t, -128, imm_m128)
DEF_SET_IMM (int16_t, -128, imm_m128)
DEF_SET_IMM (int32_t, -128, imm_m128)
DEF_SET_IMM (int64_t, -128, imm_m128)

// No uint8_t variant - size too large for a byte
DEF_SET_IMM (int16_t, 256, imm_256)
DEF_SET_IMM (int32_t, 256, imm_256)
DEF_SET_IMM (int64_t, 256, imm_256)

// No uint8_t variant - size too large for a byte
DEF_SET_IMM (int16_t, 32512, imm_32512)
DEF_SET_IMM (int32_t, 32512, imm_32512)
DEF_SET_IMM (int64_t, 32512, imm_32512)

// No uint8_t variant - size too large for a byte
DEF_SET_IMM (int16_t, -32768, imm_m32768)
DEF_SET_IMM (int32_t, -32768, imm_m32768)
DEF_SET_IMM (int64_t, -32768, imm_m32768)

/* gcc will generate:
     dup z0.b, 0x01
*/
DEF_SET_IMM (int16_t, 0x0101, imm_01_pat)
DEF_SET_IMM (int32_t, 0x01010101, imm_01_pat)
DEF_SET_IMM (int64_t, 0x0101010101010101LL, imm_01_pat)

/* gcc will generate:
     dup z0.h, 0x01
*/
DEF_SET_IMM (int32_t, 0x00010001, imm_0001_pat)
DEF_SET_IMM (int64_t, 0x0001000100010001LL, imm_0001_pat)

/* gcc will generate:
     dup z0.b, 0xFE (-2)
*/
DEF_SET_IMM (int16_t, 0xFEFE, imm_FE_pat)
DEF_SET_IMM (int32_t, 0xFEFEFEFE, imm_FE_pat)
DEF_SET_IMM (int64_t, 0xFEFEFEFEFEFEFEFE, imm_FE_pat)

/* gcc will generate:
     dup z0.h, 0xFFFE (-2)
*/
DEF_SET_IMM (int32_t, 0xFFFEFFFE, imm_FFFE_pat)
DEF_SET_IMM (int64_t, 0xFFFEFFFEFFFEFFFELL, imm_FFFE_pat)

/* gcc will generate:
     dup z0.h, 0xFE00
*/
DEF_SET_IMM (int32_t, 0xFE00FE00, imm_FE00_pat)
DEF_SET_IMM (int64_t, 0xFE00FE00FE00FE00LL, imm_FE00_pat)


/* --- INVALID --- */

// This shouldn't generate a dup as it's out of range, but also the compiler
// shouldn't assert!
DEF_SET_IMM (int32_t, 129, imm_m129)
DEF_SET_IMM (int32_t, 32513, imm_32513)
DEF_SET_IMM (int32_t, -32763, imm_m32763)

/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.b, #-1\n} } } */

/* { dg-final { scan-assembler {\tmov(?:i\td|\tz)([0-9]+)(?:\.[bhsd])?, #0\n} } } */

/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.b, #1\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.h, #1\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.s, #1\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.d, #1\n} } } */

/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.b, #127\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.h, #127\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.s, #127\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.d, #127\n} } } */

/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.b, #-128\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.h, #-128\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.s, #-128\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.d, #-128\n} } } */

/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.h, #256\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.s, #256\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.d, #256\n} } } */

/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.h, #32512\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.s, #32512\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.d, #32512\n} } } */

/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.h, #-32768\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.s, #-32768\n} } } */
/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.d, #-32768\n} } } */

/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.b, #-2\n} } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, #-2\n} 2 } } */

/* { dg-final { scan-assembler {\tmov\tz[0-9]+\.h, #-512\n} } } */

/* { dg-final { scan-assembler-not {#129\n} } } */
/* { dg-final { scan-assembler-not {#32513\n} } } */
/* { dg-final { scan-assembler-not {#-32763\n} } } */
