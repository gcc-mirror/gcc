/* PR target/111609 : a widening shift-left by zero must not emit an
   unencodable "vshll #0"; it is a plain widening move (vmovl).  */
/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2 --save-temps" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

int16x8_t  f_s8  (int8x8_t a)   { return vshll_n_s8  (a, 0); }
/* { dg-final { scan-assembler-times {vmovl\.s8\t} 1 } } */
int32x4_t  f_s16 (int16x4_t a)  { return vshll_n_s16 (a, 0); }
/* { dg-final { scan-assembler-times {vmovl\.s16\t} 1 } } */
int64x2_t  f_s32 (int32x2_t a)  { return vshll_n_s32 (a, 0); }
/* { dg-final { scan-assembler-times {vmovl\.s32\t} 1 } } */
uint16x8_t f_u8  (uint8x8_t a)  { return vshll_n_u8  (a, 0); }
/* { dg-final { scan-assembler-times {vmovl\.u8\t} 1 } } */
uint32x4_t f_u16 (uint16x4_t a) { return vshll_n_u16 (a, 0); }
/* { dg-final { scan-assembler-times {vmovl\.u16\t} 1 } } */
uint64x2_t f_u32 (uint32x2_t a) { return vshll_n_u32 (a, 0); }
/* { dg-final { scan-assembler-times {vmovl\.u32\t} 1 } } */

int16x8_t  g_s8  (int8x8_t a)   { return vshll_n_s8  (a, 1); }
/* { dg-final { scan-assembler-times {vshll\.s8\tq[0-9]+, d[0-9]+, #1} 1 } } */

int16x8_t  h_s8  (int8x8_t a)    { return vshll_n_s8  (a, 8); }
/* { dg-final { scan-assembler-times {vshll\.i8\tq[0-9]+, d[0-9]+, #8} 1 } } */
uint32x4_t  h_u16  (uint16x4_t a) { return vshll_n_u16  (a, 16); }
/* { dg-final { scan-assembler-times {vshll\.i16\tq[0-9]+, d[0-9]+, #16} 1 } } */
int64x2_t  h_s32  (int32x2_t a)  { return vshll_n_s32  (a, 32); }
/* { dg-final { scan-assembler-times {vshll\.i32\tq[0-9]+, d[0-9]+, #32} 1 } } */

/* { dg-final { scan-assembler-not {vshll\.[su][0-9]+\t[^\n]*#0\n} } } */
