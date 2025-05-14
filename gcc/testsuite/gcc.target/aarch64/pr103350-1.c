/* { dg-do run { target le } } */
/* { dg-additional-options "-Os -fno-tree-ter -save-temps -fdump-rtl-ree-all -free -std=c99 -w -fno-caller-saves" } */

typedef unsigned char u8;
typedef unsigned char __attribute__((__vector_size__ (8))) v64u8;
typedef unsigned char __attribute__((__vector_size__ (16))) v128u8;
typedef unsigned char __attribute__((__vector_size__ (32))) v256u8;
typedef unsigned short __attribute__((__vector_size__ (8))) v64u16;
typedef unsigned int __attribute__((__vector_size__ (16))) v128u32;
typedef unsigned long long u64;
typedef unsigned __int128 u128;

v64u16 foo0_v32u16_0 = { 4, 5, 6, 7 };
u64 foo0_u64_0 = 0x30;

__attribute__((__noipa__))
v64u8 foo0 (v64u16 v64u16_0, u128 u128_0)
{
  /* 03 00 05 00 03 00 02 00 ... */
  v256u8 v256u16_1 = (v256u8)__builtin_shufflevector (v64u16_0, foo0_v32u16_0,
			     3, 5, 3, 2, 3, 5, 1, 0,
			     3, 5, 3, 2, 2, 0, 2, 0);
  /* 00 00 00 00 01 00 00 00 ... */
  v128u8 v128u8_1 = (v128u8) __builtin_convertvector (v64u16_0, v128u32);
  /* 10 */
  u8 u8_1 = foo0_u64_0 % u128_0;
  /* 03 00 05 00 04 00 02 00 ... */
  v128u8 v128u8_r = ((union {v256u8 a; v128u8 b[2];}) v256u16_1).b[0] + v128u8_1;
  /* 00 00 01 00 02 00 03 00 */
  v64u8 v64u8_0 = (v64u8)v64u16_0;
  /* 03 00 06 00 06 00 05 00 */
  v64u8 v64u8_r = ((union {v128u8 a; v64u8 b[2];}) v128u8_r).b[0] + v64u8_0;
  /* 13 10 16 10 16 10 15 10 */
  return v64u8_r + u8_1;
}

int
main (void)
{
  v64u8 x = foo0 ((v64u16){ 0, 1, 2, 3 }, 0x20);
  v64u8 exp = { 0x13, 0x10, 0x16, 0x10, 0x16, 0x10, 0x15, 0x10 };
  for (unsigned i = 0; i < sizeof(x); i++)
    if (x[i] != exp[i])
      __builtin_abort();
  return 0;
}

/* { dg-final { scan-rtl-dump {because some vector uses aren't extension} ree } } */
