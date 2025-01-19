/* { dg-do compile } */

typedef __UINT8_TYPE__ u8;
typedef __UINT16_TYPE__ u16;
__extension__ typedef __uint24 u24;
typedef __UINT32_TYPE__ u32;

u32 sub_32_8  (u32 a, u8 b)  { return a - b; }
u32 sub_32_16 (u32 a, u16 b) { return a - b; }
u32 sub_32_24 (u32 a, u24 b) { return a - b; }

u24 sub_24_8  (u24 a, u8 b)  { return a - b; }
u24 sub_24_16 (u24 a, u16 b) { return a - b; }

u16 sub_16_8  (u16 a, u8 b)  { return a - b; }
