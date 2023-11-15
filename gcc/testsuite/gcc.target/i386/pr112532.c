/* { dg-do compile } */
/* { dg-options "-msse4 -O2" } */

typedef char __attribute__((__vector_size__(2))) v16u8;
typedef int __attribute__((__vector_size__(8))) v64u8;
typedef unsigned short __attribute__((__vector_size__(2))) v16u16;
typedef unsigned short __attribute__((__vector_size__(8))) v64u16;
v64u16 foo0_v64u16_0;
int __attribute__((__vector_size__(4 * sizeof(int)))) foo0_v128u32_0;
__attribute__((__vector_size__(8 * sizeof(short)))) unsigned short foo0_v128u16_0;
v16u16 foo0_v16u16_0;
v16u8 foo0() {
  v16u16 v16u16_1 = __builtin_shufflevector(__builtin_shufflevector(__builtin_convertvector(foo0_v128u32_0, v64u16),foo0_v16u16_0, 1, 4, 2, 0, 0, 2, 2, 2),foo0_v16u16_0, 7);
  foo0_v64u16_0 -= (short)v16u16_1;
  v64u16 v64u16_3 = __builtin_shufflevector(v16u16_1, __builtin_shufflevector((v16u16){}, foo0_v128u16_0, 7, 0), 0, 1, 2, 2);
  return (union {v16u8 b;})
    {((union {
      v64u8 a;
      int b;
    })(v64u8)v64u16_3).b}.b + (v16u8)v16u16_1;
}
