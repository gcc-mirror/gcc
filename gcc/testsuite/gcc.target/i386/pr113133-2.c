/* PR target/113133 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -fno-tree-ter -mavx512f -mtune=barcelona" } */

typedef char v8u8;
typedef unsigned char __attribute__((__vector_size__(2))) v16u8;
typedef signed char __attribute__((__vector_size__(2))) v16s8;
typedef char __attribute__((__vector_size__(4))) v32u8;
typedef unsigned char __attribute__((__vector_size__(8))) v64u8;
typedef char __attribute__((__vector_size__(16))) v128u8;
typedef signed char __attribute__((__vector_size__(16))) v128s8;
typedef short __attribute__((__vector_size__(8))) v64u16;
typedef int __attribute__((__vector_size__(16))) v128u32;
typedef _Float16 __attribute__((__vector_size__(8))) v64f16;
typedef _Float32 f32;
char foo0_u8_0, foo0_ret;
v16s8 foo0_v16s8_0;
v64u8 foo0_v64u8_0;
v128u8 foo0_v128u8_0;
v128s8 foo0_v128s8_0;
__attribute__((__vector_size__(2 * sizeof(int)))) int foo0_v64s32_0;
v128u32 foo0_v128u32_0, foo0_v128f32_0;
f32 foo0_f32_0, foo0_f128_0;
v16u8 foo0_v16u8_0;
v64u16 foo0_v64u16_1;
void foo0(__attribute__((__vector_size__(4 * sizeof(int)))) int v128s32_0,
          __attribute__((__vector_size__(sizeof(long)))) long v64s64_0,
          __attribute__((__vector_size__(2 * sizeof(long)))) long v128u64_0,
          __attribute__((__vector_size__(2 * sizeof(long)))) long v128s64_0,
          _Float16 f16_0) {
  v64f16 v64f16_1 = __builtin_convertvector(foo0_v128f32_0, v64f16);
  v128u32 v128u32_1 = 0 != foo0_v128u32_0;
  v16s8 v16s8_1 = __builtin_shufflevector(
      __builtin_convertvector(foo0_v128s8_0, v128s8), foo0_v16s8_0, 2, 3);
  v128u8 v128u8_1 = foo0_v128u8_0;
  v64f16 v64f16_2 = __builtin_convertvector(v128s32_0, v64f16);
  __attribute__((__vector_size__(2 * sizeof(int)))) int v64u32_1 =
      -foo0_v64s32_0;
  __attribute__((__vector_size__(4))) signed char v32s8_1 =
      __builtin_shufflevector((v16s8){}, v16s8_1, 2, 2, 3, 0);
  v64u16 v64u16_2 = foo0_v64u16_1 ^ foo0_u8_0;
  v64u8 v64u8_1 = __builtin_shufflevector(foo0_v64u8_0, foo0_v16u8_0, 6, 7, 4,
                                          7, 0, 2, 6, 0);
  foo0_f32_0 *= __builtin_asinh(foo0_f128_0);
  v128u8 v128u8_r = foo0_v128u8_0 + v128u8_1 + foo0_v128s8_0 +
                    (v128u8)foo0_v128u32_0 + (v128u8)v128u32_1 +
                    (v128u8)v128s32_0 + (v128u8)v128u64_0 + (v128u8)v128s64_0 +
                    (v128u8)foo0_v128f32_0;
  v64u8 v64u8_r = ((union {
                    v128u8 a;
                    v64u8 b;
                  })v128u8_r)
                      .b +
                  foo0_v64u8_0 + v64u8_1 + (v64u8)v64u16_2 + (v64u8)v64u32_1 +
                  (v64u8)v64s64_0 + (v64u8)v64f16_1 + (v64u8)v64f16_2;
  v32u8 v32u8_r = ((union {
                    v64u8 a;
                    v32u8 b;
                  })v64u8_r)
                      .b +
                  v32s8_1;
  foo0_ret = ((union {
                v16u8 a;
                v8u8 b;
              })((union {
                v32u8 a;
                v16u8 b;
              })v32u8_r)
                  .b)
                 .b +
             f16_0;
}
