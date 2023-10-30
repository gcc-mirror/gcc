/* { dg-do compile { target { dfp && { ! ia32 } } } } */
/* { dg-options "-O2 -mavx512bw -fno-tree-ter -Wno-div-by-zero" } */

typedef int __attribute__((__vector_size__ (8))) v64u8;
typedef char __attribute__((__vector_size__ (16))) v128u8;
typedef int __attribute__((__vector_size__ (16))) v128u32;
typedef int __attribute__((__vector_size__ (32))) v256u8;
typedef int __attribute__((__vector_size__ (64))) v512u8;
typedef short __attribute__((__vector_size__ (4))) v32s16;
typedef short __attribute__((__vector_size__ (16))) v128s16;
typedef short __attribute__((__vector_size__ (32))) v256s16;
typedef _Float16 __attribute__((__vector_size__ (16))) f16;
typedef _Float32 f32;
typedef double __attribute__((__vector_size__ (64))) v512f64;
typedef _Decimal32 d32;
typedef _Decimal64 __attribute__((__vector_size__ (32))) v256d64;
typedef _Decimal64 __attribute__((__vector_size__ (64))) v512d64;
d32 foo0_d32_0, foo0_ret;
v256d64 foo0_v256d64_0;
v128s16 foo0_v128s16_0;
int foo0_v256d128_0;

extern void bar(int);

void
foo (v64u8, v128u8 v128u8_0, v128u8 v128s8_0,
     v256u8 v256u8_0, int v256s8_0, v512u8 v512u8_0, int v512s8_0,
     v256s16 v256s16_0,
     v512u8 v512s16_0,
     v128u32 v128u64_0,
     v128u32 v128s64_0,
     int, int, __int128 v128u128_0, __int128 v128s128_0, v128u32 v128f64_0)
{
  v512d64 v512d64_0;
  v256u8 v256f32_0, v256d64_1 = foo0_v256d64_0 == foo0_d32_0;
  f32 f32_0;
  f16 v128f16_0;
  f32_0 /= 0;
  v128u8 v128u8_1 = v128u8_0 != 0;
  int v256d32_1;
  v256f32_0 /= 0;
  v32s16 v32s16_1 = __builtin_shufflevector ((v128s16) { }, v256s16_0, 5, 10);
  v512f64 v512f64_1 = __builtin_convertvector (v512d64_0, v512f64);
  v512u8 v512d128_1 = v512s16_0;
  v128s16 v128s16_2 =
    __builtin_shufflevector ((v32s16) { }, v32s16_1, 0, 3, 2, 1,
			     0, 0, 0, 3), v128s16_3 = foo0_v128s16_0 > 0;
  v128f16_0 /= 0;
  __int128 v128s128_1 = 0 == v128s128_0;
  v512u8 v512u8_r = v512u8_0 + v512s8_0 + (v512u8) v512f64_1 + v512s16_0;
  v256u8 v256u8_r = ((union {
		      v512u8 a;
		      v256u8 b;}) v512u8_r).b +
    v256u8_0 + v256s8_0 + v256f32_0 + v256d32_1 +
    (v256u8) v256d64_1 + foo0_v256d128_0;
  v128u8 v128u8_r = ((union {
		      v256u8 a;
		      v128u8 b;}) v256u8_r).b +
    v128u8_0 + v128u8_1 + v128s8_0 + (v128u8) v128s16_2 +
    (v128u8) v128s16_3 + (v128u8) v128u64_0 + (v128u8) v128s64_0 +
    (v128u8) v128u128_0 + (v128u8) v128s128_1 +
    (v128u8) v128f16_0 + (v128u8) v128f64_0;
  bar (f32_0 + (int) foo0_d32_0);
  foo0_ret = ((union {
	       v64u8 a;
	       int b;}) ((union {
			  v128u8 a;
			  v64u8 b;}) v128u8_r).b).b;
}
