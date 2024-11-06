/* PR target/112943 */
/* { dg-do compile { target { dfp && { ! ia32 } } } } */
/* { dg-options "-O2 -march=westmere -mapxf" } */

typedef unsigned char __attribute__((__vector_size__(1))) v8u8;
typedef char __attribute__((__vector_size__(2))) v16u8;
typedef char __attribute__((__vector_size__(4))) v32u8;
typedef char __attribute__((__vector_size__(8))) v64u8;
typedef char __attribute__((__vector_size__(16))) v128u8;
typedef _Float16 __attribute__((__vector_size__(2))) v16f16;
typedef _Float16 __attribute__((__vector_size__(16))) v128f16;
typedef _Float64x __attribute__((__vector_size__(16))) v128f128;
typedef _Decimal64 d64;
char foo0_u8_0;
v8u8 foo0_v8u8_0;
__attribute__((__vector_size__(sizeof(char)))) char foo0_v8s8_0;
__attribute__((__vector_size__(sizeof(long long)))) unsigned long long v64u64_0;
_Float16 foo0_f16_0;
v128f16 foo0_v128f16_0;
double foo0_f64_0;
int foo0_f128_0, foo0_v32d32_0, foo0__0;
d64 foo0_d64_0;
v8u8 *foo0_ret;
unsigned __int128 foo0_u128_3;
v8u8 d;
void foo0() {
    v64u64_0 -= foo0_u8_0;
    v8u8 v8u8_1 = foo0_v8u8_0 % d;
    v128f128 v128f128_1 = __builtin_convertvector(v64u64_0, v128f128);
    __int128 u128_2 = ((9223372036854775807 + (__int128) 1) << 4) * foo0_u8_0,
	     u128_r = u128_2 + foo0_u128_3 + foo0_f128_0 + (__int128)foo0_d64_0;
    v16f16 v16f16_1 = __builtin_convertvector(foo0_v8s8_0, v16f16);
    v128f16 v128f16_1 = 0 > foo0_v128f16_0;
    v128u8 v128u8_r = (v128u8)v128f16_1 + (v128u8)v128f128_1;
    v64u8 v64u8_r = ((union {
		      v128u8 a;
		      v64u8 b;
		      })v128u8_r)
    .b +
      (v64u8)v64u64_0;
    v32u8 v32u8_r = ((union {
		      v64u8 a;
		      v32u8 b;
		      })v64u8_r)
    .b +
      (v32u8)foo0_v32d32_0;
    v16u8 v16u8_r = ((union {
		      v32u8 a;
		      v16u8 b;
		      })v32u8_r)
    .b +
      (v16u8)v16f16_1;
    v8u8 v8u8_r = ((union {
		    v16u8 a;
		    v8u8 b;
		    })v16u8_r)
    .b +
      foo0_v8u8_0 + v8u8_1 + foo0_v8s8_0;
    long long u64_r = u128_r + foo0_f64_0 + (unsigned long long)foo0__0;
    short u16_r = u64_r + foo0_f16_0;
    char u8_r = u16_r + foo0_u8_0;
    *foo0_ret = v8u8_r + u8_r;
}
