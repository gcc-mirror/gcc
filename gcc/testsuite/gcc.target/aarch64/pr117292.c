/* { dg-do compile } */
/* { dg-options "-Os" } */

#pragma GCC target "+nosve"

typedef char v8u8;
typedef __attribute__((__vector_size__ (2))) char v16u8;
typedef __attribute__((__vector_size__ (4))) char v32u8;
typedef __attribute__((__vector_size__ (8))) char v64u8;
typedef short v128u8;
typedef __attribute__((__vector_size__ (32))) char v256u8;
typedef __attribute__((__vector_size__ (64))) char v512u8;
v16u8 foo0_v16s16_0;
__attribute__((__vector_size__ (16))) int foo0_v512u32_0;
v8u8 foo0_ret;

static __attribute__((__noinline__)) __attribute__((__noclone__)) void
foo0 (signed char s8_0, v512u8 v512u16_0, v512u8 v512s16_0,
	     v512u8 v512s32_0, v512u8 v512u64_0, v512u8 v512s64_0,
	     v512u8 v512u128_0, v512u8 v512s128_0)
{
  char v8s8_0;
  v8s8_0 ^= s8_0;
  foo0_v512u32_0 /= foo0_v512u32_0 ^ s8_0;
  v512u8 v512u8_r = v512u16_0 + v512s16_0 + v512s32_0 +
    v512u64_0 + v512s64_0 + v512u128_0 + v512s128_0;
  v16u8 v16u8_r = ((union { v64u8 a; v16u8 b;})
		  ((union { v128u8 a; v64u8 b;})
		  ((union { v256u8 a; v128u8 b;})
		  ((union { v512u8 a; v256u8 b;})  v512u8_r).b).b).b).b +
    foo0_v16s16_0;
  v8u8 v8u8_r = ((union { v16u8 a; v8u8 b;}) v16u8_r).b + v8s8_0;
  foo0_ret = v8u8_r;
}

void
main ()
{
  foo0 (3, (v512u8){}, (v512u8){}, (v512u8){}, (v512u8){},
        (v512u8){}, (v512u8){}, (v512u8){});
}
