/* { dg-do run } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -Wno-psabi -w" } */
/* { dg-additional-options "-mno-sse2" { target x86_64-*-* i?86-*-* } } */

#include <stdint.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef u64 __attribute__((__vector_size__ (16))) v128u64;
u16 foo0_u16_0;
v128u64 foo0_v64u32_0;
u64 foo0_u64_0;
v128u64 foo0_v128u64_2;

v128u64
foo0 (u8 u8_0, v128u64 v128u64_0)
{
  u32 u32_1 = u8_0 || (0, 0);
  foo0_v128u64_2 - u8_0;
  foo0_u16_0 |= foo0_u64_0 && u8_0 > foo0_u64_0 <= u32_1;
  v128u64 v128u64_4 = v128u64_0 >= u8_0;
  return v128u64_4 + foo0_v64u32_0;
}

int
main ()
{
  v128u64 x = foo0 (3, (v128u64) { 0, 12 });
  if (x[0] != 0) __builtin_abort();
  if (x[1] != 0xffffffffffffffff) __builtin_abort();
  return 0;
}
