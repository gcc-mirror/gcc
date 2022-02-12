/* { dg-do run { target int128 } } */
/* { dg-options "-fharden-compares -fno-tree-dce -fno-tree-fre" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef unsigned char u8;
typedef unsigned char __attribute__((__vector_size__ (32))) v256u8;
typedef unsigned short __attribute__((__vector_size__ (32))) v256u16;
typedef unsigned short __attribute__((__vector_size__ (64))) v512u16;
typedef unsigned int u32;
typedef unsigned int __attribute__((__vector_size__ (4))) v512u32;
typedef unsigned long long __attribute__((__vector_size__ (32))) v256u64;
typedef unsigned long long __attribute__((__vector_size__ (64))) v512u64;
typedef unsigned __int128 __attribute__((__vector_size__ (32))) v256u128;
typedef unsigned __int128 __attribute__((__vector_size__ (64))) v512u128;

v512u16 g;

void
foo0 (u8 u8_0, v256u16 v256u16_0, v512u16 v512u16_0, u32 u32_0, v512u32,
      v256u64 v256u64_0, v512u64 v512u64_0, v256u128 v256u128_0,
      v512u128 v512u128_0)
{
  u32_0 <= (v512u128) (v512u128_0 != u8_0);
  v512u64 v512u64_1 =
    __builtin_shufflevector (v256u64_0, v512u64_0, 7, 8, 0, 9, 5, 0, 3, 1);
  g = v512u16_0;
  (v256u8) v256u16_0 + (v256u8) v256u128_0;
}

int
main (void)
{
  foo0 (40, (v256u16)
	{
	}, (v512u16)
	{
	}, 0, (v512u32)
	{
	}, (v256u64)
	{
	}, (v512u64)
	{
	}, (v256u128)
	{
	}, (v512u128)
	{
	});
}
