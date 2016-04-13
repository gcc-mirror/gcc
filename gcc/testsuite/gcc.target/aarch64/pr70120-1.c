/* { dg-do compile } */
/* { dg-options "-Og -fschedule-insns -mno-pc-relative-literal-loads -g" } */

typedef short v32u16 __attribute__ ((vector_size (32)));
typedef int v32u32 __attribute__ ((vector_size (32)));
typedef long v32u64 __attribute__ ((vector_size (32)));
typedef __int128 u128;
typedef __int128 v32u128 __attribute__ ((vector_size (32)));

int
foo(int u16_0, int u32_0, int u64_0, u128 u128_0, int u16_1, int u32_1, int u64_1, u128 u128_1, v32u16 v32u16_0, v32u32 v32u32_0, v32u64 v32u64_0, v32u128 v32u128_0, v32u16 v32u16_1, v32u32 v32u32_1, v32u64 v32u64_1, v32u128 v32u128_1)
{
  v32u32_1 ^= (v32u32) ~ v32u64_0;
  v32u32_1 %= (v32u32) - v32u16_1 | 1;
  v32u16_1 -= (v32u16) v32u16_1;
  v32u64_0 *= (v32u64){~ u128_0, v32u16_1[5], v32u16_0[15], v32u32_1[4]};
  v32u16_0 /= (v32u16){0x574c, ~u128_1, v32u128_1[0], u64_1, v32u64_0[1], v32u64_1[2], 0, 0x8ce6, u128_1, 0x5e69} |1;
  return v32u16_0[0] + v32u16_0[6] + v32u16_0[8] + v32u16_0[9] + v32u32_0[0] + v32u32_0[1] + v32u32_0[2] + v32u32_0[3] + v32u32_0[4] + v32u32_0[6] + v32u64_0[0] + v32u64_0[2] + v32u64_0[3] + v32u128_0[0] + v32u128_0[1] + v32u32_1[0] + v32u32_1[2] + v32u64_1[2] + v32u64_1[3] + v32u128_1[1];
}
