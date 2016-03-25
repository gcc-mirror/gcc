/* { dg-do compile } */
/* { dg-options "-Og -freorder-functions -g3 -mcmodel=large" } */

typedef short v32u16 __attribute__ ((vector_size (32)));
typedef int v32u32 __attribute__ ((vector_size (32)));
typedef long v32u64 __attribute__ ((vector_size (32)));
typedef __int128 u128;
typedef __int128 v32u128 __attribute__ ((vector_size (32)));

int
foo (int u16_0, int u32_0, int u64_0, u128 u128_0, int u16_1, int u32_1, v32u16 v32u16_0, v32u32 v32u32_0, v32u64 v32u64_0, v32u128 v32u128_0, v32u16 v32u16_1, v32u32 v32u32_1, v32u64 v32u64_1, v32u128 v32u128_1)
{
  u128_0 <<= 0x6c;
  v32u16_1 %= (v32u16) { 1, 64, 0xf294, 0, u32_1, v32u32_1[6], ~u128_0, 0x2912, v32u32_0[2]} | 1;
  v32u16_0 ^= (v32u16){-v32u16_1[11], -u32_1, 64, ~u128_0, 0, 1, 64, ~u64_0, 0};
  return u16_0 + u32_0 + u16_1 + v32u16_0[0] + v32u32_0[1] + v32u32_0[2] + v32u32_0[4] + v32u32_0[6] + v32u64_0[0] + v32u64_0[1] + v32u64_0[2] + v32u64_0[3] + v32u128_0[0] + v32u128_0[1] + v32u16_1[0] + v32u32_1[7] + v32u64_1[0] + v32u64_1[1] + v32u64_1[2] + v32u64_1[3] + v32u128_1[0] + v32u128_1[1];
}
