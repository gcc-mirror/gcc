/* { dg-do compile { target int128 } } */
/* { dg-options "-O -fno-sched-critical-path-heuristic -fschedule-insns -mavx -m8bit-idiv" } */

typedef int v8si __attribute__ ((vector_size (32)));
typedef __int128 i128;

i128
foo(int u16_0, int u64_0, i128 u128_0, i128 u128_1, v8si v32u32_0, v8si v32u32_1, v8si v32u64_1)
{
  v32u32_0[6] <<= u128_1 & 31;
  v32u32_0 &= (v8si){v32u64_1[2], v32u32_1[6], 0xc5a661b, 0, 2};
  u128_1 += 0x16fe7853d732;
  v32u32_1 /= (v8si){v32u32_0[5], u128_1, 0x92d} | 1;
  return u128_0 + v32u32_1[1];
}
