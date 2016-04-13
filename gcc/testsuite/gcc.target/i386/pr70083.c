/* { dg-do compile } */
/* { dg-options "-Wno-psabi -O2 -fno-dce -fschedule-insns -fno-sched-critical-path-heuristic -mavx512dq --param=max-cse-insns=1" } */

typedef short v16hi __attribute__ ((vector_size (32)));
typedef int v8si __attribute__ ((vector_size (32)));
typedef long long v4di __attribute__ ((vector_size (32)));

int
foo(int u32_0, int u64_0, int u64_1, v16hi v32u16_0, v8si v32u32_0, v4di v32u64_0, v16hi v32u16_1, v8si v32u32_1, v4di v32u64_1)
{
  v32u32_1 %= (v8si) v32u16_1 | 1;
  v32u64_1[1] |= ((1));
  v32u16_0 /= (v16hi){~u64_1, 1, 0xb56c, 0xd279, 0x26b6, 0x74d9, 0xf764, 0, 0, -v32u16_1[6]} | 1;
  v32u16_1 ^= (v16hi){0xc98d, 1, 0x8c71, u32_0, 0x5366, 0, ~v32u64_1[1]} & 31;
  v32u32_0 -= (v8si)~v32u64_1;
  v32u32_1[2] |= 0x1f;
  v32u16_0 %= (v16hi){2, 0xffff, u32_0, 1, v32u64_0[1], u32_0 };
  v32u32_1 /= (v8si){0x1e7390, v32u16_0[12], ~v32u16_1[2], -u64_0};
  return v32u16_0[4] + v32u16_0[5] + v32u32_0[5] + v32u32_1[6] + v32u64_1[3];
}
