/* { dg-do assemble { target { ! ia32 } } } */
/* { dg-require-effective-target avx512vbmi } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-Og -fschedule-insns -fno-tree-fre -mavx512vbmi --param=max-sched-ready-insns=1 -masm=intel" } */

typedef char v64u8 __attribute__((vector_size(64)));
typedef int v64u32 __attribute__((vector_size(64)));
typedef long v64u64 __attribute__((vector_size(64)));
typedef __int128 v64u128 __attribute__((vector_size(64)));

v64u128
foo(int u8_0, unsigned u128_0, v64u32 v64u32_1, v64u32 v64u32_0, v64u64 v64u64_0, v64u128 v64u128_0)
{
  v64u8 v64u8_0 = v64u8_0;
  v64u32_0 = v64u32_0 >> (v64u32){0, 0, 0, 1, 0, ((v64u64)v64u64_0)[u8_0], ((v64u32)v64u128_0)[15], 0, 0, 0, 0, 4, ((v64u64)v64u64_0)[v64u32_0[0]] - 1};
  v64u8_0 = v64u8_0 << ((v64u8)v64u32_1 & 1);
  v64u64_0[0] >>= 0;
  return u128_0 + (v64u128)v64u8_0 + (v64u128)v64u32_0 + (v64u128)v64u64_0;
}
