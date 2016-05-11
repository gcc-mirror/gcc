/* { dg-do compile } */
/* { dg-options "-O2 -fno-dce -fschedule-insns -fno-tree-vrp -fcompare-debug" } */
/* { dg-additional-options "-Wno-psabi -mno-sse" { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target scheduling } */

typedef unsigned short u16;
typedef short v16u16 __attribute__ ((vector_size (16)));
typedef unsigned v16u32 __attribute__ ((vector_size (16)));
typedef unsigned long long v16u64 __attribute__ ((vector_size (16)));

u16
foo(u16 u16_1, v16u16 v16u16_0, v16u32 v16u64_0, v16u16 v16u16_1, v16u32 v16u32_1, v16u64 v16u64_1)
{
  v16u64_1 /= (v16u64){~v16u32_1[1]};
  u16_1 = 0;
  u16_1 /= v16u32_1[2];
  v16u64_1 -= (v16u64) v16u16_1;
  u16_1 >>= 1;
  u16_1 -= ~0;
  v16u16_1 /= (v16u16){~u16_1, 1 - v16u64_0[0], 0xffb6};
  return u16_1 + v16u16_0[1] + v16u16_1[3] + v16u64_1[0] + v16u64_1[1];
}
