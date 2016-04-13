/* { dg-do run } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-options "-Os -w -fno-expensive-optimizations -fschedule-insns -mtpcs-leaf-frame -fira-algorithm=priority" } */


typedef unsigned short u16;
typedef unsigned short v16u16 __attribute__ ((vector_size (16)));
typedef unsigned int u32;
typedef unsigned int v16u32 __attribute__ ((vector_size (16)));
typedef unsigned long long u64;
typedef unsigned long long v16u64 __attribute__ ((vector_size (16)));

u64 __attribute__ ((noinline, noclone))
foo(u16 u16_0, u32 u32_0, u64 u64_0, u16 u16_1, u32 u32_1, u64 u64_1,
    v16u16 v16u16_0, v16u32 v16u32_0, v16u64 v16u64_0, v16u16 v16u16_1, v16u32 v16u32_1, v16u64 v16u64_1)
{
  v16u64_0 %= (v16u64){(u16) v16u16_0[5], ~v16u64_1[1]};
  v16u64_0[1] = 1;
  v16u32_1[3] >>= 31;
  v16u64_1 ^= (v16u64){v16u16_1[4], u64_1};
  v16u64_1[0] = (v16u64_1[0] >> 63) | (v16u64_1[0] << 1);
  u16_0 -= 1;
  v16u32_1 %= (v16u32)-v16u64_0 | 1;
  v16u16_0 /= (v16u16){-u64_1} | 1;
  v16u32_0[2] |= (u16)~u16_1;
    return u16_0 + u64_0 + u32_1 + u64_1 +
                v16u16_0[0] + v16u16_0[1] + v16u16_0[2] + v16u16_0[3] + v16u16_0[4] + v16u16_0[5] + v16u16_0[6] + v16u32_0[2] + v16u32_0[3] + v16u64_0[0] +
      v16u16_1[2] + v16u16_1[4] + v16u32_1[0] + v16u32_1[1] + v16u32_1[2] + v16u32_1[3] + v16u64_1[0] + v16u64_1[1];
}

int
main ()
{
  u64 x = foo(0, 0, 1, 0, 0, 1, (v16u16){-1, 0, 0, 0, 0, 1}, (v16u32){0}, (v16u64){0}, (v16u16){0}, (v16u32){0}, (v16u64){0x67784fdb22, 1});
  __builtin_printf ("%016llx\n", (unsigned long long) (x >> 0));
  if (x != 0x000000cef0a1b646)
    __builtin_abort();
  return 0;
}
