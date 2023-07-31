/* PR target/110843 */
/* derived from gcc.target/i386/pr70007.c */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Os -mavx512ifma -Wno-psabi" } */

typedef unsigned short v32u16 __attribute__ ((vector_size (32)));
typedef unsigned long long v32u64 __attribute__ ((vector_size (32)));
typedef unsigned __int128 u128;
typedef unsigned __int128 v32u128 __attribute__ ((vector_size (32)));

u128 foo (v32u16 v32u16_0, v32u64 v32u64_0, v32u64 v32u64_1)
{
  do {
    v32u16_0[13] |= v32u64_1[3] = (v32u64_1[3] >> 19) | (v32u64_1[3] << 45);
    v32u64_1 %= ~v32u64_1;
    v32u64_0 *= (v32u64) v32u16_0;
  } while (v32u64_0[0]);
  return v32u64_1[3];
}

