/* PR target/69549 */
/* { dg-do run { target sse2_runtime } } */
/* { dg-options "-O2 -msse2" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
typedef unsigned char v16u8 __attribute__ ((vector_size (16)));
typedef unsigned short v16u16 __attribute__ ((vector_size (16)));
typedef unsigned int v16u32 __attribute__ ((vector_size (16)));
typedef unsigned long long v16u64 __attribute__ ((vector_size (16)));

u64 __attribute__((noinline, noclone))
foo (u8 u8_0, u16 u16_3, v16u8 v16u8_0, v16u16 v16u16_0, v16u32 v16u32_0, v16u64 v16u64_0, v16u8 v16u8_1, v16u16 v16u16_1, v16u32 v16u32_1, v16u64 v16u64_1, v16u8 v16u8_2, v16u16 v16u16_2, v16u32 v16u32_2, v16u64 v16u64_2, v16u8 v16u8_3, v16u16 v16u16_3, v16u32 v16u32_3, v16u64 v16u64_3)
{
  v16u64_0 /= (v16u64){u16_3, ((0))} | 1;
  v16u64_1 += (v16u64)~v16u32_0;
  v16u16_1 /= (v16u16){-v16u64_3[1]} | 1;
  v16u64_3[1] -= 0x1fffffff;
  v16u32_2 /= (v16u32)-v16u64_0 | 1;
  v16u32_1 += ~v16u32_1;
  v16u16_3 %= (v16u16){0xfff, v16u32_2[3], v16u8_0[14]} | 1;
  v16u64_3 -= (v16u64)v16u32_2;
  if (v16u64_1[1] >= 1) {
    v16u64_0 %= (v16u64){v16u32_0[1]} | 1;
    v16u32_1[1] %= 0x5fb856;
    v16u64_1 |= -v16u64_0;
  }
  v16u8_0 *= (v16u8)v16u32_1;
  return u8_0 + v16u8_0 [12] + v16u8_0 [13] + v16u8_0 [14] + v16u8_0 [15] + v16u16_0 [0] + v16u16_0 [1] + v16u32_0 [0] + v16u32_0 [1] + v16u32_0 [2] + v16u32_0 [3] + v16u64_0 [0] + v16u64_0 [1] + v16u8_1 [9] + v16u8_1 [10] + v16u8_1 [11] + v16u8_1 [15] + v16u16_1 [0] + v16u16_1 [1] + v16u16_1 [3] + v16u64_1 [0] + v16u64_1 [1] + v16u8_2 [3] + v16u8_2 [4] + v16u8_2 [5] + v16u8_2 [0] + v16u32_2 [1] + v16u32_2 [2] + v16u32_2 [3] + v16u64_2 [0] + v16u64_2 [1] + v16u8_3 [0] + v16u16_3 [6] + v16u16_3[7] + v16u32_3[1] + v16u32_3[2] + v16u64_3[0] + v16u64_3[1];
}

int
main ()
{
  u64 x = foo(1, 1, (v16u8){1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, (v16u16){1, 1}, (v16u32){1}, (v16u64){1}, (v16u8){1}, (v16u16){1, 1}, (v16u32){1}, (v16u64){1}, (v16u8){1, 1, 1, 1, 1}, (v16u16){1}, (v16u32){1}, (v16u64){1}, (v16u8){1}, (v16u16){1}, (v16u32){1}, (v16u64){1});

  if (x != 0xffffffffe0000209)
    __builtin_abort();
  return 0;
}
