/* { dg-do compile { target int128 } }  */
/* { dg-options "-Os -fno-tree-dominator-opts -fno-tree-vrp" } */

int bar0_u8_0, bar0_u16_0, bar0_u32_0, bar0_u16_1, bar0_u32_1;
unsigned __int128 bar0_u128_0;

int
bar0() {
  bar0_u16_1 *=
      __builtin_add_overflow_p(bar0_u16_0, bar0_u32_1, (long)bar0_u8_0);
  bar0_u128_0 = bar0_u128_0 >> bar0_u16_1 | bar0_u128_0 << (-bar0_u16_1 & 127);
  bar0_u128_0 += __builtin_mul_overflow_p(bar0_u32_0, 20, 0);
}
