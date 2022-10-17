/* PR target/103900 */
/* { dg-do compile } */
/* { dg-options "-O -fno-tree-dce -fno-tree-dse" } */

typedef unsigned char __attribute__((__vector_size__(2))) T;
typedef unsigned char __attribute__((__vector_size__(32))) U;
typedef int __attribute__((__vector_size__(64))) V;
typedef int __attribute__((__vector_size__(32))) W;
T foo0_v128u8_0;
U foo0_v256u8_0;
T foo0_v16u16_0;
int foo0_v128u64_0, foo0_v512u64_0;

void
foo0() {
  V v512u128_0;
  T v16u8_0;
  foo0_v128u64_0 += (short)v16u8_0;
  T v16u8_1 = ~__builtin_shufflevector(foo0_v128u8_0, foo0_v256u8_0, 0, 5);
  W v256u128_1;
  V v512u8_r =
      foo0_v512u64_0 + v512u128_0;
  (union {U b;}){}.b + (U)v256u128_1;
  T v16u8_r = v16u8_0 + v16u8_1 + foo0_v16u16_0;
}
