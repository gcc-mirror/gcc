/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fno-tree-bit-ccp" } */

int foo_u64_1;
unsigned __int128 foo_u128_1;

void
foo (void)
{
  foo_u128_1 <<= 127;
  foo_u64_1 += __builtin_sub_overflow_p (0, (long) foo_u128_1, 0);
  foo_u128_1 =
    foo_u128_1 >> (foo_u128_1 & 127) | foo_u128_1 << (-foo_u128_1 & 127);
}
