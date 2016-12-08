/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-march=skylake-avx512 -Og" } */

typedef unsigned __int128 u128;
typedef unsigned __int128 v64u128 __attribute__ ((vector_size (64)));

v64u128
foo (u128 u128_3, v64u128 v64u128_3, v64u128 v64u128_2, v64u128 v64u128_1,
     v64u128 v64u128_0)
{
  v64u128_0 <<= 1;
  v64u128_2 >>= 0 != v64u128_2;
  v64u128_3[v64u128_3[0]] &= 1;
  v64u128_3 = v64u128_3 & 1;
  v64u128_2 = v64u128_2 >> 1 | v64u128_2 << v64u128_1[0];
  v64u128_0[0] >>= 127;
  return u128_3 + v64u128_0 + v64u128_2 + v64u128_3;
}
