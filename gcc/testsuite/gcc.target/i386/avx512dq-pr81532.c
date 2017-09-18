/* PR target/81532 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -mavx512dq -mavx512vl -mno-avx512bw" } */

typedef unsigned __int128 V __attribute__ ((vector_size (64)));

V
foo (V c)
{
  c >>= 0 != c;
  return c;
}
