/* { dg-do compile { target int128 } } */
/* { dg-options "-O -fno-forward-propagate -fno-split-wide-types -mavx " } */

typedef unsigned __int128 v32u128 __attribute__ ((vector_size (32)));

v32u128
foo (v32u128 v32u128_0)
{
  v32u128_0[0] *= v32u128_0[1];
  return v32u128_0;
}
