/* { dg-do run { target avx_runtime } } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O -fno-forward-propagate -fno-split-wide-types -mavx" } */

typedef unsigned int u32;
typedef unsigned __int128 u128;
typedef unsigned __int128 v32u128 __attribute__ ((vector_size (32)));

u128 __attribute__ ((noinline, noclone))
foo (u32 u32_0, v32u128 v32u128_0)
{
  v32u128_0[0] >>= u32_0;
  v32u128_0 += (v32u128) {u32_0, 0};
  return u32_0 + v32u128_0[0] + v32u128_0[1];
}

int
main()
{
  u128 x = foo (1, (v32u128) {1, 4});
  if (x != 6)
    __builtin_abort ();
  return 0;
}
