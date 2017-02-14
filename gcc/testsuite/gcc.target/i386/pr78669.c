/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-Os -fno-tree-ter -mavx512bw " } */
typedef __int128 u128;
typedef __int128 v64u128 __attribute__((vector_size(64)));

v64u128 v64u128_g;

static inline v64u128
baz(v64u128 v64u128_0, v64u128 v64u128_3)
{
  v64u128_0 |= (v64u128){} == v64u128_0;
  v64u128_3 = (v64u128){} >= v64u128_3;
  return v64u128_0 + v64u128_3;
}

static void __attribute__((noinline, noclone))
bar(u128 u128_0, v64u128 v64u128_3)
{
  v64u128_g = baz((v64u128){(u128)1 << 64, u128_0}, v64u128_3);
}

void
foo(v64u128 v64u128_3)
{
  bar(3, v64u128_3);
}
