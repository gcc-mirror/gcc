/* { dg-do compile } */
/* { dg-additional-options "-mavx512vl -mavx512vpopcntdq" { target avx512vpopcntdq } } */

typedef unsigned uv4si __attribute__((vector_size(16)));

uv4si __attribute__((noinline))
vpopctf (uv4si a)
{
  uv4si r;
  r[2] = __builtin_popcount (a[2]);
  r[1] = __builtin_popcount (a[1]);
  r[0] = __builtin_popcount (a[0]);
  r[3] = __builtin_popcount (a[3]);
  return r;
}

/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" { target avx512vpopcntdq } } } */
