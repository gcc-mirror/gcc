/* PR tree-optimization/109011 */
/* { dg-do compile } */
/* { dg-additional-options "-O3 -fno-unroll-loops --param=vect-epilogues-nomask=0 -fdump-tree-optimized" } */
/* { dg-additional-options "-mno-avx512cd -mbmi -mlzcnt -mavx512vpopcntdq" { target { { { { i?86-*-* x86_64-*-* } && avx512vpopcntdq } && lzcnt } && bmi } } } */
/* { dg-additional-options "-mdejagnu-cpu=power8 -mvsx" { target powerpc_vsx_ok } } */

void
foo (long long *p, long long *q)
{
#pragma omp simd
  for (int i = 0; i < 2048; ++i)
    p[i] = 2 * q[i] + __builtin_ctzll (q[i]);
}

void
bar (long long *p, long long *q)
{
#pragma omp simd
  for (int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctzll (q[i]) : __SIZEOF_LONG_LONG__ * __CHAR_BIT__;
}

void
baz (long long *p, long long *q)
{
#pragma omp simd
  for (int i = 0; i < 2048; ++i)
    p[i] = __builtin_ffsll (q[i]);
}

/* { dg-final { scan-tree-dump-times " = \.POPCOUNT \\\(vect" 3 "optimized" { target { { { { i?86-*-* x86_64-*-* } && avx512vpopcntdq } && lzcnt } && bmi } } } } */
/* { dg-final { scan-tree-dump-times " = \.CLZ \\\(vect" 3 "optimized" { target powerpc_vsx_ok } } } */
