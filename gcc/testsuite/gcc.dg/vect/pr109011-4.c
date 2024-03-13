/* PR tree-optimization/109011 */
/* { dg-do compile } */
/* { dg-additional-options "-O3 -fno-unroll-loops --param=vect-epilogues-nomask=0 -fdump-tree-optimized" } */
/* { dg-additional-options "-mavx512cd -mbmi -mlzcnt -mno-avx512vpopcntdq" { target { { { { i?86-*-* x86_64-*-* } && avx512cd } && lzcnt } && bmi } } } */
/* { dg-additional-options "-mvsx" { target { powerpc_vsx_ok && has_arch_pwr9 } } } */
/* { dg-additional-options "-mdejagnu-cpu=power9 -mvsx" { target { powerpc_vsx_ok && { ! has_arch_pwr9 } } } } */
/* { dg-additional-options "-march=z13 -mzarch" { target s390_vx } } */

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

/* { dg-final { scan-tree-dump-times " = \.CLZ \\\(vect" 3 "optimized" { target { { { { i?86-*-* x86_64-*-* } && avx512cd } && lzcnt } && bmi } } } } */
/* { dg-final { scan-tree-dump-times " = \.CTZ \\\(vect" 3 "optimized" { target powerpc_vsx_ok } } } */
/* { dg-final { scan-tree-dump-times " = \.CTZ \\\(vect" 2 "optimized" { target s390_vx } } } */
/* { dg-final { scan-tree-dump-times " = \.POPCOUNT \\\(vect" 1 "optimized" { target s390_vx } } } */
