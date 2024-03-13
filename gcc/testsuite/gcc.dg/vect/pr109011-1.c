/* PR tree-optimization/109011 */
/* { dg-do compile } */
/* { dg-additional-options "-O3 -fno-unroll-loops --param=vect-epilogues-nomask=0 -fdump-tree-optimized" } */
/* { dg-additional-options "-mavx512cd" { target { { i?86-*-* x86_64-*-* } && avx512cd } } } */
/* { dg-additional-options "-mavx512vpopcntdq" { target { { i?86-*-* x86_64-*-* } && avx512vpopcntdq } } } */
/* { dg-additional-options "-mvsx" { target { powerpc_vsx_ok && has_arch_pwr8 } } } */
/* { dg-additional-options "-mdejagnu-cpu=power8 -mvsx" { target { powerpc_vsx_ok && { ! has_arch_pwr8 } } } } */
/* { dg-additional-options "-march=z13 -mzarch" { target s390_vx } } */

void
foo (long long *p, long long *q)
{
#pragma omp simd
  for (int i = 0; i < 2048; ++i)
    p[i] = __builtin_popcountll (q[i]);
}

/* { dg-final { scan-tree-dump-times " = \.POPCOUNT \\\(vect" 1 "optimized" { target { { i?86-*-* x86_64-*-* } && avx512vpopcntdq } } } } */
/* { dg-final { scan-tree-dump-times " = \.POPCOUNT \\\(vect" 1 "optimized" { target { powerpc_vsx_ok || s390_vx } } } } */

void
bar (long long *p, long long *q)
{
#pragma omp simd
  for (int i = 0; i < 2048; ++i)
    p[i] = __builtin_clzll (q[i]);
}

/* { dg-final { scan-tree-dump-times " = \.CLZ \\\(vect" 1 "optimized" { target { { i?86-*-* x86_64-*-* } && avx512cd } } } } */
/* { dg-final { scan-tree-dump-times " = \.CLZ \\\(vect" 1 "optimized" { target { powerpc_vsx_ok || s390_vx } } } } */
