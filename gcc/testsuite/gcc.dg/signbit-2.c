/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

/* This test does not work when the truth type does not match vector type.  */
/* { dg-additional-options "-mno-avx512f" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-additional-options "-march=armv8-a" { target aarch64_sve } } */
/* { dg-additional-options "-maltivec" { target powerpc_altivec_ok } } */
/* { dg-skip-if "no fallback for MVE" { arm_mve } } */

#include <stdint.h>

void fun1(int32_t *x, int n)
{
    for (int i = 0; i < (n & -16); i++)
      x[i] = (-x[i]) >> 31;
}

void fun2(int32_t *x, int n)
{
    for (int i = 0; i < (n & -16); i++)
      x[i] = (-x[i]) >> 30;
}

/* Xfail amdgcn where vector truth type is not integer type.  */
/* { dg-final { scan-tree-dump {\s+>\s+\{ 0(, 0)+ \}} optimized { target vect_shift xfail amdgcn-*-* } } } */
/* { dg-final { scan-tree-dump {\s+>\s+0} optimized { target { ! vect_shift } } } } */
/* { dg-final { scan-tree-dump-not {\s+>>\s+31} optimized { xfail amdgcn-*-* } } } */
