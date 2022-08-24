/* { dg-do assemble } */
/* { dg-options "-O3 --save-temps -fdump-tree-optimized" } */

/* This test does not work when the truth type does not match vector type.  */
/* { dg-additional-options "-mno-avx512f" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-additional-options "-march=armv8-a" { target aarch64_sve } } */
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

/* { dg-final { scan-tree-dump {\s+>\s+\{ 0(, 0)+ \}} optimized { target vect_int } } } */
/* { dg-final { scan-tree-dump {\s+>\s+0} optimized { target { ! vect_int } } } } */
/* { dg-final { scan-tree-dump-not {\s+>>\s+31} optimized } } */
