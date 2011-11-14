/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -ftree-vectorize -mavx -mtune=generic" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse2-copysign-vec.c"
