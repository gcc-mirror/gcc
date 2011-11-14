/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx" } */
/* { dg-require-effective-target avx } */
/* { dg-skip-if "no M_PI" { vxworks_kernel } } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse4_1-round-vec.c"
