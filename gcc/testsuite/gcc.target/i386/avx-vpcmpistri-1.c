/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse4_2-pcmpistri-1.c"
