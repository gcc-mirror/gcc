/* { dg-do run } */
/* { dg-options "-O3 -mavx -mno-avx2" } */
/* { dg-require-effective-target avx_runtime } */

#ifndef CHECK_H
#define CHECK_H "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include "sse2-cvt-1.c"
