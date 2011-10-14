/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -mavx" } */

#ifndef CHECK_H
#define CHECK_H "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include "sse2-mul-1.c"
