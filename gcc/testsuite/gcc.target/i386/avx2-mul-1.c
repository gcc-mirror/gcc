/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O3 -mavx2" } */

#ifndef CHECK_H
#define CHECK_H "avx2-check.h"
#endif

#ifndef TEST
#define TEST avx2_test
#endif

#include "sse2-mul-1.c"
