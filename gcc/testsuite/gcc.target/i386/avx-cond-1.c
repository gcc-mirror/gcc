/* { dg-do run } */
/* { dg-options "-O3 -mavx" } */
/* { dg-require-effective-target avx_runtime } */

#ifndef CHECK_H
#define CHECK_H "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include "sse4_1-cond-1.c"
