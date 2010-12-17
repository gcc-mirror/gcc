/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -mavx -std=c99" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse-cmpss-1.c"
