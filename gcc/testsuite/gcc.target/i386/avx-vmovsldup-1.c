/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -mfpmath=sse" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse3-movsldup.c"
