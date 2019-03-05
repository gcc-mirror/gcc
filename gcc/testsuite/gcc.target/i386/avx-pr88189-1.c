/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -mfpmath=sse" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse4_1-pr88189-1.c"
