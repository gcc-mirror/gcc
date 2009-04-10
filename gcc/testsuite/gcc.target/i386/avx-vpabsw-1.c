/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mfpmath=sse -mavx -fno-strict-aliasing" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "ssse3-pabsw.c"
