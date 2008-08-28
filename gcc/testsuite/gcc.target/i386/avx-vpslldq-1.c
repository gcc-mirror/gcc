/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mfpmath=sse -mavx" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse2-pslldq-1.c"
