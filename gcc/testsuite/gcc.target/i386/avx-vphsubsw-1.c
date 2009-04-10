/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -fno-strict-aliasing" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "ssse3-phsubsw.c"
