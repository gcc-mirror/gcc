/* { dg-do run } */
/* { dg-options "-O2 -mavx -mno-avx2" } */
/* { dg-require-effective-target avx } */

#ifndef CHECK_H
#define CHECK_H "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include "sse2-psraq-1.c"
