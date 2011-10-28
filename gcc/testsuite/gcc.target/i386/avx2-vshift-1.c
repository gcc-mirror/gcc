/* { dg-do run } */
/* { dg-options "-O3 -mavx2" } */
/* { dg-require-effective-target avx2 } */

#ifndef CHECK_H
#define CHECK_H "avx2-check.h"
#endif

#ifndef TEST
#define TEST avx2_test
#endif

#include "xop-vshift-1.c"
