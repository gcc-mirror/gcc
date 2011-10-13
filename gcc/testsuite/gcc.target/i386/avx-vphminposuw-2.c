/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -mavx -mno-avx2" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse4_1-phminposuw-2.c"
