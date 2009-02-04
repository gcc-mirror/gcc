/* { dg-do run } */
/* { dg-require-effective-target vpclmul } */
/* { dg-options "-O2 -mpclmul -mavx" } */

#define CHECK_H "pclmul-avx-check.h"
#define TEST pclmul_avx_test

#include "pclmulqdq.c"
