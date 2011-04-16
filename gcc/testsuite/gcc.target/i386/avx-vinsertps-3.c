/* { dg-do run { target ilp32 } } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mfpmath=sse -mavx -mtune=geode" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse4_1-insertps-3.c"
