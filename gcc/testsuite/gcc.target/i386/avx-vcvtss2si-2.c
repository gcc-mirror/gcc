/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse-cvtss2si-2.c"
