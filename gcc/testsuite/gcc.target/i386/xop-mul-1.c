/* { dg-do run } */
/* { dg-require-effective-target xop } */
/* { dg-options "-O3 -mxop" } */

#ifndef CHECK_H
#define CHECK_H "xop-check.h"
#endif

#ifndef TEST
#define TEST xop_test
#endif

#include "sse2-mul-1.c"
