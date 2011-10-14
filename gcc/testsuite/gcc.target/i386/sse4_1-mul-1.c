/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O3 -msse4.1" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include "sse2-mul-1.c"
