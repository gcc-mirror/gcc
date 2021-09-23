/* { dg-do run } */
/* { dg-options "-O2 -msse4.2 -mno-avx" } */
/* { dg-require-effective-target sse4 } */

#ifndef CHECK_H
#define CHECK_H "sse4_2-check.h"
#endif

#ifndef TEST
#define TEST sse4_2_test
#endif

#include "sse2-psraq-1.c"
