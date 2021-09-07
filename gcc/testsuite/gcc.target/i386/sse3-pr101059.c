/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -msse3" } */
/* { dg-require-effective-target sse3 } */

#ifndef CHECK_H
#define CHECK_H "sse3-check.h"
#endif

#ifndef TEST
#define TEST sse3_test
#endif

#include "sse2-pr101059.c"
