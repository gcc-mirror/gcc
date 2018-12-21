/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#define CHECK_H "sse4_1-check.h"
#define TEST sse4_1_test

#include "sse2-pr88547-2.c"
