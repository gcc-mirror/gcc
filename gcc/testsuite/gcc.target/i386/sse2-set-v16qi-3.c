/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#define CHECK_H "sse2-check.h"
#define TEST sse2_test

#include "set-v16qi-3.h"
