/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -mtune=core2 -msse4.1" } */

#define CHECK_H "sse4_1-check.h"
#define TEST sse4_1_test

#include "set-v16qi-2.h"
