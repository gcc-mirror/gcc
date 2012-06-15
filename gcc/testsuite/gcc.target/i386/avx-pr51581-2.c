/* PR tree-optimization/51581 */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -mavx -fno-vect-cost-model" } */
/* { dg-require-effective-target avx } */

#ifndef CHECK_H
#define CHECK_H "avx-check.h"
#endif
#ifndef TEST
#define TEST avx_test
#endif

#define main main1
#include "../../gcc.c-torture/execute/pr51581-2.c"
#undef main

#include CHECK_H

static void
TEST (void)
{
  main1 ();
}
