/* PR target/102224 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx } */

#ifndef CHECK_H
#define CHECK_H "avx-check.h"
#endif
#ifndef TEST
#define TEST avx_test
#endif

#define main main1
#include "../../gcc.dg/pr102224.c"
#undef main

#include CHECK_H

static void
TEST (void)
{
  main1 ();
}
