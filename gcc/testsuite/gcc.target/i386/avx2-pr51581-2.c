/* PR tree-optimization/51581 */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -mavx2 -fno-vect-cost-model" } */
/* { dg-require-effective-target avx2 } */

#define CHECK_H "avx2-check.h"
#define TEST avx2_test

#include "avx-pr51581-2.c"
