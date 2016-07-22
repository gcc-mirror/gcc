/* PR target/71559 */
/* { dg-do run { target avx } } */
/* { dg-options "-O2 -ftree-vectorize -mavx" } */

#include "avx-check.h"
#define PR71559_TEST avx_test

#include "sse2-pr71559.c"
