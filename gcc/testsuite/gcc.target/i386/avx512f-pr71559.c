/* PR target/71559 */
/* { dg-do run { target avx512f } } */
/* { dg-options "-O2 -ftree-vectorize -mavx512f" } */

#include "avx512f-check.h"
#undef TEST
#define PR71559_TEST avx512f_test

#include "sse2-pr71559.c"
