/* PR target/88513 */
/* { dg-do run } */
/* { dg-options "-O2 -fopenmp-simd -mavx512f -mtune=intel -mprefer-vector-width=512 -fno-vect-cost-model" } */
/* { dg-require-effective-target avx512f } */

#define CHECK_H "avx512f-check.h"

#include "../../gcc.dg/vect/pr59591-2.c"

#include CHECK_H

static void
test_512 (void)
{
  bar ();
}
