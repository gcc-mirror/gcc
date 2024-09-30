/* PR target/116738 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F
#include "avx512f-helper.h"

#include "avx512f-pr116738-1.c"

void
TEST (void)
{
  test_pr116738 ();
}
