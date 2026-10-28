/* PR target/122382 */
/* { dg-do run } */
/* { dg-options "-O3 -mavx512vl -mno-f16c -mno-avx512fp16 -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx512vl } */

#include "avx512vl-check.h"
#include "pr122382.h"

static void
avx512vl_test (void)
{
  run_tests ();
}
