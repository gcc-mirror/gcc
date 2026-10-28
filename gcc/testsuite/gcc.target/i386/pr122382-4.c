/* PR target/122382 */
/* { dg-do run } */
/* { dg-options "-O3 -mf16c -mno-avx512f" } */
/* { dg-require-effective-target f16c } */

#include "f16c-check.h"
#include "pr122382.h"

static void
f16c_test (void)
{
  run_tests ();
}
