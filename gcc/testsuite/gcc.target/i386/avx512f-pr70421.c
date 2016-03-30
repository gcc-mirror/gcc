/* PR target/70421 */
/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include "avx512f-check.h"

#define main() do_main()
#include "../../gcc.dg/torture/pr70421.c"

static void
avx512f_test (void)
{
  do_main ();
}
