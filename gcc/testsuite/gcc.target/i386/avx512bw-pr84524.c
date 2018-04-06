/* PR target/84524 */
/* { dg-do run { target avx512bw } } */
/* { dg-options "-O3 -mavx512bw" } */

#include "avx512bw-check.h"

#define main() do_main()
#include "../../gcc.c-torture/execute/pr84524.c"

static void
avx512bw_test (void)
{
  do_main ();
}
