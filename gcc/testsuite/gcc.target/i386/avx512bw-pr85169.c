/* PR target/85169 */
/* { dg-do run { target avx512bw } } */
/* { dg-options "-O2 -mavx512bw" } */

#include "avx512bw-check.h"

int do_main (void);

static void
avx512bw_test (void)
{
  do_main ();
}

#undef main
#define main() do_main ()

#include "../../gcc.c-torture/execute/pr85169.c"
