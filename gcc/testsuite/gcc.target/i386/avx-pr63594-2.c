/* PR target/63594 */
/* { dg-do run { target avx } } */
/* { dg-options "-O2 -mavx -mno-mmx -Wno-psabi" } */

#include "avx-check.h"

int do_main (void);

static void
avx_test (void)
{
  do_main ();
}

#undef main
#define main() do_main ()

#include "../../gcc.dg/pr63594-2.c"
