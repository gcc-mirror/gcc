/* PR target/63594 */
/* { dg-do run { target avx2 } } */
/* { dg-options "-O2 -mavx2 -mno-mmx -Wno-psabi" } */

#include "avx2-check.h"

int do_main (void);

static void
avx2_test (void)
{
  do_main ();
}

#undef main
#define main() do_main ()

#include "../../gcc.dg/pr63594-2.c"
