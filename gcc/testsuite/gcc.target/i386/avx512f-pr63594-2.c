/* PR target/63594 */
/* { dg-do run { target avx512f } } */
/* { dg-options "-O2 -mavx512f -mno-mmx -Wno-psabi" } */

#include "avx512f-check.h"

int do_main (void);

static void
avx512f_test (void)
{
  do_main ();
}

#undef main
#define main() do_main ()

#include "../../gcc.dg/pr63594-2.c"
