/* PR target/63594 */
/* { dg-do run { target sse2 } } */
/* { dg-options "-O2 -msse2 -mno-mmx -Wno-psabi" } */

#include "sse2-check.h"

int do_main (void);

static void
sse2_test (void)
{
  do_main ();
}

#undef main
#define main() do_main ()

#include "../../gcc.dg/pr63594-2.c"
