/* { dg-do run } */
/* { dg-options "-O2 -mavx2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

#define main() do_main ()

#include "../../gcc.dg/pr64252.c"

static void
avx2_test (void)
{
  do_main ();
}
