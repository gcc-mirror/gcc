/* { dg-do run } */
/* { dg-options "-O2 -march=armv8-a" } */

#define main do_main

#include "../../c-c++-common/builtin-clmulg-1.c"

#undef main

int
main ()
{
  do_main ();
  return 0;
}
