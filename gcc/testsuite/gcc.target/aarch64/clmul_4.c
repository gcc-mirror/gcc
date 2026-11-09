/* { dg-do run { target aarch64_pmull_hw } } */
/* { dg-options "-O2 -march=armv8-a+aes" } */

#define main do_main

#include "../../c-c++-common/builtin-clmulg-1.c"

#undef main

int
main ()
{
  do_main ();
  return 0;
}
