/* { dg-do run { target aarch64_sve2_hw } } */
/* { dg-options "-O2 -march=armv8-a+sve2" } */

/* Without +aes, 32-bit and 16-bit carry-less multiplications use SVE2
   PMULLB.  */

#define main do_main

#include "../../../c-c++-common/builtin-clmulg-1.c"

#undef main

int
main ()
{
  do_main ();
  return 0;
}
