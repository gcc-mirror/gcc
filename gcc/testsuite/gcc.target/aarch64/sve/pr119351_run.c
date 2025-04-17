/* Fix for PR119351 alignment peeling with vectors and VLS.  */
/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-Ofast --param aarch64-autovec-preference=sve-only" } */
/* { dg-additional-options "-msve-vector-bits=256" { target aarch64_sve256_hw } } */
/* { dg-additional-options "-msve-vector-bits=128" { target aarch64_sve128_hw } } */

#include "pr119351.c"

int __attribute__ ((optimize (1)))
main (void)
{
  x[0] = 1;
  x[1] = 21;
  x[2] = 39;
  x[3] = 59;
  int res = foo ();
  if (res != 4)
    __builtin_abort ();
  return 0;
}
