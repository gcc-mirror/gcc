/* Fix for PR119351 alignment peeling with vectors and VLS.  */
/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-Ofast --param aarch64-autovec-preference=sve-only" } */
/* { dg-additional-options "-msve-vector-bits=256" { target aarch64_sve256_hw } } */
/* { dg-additional-options "-msve-vector-bits=128" { target aarch64_sve128_hw } } */

#include "peel_ind_5.c"

int __attribute__ ((optimize (1)))
main (void)
{
  int res = foo ();
  asm volatile ("");
  if (res != START)
    __builtin_abort ();
  return 0;
}
