/* Known inbounds DR in VLA modes.  */
/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-Ofast -msve-vector-bits=scalable --param aarch64-autovec-preference=sve-only" } */

#include "peel_ind_13.c"

int __attribute__ ((optimize (1)))
main (void)
{
  int res = foo ();
  asm volatile ("");
  if (res != START)
    __builtin_abort ();
  return 0;
}
