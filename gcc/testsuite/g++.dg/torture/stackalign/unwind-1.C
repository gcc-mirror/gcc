/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

#include "test-unwind.h"

#ifndef __PIC__
/* Test situation 1: Stack really realign without DRAP */
void __attribute__ ((noinline))
foo ()
{
  int __attribute__ ((aligned(64))) a=1;
  if (check_int (&a,  __alignof__(a)) != a)
    abort ();
  ALTER_REGS();
  throw a;
}
#endif
