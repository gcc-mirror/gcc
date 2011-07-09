/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

#include "test-unwind.h"

#if !defined __PIC__ && !defined __USING_SJLJ_EXCEPTIONS__
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
