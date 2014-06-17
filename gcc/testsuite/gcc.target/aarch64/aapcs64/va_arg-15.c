/* Test AAPCS64 layout and __builtin_va_start.

   Pass named __128int argument on stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-15.c"
#include "type-def.h"

union int128_t qword;

#define HAS_DATA_INIT_FUNC
void
init_data ()
{
  /* Init signed quad-word integer.  */
  qword.l64 = 0xfdb9753102468aceLL;
  qword.h64 = 0xeca8642013579bdfLL;
}

#include "abitest.h"
#else
  ARG (int, 1, W0, 0)
  ARG (int, 2, W1, 1)
  ARG (int, 3, W2, 2)
  ARG (int, 4, W3, 3)
  ARG (int, 5, W4, 4)
  ARG (int, 6, W5, 5)
  ARG (int, 7, W6, 6)
  ARG (__int128, qword.i, STACK, LAST_NAMED_ARG_ID)
  DOTS
#ifndef __AAPCS64_BIG_ENDIAN__
  LAST_ANON (int, 8, STACK + 16, 8)
#else
  LAST_ANON (int, 8, STACK + 20, 8)
#endif
#endif
