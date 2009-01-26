/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

#include "test-unwind.h"

#if !defined __PIC__ && !defined __USING_SJLJ_EXCEPTIONS__
/* Test situation 2: stack really realign with DRAP reg CX */
void __attribute__ ((noinline))
foo ()
{
  int __attribute__ ((aligned(64))) a=4;
  char * s = (char *) __builtin_alloca (a + 1);

  copy (s, a);
  if (__builtin_strncmp (s, "good", a) != 0)
    {
#ifdef DEBUG
      s[a] = '\0';
      printf ("Failed: %s != good\n", s);
#endif
      abort ();
    }

  if (check_int (&a,  __alignof__(a)) != a)
    abort ();

  ALTER_REGS();
  throw a;
}
#endif
