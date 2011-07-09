/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

#include "test-unwind.h"

#if !defined __PIC__ && !defined __USING_SJLJ_EXCEPTIONS__
/* Test situation 3: Stack realign really happen with DRAP reg DI */
void __attribute__ ((noinline)) __attribute__ ((regparm(3))) 
bar (int arg1, int arg2, int arg3)
{
  int __attribute__ ((aligned(64))) a=1;
  char * s = (char *) __builtin_alloca (arg3 + 1);

  copy (s, arg3);
  if (__builtin_strncmp (s, "good", arg3) != 0)
    {
#ifdef DEBUG
      s[arg3] = '\0';
      printf ("Failed: %s != good\n", s);
#endif
      abort ();
    }

  if (check_int (&a,  __alignof__(a)) != a)
    abort ();

  ALTER_REGS();
  throw arg1+arg2+arg3+a;
}

void
foo()
{
  bar (1,2,3);
}
#endif
