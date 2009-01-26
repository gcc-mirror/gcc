/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

#include "test-unwind.h"

#if !defined __PIC__ && !defined __USING_SJLJ_EXCEPTIONS__
double g_f=1.0;
/* Test situation 5: Stack realign dosn't really happen with DRAP reg CX */
void __attribute__ ((noinline)) __attribute__ ((regparm(2))) 
bar(int arg1, int arg2, int arg3, int arg4)
{
  char * s = (char *) __builtin_alloca (arg4 + 1);

  copy (s, arg4);
  if (__builtin_strncmp (s, "good", arg4) != 0)
    {
#ifdef DEBUG
      s[arg4] = '\0';
      printf ("Failed: %s != good\n", s);
#endif
      abort ();
    }
  ALTER_REGS();
  if (g_f) throw arg1+arg2+arg3+ g_f;
}

void __attribute__((noinline))
foo()
{
  bar(1,2,3,4);
}
#endif
