/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse2 -mstv -mpreferred-stack-boundary=4 -mno-stackrealign -W" } */
/* { dg-final { scan-assembler "movq\[ \t\]%xmm\[0-9\]+, \\(%esp\\)" } } */
/* { dg-final { scan-assembler-not "psrlq" } } */

#include <setjmp.h>

extern jmp_buf buf;

extern long long *target_p;
extern long long *c;

extern void foo (long long);

__attribute__ ((noclone, noinline))
void
bar (void)
{
  if (setjmp (buf))
    {
      long long target = *target_p;
      *c = target;
      foo (target);
    }
  else
    foo (0);
}
