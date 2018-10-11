/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O2" } */

#include <stdlib.h>
#include <setjmp.h>

extern void foo (jmp_buf);

long
c (long var)
{
  jmp_buf env;
  if (setjmp(env) != 0)
    abort();
  foo (env);
  return var;
}

/* { dg-final { scan-assembler {\mmr\M} } } */
