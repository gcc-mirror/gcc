/* { dg-do compile } */
/* { dg-options "-O2 --param=builtin-expect-probability=100" } */
#include <string.h>
t(int c)
{
  if (__builtin_expect (c, 0))
    {
      cold_hint ();
      return c * 11;
    }
  return c;
}

/* { dg-final { scan-assembler "imul" } } */
