/* { dg-do compile } */
/* { dg-options "-O2" } */
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
