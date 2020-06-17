/* { dg-additional-options "-O1 -g" } */

#include <setjmp.h>

jmp_buf buf;

int
test (void)
{
  if (setjmp (buf) != 0)
    return 0;

  longjmp (buf, 1);
  return 1;
}
