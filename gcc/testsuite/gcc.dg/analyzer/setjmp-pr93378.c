/* { dg-additional-options "-O1 -g" } */
/* { dg-require-effective-target indirect_jumps } */

#include "test-setjmp.h"

jmp_buf buf;

int
test (void)
{
  if (setjmp (buf) != 0)
    return 0;

  longjmp (buf, 1);
  return 1;
}
