/* { dg-do run { target native } } */
/* { dg-options "-fstack-protector" } */
/* { dg-require-effective-target fstack_protector } */

#include <stdlib.h>

void
__stack_chk_fail (void)
{
  exit (0); /* pass */
}

void
overflow()
{
  int i = 0;
  char foo[30];

  /* Overflow buffer.  */
  for (i = 0; i < 50; i++)
      foo[i] = 42;
}

int main (void)
{
  overflow ();
  return 1; /* fail */
}
