/* { dg-do run { target native } } */
/* { dg-options "-fstack-protector-explicit" } */
/* { dg-require-effective-target fstack_protector } */

#include <stdlib.h>

void
__stack_chk_fail (void)
{
  exit (0); /* pass */
}

int __attribute__((stack_protect)) main ()
{
  int i;
  char foo[255];

  // smash stack
  for (i = 0; i <= 400; i++)
    foo[i] = 42;

  return 1; /* fail */
}
