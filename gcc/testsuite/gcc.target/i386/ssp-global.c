/* { dg-do run { target fstack_protector } } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=global" } */

#include <stdlib.h>

#ifdef __LP64__
const unsigned long int __stack_chk_guard = 0x2d853605a4d9a09cUL;
#else
const unsigned long int __stack_chk_guard = 0xdd2cc927UL;
#endif

void
__stack_chk_fail (void)
{
  exit (0); /* pass */
}

__attribute__ ((noipa))
void
smash (char *p, int i)
{
  p[i] = 42;
}

int
main (void)
{
  char foo[255];

   /* smash stack */
  for (int i = 0; i <= 400; i++)
    smash (foo, i);

  return 1;
}
