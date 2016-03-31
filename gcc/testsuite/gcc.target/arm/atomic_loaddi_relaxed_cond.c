/* { dg-do assemble } */
/* { dg-options "-std=c11 -O" } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-add-options arm_arch_v8a } */

/* Check that if we conditionalise the atomic load we put the condition
   code in the right place to create valid assembly.  */

#include <stdatomic.h>

atomic_ullong foo;
int glob;

int
main (int argc, char *argv[])
{
  if (argc > 2)
    atomic_load_explicit (&foo, memory_order_relaxed);
  return glob;
}
