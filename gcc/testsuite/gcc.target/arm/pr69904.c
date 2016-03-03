/* { dg-do compile } */
/* { dg-options "-O2 -marm" } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-add-options arm_arch_v7a } */

/* Make sure that RTL optimizers don't do any unexpected transformations
   on the compare_exchange loop.  */

#include <stdatomic.h>

atomic_uint foo;
atomic_uint bar;
int glob;

int
main (void)
{
  glob = atomic_compare_exchange_strong (&foo, &bar, 0);
  return glob;
}

/* { dg-final { scan-assembler-times "dmb\tish" 2 } } */
/* { dg-final { scan-assembler-times "ldrex\t" 1 } } */
/* { dg-final { scan-assembler-times "strex\t" 1 } } */
