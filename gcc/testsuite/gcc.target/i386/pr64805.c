/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

#include <stdio.h>

static inline void __attribute ((always_inline)) functionA(void)
{
  return;
}

static inline void __attribute ((always_inline)) functionB(void)
{
  functionA();
}

int test(void)
{
  functionB();

  return 0;
}
