/* { dg-do run } */

#include <stdio.h>
#include <assert.h>

#define align (1ul << __ARM_ALIGN_MAX_PWR)
static int x __attribute__ ((aligned (align)));

int
main ()
{
  assert ((((unsigned long)&x) & (align - 1)) == 0);

  return 0;
}
