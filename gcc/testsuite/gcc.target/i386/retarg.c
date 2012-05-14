/* { dg-require-effective-target lp64 } */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <string.h>

void *p (void *x, void *y, int z)
{
  memcpy (x, y, z);
  return x;
}

/* { dg-final { scan-assembler-not "%rdi" } } */
