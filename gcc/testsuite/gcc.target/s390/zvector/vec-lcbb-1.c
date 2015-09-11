/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector" } */

/* { dg-final { scan-assembler-times "\tlcbb\t" 4 } } */

#include <vecintrin.h>

/* CC will be extracted into a GPR and returned.  */
int
foo1 (void *ptr)
{
  return __lcbb (ptr, 64);
}

int
foo2 (void *ptr)
{
  return __lcbb (ptr, 128) > 16;
}

int
foo3 (void *ptr)
{
  return __lcbb (ptr, 256) == 16;
}

int
foo4 (void *ptr)
{
  return __lcbb (ptr, 512) < 16;
}
