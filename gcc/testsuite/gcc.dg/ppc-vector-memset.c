/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O -maltivec" } */
/* { dg-final { scan-assembler "stvx" } } */

#include <string.h>

void foo(void)
{
  int x[8] __attribute__((aligned(128)));
  memset (x, 0, sizeof (x));
  bar (x);
}
