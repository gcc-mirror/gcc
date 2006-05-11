/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O -maltivec" } */
/* { dg-final { scan-assembler "stvx" } } */

#include <string.h>

void foo(void)
{
  int x[8] __attribute__((aligned(128)));
  memset (x, 0, sizeof (x));
  bar (x);
}
