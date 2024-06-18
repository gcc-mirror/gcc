/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O -maltivec -mno-vsx" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-final { scan-assembler "stvx" } } */

#include <string.h>

void bar (int *);

void foo(void)
{
  int x[8] __attribute__((aligned(128)));
  memset (x, 0, sizeof (x));
  bar (x);
}
