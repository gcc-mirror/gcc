/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O -maltivec -mno-vsx" } */
/* { dg-final { scan-assembler "lvx" } } */

#include <string.h>

void foo(void)
{
  extern int x[8] __attribute__((aligned(128)));
  int y[8] __attribute__((aligned(128)));
  memcpy (y, x, sizeof (x));
  bar (y);
}
