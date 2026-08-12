/* PR tree-optimization/126778 */
/* { dg-do run } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target aarch64_sve_hw { target aarch64*-*-* } } */
/* { dg-additional-options "-O3 -fno-vect-cost-model" } */
/* { dg-additional-options "-march=armv8-a+sve" { target aarch64*-*-* } } */

#include "tree-vect.h"

__attribute__ ((noipa))
void
glob3 (char *sc, short *dc)
{
  while (dc && (*dc++ = *sc))
    ;
}

int
main (void)
{
  check_vect ();

  char c = 0;
  short dst[16];

#pragma GCC novector
  for (int i = 0; i < 16; ++i)
    dst[i] = 42;

  glob3 (&c, dst);

  if (dst[0] != 0)
    __builtin_abort ();

#pragma GCC novector
  for (int i = 1; i < 16; ++i)
    if (dst[i] != 42)
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target aarch64*-*-* } } } */
