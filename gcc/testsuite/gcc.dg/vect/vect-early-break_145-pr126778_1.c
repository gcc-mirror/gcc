/* PR tree-optimization/126778 */
/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3 -fno-vect-cost-model" } */
/* { dg-additional-options "-march=armv8-a+sve" { target aarch64*-*-* } } */

void
glob3 (char *sc, short *dc)
{
  while (dc && (*dc++ = *sc))
    ;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target aarch64*-*-* } } } */
