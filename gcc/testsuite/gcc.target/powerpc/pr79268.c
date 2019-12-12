/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* powerpc-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */

/* Verify that vec_xl and vec_xst accept vector pixel parameters.  */

/* Test case to resolve PR79268.  */

#include <altivec.h>

vector pixel a;

vector pixel
pr79268 (vector pixel *x)
{
  vec_xst (a, 0, x);
  return vec_xl (0, x);
}
