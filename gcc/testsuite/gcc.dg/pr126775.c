/* { dg-do compile } */
/* { dg-options "-O2 -ffinite-math-only" } */

_Complex float foo(_Complex float x)
{
  _Complex float negzero = -0.0f + -0.0fi;
  return negzero - x;
}
