/* { dg-do compile } */
/* { dg-options "-O -frounding-math -funsafe-math-optimizations" } */

double
f (double g)
{
  return g / 3;
}
