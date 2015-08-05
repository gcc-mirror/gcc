/* { dg-do compile } */
/* { dg-options "-frounding-math -funsafe-math-optimizations" } */

int test ()
{
  return 5.0 < 5.0 - 0.1;
}
