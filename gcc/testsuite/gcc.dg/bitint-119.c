/* PR tree-optimization/118023 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

_BitInt(63) b;

int
foo (void)
{
  return !*(_Complex char *) &b;
}
