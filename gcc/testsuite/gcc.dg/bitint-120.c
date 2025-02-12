/* PR tree-optimization/118522 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

_BitInt(32) b;

int
foo (unsigned short p)
{
  return p == (double) b;
}
