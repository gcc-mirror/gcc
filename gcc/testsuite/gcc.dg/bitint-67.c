/* PR tree-optimization/113316 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-std=c23 -O2 -w" } */

void bar (_BitInt(535) y);

void
foo (void)
{
  _BitInt(535) y;
  bar (y);
}
