/* PR c/94842 */
/* { dg-do compile } */
/* { dg-options "" } */

_Atomic float x = 5;

void
foo (void)
{
  void bar (float y[(int) (x += 2)]) {}
}
