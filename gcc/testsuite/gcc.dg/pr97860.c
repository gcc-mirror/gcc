/* PR c/97860 */
/* { dg-do compile } */
/* { dg-options "" } */

void
foo (int n)
{
  typedef int T[0];
  typedef T V[n];
  void bar (V);
}
