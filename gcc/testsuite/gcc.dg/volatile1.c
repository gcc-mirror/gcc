/* PR c/12553: we were erroneously setting TREE_SIDE_EFFECTS on &y, which
   confused tree-ssa.  */

void f()
{
  int x;
  volatile int y;
  &x == &y;
}
