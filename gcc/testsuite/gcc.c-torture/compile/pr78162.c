/* PR tree-optimization/78162.
   Handle negative offsets in store merging gracefully.  */

int a, b[1][2];

void fn1()
{
  for (a = 0; a < 2; a++)
    b[-1][a] = 0;
}
