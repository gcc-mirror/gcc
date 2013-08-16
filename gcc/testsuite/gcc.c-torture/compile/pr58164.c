/* PR tree-optimization/58164 */

int
foo (void)
{
  int x = 0;
  goto *&x;
}
