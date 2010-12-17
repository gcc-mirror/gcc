/* PR rtl-optimization/46034 */

void bar (int);

void
foo (int x, int y)
{
  int i;
  for (i = 0; i < x; i++)
    {
      y = __builtin_abs (y);
      bar (y / 2);
    }
}
