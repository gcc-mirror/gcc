/* PR tree-optimization/51246 */

int a, *b;

void
test (void)
{
  while (1)
    {
      int c;
      a = c;
      b = &c;
    }
}
