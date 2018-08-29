/* PR rtl-optimization/82913 */

unsigned int a;
unsigned long int b;

int
foo (void)
{
  ++a;
  b = 0;
}

unsigned long int
bar (int x)
{
  if (!foo () || !a)
    {
      int c = a != b;
      if (c != x)
        return a;
    }
  return 0;
}
