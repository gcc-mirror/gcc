/* PR tree-optimization/110251 */

int a, b;
signed char c;

int
foo (int e)
{
  if (e >= 'a')
    return e;
}

int
bar (unsigned short e)
{
  for (; e; a++)
    e &= e - 1;
}

void
baz (void)
{
  while (c < 1)
    ;
  for (; bar (c - 1); b = foo (c))
    ;
}
