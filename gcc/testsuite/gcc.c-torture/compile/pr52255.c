/* PR tree-optimization/52255 */

int a, b, c[10], d[10] = { 0, 0 };

void
foo (void)
{
  for (a = 1; a <= 4; a += 1)
    d[a] = d[1];
  for (; b; ++b)
    c[0] |= 1;
}
