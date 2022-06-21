/* PR middle-end/106030 */

int a, b, c;

char
foo (int x, int y)
{
  return x * y;
}

void
bar (void)
{
  char d = (foo <= b) * a;
  c = foo (2 != bar, d);
}
