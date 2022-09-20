/* PR tree-optimization/106958 */

int a;
void bar (int);

void
foo (char *x, char *y)
{
  int b = a != 0;
  int c = x != 0;
  int d = y != 0;
  bar (b | c | d);
}
