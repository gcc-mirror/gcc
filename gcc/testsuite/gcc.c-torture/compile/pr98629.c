/* PR tree-optimization/98629 */

unsigned int a;
int b, c;

void
foo (void)
{
  unsigned int *e = &a;
  (a /= a |= b) - (0 <= (*e += *e)) * (c *= *e);
}
