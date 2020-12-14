/* PR tree-optimization/98256 */

unsigned a, b;

int
foo (void)
{
  return !!(~a / b);
}
