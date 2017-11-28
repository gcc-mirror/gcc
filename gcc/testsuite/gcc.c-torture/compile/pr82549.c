/* PR tree-optimization/82549 */

int a, b[1];

int
main ()
{
  return !a || b[-2] || b[-2];
}
