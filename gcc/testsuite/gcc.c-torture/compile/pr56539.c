/* PR tree-optimization/56539 */

short
foo (const char *x, unsigned y)
{
  return y > 1 ? (x[y - 1] - '0') + 10 * foo (x, y - 1) : (*x - '0');
}
