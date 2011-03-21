/* PR middle-end/48136 */

int
foo (int x, int y)
{
  return (x ^ 5U) == (y ^ 1);
}
