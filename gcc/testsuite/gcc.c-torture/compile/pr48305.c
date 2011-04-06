/* PR c/48305 */

int
foo (int x)
{
  return (x ^ 1) == (x ^ 1U);
}
