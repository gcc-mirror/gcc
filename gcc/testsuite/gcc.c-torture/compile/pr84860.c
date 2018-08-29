/* PR target/84860 */

void
foo (int x, int y)
{
  while (x < 1)
    {
      x = y;
      y = ((float)1 / 0) ? 2 : 0;
    }
}
