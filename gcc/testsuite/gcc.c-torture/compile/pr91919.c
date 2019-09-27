/* PR target/91919 */

unsigned int
foo (unsigned int x, int y)
{
  return (x * 3355443200ULL + (y * 1801439851ULL >> 29) >> 25);
}
