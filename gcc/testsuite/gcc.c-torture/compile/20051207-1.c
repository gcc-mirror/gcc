/* PR target/25268 */

long long
foo (long long x, int y)
{
  return x << ((y + 1) & 63);
}
