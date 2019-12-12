/* PR middle-end/91725 */

unsigned long long
foo (unsigned long long x, unsigned long long y, int z)
{
  return (x + y) / (1 << z);
}
