/* PR middle-end/38771 */

unsigned long long
foo (long long x)
{
  return -(unsigned long long) (x ? : x);
}
