/* PR bootstrap/42347 */

long
foo (long x, long y)
{
  x = x & y;
  switch (y)
    {
    case 63L: x >>= 0; break;
    case 4032L: x >>= 6; break;
    case 258048L: x >>= 12; break;
    case 16515072L: x >>= 18; break;
    default: __builtin_unreachable ();
    }
  return x;
}
