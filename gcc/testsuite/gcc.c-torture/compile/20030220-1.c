/* PR optimization/9768 */
/* Originator: Randolph Chung <tausq@debian.org> */

inline int fixfloor (long x)
{
  if (x >= 0)
    return (x >> 16);
  else
    return ~((~x) >> 16);
}

inline int fixtoi (long x)
{
  return fixfloor(x) + ((x & 0x8000) >> 15);
}

int foo(long x, long y)
{
  return fixtoi(x*y);
}
