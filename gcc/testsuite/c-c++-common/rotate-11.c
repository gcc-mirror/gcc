/* PR tree-optimization/108440 */
/* { dg-do compile { target { { ilp32 || lp64 } || llp64 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " r<< " 5 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\& 7;" 4 "optimized" } } */

unsigned char
foo (unsigned char x, unsigned int y)
{
  if (y > __CHAR_BIT__)
    return 42;
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ - 1)));
}

unsigned char
bar (unsigned char x, unsigned int y)
{
  if (y >= __CHAR_BIT__)
    return 42;
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ - 1)));
}

unsigned char
baz (unsigned char x, unsigned int y)
{
  if (y > __CHAR_BIT__ && y != 2 * __CHAR_BIT__)
    return 42;
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ - 1)));
}

unsigned char
qux (unsigned char x, unsigned int y)
{
  if (y > __CHAR_BIT__ && y != 2 * __CHAR_BIT__ && y != __CHAR_BIT__ + 2)
    return 42;
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ - 1)));
}

unsigned char
quux (unsigned char x, unsigned int y)
{
  if (y > __CHAR_BIT__)
    return 42;
  return (x << y) | (x >> (__CHAR_BIT__ - y));
}

unsigned char
corge (unsigned char x, unsigned int y)
{
  if (y >= __CHAR_BIT__)
    return 42;
  return (x << y) | (x >> (__CHAR_BIT__ - y));
}
