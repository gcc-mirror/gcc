/* Check rotate pattern detection.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "r\[<>]\[<>]" "optimized" } } */

unsigned short int
f5 (unsigned short int x, int y)
{
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ * __SIZEOF_SHORT__ - 1)));
}

unsigned short int
f6 (unsigned short int x, long int y)
{
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ * __SIZEOF_SHORT__ - 1)));
}

unsigned char
f7 (unsigned char x, int y)
{
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ - 1)));
}

unsigned char
f8 (unsigned char x, long int y)
{
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ - 1)));
}

unsigned short int
f13 (unsigned short int x, int y)
{
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ * sizeof (unsigned short) - 1)));
}

unsigned short int
f14 (unsigned short int x, long int y)
{
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ * sizeof (unsigned short) - 1)));
}

unsigned char
f15 (unsigned char x, int y)
{
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ * sizeof (unsigned char) - 1)));
}

unsigned char
f16 (unsigned char x, long int y)
{
  return (x << y) | (x >> ((-y) & (__CHAR_BIT__ * sizeof (unsigned char) - 1)));
}

unsigned short int
f37 (unsigned short int x, int y)
{
  return (x >> y) | (x << ((-y) & (__CHAR_BIT__ * __SIZEOF_SHORT__ - 1)));
}

unsigned short int
f38 (unsigned short int x, long int y)
{
  return (x >> y) | (x << ((-y) & (__CHAR_BIT__ * __SIZEOF_SHORT__ - 1)));
}

unsigned char
f39 (unsigned char x, int y)
{
  return (x >> y) | (x << ((-y) & (__CHAR_BIT__ - 1)));
}

unsigned char
f40 (unsigned char x, long int y)
{
  return (x >> y) | (x << ((-y) & (__CHAR_BIT__ - 1)));
}

unsigned short int
f45 (unsigned short int x, int y)
{
  return (x >> y) | (x << ((-y) & (__CHAR_BIT__ * sizeof (unsigned short) - 1)));
}

unsigned short int
f46 (unsigned short int x, long int y)
{
  return (x >> y) | (x << ((-y) & (__CHAR_BIT__ * sizeof (unsigned short) - 1)));
}

unsigned char
f47 (unsigned char x, int y)
{
  return (x >> y) | (x << ((-y) & (__CHAR_BIT__ * sizeof (unsigned char) - 1)));
}

unsigned char
f48 (unsigned char x, long int y)
{
  return (x >> y) | (x << ((-y) & (__CHAR_BIT__ * sizeof (unsigned char) - 1)));
}
