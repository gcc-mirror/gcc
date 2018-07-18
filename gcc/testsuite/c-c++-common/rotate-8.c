/* PR middle-end/62263 */
/* PR middle-end/82498 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "r\[<>]\[<>]" 23 "optimized" } } */
/* { dg-final { scan-tree-dump-not "PHI <" "optimized" } } */

unsigned int
f1 (unsigned int x, unsigned char y)
{
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned int
f2 (unsigned int x, signed char y)
{
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned int
f3 (unsigned int x, unsigned char y)
{
  return (x << (y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - (y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))));
}

unsigned int
f4 (unsigned int x, unsigned char y)
{
  y = y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1);
  return y ? (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y)) : x;
}

unsigned int
f5 (unsigned int x, unsigned char y)
{
  y = y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1);
  return (x << y) | (x >> ((__CHAR_BIT__ * __SIZEOF_INT__ - y) & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned int
f6 (unsigned int x, unsigned char y)
{
  return (x << (y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) | (x >> ((__CHAR_BIT__ * __SIZEOF_INT__ - (y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned int
f7 (unsigned int x, unsigned char y)
{
  return (x << (y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) | (x >> ((__CHAR_BIT__ * __SIZEOF_INT__ - y) & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned int
f8 (unsigned int x, unsigned char y)
{
  return (x << (y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) | (x >> ((-y) & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned int
f9 (unsigned int x, int y)
{
  return (0x12345678U << (y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) | (0x12345678U >> (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned int
f10 (unsigned int x, int y)
{
  return (0x12345678U >> (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) | (0x12345678U << (y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned int
f11 (unsigned int x, int y)
{
  return (0x12345678U >> (y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) | (0x12345678U << (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned int
f12 (unsigned int x, int y)
{
  return (0x12345678U << (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) | (0x12345678U >> (y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned
f13 (unsigned x, unsigned char y)
{
  if (y == 0)
    return x;
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f14 (unsigned x, unsigned y)
{
  if (y == 0)
    return x;
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f15 (unsigned x, unsigned short y)
{
  if (y == 0)
    return x;
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f16 (unsigned x, unsigned char y)
{
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  if (y == 0)
    return x;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f17 (unsigned x, unsigned y)
{
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  if (y == 0)
    return x;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f18 (unsigned x, unsigned short y)
{
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  if (y == 0)
    return x;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f19 (unsigned x, unsigned char y)
{
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  return (x << y) | (x >> (((unsigned char) -y) % (__CHAR_BIT__ * __SIZEOF_INT__)));
}

unsigned
f20 (unsigned x, unsigned int y)
{
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  return (x << y) | (x >> (-y % (__CHAR_BIT__ * __SIZEOF_INT__)));
}

unsigned
f21 (unsigned x, unsigned short y)
{
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  return (x << y) | (x >> (((unsigned short) -y) % (__CHAR_BIT__ * __SIZEOF_INT__)));
}

unsigned
f22 (unsigned x, unsigned char y)
{
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  return (x << y) | (x >> (-y & ((__CHAR_BIT__ * __SIZEOF_INT__) - 1)));
}

unsigned
f23 (unsigned x, unsigned short y)
{
  y %= __CHAR_BIT__ * __SIZEOF_INT__;
  return (x << y) | (x >> (-y & ((__CHAR_BIT__ * __SIZEOF_INT__) - 1)));
}
