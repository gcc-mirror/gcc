/* PR target/82498 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -masm=att" } */
/* { dg-final { scan-assembler-not {\mand[bwlq]\M} } } */

unsigned
f1 (unsigned x, unsigned char y)
{
  if (y == 0)
    return x;
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f2 (unsigned x, unsigned y)
{
  if (y == 0)
    return x;
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f3 (unsigned x, unsigned short y)
{
  if (y == 0)
    return x;
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f4 (unsigned x, unsigned char y)
{
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return (x << y) | (x >> (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned
f5 (unsigned x, unsigned int y)
{
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return (x << y) | (x >> (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned
f6 (unsigned x, unsigned short y)
{
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return (x << y) | (x >> (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}
