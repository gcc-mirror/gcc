/* PR target/82498 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -masm=att" } */
/* { dg-final { scan-assembler-not {\mand[bwlq]\M} } } */

int
f1 (int x, unsigned char y)
{
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return x >> y;
}

unsigned
f2 (unsigned x, unsigned char y)
{
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return x >> y;
}

unsigned
f3 (unsigned x, unsigned char y)
{
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return x << y;
}

unsigned
f4 (unsigned x, unsigned char y)
{
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return x | (1U << y);
}

unsigned
f5 (unsigned x, unsigned char y)
{
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return x ^ (1U << y);
}

unsigned
f6 (unsigned x, unsigned char y)
{
  y &= __CHAR_BIT__ * __SIZEOF_INT__ - 1;
  return (x + 2) & ~(1U << y);
}
