/* PR target/82498 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
/* { dg-final { scan-tree-dump-times "x r<< y" 4 "original" { target int32 } } } */
/* { dg-final { scan-tree-dump-times "x r>> y" 4 "original" { target int32 } } } */

unsigned
f1 (unsigned x, int y)
{
  return (x << y) | (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f2 (unsigned x, int y)
{
  return (x << y) | (x >> (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned
f3 (unsigned x, int y)
{
  return (x >> y) | (x << (__CHAR_BIT__ * __SIZEOF_INT__ - y));
}

unsigned
f4 (unsigned x, int y)
{
  return (x >> y) | (x << (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1)));
}

unsigned
f5 (unsigned x, int y)
{
  return (x >> (__CHAR_BIT__ * __SIZEOF_INT__ - y)) | (x << y);
}

unsigned
f6 (unsigned x, int y)
{
  return (x >> (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) | (x << y);
}

unsigned
f7 (unsigned x, int y)
{
  return (x << (__CHAR_BIT__ * __SIZEOF_INT__ - y)) | (x >> y);
}

unsigned
f8 (unsigned x, int y)
{
  return (x << (-y & (__CHAR_BIT__ * __SIZEOF_INT__ - 1))) | (x >> y);
}
