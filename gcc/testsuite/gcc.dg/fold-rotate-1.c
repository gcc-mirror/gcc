/* PR middle-end/29749 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

#if __SCHAR_MAX__ == 127

unsigned char
e1 (unsigned char a)
{
  return a >> 5 | a << 3;
}

unsigned char
e2 (unsigned char a)
{
  return (a & 0xe0) >> 5 | (a & 0x1f) << 3;
}

unsigned char
e3 (unsigned char a)
{
  return ((a >> 5) & 0x07) | ((a << 3) & 0xf8);
}

#endif

#if __SHRT_MAX__ == 32767

unsigned short
f1 (unsigned short a)
{
  return a >> 8 | a << 8;
}

unsigned short
f2 (unsigned short a)
{
  return (a & 0xff00) >> 8 | (a & 0x00ff) << 8;
}

unsigned short
f3 (unsigned short a)
{
  return ((a >> 8) & 0x00ff) | ((a << 8) & 0xff00);
}

#endif

#if __INT_MAX__ == 2147483647

unsigned int
g1 (unsigned int a)
{
  return a >> 24 | a << 8;
}

unsigned int
g2 (unsigned int a)
{
  return (a & 0xff000000) >> 24 | (a & 0x00ffffff) << 8;
}

unsigned int
g3 (unsigned int a)
{
  return ((a >> 24) & 0x000000ff) | ((a << 8) & 0xffffff00U);
}

#endif

int i;

/* { dg-final { scan-tree-dump-times "&" 0 "original" } } */
