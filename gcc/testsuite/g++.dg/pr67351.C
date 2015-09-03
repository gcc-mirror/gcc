/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long long uint64;

class MyRgba
{
  uint rgba;

public:
    explicit MyRgba (uint c):rgba (c)
  {
  };

  static MyRgba fromRgba (uchar r, uchar g, uchar b, uchar a)
  {
    return MyRgba (uint (r) << 24
		   | uint (g) << 16 | uint (b) << 8 | uint (a));
  }

  uchar r ()
  {
    return rgba >> 24;
  }
  uchar g ()
  {
    return rgba >> 16;
  }
  uchar b ()
  {
    return rgba >> 8;
  }
  uchar a ()
  {
    return rgba;
  }

  void setG (uchar _g)
  {
    *this = fromRgba (r (), _g, b (), a ());
  }
};

extern MyRgba giveMe ();

MyRgba
test ()
{
  MyRgba a = giveMe ();
  a.setG (0xf0);
  return a;
}

class MyRgba64
{
  uint64 rgba;

public:
    explicit MyRgba64 (uint64 c):rgba (c)
  {
  };

  static MyRgba64 fromRgba64 (ushort r, ushort g, ushort b, ushort a)
  {
    return MyRgba64 (uint64 (r) << 48
		     | uint64 (g) << 32 | uint64 (b) << 16 | uint64 (a));
  }

  ushort r ()
  {
    return rgba >> 48;
  }
  ushort g ()
  {
    return rgba >> 32;
  }
  ushort b ()
  {
    return rgba >> 16;
  }
  ushort a ()
  {
    return rgba;
  }

  void setG (ushort _g)
  {
    *this = fromRgba64 (r (), _g, b (), a ());
  }
};

extern MyRgba64 giveMe64 ();

MyRgba64
test64 ()
{
  MyRgba64 a = giveMe64 ();
  a.setG (0xf0f0);
  return a;
}

/* { dg-final { scan-tree-dump-not "<<" "optimized" } } */
/* { dg-final { scan-tree-dump-not ">>" "optimized" } } */
