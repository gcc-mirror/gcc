// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Jul 2003 <nathan@codesourcery.com>

// Packed fields are unsuitable for direct reference binding.

struct Unpacked { int i; };

int ConstRef (int const &p, int const *ptr, int v)
{
  if (p != v)
    return 1;
  if (&p == ptr)
    return 2;
  return 0;
}

int ConstRef (Unpacked const &p, Unpacked const *ptr, int v)
{
  if (p.i != v)
    return 1;
  if (&p == ptr)
    return 2;
  return 0;
}

int Val (int p, int v)
{
  if (p != v)
    return 1;
  return 0;
}
int Val (Unpacked p, int v)
{
  if (p.i != v)
    return 1;
  return 0;
}

struct  __attribute__ ((packed)) Packed
{
  char c;
  int i;
  Unpacked u;
  char t;
};

int Foo (Packed &p, int i, int ui)
{
  int r;
  
  if ((r = Val (p.i, i)))
    return r;
  if ((r = Val (p.u.i, ui)))
    return r + 2;
  if ((r = Val (p.u, ui)))
    return r + 4;
  
  if ((r = ConstRef (p.i, &p.i, i)))
    return r + 6;

  return 0;
}

int main ()
{
  Packed p;

  p.c = 0x12;
  p.i = 0x3456789a;
  p.u.i = 0xbcdef00f;
  p.t = 0xed;

  return Foo (p, 0x3456789a, 0xbcdef00f);
}
