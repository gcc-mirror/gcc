// Special g++ Options: -O2
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 May 2001 <nathan@codesourcery.com>

// Bug 2781. We forgot to copy addressability information when
// cloning.

struct B
{
  B(int v1);
  void Member (int v1);
  static void Static (int v1);
};

struct D : B
{
  D (int v1);
};

void xswap(int& x1) ;

int xxx = 0;

B::B(int v1) 
{
  xswap(v1);
  xxx = v1;
}

void B::Member(int v1) 
{
  xswap(v1);
  xxx = v1;
}

void B::Static(int v1) 
{
  xswap(v1);
  xxx = v1;
}

D::D(int v1)
  : B (v1)
{
}

void xswap (int& x1) { x1 = 2; }

int main ()
{
  B p (1);

  if (xxx != 2)
    return 1;

  D q (1);
  if (xxx != 2)
    return 2;
  
  p.Member (1);
  if (xxx != 2)
    return 3;

  p.Static (1);
  if (xxx != 2)
    return 4;

  return 0;
}
