// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }
export module One;
// { dg-module-cmi "One" }

export struct base 
{
  int b;

  base (int b_)
    : b (b_)
  {
  }
  
};

export struct derived : base
{
  int d;

  derived (int b_, int d_)
    : base (b_), d (d_)
  {
  }
};
