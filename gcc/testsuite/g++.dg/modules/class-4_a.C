// { dg-module-do run }

export module One;
// { dg-module-bmi "One" }

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
