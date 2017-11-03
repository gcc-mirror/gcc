// { dg-module-do run }

export module One;
// { dg-module-bmi "One" }

export struct base
{
  long long b;

  base (int b_)
    :b (b_)
  {
  }

  base ()
    :b(99)
  {
  }
};
