// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }
export module One;
// { dg-module-cmi "One" }

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
