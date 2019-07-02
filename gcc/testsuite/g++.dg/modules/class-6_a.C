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

  virtual int getter () const;
};

export struct pad
{
  int pad;
  virtual ~pad ();
};

export struct derived : pad, virtual base
{
  derived (int v)
    :base (v)
  {
  }
};
