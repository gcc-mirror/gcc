// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }
export module One;
// { dg-module-cmi "One" }

export struct X
{
  X (int, int);
  X (int a_)
    : a(a_), b (a_ << 16)
  {
  }
  int a;
  int b;
};
