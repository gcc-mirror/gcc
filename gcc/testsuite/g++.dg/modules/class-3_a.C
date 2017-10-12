// { dg-module-do run }

export module One;
// { dg-module-bmi "One" }

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
