// PR c++/48523
// { dg-options -std=c++0x }

template<typename>
struct X
{
  bool b;

  void f()
  {
    [this]{ return b; };
  }
};
