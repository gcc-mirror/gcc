// PR c++/48523
// { dg-do compile { target c++11 } }

template<typename>
struct X
{
  bool b;

  void f()
  {
    [this]{ return b; };
  }
};
