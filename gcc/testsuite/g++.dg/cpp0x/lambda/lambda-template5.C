// PR c++/53137
// { dg-do compile { target c++11 } }

struct A
{
  template <typename T> void f();

  template <typename T> void g()
  {
    [this]{ f<T>(); }();
  }

  void h()
  {
    g<int>();
  }
};
