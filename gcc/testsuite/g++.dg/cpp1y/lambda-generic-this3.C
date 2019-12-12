// PR c++/87685
// { dg-do compile { target c++14 } }

struct A
{
  template <typename T> static void f(T) {}
  void f() {}

  void foo()
  {
    [] (auto&& v) { A::f(v); }; // OK if parameter type is specified
  }
};
