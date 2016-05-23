// { dg-do compile { target c++11 } }
template <typename... T> struct A
{
  void foo(T...);
  A(T... t) { foo(t); } // { dg-error "18:parameter packs|t" }
};

A<int> a(0);
