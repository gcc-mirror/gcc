// { dg-options "-std=gnu++11" }
template <typename... T> struct A
{
  void foo(T...);
  A(T... t) { foo(t); } // { dg-error "parameter packs|t" }
};

A<int> a(0);
