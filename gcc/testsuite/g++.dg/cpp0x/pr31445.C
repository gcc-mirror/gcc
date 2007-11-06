// { dg-options "-std=gnu++0x" }
template <typename... T> struct A
{
  void foo(T...);
  A(T... t) { foo(t); } // { dg-error "parameter packs|t" }
};

A<int> a(0);
