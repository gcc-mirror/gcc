// { dg-options "-std=gnu++0x" }
template <typename... T> struct A
{
  void foo(T...); // { dg-error "candidates" }
  A(T... t) { foo(t); } // { dg-error "parameter packs|t|no matching" }
};

A<int> a(0);
