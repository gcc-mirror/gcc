// PR c++/23307

class A
{
  template<class R>
  static void f(X&); // { dg-error "" }
  inline void A::f<void>(X&); // { dg-error "f|expected" }
};
