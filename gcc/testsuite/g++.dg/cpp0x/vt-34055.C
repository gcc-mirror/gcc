// { dg-options "-std=c++0x" }
// PR c++/34055
template<typename...> struct A; // { dg-error "declaration" }

template<typename...T> struct A<T*> // { dg-error "parameter packs|T" }
{
  void foo();
};

template<typename...T> void A<T*>::foo() {} // { dg-error "invalid use" }
