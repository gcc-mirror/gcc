// { dg-options "-std=c++0x" }
template<typename... T> struct A
{
  T* x[1]; // { dg-error "parameter packs|T" }
};

A<int> a;
