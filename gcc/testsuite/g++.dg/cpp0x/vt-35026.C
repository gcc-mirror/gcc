// { dg-do compile { target c++11 } }
template<typename... T> struct A
{
  T* x[1]; // { dg-error "parameter packs|T" }
};

A<int> a;
