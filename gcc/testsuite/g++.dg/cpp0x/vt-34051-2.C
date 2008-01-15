// { dg-options "-std=c++0x" }
template<typename... T> struct A
{
  int i __attribute__((aligned(__alignof(T)))); // { dg-error "parameter packs|T" }
};

A<int> a;
