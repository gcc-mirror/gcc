// { dg-options "-std=c++0x" }
template<typename...T> struct A
{
  static T i; // { dg-error "parameter packs|T" }
};

int j = A<int>::i; // { dg-error "not a member" }
