// { dg-do compile { target c++11 } }
template<typename...T> struct A
{
  static T i; // { dg-error "parameter packs|T" }
};

int j = A<int>::i; // { dg-error "not a member" }
