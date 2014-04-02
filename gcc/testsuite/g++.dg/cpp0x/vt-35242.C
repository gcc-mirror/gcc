// { dg-do compile { target c++11 } }
struct A
{
  template<typename... T> struct B;
};

template<typename... T> struct A::B<T*> {}; // { dg-error "parameter packs|T" }
