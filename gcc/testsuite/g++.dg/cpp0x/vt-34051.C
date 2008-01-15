// { dg-options "-std=c++0x" }
struct A
{
  operator int();
};

template <typename... T> struct B : A
{
  using A::operator T; // { dg-error "parameter packs|T" }
};

B<int> b;
