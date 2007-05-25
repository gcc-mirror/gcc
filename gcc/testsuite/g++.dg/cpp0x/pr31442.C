// { dg-options "-std=gnu++0x" }
template<typename... T, T = 0> struct A {}; // { dg-error "parameter packs|T|the end" }

struct B
{
  template <template <typename...> class C> B(C<int>);
};

B b = A<int>();
