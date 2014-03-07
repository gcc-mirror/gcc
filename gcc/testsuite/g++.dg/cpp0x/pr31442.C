// { dg-do compile { target c++11 } }
template<typename... T, T = 0> struct A {}; // { dg-error "parameter packs|T|the end|parameter packs|anonymous" }

struct B
{
  template <template <typename...> class C> B(C<int>);
};

B b = A<int>(); // { dg-error "mismatch|expected" }
