// PR c++/64521
// { dg-options "-frepo -std=c++11" }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-final cleanup-repo-files }

template <typename H> struct J { J(H) {} };
template <unsigned long, typename...> struct K;
template <unsigned long I> struct K<I> {};
template <unsigned long I, typename H, typename... T>
struct K<I, H, T...> : K<I + 1, T...>, J<H> {
  K(const H &p1, const T &... p2) : K<I + 1, T...>(p2...), J<H>(p1) {}
};
template <typename... E> struct C : K<0, E...> {
  C(const E &... p1) : K<0, E...>(p1...) {}
};
template <typename> struct A {
  A() = default;
};
struct M;
template <typename> struct L {
  struct B {
    template <typename> static M *__test(...);
    typedef A<int> _Del;
    typedef decltype(__test<_Del>()) type;
  };
  C<typename B::type, A<M>> _M_t;
  L(typename B::type) : _M_t(0, A<M>()) {}
};
struct M {};
int main() { L<int>(new M); }
