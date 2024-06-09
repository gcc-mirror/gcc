// PR c++/114349
// { dg-do compile { target c++11 } }

using A = struct {};
template <template <typename> class, typename, typename>
using B = A;
template <typename T>
using C = typename T::D;
struct E {
  using D = B<C, int, A>;
};
template <class> constexpr bool foo (A) { return false; }
template <class T> struct F {
  using G = T;
  using H = E;
  F(const F &);
  void operator=(F) noexcept(foo <G> (H::D{}));
};
template <typename, typename, typename>
using I = F<int>;
template <typename K, typename V, typename H = K>
using J = I<K, V, H>;
struct K {
  typedef J<long, char> L;
  L k;
  K();
};
struct M {
  bool bar () const;
  K::L m;
};
K n;
bool M::bar () const { n.k = m; return true; }
