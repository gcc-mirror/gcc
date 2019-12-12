// PR c++/89648
// { dg-do compile { target c++11 } }

template <typename T, T U> struct A { static const T e = U; };
template <typename> struct B;
template <unsigned long, typename> struct C;
template <long i, typename T> using E = typename C<i, T>::k;
template <typename T, T...> struct F {};
template <typename T, T U> using G = F<T, __integer_pack(U)...>;
template <unsigned long... U> using H = F<unsigned long, U...>;
template <unsigned long U> using I = G<unsigned long, U>;
template <typename...> class J {};
template <typename... U> struct B<J<U...>> : A<long, sizeof...(U)> {};
template <unsigned long N, typename T, typename... U>
struct C<N, J<T, U...>> : C<N - 1, J<U...>> {};
template <typename T, typename... U> struct C<0, J<T, U...>> { typedef T k; };
template <unsigned long N, typename... U> E<N, J<U...>> qux (J<U...>);
struct D { template <typename T> void foo (T) {} };
void bar (...);
struct K { void baz (int &); int l; D m; int *n, o; };
template <typename T, typename U, typename V, unsigned long... W>
void quux (int, int *, D x, int *, int, int, T, U, V y, H<W...>) {
  x.foo ([=] { bar (qux <W>(y)...); });
}
void K::baz (int &x) {
  J<int, int> p;
  int q, r;
  long s;
  quux (x, &l, m, n, o, r, s, q, p, I<B<decltype(p)>::e> ());
}
