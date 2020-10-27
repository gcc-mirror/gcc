// PR c++/96241
// { dg-do compile { target c++11 } }

template <typename T, T...> struct S {};
template <typename T, T t> using U = S<T, __integer_pack(t)...>;
template <long... N> using f = S<unsigned long, N...>;
template <long N> using V = U<unsigned long, N>;
template <int N> struct A { typedef int type[N]; };
template <int N> struct B { typename A<N>::type k; };
template <typename T, int N, unsigned long... P>
constexpr B<N> bar(T (&arr)[N], f<P...>) {
  return {arr[P]...};
}
template <typename T, int N> constexpr B<N> foo(T (&arr)[N]) {
  return bar(arr, V<N>{});
}
constexpr char arr[2]{};
B<2> b = foo(arr);
