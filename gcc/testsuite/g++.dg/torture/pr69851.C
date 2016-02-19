// PR c++/69851
// { dg-do compile }
// { dg-options "-std=c++11" }

template <typename T>
struct A { T a; };
template <unsigned long, typename...>
struct B;
template <unsigned long N, typename T, typename... U>
struct B<N, T, U...> : B<1, U...>, A<T>
{
  B (B &) = default;
  B (B &&x) : B(x) {}
};
template <unsigned long N, typename T>
struct B<N, T> {};
struct C { C (C &); };
struct D {};

void
foo (B<0, C, D, int, int> a)
{
  B<0, C, D, int, int> b (a);
}
