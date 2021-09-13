// PR c++/98463
// { dg-do compile { target c++11 } }

template <typename T> struct A { constexpr A () : a() {} [[no_unique_address]] T a; };
template <unsigned long, typename...> struct B;
template <unsigned long T, typename U, typename... V>
struct B<T, U, V...> : B<1, V...>, A<U> {};
template <unsigned long T, typename U> struct B<T, U> : A<U> {};
template <typename... h> struct C : B<0, h...> {};
struct D {};
struct E { C<int, D> k; virtual ~E (); } a;
