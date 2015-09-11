// { dg-do compile { target c++11 } }
// PR c++/65186

template<typename A, A x, A y>
using Id = int;

template<
  typename A,
  A x,
  A y,
  Id<A, x, y> p,
  template<A a, A b, Id<A, a, b>> class C,
  C<x, x, x> // { dg-bogus "not a valid type" }
> using J = C<x, y, p>;


template<class A>
using Z = A;

template<
  template <class> class A,
  A<int> B // { dg-bogus "not a valid type" }
>
struct C { };

C<Z, 5> a;
