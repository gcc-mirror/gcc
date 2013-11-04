// Various tests for variadic templates and partial specialization.
// { dg-options "-std=c++11" }

// PR c++/36846
template<typename A, typename B>
struct pair;

template<typename... T>
struct pairs;

template<typename... AS, typename... BS>
struct pairs<pair<AS, BS>...> {
  struct mismatched_packs {};
};

template class pairs<
  pair<int, int>,
  pair<int, int>
>;

template<int A, int B>
struct point;

template<typename... T>
struct points;

template<int... AS, int... BS>
struct points<point<AS, BS>...> {
  struct mismatched_packs {};
};

template class points<
  point<0, 1>,
  point<0, 1>
>;

// PR c++/35477
template <class...ARGS> struct tuple {};
template <class A, class B> struct test {};
template <class... ARGS, class B> struct test<B, tuple<ARGS...>>
{
    template <class T> struct inside {};
};

// PR c++/38276
template<typename...> struct A;

template<typename, typename> struct B;

template<typename... T, typename... U> struct B<A<T...>, A<U...> >
{
  static int i;
};

B<A<>, A<int> > b1;

B<A<int>, A<> > b2;

// PR c++/35784
template <typename...> struct p;

template <typename, typename> struct d;

template <typename... A, typename... B>
struct d<p<A...>, p<B...> > { typedef int t; };

typedef d<p<>, p<int, float> >::t q;
typedef d<q, d<p<int>, p<float> >::t> r; // *

typedef d<d<p<>, p<int, float> >::t, d<p<>, p<> >::t> s;
