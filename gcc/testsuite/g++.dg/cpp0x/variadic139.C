// Origin: PR c++/53609
// { dg-do compile { target c++11 } }

template<class...I> struct List {};
template<int T> struct Z {static const int value = T;};
template<int...T> using LZ = List<Z<T>...>;

template<class...U>
struct F
{
  using N = LZ<U::value...>;
};

F<Z<1>, Z<2> >::N A;
