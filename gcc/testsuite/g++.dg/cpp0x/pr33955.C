// { dg-options "-std=c++11" }
template<typename T>
struct uncvref
{
  typedef T type;
};

template<typename... Args>
struct args
{
  static const int size = sizeof...(Args);
};

template<typename G, typename E, typename S, typename V, long GN = G::size, long EN = E::size>
struct apply_args;

template<typename... G, typename... E, typename S, typename V, long N>
struct apply_args<args<G...>, args<E...>, S, V, N, N>
{
  typedef args<
    typename G::template apply<typename uncvref<E>::type, S, V>::type...
    > type;
};

struct or_
{
  template<typename E, typename S, typename V>
  struct apply {
    typedef typename E::type type;
  };
};

template<typename T>
struct identity
{
  typedef T type;
};

apply_args<args<or_>, args<identity<int>>, float, double> a1;
