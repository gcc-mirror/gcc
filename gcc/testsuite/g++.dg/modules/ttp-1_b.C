// { dg-additional-options -fmodules-ts }

import bob;

template<typename T> struct X
{
  using type = T;
};

template<typename X, typename Y> struct same;
template<typename X> struct same<X, X> {};

void frob ()
{
  using type = Wrapper<X, int>::type::type;

  same<type, int> v;
}
