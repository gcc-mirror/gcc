// PR c++/71515

template < typename, typename = int > struct A;

template < typename T > struct A < T, typename A < T >::type >
{
  A < int > *a;
};
