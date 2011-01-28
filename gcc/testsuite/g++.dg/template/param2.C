// Origin PR c++/47311
// { dg-do compile }

template < typename > class A0;
template <class Key, class T, template < typename TF = T> class TC = A0> class B0;

template <int> class A1;
template <class Key, class T, template <T p> class TC = A1> class B1;
