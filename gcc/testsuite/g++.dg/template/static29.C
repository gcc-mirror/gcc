// PR c++/29570

template<int> struct A { static const int i; };

template<int N> const int A<N>::i = { A<N>::i };
