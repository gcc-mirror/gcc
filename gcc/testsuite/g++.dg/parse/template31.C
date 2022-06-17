// PR c++/105436

template<bool> struct A;
template<int N> A<N >= 5> f();
