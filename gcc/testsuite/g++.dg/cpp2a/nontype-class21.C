// PR c++/90101
// { dg-do compile { target c++2a } }

template<int N>
struct A{};

template<int N, A<N>>
struct B {};

B<2,A<2>{}> b;
