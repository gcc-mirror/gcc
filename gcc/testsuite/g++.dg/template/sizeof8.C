// { dg-do compile }
// Testcase by: bangerth@dealii.org
// PR c++/11406: ICE

template <int> struct S{};

template <int N> S<sizeof(new double[N])> f() {}

template S<sizeof(void*)> f<2>();
