// { dg-do compile { target c++11 } }

template <class T, int N>
constexpr int foo () { return N; }

struct B { unsigned c: foo<int>, 3(); };  // { dg-error "non-integral type|expected" }
