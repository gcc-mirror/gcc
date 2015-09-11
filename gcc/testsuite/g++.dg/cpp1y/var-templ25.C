// { dg-do compile { target c++14 } }

using fl = float;

template<class T> const int V = 0;
template<> const int V<fl> = 42;

static_assert(V<float> == 42, "");
