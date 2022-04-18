// PR c++/104996
// { dg-do compile { target c++11 } }

template<unsigned size> char f(int (&&)[size]);
template<unsigned size> int f(int const (&)[size]);
static_assert(sizeof(f({1, 2, 3})) == 1, "");
