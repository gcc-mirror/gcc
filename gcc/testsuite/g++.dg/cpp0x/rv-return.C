// PR c++/41815
// { dg-options -std=c++0x }

template<typename T, typename U> struct same_type;
template<typename T> struct same_type<T, T> {};

int const f() { return 0; }

int &&r = f(); // binding "int&&" to "int" should succeed
same_type<decltype(f()), int const> s1;
same_type<decltype(0,f()), int> s2;

template <class T>
T const g() { return 0; }

int &&r2 = g<int>();
same_type<decltype(g<int>()), int const> s3;
same_type<decltype(0,g<int>()), int> s4;
