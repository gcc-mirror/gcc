// { dg-do compile { target c++11 } }

template<class T> constexpr inline T bar(T x) { return x; }

template short bar(short x); // #EI

constexpr auto yz = bar(0); // OK
constexpr auto ab = bar(short()); // #1 Error, but should be OK
constexpr auto mn = bar(short{}); // #2 Error, but should be OK
