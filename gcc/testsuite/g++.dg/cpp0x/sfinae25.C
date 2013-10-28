// PR c++/49105
// { dg-options -std=c++11 }

template<class T, class = decltype(T{})>
char f(int);

template<class T>
auto f(...) -> char(&)[2];

static_assert(sizeof(f<const int&&>(0)) == 1, "Error"); // #
