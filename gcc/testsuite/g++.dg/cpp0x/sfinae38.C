// { dg-do compile { target c++11 } }

template<class T, unsigned = sizeof(T)>
auto f(int) -> char;

template<class>
auto f(...) -> char(&)[2];

static_assert(sizeof(f<void>(0)) != 1, "");
static_assert(sizeof(f<void()>(0)) != 1, "");
