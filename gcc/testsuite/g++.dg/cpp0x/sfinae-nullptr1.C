// DR 1423, PR c++/52174
// { dg-do compile { target c++11 } }

template<class T>
T&& make();

template<class T>
void sink(T);

template<class T1, class T2,
  class = decltype(sink<T2>(make<T1>()))
>
auto f(int) -> char(&)[1];

template<class, class>
auto f(...) -> char(&)[2];

static_assert(sizeof(f<decltype(nullptr), bool>(0)) != 1, "");
