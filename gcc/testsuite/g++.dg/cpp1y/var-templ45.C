// PR c++/67810
// { dg-do compile { target c++14 } }

template <class...>
constexpr bool Test = true;

template <typename...Ts, bool = (Test<Ts&&...>)>
void f();
