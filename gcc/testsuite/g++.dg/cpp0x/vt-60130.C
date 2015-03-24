// PR c++/60130
// { dg-do compile { target c++11 } }

#include <tuple>

template <class S, class F, class... T>
S f1(F f, T... x)
{
  return std::get<0>(std::make_tuple(f(x)...));
}

template <class... T>
int f2(const T... x)
{
  return std::get<0>(std::make_tuple(f1<T>([](int n){return n;}, x)...));
}

int main()
{
  return f2(42);
}
