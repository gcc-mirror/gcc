// { dg-options "-std=c++11" }
template<typename...> struct tuple { };

template<typename... Args1, typename... Args2>
void foo(tuple<Args1..., Args2...>, tuple<Args1...>, tuple<Args2...>);

struct X{ };

void bar()
{
  tuple<int, float> tif;
  tuple<double, X> tdx;
  tuple<int, float, double, X> tall;
  foo(tall, tif, tdx);
}
