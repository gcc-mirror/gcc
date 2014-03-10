// PR c++/60033
// { dg-options -std=c++1y }

template <typename... T>
auto f(T&&... ts)
{
   return sizeof...(ts);
}

template <typename... T>
auto g(T&&... ts) {
  return [&] (auto v) {
    return f(ts...);
  };
}

int main()
{
   return g(1,2,3,4)(5) == 4 ? 0 : 1;
}
