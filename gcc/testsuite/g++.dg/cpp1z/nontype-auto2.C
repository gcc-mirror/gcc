// Testcase from P0127R2
// { dg-options -std=c++17 }

template <typename T> struct S;
template <typename T, T n> struct S<int[n]> {
  using Q = T;
};

typedef S<int[42]>::Q V;
typedef decltype(sizeof 0) V;  // OK; T was deduced to std::size_t from the type int[42]
