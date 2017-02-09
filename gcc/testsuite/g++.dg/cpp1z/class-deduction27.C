// PR c++/79316
// { dg-options -std=c++1z }

  template<typename T> struct S { S(T t) {} };
  template<typename T> S(T, int = 7) -> S<T>;
