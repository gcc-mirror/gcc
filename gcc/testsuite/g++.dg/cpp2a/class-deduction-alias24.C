// PR c++/119379
// { dg-do compile { target c++20 } }

template<class T, class U>
struct pair {
  pair(T, U);
};

template<class T>
struct S {
  template<class U> requires true
  using P = pair<T, U>;
};

using type = decltype(S<int>::P(1, 2));
using type = S<int>::P<int>;
