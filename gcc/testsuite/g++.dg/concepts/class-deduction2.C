// PR c++/85706
// { dg-additional-options "-std=c++17 -fconcepts" }

template<class T> struct S {
  S(T);
};

template<class = void>
auto f() -> decltype(S(42)); // error
