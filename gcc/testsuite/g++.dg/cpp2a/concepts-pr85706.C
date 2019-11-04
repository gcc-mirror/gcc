// PR c++/85706
// { dg-do compile { target c++17 } }
// { dg-additional-options "-fconcepts" }

template<class T> struct S {
  S(T);
};

template<class = void>
auto f() -> decltype(S(42)); // error
