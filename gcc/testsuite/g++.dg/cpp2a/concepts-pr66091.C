// PR c++/66091
// { dg-do compile { target c++17 } }
// { dg-additional-options "-fconcepts" }

template<typename T>
concept C1 =
  requires() { typename T::type1; };


template<typename T>
concept C2 =
  C1<T> && requires() { typename T::type2; };


template<C1 T>
struct S {
  S& operator++() { return *this; }
  S& operator++() requires (C2<T>) { return *this; }
};
